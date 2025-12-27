"""
Generic Extraction Validation for YAML Agents (TEA-YAML-004).

Provides a declarative framework for validating LLM extractions with:
- Layer 1: Structural validation via extraction_schema YAML definitions
- Layer 2: Semantic validation via Prolog constraint rules
- Layer 3: LLM grounding via semantic probes

Additionally supports neurosymbolic enhancements:
- Schema-guided extraction prompt generation (symbolic→neural)
- Confidence score passthrough for probabilistic reasoning
- Validation failure logging for learning loops

Example YAML:
    extraction_schema:
      entities:
        required_fields: [name]
        optional_fields: [birth_date]
      relationships:
        types: [mother, father, affair]
        required_fields: [type, subject, object]
      guide_extraction: true
      confidence_tracking: true

    validation_constraints:
      language: prolog
      rules: |
        validation_error(missing_mother, Child) :-
          entity(Child, child),
          \\+ relationship(mother, _, Child).

    semantic_probes:
      - for_each: relationship
        where: "type == 'mother'"
        probe: "Does the text state that {{ subject }} is the mother of {{ object }}?"
        on_fail: reject

    validation_logging:
      enabled: true
      log_path: "${VALIDATION_LOG_PATH:-./validation_failures.jsonl}"
"""

import hashlib
import json
import logging
import os
import re
import tempfile
import threading
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

from jinja2 import Environment, BaseLoader, TemplateError

logger = logging.getLogger(__name__)


# ==============================================================================
# Data Classes for Schema Definition
# ==============================================================================


@dataclass
class EntitySpec:
    """Specification for entity extraction validation."""

    required_fields: List[str] = field(default_factory=list)
    optional_fields: List[str] = field(default_factory=list)
    type_field: str = "type"  # Field containing entity type


@dataclass
class RelationshipSpec:
    """Specification for relationship extraction validation."""

    types: List[str] = field(default_factory=list)
    required_fields: List[str] = field(default_factory=list)
    optional_fields: List[str] = field(default_factory=list)
    # Type-specific required fields (e.g., affair requires start_date, end_date)
    type_requirements: Dict[str, List[str]] = field(default_factory=dict)


@dataclass
class ExtractionSchema:
    """
    Schema definition for extraction validation.

    Parsed from YAML extraction_schema section and used for:
    - Structural validation of extracted entities/relationships
    - Schema-guided prompt generation (when guide_extraction=True)
    - Confidence score handling (when confidence_tracking=True)
    """

    entities: EntitySpec = field(default_factory=EntitySpec)
    relationships: RelationshipSpec = field(default_factory=RelationshipSpec)
    guide_extraction: bool = False
    confidence_tracking: bool = False

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExtractionSchema":
        """
        Create ExtractionSchema from YAML dictionary.

        Args:
            data: Dictionary from YAML extraction_schema section

        Returns:
            Parsed ExtractionSchema instance

        Raises:
            ValueError: If schema definition is invalid
        """
        entities_data = data.get("entities", {})
        relationships_data = data.get("relationships", {})

        # Parse entity spec
        entities = EntitySpec(
            required_fields=entities_data.get("required_fields", []),
            optional_fields=entities_data.get("optional_fields", []),
            type_field=entities_data.get("type_field", "type"),
        )

        # Parse relationship spec
        relationships = RelationshipSpec(
            types=relationships_data.get("types", []),
            required_fields=relationships_data.get("required_fields", []),
            optional_fields=relationships_data.get("optional_fields", []),
            type_requirements=relationships_data.get("type_requirements", {}),
        )

        return cls(
            entities=entities,
            relationships=relationships,
            guide_extraction=data.get("guide_extraction", False),
            confidence_tracking=data.get("confidence_tracking", False),
        )


@dataclass
class ValidationConstraints:
    """
    Prolog-based validation constraint definitions.

    Parsed from YAML validation_constraints section.
    """

    language: str = "prolog"
    rules: str = ""

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ValidationConstraints":
        """
        Create ValidationConstraints from YAML dictionary.

        Args:
            data: Dictionary from YAML validation_constraints section

        Returns:
            Parsed ValidationConstraints instance

        Raises:
            ValueError: If language is not 'prolog'
        """
        language = data.get("language", "prolog")
        if language != "prolog":
            raise ValueError(
                f"validation_constraints language must be 'prolog', got '{language}'"
            )
        return cls(
            language=language,
            rules=data.get("rules", ""),
        )


@dataclass
class SemanticProbe:
    """
    Single semantic probe definition for LLM grounding validation.
    """

    for_each: str  # "entity" or "relationship"
    where: Optional[str] = None  # Filter condition
    probe: str = ""  # Jinja2 template for yes/no question
    on_fail: str = "reject"  # "reject" or "warn"

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "SemanticProbe":
        """Create SemanticProbe from dictionary."""
        return cls(
            for_each=data.get("for_each", ""),
            where=data.get("where"),
            probe=data.get("probe", ""),
            on_fail=data.get("on_fail", "reject"),
        )


@dataclass
class ValidationLoggingConfig:
    """Configuration for validation failure logging."""

    enabled: bool = False
    log_failures: bool = True
    log_path: str = "./validation_failures.jsonl"

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ValidationLoggingConfig":
        """Create ValidationLoggingConfig from dictionary."""
        return cls(
            enabled=data.get("enabled", False),
            log_failures=data.get("log_failures", True),
            log_path=expand_env_vars(
                data.get("log_path", "./validation_failures.jsonl")
            ),
        )


@dataclass
class ValidationResult:
    """Result of extraction validation."""

    valid: bool
    errors: List[Dict[str, Any]] = field(default_factory=list)
    validated_at: str = ""

    def __post_init__(self):
        if not self.validated_at:
            self.validated_at = datetime.now(timezone.utc).isoformat()

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for action return value."""
        return {
            "valid": self.valid,
            "errors": self.errors,
            "validated_at": self.validated_at,
        }


# ==============================================================================
# Utility Functions
# ==============================================================================


def expand_env_vars(value: Any) -> Any:
    """
    Expand environment variables in a value.

    Supports ${VAR:-default} syntax.
    """
    if isinstance(value, str):
        # Pattern: ${VAR:-default} or ${VAR}
        pattern = r"\$\{([A-Z_][A-Z0-9_]*)(?::-([^}]*))?\}"

        def replacer(match):
            var_name = match.group(1)
            default = match.group(2) if match.group(2) is not None else ""
            return os.environ.get(var_name, default)

        return re.sub(pattern, replacer, value)
    elif isinstance(value, dict):
        return {k: expand_env_vars(v) for k, v in value.items()}
    elif isinstance(value, list):
        return [expand_env_vars(item) for item in value]
    return value


def compute_input_hash(data: Any) -> str:
    """Compute SHA256 hash of input data for deduplication."""
    serialized = json.dumps(data, sort_keys=True, default=str)
    return hashlib.sha256(serialized.encode()).hexdigest()[:16]


# ==============================================================================
# Structural Validation (Layer 1)
# ==============================================================================


class StructuralValidator:
    """
    Validates extracted entities and relationships against an ExtractionSchema.

    Implements AC: 2-5 for structural validation with field path error messages.
    """

    def __init__(self, schema: ExtractionSchema):
        self.schema = schema

    def validate_entities(self, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Validate entities against schema.

        Args:
            entities: List of extracted entity dictionaries

        Returns:
            List of validation errors (empty if valid)
        """
        errors = []
        for idx, entity in enumerate(entities):
            # Check required fields
            for field_name in self.schema.entities.required_fields:
                if field_name not in entity or entity[field_name] is None:
                    errors.append(
                        {
                            "type": "missing_required_field",
                            "path": f"entities[{idx}].{field_name}",
                            "entity": entity,
                            "expected": field_name,
                            "actual": None,
                            "message": f"Entity missing required field '{field_name}'",
                        }
                    )
        return errors

    def validate_relationships(
        self, relationships: List[Dict[str, Any]]
    ) -> List[Dict[str, Any]]:
        """
        Validate relationships against schema.

        Args:
            relationships: List of extracted relationship dictionaries

        Returns:
            List of validation errors (empty if valid)
        """
        errors = []
        for idx, rel in enumerate(relationships):
            # Check required fields
            for field_name in self.schema.relationships.required_fields:
                if field_name not in rel or rel[field_name] is None:
                    errors.append(
                        {
                            "type": "missing_required_field",
                            "path": f"relationships[{idx}].{field_name}",
                            "relationship": rel,
                            "expected": field_name,
                            "actual": None,
                            "message": f"Relationship missing required field '{field_name}'",
                        }
                    )

            # Check relationship type is valid (if types are specified)
            rel_type = rel.get("type")
            if (
                self.schema.relationships.types
                and rel_type not in self.schema.relationships.types
            ):
                errors.append(
                    {
                        "type": "invalid_relationship_type",
                        "path": f"relationships[{idx}].type",
                        "relationship": rel,
                        "expected": self.schema.relationships.types,
                        "actual": rel_type,
                        "message": f"Invalid relationship type '{rel_type}', expected one of {self.schema.relationships.types}",
                    }
                )

            # Check type-specific required fields
            if rel_type and rel_type in self.schema.relationships.type_requirements:
                for field_name in self.schema.relationships.type_requirements[rel_type]:
                    if field_name not in rel or rel[field_name] is None:
                        errors.append(
                            {
                                "type": "missing_type_specific_field",
                                "path": f"relationships[{idx}].{field_name}",
                                "relationship": rel,
                                "expected": field_name,
                                "actual": None,
                                "message": f"Relationship type '{rel_type}' requires field '{field_name}'",
                            }
                        )

        return errors

    def validate(
        self,
        entities: Optional[List[Dict[str, Any]]] = None,
        relationships: Optional[List[Dict[str, Any]]] = None,
    ) -> ValidationResult:
        """
        Validate entities and relationships against schema.

        Args:
            entities: List of extracted entities (default: empty list)
            relationships: List of extracted relationships (default: empty list)

        Returns:
            ValidationResult with valid=True if no errors
        """
        all_errors = []

        if entities:
            all_errors.extend(self.validate_entities(entities))

        if relationships:
            all_errors.extend(self.validate_relationships(relationships))

        return ValidationResult(
            valid=len(all_errors) == 0,
            errors=all_errors,
        )


# ==============================================================================
# Prolog Constraint Validation (Layer 2)
# ==============================================================================


class PrologConstraintValidator:
    """
    Validates extracted data using Prolog constraint rules.

    Implements AC: 7-10 for semantic validation using validation_error/2 predicates.
    """

    def __init__(
        self,
        constraints: ValidationConstraints,
        prolog_runtime: Any = None,
        timeout: float = 30.0,
        sandbox: bool = True,
    ):
        self.constraints = constraints
        self.timeout = timeout
        self.sandbox = sandbox
        self._runtime = prolog_runtime
        self._runtime_lock = threading.Lock()

    def _get_runtime(self):
        """Get or create Prolog runtime (lazy initialization)."""
        if self._runtime is None:
            with self._runtime_lock:
                if self._runtime is None:
                    from .prolog_runtime import PrologRuntime

                    self._runtime = PrologRuntime(
                        timeout=self.timeout,
                        sandbox=self.sandbox,
                    )
        return self._runtime

    def _assert_entity_facts(
        self,
        entities: List[Dict[str, Any]],
        confidence_tracking: bool = False,
    ) -> str:
        """
        Generate Prolog facts for entities.

        Args:
            entities: List of entity dictionaries
            confidence_tracking: Include confidence in facts (entity/3)

        Returns:
            Prolog code string with entity facts
        """
        facts = []
        for entity in entities:
            name = entity.get("name", "unknown")
            entity_type = entity.get("type", "unknown")
            # Escape single quotes in Prolog atoms
            name_escaped = str(name).replace("'", "\\'")
            type_escaped = str(entity_type).replace("'", "\\'")

            if confidence_tracking and "confidence" in entity:
                conf = float(entity["confidence"])
                facts.append(f"entity('{name_escaped}', '{type_escaped}', {conf}).")
            else:
                facts.append(f"entity('{name_escaped}', '{type_escaped}').")

        return "\n".join(facts)

    def _assert_relationship_facts(
        self,
        relationships: List[Dict[str, Any]],
        confidence_tracking: bool = False,
    ) -> str:
        """
        Generate Prolog facts for relationships.

        Args:
            relationships: List of relationship dictionaries
            confidence_tracking: Include confidence in facts

        Returns:
            Prolog code string with relationship facts
        """
        facts = []
        for rel in relationships:
            rel_type = str(rel.get("type", "unknown")).replace("'", "\\'")
            subject = str(rel.get("subject", "unknown")).replace("'", "\\'")
            obj = str(rel.get("object", "unknown")).replace("'", "\\'")
            start_date = rel.get("start_date")
            end_date = rel.get("end_date")
            confidence = rel.get("confidence")

            # Determine arity based on available fields
            if start_date is not None and end_date is not None:
                start_escaped = str(start_date).replace("'", "\\'")
                end_escaped = str(end_date).replace("'", "\\'")
                if confidence_tracking and confidence is not None:
                    # relationship/6 with dates and confidence
                    facts.append(
                        f"relationship('{rel_type}', '{subject}', '{obj}', "
                        f"'{start_escaped}', '{end_escaped}', {float(confidence)})."
                    )
                else:
                    # relationship/5 with dates
                    facts.append(
                        f"relationship('{rel_type}', '{subject}', '{obj}', "
                        f"'{start_escaped}', '{end_escaped}')."
                    )
            elif confidence_tracking and confidence is not None:
                # relationship/4 with confidence
                facts.append(
                    f"relationship('{rel_type}', '{subject}', '{obj}', {float(confidence)})."
                )
            else:
                # relationship/3 basic
                facts.append(f"relationship('{rel_type}', '{subject}', '{obj}').")

        return "\n".join(facts)

    def validate(
        self,
        entities: Optional[List[Dict[str, Any]]] = None,
        relationships: Optional[List[Dict[str, Any]]] = None,
        confidence_tracking: bool = False,
    ) -> ValidationResult:
        """
        Validate entities and relationships using Prolog constraints.

        The validation looks for validation_error(ErrorType, Context) predicate
        matches. First match causes validation failure (fail-fast).

        Args:
            entities: List of extracted entities
            relationships: List of extracted relationships
            confidence_tracking: Include confidence in Prolog facts

        Returns:
            ValidationResult with any constraint violations
        """
        runtime = self._get_runtime()

        # Build Prolog program with facts and user rules
        entity_facts = self._assert_entity_facts(entities or [], confidence_tracking)
        relationship_facts = self._assert_relationship_facts(
            relationships or [], confidence_tracking
        )

        # Combine: dynamic declarations + cleanup + facts + user rules + helper + query
        # We need to use list format [Type, Context] instead of error(Type, Context)
        # because janus can't convert compound terms to Python
        #
        # IMPORTANT: We retractall existing facts/rules to ensure clean state
        # between validation runs (especially important in shared Prolog engine)
        prolog_code = f"""
:- dynamic(entity/2).
:- dynamic(entity/3).
:- dynamic(relationship/3).
:- dynamic(relationship/4).
:- dynamic(relationship/5).
:- dynamic(relationship/6).
:- dynamic(validation_error/2).
:- dynamic(tea_get_validation_errors/1).

:- retractall(entity(_,_)).
:- retractall(entity(_,_,_)).
:- retractall(relationship(_,_,_)).
:- retractall(relationship(_,_,_,_)).
:- retractall(relationship(_,_,_,_,_)).
:- retractall(relationship(_,_,_,_,_,_)).
:- retractall(validation_error(_,_)).
:- retractall(tea_get_validation_errors(_)).

{entity_facts}

{relationship_facts}

{self.constraints.rules}

tea_get_validation_errors(Errors) :-
    findall([Type, Context], validation_error(Type, Context), Errors).
"""

        try:
            # Execute and look for validation_error/2 matches
            result = runtime.execute_node_code(
                prolog_code
                + """
tea_get_validation_errors(Errors),
return(errors, Errors).
""",
                {},
            )

            errors = result.get("errors", [])

            if errors:
                # Convert Prolog results to error format
                validation_errors = []
                for err in errors:
                    if isinstance(err, dict) and "functor" in err:
                        # Prolog compound term
                        args = err.get("args", [])
                        error_type = args[0] if len(args) > 0 else "unknown"
                        context = args[1] if len(args) > 1 else None
                    elif isinstance(err, (list, tuple)) and len(err) >= 2:
                        error_type, context = err[0], err[1]
                    else:
                        error_type = str(err)
                        context = None

                    validation_errors.append(
                        {
                            "type": "prolog_constraint_violation",
                            "error_type": error_type,
                            "context": context,
                            "message": f"Constraint violation: {error_type}({context})",
                        }
                    )

                return ValidationResult(valid=False, errors=validation_errors)

            return ValidationResult(valid=True, errors=[])

        except Exception as e:
            # Prolog syntax/runtime error
            return ValidationResult(
                valid=False,
                errors=[
                    {
                        "type": "prolog_error",
                        "message": str(e),
                    }
                ],
            )


# ==============================================================================
# Semantic Probes (Layer 3)
# ==============================================================================


class SemanticProbeExecutor:
    """
    Executes semantic probes to verify LLM grounding.

    Implements AC: 11-15 for LLM-based validation probes.
    """

    def __init__(
        self,
        probes: List[SemanticProbe],
        llm_call: Optional[Callable] = None,
        jinja_env: Optional[Environment] = None,
    ):
        self.probes = probes
        self.llm_call = llm_call
        self._jinja_env = jinja_env or Environment(
            loader=BaseLoader(),
            keep_trailing_newline=True,
        )

    def _render_probe(self, probe_template: str, context: Dict[str, Any]) -> str:
        """Render probe template with context variables."""
        try:
            template = self._jinja_env.from_string(probe_template)
            return template.render(**context)
        except TemplateError as e:
            raise ValueError(f"Error rendering probe template: {e}")

    def _evaluate_where_condition(self, where: str, item: Dict[str, Any]) -> bool:
        """
        Evaluate where condition against an item.

        Simple expression evaluator for conditions like "type == 'mother'"
        """
        if not where:
            return True

        # Create evaluation context
        context = dict(item)

        # Handle simple equality checks: field == 'value'
        eq_match = re.match(r"(\w+)\s*==\s*['\"]([^'\"]+)['\"]", where)
        if eq_match:
            field_name, expected_value = eq_match.groups()
            return str(context.get(field_name)) == expected_value

        # Handle inequality checks: field != 'value'
        neq_match = re.match(r"(\w+)\s*!=\s*['\"]([^'\"]+)['\"]", where)
        if neq_match:
            field_name, expected_value = neq_match.groups()
            return str(context.get(field_name)) != expected_value

        # For complex expressions, use a simple eval with restricted context
        try:
            return bool(eval(where, {"__builtins__": {}}, context))
        except Exception:
            return False

    def _parse_llm_response(self, response: str) -> bool:
        """
        Parse LLM yes/no response.

        Returns True for affirmative, False for negative.
        """
        response_lower = response.lower().strip()

        # Negative patterns FIRST (to catch "incorrect" before "correct")
        negative_patterns = [
            "no",
            "false",
            "incorrect",
            "negative",
            "not true",
            "not correct",
        ]
        if any(word in response_lower for word in negative_patterns):
            return False

        # Affirmative patterns
        affirmative_patterns = ["yes", "true", "correct", "affirmative", "indeed"]
        if any(word in response_lower for word in affirmative_patterns):
            return True

        # Default to False for ambiguous responses
        return False

    def validate(
        self,
        entities: Optional[List[Dict[str, Any]]] = None,
        relationships: Optional[List[Dict[str, Any]]] = None,
        source_text: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
    ) -> ValidationResult:
        """
        Execute semantic probes against extracted data.

        Args:
            entities: List of extracted entities
            relationships: List of extracted relationships
            source_text: Original source text for grounding
            state: Full state for template access

        Returns:
            ValidationResult with any probe failures
        """
        if not self.llm_call:
            return ValidationResult(
                valid=False,
                errors=[
                    {
                        "type": "configuration_error",
                        "message": "No LLM call function provided for semantic probes",
                    }
                ],
            )

        all_errors = []
        entities = entities or []
        relationships = relationships or []
        state = state or {}

        for probe in self.probes:
            # Determine items to iterate
            if probe.for_each == "entity":
                items = entities
            elif probe.for_each == "relationship":
                items = relationships
            else:
                all_errors.append(
                    {
                        "type": "invalid_probe_config",
                        "message": f"Invalid for_each value: {probe.for_each}",
                    }
                )
                continue

            # Filter by where condition
            for item in items:
                if not self._evaluate_where_condition(probe.where, item):
                    continue

                # Build context for template rendering
                context = {
                    **item,
                    "state": state,
                    "text": source_text or state.get("text", ""),
                }

                # Render probe question
                try:
                    question = self._render_probe(probe.probe, context)
                except ValueError as e:
                    all_errors.append(
                        {
                            "type": "probe_render_error",
                            "message": str(e),
                            "probe": probe.probe,
                            "item": item,
                        }
                    )
                    if probe.on_fail == "reject":
                        return ValidationResult(valid=False, errors=all_errors)
                    continue

                # Call LLM with yes/no question
                try:
                    # Construct a prompt that asks for yes/no
                    messages = [
                        {
                            "role": "system",
                            "content": "You are a fact-checker. Answer with ONLY 'yes' or 'no'.",
                        },
                        {"role": "user", "content": question},
                    ]

                    response = self.llm_call(
                        state={},
                        messages=messages,
                        max_tokens=10,
                    )

                    # Extract response text
                    response_text = ""
                    if isinstance(response, dict):
                        response_text = response.get(
                            "response", response.get("content", "")
                        )
                    elif isinstance(response, str):
                        response_text = response

                    # Parse response
                    is_valid = self._parse_llm_response(response_text)

                    if not is_valid:
                        error = {
                            "type": "semantic_probe_failed",
                            "probe": probe.probe,
                            "question": question,
                            "response": response_text,
                            "item": item,
                            "message": f"LLM probe failed: {question}",
                        }
                        all_errors.append(error)

                        # Fail-fast on rejection
                        if probe.on_fail == "reject":
                            return ValidationResult(valid=False, errors=all_errors)

                except Exception as e:
                    error = {
                        "type": "probe_execution_error",
                        "message": str(e),
                        "probe": probe.probe,
                        "question": question if "question" in dir() else probe.probe,
                        "item": item,
                    }
                    all_errors.append(error)

                    if probe.on_fail == "reject":
                        return ValidationResult(valid=False, errors=all_errors)

        return ValidationResult(valid=len(all_errors) == 0, errors=all_errors)


# ==============================================================================
# Schema-Guided Extraction Prompt Generation
# ==============================================================================


def generate_extraction_prompt(
    schema: ExtractionSchema,
    constraints: Optional[ValidationConstraints] = None,
    confidence_tracking: bool = False,
) -> str:
    """
    Generate an extraction prompt from schema definition (AC: 23-27).

    Creates a prompt that guides LLM extraction according to the declared
    ontology, implementing symbolic→neural flow for neurosymbolic AI.

    Args:
        schema: ExtractionSchema with entity/relationship specs
        constraints: Optional Prolog constraints (comments parsed for descriptions)
        confidence_tracking: Include instructions for confidence scores

    Returns:
        Generated extraction prompt string
    """
    lines = ["You are extracting entities and relationships from text.", ""]
    lines.append("ONTOLOGY:")

    # Entity specification
    entity_fields = []
    if schema.entities.required_fields:
        entity_fields.extend(f"{f} (required)" for f in schema.entities.required_fields)
    if schema.entities.optional_fields:
        entity_fields.extend(f"{f} (optional)" for f in schema.entities.optional_fields)
    if entity_fields:
        lines.append(f"- Entities must have: {', '.join(entity_fields)}")

    # Relationship specification
    if schema.relationships.types:
        lines.append(
            f"- Valid relationship types: {', '.join(schema.relationships.types)}"
        )
    rel_fields = []
    if schema.relationships.required_fields:
        rel_fields.extend(
            f"{f} (required)" for f in schema.relationships.required_fields
        )
    if rel_fields:
        lines.append(f"- Relationships must have: {', '.join(rel_fields)}")

    # Type-specific requirements
    for rel_type, fields in schema.relationships.type_requirements.items():
        lines.append(f"- {rel_type} relationships require: {', '.join(fields)}")

    # Parse human-readable constraints from Prolog comments
    if constraints and constraints.rules:
        constraint_descriptions = _parse_prolog_comments(constraints.rules)
        if constraint_descriptions:
            lines.append("")
            lines.append("CONSTRAINTS TO SATISFY:")
            for desc in constraint_descriptions:
                lines.append(f"- {desc}")

    # Confidence tracking instructions
    if confidence_tracking or schema.confidence_tracking:
        lines.append("")
        lines.append(
            "Include confidence scores (0.0-1.0) for each extraction. "
            "Add a 'confidence' field to each entity and relationship."
        )

    lines.append("")
    lines.append("Extract according to this schema.")

    return "\n".join(lines)


def _parse_prolog_comments(prolog_code: str) -> List[str]:
    """
    Parse human-readable constraint descriptions from Prolog comments.

    Looks for comments (% ...) that describe constraints and returns
    them as a list of descriptions.
    """
    descriptions = []
    for line in prolog_code.split("\n"):
        line = line.strip()
        if line.startswith("%"):
            # Extract comment text
            comment = line[1:].strip()
            # Skip empty comments and structural comments
            if comment and not comment.startswith("-"):
                descriptions.append(comment)
    return descriptions


# ==============================================================================
# Validation Failure Logging
# ==============================================================================


class ValidationLogger:
    """
    Logs validation failures in JSONL format for learning loops (AC: 32-36).

    Thread-safe, append-only logging with environment variable expansion
    for log path configuration.
    """

    def __init__(self, config: ValidationLoggingConfig):
        self.config = config
        self._lock = threading.Lock()
        self._file_handle = None

    def log_failure(
        self,
        agent_name: str,
        input_data: Any,
        failure_type: str,
        context: Dict[str, Any],
        extraction_attempt: Optional[Dict[str, Any]] = None,
    ) -> None:
        """
        Log a validation failure.

        Args:
            agent_name: Name of the agent that produced the extraction
            input_data: Original input data (for hashing)
            failure_type: Type of failure (schema, prolog, probe)
            context: Additional context about the failure
            extraction_attempt: The extraction that failed validation
        """
        if not self.config.enabled or not self.config.log_failures:
            return

        log_entry = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "agent_name": agent_name,
            "input_hash": compute_input_hash(input_data),
            "failure_type": failure_type,
            "context": context,
        }

        if extraction_attempt:
            log_entry["extraction_attempt"] = extraction_attempt

        self._write_log_entry(log_entry)

    def _write_log_entry(self, entry: Dict[str, Any]) -> None:
        """Write a log entry to the JSONL file (append-only)."""
        with self._lock:
            log_path = Path(self.config.log_path)

            # Ensure parent directory exists
            log_path.parent.mkdir(parents=True, exist_ok=True)

            # Append to file
            with open(log_path, "a", encoding="utf-8") as f:
                f.write(json.dumps(entry, default=str) + "\n")

    def close(self) -> None:
        """Close any open file handles."""
        pass  # Currently using context managers per-write


# ==============================================================================
# Main Extraction Validator (Unified Interface)
# ==============================================================================


class ExtractionValidator:
    """
    Unified extraction validation combining all three layers.

    This is the main entry point for the validate.extraction action.
    """

    def __init__(
        self,
        schema: Optional[ExtractionSchema] = None,
        constraints: Optional[ValidationConstraints] = None,
        probes: Optional[List[SemanticProbe]] = None,
        logging_config: Optional[ValidationLoggingConfig] = None,
        llm_call: Optional[Callable] = None,
        prolog_runtime: Any = None,
        jinja_env: Optional[Environment] = None,
        prolog_timeout: float = 30.0,
        prolog_sandbox: bool = True,
    ):
        self.schema = schema
        self.constraints = constraints
        self.probes = probes or []
        self.logging_config = logging_config or ValidationLoggingConfig()
        self.llm_call = llm_call
        self.prolog_runtime = prolog_runtime
        self.jinja_env = jinja_env
        self.prolog_timeout = prolog_timeout
        self.prolog_sandbox = prolog_sandbox

        # Initialize sub-validators lazily
        self._structural_validator: Optional[StructuralValidator] = None
        self._prolog_validator: Optional[PrologConstraintValidator] = None
        self._probe_executor: Optional[SemanticProbeExecutor] = None
        self._logger: Optional[ValidationLogger] = None

    def _get_structural_validator(self) -> Optional[StructuralValidator]:
        """Get or create structural validator."""
        if self.schema and self._structural_validator is None:
            self._structural_validator = StructuralValidator(self.schema)
        return self._structural_validator

    def _get_prolog_validator(self) -> Optional[PrologConstraintValidator]:
        """Get or create Prolog constraint validator."""
        if (
            self.constraints
            and self.constraints.rules
            and self._prolog_validator is None
        ):
            self._prolog_validator = PrologConstraintValidator(
                self.constraints,
                prolog_runtime=self.prolog_runtime,
                timeout=self.prolog_timeout,
                sandbox=self.prolog_sandbox,
            )
        return self._prolog_validator

    def _get_probe_executor(self) -> Optional[SemanticProbeExecutor]:
        """Get or create semantic probe executor."""
        if self.probes and self._probe_executor is None:
            self._probe_executor = SemanticProbeExecutor(
                self.probes,
                llm_call=self.llm_call,
                jinja_env=self.jinja_env,
            )
        return self._probe_executor

    def _get_logger(self) -> Optional[ValidationLogger]:
        """Get or create validation logger."""
        if self.logging_config.enabled and self._logger is None:
            self._logger = ValidationLogger(self.logging_config)
        return self._logger

    def validate(
        self,
        entities: Optional[List[Dict[str, Any]]] = None,
        relationships: Optional[List[Dict[str, Any]]] = None,
        source_text: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
        agent_name: str = "unknown",
    ) -> ValidationResult:
        """
        Run all validation layers on extracted data.

        Args:
            entities: List of extracted entities
            relationships: List of extracted relationships
            source_text: Original source text for probe grounding
            state: Full state for template access
            agent_name: Name of agent for logging

        Returns:
            ValidationResult combining all validation layers
        """
        all_errors = []
        confidence_tracking = self.schema.confidence_tracking if self.schema else False

        # Layer 1: Structural validation
        structural_validator = self._get_structural_validator()
        if structural_validator:
            result = structural_validator.validate(entities, relationships)
            if not result.valid:
                all_errors.extend(result.errors)
                # Log structural failures
                logger_inst = self._get_logger()
                if logger_inst:
                    logger_inst.log_failure(
                        agent_name=agent_name,
                        input_data=source_text or state,
                        failure_type="schema",
                        context={"errors": result.errors},
                        extraction_attempt={
                            "entities": entities,
                            "relationships": relationships,
                        },
                    )
                return ValidationResult(valid=False, errors=all_errors)

        # Layer 2: Prolog constraint validation
        prolog_validator = self._get_prolog_validator()
        if prolog_validator:
            result = prolog_validator.validate(
                entities, relationships, confidence_tracking
            )
            if not result.valid:
                all_errors.extend(result.errors)
                # Log constraint failures
                logger_inst = self._get_logger()
                if logger_inst:
                    logger_inst.log_failure(
                        agent_name=agent_name,
                        input_data=source_text or state,
                        failure_type="prolog",
                        context={"errors": result.errors},
                        extraction_attempt={
                            "entities": entities,
                            "relationships": relationships,
                        },
                    )
                return ValidationResult(valid=False, errors=all_errors)

        # Layer 3: Semantic probes
        probe_executor = self._get_probe_executor()
        if probe_executor:
            result = probe_executor.validate(
                entities, relationships, source_text, state
            )
            if not result.valid:
                all_errors.extend(result.errors)
                # Log probe failures
                logger_inst = self._get_logger()
                if logger_inst:
                    logger_inst.log_failure(
                        agent_name=agent_name,
                        input_data=source_text or state,
                        failure_type="probe",
                        context={"errors": result.errors},
                        extraction_attempt={
                            "entities": entities,
                            "relationships": relationships,
                        },
                    )
                return ValidationResult(valid=False, errors=all_errors)

        return ValidationResult(valid=True, errors=[])

    def generate_extraction_prompt(self) -> str:
        """
        Generate schema-guided extraction prompt.

        Returns:
            Extraction prompt string if guide_extraction is enabled,
            empty string otherwise.
        """
        if not self.schema or not self.schema.guide_extraction:
            return ""

        return generate_extraction_prompt(
            self.schema,
            self.constraints,
            self.schema.confidence_tracking,
        )
