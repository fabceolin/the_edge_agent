"""
Tests for neurosymbolic enhancements (TEA-YAML-004 AC: 23-36).

Test coverage:
- Schema-guided extraction prompt generation (AC: 23-27)
- Confidence score passthrough (AC: 28-31)
- Validation failure logging (AC: 32-36)
"""

import json
import os
import pytest
import tempfile
from pathlib import Path

from the_edge_agent.extraction_validation import (
    ExtractionSchema,
    EntitySpec,
    RelationshipSpec,
    ValidationConstraints,
    ValidationLoggingConfig,
    ValidationLogger,
    ValidationResult,
    PrologConstraintValidator,
)
from the_edge_agent import YAMLEngine


# Check if Prolog runtime is available
try:
    from the_edge_agent.prolog_runtime import JANUS_AVAILABLE
except ImportError:
    JANUS_AVAILABLE = False


class TestSchemaGuidedExtractionPrompt:
    """Tests for schema-guided prompt generation (AC: 23-27)."""

    def test_extraction_prompt_generated_when_guide_extraction_true(self):
        """extraction_prompt variable is set when guide_extraction=True (AC: 23)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name", "type"]},
                "relationships": {"types": ["mother", "father"]},
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        assert "extraction_prompt" in engine.variables
        prompt = engine.variables["extraction_prompt"]
        assert isinstance(prompt, str)
        assert len(prompt) > 0

    def test_extraction_prompt_not_generated_when_guide_extraction_false(self):
        """No extraction_prompt when guide_extraction=False (AC: 23)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name"]},
                "guide_extraction": False,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        assert "extraction_prompt" not in engine.variables

    def test_extraction_prompt_contains_entity_fields(self):
        """Prompt includes entity field specifications (AC: 24)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "entities": {
                    "required_fields": ["name", "type"],
                    "optional_fields": ["birth_date"],
                },
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        prompt = engine.variables["extraction_prompt"]
        assert "name" in prompt
        assert "type" in prompt
        assert "required" in prompt.lower()

    def test_extraction_prompt_contains_relationship_types(self):
        """Prompt includes allowed relationship types (AC: 25)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "relationships": {
                    "types": ["mother", "father", "sibling"],
                    "required_fields": ["type", "subject", "object"],
                },
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        prompt = engine.variables["extraction_prompt"]
        # At least one relationship type should be mentioned
        assert any(rel in prompt for rel in ["mother", "father", "sibling"])

    def test_extraction_prompt_contains_type_specific_requirements(self):
        """Prompt includes type-specific field requirements (AC: 26)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "relationships": {
                    "types": ["affair"],
                    "required_fields": ["type", "subject", "object"],
                    "type_requirements": {
                        "affair": ["start_date", "end_date"],
                    },
                },
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        prompt = engine.variables["extraction_prompt"]
        # Type-specific fields should be mentioned
        assert (
            "affair" in prompt.lower() or "start_date" in prompt or "end_date" in prompt
        )

    def test_extraction_prompt_mentions_confidence_when_enabled(self):
        """Prompt mentions confidence when confidence_tracking=True (AC: 27)."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name"]},
                "confidence_tracking": True,
                "guide_extraction": True,
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        prompt = engine.variables["extraction_prompt"]
        assert "confidence" in prompt.lower()


@pytest.mark.skipif(not JANUS_AVAILABLE, reason="Prolog runtime not available")
class TestConfidenceScorePassthrough:
    """Tests for confidence score passthrough to Prolog (AC: 28-31)."""

    def test_entity_confidence_available_in_prolog(self):
        """Entity confidence scores are passed to Prolog facts (AC: 28)."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(low_confidence, E) :-
                    entity(E, _, C),
                    C < 0.5.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        # Low confidence entity
        result = validator.validate(
            entities=[{"name": "Alice", "type": "person", "confidence": 0.3}],
            relationships=[],
            confidence_tracking=True,
        )

        assert result.valid is False
        assert any("low_confidence" in str(e) for e in result.errors)

    def test_relationship_confidence_available_in_prolog(self):
        """Relationship confidence scores are passed to Prolog facts (AC: 29)."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(low_confidence_rel, Pair) :-
                    relationship(T, S, O, C),
                    C < 0.6,
                    Pair = T-S-O.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[],
            relationships=[
                {
                    "type": "mother",
                    "subject": "Alice",
                    "object": "Bob",
                    "confidence": 0.4,
                }
            ],
            confidence_tracking=True,
        )

        assert result.valid is False
        assert any("low_confidence_rel" in str(e) for e in result.errors)

    def test_high_confidence_passes(self):
        """High confidence scores pass confidence threshold constraints (AC: 30)."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                validation_error(low_confidence, E) :-
                    entity(E, _, C),
                    C < 0.5.
            """,
        )
        validator = PrologConstraintValidator(constraints)

        result = validator.validate(
            entities=[{"name": "Alice", "type": "person", "confidence": 0.95}],
            relationships=[],
            confidence_tracking=True,
        )

        assert result.valid is True

    def test_confidence_tracking_disabled_uses_entity2(self):
        """When confidence_tracking=False, entity/2 is used (AC: 31)."""
        constraints = ValidationConstraints(
            language="prolog",
            rules="""
                % Only matches entity/2 (without confidence)
                validation_error(found_entity, E) :-
                    entity(E, 'person').
            """,
        )
        validator = PrologConstraintValidator(constraints)

        # This should match since we're not using confidence_tracking
        result = validator.validate(
            entities=[{"name": "Alice", "type": "person", "confidence": 0.9}],
            relationships=[],
            confidence_tracking=False,  # Disabled
        )

        assert result.valid is False  # Should match entity/2


class TestValidationFailureLogging:
    """Tests for validation failure logging (AC: 32-36)."""

    def test_logger_writes_to_file(self):
        """ValidationLogger writes failures to JSONL file (AC: 32)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "failures.jsonl"

            config = ValidationLoggingConfig(
                enabled=True,
                log_path=str(log_path),
            )
            logger = ValidationLogger(config)

            logger.log_failure(
                agent_name="test-agent",
                input_data="Test source text",
                failure_type="schema",
                context={"error": "missing_field"},
            )

            assert log_path.exists()
            with open(log_path) as f:
                line = f.readline()
                data = json.loads(line)
                assert data["agent_name"] == "test-agent"
                assert data["failure_type"] == "schema"

    def test_logger_includes_timestamp(self):
        """Log entries include ISO timestamp (AC: 34)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "failures.jsonl"

            config = ValidationLoggingConfig(
                enabled=True,
                log_path=str(log_path),
            )
            logger = ValidationLogger(config)

            logger.log_failure(
                agent_name="test-agent",
                input_data="Test text",
                failure_type="prolog",
                context={},
            )

            with open(log_path) as f:
                data = json.loads(f.readline())
                assert "timestamp" in data
                # ISO format contains 'T'
                assert "T" in data["timestamp"]

    def test_logger_includes_input_hash(self):
        """Log entries include input hash for deduplication (AC: 35)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "failures.jsonl"

            config = ValidationLoggingConfig(
                enabled=True,
                log_path=str(log_path),
            )
            logger = ValidationLogger(config)

            logger.log_failure(
                agent_name="test-agent",
                input_data="Test text for hashing",
                failure_type="probe",
                context={},
            )

            with open(log_path) as f:
                data = json.loads(f.readline())
                assert "input_hash" in data
                # Should be a hex string
                assert all(c in "0123456789abcdef" for c in data["input_hash"])

    def test_logger_disabled_does_not_write(self):
        """Logger does not write when enabled=False (AC: 36)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "failures.jsonl"

            config = ValidationLoggingConfig(
                enabled=False,
                log_path=str(log_path),
            )
            logger = ValidationLogger(config)

            logger.log_failure(
                agent_name="test-agent",
                input_data="Test",
                failure_type="schema",
                context={},
            )

            assert not log_path.exists()

    def test_logger_appends_to_existing_file(self):
        """Logger appends to existing log file (AC: 36)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "failures.jsonl"

            config = ValidationLoggingConfig(
                enabled=True,
                log_path=str(log_path),
            )
            logger = ValidationLogger(config)

            # Log twice
            for i in range(2):
                logger.log_failure(
                    agent_name=f"test-agent-{i}",
                    input_data=f"text-{i}",
                    failure_type="schema",
                    context={},
                )

            with open(log_path) as f:
                lines = f.readlines()
                assert len(lines) == 2


class TestValidationLoggingConfigParsing:
    """Tests for ValidationLoggingConfig.from_dict()."""

    def test_parse_full_config(self):
        """Parse validation_logging with all options."""
        data = {
            "enabled": True,
            "log_path": "/var/log/validation.jsonl",
            "log_failures": True,
        }

        config = ValidationLoggingConfig.from_dict(data)

        assert config.enabled is True
        assert config.log_path == "/var/log/validation.jsonl"
        assert config.log_failures is True

    def test_parse_minimal_config(self):
        """Parse validation_logging with defaults."""
        data = {"enabled": True}

        config = ValidationLoggingConfig.from_dict(data)

        assert config.enabled is True
        assert config.log_failures is True  # Default

    def test_parse_with_env_var_expansion(self):
        """Log path supports environment variable expansion."""
        os.environ["TEST_LOG_PATH"] = "/custom/path"
        data = {
            "enabled": True,
            "log_path": "${TEST_LOG_PATH}/failures.jsonl",
        }

        config = ValidationLoggingConfig.from_dict(data)

        # Environment variable should be expanded
        assert "/custom/path" in config.log_path


class TestEndToEndNeurosymbolic:
    """Integration tests for full neurosymbolic workflow."""

    def test_yaml_engine_with_full_validation_config(self):
        """YAMLEngine correctly parses all validation config sections."""
        engine = YAMLEngine()
        config = {
            "extraction_schema": {
                "entities": {"required_fields": ["name"]},
                "relationships": {"types": ["mother"]},
                "guide_extraction": True,
                "confidence_tracking": True,
            },
            "validation_constraints": {
                "language": "prolog",
                "rules": "validation_error(test, x) :- fail.",
            },
            "semantic_probes": [
                {
                    "for_each": "entity",
                    "probe": "Is {{ name }} valid?",
                }
            ],
            "validation_logging": {
                "enabled": True,
                "log_path": "/tmp/test.jsonl",
            },
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [],
        }

        engine.load_from_dict(config)

        # Check all sections were parsed (keys match YAML structure)
        assert engine._extraction_validation_config is not None
        ev_config = engine._extraction_validation_config

        assert ev_config.get("extraction_schema") is not None
        assert ev_config["extraction_schema"]["guide_extraction"] is True
        assert ev_config["extraction_schema"]["confidence_tracking"] is True

        assert ev_config.get("validation_constraints") is not None
        assert ev_config["validation_constraints"]["language"] == "prolog"

        assert len(ev_config.get("semantic_probes", [])) == 1
        assert ev_config["semantic_probes"][0]["for_each"] == "entity"

        assert ev_config.get("validation_logging") is not None
        assert ev_config["validation_logging"]["enabled"] is True
