"""
Template processing for YAMLEngine using Jinja2.

This module provides the TemplateProcessor class for handling Jinja2 template
processing in YAML agent configurations. It supports:

- Variable interpolation: {{ state.key }}, {{ variables.key }}, {{ secrets.key }}
- Checkpoint context: {{ checkpoint.dir }}, {{ checkpoint.last }}
- Jinja2 filters: | tojson, | upper, | lower, | length, | fromjson
- Object passthrough: single expressions return native Python objects
- Template caching for performance

TEA-PY-008.1: Extracted from yaml_engine.py for modularity.

Example usage:
    >>> from jinja2 import Environment, BaseLoader, StrictUndefined
    >>> jinja_env = Environment(loader=BaseLoader(), undefined=StrictUndefined)
    >>> processor = TemplateProcessor(jinja_env, {"api_url": "https://api.example.com"}, {})
    >>> result = processor.process_template("{{ variables.api_url }}", {})
    >>> print(result)
    https://api.example.com
"""

import json
import os
import re
from typing import Any, Dict, List, Optional, Union

from jinja2 import Environment, TemplateError


class DotDict(dict):
    """
    Dictionary subclass that allows attribute-style access to keys.

    This enables Jinja2 templates to use dot notation for accessing
    nested dictionary values (e.g., {{ state.user.name }} instead of
    {{ state['user']['name'] }}).

    Missing keys return None instead of raising AttributeError, which allows
    Jinja2 templates to safely use patterns like {% if state.user_instruction %}
    without requiring | default(false) for every access.

    Example:
        >>> d = DotDict({'user': {'name': 'Alice'}})
        >>> d.user.name
        'Alice'
        >>> d.missing_key  # Returns None instead of raising
        None
    """

    def __getattr__(self, key: str) -> Any:
        try:
            value = self[key]
            if isinstance(value, dict) and not isinstance(value, DotDict):
                return DotDict(value)
            return value
        except KeyError:
            # Return None for missing keys instead of raising AttributeError
            # This allows safe access in Jinja2 templates like {% if state.x %}
            return None

    def __setattr__(self, key: str, value: Any) -> None:
        self[key] = value


class TemplateProcessor:
    """
    Process Jinja2 templates for YAML configurations.

    This class handles all template processing for the YAMLEngine, including
    variable interpolation, condition evaluation, and parameter processing.
    It maintains a cache of compiled templates for performance.

    Attributes:
        _jinja_env: The Jinja2 environment for template compilation
        _engine: Reference to the YAMLEngine for accessing variables/secrets
        _template_cache: Cache of compiled templates for performance
    """

    def __init__(
        self,
        jinja_env: Environment,
        variables: Dict[str, Any],
        secrets: Dict[str, Any],
        engine: Optional[Any] = None,
    ):
        """
        Initialize the template processor.

        Args:
            jinja_env: Jinja2 Environment instance for template compilation.
                      Should be configured with StrictUndefined for helpful
                      error messages on undefined variables.
            variables: Dictionary of global variables accessible in templates
                      via {{ variables.key }}. Only used if engine is None.
            secrets: Dictionary of secret values accessible in templates
                    via {{ secrets.key }}. Only used if engine is None.
            engine: Optional YAMLEngine reference. If provided, variables and
                   secrets are accessed dynamically from the engine.
        """
        self._jinja_env = jinja_env
        self._engine = engine
        # Fallback for standalone use (without engine)
        self._variables_fallback = variables
        self._secrets_fallback = secrets
        self._template_cache: Dict[str, Any] = {}

    @property
    def _variables(self) -> Dict[str, Any]:
        """Get variables from engine or fallback."""
        if self._engine is not None:
            return self._engine.variables
        return self._variables_fallback

    @property
    def _secrets(self) -> Dict[str, Any]:
        """Get secrets from engine or fallback."""
        if self._engine is not None:
            return self._engine.secrets
        return self._secrets_fallback

    @property
    def _data(self) -> Dict[str, Any]:
        """Get data section from engine (for reusable prompts, templates, constants)."""
        if self._engine is not None:
            return getattr(self._engine, "data", {})
        return {}

    def process_template(
        self,
        text: str,
        state: Dict[str, Any],
        checkpoint_dir: Optional[str] = None,
        last_checkpoint: Optional[str] = None,
        extra_context: Optional[Dict[str, Any]] = None,
    ) -> Any:
        """
        Process template variables in text using Jinja2.

        TEA-YAML-001: Refactored to use Jinja2 instead of Python eval().

        Supports:
        - {{ state.key }} - access state values
        - {{ variables.key }} - access global variables
        - {{ secrets.key }} - access secrets
        - {{ checkpoint.dir }} - configured checkpoint directory
        - {{ checkpoint.last }} - most recent auto-saved checkpoint path
        - {{ value | filter }} - Jinja2 filter syntax (tojson, upper, lower, length, etc.)
        - {% if %}...{% endif %} - Jinja2 conditionals
        - {% for %}...{% endfor %} - Jinja2 loops
        - {{ data | fromjson }} - custom filter to parse JSON strings
        - {{ result.key }} - access result from action (when extra_context provided)

        When the entire value is a single template expression (e.g., "{{ state.data }}"),
        returns the actual object instead of converting to string. This allows passing
        complex objects between actions (AC: 5).

        Uses StrictUndefined mode for helpful error messages on undefined variables (AC: 7).
        Templates are cached for performance (AC: 8).

        Args:
            text: The template string to process
            state: Current state dictionary accessible via {{ state.key }}
            checkpoint_dir: Checkpoint directory accessible via {{ checkpoint.dir }}
            last_checkpoint: Last checkpoint path accessible via {{ checkpoint.last }}
            extra_context: Optional additional context variables (e.g., result from action)

        Returns:
            Processed value. For single expressions, returns the native Python object.
            For mixed content or multi-expression templates, returns a string.

        Raises:
            ValueError: If template contains syntax errors or undefined variables
        """
        if not isinstance(text, str):
            return text

        # Build render context with checkpoint support (AC: 4)
        context = {
            "state": DotDict(state),
            "variables": DotDict(self._variables),
            "secrets": DotDict(self._secrets),
            "data": DotDict(self._data),
            "env": DotDict(
                os.environ
            ),  # Access environment variables via {{ env.VAR }}
            "checkpoint": DotDict(
                {
                    "dir": checkpoint_dir or "",
                    "last": last_checkpoint or "",
                }
            ),
        }

        # Add extra context variables (e.g., result from action)
        if extra_context:
            for key, value in extra_context.items():
                if isinstance(value, dict):
                    context[key] = DotDict(value)
                else:
                    context[key] = value

        text_stripped = text.strip()

        # Check if the entire string is a single template expression (AC: 5)
        # This allows returning actual objects instead of string representation
        # Pattern matches {{ expr }} where expr doesn't contain }} (no nested expressions)
        # Using [^}]+ instead of .+? to prevent matching multiple expressions
        single_expr_pattern = r"^\{\{\s*([^}]+(?:\}(?!\})[^}]*)*)\s*\}\}$"
        single_match = re.match(single_expr_pattern, text_stripped)

        # Also check there's no second {{ after the first expression
        if (
            single_match
            and "{%" not in text_stripped
            and text_stripped.count("{{") == 1
        ):
            expr = single_match.group(1).strip()

            try:
                # Use compile_expression for single expressions to return native objects
                # Check cache first
                cache_key = f"expr:{expr}"
                if cache_key not in self._template_cache:
                    self._template_cache[cache_key] = (
                        self._jinja_env.compile_expression(expr)
                    )

                compiled = self._template_cache[cache_key]
                return compiled(**context)
            except TemplateError as e:
                # Re-raise with helpful context
                raise ValueError(
                    f"Template error in expression '{{{{ {expr} }}}}': {e}"
                )
            except Exception:
                # Return original text if evaluation fails (backward compat)
                return text

        # Check if this has any template syntax at all
        if "{{" not in text and "{%" not in text and "${" not in text:
            return text

        # Multi-expression or mixed content: render as string (AC: 1, 3)
        try:
            # Check cache for full templates
            cache_key = f"tmpl:{text}"
            if cache_key not in self._template_cache:
                self._template_cache[cache_key] = self._jinja_env.from_string(text)

            template = self._template_cache[cache_key]
            result = template.render(**context)
        except TemplateError as e:
            # Re-raise with helpful context
            raise ValueError(f"Template error: {e}")

        # Also handle ${ } style (GitLab CI) for backward compatibility
        pattern2 = r"\$\{([^}]+)\}"
        result = re.sub(
            pattern2,
            lambda m: str(self._variables.get(m.group(1), m.group(0))),
            result,
        )

        return result

    def process_params(
        self,
        params: Dict[str, Any],
        state: Dict[str, Any],
        checkpoint_dir: Optional[str] = None,
        last_checkpoint: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Recursively process parameters, replacing template variables.

        Traverses nested dictionaries and lists, processing all string values
        as templates. Non-string values are passed through unchanged.

        Args:
            params: Dictionary of parameters to process
            state: Current state dictionary for template context
            checkpoint_dir: Checkpoint directory for template context
            last_checkpoint: Last checkpoint path for template context

        Returns:
            New dictionary with all template strings processed
        """
        processed = {}

        for key, value in params.items():
            if isinstance(value, str):
                processed[key] = self.process_template(
                    value, state, checkpoint_dir, last_checkpoint
                )
            elif isinstance(value, dict):
                processed[key] = self.process_params(
                    value, state, checkpoint_dir, last_checkpoint
                )
            elif isinstance(value, list):
                processed[key] = [
                    (
                        self.process_template(
                            item, state, checkpoint_dir, last_checkpoint
                        )
                        if isinstance(item, str)
                        else (
                            self.process_params(
                                item, state, checkpoint_dir, last_checkpoint
                            )
                            if isinstance(item, dict)
                            else item
                        )
                    )
                    for item in value
                ]
            else:
                processed[key] = value

        return processed

    def evaluate_condition(self, expr: str, state: Dict[str, Any]) -> bool:
        """
        Evaluate a condition expression using Jinja2.

        TEA-YAML-001: Unified condition evaluation using Jinja2 templates.

        Supports:
        - Jinja2 template: "{{ state.x > 5 }}" or "{{ 'urgent' in state.tags }}"
        - Simple variable: "has_results" -> state.get('has_results', False)
        - Negation: "!escalate" -> not state.get('escalate', False)
        - Jinja2 expressions without braces: "state.x > 5"
        - Python boolean literals: "True", "False"

        Args:
            expr: Condition expression to evaluate
            state: Current state dictionary for evaluation context

        Returns:
            bool: Result of the condition evaluation
        """
        if not isinstance(expr, str):
            return bool(expr)

        expr = expr.strip()

        # Handle Python boolean literals explicitly
        if expr == "True":
            return True
        if expr == "False":
            return False

        # Handle simple negation syntax: "!variable"
        if expr.startswith("!") and not expr.startswith("{{"):
            var_name = expr[1:].strip()
            if var_name.isidentifier():
                return not state.get(var_name, False)

        # Handle simple variable reference: "variable_name"
        if expr.isidentifier():
            return bool(state.get(expr, False))

        # If already a Jinja2 template, process it
        if "{{" in expr or "{%" in expr:
            result = self.process_template(expr, state)
            return bool(result)

        # Otherwise, treat as a Jinja2 expression without braces
        # Wrap it to make a proper template
        template_expr = "{{ " + expr + " }}"
        result = self.process_template(template_expr, state)
        return bool(result)

    def _convert_simple_expression(self, expr: str) -> str:
        """
        Convert simple expression syntax to Python.

        DEPRECATED: Use evaluate_condition with Jinja2 instead (TEA-YAML-001).

        Examples:
        - "!escalate" -> "not state.get('escalate', False)"
        - "has_results" -> "state.get('has_results', False)"

        Args:
            expr: Simple expression to convert

        Returns:
            Python expression string
        """
        expr = expr.strip()

        # Handle negation
        if expr.startswith("!"):
            var_name = expr[1:].strip()
            return f"not state.get('{var_name}', False)"

        # Simple variable reference
        if expr.isidentifier():
            return f"state.get('{expr}', False)"

        # Otherwise return as-is
        return expr

    def update_variables(self, variables: Dict[str, Any]) -> None:
        """
        Update the variables dictionary reference.

        Used when YAMLEngine.variables is updated after processor creation.
        Only applies to fallback mode (no engine reference).

        Args:
            variables: New variables dictionary
        """
        self._variables_fallback = variables

    def update_secrets(self, secrets: Dict[str, Any]) -> None:
        """
        Update the secrets dictionary reference.

        Used when YAMLEngine.secrets is updated after processor creation.
        Only applies to fallback mode (no engine reference).

        Args:
            secrets: New secrets dictionary
        """
        self._secrets_fallback = secrets

    def clear_cache(self) -> None:
        """
        Clear the template cache.

        Useful when template behavior changes (e.g., new filters added).
        """
        self._template_cache.clear()
