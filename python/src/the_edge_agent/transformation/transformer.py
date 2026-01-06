"""
Output Transformation Logic.

TEA-BUILTIN-015.5: Transforms state to output using schema definitions
with Jinja2 template evaluation and conditional field handling.
"""

import json
import logging
from datetime import datetime
from typing import Any, Dict, Optional

from jinja2 import Environment

from .schema import OutputSchema, OutputSchemaField

logger = logging.getLogger(__name__)


def transform_output(
    state: Dict[str, Any],
    schema: OutputSchema,
    env: Environment,
) -> Dict[str, Any]:
    """
    Transform state to output using schema.

    Evaluates Jinja2 templates for field values, handles conditional
    inclusion, applies defaults, and processes nested structures.

    Args:
        state: Current agent state dictionary.
        schema: Output schema definition with field mappings.
        env: Jinja2 environment for template rendering.

    Returns:
        Transformed output dictionary.

    Example:
        >>> from jinja2 import Environment, BaseLoader
        >>> env = Environment(loader=BaseLoader())
        >>> schema = OutputSchema.from_yaml({
        ...     "success": True,
        ...     "answer": "{{ state.result }}"
        ... })
        >>> state = {"result": "Hello"}
        >>> output = transform_output(state, schema, env)
        >>> output == {"success": True, "answer": "Hello"}
        True
    """
    output: Dict[str, Any] = {}

    # Build template context with useful helpers
    context = _build_context(state)

    for field_name, field_config in schema.fields.items():
        try:
            if isinstance(field_config, OutputSchemaField):
                # Complex field with include_if, default, etc.
                value = _process_complex_field(field_name, field_config, context, env)
                if value is not None:
                    output[field_name] = value
            else:
                # Simple value (static or template)
                value = _evaluate_value(field_config, context, env)
                output[field_name] = value
        except Exception as e:
            logger.warning(f"Error processing output field '{field_name}': {e}")
            # Skip field on error rather than failing entire transform
            continue

    return output


def _build_context(state: Dict[str, Any]) -> Dict[str, Any]:
    """
    Build template context with state and utility functions.

    Args:
        state: Current agent state.

    Returns:
        Context dictionary for Jinja2 rendering.
    """
    return {
        "state": state,
        "now": lambda: datetime.utcnow().isoformat() + "Z",
    }


def _process_complex_field(
    field_name: str,
    field_config: OutputSchemaField,
    context: Dict[str, Any],
    env: Environment,
) -> Optional[Any]:
    """
    Process a complex field with conditional inclusion and defaults.

    Args:
        field_name: Name of the field being processed.
        field_config: OutputSchemaField configuration.
        context: Template context with state and helpers.
        env: Jinja2 environment.

    Returns:
        Processed value, or None if field should be omitted.
    """
    # Check conditional inclusion first
    if field_config.include_if:
        should_include = _evaluate_condition(field_config.include_if, context, env)
        if not should_include:
            logger.debug(f"Field '{field_name}' excluded by include_if condition")
            return None

    # Evaluate the value template
    value = _evaluate_value(field_config.value, context, env)

    # Apply default if value is None, undefined, or empty string
    # Empty string from template like "{{ state.missing }}" should trigger default
    if (value is None or value == "") and field_config.default is not None:
        logger.debug(f"Field '{field_name}' using default value")
        value = field_config.default

    return value


def _evaluate_condition(
    condition: str,
    context: Dict[str, Any],
    env: Environment,
) -> bool:
    """
    Evaluate a condition expression.

    Args:
        condition: Condition expression (e.g., "state.completed").
        context: Template context.
        env: Jinja2 environment.

    Returns:
        Boolean result of condition evaluation.
    """
    try:
        # Wrap condition in Jinja2 template for evaluation
        template = env.from_string(f"{{{{ {condition} }}}}")
        result = template.render(**context).strip().lower()
        # Handle various truthy representations
        return result in ("true", "1", "yes", "on")
    except Exception as e:
        logger.warning(f"Error evaluating condition '{condition}': {e}")
        return False


def _evaluate_value(
    value: Any,
    context: Dict[str, Any],
    env: Environment,
) -> Any:
    """
    Evaluate a value, rendering Jinja2 templates if string contains {{ }}.

    Handles:
    - Static values (int, bool, float) - passed through
    - Template strings - rendered with Jinja2
    - Nested dicts - recursively processed
    - Lists - each item processed

    Args:
        value: Value to evaluate.
        context: Template context.
        env: Jinja2 environment.

    Returns:
        Evaluated value.
    """
    if value is None:
        return None

    if isinstance(value, bool):
        # Preserve boolean type (must check before int since bool is subclass)
        return value

    if isinstance(value, (int, float)):
        # Preserve numeric types
        return value

    if isinstance(value, str):
        if "{{" in value and "}}" in value:
            # Template string - render it
            return _render_template(value, context, env)
        else:
            # Static string
            return value

    if isinstance(value, dict):
        # Recursively process nested dict
        return {k: _evaluate_value(v, context, env) for k, v in value.items()}

    if isinstance(value, list):
        # Process each list item
        return [_evaluate_value(item, context, env) for item in value]

    # Unknown type - pass through
    return value


def _render_template(
    template_str: str,
    context: Dict[str, Any],
    env: Environment,
) -> Any:
    """
    Render a Jinja2 template string.

    If the template is a single expression that returns an object,
    returns the object directly (not stringified).

    If the template contains multiline YAML (e.g., for loops),
    attempts to parse as YAML/JSON.

    Args:
        template_str: Template string with {{ }} expressions.
        context: Template context.
        env: Jinja2 environment.

    Returns:
        Rendered value (string or native object).
    """
    try:
        template = env.from_string(template_str)
        result = template.render(**context)

        # Attempt to parse as JSON if result looks like JSON
        stripped = result.strip()
        if stripped.startswith(("[", "{")) or stripped in ("true", "false", "null"):
            try:
                return json.loads(stripped)
            except json.JSONDecodeError:
                pass

        # Try to convert numeric strings
        if stripped.isdigit():
            return int(stripped)
        try:
            return float(stripped)
        except ValueError:
            pass

        # Handle "None" string from template
        if stripped == "None":
            return None

        return result
    except Exception as e:
        logger.warning(f"Error rendering template: {e}")
        return None
