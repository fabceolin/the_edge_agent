"""
Input Validators for YAML Agent Input Validation (TEA-BUILTIN-015.4).

Implements validation logic for input schemas:
- Type validation with coercion
- Required field enforcement
- Default value injection
- String constraints (min_length, max_length, pattern)
- Numeric constraints (min, max)
- Enum choices validation
- Nested object/list validation with path reporting
"""

import re
from typing import Any, Dict, List, Tuple, Union

from .schema import InputSchema, InputSchemaField
from .errors import ValidationError, ValidationErrorDetail


# Type mapping for Python types
TYPE_MAP = {
    "str": str,
    "int": int,
    "float": float,
    "bool": bool,
    "list": list,
    "dict": dict,
}


def validate_input(data: Dict[str, Any], schema: InputSchema) -> Dict[str, Any]:
    """
    Validate input data against schema.

    Returns validated data with defaults applied and types coerced.
    Raises ValidationError if validation fails with all errors aggregated.

    Args:
        data: Input data dictionary
        schema: InputSchema instance defining field requirements

    Returns:
        Validated data with defaults applied and types coerced

    Raises:
        ValidationError: If validation fails (contains all errors)

    Example:
        >>> schema = InputSchema.from_yaml({
        ...     "query": {"type": "str", "required": True},
        ...     "limit": {"type": "int", "default": 10}
        ... })
        >>> result = validate_input({"query": "hello"}, schema)
        >>> print(result)  # {"query": "hello", "limit": 10}
    """
    if data is None:
        data = {}

    errors: List[ValidationErrorDetail] = []
    validated: Dict[str, Any] = {}

    for field_name, field_schema in schema.fields.items():
        value = data.get(field_name)
        field_errors, validated_value = _validate_field(field_name, value, field_schema)
        errors.extend(field_errors)
        if validated_value is not None or (not field_errors and value is not None):
            validated[field_name] = validated_value
        elif field_schema.default is not None and value is None:
            # Default was applied in _validate_field
            validated[field_name] = validated_value

    if errors:
        raise ValidationError(errors=errors)

    return validated


def _validate_field(
    field_path: str,
    value: Any,
    schema: InputSchemaField,
) -> Tuple[List[ValidationErrorDetail], Any]:
    """
    Validate a single field against its schema.

    Returns tuple of (errors, validated_value).
    """
    errors: List[ValidationErrorDetail] = []

    # Handle missing values
    if value is None:
        if schema.required:
            errors.append(
                ValidationErrorDetail(
                    field=field_path,
                    error="required",
                    message=f"Field '{field_path}' is required",
                )
            )
            return errors, None
        elif schema.default is not None:
            return errors, schema.default
        else:
            return errors, None

    # Type validation and coercion
    validated_value, type_errors = _validate_and_coerce_type(
        field_path, value, schema.type
    )
    if type_errors:
        errors.extend(type_errors)
        return errors, None

    # String constraints
    if schema.type == "str":
        str_errors = _validate_string_constraints(field_path, validated_value, schema)
        errors.extend(str_errors)

    # Numeric constraints
    if schema.type in ("int", "float"):
        num_errors = _validate_numeric_constraints(field_path, validated_value, schema)
        errors.extend(num_errors)

    # Choices validation (enum)
    if schema.choices is not None:
        choice_errors = _validate_choices(field_path, validated_value, schema.choices)
        errors.extend(choice_errors)

    # Nested dict validation
    if schema.type == "dict" and schema.properties is not None:
        nested_errors, validated_value = _validate_nested_object(
            field_path, validated_value, schema.properties
        )
        errors.extend(nested_errors)

    # List item validation
    if schema.type == "list" and schema.items is not None:
        list_errors, validated_value = _validate_list_items(
            field_path, validated_value, schema.items
        )
        errors.extend(list_errors)

    return errors, validated_value


def _validate_and_coerce_type(
    field_path: str,
    value: Any,
    expected_type: str,
) -> Tuple[Any, List[ValidationErrorDetail]]:
    """
    Validate and coerce value to expected type.

    Supports type coercion:
    - String "123" -> int 123
    - String "12.5" -> float 12.5
    - String "true"/"false" -> bool True/False
    - int -> float (implicit)
    """
    errors: List[ValidationErrorDetail] = []
    target_type = TYPE_MAP[expected_type]

    # Already correct type
    if isinstance(value, target_type):
        return value, errors

    # Type coercion
    try:
        if expected_type == "int":
            if isinstance(value, float):
                # Check if float is actually an integer
                if value != int(value):
                    errors.append(
                        ValidationErrorDetail(
                            field=field_path,
                            error="type",
                            message=f"Field '{field_path}' must be {expected_type}",
                            value=value,
                            expected=expected_type,
                        )
                    )
                    return value, errors
                return int(value), errors
            elif isinstance(value, str):
                # Try to parse as int
                return int(value), errors
            elif isinstance(value, bool):
                # Don't coerce bool to int
                errors.append(
                    ValidationErrorDetail(
                        field=field_path,
                        error="type",
                        message=f"Field '{field_path}' must be {expected_type}",
                        value=value,
                        expected=expected_type,
                    )
                )
                return value, errors

        elif expected_type == "float":
            if isinstance(value, (int, str)):
                return float(value), errors

        elif expected_type == "bool":
            if isinstance(value, str):
                lower_val = value.lower()
                if lower_val in ("true", "1", "yes"):
                    return True, errors
                elif lower_val in ("false", "0", "no"):
                    return False, errors

        elif expected_type == "str":
            # Coerce numbers to string
            if isinstance(value, (int, float, bool)):
                return str(value), errors

        # Type mismatch - no coercion possible
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="type",
                message=f"Field '{field_path}' must be {expected_type}",
                value=value,
                expected=expected_type,
            )
        )

    except (ValueError, TypeError):
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="type",
                message=f"Field '{field_path}' must be {expected_type}",
                value=value,
                expected=expected_type,
            )
        )

    return value, errors


def _validate_string_constraints(
    field_path: str,
    value: str,
    schema: InputSchemaField,
) -> List[ValidationErrorDetail]:
    """Validate string-specific constraints."""
    errors: List[ValidationErrorDetail] = []

    # min_length
    if schema.min_length is not None and len(value) < schema.min_length:
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="min_length",
                message=f"Field '{field_path}' must have at least {schema.min_length} character(s)",
                value=value,
                constraint=schema.min_length,
            )
        )

    # max_length
    if schema.max_length is not None and len(value) > schema.max_length:
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="max_length",
                message=f"Field '{field_path}' must have at most {schema.max_length} character(s)",
                value=value,
                constraint=schema.max_length,
            )
        )

    # pattern (regex)
    if schema.pattern is not None:
        try:
            # Use timeout-like behavior by limiting pattern complexity
            # Note: Python's re module doesn't support timeout directly,
            # but we compile with a simple match to catch obvious issues
            pattern = re.compile(schema.pattern)
            if not pattern.match(value):
                errors.append(
                    ValidationErrorDetail(
                        field=field_path,
                        error="pattern",
                        message=f"Field '{field_path}' must match pattern: {schema.pattern}",
                        value=value,
                        constraint=schema.pattern,
                    )
                )
        except re.error as e:
            errors.append(
                ValidationErrorDetail(
                    field=field_path,
                    error="pattern",
                    message=f"Invalid regex pattern for '{field_path}': {e}",
                    constraint=schema.pattern,
                )
            )

    return errors


def _validate_numeric_constraints(
    field_path: str,
    value: Union[int, float],
    schema: InputSchemaField,
) -> List[ValidationErrorDetail]:
    """Validate numeric-specific constraints."""
    errors: List[ValidationErrorDetail] = []

    # min
    if schema.min is not None and value < schema.min:
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="min",
                message=f"Field '{field_path}' must be at least {schema.min}",
                value=value,
                constraint=schema.min,
            )
        )

    # max
    if schema.max is not None and value > schema.max:
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="max",
                message=f"Field '{field_path}' must be at most {schema.max}",
                value=value,
                constraint=schema.max,
            )
        )

    return errors


def _validate_choices(
    field_path: str,
    value: Any,
    choices: List[Any],
) -> List[ValidationErrorDetail]:
    """Validate value is in allowed choices."""
    errors: List[ValidationErrorDetail] = []

    if value not in choices:
        errors.append(
            ValidationErrorDetail(
                field=field_path,
                error="choices",
                message=f"Field '{field_path}' must be one of: {choices}",
                value=value,
                constraint=choices,
            )
        )

    return errors


def _validate_nested_object(
    field_path: str,
    value: Dict[str, Any],
    properties: Dict[str, InputSchemaField],
) -> Tuple[List[ValidationErrorDetail], Dict[str, Any]]:
    """Validate nested object properties recursively."""
    errors: List[ValidationErrorDetail] = []
    validated: Dict[str, Any] = {}

    for prop_name, prop_schema in properties.items():
        prop_value = value.get(prop_name)
        nested_path = f"{field_path}.{prop_name}"

        prop_errors, validated_value = _validate_field(
            nested_path, prop_value, prop_schema
        )
        errors.extend(prop_errors)

        if validated_value is not None or (not prop_errors and prop_value is not None):
            validated[prop_name] = validated_value
        elif prop_schema.default is not None and prop_value is None:
            validated[prop_name] = validated_value

    # Include any extra fields from the original value that aren't in schema
    for key, val in value.items():
        if key not in properties:
            validated[key] = val

    return errors, validated


def _validate_list_items(
    field_path: str,
    value: List[Any],
    items_schema: InputSchemaField,
) -> Tuple[List[ValidationErrorDetail], List[Any]]:
    """Validate list items against item schema."""
    errors: List[ValidationErrorDetail] = []
    validated: List[Any] = []

    for idx, item in enumerate(value):
        item_path = f"{field_path}[{idx}]"
        item_errors, validated_item = _validate_field(item_path, item, items_schema)
        errors.extend(item_errors)
        validated.append(validated_item)

    return errors, validated
