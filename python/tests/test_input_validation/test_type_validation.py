"""
Tests for Type Validation (AC2).

015.4-UNIT-005 to 015.4-UNIT-013
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestTypeValidation:
    """Tests for basic type validation."""

    def test_validate_string_type(self):
        """015.4-UNIT-005: Validate string type."""
        schema = InputSchema.from_yaml({"name": {"type": "str"}})
        result = validate_input({"name": "hello"}, schema)
        assert result["name"] == "hello"

    def test_validate_int_type(self):
        """015.4-UNIT-006: Validate int type."""
        schema = InputSchema.from_yaml({"count": {"type": "int"}})
        result = validate_input({"count": 42}, schema)
        assert result["count"] == 42

    def test_validate_float_type(self):
        """015.4-UNIT-007: Validate float type."""
        schema = InputSchema.from_yaml({"price": {"type": "float"}})
        result = validate_input({"price": 3.14}, schema)
        assert result["price"] == 3.14

    def test_validate_bool_type(self):
        """015.4-UNIT-008: Validate bool type."""
        schema = InputSchema.from_yaml({"enabled": {"type": "bool"}})
        result = validate_input({"enabled": True}, schema)
        assert result["enabled"] is True

    def test_validate_list_type(self):
        """015.4-UNIT-009: Validate list type."""
        schema = InputSchema.from_yaml({"tags": {"type": "list"}})
        result = validate_input({"tags": ["a", "b", "c"]}, schema)
        assert result["tags"] == ["a", "b", "c"]

    def test_validate_dict_type(self):
        """015.4-UNIT-010: Validate dict type."""
        schema = InputSchema.from_yaml({"options": {"type": "dict"}})
        result = validate_input({"options": {"key": "value"}}, schema)
        assert result["options"] == {"key": "value"}


class TestTypeCoercion:
    """Tests for type coercion (string to numeric, etc.)."""

    def test_coerce_string_to_int(self):
        """015.4-UNIT-011: Coerce string "123" to int 123."""
        schema = InputSchema.from_yaml({"count": {"type": "int"}})
        result = validate_input({"count": "42"}, schema)
        assert result["count"] == 42
        assert isinstance(result["count"], int)

    def test_coerce_string_to_float(self):
        """015.4-UNIT-012: Coerce string "3.14" to float 3.14."""
        schema = InputSchema.from_yaml({"price": {"type": "float"}})
        result = validate_input({"price": "3.14"}, schema)
        assert result["price"] == 3.14
        assert isinstance(result["price"], float)

    def test_coerce_int_to_float(self):
        """Coerce int to float (implicit widening)."""
        schema = InputSchema.from_yaml({"value": {"type": "float"}})
        result = validate_input({"value": 42}, schema)
        assert result["value"] == 42.0
        assert isinstance(result["value"], float)

    def test_coerce_string_to_bool_true(self):
        """Coerce string "true" to bool True."""
        schema = InputSchema.from_yaml({"enabled": {"type": "bool"}})
        result = validate_input({"enabled": "true"}, schema)
        assert result["enabled"] is True

    def test_coerce_string_to_bool_false(self):
        """Coerce string "false" to bool False."""
        schema = InputSchema.from_yaml({"enabled": {"type": "bool"}})
        result = validate_input({"enabled": "false"}, schema)
        assert result["enabled"] is False

    def test_coerce_number_to_string(self):
        """Coerce number to string."""
        schema = InputSchema.from_yaml({"id": {"type": "str"}})
        result = validate_input({"id": 123}, schema)
        assert result["id"] == "123"
        assert isinstance(result["id"], str)


class TestTypeMismatch:
    """Tests for type mismatch errors."""

    def test_wrong_type_error(self):
        """015.4-UNIT-013: Wrong type returns error with type info."""
        schema = InputSchema.from_yaml({"count": {"type": "int"}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"count": "not-a-number"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "count"
        assert errors[0].error == "type"
        assert errors[0].expected == "int"

    def test_list_for_string_error(self):
        """List value for string field returns error."""
        schema = InputSchema.from_yaml({"name": {"type": "str"}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"name": ["a", "b"]}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].error == "type"

    def test_string_for_list_error(self):
        """String value for list field returns error."""
        schema = InputSchema.from_yaml({"items": {"type": "list"}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"items": "not-a-list"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].error == "type"
