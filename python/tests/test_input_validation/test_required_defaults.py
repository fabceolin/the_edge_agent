"""
Tests for Required Fields and Default Values (AC3, AC4).

015.4-UNIT-014 to 015.4-UNIT-021
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestRequiredFields:
    """Tests for required field validation."""

    def test_required_field_present(self):
        """015.4-UNIT-014: Required field present passes validation."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "required": True}})
        result = validate_input({"query": "hello"}, schema)
        assert result["query"] == "hello"

    def test_required_field_missing(self):
        """015.4-UNIT-015: Required field missing returns error."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "required": True}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "query"
        assert errors[0].error == "required"
        assert "required" in errors[0].message.lower()

    def test_required_field_empty_string(self):
        """Required field with empty string passes (empty is not None)."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "required": True}})
        result = validate_input({"query": ""}, schema)
        assert result["query"] == ""

    def test_required_field_null_fails(self):
        """Required field with None value fails."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "required": True}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"query": None}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "required"

    def test_multiple_required_fields_missing(self):
        """Multiple required fields missing returns multiple errors."""
        schema = InputSchema.from_yaml(
            {
                "name": {"type": "str", "required": True},
                "email": {"type": "str", "required": True},
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 2
        fields = [e.field for e in errors]
        assert "name" in fields
        assert "email" in fields


class TestDefaultValues:
    """Tests for default value injection."""

    def test_default_value_injection(self):
        """015.4-UNIT-016: Default value is injected when field missing."""
        schema = InputSchema.from_yaml({"limit": {"type": "int", "default": 10}})
        result = validate_input({}, schema)
        assert result["limit"] == 10

    def test_default_value_not_used_when_provided(self):
        """015.4-UNIT-017: Default value not used when field is provided."""
        schema = InputSchema.from_yaml({"limit": {"type": "int", "default": 10}})
        result = validate_input({"limit": 20}, schema)
        assert result["limit"] == 20

    def test_default_value_for_string(self):
        """Default string value injection."""
        schema = InputSchema.from_yaml({"format": {"type": "str", "default": "json"}})
        result = validate_input({}, schema)
        assert result["format"] == "json"

    def test_default_value_for_bool(self):
        """Default bool value injection."""
        schema = InputSchema.from_yaml({"enabled": {"type": "bool", "default": True}})
        result = validate_input({}, schema)
        assert result["enabled"] is True

    def test_default_value_for_list(self):
        """Default list value injection."""
        schema = InputSchema.from_yaml(
            {"tags": {"type": "list", "default": ["default"]}}
        )
        result = validate_input({}, schema)
        assert result["tags"] == ["default"]

    def test_optional_field_no_default(self):
        """015.4-UNIT-018: Optional field without default is not in result."""
        schema = InputSchema.from_yaml({"optional": {"type": "str", "required": False}})
        result = validate_input({}, schema)
        assert "optional" not in result

    def test_nested_default_values(self):
        """Default values work for nested properties."""
        schema = InputSchema.from_yaml(
            {
                "options": {
                    "type": "dict",
                    "properties": {
                        "temperature": {"type": "float", "default": 0.7},
                        "model": {"type": "str", "default": "gpt-4o-mini"},
                    },
                }
            }
        )
        result = validate_input({"options": {}}, schema)
        assert result["options"]["temperature"] == 0.7
        assert result["options"]["model"] == "gpt-4o-mini"
