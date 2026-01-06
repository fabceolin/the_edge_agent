"""
Tests for Enum/Choices Validation (AC7).

015.4-UNIT-034 to 015.4-UNIT-037
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestChoicesValidation:
    """Tests for choices/enum constraint."""

    def test_valid_choice(self):
        """015.4-UNIT-034: Valid choice passes."""
        schema = InputSchema.from_yaml(
            {"format": {"type": "str", "choices": ["json", "text", "markdown"]}}
        )
        result = validate_input({"format": "json"}, schema)
        assert result["format"] == "json"

    def test_invalid_choice(self):
        """015.4-UNIT-035: Invalid choice returns error."""
        schema = InputSchema.from_yaml(
            {"format": {"type": "str", "choices": ["json", "text", "markdown"]}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"format": "xml"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "format"
        assert errors[0].error == "choices"
        assert errors[0].value == "xml"
        assert errors[0].constraint == ["json", "text", "markdown"]

    def test_int_choices(self):
        """015.4-UNIT-036: Integer choices validation."""
        schema = InputSchema.from_yaml(
            {"priority": {"type": "int", "choices": [1, 2, 3]}}
        )
        result = validate_input({"priority": 2}, schema)
        assert result["priority"] == 2

    def test_int_choice_invalid(self):
        """Invalid integer choice returns error."""
        schema = InputSchema.from_yaml(
            {"priority": {"type": "int", "choices": [1, 2, 3]}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"priority": 5}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "choices"

    def test_choice_with_default(self):
        """015.4-UNIT-037: Default value must be valid choice."""
        schema = InputSchema.from_yaml(
            {"format": {"type": "str", "default": "json", "choices": ["json", "text"]}}
        )
        result = validate_input({}, schema)
        assert result["format"] == "json"

    def test_choice_case_sensitive(self):
        """Choices are case-sensitive."""
        schema = InputSchema.from_yaml(
            {"format": {"type": "str", "choices": ["JSON", "TEXT"]}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"format": "json"}, schema)  # lowercase fails
        errors = exc_info.value.errors
        assert errors[0].error == "choices"

    def test_bool_choices(self):
        """Bool values in choices."""
        schema = InputSchema.from_yaml(
            {"enabled": {"type": "bool", "choices": [True]}}  # Only True allowed
        )
        result = validate_input({"enabled": True}, schema)
        assert result["enabled"] is True

        with pytest.raises(ValidationError):
            validate_input({"enabled": False}, schema)
