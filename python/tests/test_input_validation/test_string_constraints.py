"""
Tests for String Constraints (AC5).

015.4-UNIT-022 to 015.4-UNIT-028
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestMinLength:
    """Tests for min_length constraint."""

    def test_min_length_pass(self):
        """015.4-UNIT-022: String meeting min_length passes."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "min_length": 3}})
        result = validate_input({"query": "hello"}, schema)
        assert result["query"] == "hello"

    def test_min_length_exact(self):
        """String at exactly min_length passes."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "min_length": 5}})
        result = validate_input({"query": "hello"}, schema)
        assert result["query"] == "hello"

    def test_min_length_fail(self):
        """015.4-UNIT-023: String below min_length returns error."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "min_length": 5}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"query": "hi"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "query"
        assert errors[0].error == "min_length"
        assert errors[0].constraint == 5


class TestMaxLength:
    """Tests for max_length constraint."""

    def test_max_length_pass(self):
        """015.4-UNIT-024: String within max_length passes."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "max_length": 10}})
        result = validate_input({"query": "hello"}, schema)
        assert result["query"] == "hello"

    def test_max_length_exact(self):
        """String at exactly max_length passes."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "max_length": 5}})
        result = validate_input({"query": "hello"}, schema)
        assert result["query"] == "hello"

    def test_max_length_fail(self):
        """015.4-UNIT-025: String exceeding max_length returns error."""
        schema = InputSchema.from_yaml({"query": {"type": "str", "max_length": 5}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"query": "hello world"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "query"
        assert errors[0].error == "max_length"
        assert errors[0].constraint == 5


class TestPattern:
    """Tests for regex pattern constraint."""

    def test_pattern_match(self):
        """015.4-UNIT-026: String matching pattern passes."""
        schema = InputSchema.from_yaml(
            {"email": {"type": "str", "pattern": r"^[\w.-]+@[\w.-]+\.\w+$"}}
        )
        result = validate_input({"email": "user@example.com"}, schema)
        assert result["email"] == "user@example.com"

    def test_pattern_no_match(self):
        """015.4-UNIT-027: String not matching pattern returns error."""
        schema = InputSchema.from_yaml(
            {"email": {"type": "str", "pattern": r"^[\w.-]+@[\w.-]+\.\w+$"}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"email": "not-an-email"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "email"
        assert errors[0].error == "pattern"

    def test_uuid_pattern(self):
        """UUID pattern validation."""
        schema = InputSchema.from_yaml(
            {"session_id": {"type": "str", "pattern": r"^[a-f0-9-]{36}$"}}
        )
        result = validate_input(
            {"session_id": "12345678-1234-1234-1234-123456789abc"}, schema
        )
        assert result["session_id"] == "12345678-1234-1234-1234-123456789abc"

    def test_invalid_regex_pattern(self):
        """015.4-UNIT-028: Invalid regex pattern returns error."""
        schema = InputSchema.from_yaml(
            {"value": {"type": "str", "pattern": "[invalid(regex"}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"value": "test"}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].error == "pattern"
        assert "Invalid regex" in errors[0].message


class TestCombinedStringConstraints:
    """Tests for combined string constraints."""

    def test_min_max_length_combined(self):
        """Combined min_length and max_length."""
        schema = InputSchema.from_yaml(
            {"username": {"type": "str", "min_length": 3, "max_length": 20}}
        )
        result = validate_input({"username": "john_doe"}, schema)
        assert result["username"] == "john_doe"

    def test_length_and_pattern_combined(self):
        """Combined length and pattern constraints."""
        schema = InputSchema.from_yaml(
            {
                "code": {
                    "type": "str",
                    "min_length": 6,
                    "max_length": 6,
                    "pattern": r"^[A-Z0-9]+$",
                }
            }
        )
        result = validate_input({"code": "ABC123"}, schema)
        assert result["code"] == "ABC123"

    def test_multiple_violations(self):
        """Multiple constraint violations returns multiple errors."""
        schema = InputSchema.from_yaml(
            {"code": {"type": "str", "min_length": 6, "pattern": r"^[A-Z]+$"}}
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"code": "ab"}, schema)  # Too short and wrong pattern
        errors = exc_info.value.errors
        assert len(errors) == 2
        error_types = {e.error for e in errors}
        assert "min_length" in error_types
        assert "pattern" in error_types
