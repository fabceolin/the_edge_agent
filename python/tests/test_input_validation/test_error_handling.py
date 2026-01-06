"""
Tests for Error Handling (AC9).

015.4-UNIT-044 to 015.4-UNIT-049
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
    ValidationErrorDetail,
)


class TestValidationErrorDetail:
    """Tests for ValidationErrorDetail structure."""

    def test_error_detail_to_dict(self):
        """015.4-UNIT-044: Error detail serializes to dict."""
        error = ValidationErrorDetail(
            field="query",
            error="required",
            message="Field 'query' is required",
        )
        result = error.to_dict()
        assert result["field"] == "query"
        assert result["error"] == "required"
        assert result["message"] == "Field 'query' is required"

    def test_error_detail_with_value(self):
        """Error detail includes value when provided."""
        error = ValidationErrorDetail(
            field="count",
            error="max",
            message="Field 'count' must be at most 100",
            value=150,
            constraint=100,
        )
        result = error.to_dict()
        assert result["value"] == 150
        assert result["constraint"] == 100

    def test_error_detail_with_expected(self):
        """Error detail includes expected type."""
        error = ValidationErrorDetail(
            field="count",
            error="type",
            message="Field 'count' must be int",
            value="abc",
            expected="int",
        )
        result = error.to_dict()
        assert result["expected"] == "int"

    def test_error_detail_repr(self):
        """Error detail has useful repr."""
        error = ValidationErrorDetail(
            field="query",
            error="required",
            message="Field 'query' is required",
        )
        repr_str = repr(error)
        assert "query" in repr_str
        assert "required" in repr_str


class TestValidationError:
    """Tests for ValidationError exception."""

    def test_validation_error_to_dict(self):
        """015.4-UNIT-045: ValidationError serializes to 422 format."""
        errors = [
            ValidationErrorDetail(
                field="query",
                error="required",
                message="Field 'query' is required",
            ),
            ValidationErrorDetail(
                field="count",
                error="max",
                message="Field 'count' must be at most 100",
                value=150,
                constraint=100,
            ),
        ]
        exc = ValidationError(errors=errors)
        result = exc.to_dict()
        assert result["success"] is False
        assert len(result["errors"]) == 2
        assert result["errors"][0]["field"] == "query"
        assert result["errors"][1]["field"] == "count"

    def test_validation_error_message(self):
        """ValidationError has informative message."""
        errors = [
            ValidationErrorDetail(
                field="query",
                error="required",
                message="Field 'query' is required",
            ),
        ]
        exc = ValidationError(errors=errors)
        assert "1 error" in str(exc)

    def test_validation_error_repr(self):
        """ValidationError has useful repr."""
        errors = [
            ValidationErrorDetail(
                field="query",
                error="required",
                message="Field 'query' is required",
            ),
        ]
        exc = ValidationError(errors=errors)
        assert "1" in repr(exc)


class TestErrorAggregation:
    """Tests for error aggregation (multiple errors reported)."""

    def test_multiple_errors_aggregated(self):
        """015.4-UNIT-046: Multiple errors are aggregated."""
        schema = InputSchema.from_yaml(
            {
                "name": {"type": "str", "required": True},
                "email": {"type": "str", "required": True},
                "age": {"type": "int", "min": 0, "max": 150},
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"age": 200}, schema)
        errors = exc_info.value.errors
        # Should have errors for: name (required), email (required), age (max)
        assert len(errors) == 3
        fields = {e.field for e in errors}
        assert "name" in fields
        assert "email" in fields
        assert "age" in fields

    def test_errors_not_stopped_at_first(self):
        """Validation doesn't stop at first error."""
        schema = InputSchema.from_yaml(
            {
                "a": {"type": "str", "required": True},
                "b": {"type": "str", "required": True},
                "c": {"type": "str", "required": True},
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 3

    def test_nested_errors_aggregated(self):
        """Nested errors are aggregated with paths."""
        schema = InputSchema.from_yaml(
            {
                "user": {
                    "type": "dict",
                    "properties": {
                        "name": {"type": "str", "min_length": 1},
                        "age": {"type": "int", "min": 0},
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"user": {"name": "", "age": -5}}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 2
        fields = [e.field for e in errors]
        assert "user.name" in fields
        assert "user.age" in fields


class TestHTTP422Format:
    """Tests for HTTP 422 response format."""

    def test_422_format(self):
        """015.4-UNIT-047: Error output matches HTTP 422 format."""
        schema = InputSchema.from_yaml(
            {
                "query": {"type": "str", "required": True},
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({}, schema)
        result = exc_info.value.to_dict()

        # Verify format
        assert "success" in result
        assert result["success"] is False
        assert "errors" in result
        assert isinstance(result["errors"], list)
        assert len(result["errors"]) > 0
        assert "field" in result["errors"][0]
        assert "error" in result["errors"][0]
        assert "message" in result["errors"][0]

    def test_error_field_path_format(self):
        """015.4-UNIT-048: Nested error field paths are dot-separated."""
        schema = InputSchema.from_yaml(
            {
                "config": {
                    "type": "dict",
                    "properties": {
                        "nested": {
                            "type": "dict",
                            "properties": {
                                "value": {"type": "int", "min": 0},
                            },
                        },
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"config": {"nested": {"value": -1}}}, schema)
        errors = exc_info.value.errors
        assert errors[0].field == "config.nested.value"

    def test_list_index_in_path(self):
        """015.4-UNIT-049: List errors include index in path."""
        schema = InputSchema.from_yaml(
            {
                "items": {
                    "type": "list",
                    "items": {"type": "int", "min": 0},
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"items": [1, -2, 3, -4]}, schema)
        errors = exc_info.value.errors
        fields = [e.field for e in errors]
        assert "items[1]" in fields
        assert "items[3]" in fields
