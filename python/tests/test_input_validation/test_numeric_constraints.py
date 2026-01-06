"""
Tests for Numeric Constraints (AC6).

015.4-UNIT-029 to 015.4-UNIT-033
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestMinConstraint:
    """Tests for min constraint."""

    def test_min_int_pass(self):
        """015.4-UNIT-029: Int above min passes."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1}})
        result = validate_input({"count": 5}, schema)
        assert result["count"] == 5

    def test_min_int_exact(self):
        """Int at exactly min passes (boundary)."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1}})
        result = validate_input({"count": 1}, schema)
        assert result["count"] == 1

    def test_min_int_fail(self):
        """015.4-UNIT-030: Int below min returns error."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"count": 0}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "count"
        assert errors[0].error == "min"
        assert errors[0].constraint == 1
        assert errors[0].value == 0

    def test_min_float_pass(self):
        """Float above min passes."""
        schema = InputSchema.from_yaml({"temperature": {"type": "float", "min": 0.0}})
        result = validate_input({"temperature": 0.5}, schema)
        assert result["temperature"] == 0.5

    def test_min_float_fail(self):
        """Float below min returns error."""
        schema = InputSchema.from_yaml({"temperature": {"type": "float", "min": 0.0}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"temperature": -0.1}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "min"


class TestMaxConstraint:
    """Tests for max constraint."""

    def test_max_int_pass(self):
        """015.4-UNIT-031: Int below max passes."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "max": 100}})
        result = validate_input({"count": 50}, schema)
        assert result["count"] == 50

    def test_max_int_exact(self):
        """Int at exactly max passes (boundary)."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "max": 100}})
        result = validate_input({"count": 100}, schema)
        assert result["count"] == 100

    def test_max_int_fail(self):
        """015.4-UNIT-032: Int above max returns error."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "max": 100}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"count": 150}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "count"
        assert errors[0].error == "max"
        assert errors[0].constraint == 100
        assert errors[0].value == 150

    def test_max_float_pass(self):
        """Float below max passes."""
        schema = InputSchema.from_yaml({"temperature": {"type": "float", "max": 2.0}})
        result = validate_input({"temperature": 1.5}, schema)
        assert result["temperature"] == 1.5

    def test_max_float_fail(self):
        """Float above max returns error."""
        schema = InputSchema.from_yaml({"temperature": {"type": "float", "max": 2.0}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"temperature": 2.5}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "max"


class TestMinMaxCombined:
    """Tests for combined min/max constraints."""

    def test_min_max_int_pass(self):
        """015.4-UNIT-033: Int within range passes."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1, "max": 100}})
        result = validate_input({"count": 50}, schema)
        assert result["count"] == 50

    def test_min_max_int_fail_below(self):
        """Int below range returns min error."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1, "max": 100}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"count": 0}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "min"

    def test_min_max_int_fail_above(self):
        """Int above range returns max error."""
        schema = InputSchema.from_yaml({"count": {"type": "int", "min": 1, "max": 100}})
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"count": 101}, schema)
        errors = exc_info.value.errors
        assert errors[0].error == "max"

    def test_min_max_float_range(self):
        """Float within range passes."""
        schema = InputSchema.from_yaml(
            {"temperature": {"type": "float", "min": 0.0, "max": 2.0}}
        )
        result = validate_input({"temperature": 0.7}, schema)
        assert result["temperature"] == 0.7
