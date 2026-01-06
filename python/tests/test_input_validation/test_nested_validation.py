"""
Tests for Nested Object Validation (AC8).

015.4-UNIT-038 to 015.4-UNIT-043
"""

import pytest
from the_edge_agent.validation import (
    InputSchema,
    validate_input,
    ValidationError,
)


class TestNestedDictValidation:
    """Tests for nested dict/object validation."""

    def test_valid_nested_object(self):
        """015.4-UNIT-038: Valid nested object passes."""
        schema = InputSchema.from_yaml(
            {
                "options": {
                    "type": "dict",
                    "properties": {
                        "temperature": {"type": "float", "min": 0.0, "max": 2.0},
                        "model": {"type": "str"},
                    },
                }
            }
        )
        result = validate_input(
            {"options": {"temperature": 0.7, "model": "gpt-4"}}, schema
        )
        assert result["options"]["temperature"] == 0.7
        assert result["options"]["model"] == "gpt-4"

    def test_invalid_nested_field(self):
        """015.4-UNIT-039: Invalid nested field returns error with path."""
        schema = InputSchema.from_yaml(
            {
                "options": {
                    "type": "dict",
                    "properties": {
                        "temperature": {"type": "float", "max": 2.0},
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"options": {"temperature": 3.0}}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "options.temperature"
        assert errors[0].error == "max"

    def test_deeply_nested_object(self):
        """015.4-UNIT-040: Deeply nested validation (3+ levels)."""
        schema = InputSchema.from_yaml(
            {
                "config": {
                    "type": "dict",
                    "properties": {
                        "llm": {
                            "type": "dict",
                            "properties": {
                                "settings": {
                                    "type": "dict",
                                    "properties": {
                                        "temperature": {"type": "float", "max": 1.0},
                                    },
                                },
                            },
                        },
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input(
                {"config": {"llm": {"settings": {"temperature": 2.0}}}}, schema
            )
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "config.llm.settings.temperature"

    def test_nested_defaults(self):
        """Nested fields with defaults."""
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

    def test_nested_required(self):
        """Nested required field validation."""
        schema = InputSchema.from_yaml(
            {
                "options": {
                    "type": "dict",
                    "properties": {
                        "api_key": {"type": "str", "required": True},
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"options": {}}, schema)
        errors = exc_info.value.errors
        assert errors[0].field == "options.api_key"
        assert errors[0].error == "required"

    def test_extra_fields_preserved(self):
        """Extra fields in nested object are preserved."""
        schema = InputSchema.from_yaml(
            {
                "options": {
                    "type": "dict",
                    "properties": {
                        "known": {"type": "str"},
                    },
                }
            }
        )
        result = validate_input(
            {"options": {"known": "value", "extra": "preserved"}}, schema
        )
        assert result["options"]["extra"] == "preserved"


class TestListItemValidation:
    """Tests for list item validation."""

    def test_valid_list_items(self):
        """015.4-UNIT-041: Valid list items pass."""
        schema = InputSchema.from_yaml(
            {
                "tags": {
                    "type": "list",
                    "items": {"type": "str", "max_length": 50},
                }
            }
        )
        result = validate_input({"tags": ["python", "yaml", "agent"]}, schema)
        assert result["tags"] == ["python", "yaml", "agent"]

    def test_invalid_list_item(self):
        """015.4-UNIT-042: Invalid list item returns error with index."""
        schema = InputSchema.from_yaml(
            {
                "tags": {
                    "type": "list",
                    "items": {"type": "str", "max_length": 5},
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"tags": ["ok", "toolong"]}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 1
        assert errors[0].field == "tags[1]"
        assert errors[0].error == "max_length"

    def test_multiple_invalid_items(self):
        """Multiple invalid list items return multiple errors."""
        schema = InputSchema.from_yaml(
            {
                "values": {
                    "type": "list",
                    "items": {"type": "int", "min": 0},
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input({"values": [1, -1, 2, -3]}, schema)
        errors = exc_info.value.errors
        assert len(errors) == 2
        fields = [e.field for e in errors]
        assert "values[1]" in fields
        assert "values[3]" in fields

    def test_nested_object_in_list(self):
        """015.4-UNIT-043: Nested object in list validation."""
        schema = InputSchema.from_yaml(
            {
                "items": {
                    "type": "list",
                    "items": {
                        "type": "dict",
                        "properties": {
                            "name": {"type": "str", "required": True},
                            "count": {"type": "int", "min": 0},
                        },
                    },
                }
            }
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_input(
                {"items": [{"name": "a", "count": 1}, {"count": -1}]}, schema
            )
        errors = exc_info.value.errors
        assert len(errors) == 2
        fields = [e.field for e in errors]
        assert "items[1].name" in fields
        assert "items[1].count" in fields

    def test_empty_list_passes(self):
        """Empty list passes validation."""
        schema = InputSchema.from_yaml(
            {
                "tags": {
                    "type": "list",
                    "items": {"type": "str"},
                }
            }
        )
        result = validate_input({"tags": []}, schema)
        assert result["tags"] == []

    def test_list_item_type_coercion(self):
        """List items are coerced to correct type."""
        schema = InputSchema.from_yaml(
            {
                "ids": {
                    "type": "list",
                    "items": {"type": "int"},
                }
            }
        )
        result = validate_input({"ids": ["1", "2", "3"]}, schema)
        assert result["ids"] == [1, 2, 3]
