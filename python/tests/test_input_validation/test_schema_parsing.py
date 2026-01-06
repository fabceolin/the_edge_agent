"""
Tests for Input Schema Parsing (AC1).

015.4-UNIT-001 to 015.4-UNIT-005
"""

import pytest
from the_edge_agent.validation import InputSchema, InputSchemaField


class TestSchemaFieldParsing:
    """Tests for InputSchemaField creation and validation."""

    def test_create_string_field(self):
        """015.4-UNIT-001: Parse simple string field from YAML."""
        field = InputSchemaField(type="str", required=True)
        assert field.type == "str"
        assert field.required is True
        assert field.default is None

    def test_create_int_field_with_constraints(self):
        """015.4-UNIT-002: Parse int field with min/max constraints."""
        field = InputSchemaField(type="int", min=1, max=100, default=10)
        assert field.type == "int"
        assert field.min == 1
        assert field.max == 100
        assert field.default == 10

    def test_create_string_field_with_pattern(self):
        """015.4-UNIT-003: Parse string field with regex pattern."""
        field = InputSchemaField(
            type="str", pattern="^[a-z]+$", min_length=1, max_length=50
        )
        assert field.type == "str"
        assert field.pattern == "^[a-z]+$"
        assert field.min_length == 1
        assert field.max_length == 50

    def test_create_field_with_choices(self):
        """015.4-UNIT-004: Parse field with enum choices."""
        field = InputSchemaField(type="str", choices=["json", "text", "markdown"])
        assert field.type == "str"
        assert field.choices == ["json", "text", "markdown"]

    def test_invalid_type_raises_error(self):
        """015.4-UNIT-005: Invalid type raises ValueError."""
        with pytest.raises(ValueError, match="Invalid type"):
            InputSchemaField(type="invalid")


class TestSchemaParsing:
    """Tests for InputSchema.from_yaml() method."""

    def test_parse_simple_schema(self):
        """Parse schema with multiple fields."""
        yaml_dict = {
            "query": {"type": "str", "required": True},
            "limit": {"type": "int", "default": 10},
        }
        schema = InputSchema.from_yaml(yaml_dict)
        assert len(schema.fields) == 2
        assert "query" in schema.fields
        assert "limit" in schema.fields
        assert schema.fields["query"].required is True
        assert schema.fields["limit"].default == 10

    def test_parse_nested_schema(self):
        """Parse schema with nested properties."""
        yaml_dict = {
            "options": {
                "type": "dict",
                "properties": {
                    "temperature": {"type": "float", "default": 0.7},
                    "model": {"type": "str", "default": "gpt-4o-mini"},
                },
            }
        }
        schema = InputSchema.from_yaml(yaml_dict)
        assert schema.fields["options"].type == "dict"
        assert schema.fields["options"].properties is not None
        assert "temperature" in schema.fields["options"].properties
        assert schema.fields["options"].properties["temperature"].type == "float"

    def test_parse_list_schema(self):
        """Parse schema with list items."""
        yaml_dict = {
            "tags": {
                "type": "list",
                "items": {"type": "str", "max_length": 50},
            }
        }
        schema = InputSchema.from_yaml(yaml_dict)
        assert schema.fields["tags"].type == "list"
        assert schema.fields["tags"].items is not None
        assert schema.fields["tags"].items.type == "str"
        assert schema.fields["tags"].items.max_length == 50

    def test_schema_to_dict(self):
        """Test schema serialization to dict."""
        yaml_dict = {
            "query": {"type": "str", "required": True, "min_length": 1},
        }
        schema = InputSchema.from_yaml(yaml_dict)
        result = schema.to_dict()
        assert result["query"]["type"] == "str"
        assert result["query"]["required"] is True
        assert result["query"]["min_length"] == 1
