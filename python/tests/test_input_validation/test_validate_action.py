"""
Tests for Validate Action (AC10).

015.4-UNIT-050 to 015.4-UNIT-057, 015.4-INT-001 to 015.4-INT-004
"""

import pytest
from the_edge_agent import YAMLEngine
from the_edge_agent.actions import build_actions_registry


class TestValidateInputAction:
    """Tests for validate.input action."""

    def setup_method(self):
        """Set up test engine and registry."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_action_registered(self):
        """015.4-UNIT-050: validate.input action is registered."""
        assert "validate.input" in self.registry
        assert "actions.validate_input" in self.registry

    def test_valid_input_passes(self):
        """015.4-UNIT-051: Valid input returns valid=True and data."""
        action = self.registry["validate.input"]
        result = action(
            state={},
            data={"name": "John", "age": 30},
            schema={
                "name": {"type": "str", "required": True},
                "age": {"type": "int", "min": 0},
            },
        )
        assert result["valid"] is True
        assert result["data"]["name"] == "John"
        assert result["data"]["age"] == 30

    def test_invalid_input_returns_errors(self):
        """015.4-UNIT-052: Invalid input returns valid=False and errors."""
        action = self.registry["validate.input"]
        result = action(
            state={},
            data={"age": -5},
            schema={
                "name": {"type": "str", "required": True},
                "age": {"type": "int", "min": 0},
            },
        )
        assert result["valid"] is False
        assert "errors" in result
        assert len(result["errors"]) == 2

    def test_validates_state_by_default(self):
        """015.4-UNIT-053: Validates state if data not provided."""
        action = self.registry["validate.input"]
        result = action(
            state={"name": "John"},
            schema={"name": {"type": "str", "required": True}},
        )
        assert result["valid"] is True
        assert result["data"]["name"] == "John"

    def test_missing_schema_error(self):
        """015.4-UNIT-054: Missing schema returns error."""
        action = self.registry["validate.input"]
        result = action(
            state={"name": "John"},
            data={"name": "John"},
        )
        assert result["valid"] is False
        assert result["errors"][0]["error"] == "missing"

    def test_invalid_schema_error(self):
        """015.4-UNIT-055: Invalid schema returns error."""
        action = self.registry["validate.input"]
        result = action(
            state={},
            data={"name": "John"},
            schema={"name": {"type": "invalid_type"}},
        )
        assert result["valid"] is False
        assert "invalid_schema" in result["errors"][0]["error"]

    def test_raise_on_error_option(self):
        """015.4-UNIT-056: raise_on_error=True raises exception."""
        from the_edge_agent.validation import ValidationError

        action = self.registry["validate.input"]
        with pytest.raises(ValidationError):
            action(
                state={},
                data={"age": -5},
                schema={"age": {"type": "int", "min": 0}},
                raise_on_error=True,
            )

    def test_defaults_applied(self):
        """015.4-UNIT-057: Default values are applied."""
        action = self.registry["validate.input"]
        result = action(
            state={},
            data={},
            schema={"limit": {"type": "int", "default": 10}},
        )
        assert result["valid"] is True
        assert result["data"]["limit"] == 10


class TestValidateSchemaAction:
    """Tests for validate.schema action."""

    def setup_method(self):
        """Set up test engine and registry."""
        self.engine = YAMLEngine()
        self.registry = build_actions_registry(self.engine)

    def test_schema_action_registered(self):
        """validate.schema action is registered."""
        assert "validate.schema" in self.registry

    def test_create_valid_schema(self):
        """Create valid schema returns schema object."""
        action = self.registry["validate.schema"]
        result = action(
            state={},
            schema={"name": {"type": "str", "required": True}},
        )
        assert result["valid"] is True
        assert result["schema"] is not None

    def test_create_invalid_schema(self):
        """Create invalid schema returns error."""
        action = self.registry["validate.schema"]
        result = action(
            state={},
            schema={"name": {"type": "unknown_type"}},
        )
        assert result["valid"] is False
        assert result["error"] is not None


class TestActionIntegration:
    """Integration tests for validate.input in YAML workflows."""

    def test_action_in_yaml_workflow(self):
        """015.4-INT-001: validate.input works in YAML workflow."""
        config = {
            "nodes": [
                {
                    "name": "validate",
                    "uses": "validate.input",
                    "with": {
                        "data": "{{ state }}",
                        "schema": {
                            "query": {"type": "str", "required": True},
                        },
                    },
                    "output": "validation_result",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "validate"},
                {"from": "validate", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"query": "hello"}))
        final_state = events[-1]["state"]
        assert final_state["validation_result"]["valid"] is True

    def test_action_validation_failure_in_workflow(self):
        """015.4-INT-002: Validation failure captured in workflow state."""
        config = {
            "nodes": [
                {
                    "name": "validate",
                    "uses": "validate.input",
                    "with": {
                        "data": "{{ state }}",
                        "schema": {
                            "query": {"type": "str", "required": True},
                        },
                    },
                    "output": "validation_result",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "validate"},
                {"from": "validate", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))  # Missing required 'query'
        final_state = events[-1]["state"]
        assert final_state["validation_result"]["valid"] is False
        assert len(final_state["validation_result"]["errors"]) > 0
