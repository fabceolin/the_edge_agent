"""
End-to-End Tests for Input Validation (AC1-AC9).

015.4-E2E-001 to 015.4-E2E-004
"""

import pytest
from the_edge_agent import YAMLEngine


class TestYAMLEngineInputSchema:
    """E2E tests for input_schema in YAMLEngine."""

    def test_input_schema_validates_before_execution(self):
        """015.4-E2E-001: input_schema validates before graph execution."""
        config = {
            "input_schema": {
                "query": {"type": "str", "required": True, "min_length": 1},
                "limit": {"type": "int", "default": 10, "min": 1, "max": 100},
            },
            "nodes": [
                {
                    "name": "process",
                    "run": "return {'result': state['query'].upper()}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "process", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Valid input passes
        events = list(graph.invoke({"query": "hello"}))
        final_event = events[-1]
        assert final_event["type"] == "final"
        assert final_event["state"]["query"] == "hello"
        assert final_event["state"]["limit"] == 10  # Default injected
        assert final_event["state"]["result"] == "HELLO"

    def test_input_schema_returns_validation_error(self):
        """015.4-E2E-002: Invalid input returns validation_error event."""
        config = {
            "input_schema": {
                "query": {"type": "str", "required": True},
            },
            "nodes": [
                {
                    "name": "process",
                    "run": "return {'result': 'processed'}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "process", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Missing required field returns validation error
        events = list(graph.invoke({}))
        assert len(events) == 1
        assert events[0]["type"] == "validation_error"
        assert events[0]["status_code"] == 422
        assert len(events[0]["errors"]) > 0
        assert events[0]["errors"][0]["field"] == "query"
        assert events[0]["errors"][0]["error"] == "required"

    def test_input_schema_complex_validation(self):
        """015.4-E2E-003: Complex schema with nested objects and lists."""
        config = {
            "input_schema": {
                "query": {
                    "type": "str",
                    "required": True,
                    "min_length": 1,
                    "max_length": 1000,
                },
                "options": {
                    "type": "dict",
                    "properties": {
                        "temperature": {
                            "type": "float",
                            "default": 0.7,
                            "min": 0.0,
                            "max": 2.0,
                        },
                        "model": {"type": "str", "default": "gpt-4o-mini"},
                    },
                },
                "tags": {
                    "type": "list",
                    "items": {"type": "str", "max_length": 50},
                },
            },
            "nodes": [
                {"name": "echo", "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "echo"},
                {"from": "echo", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Valid complex input
        events = list(
            graph.invoke(
                {
                    "query": "hello world",
                    "options": {"temperature": 1.0},
                    "tags": ["python", "yaml"],
                }
            )
        )
        final = events[-1]
        assert final["type"] == "final"
        assert final["state"]["query"] == "hello world"
        assert final["state"]["options"]["temperature"] == 1.0
        assert final["state"]["options"]["model"] == "gpt-4o-mini"  # Default
        assert final["state"]["tags"] == ["python", "yaml"]

    def test_input_schema_type_coercion(self):
        """015.4-E2E-004: Type coercion works in input_schema."""
        config = {
            "input_schema": {
                "count": {"type": "int"},
                "price": {"type": "float"},
                "enabled": {"type": "bool"},
            },
            "nodes": [
                {"name": "echo", "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "echo"},
                {"from": "echo", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # String values coerced to correct types
        events = list(
            graph.invoke(
                {
                    "count": "42",
                    "price": "3.14",
                    "enabled": "true",
                }
            )
        )
        final = events[-1]
        assert final["type"] == "final"
        assert final["state"]["count"] == 42
        assert isinstance(final["state"]["count"], int)
        assert final["state"]["price"] == 3.14
        assert isinstance(final["state"]["price"], float)
        assert final["state"]["enabled"] is True
        assert isinstance(final["state"]["enabled"], bool)


class TestInputSchemaErrorFormat:
    """Tests for error format in input_schema validation."""

    def test_multiple_errors_reported(self):
        """Multiple validation errors are all reported."""
        config = {
            "input_schema": {
                "name": {"type": "str", "required": True},
                "email": {
                    "type": "str",
                    "required": True,
                    "pattern": r"^[\w.-]+@[\w.-]+\.\w+$",
                },
                "age": {"type": "int", "min": 0, "max": 150},
            },
            "nodes": [{"name": "echo", "run": "return state"}],
            "edges": [
                {"from": "__start__", "to": "echo"},
                {"from": "echo", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Multiple validation failures
        events = list(
            graph.invoke(
                {
                    "email": "not-an-email",
                    "age": 200,
                }
            )
        )
        assert events[0]["type"] == "validation_error"
        errors = events[0]["errors"]
        assert len(errors) == 3  # name required, email pattern, age max
        fields = {e["field"] for e in errors}
        assert "name" in fields
        assert "email" in fields
        assert "age" in fields

    def test_nested_error_paths(self):
        """Nested validation errors include full path."""
        config = {
            "input_schema": {
                "config": {
                    "type": "dict",
                    "properties": {
                        "settings": {
                            "type": "dict",
                            "properties": {
                                "value": {"type": "int", "min": 0},
                            },
                        },
                    },
                },
            },
            "nodes": [{"name": "echo", "run": "return state"}],
            "edges": [
                {"from": "__start__", "to": "echo"},
                {"from": "echo", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        events = list(
            graph.invoke(
                {
                    "config": {"settings": {"value": -5}},
                }
            )
        )
        assert events[0]["type"] == "validation_error"
        assert events[0]["errors"][0]["field"] == "config.settings.value"
