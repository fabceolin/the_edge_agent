"""
Tests for Response Transformation (TEA-BUILTIN-015.5).

This module tests:
- OutputSchema model parsing
- Field mapping with Jinja2 templates
- Conditional field inclusion (include_if)
- Nested structures
- Default values
- HTTP response action
- Auto-application at graph end
"""

import pytest
from typing import Any, Dict

from jinja2 import Environment, BaseLoader


class TestOutputSchemaModel:
    """Test OutputSchema Pydantic model (AC1)."""

    def test_from_yaml_empty(self):
        """Empty schema parses successfully."""
        from the_edge_agent.transformation import OutputSchema

        schema = OutputSchema.from_yaml({})
        assert schema.fields == {}
        assert schema.get_field_names() == []

    def test_from_yaml_simple_static_values(self):
        """Static values (bool, int, str) preserved."""
        from the_edge_agent.transformation import OutputSchema

        config = {
            "success": True,
            "count": 42,
            "name": "test",
        }
        schema = OutputSchema.from_yaml(config)

        assert schema.fields["success"] is True
        assert schema.fields["count"] == 42
        assert schema.fields["name"] == "test"

    def test_from_yaml_template_strings(self):
        """Template strings preserved as-is for later evaluation."""
        from the_edge_agent.transformation import OutputSchema

        config = {
            "answer": "{{ state.result }}",
            "session_id": "{{ state.sid }}",
        }
        schema = OutputSchema.from_yaml(config)

        assert schema.fields["answer"] == "{{ state.result }}"
        assert schema.fields["session_id"] == "{{ state.sid }}"

    def test_from_yaml_complex_field(self):
        """Complex field config with value/include_if/default."""
        from the_edge_agent.transformation import OutputSchema, OutputSchemaField

        config = {
            "result": {
                "value": "{{ state.data }}",
                "include_if": "state.show_data",
                "default": "N/A",
            }
        }
        schema = OutputSchema.from_yaml(config)

        field = schema.fields["result"]
        assert isinstance(field, OutputSchemaField)
        assert field.value == "{{ state.data }}"
        assert field.include_if == "state.show_data"
        assert field.default == "N/A"

    def test_from_yaml_nested_dict_not_complex(self):
        """Nested dict without value/include_if/default is passed through."""
        from the_edge_agent.transformation import OutputSchema, OutputSchemaField

        config = {
            "metadata": {
                "author": "test",
                "version": "1.0",
            }
        }
        schema = OutputSchema.from_yaml(config)

        # Should not be OutputSchemaField since no value/include_if/default
        field = schema.fields["metadata"]
        assert not isinstance(field, OutputSchemaField)
        assert field == {"author": "test", "version": "1.0"}

    def test_get_field_names(self):
        """get_field_names returns all field names."""
        from the_edge_agent.transformation import OutputSchema

        schema = OutputSchema.from_yaml(
            {
                "a": 1,
                "b": "{{ x }}",
                "c": {"value": "y"},
            }
        )
        names = schema.get_field_names()
        assert set(names) == {"a", "b", "c"}

    def test_is_conditional(self):
        """is_conditional detects include_if fields."""
        from the_edge_agent.transformation import OutputSchema

        schema = OutputSchema.from_yaml(
            {
                "always": "{{ x }}",
                "maybe": {"value": "{{ y }}", "include_if": "state.show"},
            }
        )
        assert not schema.is_conditional("always")
        assert schema.is_conditional("maybe")

    def test_has_default(self):
        """has_default detects default fields."""
        from the_edge_agent.transformation import OutputSchema

        schema = OutputSchema.from_yaml(
            {
                "no_default": "{{ x }}",
                "with_default": {"value": "{{ y }}", "default": 0},
            }
        )
        assert not schema.has_default("no_default")
        assert schema.has_default("with_default")


class TestFieldMapping:
    """Test field mapping with Jinja2 templates (AC2, AC3)."""

    def setup_method(self):
        """Create Jinja2 environment for tests."""
        self.env = Environment(loader=BaseLoader())

    def test_static_value_passthrough(self):
        """Static values passed through unchanged."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "success": True,
                "count": 42,
            }
        )
        state = {"foo": "bar"}
        output = transform_output(state, schema, self.env)

        assert output["success"] is True
        assert output["count"] == 42

    def test_template_string_rendering(self):
        """Template strings rendered with state."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "answer": "{{ state.result }}",
                "greeting": "Hello, {{ state.name }}!",
            }
        )
        state = {"result": "42", "name": "Alice"}
        output = transform_output(state, schema, self.env)

        # Note: numeric strings are converted to numbers
        assert output["answer"] == 42
        assert output["greeting"] == "Hello, Alice!"

    def test_nested_template_access(self):
        """Templates can access nested state properties."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "user_name": "{{ state.user.name }}",
                "city": "{{ state.user.address.city }}",
            }
        )
        state = {
            "user": {
                "name": "Bob",
                "address": {"city": "NYC"},
            }
        }
        output = transform_output(state, schema, self.env)

        assert output["user_name"] == "Bob"
        assert output["city"] == "NYC"

    def test_template_with_filter(self):
        """Jinja2 filters work in templates."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "upper_name": "{{ state.name | upper }}",
                "lower_name": "{{ state.name | lower }}",
            }
        )
        state = {"name": "TEST"}
        output = transform_output(state, schema, self.env)

        assert output["upper_name"] == "TEST"
        assert output["lower_name"] == "test"


class TestConditionalFields:
    """Test conditional field inclusion (AC4)."""

    def setup_method(self):
        """Create Jinja2 environment for tests."""
        self.env = Environment(loader=BaseLoader())

    def test_include_if_true(self):
        """Field included when condition is true."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "error": {
                    "value": "{{ state.error_msg }}",
                    "include_if": "state.has_error",
                }
            }
        )
        state = {"has_error": True, "error_msg": "Something failed"}
        output = transform_output(state, schema, self.env)

        assert "error" in output
        assert output["error"] == "Something failed"

    def test_include_if_false(self):
        """Field excluded when condition is false."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "error": {
                    "value": "{{ state.error_msg }}",
                    "include_if": "state.has_error",
                }
            }
        )
        state = {"has_error": False, "error_msg": "Something failed"}
        output = transform_output(state, schema, self.env)

        assert "error" not in output

    def test_include_if_missing_key(self):
        """Field excluded when condition key is missing (falsy)."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "debug": {
                    "value": "{{ state.debug_info }}",
                    "include_if": "state.debug_mode",
                }
            }
        )
        state = {"debug_info": "some info"}  # no debug_mode key
        output = transform_output(state, schema, self.env)

        assert "debug" not in output

    def test_conditional_with_complex_expression(self):
        """Conditional can use complex expressions."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "premium_content": {
                    "value": "{{ state.content }}",
                    "include_if": "state.user.is_premium",
                }
            }
        )
        state = {
            "content": "secret stuff",
            "user": {"is_premium": True},
        }
        output = transform_output(state, schema, self.env)

        assert "premium_content" in output


class TestNestedStructures:
    """Test nested structure handling (AC5, AC6)."""

    def setup_method(self):
        """Create Jinja2 environment for tests."""
        self.env = Environment(loader=BaseLoader())

    def test_static_nested_dict(self):
        """Static nested dicts preserved."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "metadata": {
                    "version": "1.0",
                    "author": "test",
                }
            }
        )
        state = {}
        output = transform_output(state, schema, self.env)

        assert output["metadata"] == {"version": "1.0", "author": "test"}

    def test_template_in_nested_dict(self):
        """Templates in nested dicts are rendered."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "result": {
                    "answer": "{{ state.answer }}",
                    "timestamp": "{{ state.ts }}",
                }
            }
        )
        state = {"answer": "42", "ts": "2024-01-01"}
        output = transform_output(state, schema, self.env)

        # Note: numeric strings are converted to numbers
        assert output["result"]["answer"] == 42
        assert output["result"]["timestamp"] == "2024-01-01"

    def test_deeply_nested(self):
        """Deeply nested structures work."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "data": {
                    "level1": {
                        "level2": {
                            "value": "{{ state.deep }}",
                        }
                    }
                }
            }
        )
        state = {"deep": "found"}
        output = transform_output(state, schema, self.env)

        assert output["data"]["level1"]["level2"]["value"] == "found"

    def test_list_in_output(self):
        """Lists in schema are processed."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "items": ["{{ state.a }}", "{{ state.b }}", "static"],
            }
        )
        state = {"a": "first", "b": "second"}
        output = transform_output(state, schema, self.env)

        assert output["items"] == ["first", "second", "static"]


class TestDefaults:
    """Test default value handling (AC7)."""

    def setup_method(self):
        """Create Jinja2 environment for tests."""
        self.env = Environment(loader=BaseLoader())

    def test_default_when_value_none(self):
        """Default used when template evaluates to None."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "result": {
                    "value": "{{ state.missing }}",
                    "default": "N/A",
                }
            }
        )
        state = {}
        output = transform_output(state, schema, self.env)

        assert output["result"] == "N/A"

    def test_default_not_used_when_value_present(self):
        """Default not used when value exists."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "result": {
                    "value": "{{ state.data }}",
                    "default": "N/A",
                }
            }
        )
        state = {"data": "real value"}
        output = transform_output(state, schema, self.env)

        assert output["result"] == "real value"

    def test_default_with_numeric_zero(self):
        """Zero as default works correctly."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "count": {
                    "value": "{{ state.cnt }}",
                    "default": 0,
                }
            }
        )
        state = {}
        output = transform_output(state, schema, self.env)

        assert output["count"] == 0

    def test_default_with_complex_value(self):
        """Complex default values work."""
        from the_edge_agent.transformation import OutputSchema, transform_output

        schema = OutputSchema.from_yaml(
            {
                "data": {
                    "value": "{{ state.missing }}",
                    "default": {"status": "unknown"},
                }
            }
        )
        state = {}
        output = transform_output(state, schema, self.env)

        assert output["data"] == {"status": "unknown"}


class TestHTTPResponseAction:
    """Test http.respond action (AC8)."""

    def test_http_response_exception_structure(self):
        """HTTPResponse exception has correct structure."""
        from the_edge_agent.actions.http_response_actions import HTTPResponse

        response = HTTPResponse(
            status=401,
            body={"error": "unauthorized"},
            headers={"WWW-Authenticate": "Bearer"},
        )

        assert response.status == 401
        assert response.body == {"error": "unauthorized"}
        assert response.headers["WWW-Authenticate"] == "Bearer"
        assert response.headers["Content-Type"] == "application/json"

    def test_http_response_to_dict(self):
        """HTTPResponse.to_dict() returns complete structure."""
        from the_edge_agent.actions.http_response_actions import HTTPResponse

        response = HTTPResponse(
            status=200,
            body={"success": True},
        )
        d = response.to_dict()

        assert d["status"] == 200
        assert d["body"] == {"success": True}
        assert "headers" in d

    def test_http_respond_raises_exception(self):
        """http.respond action raises HTTPResponse exception."""
        from the_edge_agent.actions.http_response_actions import (
            http_respond_sync,
            HTTPResponse,
        )

        with pytest.raises(HTTPResponse) as exc_info:
            http_respond_sync(status=403, body={"error": "forbidden"})

        assert exc_info.value.status == 403
        assert exc_info.value.body == {"error": "forbidden"}


class TestStateGraphHTTPResponseHandling:
    """Test StateGraph handles HTTPResponse exceptions (AC8)."""

    def test_graph_yields_http_response_event(self):
        """Graph catches HTTPResponse and yields http_response event."""
        from the_edge_agent import StateGraph
        from the_edge_agent.actions.http_response_actions import HTTPResponse

        def raise_http():
            raise HTTPResponse(status=401, body={"error": "unauthorized"})

        graph = StateGraph({"x": int})
        graph.add_node("check_auth", run=lambda state: raise_http())
        graph.set_entry_point("check_auth")
        graph.set_finish_point("check_auth")

        events = list(graph.compile().invoke({"x": 1}))

        assert len(events) == 1
        event = events[0]
        assert event["type"] == "http_response"
        assert event["status"] == 401
        assert event["body"] == {"error": "unauthorized"}
        assert event["node"] == "check_auth"

    def test_graph_stops_after_http_response(self):
        """Graph execution stops after HTTPResponse."""
        from the_edge_agent import StateGraph
        from the_edge_agent.actions.http_response_actions import HTTPResponse

        executed = []

        def node1(state):
            executed.append("node1")
            raise HTTPResponse(status=200, body={"done": True})

        def node2(state):
            executed.append("node2")
            return {}

        graph = StateGraph({"x": int})
        graph.add_node("node1", run=node1)
        graph.add_node("node2", run=node2)
        graph.set_entry_point("node1")
        graph.add_edge("node1", "node2")
        graph.set_finish_point("node2")

        list(graph.compile().invoke({"x": 1}))

        assert executed == ["node1"]  # node2 never executed


class TestYAMLEngineOutputSchema:
    """Test YAML engine output_schema integration (AC9)."""

    def test_yaml_engine_parses_output_schema(self):
        """YAMLEngine parses output_schema from config."""
        from the_edge_agent import YAMLEngine

        config = """
name: test
state_schema:
  input: str
  result: str

output_schema:
  success: true
  answer: "{{ state.result }}"

nodes:
  - name: process
    run: |
      return {"result": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        import yaml

        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(config))

        # Check output_schema was attached
        assert hasattr(graph, "_output_schema")
        assert graph._output_schema is not None

    def test_yaml_engine_auto_applies_transformation(self):
        """YAMLEngine auto-applies output transformation at graph end."""
        from the_edge_agent import YAMLEngine
        import yaml

        config = """
name: test
state_schema:
  input: str
  result: str

output_schema:
  success: true
  answer: "{{ state.result }}"
  original: "{{ state.input }}"

nodes:
  - name: process
    run: |
      return {"result": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(config))

        events = list(graph.invoke({"input": "hello"}))
        final_event = events[-1]

        assert final_event["type"] == "final"
        assert final_event["output"] is not None
        assert final_event["output"]["success"] is True
        assert final_event["output"]["answer"] == "HELLO"
        assert final_event["output"]["original"] == "hello"

    def test_yaml_engine_conditional_fields(self):
        """Output schema conditional fields work in YAML."""
        from the_edge_agent import YAMLEngine
        import yaml

        config = """
name: test
state_schema:
  has_error: bool
  error_msg: str
  result: str

output_schema:
  success: "{{ not state.has_error }}"
  result: "{{ state.result }}"
  error:
    value: "{{ state.error_msg }}"
    include_if: "state.has_error"

nodes:
  - name: noop
    run: |
      return {}

edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(config))

        # Test without error
        events = list(
            graph.invoke(
                {
                    "has_error": False,
                    "error_msg": "",
                    "result": "ok",
                }
            )
        )
        output = events[-1]["output"]
        assert "error" not in output
        assert output["result"] == "ok"

        # Test with error
        events = list(
            graph.invoke(
                {
                    "has_error": True,
                    "error_msg": "something failed",
                    "result": "",
                }
            )
        )
        output = events[-1]["output"]
        assert "error" in output
        assert output["error"] == "something failed"

    def test_yaml_engine_default_values(self):
        """Output schema default values work in YAML."""
        from the_edge_agent import YAMLEngine
        import yaml

        config = """
name: test
state_schema:
  maybe_result: str

output_schema:
  answer:
    value: "{{ state.maybe_result }}"
    default: "no answer"

nodes:
  - name: noop
    run: |
      return {}

edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(config))

        # Test with value
        events = list(graph.invoke({"maybe_result": "got it"}))
        assert events[-1]["output"]["answer"] == "got it"

        # Test without value (should use default)
        # Note: need to test with missing/None value
        engine2 = YAMLEngine()
        graph2 = engine2.load_from_dict(yaml.safe_load(config))
        events2 = list(graph2.invoke({}))
        assert events2[-1]["output"]["answer"] == "no answer"


class TestHTTPRespondInYAML:
    """Test http.respond action in YAML workflows."""

    def test_http_respond_action_in_yaml(self):
        """http.respond action works in YAML workflow."""
        from the_edge_agent import YAMLEngine
        import yaml

        config = """
name: test
state_schema:
  authorized: bool

nodes:
  - name: check_auth
    run: |
      return {"checked": True}
    goto:
      - to: unauthorized
        if: "not state.get('authorized', False)"
      - to: success

  - name: unauthorized
    uses: http.respond
    with:
      status: 401
      body:
        error: "unauthorized"
        message: "You must be logged in"

  - name: success
    run: |
      return {"result": "ok"}
    goto: __end__

edges:
  - from: __start__
    to: check_auth
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(config))

        # Test unauthorized path
        events = list(graph.invoke({"authorized": False}))
        event = events[-1]
        assert event["type"] == "http_response"
        assert event["status"] == 401
        assert event["body"]["error"] == "unauthorized"


class TestActionRegistration:
    """Test action registration."""

    def test_http_respond_registered(self):
        """http.respond action is registered in actions registry."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        assert "http.respond" in engine.actions_registry


class TestOutputSchemaField:
    """Test OutputSchemaField model."""

    def test_field_with_value_only(self):
        """Field can have just value."""
        from the_edge_agent.transformation import OutputSchemaField

        field = OutputSchemaField(value="{{ state.x }}")
        assert field.value == "{{ state.x }}"
        assert field.include_if is None
        assert field.default is None

    def test_field_with_all_properties(self):
        """Field can have all properties."""
        from the_edge_agent.transformation import OutputSchemaField

        field = OutputSchemaField(
            value="{{ state.x }}",
            include_if="state.show",
            default="N/A",
        )
        assert field.value == "{{ state.x }}"
        assert field.include_if == "state.show"
        assert field.default == "N/A"

    def test_field_rejects_extra_properties(self):
        """Field rejects unknown properties."""
        from the_edge_agent.transformation import OutputSchemaField
        from pydantic import ValidationError

        with pytest.raises(ValidationError):
            OutputSchemaField(value="{{ x }}", unknown="foo")
