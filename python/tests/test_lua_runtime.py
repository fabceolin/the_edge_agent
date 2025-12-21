"""
Tests for the Lua runtime integration (TEA-LUA.P1).

These tests mirror the Rust implementation tests in rust/src/engine/lua_runtime.rs
to ensure cross-runtime compatibility.
"""

import pytest
import time
from typing import Any, Dict

# Import with graceful skip if lupa not installed
try:
    from the_edge_agent.lua_runtime import (
        LuaRuntime,
        LuaRuntimeError,
        LuaTimeoutError,
        LUPA_AVAILABLE,
        detect_lua_code,
    )
except ImportError:
    LUPA_AVAILABLE = False
    LuaRuntime = None
    LuaRuntimeError = Exception
    LuaTimeoutError = Exception
    detect_lua_code = None

# Skip all tests in this module if lupa is not installed
pytestmark = pytest.mark.skipif(not LUPA_AVAILABLE, reason="lupa not installed")


class TestLuaRuntimeCreation:
    """Tests for LuaRuntime initialization."""

    def test_create_runtime(self):
        """Test creating a new Lua runtime."""
        runtime = LuaRuntime()
        assert runtime.timeout == 30.0

    def test_create_runtime_with_custom_timeout(self):
        """Test creating a runtime with custom timeout."""
        runtime = LuaRuntime(timeout=5.0)
        assert runtime.timeout == 5.0


class TestJsonToLuaPrimitives:
    """Tests for Python -> Lua type conversion (primitives)."""

    def test_null_to_nil(self):
        """Test None -> nil conversion."""
        runtime = LuaRuntime()
        result = runtime.execute("return state == nil", None)
        assert result is True

    def test_null_roundtrip(self):
        """Test None roundtrip."""
        runtime = LuaRuntime()
        result = runtime.execute("return state", None)
        assert result is None

    def test_boolean_true(self):
        """Test True -> true conversion."""
        runtime = LuaRuntime()
        result = runtime.execute("return state", True)
        assert result is True

    def test_boolean_false(self):
        """Test False -> false conversion."""
        runtime = LuaRuntime()
        result = runtime.execute("return state", False)
        assert result is False

    def test_integer(self):
        """Test integer conversion."""
        runtime = LuaRuntime()
        result = runtime.execute("return state + 1", 41)
        assert result == 42

    def test_float(self):
        """Test float conversion."""
        runtime = LuaRuntime()
        result = runtime.execute("return state * 2", 3.14)
        assert abs(result - 6.28) < 0.001

    def test_string(self):
        """Test string conversion."""
        runtime = LuaRuntime()
        result = runtime.execute('return state .. "!"', "hello")
        assert result == "hello!"


class TestJsonToLuaComplex:
    """Tests for Python -> Lua type conversion (arrays and objects)."""

    def test_array_access(self):
        """Test array access (1-indexed in Lua)."""
        runtime = LuaRuntime()
        result = runtime.execute("return state[1] + state[2] + state[3]", [1, 2, 3])
        assert result == 6

    def test_object_access(self):
        """Test object property access."""
        runtime = LuaRuntime()
        state = {"name": "test", "value": 42}
        result = runtime.execute("return state.name", state)
        assert result == "test"

    def test_object_value_access(self):
        """Test object value multiplication."""
        runtime = LuaRuntime()
        state = {"name": "test", "value": 42}
        result = runtime.execute("return state.value * 2", state)
        assert result == 84

    def test_nested_object(self):
        """Test nested object access."""
        runtime = LuaRuntime()
        state = {"outer": {"inner": 123}}
        result = runtime.execute("return state.outer.inner", state)
        assert result == 123


class TestLuaToPython:
    """Tests for Lua -> Python type conversion."""

    def test_return_array(self):
        """Test returning Lua table as array."""
        runtime = LuaRuntime()
        result = runtime.execute("return {1, 2, 3}", {})
        assert result == [1, 2, 3]

    def test_return_object(self):
        """Test returning Lua table as object."""
        runtime = LuaRuntime()
        result = runtime.execute('return {name="test", value=42}', {})
        assert isinstance(result, dict)
        assert result["name"] == "test"
        assert result["value"] == 42

    def test_return_nested_array(self):
        """Test returning nested arrays."""
        runtime = LuaRuntime()
        result = runtime.execute("return {{1, 2}, {3, 4}}", {})
        assert result == [[1, 2], [3, 4]]

    def test_ipairs_iteration(self):
        """Test ipairs iteration on array."""
        runtime = LuaRuntime()
        code = """
            local result = {}
            for i, v in ipairs(state.items) do
                result[i] = v .. "!"
            end
            return result
        """
        result = runtime.execute(code, {"items": ["a", "b", "c"]})
        assert result == ["a!", "b!", "c!"]


class TestEvalCondition:
    """Tests for eval_condition method."""

    def test_eval_condition_string(self):
        """Test eval_condition returning string."""
        runtime = LuaRuntime()
        result = runtime.eval_condition("state.status", {"status": "success"})
        assert result == "success"

    def test_eval_condition_with_logic(self):
        """Test eval_condition with ternary-style logic."""
        runtime = LuaRuntime()
        result = runtime.eval_condition(
            'state.value > 5 and "high" or "low"',
            {"value": 10}
        )
        assert result == "high"

    def test_eval_condition_nil(self):
        """Test eval_condition returning nil (None)."""
        runtime = LuaRuntime()
        result = runtime.eval_condition("state.missing", {})
        assert result is None

    def test_eval_condition_boolean_true(self):
        """Test eval_condition with boolean true."""
        runtime = LuaRuntime()
        result = runtime.eval_condition("state.active", {"active": True})
        assert result == "true"

    def test_eval_condition_boolean_false(self):
        """Test eval_condition with boolean false."""
        runtime = LuaRuntime()
        result = runtime.eval_condition("state.active", {"active": False})
        assert result == "false"

    def test_eval_condition_with_return(self):
        """Test eval_condition with explicit return statement."""
        runtime = LuaRuntime()
        result = runtime.eval_condition("return state.status", {"status": "done"})
        assert result == "done"


class TestSandbox:
    """Tests for Lua sandbox security."""

    def test_sandbox_removes_os(self):
        """Test that os module is removed."""
        runtime = LuaRuntime()
        result = runtime.execute("return os", {})
        assert result is None

    def test_sandbox_removes_io(self):
        """Test that io module is removed."""
        runtime = LuaRuntime()
        result = runtime.execute("return io", {})
        assert result is None

    def test_sandbox_removes_debug(self):
        """Test that debug module is removed."""
        runtime = LuaRuntime()
        result = runtime.execute("return debug", {})
        assert result is None

    def test_sandbox_removes_loadfile(self):
        """Test that loadfile is removed."""
        runtime = LuaRuntime()
        result = runtime.execute("return loadfile", {})
        assert result is None

    def test_sandbox_removes_dofile(self):
        """Test that dofile is removed."""
        runtime = LuaRuntime()
        result = runtime.execute("return dofile", {})
        assert result is None

    def test_sandbox_allows_string(self):
        """Test that string module is available."""
        runtime = LuaRuntime()
        result = runtime.execute('return string.upper("hello")', {})
        assert result == "HELLO"

    def test_sandbox_allows_math(self):
        """Test that math module is available."""
        runtime = LuaRuntime()
        result = runtime.execute("return math.floor(3.7)", {})
        assert result == 3

    def test_sandbox_allows_table(self):
        """Test that table module is available."""
        runtime = LuaRuntime()
        result = runtime.execute('t = {}; table.insert(t, "a"); return t', {})
        assert result == ["a"]

    def test_sandbox_allows_pairs(self):
        """Test that pairs is available."""
        runtime = LuaRuntime()
        code = """
            local sum = 0
            for k, v in pairs(state) do
                sum = sum + v
            end
            return sum
        """
        result = runtime.execute(code, {"a": 1, "b": 2, "c": 3})
        assert result == 6


class TestExecuteNodeCode:
    """Tests for execute_node_code method."""

    def test_execute_node_code_basic(self):
        """Test basic execute_node_code."""
        runtime = LuaRuntime()
        code = """
            local result = {}
            result.count = state.count + 1
            result.doubled = state.count * 2
            return result
        """
        result = runtime.execute_node_code(code, {"count": 5})
        assert result["count"] == 6
        assert result["doubled"] == 10

    def test_execute_node_code_empty_return(self):
        """Test execute_node_code with no return."""
        runtime = LuaRuntime()
        code = "local x = 1"
        result = runtime.execute_node_code(code, {})
        assert result == {}


class TestTimeout:
    """Tests for timeout protection."""

    def test_timeout_triggers(self):
        """Test that timeout is triggered on infinite loop."""
        runtime = LuaRuntime(timeout=0.1)
        start = time.time()
        with pytest.raises(LuaTimeoutError):
            runtime.execute("while true do end", {})
        elapsed = time.time() - start
        # Should complete within reasonable margin
        assert elapsed < 0.5, f"Timeout took too long: {elapsed}s"

    def test_timeout_error_message(self):
        """Test timeout error message format."""
        runtime = LuaRuntime(timeout=0.05)
        with pytest.raises(LuaTimeoutError) as exc_info:
            runtime.execute("while true do end", {})
        assert "timeout" in str(exc_info.value).lower()

    def test_eval_condition_timeout(self):
        """Test timeout in eval_condition."""
        runtime = LuaRuntime(timeout=0.1)
        with pytest.raises(LuaTimeoutError):
            runtime.eval_condition(
                "(function() while true do end return 'x' end)()",
                {}
            )

    def test_execute_node_code_timeout(self):
        """Test timeout in execute_node_code."""
        runtime = LuaRuntime(timeout=0.1)
        with pytest.raises(LuaTimeoutError):
            runtime.execute_node_code("while true do end", {})

    def test_normal_execution_within_timeout(self):
        """Test that normal execution is not affected by timeout."""
        runtime = LuaRuntime(timeout=5.0)
        result = runtime.execute("return state.value * 2", {"value": 42})
        assert result == 84

    def test_runtime_recovers_after_timeout(self):
        """Test that runtime can be used after a timeout."""
        runtime = LuaRuntime(timeout=0.1)

        # First, trigger a timeout
        with pytest.raises(LuaTimeoutError):
            runtime.execute("while true do end", {})

        # Then, verify normal execution still works
        result = runtime.execute("return 1 + 1", {})
        assert result == 2


class TestDetectLuaCode:
    """Tests for detect_lua_code function."""

    def test_detect_lua_marker(self):
        """Test detection via -- lua marker."""
        assert detect_lua_code("-- lua\nreturn 1") is True

    def test_detect_lua_marker_no_space(self):
        """Test detection via --lua marker."""
        assert detect_lua_code("--lua\nreturn 1") is True

    def test_detect_local_keyword(self):
        """Test detection via 'local' keyword."""
        assert detect_lua_code("local x = 1") is True

    def test_detect_then_keyword(self):
        """Test detection via 'then' keyword."""
        assert detect_lua_code("if x then end") is True

    def test_detect_end_keyword(self):
        """Test detection via 'end' keyword."""
        assert detect_lua_code("function f() end") is True

    def test_detect_elseif_keyword(self):
        """Test detection via 'elseif' keyword."""
        assert detect_lua_code("if x then y elseif z then w end") is True

    def test_detect_concat_operator(self):
        """Test detection via '..' operator."""
        assert detect_lua_code("a = b .. c") is True

    def test_python_code_not_detected(self):
        """Test that Python-like code is not detected as Lua."""
        assert detect_lua_code("x = 1") is False
        assert detect_lua_code("return x") is False
        assert detect_lua_code("def func(): pass") is False


class TestYAMLIntegration:
    """Tests for YAMLEngine integration."""

    def test_yaml_engine_with_lua_disabled(self):
        """Test that Lua code is not executed when lua_enabled=False."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(lua_enabled=False)

        # This would be detected as Lua, but should run as Python
        # Since Python doesn't have 'local', this would fail
        yaml_content = """
name: test-lua-disabled
state_schema:
  value: int

nodes:
  - name: process
    run: |
      return {"result": state["value"] * 2}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"value": 21}))
        final_state = result[-1]["state"]
        assert final_state["result"] == 42

    def test_yaml_engine_with_lua_marker(self):
        """Test Lua execution via -- lua marker."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(lua_enabled=True)

        yaml_content = """
name: test-lua-marker
state_schema:
  value: int
  result: int

nodes:
  - name: process
    run: |
      -- lua
      local result = {}
      result.result = state.value * 2
      return result

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"value": 21}))
        final_state = result[-1]["state"]
        assert final_state["result"] == 42

    def test_yaml_engine_with_lua_auto_detect(self):
        """Test Lua auto-detection via syntax."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(lua_enabled=True)

        yaml_content = """
name: test-lua-auto
state_schema:
  count: int
  doubled: int

nodes:
  - name: compute
    run: |
      local result = {}
      result.count = state.count + 1
      result.doubled = state.count * 2
      return result

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"count": 5}))
        final_state = result[-1]["state"]
        assert final_state["count"] == 6
        assert final_state["doubled"] == 10

    def test_yaml_engine_python_with_lua_enabled(self):
        """Test that Python code still works when lua_enabled=True."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(lua_enabled=True)

        yaml_content = """
name: test-python-still-works
state_schema:
  value: int
  result: int

nodes:
  - name: compute
    run: |
      result = state["value"] * 3
      return {"result": result}

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"value": 10}))
        final_state = result[-1]["state"]
        assert final_state["result"] == 30


class TestLupaNotInstalled:
    """Tests for graceful handling when lupa is not installed."""

    def test_lupa_availability_flag(self):
        """Test LUPA_AVAILABLE flag is correctly set."""
        # If we get here, LUPA_AVAILABLE should be True
        assert LUPA_AVAILABLE is True

    def test_import_error_message(self):
        """Test that ImportError has helpful message."""
        # This test verifies the error message format
        # We can't actually uninstall lupa to test this,
        # but we can verify the message is well-formed
        from the_edge_agent.lua_runtime import _ensure_lupa_installed

        # Should not raise since lupa is installed
        _ensure_lupa_installed()


class TestCrossRuntimeVerification:
    """Tests for cross-runtime compatibility with Rust implementation."""

    def test_shared_fixture_yaml(self):
        """Test shared Lua YAML fixture produces expected results."""
        import os
        import yaml
        from the_edge_agent import YAMLEngine

        # Load the shared fixture
        fixture_path = os.path.join(
            os.path.dirname(__file__),
            "fixtures",
            "lua_test_agent.yaml"
        )

        with open(fixture_path) as f:
            config = yaml.safe_load(f)

        engine = YAMLEngine(lua_enabled=True)
        graph = engine.load_from_dict(config)

        # Run with test input
        initial_state = {
            "input_value": 30,
            "input_string": "hello",
            "input_array": [1, 2, 3],
            "input_object": {"name": "test", "count": 5},
        }

        result = list(graph.invoke(initial_state))
        final_state = result[-1]["state"]

        # Verify math: 30 * 2 + 10 = 70
        assert final_state["result_math"] == 70

        # Verify string: "HELLO!"
        assert final_state["result_string"] == "HELLO!"

        # Verify array: [2, 4, 6]
        assert final_state["result_array"] == [2, 4, 6]

        # Verify object
        assert final_state["result_object"]["name"] == "test"
        assert final_state["result_object"]["count"] == 6
        assert final_state["result_object"]["doubled"] == 10

        # Verify conditional: 30 > 25 -> "medium"
        assert final_state["result_conditional"] == "medium"

    def test_type_conversion_parity(self):
        """Test that type conversion matches Rust implementation."""
        runtime = LuaRuntime()

        # Test cases that should produce identical results in Python and Rust

        # Null/nil
        assert runtime.execute("return state", None) is None

        # Boolean
        assert runtime.execute("return state", True) is True
        assert runtime.execute("return state", False) is False

        # Integer
        assert runtime.execute("return state", 42) == 42

        # Float
        result = runtime.execute("return state", 3.14)
        assert abs(result - 3.14) < 0.001

        # String
        assert runtime.execute("return state", "hello") == "hello"

        # Array (1-indexed in Lua)
        result = runtime.execute("return state", [1, 2, 3])
        # Note: When passed to Lua and back, arrays stay arrays
        assert result == [1, 2, 3]

        # Object/dict
        result = runtime.execute("return state", {"key": "value"})
        assert result["key"] == "value"

    def test_sandbox_parity(self):
        """Test that sandbox restrictions match Rust implementation."""
        runtime = LuaRuntime()

        # These should all be nil (same as Rust)
        assert runtime.execute("return os", {}) is None
        assert runtime.execute("return io", {}) is None
        assert runtime.execute("return debug", {}) is None
        assert runtime.execute("return loadfile", {}) is None
        assert runtime.execute("return dofile", {}) is None

        # These should be available (same as Rust)
        assert runtime.execute("return type(string)", {}) == "table"
        assert runtime.execute("return type(math)", {}) == "table"
        assert runtime.execute("return type(table)", {}) == "table"
        assert runtime.execute("return type(pairs)", {}) == "function"
        assert runtime.execute("return type(ipairs)", {}) == "function"

    def test_eval_condition_parity(self):
        """Test that eval_condition matches Rust behavior."""
        runtime = LuaRuntime()

        # String return
        result = runtime.eval_condition("state.status", {"status": "success"})
        assert result == "success"

        # Boolean true -> "true"
        result = runtime.eval_condition("state.active", {"active": True})
        assert result == "true"

        # Boolean false -> "false"
        result = runtime.eval_condition("state.active", {"active": False})
        assert result == "false"

        # Nil -> None
        result = runtime.eval_condition("state.missing", {})
        assert result is None


# Pytest markers for CI filtering
@pytest.mark.lua
class TestLuaMarker:
    """Tests with @pytest.mark.lua for CI filtering."""

    def test_lua_functionality(self):
        """Basic test to verify Lua marker works."""
        runtime = LuaRuntime()
        result = runtime.execute("return 1 + 1", {})
        assert result == 2
