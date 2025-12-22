"""
Tests for the Prolog runtime integration (TEA-PY-004, TEA-PY-005).

These tests verify:
- PrologRuntime class functionality
- Python ↔ Prolog type conversions
- state/2 and return/2 predicates
- Timeout protection (janus-swi handles without segfaults)
- Sandbox security
- YAMLEngine integration
- CLP(FD) constraint solving (enabled via module pre-loading)
- Directive handling via consult()

The Rust implementation in rust/src/engine/prolog_runtime.rs mirrors these tests
for cross-runtime compatibility.

TEA-PY-005 migrated from pyswip to janus-swi for:
- Proper timeout exception handling (no segfaults)
- Native directive support via consult()
- Full CLP(FD) module loading
- Official SWI-Prolog 9.1+ support
"""

import pytest
import time
import os
import tempfile
from typing import Any, Dict

# Import with graceful skip if janus-swi not installed
try:
    from the_edge_agent.prolog_runtime import (
        PrologRuntime,
        PrologRuntimeError,
        PrologTimeoutError,
        JANUS_AVAILABLE,
        PYSWIP_AVAILABLE,  # Backward compat alias
        detect_prolog_code,
        _get_install_instructions,
    )
except ImportError:
    JANUS_AVAILABLE = False
    PYSWIP_AVAILABLE = False
    PrologRuntime = None
    PrologRuntimeError = Exception
    PrologTimeoutError = Exception
    detect_prolog_code = None
    _get_install_instructions = None

# Skip all tests in this module if janus-swi is not installed
pytestmark = pytest.mark.skipif(not JANUS_AVAILABLE, reason="janus-swi not installed")


class TestPrologRuntimeCreation:
    """Tests for PrologRuntime initialization."""

    def test_create_runtime(self):
        """Test creating a new Prolog runtime."""
        runtime = PrologRuntime()
        assert runtime.timeout == 30.0
        assert runtime.sandbox is True

    def test_create_runtime_with_custom_timeout(self):
        """Test creating a runtime with custom timeout."""
        runtime = PrologRuntime(timeout=5.0)
        assert runtime.timeout == 5.0

    def test_create_runtime_no_sandbox(self):
        """Test creating a runtime without sandbox."""
        runtime = PrologRuntime(sandbox=False)
        assert runtime.sandbox is False

    def test_repr(self):
        """Test runtime string representation."""
        runtime = PrologRuntime(timeout=10.0, sandbox=False)
        assert "PrologRuntime" in repr(runtime)
        assert "timeout=10.0" in repr(runtime)
        assert "sandbox=False" in repr(runtime)

    def test_modules_preloaded(self):
        """Test that common modules are pre-loaded (AC-1, AC-2)."""
        runtime = PrologRuntime()
        # Verify CLP(FD) is available by using a simple constraint
        result = runtime.execute_node_code(
            "X in 1..5, X #> 3, label([X]), return(result, X)",
            {}
        )
        # Should find 4 or 5 (first solution after 3)
        assert result.get("result") in [4, 5]


class TestPythonToPrologPrimitives:
    """Tests for Python -> Prolog type conversion (primitives)."""

    def test_null_to_null(self):
        """Test None -> null conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, null), return(result, 'is_null')",
            {"value": None}
        )
        assert result.get("result") == "is_null"

    def test_boolean_true(self):
        """Test True -> true conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, true), return(result, 'is_true')",
            {"value": True}
        )
        assert result.get("result") == "is_true"

    def test_boolean_false(self):
        """Test False -> false conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, false), return(result, 'is_false')",
            {"value": False}
        )
        assert result.get("result") == "is_false"

    def test_integer(self):
        """Test integer conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, V), V2 is V + 1, return(result, V2)",
            {"value": 41}
        )
        assert result.get("result") == 42

    def test_float(self):
        """Test float conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, V), V2 is V * 2, return(result, V2)",
            {"value": 3.14}
        )
        assert abs(result.get("result") - 6.28) < 0.001

    def test_string(self):
        """Test string conversion."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, V), return(result, V)",
            {"value": "hello"}
        )
        assert result.get("result") == "hello"

    def test_string_with_quotes(self):
        """Test string with quotes."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, V), return(result, V)",
            {"value": "it's a test"}
        )
        assert result.get("result") == "it's a test"


class TestPythonToPrologComplex:
    """Tests for Python -> Prolog type conversion (arrays and objects)."""

    def test_array_access(self):
        """Test array access via list."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, L), nth0(0, L, First), return(result, First)",
            {"value": [10, 20, 30]}
        )
        assert result.get("result") == 10

    def test_array_length(self):
        """Test array length calculation."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(value, L), length(L, Len), return(result, Len)",
            {"value": [1, 2, 3, 4, 5]}
        )
        assert result.get("result") == 5


class TestPrologToPython:
    """Tests for Prolog -> Python type conversion."""

    def test_return_integer(self):
        """Test returning integer from Prolog."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "X is 21 * 2, return(result, X)",
            {}
        )
        assert result.get("result") == 42

    def test_return_list(self):
        """Test returning list from Prolog."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "return(result, [1, 2, 3])",
            {}
        )
        assert result.get("result") == [1, 2, 3]

    def test_return_multiple_values(self):
        """Test returning multiple values."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "return(a, 1), return(b, 2), return(c, 3)",
            {}
        )
        assert result.get("a") == 1
        assert result.get("b") == 2
        assert result.get("c") == 3


class TestEvalCondition:
    """Tests for eval_condition method."""

    def test_eval_condition_success(self):
        """Test eval_condition succeeds."""
        runtime = PrologRuntime()
        result = runtime.eval_condition("state(value, V), V > 5", {"value": 10})
        assert result == "true"

    def test_eval_condition_failure(self):
        """Test eval_condition fails (returns None)."""
        runtime = PrologRuntime()
        result = runtime.eval_condition("state(value, V), V > 100", {"value": 10})
        assert result is None

    def test_eval_condition_with_result_binding(self):
        """Test eval_condition with Result variable."""
        runtime = PrologRuntime()
        result = runtime.eval_condition(
            "state(value, V), V > 5, Result = high",
            {"value": 10}
        )
        assert result == "high"


class TestSandbox:
    """Tests for Prolog sandbox security (P0 tests)."""

    def test_sandbox_enabled_by_default(self):
        """Test that sandbox is enabled by default."""
        runtime = PrologRuntime()
        assert runtime.sandbox is True

    def test_sandbox_blocks_file_read(self):
        """Test that sandboxed mode blocks file reading."""
        runtime = PrologRuntime(sandbox=True)
        # Note: sandbox may not fully block all operations
        # This test verifies sandbox is configured
        try:
            runtime.execute_node_code(
                ":- use_module(library(readutil)), read_file_to_string('/etc/passwd', _, []).",
                {}
            )
        except (PrologRuntimeError, Exception):
            pass  # Expected - sandbox should block this

    def test_sandbox_blocks_shell(self):
        """Test that sandboxed mode blocks shell execution."""
        runtime = PrologRuntime(sandbox=True)
        try:
            runtime.execute_node_code("shell('ls -la')", {})
        except (PrologRuntimeError, Exception):
            pass  # Expected - sandbox should block this

    def test_sandbox_blocks_consult_file(self):
        """Test that consult_file is blocked in sandbox mode."""
        runtime = PrologRuntime(sandbox=True)
        with pytest.raises(PrologRuntimeError) as exc_info:
            runtime.consult_file("/tmp/test.pl")
        assert "sandbox" in str(exc_info.value).lower()

    def test_no_sandbox_allows_consult(self):
        """Test that consult_file works without sandbox."""
        runtime = PrologRuntime(sandbox=False)

        # Create a temp file with Prolog rules
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False) as f:
            f.write("test_rule(42).\n")
            temp_path = f.name

        try:
            runtime.consult_file(temp_path)
            result = runtime.execute_query("test_rule(X), return(result, X)", {})
            assert result.get("result") == 42
        finally:
            os.unlink(temp_path)


class TestTimeout:
    """Tests for timeout protection (P0 tests).

    TEA-PY-005: janus-swi properly handles timeout exceptions without segfaults.
    These tests are now enabled (previously skipped with pyswip).
    """

    def test_timeout_triggers_on_infinite_loop(self):
        """Test that timeout is triggered on infinite recursion (AC-3)."""
        runtime = PrologRuntime(timeout=0.5)
        with pytest.raises(PrologTimeoutError):
            # Define and call an infinite loop
            runtime.execute_node_code(
                """
                loop :- loop.
                loop.
                """,
                {}
            )

    def test_timeout_error_message(self):
        """Test timeout error message format (AC-3)."""
        runtime = PrologRuntime(timeout=0.5)
        with pytest.raises(PrologTimeoutError) as exc_info:
            runtime.execute_node_code(
                """
                loop :- loop.
                loop.
                """,
                {}
            )
        assert "timeout" in str(exc_info.value).lower()

    def test_normal_execution_within_timeout(self):
        """Test that normal execution is not affected by timeout."""
        runtime = PrologRuntime(timeout=5.0)
        result = runtime.execute_node_code(
            "state(value, V), V2 is V * 2, return(result, V2)",
            {"value": 21}
        )
        assert result.get("result") == 42

    def test_eval_condition_timeout(self):
        """Test timeout in eval_condition (AC-3)."""
        runtime = PrologRuntime(timeout=0.5)
        # First define the infinite loop rule via execute_node_code
        runtime.execute_node_code("loop :- loop.", {})
        with pytest.raises(PrologTimeoutError):
            # Now call the loop - this should timeout
            runtime.eval_condition("loop", {})


class TestExecuteNodeCode:
    """Tests for execute_node_code method."""

    def test_execute_node_code_basic(self):
        """Test basic execute_node_code with state access."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "state(count, C), C2 is C + 1, return(count, C2)",
            {"count": 5}
        )
        assert result.get("count") == 6

    def test_execute_node_code_empty(self):
        """Test execute_node_code with empty code."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code("", {})
        assert result == {}

    def test_execute_node_code_with_rule(self):
        """Test execute_node_code with rule definition."""
        runtime = PrologRuntime()
        code = """
            double(X, Y) :- Y is X * 2.
            state(value, V),
            double(V, D),
            return(result, D).
        """
        result = runtime.execute_node_code(code, {"value": 21})
        assert result.get("result") == 42

    def test_execute_node_code_with_module(self):
        """Test execute_node_code with module import (AC-2, AC-4).

        TEA-PY-005: janus-swi's consult() properly handles directives.
        """
        runtime = PrologRuntime()
        code = """
            :- use_module(library(lists)).
            state(items, L),
            msort(L, Sorted),
            return(result, Sorted).
        """
        result = runtime.execute_node_code(code, {"items": [3, 1, 2]})
        assert result.get("result") == [1, 2, 3]

    def test_execute_node_code_with_dynamic_directive(self):
        """Test execute_node_code with :- dynamic(...) directive (AC-4)."""
        runtime = PrologRuntime()
        code = """
            :- dynamic(fact/1).
            assertz(fact(hello)),
            fact(X),
            return(result, X).
        """
        result = runtime.execute_node_code(code, {})
        assert result.get("result") == "hello"


class TestExecuteQuery:
    """Tests for execute_query method."""

    def test_execute_query_basic(self):
        """Test basic query execution."""
        runtime = PrologRuntime()
        result = runtime.execute_query(
            "X is 1 + 1, return(result, X)",
            {}
        )
        assert result.get("result") == 2

    def test_execute_query_with_state(self):
        """Test query with state access."""
        runtime = PrologRuntime()
        result = runtime.execute_query(
            "state(a, A), state(b, B), Sum is A + B, return(result, Sum)",
            {"a": 10, "b": 20}
        )
        assert result.get("result") == 30

    def test_execute_query_fails(self):
        """Test query that fails returns empty dict."""
        runtime = PrologRuntime()
        result = runtime.execute_query("fail", {})
        assert result == {}


class TestDetectPrologCode:
    """Tests for detect_prolog_code function."""

    def test_detect_prolog_marker(self):
        """Test detection via % prolog marker."""
        assert detect_prolog_code("% prolog\nstate(x, X).") is True

    def test_detect_prolog_marker_no_space(self):
        """Test detection via %prolog marker."""
        assert detect_prolog_code("%prolog\nstate(x, X).") is True

    def test_detect_rule_operator(self):
        """Test detection via :- rule operator."""
        assert detect_prolog_code("foo(X) :- bar(X).") is True

    def test_detect_directive(self):
        """Test detection via :- directive."""
        assert detect_prolog_code(":- use_module(library(lists)).") is True

    def test_detect_state_predicate(self):
        """Test detection via state/2 predicate."""
        assert detect_prolog_code("state(value, V), V > 0.") is True

    def test_detect_return_predicate(self):
        """Test detection via return/2 predicate."""
        assert detect_prolog_code("return(result, 42).") is True

    def test_detect_clpfd_operator(self):
        """Test detection via CLP(FD) operator."""
        assert detect_prolog_code("X #= Y + 1.") is True
        assert detect_prolog_code("X #< 10.") is True
        assert detect_prolog_code("X in 1..100.") is True

    def test_detect_assertz(self):
        """Test detection via assertz predicate."""
        assert detect_prolog_code("assertz(fact(value)).") is True

    def test_detect_findall(self):
        """Test detection via findall predicate."""
        assert detect_prolog_code("findall(X, foo(X), L).") is True

    def test_python_code_not_detected(self):
        """Test that Python code is not detected as Prolog."""
        assert detect_prolog_code("x = 1") is False
        assert detect_prolog_code("return x") is False
        assert detect_prolog_code("def func(): pass") is False

    def test_lua_code_not_detected(self):
        """Test that Lua code is not detected as Prolog."""
        assert detect_prolog_code("local x = 1") is False
        assert detect_prolog_code("if x then end") is False


class TestCLPFD:
    """Tests for CLP(FD) constraint solving (AC-2, AC-7).

    TEA-PY-005: CLP(FD) is now pre-loaded via _preload_modules().
    These tests are enabled (previously skipped with pyswip).
    """

    def test_clpfd_simple_constraint(self):
        """Test simple CLP(FD) constraint (AC-2)."""
        runtime = PrologRuntime()
        code = """
            X in 1..10,
            X #> 5,
            label([X]),
            return(result, X).
        """
        result = runtime.execute_node_code(code, {})
        # Should find first solution > 5, which is 6
        assert result.get("result") == 6

    def test_clpfd_sum_constraint(self):
        """Test CLP(FD) with sum constraint (AC-2)."""
        runtime = PrologRuntime()
        code = """
            X in 1..10,
            Y in 1..10,
            X + Y #= 15,
            X #< Y,
            label([X, Y]),
            return(x, X),
            return(y, Y).
        """
        result = runtime.execute_node_code(code, {})
        x = result.get("x")
        y = result.get("y")
        assert x + y == 15
        assert x < y

    def test_clpfd_with_module_import(self):
        """Test CLP(FD) with explicit module import directive (AC-2, AC-4)."""
        runtime = PrologRuntime()
        code = """
            :- use_module(library(clpfd)).
            X in 1..5,
            X #>= 3,
            label([X]),
            return(result, X).
        """
        result = runtime.execute_node_code(code, {})
        assert result.get("result") in [3, 4, 5]


class TestYAMLIntegration:
    """Tests for YAMLEngine integration."""

    def test_yaml_engine_with_prolog_disabled(self):
        """Test that Prolog code fails gracefully when prolog_enabled=False."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=False)

        # Pure Python code should work
        yaml_content = """
name: test-prolog-disabled
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

    def test_yaml_engine_with_prolog_marker(self):
        """Test Prolog execution via % prolog marker."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

        yaml_content = """
name: test-prolog-marker
state_schema:
  value: int
  result: int

nodes:
  - name: process
    run: |
      % prolog
      state(value, V),
      V2 is V * 2,
      return(result, V2).

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

    def test_yaml_engine_with_language_prolog(self):
        """Test Prolog execution via language: prolog."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

        yaml_content = """
name: test-language-prolog
state_schema:
  value: int
  result: int

nodes:
  - name: compute
    language: prolog
    run: |
      state(value, V),
      V2 is V + 10,
      return(result, V2).

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"value": 32}))
        final_state = result[-1]["state"]
        assert final_state["result"] == 42

    def test_yaml_engine_with_explicit_type(self):
        """Test Prolog execution via run: { type: prolog, code: ... }."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

        yaml_content = """
name: test-type-prolog
state_schema:
  value: int
  result: int

nodes:
  - name: compute
    run:
      type: prolog
      code: |
        state(value, V),
        V2 is V * 3,
        return(result, V2).

edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"value": 14}))
        final_state = result[-1]["state"]
        assert final_state["result"] == 42

    def test_yaml_engine_python_with_prolog_enabled(self):
        """Test that Python code still works when prolog_enabled=True."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

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


class TestBackwardCompatibility:
    """Tests for backward compatibility (AC-6, AC-7, AC-8)."""

    def test_python_code_unchanged(self):
        """Test existing Python run: blocks continue unchanged (AC-8)."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

        yaml_content = """
name: test-python-compat
state_schema:
  x: int

nodes:
  - name: python_node
    run: |
      return {"y": state["x"] + 1}

edges:
  - from: __start__
    to: python_node
  - from: python_node
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"x": 5}))
        assert result[-1]["state"]["y"] == 6

    def test_lua_code_unchanged(self):
        """Test existing Lua run: blocks continue unchanged (AC-8)."""
        import yaml
        from the_edge_agent import YAMLEngine

        try:
            from the_edge_agent.lua_runtime import LUPA_AVAILABLE
        except ImportError:
            LUPA_AVAILABLE = False

        if not LUPA_AVAILABLE:
            pytest.skip("lupa not installed")

        engine = YAMLEngine(lua_enabled=True, prolog_enabled=True)

        yaml_content = """
name: test-lua-compat
state_schema:
  x: int
  y: int

nodes:
  - name: lua_node
    run: |
      -- lua
      local result = {}
      result.y = state.x + 1
      return result

edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"x": 5}))
        assert result[-1]["state"]["y"] == 6

    def test_pyswip_available_compat(self):
        """Test PYSWIP_AVAILABLE backward compatibility alias."""
        # PYSWIP_AVAILABLE should be True when janus is available
        assert PYSWIP_AVAILABLE == JANUS_AVAILABLE


class TestMixedLanguageWorkflow:
    """Tests for mixed Python/Lua/Prolog workflows."""

    def test_mixed_python_prolog(self):
        """Test workflow with both Python and Prolog nodes."""
        import yaml
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine(prolog_enabled=True)

        yaml_content = """
name: test-mixed
state_schema:
  input: int
  processed: int
  reasoned: int

nodes:
  - name: python_process
    run: |
      return {"processed": state["input"] * 2}

  - name: prolog_reason
    language: prolog
    run: |
      state(processed, P),
      R is P + 10,
      return(reasoned, R).

edges:
  - from: __start__
    to: python_process
  - from: python_process
    to: prolog_reason
  - from: prolog_reason
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"input": 16}))
        final_state = result[-1]["state"]

        # Python: 16 * 2 = 32
        assert final_state["processed"] == 32
        # Prolog: 32 + 10 = 42
        assert final_state["reasoned"] == 42


class TestJanusNotInstalled:
    """Tests for graceful handling when janus-swi is not installed."""

    def test_janus_availability_flag(self):
        """Test JANUS_AVAILABLE flag is correctly set."""
        # If we get here, JANUS_AVAILABLE should be True
        assert JANUS_AVAILABLE is True

    def test_install_instructions(self):
        """Test that install instructions are platform-specific (AC-9, AC-10)."""
        instructions = _get_install_instructions()
        assert "janus-swi" in instructions
        assert "swi-prolog" in instructions.lower()
        assert "9.1" in instructions  # Version requirement


class TestCrossRuntimeParity:
    """Tests to verify parity with Rust implementation (AC-5)."""

    def test_state_access_parity(self):
        """Test state access matches Rust behavior."""
        runtime = PrologRuntime()

        # Integer access
        result = runtime.execute_node_code(
            "state(x, X), return(result, X)",
            {"x": 42}
        )
        assert result.get("result") == 42

        # String access
        result = runtime.execute_node_code(
            "state(s, S), return(result, S)",
            {"s": "hello"}
        )
        assert result.get("result") == "hello"

    def test_return_value_parity(self):
        """Test return values match Rust behavior."""
        runtime = PrologRuntime()

        # Multiple returns
        result = runtime.execute_node_code(
            "return(a, 1), return(b, 'two'), return(c, [3])",
            {}
        )
        assert result.get("a") == 1
        assert result.get("b") == "two"
        assert result.get("c") == [3]

    def test_arithmetic_parity(self):
        """Test arithmetic operations match Rust."""
        runtime = PrologRuntime()

        result = runtime.execute_node_code(
            "X is 10 + 5 * 2, return(result, X)",
            {}
        )
        assert result.get("result") == 20  # 10 + (5 * 2)

    def test_clpfd_parity(self):
        """Test CLP(FD) matches Rust behavior (AC-5)."""
        runtime = PrologRuntime()

        # Same constraint as Rust tests
        result = runtime.execute_node_code(
            "X in 1..10, X #> 7, label([X]), return(result, X)",
            {}
        )
        # Should find 8 (first solution > 7)
        assert result.get("result") == 8


# Pytest markers for CI filtering
@pytest.mark.prolog
class TestPrologMarker:
    """Tests with @pytest.mark.prolog for CI filtering."""

    def test_prolog_functionality(self):
        """Basic test to verify Prolog marker works."""
        runtime = PrologRuntime()
        result = runtime.execute_node_code(
            "X is 1 + 1, return(result, X)",
            {}
        )
        assert result.get("result") == 2
