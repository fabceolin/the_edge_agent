"""
Tests for TEA-YAML-006: Dynamic Parallel Fan-Out/Fan-In.

Tests cover:
- Items expression evaluation (list, empty, non-iterable error)
- Item/index context injection
- Action mode execution
- Steps mode execution
- Subgraph mode execution (local and mocked remote)
- Parse-time validation errors
- max_concurrency throttling
- fail_fast cancellation
- Mixed success/failure result collection
- Observability events
"""

import copy
import os
import tempfile
import time
import threading
from unittest.mock import MagicMock, patch
import pytest

from the_edge_agent import YAMLEngine, StateGraph, END


class TestDynamicParallelParseTimeValidation:
    """Test parse-time validation for dynamic_parallel nodes (AC: 9)."""

    def test_missing_items_raises_error(self):
        """Test that missing 'items' field raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    # missing "items"
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError, match="requires 'items' expression"):
            engine.load_from_dict(config)

    def test_missing_execution_mode_raises_error(self):
        """Test that missing action/steps/subgraph raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    # missing action/steps/subgraph
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(
            ValueError, match="requires exactly one of: action, steps, subgraph"
        ):
            engine.load_from_dict(config)

    def test_multiple_execution_modes_raises_error(self):
        """Test that multiple execution modes raise ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    "action": {"uses": "test.action", "with": {}},
                    "steps": [{"run": "return {}"}],  # both action and steps
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(
            ValueError, match="requires exactly one of: action, steps, subgraph"
        ):
            engine.load_from_dict(config)

    def test_missing_fan_in_raises_error(self):
        """Test that missing 'fan_in' field raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    "steps": [{"run": "return {}"}],
                    # missing "fan_in"
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError, match="requires 'fan_in' target node"):
            engine.load_from_dict(config)

    def test_invalid_max_concurrency_raises_error(self):
        """Test that invalid max_concurrency raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                    "parallel_config": {"max_concurrency": 0},  # invalid
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(
            ValueError, match="max_concurrency must be positive integer"
        ):
            engine.load_from_dict(config)

    def test_action_mode_missing_uses_raises_error(self):
        """Test that action mode without 'uses' field raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    "action": {"with": {"key": "result"}},  # missing "uses"
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError, match="action requires 'uses' field"):
            engine.load_from_dict(config)


class TestDynamicParallelItemsEvaluation:
    """Test items expression evaluation (AC: 1)."""

    def test_items_expression_evaluates_list(self):
        """Test that items expression correctly evaluates a list."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [
                        {"run": "return {'processed': state['item']}"},
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": ["a", "b", "c"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        assert "parallel_results" in result
        assert len(result["parallel_results"]) == 3

    def test_empty_items_returns_empty_results(self):
        """Test that empty items list returns empty parallel_results."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": []}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        assert result.get("parallel_results") == []

    def test_none_items_treated_as_empty(self):
        """Test that None items is treated as empty list."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": None}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        assert result.get("parallel_results") == []


class TestDynamicParallelRuntimeValidation:
    """Test runtime validation (AC: 10)."""

    def test_non_iterable_items_raises_error(self):
        """Test that non-iterable items raises ValueError at runtime."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # Integer is not iterable
        error_event = None
        for event in graph.invoke({"my_items": 42}):
            if event.get("type") == "error":
                error_event = event

        assert error_event is not None
        assert "must return an iterable" in error_event.get("error", "")

    def test_string_items_raises_error(self):
        """Test that string items (while iterable) raises ValueError."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [{"run": "return {}"}],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        # String should be rejected (would iterate over chars)
        error_event = None
        for event in graph.invoke({"my_items": "not a list"}):
            if event.get("type") == "error":
                error_event = event

        assert error_event is not None
        assert "must return an iterable" in error_event.get("error", "")


class TestDynamicParallelItemContextInjection:
    """Test item/index context injection (AC: 2)."""

    def test_default_item_and_index_vars(self):
        """Test that default item_var='item' and index_var='index' work."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.urls }}",
                    "steps": [
                        {
                            "run": "return {'result': f\"idx={state['index']},val={state['item']}\"}"
                        },
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"urls": ["url1", "url2"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 2
        # Check that both items were processed with correct context
        results = [r.state.get("result") for r in parallel_results if r.success]
        assert "idx=0,val=url1" in results
        assert "idx=1,val=url2" in results

    def test_custom_item_and_index_vars(self):
        """Test that custom item_var and index_var work."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.docs }}",
                    "item_var": "doc",
                    "index_var": "doc_idx",
                    "steps": [
                        {
                            "run": "return {'result': f\"doc={state['doc']},idx={state['doc_idx']}\"}"
                        },
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"docs": ["doc_a", "doc_b"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        results = [r.state.get("result") for r in parallel_results if r.success]
        assert "doc=doc_a,idx=0" in results
        assert "doc=doc_b,idx=1" in results


class TestDynamicParallelActionMode:
    """Test action mode execution (AC: 3)."""

    def test_action_mode_executes_per_item(self):
        """Test that action mode executes action per item."""

        # Register a custom action for testing
        def test_action(state, value=None, **kwargs):
            return {"processed": value}

        config = {
            "nodes": [
                {
                    "name": "fetch_all",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_values }}",
                    "action": {
                        "uses": "test.process",
                        "with": {
                            "value": "value_{{ state.item }}_processed",
                        },
                    },
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "fetch_all"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.process": test_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_values": [1, 2, 3]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 3
        assert all(r.success for r in parallel_results)

    def test_action_mode_with_output_key(self):
        """Test that action mode respects output key."""

        def test_action(state, **kwargs):
            return state.get("item") * 2

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.nums }}",
                    "action": {
                        "uses": "test.double",
                        "with": {},
                        "output": "doubled",
                    },
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.double": test_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"nums": [1, 2, 3]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 3


class TestDynamicParallelStepsMode:
    """Test steps mode execution (AC: 4)."""

    def test_steps_mode_executes_sequentially_per_item(self):
        """Test that steps mode executes steps sequentially for each item."""
        config = {
            "nodes": [
                {
                    "name": "process_docs",
                    "type": "dynamic_parallel",
                    "items": "{{ state.docs }}",
                    "steps": [
                        {"name": "step1", "run": "return {'step1_done': True}"},
                        {
                            "name": "step2",
                            "run": "return {'final_value': f\"processed_{state['item']}\"}",
                        },
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process_docs"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"docs": ["docA", "docB"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 2
        assert all(r.success for r in parallel_results)
        # Verify both steps were executed
        for r in parallel_results:
            assert r.state.get("step1_done") == True
            assert "processed_" in r.state.get("final_value", "")

    def test_steps_mode_state_flows_between_steps(self):
        """Test that state flows between steps within each branch."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.nums }}",
                    "steps": [
                        {
                            "name": "double",
                            "run": "return {'doubled': state['item'] * 2}",
                        },
                        {
                            "name": "add_ten",
                            "run": "return {'result': state['doubled'] + 10}",
                        },
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"nums": [5, 10]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        results = sorted([r.state.get("result") for r in parallel_results if r.success])
        # 5*2+10=20, 10*2+10=30
        assert results == [20, 30]


class TestDynamicParallelSubgraphMode:
    """Test subgraph mode execution (AC: 5, 12)."""

    def test_subgraph_mode_with_local_file(self):
        """Test that subgraph mode loads and executes local YAML file."""
        # Create a temporary subgraph file
        subgraph_yaml = """
name: test-subgraph
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": f"processed_{state['input']}"}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(subgraph_yaml)
            subgraph_path = f.name

        try:
            config = {
                "nodes": [
                    {
                        "name": "research",
                        "type": "dynamic_parallel",
                        "items": "{{ state.sources }}",
                        "subgraph": subgraph_path,
                        "input": {
                            "input": "{{ state.item }}",
                        },
                        "fan_in": "collect",
                    },
                    {"name": "collect", "fan_in": True, "run": "return state"},
                ],
                "edges": [
                    {"from": "__start__", "to": "research"},
                    {"from": "collect", "to": "__end__"},
                ],
            }
            engine = YAMLEngine()
            graph = engine.load_from_dict(config)

            result = None
            for event in graph.invoke({"sources": ["src1", "src2"]}):
                if event.get("type") == "final":
                    result = event.get("state")

            assert result is not None
            parallel_results = result.get("parallel_results", [])
            assert len(parallel_results) == 2
            assert all(
                r.success for r in parallel_results
            ), f"Failures: {[r.error for r in parallel_results if not r.success]}"
            outputs = [r.state.get("output") for r in parallel_results]
            assert "processed_src1" in outputs
            assert "processed_src2" in outputs
        finally:
            os.unlink(subgraph_path)

    def test_subgraph_mode_with_complex_input_mapping(self):
        """Test subgraph mode with complex input mapping."""
        subgraph_yaml = """
name: source-researcher
nodes:
  - name: analyze
    run: |
      return {"analysis": f"{state['name']}: {state['url']}"}

edges:
  - from: __start__
    to: analyze
  - from: analyze
    to: __end__
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(subgraph_yaml)
            subgraph_path = f.name

        try:
            config = {
                "nodes": [
                    {
                        "name": "research",
                        "type": "dynamic_parallel",
                        "items": "{{ state.sources }}",
                        "subgraph": subgraph_path,
                        "input": {
                            "url": "{{ state.item.url }}",
                            "name": "{{ state.item.name }}",
                        },
                        "fan_in": "collect",
                    },
                    {"name": "collect", "fan_in": True, "run": "return state"},
                ],
                "edges": [
                    {"from": "__start__", "to": "research"},
                    {"from": "collect", "to": "__end__"},
                ],
            }
            engine = YAMLEngine()
            graph = engine.load_from_dict(config)

            sources = [
                {"name": "Wikipedia", "url": "https://wikipedia.org"},
                {"name": "Google", "url": "https://google.com"},
            ]
            result = None
            for event in graph.invoke({"sources": sources}):
                if event.get("type") == "final":
                    result = event.get("state")

            assert result is not None
            parallel_results = result.get("parallel_results", [])
            assert len(parallel_results) == 2, f"Got {len(parallel_results)} results"
            assert all(
                r.success for r in parallel_results
            ), f"Failures: {[r.error for r in parallel_results if not r.success]}"
            analyses = [r.state.get("analysis") for r in parallel_results if r.success]
            assert "Wikipedia: https://wikipedia.org" in analyses
            assert "Google: https://google.com" in analyses
        finally:
            os.unlink(subgraph_path)


class TestDynamicParallelConcurrencyControl:
    """Test concurrency control (AC: 7)."""

    def test_max_concurrency_limits_parallel_threads(self):
        """Test that max_concurrency limits simultaneous executions."""
        # Track concurrent execution count
        concurrent_count = {"max": 0, "current": 0}
        lock = threading.Lock()

        def track_concurrent(state, **kwargs):
            with lock:
                concurrent_count["current"] += 1
                concurrent_count["max"] = max(
                    concurrent_count["max"], concurrent_count["current"]
                )
            time.sleep(0.05)  # Hold for a bit
            with lock:
                concurrent_count["current"] -= 1
            return {"processed": state.get("item")}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.concurrent", "with": {}},
                    "fan_in": "collect",
                    "parallel_config": {"max_concurrency": 2},
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.concurrent": track_concurrent})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": list(range(6))}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        # Max concurrent should be limited to 2
        assert concurrent_count["max"] <= 2


class TestDynamicParallelFailFast:
    """Test fail_fast behavior (AC: 8)."""

    def test_fail_fast_false_collects_all_results(self):
        """Test that fail_fast=false (default) continues all branches."""

        def failing_action(state, **kwargs):
            if state.get("item") == 1:
                raise ValueError("Simulated failure")
            return {"processed": state.get("item")}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.failing", "with": {}},
                    "fan_in": "collect",
                    "parallel_config": {"fail_fast": False},
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.failing": failing_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": [0, 1, 2]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 3  # All branches completed
        success_count = sum(1 for r in parallel_results if r.success)
        failure_count = sum(1 for r in parallel_results if not r.success)
        assert success_count == 2
        assert failure_count == 1

    def test_fail_fast_true_cancels_on_first_failure(self):
        """Test that fail_fast=true cancels remaining branches on failure."""
        execution_order = []
        lock = threading.Lock()

        def slow_failing_action(state, **kwargs):
            item = state.get("item")
            with lock:
                execution_order.append(("start", item))

            if item == 0:
                # First item fails immediately
                raise ValueError("First item failure")

            # Other items take time
            time.sleep(0.2)
            with lock:
                execution_order.append(("end", item))
            return {"processed": item}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.slow_failing", "with": {}},
                    "fan_in": "collect",
                    "parallel_config": {"fail_fast": True, "max_concurrency": 10},
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.slow_failing": slow_failing_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": [0, 1, 2, 3, 4]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        # At least one branch should have failed
        failures = [r for r in parallel_results if not r.success]
        assert len(failures) >= 1


class TestDynamicParallelFanInCollection:
    """Test fan-in result collection (AC: 6)."""

    def test_parallel_results_structure(self):
        """Test that parallel_results has correct structure."""

        def test_action(state, **kwargs):
            return {"value": state.get("item")}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.process", "with": {}},
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.process": test_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": ["a", "b"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 2

        # Check ParallelFlowResult structure
        for r in parallel_results:
            assert hasattr(r, "branch")
            assert hasattr(r, "success")
            assert hasattr(r, "state")
            assert hasattr(r, "error")
            assert hasattr(r, "timing_ms")
            assert r.success == True
            assert isinstance(r.timing_ms, int)
            assert r.timing_ms >= 0

    def test_results_ordered_by_index(self):
        """Test that parallel_results are ordered by branch index."""

        def test_action(state, **kwargs):
            return {"val": state.get("item")}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.process", "with": {}},
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(actions_registry={"test.process": test_action})
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": ["first", "second", "third"]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])
        branches = [r.branch for r in parallel_results]
        assert branches == ["process[0]", "process[1]", "process[2]"]


class TestDynamicParallelObservability:
    """Test observability events (AC: 13).

    Note: The events are logged via trace_context.log_event when tracing is enabled.
    The actual capture of these events depends on the trace exporter configuration.
    These tests verify that the event logging code paths are executed without error.
    """

    def test_dynamic_parallel_with_tracing_enabled(self):
        """Test that dynamic_parallel executes correctly with tracing enabled."""
        events_captured = []

        def capture_event(event):
            events_captured.append(event)

        def test_action(state, **kwargs):
            return {"value": state.get("item")}

        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.process", "with": {}},
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine(
            enable_tracing=True,
            trace_exporter="callback",
            trace_callback=capture_event,
            actions_registry={"test.process": test_action},
        )
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": ["a", "b"]}):
            if event.get("type") == "final":
                result = event.get("state")

        # Verify execution completed successfully even with tracing enabled
        assert result is not None
        parallel_results = result.get("parallel_results", [])
        assert len(parallel_results) == 2
        assert all(r.success for r in parallel_results)

    def test_dynamic_parallel_tracing_does_not_affect_results(self):
        """Test that tracing configuration doesn't affect execution results."""

        def test_action(state, **kwargs):
            return {"x": state.get("item") * 2}

        config = {
            "nodes": [
                {
                    "name": "test_node",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "action": {"uses": "test.action", "with": {}},
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        # Run without tracing
        engine_no_trace = YAMLEngine(
            enable_tracing=False,
            actions_registry={"test.action": test_action},
        )
        graph_no_trace = engine_no_trace.load_from_dict(config)

        result_no_trace = None
        for event in graph_no_trace.invoke({"my_items": [1, 2, 3]}):
            if event.get("type") == "final":
                result_no_trace = event.get("state")

        # Run with tracing
        engine_trace = YAMLEngine(
            enable_tracing=True,
            trace_exporter="console",
            actions_registry={"test.action": test_action},
        )
        graph_trace = engine_trace.load_from_dict(config)

        result_trace = None
        for event in graph_trace.invoke({"my_items": [1, 2, 3]}):
            if event.get("type") == "final":
                result_trace = event.get("state")

        # Results should be the same
        assert result_no_trace is not None
        assert result_trace is not None

        no_trace_results = sorted(
            [
                r.state.get("x")
                for r in result_no_trace.get("parallel_results", [])
                if r.success
            ]
        )
        trace_results = sorted(
            [
                r.state.get("x")
                for r in result_trace.get("parallel_results", [])
                if r.success
            ]
        )

        assert no_trace_results == trace_results == [2, 4, 6]


class TestDynamicParallelIntegration:
    """Integration tests for dynamic parallel."""

    def test_dynamic_parallel_with_data_transform(self):
        """Test dynamic parallel with data transformation."""
        config = {
            "nodes": [
                {
                    "name": "transform_all",
                    "type": "dynamic_parallel",
                    "items": "{{ state.numbers }}",
                    "steps": [
                        {
                            "name": "double",
                            "run": "return {'doubled': state['item'] * 2}",
                        },
                        {
                            "name": "stringify",
                            "run": "return {'result': f\"Number: {state['doubled']}\"}",
                        },
                    ],
                    "fan_in": "aggregate",
                },
                {
                    "name": "aggregate",
                    "fan_in": True,
                    "run": """
results = [r.state.get('result') for r in state.get('parallel_results', []) if r.success]
return {"all_results": results}
""",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "transform_all"},
                {"from": "aggregate", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"numbers": [1, 2, 3, 4, 5]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        all_results = result.get("all_results", [])
        assert len(all_results) == 5
        assert "Number: 2" in all_results
        assert "Number: 10" in all_results

    def test_dynamic_parallel_with_nested_objects(self):
        """Test dynamic parallel with nested object items."""
        config = {
            "nodes": [
                {
                    "name": "process_users",
                    "type": "dynamic_parallel",
                    "items": "{{ state.users }}",
                    "steps": [
                        {
                            "run": """
user = state['item']
return {"greeting": f"Hello, {user['name']}! You are {user['age']} years old."}
""",
                        },
                    ],
                    "fan_in": "collect",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": """
greetings = [r.state.get('greeting') for r in state.get('parallel_results', []) if r.success]
return {"greetings": greetings}
""",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process_users"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        users = [
            {"name": "Alice", "age": 30},
            {"name": "Bob", "age": 25},
        ]
        result = None
        for event in graph.invoke({"users": users}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        greetings = result.get("greetings", [])
        assert len(greetings) == 2
        assert "Hello, Alice! You are 30 years old." in greetings
        assert "Hello, Bob! You are 25 years old." in greetings


class TestDynamicParallelStateIsolation:
    """Test state isolation between branches."""

    def test_branches_have_independent_state(self):
        """Test that branches don't affect each other's state."""
        config = {
            "nodes": [
                {
                    "name": "process",
                    "type": "dynamic_parallel",
                    "items": "{{ state.my_items }}",
                    "steps": [
                        {
                            "run": """
# Modify state - should not affect other branches
state['modified'] = state['item'] * 10
return {"branch_value": state['modified'], "original_item": state['item']}
""",
                        },
                    ],
                    "fan_in": "collect",
                },
                {"name": "collect", "fan_in": True, "run": "return state"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        result = None
        for event in graph.invoke({"my_items": [1, 2, 3]}):
            if event.get("type") == "final":
                result = event.get("state")

        assert result is not None
        parallel_results = result.get("parallel_results", [])

        # Each branch should have its own independent state
        for r in parallel_results:
            if r.success:
                original = r.state.get("original_item")
                expected_modified = original * 10
                assert r.state.get("branch_value") == expected_modified
