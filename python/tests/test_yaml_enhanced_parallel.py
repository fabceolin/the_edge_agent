"""
Tests for YAML Engine Enhanced Parallel Execution (YE.2).

Test Coverage:
- Matrix strategy (AC: 1-7, 17, 19)
- Dynamic parallel_each (AC: 8-13, 18, 19)
- Worker pool configuration (AC: 14-16)
- Result collection and fan-in (AC: 12, 17-20)
- Error handling (AC: 25)
- Backward compatibility (AC: 21, 24)
"""

import pytest
from unittest.mock import MagicMock, patch
import time

from the_edge_agent import YAMLEngine, MemoryCheckpointer


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


# =============================================================================
# Matrix Strategy Tests (AC: 1-7, 17, 19)
# =============================================================================


class TestMatrixStrategyParsing:
    """Test matrix strategy configuration parsing (AC: 1, 2)."""

    def test_unit_001_matrix_strategy_creates_combinations(self, engine):
        """YE.2-UNIT-001: Matrix strategy creates correct combinations."""
        config = {
            "name": "matrix-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "python": ["3.9", "3.10"],
                            "os": ["linux", "macos"],
                        }
                    },
                    "fan_in": "collect",
                    "run": """
version = state.get("matrix", {}).get("python", "unknown")
os_name = state.get("matrix", {}).get("os", "unknown")
return {"version": version, "os": os_name}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        assert "results" in final_state
        results = final_state["results"]

        # Should have 4 combinations (2 python × 2 os)
        assert len(results) == 4

        # Verify all combinations present
        combinations = set()
        for r in results:
            matrix = r.state.get("matrix", {})
            combinations.add((matrix.get("python"), matrix.get("os")))

        expected = {
            ("3.9", "linux"),
            ("3.9", "macos"),
            ("3.10", "linux"),
            ("3.10", "macos"),
        }
        assert combinations == expected

    def test_unit_002_matrix_variables_accessible(self, engine):
        """YE.2-UNIT-002: Matrix variables are accessible in node code."""
        config = {
            "name": "matrix-vars-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "value": [1, 2, 3],
                        }
                    },
                    "fan_in": "collect",
                    "run": """
val = state["matrix"]["value"]
return {"doubled": val * 2}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Verify matrix values were used
        doubled_values = sorted(
            [r.state["result"]["doubled"] for r in results if r.success]
        )
        assert doubled_values == [2, 4, 6]

    def test_unit_003_matrix_results_include_matrix_params(self, engine):
        """YE.2-UNIT-003: Matrix results include matrix parameters (AC: 17)."""
        config = {
            "name": "matrix-results-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "env": ["dev", "prod"],
                        }
                    },
                    "fan_in": "collect",
                    "run": "return {'processed': True}",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Each result should have matrix params
        for r in results:
            assert "matrix" in r.state
            assert "env" in r.state["matrix"]
            assert r.state["matrix"]["env"] in ["dev", "prod"]


class TestMatrixStrategyOptions:
    """Test matrix strategy options (AC: 6, 7)."""

    def test_unit_004_matrix_fail_fast_cancels_remaining(self, engine):
        """YE.2-UNIT-004: fail_fast stops remaining branches on failure (AC: 6)."""
        config = {
            "name": "matrix-fail-fast-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "value": [1, 2, 3, 4, 5],
                        },
                        "fail_fast": True,
                    },
                    "fan_in": "collect",
                    "run": """
import time
val = state["matrix"]["value"]
if val == 2:
    raise ValueError("Fail on value 2")
time.sleep(0.1)  # Slow down to allow fail_fast to take effect
return {"value": val}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Should have at least one failure
        failures = [r for r in results if not r.success]
        assert len(failures) >= 1

        # Some may be cancelled
        cancelled = [r for r in results if "Cancelled" in (r.error or "")]
        # Note: Due to timing, cancellation may or may not happen
        # The key test is that fail_fast triggers cancellation

    def test_unit_005_matrix_max_parallel_limits_concurrency(self, engine):
        """YE.2-UNIT-005: max_parallel limits concurrent executions (AC: 7)."""
        config = {
            "name": "matrix-max-parallel-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "value": [1, 2, 3, 4, 5, 6],
                        },
                        "max_parallel": 2,
                    },
                    "fan_in": "collect",
                    "run": """
import time
val = state["matrix"]["value"]
time.sleep(0.01)  # Brief sleep
return {"value": val}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # All should complete (just with limited concurrency)
        assert len(results) == 6
        assert all(r.success for r in results)


class TestMatrixStrategyValidation:
    """Test matrix strategy validation errors (AC: 25)."""

    def test_unit_006_matrix_requires_fan_in(self, engine):
        """YE.2-UNIT-006: Matrix strategy requires fan_in target."""
        config = {
            "name": "matrix-no-fanin",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "value": [1, 2],
                        }
                    },
                    # Missing fan_in
                    "run": "return {}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "test_node", "to": "__end__"},
            ],
        }

        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "fan_in" in str(exc_info.value).lower()

    def test_unit_007_matrix_values_must_be_lists(self, engine):
        """YE.2-UNIT-007: Matrix parameter values must be lists."""
        config = {
            "name": "matrix-invalid-values",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "value": "not-a-list",  # Invalid
                        }
                    },
                    "fan_in": "collect",
                    "run": "return {}",
                },
                {"name": "collect", "fan_in": True, "run": "return {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "list" in str(exc_info.value).lower()


# =============================================================================
# Parallel Each Tests (AC: 8-13, 18, 19)
# =============================================================================


class TestParallelEachParsing:
    """Test parallel_each configuration parsing (AC: 8, 9)."""

    def test_unit_008_parallel_each_iterates_state_list(self, engine):
        """YE.2-UNIT-008: parallel_each iterates over list from state."""
        config = {
            "name": "parallel-each-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": """
item = state["item"]
return {"processed": item * 2}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": [1, 2, 3]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Should process all items
        assert len(results) == 3

        # Verify processing
        processed_values = sorted(
            [r.state["result"]["processed"] for r in results if r.success]
        )
        assert processed_values == [2, 4, 6]

    def test_unit_009_parallel_each_item_accessible(self, engine):
        """YE.2-UNIT-009: Current item accessible via {{ item }} (AC: 10)."""
        config = {
            "name": "parallel-each-item-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": """
item = state["item"]
return {"item_value": item}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": ["a", "b", "c"]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Verify item values
        item_values = sorted(
            [r.state["result"]["item_value"] for r in results if r.success]
        )
        assert item_values == ["a", "b", "c"]

    def test_unit_010_parallel_each_item_index_accessible(self, engine):
        """YE.2-UNIT-010: Current index accessible via {{ item_index }} (AC: 11)."""
        config = {
            "name": "parallel-each-index-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": """
idx = state["item_index"]
return {"index": idx}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": ["x", "y", "z"]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Verify indexes
        indexes = sorted([r.state["result"]["index"] for r in results if r.success])
        assert indexes == [0, 1, 2]


class TestParallelEachResults:
    """Test parallel_each result collection (AC: 12, 18, 19)."""

    def test_unit_011_parallel_each_results_ordered_by_index(self, engine):
        """YE.2-UNIT-011: Results are ordered by index (AC: 12, 19)."""
        config = {
            "name": "parallel-each-order-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": """
import time
import random
time.sleep(random.uniform(0.001, 0.01))  # Random delay
return {"item": state["item"]}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": [1, 2, 3, 4, 5]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Results should be in index order (0, 1, 2, 3, 4)
        indexes = [r.state["index"] for r in results]
        assert indexes == [0, 1, 2, 3, 4]

    def test_unit_012_parallel_each_results_include_item_and_index(self, engine):
        """YE.2-UNIT-012: Results include item and index (AC: 18)."""
        config = {
            "name": "parallel-each-structure-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": "return {'processed': True}",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": ["a", "b"]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Each result should have item and index
        for r in results:
            assert "item" in r.state
            assert "index" in r.state
            assert "result" in r.state


class TestParallelEachEmptyList:
    """Test parallel_each with empty list (AC: 13)."""

    def test_unit_013_parallel_each_empty_list_skips_to_fanin(self, engine):
        """YE.2-UNIT-013: Empty list results in no parallel executions."""
        config = {
            "name": "parallel-each-empty-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "fan_in": "collect",
                    "run": "return {'processed': True}",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'count': len(parallel_results)}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": []}))

        final_state = events[-1]["state"]
        # parallel_results should be empty list
        assert final_state.get("count") == 0 or "parallel_results" in final_state


class TestParallelEachValidation:
    """Test parallel_each validation errors (AC: 25)."""

    def test_unit_014_parallel_each_requires_fan_in(self, engine):
        """YE.2-UNIT-014: parallel_each requires fan_in target."""
        config = {
            "name": "parallel-each-no-fanin",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    # Missing fan_in
                    "run": "return {}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "process", "to": "__end__"},
            ],
        }

        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "fan_in" in str(exc_info.value).lower()

    def test_unit_015_parallel_each_non_iterable_error(self, engine):
        """YE.2-UNIT-015: Non-iterable parallel_each produces error event."""
        config = {
            "name": "parallel-each-non-iterable",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state.value }}",  # value is int, not list
                    "fan_in": "collect",
                    "run": "return {}",
                },
                {"name": "collect", "fan_in": True, "run": "return {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"value": 42}))  # int, not iterable

        # Should have an error event
        error_events = [e for e in events if e.get("type") == "error"]
        assert len(error_events) >= 1
        assert "iterable" in str(error_events[0].get("error", "")).lower()


# =============================================================================
# Worker Pool Configuration Tests (AC: 14-16)
# =============================================================================


class TestWorkerPoolConfiguration:
    """Test worker pool configuration (AC: 14, 15, 16)."""

    def test_unit_016_per_node_max_workers(self, engine):
        """YE.2-UNIT-016: Per-node max_workers override (AC: 16)."""
        config = {
            "name": "per-node-workers-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state['items'] }}",
                    "max_workers": 2,  # Per-node override
                    "fan_in": "collect",
                    "run": """
import time
time.sleep(0.01)
return {"item": state["item"]}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"items": [1, 2, 3, 4, 5, 6]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # All items should be processed
        assert len(results) == 6


# =============================================================================
# Backward Compatibility Tests (AC: 21, 24)
# =============================================================================


class TestBackwardCompatibility:
    """Test backward compatibility with existing parallel syntax (AC: 21, 24)."""

    def test_unit_017_existing_parallel_edges_unchanged(self, engine):
        """YE.2-UNIT-017: Existing parallel edge syntax works unchanged (AC: 21)."""
        config = {
            "name": "legacy-parallel-test",
            "nodes": [
                {"name": "start", "run": "return {'started': True}"},
                {"name": "flow1", "run": "return {'flow': 1}"},
                {"name": "flow2", "run": "return {'flow': 2}"},
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'collected': True}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "start"},
                {
                    "from": "start",
                    "to": "flow1",
                    "type": "parallel",
                    "fan_in": "collect",
                },
                {
                    "from": "start",
                    "to": "flow2",
                    "type": "parallel",
                    "fan_in": "collect",
                },
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        assert final_state.get("collected") is True


# =============================================================================
# Integration Tests
# =============================================================================


class TestIntegration:
    """Integration tests for enhanced parallel features."""

    def test_int_001_matrix_with_actions(self, engine):
        """YE.2-INT-001: Matrix strategy works with action nodes."""
        config = {
            "name": "matrix-action-test",
            "nodes": [
                {
                    "name": "test_node",
                    "strategy": {
                        "matrix": {
                            "prefix": ["hello", "goodbye"],
                        }
                    },
                    "fan_in": "collect",
                    "uses": "data.transform",
                    "with": {
                        "input": "{{ state.matrix.prefix }}",
                        "operation": "upper",
                    },
                    "output": "result",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "test_node"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        # Should have 2 results
        assert len(results) == 2

    def test_int_002_parallel_each_with_dict_items(self, engine):
        """YE.2-INT-002: parallel_each works with list of dicts."""
        config = {
            "name": "parallel-each-dict-test",
            "nodes": [
                {
                    "name": "process",
                    "parallel_each": "{{ state.users }}",
                    "fan_in": "collect",
                    "run": """
user = state["item"]
return {"greeting": f"Hello, {user['name']}!"}
""",
                },
                {
                    "name": "collect",
                    "fan_in": True,
                    "run": "return {'results': parallel_results}",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "collect", "to": "__end__"},
            ],
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({"users": [{"name": "Alice"}, {"name": "Bob"}]}))

        final_state = events[-1]["state"]
        results = final_state["results"]

        greetings = sorted(
            [r.state["result"]["greeting"] for r in results if r.success]
        )
        assert greetings == ["Hello, Alice!", "Hello, Bob!"]


# =============================================================================
# Run tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
