"""
Unit tests for the serialization module.

Tests TEA-BUG-001 fix: ParallelFlowResult JSON serialization.

Test coverage:
- ParallelFlowResult serialization via TeaJSONEncoder
- Nested ParallelFlowResult in state dicts
- Round-trip serialization (serialize -> deserialize)
- Other dataclass serialization (generic fallback)
- Standard types passthrough
"""

import json
import unittest
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from the_edge_agent.serialization import TeaJSONEncoder, dumps, loads
from the_edge_agent.parallel import ParallelFlowResult


class TestTeaJSONEncoder(unittest.TestCase):
    """Test suite for TeaJSONEncoder."""

    def test_parallel_flow_result_basic_success(self):
        """Test serialization of successful ParallelFlowResult."""
        result = ParallelFlowResult(
            branch="flow_a",
            success=True,
            state={"value": 42, "name": "test"},
            timing_ms=150.5,
        )

        serialized = json.dumps(result, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["branch"], "flow_a")
        self.assertEqual(parsed["success"], True)
        self.assertEqual(parsed["state"]["value"], 42)
        self.assertEqual(parsed["state"]["name"], "test")
        self.assertEqual(parsed["timing_ms"], 150.5)
        self.assertIsNone(parsed["error"])
        self.assertIsNone(parsed["error_type"])
        self.assertIsNone(parsed["traceback"])
        self.assertEqual(parsed["timeout"], False)

    def test_parallel_flow_result_failure(self):
        """Test serialization of failed ParallelFlowResult."""
        result = ParallelFlowResult(
            branch="flow_b",
            success=False,
            error="Connection timeout",
            error_type="TimeoutError",
            timeout=True,
            timing_ms=30000.0,
            retry_count=3,
            circuit_state="open",
            attempt_errors=[
                {"attempt": 1, "error": "timeout"},
                {"attempt": 2, "error": "timeout"},
                {"attempt": 3, "error": "timeout"},
            ],
        )

        serialized = json.dumps(result, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["branch"], "flow_b")
        self.assertEqual(parsed["success"], False)
        self.assertEqual(parsed["error"], "Connection timeout")
        self.assertEqual(parsed["error_type"], "TimeoutError")
        self.assertEqual(parsed["timeout"], True)
        self.assertEqual(parsed["retry_count"], 3)
        self.assertEqual(parsed["circuit_state"], "open")
        self.assertEqual(len(parsed["attempt_errors"]), 3)

    def test_parallel_flow_result_in_state(self):
        """Test serialization when ParallelFlowResult is nested in state dict."""
        result1 = ParallelFlowResult(
            branch="flow_a", success=True, state={"output": "A"}
        )
        result2 = ParallelFlowResult(
            branch="flow_b", success=True, state={"output": "B"}
        )

        state = {
            "input": "test",
            "parallel_results": [result1, result2],
            "final_output": "combined",
        }

        serialized = json.dumps(state, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["input"], "test")
        self.assertEqual(len(parsed["parallel_results"]), 2)
        self.assertEqual(parsed["parallel_results"][0]["branch"], "flow_a")
        self.assertEqual(parsed["parallel_results"][0]["state"]["output"], "A")
        self.assertEqual(parsed["parallel_results"][1]["branch"], "flow_b")
        self.assertEqual(parsed["parallel_results"][1]["state"]["output"], "B")

    def test_deeply_nested_parallel_results(self):
        """Test serialization with deeply nested ParallelFlowResult objects."""
        inner_result = ParallelFlowResult(
            branch="inner", success=True, state={"data": [1, 2, 3]}
        )

        # ParallelFlowResult containing another ParallelFlowResult in its state
        outer_result = ParallelFlowResult(
            branch="outer",
            success=True,
            state={
                "nested_result": inner_result,
                "metadata": {"count": 1},
            },
        )

        state = {"results": [outer_result]}

        serialized = json.dumps(state, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        # Check outer result
        self.assertEqual(parsed["results"][0]["branch"], "outer")
        # Check inner result (nested inside outer's state)
        nested = parsed["results"][0]["state"]["nested_result"]
        self.assertEqual(nested["branch"], "inner")
        self.assertEqual(nested["state"]["data"], [1, 2, 3])

    def test_parallel_flow_result_list(self):
        """Test serialization of list of ParallelFlowResults."""
        results = [
            ParallelFlowResult(branch=f"flow_{i}", success=True) for i in range(5)
        ]

        serialized = json.dumps(results, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(len(parsed), 5)
        for i, result in enumerate(parsed):
            self.assertEqual(result["branch"], f"flow_{i}")


class TestDumpsConvenienceFunction(unittest.TestCase):
    """Test suite for dumps() convenience function."""

    def test_dumps_with_parallel_flow_result(self):
        """Test dumps() handles ParallelFlowResult correctly."""
        result = ParallelFlowResult(branch="test", success=True)

        serialized = dumps(result)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["branch"], "test")

    def test_dumps_with_indent(self):
        """Test dumps() passes through indent parameter."""
        result = ParallelFlowResult(branch="test", success=True)

        serialized = dumps(result, indent=2)

        # Should have newlines from indentation
        self.assertIn("\n", serialized)
        # Should be valid JSON
        parsed = json.loads(serialized)
        self.assertEqual(parsed["branch"], "test")

    def test_dumps_standard_dict(self):
        """Test dumps() works with standard dicts."""
        data = {"key": "value", "number": 42}

        serialized = dumps(data)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["key"], "value")
        self.assertEqual(parsed["number"], 42)


class TestRoundTrip(unittest.TestCase):
    """Test round-trip serialization (serialize -> deserialize)."""

    def test_parallel_flow_result_round_trip(self):
        """Test that serialized data can be deserialized correctly."""
        original = ParallelFlowResult(
            branch="roundtrip_test",
            success=True,
            state={"complex": {"nested": [1, 2, 3]}},
            timing_ms=123.456,
            retry_count=1,
        )

        serialized = dumps(original)
        deserialized = loads(serialized)

        # Deserialized data is a dict, not ParallelFlowResult
        self.assertIsInstance(deserialized, dict)
        self.assertEqual(deserialized["branch"], "roundtrip_test")
        self.assertEqual(deserialized["success"], True)
        self.assertEqual(deserialized["state"]["complex"]["nested"], [1, 2, 3])
        self.assertEqual(deserialized["timing_ms"], 123.456)

    def test_state_with_parallel_results_round_trip(self):
        """Test round-trip for state containing parallel_results."""
        state = {
            "input": "test_input",
            "parallel_results": [
                ParallelFlowResult(branch="a", success=True, state={"out": 1}),
                ParallelFlowResult(branch="b", success=False, error="failed"),
            ],
        }

        serialized = dumps(state)
        deserialized = loads(serialized)

        self.assertEqual(deserialized["input"], "test_input")
        self.assertEqual(len(deserialized["parallel_results"]), 2)
        self.assertEqual(deserialized["parallel_results"][0]["state"]["out"], 1)
        self.assertEqual(deserialized["parallel_results"][1]["error"], "failed")


class TestGenericDataclassFallback(unittest.TestCase):
    """Test that other dataclasses are serialized via asdict fallback."""

    def test_custom_dataclass_serialization(self):
        """Test serialization of a custom dataclass without to_dict()."""

        @dataclass
        class CustomResult:
            name: str
            value: int
            items: List[str]

        obj = CustomResult(name="test", value=42, items=["a", "b", "c"])

        serialized = json.dumps(obj, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed["name"], "test")
        self.assertEqual(parsed["value"], 42)
        self.assertEqual(parsed["items"], ["a", "b", "c"])


class TestStandardTypesPassthrough(unittest.TestCase):
    """Test that standard JSON types pass through unchanged."""

    def test_primitives(self):
        """Test primitive types serialize correctly."""
        data = {
            "string": "hello",
            "int": 42,
            "float": 3.14,
            "bool": True,
            "null": None,
        }

        serialized = json.dumps(data, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed, data)

    def test_nested_structures(self):
        """Test nested dicts and lists serialize correctly."""
        data = {
            "list": [1, 2, [3, 4, {"nested": "value"}]],
            "dict": {"a": {"b": {"c": "deep"}}},
        }

        serialized = json.dumps(data, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        self.assertEqual(parsed, data)


class TestErrorHandling(unittest.TestCase):
    """Test error handling for non-serializable types."""

    def test_non_serializable_type_raises(self):
        """Test that truly non-serializable types raise TypeError."""

        class NotSerializable:
            pass

        obj = NotSerializable()

        with self.assertRaises(TypeError):
            json.dumps(obj, cls=TeaJSONEncoder)


if __name__ == "__main__":
    unittest.main()
