"""
Integration tests for ParallelFlowResult JSON serialization.

TEA-BUG-001: Tests that parallel workflow state can be serialized to JSON
without errors in all code paths (CLI, observability, interactive).

These tests verify the fix works end-to-end with real workflow execution.
"""

import json
import os
import tempfile
import unittest
from io import StringIO
from pathlib import Path
from unittest.mock import patch

import the_edge_agent as tea
from the_edge_agent.serialization import TeaJSONEncoder, dumps
from the_edge_agent.parallel import ParallelFlowResult


class TestParallelWorkflowSerialization(unittest.TestCase):
    """Integration tests for parallel workflow serialization."""

    def setUp(self):
        """Create a graph with parallel flows for testing."""
        self.graph = tea.StateGraph(state_schema={}, raise_exceptions=True)

        def start_run(state, config, node, graph):
            state.setdefault("value", 0)
            return {}

        def flow_a_run(state, config, node, graph):
            return {"flow_a_value": state.get("value", 0) + 10}

        def flow_b_run(state, config, node, graph):
            return {"flow_b_value": state.get("value", 0) + 20}

        def fan_in_run(state, config, node, graph):
            parallel_results = state.get("parallel_results", [])
            total = sum(
                result.get("flow_a_value", 0) + result.get("flow_b_value", 0)
                for result in parallel_results
            )
            return {"total": total, "parallel_results": parallel_results}

        self.graph.add_node("start", run=start_run)
        self.graph.add_node("flow_a", run=flow_a_run)
        self.graph.add_node("flow_b", run=flow_b_run)
        self.graph.add_fanin_node("fan_in", run=fan_in_run)

        self.graph.set_entry_point("start")
        self.graph.set_finish_point("fan_in")

        self.graph.add_parallel_edge("start", "flow_a", "fan_in")
        self.graph.add_parallel_edge("start", "flow_b", "fan_in")
        self.graph.add_edge("flow_a", "fan_in")
        self.graph.add_edge("flow_b", "fan_in")

    def test_parallel_results_in_final_state_serializes(self):
        """Test that final state with parallel_results can be JSON serialized."""
        initial_state = {"value": 5}

        final_state = None
        for event in self.graph.invoke(initial_state):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        self.assertIsNotNone(final_state)
        self.assertIn("parallel_results", final_state)

        # This would fail before the fix with:
        # TypeError: Object of type ParallelFlowResult is not JSON serializable
        serialized = json.dumps(final_state, cls=TeaJSONEncoder)

        # Verify it's valid JSON
        parsed = json.loads(serialized)
        self.assertIn("parallel_results", parsed)
        self.assertIsInstance(parsed["parallel_results"], list)
        self.assertEqual(len(parsed["parallel_results"]), 2)

    def test_parallel_results_preserve_all_fields(self):
        """Test that serialized parallel_results contain all expected fields."""
        initial_state = {"value": 5}

        final_state = None
        for event in self.graph.invoke(initial_state):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        self.assertIsNotNone(final_state)
        serialized = json.dumps(final_state, cls=TeaJSONEncoder)
        parsed = json.loads(serialized)

        for result in parsed["parallel_results"]:
            # Check required fields exist
            self.assertIn("branch", result)
            self.assertIn("success", result)
            self.assertIn("state", result)
            self.assertIn("timing_ms", result)
            self.assertIn("retry_count", result)
            self.assertIn("timeout", result)

    def test_stream_events_serializable(self):
        """Test that all stream events can be JSON serialized."""
        initial_state = {"value": 5}

        for event in self.graph.stream(initial_state):
            # Every event should be serializable
            # This would fail before fix if event contains ParallelFlowResult
            try:
                serialized = json.dumps(event, cls=TeaJSONEncoder)
                # Verify it's valid JSON
                json.loads(serialized)
            except TypeError as e:
                self.fail(f"Failed to serialize event: {event}. Error: {e}")

    def test_dumps_convenience_function_with_parallel_state(self):
        """Test the dumps() convenience function works with parallel state."""
        initial_state = {"value": 5}

        final_state = None
        for event in self.graph.invoke(initial_state):
            if event.get("type") == "final":
                final_state = event.get("state", {})

        self.assertIsNotNone(final_state)

        # Use convenience function
        serialized = dumps(final_state, indent=2)

        # Verify output
        parsed = json.loads(serialized)
        self.assertIn("parallel_results", parsed)


class TestObservabilitySerializationIntegration(unittest.TestCase):
    """Test observability logging with parallel results."""

    def test_file_handler_serializes_parallel_results(self):
        """Test that FileHandler can write events with ParallelFlowResult."""
        from the_edge_agent.observability import FileHandler

        with tempfile.TemporaryDirectory() as tmpdir:
            log_path = Path(tmpdir) / "events.jsonl"
            handler = FileHandler(str(log_path))

            # Create event with ParallelFlowResult
            result = ParallelFlowResult(
                branch="test_flow",
                success=True,
                state={"output": "test_value"},
                timing_ms=100.5,
            )

            event = {
                "flow_id": "test-123",
                "event_type": "node_complete",
                "state": {
                    "parallel_results": [result],
                    "other_data": "value",
                },
            }

            # This would fail before the fix
            handler.handle(event)

            # Verify file was written and is valid JSON
            self.assertTrue(log_path.exists())
            with open(log_path) as f:
                line = f.readline()
                parsed = json.loads(line)

            self.assertEqual(parsed["flow_id"], "test-123")
            self.assertEqual(
                parsed["state"]["parallel_results"][0]["branch"], "test_flow"
            )


class TestCLIOutputSerializationIntegration(unittest.TestCase):
    """Test CLI output serialization with parallel results."""

    def test_emit_ndjson_event_with_parallel_state(self):
        """Test that emit_ndjson_event handles ParallelFlowResult in state."""
        from the_edge_agent.cli import emit_ndjson_event

        result = ParallelFlowResult(
            branch="api_call",
            success=True,
            state={"response": {"status": 200}},
            timing_ms=250.0,
        )

        state = {
            "input": "test",
            "parallel_results": [result],
        }

        # Capture stdout
        with patch("sys.stdout", new_callable=StringIO) as mock_stdout:
            emit_ndjson_event("final", state=state, node="fan_in")
            output = mock_stdout.getvalue()

        # Parse the output as JSON (NDJSON line)
        parsed = json.loads(output.strip())

        self.assertEqual(parsed["type"], "final")
        self.assertEqual(parsed["node"], "fan_in")
        self.assertEqual(parsed["state"]["parallel_results"][0]["branch"], "api_call")


class TestInteractiveFormatSerialization(unittest.TestCase):
    """Test interactive module format methods handle parallel results."""

    def test_format_value_with_parallel_result_in_dict(self):
        """Test _format_value handles dict containing ParallelFlowResult."""
        from the_edge_agent.interactive import InteractiveRunner

        # Create a minimal runner to test _format_value method
        runner = InteractiveRunner.__new__(InteractiveRunner)
        runner.display_format = "pretty"

        result = ParallelFlowResult(
            branch="analysis",
            success=True,
            state={"findings": ["item1", "item2"]},
        )

        # Dict containing ParallelFlowResult
        value = {
            "results": [result],
            "count": 1,
        }

        # This would fail before the fix with TypeError
        formatted = runner._format_value(value)

        # Verify output is valid JSON
        parsed = json.loads(formatted)
        self.assertEqual(parsed["results"][0]["branch"], "analysis")
        self.assertEqual(parsed["count"], 1)

    def test_format_value_json_mode_with_parallel_result(self):
        """Test _format_value in json mode handles ParallelFlowResult."""
        from the_edge_agent.interactive import InteractiveRunner

        runner = InteractiveRunner.__new__(InteractiveRunner)
        runner.display_format = "json"

        result = ParallelFlowResult(
            branch="test",
            success=False,
            error="timeout",
        )

        value = {"parallel_results": [result]}

        formatted = runner._format_value(value)
        parsed = json.loads(formatted)

        self.assertEqual(parsed["parallel_results"][0]["branch"], "test")
        self.assertEqual(parsed["parallel_results"][0]["error"], "timeout")


if __name__ == "__main__":
    unittest.main()
