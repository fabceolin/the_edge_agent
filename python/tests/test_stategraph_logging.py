import unittest
import logging

import the_edge_agent as tea


class TestStateGraphLogging(unittest.TestCase):
    """Tests for logging functionality in StateGraph."""

    def test_logging_node_entry_debug(self):
        """Test DEBUG logs appear for node entry when log_level=DEBUG."""
        graph = tea.StateGraph({"value": int}, log_level=logging.DEBUG)
        graph.add_node("process", run=lambda state: {"value": state["value"] + 1})
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.DEBUG) as cm:
            list(graph.invoke({"value": 1}))

        # Check for node entry log
        self.assertTrue(
            any("Entering node: process" in log for log in cm.output),
            f"Expected 'Entering node: process' in logs, got: {cm.output}"
        )

    def test_logging_info_node_completion(self):
        """Test INFO logs appear for node completion."""
        graph = tea.StateGraph({"value": int}, log_level=logging.INFO)
        graph.add_node("process", run=lambda state: {"value": state["value"] + 1})
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.INFO) as cm:
            list(graph.invoke({"value": 1}))

        # Check for node completion log
        self.assertTrue(
            any("Node 'process' completed successfully" in log for log in cm.output),
            f"Expected node completion log in: {cm.output}"
        )

    def test_logging_parallel_flow_start_join(self):
        """Test INFO logs appear for parallel flow start/join."""
        graph = tea.StateGraph({"value": int}, log_level=logging.INFO)
        graph.add_node("start", run=lambda state: {"value": state["value"]})
        graph.add_node("flow1", run=lambda state: {"flow1": True})
        graph.add_node("flow2", run=lambda state: {"flow2": True})
        graph.add_fanin_node("fan_in", run=lambda state, parallel_results: {"collected": len(parallel_results)})
        graph.add_node("end", run=lambda state: state)
        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow1", "fan_in")
        graph.add_parallel_edge("start", "flow2", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.INFO) as cm:
            list(graph.invoke({"value": 1}))

        # Check for parallel flow logs
        self.assertTrue(
            any("Starting" in log and "parallel flow" in log for log in cm.output),
            f"Expected parallel flow start log in: {cm.output}"
        )
        self.assertTrue(
            any("Joining" in log and "parallel flow" in log for log in cm.output),
            f"Expected parallel flow join log in: {cm.output}"
        )

    def test_logging_error_on_exception(self):
        """Test ERROR logs appear when node raises exception."""
        def error_func(state):
            raise ValueError("Test error")

        graph = tea.StateGraph({"value": int}, log_level=logging.ERROR)
        graph.add_node("error_node", run=error_func)
        graph.set_entry_point("error_node")
        graph.set_finish_point("error_node")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.ERROR) as cm:
            list(graph.invoke({"value": 1}))

        # Check for error log
        self.assertTrue(
            any("Error in node 'error_node'" in log for log in cm.output),
            f"Expected error log in: {cm.output}"
        )

    def test_state_values_not_logged_by_default(self):
        """Test state values are NOT logged when log_state_values=False (default)."""
        secret_value = "super_secret_api_key_12345"
        graph = tea.StateGraph({"secret": str}, log_level=logging.DEBUG, log_state_values=False)
        graph.add_node("process", run=lambda state: state)
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.DEBUG) as cm:
            list(graph.invoke({"secret": secret_value}))

        # Secret should NOT appear in any log
        all_logs = ' '.join(cm.output)
        self.assertNotIn(
            secret_value, all_logs,
            f"Secret value should not be logged when log_state_values=False"
        )

    def test_state_values_logged_when_enabled(self):
        """Test state values ARE logged when log_state_values=True."""
        test_value = "visible_test_value_67890"
        graph = tea.StateGraph({"data": str}, log_level=logging.DEBUG, log_state_values=True)
        graph.add_node("process", run=lambda state: state)
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.DEBUG) as cm:
            list(graph.invoke({"data": test_value}))

        # Value should appear in logs
        all_logs = ' '.join(cm.output)
        self.assertIn(
            test_value, all_logs,
            f"Test value should be logged when log_state_values=True"
        )

    def test_stream_logging(self):
        """Test logging works with stream() method."""
        graph = tea.StateGraph({"value": int}, log_level=logging.DEBUG)
        graph.add_node("process", run=lambda state: {"value": state["value"] + 1})
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        with self.assertLogs('the_edge_agent.stategraph', level=logging.DEBUG) as cm:
            list(graph.stream({"value": 1}))

        # Check for stream-specific log
        self.assertTrue(
            any("Starting stream execution" in log for log in cm.output),
            f"Expected 'Starting stream execution' in logs, got: {cm.output}"
        )


if __name__ == '__main__':
    unittest.main()
