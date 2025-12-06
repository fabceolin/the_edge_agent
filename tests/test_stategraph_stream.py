import unittest

import the_edge_agent as tea


class TestStateGraphStreamParallel(unittest.TestCase):
    """Tests for parallel execution support in stream() method (TD.7)."""

    def test_stream_parallel_basic_two_branches(self):
        """
        Test that stream() handles basic parallel execution with 2 branches.
        Verifies all intermediate states are yielded with proper branch identification.
        """
        graph = tea.StateGraph({"value": int, "results": list})

        def branch_a(state):
            return {"results": state.get("results", []) + ["a"]}

        def branch_b(state):
            return {"results": state.get("results", []) + ["b"]}

        def fan_in(state, parallel_results):
            combined = []
            for r in parallel_results:
                combined.extend(r.get("results", []))
            return {"results": combined}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_a", run=branch_a)
        graph.add_node("branch_b", run=branch_b)
        graph.add_fanin_node("fan_in", run=fan_in)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_a", "fan_in")
        graph.add_parallel_edge("start", "branch_b", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_a", "fan_in")
        graph.add_edge("branch_b", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        compiled = graph.compile()
        events = list(compiled.stream({"value": 1}))

        # Verify we got parallel_state events from both branches
        parallel_events = [e for e in events if e.get("type") == "parallel_state"]
        branches_seen = set(e.get("branch") for e in parallel_events)
        self.assertEqual(branches_seen, {"branch_a", "branch_b"})

        # Verify final state has combined results
        final = [e for e in events if e.get("type") == "final"][0]
        self.assertIn("a", final["state"]["results"])
        self.assertIn("b", final["state"]["results"])

    def test_stream_parallel_three_branches(self):
        """
        Test stream() with 3 parallel branches to verify fan-in receives all results.
        """
        graph = tea.StateGraph({"value": int, "collected": list})

        def branch_work(branch_name):
            def work(state):
                return {"branch": branch_name}
            return work

        def fan_in(state, parallel_results):
            branches = [r.get("branch") for r in parallel_results]
            return {"collected": branches}

        graph.add_node("start", run=lambda state: {"value": state.get("value", 0)})
        graph.add_node("branch_1", run=branch_work("b1"))
        graph.add_node("branch_2", run=branch_work("b2"))
        graph.add_node("branch_3", run=branch_work("b3"))
        graph.add_fanin_node("fan_in", run=fan_in)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_1", "fan_in")
        graph.add_parallel_edge("start", "branch_2", "fan_in")
        graph.add_parallel_edge("start", "branch_3", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_1", "fan_in")
        graph.add_edge("branch_2", "fan_in")
        graph.add_edge("branch_3", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        events = list(graph.compile().stream({"value": 1}))

        # Verify parallel events from all 3 branches
        parallel_events = [e for e in events if e.get("type") == "parallel_state"]
        branches_seen = set(e.get("branch") for e in parallel_events)
        self.assertEqual(branches_seen, {"branch_1", "branch_2", "branch_3"})

        # Verify fan-in received all 3 results
        final = [e for e in events if e.get("type") == "final"][0]
        collected = final["state"]["collected"]
        self.assertEqual(len(collected), 3)
        self.assertEqual(set(collected), {"b1", "b2", "b3"})

    def test_stream_parallel_one_branch_failure(self):
        """
        Test that one branch failure yields parallel_error but other branches continue.
        Fan-in still executes with partial results.
        """
        graph = tea.StateGraph({"value": int}, raise_exceptions=False)

        def branch_ok(state):
            return {"ok": True}

        def branch_fail(state):
            raise ValueError("Intentional branch failure")

        def fan_in(state, parallel_results):
            return {"result_count": len(parallel_results)}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_ok", run=branch_ok)
        graph.add_node("branch_fail", run=branch_fail)
        graph.add_fanin_node("fan_in", run=fan_in)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_ok", "fan_in")
        graph.add_parallel_edge("start", "branch_fail", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_ok", "fan_in")
        graph.add_edge("branch_fail", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        events = list(graph.compile().stream({"value": 1}))

        # Should have parallel_error event
        error_events = [e for e in events if e.get("type") == "parallel_error"]
        self.assertEqual(len(error_events), 1)
        self.assertEqual(error_events[0]["branch"], "branch_fail")
        self.assertIn("Intentional branch failure", error_events[0]["error"])

        # Should have parallel_state from successful branch
        state_events = [e for e in events if e.get("type") == "parallel_state"]
        self.assertEqual(len(state_events), 1)
        self.assertEqual(state_events[0]["branch"], "branch_ok")

        # Should have final state (fan-in executed with partial results)
        final_events = [e for e in events if e.get("type") == "final"]
        self.assertEqual(len(final_events), 1)
        # Only 1 result because one branch failed
        self.assertEqual(final_events[0]["state"]["result_count"], 1)

    def test_stream_parallel_all_branches_fail(self):
        """
        Test graceful handling when all parallel branches fail.
        Fan-in should still execute with empty parallel_results.
        """
        graph = tea.StateGraph({"value": int}, raise_exceptions=False)

        def branch_fail_1(state):
            raise ValueError("Branch 1 error")

        def branch_fail_2(state):
            raise ValueError("Branch 2 error")

        def fan_in(state, parallel_results):
            return {"result_count": len(parallel_results), "results": parallel_results}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_1", run=branch_fail_1)
        graph.add_node("branch_2", run=branch_fail_2)
        graph.add_fanin_node("fan_in", run=fan_in)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_1", "fan_in")
        graph.add_parallel_edge("start", "branch_2", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_1", "fan_in")
        graph.add_edge("branch_2", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        events = list(graph.compile().stream({"value": 1}))

        # Should have 2 parallel_error events
        error_events = [e for e in events if e.get("type") == "parallel_error"]
        self.assertEqual(len(error_events), 2)

        # Should have final state (fan-in executed with empty results)
        final_events = [e for e in events if e.get("type") == "final"]
        self.assertEqual(len(final_events), 1)
        self.assertEqual(final_events[0]["state"]["result_count"], 0)

    def test_stream_parallel_results_available_to_fanin(self):
        """
        Test that parallel_results is properly passed to fan-in node,
        matching invoke() behavior.
        """
        graph = tea.StateGraph({"value": int})

        def branch_a(state):
            return {"branch_a_value": state.get("value", 0) + 10}

        def branch_b(state):
            return {"branch_b_value": state.get("value", 0) + 20}

        def fan_in(state, parallel_results):
            # parallel_results should be a list of dicts from each branch
            total = 0
            for result in parallel_results:
                total += result.get("branch_a_value", 0) + result.get("branch_b_value", 0)
            return {"total": total, "num_results": len(parallel_results)}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_a", run=branch_a)
        graph.add_node("branch_b", run=branch_b)
        graph.add_fanin_node("fan_in", run=fan_in)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_a", "fan_in")
        graph.add_parallel_edge("start", "branch_b", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_a", "fan_in")
        graph.add_edge("branch_b", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        events = list(graph.compile().stream({"value": 5}))

        final = [e for e in events if e.get("type") == "final"][0]
        # 2 branches should have contributed
        self.assertEqual(final["state"]["num_results"], 2)
        # branch_a: 5+10=15, branch_b: 5+20=25 -> total=40
        self.assertEqual(final["state"]["total"], 40)

    def test_stream_parallel_yield_types(self):
        """
        Test that stream() yields correct event types for parallel execution:
        - parallel_state for intermediate states from branches
        - state for fan-in node
        - final for completion
        """
        graph = tea.StateGraph({"value": int})

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_a", run=lambda state: {"from_a": True})
        graph.add_node("branch_b", run=lambda state: {"from_b": True})
        graph.add_fanin_node("fan_in", run=lambda state, parallel_results: {"merged": len(parallel_results)})
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_a", "fan_in")
        graph.add_parallel_edge("start", "branch_b", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_a", "fan_in")
        graph.add_edge("branch_b", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        events = list(graph.compile().stream({"value": 1}))

        # Collect event types
        event_types = [e.get("type") for e in events]

        # Should have parallel_state events
        self.assertIn("parallel_state", event_types)

        # Should have state events (from start and fan_in)
        self.assertIn("state", event_types)

        # Should have final event
        self.assertIn("final", event_types)

        # Verify parallel_state events have branch field
        for event in events:
            if event.get("type") == "parallel_state":
                self.assertIn("branch", event)
                self.assertIn("node", event)
                self.assertIn("state", event)

    def test_stream_parallel_interrupt_support(self):
        """
        Test that interrupts work with parallel streaming (at fan-in node).
        """
        graph = tea.StateGraph({"value": int})

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_a", run=lambda state: {"from_a": True})
        graph.add_node("branch_b", run=lambda state: {"from_b": True})
        graph.add_fanin_node("fan_in", run=lambda state, parallel_results: {"merged": len(parallel_results)})
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "branch_a", "fan_in")
        graph.add_parallel_edge("start", "branch_b", "fan_in")
        # Parallel flows need edges to reach fan-in
        graph.add_edge("branch_a", "fan_in")
        graph.add_edge("branch_b", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        # Interrupt before fan_in
        events = list(graph.compile(interrupt_before=["fan_in"]).stream({"value": 1}))

        # Should have interrupt_before for fan_in
        interrupt_events = [e for e in events if e.get("type") == "interrupt_before"]
        self.assertEqual(len(interrupt_events), 1)
        self.assertEqual(interrupt_events[0]["node"], "fan_in")

    def test_stream_parallel_matches_invoke_results(self):
        """
        Test that stream() with parallel edges produces equivalent final state to invoke().
        """
        def create_graph():
            graph = tea.StateGraph({"value": int})

            graph.add_node("start", run=lambda state: {"value": state.get("value", 0)})
            graph.add_node("branch_a", run=lambda state: {"a_result": state["value"] * 2})
            graph.add_node("branch_b", run=lambda state: {"b_result": state["value"] * 3})
            graph.add_fanin_node("fan_in", run=lambda state, parallel_results: {
                "combined": sum(
                    r.get("a_result", 0) + r.get("b_result", 0)
                    for r in parallel_results
                )
            })
            graph.add_node("end", run=lambda state: state)

            graph.set_entry_point("start")
            graph.add_parallel_edge("start", "branch_a", "fan_in")
            graph.add_parallel_edge("start", "branch_b", "fan_in")
            # Parallel flows need edges to reach fan-in
            graph.add_edge("branch_a", "fan_in")
            graph.add_edge("branch_b", "fan_in")
            graph.add_edge("fan_in", "end")
            graph.set_finish_point("end")

            return graph.compile()

        initial_state = {"value": 10}

        # Test with invoke
        invoke_graph = create_graph()
        invoke_events = list(invoke_graph.invoke(initial_state))
        invoke_final = [e for e in invoke_events if e.get("type") == "final"][0]

        # Test with stream
        stream_graph = create_graph()
        stream_events = list(stream_graph.stream(initial_state))
        stream_final = [e for e in stream_events if e.get("type") == "final"][0]

        # Final states should match (combined = 10*2 + 10*3 = 50)
        self.assertEqual(invoke_final["state"]["combined"], stream_final["state"]["combined"])
        self.assertEqual(stream_final["state"]["combined"], 50)


if __name__ == '__main__':
    unittest.main()
