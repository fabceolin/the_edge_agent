
import unittest
from the_edge_agent import StateGraph, MemoryCheckpointer
import logging

class TestInterruptStateUpdate(unittest.TestCase):
    def test_interrupt_before_and_resume_with_state_update(self):
        """
        Test if we can update state when resuming from interrupt_before.
        LangGraph allows updating state when resuming.
        """
        def step1(state):
            return {"count": state["count"] + 1}

        def step2(state):
            return {"count": state["count"] + 1, "approved": state.get("approved", False)}

        graph = StateGraph({"count": int, "approved": bool})
        graph.add_node("step1", step1)
        graph.add_node("step2", step2)
        graph.set_entry_point("step1")
        graph.add_edge("step1", "step2")
        graph.set_finish_point("step2")

        checkpointer = MemoryCheckpointer()
        compiled_graph = graph.compile(interrupt_before=["step2"], checkpointer=checkpointer)

        # Run until interrupt
        events = list(compiled_graph.invoke({"count": 0}))
        self.assertEqual(len(events), 1)
        self.assertEqual(events[0]["type"], "interrupt")
        self.assertEqual(events[0]["node"], "step2")
        self.assertEqual(events[0]["state"]["count"], 1)

        checkpoint_path = events[0]["checkpoint_path"]

        # Resume with state update attempt
        # We pass input_state to invoke, hoping it merges or updates the state
        resume_events = list(compiled_graph.invoke({"approved": True}, checkpoint=checkpoint_path))

        final_state = resume_events[-1]["state"]

        self.assertTrue(final_state["approved"], "State was not updated during resume")
        self.assertEqual(final_state["count"], 2)

    def test_stream_interrupt_resume_with_state_update(self):
        """
        Test state update during resume with streaming.
        """
        def step1(state):
            return {"count": state["count"] + 1}

        def step2(state):
            return {"count": state["count"] + 1, "approved": state.get("approved", False)}

        graph = StateGraph({"count": int, "approved": bool})
        graph.add_node("step1", step1)
        graph.add_node("step2", step2)
        graph.set_entry_point("step1")
        graph.add_edge("step1", "step2")
        graph.set_finish_point("step2")

        checkpointer = MemoryCheckpointer()
        compiled_graph = graph.compile(interrupt_before=["step2"], checkpointer=checkpointer)

        events = list(compiled_graph.stream({"count": 0}))
        checkpoint_path = events[-1]["checkpoint_path"]

        resume_events = list(compiled_graph.stream({"approved": True}, checkpoint=checkpoint_path))
        final_state = resume_events[-1]["state"]

        self.assertTrue(final_state["approved"])
        self.assertEqual(final_state["count"], 2)

if __name__ == "__main__":
    unittest.main()
