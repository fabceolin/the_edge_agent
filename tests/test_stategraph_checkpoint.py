import unittest
import tempfile
import os
import pickle

import the_edge_agent as tea


class TestStateGraphCheckpoint(unittest.TestCase):
    """Test checkpoint persistence functionality."""

    def setUp(self):
        self.graph = tea.StateGraph({"value": int})
        self.graph.add_node("node_a", run=lambda state: {"value": state["value"] + 10})
        self.graph.add_node("node_b", run=lambda state: {"value": state["value"] + 100})
        self.graph.set_entry_point("node_a")
        self.graph.add_edge("node_a", "node_b")
        self.graph.set_finish_point("node_b")
        self.graph.compile()

    def test_save_checkpoint_creates_valid_pickle_file(self):
        """Test that save_checkpoint creates a valid pickle file with expected structure."""
        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            state = {"value": 42}
            config = {"key": "val"}

            self.graph.save_checkpoint(ckpt_path, state, "node_a", config)

            # Verify file exists
            self.assertTrue(os.path.exists(ckpt_path))

            # Verify it's valid pickle
            with open(ckpt_path, "rb") as f:
                data = pickle.load(f)

            # Verify structure
            self.assertIn("state", data)
            self.assertIn("node", data)
            self.assertIn("config", data)
            self.assertIn("timestamp", data)
            self.assertIn("version", data)
            self.assertEqual(data["version"], "1.0")

    def test_load_checkpoint_returns_correct_structure(self):
        """Test that load_checkpoint returns dict with state, node, config, timestamp, version."""
        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            state = {"value": 42}
            config = {"key": "val"}

            self.graph.save_checkpoint(ckpt_path, state, "node_a", config)
            checkpoint = tea.StateGraph.load_checkpoint(ckpt_path)

            self.assertEqual(checkpoint["state"], {"value": 42})
            self.assertEqual(checkpoint["node"], "node_a")
            self.assertEqual(checkpoint["config"], {"key": "val"})
            self.assertIsInstance(checkpoint["timestamp"], float)
            self.assertEqual(checkpoint["version"], "1.0")

    def test_resume_from_checkpoint_continues_execution(self):
        """Test that resume_from_checkpoint continues from the saved node."""
        call_counts = {"node_a": 0, "node_b": 0}

        def node_a(state):
            call_counts["node_a"] += 1
            return {"value": state["value"] + 10}

        def node_b(state):
            call_counts["node_b"] += 1
            return {"value": state["value"] + 100}

        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=node_a)
        graph.add_node("node_b", run=node_b)
        graph.set_entry_point("node_a")
        graph.add_edge("node_a", "node_b")
        graph.set_finish_point("node_b")
        graph.compile()

        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            # Save checkpoint at node_b with state after node_a
            graph.save_checkpoint(ckpt_path, {"value": 15}, "node_b", {})

            events = list(graph.resume_from_checkpoint(ckpt_path))

            # Should only execute node_b (re-execute saved node)
            self.assertEqual(call_counts["node_a"], 0)
            self.assertEqual(call_counts["node_b"], 1)

            # Final state should be 15 + 100 = 115
            final_event = events[-1]
            self.assertEqual(final_event["type"], "final")
            self.assertEqual(final_event["state"]["value"], 115)

    def test_invoke_checkpoint_resumes_correctly(self):
        """Test that invoke(checkpoint=path) resumes from checkpoint."""
        call_counts = {"node_a": 0, "node_b": 0}

        def node_a(state):
            call_counts["node_a"] += 1
            return {"value": state["value"] + 10}

        def node_b(state):
            call_counts["node_b"] += 1
            return {"value": state["value"] + 100}

        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=node_a)
        graph.add_node("node_b", run=node_b)
        graph.set_entry_point("node_a")
        graph.add_edge("node_a", "node_b")
        graph.set_finish_point("node_b")
        graph.compile()

        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            graph.save_checkpoint(ckpt_path, {"value": 20}, "node_b", {})

            events = list(graph.invoke(checkpoint=ckpt_path))

            self.assertEqual(call_counts["node_a"], 0)
            self.assertEqual(call_counts["node_b"], 1)
            self.assertEqual(events[-1]["state"]["value"], 120)

    def test_stream_checkpoint_resumes_correctly(self):
        """Test that stream(checkpoint=path) resumes from checkpoint."""
        call_counts = {"node_a": 0, "node_b": 0}

        def node_a(state):
            call_counts["node_a"] += 1
            return {"value": state["value"] + 10}

        def node_b(state):
            call_counts["node_b"] += 1
            return {"value": state["value"] + 100}

        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=node_a)
        graph.add_node("node_b", run=node_b)
        graph.set_entry_point("node_a")
        graph.add_edge("node_a", "node_b")
        graph.set_finish_point("node_b")
        graph.compile()

        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            graph.save_checkpoint(ckpt_path, {"value": 20}, "node_b", {})

            events = list(graph.stream(checkpoint=ckpt_path))

            self.assertEqual(call_counts["node_a"], 0)
            self.assertEqual(call_counts["node_b"], 1)

            # Should have state event from node_b and final
            event_types = [e["type"] for e in events]
            self.assertIn("state", event_types)
            self.assertIn("final", event_types)

    def test_auto_save_at_interrupt_before(self):
        """Test that checkpoints are auto-saved at interrupt_before points."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=lambda state: {"value": state["value"] + 10})
        graph.add_node("node_b", run=lambda state: {"value": state["value"] + 100})
        graph.set_entry_point("node_a")
        graph.add_edge("node_a", "node_b")
        graph.set_finish_point("node_b")

        with tempfile.TemporaryDirectory() as tmpdir:
            graph.compile(interrupt_before=["node_b"], checkpoint_dir=tmpdir)

            events = list(graph.invoke({"value": 5}))

            # Should have interrupt and final
            event_types = [e["type"] for e in events]
            self.assertIn("interrupt", event_types)

            # Checkpoint should be created
            files = os.listdir(tmpdir)
            self.assertEqual(len(files), 1)
            self.assertTrue(files[0].startswith("node_b_"))
            self.assertTrue(files[0].endswith(".pkl"))

            # Verify checkpoint content
            ckpt = tea.StateGraph.load_checkpoint(os.path.join(tmpdir, files[0]))
            self.assertEqual(ckpt["node"], "node_b")
            self.assertEqual(ckpt["state"]["value"], 15)  # After node_a

    def test_auto_save_at_interrupt_after(self):
        """Test that checkpoints are auto-saved at interrupt_after points."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=lambda state: {"value": state["value"] + 10})
        graph.add_node("node_b", run=lambda state: {"value": state["value"] + 100})
        graph.set_entry_point("node_a")
        graph.add_edge("node_a", "node_b")
        graph.set_finish_point("node_b")

        with tempfile.TemporaryDirectory() as tmpdir:
            graph.compile(interrupt_after=["node_a"], checkpoint_dir=tmpdir)

            events = list(graph.invoke({"value": 5}))

            files = os.listdir(tmpdir)
            self.assertEqual(len(files), 1)

            ckpt = tea.StateGraph.load_checkpoint(os.path.join(tmpdir, files[0]))
            self.assertEqual(ckpt["node"], "node_a")
            self.assertEqual(ckpt["state"]["value"], 15)  # After node_a executed

    def test_error_handling_file_not_found(self):
        """Test that load_checkpoint raises FileNotFoundError for missing file."""
        with self.assertRaises(FileNotFoundError):
            tea.StateGraph.load_checkpoint("/nonexistent/path/checkpoint.pkl")

    def test_error_handling_corrupt_file(self):
        """Test that load_checkpoint raises ValueError for corrupt pickle file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            corrupt_path = os.path.join(tmpdir, "corrupt.pkl")
            with open(corrupt_path, "wb") as f:
                f.write(b"not valid pickle data")

            with self.assertRaises(ValueError) as context:
                tea.StateGraph.load_checkpoint(corrupt_path)
            self.assertIn("Corrupt or incompatible", str(context.exception))

    def test_error_handling_incompatible_checkpoint(self):
        """Test that load_checkpoint raises ValueError for checkpoint missing required keys."""
        with tempfile.TemporaryDirectory() as tmpdir:
            incomplete_path = os.path.join(tmpdir, "incomplete.pkl")
            with open(incomplete_path, "wb") as f:
                pickle.dump({"state": {}, "node": "test"}, f)  # Missing config, timestamp, version

            with self.assertRaises(ValueError) as context:
                tea.StateGraph.load_checkpoint(incomplete_path)
            self.assertIn("missing keys", str(context.exception))

    def test_existing_tests_still_pass(self):
        """Verify basic invoke/stream still work without checkpoints."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=lambda state: {"value": state["value"] + 10})
        graph.set_entry_point("node_a")
        graph.set_finish_point("node_a")
        graph.compile()

        # invoke should work
        events = list(graph.invoke({"value": 5}))
        self.assertEqual(events[-1]["state"]["value"], 15)

        # stream should work
        events = list(graph.stream({"value": 5}))
        final = [e for e in events if e["type"] == "final"][0]
        self.assertEqual(final["state"]["value"], 15)

    def test_checkpoint_at_fanin_includes_parallel_results(self):
        """Test that checkpoint at fan-in node includes parallel_results."""
        def flow_a(state):
            return {"flow_a_result": state["value"] * 2}

        def flow_b(state):
            return {"flow_b_result": state["value"] * 3}

        def fan_in(state, parallel_results):
            total = sum(r.get("flow_a_result", 0) + r.get("flow_b_result", 0) for r in parallel_results)
            return {"total": total}

        with tempfile.TemporaryDirectory() as tmpdir:
            graph = tea.StateGraph({"value": int})
            graph.add_node("start", run=lambda state: state)
            graph.add_node("flow_a", run=flow_a)
            graph.add_node("flow_b", run=flow_b)
            graph.add_fanin_node("fan_in", run=fan_in)
            graph.add_node("end", run=lambda state: state)

            graph.set_entry_point("start")
            graph.add_parallel_edge("start", "flow_a", "fan_in")
            graph.add_parallel_edge("start", "flow_b", "fan_in")
            graph.add_edge("flow_a", "fan_in")
            graph.add_edge("flow_b", "fan_in")
            graph.add_edge("fan_in", "end")
            graph.set_finish_point("end")

            graph.compile(interrupt_before=["fan_in"], checkpoint_dir=tmpdir)

            events = list(graph.invoke({"value": 10}))

            files = os.listdir(tmpdir)
            self.assertEqual(len(files), 1)

            ckpt = tea.StateGraph.load_checkpoint(os.path.join(tmpdir, files[0]))

            # Should have parallel_results in state
            self.assertIn("parallel_results", ckpt["state"])
            pr = ckpt["state"]["parallel_results"]

            # Verify branch results are captured
            has_flow_a = any("flow_a_result" in r for r in pr)
            has_flow_b = any("flow_b_result" in r for r in pr)
            self.assertTrue(has_flow_a, "flow_a_result missing from parallel_results")
            self.assertTrue(has_flow_b, "flow_b_result missing from parallel_results")

    def test_config_merge_on_resume(self):
        """Test that resume_from_checkpoint merges provided config with saved config."""
        config_seen = {}

        def capture_config(state, config):
            config_seen.update(config)
            return {"value": state["value"] + 1}

        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=capture_config)
        graph.set_entry_point("node_a")
        graph.set_finish_point("node_a")
        graph.compile()

        with tempfile.TemporaryDirectory() as tmpdir:
            ckpt_path = os.path.join(tmpdir, "test.pkl")
            # Save with original config
            graph.save_checkpoint(ckpt_path, {"value": 1}, "node_a", {"saved_key": "saved_val"})

            # Resume with override config
            list(graph.resume_from_checkpoint(ckpt_path, config={"override_key": "new_val"}))

            # Both keys should be present (merged)
            self.assertIn("saved_key", config_seen)
            self.assertIn("override_key", config_seen)

    def test_no_checkpoint_when_checkpoint_dir_none(self):
        """Test that no checkpoint files are created when checkpoint_dir is None."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("node_a", run=lambda state: state)
        graph.set_entry_point("node_a")
        graph.set_finish_point("node_a")

        with tempfile.TemporaryDirectory() as tmpdir:
            # Compile WITHOUT checkpoint_dir
            graph.compile(interrupt_before=["node_a"])

            list(graph.invoke({"value": 5}))

            # No files should be created in tmpdir (we didn't set checkpoint_dir)
            # The tmpdir was just created for checking, nothing should be in it
            self.assertEqual(len(os.listdir(tmpdir)), 0)


if __name__ == '__main__':
    unittest.main()
