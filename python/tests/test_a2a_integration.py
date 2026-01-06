"""
Integration tests for A2A Communication.

Tests complete workflows involving multiple agents communicating.

Story: TEA-AGENT-001.5 (Task 8)
"""

import threading
import time
import unittest
from unittest.mock import MagicMock

from the_edge_agent.a2a.message_queue import (
    get_global_queue,
    reset_global_queue,
    Message,
)
from the_edge_agent.a2a.shared_state import (
    get_global_state,
    reset_global_state,
)
from the_edge_agent.a2a.discovery import (
    get_global_discovery,
    reset_global_discovery,
)
from the_edge_agent.actions.a2a_actions import register_actions


class TestMultiAgentWorkflow(unittest.TestCase):
    """Test multi-agent workflow with coordinator and workers."""

    def setUp(self):
        """Reset global singletons."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)

    def tearDown(self):
        """Clean up."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

    def test_coordinator_worker_pattern(self):
        """Test coordinator distributing tasks to workers and collecting results."""
        namespace = "research-team"
        results_collected = []

        # Register workers
        for i in range(3):
            state = {"_namespace": namespace}
            self.registry["a2a.register"](
                state,
                agent_id=f"worker-{i}",
                capabilities=["search"],
                agent_type="worker",
            )

        # Coordinator broadcasts task
        coordinator_state = {
            "_agent_id": "coordinator",
            "_namespace": namespace,
        }

        # Discover available workers
        discovered = self.registry["a2a.discover"](
            coordinator_state, agent_type="worker"
        )
        worker_ids = [a["agent_id"] for a in discovered["a2a_agents"]]
        self.assertEqual(len(worker_ids), 3)

        # Assign tasks to each worker
        for worker_id in worker_ids:
            self.registry["a2a.send"](
                coordinator_state,
                to=worker_id,
                message={
                    "type": "task_assignment",
                    "payload": {"task": f"search_{worker_id}"},
                },
            )

        # Workers receive tasks and respond
        for worker_id in worker_ids:
            worker_state = {"_agent_id": worker_id, "_namespace": namespace}

            # Receive task
            received = self.registry["a2a.receive"](worker_state)
            self.assertEqual(len(received["a2a_messages"]), 1)
            task = received["a2a_messages"][0]["payload"]["task"]

            # Send result
            self.registry["a2a.send"](
                worker_state,
                to="coordinator",
                message={
                    "type": "task_result",
                    "payload": {"result": f"completed_{task}"},
                },
            )

        # Coordinator collects all results
        all_results = self.registry["a2a.receive"](
            coordinator_state,
            from_agents=worker_ids,
            type="task_result",
        )
        self.assertEqual(len(all_results["a2a_messages"]), 3)

    def test_delegation_round_trip(self):
        """Test delegation pattern with specialist agent."""
        namespace = "test-delegation"

        # Register search specialist
        spec_state = {"_namespace": namespace}
        self.registry["a2a.register"](
            spec_state,
            agent_id="search-specialist",
            capabilities=["search"],
            agent_type="specialist",
        )

        # Run specialist in background thread
        specialist_done = threading.Event()

        def specialist_loop():
            queue = get_global_queue()
            try:
                messages = queue.receive(
                    agent_id="search-specialist",
                    namespace=namespace,
                    timeout=1.0,
                )
                for msg in messages:
                    if msg.type == "search":
                        # Process search and respond
                        queue.send(
                            Message(
                                from_agent="search-specialist",
                                to_agent=msg.from_agent,
                                namespace=namespace,
                                type="search_result",
                                payload={
                                    "results": [
                                        "found: " + msg.payload.get("query", "")
                                    ]
                                },
                                correlation_id=msg.correlation_id,
                            )
                        )
            finally:
                specialist_done.set()

        thread = threading.Thread(target=specialist_loop)
        thread.start()

        # Coordinator delegates search
        coordinator_state = {"_agent_id": "coordinator", "_namespace": namespace}
        result = self.registry["a2a.delegate"](
            coordinator_state,
            to="search-specialist",
            task={"type": "search", "query": "test query"},
            timeout="2s",
        )

        thread.join(timeout=3)

        self.assertTrue(result["a2a_delegation_success"])
        self.assertIn(
            "found: test query", result["a2a_delegation_result"]["results"][0]
        )

    def test_broadcast_consensus(self):
        """Test broadcast-based consensus pattern."""
        namespace = "consensus-test"
        votes_collected = []

        # Register voters
        voter_ids = ["voter-1", "voter-2", "voter-3"]
        for voter_id in voter_ids:
            state = {"_namespace": namespace}
            self.registry["a2a.register"](state, agent_id=voter_id, agent_type="voter")

        # Proposer broadcasts proposal
        proposer_state = {"_agent_id": "proposer", "_namespace": namespace}
        self.registry["a2a.register"](
            proposer_state, agent_id="proposer", agent_type="proposer"
        )

        broadcast_result = self.registry["a2a.broadcast"](
            proposer_state,
            message={
                "type": "proposal",
                "payload": {"proposal": "accept_change", "proposal_id": "p1"},
            },
            agent_type_filter="voter",
        )
        self.assertEqual(broadcast_result["a2a_broadcast_count"], 3)

        # Each voter receives and responds
        for voter_id in voter_ids:
            voter_state = {"_agent_id": voter_id, "_namespace": namespace}

            received = self.registry["a2a.receive"](voter_state)
            self.assertEqual(len(received["a2a_messages"]), 1)

            # Vote yes
            self.registry["a2a.send"](
                voter_state,
                to="proposer",
                message={
                    "type": "vote",
                    "payload": {"vote": "yes", "voter": voter_id},
                },
            )

        # Proposer collects votes
        votes = self.registry["a2a.receive"](
            proposer_state,
            from_agents=voter_ids,
            type="vote",
            require_all=True,
            timeout="1s",
        )
        self.assertEqual(len(votes["a2a_messages"]), 3)
        all_yes = all(m["payload"]["vote"] == "yes" for m in votes["a2a_messages"])
        self.assertTrue(all_yes)

    def test_shared_state_coordination(self):
        """Test agents coordinating via shared state."""
        namespace = "shared-state-test"

        # Multiple agents update shared progress
        total_tasks = 10
        self.registry["a2a.state.set"](
            {"_namespace": namespace},
            key="task_progress",
            value={"completed": 0, "total": total_tasks},
        )

        # Workers increment completed count
        workers = 3
        threads = []

        def worker_task(worker_id):
            state = {"_namespace": namespace}
            for _ in range(3):
                # Read current progress
                current = self.registry["a2a.state.get"](state, key="task_progress")
                progress = current["a2a_shared_state"]
                version = current["a2a_state_version"]

                # Attempt to update with optimistic locking
                new_progress = {
                    "completed": progress["completed"] + 1,
                    "total": progress["total"],
                }

                # May fail due to concurrent update - that's expected
                self.registry["a2a.state.set"](
                    state,
                    key="task_progress",
                    value=new_progress,
                    expected_version=version,
                )
                time.sleep(0.01)

        for i in range(workers):
            t = threading.Thread(target=worker_task, args=(f"worker-{i}",))
            threads.append(t)
            t.start()

        for t in threads:
            t.join()

        # Final progress should be updated (may not be 9 due to conflicts)
        final = self.registry["a2a.state.get"](
            {"_namespace": namespace}, key="task_progress"
        )
        self.assertGreater(final["a2a_shared_state"]["completed"], 0)
        self.assertLessEqual(final["a2a_shared_state"]["completed"], 9)

    def test_timeout_handling(self):
        """Test timeout handling in multi-agent scenario."""
        namespace = "timeout-test"

        # Try to receive from non-existent agents
        state = {"_agent_id": "coordinator", "_namespace": namespace}

        start = time.time()
        result = self.registry["a2a.receive"](
            state,
            from_agents=["ghost-agent"],
            require_all=True,
            timeout="0.2s",
        )
        elapsed = time.time() - start

        self.assertTrue(result.get("a2a_timeout"))
        self.assertGreaterEqual(elapsed, 0.19)
        self.assertIn("ghost-agent", result.get("a2a_error", ""))


class TestNamespaceIsolation(unittest.TestCase):
    """Test namespace isolation between workflows."""

    def setUp(self):
        """Reset global singletons."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)

    def tearDown(self):
        """Clean up."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

    def test_messages_isolated_by_namespace(self):
        """Test messages don't cross namespace boundaries."""
        # Send in ns1
        state1 = {"_agent_id": "sender", "_namespace": "ns1"}
        self.registry["a2a.send"](
            state1, to="receiver", message={"type": "test", "payload": {"ns": "ns1"}}
        )

        # Send in ns2
        state2 = {"_agent_id": "sender", "_namespace": "ns2"}
        self.registry["a2a.send"](
            state2, to="receiver", message={"type": "test", "payload": {"ns": "ns2"}}
        )

        # Receive in ns1
        recv1 = {"_agent_id": "receiver", "_namespace": "ns1"}
        result1 = self.registry["a2a.receive"](recv1)
        self.assertEqual(len(result1["a2a_messages"]), 1)
        self.assertEqual(result1["a2a_messages"][0]["payload"]["ns"], "ns1")

        # Receive in ns2
        recv2 = {"_agent_id": "receiver", "_namespace": "ns2"}
        result2 = self.registry["a2a.receive"](recv2)
        self.assertEqual(len(result2["a2a_messages"]), 1)
        self.assertEqual(result2["a2a_messages"][0]["payload"]["ns"], "ns2")

    def test_shared_state_isolated_by_namespace(self):
        """Test shared state doesn't cross namespaces."""
        state1 = {"_namespace": "ns1"}
        state2 = {"_namespace": "ns2"}

        self.registry["a2a.state.set"](state1, key="data", value="ns1-data")
        self.registry["a2a.state.set"](state2, key="data", value="ns2-data")

        result1 = self.registry["a2a.state.get"](state1, key="data")
        result2 = self.registry["a2a.state.get"](state2, key="data")

        self.assertEqual(result1["a2a_shared_state"], "ns1-data")
        self.assertEqual(result2["a2a_shared_state"], "ns2-data")

    def test_discovery_isolated_by_namespace(self):
        """Test discovery only finds agents in same namespace."""
        # Register in ns1
        self.registry["a2a.register"](
            {"_namespace": "ns1"}, agent_id="worker-ns1", agent_type="worker"
        )

        # Register in ns2
        self.registry["a2a.register"](
            {"_namespace": "ns2"}, agent_id="worker-ns2", agent_type="worker"
        )

        # Discover in ns1
        result1 = self.registry["a2a.discover"]({"_namespace": "ns1"})
        agent_ids = [a["agent_id"] for a in result1["a2a_agents"]]
        self.assertIn("worker-ns1", agent_ids)
        self.assertNotIn("worker-ns2", agent_ids)

    def test_broadcast_isolated_by_namespace(self):
        """Test broadcast only reaches agents in same namespace."""
        queue = get_global_queue()

        # Register agents in different namespaces
        queue.register_agent("worker1", "ns1", "worker")
        queue.register_agent("worker2", "ns2", "worker")

        # Broadcast in ns1
        state = {"_agent_id": "coordinator", "_namespace": "ns1"}
        result = self.registry["a2a.broadcast"](state, message={"type": "test"})
        self.assertEqual(result["a2a_broadcast_count"], 1)


class TestCorrelationIdMatching(unittest.TestCase):
    """Test correlation ID matching in request/response patterns."""

    def setUp(self):
        """Reset global singletons."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)

    def tearDown(self):
        """Clean up."""
        reset_global_queue()
        reset_global_state()
        reset_global_discovery()

    def test_mismatched_correlation_id_ignored(self):
        """Test that responses with wrong correlation ID are not matched."""
        namespace = "corr-test"
        queue = get_global_queue()

        # Simulate a responder sending with wrong correlation ID
        def respond_wrong_corr():
            time.sleep(0.05)
            messages = queue.receive(
                agent_id="responder",
                namespace=namespace,
                timeout=0.5,
            )
            if messages:
                # Respond with WRONG correlation ID
                queue.send(
                    Message(
                        from_agent="responder",
                        to_agent="requester",
                        namespace=namespace,
                        type="response",
                        payload={"result": "wrong corr"},
                        correlation_id="wrong_corr_id",
                    )
                )

        thread = threading.Thread(target=respond_wrong_corr)
        thread.start()

        # Delegation should timeout since correlation ID doesn't match
        state = {"_agent_id": "requester", "_namespace": namespace}
        result = self.registry["a2a.delegate"](
            state,
            to="responder",
            task={"type": "request"},
            timeout="0.3s",
            on_timeout="raise",
        )

        thread.join()

        self.assertFalse(result["a2a_delegation_success"])
        self.assertTrue(result.get("a2a_delegation_timeout"))


if __name__ == "__main__":
    unittest.main()
