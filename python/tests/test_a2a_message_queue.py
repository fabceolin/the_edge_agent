"""
Unit tests for A2A Message Queue.

Story: TEA-AGENT-001.5 (AC: 5)
"""

import json
import time
import threading
import unittest
from datetime import datetime

from the_edge_agent.a2a.message_queue import (
    Message,
    InMemoryMessageQueue,
    MessageTimeoutError,
    get_global_queue,
    reset_global_queue,
)


class TestMessage(unittest.TestCase):
    """Tests for Message dataclass."""

    def test_message_creation(self):
        """Test basic message creation."""
        msg = Message(
            from_agent="worker-1",
            to_agent="coordinator",
            namespace="test-ns",
            type="status_update",
            payload={"progress": 50},
        )
        self.assertEqual(msg.from_agent, "worker-1")
        self.assertEqual(msg.to_agent, "coordinator")
        self.assertEqual(msg.namespace, "test-ns")
        self.assertEqual(msg.type, "status_update")
        self.assertEqual(msg.payload, {"progress": 50})
        self.assertIsNotNone(msg.id)
        self.assertTrue(msg.id.startswith("msg_"))
        self.assertIsNone(msg.correlation_id)
        self.assertIsNone(msg.ttl)

    def test_message_with_correlation_id(self):
        """Test message with correlation ID for request/response."""
        msg = Message(
            from_agent="worker-1",
            to_agent="coordinator",
            namespace="test-ns",
            type="task_result",
            payload={"result": "success"},
            correlation_id="req_123",
        )
        self.assertEqual(msg.correlation_id, "req_123")

    def test_message_with_ttl(self):
        """Test message with TTL."""
        msg = Message(
            from_agent="worker-1",
            to_agent="coordinator",
            namespace="test-ns",
            type="status",
            payload={},
            ttl=60,
        )
        self.assertEqual(msg.ttl, 60)
        self.assertFalse(msg.is_expired())

    def test_message_to_dict(self):
        """Test message serialization to dict."""
        msg = Message(
            from_agent="worker-1",
            to_agent="coordinator",
            namespace="test-ns",
            type="test",
            payload={"key": "value"},
        )
        d = msg.to_dict()
        self.assertEqual(d["from_agent"], "worker-1")
        self.assertEqual(d["to_agent"], "coordinator")
        self.assertEqual(d["namespace"], "test-ns")
        self.assertEqual(d["type"], "test")
        self.assertEqual(d["payload"], {"key": "value"})

    def test_message_json_round_trip(self):
        """Test message JSON serialization/deserialization."""
        original = Message(
            from_agent="worker-1",
            to_agent="coordinator",
            namespace="test-ns",
            type="test",
            payload={"nested": {"key": [1, 2, 3]}},
            correlation_id="corr_123",
            ttl=300,
        )
        json_str = original.to_json()
        restored = Message.from_json(json_str)

        self.assertEqual(restored.from_agent, original.from_agent)
        self.assertEqual(restored.to_agent, original.to_agent)
        self.assertEqual(restored.namespace, original.namespace)
        self.assertEqual(restored.type, original.type)
        self.assertEqual(restored.payload, original.payload)
        self.assertEqual(restored.correlation_id, original.correlation_id)
        self.assertEqual(restored.ttl, original.ttl)

    def test_message_expiration(self):
        """Test TTL-based message expiration."""
        # Create message with very short TTL
        msg = Message(
            from_agent="a",
            to_agent="b",
            namespace="ns",
            type="test",
            payload={},
            ttl=0,  # Expires immediately
        )
        # Give time for expiration
        time.sleep(0.01)
        self.assertTrue(msg.is_expired())

    def test_message_no_expiration(self):
        """Test message without TTL doesn't expire."""
        msg = Message(
            from_agent="a",
            to_agent="b",
            namespace="ns",
            type="test",
            payload={},
            ttl=None,
        )
        self.assertFalse(msg.is_expired())


class TestInMemoryMessageQueue(unittest.TestCase):
    """Tests for InMemoryMessageQueue."""

    def setUp(self):
        """Create fresh queue for each test."""
        self.queue = InMemoryMessageQueue()

    def test_send_and_receive(self):
        """Test basic send and receive."""
        msg = Message(
            from_agent="sender",
            to_agent="receiver",
            namespace="test",
            type="greeting",
            payload={"text": "hello"},
        )
        self.queue.send(msg)

        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
        )
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0].payload, {"text": "hello"})

    def test_send_with_confirmation(self):
        """Test send with delivery confirmation."""
        msg = Message(
            from_agent="sender",
            to_agent="receiver",
            namespace="test",
            type="important",
            payload={},
        )
        msg_id = self.queue.send(msg, confirm=True)
        self.assertEqual(msg_id, msg.id)

    def test_receive_filters_by_sender(self):
        """Test receive filters by from_agents."""
        self.queue.send(
            Message(
                from_agent="alice",
                to_agent="receiver",
                namespace="test",
                type="msg",
                payload={"from": "alice"},
            )
        )
        self.queue.send(
            Message(
                from_agent="bob",
                to_agent="receiver",
                namespace="test",
                type="msg",
                payload={"from": "bob"},
            )
        )

        # Only receive from alice
        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
            from_agents=["alice"],
        )
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0].from_agent, "alice")

        # Bob's message still in queue
        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
        )
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0].from_agent, "bob")

    def test_receive_filters_by_type(self):
        """Test receive filters by message type."""
        self.queue.send(
            Message(
                from_agent="sender",
                to_agent="receiver",
                namespace="test",
                type="status",
                payload={"status": "ok"},
            )
        )
        self.queue.send(
            Message(
                from_agent="sender",
                to_agent="receiver",
                namespace="test",
                type="error",
                payload={"error": "oops"},
            )
        )

        # Only receive errors
        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
            message_type="error",
        )
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0].type, "error")

    def test_receive_timeout_no_messages(self):
        """Test receive returns empty list after timeout."""
        start = time.time()
        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
            timeout=0.1,
        )
        elapsed = time.time() - start

        self.assertEqual(len(received), 0)
        self.assertGreaterEqual(elapsed, 0.09)

    def test_receive_require_all(self):
        """Test require_all waits for all specified agents."""
        # Send from one agent
        self.queue.send(
            Message(
                from_agent="alice",
                to_agent="receiver",
                namespace="test",
                type="vote",
                payload={"vote": "yes"},
            )
        )

        # Try to receive from both - should timeout
        with self.assertRaises(MessageTimeoutError) as ctx:
            self.queue.receive(
                agent_id="receiver",
                namespace="test",
                from_agents=["alice", "bob"],
                require_all=True,
                timeout=0.1,
            )
        self.assertIn("bob", str(ctx.exception))

    def test_receive_require_all_success(self):
        """Test require_all succeeds when all messages arrive."""
        self.queue.send(
            Message(
                from_agent="alice",
                to_agent="receiver",
                namespace="test",
                type="vote",
                payload={"vote": "yes"},
            )
        )
        self.queue.send(
            Message(
                from_agent="bob",
                to_agent="receiver",
                namespace="test",
                type="vote",
                payload={"vote": "no"},
            )
        )

        received = self.queue.receive(
            agent_id="receiver",
            namespace="test",
            from_agents=["alice", "bob"],
            require_all=True,
        )
        self.assertEqual(len(received), 2)

    def test_namespace_isolation(self):
        """Test messages don't cross namespaces."""
        self.queue.send(
            Message(
                from_agent="sender",
                to_agent="receiver",
                namespace="ns1",
                type="test",
                payload={"ns": "ns1"},
            )
        )
        self.queue.send(
            Message(
                from_agent="sender",
                to_agent="receiver",
                namespace="ns2",
                type="test",
                payload={"ns": "ns2"},
            )
        )

        # Receive from ns1 only
        received = self.queue.receive(
            agent_id="receiver",
            namespace="ns1",
        )
        self.assertEqual(len(received), 1)
        self.assertEqual(received[0].payload["ns"], "ns1")

    def test_broadcast(self):
        """Test broadcast sends to all registered agents."""
        # Register agents
        self.queue.register_agent("worker-1", "test", "worker")
        self.queue.register_agent("worker-2", "test", "worker")
        self.queue.register_agent("manager", "test", "manager")

        # Broadcast from coordinator
        msg = Message(
            from_agent="coordinator",
            to_agent="*",
            namespace="test",
            type="announcement",
            payload={"message": "All hands meeting!"},
        )
        count = self.queue.broadcast(msg)
        self.assertEqual(count, 3)

        # Each agent should have received
        for agent in ["worker-1", "worker-2", "manager"]:
            received = self.queue.receive(agent_id=agent, namespace="test")
            self.assertEqual(len(received), 1)
            self.assertEqual(received[0].type, "announcement")

    def test_broadcast_with_type_filter(self):
        """Test broadcast with agent type filter."""
        self.queue.register_agent("worker-1", "test", "worker")
        self.queue.register_agent("worker-2", "test", "worker")
        self.queue.register_agent("manager", "test", "manager")

        msg = Message(
            from_agent="coordinator",
            to_agent="*",
            namespace="test",
            type="work_order",
            payload={"task": "do work"},
        )
        count = self.queue.broadcast(msg, agent_type_filter="worker")
        self.assertEqual(count, 2)

        # Workers should have received
        for agent in ["worker-1", "worker-2"]:
            received = self.queue.receive(agent_id=agent, namespace="test")
            self.assertEqual(len(received), 1)

        # Manager should not have received
        received = self.queue.receive(agent_id="manager", namespace="test")
        self.assertEqual(len(received), 0)

    def test_broadcast_excludes_sender(self):
        """Test broadcast doesn't send to the sender."""
        self.queue.register_agent("coordinator", "test", "coordinator")
        self.queue.register_agent("worker", "test", "worker")

        msg = Message(
            from_agent="coordinator",
            to_agent="*",
            namespace="test",
            type="announcement",
            payload={},
        )
        count = self.queue.broadcast(msg)
        self.assertEqual(count, 1)

        # Coordinator should not have received their own broadcast
        received = self.queue.receive(agent_id="coordinator", namespace="test")
        self.assertEqual(len(received), 0)

    def test_clear_namespace(self):
        """Test clearing all messages in a namespace."""
        for i in range(5):
            self.queue.send(
                Message(
                    from_agent="sender",
                    to_agent=f"receiver-{i}",
                    namespace="test",
                    type="test",
                    payload={},
                )
            )
        self.queue.send(
            Message(
                from_agent="sender",
                to_agent="receiver",
                namespace="other",
                type="test",
                payload={},
            )
        )

        count = self.queue.clear_namespace("test")
        self.assertEqual(count, 5)

        # Other namespace unaffected
        received = self.queue.receive(agent_id="receiver", namespace="other")
        self.assertEqual(len(received), 1)

    def test_expired_messages_not_received(self):
        """Test expired messages are not returned."""
        msg = Message(
            from_agent="sender",
            to_agent="receiver",
            namespace="test",
            type="ephemeral",
            payload={},
            ttl=0,  # Expires immediately
        )
        self.queue.send(msg)

        # Wait for expiration
        time.sleep(0.01)

        received = self.queue.receive(agent_id="receiver", namespace="test")
        self.assertEqual(len(received), 0)

    def test_queue_size(self):
        """Test queue size reporting."""
        for i in range(3):
            self.queue.send(
                Message(
                    from_agent="sender",
                    to_agent="receiver",
                    namespace="test",
                    type="test",
                    payload={"n": i},
                )
            )

        size = self.queue.get_queue_size("test", "receiver")
        self.assertEqual(size, 3)

        # Consume one
        self.queue.receive(agent_id="receiver", namespace="test")

        size = self.queue.get_queue_size("test", "receiver")
        self.assertEqual(size, 0)

    def test_thread_safety(self):
        """Test concurrent send/receive operations."""
        results = {"sent": 0, "received": 0}
        lock = threading.Lock()

        def sender():
            for i in range(100):
                self.queue.send(
                    Message(
                        from_agent="sender",
                        to_agent="receiver",
                        namespace="test",
                        type="test",
                        payload={"n": i},
                    )
                )
                with lock:
                    results["sent"] += 1

        def receiver():
            total = 0
            while total < 100:
                msgs = self.queue.receive(
                    agent_id="receiver",
                    namespace="test",
                    timeout=0.1,
                )
                total += len(msgs)
            with lock:
                results["received"] = total

        sender_thread = threading.Thread(target=sender)
        receiver_thread = threading.Thread(target=receiver)

        sender_thread.start()
        receiver_thread.start()

        sender_thread.join()
        receiver_thread.join()

        self.assertEqual(results["sent"], 100)
        self.assertEqual(results["received"], 100)


class TestGlobalQueue(unittest.TestCase):
    """Tests for global queue singleton."""

    def setUp(self):
        """Reset global queue before each test."""
        reset_global_queue()

    def tearDown(self):
        """Clean up global queue."""
        reset_global_queue()

    def test_global_queue_singleton(self):
        """Test global queue is a singleton."""
        q1 = get_global_queue()
        q2 = get_global_queue()
        self.assertIs(q1, q2)

    def test_reset_global_queue(self):
        """Test resetting global queue creates new instance."""
        q1 = get_global_queue()
        q1.send(
            Message(
                from_agent="a",
                to_agent="b",
                namespace="test",
                type="test",
                payload={},
            )
        )
        reset_global_queue()
        q2 = get_global_queue()

        self.assertIsNot(q1, q2)
        # New queue should be empty
        received = q2.receive(agent_id="b", namespace="test")
        self.assertEqual(len(received), 0)


if __name__ == "__main__":
    unittest.main()
