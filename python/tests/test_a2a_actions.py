"""
Unit tests for A2A Communication Actions.

Story: TEA-AGENT-001.5 (AC: 1-7)
"""

import time
import threading
import unittest
from unittest.mock import MagicMock, patch

from the_edge_agent.a2a.message_queue import (
    Message,
    get_global_queue,
    reset_global_queue,
)
from the_edge_agent.a2a.shared_state import (
    get_global_state,
    reset_global_state,
    OptimisticLockError,
)
from the_edge_agent.a2a.discovery import (
    get_global_discovery,
    reset_global_discovery,
)
from the_edge_agent.actions.a2a_actions import register_actions


class TestA2AActionsBase(unittest.TestCase):
    """Base class for A2A action tests."""

    def setUp(self):
        """Reset global singletons and build registry."""
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


class TestA2ASend(TestA2AActionsBase):
    """Tests for a2a.send action (AC: 1)."""

    def test_send_basic_message(self):
        """Test sending basic message to another agent."""
        state = {
            "_agent_id": "worker-1",
            "_namespace": "test-ns",
        }
        result = self.registry["a2a.send"](
            state,
            to="coordinator",
            message={"type": "status_update", "payload": {"progress": 50}},
        )
        self.assertTrue(result["a2a_message_sent"])

        # Verify message is in queue
        queue = get_global_queue()
        messages = queue.receive(agent_id="coordinator", namespace="test-ns")
        self.assertEqual(len(messages), 1)
        self.assertEqual(messages[0].type, "status_update")
        self.assertEqual(messages[0].payload, {"progress": 50})

    def test_send_with_confirmation(self):
        """Test send with delivery confirmation."""
        state = {"_agent_id": "worker", "_namespace": "test"}
        result = self.registry["a2a.send"](
            state,
            to="coordinator",
            message={"type": "important"},
            confirm=True,
        )
        self.assertTrue(result["a2a_message_sent"])
        self.assertIn("a2a_message_id", result)

    def test_send_with_correlation_id(self):
        """Test send with correlation ID for request/response."""
        state = {"_agent_id": "worker", "_namespace": "test"}
        result = self.registry["a2a.send"](
            state,
            to="coordinator",
            message={"type": "request"},
            correlation_id="req_123",
        )
        self.assertTrue(result["a2a_message_sent"])

        queue = get_global_queue()
        messages = queue.receive(agent_id="coordinator", namespace="test")
        self.assertEqual(messages[0].correlation_id, "req_123")

    def test_send_with_ttl(self):
        """Test send with TTL."""
        state = {"_agent_id": "worker", "_namespace": "test"}
        self.registry["a2a.send"](
            state,
            to="coordinator",
            message={"type": "ephemeral"},
            ttl=60,
        )

        queue = get_global_queue()
        messages = queue.receive(agent_id="coordinator", namespace="test")
        self.assertEqual(messages[0].ttl, 60)

    def test_send_uses_settings_config(self):
        """Test send uses settings.a2a configuration."""
        state = {
            "settings": {
                "a2a": {
                    "agent_id": "config-worker",
                    "namespace": "config-ns",
                }
            }
        }
        self.registry["a2a.send"](state, to="coordinator", message={"type": "test"})

        queue = get_global_queue()
        messages = queue.receive(agent_id="coordinator", namespace="config-ns")
        self.assertEqual(len(messages), 1)
        self.assertEqual(messages[0].from_agent, "config-worker")


class TestA2AReceive(TestA2AActionsBase):
    """Tests for a2a.receive action (AC: 2)."""

    def test_receive_basic(self):
        """Test basic message receive."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}

        # Send message
        queue = get_global_queue()
        queue.send(
            Message(
                from_agent="worker",
                to_agent="coordinator",
                namespace="test",
                type="status",
                payload={"done": True},
            )
        )

        result = self.registry["a2a.receive"](state)
        self.assertEqual(len(result["a2a_messages"]), 1)
        self.assertEqual(result["a2a_messages"][0]["type"], "status")

    def test_receive_with_timeout(self):
        """Test receive with timeout."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}

        start = time.time()
        result = self.registry["a2a.receive"](state, timeout="0.1s")
        elapsed = time.time() - start

        self.assertEqual(len(result["a2a_messages"]), 0)
        self.assertGreaterEqual(elapsed, 0.09)

    def test_receive_filter_by_type(self):
        """Test receive filters by message type."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        queue.send(
            Message(
                from_agent="worker",
                to_agent="coordinator",
                namespace="test",
                type="status",
                payload={},
            )
        )
        queue.send(
            Message(
                from_agent="worker",
                to_agent="coordinator",
                namespace="test",
                type="error",
                payload={},
            )
        )

        result = self.registry["a2a.receive"](state, type="error")
        self.assertEqual(len(result["a2a_messages"]), 1)
        self.assertEqual(result["a2a_messages"][0]["type"], "error")

    def test_receive_filter_by_sender(self):
        """Test receive filters by sender agents."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        queue.send(
            Message(
                from_agent="alice",
                to_agent="coordinator",
                namespace="test",
                type="msg",
                payload={"from": "alice"},
            )
        )
        queue.send(
            Message(
                from_agent="bob",
                to_agent="coordinator",
                namespace="test",
                type="msg",
                payload={"from": "bob"},
            )
        )

        result = self.registry["a2a.receive"](state, from_agents=["alice"])
        self.assertEqual(len(result["a2a_messages"]), 1)
        self.assertEqual(result["a2a_messages"][0]["from_agent"], "alice")

    def test_receive_require_all(self):
        """Test receive with require_all waits for all agents."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        queue.send(
            Message(
                from_agent="alice",
                to_agent="coordinator",
                namespace="test",
                type="vote",
                payload={},
            )
        )

        # Only alice sent, require_all with alice+bob should timeout
        result = self.registry["a2a.receive"](
            state,
            from_agents=["alice", "bob"],
            require_all=True,
            timeout="0.1s",
        )
        self.assertTrue(result.get("a2a_timeout"))
        self.assertIn("bob", result.get("a2a_error", ""))

    def test_receive_require_all_success(self):
        """Test receive require_all succeeds when all agents respond."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        queue.send(
            Message(
                from_agent="alice",
                to_agent="coordinator",
                namespace="test",
                type="vote",
                payload={},
            )
        )
        queue.send(
            Message(
                from_agent="bob",
                to_agent="coordinator",
                namespace="test",
                type="vote",
                payload={},
            )
        )

        result = self.registry["a2a.receive"](
            state, from_agents=["alice", "bob"], require_all=True
        )
        self.assertEqual(len(result["a2a_messages"]), 2)
        self.assertNotIn("a2a_timeout", result)


class TestA2ABroadcast(TestA2AActionsBase):
    """Tests for a2a.broadcast action (AC: 3)."""

    def test_broadcast_to_all(self):
        """Test broadcast sends to all registered agents."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        # Register agents
        queue.register_agent("worker-1", "test", "worker")
        queue.register_agent("worker-2", "test", "worker")
        queue.register_agent("manager", "test", "manager")

        result = self.registry["a2a.broadcast"](
            state, message={"type": "announcement", "payload": {"text": "hello"}}
        )
        self.assertEqual(result["a2a_broadcast_count"], 3)

    def test_broadcast_with_type_filter(self):
        """Test broadcast with agent type filter."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        queue.register_agent("worker-1", "test", "worker")
        queue.register_agent("worker-2", "test", "worker")
        queue.register_agent("manager", "test", "manager")

        result = self.registry["a2a.broadcast"](
            state,
            message={"type": "work_order"},
            agent_type_filter="worker",
        )
        self.assertEqual(result["a2a_broadcast_count"], 2)

    def test_broadcast_namespace_isolation(self):
        """Test broadcast is isolated to namespace."""
        state = {"_agent_id": "coordinator", "_namespace": "ns1"}
        queue = get_global_queue()

        queue.register_agent("worker-1", "ns1", "worker")
        queue.register_agent("worker-2", "ns2", "worker")

        result = self.registry["a2a.broadcast"](state, message={"type": "test"})
        self.assertEqual(result["a2a_broadcast_count"], 1)


class TestA2ADelegate(TestA2AActionsBase):
    """Tests for a2a.delegate action (AC: 4)."""

    def test_delegate_success(self):
        """Test successful delegation with response."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}
        queue = get_global_queue()

        # Simulate delegate responding in background
        def respond():
            time.sleep(0.05)
            messages = queue.receive(
                agent_id="search-agent",
                namespace="test",
                timeout=0.5,
            )
            if messages:
                queue.send(
                    Message(
                        from_agent="search-agent",
                        to_agent="coordinator",
                        namespace="test",
                        type="task_result",
                        payload={"result": "found it"},
                        correlation_id=messages[0].correlation_id,
                    )
                )

        thread = threading.Thread(target=respond)
        thread.start()

        result = self.registry["a2a.delegate"](
            state,
            to="search-agent",
            task={"type": "search", "query": "test"},
            timeout="1s",
        )
        thread.join()

        self.assertTrue(result["a2a_delegation_success"])
        self.assertEqual(result["a2a_delegation_result"], {"result": "found it"})

    def test_delegate_timeout_raise(self):
        """Test delegation timeout with raise strategy."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}

        result = self.registry["a2a.delegate"](
            state,
            to="absent-agent",
            task={"type": "search"},
            timeout="0.1s",
            on_timeout="raise",
        )
        self.assertFalse(result["a2a_delegation_success"])
        self.assertTrue(result.get("a2a_delegation_timeout"))
        self.assertIn("timed out", result.get("a2a_error", ""))

    def test_delegate_timeout_retry(self):
        """Test delegation timeout with retry strategy."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}

        result = self.registry["a2a.delegate"](
            state,
            to="absent-agent",
            task={"type": "search"},
            timeout="0.1s",
            on_timeout="retry",
        )
        self.assertFalse(result["a2a_delegation_success"])
        self.assertTrue(result.get("a2a_should_retry"))

    def test_delegate_fallback_local(self):
        """Test delegation timeout with fallback_local strategy."""
        state = {"_agent_id": "coordinator", "_namespace": "test"}

        # Register a fallback action
        def mock_fallback(state, **kwargs):
            return {"fallback_result": "local search result"}

        self.registry["web.search"] = mock_fallback

        result = self.registry["a2a.delegate"](
            state,
            to="absent-agent",
            task={"type": "search"},
            timeout="0.1s",
            on_timeout="fallback_local",
            fallback={"action": "web.search", "with": {"query": "test"}},
        )
        self.assertFalse(result["a2a_delegation_success"])
        self.assertTrue(result.get("a2a_delegation_fallback"))


class TestA2ASharedState(TestA2AActionsBase):
    """Tests for a2a.state.get and a2a.state.set actions (AC: 6)."""

    def test_state_set_and_get(self):
        """Test basic set and get operations."""
        state = {"_namespace": "test"}

        # Set value
        set_result = self.registry["a2a.state.set"](state, key="counter", value=42)
        self.assertTrue(set_result["a2a_state_updated"])
        self.assertEqual(set_result["a2a_state_version"], 1)

        # Get value
        get_result = self.registry["a2a.state.get"](state, key="counter")
        self.assertEqual(get_result["a2a_shared_state"], 42)
        self.assertEqual(get_result["a2a_state_version"], 1)

    def test_state_get_default(self):
        """Test get with default value for non-existent key."""
        state = {"_namespace": "test"}

        result = self.registry["a2a.state.get"](
            state, key="missing", default={"default": True}
        )
        self.assertEqual(result["a2a_shared_state"], {"default": True})
        self.assertEqual(result["a2a_state_version"], 0)

    def test_state_set_with_ttl(self):
        """Test set with TTL expiration."""
        state = {"_namespace": "test"}

        self.registry["a2a.state.set"](state, key="ephemeral", value="temp", ttl=0)
        time.sleep(0.01)

        result = self.registry["a2a.state.get"](state, key="ephemeral", default=None)
        self.assertIsNone(result["a2a_shared_state"])

    def test_state_optimistic_locking_success(self):
        """Test optimistic locking succeeds with correct version."""
        state = {"_namespace": "test"}

        # Initial set
        self.registry["a2a.state.set"](state, key="counter", value=1)

        # Update with expected version
        result = self.registry["a2a.state.set"](
            state, key="counter", value=2, expected_version=1
        )
        self.assertTrue(result["a2a_state_updated"])
        self.assertEqual(result["a2a_state_version"], 2)

    def test_state_optimistic_locking_conflict(self):
        """Test optimistic locking detects conflict."""
        state = {"_namespace": "test"}

        # Initial set creates version 1
        self.registry["a2a.state.set"](state, key="counter", value=1)

        # Try to update with wrong version
        result = self.registry["a2a.state.set"](
            state, key="counter", value=2, expected_version=0
        )
        self.assertFalse(result["a2a_state_updated"])
        self.assertTrue(result.get("a2a_state_conflict"))

    def test_state_namespace_isolation(self):
        """Test state is isolated between namespaces."""
        state1 = {"_namespace": "ns1"}
        state2 = {"_namespace": "ns2"}

        self.registry["a2a.state.set"](state1, key="value", value="ns1-value")
        self.registry["a2a.state.set"](state2, key="value", value="ns2-value")

        result1 = self.registry["a2a.state.get"](state1, key="value")
        result2 = self.registry["a2a.state.get"](state2, key="value")

        self.assertEqual(result1["a2a_shared_state"], "ns1-value")
        self.assertEqual(result2["a2a_shared_state"], "ns2-value")


class TestA2ADiscovery(TestA2AActionsBase):
    """Tests for a2a.discover action (AC: 7)."""

    def test_register_and_discover(self):
        """Test agent registration and discovery."""
        state = {"_namespace": "test"}

        # Register agent
        reg_result = self.registry["a2a.register"](
            state,
            agent_id="worker-1",
            capabilities=["search", "summarize"],
            agent_type="worker",
        )
        self.assertTrue(reg_result["a2a_registered"])
        self.assertEqual(reg_result["_agent_id"], "worker-1")

        # Discover agents
        disc_result = self.registry["a2a.discover"](state)
        self.assertEqual(len(disc_result["a2a_agents"]), 1)
        self.assertEqual(disc_result["a2a_agents"][0]["agent_id"], "worker-1")

    def test_discover_filter_by_capability(self):
        """Test discovery filters by capability."""
        state = {"_namespace": "test"}

        # Register multiple agents
        self.registry["a2a.register"](
            state,
            agent_id="searcher",
            capabilities=["search"],
            agent_type="worker",
        )
        self.registry["a2a.register"](
            state,
            agent_id="summarizer",
            capabilities=["summarize"],
            agent_type="worker",
        )

        # Discover with capability filter
        result = self.registry["a2a.discover"](state, capability="search")
        self.assertEqual(len(result["a2a_agents"]), 1)
        self.assertEqual(result["a2a_agents"][0]["agent_id"], "searcher")

    def test_discover_filter_by_type(self):
        """Test discovery filters by agent type."""
        state = {"_namespace": "test"}

        self.registry["a2a.register"](state, agent_id="worker-1", agent_type="worker")
        self.registry["a2a.register"](state, agent_id="manager-1", agent_type="manager")

        result = self.registry["a2a.discover"](state, agent_type="worker")
        self.assertEqual(len(result["a2a_agents"]), 1)
        self.assertEqual(result["a2a_agents"][0]["agent_type"], "worker")

    def test_unregister(self):
        """Test agent unregistration."""
        state = {"_namespace": "test"}

        self.registry["a2a.register"](state, agent_id="worker-1")
        result = self.registry["a2a.unregister"](state, agent_id="worker-1")
        self.assertTrue(result["a2a_unregistered"])

        # Should not appear in discovery
        disc_result = self.registry["a2a.discover"](state)
        self.assertEqual(len(disc_result["a2a_agents"]), 0)

    def test_heartbeat(self):
        """Test agent heartbeat updates last_seen."""
        state = {"_namespace": "test"}

        self.registry["a2a.register"](state, agent_id="worker-1")
        time.sleep(0.01)

        result = self.registry["a2a.heartbeat"]({**state, "_agent_id": "worker-1"})
        self.assertIsNotNone(result["a2a_heartbeat"])


class TestTimeoutParsing(TestA2AActionsBase):
    """Tests for timeout string parsing."""

    def test_timeout_seconds(self):
        """Test parsing seconds."""
        from the_edge_agent.actions.a2a_actions import _parse_timeout

        self.assertEqual(_parse_timeout("30s"), 30.0)
        self.assertEqual(_parse_timeout("1.5s"), 1.5)

    def test_timeout_minutes(self):
        """Test parsing minutes."""
        from the_edge_agent.actions.a2a_actions import _parse_timeout

        self.assertEqual(_parse_timeout("5m"), 300.0)

    def test_timeout_hours(self):
        """Test parsing hours."""
        from the_edge_agent.actions.a2a_actions import _parse_timeout

        self.assertEqual(_parse_timeout("1h"), 3600.0)

    def test_timeout_numeric(self):
        """Test parsing numeric values."""
        from the_edge_agent.actions.a2a_actions import _parse_timeout

        self.assertEqual(_parse_timeout(30), 30.0)
        self.assertEqual(_parse_timeout(30.5), 30.5)

    def test_timeout_none(self):
        """Test None returns 0."""
        from the_edge_agent.actions.a2a_actions import _parse_timeout

        self.assertEqual(_parse_timeout(None), 0.0)


if __name__ == "__main__":
    unittest.main()
