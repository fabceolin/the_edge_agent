"""
Tests for Memory Actions (TEA-BUILTIN-001.1).

Tests cover:
- memory.store with various value types
- memory.retrieve with found/not-found/expired cases
- memory.summarize with mock LLM
- Namespace isolation
- Checkpoint serialization/restoration
- TTL expiration behavior
- Dual namespace access (memory.* and actions.memory_*)
- Error handling
"""

import unittest
import time
from unittest.mock import patch, MagicMock

from the_edge_agent import YAMLEngine, InMemoryBackend


class TestMemoryBackend(unittest.TestCase):
    """Tests for InMemoryBackend implementation."""

    def setUp(self):
        """Create fresh backend for each test."""
        self.backend = InMemoryBackend()

    def test_store_basic(self):
        """P0: Basic store operation."""
        result = self.backend.store("key1", "value1")
        self.assertTrue(result)

    def test_retrieve_found(self):
        """P0: Retrieve existing key."""
        self.backend.store("key1", "value1")
        value = self.backend.retrieve("key1")
        self.assertEqual(value, "value1")

    def test_retrieve_not_found(self):
        """P1: Retrieve non-existent key."""
        value = self.backend.retrieve("nonexistent")
        self.assertIsNone(value)

    def test_store_with_ttl(self):
        """P1: Store with TTL."""
        result = self.backend.store("key1", "value1", ttl=1.0)
        self.assertTrue(result)
        # Value should exist immediately
        value = self.backend.retrieve("key1")
        self.assertEqual(value, "value1")

    def test_retrieve_expired(self):
        """P1: Retrieve expired key returns None."""
        self.backend.store("key1", "value1", ttl=0.01)  # 10ms TTL
        time.sleep(0.02)  # Wait for expiration
        value = self.backend.retrieve("key1")
        self.assertIsNone(value)

    def test_namespace_isolation(self):
        """P2: Keys in different namespaces are isolated."""
        self.backend.store("key1", "ns1_value", namespace="ns1")
        self.backend.store("key1", "ns2_value", namespace="ns2")

        self.assertEqual(self.backend.retrieve("key1", namespace="ns1"), "ns1_value")
        self.assertEqual(self.backend.retrieve("key1", namespace="ns2"), "ns2_value")

    def test_delete_key(self):
        """Delete existing key."""
        self.backend.store("key1", "value1")
        result = self.backend.delete("key1")
        self.assertTrue(result)
        self.assertIsNone(self.backend.retrieve("key1"))

    def test_delete_nonexistent(self):
        """Delete non-existent key returns False."""
        result = self.backend.delete("nonexistent")
        self.assertFalse(result)

    def test_exists_true(self):
        """Exists returns True for existing key."""
        self.backend.store("key1", "value1")
        self.assertTrue(self.backend.exists("key1"))

    def test_exists_false(self):
        """Exists returns False for non-existent key."""
        self.assertFalse(self.backend.exists("nonexistent"))

    def test_clear_namespace(self):
        """Clear specific namespace."""
        self.backend.store("key1", "value1", namespace="ns1")
        self.backend.store("key2", "value2", namespace="ns1")
        self.backend.store("key1", "value1", namespace="ns2")

        count = self.backend.clear(namespace="ns1")
        self.assertEqual(count, 2)
        self.assertIsNone(self.backend.retrieve("key1", namespace="ns1"))
        self.assertEqual(self.backend.retrieve("key1", namespace="ns2"), "value1")

    def test_clear_all(self):
        """Clear all namespaces."""
        self.backend.store("key1", "value1", namespace="ns1")
        self.backend.store("key2", "value2", namespace="ns2")

        count = self.backend.clear()
        self.assertEqual(count, 2)
        self.assertIsNone(self.backend.retrieve("key1", namespace="ns1"))
        self.assertIsNone(self.backend.retrieve("key2", namespace="ns2"))

    def test_store_various_types(self):
        """P0: Store various value types."""
        # String
        self.backend.store("str", "hello")
        self.assertEqual(self.backend.retrieve("str"), "hello")

        # Integer
        self.backend.store("int", 42)
        self.assertEqual(self.backend.retrieve("int"), 42)

        # Float
        self.backend.store("float", 3.14)
        self.assertEqual(self.backend.retrieve("float"), 3.14)

        # List
        self.backend.store("list", [1, 2, 3])
        self.assertEqual(self.backend.retrieve("list"), [1, 2, 3])

        # Dict
        self.backend.store("dict", {"a": 1, "b": 2})
        self.assertEqual(self.backend.retrieve("dict"), {"a": 1, "b": 2})

        # Nested
        nested = {"users": [{"name": "Alice"}, {"name": "Bob"}]}
        self.backend.store("nested", nested)
        self.assertEqual(self.backend.retrieve("nested"), nested)

    def test_checkpoint_persistence(self):
        """P0: Get state and restore state for checkpointing."""
        # Store some values
        self.backend.store("key1", "value1")
        self.backend.store("key2", {"nested": "data"}, ttl=60.0)
        self.backend.store("key3", "ns_value", namespace="custom")

        # Get state
        state = self.backend.get_state()
        self.assertEqual(state["version"], "1.0")
        self.assertIn("namespaces", state)

        # Create new backend and restore
        new_backend = InMemoryBackend()
        new_backend.restore_state(state)

        # Verify restoration
        self.assertEqual(new_backend.retrieve("key1"), "value1")
        self.assertEqual(new_backend.retrieve("key2"), {"nested": "data"})
        self.assertEqual(new_backend.retrieve("key3", namespace="custom"), "ns_value")

    def test_checkpoint_ttl_conversion(self):
        """P2: TTL is correctly converted to remaining time in checkpoint."""
        self.backend.store("key1", "value1", ttl=60.0)

        # Get state
        state = self.backend.get_state()

        # Check remaining TTL is stored
        entry = state["namespaces"]["default"]["key1"]
        self.assertIn("remaining_ttl", entry)
        self.assertLessEqual(entry["remaining_ttl"], 60.0)
        self.assertGreater(entry["remaining_ttl"], 59.0)  # Should be close to 60

    def test_checkpoint_expired_excluded(self):
        """P2: Expired entries are excluded from checkpoint."""
        self.backend.store("key1", "value1", ttl=0.01)
        time.sleep(0.02)

        state = self.backend.get_state()

        # key1 should not be in the state (expired)
        if "default" in state["namespaces"]:
            self.assertNotIn("key1", state["namespaces"]["default"])


class TestMemoryActions(unittest.TestCase):
    """Unit tests for memory actions in YAMLEngine."""

    def setUp(self):
        """Create fresh YAMLEngine for each test."""
        self.engine = YAMLEngine()

    def test_memory_store_basic(self):
        """P0: Basic memory.store action."""
        action = self.engine.actions_registry['memory.store']
        result = action(state={}, key="user_name", value="Alice")

        self.assertTrue(result["stored"])
        self.assertEqual(result["key"], "user_name")
        self.assertEqual(result["namespace"], "default")

    def test_memory_retrieve_found(self):
        """P0: memory.retrieve finds stored value."""
        store_action = self.engine.actions_registry['memory.store']
        retrieve_action = self.engine.actions_registry['memory.retrieve']

        store_action(state={}, key="user_name", value="Alice")
        result = retrieve_action(state={}, key="user_name")

        self.assertTrue(result["found"])
        self.assertEqual(result["value"], "Alice")
        self.assertEqual(result["key"], "user_name")

    def test_memory_retrieve_not_found_default(self):
        """P1: memory.retrieve returns default for missing key."""
        retrieve_action = self.engine.actions_registry['memory.retrieve']
        result = retrieve_action(state={}, key="nonexistent", default="fallback")

        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "fallback")

    def test_memory_retrieve_expired(self):
        """P1: memory.retrieve returns default for expired key."""
        store_action = self.engine.actions_registry['memory.store']
        retrieve_action = self.engine.actions_registry['memory.retrieve']

        store_action(state={}, key="temp_key", value="temp_value", ttl=0.01)
        time.sleep(0.02)

        result = retrieve_action(state={}, key="temp_key", default="expired_default")

        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "expired_default")

    def test_memory_store_with_ttl(self):
        """P1: memory.store with TTL expiration."""
        store_action = self.engine.actions_registry['memory.store']
        retrieve_action = self.engine.actions_registry['memory.retrieve']

        store_action(state={}, key="ttl_key", value="ttl_value", ttl=1.0)

        # Should exist immediately
        result = retrieve_action(state={}, key="ttl_key")
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], "ttl_value")

    def test_memory_dual_namespace_access(self):
        """P1: AC8 - Verify both memory.* and actions.memory_* work."""
        # Test memory.store and actions.memory_store are the same
        store1 = self.engine.actions_registry['memory.store']
        store2 = self.engine.actions_registry['actions.memory_store']
        self.assertIs(store1, store2)

        # Test memory.retrieve and actions.memory_retrieve are the same
        retrieve1 = self.engine.actions_registry['memory.retrieve']
        retrieve2 = self.engine.actions_registry['actions.memory_retrieve']
        self.assertIs(retrieve1, retrieve2)

        # Test memory.summarize and actions.memory_summarize are the same
        summarize1 = self.engine.actions_registry['memory.summarize']
        summarize2 = self.engine.actions_registry['actions.memory_summarize']
        self.assertIs(summarize1, summarize2)

    def test_memory_store_non_serializable(self):
        """P1: Error handling for storage (values are actually stored directly)."""
        store_action = self.engine.actions_registry['memory.store']

        # Store a lambda (non-serializable in pickle)
        # The store should succeed since we're using in-memory, but
        # checkpoint serialization might fail later
        result = store_action(state={}, key="func", value=lambda x: x)
        self.assertTrue(result["stored"])

    def test_memory_store_none_key(self):
        """Error handling when key is None."""
        store_action = self.engine.actions_registry['memory.store']
        result = store_action(state={}, key=None, value="value")

        self.assertFalse(result["stored"])
        self.assertIn("error", result)

    def test_memory_retrieve_none_key(self):
        """Error handling when key is None."""
        retrieve_action = self.engine.actions_registry['memory.retrieve']
        result = retrieve_action(state={}, key=None, default="default")

        self.assertFalse(result["found"])
        self.assertIn("error", result)

    def test_memory_namespace_isolation(self):
        """P2: Keys in different namespaces are isolated."""
        store_action = self.engine.actions_registry['memory.store']
        retrieve_action = self.engine.actions_registry['memory.retrieve']

        store_action(state={}, key="shared_key", value="ns1_value", namespace="ns1")
        store_action(state={}, key="shared_key", value="ns2_value", namespace="ns2")

        result1 = retrieve_action(state={}, key="shared_key", namespace="ns1")
        result2 = retrieve_action(state={}, key="shared_key", namespace="ns2")

        self.assertEqual(result1["value"], "ns1_value")
        self.assertEqual(result2["value"], "ns2_value")

    def test_memory_checkpoint_persistence(self):
        """P0: Memory state persists through checkpoint serialization."""
        store_action = self.engine.actions_registry['memory.store']

        # Store values
        store_action(state={}, key="key1", value="value1")
        store_action(state={}, key="key2", value={"nested": "data"})

        # Get checkpoint state
        memory_state = self.engine.get_memory_state()

        # Create new engine and restore
        new_engine = YAMLEngine()
        new_engine.restore_memory_state(memory_state)

        # Verify
        retrieve_action = new_engine.actions_registry['memory.retrieve']
        result1 = retrieve_action(state={}, key="key1")
        result2 = retrieve_action(state={}, key="key2")

        self.assertEqual(result1["value"], "value1")
        self.assertEqual(result2["value"], {"nested": "data"})

    def test_memory_backend_switching(self):
        """P2: AC4 - Verify custom backends can be injected."""
        custom_backend = InMemoryBackend()
        custom_backend.store("pre_key", "pre_value")

        engine = YAMLEngine(memory_backend=custom_backend)

        # Should see pre-existing value
        retrieve_action = engine.actions_registry['memory.retrieve']
        result = retrieve_action(state={}, key="pre_key")

        self.assertTrue(result["found"])
        self.assertEqual(result["value"], "pre_value")

    def test_memory_summarize_llm_unavailable(self):
        """P1: Graceful handling when OpenAI unavailable."""
        summarize_action = self.engine.actions_registry['memory.summarize']

        state = {
            "messages": [
                {"role": "user", "content": "Hello"},
                {"role": "assistant", "content": "Hi there!"}
            ]
        }

        # Mock the llm.call action to simulate ImportError
        original_llm_call = self.engine.actions_registry.get('llm.call')

        def mock_llm_call(*args, **kwargs):
            raise ImportError("No module named 'openai'")

        self.engine.actions_registry['llm.call'] = mock_llm_call

        try:
            # Short conversation should return as-is
            result = summarize_action(state=state, messages_key="messages", max_tokens=1000)
            # Should succeed without calling LLM since within limit
            self.assertTrue(result["success"])
        finally:
            if original_llm_call:
                self.engine.actions_registry['llm.call'] = original_llm_call

    def test_memory_summarize_missing_key(self):
        """memory.summarize with missing messages key."""
        summarize_action = self.engine.actions_registry['memory.summarize']

        result = summarize_action(state={}, messages_key="missing")

        self.assertFalse(result["success"])
        self.assertIn("error", result)
        self.assertIn("not found", result["error"])

    def test_memory_summarize_empty_messages(self):
        """memory.summarize with empty messages list."""
        summarize_action = self.engine.actions_registry['memory.summarize']

        result = summarize_action(state={"messages": []}, messages_key="messages")

        self.assertTrue(result["success"])
        self.assertEqual(result["summary"], "")
        self.assertEqual(result["original_count"], 0)

    def test_memory_summarize_short_conversation(self):
        """memory.summarize returns as-is for short conversations."""
        summarize_action = self.engine.actions_registry['memory.summarize']

        state = {
            "messages": [
                {"role": "user", "content": "Hi"},
                {"role": "assistant", "content": "Hello!"}
            ]
        }

        result = summarize_action(state=state, messages_key="messages", max_tokens=1000)

        self.assertTrue(result["success"])
        self.assertEqual(result["original_count"], 2)
        self.assertIn("user: Hi", result["summary"])
        self.assertIn("assistant: Hello!", result["summary"])

    def test_memory_summarize_with_mock_llm(self):
        """P1: memory.summarize uses LLM for long conversations."""
        # Create a mock llm.call action
        def mock_llm_call(state, model, messages, temperature=0.7, **kwargs):
            return {
                "content": "Summary: User greeted assistant.",
                "usage": {"total_tokens": 10}
            }

        # Replace llm.call with our mock
        self.engine.actions_registry['llm.call'] = mock_llm_call

        summarize_action = self.engine.actions_registry['memory.summarize']

        # Create a long conversation that exceeds token limit
        long_messages = [
            {"role": "user", "content": "Hello " * 500}  # ~500 tokens
        ]

        state = {"messages": long_messages}

        result = summarize_action(state=state, messages_key="messages", max_tokens=100)

        self.assertTrue(result["success"])
        self.assertEqual(result["summary"], "Summary: User greeted assistant.")


class TestMemoryActionsIntegration(unittest.TestCase):
    """Integration tests for memory actions in YAML workflows."""

    def test_memory_in_yaml_workflow(self):
        """P0: Memory actions work in a YAML-defined workflow."""
        config = {
            "state_schema": {"input": str, "memory_result": dict},
            "nodes": [
                {
                    "name": "store_node",
                    "uses": "memory.store",
                    "with": {
                        "key": "conversation_id",
                        "value": "{{ state.input }}"
                    },
                    "output": "store_result"
                },
                {
                    "name": "retrieve_node",
                    "uses": "memory.retrieve",
                    "with": {
                        "key": "conversation_id"
                    },
                    "output": "retrieve_result"
                }
            ],
            "edges": [
                {"from": "__start__", "to": "store_node"},
                {"from": "store_node", "to": "retrieve_node"},
                {"from": "retrieve_node", "to": "__end__"}
            ]
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({"input": "conv_123"}))
        final_state = events[-1]["state"]

        self.assertEqual(final_state["store_result"]["stored"], True)
        self.assertEqual(final_state["retrieve_result"]["value"], "conv_123")
        self.assertTrue(final_state["retrieve_result"]["found"])

    def test_memory_across_multiple_invokes(self):
        """P1: Memory persists across multiple graph invocations."""
        engine = YAMLEngine()

        store_config = {
            "state_schema": {"key": str, "value": str},
            "nodes": [
                {
                    "name": "store",
                    "uses": "memory.store",
                    "with": {
                        "key": "{{ state.key }}",
                        "value": "{{ state.value }}"
                    }
                }
            ],
            "edges": [
                {"from": "__start__", "to": "store"},
                {"from": "store", "to": "__end__"}
            ]
        }

        retrieve_config = {
            "state_schema": {"key": str},
            "nodes": [
                {
                    "name": "retrieve",
                    "uses": "memory.retrieve",
                    "with": {
                        "key": "{{ state.key }}"
                    },
                    "output": "result"
                }
            ],
            "edges": [
                {"from": "__start__", "to": "retrieve"},
                {"from": "retrieve", "to": "__end__"}
            ]
        }

        # First invocation: store
        store_graph = engine.load_from_dict(store_config)
        list(store_graph.invoke({"key": "session_id", "value": "sess_abc"}))

        # Second invocation: retrieve
        retrieve_graph = engine.load_from_dict(retrieve_config)
        events = list(retrieve_graph.invoke({"key": "session_id"}))
        final_state = events[-1]["state"]

        self.assertEqual(final_state["result"]["value"], "sess_abc")
        self.assertTrue(final_state["result"]["found"])

    def test_memory_with_checkpoint_save_resume(self):
        """P0: Memory state is preserved through checkpoint save/resume."""
        engine = YAMLEngine()

        # Store value
        store_action = engine.actions_registry['memory.store']
        store_action(state={}, key="checkpoint_key", value="checkpoint_value")

        # Get memory state (simulating checkpoint save)
        memory_state = engine.get_memory_state()

        # Simulate resume: new engine, restore memory
        new_engine = YAMLEngine()
        new_engine.restore_memory_state(memory_state)

        # Retrieve in new engine
        retrieve_action = new_engine.actions_registry['memory.retrieve']
        result = retrieve_action(state={}, key="checkpoint_key")

        self.assertEqual(result["value"], "checkpoint_value")
        self.assertTrue(result["found"])


class TestMemoryBackendFailure(unittest.TestCase):
    """P2: Tests for backend failure handling."""

    def test_memory_backend_failure(self):
        """P2: Error handling when backend fails."""
        class FailingBackend:
            def store(self, key, value, ttl=None, namespace="default"):
                raise RuntimeError("Backend failure")

            def retrieve(self, key, namespace="default"):
                raise RuntimeError("Backend failure")

        engine = YAMLEngine(memory_backend=FailingBackend())

        store_action = engine.actions_registry['memory.store']
        result = store_action(state={}, key="key", value="value")

        self.assertFalse(result["stored"])
        self.assertIn("error", result)
        self.assertIn("Backend failure", result["error"])


if __name__ == '__main__':
    unittest.main()
