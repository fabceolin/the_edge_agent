"""
Unit Tests for Mem0 Memory Actions (TEA-AGENT-001.6).

These tests verify the Mem0 integration for universal memory management:
- memory.mem0.add: Store messages with fact extraction
- memory.mem0.search: Semantic search over memories
- memory.mem0.get_all: Get all memories for a scope
- memory.mem0.get: Get a specific memory by ID
- memory.mem0.update: Update memory content or metadata
- memory.mem0.delete: Delete by ID or bulk delete by scope
- memory.mem0.test: Test connection to Mem0 service

Tests use mocking to avoid external dependencies while verifying behavior.
"""

import unittest
from unittest.mock import MagicMock, patch, PropertyMock
import sys


class MockMem0Client:
    """Mock Mem0 client for testing without mem0ai dependency."""

    def __init__(self, **kwargs):
        self.api_key = kwargs.get("api_key")
        self.host = kwargs.get("host")
        self._memories = {}
        self._next_id = 1

    def add(
        self,
        messages,
        user_id=None,
        session_id=None,
        agent_id=None,
        metadata=None,
        **kwargs,
    ):
        """Mock add operation."""
        memory_id = f"mem_{self._next_id}"
        self._next_id += 1

        # Extract text from messages
        text = ""
        if isinstance(messages, list):
            for msg in messages:
                if isinstance(msg, dict):
                    text += msg.get("content", "") + " "
        elif isinstance(messages, str):
            text = messages

        memory = {
            "id": memory_id,
            "memory": text.strip(),
            "user_id": user_id,
            "session_id": session_id,
            "agent_id": agent_id,
            "metadata": metadata or {},
        }

        key = (user_id, session_id, agent_id)
        if key not in self._memories:
            self._memories[key] = []
        self._memories[key].append(memory)

        return {"id": memory_id, "results": [memory]}

    def search(
        self, query, user_id=None, session_id=None, agent_id=None, top_k=5, **kwargs
    ):
        """Mock search operation with fake scores."""
        key = (user_id, session_id, agent_id)
        memories = self._memories.get(key, [])

        # Simple "semantic" matching - check if query words in memory
        results = []
        for mem in memories[:top_k]:
            score = 0.9 if query.lower() in mem["memory"].lower() else 0.5
            results.append({**mem, "score": score})

        return {"results": results}

    def get_all(self, user_id=None, session_id=None, agent_id=None, **kwargs):
        """Mock get_all operation."""
        key = (user_id, session_id, agent_id)
        memories = self._memories.get(key, [])
        return {"results": memories, "total": len(memories)}

    def get(self, memory_id, **kwargs):
        """Mock get by ID operation."""
        for memories in self._memories.values():
            for mem in memories:
                if mem["id"] == memory_id:
                    return mem
        return None

    def update(self, memory_id, text=None, metadata=None, **kwargs):
        """Mock update operation."""
        for memories in self._memories.values():
            for mem in memories:
                if mem["id"] == memory_id:
                    if text is not None:
                        mem["memory"] = text
                    if metadata is not None:
                        mem["metadata"].update(metadata)
                    return mem
        raise ValueError(f"Memory {memory_id} not found")

    def delete(self, memory_id, **kwargs):
        """Mock delete by ID operation."""
        for key, memories in self._memories.items():
            for i, mem in enumerate(memories):
                if mem["id"] == memory_id:
                    del memories[i]
                    return {"deleted": True}
        raise ValueError(f"Memory {memory_id} not found")

    def delete_all(self, user_id=None, session_id=None, agent_id=None, **kwargs):
        """Mock bulk delete operation."""
        key = (user_id, session_id, agent_id)
        count = len(self._memories.get(key, []))
        self._memories[key] = []
        return {"deleted_count": count}


class MockMemory:
    """Mock local Memory class."""

    def __init__(self, config=None):
        self.config = config
        self._client = MockMem0Client()

    def add(self, messages, **kwargs):
        return self._client.add(messages, **kwargs)

    def search(self, query, **kwargs):
        return self._client.search(query, **kwargs)

    def get_all(self, **kwargs):
        return self._client.get_all(**kwargs)

    def get(self, memory_id, **kwargs):
        return self._client.get(memory_id, **kwargs)

    def update(self, memory_id, **kwargs):
        return self._client.update(memory_id, **kwargs)

    def delete(self, memory_id, **kwargs):
        return self._client.delete(memory_id, **kwargs)

    def delete_all(self, **kwargs):
        return self._client.delete_all(**kwargs)


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self, settings=None):
        self.settings = settings or {}
        self.actions_registry = {}
        self._memory_backend = MagicMock()
        self._mem0_client = None


class TestMem0ClientWrapper(unittest.TestCase):
    """Test the Mem0Client wrapper class."""

    @patch.dict(
        sys.modules,
        {
            "mem0": MagicMock(),
            "mem0.MemoryClient": MockMem0Client,
            "mem0.Memory": MockMemory,
        },
    )
    def test_client_from_settings(self):
        """Test creating client from settings."""
        # Import after patching
        from the_edge_agent.memory.mem0_client import Mem0Client

        client = Mem0Client.from_settings(
            {
                "memory": {
                    "backend": "mem0",
                    "user_id": "user123",
                    "session_id": "session456",
                    "graph": True,
                }
            }
        )

        self.assertEqual(client.default_user_id, "user123")
        self.assertEqual(client.default_session_id, "session456")
        self.assertTrue(client.graph_enabled)

    @patch.dict(
        sys.modules,
        {
            "mem0": MagicMock(),
            "mem0.MemoryClient": MockMem0Client,
            "mem0.Memory": MockMemory,
        },
    )
    def test_client_env_var_expansion(self):
        """Test environment variable expansion in API key."""
        import os
        from the_edge_agent.memory.mem0_client import Mem0Client

        # Set env var
        os.environ["TEST_MEM0_KEY"] = "test_api_key_123"

        client = Mem0Client.from_settings(
            {
                "memory": {
                    "backend": "mem0",
                    "api_key": "${TEST_MEM0_KEY}",
                }
            }
        )

        self.assertEqual(client.api_key, "test_api_key_123")

        # Clean up
        del os.environ["TEST_MEM0_KEY"]

    @patch.dict(
        sys.modules,
        {
            "mem0": MagicMock(),
            "mem0.MemoryClient": MockMem0Client,
            "mem0.Memory": MockMemory,
        },
    )
    def test_scope_params(self):
        """Test scope parameter handling with defaults."""
        from the_edge_agent.memory.mem0_client import Mem0Client

        client = Mem0Client(
            user_id="default_user",
            session_id="default_session",
        )

        # Test with overrides
        params = client._get_scope_params(user_id="override_user")
        self.assertEqual(params["user_id"], "override_user")
        self.assertEqual(params["session_id"], "default_session")

        # Test with defaults
        params = client._get_scope_params()
        self.assertEqual(params["user_id"], "default_user")
        self.assertEqual(params["session_id"], "default_session")


class TestMem0Actions(unittest.TestCase):
    """Test the Mem0 action functions."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = MockEngine(
            settings={
                "memory": {
                    "backend": "mem0",
                    "user_id": "test_user",
                }
            }
        )

        # Create mock client
        self.mock_client = MagicMock()
        self.mock_client.add.return_value = {
            "success": True,
            "memory_id": "mem_1",
            "memories": [{"id": "mem_1", "memory": "Test memory"}],
        }
        self.mock_client.search.return_value = {
            "success": True,
            "results": [{"id": "mem_1", "memory": "Test memory", "score": 0.95}],
        }
        self.mock_client.get_all.return_value = {
            "success": True,
            "memories": [{"id": "mem_1", "memory": "Test memory"}],
            "total": 1,
        }
        self.mock_client.get.return_value = {
            "success": True,
            "memory": {"id": "mem_1", "memory": "Test memory"},
        }
        self.mock_client.update.return_value = {
            "success": True,
            "memory": {"id": "mem_1", "memory": "Updated memory"},
        }
        self.mock_client.delete.return_value = {
            "success": True,
            "deleted_count": 1,
        }
        self.mock_client.test_connection.return_value = {
            "success": True,
            "message": "Connection successful",
        }
        self.mock_client.is_available.return_value = True

        # Cache client on engine
        self.engine._mem0_client = self.mock_client

    def test_add_action_with_messages(self):
        """Test memory.mem0.add with message list."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.add"](
            state={},
            messages=[
                {"role": "user", "content": "My favorite color is blue"},
                {"role": "assistant", "content": "I'll remember that!"},
            ],
            user_id="user123",
        )

        self.assertTrue(result["success"])
        self.mock_client.add.assert_called_once()

    def test_add_action_with_string(self):
        """Test memory.mem0.add with string message."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.add"](
            state={},
            messages="I prefer dark mode",
            user_id="user123",
        )

        self.assertTrue(result["success"])

    def test_add_action_requires_messages(self):
        """Test memory.mem0.add fails without messages."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.add"](
            state={},
            user_id="user123",
        )

        self.assertFalse(result["success"])
        self.assertIn("messages", result["error"].lower())

    def test_search_action(self):
        """Test memory.mem0.search action."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.search"](
            state={},
            query="What is my favorite color?",
            user_id="user123",
            limit=5,
        )

        self.assertTrue(result["success"])
        self.assertIn("results", result)
        self.mock_client.search.assert_called_once()

    def test_search_action_requires_query(self):
        """Test memory.mem0.search fails without query."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.search"](
            state={},
            user_id="user123",
        )

        self.assertFalse(result["success"])
        self.assertIn("query", result["error"].lower())

    def test_get_all_action(self):
        """Test memory.mem0.get_all action."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.get_all"](
            state={},
            user_id="user123",
            limit=10,
            offset=0,
        )

        self.assertTrue(result["success"])
        self.assertIn("memories", result)
        self.mock_client.get_all.assert_called_once()

    def test_get_action(self):
        """Test memory.mem0.get action."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.get"](
            state={},
            memory_id="mem_1",
        )

        self.assertTrue(result["success"])
        self.assertIn("memory", result)

    def test_get_action_requires_id(self):
        """Test memory.mem0.get fails without memory_id."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.get"](state={})

        self.assertFalse(result["success"])
        self.assertIn("memory_id", result["error"].lower())

    def test_update_action(self):
        """Test memory.mem0.update action."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.update"](
            state={},
            memory_id="mem_1",
            text="Updated content",
            metadata={"verified": True},
        )

        self.assertTrue(result["success"])
        self.mock_client.update.assert_called_once()

    def test_update_action_requires_content(self):
        """Test memory.mem0.update fails without text or metadata."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.update"](
            state={},
            memory_id="mem_1",
        )

        self.assertFalse(result["success"])
        self.assertIn("text or metadata", result["error"].lower())

    def test_delete_action_single(self):
        """Test memory.mem0.delete for single memory."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.delete"](
            state={},
            memory_id="mem_1",
        )

        self.assertTrue(result["success"])
        self.mock_client.delete.assert_called_once()

    def test_delete_action_bulk_requires_flag(self):
        """Test memory.mem0.delete bulk requires delete_all flag."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.delete"](
            state={},
            user_id="user123",
            # delete_all not set
        )

        self.assertFalse(result["success"])
        self.assertIn("delete_all", result["error"].lower())

    def test_delete_action_bulk(self):
        """Test memory.mem0.delete bulk delete."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.delete"](
            state={},
            user_id="user123",
            delete_all=True,
        )

        self.assertTrue(result["success"])
        self.mock_client.delete.assert_called_once()

    def test_test_action(self):
        """Test memory.mem0.test action."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        result = registry["memory.mem0.test"](state={})

        self.assertTrue(result["success"])


class TestMem0Fallback(unittest.TestCase):
    """Test graceful fallback when Mem0 is unavailable."""

    def test_fallback_for_add(self):
        """Test fallback to native memory for add action."""
        engine = MockEngine(settings={"memory": {"backend": "sqlite"}})
        engine._mem0_client = None  # Mem0 not available

        # Set up native memory action
        native_store = MagicMock(return_value={"stored": True, "key": "test"})
        engine.actions_registry = {"memory.store": native_store}

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        result = registry["memory.mem0.add"](
            state={},
            messages=[{"role": "user", "content": "Test"}],
            user_id="user123",
        )

        # Should fallback to native
        self.assertTrue(result.get("fallback", False) or result.get("stored", False))

    def test_fallback_for_search(self):
        """Test fallback for search action (semantic search unavailable)."""
        engine = MockEngine(settings={"memory": {"backend": "sqlite"}})
        engine._mem0_client = None

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        result = registry["memory.mem0.search"](
            state={},
            query="test query",
            user_id="user123",
        )

        self.assertFalse(result["success"])
        self.assertTrue(result.get("fallback", False))
        self.assertIn("Mem0", result["error"])


class TestMem0ScopeIsolation(unittest.TestCase):
    """Test scope isolation between users, sessions, and agents."""

    def setUp(self):
        """Set up test fixtures with separate scopes."""
        self.engine = MockEngine(settings={"memory": {"backend": "mem0"}})

        # Create mock client that tracks scope
        self.mock_client = MagicMock()
        self.mock_client.is_available.return_value = True
        self.engine._mem0_client = self.mock_client

    def test_user_scope_isolation(self):
        """Test that user_id provides isolation."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        # Search for user1
        self.mock_client.search.return_value = {"success": True, "results": []}

        registry["memory.mem0.search"](
            state={},
            query="test",
            user_id="user1",
        )

        # Verify user_id was passed
        call_kwargs = self.mock_client.search.call_args[1]
        self.assertEqual(call_kwargs["user_id"], "user1")

    def test_session_scope_isolation(self):
        """Test that session_id provides isolation."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        self.mock_client.get_all.return_value = {
            "success": True,
            "memories": [],
            "total": 0,
        }

        registry["memory.mem0.get_all"](
            state={},
            session_id="session_abc",
        )

        call_kwargs = self.mock_client.get_all.call_args[1]
        self.assertEqual(call_kwargs["session_id"], "session_abc")

    def test_agent_scope_isolation(self):
        """Test that agent_id provides isolation."""
        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, self.engine)

        self.mock_client.add.return_value = {"success": True, "memory_id": "mem_1"}

        registry["memory.mem0.add"](
            state={},
            messages="Test message",
            agent_id="agent_xyz",
        )

        call_kwargs = self.mock_client.add.call_args[1]
        self.assertEqual(call_kwargs["agent_id"], "agent_xyz")


class TestMem0GraphMemory(unittest.TestCase):
    """Test graph memory features (Mem0g)."""

    def test_graph_enabled_config(self):
        """Test that graph mode is configured via settings."""
        from the_edge_agent.memory.mem0_client import Mem0Client

        client = Mem0Client.from_settings(
            {
                "memory": {
                    "backend": "mem0",
                    "graph": True,
                }
            }
        )

        self.assertTrue(client.graph_enabled)

    def test_search_with_relations(self):
        """Test search with include_relations flag."""
        engine = MockEngine(settings={"memory": {"backend": "mem0", "graph": True}})

        mock_client = MagicMock()
        mock_client.is_available.return_value = True
        mock_client.graph_enabled = True
        mock_client.search.return_value = {
            "success": True,
            "results": [{"id": "mem_1", "memory": "Test"}],
            "relations": [{"source": "Alice", "relation": "knows", "target": "Bob"}],
        }
        engine._mem0_client = mock_client

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        result = registry["memory.mem0.search"](
            state={},
            query="Who does Alice know?",
            user_id="user123",
            include_relations=True,
        )

        self.assertTrue(result["success"])
        self.mock_client = mock_client
        call_kwargs = mock_client.search.call_args[1]
        self.assertTrue(call_kwargs.get("include_relations", False))


class TestMem0RegistryIntegration(unittest.TestCase):
    """Test that Mem0 actions are properly registered."""

    def test_actions_registered(self):
        """Test that all Mem0 actions are in registry."""
        engine = MockEngine()

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        # Check all actions are registered
        expected_actions = [
            "memory.mem0.add",
            "memory.mem0.search",
            "memory.mem0.get_all",
            "memory.mem0.get",
            "memory.mem0.update",
            "memory.mem0.delete",
            "memory.mem0.test",
        ]

        for action in expected_actions:
            self.assertIn(action, registry, f"Missing action: {action}")

        # Check aliases
        expected_aliases = [
            "actions.memory_mem0_add",
            "actions.memory_mem0_search",
            "actions.memory_mem0_get_all",
            "actions.memory_mem0_get",
            "actions.memory_mem0_update",
            "actions.memory_mem0_delete",
            "actions.memory_mem0_test",
        ]

        for alias in expected_aliases:
            self.assertIn(alias, registry, f"Missing alias: {alias}")


class TestMem0Metadata(unittest.TestCase):
    """Test metadata handling in Mem0 actions."""

    def test_add_with_metadata(self):
        """Test adding memory with custom metadata."""
        engine = MockEngine(settings={"memory": {"backend": "mem0"}})

        mock_client = MagicMock()
        mock_client.is_available.return_value = True
        mock_client.add.return_value = {"success": True, "memory_id": "mem_1"}
        engine._mem0_client = mock_client

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        metadata = {"source": "chat", "timestamp": "2026-01-05", "priority": "high"}

        registry["memory.mem0.add"](
            state={},
            messages="Important information",
            user_id="user123",
            metadata=metadata,
        )

        call_kwargs = mock_client.add.call_args[1]
        self.assertEqual(call_kwargs["metadata"], metadata)

    def test_update_metadata_merge(self):
        """Test that update merges metadata."""
        engine = MockEngine(settings={"memory": {"backend": "mem0"}})

        mock_client = MagicMock()
        mock_client.is_available.return_value = True
        mock_client.update.return_value = {"success": True, "memory": {}}
        engine._mem0_client = mock_client

        from the_edge_agent.actions.mem0_actions import register_actions

        registry = {}
        register_actions(registry, engine)

        registry["memory.mem0.update"](
            state={},
            memory_id="mem_1",
            metadata={"verified": True, "updated_by": "admin"},
        )

        call_kwargs = mock_client.update.call_args[1]
        self.assertIn("metadata", call_kwargs)


if __name__ == "__main__":
    unittest.main()
