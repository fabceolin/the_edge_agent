"""
Tests for LTM Backend Interface (TEA-BUILTIN-001.5).

Tests cover:
- LTMBackend ABC contract
- Backend registry and factory pattern
- Backend configuration parsing
- Distributed lock ABC contract
- Lock registry and factory pattern
"""

import unittest
from abc import ABC
from unittest.mock import patch, MagicMock

from the_edge_agent.memory import (
    LTMBackend,
    LongTermMemoryBackend,
    SQLiteBackend,
    register_backend,
    get_registered_backends,
    create_ltm_backend,
    parse_backend_config,
)
from the_edge_agent.memory.locks import (
    DistributedLock,
    register_lock,
    get_registered_locks,
    create_lock,
)


class TestLTMBackendABC(unittest.TestCase):
    """Tests for LTMBackend abstract base class."""

    def test_ltm_backend_is_abc(self):
        """P0: LTMBackend is an ABC."""
        self.assertTrue(issubclass(LTMBackend, ABC))

    def test_ltm_backend_abstract_methods(self):
        """P0: LTMBackend defines required abstract methods."""
        abstract_methods = LTMBackend.__abstractmethods__
        required = {'store', 'retrieve', 'delete', 'search', 'close'}
        self.assertEqual(abstract_methods, required)

    def test_sqlite_backend_is_ltm_backend(self):
        """P0: SQLiteBackend implements LTMBackend ABC."""
        self.assertTrue(issubclass(SQLiteBackend, LTMBackend))

    def test_sqlite_backend_instance_is_ltm_backend(self):
        """P0: SQLiteBackend instance is LTMBackend."""
        backend = SQLiteBackend(":memory:")
        self.assertIsInstance(backend, LTMBackend)
        backend.close()

    def test_backward_compatibility_alias(self):
        """P1: LongTermMemoryBackend is alias for LTMBackend."""
        self.assertIs(LongTermMemoryBackend, LTMBackend)

    def test_context_manager_support(self):
        """P1: LTMBackend supports context manager."""
        with SQLiteBackend(":memory:") as backend:
            result = backend.store("test", "value")
            self.assertTrue(result['success'])
        # Backend should be closed after context

    def test_iterate_all_method(self):
        """P1: LTMBackend.iterate_all() works."""
        backend = SQLiteBackend(":memory:")
        backend.store("key1", "value1")
        backend.store("key2", "value2")

        items = list(backend.iterate_all())
        self.assertEqual(len(items), 2)
        keys = {item[0] for item in items}
        self.assertEqual(keys, {"key1", "key2"})
        backend.close()


class TestBackendRegistry(unittest.TestCase):
    """Tests for backend registry and factory pattern."""

    def test_sqlite_registered_by_default(self):
        """P0: SQLite backend is registered by default."""
        backends = get_registered_backends()
        self.assertIn("sqlite", backends)

    def test_create_sqlite_backend(self):
        """P0: Factory can create SQLite backend."""
        backend = create_ltm_backend("sqlite", db_path=":memory:")
        self.assertIsInstance(backend, SQLiteBackend)
        backend.close()

    def test_create_backend_case_insensitive(self):
        """P1: Factory handles case-insensitive names."""
        backend1 = create_ltm_backend("SQLite", db_path=":memory:")
        backend2 = create_ltm_backend("SQLITE", db_path=":memory:")
        self.assertIsInstance(backend1, SQLiteBackend)
        self.assertIsInstance(backend2, SQLiteBackend)
        backend1.close()
        backend2.close()

    def test_create_unknown_backend_raises(self):
        """P0: Factory raises ValueError for unknown backend."""
        with self.assertRaises(ValueError) as ctx:
            create_ltm_backend("nonexistent")
        self.assertIn("Unknown backend type", str(ctx.exception))
        self.assertIn("nonexistent", str(ctx.exception))

    def test_register_custom_backend(self):
        """P1: Custom backends can be registered."""
        class MockBackend(LTMBackend):
            def store(self, key, value, metadata=None):
                return {"success": True}

            def retrieve(self, key, default=None):
                return {"success": True, "value": None, "found": False}

            def delete(self, key):
                return {"success": True, "deleted": False}

            def search(self, query=None, metadata_filter=None, limit=10):
                return {"success": True, "results": [], "count": 0}

            def close(self):
                pass

        register_backend("mock", MockBackend)
        self.assertIn("mock", get_registered_backends())

        backend = create_ltm_backend("mock")
        self.assertIsInstance(backend, MockBackend)


class TestBackendConfigParsing(unittest.TestCase):
    """Tests for backend configuration parsing."""

    def test_parse_sqlite_config(self):
        """P1: Parse SQLite configuration."""
        config = {
            "ltm_backend": "sqlite",
            "ltm_path": "/tmp/test.db"
        }
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "sqlite")
        self.assertEqual(kwargs["db_path"], "/tmp/test.db")

    def test_parse_turso_config(self):
        """P1: Parse Turso configuration."""
        config = {
            "ltm_backend": "turso",
            "ltm_url": "libsql://my-db.turso.io",
            "ltm_auth_token": "secret-token"
        }
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "turso")
        self.assertEqual(kwargs["url"], "libsql://my-db.turso.io")
        self.assertEqual(kwargs["auth_token"], "secret-token")

    def test_parse_d1_config(self):
        """P1: Parse Cloudflare D1 configuration."""
        config = {
            "ltm_backend": "d1",
            "ltm_account_id": "account123",
            "ltm_database_id": "db456",
            "ltm_api_token": "api-token"
        }
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "d1")
        self.assertEqual(kwargs["account_id"], "account123")
        self.assertEqual(kwargs["database_id"], "db456")
        self.assertEqual(kwargs["api_token"], "api-token")

    def test_parse_blob_sqlite_config(self):
        """P1: Parse Blob SQLite configuration."""
        config = {
            "ltm_backend": "blob-sqlite",
            "ltm_blob_uri": "gs://bucket/memory.db",
            "ltm_lock_backend": "firestore",
            "ltm_lock_ttl": 300
        }
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "blob-sqlite")
        self.assertEqual(kwargs["blob_uri"], "gs://bucket/memory.db")
        self.assertEqual(kwargs["lock_backend"], "firestore")
        self.assertEqual(kwargs["lock_ttl"], 300)

    def test_parse_litestream_config(self):
        """P1: Parse Litestream configuration."""
        config = {
            "ltm_backend": "litestream",
            "ltm_path": "/tmp/memory.db",
            "litestream_replica": "s3://bucket/replicas/memory"
        }
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "litestream")
        self.assertEqual(kwargs["db_path"], "/tmp/memory.db")
        self.assertEqual(kwargs["replica_url"], "s3://bucket/replicas/memory")

    def test_parse_default_backend(self):
        """P1: Default backend is SQLite."""
        config = {}
        backend_type, kwargs = parse_backend_config(config)
        self.assertEqual(backend_type, "sqlite")


class TestDistributedLockABC(unittest.TestCase):
    """Tests for DistributedLock abstract base class."""

    def test_distributed_lock_is_abc(self):
        """P0: DistributedLock is an ABC."""
        self.assertTrue(issubclass(DistributedLock, ABC))

    def test_distributed_lock_abstract_methods(self):
        """P0: DistributedLock defines required abstract methods."""
        abstract_methods = DistributedLock.__abstractmethods__
        required = {'acquire', 'release', 'refresh', 'is_held'}
        self.assertEqual(abstract_methods, required)

    def test_lock_owner_id_auto_generation(self):
        """P1: Lock auto-generates owner_id if not provided."""
        class MockLock(DistributedLock):
            def acquire(self, timeout=30.0):
                return {"success": True, "acquired": True}

            def release(self):
                return {"success": True, "released": True}

            def refresh(self):
                return {"success": True, "refreshed": True}

            def is_held(self):
                return {"success": True, "held": False}

        lock = MockLock(resource_id="test_resource", ttl=300)
        self.assertIsNotNone(lock.owner_id)
        self.assertTrue(len(lock.owner_id) > 0)

    def test_lock_context_manager_acquire_success(self):
        """P1: Lock context manager calls acquire on enter."""
        class MockLock(DistributedLock):
            def __init__(self, *args, **kwargs):
                super().__init__(*args, **kwargs)
                self.acquire_called = False
                self.release_called = False

            def acquire(self, timeout=30.0):
                self.acquire_called = True
                return {"success": True, "acquired": True}

            def release(self):
                self.release_called = True
                return {"success": True, "released": True}

            def refresh(self):
                return {"success": True, "refreshed": True}

            def is_held(self):
                return {"success": True, "held": True}

        lock = MockLock(resource_id="test", ttl=300)
        with lock:
            self.assertTrue(lock.acquire_called)
        self.assertTrue(lock.release_called)

    def test_lock_context_manager_acquire_failure(self):
        """P1: Lock context manager raises on acquire failure."""
        class FailingLock(DistributedLock):
            def acquire(self, timeout=30.0):
                return {"success": False, "error": "Lock held", "error_type": "lock_timeout"}

            def release(self):
                return {"success": True, "released": True}

            def refresh(self):
                return {"success": True, "refreshed": True}

            def is_held(self):
                return {"success": True, "held": False}

        lock = FailingLock(resource_id="test", ttl=300)
        with self.assertRaises(RuntimeError) as ctx:
            with lock:
                pass
        self.assertIn("Lock acquisition failed", str(ctx.exception))


class TestLockRegistry(unittest.TestCase):
    """Tests for lock registry and factory pattern."""

    def test_get_registered_locks_empty_initially(self):
        """P1: Lock registry may be empty if no backends installed."""
        # This test just verifies the registry works
        locks = get_registered_locks()
        self.assertIsInstance(locks, list)

    def test_register_custom_lock(self):
        """P1: Custom locks can be registered."""
        class MockLock(DistributedLock):
            def acquire(self, timeout=30.0):
                return {"success": True, "acquired": True}

            def release(self):
                return {"success": True, "released": True}

            def refresh(self):
                return {"success": True, "refreshed": True}

            def is_held(self):
                return {"success": True, "held": False}

        register_lock("mock", MockLock)
        self.assertIn("mock", get_registered_locks())

        lock = create_lock("mock", resource_id="test", ttl=300)
        self.assertIsInstance(lock, MockLock)

    def test_create_unknown_lock_raises(self):
        """P0: Factory raises ValueError for unknown lock."""
        with self.assertRaises(ValueError) as ctx:
            create_lock("nonexistent", resource_id="test")
        self.assertIn("Unknown lock type", str(ctx.exception))


class TestLTMBackendContract(unittest.TestCase):
    """Tests verifying LTMBackend implementations follow the contract."""

    def setUp(self):
        """Create fresh backend for each test."""
        self.backend = SQLiteBackend(":memory:")

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    def test_store_returns_dict_with_success(self):
        """P0: store() returns dict with 'success' key."""
        result = self.backend.store("key", "value")
        self.assertIn("success", result)
        self.assertIsInstance(result["success"], bool)

    def test_store_success_has_required_fields(self):
        """P0: Successful store has required fields."""
        result = self.backend.store("key", "value")
        self.assertTrue(result["success"])
        self.assertIn("stored", result)
        self.assertIn("key", result)
        self.assertIn("created", result)

    def test_retrieve_returns_dict_with_success(self):
        """P0: retrieve() returns dict with 'success' key."""
        result = self.backend.retrieve("nonexistent")
        self.assertIn("success", result)
        self.assertIsInstance(result["success"], bool)

    def test_retrieve_success_has_required_fields(self):
        """P0: Successful retrieve has required fields."""
        self.backend.store("key", "value")
        result = self.backend.retrieve("key")
        self.assertTrue(result["success"])
        self.assertIn("value", result)
        self.assertIn("found", result)
        self.assertIn("metadata", result)

    def test_delete_returns_dict_with_success(self):
        """P0: delete() returns dict with 'success' key."""
        result = self.backend.delete("nonexistent")
        self.assertIn("success", result)
        self.assertIsInstance(result["success"], bool)

    def test_delete_success_has_required_fields(self):
        """P0: Successful delete has required fields."""
        self.backend.store("key", "value")
        result = self.backend.delete("key")
        self.assertTrue(result["success"])
        self.assertIn("deleted", result)
        self.assertIn("key", result)

    def test_search_returns_dict_with_success(self):
        """P0: search() returns dict with 'success' key."""
        result = self.backend.search()
        self.assertIn("success", result)
        self.assertIsInstance(result["success"], bool)

    def test_search_success_has_required_fields(self):
        """P0: Successful search has required fields."""
        result = self.backend.search()
        self.assertTrue(result["success"])
        self.assertIn("results", result)
        self.assertIn("count", result)
        self.assertIsInstance(result["results"], list)

    def test_error_response_has_required_fields(self):
        """P0: Error responses have error and error_type."""
        result = self.backend.store("", "value")  # Empty key triggers error
        self.assertFalse(result["success"])
        self.assertIn("error", result)
        self.assertIn("error_type", result)


if __name__ == '__main__':
    unittest.main()
