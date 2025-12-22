"""
Integration Tests for LTM Backends (TEA-BUILTIN-001.5).

Tests cover:
- Backend switching (apps can switch between backends)
- Graceful degradation (error handling when deps missing)
- Search parity (FTS5 works consistently across backends)
- Factory pattern integration
"""

import os
import tempfile
import unittest
from unittest.mock import patch, MagicMock

from the_edge_agent.memory import (
    LTMBackend,
    SQLiteBackend,
    LitestreamBackend,
    BlobSQLiteBackend,
    create_ltm_backend,
    get_registered_backends,
)
from the_edge_agent.memory.locks import register_lock, DistributedLock


class MockLockForIntegration(DistributedLock):
    """Mock lock for integration testing."""

    def __init__(self, resource_id: str, ttl: int = 300, owner_id=None, **kwargs):
        super().__init__(resource_id, ttl, owner_id)
        self._held = False

    def acquire(self, timeout: float = 30.0):
        self._held = True
        self._acquired = True
        return {"success": True, "acquired": True, "owner_id": self.owner_id}

    def release(self):
        self._held = False
        self._acquired = False
        return {"success": True, "released": True}

    def refresh(self):
        if self._held:
            return {"success": True, "refreshed": True, "new_ttl": self.ttl}
        return {"success": False, "error": "Lock not held", "error_type": "lock_lost"}

    def is_held(self):
        return {"success": True, "held": self._held, "remaining_ttl": self.ttl if self._held else None}


# Register mock lock for testing
register_lock("mock-integration", MockLockForIntegration)


class TestBackendSwitching(unittest.TestCase):
    """Test that applications can switch between backends seamlessly."""

    def test_all_backends_implement_ltm_interface(self):
        """P0: All backends implement LTMBackend interface."""
        backends = [SQLiteBackend, LitestreamBackend, BlobSQLiteBackend]
        for backend_class in backends:
            self.assertTrue(
                issubclass(backend_class, LTMBackend),
                f"{backend_class.__name__} should be LTMBackend subclass"
            )

    def test_factory_creates_correct_types(self):
        """P0: Factory creates correct backend types."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # SQLite
            sqlite_path = os.path.join(tmpdir, "sqlite.db")
            backend = create_ltm_backend("sqlite", db_path=sqlite_path)
            self.assertIsInstance(backend, SQLiteBackend)
            backend.close()

    def test_switch_from_sqlite_to_litestream(self):
        """P1: Can switch from SQLite to Litestream backend."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "memory.db")

            # Start with SQLite
            sqlite = SQLiteBackend(db_path)
            sqlite.store("key1", {"data": "original"})
            sqlite.close()

            # Switch to Litestream (wraps same SQLite file)
            with patch('subprocess.run') as mock_run:
                mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
                litestream = LitestreamBackend(
                    db_path=db_path,
                    replica_url="s3://bucket/replica",
                    auto_restore=False
                )

                # Data should still be there
                result = litestream.retrieve("key1")
                self.assertTrue(result['success'])
                self.assertTrue(result['found'])
                self.assertEqual(result['value'], {"data": "original"})

                litestream.close()

    def test_backend_interface_consistency(self):
        """P0: All backends return consistent response formats."""
        with tempfile.TemporaryDirectory() as tmpdir:
            backends = []

            # SQLite backend
            sqlite_path = os.path.join(tmpdir, "sqlite.db")
            backends.append(("SQLite", SQLiteBackend(sqlite_path)))

            # Litestream backend
            with patch('subprocess.run') as mock_run:
                mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
                litestream_path = os.path.join(tmpdir, "litestream.db")
                backends.append(("Litestream", LitestreamBackend(
                    db_path=litestream_path,
                    replica_url="s3://bucket/replica",
                    auto_restore=False
                )))

            for name, backend in backends:
                # Test store response format
                result = backend.store("test_key", {"value": 42})
                self.assertIn('success', result, f"{name}: store should have 'success'")
                self.assertTrue(result['success'], f"{name}: store should succeed")

                # Test retrieve response format
                result = backend.retrieve("test_key")
                self.assertIn('success', result, f"{name}: retrieve should have 'success'")
                self.assertIn('found', result, f"{name}: retrieve should have 'found'")
                self.assertIn('value', result, f"{name}: retrieve should have 'value'")

                # Test delete response format
                result = backend.delete("test_key")
                self.assertIn('success', result, f"{name}: delete should have 'success'")
                self.assertIn('deleted', result, f"{name}: delete should have 'deleted'")

                # Test search response format
                backend.store("doc1", {"content": "hello world"})
                result = backend.search("hello")
                self.assertIn('success', result, f"{name}: search should have 'success'")
                self.assertIn('results', result, f"{name}: search should have 'results'")
                self.assertIn('count', result, f"{name}: search should have 'count'")

                backend.close()


class TestGracefulDegradation(unittest.TestCase):
    """Test graceful error handling when dependencies are missing."""

    def test_blob_sqlite_invalid_lock_backend(self):
        """P0: BlobSQLite raises helpful error for invalid lock."""
        with self.assertRaises(ValueError) as ctx:
            BlobSQLiteBackend(
                blob_uri="memory://test/db.sqlite",
                lock_backend="nonexistent_lock"
            )
        self.assertIn("nonexistent_lock", str(ctx.exception))

    def test_litestream_memory_path_rejected(self):
        """P0: Litestream rejects :memory: path."""
        with self.assertRaises(ValueError) as ctx:
            LitestreamBackend(
                db_path=":memory:",
                replica_url="s3://bucket/replica"
            )
        self.assertIn(":memory:", str(ctx.exception))

    def test_operations_after_close_fail_gracefully(self):
        """P0: Operations fail gracefully after backend close."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")
            backend = SQLiteBackend(db_path)
            backend.close()

            # All operations should fail gracefully with error dict
            result = backend.store("key", "value")
            self.assertFalse(result['success'])
            self.assertIn('error', result)

            result = backend.retrieve("key")
            self.assertFalse(result['success'])

            result = backend.delete("key")
            self.assertFalse(result['success'])

            result = backend.search("query")
            self.assertFalse(result['success'])


class TestSearchParity(unittest.TestCase):
    """Test that FTS5 search works consistently across backends."""

    def _create_test_documents(self, backend):
        """Create test documents for search testing."""
        docs = [
            ("doc1", {"content": "Python programming language guide"}),
            ("doc2", {"content": "JavaScript web development tutorial"}),
            ("doc3", {"content": "Python machine learning basics"}),
            ("doc4", {"content": "Database SQL programming fundamentals"}),
            ("doc5", {"content": "Python data science and analytics"}),
        ]
        for key, value in docs:
            backend.store(key, value)
        return docs

    def test_sqlite_fts5_search(self):
        """P0: SQLite FTS5 search works correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")
            backend = SQLiteBackend(db_path)

            self._create_test_documents(backend)

            # Search for Python docs
            result = backend.search("Python")
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 3)  # doc1, doc3, doc5

            # Search for programming
            result = backend.search("programming")
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 2)  # doc1, doc4

            backend.close()

    @patch('subprocess.run')
    def test_litestream_fts5_search(self, mock_run):
        """P0: Litestream FTS5 search works correctly."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")
            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            self._create_test_documents(backend)

            # Search for Python docs
            result = backend.search("Python")
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 3)

            # Search for programming
            result = backend.search("programming")
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 2)

            backend.close()

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_blob_sqlite_fts5_search(self, mock_fs, mock_open_ctx):
        """P0: BlobSQLite FTS5 search works correctly."""
        mock_fs.return_value.exists.return_value = False
        mock_open_ctx.return_value.__enter__ = MagicMock()
        mock_open_ctx.return_value.__exit__ = MagicMock(return_value=False)

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock-integration"
        )

        self._create_test_documents(backend)

        # Search for Python docs
        result = backend.search("Python")
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 3)

        # Search for programming
        result = backend.search("programming")
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 2)

        backend.close()

    def test_search_with_metadata_filter(self):
        """P1: Metadata filtering works consistently."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")
            backend = SQLiteBackend(db_path)

            # Store with metadata
            backend.store("py1", {"content": "Python basics"}, metadata={"lang": "python"})
            backend.store("py2", {"content": "Python advanced"}, metadata={"lang": "python"})
            backend.store("js1", {"content": "JavaScript basics"}, metadata={"lang": "javascript"})

            # Filter by metadata
            result = backend.search(metadata_filter={"lang": "python"})
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 2)

            # Combine text search with metadata filter
            result = backend.search("basics", metadata_filter={"lang": "python"})
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 1)

            backend.close()


class TestFactoryPatternIntegration(unittest.TestCase):
    """Test the factory pattern for backend creation."""

    def test_all_backends_registered(self):
        """P0: All backends are registered in factory."""
        backends = get_registered_backends()
        self.assertIn("sqlite", backends)
        self.assertIn("litestream", backends)
        self.assertIn("blob-sqlite", backends)

    def test_factory_with_kwargs(self):
        """P0: Factory passes kwargs correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            # SQLite with db_path
            backend = create_ltm_backend("sqlite", db_path=db_path)
            self.assertEqual(backend.db_path, db_path)
            backend.close()

    @patch('subprocess.run')
    def test_factory_litestream(self, mock_run):
        """P1: Factory creates Litestream correctly."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = create_ltm_backend(
                "litestream",
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )
            self.assertIsInstance(backend, LitestreamBackend)
            self.assertEqual(backend.replica_url, "s3://bucket/replica")
            backend.close()

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_factory_blob_sqlite(self, mock_fs, mock_open_ctx):
        """P1: Factory creates BlobSQLite correctly."""
        mock_fs.return_value.exists.return_value = False

        backend = create_ltm_backend(
            "blob-sqlite",
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock-integration"
        )
        self.assertIsInstance(backend, BlobSQLiteBackend)
        self.assertEqual(backend.blob_uri, "memory://test/db.sqlite")
        backend.close()

    def test_factory_unknown_backend(self):
        """P0: Factory raises error for unknown backend."""
        with self.assertRaises(ValueError) as ctx:
            create_ltm_backend("nonexistent_backend")
        self.assertIn("nonexistent_backend", str(ctx.exception))


class TestDataMigration(unittest.TestCase):
    """Test data migration between backends."""

    @patch('subprocess.run')
    def test_data_survives_backend_switch(self, mock_run):
        """P1: Data survives switching between backends."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "memory.db")

            # Phase 1: Use SQLite
            sqlite = SQLiteBackend(db_path)
            sqlite.store("user", {"name": "Alice", "age": 30})
            sqlite.store("config", {"theme": "dark", "lang": "en"})
            sqlite.close()

            # Phase 2: Switch to Litestream (same file)
            litestream = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            # Verify data
            result = litestream.retrieve("user")
            self.assertTrue(result['found'])
            self.assertEqual(result['value']['name'], "Alice")

            result = litestream.retrieve("config")
            self.assertTrue(result['found'])
            self.assertEqual(result['value']['theme'], "dark")

            # Add new data
            litestream.store("session", {"id": "abc123"})
            litestream.close()

            # Phase 3: Back to SQLite
            sqlite2 = SQLiteBackend(db_path)
            result = sqlite2.retrieve("session")
            self.assertTrue(result['found'])
            self.assertEqual(result['value']['id'], "abc123")
            sqlite2.close()


if __name__ == '__main__':
    unittest.main()
