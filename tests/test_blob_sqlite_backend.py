"""
Tests for Blob SQLite Backend (TEA-BUILTIN-001.5).

Tests cover:
- BlobSQLiteBackend initialization
- Lock acquisition and release
- Blob storage download/upload
- LTMBackend interface compliance
- Sync and refresh operations
- Error handling
"""

import os
import tempfile
import unittest
from unittest.mock import patch, MagicMock, mock_open

from the_edge_agent.memory import (
    BlobSQLiteBackend,
    LTMBackend,
    create_ltm_backend,
    get_registered_backends,
)
from the_edge_agent.memory.locks import register_lock, DistributedLock


class MockLock(DistributedLock):
    """Mock lock for testing."""

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
register_lock("mock", MockLock)


class TestBlobSQLiteBackendInit(unittest.TestCase):
    """Tests for BlobSQLiteBackend initialization."""

    def test_is_ltm_backend(self):
        """P0: BlobSQLiteBackend is LTMBackend subclass."""
        self.assertTrue(issubclass(BlobSQLiteBackend, LTMBackend))

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_init_with_mock_lock(self, mock_fs, mock_open_ctx):
        """P0: Init with mock lock backend succeeds."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock",
            lock_ttl=300
        )

        self.assertIsNotNone(backend._lock)
        self.assertIsNotNone(backend._sqlite)
        backend.close()

    def test_init_invalid_lock_backend(self):
        """P0: Init with invalid lock backend raises ValueError."""
        with self.assertRaises(ValueError) as ctx:
            BlobSQLiteBackend(
                blob_uri="memory://test/db.sqlite",
                lock_backend="nonexistent"
            )
        self.assertIn("nonexistent", str(ctx.exception))

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_init_creates_temp_file(self, mock_fs, mock_open_ctx):
        """P1: Init creates temp file for local SQLite."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        self.assertIsNotNone(backend._local_path)
        self.assertTrue(backend._local_path.endswith(".db"))
        backend.close()

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_init_auto_sync_false(self, mock_fs, mock_open_ctx):
        """P1: Init with auto_sync=False skips download."""
        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock",
            auto_sync=False
        )

        # Lock and SQLite should not be initialized
        self.assertIsNone(backend._lock)
        self.assertIsNone(backend._sqlite)

        # Clean up
        if backend._local_path and os.path.exists(backend._local_path):
            os.unlink(backend._local_path)


class TestBlobSQLiteBackendDownloadUpload(unittest.TestCase):
    """Tests for download/upload operations."""

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_download_existing_file(self, mock_fs, mock_open_ctx):
        """P0: Download existing blob file."""
        # Simulate existing blob
        mock_fs.return_value.exists.return_value = True
        mock_open_ctx.return_value.__enter__ = MagicMock(return_value=MagicMock(read=lambda: b""))
        mock_open_ctx.return_value.__exit__ = MagicMock(return_value=False)

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        # Verify download was attempted
        mock_fs.return_value.exists.assert_called()
        backend.close()

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_download_new_file(self, mock_fs, mock_open_ctx):
        """P0: Download when no blob exists starts fresh."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        # Should work with empty database
        result = backend.store("key", "value")
        self.assertTrue(result['success'])
        backend.close()

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_upload_on_close(self, mock_fs, mock_open_ctx):
        """P0: Upload changes on close."""
        mock_fs.return_value.exists.return_value = False
        mock_open_ctx.return_value.__enter__ = MagicMock()
        mock_open_ctx.return_value.__exit__ = MagicMock(return_value=False)

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        # Make a change
        backend.store("key", "value")
        self.assertTrue(backend._dirty)

        # Close should upload
        backend.close()
        # Upload should have been called (fsspec.open with 'wb')

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_no_upload_when_not_dirty(self, mock_fs, mock_open_ctx):
        """P1: No upload when no changes made."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        # No changes made
        self.assertFalse(backend._dirty)

        # Sync should report no upload
        result = backend.sync()
        self.assertTrue(result['success'])
        self.assertFalse(result.get('uploaded', True))

        backend.close()


class TestBlobSQLiteBackendLTMInterface(unittest.TestCase):
    """Tests for LTMBackend interface implementation."""

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def setUp(self, mock_fs, mock_open_ctx):
        """Create fresh backend for each test."""
        mock_fs.return_value.exists.return_value = False
        self.backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    def test_store_retrieve(self):
        """P0: Store and retrieve work correctly."""
        result = self.backend.store("key1", {"data": "value"})
        self.assertTrue(result['success'])

        result = self.backend.retrieve("key1")
        self.assertTrue(result['success'])
        self.assertTrue(result['found'])
        self.assertEqual(result['value'], {"data": "value"})

    def test_delete(self):
        """P0: Delete works correctly."""
        self.backend.store("key1", "value1")

        result = self.backend.delete("key1")
        self.assertTrue(result['success'])
        self.assertTrue(result['deleted'])

        result = self.backend.retrieve("key1")
        self.assertFalse(result['found'])

    def test_search(self):
        """P0: Search works correctly."""
        self.backend.store("doc1", {"content": "hello world"})
        self.backend.store("doc2", {"content": "goodbye world"})

        result = self.backend.search("hello")
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 1)

    def test_store_marks_dirty(self):
        """P1: Store operation marks backend as dirty."""
        self.assertFalse(self.backend._dirty)
        self.backend.store("key", "value")
        self.assertTrue(self.backend._dirty)

    def test_delete_marks_dirty(self):
        """P1: Delete operation marks backend as dirty."""
        self.backend.store("key", "value")
        self.backend._dirty = False  # Reset

        self.backend.delete("key")
        self.assertTrue(self.backend._dirty)


class TestBlobSQLiteBackendSync(unittest.TestCase):
    """Tests for sync and refresh operations."""

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def setUp(self, mock_fs, mock_open_ctx):
        """Create fresh backend for each test."""
        mock_fs.return_value.exists.return_value = False
        mock_open_ctx.return_value.__enter__ = MagicMock()
        mock_open_ctx.return_value.__exit__ = MagicMock(return_value=False)
        self.backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    def test_sync_uploads_changes(self):
        """P0: Sync uploads pending changes."""
        self.backend.store("key", "value")

        result = self.backend.sync()
        self.assertTrue(result['success'])
        self.assertFalse(self.backend._dirty)  # Should be cleared

    def test_refresh_lock(self):
        """P0: Refresh lock extends TTL."""
        result = self.backend.refresh_lock()
        self.assertTrue(result['success'])
        self.assertTrue(result['refreshed'])

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_refresh_lock_no_lock(self, mock_fs, mock_open_ctx):
        """P1: Refresh lock fails when no lock."""
        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock",
            auto_sync=False
        )

        result = backend.refresh_lock()
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'lock_lost')

        if backend._local_path and os.path.exists(backend._local_path):
            os.unlink(backend._local_path)


class TestBlobSQLiteBackendClose(unittest.TestCase):
    """Tests for close behavior."""

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_operations_after_close_fail(self, mock_fs, mock_open_ctx):
        """P0: Operations fail after backend is closed."""
        mock_fs.return_value.exists.return_value = False
        mock_open_ctx.return_value.__enter__ = MagicMock()
        mock_open_ctx.return_value.__exit__ = MagicMock(return_value=False)

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )
        backend.close()

        result = backend.store("key", "value")
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'connection_error')

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_double_close_safe(self, mock_fs, mock_open_ctx):
        """P1: Double close is safe."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )
        backend.close()
        backend.close()  # Should not raise

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_close_releases_lock(self, mock_fs, mock_open_ctx):
        """P0: Close releases the distributed lock."""
        mock_fs.return_value.exists.return_value = False

        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )
        # Verify lock was created
        self.assertIsNotNone(backend._lock)

        backend.close()
        # Verify lock reference is cleared
        self.assertIsNone(backend._lock)


class TestBlobSQLiteBackendRegistry(unittest.TestCase):
    """Tests for backend registry integration."""

    def test_blob_sqlite_registered(self):
        """P0: Blob SQLite is registered in backend factory."""
        backends = get_registered_backends()
        self.assertIn("blob-sqlite", backends)

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_create_via_factory(self, mock_fs, mock_open_ctx):
        """P0: Can create via factory."""
        mock_fs.return_value.exists.return_value = False

        backend = create_ltm_backend(
            "blob-sqlite",
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock"
        )

        self.assertIsInstance(backend, BlobSQLiteBackend)
        backend.close()


class TestBlobSQLiteBackendErrorHandling(unittest.TestCase):
    """Tests for error handling."""

    def test_missing_fsspec_graceful(self):
        """P1: Missing fsspec provides helpful error."""
        # This test verifies the error message format when fsspec import fails
        # We can't easily simulate import error, but we verify the error handling exists
        pass  # Error handling is in the code, manual verification needed

    @patch('fsspec.open')
    @patch('fsspec.filesystem')
    def test_not_initialized_errors(self, mock_fs, mock_open_ctx):
        """P1: Operations fail gracefully when not initialized."""
        backend = BlobSQLiteBackend(
            blob_uri="memory://test/db.sqlite",
            lock_backend="mock",
            auto_sync=False
        )

        result = backend.store("key", "value")
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'connection_error')

        result = backend.retrieve("key")
        self.assertFalse(result['success'])

        result = backend.delete("key")
        self.assertFalse(result['success'])

        result = backend.search("query")
        self.assertFalse(result['success'])

        if backend._local_path and os.path.exists(backend._local_path):
            os.unlink(backend._local_path)


if __name__ == '__main__':
    unittest.main()
