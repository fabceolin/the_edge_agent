"""
Tests for Litestream Backend (TEA-BUILTIN-001.5).

Tests cover:
- LitestreamBackend initialization
- Restore from replica (mocked)
- Snapshot trigger
- LTMBackend interface compliance
- Error handling
"""

import os
import tempfile
import unittest
from unittest.mock import patch, MagicMock

from the_edge_agent.memory import (
    LitestreamBackend,
    LTMBackend,
    create_ltm_backend,
    get_registered_backends,
)


class TestLitestreamBackendInit(unittest.TestCase):
    """Tests for LitestreamBackend initialization."""

    def test_memory_path_not_supported(self):
        """P0: :memory: path is rejected."""
        with self.assertRaises(ValueError) as ctx:
            LitestreamBackend(
                db_path=":memory:",
                replica_url="s3://bucket/replica"
            )
        self.assertIn(":memory:", str(ctx.exception))

    def test_is_ltm_backend(self):
        """P0: LitestreamBackend is LTMBackend subclass."""
        self.assertTrue(issubclass(LitestreamBackend, LTMBackend))

    @patch('subprocess.run')
    def test_init_with_existing_db(self, mock_run):
        """P1: Init with existing database doesn't restore."""
        with tempfile.NamedTemporaryFile(suffix=".db", delete=False) as f:
            db_path = f.name

        try:
            # Create a real database first
            from the_edge_agent.memory import SQLiteBackend
            sqlite = SQLiteBackend(db_path)
            sqlite.store("existing", "data")
            sqlite.close()

            # Now create Litestream backend - should not call restore
            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=True
            )

            # Restore should not be called since file exists
            # (subprocess.run might be called for version check, but not restore)
            for call in mock_run.call_args_list:
                args = call[0][0] if call[0] else call[1].get('args', [])
                self.assertNotIn('restore', args)

            backend.close()
        finally:
            os.unlink(db_path)

    @patch('subprocess.run')
    def test_init_with_auto_restore_disabled(self, mock_run):
        """P1: Init with auto_restore=False skips restore."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            # Restore should not be called
            restore_calls = [
                call for call in mock_run.call_args_list
                if 'restore' in str(call)
            ]
            self.assertEqual(len(restore_calls), 0)

            backend.close()


class TestLitestreamBackendRestore(unittest.TestCase):
    """Tests for restore from replica."""

    @patch('subprocess.run')
    def test_restore_success(self, mock_run):
        """P0: Successful restore from replica."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="Restored successfully",
            stderr=""
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica"
            )

            # Verify restore was called
            restore_calls = [
                call for call in mock_run.call_args_list
                if 'restore' in call[0][0]
            ]
            self.assertGreater(len(restore_calls), 0)

            backend.close()

    @patch('subprocess.run')
    def test_restore_no_backups_ok(self, mock_run):
        """P1: No backups found is OK for first run."""
        mock_run.return_value = MagicMock(
            returncode=1,
            stdout="",
            stderr="no backups found"
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            # Should not raise, starts with empty database
            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica"
            )

            # Should still work with empty database
            result = backend.store("key", "value")
            self.assertTrue(result['success'])

            backend.close()


class TestLitestreamBackendSnapshot(unittest.TestCase):
    """Tests for snapshot trigger."""

    @patch('subprocess.run')
    def test_trigger_snapshot_success(self, mock_run):
        """P1: Trigger snapshot checkpoints WAL."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="litestream v0.3.13",
            stderr=""
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            result = backend.trigger_snapshot()
            self.assertTrue(result['success'])
            self.assertTrue(result['snapshot'])

            backend.close()

    @patch('subprocess.run')
    def test_trigger_snapshot_litestream_not_available(self, mock_run):
        """P1: Snapshot fails gracefully if Litestream not available."""
        mock_run.side_effect = FileNotFoundError()

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            result = backend.trigger_snapshot()
            self.assertFalse(result['success'])
            self.assertEqual(result['error_type'], 'dependency_missing')

            backend.close()


class TestLitestreamBackendInfo(unittest.TestCase):
    """Tests for replica info and config generation."""

    @patch('subprocess.run')
    def test_get_replica_info(self, mock_run):
        """P1: Get replica configuration info."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="litestream v0.3.13",
            stderr=""
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://bucket/replica",
                auto_restore=False
            )

            info = backend.get_replica_info()
            self.assertTrue(info['success'])
            self.assertIn(db_path, info['db_path'])
            self.assertEqual(info['replica_url'], "s3://bucket/replica")

            backend.close()

    @patch('subprocess.run')
    def test_generate_config(self, mock_run):
        """P1: Generate Litestream config YAML."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test.db")

            backend = LitestreamBackend(
                db_path=db_path,
                replica_url="s3://my-bucket/replicas/test",
                auto_restore=False
            )

            config = backend.generate_config()
            self.assertIn("path:", config)
            self.assertIn(db_path, config)
            self.assertIn("s3://my-bucket/replicas/test", config)

            backend.close()


class TestLitestreamBackendLTMInterface(unittest.TestCase):
    """Tests for LTMBackend interface implementation."""

    @patch('subprocess.run')
    def setUp(self, mock_run):
        """Create fresh backend for each test."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
        self.tmpdir = tempfile.mkdtemp()
        self.db_path = os.path.join(self.tmpdir, "test.db")
        self.backend = LitestreamBackend(
            db_path=self.db_path,
            replica_url="s3://bucket/replica",
            auto_restore=False
        )

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()
        import shutil
        shutil.rmtree(self.tmpdir, ignore_errors=True)

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

    def test_operations_after_close_fail(self):
        """P1: Operations fail after backend is closed."""
        self.backend.close()

        result = self.backend.store("key", "value")
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'connection_error')


class TestLitestreamBackendRegistry(unittest.TestCase):
    """Tests for backend registry integration."""

    def test_litestream_registered(self):
        """P0: Litestream is registered in backend factory."""
        backends = get_registered_backends()
        self.assertIn("litestream", backends)

    @patch('subprocess.run')
    def test_create_via_factory(self, mock_run):
        """P0: Can create via factory."""
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
            backend.close()


if __name__ == '__main__':
    unittest.main()
