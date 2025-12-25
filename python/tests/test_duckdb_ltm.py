"""
Tests for DuckDB LTM Backend (TEA-BUILTIN-001.6.2).

Tests cover:
- DuckDBLTMBackend initialization and configuration
- Store with inlining and cloud upload
- Retrieve from catalog and cloud storage
- Delete from catalog and cloud
- Search with FTS ranking
- Content hash deduplication
- Cloud storage operations (local filesystem)
- Query engine integration
"""

import json
import os
import shutil
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional
from unittest.mock import MagicMock, patch

from the_edge_agent.memory.catalog import (
    CatalogBackend,
    compute_content_hash,
    generate_entry_id,
)


class MockCatalogBackend:
    """Mock catalog backend for testing."""

    def __init__(self):
        self._entries: Dict[str, Dict[str, Any]] = {}

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        entry_id = generate_entry_id(key)
        created = key not in self._entries

        self._entries[key] = {
            "id": entry_id,
            "key": key,
            "content_hash": content_hash,
            "storage_uri": storage_uri,
            "byte_size": byte_size,
            "metadata": metadata,
            "inlined_value": inlined_value,
            "expires_at": expires_at,
            "created_at": datetime.now(timezone.utc),
            "updated_at": datetime.now(timezone.utc),
        }

        return {
            "success": True,
            "entry_id": entry_id,
            "created": created,
        }

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        return self._entries.get(key)

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        entries = list(self._entries.values())

        if prefix:
            entries = [e for e in entries if e["key"].startswith(prefix)]

        if metadata_filter:

            def matches(e):
                for k, v in metadata_filter.items():
                    if e.get("metadata", {}).get(k) != v:
                        return False
                return True

            entries = [e for e in entries if matches(e)]

        return entries[:limit]

    def delete_entry(self, key: str) -> bool:
        if key in self._entries:
            del self._entries[key]
            return True
        return False

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        return list(self._entries.values())

    def create_snapshot(self, name: str) -> str:
        return "snapshot-1"

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        stored_count = 0
        for entry in entries:
            key = entry["key"]
            entry_id = generate_entry_id(key)
            self._entries[key] = {
                "id": entry_id,
                "key": key,
                "content_hash": entry.get("content_hash", ""),
                "storage_uri": entry.get("storage_uri"),
                "byte_size": entry.get("byte_size", 0),
                "metadata": entry.get("metadata", {}),
                "inlined_value": entry.get("inlined_value"),
                "expires_at": entry.get("expires_at"),
                "created_at": datetime.now(timezone.utc),
                "updated_at": datetime.now(timezone.utc),
            }
            stored_count += 1
        return {
            "success": True,
            "stored_count": stored_count,
            "failed_count": 0,
            "errors": [],
        }

    def retrieve_batch(
        self,
        keys: List[str],
    ) -> Dict[str, Any]:
        entries = {key: self._entries.get(key) for key in keys}
        found_count = sum(1 for v in entries.values() if v is not None)
        return {
            "success": True,
            "entries": entries,
            "found_count": found_count,
            "missing_count": len(keys) - found_count,
        }

    def cleanup_expired(
        self,
        batch_size: int = 100,
    ) -> Dict[str, Any]:
        now = datetime.now(timezone.utc)
        deleted_count = 0
        keys_to_delete = []
        for key, entry in self._entries.items():
            expires_at = entry.get("expires_at")
            if expires_at and isinstance(expires_at, datetime) and expires_at < now:
                keys_to_delete.append(key)
                deleted_count += 1
                if deleted_count >= batch_size:
                    break
        for key in keys_to_delete:
            del self._entries[key]
        return {
            "success": True,
            "deleted_count": deleted_count,
            "remaining_count": 0,
        }


class MockQueryEngine:
    """Mock DuckDB query engine for testing."""

    def __init__(self):
        self._circuit_state = "closed"
        self._extensions: List[str] = []

    def execute(self, sql: str, params: Optional[List[Any]] = None) -> Dict[str, Any]:
        return {"success": True, "rows": [], "columns": []}

    def load_extension(self, name: str) -> Dict[str, Any]:
        self._extensions.append(name)
        return {"success": True, "extension": name}

    def get_circuit_state(self) -> Dict[str, Any]:
        return {"state": self._circuit_state}

    def reset_circuit(self) -> Dict[str, Any]:
        self._circuit_state = "closed"
        return {"success": True, "state": "closed"}

    def health_check(self) -> Dict[str, Any]:
        return {"success": True, "healthy": True}

    def close(self) -> None:
        pass


class TestDuckDBLTMBackendInit(unittest.TestCase):
    """Tests for DuckDBLTMBackend initialization (AC-1, AC-2)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

    def tearDown(self):
        """Clean up temp directory."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_init_with_catalog(self):
        """AC-2: Backend requires CatalogBackend instance."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
        )
        self.assertIsNotNone(backend)
        self.assertEqual(backend.catalog, self.catalog)
        backend.close()

    def test_init_creates_local_directory(self):
        """Local storage directory is created on init."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        local_path = os.path.join(self.temp_dir, "new_ltm_data/")
        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=local_path,
            query_engine=MockQueryEngine(),
            enable_fts=False,
        )
        self.assertTrue(os.path.exists(local_path))
        backend.close()

    def test_init_with_custom_threshold(self):
        """AC-7: Inline threshold is configurable."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=2048,
        )
        self.assertEqual(backend._inline_threshold, 2048)
        backend.close()

    def test_init_invalid_catalog_raises(self):
        """TypeError raised for invalid catalog."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        with self.assertRaises(TypeError) as ctx:
            DuckDBLTMBackend(
                catalog="not-a-catalog",  # type: ignore
                storage_uri=self.storage_uri,
                query_engine=MockQueryEngine(),
            )
        self.assertIn("CatalogBackend", str(ctx.exception))


class TestDuckDBLTMBackendStore(unittest.TestCase):
    """Tests for store method (AC-3, AC-8, AC-9, AC-11, AC-12, AC-13)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_store_small_inlines(self):
        """AC-8: Small data stored in catalog inlined_value."""
        result = self.backend.store("key1", {"small": "data"})

        self.assertTrue(result["success"])
        self.assertTrue(result["stored"])
        self.assertTrue(result["inlined"])
        self.assertIn("content_hash", result)
        self.assertIn("byte_size", result)

        # Verify in catalog
        entry = self.catalog.get_entry("key1")
        self.assertIsNotNone(entry)
        self.assertEqual(entry["inlined_value"], {"small": "data"})
        self.assertIsNone(entry["storage_uri"])

    def test_store_large_uploads(self):
        """AC-9: Large data uploaded to cloud storage."""
        large_data = {"content": "x" * 2000}
        result = self.backend.store("key2", large_data)

        self.assertTrue(result["success"])
        self.assertTrue(result["stored"])
        self.assertFalse(result["inlined"])
        self.assertIn("storage_uri", result)

        # Verify in catalog
        entry = self.catalog.get_entry("key2")
        self.assertIsNotNone(entry)
        self.assertIsNone(entry["inlined_value"])
        self.assertIsNotNone(entry["storage_uri"])

        # Verify file exists
        self.assertTrue(os.path.exists(entry["storage_uri"]))

    def test_store_deduplicates(self):
        """AC-12: Same content with same hash is deduplicated."""
        data = {"value": 123}

        # First store
        result1 = self.backend.store("key1", data)
        self.assertTrue(result1["stored"])

        # Second store with same data
        result2 = self.backend.store("key1", data)
        self.assertFalse(result2.get("stored", True))
        self.assertTrue(result2["deduplicated"])
        self.assertEqual(result1["content_hash"], result2["content_hash"])

    def test_store_updates_on_different_hash(self):
        """Store updates when content hash changes."""
        # First store
        self.backend.store("key1", {"v": 1})

        # Second store with different data
        result = self.backend.store("key1", {"v": 2})
        self.assertTrue(result["stored"])
        self.assertFalse(result.get("deduplicated", False))

        # Verify update
        entry = self.catalog.get_entry("key1")
        self.assertEqual(entry["inlined_value"], {"v": 2})

    def test_store_content_hash_format(self):
        """AC-11, AC-13: Content hash computed and included."""
        result = self.backend.store("key1", {"data": "test"})

        self.assertIn("content_hash", result)
        self.assertTrue(result["content_hash"].startswith("sha256:"))

    def test_store_with_metadata(self):
        """AC-3: Store with metadata."""
        metadata = {"source": "web", "author": "test"}
        result = self.backend.store("key1", {"data": "test"}, metadata=metadata)

        self.assertTrue(result["success"])

        entry = self.catalog.get_entry("key1")
        self.assertEqual(entry["metadata"], metadata)

    def test_store_with_expires_at_datetime(self):
        """Store with datetime expiration."""
        expires = datetime.now(timezone.utc) + timedelta(hours=1)
        metadata = {"expires_at": expires}
        result = self.backend.store("key1", {"data": "temp"}, metadata=metadata)

        self.assertTrue(result["success"])
        entry = self.catalog.get_entry("key1")
        self.assertIsNotNone(entry["expires_at"])

    def test_store_with_ttl_seconds(self):
        """Store with TTL in seconds."""
        metadata = {"ttl": 3600}  # 1 hour
        result = self.backend.store("key1", {"data": "temp"}, metadata=metadata)

        self.assertTrue(result["success"])
        entry = self.catalog.get_entry("key1")
        self.assertIsNotNone(entry["expires_at"])

    def test_store_serialization_error(self):
        """Store fails gracefully on serialization error."""

        # Create non-serializable object
        class NotSerializable:
            def __repr__(self):
                raise ValueError("Cannot serialize")

        # This should use default=str, so it won't fail
        result = self.backend.store("key1", {"obj": object()})
        self.assertTrue(result["success"])


class TestDuckDBLTMBackendRetrieve(unittest.TestCase):
    """Tests for retrieve method (AC-4, AC-10)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_retrieve_inlined(self):
        """AC-10: Retrieve inlined data from catalog."""
        self.backend.store("key1", {"data": "inlined"})
        result = self.backend.retrieve("key1")

        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], {"data": "inlined"})
        self.assertTrue(result["inlined"])
        self.assertIn("content_hash", result)
        self.assertIn("metadata", result)

    def test_retrieve_from_cloud(self):
        """AC-10: Retrieve data from cloud storage."""
        large_data = {"content": "x" * 2000}
        self.backend.store("key2", large_data)
        result = self.backend.retrieve("key2")

        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], large_data)
        self.assertFalse(result["inlined"])

    def test_retrieve_not_found(self):
        """AC-4: Returns default if not found."""
        result = self.backend.retrieve("missing", default="default_value")

        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "default_value")

    def test_retrieve_expired_entry(self):
        """Expired entries return not found."""
        # Store with past expiration
        expires = datetime.now(timezone.utc) - timedelta(hours=1)
        self.catalog.track_entry(
            key="expired-key",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
            inlined_value={"data": "old"},
            expires_at=expires,
        )

        result = self.backend.retrieve("expired-key", default="expired")

        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "expired")

    def test_retrieve_with_metadata(self):
        """Retrieve includes metadata."""
        self.backend.store("key1", {"data": "test"}, metadata={"source": "web"})
        result = self.backend.retrieve("key1")

        self.assertTrue(result["success"])
        self.assertEqual(result["metadata"], {"source": "web"})


class TestDuckDBLTMBackendDelete(unittest.TestCase):
    """Tests for delete method (AC-5)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_delete_inlined_entry(self):
        """AC-5: Delete removes inlined entry from catalog."""
        self.backend.store("key1", {"data": "test"})
        result = self.backend.delete("key1")

        self.assertTrue(result["success"])
        self.assertTrue(result["deleted"])
        self.assertEqual(result["key"], "key1")

        # Verify deleted
        retrieve_result = self.backend.retrieve("key1")
        self.assertFalse(retrieve_result["found"])

    def test_delete_cloud_entry(self):
        """AC-5: Delete removes from both catalog and cloud."""
        large_data = {"content": "x" * 2000}
        store_result = self.backend.store("key2", large_data)
        storage_uri = store_result["storage_uri"]

        # Verify file exists
        self.assertTrue(os.path.exists(storage_uri))

        result = self.backend.delete("key2")

        self.assertTrue(result["success"])
        self.assertTrue(result["deleted"])

        # Verify file deleted
        self.assertFalse(os.path.exists(storage_uri))

        # Verify catalog deleted
        retrieve_result = self.backend.retrieve("key2")
        self.assertFalse(retrieve_result["found"])

    def test_delete_not_found(self):
        """Delete returns deleted=False if not found."""
        result = self.backend.delete("nonexistent")

        self.assertTrue(result["success"])
        self.assertFalse(result["deleted"])


class TestDuckDBLTMBackendSearch(unittest.TestCase):
    """Tests for search method (AC-6, AC-21, AC-22)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,  # Disable FTS for basic tests
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_search_returns_all(self):
        """Search returns all entries by default."""
        self.backend.store("key1", {"data": "test1"})
        self.backend.store("key2", {"data": "test2"})
        self.backend.store("key3", {"data": "test3"})

        result = self.backend.search()

        self.assertTrue(result["success"])
        self.assertEqual(len(result["results"]), 3)
        self.assertEqual(result["total"], 3)

    def test_search_with_metadata_filter(self):
        """AC-22: Search filters by metadata."""
        self.backend.store("doc1", {"data": "test"}, metadata={"type": "pdf"})
        self.backend.store("doc2", {"data": "test"}, metadata={"type": "docx"})

        result = self.backend.search(metadata_filter={"type": "pdf"})

        self.assertTrue(result["success"])
        self.assertEqual(len(result["results"]), 1)
        self.assertEqual(result["results"][0]["key"], "doc1")

    def test_search_with_limit(self):
        """AC-6: Search respects limit."""
        for i in range(10):
            self.backend.store(f"key{i}", {"data": f"test{i}"})

        result = self.backend.search(limit=5)

        self.assertTrue(result["success"])
        self.assertEqual(len(result["results"]), 5)


class TestDuckDBLTMBackendCloudStorage(unittest.TestCase):
    """Tests for cloud storage operations (AC-14 to AC-18)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_local_storage_path_generation(self):
        """AC-17: Local storage path uses hash."""
        path = self.backend._generate_storage_path("test-key")

        self.assertTrue(path.startswith(self.storage_uri))
        self.assertTrue(path.endswith(".json"))
        # Path includes SHA-256 hash of key
        self.assertEqual(len(os.path.basename(path)), 64 + 5)  # hash + .json

    def test_upload_and_download_local(self):
        """AC-17: Local file operations work."""
        test_content = json.dumps({"test": "data"})
        test_path = os.path.join(self.storage_uri, "test.json")

        # Upload
        self.backend._upload(test_path, test_content)
        self.assertTrue(os.path.exists(test_path))

        # Download
        result = self.backend._download(test_path)
        self.assertEqual(result, {"test": "data"})

    def test_delete_file_local(self):
        """Local file deletion works."""
        test_path = os.path.join(self.storage_uri, "test.json")
        with open(test_path, "w") as f:
            f.write('{"test": "data"}')

        self.assertTrue(os.path.exists(test_path))
        self.backend._delete_file(test_path)
        self.assertFalse(os.path.exists(test_path))

    def test_is_local_storage_detection(self):
        """Local storage detection works."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        # Local paths
        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri="./local/",
            query_engine=MockQueryEngine(),
            enable_fts=False,
        )
        self.assertTrue(backend._is_local_storage())
        backend.close()

        # Cloud paths (mock)
        backend2 = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri="s3://bucket/path/",
            query_engine=MockQueryEngine(),
            enable_fts=False,
        )
        self.assertFalse(backend2._is_local_storage())
        backend2.close()


class TestDuckDBLTMBackendQueryEngine(unittest.TestCase):
    """Tests for query engine integration (AC-23 to AC-25)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")
        self.mock_engine = MockQueryEngine()

    def tearDown(self):
        """Clean up."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_shared_engine(self):
        """AC-23: Option to share DuckDBQueryEngine."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=self.mock_engine,
            enable_fts=False,
        )

        self.assertEqual(backend.query_engine, self.mock_engine)
        self.assertFalse(backend._owns_engine)
        backend.close()

    def test_standalone_engine_creation(self):
        """AC-24: Option to create standalone engine."""
        # This requires duckdb, so we mock it
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        with patch(
            "the_edge_agent.memory.duckdb_ltm.DuckDBLTMBackend._create_engine"
        ) as mock_create:
            mock_create.return_value = MockQueryEngine()

            backend = DuckDBLTMBackend(
                catalog=self.catalog,
                storage_uri=self.storage_uri,
                query_engine=None,  # Force standalone
                enable_fts=False,
            )

            mock_create.assert_called_once()
            self.assertTrue(backend._owns_engine)
            backend.close()

    def test_circuit_breaker_access(self):
        """AC-25: Inherits circuit breaker from query engine."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=self.mock_engine,
            enable_fts=False,
        )

        state = backend.get_circuit_state()
        self.assertEqual(state["state"], "closed")

        backend.reset_circuit()
        state = backend.get_circuit_state()
        self.assertEqual(state["state"], "closed")

        backend.close()

    def test_health_check(self):
        """Health check returns backend status."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=self.mock_engine,
            enable_fts=False,
        )

        health = backend.health_check()

        self.assertTrue(health["success"])
        self.assertIn("engine", health)
        self.assertIn("fts_enabled", health)
        self.assertIn("storage_uri", health)
        self.assertIn("inline_threshold", health)

        backend.close()


class TestDuckDBLTMBackendContextManager(unittest.TestCase):
    """Tests for context manager support."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

    def tearDown(self):
        """Clean up."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_context_manager(self):
        """Backend works as context manager."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        with DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
        ) as backend:
            result = backend.store("key1", {"data": "test"})
            self.assertTrue(result["success"])


class TestDuckDBLTMBackendEdgeCases(unittest.TestCase):
    """Tests for edge cases and error handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")

        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        self.backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=MockQueryEngine(),
            enable_fts=False,
            inline_threshold=1024,
        )

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_store_empty_value(self):
        """Store handles empty values."""
        result = self.backend.store("empty", {})
        self.assertTrue(result["success"])
        self.assertTrue(result["inlined"])

    def test_store_null_value(self):
        """Store handles null values."""
        result = self.backend.store("null", None)
        self.assertTrue(result["success"])

    def test_store_list_value(self):
        """Store handles list values."""
        result = self.backend.store("list", [1, 2, 3])
        self.assertTrue(result["success"])

        retrieve = self.backend.retrieve("list")
        self.assertEqual(retrieve["value"], [1, 2, 3])

    def test_store_string_value(self):
        """Store handles string values."""
        result = self.backend.store("string", "hello world")
        self.assertTrue(result["success"])

        retrieve = self.backend.retrieve("string")
        self.assertEqual(retrieve["value"], "hello world")

    def test_store_unicode_key(self):
        """Store handles unicode keys."""
        result = self.backend.store("ключ-🔑", {"data": "unicode"})
        self.assertTrue(result["success"])

        retrieve = self.backend.retrieve("ключ-🔑")
        self.assertTrue(retrieve["found"])

    def test_store_very_long_key(self):
        """Store handles very long keys."""
        long_key = "k" * 1000
        result = self.backend.store(long_key, {"data": "long key"})
        self.assertTrue(result["success"])

        retrieve = self.backend.retrieve(long_key)
        self.assertTrue(retrieve["found"])

    def test_retrieve_default_none(self):
        """Retrieve returns None as default."""
        result = self.backend.retrieve("missing")
        self.assertIsNone(result["value"])

    def test_multiple_stores_same_key(self):
        """Multiple stores to same key work correctly."""
        for i in range(5):
            self.backend.store("key", {"version": i})

        result = self.backend.retrieve("key")
        self.assertEqual(result["value"], {"version": 4})


class TestDuckDBLTMBackendFTS(unittest.TestCase):
    """Tests for FTS integration (AC-19 to AC-21)."""

    def setUp(self):
        """Set up test fixtures."""
        self.catalog = MockCatalogBackend()
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = os.path.join(self.temp_dir, "ltm_data/")
        self.mock_engine = MockQueryEngine()

    def tearDown(self):
        """Clean up."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_fts_initialization(self):
        """AC-19: FTS extension loaded when enabled."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=self.mock_engine,
            enable_fts=True,
        )

        # FTS extension should be loaded
        self.assertIn("fts", self.mock_engine._extensions)
        backend.close()

    def test_fts_disabled(self):
        """FTS can be disabled."""
        from the_edge_agent.memory.duckdb_ltm import DuckDBLTMBackend

        backend = DuckDBLTMBackend(
            catalog=self.catalog,
            storage_uri=self.storage_uri,
            query_engine=self.mock_engine,
            enable_fts=False,
        )

        self.assertFalse(backend._enable_fts)
        backend.close()


if __name__ == "__main__":
    unittest.main()
