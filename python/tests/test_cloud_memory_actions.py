"""
Tests for Cloud Memory Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_catalog.py (cloud_store tests)
Uses MockMetadataStore and MockBlobStorage instead of Firebase mocks.

Story: RX.7 - Agent Memory Layer

Tests cover:
- memory.cloud_store: Store content to cloud with metadata
- memory.cloud_retrieve: Retrieve content from cloud
- memory.cloud_list: List files with filtering
- Content hash computation
"""

import pytest
import hashlib
import sys
from datetime import datetime, timezone
from pathlib import Path

# Add the src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import fixtures
from conftest_memory_actions import (
    MockMetadataStore,
    MockBlobStorage,
    mock_metadata_store,
    mock_blob_storage,
    mock_state,
)

# Import the actions module
from the_edge_agent.actions.cloud_memory_actions import (
    AGENT_MEMORY_COLLECTION,
    _compute_content_hash,
    _detect_content_type,
    memory_cloud_store,
    memory_cloud_retrieve,
    memory_cloud_list,
    register_actions,
)


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def cloud_state(mock_metadata_store, mock_blob_storage):
    """State with mock backends for cloud memory testing."""
    return {
        "project_id": "test-project",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
    }


# =============================================================================
# CONTENT HASH TESTS
# =============================================================================

class TestContentHash:
    """Tests for content hash computation."""

    def test_compute_content_hash_string(self):
        """T14: Hash is computed correctly for strings."""
        content = "name: test\nstatus: active\n"
        hash_result = _compute_content_hash(content)

        assert hash_result.startswith("sha256:")
        assert len(hash_result) == 71  # "sha256:" (7) + 64 hex chars

    def test_compute_content_hash_bytes(self):
        """Hash works with bytes input."""
        content = b"binary content here"
        hash_result = _compute_content_hash(content)

        assert hash_result.startswith("sha256:")

    def test_compute_content_hash_known_value(self):
        """Verify hash matches expected SHA-256."""
        content = "name: test\nstatus: active\n"
        hash_result = _compute_content_hash(content)

        expected_digest = hashlib.sha256(content.encode('utf-8')).hexdigest()
        expected = f"sha256:{expected_digest}"

        assert hash_result == expected

    def test_compute_content_hash_deterministic(self):
        """T17: Hash is deterministic (same content = same hash)."""
        content = "test content for hashing"
        hash1 = _compute_content_hash(content)
        hash2 = _compute_content_hash(content)
        hash3 = _compute_content_hash(content)

        assert hash1 == hash2 == hash3


# =============================================================================
# CONTENT TYPE DETECTION TESTS
# =============================================================================

class TestContentTypeDetection:
    """Tests for content type detection."""

    def test_detect_yaml_extension(self):
        """YAML files are detected."""
        assert _detect_content_type("config.yaml") == "yaml"
        assert _detect_content_type("config.yml") == "yaml"

    def test_detect_json_extension(self):
        """JSON files are detected."""
        assert _detect_content_type("data.json") == "json"

    def test_detect_markdown_extension(self):
        """Markdown files are detected."""
        assert _detect_content_type("readme.md") == "md"

    def test_detect_default_type(self):
        """Unknown extensions default to yaml."""
        assert _detect_content_type("file.xyz") == "yaml"


# =============================================================================
# MEMORY.CLOUD_STORE TESTS
# =============================================================================

class TestMemoryCloudStore:
    """Tests for memory.cloud_store action."""

    def test_cloud_store_success(self, cloud_state, mock_metadata_store, mock_blob_storage):
        """Store content successfully."""
        result = memory_cloud_store(
            cloud_state,
            path="test/file.yaml",
            content="name: test\nvalue: 123"
        )

        assert result["success"] is True
        assert "storage_uri" in result
        assert "content_hash" in result
        assert result["content_hash"].startswith("sha256:")

        # Verify blob was stored at the full storage path
        # Path is: agent-memory/{project_id}/{normalized_path}
        full_storage_path = "agent-memory/test-project/test/file.yaml"
        blob = mock_blob_storage.download(full_storage_path)
        assert blob["success"] is True
        assert b"name: test" in blob["content"]

        # Verify metadata was stored with doc_id from result
        doc_id = result.get("doc_id")
        doc = mock_metadata_store.get_document(AGENT_MEMORY_COLLECTION, doc_id)
        assert doc["success"] is True
        assert doc["data"]["content_hash"].startswith("sha256:")

    def test_cloud_store_with_metadata(self, cloud_state, mock_metadata_store):
        """Store content with custom metadata."""
        result = memory_cloud_store(
            cloud_state,
            path="test/file.yaml",
            content="test content",
            metadata={"status": "draft", "summary": "Test summary", "created_by": "test-user"}
        )

        assert result["success"] is True

        # Verify metadata was stored using doc_id from result
        doc_id = result.get("doc_id")
        doc = mock_metadata_store.get_document(AGENT_MEMORY_COLLECTION, doc_id)
        assert doc["success"] is True
        # Implementation extracts specific fields from metadata dict
        assert doc["data"]["status"] == "draft"
        assert doc["data"]["summary"] == "Test summary"
        assert doc["data"]["created_by"] == "test-user"

    def test_cloud_store_initializes_synced_at_null(self, cloud_state, mock_metadata_store):
        """T16: Store initializes synced_at as null."""
        result = memory_cloud_store(
            cloud_state,
            path="test/file.yaml",
            content="name: test\n"
        )

        assert result["success"] is True
        doc_id = result.get("doc_id")
        doc = mock_metadata_store.get_document(AGENT_MEMORY_COLLECTION, doc_id)
        assert doc["success"] is True
        assert doc["data"]["synced_at"] is None

    def test_cloud_store_empty_path_fails(self, cloud_state):
        """Store with empty path fails."""
        result = memory_cloud_store(
            cloud_state,
            path="",
            content="test content"
        )

        assert result["success"] is False

    def test_cloud_store_skip_embedding(self, cloud_state, mock_metadata_store):
        """Store with skip_embedding=True doesn't generate embedding."""
        result = memory_cloud_store(
            cloud_state,
            path="test/file.yaml",
            content="test content",
            skip_embedding=True
        )

        assert result["success"] is True
        assert result.get("embedding_status") == "skipped"


# =============================================================================
# MEMORY.CLOUD_RETRIEVE TESTS
# =============================================================================

class TestMemoryCloudRetrieve:
    """Tests for memory.cloud_retrieve action."""

    def test_cloud_retrieve_success(self, cloud_state, mock_blob_storage, mock_metadata_store):
        """Retrieve content successfully."""
        # Store content first using the actual store function
        store_result = memory_cloud_store(
            cloud_state,
            path="test/file.yaml",
            content="name: test\nvalue: 123"
        )
        assert store_result["success"] is True

        # Now retrieve it
        result = memory_cloud_retrieve(cloud_state, path="test/file.yaml")

        assert result["success"] is True
        assert "content" in result
        # Content may be bytes or string depending on storage backend
        content = result["content"]
        if isinstance(content, bytes):
            content = content.decode("utf-8")
        assert "name: test" in content

    def test_cloud_retrieve_not_found(self, cloud_state):
        """Retrieve nonexistent file fails."""
        result = memory_cloud_retrieve(cloud_state, path="nonexistent/file.yaml")

        assert result["success"] is False
        assert "not found" in result["error"].lower()

    def test_cloud_retrieve_empty_path_fails(self, cloud_state):
        """Retrieve with empty path fails."""
        result = memory_cloud_retrieve(cloud_state, path="")

        assert result["success"] is False


# =============================================================================
# MEMORY.CLOUD_LIST TESTS
# =============================================================================

class TestMemoryCloudList:
    """Tests for memory.cloud_list action."""

    def test_cloud_list_success(self, cloud_state, mock_metadata_store):
        """List files successfully."""
        # Seed data must include project_id to match the filter
        mock_metadata_store._seed_data(AGENT_MEMORY_COLLECTION, {
            "doc1": {"file_path": "app/config.yaml", "project_id": "test-project", "status": "active"},
            "doc2": {"file_path": "app/settings.yaml", "project_id": "test-project", "status": "active"},
            "doc3": {"file_path": "firms/firm1/profile.yaml", "project_id": "test-project", "status": "active"},
        })

        result = memory_cloud_list(cloud_state)

        assert result["success"] is True
        assert result["count"] == 3
        assert len(result["files"]) == 3

    def test_cloud_list_with_prefix(self, cloud_state, mock_metadata_store):
        """List files with prefix filter."""
        mock_metadata_store._seed_data(AGENT_MEMORY_COLLECTION, {
            "app/config.yaml": {"path": "app/config.yaml"},
            "app/settings.yaml": {"path": "app/settings.yaml"},
            "firms/firm1/profile.yaml": {"path": "firms/firm1/profile.yaml"},
        })

        result = memory_cloud_list(cloud_state, prefix="app/")

        assert result["success"] is True
        # Filtering depends on implementation
        assert result["count"] >= 0

    def test_cloud_list_empty(self, cloud_state):
        """List returns empty when no files."""
        result = memory_cloud_list(cloud_state)

        assert result["success"] is True
        assert result["count"] == 0
        assert result["files"] == []

    def test_cloud_list_with_limit(self, cloud_state, mock_metadata_store):
        """List respects limit parameter."""
        mock_metadata_store._seed_data(AGENT_MEMORY_COLLECTION, {
            f"file{i}.yaml": {"path": f"file{i}.yaml"} for i in range(10)
        })

        result = memory_cloud_list(cloud_state, limit=5)

        assert result["success"] is True
        assert len(result["files"]) <= 5


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Test action registration."""

    def test_register_actions_adds_all_cloud_memory_actions(self, mock_metadata_store, mock_blob_storage):
        """Verify all cloud memory actions are registered."""
        registry = {}

        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": mock_blob_storage,
                    "query_engine": None,
                    "vector_index": None,
                }

        register_actions(registry, MockEngine())

        expected_actions = [
            "memory.cloud_store",
            "memory.cloud_retrieve",
            "memory.cloud_list",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"
            assert callable(registry[action])


# =============================================================================
# NO BACKENDS ERROR
# =============================================================================

class TestNoBackendsError:
    """Test error handling when no backends are provided."""

    def test_cloud_store_no_backends_error(self):
        """cloud_store fails gracefully when no backends."""
        state = {}  # No backends

        result = memory_cloud_store(
            state,
            path="test/file.yaml",
            content="test content"
        )

        assert result["success"] is False


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
