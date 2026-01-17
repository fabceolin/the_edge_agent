"""
Tests for SQLAlchemy Catalog Backend (TEA-LTM-012).

Tests the SQLAlchemyCatalog implementation with SQLite in-memory database.
"""

import pytest
from datetime import datetime, timedelta, timezone

# Check if SQLAlchemy is available
try:
    from sqlalchemy import create_engine

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False

# Skip all tests if SQLAlchemy not installed
pytestmark = pytest.mark.skipif(
    not SQLALCHEMY_AVAILABLE, reason="SQLAlchemy not installed"
)


@pytest.fixture
def catalog():
    """Create in-memory SQLite catalog for testing."""
    from the_edge_agent.memory.catalog_sqlalchemy import SQLAlchemyCatalog

    catalog = SQLAlchemyCatalog(url="sqlite:///:memory:")
    yield catalog
    catalog.close()


@pytest.fixture
def lazy_catalog():
    """Create lazy-initialized catalog for testing."""
    from the_edge_agent.memory.catalog_sqlalchemy import SQLAlchemyCatalog

    catalog = SQLAlchemyCatalog(url="sqlite:///:memory:", lazy=True)
    yield catalog
    catalog.close()


class TestSQLAlchemyCatalogInit:
    """Test catalog initialization."""

    def test_init_creates_engine(self, catalog):
        """Test that initialization creates engine and tables."""
        assert catalog._initialized is True
        assert catalog._engine is not None
        assert catalog._session_factory is not None

    def test_lazy_init_defers_engine_creation(self, lazy_catalog):
        """Test lazy initialization defers engine creation."""
        assert lazy_catalog._initialized is False
        assert lazy_catalog._engine is None

    def test_lazy_init_creates_engine_on_first_use(self, lazy_catalog):
        """Test lazy catalog creates engine on first operation."""
        result = lazy_catalog.get_entry("nonexistent")
        assert result is None
        assert lazy_catalog._initialized is True
        assert lazy_catalog._engine is not None


class TestSQLAlchemyCatalogTrackEntry:
    """Test track_entry operations."""

    def test_track_new_entry(self, catalog):
        """Test tracking a new entry."""
        result = catalog.track_entry(
            key="user:123",
            content_hash="sha256:abc123",
            storage_uri="gs://bucket/path",
            byte_size=1024,
            metadata={"type": "profile"},
        )
        assert result["success"] is True
        assert result["created"] is True
        assert "entry_id" in result

    def test_track_entry_with_inlined_value(self, catalog):
        """Test tracking entry with inlined value."""
        result = catalog.track_entry(
            key="small:entry",
            content_hash="sha256:def456",
            storage_uri=None,
            byte_size=100,
            metadata={"type": "small"},
            inlined_value={"small": "data"},
        )
        assert result["success"] is True
        assert result["created"] is True

    def test_track_entry_update_existing(self, catalog):
        """Test updating existing entry."""
        catalog.track_entry(
            key="user:123",
            content_hash="sha256:original",
            storage_uri="gs://bucket/v1",
            byte_size=1024,
            metadata={"version": 1},
        )

        result = catalog.track_entry(
            key="user:123",
            content_hash="sha256:updated",
            storage_uri="gs://bucket/v2",
            byte_size=2048,
            metadata={"version": 2},
        )
        assert result["success"] is True
        assert result["created"] is False

    def test_track_entry_with_expiration(self, catalog):
        """Test tracking entry with expiration."""
        expires = datetime.now(timezone.utc) + timedelta(hours=1)
        result = catalog.track_entry(
            key="temp:entry",
            content_hash="sha256:temp",
            storage_uri=None,
            byte_size=100,
            metadata={"temp": True},
            expires_at=expires,
        )
        assert result["success"] is True


class TestSQLAlchemyCatalogGetEntry:
    """Test get_entry operations."""

    def test_get_existing_entry(self, catalog):
        """Test getting an existing entry."""
        catalog.track_entry(
            key="user:123",
            content_hash="sha256:abc123",
            storage_uri="gs://bucket/path",
            byte_size=1024,
            metadata={"type": "profile"},
            inlined_value={"data": "value"},
        )

        entry = catalog.get_entry("user:123")
        assert entry is not None
        assert entry["key"] == "user:123"
        assert entry["content_hash"] == "sha256:abc123"
        assert entry["storage_uri"] == "gs://bucket/path"
        assert entry["byte_size"] == 1024
        assert entry["metadata"] == {"type": "profile"}
        assert entry["inlined_value"] == {"data": "value"}

    def test_get_nonexistent_entry(self, catalog):
        """Test getting a nonexistent entry."""
        entry = catalog.get_entry("nonexistent")
        assert entry is None


class TestSQLAlchemyCatalogListEntries:
    """Test list_entries operations."""

    def test_list_all_entries(self, catalog):
        """Test listing all entries."""
        catalog.track_entry("user:1", "sha256:a", None, 100, {"type": "user"})
        catalog.track_entry("user:2", "sha256:b", None, 200, {"type": "user"})

        entries = catalog.list_entries(limit=100)
        assert len(entries) == 2

    def test_list_entries_with_prefix(self, catalog):
        """Test listing entries with prefix filter."""
        catalog.track_entry("user:1", "sha256:a", None, 100, {"type": "user"})
        catalog.track_entry("user:2", "sha256:b", None, 200, {"type": "user"})
        catalog.track_entry("order:1", "sha256:c", None, 300, {"type": "order"})

        entries = catalog.list_entries(prefix="user:")
        assert len(entries) == 2
        assert all(e["key"].startswith("user:") for e in entries)

    def test_list_entries_with_metadata_filter(self, catalog):
        """Test listing entries with metadata filter."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {"status": "active"})
        catalog.track_entry("item:2", "sha256:b", None, 200, {"status": "inactive"})

        entries = catalog.list_entries(metadata_filter={"status": "active"})
        assert len(entries) == 1
        assert entries[0]["key"] == "item:1"

    def test_list_entries_with_limit(self, catalog):
        """Test listing entries respects limit."""
        for i in range(10):
            catalog.track_entry(f"item:{i}", f"sha256:{i}", None, 100, {})

        entries = catalog.list_entries(limit=5)
        assert len(entries) == 5


class TestSQLAlchemyCatalogDeleteEntry:
    """Test delete_entry operations."""

    def test_delete_existing_entry(self, catalog):
        """Test deleting an existing entry."""
        catalog.track_entry("user:123", "sha256:abc", None, 100, {})

        result = catalog.delete_entry("user:123")
        assert result is True

        # Verify deletion
        assert catalog.get_entry("user:123") is None

    def test_delete_nonexistent_entry(self, catalog):
        """Test deleting a nonexistent entry."""
        result = catalog.delete_entry("nonexistent")
        assert result is False


class TestSQLAlchemyCatalogSnapshot:
    """Test snapshot operations."""

    def test_create_snapshot(self, catalog):
        """Test creating a snapshot."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {})
        catalog.track_entry("item:2", "sha256:b", None, 200, {})

        snapshot_id = catalog.create_snapshot("test-snapshot")
        assert snapshot_id is not None
        assert len(snapshot_id) == 36  # UUID format

    def test_get_snapshot(self, catalog):
        """Test getting snapshot info."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {})
        catalog.track_entry("item:2", "sha256:b", None, 200, {})

        snapshot_id = catalog.create_snapshot("test-snapshot")
        snapshot = catalog.get_snapshot(snapshot_id)

        assert snapshot is not None
        assert snapshot["id"] == snapshot_id
        assert snapshot["name"] == "test-snapshot"
        assert snapshot["entry_count"] == 2
        assert snapshot["total_bytes"] == 300

    def test_get_changed_entries_since_snapshot(self, catalog):
        """Test getting changed entries since snapshot."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {})
        snapshot_id = catalog.create_snapshot("baseline")

        # Add new entry after snapshot
        catalog.track_entry("item:2", "sha256:b", None, 200, {})

        changed = catalog.get_changed_entries(since_snapshot_id=snapshot_id)
        assert len(changed) == 1
        assert changed[0]["key"] == "item:2"

    def test_get_changed_entries_no_snapshot(self, catalog):
        """Test getting all entries when no snapshot specified."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {})
        catalog.track_entry("item:2", "sha256:b", None, 200, {})

        changed = catalog.get_changed_entries(since_snapshot_id=None)
        assert len(changed) == 2


class TestSQLAlchemyCatalogBatchOperations:
    """Test batch operations."""

    def test_store_batch(self, catalog):
        """Test storing multiple entries in batch."""
        entries = [
            {
                "key": "batch:1",
                "content_hash": "sha256:a",
                "byte_size": 100,
                "metadata": {"batch": True},
            },
            {
                "key": "batch:2",
                "content_hash": "sha256:b",
                "byte_size": 200,
                "metadata": {"batch": True},
            },
        ]

        result = catalog.store_batch(entries)
        assert result["success"] is True
        assert result["stored_count"] == 2
        assert result["failed_count"] == 0

        # Verify entries exist
        assert catalog.get_entry("batch:1") is not None
        assert catalog.get_entry("batch:2") is not None

    def test_store_batch_empty(self, catalog):
        """Test storing empty batch."""
        result = catalog.store_batch([])
        assert result["success"] is True
        assert result["stored_count"] == 0

    def test_retrieve_batch(self, catalog):
        """Test retrieving multiple entries in batch."""
        catalog.track_entry("item:1", "sha256:a", None, 100, {})
        catalog.track_entry("item:2", "sha256:b", None, 200, {})

        result = catalog.retrieve_batch(["item:1", "item:2", "item:3"])
        assert result["success"] is True
        assert result["found_count"] == 2
        assert result["missing_count"] == 1
        assert result["entries"]["item:1"] is not None
        assert result["entries"]["item:2"] is not None
        assert result["entries"]["item:3"] is None

    def test_retrieve_batch_empty(self, catalog):
        """Test retrieving empty batch."""
        result = catalog.retrieve_batch([])
        assert result["success"] is True
        assert result["found_count"] == 0


class TestSQLAlchemyCatalogCleanupExpired:
    """Test cleanup_expired operations."""

    def test_cleanup_expired(self, catalog):
        """Test cleaning up expired entries."""
        # Create expired entry
        expired = datetime.now(timezone.utc) - timedelta(hours=1)
        catalog.track_entry("expired:1", "sha256:a", None, 100, {}, expires_at=expired)

        # Create non-expired entry
        future = datetime.now(timezone.utc) + timedelta(hours=1)
        catalog.track_entry("active:1", "sha256:b", None, 100, {}, expires_at=future)

        result = catalog.cleanup_expired()
        assert result["success"] is True
        assert result["deleted_count"] == 1

        # Verify expired is gone, active remains
        assert catalog.get_entry("expired:1") is None
        assert catalog.get_entry("active:1") is not None


class TestSQLAlchemyCatalogClose:
    """Test close operations."""

    def test_close_disposes_engine(self, catalog):
        """Test close disposes engine."""
        catalog.close()
        assert catalog._engine is None
        assert catalog._initialized is False


class TestSQLAlchemyCatalogFactory:
    """Test factory registration."""

    def test_catalog_registered_in_factory(self):
        """Test that catalog is registered with the factory."""
        from the_edge_agent.memory import get_registered_catalog_backends

        backends = get_registered_catalog_backends()
        assert "sqlalchemy" in backends

    def test_create_catalog_via_factory(self):
        """Test creating catalog via factory function."""
        from the_edge_agent.memory import create_catalog_backend

        catalog = create_catalog_backend("sqlalchemy", url="sqlite:///:memory:")
        assert catalog is not None

        result = catalog.track_entry("test:key", "sha256:abc", None, 100, {})
        assert result["success"] is True

        catalog.close()


class TestSQLAlchemyCatalogContextManager:
    """Test context manager support."""

    def test_context_manager(self):
        """Test using catalog as context manager."""
        from the_edge_agent.memory.catalog_sqlalchemy import SQLAlchemyCatalog

        with SQLAlchemyCatalog(url="sqlite:///:memory:") as catalog:
            result = catalog.track_entry("test:key", "sha256:abc", None, 100, {})
            assert result["success"] is True

        # Catalog should be closed after context
        assert catalog._engine is None
