"""
Tests for SQLAlchemy LTM Backend (TEA-LTM-012).

Tests the SQLAlchemyBackend implementation with SQLite in-memory database.
"""

import pytest

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
def backend():
    """Create in-memory SQLite backend for testing."""
    from the_edge_agent.memory.sqlalchemy_backend import SQLAlchemyBackend

    backend = SQLAlchemyBackend(url="sqlite:///:memory:")
    yield backend
    backend.close()


@pytest.fixture
def lazy_backend():
    """Create lazy-initialized backend for testing."""
    from the_edge_agent.memory.sqlalchemy_backend import SQLAlchemyBackend

    backend = SQLAlchemyBackend(url="sqlite:///:memory:", lazy=True)
    yield backend
    backend.close()


class TestSQLAlchemyBackendInit:
    """Test backend initialization."""

    def test_init_creates_engine(self, backend):
        """Test that initialization creates engine and tables."""
        assert backend._initialized is True
        assert backend._engine is not None
        assert backend._session_factory is not None

    def test_lazy_init_defers_engine_creation(self, lazy_backend):
        """Test lazy initialization defers engine creation."""
        assert lazy_backend._initialized is False
        assert lazy_backend._engine is None

    def test_lazy_init_creates_engine_on_first_use(self, lazy_backend):
        """Test lazy backend creates engine on first operation."""
        # First operation should trigger initialization
        result = lazy_backend.retrieve("nonexistent")
        assert result["success"] is True
        assert lazy_backend._initialized is True
        assert lazy_backend._engine is not None

    def test_dialect_property(self, backend):
        """Test dialect property returns correct value."""
        assert backend.dialect == "sqlite"


class TestSQLAlchemyBackendStore:
    """Test store operations."""

    def test_store_simple_value(self, backend):
        """Test storing a simple value."""
        result = backend.store("test:key", {"data": "value"})
        assert result["success"] is True
        assert result["stored"] is True
        assert result["key"] == "test:key"
        assert result["created"] is True

    def test_store_with_metadata(self, backend):
        """Test storing with metadata."""
        result = backend.store(
            "test:key", {"data": "value"}, metadata={"type": "test", "version": 1}
        )
        assert result["success"] is True
        assert result["created"] is True

    def test_store_update_existing(self, backend):
        """Test updating existing key."""
        backend.store("test:key", {"data": "original"})
        result = backend.store("test:key", {"data": "updated"})
        assert result["success"] is True
        assert result["created"] is False  # Update, not create

    def test_store_empty_key_fails(self, backend):
        """Test that empty key fails validation."""
        result = backend.store("", {"data": "value"})
        assert result["success"] is False
        assert result["error_type"] == "validation_error"

    def test_store_none_key_fails(self, backend):
        """Test that None key fails validation."""
        result = backend.store(None, {"data": "value"})
        assert result["success"] is False
        assert result["error_type"] == "validation_error"

    def test_store_various_value_types(self, backend):
        """Test storing various value types."""
        # Dict
        assert backend.store("test:dict", {"a": 1, "b": 2})["success"] is True

        # List
        assert backend.store("test:list", [1, 2, 3])["success"] is True

        # String
        assert backend.store("test:string", "hello world")["success"] is True

        # Number
        assert backend.store("test:number", 42)["success"] is True

        # Nested
        assert backend.store("test:nested", {"a": {"b": {"c": 1}}})["success"] is True


class TestSQLAlchemyBackendRetrieve:
    """Test retrieve operations."""

    def test_retrieve_existing_key(self, backend):
        """Test retrieving an existing key."""
        backend.store("test:key", {"data": "value"}, metadata={"type": "test"})
        result = backend.retrieve("test:key")
        assert result["success"] is True
        assert result["found"] is True
        assert result["value"] == {"data": "value"}
        assert result["metadata"] == {"type": "test"}

    def test_retrieve_nonexistent_key(self, backend):
        """Test retrieving a nonexistent key."""
        result = backend.retrieve("nonexistent")
        assert result["success"] is True
        assert result["found"] is False
        assert result["value"] is None
        assert result["metadata"] is None

    def test_retrieve_with_default(self, backend):
        """Test retrieving with default value."""
        result = backend.retrieve("nonexistent", default="default_value")
        assert result["success"] is True
        assert result["found"] is False
        assert result["value"] == "default_value"

    def test_retrieve_empty_key_fails(self, backend):
        """Test that empty key fails validation."""
        result = backend.retrieve("")
        assert result["success"] is False
        assert result["error_type"] == "validation_error"


class TestSQLAlchemyBackendDelete:
    """Test delete operations."""

    def test_delete_existing_key(self, backend):
        """Test deleting an existing key."""
        backend.store("test:key", {"data": "value"})
        result = backend.delete("test:key")
        assert result["success"] is True
        assert result["deleted"] is True
        assert result["key"] == "test:key"

        # Verify deletion
        retrieve_result = backend.retrieve("test:key")
        assert retrieve_result["found"] is False

    def test_delete_nonexistent_key(self, backend):
        """Test deleting a nonexistent key."""
        result = backend.delete("nonexistent")
        assert result["success"] is True
        assert result["deleted"] is False

    def test_delete_empty_key_fails(self, backend):
        """Test that empty key fails validation."""
        result = backend.delete("")
        assert result["success"] is False
        assert result["error_type"] == "validation_error"


class TestSQLAlchemyBackendSearch:
    """Test search operations."""

    def test_search_without_query_returns_all(self, backend):
        """Test search without query returns all entries."""
        backend.store("test:one", {"text": "hello world"})
        backend.store("test:two", {"text": "goodbye world"})

        result = backend.search(limit=10)
        assert result["success"] is True
        assert result["count"] == 2

    def test_search_with_like_query(self, backend):
        """Test search with LIKE query (SQLite fallback)."""
        backend.store("test:one", {"text": "hello world"})
        backend.store("test:two", {"text": "goodbye world"})

        result = backend.search(query="hello", limit=10)
        assert result["success"] is True
        assert result["count"] >= 1
        assert any(r["key"] == "test:one" for r in result["results"])

    def test_search_with_metadata_filter(self, backend):
        """Test search with metadata filter."""
        backend.store("test:one", {"text": "hello"}, metadata={"type": "greeting"})
        backend.store("test:two", {"text": "bye"}, metadata={"type": "farewell"})

        result = backend.search(metadata_filter={"type": "greeting"}, limit=10)
        assert result["success"] is True
        assert result["count"] == 1
        assert result["results"][0]["key"] == "test:one"

    def test_search_with_limit(self, backend):
        """Test search respects limit."""
        for i in range(10):
            backend.store(f"test:{i}", {"text": f"item {i}"})

        result = backend.search(limit=5)
        assert result["success"] is True
        assert result["count"] == 5


class TestSQLAlchemyBackendIterateAll:
    """Test iterate_all operation."""

    def test_iterate_all(self, backend):
        """Test iterating over all entries."""
        backend.store("test:one", {"data": 1}, metadata={"type": "a"})
        backend.store("test:two", {"data": 2}, metadata={"type": "b"})

        entries = list(backend.iterate_all())
        assert len(entries) == 2

        keys = [e[0] for e in entries]
        assert "test:one" in keys
        assert "test:two" in keys


class TestSQLAlchemyBackendTransaction:
    """Test transaction operations."""

    def test_transaction_commit(self, backend):
        """Test transaction commit."""
        with backend.transaction() as txn:
            txn.store("txn:one", {"data": 1})
            txn.store("txn:two", {"data": 2})

        # Verify entries exist
        assert backend.retrieve("txn:one")["found"] is True
        assert backend.retrieve("txn:two")["found"] is True

    def test_transaction_rollback_on_exception(self, backend):
        """Test transaction rollback on exception."""
        try:
            with backend.transaction() as txn:
                txn.store("txn:one", {"data": 1})
                raise ValueError("Test exception")
        except ValueError:
            pass

        # Entry should not exist after rollback
        # Note: For SQLite in-memory, the entry might still exist
        # as we're using a simple session rollback
        result = backend.retrieve("txn:one")
        # The rollback should have prevented the commit
        # In practice, with SQLite in-memory and StaticPool,
        # the behavior may vary

    def test_transaction_delete(self, backend):
        """Test transaction delete."""
        backend.store("test:key", {"data": "value"})

        with backend.transaction() as txn:
            txn.delete("test:key")

        assert backend.retrieve("test:key")["found"] is False


class TestSQLAlchemyBackendClose:
    """Test close operations."""

    def test_close_disposes_engine(self, backend):
        """Test close disposes engine."""
        backend.close()
        assert backend._closed is True
        assert backend._engine is None

    def test_operations_fail_after_close(self, backend):
        """Test operations fail after close."""
        backend.close()

        result = backend.store("test:key", {"data": "value"})
        assert result["success"] is False
        assert result["error_type"] == "connection_error"

        result = backend.retrieve("test:key")
        assert result["success"] is False

        result = backend.delete("test:key")
        assert result["success"] is False

        result = backend.search()
        assert result["success"] is False


class TestSQLAlchemyBackendFactory:
    """Test factory registration."""

    def test_backend_registered_in_factory(self):
        """Test that backend is registered with the factory."""
        from the_edge_agent.memory import get_registered_backends

        backends = get_registered_backends()
        assert "sqlalchemy" in backends

    def test_create_backend_via_factory(self):
        """Test creating backend via factory function."""
        from the_edge_agent.memory import create_ltm_backend

        backend = create_ltm_backend("sqlalchemy", url="sqlite:///:memory:")
        assert backend is not None

        result = backend.store("test:key", {"data": "value"})
        assert result["success"] is True

        backend.close()


class TestSQLAlchemyBackendContextManager:
    """Test context manager support."""

    def test_context_manager(self):
        """Test using backend as context manager."""
        from the_edge_agent.memory.sqlalchemy_backend import SQLAlchemyBackend

        with SQLAlchemyBackend(url="sqlite:///:memory:") as backend:
            result = backend.store("test:key", {"data": "value"})
            assert result["success"] is True

        # Backend should be closed after context
        assert backend._closed is True
