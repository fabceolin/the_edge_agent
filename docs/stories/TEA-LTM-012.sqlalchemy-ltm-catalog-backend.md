# Story TEA-LTM-012: SQLAlchemy LTM and Catalog Backends

## Status

Approved

## Story

**As a** TEA agent developer,
**I want** to use SQLAlchemy as both the LTM storage backend and catalog backend,
**so that** I can leverage any SQLAlchemy-supported database (PostgreSQL, MySQL, SQLite, MariaDB, etc.) for persistent workflow state without being locked to a specific database implementation.

## Affected Codebases

- main (the_edge_agent)

## Acceptance Criteria

1. **AC1: SQLAlchemy LTM Backend Implementation** - A `SQLAlchemyBackend` class exists that implements the `LTMBackend` abstract base class with all required methods (`store`, `retrieve`, `delete`, `search`, `close`, `iterate_all`, `transaction`).

2. **AC2: SQLAlchemy Catalog Implementation** - A `SQLAlchemyCatalog` class exists that implements the `CatalogBackend` protocol with all required methods (`track_entry`, `get_entry`, `list_entries`, `delete_entry`, `get_changed_entries`, `create_snapshot`, `store_batch`, `retrieve_batch`, `cleanup_expired`).

3. **AC3: Backend Registration** - Both "sqlalchemy" LTM and catalog types are registered in their respective registries. `create_ltm_backend("sqlalchemy", url="...")` and `create_catalog_backend("sqlalchemy", url="...")` work correctly.

4. **AC4: Database Agnostic** - The implementation works with any SQLAlchemy-supported database dialect via standard connection URLs (`postgresql://`, `mysql://`, `sqlite://`, etc.).

5. **AC5: YAML Configuration** - The YAML engine correctly parses `backend: sqlalchemy` and `catalog.type: sqlalchemy` with `url` parameter and optional `pool_size`, `echo` parameters.

6. **AC6: Full-Text Search** - Text search works across supported dialects (PostgreSQL tsvector, MySQL FULLTEXT, SQLite FTS5 fallback via LIKE).

7. **AC7: Connection Pooling** - Uses SQLAlchemy's built-in connection pooling with configurable pool size.

8. **AC8: Tests Pass** - Unit tests for both backends, integration tests, and parity tests confirming behavior consistent with existing backends all pass.

9. **AC9: Documentation Updated** - CLAUDE.md and relevant docs include SQLAlchemy backend option with configuration examples.

## Tasks / Subtasks

- [ ] **Task 1: Create SQLAlchemyBackend class** (AC1, AC4, AC6, AC7)
  - [ ] Create `python/src/the_edge_agent/memory/sqlalchemy_backend.py`
  - [ ] Define SQLAlchemy ORM model (`LTMEntry`)
  - [ ] Implement `__init__(url, pool_size=5, echo=False, lazy=False)` with engine creation
  - [ ] Implement `store()` method with UPSERT semantics
  - [ ] Implement `retrieve()` method
  - [ ] Implement `delete()` method
  - [ ] Implement `search()` with dialect-aware full-text search
  - [ ] Implement `close()` method with engine disposal
  - [ ] Implement `iterate_all()` method
  - [ ] Implement `transaction()` context manager using SQLAlchemy sessions

- [ ] **Task 2: Create SQLAlchemyCatalog class** (AC2, AC4)
  - [ ] Create `python/src/the_edge_agent/memory/catalog_sqlalchemy.py`
  - [ ] Define SQLAlchemy ORM models (`CatalogEntry`, `CatalogSnapshot` for snapshots)
  - [ ] Implement all `CatalogBackend` protocol methods
  - [ ] Support inlined values and metadata JSON storage
  - [ ] Implement snapshot support with `CatalogSnapshot` table

- [ ] **Task 3: Register backends** (AC3)
  - [ ] Add `register_backend("sqlalchemy", SQLAlchemyBackend)` call
  - [ ] Add `register_catalog_backend("sqlalchemy", SQLAlchemyCatalog)` call
  - [ ] Update `memory/__init__.py` with conditional imports
  - [ ] Verify factory functions work

- [ ] **Task 4: YAML configuration parsing** (AC5)
  - [ ] Add `url` parameter handling to `parse_backend_config()`
  - [ ] Add `pool_size` and `echo` optional parameters
  - [ ] Test YAML config parsing end-to-end

- [ ] **Task 5: Write tests** (AC8)
  - [ ] Create `tests/test_sqlalchemy_backend.py`
  - [ ] Create `tests/test_catalog_sqlalchemy.py`
  - [ ] Unit tests for all LTMBackend methods
  - [ ] Unit tests for all CatalogBackend methods
  - [ ] Integration test with SQLite dialect (`:memory:`)
  - [ ] Parity tests vs PostgresBackend/PostgresCatalog if available

- [ ] **Task 6: Update documentation** (AC9)
  - [ ] Update CLAUDE.md LTM section with SQLAlchemy option
  - [ ] Add YAML configuration examples in docs
  - [ ] Update story status to Done

## Dev Notes

### Source Tree (Relevant Files)

```
python/src/the_edge_agent/memory/
├── __init__.py              # Add SQLAlchemy imports (conditional)
├── base.py                  # LTMBackend ABC, register_backend(), factory
├── catalog.py               # CatalogBackend protocol, register_catalog_backend()
├── postgres.py              # PostgresBackend (reference for SQL patterns)
├── catalog_postgres.py      # PostgresCatalog (reference for catalog patterns)
├── sqlalchemy_backend.py    # NEW: SQLAlchemyBackend (this story)
└── catalog_sqlalchemy.py    # NEW: SQLAlchemyCatalog (this story)
```

### LTMBackend Abstract Methods (from base.py)

```python
class LTMBackend(ABC):
    @abstractmethod
    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> Dict[str, Any]:
        """Returns: {"success": True, "stored": True, "key": str, "created": bool}"""

    @abstractmethod
    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Returns: {"success": True, "value": any, "found": bool, "metadata": dict|None}"""

    @abstractmethod
    def delete(self, key: str) -> Dict[str, Any]:
        """Returns: {"success": True, "deleted": bool, "key": str}"""

    @abstractmethod
    def search(self, query=None, metadata_filter=None, limit=10) -> Dict[str, Any]:
        """Returns: {"success": True, "results": [...], "count": int}"""

    @abstractmethod
    def close(self) -> None:
        """Cleanup and release resources"""

    # These methods have DEFAULT implementations - override only if needed:
    def iterate_all(self) -> Iterator[Tuple[str, Any, Dict]]:
        """Yield (key, value, metadata) for all items (default uses search())"""

    def transaction(self) -> LTMTransaction:
        """Returns context manager for atomic ops (default uses LTMTransaction)"""
```

### CatalogBackend Protocol Methods (from catalog.py)

```python
@runtime_checkable
class CatalogBackend(Protocol):
    def track_entry(key, content_hash, storage_uri, byte_size, metadata,
                    inlined_value=None, expires_at=None) -> Dict
    def get_entry(key: str) -> Optional[Dict]
    def list_entries(prefix=None, metadata_filter=None, limit=100) -> List[Dict]
    def delete_entry(key: str) -> bool
    def get_changed_entries(since_snapshot_id=None) -> List[Dict]
    def create_snapshot(name: str) -> str
    def store_batch(entries: List[Dict], atomic=True) -> Dict
    def retrieve_batch(keys: List[str]) -> Dict
    def cleanup_expired(batch_size=100) -> Dict
```

### SQLAlchemy ORM Model Design

```python
from sqlalchemy import Column, String, Text, Integer, DateTime, JSON, Index
from sqlalchemy.orm import declarative_base

Base = declarative_base()

class LTMEntry(Base):
    __tablename__ = "ltm_entries"

    key = Column(String(512), primary_key=True)
    value = Column(JSON, nullable=False)
    metadata_ = Column("metadata", JSON)
    created_at = Column(DateTime, server_default=func.now())
    updated_at = Column(DateTime, onupdate=func.now())

    # Dialect-aware full-text search index added conditionally

class CatalogEntry(Base):
    __tablename__ = "ltm_catalog"

    id = Column(String(64), primary_key=True)  # SHA-256 of key
    key = Column(String(512), unique=True, nullable=False)
    content_hash = Column(String(128), nullable=False)
    storage_uri = Column(Text)  # NULL if inlined
    byte_size = Column(Integer, nullable=False)
    inlined_value = Column(JSON)
    metadata_ = Column("metadata", JSON)
    expires_at = Column(DateTime)
    created_at = Column(DateTime, server_default=func.now())
    updated_at = Column(DateTime, onupdate=func.now())
```

### YAML Configuration Examples

```yaml
# SQLAlchemy with PostgreSQL
settings:
  ltm:
    backend: sqlalchemy
    url: postgresql://user:pass@localhost/dbname
    pool_size: 10
    lazy: true  # Defer connection until first use (serverless)
    catalog:
      type: sqlalchemy
      url: postgresql://user:pass@localhost/dbname

# SQLAlchemy with MySQL
settings:
  ltm:
    backend: sqlalchemy
    url: mysql+pymysql://user:pass@localhost/dbname
    catalog:
      type: sqlalchemy
      url: mysql+pymysql://user:pass@localhost/dbname

# SQLAlchemy with SQLite (local dev)
settings:
  ltm:
    backend: sqlalchemy
    url: sqlite:///./ltm.db
    catalog:
      type: sqlalchemy
      url: sqlite:///./ltm.db
```

### Full-Text Search Strategy by Dialect

| Dialect | FTS Strategy |
|---------|--------------|
| PostgreSQL | `to_tsvector()` + GIN index + `@@` operator |
| MySQL | `FULLTEXT` index + `MATCH(...) AGAINST(...)` |
| SQLite | `LIKE '%query%'` fallback (or FTS5 extension if available) |
| Other | `LIKE '%query%'` fallback |

Detect dialect at runtime via `engine.dialect.name`.

### Error Handling Pattern (from existing backends)

Return dicts with consistent format:
- **Success**: `{"success": True, ...additional fields...}`
- **Failure**: `{"success": False, "error": str, "error_type": str}`

Wrap SQLAlchemy exceptions:
- `IntegrityError` → `{"error_type": "validation_error"}`
- `OperationalError` → `{"error_type": "connection_error"}`
- `TimeoutError` → `{"error_type": "connection_timeout"}`

### Optional Dependency Pattern

```python
# In sqlalchemy_backend.py
try:
    from sqlalchemy import create_engine, Column, String, JSON, DateTime
    from sqlalchemy.orm import sessionmaker, declarative_base
    from sqlalchemy.exc import IntegrityError, OperationalError
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False

# In memory/__init__.py
try:
    from .sqlalchemy_backend import SQLAlchemyBackend
    from .catalog_sqlalchemy import SQLAlchemyCatalog
except ImportError:
    pass  # SQLAlchemy not installed
```

### Key Constraints

- **Thread Safety**: Use per-call sessions (simpler, safer for concurrent access)
- **Connection Pooling**: Use SQLAlchemy's QueuePool (default)
- **No Global State**: Engine/session created per instance
- **Lazy Initialization**: Support `lazy=True` parameter - defer engine/table creation until first use (for serverless cold starts)

## Testing

**Framework**: pytest

**Test file locations**:
- `python/tests/test_sqlalchemy_backend.py`
- `python/tests/test_catalog_sqlalchemy.py`

**Test patterns**:

```python
import pytest
from the_edge_agent.memory.sqlalchemy_backend import SQLAlchemyBackend

@pytest.fixture
def backend():
    """In-memory SQLite via SQLAlchemy for testing."""
    backend = SQLAlchemyBackend(url="sqlite:///:memory:")
    yield backend
    backend.close()

def test_store_and_retrieve(backend):
    result = backend.store("test:key", {"data": "value"}, {"type": "test"})
    assert result["success"] is True
    assert result["created"] is True

    retrieved = backend.retrieve("test:key")
    assert retrieved["success"] is True
    assert retrieved["found"] is True
    assert retrieved["value"] == {"data": "value"}

def test_search(backend):
    backend.store("test:one", {"text": "hello world"})
    backend.store("test:two", {"text": "goodbye world"})

    result = backend.search(query="hello", limit=10)
    assert result["success"] is True
    assert result["count"] >= 1

@pytest.mark.skipif(not POSTGRES_AVAILABLE, reason="PostgreSQL not configured")
def test_postgres_integration():
    """Integration test with real PostgreSQL (requires TEST_POSTGRES_URL env)."""
    url = os.environ.get("TEST_POSTGRES_URL")
    backend = SQLAlchemyBackend(url=url)
    # ... run tests
```

**Parity test pattern**:
```python
@pytest.mark.parametrize("backend_factory", [
    lambda: SQLAlchemyBackend(url="sqlite:///:memory:"),
    lambda: SQLiteBackend(path=":memory:"),
])
def test_backend_parity(backend_factory):
    backend = backend_factory()
    # ... validate identical behavior
```

## Risk and Compatibility Check

### Minimal Risk Assessment

- **Primary Risk**: Dialect-specific SQL differences could cause inconsistent behavior
- **Mitigation**: Use SQLAlchemy Core/ORM abstractions that handle dialect differences; test with multiple dialects
- **Rollback**: Backend is additive-only; no changes to existing backends; remove files to rollback

### Compatibility Verification

- [x] No breaking changes to existing APIs
- [x] Database changes are additive only (new tables)
- [x] No UI changes
- [x] Performance impact is negligible (uses SQLAlchemy's optimized pooling)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-15 | 0.1 | Initial draft | Sarah (PO) |
| 2026-01-15 | 0.2 | Validation fixes: clarified default methods, added lazy param, fixed model placement | Sarah (PO) |

## Dev Agent Record

### Agent Model Used

(To be populated by development agent)

### Debug Log References

(To be populated by development agent)

### Completion Notes List

(To be populated by development agent)

### File List

| File | Action |
|------|--------|
| (To be populated by development agent) | |

## QA Results

(To be populated by QA agent)
