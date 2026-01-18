"""
Memory Backend Infrastructure for YAMLEngine (TEA-BUILTIN-001.1, 001.4, 001.5, 001.6, 006).

This package provides pluggable memory backends for short-term, long-term,
graph storage, catalog metadata, and Firebase agent memory infrastructure:

SHORT-TERM MEMORY (001.1):
- InMemoryBackend: Session-scoped key-value storage with TTL support
- Uses monotonic time for consistent expiration behavior

LONG-TERM MEMORY (001.4, 001.5):
- LTMBackend: Abstract base class for persistent storage backends
- SQLiteBackend: Local SQLite with FTS5 search (default)
- TursoBackend: Turso/libSQL (edge-native, HTTP-based)
- D1Backend: Cloudflare D1 (serverless SQLite)
- FirestoreBackend: Firebase Firestore
- PostgresBackend: PostgreSQL with tsvector search
- LitestreamBackend: SQLite with cloud replication
- BlobSQLiteBackend: SQLite on blob storage with distributed locking

FIREBASE AGENT MEMORY (006):
- MetadataStore: Document database interface (Firestore)
- BlobStorage: Object storage interface (GCS)
- QueryEngine: SQL query interface with resilience (DuckDB)
- VectorIndex: Vector similarity search interface (DuckDB VSS)

GRAPH DATABASE (001.4, 001.7):
- CozoBackend: CozoDB with Datalog queries and HNSW vectors
- KuzuBackend: Kuzu with Cypher queries and cloud storage
- Neo4jBackend: Neo4j with Cypher queries and remote graph support

CATALOG METADATA (001.6):
- CatalogBackend: Protocol for metadata catalog storage
- SQLiteCatalog: Local SQLite catalog (development/testing)
- FirestoreCatalog: Firestore catalog (serverless)
- PostgresCatalog: PostgreSQL catalog (self-hosted)
- SupabaseCatalog: Supabase REST API catalog (edge)

Example (Short-Term):
    >>> from the_edge_agent.memory import InMemoryBackend
    >>>
    >>> backend = InMemoryBackend()
    >>> backend.store("user_name", "Alice", ttl=300)  # 5 min TTL
    True
    >>> backend.retrieve("user_name")
    'Alice'

Example (Long-Term):
    >>> from the_edge_agent.memory import SQLiteBackend
    >>>
    >>> backend = SQLiteBackend("./agent_memory.db")
    >>> result = backend.store("knowledge", {"topic": "AI"}, metadata={"source": "web"})
    >>> print(result['success'])  # True
    >>> result = backend.search("AI", limit=10)
    >>> print(result['results'])  # [{"key": "knowledge", "value": {...}, ...}]
    >>> backend.close()

Example (Factory Pattern):
    >>> from the_edge_agent.memory import create_ltm_backend
    >>>
    >>> # Create backend by name
    >>> backend = create_ltm_backend("sqlite", db_path="./memory.db")
    >>> backend = create_ltm_backend("turso", url="libsql://...", auth_token="...")
"""

# Short-term memory (TEA-BUILTIN-001.1)
from .short_term import (
    MemoryBackend,
    InMemoryBackend,
)

# LTM base class and factory (TEA-BUILTIN-001.5, 001.6.4, TEA-LTM-010)
from .base import (
    LTMBackend,
    LTMTransaction,
    register_backend,
    get_registered_backends,
    create_ltm_backend,
    parse_backend_config,
    expand_env_vars,
    expand_ltm_config,
    parse_ltm_config,
)

# SQLite backend (TEA-BUILTIN-001.4, refactored in 001.5)
from .sqlite import SQLiteBackend

# Litestream backend (TEA-BUILTIN-001.5)
from .litestream import LitestreamBackend

# Blob SQLite backend (TEA-BUILTIN-001.5)
from .blob_sqlite import BlobSQLiteBackend

# Turso/libSQL backend (TEA-BUILTIN-001.5)
try:
    from .turso import TursoBackend, check_turso_available

    TURSO_AVAILABLE = check_turso_available()
except ImportError:
    TursoBackend = None  # type: ignore
    TURSO_AVAILABLE = False

# Cloudflare D1 backend (TEA-BUILTIN-001.5)
try:
    from .d1 import D1Backend, check_d1_available

    D1_AVAILABLE = check_d1_available()
except ImportError:
    D1Backend = None  # type: ignore
    D1_AVAILABLE = False

# Firestore LTM backend (TEA-BUILTIN-001.5)
try:
    from .firestore_backend import FirestoreBackend, check_firestore_available

    FIRESTORE_LTM_AVAILABLE = check_firestore_available()
except ImportError:
    FirestoreBackend = None  # type: ignore
    FIRESTORE_LTM_AVAILABLE = False

# PostgreSQL backend (TEA-BUILTIN-001.5)
try:
    from .postgres import PostgresBackend, check_postgres_available

    POSTGRES_AVAILABLE = check_postgres_available()
except ImportError:
    PostgresBackend = None  # type: ignore
    POSTGRES_AVAILABLE = False

# Graph backends (TEA-BUILTIN-001.4, 001.7, 001.8)
from .graph import (
    GraphBackend,
    COZO_AVAILABLE,
    KUZU_AVAILABLE,
    NEO4J_AVAILABLE,
    DUCKPGQ_AVAILABLE,
    _check_cozo_available,
    _check_kuzu_available,
    _check_neo4j_available,
)

# Distributed locks (TEA-BUILTIN-001.5)
from .locks import (
    DistributedLock,
    register_lock,
    get_registered_locks,
    create_lock,
)

# Backward compatibility alias
LongTermMemoryBackend = LTMBackend

# Conditionally import CozoBackend
try:
    from .graph import CozoBackend
except ImportError:
    CozoBackend = None  # type: ignore

# Conditionally import KuzuBackend (Bighorn)
try:
    from .graph import KuzuBackend, BighornBackend
except ImportError:
    KuzuBackend = None  # type: ignore
    BighornBackend = None  # type: ignore

# Conditionally import Neo4jBackend (TEA-BUILTIN-001.7)
try:
    from .graph import Neo4jBackend, Neo4jTransaction
except ImportError:
    Neo4jBackend = None  # type: ignore
    Neo4jTransaction = None  # type: ignore

# Conditionally import DuckPGQBackend (TEA-BUILTIN-001.8)
try:
    from .graph import DuckPGQBackend
except ImportError:
    DuckPGQBackend = None  # type: ignore

# Conditionally import lock implementations
try:
    from .locks import FirestoreLock
except (ImportError, NameError):
    FirestoreLock = None  # type: ignore

try:
    from .locks import RedisLock
except (ImportError, NameError):
    RedisLock = None  # type: ignore

# Firebase Agent Memory Infrastructure (TEA-BUILTIN-006)
# Metadata Store
from .metadata import (
    MetadataStore,
    MetadataQuery,
    create_metadata_store,
    register_metadata_store,
    get_registered_metadata_stores,
    FIRESTORE_AVAILABLE,
)

try:
    from .metadata import FirestoreMetadataStore
except ImportError:
    FirestoreMetadataStore = None  # type: ignore

# Blob Storage
from .blob import (
    BlobStorage,
    BlobInfo,
    create_blob_storage,
    register_blob_storage,
    get_registered_blob_storages,
    GCS_AVAILABLE,
)

try:
    from .blob import GCSBlobStorage
except ImportError:
    GCSBlobStorage = None  # type: ignore

# Query Engine
from .query import (
    QueryEngine,
    QueryConfig,
    CircuitState,
    create_query_engine,
    register_query_engine,
    get_registered_query_engines,
    DUCKDB_AVAILABLE,
)

try:
    from .query import DuckDBQueryEngine
except ImportError:
    DuckDBQueryEngine = None  # type: ignore

# Vector Index
from .vector import (
    VectorIndex,
    VectorSearchConfig,
    SearchResult,
    create_vector_index,
    register_vector_index,
    get_registered_vector_indexes,
    DUCKDB_VSS_AVAILABLE,
)

try:
    from .vector import DuckDBVSSIndex
except ImportError:
    DuckDBVSSIndex = None  # type: ignore

# Catalog Backend (TEA-BUILTIN-001.6)
from .catalog import (
    CatalogBackend,
    compute_content_hash,
    generate_entry_id,
    register_catalog_backend,
    get_registered_catalog_backends,
    create_catalog_backend,
    parse_catalog_config,
)

# Conditionally import catalog implementations
try:
    from .catalog_sqlite import SQLiteCatalog
except ImportError:
    SQLiteCatalog = None  # type: ignore

try:
    from .catalog_firestore import FirestoreCatalog
except ImportError:
    FirestoreCatalog = None  # type: ignore

try:
    from .catalog_postgres import PostgresCatalog
except ImportError:
    PostgresCatalog = None  # type: ignore

try:
    from .catalog_supabase import SupabaseCatalog
except ImportError:
    SupabaseCatalog = None  # type: ignore

try:
    from .catalog_duckdb import DuckDBCatalog, DUCKDB_CATALOG_AVAILABLE
except ImportError:
    DuckDBCatalog = None  # type: ignore
    DUCKDB_CATALOG_AVAILABLE = False

# SQLAlchemy backends (TEA-LTM-012)
try:
    from .sqlalchemy_backend import SQLAlchemyBackend, check_sqlalchemy_available

    SQLALCHEMY_AVAILABLE = check_sqlalchemy_available()
except ImportError:
    SQLAlchemyBackend = None  # type: ignore
    SQLALCHEMY_AVAILABLE = False

try:
    from .catalog_sqlalchemy import SQLAlchemyCatalog
except ImportError:
    SQLAlchemyCatalog = None  # type: ignore

# DuckDB LTM Backend (TEA-BUILTIN-001.6.2)
try:
    from .duckdb_ltm import DuckDBLTMBackend
except ImportError:
    DuckDBLTMBackend = None  # type: ignore

# Entity Hierarchy (TEA-LTM-013)
try:
    from .entity_hierarchy import (
        EntityHierarchy,
        check_hierarchy_available,
        SQLALCHEMY_AVAILABLE as ENTITY_HIERARCHY_AVAILABLE,
    )
except ImportError:
    EntityHierarchy = None  # type: ignore
    ENTITY_HIERARCHY_AVAILABLE = False

    def check_hierarchy_available():
        return False


# Hierarchical LTM Backend (TEA-LTM-015)
try:
    from .hierarchical_ltm import (
        HierarchicalLTMBackend,
        check_hierarchical_available,
        StorageError,
        CatalogError,
    )

    HIERARCHICAL_AVAILABLE = check_hierarchical_available()
except ImportError:
    HierarchicalLTMBackend = None  # type: ignore
    HIERARCHICAL_AVAILABLE = False
    StorageError = Exception  # type: ignore
    CatalogError = Exception  # type: ignore

    def check_hierarchical_available():
        return False


__all__ = [
    # Short-term memory (TEA-BUILTIN-001.1)
    "MemoryBackend",
    "InMemoryBackend",
    # Long-term memory ABC and factory (TEA-BUILTIN-001.5)
    "LTMBackend",
    "LTMTransaction",
    "LongTermMemoryBackend",  # Backward compatibility
    "register_backend",
    "get_registered_backends",
    "create_ltm_backend",
    "parse_backend_config",
    "expand_env_vars",
    "expand_ltm_config",
    "parse_ltm_config",
    # SQLite backend
    "SQLiteBackend",
    # Litestream backend (TEA-BUILTIN-001.5)
    "LitestreamBackend",
    # Blob SQLite backend (TEA-BUILTIN-001.5)
    "BlobSQLiteBackend",
    # Turso backend (TEA-BUILTIN-001.5)
    "TursoBackend",
    "TURSO_AVAILABLE",
    # Cloudflare D1 backend (TEA-BUILTIN-001.5)
    "D1Backend",
    "D1_AVAILABLE",
    # Firestore LTM backend (TEA-BUILTIN-001.5)
    "FirestoreBackend",
    "FIRESTORE_LTM_AVAILABLE",
    # PostgreSQL backend (TEA-BUILTIN-001.5)
    "PostgresBackend",
    "POSTGRES_AVAILABLE",
    # Graph backends (TEA-BUILTIN-001.4, 001.7)
    "GraphBackend",
    "CozoBackend",
    "COZO_AVAILABLE",
    "_check_cozo_available",
    "KuzuBackend",
    "BighornBackend",
    "KUZU_AVAILABLE",
    "_check_kuzu_available",
    "Neo4jBackend",
    "Neo4jTransaction",
    "NEO4J_AVAILABLE",
    "_check_neo4j_available",
    # DuckPGQ backend (TEA-BUILTIN-001.8)
    "DuckPGQBackend",
    "DUCKPGQ_AVAILABLE",
    # Distributed locks (TEA-BUILTIN-001.5)
    "DistributedLock",
    "register_lock",
    "get_registered_locks",
    "create_lock",
    "FirestoreLock",
    "RedisLock",
    # Firebase Agent Memory Infrastructure (TEA-BUILTIN-006)
    # Metadata Store
    "MetadataStore",
    "MetadataQuery",
    "FirestoreMetadataStore",
    "create_metadata_store",
    "register_metadata_store",
    "get_registered_metadata_stores",
    "FIRESTORE_AVAILABLE",
    # Blob Storage
    "BlobStorage",
    "BlobInfo",
    "GCSBlobStorage",
    "create_blob_storage",
    "register_blob_storage",
    "get_registered_blob_storages",
    "GCS_AVAILABLE",
    # Query Engine
    "QueryEngine",
    "QueryConfig",
    "CircuitState",
    "DuckDBQueryEngine",
    "create_query_engine",
    "register_query_engine",
    "get_registered_query_engines",
    "DUCKDB_AVAILABLE",
    # Vector Index
    "VectorIndex",
    "VectorSearchConfig",
    "SearchResult",
    "DuckDBVSSIndex",
    "create_vector_index",
    "register_vector_index",
    "get_registered_vector_indexes",
    "DUCKDB_VSS_AVAILABLE",
    # Catalog Backend (TEA-BUILTIN-001.6)
    "CatalogBackend",
    "compute_content_hash",
    "generate_entry_id",
    "register_catalog_backend",
    "get_registered_catalog_backends",
    "create_catalog_backend",
    "parse_catalog_config",
    "SQLiteCatalog",
    "FirestoreCatalog",
    "PostgresCatalog",
    "SupabaseCatalog",
    "DuckDBCatalog",
    "DUCKDB_CATALOG_AVAILABLE",
    # DuckDB LTM Backend (TEA-BUILTIN-001.6.2)
    "DuckDBLTMBackend",
    # SQLAlchemy backends (TEA-LTM-012)
    "SQLAlchemyBackend",
    "SQLAlchemyCatalog",
    "SQLALCHEMY_AVAILABLE",
    # Entity Hierarchy (TEA-LTM-013)
    "EntityHierarchy",
    "check_hierarchy_available",
    "ENTITY_HIERARCHY_AVAILABLE",
    # Hierarchical LTM Backend (TEA-LTM-015)
    "HierarchicalLTMBackend",
    "check_hierarchical_available",
    "HIERARCHICAL_AVAILABLE",
    "StorageError",
    "CatalogError",
]
