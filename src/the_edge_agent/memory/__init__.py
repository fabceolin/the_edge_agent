"""
Memory Backend Infrastructure for YAMLEngine (TEA-BUILTIN-001.1, 001.4, 001.5).

This package provides pluggable memory backends for short-term, long-term,
and graph storage:

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

GRAPH DATABASE (001.4):
- CozoBackend: CozoDB with Datalog queries and HNSW vectors
- KuzuBackend: Kuzu with Cypher queries and cloud storage

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

# LTM base class and factory (TEA-BUILTIN-001.5)
from .base import (
    LTMBackend,
    register_backend,
    get_registered_backends,
    create_ltm_backend,
    parse_backend_config,
)

# SQLite backend (TEA-BUILTIN-001.4, refactored in 001.5)
from .sqlite import SQLiteBackend

# Litestream backend (TEA-BUILTIN-001.5)
from .litestream import LitestreamBackend

# Blob SQLite backend (TEA-BUILTIN-001.5)
from .blob_sqlite import BlobSQLiteBackend

# Graph backends (TEA-BUILTIN-001.4)
from .graph import (
    GraphBackend,
    COZO_AVAILABLE,
    KUZU_AVAILABLE,
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

# Conditionally import lock implementations
try:
    from .locks import FirestoreLock
except (ImportError, NameError):
    FirestoreLock = None  # type: ignore

try:
    from .locks import RedisLock
except (ImportError, NameError):
    RedisLock = None  # type: ignore

__all__ = [
    # Short-term memory (TEA-BUILTIN-001.1)
    "MemoryBackend",
    "InMemoryBackend",
    # Long-term memory ABC and factory (TEA-BUILTIN-001.5)
    "LTMBackend",
    "LongTermMemoryBackend",  # Backward compatibility
    "register_backend",
    "get_registered_backends",
    "create_ltm_backend",
    "parse_backend_config",
    # SQLite backend
    "SQLiteBackend",
    # Litestream backend (TEA-BUILTIN-001.5)
    "LitestreamBackend",
    # Blob SQLite backend (TEA-BUILTIN-001.5)
    "BlobSQLiteBackend",
    # Graph backends (TEA-BUILTIN-001.4)
    "GraphBackend",
    "CozoBackend",
    "COZO_AVAILABLE",
    "KuzuBackend",
    "BighornBackend",
    "KUZU_AVAILABLE",
    # Distributed locks (TEA-BUILTIN-001.5)
    "DistributedLock",
    "register_lock",
    "get_registered_locks",
    "create_lock",
    "FirestoreLock",
    "RedisLock",
]
