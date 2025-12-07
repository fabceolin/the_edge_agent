"""
Memory Backend Infrastructure for YAMLEngine.

This module provides pluggable memory backends for both short-term (TEA-BUILTIN-001.1)
and long-term (TEA-BUILTIN-001.4) storage:

SHORT-TERM MEMORY (001.1):
- InMemoryBackend: Session-scoped key-value storage with TTL support
- Uses monotonic time for consistent expiration behavior

LONG-TERM MEMORY (001.4):
- LongTermMemoryBackend: Protocol for persistent storage
- SQLiteBackend: Persistent key-value storage with FTS5 search
- CozoBackend: Graph database with Datalog queries and HNSW vectors

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
"""

import copy
import json
import os
import sqlite3
import threading
import time
from typing import Any, Callable, Dict, List, Optional, Protocol, Union


class MemoryBackend(Protocol):
    """
    Protocol for memory backends.

    Memory backends provide key-value storage with optional TTL support.
    Implementations must be thread-safe for use in parallel execution.
    """

    def store(self, key: str, value: Any, ttl: Optional[float] = None, namespace: str = "default") -> bool:
        """
        Store a value with optional TTL.

        Args:
            key: The key to store the value under
            value: The value to store (must be pickle-serializable)
            ttl: Time-to-live in seconds (None for no expiration)
            namespace: Namespace for key isolation

        Returns:
            True if stored successfully, False otherwise
        """
        ...

    def retrieve(self, key: str, namespace: str = "default") -> Optional[Any]:
        """
        Retrieve a value by key.

        Args:
            key: The key to retrieve
            namespace: Namespace to look in

        Returns:
            The stored value, or None if not found or expired
        """
        ...

    def delete(self, key: str, namespace: str = "default") -> bool:
        """
        Delete a value by key.

        Args:
            key: The key to delete
            namespace: Namespace to look in

        Returns:
            True if deleted, False if key didn't exist
        """
        ...

    def exists(self, key: str, namespace: str = "default") -> bool:
        """
        Check if a key exists and is not expired.

        Args:
            key: The key to check
            namespace: Namespace to look in

        Returns:
            True if key exists and is not expired
        """
        ...

    def clear(self, namespace: Optional[str] = None) -> int:
        """
        Clear all keys, optionally within a namespace.

        Args:
            namespace: If provided, only clear this namespace.
                      If None, clear all namespaces.

        Returns:
            Number of keys cleared
        """
        ...

    def get_state(self) -> Dict[str, Any]:
        """
        Get serializable state for checkpoint persistence.

        Returns:
            Dictionary containing all data needed to restore the backend
        """
        ...

    def restore_state(self, state: Dict[str, Any]) -> None:
        """
        Restore from serialized state (from checkpoint).

        Args:
            state: State dictionary from get_state()
        """
        ...


class InMemoryBackend:
    """
    In-memory implementation of MemoryBackend.

    Thread-safe key-value store with TTL support using monotonic time.
    Uses pickle for value serialization to ensure checkpoint compatibility.

    Example:
        >>> backend = InMemoryBackend()
        >>> backend.store("user_name", "Alice", ttl=300)  # 5 min TTL
        True
        >>> backend.retrieve("user_name")
        'Alice'
    """

    def __init__(self):
        """Initialize the in-memory backend."""
        self._data: Dict[str, Dict[str, Dict[str, Any]]] = {}  # namespace -> key -> {value, expires_at}
        self._lock = threading.Lock()

    def store(self, key: str, value: Any, ttl: Optional[float] = None, namespace: str = "default") -> bool:
        """Store a value with optional TTL using monotonic time."""
        with self._lock:
            if namespace not in self._data:
                self._data[namespace] = {}

            expires_at = None
            if ttl is not None:
                expires_at = time.monotonic() + ttl

            self._data[namespace][key] = {
                "value": value,
                "expires_at": expires_at,
                "stored_at": time.time()  # Wall clock for debugging
            }
            return True

    def retrieve(self, key: str, namespace: str = "default") -> Optional[Any]:
        """Retrieve a value, returning None if not found or expired."""
        with self._lock:
            if namespace not in self._data:
                return None

            entry = self._data[namespace].get(key)
            if entry is None:
                return None

            # Check TTL expiration
            if entry["expires_at"] is not None:
                if time.monotonic() > entry["expires_at"]:
                    # Expired - clean up and return None
                    del self._data[namespace][key]
                    return None

            return entry["value"]

    def delete(self, key: str, namespace: str = "default") -> bool:
        """Delete a key from the store."""
        with self._lock:
            if namespace not in self._data:
                return False

            if key in self._data[namespace]:
                del self._data[namespace][key]
                return True
            return False

    def exists(self, key: str, namespace: str = "default") -> bool:
        """Check if key exists and is not expired."""
        # Use retrieve which handles expiration
        return self.retrieve(key, namespace) is not None

    def clear(self, namespace: Optional[str] = None) -> int:
        """Clear keys, optionally within a namespace."""
        with self._lock:
            if namespace is not None:
                if namespace in self._data:
                    count = len(self._data[namespace])
                    self._data[namespace] = {}
                    return count
                return 0
            else:
                count = sum(len(ns_data) for ns_data in self._data.values())
                self._data = {}
                return count

    def get_state(self) -> Dict[str, Any]:
        """
        Get serializable state for checkpoint persistence.

        Converts monotonic TTL timestamps to relative remaining time
        for restoration across sessions.
        """
        with self._lock:
            now_monotonic = time.monotonic()
            state = {"version": "1.0", "namespaces": {}}

            for namespace, keys in self._data.items():
                state["namespaces"][namespace] = {}
                for key, entry in keys.items():
                    # Calculate remaining TTL if applicable
                    remaining_ttl = None
                    if entry["expires_at"] is not None:
                        remaining_ttl = entry["expires_at"] - now_monotonic
                        if remaining_ttl <= 0:
                            # Already expired, skip
                            continue

                    state["namespaces"][namespace][key] = {
                        "value": entry["value"],
                        "remaining_ttl": remaining_ttl,
                        "stored_at": entry["stored_at"]
                    }

            return state

    def restore_state(self, state: Dict[str, Any]) -> None:
        """
        Restore from serialized state.

        Converts relative remaining TTL back to monotonic expiration time.
        """
        with self._lock:
            self._data = {}

            if state.get("version") != "1.0":
                return  # Unknown version, start fresh

            now_monotonic = time.monotonic()

            for namespace, keys in state.get("namespaces", {}).items():
                self._data[namespace] = {}
                for key, entry in keys.items():
                    expires_at = None
                    remaining_ttl = entry.get("remaining_ttl")
                    if remaining_ttl is not None:
                        if remaining_ttl <= 0:
                            # Already expired, skip
                            continue
                        expires_at = now_monotonic + remaining_ttl

                    self._data[namespace][key] = {
                        "value": entry["value"],
                        "expires_at": expires_at,
                        "stored_at": entry.get("stored_at", time.time())
                    }


# =============================================================================
# LONG-TERM MEMORY (TEA-BUILTIN-001.4)
# =============================================================================


class LongTermMemoryBackend(Protocol):
    """
    Protocol for long-term memory backends (TEA-BUILTIN-001.4).

    Long-term memory backends provide persistent key-value storage with
    optional metadata and full-text search capabilities. Unlike short-term
    memory (MemoryBackend), long-term memory:
    - Survives application restarts
    - Does NOT support TTL (data is permanent until deleted)
    - Supports full-text search across stored values
    - Supports metadata filtering

    Implementations must be thread-safe for use in parallel execution.
    All methods return dictionaries with consistent error format:
        Success: {"success": True, ...additional fields...}
        Failure: {"success": False, "error": str, "error_type": str}

    Error types:
        - validation_error: Invalid input parameters
        - connection_error: Database connection issues
        - query_error: Query execution failure
        - serialization_error: JSON encode/decode failure
    """

    def store(self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Store a value persistently with optional metadata.

        Args:
            key: Unique key for the value (will be converted to string)
            value: Value to store (will be JSON serialized)
            metadata: Optional metadata dict (will be JSON serialized)

        Returns:
            {"success": True, "stored": True, "key": str, "created": bool}
            where "created" is True for new keys, False for updates

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """
        Retrieve a value by key.

        Args:
            key: The key to retrieve
            default: Default value if key not found

        Returns:
            {"success": True, "value": any, "found": bool, "metadata": dict|None}
            If not found: {"success": True, "value": default, "found": False, "metadata": None}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def delete(self, key: str) -> Dict[str, Any]:
        """
        Delete a value by key.

        Args:
            key: The key to delete

        Returns:
            {"success": True, "deleted": bool, "key": str}
            where "deleted" is True if key existed, False otherwise

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """
        Search across stored values using full-text search and/or metadata filtering.

        Args:
            query: Full-text search query (FTS5 syntax)
            metadata_filter: Dict of metadata key-value pairs to match
            limit: Maximum number of results to return

        Returns:
            {
                "success": True,
                "results": [
                    {"key": str, "value": any, "metadata": dict, "score": float},
                    ...
                ],
                "count": int
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def close(self) -> None:
        """
        Close the backend and release resources.

        Should be called when the backend is no longer needed.
        Safe to call multiple times.
        """
        ...


class GraphBackend(Protocol):
    """
    Protocol for graph database backends (TEA-BUILTIN-001.4).

    Graph backends provide entity-relationship storage with Datalog queries
    and optional vector search capabilities. Designed for knowledge graphs
    and entity memory in AI agents.

    All methods return dictionaries with consistent error format:
        Success: {"success": True, ...additional fields...}
        Failure: {"success": False, "error": str, "error_type": str}

    Error types:
        - validation_error: Invalid input parameters
        - connection_error: Database connection issues
        - query_error: Query/Datalog execution failure
        - dependency_missing: Required library not installed
    """

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None
    ) -> Dict[str, Any]:
        """
        Store an entity (node) in the graph.

        Args:
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity (e.g., "Person", "Document")
            properties: Optional properties dict (will be JSON serialized)
            embedding: Optional vector embedding for semantic search

        Returns:
            {"success": True, "entity_id": str, "type": str, "created": bool, "has_embedding": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Store a relation (edge) between two entities.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship (e.g., "KNOWS", "MENTIONS")
            properties: Optional properties dict

        Returns:
            {"success": True, "from": str, "to": str, "type": str, "created": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def query(
        self,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None
    ) -> Dict[str, Any]:
        """
        Execute a Datalog query or pattern match.

        Args:
            datalog: Raw Datalog query string
            pattern: Simplified pattern dict (alternative to raw Datalog)
            params: Query parameters (substituted into query)
            limit: Maximum results to return
            timeout: Query timeout in seconds (None for no timeout)

        Returns:
            {
                "success": True,
                "results": list,
                "count": int,
                "query": str (the executed query)
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20
    ) -> Dict[str, Any]:
        """
        Retrieve relevant subgraph context.

        Can be called with:
        - query: Text query (converted to embedding for HNSW search)
        - embedding: Direct embedding vector for HNSW search
        - entity_id: Start from entity and expand N hops

        Args:
            query: Text query for semantic search
            embedding: Direct embedding vector
            entity_id: Entity ID to start neighborhood expansion from
            hops: Number of relationship hops to traverse (default: 2)
            limit: Maximum entities to return

        Returns:
            {
                "success": True,
                "entities": list,
                "relations": list,
                "context_summary": str
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def close(self) -> None:
        """
        Close the backend and release resources.

        Should be called when the backend is no longer needed.
        Safe to call multiple times.
        """
        ...


# =============================================================================
# SQLITE BACKEND IMPLEMENTATION
# =============================================================================


class SQLiteBackend:
    """
    SQLite implementation of LongTermMemoryBackend.

    Provides persistent key-value storage with FTS5 full-text search.
    Uses WAL mode for concurrent access and thread-local connections
    for thread safety.

    Example:
        >>> backend = SQLiteBackend("./agent_memory.db")
        >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
        >>> print(result['success'])  # True
        >>> result = backend.search("value")
        >>> print(result['results'])  # [{"key": "key1", ...}]
        >>> backend.close()
    """

    # Schema version for migrations
    SCHEMA_VERSION = 1

    # Counter for unique in-memory database names
    _instance_counter = 0
    _instance_lock = threading.Lock()

    def __init__(self, db_path: str = ":memory:"):
        """
        Initialize SQLite backend.

        Args:
            db_path: Path to SQLite database file, or ":memory:" for in-memory
        """
        self.db_path = self._validate_path(db_path)
        self._lock = threading.Lock()
        self._closed = False

        # For in-memory databases, use shared cache to allow multi-thread access
        if db_path == ":memory:":
            # Generate unique name for this instance's in-memory database
            with SQLiteBackend._instance_lock:
                SQLiteBackend._instance_counter += 1
                instance_id = SQLiteBackend._instance_counter
            # Use URI mode with shared cache for thread-safe in-memory access
            self._shared_cache_uri = f"file:ltm_mem_{instance_id}?mode=memory&cache=shared"
            self._use_shared_cache = True
        else:
            self._shared_cache_uri = None
            self._use_shared_cache = False

        # Create a single shared connection for schema initialization
        self._conn = self._create_connection()
        self._init_schema(self._conn)

    def _validate_path(self, path: str) -> str:
        """
        Validate and normalize database path.

        Prevents directory traversal attacks by ensuring the path
        resolves within expected boundaries.

        Args:
            path: Database file path

        Returns:
            Normalized absolute path

        Raises:
            ValueError: If path contains traversal attempts
        """
        if path == ":memory:":
            return path

        # Resolve to absolute path
        abs_path = os.path.abspath(path)

        # Check for obvious traversal attempts
        if ".." in os.path.normpath(path):
            raise ValueError(f"Path traversal detected in: {path}")

        return abs_path

    def _create_connection(self) -> sqlite3.Connection:
        """
        Create a new database connection.

        Returns:
            sqlite3.Connection configured appropriately
        """
        if self._use_shared_cache:
            # Use URI mode with shared cache for in-memory databases
            conn = sqlite3.connect(
                self._shared_cache_uri,
                uri=True,
                check_same_thread=False,
                isolation_level=None  # Autocommit
            )
        else:
            conn = sqlite3.connect(
                self.db_path,
                check_same_thread=False,
                isolation_level=None  # Autocommit for WAL
            )
            conn.execute("PRAGMA journal_mode=WAL")

        conn.execute("PRAGMA busy_timeout=5000")
        conn.execute("PRAGMA synchronous=NORMAL")
        conn.row_factory = sqlite3.Row
        return conn

    def _get_connection(self) -> sqlite3.Connection:
        """
        Get the shared database connection.

        Returns:
            sqlite3.Connection
        """
        if self._closed:
            raise RuntimeError("Backend is closed")

        return self._conn

    def _init_schema(self, conn: sqlite3.Connection) -> None:
        """Initialize database schema with FTS5."""
        # Main key-value store
        conn.execute("""
            CREATE TABLE IF NOT EXISTS ltm_store (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL,
                metadata TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)

        # FTS5 virtual table for full-text search
        conn.execute("""
            CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(
                key, value, metadata,
                content='ltm_store',
                content_rowid='rowid'
            )
        """)

        # Triggers to keep FTS in sync
        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_ai AFTER INSERT ON ltm_store BEGIN
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """)

        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_ad AFTER DELETE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
            END
        """)

        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_au AFTER UPDATE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """)

    def store(self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)
            value_json = json.dumps(value)
            metadata_json = json.dumps(metadata) if metadata else None

            with self._lock:
                conn = self._get_connection()

                # Check if key exists
                cursor = conn.execute(
                    "SELECT 1 FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                exists = cursor.fetchone() is not None

                if exists:
                    conn.execute(
                        """UPDATE ltm_store
                           SET value = ?, metadata = ?, updated_at = CURRENT_TIMESTAMP
                           WHERE key = ?""",
                        (value_json, metadata_json, key_str)
                    )
                else:
                    conn.execute(
                        """INSERT INTO ltm_store (key, value, metadata)
                           VALUES (?, ?, ?)""",
                        (key_str, value_json, metadata_json)
                    )

                return {
                    "success": True,
                    "stored": True,
                    "key": key_str,
                    "created": not exists
                }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error"
            }
        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)

            with self._lock:
                conn = self._get_connection()
                cursor = conn.execute(
                    "SELECT value, metadata FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                row = cursor.fetchone()

            if row is None:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None
                }

            value = json.loads(row['value'])
            metadata = json.loads(row['metadata']) if row['metadata'] else None

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": metadata
            }

        except json.JSONDecodeError as e:
            return {
                "success": False,
                "error": f"Failed to deserialize value: {str(e)}",
                "error_type": "serialization_error"
            }
        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)

            with self._lock:
                conn = self._get_connection()
                cursor = conn.execute(
                    "DELETE FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                deleted = cursor.rowcount > 0

                return {
                    "success": True,
                    "deleted": deleted,
                    "key": key_str
                }

        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """Search across stored values using FTS5 and/or metadata filtering."""
        try:
            with self._lock:
                conn = self._get_connection()
                results = []

                if query:
                    # Use FTS5 for full-text search
                    # Escape special FTS5 characters
                    safe_query = query.replace('"', '""')

                    cursor = conn.execute(
                        """SELECT ltm_store.key, ltm_store.value, ltm_store.metadata,
                                  bm25(ltm_fts) as score
                           FROM ltm_fts
                           JOIN ltm_store ON ltm_fts.rowid = ltm_store.rowid
                           WHERE ltm_fts MATCH ?
                           ORDER BY score
                           LIMIT ?""",
                        (f'"{safe_query}"', limit)
                    )
                else:
                    # No FTS query, just list all (with optional metadata filter)
                    cursor = conn.execute(
                        """SELECT key, value, metadata, 0.0 as score
                           FROM ltm_store
                           ORDER BY updated_at DESC
                           LIMIT ?""",
                        (limit,)
                    )

                for row in cursor:
                    try:
                        value = json.loads(row['value'])
                        metadata = json.loads(row['metadata']) if row['metadata'] else None

                        # Apply metadata filter if provided
                        if metadata_filter:
                            if metadata is None:
                                continue
                            matches = all(
                                metadata.get(k) == v
                                for k, v in metadata_filter.items()
                            )
                            if not matches:
                                continue

                        results.append({
                            "key": row['key'],
                            "value": value,
                            "metadata": metadata,
                            "score": float(row['score'])
                        })
                    except json.JSONDecodeError:
                        # Skip malformed entries
                        continue

            return {
                "success": True,
                "results": results,
                "count": len(results)
            }

        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "query_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        # Close the connection
        if hasattr(self, '_conn') and self._conn is not None:
            try:
                self._conn.close()
            except Exception:
                pass
            self._conn = None

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# =============================================================================
# COZO BACKEND IMPLEMENTATION (GRAPH DATABASE)
# =============================================================================


def _check_cozo_available() -> bool:
    """Check if CozoDB is available."""
    try:
        from pycozo import Client  # noqa: F401
        return True
    except ImportError:
        return False


COZO_AVAILABLE = _check_cozo_available()


class CozoBackend:
    """
    CozoDB implementation of GraphBackend.

    Provides graph database functionality with Datalog queries and
    optional HNSW vector search for semantic retrieval. Uses SQLite
    as the storage engine for maximum compatibility.

    Requires: pip install "pycozo[embedded]"

    Example:
        >>> backend = CozoBackend("./agent_graph.db")
        >>> result = backend.store_entity("person_1", "Person", {"name": "Alice"})
        >>> print(result['success'])  # True
        >>> result = backend.store_relation("person_1", "person_2", "KNOWS")
        >>> print(result['success'])  # True
        >>> result = backend.query(datalog="?[id, type] := entity[id, type, _, _, _]")
        >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]
        >>> backend.close()
    """

    # Default embedding dimensions (OpenAI text-embedding-3-small)
    DEFAULT_EMBEDDING_DIM = 1536

    def __init__(self, db_path: str = ":memory:", embedding_dim: int = None):
        """
        Initialize CozoDB backend.

        Args:
            db_path: Path to SQLite database file, or ":memory:" for in-memory
            embedding_dim: Embedding dimensions for HNSW index (default: 1536)

        Raises:
            ImportError: If pycozo is not installed
        """
        if not COZO_AVAILABLE:
            raise ImportError(
                "CozoDB not installed. Install with: pip install 'pycozo[embedded]'"
            )

        from pycozo import Client

        self.db_path = db_path
        self.embedding_dim = embedding_dim or self.DEFAULT_EMBEDDING_DIM
        self._lock = threading.Lock()
        self._closed = False
        self._hnsw_created = False

        # Initialize CozoDB with SQLite storage
        if db_path == ":memory:":
            self._client = Client('mem')
        else:
            self._client = Client('sqlite', db_path)

        self._init_schema()

    def _init_schema(self) -> None:
        """Initialize graph schema with entity and relation tables."""
        # Create entity table
        try:
            self._client.run("""
                :create entity {
                    id: String =>
                    type: String,
                    properties: String,
                    embedding: <F32; 1536>?,
                    created_at: Float default now()
                }
            """)
        except Exception:
            # Table might already exist
            pass

        # Create relation table
        try:
            self._client.run("""
                :create relation {
                    from_id: String,
                    to_id: String,
                    rel_type: String =>
                    properties: String?,
                    created_at: Float default now()
                }
            """)
        except Exception:
            # Table might already exist
            pass

    def _ensure_hnsw_index(self) -> bool:
        """
        Create HNSW index if not exists.

        Returns:
            True if index exists/created, False on failure
        """
        if self._hnsw_created:
            return True

        try:
            self._client.run(f"""
                ::hnsw create entity:semantic_idx {{
                    dim: {self.embedding_dim},
                    m: 50,
                    ef_construction: 200,
                    fields: [embedding],
                    distance: Cosine
                }}
            """)
            self._hnsw_created = True
            return True
        except Exception:
            # Index might already exist or other error
            self._hnsw_created = True  # Assume it exists
            return True

    def _parse_result(self, result: Any) -> List[List]:
        """
        Parse pycozo result into a list of rows.

        pycozo returns: {'headers': [...], 'rows': [...], 'next': None}
        """
        if isinstance(result, dict) and 'rows' in result:
            return result['rows']
        elif hasattr(result, 'rows'):
            return result.rows
        elif isinstance(result, (list, tuple)):
            return list(result)
        return []

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None
    ) -> Dict[str, Any]:
        """Store an entity (node) in the graph."""
        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error"
            }

        try:
            entity_id_str = str(entity_id)
            entity_type_str = str(entity_type)
            props_json = json.dumps(properties or {})

            with self._lock:
                # Check if entity exists
                result = self._client.run(
                    "?[id] := *entity[id, _, _, _, _], id = $id",
                    {"id": entity_id_str}
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                if embedding:
                    # Ensure HNSW index exists
                    self._ensure_hnsw_index()

                    # Store with embedding
                    if exists:
                        self._client.run("""
                            ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, $emb, now()]]
                            :put entity {id => type, properties, embedding, created_at}
                        """, {
                            'id': entity_id_str,
                            'type': entity_type_str,
                            'props': props_json,
                            'emb': embedding
                        })
                    else:
                        self._client.run("""
                            ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, $emb, now()]]
                            :put entity {id => type, properties, embedding, created_at}
                        """, {
                            'id': entity_id_str,
                            'type': entity_type_str,
                            'props': props_json,
                            'emb': embedding
                        })
                else:
                    # Store without embedding
                    self._client.run("""
                        ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, null, now()]]
                        :put entity {id => type, properties, embedding, created_at}
                    """, {
                        'id': entity_id_str,
                        'type': entity_type_str,
                        'props': props_json
                    })

                return {
                    "success": True,
                    "entity_id": entity_id_str,
                    "type": entity_type_str,
                    "created": not exists,
                    "has_embedding": embedding is not None
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store entity: {str(e)}",
                "error_type": "query_error"
            }

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a relation (edge) between two entities."""
        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error"
            }

        try:
            from_id = str(from_entity)
            to_id = str(to_entity)
            rel_type = str(relation_type)
            props_json = json.dumps(properties) if properties else None

            with self._lock:
                # Check if relation exists
                result = self._client.run(
                    "?[from_id, to_id, rel_type] := *relation[from_id, to_id, rel_type, _, _], from_id = $from, to_id = $to, rel_type = $rel",
                    {"from": from_id, "to": to_id, "rel": rel_type}
                )
                rows = self._parse_result(result)
                exists = len(rows) > 0

                # Store relation
                self._client.run("""
                    ?[from_id, to_id, rel_type, properties, created_at] <- [[$from, $to, $rel, $props, now()]]
                    :put relation {from_id, to_id, rel_type => properties, created_at}
                """, {
                    'from': from_id,
                    'to': to_id,
                    'rel': rel_type,
                    'props': props_json
                })

                return {
                    "success": True,
                    "from": from_id,
                    "to": to_id,
                    "type": rel_type,
                    "created": not exists
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to store relation: {str(e)}",
                "error_type": "query_error"
            }

    def query(
        self,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None
    ) -> Dict[str, Any]:
        """Execute a Datalog query or pattern match."""
        if not datalog and not pattern:
            return {
                "success": False,
                "error": "Either datalog or pattern is required",
                "error_type": "validation_error"
            }

        try:
            query_str = datalog

            if pattern and not datalog:
                # Convert pattern to Datalog
                query_str = self._pattern_to_datalog(pattern, limit)

            # Add limit if not already in query
            if query_str and ":limit" not in query_str.lower():
                query_str = f"{query_str}\n:limit {limit}"

            with self._lock:
                result = self._client.run(query_str, params or {})

                # Convert result to list of dicts
                # pycozo returns: {'headers': [...], 'rows': [...], 'next': None}
                results = []
                rows = self._parse_result(result)
                headers = result.get('headers', []) if isinstance(result, dict) else []

                for row in rows:
                    if headers:
                        results.append(dict(zip(headers, row)))
                    else:
                        results.append(list(row))

                return {
                    "success": True,
                    "results": results,
                    "count": len(results),
                    "query": query_str
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "query_error"
            }

    def _pattern_to_datalog(self, pattern: Dict[str, Any], limit: int) -> str:
        """Convert a simple pattern dict to Datalog query."""
        # Simple pattern: {entity_type: "Person"} -> find all Person entities
        if 'entity_type' in pattern:
            return f"""
                ?[id, type, properties] :=
                    *entity[id, type, properties, _, _],
                    type = "{pattern['entity_type']}"
                :limit {limit}
            """

        # Simple pattern: {from_entity: "X"} -> find relations from X
        if 'from_entity' in pattern:
            return f"""
                ?[to_id, rel_type, properties] :=
                    *relation[from_id, to_id, rel_type, properties, _],
                    from_id = "{pattern['from_entity']}"
                :limit {limit}
            """

        # Default: return all entities
        return f"""
            ?[id, type, properties] := *entity[id, type, properties, _, _]
            :limit {limit}
        """

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20
    ) -> Dict[str, Any]:
        """Retrieve relevant subgraph context."""
        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error"
            }

        try:
            entities = []
            relations = []

            with self._lock:
                if entity_id:
                    # Expand from entity via N-hop traversal
                    entities, relations = self._expand_from_entity(entity_id, hops, limit)

                elif embedding:
                    # HNSW vector search
                    self._ensure_hnsw_index()
                    entities = self._hnsw_search(embedding, limit)

                    # Get relations between found entities
                    if entities:
                        entity_ids = [e['id'] for e in entities]
                        relations = self._get_relations_between(entity_ids)

                # Build context summary
                context_summary = self._build_context_summary(entities, relations)

                return {
                    "success": True,
                    "entities": entities,
                    "relations": relations,
                    "context_summary": context_summary
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to retrieve context: {str(e)}",
                "error_type": "query_error"
            }

    def _expand_from_entity(self, entity_id: str, hops: int, limit: int) -> tuple:
        """Expand N hops from an entity."""
        entities = []
        relations = []

        # Get starting entity
        result = self._client.run(
            "?[id, type, properties] := *entity[id, type, properties, _, _], id = $id",
            {"id": entity_id}
        )

        rows = self._parse_result(result)
        for row in rows:
            entities.append({
                "id": row[0],
                "type": row[1],
                "properties": json.loads(row[2]) if row[2] else {}
            })

        # Build recursive query for N hops
        if hops >= 1:
            hop_query = f"""
                hop1[id] := *relation["{entity_id}", id, _, _, _]
                hop1[id] := *relation[id, "{entity_id}", _, _, _]
            """

            for h in range(2, hops + 1):
                hop_query += f"""
                hop{h}[id] := hop{h-1}[mid], *relation[mid, id, _, _, _]
                hop{h}[id] := hop{h-1}[mid], *relation[id, mid, _, _, _]
                """

            # Combine all hops
            all_hops = " ".join([f"all_connected[id] := hop{i}[id]" for i in range(1, hops + 1)])

            query = f"""
                {hop_query}
                {all_hops}
                ?[id, type, properties] :=
                    all_connected[id],
                    *entity[id, type, properties, _, _]
                :limit {limit}
            """

            result = self._client.run(query)
            rows = self._parse_result(result)
            for row in rows:
                if row[0] != entity_id:  # Don't duplicate start entity
                    entities.append({
                        "id": row[0],
                        "type": row[1],
                        "properties": json.loads(row[2]) if row[2] else {}
                    })

        # Get relations
        entity_ids = [e['id'] for e in entities]
        if entity_ids:
            relations = self._get_relations_between(entity_ids)

        return entities, relations

    def _hnsw_search(self, embedding: List[float], limit: int) -> List[Dict]:
        """Perform HNSW vector similarity search."""
        entities = []

        try:
            result = self._client.run(f"""
                ?[id, type, properties, score] :=
                    ~entity:semantic_idx {{id, type, properties |
                        query: $query_embedding,
                        k: {limit},
                        ef: 50,
                        bind_distance: score
                    }}
            """, {'query_embedding': embedding})

            rows = self._parse_result(result)
            for row in rows:
                entities.append({
                    "id": row[0],
                    "type": row[1],
                    "properties": json.loads(row[2]) if row[2] else {},
                    "score": float(row[3])
                })
        except Exception:
            # HNSW might not be available or no embeddings stored
            pass

        return entities

    def _get_relations_between(self, entity_ids: List[str]) -> List[Dict]:
        """Get all relations between a set of entities."""
        relations = []

        if not entity_ids:
            return relations

        # Build query to find relations within the set
        ids_str = ", ".join([f'"{id}"' for id in entity_ids])
        query = f"""
            ?[from_id, to_id, rel_type, properties] :=
                *relation[from_id, to_id, rel_type, properties, _],
                from_id in [{ids_str}],
                to_id in [{ids_str}]
        """

        try:
            result = self._client.run(query)
            rows = self._parse_result(result)
            for row in rows:
                relations.append({
                    "from": row[0],
                    "to": row[1],
                    "type": row[2],
                    "properties": json.loads(row[3]) if row[3] else {}
                })
        except Exception:
            pass

        return relations

    def _build_context_summary(self, entities: List[Dict], relations: List[Dict]) -> str:
        """Build a text summary of the context."""
        if not entities:
            return "No relevant entities found."

        summary_parts = []

        # Summarize entities by type
        type_counts = {}
        for e in entities:
            t = e.get('type', 'Unknown')
            type_counts[t] = type_counts.get(t, 0) + 1

        entity_summary = ", ".join([f"{count} {t}" for t, count in type_counts.items()])
        summary_parts.append(f"Found {len(entities)} entities: {entity_summary}.")

        # Summarize relations
        if relations:
            rel_types = set(r.get('type', 'Unknown') for r in relations)
            summary_parts.append(f"Connected by {len(relations)} relations ({', '.join(rel_types)}).")

        return " ".join(summary_parts)

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if hasattr(self, '_client') and self._client is not None:
            try:
                # CozoDB doesn't have explicit close, but we can clear reference
                self._client = None
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass
