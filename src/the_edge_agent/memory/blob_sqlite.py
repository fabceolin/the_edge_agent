"""
Blob SQLite Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides SQLite on blob storage with distributed locking.
Designed for low-concurrency serverless environments where a single
writer at a time is acceptable.

Pattern: Download-Lock-Use-Upload
    1. Acquire distributed lock (Firestore/Redis)
    2. Download SQLite file from blob storage to /tmp
    3. Execute SQLite operations locally (fast)
    4. Upload modified file back to blob storage
    5. Release lock

When to use:
    - Low-concurrency agents (one request at a time is acceptable)
    - Batch processing (many operations per invocation)
    - Small databases (<50MB for reasonable download times)
    - SQLite feature parity needed (FTS5, JSON, window functions)

When NOT to use:
    - High-concurrency (use Turso instead)
    - Real-time applications (download/upload latency)
    - Large databases (>50MB)

Example:
    >>> from the_edge_agent.memory import BlobSQLiteBackend
    >>>
    >>> backend = BlobSQLiteBackend(
    ...     blob_uri="gs://my-bucket/agent_memory.db",
    ...     lock_backend="firestore",
    ...     lock_ttl=300
    ... )
    >>>
    >>> # All operations happen within an implicit lock
    >>> result = backend.store("key", {"value": 42})
    >>> result = backend.retrieve("key")
    >>> backend.close()  # Uploads changes and releases lock

Architecture:
    ┌─────────────────────────────────────────────────────────────────┐
    │                    Cloud Function Execution                      │
    ├─────────────────────────────────────────────────────────────────┤
    │                                                                  │
    │  1. ACQUIRE DISTRIBUTED LOCK (Firestore/Redis)                   │
    │     ┌─────────────────────────────────────────┐                  │
    │     │ Lock: "agent_memory.db"                 │                  │
    │     │ TTL: 300s (prevents deadlock on crash)  │                  │
    │     │ Owner: function_instance_id             │                  │
    │     └────────────────┬────────────────────────┘                  │
    │                      │                                           │
    │  2. DOWNLOAD FROM BLOB STORAGE                                   │
    │                      ▼                                           │
    │     gs://bucket/memory.db ──► /tmp/memory.db (50-500ms)         │
    │                      │                                           │
    │  3. EXECUTE SQLite OPERATIONS (fast, <1ms each)                  │
    │                      │                                           │
    │     store("key", value)                                          │
    │     retrieve("key")                                              │
    │     search("query")                                              │
    │                      │                                           │
    │  4. UPLOAD TO BLOB STORAGE                                       │
    │                      ▼                                           │
    │     /tmp/memory.db ──► gs://bucket/memory.db (50-500ms)         │
    │                      │                                           │
    │  5. RELEASE LOCK                                                 │
    │                      ▼                                           │
    │     Lock released, next request can proceed                      │
    │                                                                  │
    └─────────────────────────────────────────────────────────────────┘
"""

import os
import tempfile
import time
from typing import Any, Dict, Optional

from .base import LTMBackend, register_backend
from .sqlite import SQLiteBackend
from .locks import create_lock, get_registered_locks


class BlobSQLiteBackend(LTMBackend):
    """
    SQLite backend with blob storage and distributed locking.

    Downloads SQLite file from cloud storage, operates locally,
    and uploads changes on close. Uses distributed locks to prevent
    concurrent access.

    Example:
        >>> backend = BlobSQLiteBackend(
        ...     blob_uri="gs://my-bucket/agent_memory.db",
        ...     lock_backend="firestore",
        ...     lock_ttl=300
        ... )
        >>> result = backend.store("key", "value")
        >>> backend.close()  # Uploads changes and releases lock
    """

    def __init__(
        self,
        blob_uri: str,
        lock_backend: str = "firestore",
        lock_ttl: int = 300,
        lock_kwargs: Optional[Dict[str, Any]] = None,
        local_path: Optional[str] = None,
        auto_sync: bool = True
    ):
        """
        Initialize Blob SQLite backend.

        Args:
            blob_uri: Cloud storage URI (s3://, gs://, az://, memory://)
            lock_backend: Lock backend type ("firestore", "redis")
            lock_ttl: Lock TTL in seconds (default: 300 = 5 minutes)
            lock_kwargs: Additional kwargs for lock backend
            local_path: Local path for SQLite file (default: temp file)
            auto_sync: If True, download on init and upload on close

        Raises:
            ValueError: If lock_backend is not available
        """
        self.blob_uri = blob_uri
        self.lock_backend = lock_backend
        self.lock_ttl = lock_ttl
        self.lock_kwargs = lock_kwargs or {}
        self.auto_sync = auto_sync
        self._closed = False
        self._dirty = False
        self._lock = None
        self._sqlite = None
        self._local_path = local_path
        self._temp_file = None

        # Check if lock backend is available
        available_locks = get_registered_locks()
        if lock_backend not in available_locks:
            # Try to provide helpful error message
            if lock_backend == "firestore" and "firestore" not in available_locks:
                raise ValueError(
                    "Firestore lock backend not available. "
                    "Install with: pip install firebase-admin"
                )
            elif lock_backend == "redis" and "redis" not in available_locks:
                raise ValueError(
                    "Redis lock backend not available. "
                    "Install with: pip install redis"
                )
            else:
                raise ValueError(
                    f"Unknown lock backend: '{lock_backend}'. "
                    f"Available: {available_locks or 'none'}"
                )

        # Create local path if not provided
        if self._local_path is None:
            # Create temp file with unique name based on blob URI
            safe_name = blob_uri.replace("/", "_").replace(":", "_")[-50:]
            self._temp_file = tempfile.NamedTemporaryFile(
                prefix=f"blob_sqlite_{safe_name}_",
                suffix=".db",
                delete=False
            )
            self._local_path = self._temp_file.name
            self._temp_file.close()

        # Initialize lock and download if auto_sync
        if auto_sync:
            self._initialize()

    def _initialize(self) -> Dict[str, Any]:
        """Initialize lock and download database."""
        # Extract resource ID from blob URI for lock
        resource_id = self.blob_uri.replace("://", "_").replace("/", "_")

        # Create and acquire lock
        self._lock = create_lock(
            self.lock_backend,
            resource_id=resource_id,
            ttl=self.lock_ttl,
            **self.lock_kwargs
        )

        acquire_result = self._lock.acquire(timeout=30.0)
        if not acquire_result['success']:
            return acquire_result

        # Download from blob storage
        download_result = self._download()
        if not download_result['success']:
            self._lock.release()
            return download_result

        # Initialize SQLite backend
        self._sqlite = SQLiteBackend(self._local_path)

        return {"success": True, "initialized": True}

    def _download(self) -> Dict[str, Any]:
        """Download SQLite file from blob storage."""
        try:
            import fsspec

            # Check if blob exists
            fs = fsspec.filesystem(self.blob_uri.split("://")[0])
            blob_path = self.blob_uri.replace(f"{self.blob_uri.split('://')[0]}://", "")

            if not fs.exists(blob_path):
                # No existing database, start fresh
                return {
                    "success": True,
                    "downloaded": False,
                    "message": "No existing database, starting fresh"
                }

            # Download file
            with fsspec.open(self.blob_uri, 'rb') as src:
                with open(self._local_path, 'wb') as dst:
                    dst.write(src.read())

            return {
                "success": True,
                "downloaded": True,
                "path": self._local_path
            }

        except ImportError:
            return {
                "success": False,
                "error": "fsspec not installed. Install with: pip install fsspec",
                "error_type": "dependency_missing"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to download: {str(e)}",
                "error_type": "connection_error"
            }

    def _upload(self) -> Dict[str, Any]:
        """Upload SQLite file to blob storage."""
        if not self._dirty:
            return {
                "success": True,
                "uploaded": False,
                "message": "No changes to upload"
            }

        try:
            import fsspec

            # Ensure WAL is checkpointed before upload
            if self._sqlite and hasattr(self._sqlite, '_conn') and self._sqlite._conn:
                try:
                    self._sqlite._conn.execute("PRAGMA wal_checkpoint(TRUNCATE)")
                except Exception:
                    pass

            # Upload file
            with open(self._local_path, 'rb') as src:
                with fsspec.open(self.blob_uri, 'wb') as dst:
                    dst.write(src.read())

            self._dirty = False
            return {
                "success": True,
                "uploaded": True,
                "uri": self.blob_uri
            }

        except ImportError:
            return {
                "success": False,
                "error": "fsspec not installed. Install with: pip install fsspec",
                "error_type": "dependency_missing"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to upload: {str(e)}",
                "error_type": "connection_error"
            }

    def sync(self) -> Dict[str, Any]:
        """
        Manually sync changes to blob storage.

        Uploads any pending changes without releasing the lock.

        Returns:
            {"success": True, "uploaded": bool} or error dict
        """
        return self._upload()

    def refresh_lock(self) -> Dict[str, Any]:
        """
        Refresh the distributed lock's TTL.

        Call this periodically during long operations to prevent
        the lock from expiring.

        Returns:
            {"success": True, "refreshed": True, "new_ttl": int} or error dict
        """
        if self._lock is None:
            return {
                "success": False,
                "error": "No lock acquired",
                "error_type": "lock_lost"
            }
        return self._lock.refresh()

    # =============================================================================
    # LTMBackend interface
    # =============================================================================

    def store(self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error"
            }

        if self._sqlite is None:
            return {
                "success": False,
                "error": "Backend not initialized",
                "error_type": "connection_error"
            }

        result = self._sqlite.store(key, value, metadata)
        if result.get('success'):
            self._dirty = True
        return result

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error"
            }

        if self._sqlite is None:
            return {
                "success": False,
                "error": "Backend not initialized",
                "error_type": "connection_error"
            }

        return self._sqlite.retrieve(key, default)

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error"
            }

        if self._sqlite is None:
            return {
                "success": False,
                "error": "Backend not initialized",
                "error_type": "connection_error"
            }

        result = self._sqlite.delete(key)
        if result.get('success') and result.get('deleted'):
            self._dirty = True
        return result

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """Search across stored values using FTS5 and/or metadata filtering."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error"
            }

        if self._sqlite is None:
            return {
                "success": False,
                "error": "Backend not initialized",
                "error_type": "connection_error"
            }

        return self._sqlite.search(query, metadata_filter, limit)

    def close(self) -> None:
        """Close the backend, upload changes, and release lock."""
        if self._closed:
            return

        self._closed = True

        # Upload changes if dirty
        if self.auto_sync and self._dirty:
            try:
                self._upload()
            except Exception:
                pass

        # Close SQLite backend
        if self._sqlite is not None:
            try:
                self._sqlite.close()
            except Exception:
                pass
            self._sqlite = None

        # Release lock
        if self._lock is not None:
            try:
                self._lock.release()
            except Exception:
                pass
            self._lock = None

        # Clean up temp file
        if self._temp_file is not None and os.path.exists(self._local_path):
            try:
                os.unlink(self._local_path)
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# Register with the backend factory
register_backend("blob-sqlite", BlobSQLiteBackend)
