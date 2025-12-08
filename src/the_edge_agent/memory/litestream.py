"""
Litestream Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides SQLite with Litestream replication to cloud storage.
Litestream continuously replicates SQLite changes to S3/GCS/Azure.

WARNING: Litestream requires a daemon process running alongside your application.
This backend is NOT suitable for pure serverless (Cloud Functions, Lambda).
Use for: Cloud Run (min-instances > 0), Kubernetes, VMs, Docker containers.

For serverless, use TursoBackend, D1Backend, or BlobSQLiteBackend instead.

Architecture:
    ┌─────────────────────────────────────────────────────────────────┐
    │                  Container / VM Deployment                       │
    ├─────────────────────────────────────────────────────────────────┤
    │  ┌──────────────┐       ┌──────────────┐                        │
    │  │  Your App    │──────▶│  SQLite DB   │                        │
    │  └──────────────┘       └──────┬───────┘                        │
    │                                │                                 │
    │  ┌──────────────────────────────┘                                │
    │  │ Litestream Daemon (background process)                        │
    │  │ - Monitors WAL file                                           │
    │  │ - Streams changes to S3/GCS/Azure                            │
    │  └──────────────┬───────────────────────────────────────────────┤
    └─────────────────┼───────────────────────────────────────────────┘
                      │ Async replication
                      ▼
            ┌─────────────────┐
            │  Cloud Storage  │
            │  (backup/DR)    │
            └─────────────────┘

Litestream Daemon Setup:
    # litestream.yml
    dbs:
      - path: /data/agent_memory.db
        replicas:
          - url: s3://my-bucket/replicas/agent_memory

    # Run daemon
    litestream replicate -config litestream.yml

    # Or as Docker sidecar
    docker run -d --name litestream \\
      -v /data:/data \\
      -e LITESTREAM_CONFIG=/etc/litestream.yml \\
      litestream/litestream replicate

Example:
    >>> from the_edge_agent.memory import LitestreamBackend
    >>>
    >>> # Create backend (restores from replica if db doesn't exist)
    >>> backend = LitestreamBackend(
    ...     db_path="/data/agent_memory.db",
    ...     replica_url="s3://my-bucket/replicas/agent_memory"
    ... )
    >>>
    >>> # Use like SQLiteBackend
    >>> result = backend.store("key", {"value": 42})
    >>> print(result['success'])  # True
    >>>
    >>> # Trigger manual snapshot (optional, Litestream auto-syncs)
    >>> result = backend.trigger_snapshot()
    >>> print(result['success'])  # True
    >>>
    >>> backend.close()
"""

import json
import os
import shutil
import subprocess
import tempfile
from typing import Any, Dict, Optional

from .base import LTMBackend, register_backend
from .sqlite import SQLiteBackend


class LitestreamBackend(LTMBackend):
    """
    SQLite backend with Litestream replication.

    Wraps SQLiteBackend with:
    - Automatic restore from cloud replica on init
    - Manual snapshot trigger support
    - Replica URL tracking for daemon configuration

    Requirements:
        - Litestream binary installed: https://litestream.io/install/
        - Litestream daemon running (replicate command)
        - Cloud storage credentials configured (AWS_*, GCS_*, AZURE_*)

    Example:
        >>> backend = LitestreamBackend(
        ...     db_path="/data/agent_memory.db",
        ...     replica_url="s3://my-bucket/replicas/agent_memory"
        ... )
        >>> result = backend.store("key", "value")
        >>> backend.close()
    """

    def __init__(
        self,
        db_path: str,
        replica_url: str,
        auto_restore: bool = True,
        litestream_path: str = "litestream"
    ):
        """
        Initialize Litestream backend.

        Args:
            db_path: Path to SQLite database file (NOT :memory:)
            replica_url: Cloud storage URL for replica (s3://, gs://, abs://)
            auto_restore: If True, restore from replica if db doesn't exist
            litestream_path: Path to litestream binary (default: "litestream")

        Raises:
            ValueError: If db_path is :memory: (not supported)
        """
        if db_path == ":memory:":
            raise ValueError(
                "LitestreamBackend does not support :memory: databases. "
                "Use SQLiteBackend for in-memory storage, or provide a file path."
            )

        self.db_path = os.path.abspath(db_path)
        self.replica_url = replica_url
        self.litestream_path = litestream_path
        self._closed = False

        # Restore from replica if database doesn't exist
        if auto_restore and not os.path.exists(self.db_path):
            restore_result = self._restore_from_replica()
            if not restore_result['success']:
                # If restore fails, we'll start with empty database
                # This is OK for first run
                pass

        # Initialize underlying SQLite backend
        self._sqlite = SQLiteBackend(self.db_path)

    def _check_litestream_available(self) -> Dict[str, Any]:
        """
        Check if Litestream binary is available.

        Returns:
            {"success": True, "version": str} or
            {"success": False, "error": str, "error_type": "dependency_missing"}
        """
        try:
            result = subprocess.run(
                [self.litestream_path, "version"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                return {
                    "success": True,
                    "version": result.stdout.strip()
                }
            else:
                return {
                    "success": False,
                    "error": f"Litestream error: {result.stderr}",
                    "error_type": "dependency_missing"
                }
        except FileNotFoundError:
            return {
                "success": False,
                "error": f"Litestream not found at '{self.litestream_path}'. "
                         "Install from: https://litestream.io/install/",
                "error_type": "dependency_missing"
            }
        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Litestream command timed out",
                "error_type": "connection_timeout"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Error checking Litestream: {str(e)}",
                "error_type": "query_error"
            }

    def _restore_from_replica(self) -> Dict[str, Any]:
        """
        Restore database from cloud replica.

        Returns:
            {"success": True, "restored": True, "path": str} or
            {"success": False, "error": str, "error_type": str}
        """
        # First check if Litestream is available
        check_result = self._check_litestream_available()
        if not check_result['success']:
            return check_result

        # Ensure parent directory exists
        parent_dir = os.path.dirname(self.db_path)
        if parent_dir and not os.path.exists(parent_dir):
            os.makedirs(parent_dir, exist_ok=True)

        try:
            # Run litestream restore
            result = subprocess.run(
                [
                    self.litestream_path,
                    "restore",
                    "-o", self.db_path,
                    self.replica_url
                ],
                capture_output=True,
                text=True,
                timeout=300  # 5 minute timeout for large databases
            )

            if result.returncode == 0:
                return {
                    "success": True,
                    "restored": True,
                    "path": self.db_path
                }
            else:
                # Check if it's a "no backups found" error (OK for first run)
                stderr = result.stderr.lower()
                if "no backups" in stderr or "no generations" in stderr:
                    return {
                        "success": True,
                        "restored": False,
                        "path": self.db_path,
                        "message": "No existing replica found, starting fresh"
                    }
                return {
                    "success": False,
                    "error": f"Restore failed: {result.stderr}",
                    "error_type": "connection_error"
                }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Restore timed out (exceeded 5 minutes)",
                "error_type": "connection_timeout"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Restore error: {str(e)}",
                "error_type": "query_error"
            }

    def trigger_snapshot(self) -> Dict[str, Any]:
        """
        Trigger an immediate snapshot to the replica.

        Note: Litestream daemon handles continuous replication automatically.
        This method is for forcing an immediate sync before critical operations.

        Returns:
            {"success": True, "snapshot": True} or
            {"success": False, "error": str, "error_type": str}
        """
        check_result = self._check_litestream_available()
        if not check_result['success']:
            return check_result

        try:
            # Checkpoint WAL to ensure all changes are in main database
            self._sqlite._get_connection().execute("PRAGMA wal_checkpoint(TRUNCATE)")

            # Note: Litestream doesn't have a direct "snapshot now" command.
            # The daemon monitors WAL and syncs automatically.
            # For immediate sync, we checkpoint the WAL which triggers sync.
            return {
                "success": True,
                "snapshot": True,
                "message": "WAL checkpointed, Litestream daemon will sync"
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Snapshot error: {str(e)}",
                "error_type": "query_error"
            }

    def get_replica_info(self) -> Dict[str, Any]:
        """
        Get information about the replica configuration.

        Returns:
            {"success": True, "db_path": str, "replica_url": str, "litestream_available": bool}
        """
        check_result = self._check_litestream_available()
        return {
            "success": True,
            "db_path": self.db_path,
            "replica_url": self.replica_url,
            "litestream_available": check_result['success'],
            "litestream_version": check_result.get('version')
        }

    def generate_config(self) -> str:
        """
        Generate Litestream configuration YAML for this database.

        Returns:
            YAML configuration string
        """
        config = f"""# Litestream configuration for {self.db_path}
# Run with: litestream replicate -config litestream.yml

dbs:
  - path: {self.db_path}
    replicas:
      - url: {self.replica_url}
"""
        return config

    # =============================================================================
    # LTMBackend interface - delegated to SQLiteBackend
    # =============================================================================

    def store(self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error"
            }
        return self._sqlite.store(key, value, metadata)

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
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
        return self._sqlite.delete(key)

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
        return self._sqlite.search(query, metadata_filter, limit)

    def close(self) -> None:
        """Close the backend and release resources."""
        if self._closed:
            return

        self._closed = True

        # Trigger final checkpoint before closing
        try:
            self.trigger_snapshot()
        except Exception:
            pass

        # Close underlying SQLite backend
        if hasattr(self, '_sqlite') and self._sqlite is not None:
            self._sqlite.close()

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# Register with the backend factory
register_backend("litestream", LitestreamBackend)
