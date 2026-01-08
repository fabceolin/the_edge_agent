"""
Cloudflare D1 Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides the Cloudflare D1 implementation of LTMBackend,
offering serverless SQLite via Cloudflare's HTTP REST API.

D1 is ideal for:
    - Zero state in GCP environments (Firebase Cloud Functions)
    - Cloudflare edge deployment
    - No external packages needed (uses httpx or requests)

Features:
    - SQLite-compatible syntax (including FTS5)
    - HTTP REST API (no native dependencies)
    - Cloudflare edge network (global distribution)
    - D1 handles locking/consistency internally

Example:
    >>> from the_edge_agent.memory import D1Backend
    >>>
    >>> backend = D1Backend(
    ...     account_id="your-account-id",
    ...     database_id="your-database-id",
    ...     api_token="your-api-token"
    ... )
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search("value")
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()
"""

import json
import threading
from typing import Any, Dict, List, Optional

from .base import LTMBackend, register_backend


# Try httpx first, fall back to requests
HTTP_CLIENT = None
try:
    import httpx

    HTTP_CLIENT = "httpx"
except ImportError:
    try:
        import requests

        HTTP_CLIENT = "requests"
    except ImportError:
        pass


class D1Backend(LTMBackend):
    """
    Cloudflare D1 implementation of LTMBackend.

    Provides serverless SQLite via Cloudflare's HTTP REST API.
    No external packages required beyond httpx or requests.

    Example:
        >>> backend = D1Backend(
        ...     account_id="...",
        ...     database_id="...",
        ...     api_token="..."
        ... )
        >>> result = backend.store("key1", {"data": "value"})
        >>> print(result['success'])  # True
        >>> backend.close()
    """

    # Cloudflare D1 API endpoint
    D1_API_BASE = "https://api.cloudflare.com/client/v4/accounts"

    # Schema version for migrations
    SCHEMA_VERSION = 1

    def __init__(
        self, account_id: str, database_id: str, api_token: str, timeout: float = 30.0
    ):
        """
        Initialize D1 backend.

        Args:
            account_id: Cloudflare account ID
            database_id: D1 database ID
            api_token: Cloudflare API token with D1 permissions
            timeout: Request timeout in seconds (default: 30)

        Raises:
            ImportError: If neither httpx nor requests is installed
        """
        if HTTP_CLIENT is None:
            raise ImportError(
                "Neither httpx nor requests is installed. "
                "Install with: pip install httpx"
            )

        self.account_id = account_id
        self.database_id = database_id
        self.api_token = api_token
        self.timeout = timeout
        self._lock = threading.Lock()
        self._closed = False

        # Build API URL
        self._api_url = (
            f"{self.D1_API_BASE}/{account_id}/d1/database/{database_id}/query"
        )

        # Initialize schema
        self._init_schema()

    def _make_request(
        self, sql: str, params: Optional[List[Any]] = None
    ) -> Dict[str, Any]:
        """
        Make an HTTP request to D1 API.

        Args:
            sql: SQL statement
            params: Optional list of parameters

        Returns:
            API response dict or error dict
        """
        headers = {
            "Authorization": f"Bearer {self.api_token}",
            "Content-Type": "application/json",
        }

        payload = {"sql": sql, "params": params or []}

        try:
            if HTTP_CLIENT == "httpx":
                import httpx

                response = httpx.post(
                    self._api_url, headers=headers, json=payload, timeout=self.timeout
                )
                response_data = response.json()
            else:
                import requests

                response = requests.post(
                    self._api_url, headers=headers, json=payload, timeout=self.timeout
                )
                response_data = response.json()

            return response_data

        except Exception as e:
            error_str = str(e).lower()

            if "timeout" in error_str:
                error_type = "connection_timeout"
            elif "connect" in error_str or "network" in error_str:
                error_type = "connection_error"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "errors": [{"message": str(e)}],
                "error_type": error_type,
            }

    def _execute(self, sql: str, params: Optional[List[Any]] = None) -> Dict[str, Any]:
        """
        Execute a SQL statement via D1 API.

        Args:
            sql: SQL statement
            params: Optional list of parameters

        Returns:
            {"success": True, "rows": list, "meta": dict} or error dict
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        with self._lock:
            response = self._make_request(sql, params)

        # Check for API-level success
        if not response.get("success", False):
            errors = response.get("errors", [])
            error_message = (
                errors[0].get("message", "Unknown error") if errors else "Unknown error"
            )

            # Classify error type
            error_str = error_message.lower()
            if "unauthorized" in error_str or "auth" in error_str:
                error_type = "auth_failed"
            elif "rate limit" in error_str or "too many" in error_str:
                # Extract retry_after if present
                retry_after = None
                for error in errors:
                    if "retry_after" in error:
                        retry_after = error.get("retry_after")
                return {
                    "success": False,
                    "error": f"D1 rate limited: {error_message}",
                    "error_type": "rate_limited",
                    "retry_after": retry_after,
                }
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = response.get("error_type", "query_error")

            return {
                "success": False,
                "error": f"D1 error: {error_message}",
                "error_type": error_type,
            }

        # Extract result from response
        result = response.get("result", [])
        if result and len(result) > 0:
            first_result = result[0]
            rows = first_result.get("results", [])
            meta = first_result.get("meta", {})
        else:
            rows = []
            meta = {}

        return {"success": True, "rows": rows, "meta": meta, "rowcount": len(rows)}

    def _init_schema(self) -> None:
        """Initialize database schema with FTS5."""
        # Main key-value store
        self._execute(
            """
            CREATE TABLE IF NOT EXISTS ltm_store (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL,
                metadata TEXT,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """
        )

        # FTS5 virtual table for full-text search
        self._execute(
            """
            CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(
                key, value, metadata,
                content='ltm_store',
                content_rowid='rowid'
            )
        """
        )

        # Triggers to keep FTS in sync
        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_ai AFTER INSERT ON ltm_store BEGIN
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """
        )

        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_ad AFTER DELETE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
            END
        """
        )

        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_au AFTER UPDATE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """
        )

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)
            value_json = json.dumps(value)
            metadata_json = json.dumps(metadata) if metadata else None

            # Check if key exists
            check_result = self._execute(
                "SELECT 1 FROM ltm_store WHERE key = ?", [key_str]
            )

            if not check_result["success"]:
                return check_result

            exists = len(check_result.get("rows", [])) > 0

            if exists:
                result = self._execute(
                    """UPDATE ltm_store
                       SET value = ?, metadata = ?, updated_at = CURRENT_TIMESTAMP
                       WHERE key = ?""",
                    [value_json, metadata_json, key_str],
                )
            else:
                result = self._execute(
                    """INSERT INTO ltm_store (key, value, metadata)
                       VALUES (?, ?, ?)""",
                    [key_str, value_json, metadata_json],
                )

            if not result["success"]:
                return result

            return {
                "success": True,
                "stored": True,
                "key": key_str,
                "created": not exists,
            }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error",
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)

            result = self._execute(
                "SELECT value, metadata FROM ltm_store WHERE key = ?", [key_str]
            )

            if not result["success"]:
                return result

            rows = result.get("rows", [])
            if not rows:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            row = rows[0]
            # D1 returns dicts with column names
            value_raw = row.get(
                "value", row[0] if isinstance(row, (list, tuple)) else None
            )
            metadata_raw = row.get(
                "metadata", row[1] if isinstance(row, (list, tuple)) else None
            )

            value = json.loads(value_raw)
            metadata = json.loads(metadata_raw) if metadata_raw else None

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": metadata,
            }

        except json.JSONDecodeError as e:
            return {
                "success": False,
                "error": f"Failed to deserialize value: {str(e)}",
                "error_type": "serialization_error",
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)

            # Check if key exists before delete
            check_result = self._execute(
                "SELECT 1 FROM ltm_store WHERE key = ?", [key_str]
            )

            if not check_result["success"]:
                return check_result

            exists = len(check_result.get("rows", [])) > 0

            result = self._execute("DELETE FROM ltm_store WHERE key = ?", [key_str])

            if not result["success"]:
                return result

            return {"success": True, "deleted": exists, "key": key_str}

        except Exception as e:
            return {
                "success": False,
                "error": f"Delete error: {str(e)}",
                "error_type": "query_error",
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """Search across stored values using FTS5 and/or metadata filtering."""
        try:
            results = []

            if query:
                # Use FTS5 for full-text search
                safe_query = query.replace('"', '""')

                result = self._execute(
                    """SELECT ltm_store.key, ltm_store.value, ltm_store.metadata,
                              bm25(ltm_fts) as score
                       FROM ltm_fts
                       JOIN ltm_store ON ltm_fts.rowid = ltm_store.rowid
                       WHERE ltm_fts MATCH ?
                       ORDER BY score
                       LIMIT ?""",
                    [f'"{safe_query}"', limit],
                )
            else:
                # No FTS query, just list all
                result = self._execute(
                    """SELECT key, value, metadata, 0.0 as score
                       FROM ltm_store
                       ORDER BY updated_at DESC
                       LIMIT ?""",
                    [limit],
                )

            if not result["success"]:
                return result

            for row in result.get("rows", []):
                try:
                    # Handle both dict and tuple row formats
                    if isinstance(row, dict):
                        key_val = row.get("key")
                        value_raw = row.get("value")
                        metadata_raw = row.get("metadata")
                        score_raw = row.get("score", 0.0)
                    else:
                        key_val = row[0]
                        value_raw = row[1]
                        metadata_raw = row[2]
                        score_raw = row[3] if len(row) > 3 else 0.0

                    value = json.loads(value_raw)
                    metadata = json.loads(metadata_raw) if metadata_raw else None

                    # Apply metadata filter if provided
                    if metadata_filter:
                        if metadata is None:
                            continue
                        matches = all(
                            metadata.get(k) == v for k, v in metadata_filter.items()
                        )
                        if not matches:
                            continue

                    results.append(
                        {
                            "key": key_val,
                            "value": value,
                            "metadata": metadata,
                            "score": float(score_raw) if score_raw else 0.0,
                        }
                    )
                except json.JSONDecodeError:
                    # Skip malformed entries
                    continue

            return {"success": True, "results": results, "count": len(results)}

        except Exception as e:
            return {
                "success": False,
                "error": f"Search error: {str(e)}",
                "error_type": "query_error",
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True
        # No persistent connection to close for HTTP API

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


def check_d1_available() -> bool:
    """Check if HTTP client is available for D1."""
    return HTTP_CLIENT is not None


# Register with the backend factory
register_backend("d1", D1Backend)
