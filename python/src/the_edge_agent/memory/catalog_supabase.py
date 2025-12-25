"""
Supabase Catalog Backend (TEA-BUILTIN-001.6.1).

Provides SupabaseCatalog implementation of CatalogBackend protocol for
edge deployments using Supabase REST API.

Requirements:
    pip install httpx>=0.25.0

Environment:
    SUPABASE_URL: Supabase project URL
    SUPABASE_ANON_KEY: Supabase anonymous key (or service role key)

Example:
    >>> from the_edge_agent.memory.catalog_supabase import SupabaseCatalog
    >>>
    >>> catalog = SupabaseCatalog(
    ...     url="https://xxx.supabase.co",
    ...     key="eyJ...",
    ... )
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri="gs://bucket/path",
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ... )
"""

from __future__ import annotations

import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

try:
    import httpx

    HTTPX_AVAILABLE = True
except ImportError:
    HTTPX_AVAILABLE = False
    httpx = None  # type: ignore

from .catalog import (
    CatalogBackend,
    generate_entry_id,
    register_catalog_backend,
)


logger = logging.getLogger(__name__)


class SupabaseCatalog:
    """
    Supabase-based catalog backend for edge deployments.

    Implements CatalogBackend protocol using Supabase REST API (AC-24).
    Compatible with Row Level Security policies (AC-26).

    Args:
        url: Supabase project URL (e.g., https://xxx.supabase.co)
        key: Supabase API key (anon_key or service_role)
        use_service_role: Whether key is service_role (bypasses RLS)
    """

    def __init__(
        self,
        url: str,
        key: str,
        use_service_role: bool = False,
    ):
        """
        Initialize Supabase catalog.

        Args:
            url: Supabase project URL
            key: API key (AC-25)
            use_service_role: Use service role key (bypasses RLS)

        Raises:
            ImportError: If httpx is not installed
        """
        if not HTTPX_AVAILABLE:
            raise ImportError(
                "httpx is required for SupabaseCatalog. "
                "Install with: pip install httpx>=0.25.0"
            )

        self._url = url.rstrip("/")
        self._key = key
        self._use_service_role = use_service_role

        # Set up HTTP client with auth headers
        self._client = httpx.Client(
            base_url=f"{self._url}/rest/v1",
            headers={
                "apikey": key,
                "Authorization": f"Bearer {key}",
                "Content-Type": "application/json",
                "Prefer": "return=representation",
            },
            timeout=30.0,
        )

    def _handle_response(self, response: httpx.Response) -> Dict[str, Any]:
        """Handle Supabase API response."""
        if response.status_code >= 400:
            try:
                error = response.json()
            except json.JSONDecodeError:
                error = {"message": response.text}
            logger.error(f"Supabase API error: {error}")
            return {"success": False, "error": error.get("message", str(error))}

        try:
            data = response.json() if response.content else None
            return {"success": True, "data": data}
        except json.JSONDecodeError:
            return {"success": True, "data": None}

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        """
        Track an LTM entry using Supabase REST API (AC-24).
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc).isoformat()

        # Check if exists
        response = self._client.get(
            "/ltm_entries",
            params={"id": f"eq.{entry_id}", "select": "id"},
        )
        result = self._handle_response(response)
        if not result["success"]:
            return result

        existing = result["data"] and len(result["data"]) > 0
        created = not existing

        entry_data = {
            "id": entry_id,
            "key": key,
            "content_hash": content_hash,
            "storage_uri": storage_uri,
            "byte_size": byte_size,
            "metadata": metadata,
            "inlined_value": inlined_value,
            "expires_at": expires_at.isoformat() if expires_at else None,
            "updated_at": now,
        }

        if created:
            entry_data["created_at"] = now
            response = self._client.post("/ltm_entries", json=entry_data)
        else:
            response = self._client.patch(
                "/ltm_entries",
                params={"id": f"eq.{entry_id}"},
                json=entry_data,
            )

        result = self._handle_response(response)
        if not result["success"]:
            return result

        return {"success": True, "entry_id": entry_id, "created": created}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key (AC-3).
        """
        entry_id = generate_entry_id(key)

        response = self._client.get(
            "/ltm_entries",
            params={"id": f"eq.{entry_id}"},
        )
        result = self._handle_response(response)

        if not result["success"]:
            return None

        data = result["data"]
        if not data or len(data) == 0:
            return None

        entry = data[0]
        # Parse datetime strings
        for field in ("created_at", "updated_at", "expires_at"):
            if entry.get(field) and isinstance(entry[field], str):
                try:
                    entry[field] = datetime.fromisoformat(
                        entry[field].replace("Z", "+00:00")
                    )
                except ValueError:
                    pass

        return entry

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria (AC-4).
        """
        params: Dict[str, str] = {
            "order": "updated_at.desc",
            "limit": str(limit),
        }

        if prefix:
            params["key"] = f"like.{prefix}%"

        if metadata_filter:
            # Supabase JSONB containment: metadata->>key = value
            for k, v in metadata_filter.items():
                if isinstance(v, str):
                    params[f"metadata->>'{k}'"] = f"eq.{v}"
                else:
                    params[f"metadata->>'{k}'"] = f"eq.{json.dumps(v)}"

        response = self._client.get("/ltm_entries", params=params)
        result = self._handle_response(response)

        if not result["success"]:
            return []

        return result["data"] or []

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key (AC-5).
        """
        entry_id = generate_entry_id(key)

        # Check if exists first
        entry = self.get_entry(key)
        if not entry:
            return False

        response = self._client.delete(
            "/ltm_entries",
            params={"id": f"eq.{entry_id}"},
        )
        result = self._handle_response(response)

        return result["success"]

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot (AC-6).

        Note: For Supabase, we fetch all entries and snapshot entries,
        then compute the diff client-side for RLS compatibility.
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        # Get snapshot entries
        response = self._client.get(
            "/ltm_snapshot_entries",
            params={
                "snapshot_id": f"eq.{since_snapshot_id}",
                "select": "entry_id,content_hash",
            },
        )
        result = self._handle_response(response)

        if not result["success"]:
            return []

        snapshot_entries = {
            e["entry_id"]: e["content_hash"] for e in (result["data"] or [])
        }

        # Get current entries and filter
        all_entries = self.list_entries(limit=10000)
        changed = []
        for entry in all_entries:
            entry_id = entry["id"]
            if entry_id not in snapshot_entries:
                changed.append(entry)
            elif snapshot_entries[entry_id] != entry["content_hash"]:
                changed.append(entry)

        return changed

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot (AC-7).
        """
        snapshot_id = str(uuid.uuid4())
        now = datetime.now(timezone.utc).isoformat()

        # Get all entries for totals
        all_entries = self.list_entries(limit=10000)
        entry_count = len(all_entries)
        total_bytes = sum(e.get("byte_size", 0) for e in all_entries)

        # Create snapshot record
        response = self._client.post(
            "/ltm_snapshots",
            json={
                "id": snapshot_id,
                "name": name,
                "entry_count": entry_count,
                "total_bytes": total_bytes,
                "created_at": now,
            },
        )
        result = self._handle_response(response)
        if not result["success"]:
            raise RuntimeError(f"Failed to create snapshot: {result.get('error')}")

        # Record entry states (batch insert)
        snapshot_entries = [
            {
                "snapshot_id": snapshot_id,
                "entry_id": e["id"],
                "content_hash": e["content_hash"],
            }
            for e in all_entries
        ]

        if snapshot_entries:
            response = self._client.post(
                "/ltm_snapshot_entries",
                json=snapshot_entries,
            )
            result = self._handle_response(response)
            if not result["success"]:
                logger.warning(
                    f"Failed to record snapshot entries: {result.get('error')}"
                )

        return snapshot_id

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        response = self._client.get(
            "/ltm_snapshots",
            params={"id": f"eq.{snapshot_id}"},
        )
        result = self._handle_response(response)

        if not result["success"]:
            return None

        data = result["data"]
        if not data or len(data) == 0:
            return None

        return data[0]

    def close(self) -> None:
        """Close HTTP client."""
        self._client.close()
        logger.debug("SupabaseCatalog closed")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


# Register with factory (AC-28)
if HTTPX_AVAILABLE:
    register_catalog_backend("supabase", SupabaseCatalog)


__all__ = ["SupabaseCatalog", "HTTPX_AVAILABLE"]
