"""
Short-Term Memory Backend (TEA-BUILTIN-001.1).

This module provides the in-memory backend for session-scoped storage
with TTL support. Short-term memory does not persist across restarts.

Example:
    >>> from the_edge_agent.memory import InMemoryBackend
    >>>
    >>> backend = InMemoryBackend()
    >>> backend.store("user_name", "Alice", ttl=300)  # 5 min TTL
    True
    >>> backend.retrieve("user_name")
    'Alice'
"""

import threading
import time
from typing import Any, Dict, Optional, Protocol


class MemoryBackend(Protocol):
    """
    Protocol for short-term memory backends.

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
