"""
Shared State for A2A Communication.

This module provides namespace-scoped shared state with optimistic locking
for concurrent agent updates.

Features:
- Namespace isolation to prevent cross-workflow conflicts
- Optimistic locking for concurrent update detection
- TTL support for automatic cleanup
- Thread-safe implementation

Story: TEA-AGENT-001.5 (AC: 6)
"""

import copy
import threading
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Dict, Optional, Protocol, runtime_checkable


class SharedStateError(Exception):
    """Base exception for shared state errors."""

    pass


class OptimisticLockError(SharedStateError):
    """Raised when optimistic locking detects a concurrent modification."""

    def __init__(self, key: str, expected_version: int, actual_version: int):
        self.key = key
        self.expected_version = expected_version
        self.actual_version = actual_version
        super().__init__(
            f"Optimistic lock conflict on key '{key}': "
            f"expected version {expected_version}, found {actual_version}"
        )


@dataclass
class StateEntry:
    """
    Entry in the shared state store.

    Attributes:
        value: The stored value
        version: Version number for optimistic locking
        expires_at: Optional expiration timestamp (UTC)
        updated_at: Last update timestamp (UTC)
    """

    value: Any
    version: int = 1
    expires_at: Optional[float] = None
    updated_at: float = field(default_factory=time.time)

    def is_expired(self) -> bool:
        """Check if entry has expired."""
        if self.expires_at is None:
            return False
        return time.time() > self.expires_at


@runtime_checkable
class SharedState(Protocol):
    """
    Protocol for shared state backends.

    Implementations must be thread-safe for concurrent access.
    """

    def get(
        self,
        namespace: str,
        key: str,
        default: Any = None,
    ) -> tuple[Any, int]:
        """
        Get a value from shared state.

        Args:
            namespace: State namespace
            key: State key
            default: Default value if key doesn't exist

        Returns:
            Tuple of (value, version). Version is 0 if key doesn't exist.
        """
        ...

    def set(
        self,
        namespace: str,
        key: str,
        value: Any,
        ttl: Optional[int] = None,
        expected_version: Optional[int] = None,
    ) -> int:
        """
        Set a value in shared state.

        Args:
            namespace: State namespace
            key: State key
            value: Value to store (must be JSON-serializable)
            ttl: Optional time-to-live in seconds
            expected_version: Optional version for optimistic locking

        Returns:
            New version number

        Raises:
            OptimisticLockError: If expected_version doesn't match current version
        """
        ...

    def delete(
        self,
        namespace: str,
        key: str,
        expected_version: Optional[int] = None,
    ) -> bool:
        """
        Delete a key from shared state.

        Args:
            namespace: State namespace
            key: State key
            expected_version: Optional version for optimistic locking

        Returns:
            True if key existed and was deleted, False otherwise

        Raises:
            OptimisticLockError: If expected_version doesn't match
        """
        ...

    def keys(self, namespace: str) -> list[str]:
        """
        List all keys in a namespace.

        Args:
            namespace: State namespace

        Returns:
            List of key names
        """
        ...

    def clear_namespace(self, namespace: str) -> int:
        """
        Clear all entries in a namespace.

        Args:
            namespace: State namespace

        Returns:
            Number of entries cleared
        """
        ...


class InMemorySharedState:
    """
    In-memory shared state implementation.

    Thread-safe implementation with optimistic locking and TTL support.
    """

    def __init__(self):
        """Initialize in-memory shared state."""
        self._lock = threading.RLock()
        # State stored as: {namespace: {key: StateEntry}}
        self._state: Dict[str, Dict[str, StateEntry]] = {}

    def get(
        self,
        namespace: str,
        key: str,
        default: Any = None,
    ) -> tuple[Any, int]:
        """
        Get a value from shared state.

        Args:
            namespace: State namespace
            key: State key
            default: Default value if key doesn't exist

        Returns:
            Tuple of (value, version). Version is 0 if key doesn't exist.
        """
        with self._lock:
            ns_state = self._state.get(namespace, {})
            entry = ns_state.get(key)

            if entry is None:
                return default, 0

            if entry.is_expired():
                # Clean up expired entry
                del ns_state[key]
                return default, 0

            # Return deep copy to prevent mutation
            return copy.deepcopy(entry.value), entry.version

    def set(
        self,
        namespace: str,
        key: str,
        value: Any,
        ttl: Optional[int] = None,
        expected_version: Optional[int] = None,
    ) -> int:
        """
        Set a value in shared state.

        Args:
            namespace: State namespace
            key: State key
            value: Value to store
            ttl: Optional time-to-live in seconds
            expected_version: Optional version for optimistic locking

        Returns:
            New version number

        Raises:
            OptimisticLockError: If expected_version doesn't match current version
        """
        with self._lock:
            if namespace not in self._state:
                self._state[namespace] = {}
            ns_state = self._state[namespace]

            current_entry = ns_state.get(key)
            current_version = 0

            if current_entry is not None:
                if current_entry.is_expired():
                    # Expired entry treated as non-existent
                    del ns_state[key]
                    current_entry = None
                else:
                    current_version = current_entry.version

            # Check optimistic lock
            if expected_version is not None:
                if current_version != expected_version:
                    raise OptimisticLockError(key, expected_version, current_version)

            # Calculate new version
            new_version = current_version + 1

            # Calculate expiration time
            expires_at = None
            if ttl is not None:
                expires_at = time.time() + ttl

            # Store deep copy to prevent external mutation
            ns_state[key] = StateEntry(
                value=copy.deepcopy(value),
                version=new_version,
                expires_at=expires_at,
            )

            return new_version

    def delete(
        self,
        namespace: str,
        key: str,
        expected_version: Optional[int] = None,
    ) -> bool:
        """
        Delete a key from shared state.

        Args:
            namespace: State namespace
            key: State key
            expected_version: Optional version for optimistic locking

        Returns:
            True if key existed and was deleted, False otherwise

        Raises:
            OptimisticLockError: If expected_version doesn't match
        """
        with self._lock:
            ns_state = self._state.get(namespace, {})
            entry = ns_state.get(key)

            if entry is None:
                return False

            if entry.is_expired():
                del ns_state[key]
                return False

            # Check optimistic lock
            if expected_version is not None:
                if entry.version != expected_version:
                    raise OptimisticLockError(key, expected_version, entry.version)

            del ns_state[key]
            return True

    def keys(self, namespace: str) -> list[str]:
        """
        List all keys in a namespace.

        Args:
            namespace: State namespace

        Returns:
            List of key names (excludes expired entries)
        """
        with self._lock:
            ns_state = self._state.get(namespace, {})
            # Filter out expired entries
            valid_keys = [
                key for key, entry in ns_state.items() if not entry.is_expired()
            ]
            return valid_keys

    def clear_namespace(self, namespace: str) -> int:
        """
        Clear all entries in a namespace.

        Args:
            namespace: State namespace

        Returns:
            Number of entries cleared
        """
        with self._lock:
            if namespace in self._state:
                count = len(self._state[namespace])
                del self._state[namespace]
                return count
            return 0

    def cleanup_expired(self, namespace: Optional[str] = None) -> int:
        """
        Remove expired entries.

        Args:
            namespace: Optional namespace to clean (None = all)

        Returns:
            Number of entries removed
        """
        count = 0
        with self._lock:
            namespaces = [namespace] if namespace else list(self._state.keys())
            for ns in namespaces:
                if ns not in self._state:
                    continue
                ns_state = self._state[ns]
                expired_keys = [
                    key for key, entry in ns_state.items() if entry.is_expired()
                ]
                for key in expired_keys:
                    del ns_state[key]
                    count += 1
        return count

    def get_all(self, namespace: str) -> Dict[str, Any]:
        """
        Get all values in a namespace (for testing/debugging).

        Args:
            namespace: State namespace

        Returns:
            Dictionary of all key-value pairs (excludes expired)
        """
        with self._lock:
            ns_state = self._state.get(namespace, {})
            return {
                key: copy.deepcopy(entry.value)
                for key, entry in ns_state.items()
                if not entry.is_expired()
            }


# Global singleton for in-process shared state
_global_state: Optional[InMemorySharedState] = None
_global_state_lock = threading.Lock()


def get_global_state() -> InMemorySharedState:
    """
    Get the global in-memory shared state singleton.

    Thread-safe lazy initialization.
    """
    global _global_state
    if _global_state is None:
        with _global_state_lock:
            if _global_state is None:
                _global_state = InMemorySharedState()
    return _global_state


def reset_global_state() -> None:
    """
    Reset the global shared state.

    Useful for testing to ensure clean state.
    """
    global _global_state
    with _global_state_lock:
        _global_state = None
