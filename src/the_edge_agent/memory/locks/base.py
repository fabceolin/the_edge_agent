"""
Distributed Lock Abstract Base Class (TEA-BUILTIN-001.5).

This module provides the abstract base class for distributed locks
used by the Blob SQLite backend for coordinating access across
serverless function instances.

Supported Lock Backends:
    - firestore: Firestore document-based locking
    - redis: Redis-based distributed lock (optional)
    - dynamodb: DynamoDB-based locking (future)

Usage Pattern:
    The lock provides exclusive access to a shared resource (SQLite file
    on blob storage) with automatic TTL-based expiration to prevent
    deadlocks on function crashes.

Example:
    >>> from the_edge_agent.memory.locks import create_lock
    >>>
    >>> lock = create_lock("firestore", resource_id="agent_memory.db", ttl=300)
    >>> with lock:
    ...     # Exclusive access to resource
    ...     do_work()
    >>> # Lock automatically released

TTL (Time-To-Live):
    All locks have a TTL (default: 300 seconds / 5 minutes). If a function
    crashes while holding the lock, the lock will automatically expire
    after the TTL, allowing other functions to acquire it.

    The TTL should be longer than the expected operation time but short
    enough to recover from crashes in reasonable time.
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional, Type
import time


class DistributedLock(ABC):
    """
    Abstract base class for distributed locks.

    Distributed locks provide exclusive access to shared resources
    across multiple processes/instances (e.g., serverless functions).

    Thread Safety:
        Implementations MUST be thread-safe.

    Error Handling:
        Methods return dictionaries with consistent error format:
        - Success: {"success": True, ...}
        - Failure: {"success": False, "error": str, "error_type": str}

    Error Types:
        - lock_timeout: Failed to acquire lock within timeout
        - lock_lost: Lock was lost (TTL expired while held)
        - connection_error: Failed to connect to lock backend
        - dependency_missing: Required library not installed
    """

    def __init__(
        self,
        resource_id: str,
        ttl: int = 300,
        owner_id: Optional[str] = None
    ):
        """
        Initialize the distributed lock.

        Args:
            resource_id: Unique identifier for the resource to lock
            ttl: Lock TTL in seconds (default: 300 = 5 minutes)
            owner_id: Unique identifier for this lock holder
                     (auto-generated if not provided)
        """
        self.resource_id = resource_id
        self.ttl = ttl
        self.owner_id = owner_id or self._generate_owner_id()
        self._acquired = False
        self._acquire_time: Optional[float] = None

    def _generate_owner_id(self) -> str:
        """Generate a unique owner ID for this lock holder."""
        import uuid
        import os
        # Combine hostname, PID, and UUID for uniqueness
        hostname = os.environ.get('HOSTNAME', 'unknown')
        pid = os.getpid()
        return f"{hostname}_{pid}_{uuid.uuid4().hex[:8]}"

    @abstractmethod
    def acquire(self, timeout: float = 30.0) -> Dict[str, Any]:
        """
        Attempt to acquire the lock.

        Args:
            timeout: Maximum time in seconds to wait for lock acquisition

        Returns:
            {"success": True, "acquired": True, "owner_id": str}
            or {"success": False, "error": str, "error_type": "lock_timeout"}
        """
        pass

    @abstractmethod
    def release(self) -> Dict[str, Any]:
        """
        Release the lock.

        Returns:
            {"success": True, "released": True}
            or {"success": False, "error": str, "error_type": str}

        Note:
            Only the lock owner can release the lock.
            Releasing an already-released lock is a no-op.
        """
        pass

    @abstractmethod
    def refresh(self) -> Dict[str, Any]:
        """
        Refresh the lock's TTL.

        Call this periodically during long operations to prevent
        the lock from expiring.

        Returns:
            {"success": True, "refreshed": True, "new_ttl": int}
            or {"success": False, "error": str, "error_type": "lock_lost"}
        """
        pass

    @abstractmethod
    def is_held(self) -> Dict[str, Any]:
        """
        Check if the lock is currently held by this owner.

        Returns:
            {"success": True, "held": bool, "remaining_ttl": float|None}
        """
        pass

    def remaining_ttl(self) -> Optional[float]:
        """
        Get remaining TTL if lock is held.

        Returns:
            Remaining TTL in seconds, or None if not held
        """
        if not self._acquired or self._acquire_time is None:
            return None
        elapsed = time.time() - self._acquire_time
        remaining = self.ttl - elapsed
        return max(0, remaining)

    def __enter__(self):
        """Context manager entry - acquire lock."""
        result = self.acquire()
        if not result.get('success'):
            error_type = result.get('error_type', 'unknown')
            error_msg = result.get('error', 'Failed to acquire lock')
            raise RuntimeError(f"Lock acquisition failed ({error_type}): {error_msg}")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - release lock."""
        self.release()
        return False


# =============================================================================
# LOCK REGISTRY AND FACTORY
# =============================================================================


# Registry of lock classes by name
_LOCK_REGISTRY: Dict[str, Type[DistributedLock]] = {}


def register_lock(name: str, lock_class: Type[DistributedLock]) -> None:
    """
    Register a lock class with the registry.

    Args:
        name: Lock backend name (e.g., "firestore", "redis")
        lock_class: Lock class implementing DistributedLock ABC
    """
    _LOCK_REGISTRY[name.lower()] = lock_class


def get_registered_locks() -> List[str]:
    """
    Get list of registered lock backend names.

    Returns:
        List of lock backend names
    """
    return list(_LOCK_REGISTRY.keys())


def create_lock(
    lock_type: str,
    resource_id: str,
    ttl: int = 300,
    **kwargs
) -> DistributedLock:
    """
    Factory function to create a distributed lock instance.

    Args:
        lock_type: Type of lock backend ("firestore", "redis")
        resource_id: Unique identifier for the resource to lock
        ttl: Lock TTL in seconds (default: 300)
        **kwargs: Backend-specific configuration options

    Returns:
        DistributedLock instance

    Raises:
        ValueError: If lock_type is not registered
        ImportError: If required dependencies are not installed

    Example:
        >>> # Firestore lock
        >>> lock = create_lock("firestore", resource_id="agent_memory.db", ttl=300)
        >>>
        >>> # Redis lock
        >>> lock = create_lock(
        ...     "redis",
        ...     resource_id="agent_memory.db",
        ...     ttl=300,
        ...     redis_url="redis://localhost:6379"
        ... )
    """
    lock_name = lock_type.lower()

    if lock_name not in _LOCK_REGISTRY:
        available = ", ".join(get_registered_locks()) or "none"
        raise ValueError(
            f"Unknown lock type: '{lock_type}'. "
            f"Available lock backends: {available}"
        )

    lock_class = _LOCK_REGISTRY[lock_name]
    return lock_class(resource_id=resource_id, ttl=ttl, **kwargs)
