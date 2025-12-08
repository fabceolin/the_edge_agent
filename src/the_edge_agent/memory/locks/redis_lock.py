"""
Redis Distributed Lock (TEA-BUILTIN-001.5).

This module provides a distributed lock implementation using Redis.
Uses the Redlock algorithm for reliable distributed locking.

Requires: pip install redis

Example:
    >>> from the_edge_agent.memory.locks import RedisLock
    >>>
    >>> lock = RedisLock(
    ...     resource_id="agent_memory.db",
    ...     ttl=300,  # 5 minutes
    ...     redis_url="redis://localhost:6379"
    ... )
    >>>
    >>> with lock:
    ...     # Exclusive access to resource
    ...     do_work()
"""

import time
from typing import Any, Dict, Optional

from .base import DistributedLock


def _check_redis_available() -> bool:
    """Check if redis is available."""
    try:
        import redis  # noqa: F401
        return True
    except ImportError:
        return False


REDIS_AVAILABLE = _check_redis_available()


class RedisLock(DistributedLock):
    """
    Redis-based distributed lock.

    Uses Redis SET with NX and EX options to implement atomic lock acquisition.
    Supports lock refresh and safe release using Lua scripts.

    Example:
        >>> lock = RedisLock(
        ...     resource_id="my-resource",
        ...     ttl=300,
        ...     redis_url="redis://localhost:6379"
        ... )
        >>> with lock:
        ...     # Exclusive access
        ...     pass
    """

    # Lua script for safe lock release (only release if we own the lock)
    RELEASE_SCRIPT = """
    if redis.call("get", KEYS[1]) == ARGV[1] then
        return redis.call("del", KEYS[1])
    else
        return 0
    end
    """

    # Lua script for lock refresh (only refresh if we own the lock)
    REFRESH_SCRIPT = """
    if redis.call("get", KEYS[1]) == ARGV[1] then
        return redis.call("pexpire", KEYS[1], ARGV[2])
    else
        return 0
    end
    """

    def __init__(
        self,
        resource_id: str,
        ttl: int = 300,
        owner_id: Optional[str] = None,
        redis_url: str = "redis://localhost:6379",
        key_prefix: str = "lock:",
        db: int = 0
    ):
        """
        Initialize Redis lock.

        Args:
            resource_id: Unique identifier for the resource to lock
            ttl: Lock TTL in seconds (default: 300 = 5 minutes)
            owner_id: Unique identifier for this lock holder
            redis_url: Redis connection URL
            key_prefix: Prefix for lock keys in Redis
            db: Redis database number

        Raises:
            ImportError: If redis is not installed
        """
        if not REDIS_AVAILABLE:
            raise ImportError(
                "redis not installed. Install with: pip install redis"
            )

        super().__init__(resource_id, ttl, owner_id)

        self.redis_url = redis_url
        self.key_prefix = key_prefix
        self.db = db
        self._client = None
        self._release_script = None
        self._refresh_script = None

    def _get_client(self):
        """Get or create Redis client."""
        if self._client is not None:
            return self._client

        import redis

        self._client = redis.from_url(self.redis_url, db=self.db)

        # Register Lua scripts
        self._release_script = self._client.register_script(self.RELEASE_SCRIPT)
        self._refresh_script = self._client.register_script(self.REFRESH_SCRIPT)

        return self._client

    def _get_lock_key(self) -> str:
        """Get the Redis key for this lock."""
        # Sanitize resource_id for use as Redis key
        safe_id = self.resource_id.replace("/", "_").replace(":", "_")
        return f"{self.key_prefix}{safe_id}"

    def acquire(self, timeout: float = 30.0) -> Dict[str, Any]:
        """
        Attempt to acquire the lock.

        Uses Redis SET with NX (only if not exists) and EX (expiration).

        Args:
            timeout: Maximum time in seconds to wait for lock acquisition

        Returns:
            {"success": True, "acquired": True, "owner_id": str}
            or {"success": False, "error": str, "error_type": "lock_timeout"}
        """
        try:
            client = self._get_client()
            lock_key = self._get_lock_key()
            start_time = time.time()

            while True:
                elapsed = time.time() - start_time
                if elapsed >= timeout:
                    return {
                        "success": False,
                        "error": f"Lock acquisition timed out after {timeout}s",
                        "error_type": "lock_timeout"
                    }

                # Try to acquire lock with SET NX EX
                acquired = client.set(
                    lock_key,
                    self.owner_id,
                    nx=True,  # Only set if not exists
                    ex=self.ttl  # Expiration in seconds
                )

                if acquired:
                    self._acquired = True
                    self._acquire_time = time.time()
                    return {
                        "success": True,
                        "acquired": True,
                        "owner_id": self.owner_id
                    }

                # Check if the existing lock has expired (shouldn't happen with EX)
                current_owner = client.get(lock_key)
                if current_owner is None:
                    # Lock was released, try again immediately
                    continue

                # Wait before retrying
                time.sleep(0.1)

        except ImportError:
            return {
                "success": False,
                "error": "redis not installed. Install with: pip install redis",
                "error_type": "dependency_missing"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to acquire lock: {str(e)}",
                "error_type": "connection_error"
            }

    def release(self) -> Dict[str, Any]:
        """
        Release the lock.

        Uses Lua script to atomically check ownership and delete.

        Returns:
            {"success": True, "released": True}
            or {"success": False, "error": str, "error_type": str}
        """
        try:
            client = self._get_client()
            lock_key = self._get_lock_key()

            # Use Lua script for atomic check-and-delete
            result = self._release_script(
                keys=[lock_key],
                args=[self.owner_id]
            )

            self._acquired = False
            self._acquire_time = None

            if result:
                return {
                    "success": True,
                    "released": True
                }
            else:
                # Lock doesn't exist or is owned by someone else
                return {
                    "success": True,
                    "released": False,
                    "message": "Lock was not held or owned by another"
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to release lock: {str(e)}",
                "error_type": "connection_error"
            }

    def refresh(self) -> Dict[str, Any]:
        """
        Refresh the lock's TTL.

        Uses Lua script to atomically check ownership and extend expiration.

        Returns:
            {"success": True, "refreshed": True, "new_ttl": int}
            or {"success": False, "error": str, "error_type": "lock_lost"}
        """
        try:
            client = self._get_client()
            lock_key = self._get_lock_key()

            # Use Lua script for atomic check-and-extend
            # PEXPIRE uses milliseconds
            result = self._refresh_script(
                keys=[lock_key],
                args=[self.owner_id, self.ttl * 1000]
            )

            if result:
                self._acquire_time = time.time()
                return {
                    "success": True,
                    "refreshed": True,
                    "new_ttl": self.ttl
                }
            else:
                self._acquired = False
                return {
                    "success": False,
                    "error": "Lock was lost or held by another owner",
                    "error_type": "lock_lost"
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to refresh lock: {str(e)}",
                "error_type": "connection_error"
            }

    def is_held(self) -> Dict[str, Any]:
        """
        Check if the lock is currently held by this owner.

        Returns:
            {"success": True, "held": bool, "remaining_ttl": float|None}
        """
        try:
            client = self._get_client()
            lock_key = self._get_lock_key()

            # Get current owner
            current_owner = client.get(lock_key)

            if current_owner is None:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            # Decode if bytes
            if isinstance(current_owner, bytes):
                current_owner = current_owner.decode('utf-8')

            if current_owner != self.owner_id:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            # Get remaining TTL
            ttl_ms = client.pttl(lock_key)
            if ttl_ms <= 0:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            return {
                "success": True,
                "held": True,
                "remaining_ttl": ttl_ms / 1000.0
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to check lock: {str(e)}",
                "error_type": "connection_error"
            }
