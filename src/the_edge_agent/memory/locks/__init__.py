"""
Distributed Lock Backends for Blob SQLite (TEA-BUILTIN-001.5).

This package provides distributed lock implementations for coordinating
access to SQLite files on blob storage across serverless function instances.

Available Lock Backends:
    - firestore: Firestore document-based locking (requires firebase-admin)
    - redis: Redis-based distributed lock (requires redis)

Example:
    >>> from the_edge_agent.memory.locks import create_lock, DistributedLock
    >>>
    >>> # Create a Firestore lock
    >>> lock = create_lock("firestore", resource_id="agent_memory.db", ttl=300)
    >>>
    >>> # Use as context manager
    >>> with lock:
    ...     # Exclusive access to resource
    ...     do_work()
"""

from .base import (
    DistributedLock,
    register_lock,
    get_registered_locks,
    create_lock,
)

__all__ = [
    "DistributedLock",
    "register_lock",
    "get_registered_locks",
    "create_lock",
]

# Import and register lock implementations
# These are imported lazily to avoid dependency errors

try:
    from .firestore_lock import FirestoreLock
    register_lock("firestore", FirestoreLock)
    __all__.append("FirestoreLock")
except ImportError:
    # firebase-admin not installed
    pass

try:
    from .redis_lock import RedisLock
    register_lock("redis", RedisLock)
    __all__.append("RedisLock")
except ImportError:
    # redis not installed
    pass
