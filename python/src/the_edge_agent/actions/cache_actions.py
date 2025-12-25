"""
Cache and Memoization Actions for YAMLEngine (TEA-BUILTIN-010).

This module provides cache and memoization actions that automatically cache
action results in Long-Term Memory with configurable TTL and cache key strategies.

Actions:
    - cache.wrap: Wrap any action with automatic caching
    - cache.get: Retrieve cached value by key (for debugging/inspection)
    - cache.invalidate: Explicitly invalidate/delete cached entries
    - storage.hash: Compute SHA256 hash of file content from any URI

Key Strategies:
    - sha256: Hash of specified content
    - args: Hash of action arguments
    - custom: Jinja expression evaluated at runtime
    - file_content: Automatic file content hash from path argument

Example:
    >>> # Cache LLM call by arguments hash
    >>> result = registry['cache.wrap'](
    ...     state={},
    ...     action='llm.call',
    ...     key_strategy='args',
    ...     ttl_days=30,
    ...     args={'model': 'gpt-4', 'messages': [...]}
    ... )
    >>> print(result['_cache_hit'])  # False (first call)

    >>> # Second call returns cached result
    >>> result2 = registry['cache.wrap'](state={}, ...)
    >>> print(result2['_cache_hit'])  # True
"""

import hashlib
import json
import random
from datetime import datetime, timedelta, timezone
from typing import Any, Callable, Dict, List, Optional


def _compute_cache_key(
    action: str,
    args: Dict[str, Any],
    key: Optional[str],
    key_strategy: str,
    key_source: Optional[str],
    registry: Dict[str, Callable],
    state: Dict[str, Any],
) -> str:
    """
    Compute cache key based on strategy.

    Args:
        action: The action name being cached
        args: Arguments passed to the action
        key: Custom key (if provided, takes precedence)
        key_strategy: One of 'sha256', 'args', 'custom', 'file_content'
        key_source: For file_content strategy, the arg name containing file path
        registry: Action registry for storage.hash access
        state: Current state for template evaluation

    Returns:
        Cache key string prefixed with 'cache:'
    """
    if key:
        # Custom key already provided (from Jinja expression)
        # Add prefix if not already present
        if key.startswith("cache:"):
            return key
        return f"cache:{key}"

    if key_strategy == "file_content":
        # Hash file content from specified argument
        if not key_source:
            key_source = "file"  # Default argument name
        file_path = args.get(key_source)
        if file_path and "storage.hash" in registry:
            hash_result = registry["storage.hash"](
                state=state, path=file_path, algorithm="sha256"
            )
            if hash_result.get("success"):
                return f"cache:{action}:{hash_result['hash']}"
        # Fallback to args hash if file hashing fails
        key_strategy = "args"

    if key_strategy == "sha256":
        # Hash the key_source value
        if key_source:
            value = args.get(key_source, "")
            if isinstance(value, bytes):
                content_hash = hashlib.sha256(value).hexdigest()
            else:
                content_hash = hashlib.sha256(str(value).encode()).hexdigest()
            return f"cache:{action}:{content_hash}"
        # Fallback to args hash
        key_strategy = "args"

    # Default: args hash
    # Sort keys and serialize to JSON for deterministic hashing
    args_json = json.dumps(args, sort_keys=True, default=str)
    args_hash = hashlib.sha256(args_json.encode()).hexdigest()
    return f"cache:{action}:{args_hash}"


def _is_expired(expires_at: Optional[str]) -> bool:
    """
    Check if a cache entry has expired.

    Args:
        expires_at: ISO format timestamp string or None

    Returns:
        True if expired or no expiration time, False otherwise
    """
    if not expires_at:
        return True

    try:
        # Parse ISO format timestamp
        # Handle Z suffix (replace with +00:00 for fromisoformat)
        ts = expires_at
        if ts.endswith("Z"):
            ts = ts[:-1] + "+00:00"
        # Handle double timezone (e.g., "+00:00Z" -> "+00:00")
        if "+00:00+00:00" in ts:
            ts = ts.replace("+00:00+00:00", "+00:00")

        expiry_time = datetime.fromisoformat(ts)
        # Ensure timezone aware comparison
        now = datetime.now(timezone.utc)
        if expiry_time.tzinfo is None:
            expiry_time = expiry_time.replace(tzinfo=timezone.utc)
        return now >= expiry_time
    except (ValueError, TypeError):
        return True


def _compute_ttl_seconds(
    ttl_days: Optional[int],
    ttl_hours: Optional[int],
    ttl_seconds: Optional[int],
) -> int:
    """
    Compute TTL in seconds from various parameters.

    Priority: ttl_seconds > ttl_hours > ttl_days > default (60 days)
    """
    if ttl_seconds is not None:
        return ttl_seconds
    if ttl_hours is not None:
        return ttl_hours * 3600
    if ttl_days is not None:
        return ttl_days * 86400
    # Default: 60 days
    return 60 * 86400


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register cache and memoization actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing LTM backend and action registry
    """

    def cache_wrap(
        state: Dict[str, Any],
        action: str,
        args: Dict[str, Any],
        key: Optional[str] = None,
        key_strategy: str = "args",
        key_source: Optional[str] = None,
        ttl_days: Optional[int] = None,
        ttl_hours: Optional[int] = None,
        ttl_seconds: Optional[int] = None,
        skip_cache: bool = False,
        cache_enabled: bool = True,
        cleanup_probability: float = 0.05,
        cleanup_limit: int = 5,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Wrap any action with automatic caching.

        Checks cache before execution, stores result after successful execution.
        Uses Long-Term Memory (LTM) for persistence.

        Args:
            state: Current state dictionary
            action: The action to wrap (e.g., 'llamaextract.extract', 'llm.call')
            args: Arguments to pass to the wrapped action
            key: Cache key or Jinja expression result. If not provided, uses key_strategy.
            key_strategy: Strategy for key generation: 'sha256', 'args', 'custom', 'file_content'
            key_source: For file_content/sha256, the argument name containing source value
            ttl_days: Cache TTL in days (default: 60)
            ttl_hours: Cache TTL in hours (overrides ttl_days)
            ttl_seconds: Cache TTL in seconds (overrides ttl_hours and ttl_days)
            skip_cache: Bypass cache lookup, force fresh execution
            cache_enabled: Enable/disable caching entirely (default: True)
            cleanup_probability: Probability of running cleanup after cache miss (0.0-1.0)
            cleanup_limit: Maximum expired entries to delete per cleanup run

        Returns:
            Action result with cache metadata:
            - success: bool
            - result: The wrapped action's result
            - _cache_hit: bool - Whether result came from cache
            - _cache_key: str - The cache key used
            - _cache_created_at: str - ISO timestamp (if cache hit)
            - error: str - Error message if failed
        """
        # Validate action exists
        if action not in registry:
            return {
                "success": False,
                "error": f"Action '{action}' not found in registry",
                "error_type": "action_not_found",
            }

        # If caching is disabled, just execute the action
        if not cache_enabled:
            try:
                result = registry[action](state=state, **args)
                return {
                    "success": (
                        result.get("success", True)
                        if isinstance(result, dict)
                        else True
                    ),
                    "result": result,
                    "_cache_hit": False,
                    "_cache_key": None,
                }
            except Exception as e:
                return {
                    "success": False,
                    "error": str(e),
                    "error_type": "action_error",
                    "_cache_hit": False,
                }

        # Compute cache key
        cache_key = _compute_cache_key(
            action=action,
            args=args,
            key=key,
            key_strategy=key_strategy,
            key_source=key_source,
            registry=registry,
            state=state,
        )

        # Check cache (unless skip_cache is True)
        if not skip_cache:
            try:
                if hasattr(engine, "_ltm_backend") and engine._ltm_backend is not None:
                    cache_result = engine._ltm_backend.retrieve(key=cache_key)
                    if cache_result.get("found"):
                        metadata = cache_result.get("metadata", {})
                        expires_at = metadata.get("_cache_expires_at")
                        if not _is_expired(expires_at):
                            # Cache hit!
                            cached_value = cache_result.get("value", {})
                            return {
                                "success": True,
                                "result": cached_value.get("result", cached_value),
                                "_cache_hit": True,
                                "_cache_key": cache_key,
                                "_cache_created_at": metadata.get("_cache_created_at"),
                            }
            except Exception:
                # LTM failure is graceful - proceed with action execution (AC-17)
                pass

        # Cache miss - execute wrapped action
        try:
            result = registry[action](state=state, **args)
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "action_error",
                "_cache_hit": False,
                "_cache_key": cache_key,
            }

        # Store in cache if successful
        is_success = result.get("success", True) if isinstance(result, dict) else True
        if is_success:
            try:
                if hasattr(engine, "_ltm_backend") and engine._ltm_backend is not None:
                    now = datetime.now(timezone.utc)
                    ttl_sec = _compute_ttl_seconds(ttl_days, ttl_hours, ttl_seconds)
                    expires_at = now + timedelta(seconds=ttl_sec)

                    engine._ltm_backend.store(
                        key=cache_key,
                        value={"result": result},
                        metadata={
                            "_cache_type": "action_result",
                            "_cache_action": action,
                            "_cache_key": cache_key,
                            "_cache_created_at": now.isoformat(),
                            "_cache_expires_at": expires_at.isoformat(),
                        },
                    )

                    # Maybe cleanup with probability check (AC-14)
                    if random.random() < cleanup_probability:
                        _run_cleanup(engine, cleanup_limit)
            except Exception:
                # LTM store failure is graceful (AC-17)
                pass

        return {
            "success": is_success,
            "result": result,
            "_cache_hit": False,
            "_cache_key": cache_key,
        }

    registry["cache.wrap"] = cache_wrap
    registry["actions.cache_wrap"] = cache_wrap

    def cache_get(
        state: Dict[str, Any],
        key: str,
        include_metadata: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Retrieve cached value by key without executing any action.

        Useful for debugging and cache inspection.

        Args:
            state: Current state dictionary
            key: Cache key to retrieve
            include_metadata: Include cache metadata in response

        Returns:
            - success: bool
            - found: bool - Whether entry exists
            - value: any - Cached value
            - metadata: dict - If include_metadata=True
            - expired: bool - Whether entry has expired
        """
        if not key:
            return {
                "success": False,
                "error": "Key is required",
                "error_type": "validation_error",
            }

        # Add cache prefix if not present
        cache_key = key if key.startswith("cache:") else f"cache:{key}"

        try:
            if not hasattr(engine, "_ltm_backend") or engine._ltm_backend is None:
                return {
                    "success": False,
                    "error": "Long-term memory backend not configured",
                    "error_type": "configuration_error",
                }

            result = engine._ltm_backend.retrieve(key=cache_key)

            if not result.get("found"):
                return {
                    "success": True,
                    "found": False,
                    "value": None,
                    "expired": False,
                }

            metadata = result.get("metadata", {})
            cached_value = result.get("value", {})
            expired = _is_expired(metadata.get("_cache_expires_at"))

            response = {
                "success": True,
                "found": True,
                "value": cached_value.get("result", cached_value),
                "expired": expired,
            }

            if include_metadata:
                response["metadata"] = metadata

            return response

        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "ltm_error",
            }

    registry["cache.get"] = cache_get
    registry["actions.cache_get"] = cache_get

    def cache_invalidate(
        state: Dict[str, Any],
        key: Optional[str] = None,
        pattern: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Invalidate (delete) cached entries by exact key or pattern.

        Args:
            state: Current state dictionary
            key: Exact cache key to invalidate
            pattern: Key pattern for bulk invalidation (uses LTM search)
            metadata_filter: Metadata filter for selective invalidation

        Returns:
            - success: bool
            - deleted_count: int
            - deleted_keys: list[str]
        """
        if not key and not pattern and not metadata_filter:
            return {
                "success": False,
                "error": "At least one of key, pattern, or metadata_filter is required",
                "error_type": "validation_error",
            }

        try:
            if not hasattr(engine, "_ltm_backend") or engine._ltm_backend is None:
                return {
                    "success": False,
                    "error": "Long-term memory backend not configured",
                    "error_type": "configuration_error",
                }

            deleted_keys: List[str] = []

            # Delete by exact key
            if key:
                cache_key = key if key.startswith("cache:") else f"cache:{key}"
                result = engine._ltm_backend.delete(key=cache_key)
                if result.get("deleted"):
                    deleted_keys.append(cache_key)

            # Delete by pattern or metadata filter
            if pattern or metadata_filter:
                # Build filter to find cache entries
                search_filter = metadata_filter or {}
                search_filter["_cache_type"] = "action_result"

                search_result = engine._ltm_backend.search(
                    metadata_filter=search_filter,
                    limit=1000,  # Reasonable limit for bulk operations
                )

                if search_result.get("success") and search_result.get("results"):
                    for entry in search_result["results"]:
                        entry_key = entry.get("key", "")
                        # Apply pattern matching if specified
                        if pattern:
                            # Simple wildcard pattern matching
                            if _pattern_matches(entry_key, pattern):
                                delete_result = engine._ltm_backend.delete(
                                    key=entry_key
                                )
                                if delete_result.get("deleted"):
                                    deleted_keys.append(entry_key)
                        else:
                            # Metadata filter only
                            delete_result = engine._ltm_backend.delete(key=entry_key)
                            if delete_result.get("deleted"):
                                deleted_keys.append(entry_key)

            return {
                "success": True,
                "deleted_count": len(deleted_keys),
                "deleted_keys": deleted_keys,
            }

        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "ltm_error",
            }

    registry["cache.invalidate"] = cache_invalidate
    registry["actions.cache_invalidate"] = cache_invalidate

    def storage_hash(
        state: Dict[str, Any],
        path: str,
        algorithm: str = "sha256",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Compute hash of file content from any URI.

        Supports local files, S3, GCS, Azure via fsspec.

        Args:
            state: Current state dictionary
            path: File path or URI (file://, s3://, gs://, az://)
            algorithm: Hash algorithm: 'sha256', 'md5', 'blake2b'

        Returns:
            - success: bool
            - hash: str - Hex-encoded hash
            - algorithm: str
            - size_bytes: int - File size
            - path: str
            - error: str - If failed
        """
        if not path:
            return {
                "success": False,
                "error": "Path is required",
                "error_type": "validation_error",
            }

        try:
            import fsspec

            # Select hash algorithm
            if algorithm == "md5":
                hasher = hashlib.md5()
            elif algorithm == "blake2b":
                hasher = hashlib.blake2b()
            else:
                hasher = hashlib.sha256()
                algorithm = "sha256"

            # Open file and compute hash
            with fsspec.open(path, "rb") as f:
                content = f.read()
                hasher.update(content)
                size_bytes = len(content)

            return {
                "success": True,
                "hash": hasher.hexdigest(),
                "algorithm": algorithm,
                "size_bytes": size_bytes,
                "path": path,
            }

        except ImportError:
            return {
                "success": False,
                "error": "fsspec is required for storage.hash. Install with: pip install fsspec",
                "error_type": "dependency_error",
            }
        except Exception as e:
            # Graceful degradation (AC-18)
            return {
                "success": False,
                "error": str(e),
                "error_type": "file_error",
                "path": path,
            }

    registry["storage.hash"] = storage_hash
    registry["actions.storage_hash"] = storage_hash


def _pattern_matches(key: str, pattern: str) -> bool:
    """
    Simple wildcard pattern matching for cache keys.

    Supports '*' as wildcard for any sequence of characters.

    Args:
        key: The key to match
        pattern: Pattern with optional wildcards

    Returns:
        True if key matches pattern
    """
    import re

    # Convert pattern to regex
    regex_pattern = "^" + re.escape(pattern).replace(r"\*", ".*") + "$"
    return bool(re.match(regex_pattern, key))


def _run_cleanup(engine: Any, limit: int) -> None:
    """
    Run cache cleanup: delete expired entries (AC-14, AC-15).

    Uses the optimized cleanup_expired method if available (TEA-BUILTIN-001.6.3),
    otherwise falls back to search-and-delete approach for backward compatibility.

    Args:
        engine: YAMLEngine instance with LTM backend
        limit: Maximum entries to delete
    """
    try:
        if not hasattr(engine, "_ltm_backend") or engine._ltm_backend is None:
            return

        backend = engine._ltm_backend

        # Use optimized cleanup_expired if available (TEA-BUILTIN-001.6.3)
        if hasattr(backend, "cleanup_expired"):
            backend.cleanup_expired(batch_size=limit)
            return

        # Fallback: Search and delete expired entries individually
        search_result = backend.search(
            metadata_filter={"_cache_type": "action_result"},
            limit=limit * 4,  # Fetch more to find expired ones
        )

        if not search_result.get("success") or not search_result.get("results"):
            return

        deleted = 0
        for entry in search_result["results"]:
            if deleted >= limit:
                break

            metadata = entry.get("metadata", {})
            expires_at = metadata.get("_cache_expires_at")

            if _is_expired(expires_at):
                backend.delete(key=entry.get("key", ""))
                deleted += 1

    except Exception:
        # Cleanup failures are silent (AC-17: graceful degradation)
        pass


def register_jinja_filters(jinja_env: Any) -> None:
    """
    Register cache-related Jinja filters.

    Args:
        jinja_env: Jinja2 Environment instance
    """

    def sha256_filter(value: Any) -> str:
        """
        Compute SHA256 hash of a value.

        Usage: {{ state.content | sha256 }}
        """
        if isinstance(value, bytes):
            return hashlib.sha256(value).hexdigest()
        return hashlib.sha256(str(value).encode()).hexdigest()

    jinja_env.filters["sha256"] = sha256_filter
