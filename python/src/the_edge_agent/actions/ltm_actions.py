"""
Long-Term Memory Actions for YAMLEngine (TEA-BUILTIN-001.4).

This module provides persistent key-value storage actions with FTS5 search
for YAMLEngine workflows. Unlike short-term memory (memory.*), long-term
memory survives application restarts.

Actions:
    - ltm.store: Store a key-value pair persistently with optional metadata
    - ltm.retrieve: Retrieve a value by key with optional default fallback
    - ltm.delete: Delete a value by key
    - ltm.search: Search across stored values with FTS5 and metadata filtering

Example:
    >>> # Store with metadata
    >>> result = registry['ltm.store'](
    ...     state={},
    ...     key="knowledge",
    ...     value={"topic": "AI", "facts": ["LLMs are transformers"]},
    ...     metadata={"source": "web", "confidence": 0.9}
    ... )
    >>> print(result['success'])  # True
    >>> print(result['created'])  # True (new key)

    >>> # Retrieve value
    >>> result = registry['ltm.retrieve'](
    ...     state={},
    ...     key="knowledge"
    ... )
    >>> print(result['value'])  # {'topic': 'AI', ...}
    >>> print(result['metadata'])  # {'source': 'web', ...}

    >>> # Search with FTS5
    >>> result = registry['ltm.search'](
    ...     state={},
    ...     query="AI transformers",
    ...     limit=10
    ... )
    >>> print(result['results'])  # [{'key': 'knowledge', ...}]

    >>> # Search with metadata filter
    >>> result = registry['ltm.search'](
    ...     state={},
    ...     metadata_filter={"source": "web"},
    ...     limit=5
    ... )
"""

from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register long-term memory actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing LTM backend
    """

    def ltm_store(state, key, value, metadata=None, **kwargs):
        """
        Store a key-value pair persistently with optional metadata.

        Args:
            state: Current state dictionary
            key: The key to store the value under (required)
            value: The value to store (will be JSON serialized)
            metadata: Optional metadata dict for filtering/tagging

        Returns:
            {"success": True, "stored": True, "key": str, "created": bool} on success
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not hasattr(engine, '_ltm_backend') or engine._ltm_backend is None:
            return {
                "success": False,
                "error": "Long-term memory backend not configured",
                "error_type": "configuration_error"
            }

        if key is None:
            return {
                "success": False,
                "error": "Key is required",
                "error_type": "validation_error"
            }

        return engine._ltm_backend.store(key=str(key), value=value, metadata=metadata)

    registry['ltm.store'] = ltm_store
    registry['actions.ltm_store'] = ltm_store

    def ltm_retrieve(state, key, default=None, **kwargs):
        """
        Retrieve a value from long-term memory by key.

        Args:
            state: Current state dictionary
            key: The key to retrieve (required)
            default: Default value to return if key not found

        Returns:
            {"success": True, "value": any, "found": bool, "metadata": dict|None} on success
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not hasattr(engine, '_ltm_backend') or engine._ltm_backend is None:
            return {
                "success": False,
                "error": "Long-term memory backend not configured",
                "error_type": "configuration_error"
            }

        if key is None:
            return {
                "success": False,
                "error": "Key is required",
                "error_type": "validation_error"
            }

        return engine._ltm_backend.retrieve(key=str(key), default=default)

    registry['ltm.retrieve'] = ltm_retrieve
    registry['actions.ltm_retrieve'] = ltm_retrieve

    def ltm_delete(state, key, **kwargs):
        """
        Delete a value from long-term memory by key.

        Args:
            state: Current state dictionary
            key: The key to delete (required)

        Returns:
            {"success": True, "deleted": bool, "key": str} on success
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not hasattr(engine, '_ltm_backend') or engine._ltm_backend is None:
            return {
                "success": False,
                "error": "Long-term memory backend not configured",
                "error_type": "configuration_error"
            }

        if key is None:
            return {
                "success": False,
                "error": "Key is required",
                "error_type": "validation_error"
            }

        return engine._ltm_backend.delete(key=str(key))

    registry['ltm.delete'] = ltm_delete
    registry['actions.ltm_delete'] = ltm_delete

    def ltm_search(state, query=None, metadata_filter=None, limit=10, **kwargs):
        """
        Search across long-term memory using FTS5 and/or metadata filtering.

        Args:
            state: Current state dictionary
            query: Full-text search query (FTS5 syntax)
            metadata_filter: Dict of metadata key-value pairs to match
            limit: Maximum number of results (default: 10)

        Returns:
            {
                "success": True,
                "results": [{"key": str, "value": any, "metadata": dict, "score": float}, ...],
                "count": int
            } on success
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not hasattr(engine, '_ltm_backend') or engine._ltm_backend is None:
            return {
                "success": False,
                "error": "Long-term memory backend not configured",
                "error_type": "configuration_error"
            }

        return engine._ltm_backend.search(
            query=query,
            metadata_filter=metadata_filter,
            limit=int(limit) if limit else 10
        )

    registry['ltm.search'] = ltm_search
    registry['actions.ltm_search'] = ltm_search
