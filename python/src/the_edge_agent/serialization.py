"""
Custom JSON serialization for The Edge Agent.

This module provides JSON serialization utilities that handle TEA-specific
types like ParallelFlowResult, ensuring proper serialization across all
code paths (CLI, observability, interactive).

Usage:
    from the_edge_agent.serialization import TeaJSONEncoder, dumps

    # Use encoder directly
    json.dumps(data, cls=TeaJSONEncoder)

    # Use convenience function
    dumps(data)
"""

import json
from dataclasses import asdict, is_dataclass
from typing import Any


class TeaJSONEncoder(json.JSONEncoder):
    """
    Custom JSON encoder for TEA types.

    Handles:
    - Objects with to_dict() method (e.g., ParallelFlowResult)
    - Dataclass objects (generic fallback)
    - Standard JSON types

    This encoder is designed to be used at serialization boundaries
    (CLI output, logging, etc.) without modifying the in-memory
    representation of objects.
    """

    def default(self, obj: Any) -> Any:
        """
        Convert non-JSON-serializable objects to JSON-serializable types.

        Args:
            obj: Object to serialize

        Returns:
            JSON-serializable representation

        Priority:
            1. Objects with to_dict() method (preferred for TEA types)
            2. Dataclass objects (generic fallback)
            3. Delegate to parent (raises TypeError for unknown types)
        """
        # Handle objects with to_dict() method (e.g., ParallelFlowResult)
        # This preserves custom serialization logic defined by the class
        if hasattr(obj, "to_dict") and callable(getattr(obj, "to_dict")):
            result = obj.to_dict()
            # Recursively handle nested non-serializable objects
            return self._ensure_serializable(result)

        # Handle other dataclasses generically
        if is_dataclass(obj) and not isinstance(obj, type):
            result = asdict(obj)
            return self._ensure_serializable(result)

        # Let the parent class raise TypeError for unknown types
        return super().default(obj)

    def _ensure_serializable(self, obj: Any) -> Any:
        """
        Recursively ensure all nested objects are JSON serializable.

        Args:
            obj: Object to process

        Returns:
            JSON-serializable version of the object
        """
        if isinstance(obj, dict):
            return {k: self._ensure_serializable(v) for k, v in obj.items()}
        elif isinstance(obj, (list, tuple)):
            return [self._ensure_serializable(item) for item in obj]
        elif hasattr(obj, "to_dict") and callable(getattr(obj, "to_dict")):
            return self._ensure_serializable(obj.to_dict())
        elif is_dataclass(obj) and not isinstance(obj, type):
            return self._ensure_serializable(asdict(obj))
        else:
            return obj


def dumps(obj: Any, **kwargs) -> str:
    """
    JSON dumps with TEA type support.

    Convenience function that uses TeaJSONEncoder by default.
    All standard json.dumps() arguments are supported.

    Args:
        obj: Object to serialize
        **kwargs: Additional arguments passed to json.dumps()
            - cls: Custom encoder class (defaults to TeaJSONEncoder)
            - indent: Indentation level
            - ensure_ascii: Escape non-ASCII characters
            - etc.

    Returns:
        JSON string representation

    Example:
        >>> from the_edge_agent.serialization import dumps
        >>> from the_edge_agent.parallel import ParallelFlowResult
        >>> result = ParallelFlowResult(branch="test", success=True)
        >>> dumps(result)
        '{"branch": "test", "success": true, ...}'
    """
    kwargs.setdefault("cls", TeaJSONEncoder)
    return json.dumps(obj, **kwargs)


def loads(s: str, **kwargs) -> Any:
    """
    JSON loads (standard, included for API completeness).

    Note: This does not reconstruct ParallelFlowResult objects from JSON.
    The deserialized data will be plain dicts. Use ParallelFlowResult's
    constructor or factory methods if you need to reconstruct objects.

    Args:
        s: JSON string to parse
        **kwargs: Additional arguments passed to json.loads()

    Returns:
        Parsed Python object (dict, list, etc.)
    """
    return json.loads(s, **kwargs)
