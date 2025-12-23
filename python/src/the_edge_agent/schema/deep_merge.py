"""
Deep Merge Algorithm for JSON Schemas.

This module provides kubectl-style deep merge semantics for combining JSON Schemas:
- Objects are recursively merged
- Arrays are replaced (not concatenated)
- Scalars use last-wins semantics
- Null/None can override non-null values

TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm

Example:
    >>> from the_edge_agent.schema import deep_merge, merge_all
    >>>
    >>> base = {"a": 1, "b": {"x": 10, "y": 20}}
    >>> overlay = {"b": {"y": 30, "z": 40}, "c": 3}
    >>> result = deep_merge(base, overlay)
    >>> print(result)
    {'a': 1, 'b': {'x': 10, 'y': 30, 'z': 40}, 'c': 3}
    >>>
    >>> # Merge multiple schemas (first = lowest priority)
    >>> schemas = [base_schema, overlay1, overlay2]
    >>> merged = merge_all(schemas)
"""

from typing import Any, Dict, List, Optional, Union
import copy


def deep_merge(
    base: Optional[Dict[str, Any]],
    overlay: Optional[Dict[str, Any]]
) -> Optional[Dict[str, Any]]:
    """
    Deep merge two dictionaries with kubectl-style semantics.

    Merge rules:
    - Objects (dicts) are recursively merged
    - Arrays (lists) are replaced, not concatenated
    - Scalars use last-wins (overlay overrides base)
    - Null/None values can override non-null values

    Args:
        base: Base dictionary (lower priority). Can be None.
        overlay: Overlay dictionary (higher priority). Can be None.

    Returns:
        Merged dictionary. Returns None if overlay is None.
        Returns copy of base if overlay is empty dict.

    Examples:
        >>> # Object merge
        >>> deep_merge({"a": 1, "b": {"x": 10}}, {"b": {"y": 20}})
        {'a': 1, 'b': {'x': 10, 'y': 20}}

        >>> # Array replacement (NOT concatenation)
        >>> deep_merge({"items": [1, 2, 3]}, {"items": [4, 5]})
        {'items': [4, 5]}

        >>> # Null override
        >>> deep_merge({"enabled": True}, {"enabled": None})
        {'enabled': None}

        >>> # Scalar last-wins
        >>> deep_merge({"count": 10}, {"count": 20})
        {'count': 20}
    """
    # None overlay returns None (null can override)
    if overlay is None:
        return None

    # None base with dict overlay returns overlay copy
    if base is None:
        return copy.deepcopy(overlay)

    # Type mismatch: overlay wins
    if not isinstance(base, dict) or not isinstance(overlay, dict):
        return copy.deepcopy(overlay) if isinstance(overlay, dict) else overlay

    # Both are dicts: recursive merge
    result = copy.deepcopy(base)

    for key, overlay_value in overlay.items():
        if key in result:
            base_value = result[key]
            # Both values are dicts: recurse
            if isinstance(base_value, dict) and isinstance(overlay_value, dict):
                result[key] = deep_merge(base_value, overlay_value)
            else:
                # Arrays, scalars, or type mismatch: overlay wins
                if isinstance(overlay_value, (dict, list)):
                    result[key] = copy.deepcopy(overlay_value)
                else:
                    result[key] = overlay_value
        else:
            # New key from overlay
            if isinstance(overlay_value, (dict, list)):
                result[key] = copy.deepcopy(overlay_value)
            else:
                result[key] = overlay_value

    return result


def merge_all(schemas: List[Optional[Dict[str, Any]]]) -> Dict[str, Any]:
    """
    Merge multiple schemas in order (first is lowest priority, last is highest).

    This applies kubectl-style merge semantics across a list of schemas.
    Later schemas in the list override earlier ones.

    Args:
        schemas: List of schemas to merge. None values are treated as
                 potentially overriding if they appear in the sequence.

    Returns:
        Merged schema. Returns empty dict if input is empty or all None.

    Example:
        >>> base = {"$schema": "...", "type": "object", "properties": {"a": {"type": "string"}}}
        >>> overlay1 = {"properties": {"b": {"type": "number"}}}
        >>> overlay2 = {"properties": {"a": {"type": "string", "maxLength": 100}}}
        >>> result = merge_all([base, overlay1, overlay2])
        >>> # Result has a (with maxLength), b, and base properties
    """
    if not schemas:
        return {}

    # Filter out None values for initial result, but track positions
    non_none_schemas = [s for s in schemas if s is not None]

    if not non_none_schemas:
        return {}

    result = copy.deepcopy(non_none_schemas[0])

    for schema in non_none_schemas[1:]:
        result = deep_merge(result, schema)
        # deep_merge handles None overlay by returning None,
        # but in merge_all we skip None to maintain dict result

    return result if result is not None else {}


def validate_json_schema(schema: Dict[str, Any]) -> Dict[str, Any]:
    """
    Validate a schema against JSON Schema Draft 2020-12.

    Args:
        schema: JSON Schema to validate

    Returns:
        Dict with 'valid' (bool), 'errors' (list), and optionally 'schema' (normalized)

    Raises:
        ImportError: If jsonschema package is not installed
    """
    try:
        import jsonschema
        from jsonschema import Draft202012Validator
    except ImportError:
        return {
            "valid": False,
            "errors": ["jsonschema package not installed. Install with: pip install jsonschema"],
        }

    errors = []

    # Check if it's a valid JSON Schema
    try:
        Draft202012Validator.check_schema(schema)
    except jsonschema.SchemaError as e:
        errors.append(f"Schema validation error: {e.message}")

    return {
        "valid": len(errors) == 0,
        "errors": errors,
        "schema": schema if len(errors) == 0 else None,
    }
