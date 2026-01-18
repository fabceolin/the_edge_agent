"""
Deep Merge Algorithm for JSON Schemas.

This module provides kubectl-style deep merge semantics for combining JSON Schemas:
- Objects are recursively merged
- Arrays with identifying keys are merged by key (YE.9)
- Arrays without keys are replaced (not concatenated)
- Scalars use last-wins semantics
- Null/None can override non-null values

TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm
YE.9: Deep Array Merge by Identifying Key

Array Merge Keys (YE.9):
- nodes: merged by "name" key
- edges: merged by ("from", "to") composite key
- goto: merged by "to" key (inside nodes)

Example:
    >>> from the_edge_agent.schema import deep_merge, merge_all
    >>>
    >>> base = {"a": 1, "b": {"x": 10, "y": 20}}
    >>> overlay = {"b": {"y": 30, "z": 40}, "c": 3}
    >>> result = deep_merge(base, overlay)
    >>> print(result)
    {'a': 1, 'b': {'x': 10, 'y': 30, 'z': 40}, 'c': 3}
    >>>
    >>> # Array merge by key (YE.9)
    >>> base = {"nodes": [{"name": "a", "val": 1}, {"name": "b", "val": 2}]}
    >>> overlay = {"nodes": [{"name": "a", "val": 10}]}
    >>> result = deep_merge(base, overlay)
    >>> # nodes[0].val updated to 10, nodes[1] preserved
    >>>
    >>> # Merge multiple schemas (first = lowest priority)
    >>> schemas = [base_schema, overlay1, overlay2]
    >>> merged = merge_all(schemas)
"""

from typing import Any, Dict, List, Optional, Tuple
import copy


# YE.9: Array merge key configuration
# Maps array field names to their identifying keys
ARRAY_MERGE_KEYS: Dict[str, List[str]] = {
    "nodes": ["name"],
    "edges": ["from", "to"],
}

# For goto arrays (nested in nodes), merge by "to" field
GOTO_MERGE_KEY: List[str] = ["to"]

# Special marker for element deletion
DELETE_MARKER = "__delete__"


def _get_element_key(element: Any, keys: List[str]) -> Optional[Tuple]:
    """
    Extract composite key from an array element.

    Args:
        element: Dictionary element from array
        keys: List of key field names

    Returns:
        Tuple of key values, or None if element is not a dict or missing keys
    """
    if not isinstance(element, dict):
        return None
    key_values = []
    for k in keys:
        if k not in element:
            return None
        key_values.append(element[k])
    return tuple(key_values)


def merge_arrays_by_key(
    base: List[Any],
    overlay: List[Any],
    keys: List[str],
    path: str = ""
) -> List[Any]:
    """
    Merge two arrays by identifying key(s).

    YE.9: Strategic merge patch for arrays using identifying keys.

    Merge rules:
    - Elements with matching keys are deep-merged recursively
    - Elements with __delete__: true marker are removed
    - Non-matching overlay elements are appended

    Args:
        base: Base array (lower priority)
        overlay: Overlay array (higher priority)
        keys: List of field names that identify unique elements
        path: Current path for nested merge context

    Returns:
        Merged array with elements merged by key

    Example:
        >>> base = [{"name": "a", "val": 1}, {"name": "b", "val": 2}]
        >>> overlay = [{"name": "a", "val": 10}, {"name": "c", "val": 3}]
        >>> merge_arrays_by_key(base, overlay, ["name"])
        [{'name': 'a', 'val': 10}, {'name': 'b', 'val': 2}, {'name': 'c', 'val': 3}]
    """
    # Build index of base elements by key
    result = []
    base_index: Dict[Tuple, int] = {}

    for i, elem in enumerate(base):
        elem_copy = copy.deepcopy(elem)
        result.append(elem_copy)
        key_tuple = _get_element_key(elem, keys)
        if key_tuple is not None:
            base_index[key_tuple] = i

    # Track which indices to delete (process deletions after all merges)
    indices_to_delete = set()

    # Process overlay elements
    for overlay_elem in overlay:
        if not isinstance(overlay_elem, dict):
            # Non-dict elements: append as-is
            result.append(copy.deepcopy(overlay_elem))
            continue

        key_tuple = _get_element_key(overlay_elem, keys)

        # Check for delete marker
        if overlay_elem.get(DELETE_MARKER):
            if key_tuple is not None and key_tuple in base_index:
                indices_to_delete.add(base_index[key_tuple])
            continue

        if key_tuple is not None and key_tuple in base_index:
            # Merge with existing element
            idx = base_index[key_tuple]
            result[idx] = _deep_merge_impl(result[idx], overlay_elem, path)
        else:
            # Append new element
            result.append(copy.deepcopy(overlay_elem))
            # Update index for newly added element
            if key_tuple is not None:
                base_index[key_tuple] = len(result) - 1

    # Remove deleted elements (in reverse order to maintain indices)
    for idx in sorted(indices_to_delete, reverse=True):
        del result[idx]

    return result


def _deep_merge_impl(
    base: Any,
    overlay: Any,
    path: str = ""
) -> Any:
    """
    Internal implementation of deep merge with path tracking.

    Args:
        base: Base value
        overlay: Overlay value
        path: Current path in the structure (e.g., "nodes.0.goto")

    Returns:
        Merged value
    """
    # None overlay returns None (null can override)
    if overlay is None:
        return None

    # None base with any overlay returns overlay copy
    if base is None:
        if isinstance(overlay, (dict, list)):
            return copy.deepcopy(overlay)
        return overlay

    # Both are lists: check for array merge by key
    if isinstance(base, list) and isinstance(overlay, list):
        # Determine array name from path
        array_name = path.split(".")[-1] if path else ""

        # Check if this array should merge by key
        if array_name in ARRAY_MERGE_KEYS:
            return merge_arrays_by_key(base, overlay, ARRAY_MERGE_KEYS[array_name], path)
        elif array_name == "goto":
            return merge_arrays_by_key(base, overlay, GOTO_MERGE_KEY, path)
        else:
            # Default: replace (existing YE.8 behavior for unknown arrays)
            return copy.deepcopy(overlay)

    # Type mismatch or scalars: overlay wins
    if not isinstance(base, dict) or not isinstance(overlay, dict):
        if isinstance(overlay, (dict, list)):
            return copy.deepcopy(overlay)
        return overlay

    # Both are dicts: recursive merge
    result = copy.deepcopy(base)

    for key, overlay_value in overlay.items():
        # Skip delete marker (handled at array level)
        if key == DELETE_MARKER:
            continue

        new_path = f"{path}.{key}" if path else key

        if key in result:
            base_value = result[key]
            # Recurse for any combination
            result[key] = _deep_merge_impl(base_value, overlay_value, new_path)
        else:
            # New key from overlay
            if isinstance(overlay_value, (dict, list)):
                result[key] = copy.deepcopy(overlay_value)
            else:
                result[key] = overlay_value

    return result


def deep_merge(
    base: Optional[Dict[str, Any]],
    overlay: Optional[Dict[str, Any]]
) -> Optional[Dict[str, Any]]:
    """
    Deep merge two dictionaries with kubectl-style semantics.

    Merge rules:
    - Objects (dicts) are recursively merged
    - Arrays with identifying keys are merged by key (YE.9):
      - nodes: merged by "name"
      - edges: merged by ("from", "to")
      - goto: merged by "to"
    - Arrays without keys are replaced, not concatenated
    - Scalars use last-wins (overlay overrides base)
    - Null/None values can override non-null values
    - Elements with __delete__: true are removed (YE.9)

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

        >>> # Array merge by key (YE.9) - nodes
        >>> base = {"nodes": [{"name": "a", "val": 1}, {"name": "b", "val": 2}]}
        >>> overlay = {"nodes": [{"name": "a", "val": 10}]}
        >>> deep_merge(base, overlay)
        {'nodes': [{'name': 'a', 'val': 10}, {'name': 'b', 'val': 2}]}

        >>> # Delete element (YE.9)
        >>> base = {"nodes": [{"name": "a"}, {"name": "b"}]}
        >>> overlay = {"nodes": [{"name": "a", "__delete__": True}]}
        >>> deep_merge(base, overlay)
        {'nodes': [{'name': 'b'}]}

        >>> # Unknown arrays still replace (YE.8 behavior)
        >>> deep_merge({"items": [1, 2, 3]}, {"items": [4, 5]})
        {'items': [4, 5]}

        >>> # Null override
        >>> deep_merge({"enabled": True}, {"enabled": None})
        {'enabled': None}

        >>> # Scalar last-wins
        >>> deep_merge({"count": 10}, {"count": 20})
        {'count': 20}
    """
    return _deep_merge_impl(base, overlay, "")


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
