"""
Data Processing Actions for YAMLEngine (TEA-BUILTIN-003.2).

This module provides data parsing, transformation, and validation actions
for YAMLEngine workflows. Supports JSON, CSV, and generic data operations.

Actions:
    - json.parse: Parse JSON strings to Python objects
    - json.transform: Transform data with JMESPath/JSONPath expressions
    - json.stringify: Convert Python objects to JSON strings
    - csv.parse: Parse CSV from text or file
    - csv.stringify: Convert lists to CSV strings
    - data.validate: Validate data against JSON Schema
    - data.merge: Merge multiple dicts with various strategies
    - data.filter: Filter lists with predicate expressions

Example:
    >>> # JSON parsing
    >>> result = registry['json.parse'](state={}, text='{"name": "Alice"}')
    >>> print(result['data'])  # {'name': 'Alice'}

    >>> # JMESPath transformation
    >>> result = registry['json.transform'](
    ...     state={},
    ...     data={"users": [{"name": "Alice"}, {"name": "Bob"}]},
    ...     expression="users[].name"
    ... )
    >>> print(result['result'])  # ['Alice', 'Bob']

    >>> # Data validation
    >>> result = registry['data.validate'](
    ...     state={},
    ...     data={"age": 25},
    ...     schema={"type": "object", "properties": {"age": {"type": "integer"}}}
    ... )
    >>> print(result['valid'])  # True
"""

import copy
import csv
import io
import json
import re
from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register data processing actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # JSON Actions
    def json_parse(state, text, strict=True, default=None, **kwargs):
        """
        Parse a JSON string into a Python object.

        Args:
            state: Current state dictionary
            text: JSON string to parse
            strict: If True, use standard JSON parsing. If False, allow
                   trailing commas and comments (best effort)
            default: Default value to return on parse error (only used
                    when strict=False)

        Returns:
            {"data": any, "success": True} on success
            {"error": str, "success": False, "error_type": "parse"} on failure
        """
        if text is None:
            return {
                "success": False,
                "error": "Input text is None",
                "error_type": "parse"
            }

        try:
            # Standard JSON parsing
            data = json.loads(text)
            return {"data": data, "success": True}
        except json.JSONDecodeError as e:
            if not strict and default is not None:
                return {"data": default, "success": True}

            # Try non-strict parsing if requested
            if not strict:
                try:
                    # Remove single-line comments
                    cleaned = re.sub(r'//.*$', '', text, flags=re.MULTILINE)
                    # Remove multi-line comments
                    cleaned = re.sub(r'/\*.*?\*/', '', cleaned, flags=re.DOTALL)
                    # Remove trailing commas before } or ]
                    cleaned = re.sub(r',\s*([}\]])', r'\1', cleaned)
                    data = json.loads(cleaned)
                    return {"data": data, "success": True}
                except json.JSONDecodeError:
                    pass  # Fall through to error

            return {
                "success": False,
                "error": f"JSON parse error: {e.msg}",
                "error_type": "parse",
                "position": {"line": e.lineno, "column": e.colno}
            }

    registry['json.parse'] = json_parse
    registry['json_parse'] = json_parse

    def json_transform(state, data, expression, engine_type="jmespath", **kwargs):
        """
        Transform data using JMESPath or JSONPath expressions.

        Args:
            state: Current state dictionary
            data: Data to transform (dict or list)
            expression: JMESPath or JSONPath expression string
            engine_type: "jmespath" (default) or "jsonpath"

        Returns:
            {"result": any, "expression": str, "success": True} on success
            {"error": str, "success": False, "error_type": "transform"} on failure

        Examples:
            expression: "user.name" -> extracts nested value
            expression: "[?status=='active'].name" -> filters array
            expression: "{names: [].name, count: length(@)}" -> projects fields
        """
        if data is None:
            return {
                "success": False,
                "error": "Input data is None",
                "error_type": "transform"
            }

        if not expression:
            return {
                "success": False,
                "error": "Expression is required",
                "error_type": "transform"
            }

        # Handle the 'engine' kwarg that might be passed from YAML
        # (we renamed to engine_type to avoid conflict with the engine parameter)
        actual_engine = kwargs.get('engine', engine_type)

        try:
            if actual_engine == "jmespath":
                try:
                    import jmespath
                except ImportError:
                    return {
                        "success": False,
                        "error": "jmespath library not installed. Install with: pip install jmespath",
                        "error_type": "transform"
                    }

                try:
                    result = jmespath.search(expression, data)
                    return {
                        "result": result,
                        "expression": expression,
                        "success": True
                    }
                except jmespath.exceptions.JMESPathError as e:
                    return {
                        "success": False,
                        "error": f"JMESPath error: {str(e)}",
                        "error_type": "transform",
                        "expression": expression
                    }

            elif actual_engine == "jsonpath":
                try:
                    from jsonpath_ng import parse as jsonpath_parse
                except ImportError:
                    return {
                        "success": False,
                        "error": "jsonpath-ng library not installed. Install with: pip install jsonpath-ng",
                        "error_type": "transform"
                    }

                try:
                    jsonpath_expr = jsonpath_parse(expression)
                    matches = jsonpath_expr.find(data)
                    result = [match.value for match in matches]
                    # Return single value if only one match
                    if len(result) == 1:
                        result = result[0]
                    return {
                        "result": result,
                        "expression": expression,
                        "success": True
                    }
                except Exception as e:
                    return {
                        "success": False,
                        "error": f"JSONPath error: {str(e)}",
                        "error_type": "transform",
                        "expression": expression
                    }

            else:
                return {
                    "success": False,
                    "error": f"Unknown engine: {actual_engine}. Use 'jmespath' or 'jsonpath'",
                    "error_type": "transform"
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Transform error: {str(e)}",
                "error_type": "transform"
            }

    registry['json.transform'] = json_transform
    registry['json_transform'] = json_transform

    def json_stringify(state, data, indent=None, sort_keys=False, **kwargs):
        """
        Convert a Python object to a JSON string.

        Args:
            state: Current state dictionary
            data: Python object to serialize
            indent: Indentation level for pretty printing (None for compact)
            sort_keys: Sort dictionary keys alphabetically

        Returns:
            {"text": str, "success": True} on success
            {"error": str, "success": False, "error_type": "serialize"} on failure
        """
        try:
            text = json.dumps(data, indent=indent, sort_keys=sort_keys, default=str)
            return {"text": text, "success": True}
        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"JSON serialization error: {str(e)}",
                "error_type": "serialize"
            }

    registry['json.stringify'] = json_stringify
    registry['json_stringify'] = json_stringify

    # CSV Actions
    def csv_parse(state, text=None, path=None, delimiter=",", has_header=True, **kwargs):
        """
        Parse CSV data from text or file.

        Args:
            state: Current state dictionary
            text: CSV string to parse (mutually exclusive with path)
            path: File path to read CSV from (mutually exclusive with text)
            delimiter: Field delimiter character (default: ",")
            has_header: If True, first row is treated as headers

        Returns:
            {
                "data": List[dict] | List[List],
                "headers": Optional[List[str]],
                "row_count": int,
                "success": True
            }
            Or {"error": str, "success": False, "error_type": "parse"|"io"} on failure
        """
        if text is None and path is None:
            return {
                "success": False,
                "error": "Either 'text' or 'path' must be provided",
                "error_type": "parse"
            }

        if text is not None and path is not None:
            return {
                "success": False,
                "error": "Only one of 'text' or 'path' should be provided, not both",
                "error_type": "parse"
            }

        try:
            # Read from file if path provided
            if path is not None:
                try:
                    with open(path, 'r', newline='', encoding='utf-8') as f:
                        text = f.read()
                except FileNotFoundError:
                    return {
                        "success": False,
                        "error": f"File not found: {path}",
                        "error_type": "io"
                    }
                except IOError as e:
                    return {
                        "success": False,
                        "error": f"IO error reading file: {str(e)}",
                        "error_type": "io"
                    }

            # Parse CSV
            reader = csv.reader(io.StringIO(text), delimiter=delimiter)
            rows = list(reader)

            if not rows:
                return {
                    "data": [],
                    "headers": None,
                    "row_count": 0,
                    "success": True
                }

            if has_header:
                headers = rows[0]
                data_rows = rows[1:]
                # Convert to list of dicts
                data = []
                for i, row in enumerate(data_rows):
                    if len(row) != len(headers):
                        # Handle malformed rows gracefully
                        row_dict = {}
                        for j, header in enumerate(headers):
                            row_dict[header] = row[j] if j < len(row) else None
                        data.append(row_dict)
                    else:
                        data.append(dict(zip(headers, row)))
                return {
                    "data": data,
                    "headers": headers,
                    "row_count": len(data),
                    "success": True
                }
            else:
                return {
                    "data": rows,
                    "headers": None,
                    "row_count": len(rows),
                    "success": True
                }

        except csv.Error as e:
            return {
                "success": False,
                "error": f"CSV parse error: {str(e)}",
                "error_type": "parse"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "parse"
            }

    registry['csv.parse'] = csv_parse
    registry['csv_parse'] = csv_parse

    def csv_stringify(state, data, headers=None, delimiter=",", **kwargs):
        """
        Convert a list of dicts or list of lists to a CSV string.

        Args:
            state: Current state dictionary
            data: List of dicts or list of lists to convert
            headers: Column headers (auto-detected from dict keys if not provided)
            delimiter: Field delimiter character (default: ",")

        Returns:
            {"text": str, "row_count": int, "success": True} on success
            {"error": str, "success": False, "error_type": "serialize"} on failure
        """
        if data is None:
            return {
                "success": False,
                "error": "Input data is None",
                "error_type": "serialize"
            }

        if not isinstance(data, list):
            return {
                "success": False,
                "error": "Data must be a list",
                "error_type": "serialize"
            }

        if not data:
            return {
                "text": "",
                "row_count": 0,
                "success": True
            }

        try:
            output = io.StringIO()

            # Determine if data is list of dicts or list of lists
            first_item = data[0]
            is_dict_list = isinstance(first_item, dict)

            if is_dict_list:
                # Auto-detect headers from dict keys if not provided
                if headers is None:
                    headers = list(first_item.keys())

                writer = csv.DictWriter(output, fieldnames=headers, delimiter=delimiter)
                writer.writeheader()
                for row in data:
                    writer.writerow(row)
            else:
                writer = csv.writer(output, delimiter=delimiter)
                # Write headers if provided
                if headers:
                    writer.writerow(headers)
                for row in data:
                    writer.writerow(row)

            text = output.getvalue()
            row_count = len(data)

            return {
                "text": text,
                "row_count": row_count,
                "success": True
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"CSV serialization error: {str(e)}",
                "error_type": "serialize"
            }

    registry['csv.stringify'] = csv_stringify
    registry['csv_stringify'] = csv_stringify

    # Data Actions
    def data_validate(state, data, schema, **kwargs):
        """
        Validate data against a JSON Schema.

        Args:
            state: Current state dictionary
            data: Data to validate
            schema: JSON Schema to validate against

        Returns:
            {
                "valid": bool,
                "errors": List[{"path": str, "message": str}],
                "success": True
            }
            Or {"error": str, "success": False, "error_type": "validate"} on failure
        """
        if data is None:
            return {
                "valid": False,
                "errors": [{"path": "", "message": "Data is None"}],
                "success": True
            }

        if schema is None:
            return {
                "success": False,
                "error": "Schema is required",
                "error_type": "validate"
            }

        try:
            import jsonschema
            from jsonschema import Draft7Validator
        except ImportError:
            return {
                "success": False,
                "error": "jsonschema library not installed. Install with: pip install jsonschema",
                "error_type": "validate"
            }

        try:
            validator = Draft7Validator(schema)
            errors = []

            for error in validator.iter_errors(data):
                path = ".".join(str(p) for p in error.absolute_path) if error.absolute_path else ""
                errors.append({
                    "path": path,
                    "message": error.message
                })

            return {
                "valid": len(errors) == 0,
                "errors": errors,
                "success": True
            }

        except jsonschema.SchemaError as e:
            return {
                "success": False,
                "error": f"Invalid schema: {str(e)}",
                "error_type": "validate"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Validation error: {str(e)}",
                "error_type": "validate"
            }

    registry['data.validate'] = data_validate
    registry['data_validate'] = data_validate

    def data_merge(state, sources, strategy="deep", **kwargs):
        """
        Merge multiple dictionaries/objects.

        Args:
            state: Current state dictionary
            sources: List of dictionaries to merge
            strategy: Merge strategy - "shallow", "deep", or "replace"
                - shallow: Only merge top-level keys
                - deep: Recursively merge nested dictionaries
                - replace: Later sources completely replace earlier ones

        Returns:
            {"result": dict, "source_count": int, "success": True} on success
            {"error": str, "success": False, "error_type": "merge"} on failure
        """
        if sources is None:
            return {
                "success": False,
                "error": "Sources is required",
                "error_type": "merge"
            }

        if not isinstance(sources, list):
            return {
                "success": False,
                "error": "Sources must be a list",
                "error_type": "merge"
            }

        if not sources:
            return {
                "result": {},
                "source_count": 0,
                "success": True
            }

        if strategy not in ("shallow", "deep", "replace"):
            return {
                "success": False,
                "error": f"Unknown strategy: {strategy}. Use 'shallow', 'deep', or 'replace'",
                "error_type": "merge"
            }

        def deep_merge(base, override):
            """Recursively merge override into base."""
            result = copy.deepcopy(base)
            for key, value in override.items():
                if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                    result[key] = deep_merge(result[key], value)
                else:
                    result[key] = copy.deepcopy(value)
            return result

        try:
            result = {}

            for source in sources:
                if source is None:
                    continue
                if not isinstance(source, dict):
                    return {
                        "success": False,
                        "error": f"All sources must be dictionaries, got {type(source).__name__}",
                        "error_type": "merge"
                    }

                if strategy == "replace":
                    result = copy.deepcopy(source)
                elif strategy == "shallow":
                    result.update(source)
                elif strategy == "deep":
                    result = deep_merge(result, source)

            return {
                "result": result,
                "source_count": len(sources),
                "success": True
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Merge error: {str(e)}",
                "error_type": "merge"
            }

    registry['data.merge'] = data_merge
    registry['data_merge'] = data_merge

    def data_filter(state, data, predicate, **kwargs):
        """
        Filter list items using predicate expressions.

        Args:
            state: Current state dictionary
            data: List to filter
            predicate: Single predicate dict or list of predicates (AND logic)
                Each predicate: {"field": str, "op": str, "value": any}
                Supported ops: eq, ne, gt, gte, lt, lte, in, not_in, contains, startswith, endswith

        Returns:
            {
                "result": List,
                "original_count": int,
                "filtered_count": int,
                "success": True
            }
            Or {"error": str, "success": False, "error_type": "filter"} on failure

        Examples:
            predicate: {"field": "status", "op": "eq", "value": "active"}
            predicate: [
                {"field": "status", "op": "eq", "value": "active"},
                {"field": "role", "op": "in", "value": ["admin", "moderator"]}
            ]
        """
        if data is None:
            return {
                "success": False,
                "error": "Data is required",
                "error_type": "filter"
            }

        if not isinstance(data, list):
            return {
                "success": False,
                "error": "Data must be a list",
                "error_type": "filter"
            }

        if predicate is None:
            return {
                "success": False,
                "error": "Predicate is required",
                "error_type": "filter"
            }

        # Normalize predicate to list
        predicates = predicate if isinstance(predicate, list) else [predicate]

        def get_nested_value(obj, field):
            """Get value from nested dict using dot notation."""
            keys = field.split(".")
            value = obj
            for key in keys:
                if isinstance(value, dict):
                    value = value.get(key)
                else:
                    return None
            return value

        def evaluate_predicate(item, pred):
            """Evaluate a single predicate against an item."""
            field = pred.get("field")
            op = pred.get("op", "eq")
            expected = pred.get("value")

            if field is None:
                return True  # No field = always match

            actual = get_nested_value(item, field) if isinstance(item, dict) else None

            try:
                if op == "eq":
                    return actual == expected
                elif op == "ne":
                    return actual != expected
                elif op == "gt":
                    return actual is not None and actual > expected
                elif op == "gte":
                    return actual is not None and actual >= expected
                elif op == "lt":
                    return actual is not None and actual < expected
                elif op == "lte":
                    return actual is not None and actual <= expected
                elif op == "in":
                    return actual in expected if expected else False
                elif op == "not_in":
                    return actual not in expected if expected else True
                elif op == "contains":
                    return expected in actual if actual else False
                elif op == "startswith":
                    return actual.startswith(expected) if isinstance(actual, str) else False
                elif op == "endswith":
                    return actual.endswith(expected) if isinstance(actual, str) else False
                else:
                    return False  # Unknown op
            except (TypeError, AttributeError):
                return False

        try:
            original_count = len(data)
            result = []

            for item in data:
                # All predicates must match (AND logic)
                if all(evaluate_predicate(item, p) for p in predicates):
                    result.append(item)

            return {
                "result": result,
                "original_count": original_count,
                "filtered_count": len(result),
                "success": True
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Filter error: {str(e)}",
                "error_type": "filter"
            }

    registry['data.filter'] = data_filter
    registry['data_filter'] = data_filter
