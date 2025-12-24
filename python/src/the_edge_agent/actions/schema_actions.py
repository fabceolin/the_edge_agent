"""
Schema Actions for YAMLEngine.

This module provides schema manipulation actions:
- schema.merge: Deep merge multiple JSON Schemas with kubectl-style semantics

TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm

Example:
    >>> # In YAML workflow
    >>> - name: compose-schema
    >>>   action: schema.merge
    >>>   with:
    >>>     schemas:
    >>>       - path: ./schemas/base.json
    >>>       - uses: company/schemas@v1.0.0#overlay.json
    >>>       - inline:
    >>>           properties:
    >>>             custom_field:
    >>>               type: string
    >>>     validate: true
    >>>     output_key: extraction_schema
"""

import json
import os
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union

from the_edge_agent.schema.deep_merge import deep_merge, merge_all, validate_json_schema


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register schema actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def schema_merge(
        state,
        schemas: List[Dict[str, Any]],
        validate: bool = False,
        output_key: str = "merged_schema",
        **kwargs
    ) -> Dict[str, Any]:
        """
        Deep merge multiple JSON Schemas with kubectl-style semantics.

        Merge rules:
        - Objects are recursively merged
        - Arrays are replaced (not concatenated)
        - Scalars use last-wins
        - Null can override non-null

        Args:
            state: Current workflow state
            schemas: List of schema sources. Each item can be:
                - {"path": "./local/schema.json"} - Local file
                - {"uses": "owner/repo@ref#path"} - Git reference (requires schema_loader)
                - {"uses": "s3://bucket/path.json"} - fsspec URI
                - {"inline": {...}} - Inline schema object
            validate: Validate merged result against JSON Schema Draft 2020-12
            output_key: State key to store merged schema

        Returns:
            Dict with 'success', the output_key containing merged schema,
            and optionally 'validation' results.

        Example:
            >>> result = schema_merge(
            ...     state={},
            ...     schemas=[
            ...         {"path": "./base.json"},
            ...         {"inline": {"properties": {"extra": {"type": "string"}}}}
            ...     ],
            ...     validate=True
            ... )
            >>> print(result['success'])
            True
        """
        loaded_schemas = []
        errors = []

        for i, schema_spec in enumerate(schemas):
            try:
                schema = _load_schema_from_spec(schema_spec, engine)
                if schema is not None:
                    loaded_schemas.append(schema)
            except Exception as e:
                errors.append(f"Schema {i}: {str(e)}")

        if errors:
            return {
                "success": False,
                "error": "Failed to load schemas",
                "errors": errors,
            }

        if not loaded_schemas:
            return {
                "success": False,
                "error": "No schemas provided",
            }

        # Merge all schemas
        merged = merge_all(loaded_schemas)

        result = {
            "success": True,
            output_key: merged,
        }

        # Optional validation
        if validate:
            validation_result = validate_json_schema(merged)
            result["validation"] = validation_result
            if not validation_result["valid"]:
                result["success"] = False
                result["error"] = "Merged schema is not valid JSON Schema"

        return result

    def _load_schema_from_spec(
        spec: Dict[str, Any],
        engine: Any
    ) -> Optional[Dict[str, Any]]:
        """
        Load a schema from a specification dictionary.

        Args:
            spec: Schema specification with one of:
                - path: Local file path
                - uses: Git ref or fsspec URI
                - inline: Direct schema object
            engine: YAMLEngine instance

        Returns:
            Loaded schema dictionary

        Raises:
            ValueError: If spec format is invalid
            FileNotFoundError: If file not found
        """
        if "inline" in spec:
            return spec["inline"]

        if "path" in spec:
            path = Path(spec["path"])
            if not path.exists():
                raise FileNotFoundError(f"Schema file not found: {path}")

            content = path.read_text()
            if path.suffix in (".yaml", ".yml"):
                import yaml
                return yaml.safe_load(content)
            else:
                return json.loads(content)

        if "uses" in spec:
            # Delegate to schema_loader (Story 008.2)
            # Import here to avoid circular dependency
            try:
                from the_edge_agent.schema.schema_loader import fetch_schema
                return fetch_schema(spec["uses"])
            except ImportError:
                raise ValueError(
                    "Schema loading from Git/fsspec not available. "
                    "Use 'path' or 'inline' instead, or implement schema_loader."
                )

        raise ValueError(
            f"Invalid schema spec: must have 'path', 'uses', or 'inline' key. Got: {list(spec.keys())}"
        )

    # Register actions
    registry['schema.merge'] = schema_merge
    registry['actions.schema_merge'] = schema_merge


def schema_merge_cli(
    files: List[str],
    uses: Optional[List[str]] = None,
    output: Optional[str] = None,
    validate: bool = False,
    dry_run: bool = False,
) -> Dict[str, Any]:
    """
    CLI helper for schema merge command.

    Args:
        files: List of local file paths to merge
        uses: List of Git refs or fsspec URIs
        output: Output file path (None for stdout)
        validate: Validate merged result
        dry_run: Print to stdout without writing file

    Returns:
        Dict with merged schema and status
    """
    schemas = []

    # Load local files
    for file_path in files:
        path = Path(file_path)
        if not path.exists():
            return {
                "success": False,
                "error": f"File not found: {file_path}",
            }

        content = path.read_text()
        if path.suffix in (".yaml", ".yml"):
            import yaml
            schemas.append(yaml.safe_load(content))
        else:
            schemas.append(json.loads(content))

    # Load from uses refs
    if uses:
        try:
            from the_edge_agent.schema.schema_loader import fetch_schema
            for ref in uses:
                schemas.append(fetch_schema(ref))
        except ImportError:
            return {
                "success": False,
                "error": "Schema loading from Git/fsspec not available. Install git2 or fsspec.",
            }

    if not schemas:
        return {
            "success": False,
            "error": "No schemas provided",
        }

    # Merge
    merged = merge_all(schemas)

    result = {
        "success": True,
        "merged": merged,
    }

    # Validate
    if validate:
        validation = validate_json_schema(merged)
        result["validation"] = validation
        if not validation["valid"]:
            result["success"] = False

    # Output
    if not dry_run and output:
        output_path = Path(output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(json.dumps(merged, indent=2))
        result["output_file"] = str(output_path)

    return result
