"""
Tests for Schema Deep Merge Algorithm.

TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm

This module tests the kubectl-style deep merge semantics:
- Objects are recursively merged
- Arrays are replaced (not concatenated)
- Scalars use last-wins
- Null can override non-null
"""

import json
import pytest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch, MagicMock

from the_edge_agent.schema.deep_merge import deep_merge, merge_all, validate_json_schema


class TestDeepMerge:
    """Test deep_merge function with kubectl-style semantics."""

    def test_merge_flat_objects(self):
        """Test merging flat objects - overlay wins for conflicts."""
        base = {"a": 1, "b": 2}
        overlay = {"b": 3, "c": 4}
        result = deep_merge(base, overlay)
        assert result == {"a": 1, "b": 3, "c": 4}

    def test_merge_nested_objects(self):
        """Test recursive merge of nested objects."""
        base = {"a": 1, "b": {"x": 10, "y": 20}}
        overlay = {"b": {"y": 30, "z": 40}, "c": 3}
        result = deep_merge(base, overlay)
        assert result == {"a": 1, "b": {"x": 10, "y": 30, "z": 40}, "c": 3}

    def test_array_replacement_not_concatenation(self):
        """Arrays are replaced, not concatenated (kubectl behavior)."""
        base = {"items": [1, 2, 3]}
        overlay = {"items": [4, 5]}
        result = deep_merge(base, overlay)
        assert result == {"items": [4, 5]}

    def test_null_override(self):
        """Null/None can override non-null values."""
        base = {"enabled": True}
        overlay = {"enabled": None}
        result = deep_merge(base, overlay)
        assert result == {"enabled": None}

    def test_scalar_last_wins(self):
        """Scalar values use last-wins semantics."""
        base = {"count": 10, "name": "original"}
        overlay = {"count": 20}
        result = deep_merge(base, overlay)
        assert result == {"count": 20, "name": "original"}

    def test_type_mismatch_overlay_wins(self):
        """When types don't match, overlay wins."""
        base = {"value": {"nested": "object"}}
        overlay = {"value": "string"}
        result = deep_merge(base, overlay)
        assert result == {"value": "string"}

    def test_overlay_object_replaces_scalar(self):
        """Object in overlay replaces scalar in base."""
        base = {"value": "string"}
        overlay = {"value": {"nested": "object"}}
        result = deep_merge(base, overlay)
        assert result == {"value": {"nested": "object"}}

    def test_deep_nested_merge(self):
        """Test deeply nested object merge."""
        base = {
            "level1": {
                "level2": {
                    "level3": {"a": 1, "b": 2}
                }
            }
        }
        overlay = {
            "level1": {
                "level2": {
                    "level3": {"b": 3, "c": 4}
                }
            }
        }
        result = deep_merge(base, overlay)
        assert result == {
            "level1": {
                "level2": {
                    "level3": {"a": 1, "b": 3, "c": 4}
                }
            }
        }

    def test_none_overlay_returns_none(self):
        """None overlay returns None."""
        base = {"a": 1}
        result = deep_merge(base, None)
        assert result is None

    def test_none_base_with_dict_overlay(self):
        """None base with dict overlay returns overlay copy."""
        overlay = {"a": 1}
        result = deep_merge(None, overlay)
        assert result == {"a": 1}
        # Verify it's a copy
        overlay["a"] = 2
        assert result["a"] == 1

    def test_empty_overlay(self):
        """Empty overlay preserves base."""
        base = {"a": 1, "b": 2}
        overlay = {}
        result = deep_merge(base, overlay)
        assert result == {"a": 1, "b": 2}

    def test_empty_base(self):
        """Empty base gets overlay values."""
        base = {}
        overlay = {"a": 1, "b": 2}
        result = deep_merge(base, overlay)
        assert result == {"a": 1, "b": 2}

    def test_json_schema_merge(self):
        """Test merging JSON Schema documents."""
        base_schema = {
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "email": {"type": "string"}
            },
            "required": ["name"]
        }
        overlay_schema = {
            "properties": {
                "email": {"type": "string", "format": "email"},
                "phone": {"type": "string"}
            },
            "required": ["name", "email"]
        }
        result = deep_merge(base_schema, overlay_schema)

        assert result["$schema"] == "https://json-schema.org/draft/2020-12/schema"
        assert result["type"] == "object"
        assert result["properties"]["name"] == {"type": "string"}
        assert result["properties"]["email"] == {"type": "string", "format": "email"}
        assert result["properties"]["phone"] == {"type": "string"}
        # Arrays are replaced
        assert result["required"] == ["name", "email"]

    def test_immutability_base_unchanged(self):
        """Verify base is not mutated."""
        base = {"a": {"b": 1}}
        original_base = {"a": {"b": 1}}
        overlay = {"a": {"c": 2}}
        deep_merge(base, overlay)
        assert base == original_base

    def test_immutability_overlay_unchanged(self):
        """Verify overlay is not mutated."""
        base = {"a": {"b": 1}}
        overlay = {"a": {"c": 2}}
        original_overlay = {"a": {"c": 2}}
        deep_merge(base, overlay)
        assert overlay == original_overlay


class TestMergeAll:
    """Test merge_all function for multiple schemas."""

    def test_merge_multiple_schemas(self):
        """Test merging list of schemas in order."""
        schemas = [
            {"a": 1, "b": 2},
            {"b": 3, "c": 4},
            {"c": 5, "d": 6}
        ]
        result = merge_all(schemas)
        assert result == {"a": 1, "b": 3, "c": 5, "d": 6}

    def test_merge_empty_list(self):
        """Empty list returns empty dict."""
        result = merge_all([])
        assert result == {}

    def test_merge_single_schema(self):
        """Single schema returns copy of that schema."""
        schema = {"a": 1, "b": {"x": 10}}
        result = merge_all([schema])
        assert result == schema
        # Verify it's a copy
        schema["a"] = 2
        assert result["a"] == 1

    def test_merge_with_nested_objects(self):
        """Test merging multiple schemas with nested objects."""
        schemas = [
            {"properties": {"a": {"type": "string"}}},
            {"properties": {"b": {"type": "number"}}},
            {"properties": {"a": {"type": "string", "maxLength": 100}}}
        ]
        result = merge_all(schemas)
        assert result["properties"]["a"] == {"type": "string", "maxLength": 100}
        assert result["properties"]["b"] == {"type": "number"}

    def test_merge_all_nones(self):
        """List of all Nones returns empty dict."""
        result = merge_all([None, None, None])
        assert result == {}

    def test_merge_mixed_none_and_schemas(self):
        """None values in list are skipped."""
        schemas = [{"a": 1}, None, {"b": 2}]
        result = merge_all(schemas)
        assert result == {"a": 1, "b": 2}


class TestValidateJsonSchema:
    """Test JSON Schema validation."""

    def test_valid_schema(self):
        """Test validation of a valid JSON Schema."""
        schema = {
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "type": "object",
            "properties": {
                "name": {"type": "string"}
            }
        }
        result = validate_json_schema(schema)
        assert result["valid"] is True
        assert result["errors"] == []

    def test_invalid_schema_bad_type(self):
        """Test validation catches invalid type."""
        schema = {
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "type": "not_a_valid_type"
        }
        result = validate_json_schema(schema)
        assert result["valid"] is False
        assert len(result["errors"]) > 0


class TestPropertyBased:
    """Property-based tests for merge invariants."""

    def test_merge_with_self_is_identity(self):
        """Merging a dict with itself returns equivalent dict."""
        d = {"a": 1, "b": {"x": 10, "y": 20}, "c": [1, 2, 3]}
        result = deep_merge(d, d)
        assert result == d

    def test_overlay_keys_present(self):
        """All overlay keys should be in result."""
        base = {"a": 1, "b": 2}
        overlay = {"c": 3, "d": 4}
        result = deep_merge(base, overlay)
        for key in overlay:
            assert key in result
            assert result[key] == overlay[key]

    def test_base_keys_preserved_unless_overridden(self):
        """Base keys not in overlay are preserved."""
        base = {"a": 1, "b": 2, "c": 3}
        overlay = {"b": 20}
        result = deep_merge(base, overlay)
        assert result["a"] == 1
        assert result["c"] == 3

    def test_merge_associativity(self):
        """Test that merge order matters (last wins)."""
        a = {"x": 1}
        b = {"x": 2}
        c = {"x": 3}
        # merge_all applies left to right, so c wins
        result = merge_all([a, b, c])
        assert result["x"] == 3


class TestSchemaAction:
    """Test schema.merge action integration."""

    def test_schema_merge_action_inline(self):
        """Test schema.merge with inline schemas."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["schema.merge"](
            state={},
            schemas=[
                {"inline": {"a": 1, "b": {"x": 10}}},
                {"inline": {"b": {"y": 20}, "c": 3}}
            ]
        )

        assert result["success"] is True
        assert result["merged_schema"] == {"a": 1, "b": {"x": 10, "y": 20}, "c": 3}

    def test_schema_merge_action_local_file(self):
        """Test schema.merge with local file."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        with TemporaryDirectory() as tmpdir:
            schema_path = Path(tmpdir) / "base.json"
            schema_path.write_text(json.dumps({"a": 1, "b": 2}))

            result = registry["schema.merge"](
                state={},
                schemas=[
                    {"path": str(schema_path)},
                    {"inline": {"b": 3, "c": 4}}
                ]
            )

            assert result["success"] is True
            assert result["merged_schema"] == {"a": 1, "b": 3, "c": 4}

    def test_schema_merge_action_yaml_file(self):
        """Test schema.merge with YAML file."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        with TemporaryDirectory() as tmpdir:
            schema_path = Path(tmpdir) / "base.yaml"
            schema_path.write_text("a: 1\nb: 2\n")

            result = registry["schema.merge"](
                state={},
                schemas=[
                    {"path": str(schema_path)},
                    {"inline": {"c": 3}}
                ]
            )

            assert result["success"] is True
            assert result["merged_schema"]["a"] == 1
            assert result["merged_schema"]["c"] == 3

    def test_schema_merge_action_file_not_found(self):
        """Test schema.merge error when file not found."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["schema.merge"](
            state={},
            schemas=[
                {"path": "/nonexistent/schema.json"}
            ]
        )

        assert result["success"] is False
        assert "errors" in result

    def test_schema_merge_action_with_validation(self):
        """Test schema.merge with validation enabled."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["schema.merge"](
            state={},
            schemas=[
                {"inline": {
                    "$schema": "https://json-schema.org/draft/2020-12/schema",
                    "type": "object",
                    "properties": {"name": {"type": "string"}}
                }}
            ],
            validate=True
        )

        assert result["success"] is True
        assert "validation" in result
        assert result["validation"]["valid"] is True

    def test_schema_merge_action_custom_output_key(self):
        """Test schema.merge with custom output key."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["schema.merge"](
            state={},
            schemas=[{"inline": {"a": 1}}],
            output_key="my_schema"
        )

        assert result["success"] is True
        assert "my_schema" in result
        assert result["my_schema"] == {"a": 1}

    def test_schema_merge_action_empty_schemas(self):
        """Test schema.merge with empty schemas list."""
        from the_edge_agent.actions.schema_actions import register_actions

        registry = {}
        register_actions(registry, engine=None)

        result = registry["schema.merge"](
            state={},
            schemas=[]
        )

        assert result["success"] is False
        assert "No schemas provided" in result["error"]


class TestSchemaMergeCli:
    """Test CLI helper function."""

    def test_cli_merge_local_files(self):
        """Test CLI merge with local files."""
        from the_edge_agent.actions.schema_actions import schema_merge_cli

        with TemporaryDirectory() as tmpdir:
            base_path = Path(tmpdir) / "base.json"
            overlay_path = Path(tmpdir) / "overlay.json"
            base_path.write_text(json.dumps({"a": 1, "b": 2}))
            overlay_path.write_text(json.dumps({"b": 3, "c": 4}))

            result = schema_merge_cli(
                files=[str(base_path), str(overlay_path)],
                dry_run=True
            )

            assert result["success"] is True
            assert result["merged"] == {"a": 1, "b": 3, "c": 4}

    def test_cli_merge_with_output(self):
        """Test CLI merge with output file."""
        from the_edge_agent.actions.schema_actions import schema_merge_cli

        with TemporaryDirectory() as tmpdir:
            base_path = Path(tmpdir) / "base.json"
            output_path = Path(tmpdir) / "merged.json"
            base_path.write_text(json.dumps({"a": 1}))

            result = schema_merge_cli(
                files=[str(base_path)],
                output=str(output_path)
            )

            assert result["success"] is True
            assert output_path.exists()
            assert json.loads(output_path.read_text()) == {"a": 1}

    def test_cli_merge_with_validation(self):
        """Test CLI merge with validation."""
        from the_edge_agent.actions.schema_actions import schema_merge_cli

        with TemporaryDirectory() as tmpdir:
            schema_path = Path(tmpdir) / "schema.json"
            schema_path.write_text(json.dumps({
                "$schema": "https://json-schema.org/draft/2020-12/schema",
                "type": "object"
            }))

            result = schema_merge_cli(
                files=[str(schema_path)],
                validate=True,
                dry_run=True
            )

            assert result["success"] is True
            assert "validation" in result
            assert result["validation"]["valid"] is True

    def test_cli_file_not_found(self):
        """Test CLI error when file not found."""
        from the_edge_agent.actions.schema_actions import schema_merge_cli

        result = schema_merge_cli(files=["/nonexistent/file.json"])

        assert result["success"] is False
        assert "File not found" in result["error"]
