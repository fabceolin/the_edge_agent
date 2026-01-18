"""
Tests for Schema Deep Merge Algorithm.

TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm
YE.9: Deep Array Merge by Identifying Key

This module tests the kubectl-style deep merge semantics:
- Objects are recursively merged
- Arrays with identifying keys are merged by key (YE.9):
  - nodes: merged by "name"
  - edges: merged by ("from", "to")
  - goto: merged by "to"
- Arrays without keys are replaced (not concatenated)
- Scalars use last-wins
- Null can override non-null
- __delete__: true marker removes elements (YE.9)
"""

import json
import pytest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch, MagicMock

from the_edge_agent.schema.deep_merge import (
    deep_merge,
    merge_all,
    merge_arrays_by_key,
    validate_json_schema,
    ARRAY_MERGE_KEYS,
    GOTO_MERGE_KEY,
    DELETE_MARKER,
)


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


class TestArrayMergeByKey:
    """YE.9: Test deep array merge by identifying key."""

    # === AC 1: nodes array merges by name key ===

    def test_nodes_merge_by_name_single_modification(self):
        """AC 1: Single node modification preserves other nodes."""
        base = {
            "nodes": [
                {"name": "fetch", "uses": "http.get"},
                {"name": "process", "uses": "llm.call", "with": {"model": "gpt-4"}},
                {"name": "save", "uses": "memory.store"}
            ]
        }
        overlay = {
            "nodes": [
                {"name": "process", "with": {"model": "claude-3"}}
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 3
        # Find process node
        process_node = next(n for n in result["nodes"] if n["name"] == "process")
        assert process_node["uses"] == "llm.call"  # Preserved from base
        assert process_node["with"]["model"] == "claude-3"  # Updated from overlay

    def test_nodes_merge_by_name_multiple_modifications(self):
        """AC 1: Multiple nodes can be modified in single overlay."""
        base = {
            "nodes": [
                {"name": "a", "val": 1},
                {"name": "b", "val": 2},
                {"name": "c", "val": 3}
            ]
        }
        overlay = {
            "nodes": [
                {"name": "a", "val": 10},
                {"name": "c", "val": 30}
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 3
        assert result["nodes"][0]["val"] == 10  # Updated
        assert result["nodes"][1]["val"] == 2   # Preserved
        assert result["nodes"][2]["val"] == 30  # Updated

    def test_nodes_preserve_order(self):
        """AC 1: Node order from base is preserved."""
        base = {"nodes": [{"name": "a"}, {"name": "b"}, {"name": "c"}]}
        overlay = {"nodes": [{"name": "b", "extra": True}]}
        result = deep_merge(base, overlay)

        assert [n["name"] for n in result["nodes"]] == ["a", "b", "c"]

    # === AC 2: edges array merges by from+to composite key ===

    def test_edges_merge_by_from_to(self):
        """AC 2: Edges merge by composite from+to key."""
        base = {
            "edges": [
                {"from": "__start__", "to": "fetch"},
                {"from": "fetch", "to": "process", "timeout": 30},
                {"from": "process", "to": "__end__"}
            ]
        }
        overlay = {
            "edges": [
                {"from": "fetch", "to": "process", "timeout": 60, "retry": 3}
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["edges"]) == 3
        edge = next(e for e in result["edges"] if e["from"] == "fetch")
        assert edge["timeout"] == 60  # Updated
        assert edge["retry"] == 3     # Added

    def test_edges_composite_key_all_parts_must_match(self):
        """AC 2: Both from and to must match for edge merge."""
        base = {
            "edges": [
                {"from": "a", "to": "b", "weight": 1},
                {"from": "a", "to": "c", "weight": 2}
            ]
        }
        overlay = {
            "edges": [
                {"from": "a", "to": "b", "weight": 10}  # Matches first edge
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["edges"]) == 2
        ab_edge = next(e for e in result["edges"] if e["to"] == "b")
        ac_edge = next(e for e in result["edges"] if e["to"] == "c")
        assert ab_edge["weight"] == 10  # Updated
        assert ac_edge["weight"] == 2   # Unchanged

    # === AC 3: goto arrays merge by to key ===

    def test_goto_merge_by_to_key(self):
        """AC 3: goto arrays inside nodes merge by 'to' key."""
        base = {
            "nodes": [{
                "name": "router",
                "goto": [
                    {"if": "score > 0.9", "to": "high"},
                    {"if": "score > 0.5", "to": "medium"},
                    {"to": "low"}
                ]
            }]
        }
        overlay = {
            "nodes": [{
                "name": "router",
                "goto": [
                    {"if": "score > 0.95", "to": "high"}  # Update condition
                ]
            }]
        }
        result = deep_merge(base, overlay)

        goto = result["nodes"][0]["goto"]
        assert len(goto) == 3
        high_route = next(g for g in goto if g["to"] == "high")
        assert high_route["if"] == "score > 0.95"  # Updated

    def test_goto_add_new_route(self):
        """AC 3: New goto destinations are appended."""
        base = {
            "nodes": [{
                "name": "router",
                "goto": [{"to": "default"}]
            }]
        }
        overlay = {
            "nodes": [{
                "name": "router",
                "goto": [{"if": "special", "to": "special_handler"}]
            }]
        }
        result = deep_merge(base, overlay)

        goto = result["nodes"][0]["goto"]
        assert len(goto) == 2
        assert any(g["to"] == "default" for g in goto)
        assert any(g["to"] == "special_handler" for g in goto)

    # === AC 4: Matched elements use recursive deep merge ===

    def test_nested_deep_merge_in_matched_element(self):
        """AC 4: Nested objects in matched elements are deep-merged."""
        base = {
            "nodes": [{
                "name": "llm",
                "with": {
                    "model": "gpt-4",
                    "temperature": 0.7,
                    "params": {"max_tokens": 1000, "top_p": 0.9}
                }
            }]
        }
        overlay = {
            "nodes": [{
                "name": "llm",
                "with": {
                    "model": "claude-3",
                    "params": {"max_tokens": 2000}
                }
            }]
        }
        result = deep_merge(base, overlay)

        node = result["nodes"][0]
        assert node["with"]["model"] == "claude-3"  # Updated
        assert node["with"]["temperature"] == 0.7   # Preserved
        assert node["with"]["params"]["max_tokens"] == 2000  # Updated
        assert node["with"]["params"]["top_p"] == 0.9  # Preserved

    # === AC 5: Non-matching overlay elements are appended ===

    def test_non_matching_nodes_appended(self):
        """AC 5: New nodes not in base are appended."""
        base = {"nodes": [{"name": "a", "val": 1}]}
        overlay = {"nodes": [{"name": "b", "val": 2}]}
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 2
        assert result["nodes"][0]["name"] == "a"
        assert result["nodes"][1]["name"] == "b"

    def test_non_matching_edges_appended(self):
        """AC 5: New edges not in base are appended."""
        base = {"edges": [{"from": "a", "to": "b"}]}
        overlay = {"edges": [{"from": "b", "to": "c"}]}
        result = deep_merge(base, overlay)

        assert len(result["edges"]) == 2

    # === AC 6: __delete__ marker removes elements ===

    def test_delete_marker_removes_node(self):
        """AC 6: __delete__: true removes matching node."""
        base = {
            "nodes": [
                {"name": "keep_me", "val": 1},
                {"name": "delete_me", "val": 2},
                {"name": "also_keep", "val": 3}
            ]
        }
        overlay = {
            "nodes": [
                {"name": "delete_me", "__delete__": True}
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 2
        assert all(n["name"] != "delete_me" for n in result["nodes"])

    def test_delete_marker_removes_edge(self):
        """AC 6: __delete__: true removes matching edge."""
        base = {
            "edges": [
                {"from": "a", "to": "b"},
                {"from": "b", "to": "c"},
                {"from": "c", "to": "d"}
            ]
        }
        overlay = {
            "edges": [
                {"from": "b", "to": "c", "__delete__": True}
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["edges"]) == 2
        assert not any(e["from"] == "b" and e["to"] == "c" for e in result["edges"])

    def test_delete_marker_removes_goto(self):
        """AC 6: __delete__: true removes matching goto entry."""
        base = {
            "nodes": [{
                "name": "router",
                "goto": [
                    {"to": "route_a"},
                    {"to": "route_b"},
                    {"to": "route_c"}
                ]
            }]
        }
        overlay = {
            "nodes": [{
                "name": "router",
                "goto": [
                    {"to": "route_b", "__delete__": True}
                ]
            }]
        }
        result = deep_merge(base, overlay)

        goto = result["nodes"][0]["goto"]
        assert len(goto) == 2
        assert not any(g["to"] == "route_b" for g in goto)

    def test_delete_nonexistent_element_is_noop(self):
        """AC 6: Deleting nonexistent element has no effect."""
        base = {"nodes": [{"name": "a"}]}
        overlay = {"nodes": [{"name": "nonexistent", "__delete__": True}]}
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 1
        assert result["nodes"][0]["name"] == "a"

    # === AC 8: Breaking change - merge by key is default ===

    def test_array_merge_is_default_not_replacement(self):
        """AC 8: Arrays with keys merge by default (breaking from YE.8)."""
        base = {"nodes": [{"name": "a"}, {"name": "b"}]}
        overlay = {"nodes": [{"name": "a", "updated": True}]}
        result = deep_merge(base, overlay)

        # YE.8 would return only 1 node (replacement)
        # YE.9 returns 2 nodes (merge by key)
        assert len(result["nodes"]) == 2

    def test_unknown_arrays_still_replace(self):
        """AC 8: Arrays without configured keys still replace (YE.8 behavior)."""
        base = {"items": [1, 2, 3], "tags": ["a", "b"]}
        overlay = {"items": [4, 5], "tags": ["c"]}
        result = deep_merge(base, overlay)

        assert result["items"] == [4, 5]
        assert result["tags"] == ["c"]


class TestMergeArraysByKeyFunction:
    """Test the merge_arrays_by_key helper function directly."""

    def test_basic_merge_by_single_key(self):
        """Test merging arrays by single key field."""
        base = [{"name": "a", "v": 1}, {"name": "b", "v": 2}]
        overlay = [{"name": "a", "v": 10}]
        result = merge_arrays_by_key(base, overlay, ["name"])

        assert len(result) == 2
        assert result[0] == {"name": "a", "v": 10}
        assert result[1] == {"name": "b", "v": 2}

    def test_merge_by_composite_key(self):
        """Test merging arrays by composite key."""
        base = [{"x": 1, "y": 1, "v": "a"}, {"x": 1, "y": 2, "v": "b"}]
        overlay = [{"x": 1, "y": 1, "v": "updated"}]
        result = merge_arrays_by_key(base, overlay, ["x", "y"])

        assert len(result) == 2
        assert result[0]["v"] == "updated"
        assert result[1]["v"] == "b"

    def test_append_non_matching_elements(self):
        """Test that non-matching elements are appended."""
        base = [{"id": 1}]
        overlay = [{"id": 2}]
        result = merge_arrays_by_key(base, overlay, ["id"])

        assert len(result) == 2

    def test_delete_marker_processing(self):
        """Test __delete__ marker removes elements."""
        base = [{"id": 1}, {"id": 2}, {"id": 3}]
        overlay = [{"id": 2, "__delete__": True}]
        result = merge_arrays_by_key(base, overlay, ["id"])

        assert len(result) == 2
        assert not any(e.get("id") == 2 for e in result)

    def test_non_dict_elements_appended(self):
        """Test that non-dict overlay elements are appended."""
        base = [{"name": "a"}]
        overlay = ["scalar_value"]
        result = merge_arrays_by_key(base, overlay, ["name"])

        assert len(result) == 2
        assert result[1] == "scalar_value"

    def test_missing_key_element_appended(self):
        """Test elements missing the key field are appended."""
        base = [{"name": "a", "v": 1}]
        overlay = [{"v": 2}]  # Missing "name" key
        result = merge_arrays_by_key(base, overlay, ["name"])

        assert len(result) == 2
        assert result[1] == {"v": 2}


class TestComplexMergeScenarios:
    """Test complex real-world merge scenarios."""

    def test_realistic_agent_overlay(self):
        """Test realistic agent overlay scenario from story."""
        base = {
            "name": "test-agent",
            "nodes": [
                {"name": "fetch", "uses": "http.get", "with": {"url": "http://api"}},
                {"name": "process", "uses": "llm.call", "with": {
                    "model": "gpt-4o-mini",
                    "temperature": 0.7,
                    "max_tokens": 1000
                }},
                {"name": "validate", "run": "return {'valid': True}"},
                {"name": "save", "uses": "memory.store"}
            ],
            "edges": [
                {"from": "__start__", "to": "fetch"},
                {"from": "fetch", "to": "process"},
                {"from": "process", "to": "validate"},
                {"from": "validate", "to": "save"},
                {"from": "save", "to": "__end__"}
            ]
        }
        overlay = {
            "nodes": [
                {"name": "process", "with": {"model": "claude-3-opus", "temperature": 0.3}}
            ]
        }
        result = deep_merge(base, overlay)

        # All 4 nodes preserved
        assert len(result["nodes"]) == 4
        # All 5 edges preserved
        assert len(result["edges"]) == 5
        # Process node updated
        process = next(n for n in result["nodes"] if n["name"] == "process")
        assert process["with"]["model"] == "claude-3-opus"
        assert process["with"]["temperature"] == 0.3
        assert process["with"]["max_tokens"] == 1000  # Preserved!

    def test_complex_add_modify_delete(self):
        """Test scenario with add, modify, and delete in single overlay."""
        base = {
            "nodes": [
                {"name": "a", "v": 1},
                {"name": "b", "v": 2},
                {"name": "c", "v": 3},
                {"name": "d", "v": 4}
            ]
        }
        overlay = {
            "nodes": [
                {"name": "a", "v": 10},          # Modify
                {"name": "c", "__delete__": True}, # Delete
                {"name": "e", "v": 5}            # Add
            ]
        }
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 4  # 4 - 1 delete + 1 add = 4
        names = [n["name"] for n in result["nodes"]]
        assert "a" in names
        assert "b" in names
        assert "c" not in names  # Deleted
        assert "d" in names
        assert "e" in names  # Added

        a_node = next(n for n in result["nodes"] if n["name"] == "a")
        assert a_node["v"] == 10

    def test_empty_base_array_with_overlay(self):
        """Test overlay into empty base array."""
        base = {"nodes": []}
        overlay = {"nodes": [{"name": "new", "v": 1}]}
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 1
        assert result["nodes"][0]["name"] == "new"

    def test_empty_overlay_array_preserves_base(self):
        """Test empty overlay array preserves base."""
        base = {"nodes": [{"name": "a"}, {"name": "b"}]}
        overlay = {"nodes": []}
        result = deep_merge(base, overlay)

        assert len(result["nodes"]) == 2

    def test_immutability_base_array_unchanged(self):
        """Test that base array is not mutated."""
        base = {"nodes": [{"name": "a", "v": 1}]}
        base_copy = {"nodes": [{"name": "a", "v": 1}]}
        overlay = {"nodes": [{"name": "a", "v": 10}]}
        deep_merge(base, overlay)

        assert base == base_copy

    def test_immutability_overlay_array_unchanged(self):
        """Test that overlay array is not mutated."""
        base = {"nodes": [{"name": "a", "v": 1}]}
        overlay = {"nodes": [{"name": "a", "v": 10}]}
        overlay_copy = {"nodes": [{"name": "a", "v": 10}]}
        deep_merge(base, overlay)

        assert overlay == overlay_copy


class TestConfigurationConstants:
    """Test configuration constants are correctly defined."""

    def test_array_merge_keys_defined(self):
        """Test ARRAY_MERGE_KEYS has expected entries."""
        assert "nodes" in ARRAY_MERGE_KEYS
        assert "edges" in ARRAY_MERGE_KEYS
        assert ARRAY_MERGE_KEYS["nodes"] == ["name"]
        assert ARRAY_MERGE_KEYS["edges"] == ["from", "to"]

    def test_goto_merge_key_defined(self):
        """Test GOTO_MERGE_KEY is defined."""
        assert GOTO_MERGE_KEY == ["to"]

    def test_delete_marker_defined(self):
        """Test DELETE_MARKER constant."""
        assert DELETE_MARKER == "__delete__"
