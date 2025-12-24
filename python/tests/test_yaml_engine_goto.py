"""
TEA-YAML-002: Tests for implicit graph syntax with goto support.

Tests the new navigation system:
- Implicit chaining (nodes execute in list order by default)
- Unconditional goto (string): `goto: "target_node"`
- Conditional goto (list): `goto: [{if: expr, to: node}, ...]`
- Precedence: goto > edges > implicit chaining
- Backward compatibility with legacy edges section
"""

import pytest
from the_edge_agent import YAMLEngine


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine for each test."""
    return YAMLEngine()


# =============================================================================
# AC2: Implicit Chaining Logic
# =============================================================================


class TestImplicitChaining:
    """Tests for implicit chaining (nodes execute in list order)."""

    def test_yaml002_implicit_linear_flow(self, engine):
        """YAML-002-UNIT-001: Nodes execute in list order without edges section."""
        config = {
            "name": "implicit-linear",
            "nodes": [
                {"name": "step_a", "run": 'return {"path": "a"}'},
                {"name": "step_b", "run": 'return {"path": state["path"] + "->b"}'},
                {"name": "step_c", "run": 'return {"path": state["path"] + "->c"}'},
            ],
            # No edges section - implicit chaining applies
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({"path": ""}))

        assert events[-1]["state"]["path"] == "a->b->c"

    def test_yaml002_implicit_entry_point(self, engine):
        """YAML-002-UNIT-002: First node is implicitly the entry point."""
        config = {
            "name": "implicit-entry",
            "nodes": [
                {"name": "first", "run": 'return {"started": "first"}'},
                {"name": "second", "run": 'return {"completed": True}'},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        # First node should be executed first
        assert events[-1]["state"]["started"] == "first"
        assert events[-1]["state"]["completed"] == True

    def test_yaml002_implicit_finish_point(self, engine):
        """YAML-002-UNIT-003: Last node implicitly finishes the workflow."""
        config = {
            "name": "implicit-finish",
            "nodes": [
                {"name": "step_a", "run": 'return {"done": False}'},
                {"name": "step_b", "run": 'return {"done": True}'},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        # Should complete successfully (last node finishes)
        assert events[-1]["type"] == "final"
        assert events[-1]["state"]["done"] == True


# =============================================================================
# AC3: Unconditional Goto
# =============================================================================


class TestUnconditionalGoto:
    """Tests for unconditional goto (string type)."""

    def test_yaml002_goto_string_jump(self, engine):
        """YAML-002-UNIT-010: goto: string jumps to target node."""
        config = {
            "name": "goto-string",
            "nodes": [
                {"name": "step_a", "goto": "step_c", "run": 'return {"path": "a"}'},
                {
                    "name": "step_b",
                    "run": 'return {"path": state["path"] + "->b"}',
                },  # Skipped
                {"name": "step_c", "run": 'return {"path": state["path"] + "->c"}'},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({"path": ""}))

        # Should skip step_b
        assert events[-1]["state"]["path"] == "a->c"

    def test_yaml002_goto_end(self, engine):
        """YAML-002-UNIT-011: goto: __end__ terminates early."""
        config = {
            "name": "goto-end",
            "nodes": [
                {"name": "step_a", "run": 'return {"done": "a"}'},
                {"name": "step_b", "goto": "__end__", "run": 'return {"done": "b"}'},
                {"name": "step_c", "run": 'return {"done": "c"}'},  # Never reached
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        # Should stop after step_b
        assert events[-1]["state"]["done"] == "b"


# =============================================================================
# AC4: Conditional Goto (List)
# =============================================================================


class TestConditionalGoto:
    """Tests for conditional goto (list of rules)."""

    def test_yaml002_goto_conditional_match(self, engine):
        """YAML-002-UNIT-020: First matching condition determines target."""
        config = {
            "name": "goto-conditional",
            "nodes": [
                {
                    "name": "check",
                    "run": 'return {"score": state.get("score", 0)}',
                    "goto": [
                        {"if": "state.score > 80", "to": "excellent"},
                        {"if": "state.score > 50", "to": "good"},
                        {"to": "needs_work"},  # Fallback
                    ],
                },
                # Each branch explicitly terminates (goto: __end__)
                # Without this, implicit chaining would chain excellent -> good -> needs_work
                {
                    "name": "excellent",
                    "run": 'return {"result": "excellent"}',
                    "goto": "__end__",
                },
                {"name": "good", "run": 'return {"result": "good"}', "goto": "__end__"},
                {"name": "needs_work", "run": 'return {"result": "needs_work"}'},
            ],
        }
        graph = engine.load_from_dict(config)

        # Test high score
        events = list(graph.stream({"score": 90}))
        assert events[-1]["state"]["result"] == "excellent"

        # Test medium score
        events = list(graph.stream({"score": 60}))
        assert events[-1]["state"]["result"] == "good"

        # Test low score (fallback)
        events = list(graph.stream({"score": 30}))
        assert events[-1]["state"]["result"] == "needs_work"

    def test_yaml002_goto_with_explicit_fallback(self, engine):
        """YAML-002-UNIT-021: Explicit fallback rule handles unmatched conditions."""
        config = {
            "name": "goto-with-fallback",
            "nodes": [
                {
                    "name": "check",
                    "run": "return {}",
                    "goto": [
                        {"if": "state.jump", "to": "jumped"},
                        {"to": "continued"},  # Explicit fallback (no condition)
                    ],
                },
                # continued is the fallback target
                {
                    "name": "continued",
                    "run": 'return {"result": "continued"}',
                    "goto": "__end__",
                },
                # jumped is the conditional target
                {"name": "jumped", "run": 'return {"result": "jumped"}'},
            ],
        }
        graph = engine.load_from_dict(config)

        # When condition is False, should go to fallback (continued)
        events = list(graph.stream({"jump": False}))
        assert events[-1]["state"]["result"] == "continued"

        # When condition is True, should jump
        events = list(graph.stream({"jump": True}))
        assert events[-1]["state"]["result"] == "jumped"

    def test_yaml002_goto_loop_retry(self, engine):
        """YAML-002-UNIT-022: Conditional goto can create retry loops."""
        config = {
            "name": "goto-retry",
            "nodes": [
                {
                    "name": "attempt",
                    "run": """
count = state.get("count", 0) + 1
success = count >= 3
return {"count": count, "success": success}
""",
                    "goto": [
                        {"if": "state.success", "to": "done"},
                        {"if": "state.count < 5", "to": "attempt"},  # Retry
                    ],
                },
                {"name": "done", "run": 'return {"result": "completed"}'},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({"count": 0}))

        # Should succeed on 3rd attempt
        assert events[-1]["state"]["count"] == 3
        assert events[-1]["state"]["success"] == True
        assert events[-1]["state"]["result"] == "completed"


# =============================================================================
# AC6: Syntax Error Handling (Validation)
# =============================================================================


class TestGotoValidation:
    """Tests for goto validation errors."""

    def test_yaml002_goto_invalid_target(self, engine):
        """YAML-002-UNIT-030: goto to non-existent node raises error at parse time."""
        config = {
            "name": "invalid-goto",
            "nodes": [
                {"name": "step_a", "goto": "non_existent", "run": "return {}"},
                {"name": "step_b", "run": "return {}"},
            ],
        }
        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "non_existent" in str(exc_info.value)
        assert "non-existent" in str(exc_info.value).lower()

    def test_yaml002_goto_conditional_invalid_target(self, engine):
        """YAML-002-UNIT-031: Conditional goto to non-existent node raises error."""
        config = {
            "name": "invalid-conditional-goto",
            "nodes": [
                {
                    "name": "step_a",
                    "goto": [{"if": "True", "to": "missing_node"}],
                    "run": "return {}",
                }
            ],
        }
        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "missing_node" in str(exc_info.value)

    def test_yaml002_goto_missing_to_field(self, engine):
        """YAML-002-UNIT-032: goto rule without 'to' field raises error."""
        config = {
            "name": "missing-to",
            "nodes": [
                {
                    "name": "step_a",
                    "goto": [{"if": "True"}],  # Missing 'to' field
                    "run": "return {}",
                }
            ],
        }
        with pytest.raises(ValueError) as exc_info:
            engine.load_from_dict(config)

        assert "'to'" in str(exc_info.value).lower()


# =============================================================================
# AC7: Backward Compatibility with edges section
# =============================================================================


class TestBackwardCompatibility:
    """Tests for backward compatibility with legacy edges section."""

    def test_yaml002_legacy_edges_still_work(self, engine):
        """YAML-002-UNIT-040: Legacy edges-only YAML continues to work."""
        config = {
            "name": "legacy-edges",
            "nodes": [
                {"name": "step_a", "run": 'return {"result": "a"}'},
                {"name": "step_b", "run": 'return {"result": "b"}'},
            ],
            "edges": [
                {"from": "__start__", "to": "step_a"},
                {"from": "step_a", "to": "step_b"},
                {"from": "step_b", "to": "__end__"},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        assert events[-1]["state"]["result"] == "b"

    def test_yaml002_goto_precedence_over_edges(self, engine):
        """YAML-002-UNIT-041: goto property takes precedence over edges section."""
        config = {
            "name": "precedence-test",
            "nodes": [
                {"name": "step_a", "goto": "step_c", "run": 'return {"path": "a"}'},
                {"name": "step_b", "run": 'return {"path": state["path"] + "->b"}'},
                {"name": "step_c", "run": 'return {"path": state["path"] + "->c"}'},
            ],
            "edges": [
                # These edges should be ignored because step_a has goto
                {"from": "__start__", "to": "step_a"},
                {"from": "step_a", "to": "step_b"},  # Ignored - goto takes precedence
                {"from": "step_b", "to": "step_c"},
                {"from": "step_c", "to": "__end__"},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({"path": ""}))

        # goto should win over edges
        assert events[-1]["state"]["path"] == "a->c"

    def test_yaml002_mixed_goto_and_edges(self, engine):
        """YAML-002-UNIT-042: Mixed goto and edges work with correct precedence."""
        config = {
            "name": "mixed-test",
            "nodes": [
                {"name": "step_a", "run": 'return {"path": "a"}'},
                {
                    "name": "step_b",
                    "goto": "step_d",
                    "run": 'return {"path": state["path"] + "->b"}',
                },
                {"name": "step_c", "run": 'return {"path": state["path"] + "->c"}'},
                {"name": "step_d", "run": 'return {"path": state["path"] + "->d"}'},
            ],
            "edges": [
                # step_a uses implicit chaining (no goto, no edge)
                # step_b has goto, so its edge should be ignored
                {"from": "step_b", "to": "step_c"},  # Ignored
                {"from": "step_c", "to": "step_d"},
                {"from": "step_d", "to": "__end__"},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({"path": ""}))

        # step_a->step_b (implicit), step_b->step_d (goto), step_d ends
        assert events[-1]["state"]["path"] == "a->b->d"


# =============================================================================
# Integration Tests
# =============================================================================


class TestGotoIntegration:
    """Integration tests for complex goto scenarios."""

    def test_yaml002_self_correction_loop(self, engine):
        """YAML-002-INT-001: LLM self-correction loop pattern."""
        config = {
            "name": "self-correction",
            "nodes": [
                {
                    "name": "attempt",
                    "run": """
count = state.get("attempt_count", 0) + 1
# Simulate: succeed on 2nd attempt
valid = count >= 2
return {"attempt_count": count, "valid": valid}
""",
                    "goto": [
                        {"if": "state.valid", "to": "success"},
                        {"if": "state.attempt_count < 3", "to": "refine"},
                        {"to": "failure"},  # Explicit fallback if max attempts exceeded
                    ],
                },
                {
                    "name": "refine",
                    "run": 'return {"refined": True}',
                    "goto": "attempt",  # Loop back
                },
                {
                    "name": "success",
                    "run": 'return {"result": "success"}',
                    "goto": "__end__",
                },
                {"name": "failure", "run": 'return {"result": "failure"}'},
            ],
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        # Should succeed on 2nd attempt after 1 refinement
        assert events[-1]["state"]["attempt_count"] == 2
        assert events[-1]["state"]["valid"] == True
        assert events[-1]["state"]["result"] == "success"

    def test_yaml002_branch_and_merge(self, engine):
        """YAML-002-INT-002: Conditional branching with merge point."""
        config = {
            "name": "branch-merge",
            "nodes": [
                {
                    "name": "check",
                    "run": "return {}",
                    "goto": [
                        {"if": 'state.priority == "high"', "to": "urgent"},
                        {"to": "normal"},
                    ],
                },
                {
                    "name": "urgent",
                    "run": 'return {"handled_as": "urgent"}',
                    "goto": "finalize",
                },
                {
                    "name": "normal",
                    "run": 'return {"handled_as": "normal"}',
                    "goto": "finalize",
                },
                {"name": "finalize", "run": 'return {"finalized": True}'},
            ],
        }
        graph = engine.load_from_dict(config)

        # Test high priority path
        events = list(graph.stream({"priority": "high"}))
        assert events[-1]["state"]["handled_as"] == "urgent"
        assert events[-1]["state"]["finalized"] == True

        # Test normal priority path
        events = list(graph.stream({"priority": "low"}))
        assert events[-1]["state"]["handled_as"] == "normal"
        assert events[-1]["state"]["finalized"] == True
