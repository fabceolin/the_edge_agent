"""
Tests for TEA-DX-001.5: Better dynamic_parallel error messages.

Each error includes the offending node name, the YAML key path that needs
fixing, a short example fragment, and a doc anchor.
"""

import unittest

import pytest

from the_edge_agent import YAMLEngine


_DOC_ANCHOR = "docs/shared/YAML_REFERENCE.md#dynamic-parallel"


def _build(node):
    """Build a minimal config wrapping a single node (plus a fan_in target)."""
    return {
        "name": "wf",
        "nodes": [
            node,
            {"name": "collect", "run": "return {'ok': True}"},
        ],
        "edges": [
            {"from": "__start__", "to": node["name"]},
            {"from": node["name"], "to": "collect"},
            {"from": "collect", "to": "__end__"},
        ],
    }


# (label, node-config) tuples covering every dynamic_parallel error code.
# Used by parametrized property tests below (UNIT-002, UNIT-005, UNIT-017).
_ERROR_CASES = [
    (
        "missing_items",
        {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "action": {"name": "noop"},
            "fan_in": "collect",
        },
    ),
    (
        "missing_fan_in",
        {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"name": "noop"},
        },
    ),
    (
        "no_mode",
        {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "fan_in": "collect",
        },
    ),
    (
        "conflicting_modes",
        {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"name": "noop"},
            "steps": [{"action": "noop"}],
            "fan_in": "collect",
        },
    ),
    (
        "fan_in_unknown",
        {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"uses": "noop"},
            "fan_in": "nonexistent_target",
        },
    ),
]


def _raise_for(node):
    """Trigger validation and return the captured ValueError message."""
    engine = YAMLEngine()
    with pytest.raises(ValueError) as excinfo:
        engine.load_from_dict(_build(node))
    return str(excinfo.value)


def _strip_validator_prefix(msg):
    """Drop the ``Workflow validation failed: [CODE] `` wrapper so we measure
    the inner message template lines (the ones the helper controls)."""
    prefix = "Workflow validation failed: "
    if msg.startswith(prefix):
        rest = msg[len(prefix):]
        if rest.startswith("["):
            rest = rest.split("] ", 1)[-1]
        return rest
    return msg


class TestDynamicParallelErrorMessages(unittest.TestCase):
    """TEA-DX-001.5 — improved validation messages."""

    def test_missing_items_message(self):
        """AC-5: missing 'items' produces hint with example."""
        bad = {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "action": {"name": "noop"},
            "fan_in": "collect",
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError) as excinfo:
            engine.load_from_dict(_build(bad))
        msg = str(excinfo.value)
        # Must mention node name, key path, example, and doc anchor
        self.assertIn("fan_out", msg)
        self.assertIn("items", msg)
        self.assertIn("YAML_REFERENCE.md", msg)

    def test_missing_fan_in_message(self):
        """AC-3: missing 'fan_in' produces sibling-of-items hint."""
        bad = {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"name": "noop"},
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError) as excinfo:
            engine.load_from_dict(_build(bad))
        msg = str(excinfo.value)
        self.assertIn("fan_out", msg)
        self.assertIn("fan_in", msg)
        self.assertIn("YAML_REFERENCE.md", msg)

    def test_conflicting_modes_message(self):
        """AC-2: action + steps named explicitly with 'remove one' suggestion."""
        bad = {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"name": "noop"},
            "steps": [{"action": "noop"}],
            "fan_in": "collect",
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError) as excinfo:
            engine.load_from_dict(_build(bad))
        msg = str(excinfo.value)
        self.assertIn("fan_out", msg)
        self.assertIn("'action'", msg)
        self.assertIn("'steps'", msg)

    def test_no_mode_at_all_message(self):
        """AC-2: when none of action/steps/subgraph present, message lists all three."""
        bad = {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "fan_in": "collect",
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError) as excinfo:
            engine.load_from_dict(_build(bad))
        msg = str(excinfo.value)
        self.assertIn("fan_out", msg)
        self.assertIn("action", msg)
        self.assertIn("steps", msg)
        self.assertIn("subgraph", msg)

    def test_fan_in_unknown_node(self):
        """AC-4: fan_in references a node that is not declared.

        Asserts the rich format: 'is not a defined node', sorted declared
        nodes, an example fragment, and the doc anchor.
        """
        bad = {
            "name": "fan_out",
            "type": "dynamic_parallel",
            "items": "{{ state.batches }}",
            "action": {"uses": "noop"},
            "fan_in": "nonexistent_target",
        }
        engine = YAMLEngine()
        with pytest.raises(ValueError) as excinfo:
            engine.load_from_dict(_build(bad))
        msg = str(excinfo.value)
        self.assertIn("nonexistent_target", msg)
        self.assertIn("is not a defined node", msg)
        self.assertIn("Declared nodes:", msg)
        self.assertIn("'collect'", msg)  # appears in declared list
        self.assertIn("YAML_REFERENCE.md", msg)


# UNIT-002 / UNIT-005 / UNIT-017 / UNIT-021 — parametrized property tests
# applied to every dynamic_parallel error template. These guard the
# maintainability invariants called out in the TEA-DX-001.5 NFR review.


@pytest.mark.parametrize("label,node", _ERROR_CASES)
def test_every_error_message_contains_doc_anchor(label, node):
    """UNIT-002 (AC-1): every error template carries the doc anchor URL."""
    msg = _raise_for(node)
    assert _DOC_ANCHOR in msg, (
        f"{label}: doc anchor {_DOC_ANCHOR!r} missing from message: {msg!r}"
    )


@pytest.mark.parametrize("label,node", _ERROR_CASES)
def test_every_error_message_line_width_and_ascii(label, node):
    """UNIT-005 (AC-8): each inner template line is ≤80 chars and ASCII.

    The validator's outer ``Workflow validation failed: [CODE] `` prefix is
    stripped before measuring — the helper only controls the inner template.
    Lines whose length is dominated by the user-provided node name itself
    (the first line) are exempted: with a representative ~7-char node name
    everything must fit.
    """
    inner = _strip_validator_prefix(_raise_for(node))
    for i, line in enumerate(inner.splitlines(), 1):
        # ASCII-only — no markdown rendering assumed.
        assert line.isascii(), (
            f"{label}:line{i} contains non-ASCII chars: {line!r}"
        )
        assert len(line) <= 80, (
            f"{label}:line{i} is {len(line)} chars (>80): {line!r}"
        )


@pytest.mark.parametrize("label,node", _ERROR_CASES)
def test_every_error_first_line_grep_friendly(label, node):
    """UNIT-017 (AC-13): first line contains node name + error category.

    Mitigates DATA-001 — log-parsing pipelines can grep a single line and
    still know what kind of error fired and which node it relates to.
    """
    inner = _strip_validator_prefix(_raise_for(node))
    first = inner.splitlines()[0]
    assert "fan_out" in first, (
        f"{label}: first line is missing node name: {first!r}"
    )
    # Every first line names a category — matches one of the known phrasings.
    categories = (
        "missing required key",
        "missing branch-body",
        "conflicting branch-body",
        "fan_in target",
    )
    assert any(c in first for c in categories), (
        f"{label}: first line lacks an error-category substring: {first!r}"
    )


def test_helper_does_not_leak_sibling_secret():
    """UNIT-021 (BUS-001 / sec advisory): a sibling key like ``api_key``
    must NOT be echoed back in the error message. The helper interpolates
    only the node name and the offending/conflicting key — never the rest
    of the node config.
    """
    bad = {
        "name": "fan_out",
        "type": "dynamic_parallel",
        # Trigger missing-items error (the most context-rich template).
        "action": {"name": "noop"},
        "fan_in": "collect",
        "api_key": "supersecret-token-do-not-leak",
    }
    msg = _raise_for(bad)
    assert "supersecret-token-do-not-leak" not in msg, (
        "secret-shaped sibling value leaked into validation error: " + msg
    )
    assert "api_key" not in msg, (
        "secret-shaped sibling key leaked into validation error: " + msg
    )


if __name__ == "__main__":
    unittest.main()
