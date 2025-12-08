"""
Tests for YAML Engine - Edge Definition Tests

Edge definition tests including:
- Simple edges
- Conditional edges
- When clause
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END, MemoryCheckpointer
from the_edge_agent.yaml_engine import DotDict


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


# =============================================================================
# AC10: Edge Definition - Simple Edges
# =============================================================================

class TestSimpleEdges:
    """Tests for simple edge definitions."""

    def test_unit_030_simple_edge_creates_connection(self, engine):
        """YAML-001-UNIT-030: Simple edge from: A, to: B creates unconditional edge."""
        config = {
            'nodes': [
                {'name': 'a', 'run': 'return {"a": True}'},
                {'name': 'b', 'run': 'return {"b": True}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'a'},
                {'from': 'a', 'to': 'b'},
                {'from': 'b', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        state = events[-1]['state']
        assert state['a'] is True
        assert state['b'] is True

    def test_unit_031_start_sets_entry_point(self, engine):
        """YAML-001-UNIT-031: from: __start__ sets entry point."""
        config = {
            'nodes': [
                {'name': 'first', 'run': 'return {"first": True}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'first'},
                {'from': 'first', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        # Verify by running the graph - it should start at 'first'
        events = list(graph.stream({}))
        assert events[-1]['state']['first'] is True

    def test_unit_032_end_sets_finish_point(self, engine):
        """YAML-001-UNIT-032: to: __end__ sets finish point."""
        config = {
            'nodes': [
                {'name': 'last', 'run': 'return {"last": True}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'last'},
                {'from': 'last', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        # Verify by running - it should complete and return final state
        events = list(graph.stream({}))
        assert events[-1]['type'] == 'final'
        assert events[-1]['state']['last'] is True


# =============================================================================
# AC12: Edge Definition - Conditional Edges
# =============================================================================

class TestConditionalEdges:
    """Tests for conditional edge definitions."""

    def test_unit_035_expression_condition(self, engine):
        """YAML-001-UNIT-035: condition.type: expression creates conditional routing."""
        config = {
            'nodes': [
                {'name': 'check', 'run': 'return {}'},
                {'name': 'high', 'run': 'return {"path": "high"}'},
                {'name': 'low', 'run': 'return {"path": "low"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'check'},
                {
                    'from': 'check',
                    'to': 'high',
                    'condition': {'type': 'expression', 'value': 'state["value"] > 10'},
                    'when': True
                },
                {
                    'from': 'check',
                    'to': 'low',
                    'condition': {'type': 'expression', 'value': 'state["value"] > 10'},
                    'when': False
                },
                {'from': 'high', 'to': '__end__'},
                {'from': 'low', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test high path
        events = list(graph.stream({'value': 20}))
        assert events[-1]['state']['path'] == 'high'

        # Test low path
        events = list(graph.stream({'value': 5}))
        assert events[-1]['state']['path'] == 'low'


# =============================================================================
# AC13: Edge Definition - When Clause
# =============================================================================

class TestWhenClause:
    """Tests for when clause in edges."""

    def test_unit_039_when_negation(self, engine):
        """YAML-001-UNIT-039: when: !variable creates negation condition."""
        config = {
            'nodes': [
                {'name': 'check', 'run': 'return {}'},
                {'name': 'not_flag', 'run': 'return {"result": "not_flag"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'check'},
                {'from': 'check', 'to': 'not_flag', 'when': '!skip'},
                {'from': 'not_flag', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # When skip is False, !skip is True, so should proceed
        events = list(graph.stream({'skip': False}))
        assert events[-1]['state']['result'] == 'not_flag'

    def test_unit_040_when_variable_reference(self, engine):
        """YAML-001-UNIT-040: when: variable_name creates state lookup condition."""
        config = {
            'nodes': [
                {'name': 'check', 'run': 'return {}'},
                {'name': 'proceed', 'run': 'return {"result": "proceeded"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'check'},
                {'from': 'check', 'to': 'proceed', 'when': 'enabled'},
                {'from': 'proceed', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        events = list(graph.stream({'enabled': True}))
        assert events[-1]['state']['result'] == 'proceeded'


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
