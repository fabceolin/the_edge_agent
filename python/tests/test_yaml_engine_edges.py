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
# YE.7: Conditional Edges on __start__ Node
# =============================================================================

class TestConditionalStartEdges:
    """Tests for conditional edges from __start__ node (YE.7)."""

    def test_ye7_simple_when_on_start_edge(self, engine):
        """YE.7-UNIT-001: Simple when condition on __start__ edge is processed."""
        config = {
            'nodes': [
                {'name': 'fresh', 'run': 'return {"result": "fresh"}'},
                {'name': 'resume', 'run': 'return {"result": "resume"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'fresh', 'when': "not state.get('has_data')"},
                {'from': '__start__', 'to': 'resume', 'when': "state.get('has_data')"},
                {'from': 'fresh', 'to': '__end__'},
                {'from': 'resume', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test fresh path (no data)
        events = list(graph.invoke({'has_data': False}))
        assert events[-1]['state']['result'] == 'fresh'

        # Test resume path (has data)
        events = list(graph.invoke({'has_data': True}))
        assert events[-1]['state']['result'] == 'resume'

    def test_ye7_multiple_conditional_start_edges(self, engine):
        """YE.7-UNIT-002: Multiple conditional edges from __start__ route correctly."""
        config = {
            'nodes': [
                {'name': 'path_a', 'run': 'return {"path": "a"}'},
                {'name': 'path_b', 'run': 'return {"path": "b"}'},
                {'name': 'path_c', 'run': 'return {"path": "c"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'path_a', 'when': "state.get('mode') == 'a'"},
                {'from': '__start__', 'to': 'path_b', 'when': "state.get('mode') == 'b'"},
                {'from': '__start__', 'to': 'path_c', 'when': "state.get('mode') == 'c'"},
                {'from': 'path_a', 'to': '__end__'},
                {'from': 'path_b', 'to': '__end__'},
                {'from': 'path_c', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test each path
        for mode in ['a', 'b', 'c']:
            events = list(graph.invoke({'mode': mode}))
            assert events[-1]['state']['path'] == mode

    def test_ye7_non_conditional_start_edge_backward_compat(self, engine):
        """YE.7-UNIT-003: Non-conditional __start__ edge works unchanged (backward compatible)."""
        config = {
            'nodes': [
                {'name': 'process', 'run': 'return {"processed": True}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'process'},
                {'from': 'process', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({'value': 42}))
        assert events[-1]['state']['processed'] is True

    def test_ye7_condition_syntax_on_start_edge(self, engine):
        """YE.7-UNIT-004: condition (not when) on __start__ edge is processed."""
        config = {
            'nodes': [
                {'name': 'true_branch', 'run': 'return {"branch": "true"}'},
                {'name': 'false_branch', 'run': 'return {"branch": "false"}'}
            ],
            'edges': [
                {
                    'from': '__start__',
                    'to': 'true_branch',
                    'condition': {'type': 'expression', 'value': "state.get('flag', False)"},
                    'when': True
                },
                {
                    'from': '__start__',
                    'to': 'false_branch',
                    'condition': {'type': 'expression', 'value': "state.get('flag', False)"},
                    'when': False
                },
                {'from': 'true_branch', 'to': '__end__'},
                {'from': 'false_branch', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test true path
        events = list(graph.invoke({'flag': True}))
        assert events[-1]['state']['branch'] == 'true'

        # Test false path
        events = list(graph.invoke({'flag': False}))
        assert events[-1]['state']['branch'] == 'false'

    def test_ye7_entry_type_with_when_condition(self, engine):
        """YE.7-UNIT-005: type: entry with when condition works correctly."""
        config = {
            'nodes': [
                {'name': 'mode_a', 'run': 'return {"result": "mode_a"}'},
                {'name': 'mode_b', 'run': 'return {"result": "mode_b"}'}
            ],
            'edges': [
                {'type': 'entry', 'to': 'mode_a', 'when': "state.get('mode') == 'a'"},
                {'type': 'entry', 'to': 'mode_b', 'when': "state.get('mode') == 'b'"},
                {'from': 'mode_a', 'to': '__end__'},
                {'from': 'mode_b', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test mode_a path
        events = list(graph.invoke({'mode': 'a'}))
        assert events[-1]['state']['result'] == 'mode_a'

        # Test mode_b path
        events = list(graph.invoke({'mode': 'b'}))
        assert events[-1]['state']['result'] == 'mode_b'


class TestConditionalStartEdgesIntegration:
    """Integration tests for conditional start edges - multi-round workflow patterns."""

    def test_ye7_integration_multi_round_workflow(self, engine):
        """YE.7-INTG-001: Multi-round workflow with conditional entry routing.

        Simulates an interview agent that:
        - On first invocation (no checkpoint): starts fresh, collects answer
        - On resume (has checkpoint): continues from where it left off
        """
        config = {
            'state': {
                'has_checkpoint': 'bool',
                'round': 'int',
                'answers': 'list'
            },
            'nodes': [
                {
                    'name': 'fresh_start',
                    'run': '''
answers = state.get("answers", [])
return {"round": 1, "answers": answers, "current_question": "What is your name?"}
'''
                },
                {
                    'name': 'resume_flow',
                    'run': '''
round = state.get("round", 1)
answers = state.get("answers", [])
return {"round": round + 1, "answers": answers, "current_question": f"Question {round + 1}"}
'''
                },
                {
                    'name': 'process_answer',
                    'run': '''
answers = state.get("answers", [])
user_answer = state.get("user_answer", "")
if user_answer:
    answers.append({"round": state["round"], "answer": user_answer})
return {"answers": answers}
'''
                }
            ],
            'edges': [
                # Conditional entry routing based on checkpoint state
                {'from': '__start__', 'to': 'fresh_start', 'when': "not state.get('has_checkpoint')"},
                {'from': '__start__', 'to': 'resume_flow', 'when': "state.get('has_checkpoint')"},
                # Both paths lead to processing
                {'from': 'fresh_start', 'to': 'process_answer'},
                {'from': 'resume_flow', 'to': 'process_answer'},
                {'from': 'process_answer', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # First invocation - fresh start
        events = list(graph.invoke({
            'has_checkpoint': False,
            'answers': [],
            'user_answer': 'Alice'
        }))
        state1 = events[-1]['state']
        assert state1['round'] == 1
        assert state1['current_question'] == "What is your name?"
        assert len(state1['answers']) == 1
        assert state1['answers'][0]['answer'] == 'Alice'

        # Second invocation - resume (simulating checkpoint restore)
        events = list(graph.invoke({
            'has_checkpoint': True,
            'round': 1,
            'answers': [{'round': 1, 'answer': 'Alice'}],
            'user_answer': 'Engineer'
        }))
        state2 = events[-1]['state']
        assert state2['round'] == 2
        assert state2['current_question'] == "Question 2"
        assert len(state2['answers']) == 2
        assert state2['answers'][1]['answer'] == 'Engineer'

    def test_ye7_integration_conditional_start_with_stream(self, engine):
        """YE.7-INTG-002: Conditional start edges work with stream() method."""
        config = {
            'nodes': [
                {'name': 'path_a', 'run': 'return {"visited": ["a"]}'},
                {'name': 'path_b', 'run': 'return {"visited": ["b"]}'},
                {'name': 'final', 'run': '''
visited = state.get("visited", [])
visited.append("final")
return {"visited": visited}
'''}
            ],
            'edges': [
                {'from': '__start__', 'to': 'path_a', 'when': "state.get('choice') == 'a'"},
                {'from': '__start__', 'to': 'path_b', 'when': "state.get('choice') == 'b'"},
                {'from': 'path_a', 'to': 'final'},
                {'from': 'path_b', 'to': 'final'},
                {'from': 'final', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        # Test with stream() - path a
        events = list(graph.stream({'choice': 'a'}))
        final_state = events[-1]['state']
        assert final_state['visited'] == ['a', 'final']

        # Test with stream() - path b
        events = list(graph.stream({'choice': 'b'}))
        final_state = events[-1]['state']
        assert final_state['visited'] == ['b', 'final']


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
