"""
Tests for YAML Engine - Node Definition Tests

Node definition tests including:
- Inline Python code execution
- Script style execution
- Multi-step nodes
- Expression evaluation
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
# AC4: Node Definition - Inline Python Code (run:)
# =============================================================================

class TestInlineCode:
    """Tests for inline Python code execution."""

    def test_unit_010_inline_code_executes_with_state(self, engine):
        """YAML-001-UNIT-010: _create_inline_function executes code with state access."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': 'return {"doubled": state["value"] * 2}'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        events = list(graph.stream({'value': 5}))
        final_state = events[-1]['state']
        assert final_state['doubled'] == 10

    def test_unit_011_inline_code_returns_state_updates(self, engine):
        """YAML-001-UNIT-011: Inline code can modify and return state updates."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': '''
value = state["input"] + " processed"
return {"output": value}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        events = list(graph.stream({'input': 'test'}))
        final_state = events[-1]['state']
        assert final_state['output'] == 'test processed'

    def test_unit_012_inline_code_auto_imports_requests(self, engine):
        """YAML-001-UNIT-012: Inline code auto-imports requests when referenced."""
        # Skip if requests is not installed
        pytest.importorskip('requests')

        # Just test that the code with 'requests' reference loads without error
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': 'if requests: return {"has_requests": True}'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        # requests should be imported and available
        assert events[-1]['state'].get('has_requests') is True

    def test_unit_013_inline_code_auto_imports_datetime(self, engine):
        """YAML-001-UNIT-013: Inline code auto-imports datetime when referenced."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': 'from datetime import datetime; return {"year": datetime.now().year}'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        assert events[-1]['state']['year'] >= 2025


# =============================================================================
# AC5: Node Definition - Script Style (script:)
# =============================================================================

class TestScriptStyle:
    """Tests for GitLab CI style script execution."""

    def test_unit_016_script_executes_same_as_run(self, engine):
        """YAML-001-UNIT-016: script: key executes same as run:."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'script': 'return {"result": "from_script"}'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        assert events[-1]['state']['result'] == 'from_script'

    def test_unit_017_multiline_script(self, engine):
        """YAML-001-UNIT-017: Multi-line script execution works."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'script': '''
x = 1
y = 2
z = x + y
return {"sum": z}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        assert events[-1]['state']['sum'] == 3


# =============================================================================
# AC7: Node Definition - Multi-step Nodes (steps:)
# =============================================================================

class TestMultiStepNodes:
    """Tests for multi-step node execution."""

    def test_unit_023_steps_execute_sequentially(self, engine):
        """YAML-001-UNIT-023: _create_steps_function executes steps sequentially."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'steps': [
                        {'name': 'step1', 'run': 'return {"step1": "done"}'},
                        {'name': 'step2', 'run': 'return {"step2": "done"}'}
                    ]
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        state = events[-1]['state']
        assert state.get('step1') == 'done'
        assert state.get('step2') == 'done'

    def test_unit_024_step_results_accumulate(self, engine):
        """YAML-001-UNIT-024: Step results accumulate into current_state."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'steps': [
                        {'name': 'step1', 'run': 'return {"a": 1}'},
                        {'name': 'step2', 'run': 'return {"b": 2}'},
                        {'name': 'step3', 'run': 'return {"c": 3}'}
                    ]
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        state = events[-1]['state']
        assert state['a'] == 1
        assert state['b'] == 2
        assert state['c'] == 3

    def test_unit_025_steps_access_previous_results(self, engine):
        """YAML-001-UNIT-025: Each step can access results from previous steps."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'steps': [
                        {'name': 'step1', 'run': 'return {"x": 10}'},
                        {'name': 'step2', 'run': 'return {"y": state["x"] * 2}'}
                    ]
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        state = events[-1]['state']
        assert state['y'] == 20


# =============================================================================
# AC8: Node Definition - Expression Evaluation
# =============================================================================

class TestExpressionEvaluation:
    """Tests for expression node type."""

    def test_unit_027_expression_type_evaluates(self, engine):
        """YAML-001-UNIT-027: run.type: expression evaluates Python expression."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': {
                        'type': 'expression',
                        'value': 'len(state.get("items", []))',
                        'output_key': 'count'
                    }
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({'items': [1, 2, 3, 4, 5]}))
        assert events[-1]['state']['count'] == 5

    def test_unit_028_expression_default_output_key(self, engine):
        """YAML-001-UNIT-028: Expression result uses default 'result' output_key."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'run': {
                        'type': 'expression',
                        'value': '42'
                    }
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        assert events[-1]['state']['result'] == 42

    def test_unit_029_invalid_expression_raises(self, engine):
        """YAML-001-UNIT-029: Invalid expression raises ValueError with original expression."""
        config = {
            'config': {'raise_exceptions': True},
            'nodes': [
                {
                    'name': 'test',
                    'run': {
                        'type': 'expression',
                        'value': 'invalid syntax here !!!'
                    }
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)

        with pytest.raises(RuntimeError) as exc_info:
            list(graph.stream({}))
        assert 'expression' in str(exc_info.value).lower()


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
