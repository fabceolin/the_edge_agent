"""
Tests for YAML Engine - Actions Tests

Actions tests including:
- Built-in actions
- Template processing
- Actions registry
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
# AC6: Node Definition - Built-in Actions (uses:)
# =============================================================================

class TestBuiltinActions:
    """Tests for built-in action execution."""

    def test_unit_018_action_retrieved_from_registry(self, engine):
        """YAML-001-UNIT-018: _create_action_function retrieves action from registry."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'uses': 'actions.notify',
                    'with': {'channel': 'test', 'message': 'hello'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        assert events[-1]['state'].get('sent') is True

    def test_unit_019_unknown_action_raises_valueerror(self, engine):
        """YAML-001-UNIT-019: Action raises ValueError for unknown action."""
        config = {
            'config': {'raise_exceptions': True},
            'nodes': [
                {
                    'name': 'test',
                    'uses': 'nonexistent.action',
                    'with': {}
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
        assert 'Unknown action' in str(exc_info.value)

    def test_unit_020_action_params_template_processed(self, engine):
        """YAML-001-UNIT-020: Action parameters are template-processed."""
        engine.variables = {'prefix': 'TEST'}
        config = {
            'variables': {'prefix': 'TEST'},
            'nodes': [
                {
                    'name': 'test',
                    'uses': 'actions.notify',
                    'with': {
                        'channel': '{{ variables.prefix }}',
                        'message': '{{ state.msg }}'
                    }
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        # If template processing works, notify should receive processed params
        events = list(graph.stream({'msg': 'hello world'}))
        assert events[-1]['state'].get('sent') is True

    def test_unit_021_output_key_maps_result(self, engine):
        """YAML-001-UNIT-021: output: key maps action result to specified state key."""
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'uses': 'actions.notify',
                    'with': {'channel': 'test', 'message': 'hi'},
                    'output': 'notification_result'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))
        # Result should be under 'notification_result' key
        assert 'notification_result' in events[-1]['state']

    def test_unit_052_custom_action_registration(self, engine):
        """YAML-001-UNIT-052: Custom action registration via constructor works."""
        def custom_multiply(state, factor, **kwargs):
            return {'result': state.get('value', 0) * factor}

        engine = YAMLEngine(actions_registry={'math.multiply': custom_multiply})
        config = {
            'nodes': [
                {
                    'name': 'test',
                    'uses': 'math.multiply',
                    'with': {'factor': 3}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'test'},
                {'from': 'test', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({'value': 7}))
        assert events[-1]['state']['result'] == 21


# =============================================================================
# AC14: Template Variable Processing
# =============================================================================

class TestTemplateProcessing:
    """Tests for template variable substitution."""

    def test_unit_041_state_template(self, engine):
        """YAML-001-UNIT-041: {{ state.key }} replaced with state value."""
        state = {'name': 'Alice', 'age': 30}
        result = engine._process_template('Hello {{ state.name }}, age {{ state.age }}', state)
        assert result == 'Hello Alice, age 30'

    def test_unit_042_variables_template(self, engine):
        """YAML-001-UNIT-042: {{ variables.key }} replaced with global variable."""
        engine.variables = {'env': 'production', 'version': '1.0'}
        result = engine._process_template('Env: {{ variables.env }}', {})
        assert result == 'Env: production'

    def test_unit_043_secrets_template(self, engine):
        """YAML-001-UNIT-043: {{ secrets.key }} replaced with secret value."""
        engine.secrets = {'api_key': 'secret123'}
        result = engine._process_template('Key: {{ secrets.api_key }}', {})
        assert result == 'Key: secret123'

    def test_unit_044_gitlab_style_template(self, engine):
        """YAML-001-UNIT-044: ${ } style (GitLab) replaced with variable."""
        engine.variables = {'CI_COMMIT_SHA': 'abc123'}
        result = engine._process_template('Commit: ${CI_COMMIT_SHA}', {})
        assert result == 'Commit: abc123'

    def test_unit_045_json_filter(self, engine):
        """YAML-001-UNIT-045: Filter | json applies JSON serialization."""
        state = {'data': {'key': 'value', 'num': 42}}
        result = engine._process_template('{{ state.data | json }}', state)
        assert '"key": "value"' in result
        assert '"num": 42' in result

    def test_unit_046_upper_filter(self, engine):
        """YAML-001-UNIT-046: Filter | upper applies uppercase."""
        state = {'msg': 'hello'}
        result = engine._process_template('{{ state.msg | upper }}', state)
        assert result == 'HELLO'

    def test_unit_047_lower_filter(self, engine):
        """YAML-001-UNIT-047: Filter | lower applies lowercase."""
        state = {'msg': 'WORLD'}
        result = engine._process_template('{{ state.msg | lower }}', state)
        assert result == 'world'

    def test_unit_050_invalid_template_raises_error(self, engine):
        """YAML-001-UNIT-050: Invalid nested template expression raises ValueError.

        TEA-YAML-001: With Jinja2 StrictUndefined, nested access on undefined
        variables raises an error rather than returning the original string.
        Use `| default(val)` filter for graceful fallbacks.
        """
        with pytest.raises(ValueError) as exc_info:
            engine._process_template('{{ nonexistent.deeply.nested }}', {})
        # Error should mention the undefined access
        assert 'nonexistent' in str(exc_info.value) or 'undefined' in str(exc_info.value).lower()


# =============================================================================
# AC15: Built-in Actions Registry
# =============================================================================

class TestBuiltinActionsRegistry:
    """Tests for built-in actions."""

    def test_int_011_file_write_action(self, engine):
        """YAML-001-INT-011: file.write action writes content and creates parent dirs."""
        with tempfile.TemporaryDirectory() as tmpdir:
            config = {
                'nodes': [
                    {
                        'name': 'write',
                        'uses': 'file.write',
                        'with': {
                            'path': f'{tmpdir}/subdir/test.txt',
                            'content': 'Hello World'
                        }
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'write'},
                    {'from': 'write', 'to': '__end__'}
                ]
            }
            graph = engine.load_from_dict(config)
            list(graph.stream({}))

            assert os.path.exists(f'{tmpdir}/subdir/test.txt')
            with open(f'{tmpdir}/subdir/test.txt') as f:
                assert f.read() == 'Hello World'

    def test_int_012_file_read_action(self, engine):
        """YAML-001-INT-012: file.read action reads file content."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write('Test content')
            f.flush()

            config = {
                'nodes': [
                    {
                        'name': 'read',
                        'uses': 'file.read',
                        'with': {'path': f.name}
                    }
                ],
                'edges': [
                    {'from': '__start__', 'to': 'read'},
                    {'from': 'read', 'to': '__end__'}
                ]
            }
            graph = engine.load_from_dict(config)
            events = list(graph.stream({}))

            assert events[-1]['state']['content'] == 'Test content'
            os.unlink(f.name)

    def test_unit_051_notify_action(self, engine, capsys):
        """YAML-001-UNIT-051: actions.notify prints to console with channel prefix."""
        config = {
            'nodes': [
                {
                    'name': 'notify',
                    'uses': 'actions.notify',
                    'with': {'channel': 'slack', 'message': 'Test notification'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'notify'},
                {'from': 'notify', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        list(graph.stream({}))

        captured = capsys.readouterr()
        assert '[SLACK]' in captured.out
        assert 'Test notification' in captured.out


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
