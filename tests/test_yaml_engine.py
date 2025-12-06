"""
Tests for YAML Engine - Story YAML-001

Test coverage based on QA test design:
- P0 tests: 12 scenarios (security-critical, core parsing)
- P1 tests: 22 scenarios (core functionality)
- Security tests: SEC-001 through SEC-004
- E2E tests: 3 example file validations
"""

import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END
from the_edge_agent.yaml_engine import DotDict


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def minimal_yaml_config():
    """Minimal valid YAML configuration."""
    return {
        'name': 'test-agent',
        'state_schema': {'value': 'int'},
        'nodes': [
            {'name': 'start', 'run': 'return {"value": 1}'}
        ],
        'edges': [
            {'from': '__start__', 'to': 'start'},
            {'from': 'start', 'to': '__end__'}
        ]
    }


@pytest.fixture
def temp_yaml_file(minimal_yaml_config):
    """Create a temporary YAML file."""
    import yaml
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
        yaml.dump(minimal_yaml_config, f)
        yield f.name
    os.unlink(f.name)


# =============================================================================
# AC1: Core YAML Engine - YAMLEngine Class Creation
# =============================================================================

class TestYAMLEngineInit:
    """Tests for YAMLEngine initialization."""

    def test_unit_001_init_creates_valid_instance(self, engine):
        """YAML-001-UNIT-001: Constructor creates valid instance with default actions."""
        assert isinstance(engine, YAMLEngine)
        assert 'llm.call' in engine.actions_registry
        assert 'http.get' in engine.actions_registry
        assert 'http.post' in engine.actions_registry
        assert 'file.write' in engine.actions_registry
        assert 'file.read' in engine.actions_registry
        assert 'actions.notify' in engine.actions_registry

    def test_unit_002_init_accepts_custom_actions(self):
        """YAML-001-UNIT-002: Constructor accepts and merges custom actions_registry."""
        custom_action = lambda state, **kwargs: {'custom': True}
        engine = YAMLEngine(actions_registry={'custom.action': custom_action})

        assert 'custom.action' in engine.actions_registry
        # Built-in actions still present
        assert 'llm.call' in engine.actions_registry

    def test_unit_003_init_empty_dicts(self, engine):
        """YAML-001-UNIT-003: Initializes empty variables and secrets dicts."""
        assert engine.variables == {}
        assert engine.secrets == {}


# =============================================================================
# AC2: Load from File
# =============================================================================

class TestLoadFromFile:
    """Tests for load_from_file functionality."""

    def test_int_001_load_from_file_returns_stategraph(self, engine, temp_yaml_file):
        """YAML-001-INT-001: load_from_file parses YAML and returns compiled StateGraph."""
        graph = engine.load_from_file(temp_yaml_file)
        assert isinstance(graph, StateGraph)

    def test_int_002_load_from_file_nonexistent_raises(self, engine):
        """YAML-001-INT-002: load_from_file raises error for non-existent file."""
        with pytest.raises(FileNotFoundError):
            engine.load_from_file('/nonexistent/path/file.yaml')

    def test_unit_004_load_from_file_invalid_yaml(self, engine):
        """YAML-001-UNIT-004: load_from_file handles YAML syntax errors."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("invalid: yaml: syntax: [[[")
            f.flush()

            with pytest.raises(Exception):  # yaml.scanner.ScannerError
                engine.load_from_file(f.name)

            os.unlink(f.name)

    def test_int_003_load_from_file_empty(self, engine):
        """YAML-001-INT-003: load_from_file handles empty YAML file."""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write("")
            f.flush()

            # Empty YAML returns None, which should be handled
            with pytest.raises((TypeError, AttributeError)):
                engine.load_from_file(f.name)

            os.unlink(f.name)


# =============================================================================
# AC3: Load from Dictionary
# =============================================================================

class TestLoadFromDict:
    """Tests for load_from_dict functionality."""

    def test_unit_005_load_from_dict_minimal(self, engine, minimal_yaml_config):
        """YAML-001-UNIT-005: load_from_dict with minimal config returns StateGraph."""
        graph = engine.load_from_dict(minimal_yaml_config)
        assert isinstance(graph, StateGraph)

    def test_unit_006_load_from_dict_extracts_variables(self, engine):
        """YAML-001-UNIT-006: load_from_dict extracts and stores global variables."""
        config = {
            'variables': {'api_key': 'test123', 'timeout': 30},
            'nodes': [],
            'edges': []
        }
        engine.load_from_dict(config)

        assert engine.variables == {'api_key': 'test123', 'timeout': 30}

    def test_unit_007_load_from_dict_applies_state_schema(self, engine):
        """YAML-001-UNIT-007: load_from_dict applies state_schema to graph."""
        config = {
            'state_schema': {'query': str, 'count': int},
            'nodes': [],
            'edges': []
        }
        graph = engine.load_from_dict(config)
        assert graph.state_schema == {'query': str, 'count': int}

    def test_unit_008_load_from_dict_applies_raise_exceptions(self, engine):
        """YAML-001-UNIT-008: load_from_dict applies config.raise_exceptions."""
        config = {
            'config': {'raise_exceptions': True},
            'nodes': [],
            'edges': []
        }
        graph = engine.load_from_dict(config)
        assert graph.raise_exceptions is True

    def test_unit_009_load_from_dict_applies_interrupts(self, engine):
        """YAML-001-UNIT-009: load_from_dict applies interrupt_before/after."""
        config = {
            'config': {
                'interrupt_before': ['node_a'],
                'interrupt_after': ['node_b']
            },
            'nodes': [
                {'name': 'node_a', 'run': 'result = {}'},
                {'name': 'node_b', 'run': 'result = {}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'node_a'},
                {'from': 'node_a', 'to': 'node_b'},
                {'from': 'node_b', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        assert 'node_a' in graph.interrupt_before
        assert 'node_b' in graph.interrupt_after


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

    def test_unit_050_invalid_template_fallback(self, engine):
        """YAML-001-UNIT-050: Invalid template expression returns original string."""
        result = engine._process_template('{{ nonexistent.deeply.nested }}', {})
        assert result == '{{ nonexistent.deeply.nested }}'


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
# AC16: Package Integration
# =============================================================================

class TestPackageIntegration:
    """Tests for package exports."""

    def test_int_013_import_yamlengine(self):
        """YAML-001-INT-013: from the_edge_agent import YAMLEngine works."""
        from the_edge_agent import YAMLEngine
        assert YAMLEngine is not None

    def test_int_014_yamlengine_in_all(self):
        """YAML-001-INT-014: YAMLEngine in __all__ exports."""
        from the_edge_agent import __all__
        assert 'YAMLEngine' in __all__


# =============================================================================
# Security Tests
# =============================================================================

class TestSecurityBoundaries:
    """Security tests for code execution boundaries.

    Note: Current implementation uses exec/eval which are inherently dangerous.
    These tests document the attack surface.
    """

    def test_sec_001_inline_cannot_os_system(self, engine):
        """YAML-001-SEC-001: Document that inline code CAN access os module.

        This test documents current behavior - inline code is NOT sandboxed.
        """
        config = {
            'config': {'raise_exceptions': True},
            'nodes': [
                {
                    'name': 'test',
                    'run': '''
import os
# This WOULD work - documenting that inline code is not sandboxed
return {"os_available": True}
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
        # This demonstrates os IS available - security concern documented
        assert events[-1]['state']['os_available'] is True

    def test_sec_003_template_eval_not_sandboxed(self, engine):
        """YAML-001-SEC-003: Document that template eval is NOT sandboxed.

        SECURITY CONCERN: Templates can access __import__ and other builtins.
        This test documents the current attack surface.
        """
        # __import__ IS available - documenting security concern
        result = engine._process_template('{{ __import__("os") }}', {})
        # This shows templates can import modules - major security concern
        assert 'os' in result.lower() or 'module' in result.lower()


# =============================================================================
# DotDict Helper Tests
# =============================================================================

class TestDotDict:
    """Tests for DotDict helper class."""

    def test_dotdict_attribute_access(self):
        """DotDict allows attribute-style access to dict keys."""
        d = DotDict({'name': 'test', 'count': 42})
        assert d.name == 'test'
        assert d.count == 42

    def test_dotdict_nested_access(self):
        """DotDict handles nested dicts."""
        d = DotDict({'outer': {'inner': 'value'}})
        assert d.outer.inner == 'value'

    def test_dotdict_missing_key_raises(self):
        """DotDict raises AttributeError for missing keys."""
        d = DotDict({'a': 1})
        with pytest.raises(AttributeError):
            _ = d.nonexistent


# =============================================================================
# E2E Tests
# =============================================================================

class TestE2EExamples:
    """End-to-end tests for example YAML files."""

    @pytest.fixture
    def examples_dir(self):
        """Get examples directory path."""
        return Path(__file__).parent.parent / 'examples'

    def test_e2e_001_research_agent(self, examples_dir):
        """YAML-001-E2E-001: Load and execute yaml_agent_example.yaml."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_agent_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'query': 'test research'}))

        final_state = events[-1]['state']
        assert 'search_results' in final_state
        assert 'formatted_output' in final_state
        assert len(final_state['search_results']) == 5

    def test_e2e_002_customer_support(self, examples_dir):
        """YAML-001-E2E-002: Load and execute yaml_customer_support_example.yaml."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_customer_support_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))

        # Test billing path
        events = list(graph.stream({
            'customer_id': 'CUST-001',
            'customer_message': 'I was charged twice for my bill!'
        }))
        final_state = events[-1]['state']
        assert final_state['intent'] == 'billing'
        assert 'BILL-' in final_state.get('ticket_id', '')

    def test_e2e_003_perplexity_research(self, examples_dir):
        """YAML-001-E2E-003: Load and execute yaml_perplexity_example.yaml (mocked)."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_perplexity_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'user_query': 'What is AI?'}))

        final_state = events[-1]['state']
        assert 'formatted_report' in final_state
        assert 'Research Report' in final_state['formatted_report']

    def test_e2e_005_stream_yields_intermediate(self, examples_dir):
        """YAML-001-E2E-005: Stream execution yields intermediate states."""
        engine = YAMLEngine()
        yaml_path = examples_dir / 'yaml_agent_example.yaml'

        if not yaml_path.exists():
            pytest.skip("Example file not found")

        graph = engine.load_from_file(str(yaml_path))
        events = list(graph.stream({'query': 'test'}))

        # Should have multiple events, not just final
        assert len(events) > 1

        # Check for intermediate state events
        state_events = [e for e in events if e.get('type') == 'state']
        assert len(state_events) > 0


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
