"""
Tests for Jinja2 Template Engine Integration (TEA-YAML-001).

This module tests the Jinja2 template processing functionality:
- Basic interpolation and nested access
- Jinja2 conditionals and loops
- Object passthrough (single expression returns native type)
- fromjson custom filter
- StrictUndefined error handling
- Template caching for performance
- when: edge conditions with Jinja2
- run: code with Jinja2 interpolation

Test Categories:
- P0: Critical functionality (backward compatibility, object passthrough)
- P1: Core Jinja2 features
- P2: Edge cases and performance
"""

import pytest
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


# =============================================================================
# AC1: Basic Interpolation (P0)
# =============================================================================

class TestBasicInterpolation:
    """P0 - Tests for basic template interpolation."""

    def test_yaml_001_unit_001_simple_state_access(self, engine):
        """YAML-001-UNIT-001: {{ state.key }} returns value."""
        result = engine._process_template('{{ state.key }}', {'key': 'hello'})
        assert result == 'hello'

    def test_yaml_001_unit_002_nested_state_access(self, engine):
        """YAML-001-UNIT-002: {{ state.user.name }} works."""
        state = {'user': {'name': 'Alice', 'age': 30}}
        result = engine._process_template('{{ state.user.name }}', state)
        assert result == 'Alice'

    def test_yaml_001_unit_003_multiple_interpolations(self, engine):
        """YAML-001-UNIT-003: Multiple {{ }} in string."""
        state = {'first': 'Hello', 'second': 'World'}
        result = engine._process_template('{{ state.first }} {{ state.second }}!', state)
        assert result == 'Hello World!'


# =============================================================================
# AC2: Custom fromjson Filter (P1)
# =============================================================================

class TestFromJsonFilter:
    """P1 - Tests for the fromjson custom filter."""

    def test_yaml_001_unit_004_parse_valid_json(self, engine):
        """YAML-001-UNIT-004: fromjson parses JSON string."""
        state = {'data': '{"name": "Alice", "age": 30}'}
        result = engine._process_template('{{ state.data | fromjson }}', state)
        assert result == {'name': 'Alice', 'age': 30}

    def test_yaml_001_unit_005_parse_nested_json(self, engine):
        """YAML-001-UNIT-005: fromjson handles nested objects."""
        state = {'data': '{"user": {"profile": {"name": "Bob"}}}'}
        result = engine._process_template('{{ state.data | fromjson }}', state)
        assert result['user']['profile']['name'] == 'Bob'

    def test_yaml_001_unit_006_fromjson_multitemplate_fallback(self, engine):
        """YAML-001-UNIT-006: fromjson in multi-expression template returns data value."""
        state = {'data': 'not valid json'}
        # In a multi-expression template, the filter passes through invalid JSON
        result = engine._process_template('x={{ state.data | fromjson }}', state)
        # Returns the data value embedded in string
        assert result == 'x=not valid json'

    def test_yaml_001_unit_007_filter_chaining(self, engine):
        """YAML-001-UNIT-007: fromjson | length works."""
        state = {'data': '[1, 2, 3, 4, 5]'}
        result = engine._process_template('{{ state.data | fromjson | length }}', state)
        assert result == 5


# =============================================================================
# AC3: Jinja2 Constructs (P0/P1)
# =============================================================================

class TestJinja2Constructs:
    """Tests for Jinja2 conditionals, loops, and filters."""

    def test_yaml_001_unit_008_if_conditional(self, engine):
        """YAML-001-UNIT-008: {% if %}...{% endif %}."""
        template = "{% if state.active %}Active{% else %}Inactive{% endif %}"

        result1 = engine._process_template(template, {'active': True})
        assert result1 == 'Active'

        result2 = engine._process_template(template, {'active': False})
        assert result2 == 'Inactive'

    def test_yaml_001_unit_009_for_loop(self, engine):
        """YAML-001-UNIT-009: {% for %}...{% endfor %}."""
        # Use 'list' as key to avoid conflict with dict.items() method
        template = "{% for item in state.list %}{{ item }},{% endfor %}"
        result = engine._process_template(template, {'list': [1, 2, 3]})
        assert result == '1,2,3,'

    def test_yaml_001_unit_010_native_filters(self, engine):
        """YAML-001-UNIT-010: | upper, | lower, | length."""
        # Use 'list' as key to avoid conflict with dict.items() method
        state = {'name': 'Alice', 'list': [1, 2, 3]}

        assert engine._process_template('{{ state.name | upper }}', state) == 'ALICE'
        assert engine._process_template('{{ state.name | lower }}', state) == 'alice'
        assert engine._process_template('{{ state.list | length }}', state) == 3

    def test_yaml_001_unit_011_tojson_filter(self, engine):
        """YAML-001-UNIT-011: | tojson serializes."""
        state = {'data': {'name': 'Alice', 'age': 30}}
        result = engine._process_template('{{ state.data | tojson }}', state)
        assert json.loads(result) == {'name': 'Alice', 'age': 30}

    def test_yaml_001_unit_012_nested_constructs(self, engine):
        """YAML-001-UNIT-012: Nested if/for."""
        # Use 'list' as key to avoid conflict with dict.items() method
        template = """{% for item in state.list %}{% if item > 2 %}{{ item }}{% endif %}{% endfor %}"""
        result = engine._process_template(template, {'list': [1, 2, 3, 4]})
        assert result == '34'


# =============================================================================
# AC4: Template Context Variables (P0/P1)
# =============================================================================

class TestContextVariables:
    """Tests for template context variable access."""

    def test_yaml_001_int_003_state_accessible(self, engine):
        """YAML-001-INT-003: state dict accessible in templates."""
        state = {'key': 'value', 'nested': {'a': 1}}
        # Multi-expression templates render as strings
        result = engine._process_template('Key={{ state.key }}, Nested={{ state.nested.a }}', state)
        assert result == 'Key=value, Nested=1'

    def test_yaml_001_int_004_variables_accessible(self, engine):
        """YAML-001-INT-004: variables from YAML accessible."""
        engine.variables = {'api_url': 'https://api.example.com'}
        result = engine._process_template('{{ variables.api_url }}', {})
        assert result == 'https://api.example.com'

    def test_yaml_001_int_005_secrets_accessible(self, engine):
        """YAML-001-INT-005: secrets dict accessible."""
        engine.secrets = {'api_key': 'secret123'}
        result = engine._process_template('Key: {{ secrets.api_key }}', {})
        assert result == 'Key: secret123'

    def test_yaml_001_int_006_checkpoint_accessible(self, engine):
        """YAML-001-INT-006: checkpoint.dir/last accessible."""
        engine._checkpoint_dir = '/checkpoints'
        engine._last_checkpoint_path = '/checkpoints/state.pkl'

        result1 = engine._process_template('{{ checkpoint.dir }}', {})
        result2 = engine._process_template('{{ checkpoint.last }}', {})

        assert result1 == '/checkpoints'
        assert result2 == '/checkpoints/state.pkl'


# =============================================================================
# AC5: Object Passthrough (P0)
# =============================================================================

class TestObjectPassthrough:
    """P0 - Tests for single-expression object passthrough."""

    def test_yaml_001_unit_013_dict_passthrough(self, engine):
        """YAML-001-UNIT-013: {{ state.data }} returns dict."""
        state = {'data': {'name': 'Alice', 'age': 30}}
        result = engine._process_template('{{ state.data }}', state)
        assert isinstance(result, dict)
        assert result == {'name': 'Alice', 'age': 30}

    def test_yaml_001_unit_014_list_passthrough(self, engine):
        """YAML-001-UNIT-014: {{ state.list }} returns list."""
        # Use 'list' as key to avoid conflict with dict.items() method
        state = {'list': [1, 2, 3]}
        result = engine._process_template('{{ state.list }}', state)
        assert isinstance(result, list)
        assert result == [1, 2, 3]

    def test_yaml_001_int_007_passthrough_in_action_params(self, engine):
        """YAML-001-INT-007: Object passthrough works in with: params."""
        # Use inline run: code to test passthrough instead of actions.notify
        config = {
            'nodes': [
                {
                    'name': 'process',
                    'run': '''
data = {{ state.data }}  # Should be a dict, not string
result = {"is_dict": isinstance(data, dict), "key": data.get("key")}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'process'},
                {'from': 'process', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({'data': {'key': 'value'}}))
        final = events[-1]
        state = final.get('state', final)
        result = state.get('result', {})
        assert result.get('is_dict') == True
        assert result.get('key') == 'value'


# =============================================================================
# AC6: run: Inline Code Compatibility (P1)
# =============================================================================

class TestRunBlockCompatibility:
    """P1 - Tests for run: inline code with Jinja2."""

    def test_yaml_001_int_008_run_executes_python(self, engine):
        """YAML-001-INT-008: run: executes Python via exec()."""
        config = {
            'nodes': [
                {
                    'name': 'compute',
                    'run': 'result = {"sum": 1 + 2 + 3}'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'compute'},
                {'from': 'compute', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))
        final = events[-1]
        assert final.get('state', final).get('result', {}).get('sum') == 6

    def test_yaml_001_int_009_run_with_jinja2_interpolation(self, engine):
        """YAML-001-INT-009: Jinja2 interpolation + Python in run:."""
        config = {
            'variables': {'multiplier': 5},
            'nodes': [
                {
                    'name': 'compute',
                    'run': '''
mul = {{ variables.multiplier }}
result = {"product": state["value"] * mul}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'compute'},
                {'from': 'compute', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({'value': 3}))
        final = events[-1]
        assert final.get('state', final).get('result', {}).get('product') == 15


# =============================================================================
# AC7: StrictUndefined Error Handling (P1)
# =============================================================================

class TestStrictUndefined:
    """P1 - Tests for StrictUndefined mode."""

    def test_yaml_001_unit_015_undefined_returns_none(self, engine):
        """YAML-001-UNIT-015: Undefined variable returns None in single expressions.

        Jinja2's compile_expression() returns None for single-level undefined access
        rather than raising an error. This is Jinja2's design choice for expressions.
        Users should use `| default(val)` filter for explicit fallbacks.
        """
        result = engine._process_template('{{ state.nonexistent }}', {})
        # Single-level undefined returns None (Jinja2 compile_expression behavior)
        assert result is None

    def test_yaml_001_unit_016_error_includes_varname(self, engine):
        """YAML-001-UNIT-016: Error shows variable name."""
        # Access nested undefined attribute
        with pytest.raises(ValueError) as exc_info:
            engine._process_template('{{ state.nonexistent.deeply.nested }}', {})
        # Error should mention the undefined access
        assert 'nonexistent' in str(exc_info.value) or 'undefined' in str(exc_info.value).lower()

    def test_yaml_001_int_010_default_filter_prevents_error(self, engine):
        """YAML-001-INT-010: | default(val) works."""
        result = engine._process_template('{{ state.missing | default("fallback") }}', {})
        assert result == 'fallback'


# =============================================================================
# AC8: Template Caching (P2)
# =============================================================================

class TestCaching:
    """P2 - Tests for template caching."""

    def test_yaml_001_unit_017_template_cached(self, engine):
        """YAML-001-UNIT-017: Same string uses cache."""
        template = '{{ state.value }}'

        # First call should cache
        engine._process_template(template, {'value': 1})
        cache_key = f"expr:{template.strip()[2:-2].strip()}"

        # Check cache was populated
        assert cache_key in engine._template_cache or f"tmpl:{template}" in engine._template_cache

        # Second call should use cache
        result = engine._process_template(template, {'value': 2})
        assert result == 2


# =============================================================================
# AC9: when: Edge Conditions (P0/P1)
# =============================================================================

class TestWhenConditions:
    """Tests for when: edge conditions with Jinja2."""

    def test_yaml_001_when_jinja2_expression(self, engine):
        """when: clause with Jinja2 expression evaluates correctly."""
        config = {
            'nodes': [
                {'name': 'check', 'run': 'result = {}'},
                {'name': 'path_a', 'run': 'result = {"path": "a"}'},
                {'name': 'path_b', 'run': 'result = {"path": "b"}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'check'},
                {'from': 'check', 'to': 'path_a', 'when': '{{ state.value > 5 }}'},
                {'from': 'check', 'to': 'path_b', 'when': '{{ state.value <= 5 }}'},
                {'from': 'path_a', 'to': '__end__'},
                {'from': 'path_b', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)

        # Test value > 5 takes path_a
        events_a = list(graph.invoke({'value': 10}))
        assert events_a[-1].get('state', events_a[-1]).get('result', {}).get('path') == 'a'

        # Test value <= 5 takes path_b
        events_b = list(graph.invoke({'value': 3}))
        assert events_b[-1].get('state', events_b[-1]).get('result', {}).get('path') == 'b'

    def test_yaml_001_when_in_operator(self, engine):
        """when: clause with Jinja2 'in' operator."""
        config = {
            'nodes': [
                {'name': 'check', 'run': 'result = {}'},
                {'name': 'urgent', 'run': 'result = {"urgent": True}'}
            ],
            'edges': [
                {'from': '__start__', 'to': 'check'},
                {'from': 'check', 'to': 'urgent', 'when': "{{ 'urgent' in state.tags }}"},
                {'from': 'urgent', 'to': '__end__'},
                {'from': 'check', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)

        # With urgent tag
        events = list(graph.invoke({'tags': ['urgent', 'high']}))
        final = events[-1]
        assert final.get('state', final).get('result', {}).get('urgent') == True


# =============================================================================
# Legacy Filter Compatibility (P1)
# =============================================================================

class TestLegacyFilters:
    """P1 - Tests for legacy filter compatibility."""

    def test_yaml_001_legacy_json_filter(self, engine):
        """Legacy | json filter maps to tojson."""
        state = {'data': {'key': 'value'}}
        result = engine._process_template('{{ state.data | json }}', state)
        assert json.loads(result) == {'key': 'value'}


# =============================================================================
# Security Improvements (Bonus)
# =============================================================================

class TestSecurityImprovements:
    """Tests documenting security improvements from Jinja2."""

    def test_sec_jinja2_blocks_import(self, engine):
        """Jinja2 StrictUndefined blocks __import__ (security improvement)."""
        # Unlike the old eval() implementation, Jinja2 doesn't allow __import__
        with pytest.raises(ValueError) as exc_info:
            engine._process_template('{{ __import__("os") }}', {})
        assert 'undefined' in str(exc_info.value).lower()


# =============================================================================
# E2E Tests (P0/P1)
# =============================================================================

class TestE2E:
    """End-to-end tests for complete workflows."""

    def test_yaml_001_e2e_001_complete_agent_with_run(self, engine):
        """YAML-001-E2E-001: Complete YAML agent with run: nodes."""
        config = {
            'name': 'test-agent',
            'variables': {'greeting': 'Hello'},
            'nodes': [
                {
                    'name': 'greet',
                    'run': '''
msg = "{{ variables.greeting }}, {{ state.name }}!"
result = {"message": msg}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'greet'},
                {'from': 'greet', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({'name': 'World'}))
        final = events[-1]
        assert 'Hello, World!' in final.get('state', final).get('result', {}).get('message', '')


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
