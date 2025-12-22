"""
Tests for YAML Engine - Data Processing Actions Tests

Data processing action tests including:
- JSON parse, transform, stringify
- CSV parse, stringify
- Data validate, merge, filter
- Integration tests
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
# TEA-BUILTIN-003.2: Data Processing Actions Tests
# =============================================================================

class TestDataProcessingActions:
    """Unit tests for data processing actions (TEA-BUILTIN-003.2)."""

    # -------------------------------------------------------------------------
    # JSON Parse Tests
    # -------------------------------------------------------------------------

    def test_json_parse_valid(self, engine):
        """json.parse parses valid JSON string to Python object."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text='{"name": "Alice", "age": 30}')

        assert result['success'] is True
        assert result['data'] == {"name": "Alice", "age": 30}

    def test_json_parse_invalid(self, engine):
        """json.parse returns error for invalid JSON."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text='{"invalid json}')

        assert result['success'] is False
        assert 'error' in result
        assert result['error_type'] == 'parse'
        assert 'position' in result

    def test_json_parse_with_default(self, engine):
        """json.parse returns default value when strict=False and parsing fails."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text='invalid', strict=False, default={"fallback": True})

        assert result['success'] is True
        assert result['data'] == {"fallback": True}

    def test_json_parse_non_strict_trailing_comma(self, engine):
        """json.parse in non-strict mode handles trailing commas."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text='{"a": 1, "b": 2,}', strict=False)

        assert result['success'] is True
        assert result['data'] == {"a": 1, "b": 2}

    def test_json_parse_non_strict_comments(self, engine):
        """json.parse in non-strict mode handles comments."""
        action = engine.actions_registry['json.parse']
        json_with_comments = '''
        {
            // This is a comment
            "name": "test",
            "value": 42
        }
        '''
        result = action(state={}, text=json_with_comments, strict=False)

        assert result['success'] is True
        assert result['data']['name'] == 'test'
        assert result['data']['value'] == 42

    def test_json_parse_none_input(self, engine):
        """json.parse returns error for None input."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text=None)

        assert result['success'] is False
        assert 'None' in result['error']

    def test_json_parse_array(self, engine):
        """json.parse handles JSON arrays."""
        action = engine.actions_registry['json.parse']
        result = action(state={}, text='[1, 2, 3, "four"]')

        assert result['success'] is True
        assert result['data'] == [1, 2, 3, "four"]

    # -------------------------------------------------------------------------
    # JSON Transform Tests
    # -------------------------------------------------------------------------

    def test_json_transform_simple(self, engine):
        """json.transform extracts nested value with JMESPath."""
        pytest.importorskip('jmespath')
        action = engine.actions_registry['json.transform']
        data = {"user": {"profile": {"name": "Alice"}}}
        result = action(state={}, data=data, expression="user.profile.name")

        assert result['success'] is True
        assert result['result'] == "Alice"
        assert result['expression'] == "user.profile.name"

    def test_json_transform_filter(self, engine):
        """json.transform filters array with JMESPath."""
        pytest.importorskip('jmespath')
        action = engine.actions_registry['json.transform']
        data = {"users": [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "inactive"},
            {"name": "Charlie", "status": "active"}
        ]}
        result = action(state={}, data=data, expression="users[?status=='active'].name")

        assert result['success'] is True
        assert result['result'] == ["Alice", "Charlie"]

    def test_json_transform_invalid_expression(self, engine):
        """json.transform returns error for invalid JMESPath expression."""
        pytest.importorskip('jmespath')
        action = engine.actions_registry['json.transform']
        result = action(state={}, data={"a": 1}, expression="[[[invalid")

        assert result['success'] is False
        assert 'error' in result
        assert result['error_type'] == 'transform'

    def test_json_transform_none_data(self, engine):
        """json.transform returns error for None data."""
        action = engine.actions_registry['json.transform']
        result = action(state={}, data=None, expression="foo")

        assert result['success'] is False
        assert 'None' in result['error']

    def test_json_transform_empty_expression(self, engine):
        """json.transform returns error for empty expression."""
        action = engine.actions_registry['json.transform']
        result = action(state={}, data={"a": 1}, expression="")

        assert result['success'] is False
        assert 'required' in result['error'].lower()

    def test_json_transform_project_fields(self, engine):
        """json.transform projects fields with JMESPath."""
        pytest.importorskip('jmespath')
        action = engine.actions_registry['json.transform']
        data = {"items": [{"name": "A"}, {"name": "B"}, {"name": "C"}]}
        result = action(state={}, data=data, expression="{names: items[].name, count: length(items)}")

        assert result['success'] is True
        assert result['result'] == {"names": ["A", "B", "C"], "count": 3}

    def test_json_transform_unknown_engine(self, engine):
        """json.transform returns error for unknown engine."""
        action = engine.actions_registry['json.transform']
        result = action(state={}, data={"a": 1}, expression="a", engine="unknown")

        assert result['success'] is False
        assert 'Unknown engine' in result['error']

    # -------------------------------------------------------------------------
    # JSON Stringify Tests
    # -------------------------------------------------------------------------

    def test_json_stringify_basic(self, engine):
        """json.stringify converts dict to JSON string."""
        action = engine.actions_registry['json.stringify']
        result = action(state={}, data={"name": "Alice", "age": 30})

        assert result['success'] is True
        assert '"name"' in result['text']
        assert '"Alice"' in result['text']

    def test_json_stringify_pretty(self, engine):
        """json.stringify with indent creates pretty-printed JSON."""
        action = engine.actions_registry['json.stringify']
        result = action(state={}, data={"a": 1}, indent=2)

        assert result['success'] is True
        assert '\n' in result['text']
        assert '  ' in result['text']

    def test_json_stringify_sort_keys(self, engine):
        """json.stringify with sort_keys orders keys alphabetically."""
        action = engine.actions_registry['json.stringify']
        result = action(state={}, data={"z": 1, "a": 2, "m": 3}, sort_keys=True)

        assert result['success'] is True
        # Keys should appear in order: a, m, z
        text = result['text']
        assert text.index('"a"') < text.index('"m"') < text.index('"z"')

    def test_json_stringify_non_serializable(self, engine):
        """json.stringify handles non-serializable objects with default=str."""
        action = engine.actions_registry['json.stringify']
        from datetime import datetime
        data = {"timestamp": datetime(2025, 1, 1, 12, 0, 0)}
        result = action(state={}, data=data)

        assert result['success'] is True
        assert '2025' in result['text']

    # -------------------------------------------------------------------------
    # CSV Parse Tests
    # -------------------------------------------------------------------------

    def test_csv_parse_with_headers(self, engine):
        """csv.parse returns list of dicts when has_header=True."""
        action = engine.actions_registry['csv.parse']
        csv_text = "name,age,city\nAlice,30,NYC\nBob,25,LA"
        result = action(state={}, text=csv_text)

        assert result['success'] is True
        assert result['headers'] == ["name", "age", "city"]
        assert result['row_count'] == 2
        assert result['data'][0] == {"name": "Alice", "age": "30", "city": "NYC"}
        assert result['data'][1] == {"name": "Bob", "age": "25", "city": "LA"}

    def test_csv_parse_no_headers(self, engine):
        """csv.parse returns list of lists when has_header=False."""
        action = engine.actions_registry['csv.parse']
        csv_text = "Alice,30,NYC\nBob,25,LA"
        result = action(state={}, text=csv_text, has_header=False)

        assert result['success'] is True
        assert result['headers'] is None
        assert result['row_count'] == 2
        assert result['data'][0] == ["Alice", "30", "NYC"]
        assert result['data'][1] == ["Bob", "25", "LA"]

    def test_csv_parse_custom_delimiter(self, engine):
        """csv.parse supports custom delimiter."""
        action = engine.actions_registry['csv.parse']
        csv_text = "name;age;city\nAlice;30;NYC"
        result = action(state={}, text=csv_text, delimiter=";")

        assert result['success'] is True
        assert result['data'][0] == {"name": "Alice", "age": "30", "city": "NYC"}

    def test_csv_parse_malformed_row(self, engine):
        """csv.parse handles malformed rows gracefully."""
        action = engine.actions_registry['csv.parse']
        csv_text = "name,age,city\nAlice,30\nBob,25,LA,extra"
        result = action(state={}, text=csv_text)

        assert result['success'] is True
        # First row has missing value
        assert result['data'][0]['name'] == "Alice"
        assert result['data'][0]['age'] == "30"
        assert result['data'][0]['city'] is None

    def test_csv_parse_from_file(self, engine):
        """csv.parse reads from file path."""
        action = engine.actions_registry['csv.parse']

        with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
            f.write("col1,col2\nval1,val2")
            f.flush()
            temp_path = f.name

        try:
            result = action(state={}, path=temp_path)
            assert result['success'] is True
            assert result['data'][0] == {"col1": "val1", "col2": "val2"}
        finally:
            os.unlink(temp_path)

    def test_csv_parse_file_not_found(self, engine):
        """csv.parse returns error for non-existent file."""
        action = engine.actions_registry['csv.parse']
        result = action(state={}, path="/nonexistent/file.csv")

        assert result['success'] is False
        assert result['error_type'] == 'io'
        assert 'not found' in result['error'].lower()

    def test_csv_parse_no_input(self, engine):
        """csv.parse returns error when neither text nor path provided."""
        action = engine.actions_registry['csv.parse']
        result = action(state={})

        assert result['success'] is False
        assert 'text' in result['error'].lower() or 'path' in result['error'].lower()

    def test_csv_parse_empty(self, engine):
        """csv.parse handles empty CSV."""
        action = engine.actions_registry['csv.parse']
        result = action(state={}, text="")

        assert result['success'] is True
        assert result['data'] == []
        assert result['row_count'] == 0

    # -------------------------------------------------------------------------
    # CSV Stringify Tests
    # -------------------------------------------------------------------------

    def test_csv_stringify_from_dicts(self, engine):
        """csv.stringify converts list of dicts to CSV."""
        action = engine.actions_registry['csv.stringify']
        data = [
            {"name": "Alice", "age": "30"},
            {"name": "Bob", "age": "25"}
        ]
        result = action(state={}, data=data)

        assert result['success'] is True
        assert result['row_count'] == 2
        assert 'name' in result['text']
        assert 'Alice' in result['text']
        assert 'Bob' in result['text']

    def test_csv_stringify_from_lists(self, engine):
        """csv.stringify converts list of lists to CSV."""
        action = engine.actions_registry['csv.stringify']
        data = [
            ["Alice", "30"],
            ["Bob", "25"]
        ]
        result = action(state={}, data=data, headers=["name", "age"])

        assert result['success'] is True
        assert result['row_count'] == 2
        assert 'name,age' in result['text']
        assert 'Alice,30' in result['text']

    def test_csv_stringify_custom_delimiter(self, engine):
        """csv.stringify supports custom delimiter."""
        action = engine.actions_registry['csv.stringify']
        data = [{"a": "1", "b": "2"}]
        result = action(state={}, data=data, delimiter=";")

        assert result['success'] is True
        assert 'a;b' in result['text']
        assert '1;2' in result['text']

    def test_csv_stringify_none_data(self, engine):
        """csv.stringify returns error for None data."""
        action = engine.actions_registry['csv.stringify']
        result = action(state={}, data=None)

        assert result['success'] is False
        assert 'None' in result['error']

    def test_csv_stringify_empty_list(self, engine):
        """csv.stringify handles empty list."""
        action = engine.actions_registry['csv.stringify']
        result = action(state={}, data=[])

        assert result['success'] is True
        assert result['text'] == ""
        assert result['row_count'] == 0

    # -------------------------------------------------------------------------
    # Data Validate Tests
    # -------------------------------------------------------------------------

    def test_data_validate_valid(self, engine):
        """data.validate returns valid=True for valid data."""
        pytest.importorskip('jsonschema')
        action = engine.actions_registry['data.validate']
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"}
            },
            "required": ["name", "age"]
        }
        data = {"name": "Alice", "age": 30}
        result = action(state={}, data=data, schema=schema)

        assert result['success'] is True
        assert result['valid'] is True
        assert result['errors'] == []

    def test_data_validate_invalid(self, engine):
        """data.validate returns valid=False with errors for invalid data."""
        pytest.importorskip('jsonschema')
        action = engine.actions_registry['data.validate']
        schema = {
            "type": "object",
            "properties": {
                "name": {"type": "string"},
                "age": {"type": "integer"}
            },
            "required": ["name", "age"]
        }
        data = {"name": "Alice", "age": "thirty"}  # age should be integer
        result = action(state={}, data=data, schema=schema)

        assert result['success'] is True
        assert result['valid'] is False
        assert len(result['errors']) > 0
        assert result['errors'][0]['path'] == 'age'

    def test_data_validate_missing_required(self, engine):
        """data.validate reports missing required fields."""
        pytest.importorskip('jsonschema')
        action = engine.actions_registry['data.validate']
        schema = {
            "type": "object",
            "required": ["name", "email"]
        }
        data = {"name": "Alice"}  # missing email
        result = action(state={}, data=data, schema=schema)

        assert result['success'] is True
        assert result['valid'] is False
        assert any('email' in e['message'] for e in result['errors'])

    def test_data_validate_none_data(self, engine):
        """data.validate handles None data."""
        pytest.importorskip('jsonschema')
        action = engine.actions_registry['data.validate']
        schema = {"type": "object"}
        result = action(state={}, data=None, schema=schema)

        assert result['success'] is True
        assert result['valid'] is False

    def test_data_validate_none_schema(self, engine):
        """data.validate returns error for None schema."""
        action = engine.actions_registry['data.validate']
        result = action(state={}, data={"a": 1}, schema=None)

        assert result['success'] is False
        assert 'required' in result['error'].lower()

    # -------------------------------------------------------------------------
    # Data Merge Tests
    # -------------------------------------------------------------------------

    def test_data_merge_deep(self, engine):
        """data.merge with deep strategy recursively merges dicts."""
        action = engine.actions_registry['data.merge']
        sources = [
            {"a": 1, "nested": {"x": 10}},
            {"b": 2, "nested": {"y": 20}}
        ]
        result = action(state={}, sources=sources, strategy="deep")

        assert result['success'] is True
        assert result['source_count'] == 2
        assert result['result']['a'] == 1
        assert result['result']['b'] == 2
        assert result['result']['nested'] == {"x": 10, "y": 20}

    def test_data_merge_shallow(self, engine):
        """data.merge with shallow strategy only merges top level."""
        action = engine.actions_registry['data.merge']
        sources = [
            {"a": 1, "nested": {"x": 10}},
            {"b": 2, "nested": {"y": 20}}
        ]
        result = action(state={}, sources=sources, strategy="shallow")

        assert result['success'] is True
        assert result['result']['a'] == 1
        assert result['result']['b'] == 2
        # Shallow merge replaces nested entirely
        assert result['result']['nested'] == {"y": 20}

    def test_data_merge_replace(self, engine):
        """data.merge with replace strategy uses last source entirely."""
        action = engine.actions_registry['data.merge']
        sources = [
            {"a": 1, "b": 2},
            {"c": 3, "d": 4}
        ]
        result = action(state={}, sources=sources, strategy="replace")

        assert result['success'] is True
        assert result['result'] == {"c": 3, "d": 4}

    def test_data_merge_empty_sources(self, engine):
        """data.merge handles empty sources list."""
        action = engine.actions_registry['data.merge']
        result = action(state={}, sources=[])

        assert result['success'] is True
        assert result['result'] == {}
        assert result['source_count'] == 0

    def test_data_merge_invalid_strategy(self, engine):
        """data.merge returns error for invalid strategy."""
        action = engine.actions_registry['data.merge']
        result = action(state={}, sources=[{"a": 1}], strategy="invalid")

        assert result['success'] is False
        assert 'strategy' in result['error'].lower()

    def test_data_merge_non_dict_source(self, engine):
        """data.merge returns error for non-dict source."""
        action = engine.actions_registry['data.merge']
        result = action(state={}, sources=[{"a": 1}, [1, 2, 3]])

        assert result['success'] is False
        assert 'dictionaries' in result['error'].lower()

    # -------------------------------------------------------------------------
    # Data Filter Tests
    # -------------------------------------------------------------------------

    def test_data_filter_eq(self, engine):
        """data.filter with eq operator filters correctly."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "Alice", "status": "active"},
            {"name": "Bob", "status": "inactive"},
            {"name": "Charlie", "status": "active"}
        ]
        predicate = {"field": "status", "op": "eq", "value": "active"}
        result = action(state={}, data=data, predicate=predicate)

        assert result['success'] is True
        assert result['original_count'] == 3
        assert result['filtered_count'] == 2
        assert all(item['status'] == 'active' for item in result['result'])

    def test_data_filter_ne(self, engine):
        """data.filter with ne operator filters correctly."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "Alice", "role": "admin"},
            {"name": "Bob", "role": "user"},
            {"name": "Charlie", "role": "user"}
        ]
        predicate = {"field": "role", "op": "ne", "value": "admin"}
        result = action(state={}, data=data, predicate=predicate)

        assert result['success'] is True
        assert result['filtered_count'] == 2
        assert all(item['role'] != 'admin' for item in result['result'])

    def test_data_filter_gt_gte_lt_lte(self, engine):
        """data.filter with comparison operators works correctly."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "A", "score": 10},
            {"name": "B", "score": 20},
            {"name": "C", "score": 30}
        ]

        # Test gt (greater than)
        result = action(state={}, data=data, predicate={"field": "score", "op": "gt", "value": 15})
        assert result['filtered_count'] == 2

        # Test gte (greater than or equal)
        result = action(state={}, data=data, predicate={"field": "score", "op": "gte", "value": 20})
        assert result['filtered_count'] == 2

        # Test lt (less than)
        result = action(state={}, data=data, predicate={"field": "score", "op": "lt", "value": 25})
        assert result['filtered_count'] == 2

        # Test lte (less than or equal)
        result = action(state={}, data=data, predicate={"field": "score", "op": "lte", "value": 10})
        assert result['filtered_count'] == 1

    def test_data_filter_in_not_in(self, engine):
        """data.filter with in/not_in operators works correctly."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "Alice", "role": "admin"},
            {"name": "Bob", "role": "user"},
            {"name": "Charlie", "role": "moderator"}
        ]

        # Test in
        result = action(state={}, data=data, predicate={"field": "role", "op": "in", "value": ["admin", "moderator"]})
        assert result['filtered_count'] == 2

        # Test not_in
        result = action(state={}, data=data, predicate={"field": "role", "op": "not_in", "value": ["user"]})
        assert result['filtered_count'] == 2

    def test_data_filter_string_ops(self, engine):
        """data.filter with string operators works correctly."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "alice@example.com"},
            {"name": "bob@test.org"},
            {"name": "charlie@example.com"}
        ]

        # Test contains
        result = action(state={}, data=data, predicate={"field": "name", "op": "contains", "value": "example"})
        assert result['filtered_count'] == 2

        # Test startswith
        result = action(state={}, data=data, predicate={"field": "name", "op": "startswith", "value": "alice"})
        assert result['filtered_count'] == 1

        # Test endswith
        result = action(state={}, data=data, predicate={"field": "name", "op": "endswith", "value": ".org"})
        assert result['filtered_count'] == 1

    def test_data_filter_multiple(self, engine):
        """data.filter with multiple predicates uses AND logic."""
        action = engine.actions_registry['data.filter']
        data = [
            {"name": "Alice", "status": "active", "age": 25},
            {"name": "Bob", "status": "active", "age": 35},
            {"name": "Charlie", "status": "inactive", "age": 25}
        ]
        predicates = [
            {"field": "status", "op": "eq", "value": "active"},
            {"field": "age", "op": "lt", "value": 30}
        ]
        result = action(state={}, data=data, predicate=predicates)

        assert result['success'] is True
        assert result['filtered_count'] == 1
        assert result['result'][0]['name'] == "Alice"

    def test_data_filter_nested_field(self, engine):
        """data.filter supports nested field access with dot notation."""
        action = engine.actions_registry['data.filter']
        data = [
            {"user": {"profile": {"level": 1}}},
            {"user": {"profile": {"level": 5}}},
            {"user": {"profile": {"level": 10}}}
        ]
        predicate = {"field": "user.profile.level", "op": "gte", "value": 5}
        result = action(state={}, data=data, predicate=predicate)

        assert result['success'] is True
        assert result['filtered_count'] == 2

    def test_data_filter_none_data(self, engine):
        """data.filter returns error for None data."""
        action = engine.actions_registry['data.filter']
        result = action(state={}, data=None, predicate={"field": "x", "op": "eq", "value": 1})

        assert result['success'] is False
        assert 'required' in result['error'].lower()

    def test_data_filter_no_predicate(self, engine):
        """data.filter returns error for None predicate."""
        action = engine.actions_registry['data.filter']
        result = action(state={}, data=[{"a": 1}], predicate=None)

        assert result['success'] is False
        assert 'required' in result['error'].lower()


class TestDataProcessingActionsIntegration:
    """Integration tests for data processing actions in YAML workflows."""

    def test_json_parse_in_yaml_workflow(self, engine):
        """json.parse works correctly in YAML workflow."""
        config = {
            'nodes': [
                {
                    'name': 'parse_json',
                    'uses': 'json.parse',
                    'with': {'text': '{"message": "hello", "count": 42}'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'parse_json'},
                {'from': 'parse_json', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        final_state = events[-1]['state']
        assert final_state['success'] is True
        assert final_state['data'] == {"message": "hello", "count": 42}

    def test_csv_parse_with_template(self, engine):
        """csv.parse works with templated text from state."""
        config = {
            'nodes': [
                {
                    'name': 'parse_csv',
                    'uses': 'csv.parse',
                    'with': {'text': '{{ state.csv_data }}'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'parse_csv'},
                {'from': 'parse_csv', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({'csv_data': 'a,b\n1,2'}))

        final_state = events[-1]['state']
        assert final_state['success'] is True
        assert final_state['data'] == [{'a': '1', 'b': '2'}]

    def test_data_transform_chain(self, engine):
        """Chain of data processing actions works correctly."""
        pytest.importorskip('jmespath')

        config = {
            'nodes': [
                {
                    'name': 'parse',
                    'uses': 'json.parse',
                    'with': {'text': '{"users": [{"name": "Alice", "active": true}, {"name": "Bob", "active": false}]}'}
                },
                {
                    'name': 'transform',
                    'uses': 'json.transform',
                    'with': {
                        'data': '{{ state.data }}',
                        'expression': 'users[?active].name'
                    }
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'parse'},
                {'from': 'parse', 'to': 'transform'},
                {'from': 'transform', 'to': '__end__'}
            ]
        }
        graph = engine.load_from_dict(config)
        events = list(graph.stream({}))

        final_state = events[-1]['state']
        assert final_state['result'] == ["Alice"]

    def test_action_aliases_work(self, engine):
        """Both namespaced and underscore aliases work."""
        # Test json.parse
        assert 'json.parse' in engine.actions_registry
        assert 'json_parse' in engine.actions_registry

        # Test json.transform
        assert 'json.transform' in engine.actions_registry
        assert 'json_transform' in engine.actions_registry

        # Test csv.parse
        assert 'csv.parse' in engine.actions_registry
        assert 'csv_parse' in engine.actions_registry

        # Test data.validate
        assert 'data.validate' in engine.actions_registry
        assert 'data_validate' in engine.actions_registry

        # Test data.merge
        assert 'data.merge' in engine.actions_registry
        assert 'data_merge' in engine.actions_registry

        # Test data.filter
        assert 'data.filter' in engine.actions_registry
        assert 'data_filter' in engine.actions_registry


# =============================================================================
# Run tests
# =============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v'])
