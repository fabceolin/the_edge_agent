"""
Tests for external action module imports in YAMLEngine.

Test file: tests/test_yaml_engine_imports.py

Tests cover:
- Local file imports (path)
- Package imports (package)
- Namespace handling
- Error handling
- Circular import detection
- Integration with workflow execution
- __tea_actions__ metadata

Fixtures required:
- tests/fixtures/actions/valid_actions.py
- tests/fixtures/actions/invalid_actions.py
- tests/fixtures/actions/with_metadata.py
- tests/fixtures/actions/multi_action.py
"""

import os
import sys
import tempfile
import unittest
from unittest.mock import MagicMock, patch

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from the_edge_agent import YAMLEngine


class TestExternalActionImportsPath(unittest.TestCase):
    """Tests for local file path imports."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')
        self.engine = YAMLEngine()

    def test_load_from_path_valid_module(self):
        """Test loading a valid action module from local path."""
        valid_path = os.path.join(self.fixtures_dir, 'valid_actions.py')

        self.engine._load_from_path(valid_path, 'custom', None)

        # Check actions are registered with namespace
        self.assertIn('custom.transform', self.engine.actions_registry)
        self.assertIn('custom.echo', self.engine.actions_registry)

    def test_load_from_path_missing_file(self):
        """Test error when file does not exist."""
        with self.assertRaises(FileNotFoundError) as cm:
            self.engine._load_from_path('/nonexistent/path/actions.py', 'test', None)

        self.assertIn('Action module not found', str(cm.exception))
        self.assertIn('/nonexistent/path/actions.py', str(cm.exception))

    def test_load_from_path_invalid_module(self):
        """Test error when module lacks register_actions."""
        invalid_path = os.path.join(self.fixtures_dir, 'invalid_actions.py')

        with self.assertRaises(ValueError) as cm:
            self.engine._load_from_path(invalid_path, 'test', None)

        self.assertIn("missing required 'register_actions", str(cm.exception))

    def test_load_from_path_relative_resolution(self):
        """Test relative path resolution from YAML directory."""
        # Create a config that uses relative path
        config = {
            'imports': [
                {'path': './valid_actions.py', 'namespace': 'rel'}
            ],
            'nodes': [],
            'edges': []
        }

        # Load with fixtures_dir as yaml_dir
        self.engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Check actions are registered
        self.assertIn('rel.transform', self.engine.actions_registry)
        self.assertIn('rel.echo', self.engine.actions_registry)

    def test_load_from_path_absolute(self):
        """Test absolute path handling."""
        abs_path = os.path.abspath(os.path.join(self.fixtures_dir, 'valid_actions.py'))

        self.engine._load_from_path(abs_path, 'abs', '/some/other/dir')

        self.assertIn('abs.transform', self.engine.actions_registry)

    def test_load_from_path_empty_namespace(self):
        """Test loading with empty namespace (no prefix)."""
        valid_path = os.path.join(self.fixtures_dir, 'valid_actions.py')

        self.engine._load_from_path(valid_path, '', None)

        # Actions should be registered without namespace prefix
        self.assertIn('transform', self.engine.actions_registry)
        self.assertIn('echo', self.engine.actions_registry)


class TestExternalActionImportsPackage(unittest.TestCase):
    """Tests for installed package imports."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()

    def test_load_from_package_missing(self):
        """Test error when package is not installed."""
        with self.assertRaises(ImportError) as cm:
            self.engine._load_from_package('nonexistent_tea_package_xyz', 'test')

        self.assertIn("Failed to import package 'nonexistent_tea_package_xyz'", str(cm.exception))
        self.assertIn("pip install", str(cm.exception))

    def test_load_from_package_invalid(self):
        """Test error when package lacks register_actions."""
        # 'json' is a built-in module without register_actions
        with self.assertRaises(ValueError) as cm:
            self.engine._load_from_package('json', 'test')

        self.assertIn("missing required 'register_actions", str(cm.exception))

    def test_load_from_package_dotted_name(self):
        """Test dotted package name handling."""
        # This should fail because the package doesn't exist
        with self.assertRaises(ImportError) as cm:
            self.engine._load_from_package('tea_actions.submodule', 'test')

        self.assertIn("tea_actions.submodule", str(cm.exception))

    @patch('importlib.import_module')
    def test_load_from_package_valid_mock(self, mock_import):
        """Test successful package loading with mock."""
        # Create a mock module with register_actions
        mock_module = MagicMock()
        mock_module.register_actions = MagicMock(
            side_effect=lambda reg, eng: reg.update({'mock_action': lambda **kw: {}})
        )
        mock_import.return_value = mock_module

        self.engine._load_from_package('tea_mock_package', 'mock')

        # Check action registered
        self.assertIn('mock.mock_action', self.engine.actions_registry)


class TestExternalActionImportsNamespace(unittest.TestCase):
    """Tests for namespace handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')
        self.engine = YAMLEngine()

    def test_namespace_prefix_applied(self):
        """Test that namespace prefix is correctly applied."""
        multi_path = os.path.join(self.fixtures_dir, 'multi_action.py')

        self.engine._load_from_path(multi_path, 'ns', None)

        self.assertIn('ns.action_one', self.engine.actions_registry)
        self.assertIn('ns.action_two', self.engine.actions_registry)
        self.assertIn('ns.action_three', self.engine.actions_registry)

    def test_multiple_imports_different_namespaces(self):
        """Test multiple imports with different namespaces."""
        config = {
            'imports': [
                {'path': './valid_actions.py', 'namespace': 'first'},
                {'path': './multi_action.py', 'namespace': 'second'}
            ],
            'nodes': [],
            'edges': []
        }

        self.engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Check both namespaces
        self.assertIn('first.transform', self.engine.actions_registry)
        self.assertIn('first.echo', self.engine.actions_registry)
        self.assertIn('second.action_one', self.engine.actions_registry)
        self.assertIn('second.action_two', self.engine.actions_registry)


class TestExternalActionImportsCircular(unittest.TestCase):
    """Tests for circular import detection."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')
        self.engine = YAMLEngine()

    def test_duplicate_path_skipped(self):
        """Test that duplicate path imports are skipped."""
        valid_path = os.path.join(self.fixtures_dir, 'valid_actions.py')

        # First load
        self.engine._load_from_path(valid_path, 'first', None)
        initial_count = len(self.engine.actions_registry)

        # Second load of same path should be skipped
        self.engine._load_from_path(valid_path, 'second', None)

        # Should not add new actions (skipped)
        # Note: The second load is silently skipped, so count stays same
        self.assertIn('first.transform', self.engine.actions_registry)
        # second.transform should NOT exist because module was already loaded
        self.assertNotIn('second.transform', self.engine.actions_registry)

    def test_duplicate_package_skipped(self):
        """Test that duplicate package imports are skipped."""
        mock_module = MagicMock()
        mock_module.register_actions = MagicMock(
            side_effect=lambda reg, eng: reg.update({'action': lambda **kw: {}})
        )

        with patch('importlib.import_module', return_value=mock_module):
            self.engine._load_from_package('test_pkg', 'first')
            self.engine._load_from_package('test_pkg', 'second')

        # Only first namespace should exist
        self.assertIn('first.action', self.engine.actions_registry)
        self.assertNotIn('second.action', self.engine.actions_registry)


class TestExternalActionImportsIntegration(unittest.TestCase):
    """Integration tests for external imports."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')

    def test_imported_action_in_workflow(self):
        """Test using imported action in a workflow."""
        config = {
            'imports': [
                {'path': './valid_actions.py', 'namespace': 'custom'}
            ],
            'nodes': [
                {
                    'name': 'transform_node',
                    'uses': 'custom.transform',
                    'with': {'data': 'test_input'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'transform_node'},
                {'from': 'transform_node', 'to': '__end__'}
            ]
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Execute the graph
        events = list(graph.invoke({}))

        # Find final event
        final_event = events[-1]
        self.assertEqual(final_event['type'], 'final')
        self.assertIn('transformed', final_event['state'])
        self.assertEqual(final_event['state']['transformed'], '[test_input]')

    def test_import_error_messages_clear(self):
        """Test that import errors provide clear messages."""
        config = {
            'imports': [
                {'path': './nonexistent.py', 'namespace': 'test'}
            ],
            'nodes': [],
            'edges': []
        }

        engine = YAMLEngine()

        with self.assertRaises(ValueError) as cm:
            engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        error_msg = str(cm.exception)
        self.assertIn('Failed to load imports', error_msg)
        self.assertIn('Action module not found', error_msg)


class TestExternalActionImportsMetadata(unittest.TestCase):
    """Tests for __tea_actions__ metadata handling."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')
        self.engine = YAMLEngine()

    def test_tea_actions_metadata_logged(self):
        """Test that __tea_actions__ metadata is logged."""
        metadata_path = os.path.join(self.fixtures_dir, 'with_metadata.py')

        with patch('the_edge_agent.yaml_engine.logger') as mock_logger:
            self.engine._load_from_path(metadata_path, 'meta', None)

        # Check that info was logged
        mock_logger.info.assert_called()
        call_args = mock_logger.info.call_args[0][0]
        self.assertIn('version: 1.2.3', call_args)
        self.assertIn('namespace: meta', call_args)

    def test_module_without_metadata(self):
        """Test that modules without metadata load successfully."""
        valid_path = os.path.join(self.fixtures_dir, 'valid_actions.py')

        # Should not raise any errors
        self.engine._load_from_path(valid_path, 'test', None)

        self.assertIn('test.transform', self.engine.actions_registry)


class TestExternalActionImportsConfig(unittest.TestCase):
    """Tests for imports configuration parsing."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')

    def test_empty_imports_handled(self):
        """Test that empty imports list is handled gracefully."""
        config = {
            'imports': [],
            'nodes': [],
            'edges': []
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Should not raise any errors
        self.assertIsNotNone(graph)

    def test_missing_imports_key(self):
        """Test that missing imports key is handled."""
        config = {
            'nodes': [],
            'edges': []
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Should not raise any errors
        self.assertIsNotNone(graph)

    def test_invalid_import_type(self):
        """Test error for invalid import configuration."""
        config = {
            'imports': [
                {'url': 'https://example.com/actions.py', 'namespace': 'test'}
            ],
            'nodes': [],
            'edges': []
        }

        engine = YAMLEngine()

        with self.assertRaises(ValueError) as cm:
            engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        self.assertIn("must specify 'path' or 'package'", str(cm.exception))


class TestLoadFromFile(unittest.TestCase):
    """Tests for load_from_file with imports."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')

    def test_load_from_file_with_imports(self):
        """Test load_from_file passes yaml_dir for relative imports."""
        # Create a temporary YAML file
        yaml_content = """
imports:
  - path: ./valid_actions.py
    namespace: file_test

nodes:
  - name: test_node
    uses: file_test.echo
    with:
      message: "Hello from file"

edges:
  - from: __start__
    to: test_node
  - from: test_node
    to: __end__
"""
        # Write YAML to fixtures directory so relative path resolves
        yaml_path = os.path.join(self.fixtures_dir, 'test_import.yaml')
        try:
            with open(yaml_path, 'w') as f:
                f.write(yaml_content)

            engine = YAMLEngine()
            graph = engine.load_from_file(yaml_path)

            # Execute and verify
            events = list(graph.invoke({}))
            final_event = events[-1]
            self.assertEqual(final_event['type'], 'final')
            self.assertEqual(final_event['state']['echoed'], 'Hello from file')
        finally:
            # Cleanup
            if os.path.exists(yaml_path):
                os.remove(yaml_path)


class TestActionExecution(unittest.TestCase):
    """Tests for executing imported actions."""

    def setUp(self):
        """Set up test fixtures."""
        self.fixtures_dir = os.path.join(os.path.dirname(__file__), 'fixtures', 'actions')

    def test_action_receives_state(self):
        """Test that imported actions receive state correctly."""
        config = {
            'imports': [
                {'path': './valid_actions.py', 'namespace': 'custom'}
            ],
            'nodes': [
                {
                    'name': 'transform_node',
                    'uses': 'custom.transform',
                    'with': {'data': '{{ state.input }}'}
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'transform_node'},
                {'from': 'transform_node', 'to': '__end__'}
            ]
        }

        engine = YAMLEngine()
        graph = engine.load_from_dict(config, yaml_dir=self.fixtures_dir)

        # Execute with initial state
        events = list(graph.invoke({'input': 'state_value'}))

        final_event = events[-1]
        self.assertEqual(final_event['state']['transformed'], '[state_value]')


if __name__ == '__main__':
    unittest.main()
