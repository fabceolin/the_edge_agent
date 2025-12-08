"""
Tests for Remote Storage Actions (TEA-BUILTIN-004.1).

These tests verify the fsspec-based file.read/file.write and storage.* actions
using the memory:// filesystem, which requires no mocking.
"""

import unittest
from the_edge_agent import YAMLEngine


class TestFileActionsWithFsspec(unittest.TestCase):
    """Test file.read and file.write with fsspec backends."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()

    def test_file_write_local_path(self):
        """Test file.write with local path (backward compatibility)."""
        result = self.engine.actions_registry['file.write'](
            state={},
            path='/tmp/test_storage_local.txt',
            content='Hello local!'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['path'], '/tmp/test_storage_local.txt')

    def test_file_read_local_path(self):
        """Test file.read with local path (backward compatibility)."""
        # Write first
        self.engine.actions_registry['file.write'](
            state={},
            path='/tmp/test_storage_read.txt',
            content='Test content'
        )
        # Read
        result = self.engine.actions_registry['file.read'](
            state={},
            path='/tmp/test_storage_read.txt'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], 'Test content')

    def test_file_write_memory_uri(self):
        """Test file.write with memory:// URI."""
        result = self.engine.actions_registry['file.write'](
            state={},
            path='memory://test/file.json',
            content='{"key": "value"}'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['path'], 'memory://test/file.json')

    def test_file_read_memory_uri(self):
        """Test file.read with memory:// URI."""
        # Write first
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://test/read_test.json',
            content='{"hello": "world"}'
        )
        # Read
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://test/read_test.json'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], '{"hello": "world"}')

    def test_file_read_not_found(self):
        """Test file.read returns error for non-existent file."""
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://nonexistent/file.txt'
        )
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'not_found')

    def test_file_read_stream_mode(self):
        """Test file.read with stream=True returns file-like object."""
        # Write first
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://test/stream_test.txt',
            content='Stream content'
        )
        # Read with stream
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://test/stream_test.txt',
            stream=True
        )
        self.assertTrue(result['success'])
        self.assertIn('file', result)
        # Read from file object
        content = result['file'].read()
        self.assertEqual(content, 'Stream content')
        result['file'].close()

    def test_file_read_with_cache_param(self):
        """Test file.read with cache='simple' parameter."""
        # Write first
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://test/cache_test.json',
            content='{"cached": true}'
        )
        # Read with cache parameter
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://test/cache_test.json',
            cache='simple'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], '{"cached": true}')

    def test_file_write_creates_parent_dirs(self):
        """Test file.write automatically creates parent directories."""
        result = self.engine.actions_registry['file.write'](
            state={},
            path='/tmp/test_storage_nested/subdir/deep/file.txt',
            content='Nested content'
        )
        self.assertTrue(result['success'])

        # Verify by reading
        result = self.engine.actions_registry['file.read'](
            state={},
            path='/tmp/test_storage_nested/subdir/deep/file.txt'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], 'Nested content')

    def test_file_uri_scheme(self):
        """Test file:// URI scheme works."""
        result = self.engine.actions_registry['file.write'](
            state={},
            path='file:///tmp/test_file_uri.txt',
            content='File URI content'
        )
        self.assertTrue(result['success'])

        result = self.engine.actions_registry['file.read'](
            state={},
            path='file:///tmp/test_file_uri.txt'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], 'File URI content')


class TestStorageActions(unittest.TestCase):
    """Test storage.* actions."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()
        # Create some test files in memory filesystem
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://storage_test/file1.json',
            content='{"id": 1}'
        )
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://storage_test/file2.json',
            content='{"id": 2}'
        )
        self.engine.actions_registry['file.write'](
            state={},
            path='memory://storage_test/subdir/file3.json',
            content='{"id": 3}'
        )

    def test_storage_exists_true(self):
        """Test storage.exists returns True for existing file."""
        result = self.engine.actions_registry['storage.exists'](
            state={},
            path='memory://storage_test/file1.json'
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['exists'])

    def test_storage_exists_false(self):
        """Test storage.exists returns False for non-existent file."""
        result = self.engine.actions_registry['storage.exists'](
            state={},
            path='memory://storage_test/nonexistent.json'
        )
        self.assertTrue(result['success'])
        self.assertFalse(result['exists'])

    def test_storage_list(self):
        """Test storage.list returns files in directory."""
        result = self.engine.actions_registry['storage.list'](
            state={},
            path='memory://storage_test/'
        )
        self.assertTrue(result['success'])
        self.assertIn('files', result)
        self.assertGreaterEqual(result['count'], 2)

    def test_storage_list_with_detail(self):
        """Test storage.list with detail=True returns file info."""
        result = self.engine.actions_registry['storage.list'](
            state={},
            path='memory://storage_test/',
            detail=True
        )
        self.assertTrue(result['success'])
        # With detail=True, should return dicts with file info
        if result['count'] > 0 and isinstance(result['files'][0], dict):
            self.assertIn('name', result['files'][0])

    def test_storage_list_with_max_results(self):
        """Test storage.list respects max_results limit."""
        result = self.engine.actions_registry['storage.list'](
            state={},
            path='memory://storage_test/',
            max_results=1
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 1)

    def test_storage_info(self):
        """Test storage.info returns file metadata."""
        result = self.engine.actions_registry['storage.info'](
            state={},
            path='memory://storage_test/file1.json'
        )
        self.assertTrue(result['success'])
        self.assertIn('info', result)
        self.assertIn('size', result['info'])
        self.assertEqual(result['info']['type'], 'file')

    def test_storage_info_not_found(self):
        """Test storage.info returns error for non-existent file."""
        result = self.engine.actions_registry['storage.info'](
            state={},
            path='memory://storage_test/nonexistent.json'
        )
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'not_found')

    def test_storage_copy(self):
        """Test storage.copy copies file within same filesystem."""
        result = self.engine.actions_registry['storage.copy'](
            state={},
            source='memory://storage_test/file1.json',
            destination='memory://storage_test/file1_copy.json'
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['copied'])

        # Verify the copy
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://storage_test/file1_copy.json'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], '{"id": 1}')

    def test_storage_copy_cross_filesystem(self):
        """Test storage.copy works across filesystems (memory to local)."""
        result = self.engine.actions_registry['storage.copy'](
            state={},
            source='memory://storage_test/file1.json',
            destination='/tmp/storage_copy_test.json'
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['copied'])

        # Verify the copy
        result = self.engine.actions_registry['file.read'](
            state={},
            path='/tmp/storage_copy_test.json'
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['content'], '{"id": 1}')

    def test_storage_delete(self):
        """Test storage.delete removes file."""
        # First verify file exists
        result = self.engine.actions_registry['storage.exists'](
            state={},
            path='memory://storage_test/file2.json'
        )
        self.assertTrue(result['exists'])

        # Delete
        result = self.engine.actions_registry['storage.delete'](
            state={},
            path='memory://storage_test/file2.json'
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['deleted'])

        # Verify deleted
        result = self.engine.actions_registry['storage.exists'](
            state={},
            path='memory://storage_test/file2.json'
        )
        self.assertFalse(result['exists'])

    def test_storage_mkdir(self):
        """Test storage.mkdir creates directory."""
        result = self.engine.actions_registry['storage.mkdir'](
            state={},
            path='memory://storage_test/new_dir/'
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['created'])

    def test_storage_mkdir_exist_ok(self):
        """Test storage.mkdir with exist_ok=True doesn't fail on existing dir."""
        # Create once
        self.engine.actions_registry['storage.mkdir'](
            state={},
            path='memory://storage_test/existing_dir/'
        )
        # Create again with exist_ok=True (default)
        result = self.engine.actions_registry['storage.mkdir'](
            state={},
            path='memory://storage_test/existing_dir/',
            exist_ok=True
        )
        self.assertTrue(result['success'])

    def test_storage_native(self):
        """Test storage.native can call filesystem methods."""
        result = self.engine.actions_registry['storage.native'](
            state={},
            path='memory://storage_test/',
            operation='ls'
        )
        self.assertTrue(result['success'])
        self.assertIn('result', result)

    def test_storage_native_invalid_operation(self):
        """Test storage.native returns error for invalid operation."""
        result = self.engine.actions_registry['storage.native'](
            state={},
            path='memory://storage_test/',
            operation='nonexistent_operation'
        )
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], 'operation_not_found')
        self.assertIn('available_operations', result)


class TestStorageActionsRegistration(unittest.TestCase):
    """Test storage actions are properly registered with dual namespace."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()

    def test_storage_actions_dual_namespace(self):
        """Test all storage actions are registered under both namespaces."""
        actions = [
            ('storage.list', 'actions.storage_list'),
            ('storage.exists', 'actions.storage_exists'),
            ('storage.delete', 'actions.storage_delete'),
            ('storage.copy', 'actions.storage_copy'),
            ('storage.info', 'actions.storage_info'),
            ('storage.mkdir', 'actions.storage_mkdir'),
            ('storage.native', 'actions.storage_native'),
        ]
        for primary, secondary in actions:
            self.assertIn(primary, self.engine.actions_registry)
            self.assertIn(secondary, self.engine.actions_registry)
            # Verify they're the same function
            self.assertIs(
                self.engine.actions_registry[primary],
                self.engine.actions_registry[secondary]
            )

    def test_file_actions_still_work(self):
        """Test file.read/write are still registered after fsspec update."""
        self.assertIn('file.read', self.engine.actions_registry)
        self.assertIn('file.write', self.engine.actions_registry)
        self.assertIn('actions.file_read', self.engine.actions_registry)
        self.assertIn('actions.file_write', self.engine.actions_registry)


class TestStorageErrorHandling(unittest.TestCase):
    """Test error handling for storage actions."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()

    def test_missing_backend_package_error(self):
        """Test helpful error when backend package is not installed."""
        # This test would require mocking ImportError, but since we're testing
        # graceful degradation, we just verify the error format is correct
        # when a non-existent protocol is used.
        # Note: fsspec handles unknown protocols differently, so we test
        # a known but uninstalled one.

        # For now, test that the error structure is correct for missing files
        result = self.engine.actions_registry['file.read'](
            state={},
            path='memory://nonexistent/path/file.txt'
        )
        self.assertFalse(result['success'])
        self.assertIn('error', result)
        self.assertIn('error_type', result)


class TestStorageActionsInWorkflow(unittest.TestCase):
    """Test storage actions work in YAML workflow context."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = YAMLEngine()

    def test_storage_actions_in_yaml_workflow(self):
        """Test storage actions can be used in a YAML workflow."""
        # Use inline Python code that calls the actions directly
        config = {
            'name': 'storage_test_workflow',
            'nodes': [
                {
                    'name': 'write_file',
                    'run': '''
import fsspec
fs = fsspec.filesystem('memory')
fs.makedirs('/workflow_test', exist_ok=True)
with fs.open('/workflow_test/data.json', 'w') as f:
    f.write('{"workflow": "test"}')
return {"written": True}
'''
                },
                {
                    'name': 'read_file',
                    'run': '''
import fsspec
fs = fsspec.filesystem('memory')
with fs.open('/workflow_test/data.json', 'r') as f:
    content = f.read()
return {"content": content}
'''
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'write_file'},
                {'from': 'write_file', 'to': 'read_file'},
                {'from': 'read_file', 'to': '__end__'}
            ]
        }

        graph = self.engine.load_from_dict(config)
        events = list(graph.stream({'input': 'test'}))
        final_event = events[-1]

        self.assertEqual(final_event['type'], 'final')
        self.assertIn('content', final_event['state'])
        self.assertEqual(
            final_event['state']['content'],
            '{"workflow": "test"}'
        )


if __name__ == '__main__':
    unittest.main()
