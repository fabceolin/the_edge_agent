"""
Tests for Code Execution Actions (TEA-BUILTIN-003.1).

This module tests the sandboxed Python code execution functionality:
- code.execute action for one-off code execution
- code.sandbox action for persistent session management
- Security tests for blocked operations (P0 priority)

Test Categories:
- P0: Security tests - MUST ALL PASS before deployment
- P1: Core functionality tests

Note: These tests require RestrictedPython to be installed.
"""

import unittest
import time
import warnings
from typing import Dict, Any

import pytest

from the_edge_agent import YAMLEngine


class TestCodeExecutionActions(unittest.TestCase):
    """P1 - Core functionality tests for code execution."""

    def setUp(self):
        """Create engine with code execution enabled."""
        self.engine = YAMLEngine(enable_code_execution=True)
        self.registry = self.engine.actions_registry

    def test_code_execute_simple(self):
        """P1: Execute simple arithmetic code."""
        result = self.registry['code.execute'](
            state={},
            code="x = 1 + 2; result = x"
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value'], 3)

    def test_code_execute_returns_value(self):
        """P1: Verify 'result' variable is returned."""
        result = self.registry['code.execute'](
            state={},
            code="result = 'hello' * 3"
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value'], 'hellohellohello')

    @pytest.mark.filterwarnings("ignore:.*Prints, but never reads 'printed' variable:SyntaxWarning")
    def test_code_execute_captures_stdout(self):
        """P1: Verify print() output is captured."""
        result = self.registry['code.execute'](
            state={},
            code="print('hello'); print('world')"
        )
        self.assertTrue(result['success'])
        self.assertIn('hello', result['stdout'])
        self.assertIn('world', result['stdout'])

    def test_code_execute_captures_stderr(self):
        """P1: Verify stderr is captured (from exceptions)."""
        # Note: RestrictedPython doesn't have sys.stderr, so we test
        # that errors are captured in the error field
        result = self.registry['code.execute'](
            state={},
            code="raise ValueError('test error')"
        )
        self.assertFalse(result['success'])
        self.assertIn('test error', result['error'])

    def test_code_execute_timeout(self):
        """P1: Verify timeout enforcement for slow code."""
        result = self.registry['code.execute'](
            state={},
            code="while True: pass",
            timeout=0.5  # 500ms timeout
        )
        self.assertFalse(result['success'])
        self.assertIn('timed out', result['error'].lower())

    def test_code_sandbox_create_destroy(self):
        """P1: Create and destroy sandbox sessions."""
        # Create
        result = self.registry['code.sandbox'](state={}, action="create")
        self.assertTrue(result['success'])
        self.assertTrue(result['created'])
        sandbox_id = result['sandbox_id']
        self.assertIsNotNone(sandbox_id)

        # Verify in list
        list_result = self.registry['code.sandbox'](state={}, action="list")
        self.assertIn(sandbox_id, list_result['sandboxes'])

        # Destroy
        destroy_result = self.registry['code.sandbox'](
            state={}, action="destroy", sandbox_id=sandbox_id
        )
        self.assertTrue(destroy_result['destroyed'])

        # Verify removed from list
        list_result2 = self.registry['code.sandbox'](state={}, action="list")
        self.assertNotIn(sandbox_id, list_result2['sandboxes'])

    def test_code_sandbox_session_persistence(self):
        """P1: Variables persist across executions in same session."""
        # Create session
        result = self.registry['code.sandbox'](state={}, action="create")
        sandbox_id = result['sandbox_id']

        # Execute first code - define variable
        self.registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sandbox_id,
            code="counter = 10"
        )

        # Execute second code - use variable
        result2 = self.registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sandbox_id,
            code="counter += 5; result = counter"
        )
        self.assertTrue(result2['success'])
        self.assertEqual(result2['return_value'], 15)

        # Cleanup
        self.registry['code.sandbox'](
            state={}, action="destroy", sandbox_id=sandbox_id
        )

    def test_dual_namespace_access(self):
        """P1: Verify code.* and actions.code_* namespaces work."""
        # Test code.execute
        result1 = self.registry['code.execute'](state={}, code="result = 42")
        self.assertEqual(result1['return_value'], 42)

        # Test actions.code_execute
        result2 = self.registry['actions.code_execute'](state={}, code="result = 42")
        self.assertEqual(result2['return_value'], 42)

        # Test code.sandbox
        result3 = self.registry['code.sandbox'](state={}, action="list")
        self.assertTrue(result3['success'])

        # Test actions.code_sandbox
        result4 = self.registry['actions.code_sandbox'](state={}, action="list")
        self.assertTrue(result4['success'])

    @pytest.mark.filterwarnings("ignore:.*Prints, but never reads 'printed' variable:SyntaxWarning")
    def test_max_output_bytes_truncation(self):
        """P1: Verify output is truncated at max_output_bytes."""
        # Generate output larger than limit
        result = self.registry['code.execute'](
            state={},
            code="print('X' * 10000)",
            max_output_bytes=100
        )
        self.assertTrue(result['success'])
        # Output should be truncated
        self.assertTrue(len(result['stdout']) <= 200)  # Some margin for truncation message
        self.assertIn('truncated', result['stdout'].lower())

    def test_execution_time_tracked(self):
        """P1: Verify execution_time_ms is returned."""
        result = self.registry['code.execute'](
            state={},
            code="x = sum(range(1000)); result = x"
        )
        self.assertTrue(result['success'])
        self.assertIn('execution_time_ms', result)
        self.assertIsInstance(result['execution_time_ms'], float)
        self.assertGreaterEqual(result['execution_time_ms'], 0)

    def test_code_disabled_by_default(self):
        """P1: Verify code execution is disabled by default."""
        engine = YAMLEngine()  # Default: enable_code_execution=False
        result = engine.actions_registry['code.execute'](
            state={}, code="result = 1"
        )
        self.assertFalse(result['success'])
        self.assertIn('disabled', result['error'].lower())


class TestCodeExecutionActionsIntegration(unittest.TestCase):
    """P1 - Integration tests for code execution in workflows."""

    def test_code_execute_in_yaml_workflow(self):
        """P1: Use code.execute in a YAML-defined workflow."""
        engine = YAMLEngine(enable_code_execution=True)

        config = {
            'nodes': [
                {
                    'name': 'compute',
                    'uses': 'code.execute',
                    'with': {
                        'code': 'x = {{ state.input }}; result = x * 2'
                    },
                    'output': 'computation'
                }
            ],
            'edges': [
                {'from': '__start__', 'to': 'compute'},
                {'from': 'compute', 'to': '__end__'}
            ]
        }

        graph = engine.load_from_dict(config)
        events = list(graph.invoke({'input': 21}))

        # Find final event
        final = [e for e in events if e.get('type') == 'final'][0]
        self.assertEqual(final['state']['computation']['return_value'], 42)

    def test_code_sandbox_with_checkpoint(self):
        """P1: Sandbox sessions work with checkpoints."""
        # This tests that sandbox sessions persist correctly across
        # checkpoint save/restore cycles (if checkpointing is used)
        engine = YAMLEngine(enable_code_execution=True)

        # Create sandbox
        result = engine.actions_registry['code.sandbox'](
            state={}, action="create"
        )
        sandbox_id = result['sandbox_id']

        # Execute some code
        engine.actions_registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sandbox_id,
            code="data = [1, 2, 3]"
        )

        # Simulate checkpoint (sessions are stored on engine)
        self.assertIn(sandbox_id, engine._sandbox_sessions)

        # Execute more code
        result2 = engine.actions_registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sandbox_id,
            code="result = sum(data)"
        )
        self.assertEqual(result2['return_value'], 6)

        # Cleanup
        engine.actions_registry['code.sandbox'](
            state={}, action="destroy", sandbox_id=sandbox_id
        )


class TestCodeExecutionSecurity(unittest.TestCase):
    """P0 - Security tests - ALL MUST PASS before deployment."""

    def setUp(self):
        """Create engine with code execution enabled."""
        self.engine = YAMLEngine(enable_code_execution=True)
        self.registry = self.engine.actions_registry

    def test_blocks_open_builtin(self):
        """P0: Block open() builtin for file access."""
        result = self.registry['code.execute'](
            state={},
            code="f = open('/etc/passwd', 'r')"
        )
        self.assertFalse(result['success'])
        # Should fail because 'open' is not in allowed builtins

    def test_blocks_import_statement(self):
        """P0: Block import statements."""
        result = self.registry['code.execute'](
            state={},
            code="import os"
        )
        self.assertFalse(result['success'])
        # RestrictedPython blocks import statements

    def test_blocks_exec_builtin(self):
        """P0: Block exec() builtin."""
        result = self.registry['code.execute'](
            state={},
            code="exec('x = 1')"
        )
        self.assertFalse(result['success'])

    def test_blocks_eval_builtin(self):
        """P0: Block eval() builtin."""
        result = self.registry['code.execute'](
            state={},
            code="x = eval('1+1')"
        )
        self.assertFalse(result['success'])

    def test_blocks_dunder_class(self):
        """P0: Block __class__ attribute access."""
        result = self.registry['code.execute'](
            state={},
            code="x = ''.__class__"
        )
        self.assertFalse(result['success'])

    def test_blocks_dunder_mro(self):
        """P0: Block __mro__ attribute access."""
        result = self.registry['code.execute'](
            state={},
            code="x = str.__mro__"
        )
        self.assertFalse(result['success'])

    def test_blocks_dunder_subclasses(self):
        """P0: Block __subclasses__ attribute access."""
        result = self.registry['code.execute'](
            state={},
            code="x = object.__subclasses__()"
        )
        self.assertFalse(result['success'])

    def test_blocks_dunder_globals(self):
        """P0: Block __globals__ attribute access."""
        result = self.registry['code.execute'](
            state={},
            code="def f(): pass; x = f.__globals__"
        )
        self.assertFalse(result['success'])

    def test_blocks_dunder_builtins(self):
        """P0: Block __builtins__ attribute access."""
        result = self.registry['code.execute'](
            state={},
            code="x = __builtins__"
        )
        self.assertFalse(result['success'])

    def test_blocks_getattr_builtin(self):
        """P0: Block getattr() builtin."""
        result = self.registry['code.execute'](
            state={},
            code="x = getattr(str, '__class__')"
        )
        self.assertFalse(result['success'])

    def test_timeout_prevents_infinite_loop(self):
        """P0: Timeout stops infinite loops."""
        start = time.time()
        result = self.registry['code.execute'](
            state={},
            code="while True: pass",
            timeout=1.0
        )
        elapsed = time.time() - start

        self.assertFalse(result['success'])
        self.assertIn('timed out', result['error'].lower())
        # Should complete in roughly 1-2 seconds (timeout + overhead)
        self.assertLess(elapsed, 5.0)

    def test_sandbox_isolation_between_sessions(self):
        """P0: Different sandbox sessions don't share state."""
        # Create two sessions
        result1 = self.registry['code.sandbox'](state={}, action="create")
        result2 = self.registry['code.sandbox'](state={}, action="create")
        sid1 = result1['sandbox_id']
        sid2 = result2['sandbox_id']

        # Set variable in session 1
        self.registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sid1,
            code="secret = 'password123'"
        )

        # Try to access variable in session 2
        result = self.registry['code.sandbox'](
            state={}, action="execute", sandbox_id=sid2,
            code="result = secret"
        )

        # Should fail - variable doesn't exist in session 2
        self.assertFalse(result['success'])

        # Cleanup
        self.registry['code.sandbox'](state={}, action="destroy", sandbox_id=sid1)
        self.registry['code.sandbox'](state={}, action="destroy", sandbox_id=sid2)

    # Additional P0 tests per QA review (SEC-016 to SEC-020)

    def test_blocks_type_builtin(self):
        """P0 (SEC-016): Block type() builtin (metaclass attacks)."""
        result = self.registry['code.execute'](
            state={},
            code="MyClass = type('MyClass', (object,), {})"
        )
        self.assertFalse(result['success'])

    def test_blocks_vars_and_dir_builtins(self):
        """P0 (SEC-017): Block vars() and dir() builtins (information disclosure)."""
        result1 = self.registry['code.execute'](
            state={},
            code="x = vars()"
        )
        self.assertFalse(result1['success'])

        result2 = self.registry['code.execute'](
            state={},
            code="x = dir(str)"
        )
        self.assertFalse(result2['success'])

    def test_blocks_input_builtin(self):
        """P0 (SEC-018): Block input() builtin (STDIN hijacking)."""
        result = self.registry['code.execute'](
            state={},
            code="x = input('Enter: ')"
        )
        self.assertFalse(result['success'])

    def test_blocks_help_builtin(self):
        """P0 (SEC-019): Block help() builtin (interactive shell access)."""
        result = self.registry['code.execute'](
            state={},
            code="help(str)"
        )
        self.assertFalse(result['success'])

    def test_os_module_not_accessible(self):
        """P0 (SEC-020): Verify os module is not accessible."""
        # Try various ways to access os
        result1 = self.registry['code.execute'](
            state={},
            code="import os"
        )
        self.assertFalse(result1['success'])

        result2 = self.registry['code.execute'](
            state={},
            code="from os import system"
        )
        self.assertFalse(result2['success'])

        result3 = self.registry['code.execute'](
            state={},
            code="__import__('os')"
        )
        self.assertFalse(result3['success'])

    def test_blocks_compile_builtin(self):
        """P0: Block compile() builtin."""
        result = self.registry['code.execute'](
            state={},
            code="code = compile('x=1', '<string>', 'exec')"
        )
        self.assertFalse(result['success'])

    def test_blocks_object_builtin(self):
        """P0: Block object builtin (base class access)."""
        result = self.registry['code.execute'](
            state={},
            code="x = object()"
        )
        self.assertFalse(result['success'])

    def test_blocks_super_builtin(self):
        """P0: Block super() builtin."""
        result = self.registry['code.execute'](
            state={},
            code="class A: pass\nclass B(A):\n    def f(self): return super()"
        )
        # This might succeed in compilation but fail on execution
        # or fail in compilation due to RestrictedPython restrictions
        if result['success']:
            # If compilation succeeded, ensure we can't actually use super()
            result2 = self.registry['code.execute'](
                state={},
                code="class A:\n    x = 1\nclass B(A):\n    def f(self): return super().x\nb = B(); result = b.f()"
            )
            # Should fail when trying to use super()
            self.assertFalse(result2['success'])


class TestCodeExecutionEdgeCases(unittest.TestCase):
    """Additional edge case tests."""

    def setUp(self):
        """Create engine with code execution enabled."""
        self.engine = YAMLEngine(enable_code_execution=True)
        self.registry = self.engine.actions_registry

    def test_empty_code(self):
        """Handle empty code string."""
        result = self.registry['code.execute'](state={}, code="")
        self.assertTrue(result['success'])
        self.assertIsNone(result['return_value'])

    def test_syntax_error(self):
        """Report syntax errors clearly."""
        result = self.registry['code.execute'](
            state={},
            code="if x"  # Invalid syntax
        )
        self.assertFalse(result['success'])
        self.assertIn('SyntaxError', result['error'])

    def test_allowed_builtins_work(self):
        """Verify allowed builtins function correctly."""
        # Math
        result = self.registry['code.execute'](
            state={},
            code="result = abs(-5) + max(1, 2, 3) + min(1, 2, 3)"
        )
        self.assertEqual(result['return_value'], 5 + 3 + 1)

        # Types
        result = self.registry['code.execute'](
            state={},
            code="result = int('42') + len([1, 2, 3])"
        )
        self.assertEqual(result['return_value'], 45)

        # Iteration
        result = self.registry['code.execute'](
            state={},
            code="result = list(range(5))"
        )
        self.assertEqual(result['return_value'], [0, 1, 2, 3, 4])

        # Predicates
        result = self.registry['code.execute'](
            state={},
            code="result = all([True, True]) and any([False, True])"
        )
        self.assertEqual(result['return_value'], True)

    def test_list_operations(self):
        """Test list operations work correctly."""
        result = self.registry['code.execute'](
            state={},
            code="""
data = [3, 1, 4, 1, 5, 9]
sorted_data = sorted(data)
result = {'sorted': sorted_data, 'sum': sum(data)}
"""
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value']['sorted'], [1, 1, 3, 4, 5, 9])
        self.assertEqual(result['return_value']['sum'], 23)

    def test_dict_operations(self):
        """Test dict operations work correctly."""
        result = self.registry['code.execute'](
            state={},
            code="""
d = {'a': 1, 'b': 2}
d['c'] = 3
result = len(d)
"""
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value'], 3)

    def test_string_operations(self):
        """Test string operations work correctly."""
        result = self.registry['code.execute'](
            state={},
            code="""
s = 'hello world'
result = s.upper().split()
"""
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value'], ['HELLO', 'WORLD'])

    def test_exception_handling(self):
        """Test try/except works in sandbox."""
        result = self.registry['code.execute'](
            state={},
            code="""
try:
    x = 1 / 0
except ZeroDivisionError:
    result = 'caught'
"""
        )
        self.assertTrue(result['success'])
        self.assertEqual(result['return_value'], 'caught')

    def test_sandbox_action_errors(self):
        """Test error handling for sandbox actions."""
        # Missing sandbox_id for execute
        result = self.registry['code.sandbox'](
            state={}, action="execute", code="x = 1"
        )
        self.assertFalse(result['success'])
        self.assertIn('sandbox_id', result['error'])

        # Invalid sandbox_id
        result = self.registry['code.sandbox'](
            state={}, action="execute", sandbox_id="nonexistent", code="x = 1"
        )
        self.assertFalse(result['success'])
        self.assertIn('not found', result['error'])

        # Unknown action
        result = self.registry['code.sandbox'](
            state={}, action="invalid_action"
        )
        self.assertFalse(result['success'])
        self.assertIn('Unknown action', result['error'])


if __name__ == '__main__':
    unittest.main()
