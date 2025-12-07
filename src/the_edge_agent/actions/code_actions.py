"""
Code Execution Actions for YAMLEngine (TEA-BUILTIN-003.1).

This module provides sandboxed code execution actions for YAMLEngine workflows.
Uses RestrictedPython for Python code sandboxing with hardened security.

**SECURITY WARNING:** This feature is disabled by default. Only enable for
trusted code patterns. RestrictedPython provides bytecode transformation,
not true process isolation. Do NOT use for arbitrary LLM-generated code.

Actions:
    - code.execute: Execute Python code in RestrictedPython sandbox
    - code.sandbox: Manage persistent sandbox sessions

Example:
    >>> # Enable code execution (must be explicit)
    >>> engine = YAMLEngine(enable_code_execution=True)
    >>> registry = engine.actions_registry
    >>>
    >>> # Execute simple Python code
    >>> result = registry['code.execute'](state={}, code="x = 1 + 2; result = x")
    >>> print(result['return_value'])  # 3
    >>>
    >>> # Persistent sandbox session
    >>> result = registry['code.sandbox'](state={}, action="create")
    >>> sandbox_id = result['sandbox_id']
    >>> result = registry['code.sandbox'](
    ...     state={}, action="execute", sandbox_id=sandbox_id,
    ...     code="counter = 1"
    ... )
    >>> result = registry['code.sandbox'](
    ...     state={}, action="execute", sandbox_id=sandbox_id,
    ...     code="counter += 1; result = counter"
    ... )
    >>> print(result['return_value'])  # 2
"""

import io
import sys
import time
import threading
import signal
import uuid
from contextlib import contextmanager
from typing import Any, Callable, Dict, FrozenSet, List, Optional, Set


# ============================================================================
# RestrictedPython Sandbox Implementation
# ============================================================================

# Builtins whitelist - only safe operations allowed
ALLOWED_BUILTINS: FrozenSet[str] = frozenset([
    # Math
    'abs', 'round', 'min', 'max', 'sum', 'pow', 'divmod',
    # Types
    'int', 'float', 'str', 'bool', 'list', 'dict', 'tuple', 'set', 'frozenset',
    # Iteration
    'len', 'range', 'enumerate', 'zip', 'map', 'filter', 'sorted', 'reversed',
    # Predicates
    'all', 'any', 'isinstance',
    # String
    'chr', 'ord', 'repr',
    # Constants
    'True', 'False', 'None',
    # Exceptions (for try/except)
    'Exception', 'ValueError', 'TypeError', 'KeyError', 'IndexError',
    'AttributeError', 'RuntimeError', 'StopIteration', 'ZeroDivisionError',
])

# Dangerous dunder attributes that enable sandbox escape
BLOCKED_NAMES: FrozenSet[str] = frozenset([
    '__class__', '__bases__', '__mro__', '__subclasses__',
    '__code__', '__globals__', '__builtins__', '__import__',
    '__loader__', '__spec__', '__file__', '__cached__',
    '__dict__', '__slots__', '__weakref__',
    '__reduce__', '__reduce_ex__', '__getstate__', '__setstate__',
    '__init_subclass__', '__set_name__',
])

# Blocked builtins (dangerous operations)
BLOCKED_BUILTINS: FrozenSet[str] = frozenset([
    'exec', 'eval', 'compile', 'open', 'input', 'help',
    'getattr', 'setattr', 'delattr', 'hasattr',
    'globals', 'locals', 'vars', 'dir',
    'type', 'object', 'super',
    '__import__', 'importlib', 'reload',
    'exit', 'quit', 'copyright', 'credits', 'license',
    'memoryview', 'classmethod', 'staticmethod', 'property',
    'breakpoint', 'print',  # print captured separately
])


class RestrictedPythonSandbox:
    """
    Sandboxed Python execution environment using RestrictedPython.

    This class provides a hardened sandbox for executing Python code with:
    - Whitelist-only builtins
    - Blocked dangerous dunder attributes
    - Guarded attribute/item access
    - Timeout enforcement
    - Output capture (stdout/stderr)
    - Output size limits

    **Security Model:**
    - Uses RestrictedPython bytecode transformation
    - Custom guards block attribute traversal attacks
    - No imports allowed (import statement blocked)
    - No file/network/process access (builtins not available)
    - Timeout prevents infinite loops

    **Known Limitations:**
    - Not true process isolation (same Python process)
    - No memory limits (Python resource module unreliable)
    - Escape vectors may exist (defense-in-depth, not absolute)
    """

    def __init__(
        self,
        timeout: float = 30.0,
        max_output_bytes: int = 65536,
    ):
        """
        Initialize the sandbox.

        Args:
            timeout: Maximum execution time in seconds (default: 30)
            max_output_bytes: Maximum output size in bytes (default: 64KB)
        """
        self.timeout = timeout
        self.max_output_bytes = max_output_bytes
        self._locals: Dict[str, Any] = {}  # Persistent locals for session mode

    def _build_safe_builtins(self) -> Dict[str, Any]:
        """Build the safe builtins dictionary."""
        import builtins as python_builtins

        safe = {}
        for name in ALLOWED_BUILTINS:
            if hasattr(python_builtins, name):
                safe[name] = getattr(python_builtins, name)
            elif name == 'True':
                safe[name] = True
            elif name == 'False':
                safe[name] = False
            elif name == 'None':
                safe[name] = None

        return safe

    def _guarded_getattr(self, obj: Any, name: str) -> Any:
        """
        Guarded attribute access - blocks dangerous dunder attributes.

        Args:
            obj: Object to get attribute from
            name: Attribute name

        Returns:
            Attribute value if safe

        Raises:
            AttributeError: If accessing blocked attribute
        """
        if name in BLOCKED_NAMES:
            raise AttributeError(f"Access to '{name}' is not allowed")

        # Block any dunder access except safe ones
        if name.startswith('__') and name.endswith('__'):
            safe_dunders = {'__len__', '__iter__', '__next__', '__getitem__',
                          '__contains__', '__str__', '__repr__', '__bool__',
                          '__eq__', '__ne__', '__lt__', '__le__', '__gt__', '__ge__',
                          '__add__', '__sub__', '__mul__', '__truediv__', '__floordiv__',
                          '__mod__', '__pow__', '__neg__', '__pos__', '__abs__',
                          '__and__', '__or__', '__xor__', '__invert__',
                          '__hash__', '__int__', '__float__'}
            if name not in safe_dunders:
                raise AttributeError(f"Access to '{name}' is not allowed")

        return getattr(obj, name)

    def _guarded_getitem(self, obj: Any, key: Any) -> Any:
        """
        Guarded item access.

        Args:
            obj: Object to get item from
            key: Key/index

        Returns:
            Item value
        """
        return obj[key]

    def _guarded_setitem(self, obj: Any, key: Any, value: Any) -> None:
        """
        Guarded item setting.

        Args:
            obj: Object to set item on
            key: Key/index
            value: Value to set
        """
        obj[key] = value

    def _guarded_delitem(self, obj: Any, key: Any) -> None:
        """
        Guarded item deletion.

        Args:
            obj: Object to delete item from
            key: Key/index
        """
        del obj[key]

    def _guarded_iter(self, obj: Any) -> Any:
        """
        Guarded iteration.

        Args:
            obj: Object to iterate

        Returns:
            Iterator
        """
        return iter(obj)

    def _guarded_write(self, obj: Any) -> Any:
        """
        Guarded write - allows writing to safe container types.

        This is called when code tries to modify an object (e.g. d['key'] = value).
        We return the object if it's a safe mutable type.

        Args:
            obj: Object being written to

        Returns:
            The object if safe to write to

        Raises:
            TypeError: If object type is not allowed
        """
        # Allow writing to safe container types
        if isinstance(obj, (dict, list, set)):
            return obj
        # For print collector, allow write method
        if hasattr(obj, 'write') and callable(getattr(obj, 'write')):
            return obj
        raise TypeError(f"Cannot write to object of type {type(obj).__name__}")

    def _inplacevar(self, op: str, x: Any, y: Any) -> Any:
        """
        Handle in-place operations (+=, -=, etc.).

        Args:
            op: Operation name
            x: Left operand
            y: Right operand

        Returns:
            Result of operation
        """
        ops = {
            '+=': lambda a, b: a + b,
            '-=': lambda a, b: a - b,
            '*=': lambda a, b: a * b,
            '/=': lambda a, b: a / b,
            '//=': lambda a, b: a // b,
            '%=': lambda a, b: a % b,
            '**=': lambda a, b: a ** b,
            '&=': lambda a, b: a & b,
            '|=': lambda a, b: a | b,
            '^=': lambda a, b: a ^ b,
            '>>=': lambda a, b: a >> b,
            '<<=': lambda a, b: a << b,
        }
        if op in ops:
            return ops[op](x, y)
        raise ValueError(f"Unknown in-place operation: {op}")

    @contextmanager
    def _capture_output(self):
        """Context manager to capture stdout and stderr."""
        old_stdout = sys.stdout
        old_stderr = sys.stderr
        stdout_capture = io.StringIO()
        stderr_capture = io.StringIO()

        try:
            sys.stdout = stdout_capture
            sys.stderr = stderr_capture
            yield stdout_capture, stderr_capture
        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr

    def _truncate_output(self, text: str) -> str:
        """Truncate output to max_output_bytes."""
        if len(text.encode('utf-8')) > self.max_output_bytes:
            # Find a safe truncation point
            truncated = text.encode('utf-8')[:self.max_output_bytes].decode('utf-8', errors='ignore')
            return truncated + "\n... [output truncated]"
        return text

    def execute(
        self,
        code: str,
        timeout: Optional[float] = None,
        **extra_globals
    ) -> Dict[str, Any]:
        """
        Execute Python code in the sandbox.

        Args:
            code: Python source code to execute
            timeout: Override default timeout (seconds)
            **extra_globals: Additional safe globals to inject

        Returns:
            {
                "success": bool,
                "stdout": str,
                "stderr": str,
                "return_value": any,  # Value of 'result' variable if set
                "error": Optional[str],
                "execution_time_ms": float
            }
        """
        try:
            from RestrictedPython import compile_restricted, safe_globals
        except ImportError:
            return {
                "success": False,
                "stdout": "",
                "stderr": "",
                "return_value": None,
                "error": "RestrictedPython not installed. Install with: pip install RestrictedPython",
                "execution_time_ms": 0.0
            }

        timeout = timeout or self.timeout
        start_time = time.time()

        # Compile with RestrictedPython
        try:
            byte_code = compile_restricted(
                code,
                filename='<sandbox>',
                mode='exec'
            )
        except SyntaxError as e:
            return {
                "success": False,
                "stdout": "",
                "stderr": "",
                "return_value": None,
                "error": f"SyntaxError: {e.msg} at line {e.lineno}",
                "execution_time_ms": (time.time() - start_time) * 1000
            }
        except Exception as e:
            return {
                "success": False,
                "stdout": "",
                "stderr": "",
                "return_value": None,
                "error": f"CompilationError: {str(e)}",
                "execution_time_ms": (time.time() - start_time) * 1000
            }

        # Check for compilation errors (RestrictedPython returns None on error)
        if byte_code is None:
            return {
                "success": False,
                "stdout": "",
                "stderr": "",
                "return_value": None,
                "error": "Code contains restricted operations (imports, dangerous constructs)",
                "execution_time_ms": (time.time() - start_time) * 1000
            }

        # Build execution environment
        safe_builtins = self._build_safe_builtins()

        # Create print collector for capturing output
        captured_prints: List[str] = []

        class SandboxPrintCollector:
            """PrintCollector-compatible class for RestrictedPython."""

            def __init__(self, _getattr_=None):
                self.txt: List[str] = []
                self._getattr_ = _getattr_

            def write(self, text: str) -> None:
                self.txt.append(text)
                captured_prints.append(text)

            def __call__(self) -> str:
                return ''.join(self.txt)

            def _call_print(self, *objects, **kwargs) -> None:
                # Remove 'file' from kwargs if present (we capture to self)
                kwargs.pop('file', None)
                # Convert objects to string and write
                sep = kwargs.get('sep', ' ')
                end = kwargs.get('end', '\n')
                output = sep.join(str(obj) for obj in objects) + end
                self.write(output)

        def print_factory(_getattr_=None):
            """Factory function for creating PrintCollector instances."""
            return SandboxPrintCollector(_getattr_)

        # Build globals with guards
        exec_globals = {
            '__builtins__': safe_builtins,
            '_getattr_': self._guarded_getattr,
            '_getitem_': self._guarded_getitem,
            '_setitem_': self._guarded_setitem,
            '_delitem_': self._guarded_delitem,
            '_getiter_': self._guarded_iter,
            '_iter_unpack_sequence_': lambda seq, expected: list(seq),
            '_unpack_sequence_': lambda seq, expected: list(seq),
            '_write_': self._guarded_write,
            '_inplacevar_': self._inplacevar,
            '_print_': print_factory,
            '_apply_': lambda func, *args, **kwargs: func(*args, **kwargs),
        }

        # Add persistent locals from session
        exec_globals.update(self._locals)

        # Add extra safe globals
        for key, value in extra_globals.items():
            if not key.startswith('_'):
                exec_globals[key] = value

        exec_locals: Dict[str, Any] = {}

        # Execute with timeout
        result = {
            "success": False,
            "stdout": "",
            "stderr": "",
            "return_value": None,
            "error": None,
            "execution_time_ms": 0.0
        }

        execution_error = [None]
        execution_done = threading.Event()

        def run_code():
            try:
                with self._capture_output() as (stdout, stderr):
                    exec(byte_code, exec_globals, exec_locals)

                # Get captured output
                stdout_text = stdout.getvalue()
                stderr_text = stderr.getvalue()

                # Add print() output to stdout
                if captured_prints:
                    stdout_text = ''.join(captured_prints) + (stdout_text if stdout_text else '')

                result["stdout"] = self._truncate_output(stdout_text)
                result["stderr"] = self._truncate_output(stderr_text)

                # Get return value (look for 'result' variable)
                if 'result' in exec_locals:
                    result["return_value"] = exec_locals['result']
                elif 'result' in exec_globals:
                    result["return_value"] = exec_globals['result']

                result["success"] = True

                # Update persistent locals (for session mode)
                for key, value in exec_locals.items():
                    if not key.startswith('_'):
                        self._locals[key] = value
                for key, value in exec_globals.items():
                    if not key.startswith('_') and key not in ('__builtins__',):
                        if key not in ['_getattr_', '_getitem_', '_setitem_',
                                      '_delitem_', '_getiter_', '_write_',
                                      '_inplacevar_', '_print_', '_apply_',
                                      '_iter_unpack_sequence_', '_unpack_sequence_']:
                            self._locals[key] = value

            except Exception as e:
                execution_error[0] = str(e)
            finally:
                execution_done.set()

        # Run in thread with timeout
        thread = threading.Thread(target=run_code, daemon=True)
        thread.start()
        thread.join(timeout=timeout)

        execution_time_ms = (time.time() - start_time) * 1000
        result["execution_time_ms"] = execution_time_ms

        if thread.is_alive():
            # Timeout occurred
            result["error"] = f"Execution timed out after {timeout} seconds"
            result["success"] = False
            return result

        if execution_error[0]:
            result["error"] = execution_error[0]
            result["success"] = False
            return result

        return result

    def reset(self) -> None:
        """Reset the sandbox state (clear persistent locals)."""
        self._locals.clear()


# ============================================================================
# Action Registration
# ============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register code execution actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources

    Note:
        Code execution is DISABLED by default. The engine must have
        `enable_code_execution=True` to enable these actions.
    """
    # Sandbox session storage (attached to engine for persistence)
    if not hasattr(engine, '_sandbox_sessions'):
        engine._sandbox_sessions: Dict[str, RestrictedPythonSandbox] = {}

    def code_execute(
        state: Dict[str, Any],
        code: str,
        timeout: float = 30.0,
        max_output_bytes: int = 65536,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Execute Python code in a RestrictedPython sandbox.

        This action provides sandboxed code execution with:
        - Whitelist-only builtins (safe math, types, iteration)
        - No imports, file access, or network access
        - Timeout enforcement
        - Output capture (stdout/stderr)
        - Output size limits

        Args:
            state: Current state dictionary
            code: Python source code to execute
            timeout: Maximum execution time in seconds (default: 30)
            max_output_bytes: Maximum output size in bytes (default: 64KB)

        Returns:
            {
                "success": bool,
                "stdout": str,
                "stderr": str,
                "return_value": any,
                "error": Optional[str],
                "execution_time_ms": float
            }

        Example:
            >>> result = code_execute({}, code="x = 1 + 2; result = x * 10")
            >>> result['return_value']  # 30

        Security Notes:
            - Set `result` variable to return a value
            - Use print() for stdout output
            - Imports are blocked
            - File/network/system access blocked
        """
        # Check if code execution is enabled
        if not getattr(engine, '_enable_code_execution', False):
            return {
                "success": False,
                "stdout": "",
                "stderr": "",
                "return_value": None,
                "error": "Code execution is disabled. Enable with YAMLEngine(enable_code_execution=True)",
                "execution_time_ms": 0.0
            }

        sandbox = RestrictedPythonSandbox(
            timeout=timeout,
            max_output_bytes=max_output_bytes
        )

        return sandbox.execute(code)

    registry['code.execute'] = code_execute
    registry['actions.code_execute'] = code_execute

    def code_sandbox(
        state: Dict[str, Any],
        action: str,
        sandbox_id: Optional[str] = None,
        code: Optional[str] = None,
        timeout: float = 30.0,
        max_output_bytes: int = 65536,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Manage persistent sandbox sessions for multi-step code execution.

        This action allows creating sandbox sessions that persist state
        between executions. Variables defined in one execution are
        available in subsequent executions within the same session.

        Args:
            state: Current state dictionary
            action: One of "create", "execute", "destroy", "list"
            sandbox_id: Session ID (required for execute/destroy)
            code: Python code (required for execute)
            timeout: Execution timeout in seconds (default: 30)
            max_output_bytes: Max output size in bytes (default: 64KB)

        Returns:
            For action="create":
                {"sandbox_id": str, "created": True, "success": True}

            For action="execute":
                {"success": bool, "stdout": str, "stderr": str,
                 "return_value": any, "error": str, "execution_time_ms": float}

            For action="destroy":
                {"destroyed": bool, "sandbox_id": str, "success": True}

            For action="list":
                {"sandboxes": List[str], "count": int, "success": True}

        Example:
            >>> # Create session
            >>> result = code_sandbox({}, action="create")
            >>> sid = result['sandbox_id']
            >>>
            >>> # Execute with state persistence
            >>> code_sandbox({}, action="execute", sandbox_id=sid, code="x = 10")
            >>> result = code_sandbox({}, action="execute", sandbox_id=sid, code="result = x + 5")
            >>> result['return_value']  # 15
            >>>
            >>> # Cleanup
            >>> code_sandbox({}, action="destroy", sandbox_id=sid)
        """
        # Check if code execution is enabled
        if not getattr(engine, '_enable_code_execution', False):
            return {
                "success": False,
                "error": "Code execution is disabled. Enable with YAMLEngine(enable_code_execution=True)"
            }

        sessions = engine._sandbox_sessions

        if action == "create":
            new_id = str(uuid.uuid4())[:8]
            sessions[new_id] = RestrictedPythonSandbox(
                timeout=timeout,
                max_output_bytes=max_output_bytes
            )
            return {
                "sandbox_id": new_id,
                "created": True,
                "success": True
            }

        elif action == "execute":
            if not sandbox_id:
                return {
                    "success": False,
                    "error": "sandbox_id is required for execute action"
                }
            if sandbox_id not in sessions:
                return {
                    "success": False,
                    "error": f"Sandbox '{sandbox_id}' not found"
                }
            if not code:
                return {
                    "success": False,
                    "error": "code is required for execute action"
                }

            sandbox = sessions[sandbox_id]
            # Update timeout/max_output if provided
            sandbox.timeout = timeout
            sandbox.max_output_bytes = max_output_bytes

            return sandbox.execute(code)

        elif action == "destroy":
            if not sandbox_id:
                return {
                    "success": False,
                    "error": "sandbox_id is required for destroy action"
                }
            if sandbox_id in sessions:
                del sessions[sandbox_id]
                return {
                    "destroyed": True,
                    "sandbox_id": sandbox_id,
                    "success": True
                }
            else:
                return {
                    "destroyed": False,
                    "sandbox_id": sandbox_id,
                    "success": True,
                    "error": f"Sandbox '{sandbox_id}' not found"
                }

        elif action == "list":
            return {
                "sandboxes": list(sessions.keys()),
                "count": len(sessions),
                "success": True
            }

        else:
            return {
                "success": False,
                "error": f"Unknown action: {action}. Use 'create', 'execute', 'destroy', or 'list'"
            }

    registry['code.sandbox'] = code_sandbox
    registry['actions.code_sandbox'] = code_sandbox
