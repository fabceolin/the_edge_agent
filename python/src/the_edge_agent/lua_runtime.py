"""
Lua runtime integration via lupa (LuaJIT bindings).

Provides Lua scripting support for YAML agents with:
- Inline code execution in nodes
- Conditional edge expressions
- Timeout protection for runaway scripts

Mirrors the Rust implementation in rust/src/engine/lua_runtime.rs for cross-runtime
compatibility - YAML agents with Lua code should work identically in both runtimes.

## Timeout Protection

All execution methods support configurable timeouts to prevent runaway scripts:

```python
from the_edge_agent.lua_runtime import LuaRuntime

# Create runtime with 5 second timeout
runtime = LuaRuntime(timeout=5.0)

# Infinite loops will be terminated with an error
try:
    result = runtime.execute("while true do end", {})
except LuaTimeoutError:
    print("Script timed out!")
```

### How It Works

Timeout protection uses a threading watchdog approach:
1. A watchdog thread sets a stop flag after the timeout duration
2. A debug hook is registered that runs every 1000 Lua instructions
3. The hook checks if the stop flag is set
4. When timeout is detected, execution stops with LuaTimeoutError

### Known Limitations

**The timeout hook cannot interrupt long-running C library calls.**

This is a fundamental limitation of Lua's debug hook mechanism. Examples include:
- Pattern matching on very large strings (string.find, string.match)
- Operations in C-based Lua libraries

For most pure Lua code (loops, computations, function calls), the timeout
will reliably terminate execution within a reasonable margin of the configured time.
"""

import threading
import time
from typing import Any, Dict, List, Optional, Union

# Lazy import with availability flag
LUPA_AVAILABLE = False
_lupa = None

try:
    import lupa
    _lupa = lupa
    LUPA_AVAILABLE = True
except ImportError:
    pass


class LuaRuntimeError(Exception):
    """Exception raised for Lua syntax or runtime errors."""
    pass


class LuaTimeoutError(Exception):
    """Exception raised when Lua execution timeout is exceeded."""
    pass


def _ensure_lupa_installed():
    """Raise ImportError with installation instructions if lupa is not installed."""
    if not LUPA_AVAILABLE:
        raise ImportError(
            "Lua runtime requires the 'lupa' package.\n"
            "Install it with: pip install 'the_edge_agent[lua]'\n"
            "Or directly: pip install lupa>=2.0"
        )


class LuaRuntime:
    """
    Lua runtime for The Edge Agent.

    Provides a sandboxed Lua environment with timeout protection,
    matching the Rust implementation API for cross-runtime compatibility.

    Args:
        timeout: Maximum execution time in seconds (default: 30.0)

    Example:
        >>> runtime = LuaRuntime()
        >>> result = runtime.execute("return state.value + 1", {"value": 41})
        >>> print(result)  # 42
    """

    def __init__(self, timeout: float = 30.0):
        """
        Initialize the Lua runtime.

        Args:
            timeout: Maximum execution time in seconds (default: 30.0)
        """
        _ensure_lupa_installed()

        self.timeout = timeout
        self._lua = _lupa.LuaRuntime(unpack_returned_tuples=True)
        self._setup_timeout_hook()
        self._sandbox_lua()
        self._lock = threading.Lock()
        self._timeout_flag = False
        self._start_time = 0.0

    def _setup_timeout_hook(self) -> None:
        """Set up Lua debug hook for timeout checking."""
        # We need to capture 'self' for the callback but also keep debug.sethook
        # accessible before sandboxing. We'll inject a Python callback that
        # Lua can call to check timeout.

        # Store reference to debug.sethook before sandboxing
        self._debug_sethook = self._lua.eval('debug.sethook')
        self._debug_gethook = self._lua.eval('debug.gethook')

        # Create a Python function that checks timeout
        def check_timeout():
            if self._timeout_flag:
                return True
            if self._start_time > 0:
                elapsed = time.time() - self._start_time
                if elapsed >= self.timeout:
                    self._timeout_flag = True
                    return True
            return False

        # Make check_timeout available in Lua
        self._lua.globals()['__tea_check_timeout'] = check_timeout

        # Create Lua hook function that will be called every N instructions
        self._hook_code = """
            function(event, line)
                if __tea_check_timeout() then
                    error('execution timeout')
                end
            end
        """

    def _sandbox_lua(self) -> None:
        """Remove dangerous Lua globals for security."""
        # Note: We keep debug.sethook reference but remove the debug table
        dangerous = ['os', 'io', 'loadfile', 'dofile', 'debug']
        globals_table = self._lua.globals()
        for name in dangerous:
            globals_table[name] = None

    def _python_to_lua(self, value: Any) -> Any:
        """
        Convert Python value to Lua-compatible value.

        Type mapping (matches Rust implementation):
        - None -> nil
        - bool -> boolean
        - int/float -> number
        - str -> string
        - list -> table (array, 1-indexed)
        - dict -> table (object)

        Args:
            value: Python value to convert

        Returns:
            Lua-compatible value
        """
        if value is None:
            return None
        elif isinstance(value, bool):
            return value
        elif isinstance(value, (int, float)):
            return value
        elif isinstance(value, str):
            return value
        elif isinstance(value, list):
            # Create Lua table for array
            table = self._lua.table()
            for i, item in enumerate(value):
                table[i + 1] = self._python_to_lua(item)  # Lua is 1-indexed
            return table
        elif isinstance(value, dict):
            # Create Lua table for object
            table = self._lua.table()
            for k, v in value.items():
                table[k] = self._python_to_lua(v)
            return table
        else:
            # Try to convert to string for unknown types
            return str(value)

    def _is_lua_table(self, value: Any) -> bool:
        """Check if value is a Lua table."""
        # Check by type name rather than isinstance to handle module variations
        type_name = type(value).__name__
        return type_name == '_LuaTable' or 'LuaTable' in type_name

    def _lua_to_python(self, value: Any) -> Any:
        """
        Convert Lua value to Python value.

        Type mapping (matches Rust implementation):
        - nil -> None
        - boolean -> bool
        - number -> int or float
        - string -> str
        - table (array) -> list
        - table (object) -> dict

        Args:
            value: Lua value to convert

        Returns:
            Python value
        """
        if value is None:
            return None
        elif isinstance(value, bool):
            return value
        elif isinstance(value, (int, float)):
            # Preserve integers when possible
            if isinstance(value, float) and value.is_integer():
                return int(value)
            return value
        elif isinstance(value, str):
            return value
        elif self._is_lua_table(value):
            # Check if it's an array (sequential integer keys starting from 1)
            # by testing if key 1 exists and there are no string keys
            is_array = True
            max_index = 0

            # First pass: check for array pattern
            for key in value.keys():
                if isinstance(key, (int, float)):
                    int_key = int(key)
                    if int_key > 0:
                        max_index = max(max_index, int_key)
                    else:
                        is_array = False
                        break
                else:
                    is_array = False
                    break

            if is_array and max_index > 0:
                # It's an array
                result = []
                for i in range(1, max_index + 1):
                    lua_val = value[i]
                    result.append(self._lua_to_python(lua_val))
                return result
            else:
                # It's an object
                result = {}
                for key in value.keys():
                    str_key = str(key) if not isinstance(key, str) else key
                    result[str_key] = self._lua_to_python(value[key])
                return result
        else:
            # Unknown type - try to convert to appropriate Python type
            return value

    def execute(self, code: str, state: Dict[str, Any]) -> Any:
        """
        Execute Lua code with state access.

        The state is available as a global 'state' table in Lua.
        Returns the result of the code execution (typically via return statement).

        Args:
            code: Lua code to execute
            state: State dictionary accessible as 'state' in Lua

        Returns:
            Result of code execution (JSON-serializable)

        Raises:
            LuaRuntimeError: If Lua code has syntax or runtime errors
            LuaTimeoutError: If execution exceeds timeout
        """
        return self._execute_with_timeout(
            lambda: self._execute_impl(code, state)
        )

    def _execute_impl(self, code: str, state: Dict[str, Any]) -> Any:
        """Internal implementation of execute without timeout wrapper."""
        with self._lock:
            try:
                # Set state as global
                state_lua = self._python_to_lua(state)
                self._lua.globals()['state'] = state_lua

                # Execute code
                result = self._lua.execute(code)

                # Convert result back to Python
                return self._lua_to_python(result)
            except Exception as e:
                error_msg = str(e)
                if 'execution timeout' in error_msg.lower():
                    raise LuaTimeoutError(error_msg) from e
                raise LuaRuntimeError(error_msg) from e

    def eval_condition(self, expression: str, state: Dict[str, Any]) -> Optional[str]:
        """
        Evaluate a condition expression.

        The expression should return a string matching an edge target name.
        If it returns nil, returns None (for default edge handling).
        If it returns a boolean, returns "true" or "false".

        Args:
            expression: Lua expression to evaluate
            state: State dictionary accessible as 'state' in Lua

        Returns:
            String result for routing, or None for default edge

        Raises:
            LuaRuntimeError: If expression has syntax or runtime errors
            LuaTimeoutError: If evaluation exceeds timeout
        """
        return self._execute_with_timeout(
            lambda: self._eval_condition_impl(expression, state)
        )

    def _eval_condition_impl(self, expression: str, state: Dict[str, Any]) -> Optional[str]:
        """Internal implementation of eval_condition without timeout wrapper."""
        with self._lock:
            try:
                # Set state as global
                state_lua = self._python_to_lua(state)
                self._lua.globals()['state'] = state_lua

                # Wrap expression in return statement if needed
                code = expression.strip()
                if not code.startswith('return'):
                    code = f'return {code}'

                # Execute expression
                result = self._lua.execute(code)

                # Convert result
                if result is None:
                    return None
                elif isinstance(result, bool):
                    return "true" if result else "false"
                elif isinstance(result, str):
                    return result
                else:
                    raise LuaRuntimeError(
                        f"Condition must return string, boolean, or nil, got: {type(result).__name__}"
                    )
            except Exception as e:
                error_msg = str(e)
                if 'execution timeout' in error_msg.lower():
                    raise LuaTimeoutError(error_msg) from e
                if isinstance(e, (LuaRuntimeError, LuaTimeoutError)):
                    raise
                raise LuaRuntimeError(error_msg) from e

    def execute_node_code(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute inline Lua code for a node.

        The code receives 'state' as a local variable (passed as argument).
        Should return a table/dict with state updates.

        Args:
            code: Lua code to execute
            state: Current node state

        Returns:
            State updates as dictionary

        Raises:
            LuaRuntimeError: If code has syntax or runtime errors
            LuaTimeoutError: If execution exceeds timeout
        """
        return self._execute_with_timeout(
            lambda: self._execute_node_code_impl(code, state)
        )

    def _execute_node_code_impl(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
        """Internal implementation of execute_node_code without timeout wrapper."""
        with self._lock:
            try:
                # Wrap code in a function that receives state and returns updated state
                # This matches Rust implementation
                wrapped = f"""
                    local state = ...
                    {code}
                """

                # Create state argument
                state_lua = self._python_to_lua(state)

                # Load and call the function
                func = self._lua.eval(f"function(...) {wrapped} end")
                result = func(state_lua)

                # Convert result back to Python
                python_result = self._lua_to_python(result)

                # Ensure we return a dict
                if python_result is None:
                    return {}
                elif isinstance(python_result, dict):
                    return python_result
                else:
                    return {"result": python_result}
            except Exception as e:
                error_msg = str(e)
                if 'execution timeout' in error_msg.lower():
                    raise LuaTimeoutError(error_msg) from e
                raise LuaRuntimeError(error_msg) from e

    def _enable_timeout_hook(self) -> None:
        """Enable the timeout hook before execution."""
        self._timeout_flag = False
        self._start_time = time.time()

        # Install the debug hook - check every 1000 instructions
        hook_func = self._lua.eval(self._hook_code)
        # Call debug.sethook(hook, "", 1000) - "" means no events, just count
        self._debug_sethook(hook_func, "", 1000)

    def _disable_timeout_hook(self) -> None:
        """Disable the timeout hook after execution."""
        self._start_time = 0.0
        # Clear the hook
        self._debug_sethook(None)

    def _execute_with_timeout(self, func):
        """
        Execute a function with timeout protection.

        Uses Lua's debug.sethook for instruction counting:
        1. Set up a debug hook that checks every 1000 Lua instructions
        2. The hook calls a Python function to check elapsed time
        3. When timeout is detected, the hook raises a Lua error

        Args:
            func: Function to execute

        Returns:
            Result of the function

        Raises:
            LuaTimeoutError: If execution exceeds timeout
        """
        self._enable_timeout_hook()

        try:
            # Execute the function
            result = func()

            # Check if timeout occurred during execution
            if self._timeout_flag:
                raise LuaTimeoutError("execution timeout")

            return result
        except Exception as e:
            error_msg = str(e).lower()
            if self._timeout_flag or 'timeout' in error_msg or 'execution timeout' in error_msg:
                raise LuaTimeoutError("execution timeout") from e
            raise
        finally:
            self._disable_timeout_hook()

    def __repr__(self) -> str:
        return f"LuaRuntime(timeout={self.timeout})"


# Convenience function to detect Lua code in a string
def detect_lua_code(code: str) -> bool:
    """
    Detect if code block is Lua (vs Python/Jinja2).

    Detection rules:
    1. Explicit marker: code starts with '-- lua' or '--lua'
    2. Heuristic: contains Lua-specific keywords not valid in Python

    Args:
        code: Code string to check

    Returns:
        True if code appears to be Lua
    """
    import re

    # Strip leading whitespace for marker check
    stripped = code.strip()

    # Explicit marker
    if stripped.startswith('-- lua') or stripped.startswith('--lua'):
        return True

    # Heuristic: Lua keywords not valid in Python
    lua_patterns = [
        r'\blocal\b',      # local variable declaration
        r'\bthen\b',       # if-then
        r'\bend\b',        # block terminator
        r'\belseif\b',     # Lua uses elseif, Python uses elif
        r'\.\.+',          # string concatenation operator (.. or more dots)
    ]

    return any(re.search(pattern, code) for pattern in lua_patterns)
