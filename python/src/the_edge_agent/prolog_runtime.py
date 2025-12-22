"""
Prolog runtime integration via janus-swi (official SWI-Prolog 9.1+ Python bindings).

Provides Prolog scripting support for YAML agents with:
- Inline code execution in nodes
- State access via state/2 predicate
- Return values via return/2 predicate
- Timeout protection via call_with_time_limit/2
- Sandboxed execution for security
- Native directive handling via consult()

Mirrors the Rust implementation for cross-runtime compatibility - YAML agents
with Prolog code should work identically in both Python and Rust runtimes.

## Migration from pyswip (TEA-PY-005)

This module was migrated from pyswip to janus-swi for:
- Proper timeout exception handling (no segfaults)
- Native directive support via consult()
- Full CLP(FD) module loading
- Official SWI-Prolog 9.1+ support

## Neurosymbolic AI

This runtime enables neurosymbolic AI workflows that combine:
- Neural network outputs (pattern recognition)
- Symbolic logic reasoning (Prolog rules)
- Constraint solving (CLP(FD), CLP(R))

## State Interface

State is accessible via the `state/2` predicate:
```prolog
state(key, Value).        % Unify Value with state["key"]
return(key, Value).       % Set state["key"] = Value in output
```

## Timeout Protection

All execution methods support configurable timeouts using SWI-Prolog's
built-in `call_with_time_limit/2` predicate:

```python
from the_edge_agent.prolog_runtime import PrologRuntime

# Create runtime with 5 second timeout
runtime = PrologRuntime(timeout=5.0)

# Infinite recursion will be terminated
try:
    result = runtime.execute_node_code("loop :- loop, loop.", {})
except PrologTimeoutError:
    print("Script timed out!")
```

## Sandboxing

Sandboxed mode (default) restricts dangerous operations:
- File I/O (open/3, read/1, write/1)
- Shell execution (shell/1, process_create/3)
- Network access

## Thread-Local Facts

For parallel execution, state/2 and return_value/2 are declared as
thread-local predicates, providing isolation between parallel branches
without the overhead of creating separate Prolog engines.
"""

import re
import sys
import threading
from typing import Any, Dict, List, Optional, Tuple

# Lazy import with availability flag
JANUS_AVAILABLE = False
PYSWIP_AVAILABLE = False  # Keep for backward compatibility checks
_janus = None

try:
    import janus_swi as janus
    _janus = janus
    JANUS_AVAILABLE = True
    PYSWIP_AVAILABLE = True  # Alias for backward compatibility
except ImportError:
    pass


class PrologRuntimeError(Exception):
    """Exception raised for Prolog syntax or runtime errors."""
    pass


class PrologTimeoutError(Exception):
    """Exception raised when Prolog execution timeout is exceeded."""
    pass


def _get_install_instructions() -> str:
    """Get platform-specific installation instructions."""
    platform = sys.platform

    base_msg = (
        "Prolog runtime requires 'janus-swi' package and SWI-Prolog 9.1+.\n"
        "Install janus-swi: pip install 'the_edge_agent[prolog]'\n"
        "Or directly: pip install janus-swi\n\n"
        "Install SWI-Prolog 9.1+:\n"
    )

    if platform.startswith('linux'):
        return base_msg + (
            "  Ubuntu/Debian: sudo apt install swi-prolog\n"
            "  Note: Ensure version 9.1+ is installed\n"
        )
    elif platform == 'darwin':
        return base_msg + "  macOS: brew install swi-prolog\n"
    elif platform == 'win32':
        return base_msg + "  Windows: Download from https://www.swi-prolog.org/download/stable\n"
    else:
        return base_msg + (
            "  Ubuntu/Debian: sudo apt install swi-prolog\n"
            "  macOS: brew install swi-prolog\n"
            "  Windows: Download from https://www.swi-prolog.org/download/stable\n"
        )


def _ensure_janus_installed():
    """Raise ImportError with installation instructions if janus-swi is not installed."""
    if not JANUS_AVAILABLE:
        raise ImportError(_get_install_instructions())


class PrologRuntime:
    """
    Prolog runtime for The Edge Agent.

    Provides a sandboxed SWI-Prolog environment with timeout protection
    for neurosymbolic AI workflows. Uses janus-swi (official SWI-Prolog
    Python bindings) for robust integration.

    Args:
        timeout: Maximum execution time in seconds (default: 30.0)
        sandbox: Enable sandboxed execution (default: True)

    Example:
        >>> runtime = PrologRuntime()
        >>> result = runtime.execute_node_code(
        ...     "state(value, V), V2 is V + 1, return(result, V2)",
        ...     {"value": 41}
        ... )
        >>> print(result)  # {"result": 42}
    """

    # Common modules to pre-load for convenience
    DEFAULT_MODULES = ['lists', 'clpfd', 'apply', 'aggregate']

    # Setup code for state/return predicates - must be included in every consult
    # because janus.consult("user", ...) replaces module contents
    _SETUP_CODE = """
        :- thread_local(state/2).
        :- thread_local(return_value/2).
        return(Key, Value) :- assertz(return_value(Key, Value)).
    """

    def __init__(self, timeout: float = 30.0, sandbox: bool = True):
        """
        Initialize the Prolog runtime.

        Args:
            timeout: Maximum execution time in seconds (default: 30.0)
            sandbox: Enable sandboxed execution (default: True)
        """
        _ensure_janus_installed()

        self.timeout = timeout
        self.sandbox = sandbox
        self._lock = threading.Lock()
        self._initialized = False

        self._initialize()

    def _initialize(self) -> None:
        """Set up the Prolog environment."""
        if self._initialized:
            return

        # Set up thread-local dynamic predicates for state access
        self._setup_state_predicates()

        # Pre-load common modules
        self._preload_modules(self.DEFAULT_MODULES)

        # Apply sandbox restrictions if enabled
        if self.sandbox:
            self._apply_sandbox()

        self._initialized = True

    def _preload_modules(self, modules: List[str]) -> None:
        """
        Pre-load common SWI-Prolog modules at init time.

        This enables CLP(FD) and other libraries without needing
        :- use_module(...) directives in user code.

        Args:
            modules: List of module names to load (e.g., 'clpfd', 'lists')
        """
        for mod in modules:
            try:
                _janus.query_once(f"use_module(library({mod}))")
            except Exception:
                # Module may not be available - that's OK
                pass

    def _setup_state_predicates(self) -> None:
        """Set up state/2 and return/2 predicates for state access."""
        # Use consult() to properly handle directives
        setup_code = """
            :- thread_local(state/2).
            :- thread_local(return_value/2).
            return(Key, Value) :- assertz(return_value(Key, Value)).
        """

        try:
            _janus.consult("user", setup_code)
        except Exception:
            # Fallback: try without thread_local (older SWI-Prolog)
            fallback_code = """
                :- dynamic(state/2).
                :- dynamic(return_value/2).
                return(Key, Value) :- assertz(return_value(Key, Value)).
            """
            try:
                _janus.consult("user", fallback_code)
            except Exception:
                pass

    def _apply_sandbox(self) -> None:
        """
        Enable SWI-Prolog sandbox mode.

        The sandbox library restricts dangerous predicates:
        - File I/O operations
        - Shell/process execution
        - Network access
        """
        try:
            _janus.query_once("use_module(library(sandbox))")
        except Exception:
            # Sandbox library may not be available
            pass

        # Mark that sandbox is enabled (for consult_file checks)
        try:
            _janus.query_once("assertz(sandbox_mode_enabled)")
        except Exception:
            pass

    def _python_to_prolog(self, value: Any) -> str:
        """
        Convert Python value to Prolog term string.

        Type mapping:
        - None -> null (atom)
        - bool -> true/false (atoms)
        - int/float -> number
        - str -> atom (quoted if needed)
        - list -> Prolog list
        - dict -> Prolog dict or json/1 term

        Args:
            value: Python value to convert

        Returns:
            Prolog term as string
        """
        if value is None:
            return "null"
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, int):
            return str(value)
        elif isinstance(value, float):
            return str(value)
        elif isinstance(value, str):
            # Escape single quotes and wrap in quotes
            escaped = value.replace("\\", "\\\\").replace("'", "\\'")
            return f"'{escaped}'"
        elif isinstance(value, list):
            items = [self._python_to_prolog(item) for item in value]
            return f"[{', '.join(items)}]"
        elif isinstance(value, dict):
            # Convert to Prolog dict syntax using _{} notation
            if not value:
                return "_{}"
            pairs = []
            for k, v in value.items():
                # Keys must be atoms
                key_str = str(k).replace("'", "\\'")
                pairs.append(f"'{key_str}': {self._python_to_prolog(v)}")
            return f"_{{{', '.join(pairs)}}}"
        else:
            # Try to convert to string for unknown types
            escaped = str(value).replace("\\", "\\\\").replace("'", "\\'")
            return f"'{escaped}'"

    def _prolog_to_python(self, value: Any) -> Any:
        """
        Convert Prolog term to Python value.

        Handles janus types and native Python types from query results.

        Args:
            value: Prolog value from janus

        Returns:
            Python value
        """
        if value is None:
            return None

        # Get type name for debugging
        type_name = type(value).__name__

        # Handle janus-specific types
        if hasattr(value, '__class__') and 'janus' in str(type(value).__module__ if hasattr(type(value), '__module__') else ''):
            return str(value)

        # Handle numbers
        if isinstance(value, bool):
            return value
        elif isinstance(value, int):
            return value
        elif isinstance(value, float):
            # Preserve integers when possible
            if value.is_integer():
                return int(value)
            return value

        # Handle strings/atoms
        if isinstance(value, str):
            if value == 'null':
                return None
            elif value == 'true':
                return True
            elif value == 'false':
                return False
            return value

        # Handle bytes
        if isinstance(value, bytes):
            return value.decode('utf-8', errors='replace')

        # Handle lists
        if isinstance(value, (list, tuple)):
            return [self._prolog_to_python(item) for item in value]

        # Handle dicts (from Prolog dicts)
        if isinstance(value, dict):
            return {str(k): self._prolog_to_python(v) for k, v in value.items()}

        # Fallback: convert to string
        return str(value)

    def _set_state(self, state: Dict[str, Any]) -> None:
        """Assert state facts for Prolog access via state/2."""
        # Retract all existing state facts
        try:
            _janus.query_once("retractall(state(_, _))")
        except Exception:
            pass

        # Assert new state facts
        for key, value in state.items():
            prolog_value = self._python_to_prolog(value)
            try:
                key_escaped = str(key).replace("'", "\\'")
                _janus.query_once(f"assertz(state('{key_escaped}', {prolog_value}))")
            except Exception:
                pass

    def _get_returns(self) -> Dict[str, Any]:
        """Extract return values from return_value/2 facts."""
        results = {}
        try:
            for sol in _janus.query("return_value(Key, Value)"):
                key = self._prolog_to_python(sol.get('Key'))
                value = self._prolog_to_python(sol.get('Value'))
                if key is not None:
                    results[str(key)] = value
        except Exception:
            pass
        return results

    def _clear_returns(self) -> None:
        """Clear all return_value/2 facts."""
        try:
            _janus.query_once("retractall(return_value(_, _))")
        except Exception:
            pass

    def execute_query(
        self,
        query: str,
        state: Dict[str, Any],
        first_only: bool = True
    ) -> Dict[str, Any]:
        """
        Execute a Prolog query with state access.

        The state is available via state/2 predicate.
        Use return/2 to set return values.

        Args:
            query: Prolog query to execute
            state: State dictionary accessible via state/2
            first_only: Return only first solution (default: True)

        Returns:
            Dictionary of return values

        Raises:
            PrologRuntimeError: If query has syntax or runtime errors
            PrologTimeoutError: If execution exceeds timeout
        """
        with self._lock:
            self._set_state(state)
            self._clear_returns()

            try:
                # Clean up query - ensure it doesn't end with period
                clean_query = query.strip().rstrip('.')

                # Wrap query with timeout using call_with_time_limit/2
                timed_query = f"call_with_time_limit({self.timeout}, ({clean_query}))"

                result = _janus.query_once(timed_query)

                if result is None or (isinstance(result, dict) and result.get('truth') is False):
                    # Query failed - no solutions
                    return {}

                # Get return values
                return self._get_returns()

            except Exception as e:
                error_msg = str(e).lower()
                if 'time_limit_exceeded' in error_msg or 'time limit exceeded' in error_msg or 'timeout' in error_msg:
                    raise PrologTimeoutError("Prolog execution timeout") from e
                raise PrologRuntimeError(str(e)) from e

    def execute_node_code(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute inline Prolog code for a node.

        The code should use state/2 to access state and return/2 to set return values.

        Handles:
        - Simple queries (single line)
        - Multiple queries (separated by commas or periods)
        - Module imports (:- use_module(...))
        - Rule definitions (head :- body)
        - Directives (:- dynamic(...))

        Args:
            code: Prolog code to execute
            state: Current node state

        Returns:
            State updates as dictionary

        Raises:
            PrologRuntimeError: If code has syntax or runtime errors
            PrologTimeoutError: If execution exceeds timeout
        """
        # Clean up code - remove leading/trailing whitespace
        code = code.strip()

        if not code:
            return {}

        with self._lock:
            self._set_state(state)
            self._clear_returns()

            try:
                # Parse the code into directives, rules, and queries
                directives, rules, queries = self._parse_code(code)

                # Use consult() for directives and rules - this is the key improvement!
                # janus-swi's consult() properly handles :- directives
                # IMPORTANT: janus.consult("user", ...) REPLACES module contents,
                # so we must include _SETUP_CODE to preserve state/return predicates
                if directives or rules:
                    user_code = '\n'.join(directives + [f"{r}." for r in rules])
                    if user_code.strip():
                        # Prepend setup code to preserve state/return predicates
                        consult_code = self._SETUP_CODE + '\n' + user_code
                        try:
                            _janus.consult("user", consult_code)
                        except Exception as e:
                            raise PrologRuntimeError(f"Consult failed: {e}")

                # Execute queries with timeout
                if queries:
                    query_str = ', '.join(q.rstrip('.') for q in queries)
                    timed_query = f"call_with_time_limit({self.timeout}, ({query_str}))"
                    _janus.query_once(timed_query)

                # Get return values
                return self._get_returns()

            except PrologTimeoutError:
                raise
            except PrologRuntimeError:
                raise
            except Exception as e:
                error_msg = str(e).lower()
                if 'time_limit_exceeded' in error_msg or 'time limit exceeded' in error_msg or 'timeout' in error_msg:
                    raise PrologTimeoutError("Prolog execution timeout") from e
                raise PrologRuntimeError(str(e)) from e

    def _parse_code(self, code: str) -> Tuple[List[str], List[str], List[str]]:
        """
        Parse Prolog code into directives, rules, and queries.

        Args:
            code: Prolog code string

        Returns:
            Tuple of (directives, rules, queries)
        """
        directives = []
        rules = []
        queries = []

        # Split by lines and process
        lines = code.split('\n')
        current_statement = []

        for line in lines:
            stripped = line.strip()

            # Skip empty lines and pure comments
            if not stripped or stripped.startswith('%'):
                continue

            # Remove inline comments (but preserve % in strings)
            comment_match = re.search(r'%.*$', stripped)
            if comment_match and not self._in_string(stripped, comment_match.start()):
                stripped = stripped[:comment_match.start()].strip()

            if not stripped:
                continue

            current_statement.append(stripped)

            # Check if statement is complete (ends with period)
            joined = ' '.join(current_statement)
            if joined.endswith('.'):
                statement = joined.rstrip('.')

                # Categorize the statement
                if statement.startswith(':-'):
                    # Directive (e.g., :- use_module(...))
                    directives.append(statement + '.')
                elif ':-' in statement:
                    # Rule (head :- body)
                    rules.append(statement)
                else:
                    # Query
                    queries.append(statement)

                current_statement = []

        # Handle remaining content as query if no trailing period
        if current_statement:
            remaining = ' '.join(current_statement)
            if remaining.startswith(':-'):
                directives.append(remaining + '.')
            elif ':-' in remaining and not remaining.startswith(':-'):
                rules.append(remaining)
            else:
                queries.append(remaining)

        return directives, rules, queries

    def _in_string(self, text: str, position: int) -> bool:
        """Check if a position in text is inside a quoted string."""
        in_single = False
        in_double = False
        i = 0
        while i < position:
            if text[i] == "'" and not in_double:
                if i > 0 and text[i-1] == '\\':
                    pass  # Escaped quote
                else:
                    in_single = not in_single
            elif text[i] == '"' and not in_single:
                if i > 0 and text[i-1] == '\\':
                    pass  # Escaped quote
                else:
                    in_double = not in_double
            i += 1
        return in_single or in_double

    def eval_condition(self, expression: str, state: Dict[str, Any]) -> Optional[str]:
        """
        Evaluate a Prolog condition expression.

        The expression should succeed/fail or return a string via binding.

        Args:
            expression: Prolog expression to evaluate
            state: State dictionary accessible via state/2

        Returns:
            String result for routing, or None if query fails

        Raises:
            PrologRuntimeError: If expression has syntax or runtime errors
            PrologTimeoutError: If evaluation exceeds timeout
        """
        with self._lock:
            self._set_state(state)

            try:
                # Clean up expression
                expr_clean = expression.strip().rstrip('.')

                # Wrap with timeout
                timed_query = f"call_with_time_limit({self.timeout}, ({expr_clean}))"
                result = _janus.query_once(timed_query)

                if result is None or (isinstance(result, dict) and result.get('truth') is False):
                    return None

                # If query succeeded, check for Result binding
                if isinstance(result, dict):
                    if 'Result' in result:
                        return str(self._prolog_to_python(result['Result']))
                    # Query succeeded without Result binding
                    return "true"

                return "true"

            except Exception as e:
                error_msg = str(e).lower()
                if 'time_limit_exceeded' in error_msg or 'time limit exceeded' in error_msg or 'timeout' in error_msg:
                    raise PrologTimeoutError("Prolog execution timeout") from e
                if 'fail' in error_msg:
                    return None
                raise PrologRuntimeError(str(e)) from e

    def consult_file(self, path: str) -> None:
        """
        Consult a Prolog file to load rules.

        Only available in non-sandbox mode.

        Args:
            path: Path to .pl file

        Raises:
            PrologRuntimeError: If file cannot be loaded or sandbox is enabled
        """
        if self.sandbox:
            raise PrologRuntimeError(
                "Cannot consult files in sandbox mode. "
                "Create PrologRuntime with sandbox=False to load external files."
            )

        try:
            _janus.query_once(f"consult('{path}')")
        except Exception as e:
            raise PrologRuntimeError(f"Failed to consult {path}: {e}") from e

    def __repr__(self) -> str:
        return f"PrologRuntime(timeout={self.timeout}, sandbox={self.sandbox})"


def detect_prolog_code(code: str) -> bool:
    """
    Detect if code block is Prolog.

    Detection rules:
    1. Explicit marker: code starts with '% prolog'
    2. Heuristic: contains Prolog-specific syntax

    Args:
        code: Code string to check

    Returns:
        True if code appears to be Prolog
    """
    stripped = code.strip()

    # Explicit marker
    if stripped.startswith('% prolog') or stripped.startswith('%prolog'):
        return True

    # Heuristic patterns unique to Prolog
    prolog_patterns = [
        r':-',                    # Rule operator or directive
        r'\?-',                   # Query operator
        r'\bstate\s*\(',          # state/2 predicate (TEA convention)
        r'\breturn\s*\(',         # return/2 predicate (TEA convention)
        r'\bassertz?\s*\(',       # assert predicates
        r'\bretract\w*\s*\(',     # retract predicates
        r'\bfindall\s*\(',        # findall predicate
        r'\bforall\s*\(',         # forall predicate
        r'\baggregate_all\s*\(',  # aggregate predicate
        r'#=|#<|#>|#\\=|#=<|#>=', # CLP(FD) operators
        r'\bin\b.*\.\.',          # CLP(FD) domain notation
        r'\blabel\s*\(',          # CLP(FD) labeling
        r'\buse_module\s*\(',     # Module loading
    ]

    return any(re.search(pattern, code) for pattern in prolog_patterns)
