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

    if platform.startswith("linux"):
        return base_msg + (
            "  Ubuntu/Debian: sudo apt install swi-prolog\n"
            "  Note: Ensure version 9.1+ is installed\n"
        )
    elif platform == "darwin":
        return base_msg + "  macOS: brew install swi-prolog\n"
    elif platform == "win32":
        return (
            base_msg
            + "  Windows: Download from https://www.swi-prolog.org/download/stable\n"
        )
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
    DEFAULT_MODULES = ["lists", "clpfd", "apply", "aggregate"]

    # Setup code for state/return predicates - must be included in every consult
    # because janus.consult("user", ...) replaces module contents
    _SETUP_CODE = """
        :- thread_local(state/2).
        :- thread_local(return_value/2).
        :- thread_local(tea_user_fact/1).
        return(Key, Value) :- assertz(return_value(Key, Value)).
    """

    # Prolog-side term processing predicates
    # This is the "thin runtime" architecture: let Prolog parse Prolog!
    _TEA_PREDICATES = """
        % Action predicates - should be called, not asserted
        tea_action_predicate(return).
        tea_action_predicate(state).
        % Structural operators - must be called, not asserted
        tea_action_predicate(',').   % Conjunction
        tea_action_predicate(';').   % Disjunction
        tea_action_predicate('->').  % If-then
        tea_action_predicate('*->').  % Soft cut if-then
        % Arithmetic comparisons
        tea_action_predicate('>').
        tea_action_predicate('<').
        tea_action_predicate('>=').
        tea_action_predicate('=<').
        tea_action_predicate('=:=').
        tea_action_predicate('=\\\\=').
        % Unification and comparison
        tea_action_predicate('=').
        tea_action_predicate('\\\\=').
        tea_action_predicate('==').
        tea_action_predicate('\\\\==').
        tea_action_predicate('@<').
        tea_action_predicate('@>').
        tea_action_predicate('@=<').
        tea_action_predicate('@>=').
        tea_action_predicate('\\\\+').
        tea_action_predicate(is).
        % List predicates
        tea_action_predicate(findall).
        tea_action_predicate(bagof).
        tea_action_predicate(setof).
        tea_action_predicate(member).
        tea_action_predicate(memberchk).
        tea_action_predicate(append).
        tea_action_predicate(length).
        tea_action_predicate(nth0).
        tea_action_predicate(nth1).
        tea_action_predicate(msort).
        tea_action_predicate(sort).
        tea_action_predicate(reverse).
        tea_action_predicate(last).
        tea_action_predicate(sumlist).
        tea_action_predicate(max_list).
        tea_action_predicate(min_list).
        tea_action_predicate(forall).
        tea_action_predicate(aggregate_all).
        tea_action_predicate(aggregate).
        tea_action_predicate(call).
        tea_action_predicate(once).
        tea_action_predicate(succ).
        tea_action_predicate(plus).
        tea_action_predicate(abs).
        tea_action_predicate(sign).
        tea_action_predicate(max).
        tea_action_predicate(min).
        tea_action_predicate(write).
        tea_action_predicate(writeln).
        tea_action_predicate(print).
        tea_action_predicate(format).
        tea_action_predicate(atom).
        tea_action_predicate(number).
        tea_action_predicate(integer).
        tea_action_predicate(float).
        tea_action_predicate(compound).
        tea_action_predicate(is_list).
        tea_action_predicate(ground).
        tea_action_predicate(atom_string).
        tea_action_predicate(atom_codes).
        tea_action_predicate(atom_chars).

        % Determine if a term is a fact (should be asserted)
        % NOTE: We do NOT require ground(Term) because facts with variables
        % like convert(X, X). should still be asserted, not called as queries.
        tea_is_fact(Term) :-
            compound(Term),
            functor(Term, F, _),
            atom(F),
            \\+ tea_action_predicate(F).

        % Process a single term from user code
        tea_process_term((:-Body)) :- !, call(Body).
        tea_process_term((Head :- Body)) :- !, assertz((Head :- Body)).
        tea_process_term(Term) :-
            ( tea_is_fact(Term)
            -> assertz(Term), assertz(tea_user_fact(Term))
            ; call(Term)
            ).

        % Read and process terms from a stream
        tea_load_terms(Stream) :-
            catch(
                read_term(Stream, Term, []),
                Error,
                throw(tea_syntax_error(Error))
            ),
            ( Term == end_of_file
            -> true
            ; tea_process_term(Term),
              tea_load_terms(Stream)
            ).

        % Main entry point: load code from a string
        tea_load_code(CodeAtom) :-
            atom_string(CodeAtom, CodeString),
            open_string(CodeString, Stream),
            catch(
                tea_load_terms(Stream),
                Error,
                ( close(Stream), throw(Error) )
            ),
            close(Stream).

        % Clean up user-asserted facts
        tea_cleanup_facts :-
            forall(tea_user_fact(Fact), retract(Fact)),
            retractall(tea_user_fact(_)).
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
        """Set up state/2, return/2, and TEA predicates for Prolog-side parsing."""
        # Combine setup code with TEA predicates
        full_setup = self._SETUP_CODE + "\n" + self._TEA_PREDICATES

        try:
            _janus.consult("user", full_setup)
        except Exception:
            # Fallback: try without thread_local (older SWI-Prolog)
            fallback_code = """
                :- dynamic(state/2).
                :- dynamic(return_value/2).
                :- dynamic(tea_user_fact/1).
                return(Key, Value) :- assertz(return_value(Key, Value)).
            """
            try:
                _janus.consult("user", fallback_code)
                # Try to load TEA predicates separately
                try:
                    _janus.consult("user", self._TEA_PREDICATES)
                except Exception:
                    pass
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
        if hasattr(value, "__class__") and "janus" in str(
            type(value).__module__ if hasattr(type(value), "__module__") else ""
        ):
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
            if value == "null":
                return None
            elif value == "true":
                return True
            elif value == "false":
                return False
            return value

        # Handle bytes
        if isinstance(value, bytes):
            return value.decode("utf-8", errors="replace")

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
                key = self._prolog_to_python(sol.get("Key"))
                value = self._prolog_to_python(sol.get("Value"))
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
        self, query: str, state: Dict[str, Any], first_only: bool = True
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
                clean_query = query.strip().rstrip(".")

                # Wrap query with timeout using call_with_time_limit/2
                timed_query = f"call_with_time_limit({self.timeout}, ({clean_query}))"

                result = _janus.query_once(timed_query)

                if result is None or (
                    isinstance(result, dict) and result.get("truth") is False
                ):
                    # Query failed - no solutions
                    return {}

                # Get return values
                return self._get_returns()

            except Exception as e:
                error_msg = str(e).lower()
                if (
                    "time_limit_exceeded" in error_msg
                    or "time limit exceeded" in error_msg
                    or "timeout" in error_msg
                    or "stack limit" in error_msg
                    or "infinite recursion" in error_msg
                ):
                    raise PrologTimeoutError("Prolog execution timeout") from e
                raise PrologRuntimeError(str(e)) from e

    def execute_node_code(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute inline Prolog code for a node.

        This uses **Prolog-side parsing** via `tea_load_code/1` - letting SWI-Prolog's
        own `read_term/3` parse the code instead of error-prone Python-side heuristics.

        The code should use state/2 to access state and return/2 to set return values.

        Handles:
        - Simple queries (single line)
        - Multiple queries (separated by commas or periods)
        - Module imports (:- use_module(...))
        - Rule definitions (head :- body)
        - Fact definitions (parent(alice, bob).)
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
                # Use Prolog-side parsing via tea_load_code/1
                # This is the key architectural improvement: let Prolog parse Prolog!

                # Ensure code ends with a period (required by read_term/3)
                # This matches the Rust implementation for cross-runtime parity
                code_with_period = (
                    code if code.strip().endswith(".") else f"{code.strip()}."
                )

                # Escape backslashes first, then single quotes for Prolog atom
                # Order matters: backslashes must be escaped before quotes to avoid
                # double-escaping the backslashes used for quote escapes
                escaped_code = code_with_period.replace("\\", "\\\\").replace("'", "''")

                # Build the query with timeout
                load_query = f"tea_load_code('{escaped_code}')"
                timed_query = f"call_with_time_limit({self.timeout}, ({load_query}))"

                try:
                    _janus.query_once(timed_query)
                except Exception as e:
                    error_msg = str(e).lower()
                    if "tea_syntax_error" in error_msg or "syntax" in error_msg:
                        raise PrologRuntimeError(f"Prolog syntax error: {e}") from e
                    raise

                # Get return values
                results = self._get_returns()

                # Clean up user-asserted facts
                try:
                    _janus.query_once("tea_cleanup_facts")
                except Exception:
                    pass

                return results

            except PrologTimeoutError:
                raise
            except PrologRuntimeError:
                raise
            except Exception as e:
                error_msg = str(e).lower()
                if (
                    "time_limit_exceeded" in error_msg
                    or "time limit exceeded" in error_msg
                    or "timeout" in error_msg
                    or "stack limit" in error_msg
                    or "infinite recursion" in error_msg
                ):
                    raise PrologTimeoutError("Prolog execution timeout") from e
                raise PrologRuntimeError(str(e)) from e

    def execute_node_code_legacy(
        self, code: str, state: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Execute inline Prolog code using Python-side parsing (legacy).

        This is the old implementation kept for reference. The new `execute_node_code`
        uses Prolog-side parsing which is more robust.

        **Deprecated**: Use `execute_node_code` instead.
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

                # Use consult() for directives and rules
                if directives or rules:
                    user_code = "\n".join(directives + [f"{r}." for r in rules])
                    if user_code.strip():
                        consult_code = self._SETUP_CODE + "\n" + user_code
                        try:
                            _janus.consult("user", consult_code)
                        except Exception as e:
                            raise PrologRuntimeError(f"Consult failed: {e}")

                # Execute queries with timeout
                if queries:
                    query_str = ", ".join(q.rstrip(".") for q in queries)
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
                if (
                    "time_limit_exceeded" in error_msg
                    or "time limit exceeded" in error_msg
                    or "timeout" in error_msg
                    or "stack limit" in error_msg
                    or "infinite recursion" in error_msg
                ):
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
        lines = code.split("\n")
        current_statement = []

        for line in lines:
            stripped = line.strip()

            # Skip empty lines and pure comments
            if not stripped or stripped.startswith("%"):
                continue

            # Remove inline comments (but preserve % in strings)
            comment_match = re.search(r"%.*$", stripped)
            if comment_match and not self._in_string(stripped, comment_match.start()):
                stripped = stripped[: comment_match.start()].strip()

            if not stripped:
                continue

            current_statement.append(stripped)

            # Check if statement is complete (ends with period)
            joined = " ".join(current_statement)
            if joined.endswith("."):
                statement = joined.rstrip(".")

                # Categorize the statement
                if statement.startswith(":-"):
                    # Directive (e.g., :- use_module(...))
                    directives.append(statement + ".")
                elif ":-" in statement:
                    # Rule (head :- body)
                    rules.append(statement)
                else:
                    # Query
                    queries.append(statement)

                current_statement = []

        # Handle remaining content as query if no trailing period
        if current_statement:
            remaining = " ".join(current_statement)
            if remaining.startswith(":-"):
                directives.append(remaining + ".")
            elif ":-" in remaining and not remaining.startswith(":-"):
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
                if i > 0 and text[i - 1] == "\\":
                    pass  # Escaped quote
                else:
                    in_single = not in_single
            elif text[i] == '"' and not in_single:
                if i > 0 and text[i - 1] == "\\":
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
                expr_clean = expression.strip().rstrip(".")

                # Wrap with timeout
                timed_query = f"call_with_time_limit({self.timeout}, ({expr_clean}))"
                result = _janus.query_once(timed_query)

                if result is None or (
                    isinstance(result, dict) and result.get("truth") is False
                ):
                    return None

                # If query succeeded, check for Result binding
                if isinstance(result, dict):
                    if "Result" in result:
                        return str(self._prolog_to_python(result["Result"]))
                    # Query succeeded without Result binding
                    return "true"

                return "true"

            except Exception as e:
                error_msg = str(e).lower()
                if (
                    "time_limit_exceeded" in error_msg
                    or "time limit exceeded" in error_msg
                    or "timeout" in error_msg
                    or "stack limit" in error_msg
                    or "infinite recursion" in error_msg
                ):
                    raise PrologTimeoutError("Prolog execution timeout") from e
                if "fail" in error_msg:
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
    if stripped.startswith("% prolog") or stripped.startswith("%prolog"):
        return True

    # Heuristic patterns unique to Prolog
    prolog_patterns = [
        r":-",  # Rule operator or directive
        r"\?-",  # Query operator
        r"\bstate\s*\(",  # state/2 predicate (TEA convention)
        r"\breturn\s*\(",  # return/2 predicate (TEA convention)
        r"\bassertz?\s*\(",  # assert predicates
        r"\bretract\w*\s*\(",  # retract predicates
        r"\bfindall\s*\(",  # findall predicate
        r"\bforall\s*\(",  # forall predicate
        r"\baggregate_all\s*\(",  # aggregate predicate
        r"#=|#<|#>|#\\=|#=<|#>=",  # CLP(FD) operators
        r"\bin\b.*\.\.",  # CLP(FD) domain notation
        r"\blabel\s*\(",  # CLP(FD) labeling
        r"\buse_module\s*\(",  # Module loading
    ]

    return any(re.search(pattern, code) for pattern in prolog_patterns)
