"""Error Report Module - TEA-REPORT-001a

Provides standardized error capture protocol for bug reporting.
Captures crashes, exceptions, and errors with consistent structure.

Privacy:
    All paths are sanitized to remove PII:
    - Absolute paths converted to relative
    - Home directory stripped from paths
    - No state data values included
    - No environment variables captured

Example:
    >>> from the_edge_agent.report import ErrorReport, ErrorType, capture_exception
    >>> report = ErrorReport(
    ...     error_type=ErrorType.YAML_ERROR,
    ...     message="Invalid node configuration"
    ... )
    >>> print(report.to_json())
"""

from dataclasses import dataclass, field, asdict
from enum import Enum
from typing import Optional, List, Any, Dict, Tuple, Type
from types import TracebackType
import json
import os
import platform
import sys
import traceback

# Get version from package
try:
    from . import __version__
except ImportError:
    __version__ = "0.0.0"


class ErrorType(str, Enum):
    """Error type classification for reports."""

    PANIC = "Panic"
    YAML_ERROR = "YamlError"
    EXECUTOR_ERROR = "ExecutorError"
    ACTION_ERROR = "ActionError"


@dataclass
class StackFrame:
    """A single frame in the stack trace.

    Attributes:
        addr: Memory address of the frame (or line number for Python)
        symbol: Function/method name (if available)
        file: Source file path (relative only, for privacy)
        line: Line number in source file
    """

    addr: int
    symbol: Optional[str] = None
    file: Optional[str] = None
    line: Optional[int] = None

    @classmethod
    def from_traceback_frame(cls, frame: traceback.FrameSummary) -> "StackFrame":
        """Create a StackFrame from a traceback FrameSummary."""
        return cls(
            addr=frame.lineno or 0,
            symbol=frame.name,
            file=sanitize_path(frame.filename) if frame.filename else None,
            line=frame.lineno,
        )


@dataclass
class ErrorContext:
    """Context about where the error occurred.

    Attributes:
        node_name: Name of the node where error occurred
        action_type: Type of action being executed
        checkpoint_id: Checkpoint ID (not data, for privacy)
    """

    node_name: Optional[str] = None
    action_type: Optional[str] = None
    checkpoint_id: Optional[str] = None

    def is_empty(self) -> bool:
        """Check if context has any data."""
        return (
            self.node_name is None
            and self.action_type is None
            and self.checkpoint_id is None
        )


@dataclass
class NodeInfo:
    """Information about a node in the workflow.

    Attributes:
        name: Node name
        action_type: Type of action the node performs
    """

    name: str
    action_type: Optional[str] = None


@dataclass
class EdgeInfo:
    """Information about an edge in the workflow.

    Attributes:
        from_node: Source node (named 'from_node' because 'from' is reserved)
        to: Target node
        edge_type: Type of edge (simple, conditional, etc.)
    """

    from_node: str  # 'from' is reserved in Python
    to: str
    edge_type: Optional[str] = None


@dataclass
class ExtendedContext:
    """Extended context for opt-in detailed reports.

    Attributes:
        workflow_name: Name of the workflow
        nodes: List of nodes in the workflow
        edges: List of edges in the workflow
        schema_fields: Schema field names (not values)
        active_node: Currently executing node
        active_action: Currently executing action
    """

    workflow_name: Optional[str] = None
    nodes: List[NodeInfo] = field(default_factory=list)
    edges: List[EdgeInfo] = field(default_factory=list)
    schema_fields: List[str] = field(default_factory=list)
    active_node: Optional[str] = None
    active_action: Optional[str] = None


def _get_platform_default() -> str:
    """Default factory for platform field."""
    return get_platform()


@dataclass
class ErrorReport:
    """Main error report structure for bug reporting.

    Attributes:
        version: TEA version
        platform: Platform (e.g., "linux-x86_64")
        runtime: Runtime identifier ("python")
        error_type: Type of error
        message: Error message
        stack: Stack trace frames
        context: Error context (where it occurred)
        extended: Extended context (opt-in, more details)

    Example:
        >>> report = ErrorReport(
        ...     error_type=ErrorType.YAML_ERROR,
        ...     message="Invalid node"
        ... )
        >>> report.to_json()
        '{"version": "...", "platform": "...", ...}'
    """

    version: str = field(default_factory=lambda: __version__)
    platform: str = field(default_factory=_get_platform_default)
    runtime: str = "python"
    error_type: ErrorType = ErrorType.PANIC
    message: str = ""
    stack: List[StackFrame] = field(default_factory=list)
    context: Optional[ErrorContext] = None
    extended: Optional[ExtendedContext] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary, excluding None values."""

        def filter_none(obj: Any) -> Any:
            if isinstance(obj, dict):
                return {k: filter_none(v) for k, v in obj.items() if v is not None}
            elif isinstance(obj, list):
                return [filter_none(item) for item in obj]
            elif isinstance(obj, Enum):
                return obj.value
            return obj

        result = asdict(self)
        # Convert 'from_node' back to 'from' for JSON compatibility with Rust
        if result.get("extended") and result["extended"].get("edges"):
            for edge in result["extended"]["edges"]:
                if "from_node" in edge:
                    edge["from"] = edge.pop("from_node")
        return filter_none(result)

    def to_json(self) -> str:
        """Serialize to JSON string, excluding None values."""
        return json.dumps(self.to_dict())

    def to_json_pretty(self) -> str:
        """Serialize to pretty JSON string."""
        return json.dumps(self.to_dict(), indent=2)


def get_platform() -> str:
    """Get the current platform string (e.g., 'linux-x86_64')."""
    system = platform.system().lower()
    machine = platform.machine().lower()

    # Normalize architecture names to match Rust
    if machine in ("amd64", "x86_64"):
        machine = "x86_64"
    elif machine in ("arm64", "aarch64"):
        machine = "aarch64"

    return f"{system}-{machine}"


def sanitize_path(path: str, project_root: Optional[str] = None) -> str:
    """Convert absolute path to relative, removing PII.

    Args:
        path: The path to sanitize
        project_root: Optional project root for relative path calculation

    Returns:
        Sanitized path (relative, with home directory stripped)

    Example:
        >>> sanitize_path("/home/user/project/src/main.py")
        '~/project/src/main.py'
    """
    if not path:
        return path

    # Normalize path
    path = os.path.normpath(path)

    # Remove home directory
    home = os.path.expanduser("~")
    if path.startswith(home):
        relative = path[len(home) :].lstrip(os.sep)
        return f"~/{relative}"

    # Convert to relative from project root
    if project_root and path.startswith(project_root):
        return os.path.relpath(path, project_root)

    # If still absolute, try current directory
    if os.path.isabs(path):
        try:
            cwd = os.getcwd()
            if path.startswith(cwd):
                return os.path.relpath(path, cwd)
        except OSError:
            pass

        # Fallback: just use basename
        return os.path.basename(path)

    return path


def capture_stack_trace(skip_frames: int = 0) -> List[StackFrame]:
    """Capture the current stack trace as StackFrames.

    Args:
        skip_frames: Number of frames to skip from the top

    Returns:
        List of StackFrame objects
    """
    frames = []
    # Get current stack, excluding this function
    stack = traceback.extract_stack()[: -1 - skip_frames]

    for frame in stack:
        frames.append(StackFrame.from_traceback_frame(frame))

    return frames


ExcInfoType = Tuple[
    Optional[Type[BaseException]], Optional[BaseException], Optional[TracebackType]
]


def capture_exception_stack(exc_info: Optional[ExcInfoType] = None) -> List[StackFrame]:
    """Capture stack trace from an exception.

    Args:
        exc_info: Exception info tuple (type, value, traceback) or None for current

    Returns:
        List of StackFrame objects
    """
    if exc_info is None:
        exc_info = sys.exc_info()

    _, _, exc_tb = exc_info

    if exc_tb is None:
        return []

    frames = []
    for frame in traceback.extract_tb(exc_tb):
        frames.append(StackFrame.from_traceback_frame(frame))

    return frames


def capture_exception(
    exc: BaseException,
    error_type: ErrorType = ErrorType.PANIC,
    node_name: Optional[str] = None,
    action_type: Optional[str] = None,
    checkpoint_id: Optional[str] = None,
) -> ErrorReport:
    """Create an ErrorReport from an exception.

    Args:
        exc: The exception to capture
        error_type: Type of error (default: PANIC)
        node_name: Name of the node where error occurred
        action_type: Type of action being executed
        checkpoint_id: Checkpoint ID (for privacy, not data)

    Returns:
        ErrorReport with exception details
    """
    # Get exception message
    message = str(exc)
    if not message and exc.__class__.__name__:
        message = exc.__class__.__name__

    # Get stack trace from exception
    stack = capture_exception_stack((type(exc), exc, exc.__traceback__))

    # Build context if any fields provided
    context = None
    if node_name or action_type or checkpoint_id:
        context = ErrorContext(
            node_name=node_name,
            action_type=action_type,
            checkpoint_id=checkpoint_id,
        )

    return ErrorReport(
        error_type=error_type,
        message=message,
        stack=stack,
        context=context,
    )


def capture_yaml_error(
    error: str,
    node_name: Optional[str] = None,
    action_type: Optional[str] = None,
) -> ErrorReport:
    """Create an ErrorReport from a YAML engine error.

    Args:
        error: The error message
        node_name: Name of the node where error occurred
        action_type: Type of action being executed

    Returns:
        ErrorReport with YAML error details
    """
    context = (
        ErrorContext(
            node_name=node_name,
            action_type=action_type,
        )
        if node_name or action_type
        else None
    )

    return ErrorReport(
        error_type=ErrorType.YAML_ERROR,
        message=error,
        stack=capture_stack_trace(skip_frames=1),
        context=context,
    )


def capture_executor_error(
    error: str,
    checkpoint_id: Optional[str] = None,
    node_name: Optional[str] = None,
) -> ErrorReport:
    """Create an ErrorReport from an executor error.

    Args:
        error: The error message
        checkpoint_id: ID of the checkpoint (not data, for privacy)
        node_name: Name of the node where error occurred

    Returns:
        ErrorReport with executor error details
    """
    context = (
        ErrorContext(
            checkpoint_id=checkpoint_id,
            node_name=node_name,
        )
        if checkpoint_id or node_name
        else None
    )

    return ErrorReport(
        error_type=ErrorType.EXECUTOR_ERROR,
        message=error,
        stack=capture_stack_trace(skip_frames=1),
        context=context,
    )


def capture_action_error(
    error: str,
    action_type: str,
    node_name: Optional[str] = None,
) -> ErrorReport:
    """Create an ErrorReport from an action error.

    Args:
        error: The error message
        action_type: Type of action that failed
        node_name: Name of the node containing the action

    Returns:
        ErrorReport with action error details
    """
    context = ErrorContext(
        action_type=action_type,
        node_name=node_name,
    )

    return ErrorReport(
        error_type=ErrorType.ACTION_ERROR,
        message=error,
        stack=capture_stack_trace(skip_frames=1),
        context=context,
    )


# Global storage for the last captured error report
_last_report: Optional[ErrorReport] = None
_original_excepthook = None
_hook_installed = False


def get_last_report() -> Optional[ErrorReport]:
    """Get the last captured error report (if any)."""
    return _last_report


def clear_last_report() -> None:
    """Clear the last captured error report."""
    global _last_report
    _last_report = None


def _tea_excepthook(exc_type, exc_value, exc_tb):
    """Custom excepthook that captures errors as ErrorReports."""
    global _last_report

    # Capture the error report
    report = capture_exception(exc_value)
    _last_report = report

    # Print the report to stderr (JSON format)
    try:
        print("\n--- TEA Error Report ---", file=sys.stderr)
        print(report.to_json(), file=sys.stderr)
        print("--- End Error Report ---\n", file=sys.stderr)
    except Exception:
        pass  # Don't fail if we can't print

    # Call original excepthook for normal error output
    if _original_excepthook:
        _original_excepthook(exc_type, exc_value, exc_tb)


def install_excepthook() -> None:
    """Install the sys.excepthook that captures error reports.

    This function should be called early in the application to capture
    unhandled exceptions. It's safe to call multiple times - subsequent
    calls are no-ops.

    Example:
        >>> from the_edge_agent.report import install_excepthook
        >>> install_excepthook()
        >>> # Unhandled exceptions will now be captured as ErrorReports
    """
    global _original_excepthook, _hook_installed

    if _hook_installed:
        return

    _original_excepthook = sys.excepthook
    sys.excepthook = _tea_excepthook
    _hook_installed = True


def uninstall_excepthook() -> None:
    """Restore the original sys.excepthook.

    Useful for testing or when you want to disable error capture.
    """
    global _original_excepthook, _hook_installed

    if not _hook_installed:
        return

    if _original_excepthook:
        sys.excepthook = _original_excepthook
    _hook_installed = False
