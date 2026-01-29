from .stategraph import StateGraph, START, END
from .yaml_engine import YAMLEngine
from .checkpointers import MemoryCheckpointer

# TEA-ARCH-001: Core exceptions (zero dependencies)
from .exceptions import HTTPResponse

# Parallel Execution Reliability (TD.13)
from .parallel import (
    ParallelConfig,
    ParallelFlowResult,
    RetryPolicy,
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerRegistry,
    CircuitState,
    CircuitOpenError,
    RetryExhaustedError,
    CancellationToken,
    ParallelFlowCallback,
    ParallelFlowContext,
    CallbackManager,
)

# Parallel Executors (TEA-PARALLEL-001.1)
from .parallel_executors import (
    ParallelExecutor,
    ThreadExecutor,
    ProcessExecutor,
    FlowTask,
    ExecutorResult,
    PickleValidationError,
    get_executor,
    register_executor,
    available_strategies,
)

# Tracing (TEA-BUILTIN-001.3)
from .tracing import (
    TraceContext,
    TraceExporter,
    ConsoleExporter,
    FileExporter,
    CallbackExporter,
)

# Observability (TEA-OBS-001.1)
from .observability import (
    ObservabilityContext,
    EventStream,
    EventStreamHandler,
    ConsoleHandler,
    FileHandler,
    CallbackHandler,
)

# Memory (TEA-BUILTIN-001.1)
from .memory import (
    MemoryBackend,
    InMemoryBackend,
)

# Long-Term Memory (TEA-BUILTIN-001.4)
from .memory import (
    LongTermMemoryBackend,
    SQLiteBackend,
    GraphBackend,
    COZO_AVAILABLE,
    KUZU_AVAILABLE,
)

# Conditionally import CozoBackend
try:
    from .memory import CozoBackend
except ImportError:
    CozoBackend = None  # type: ignore

# Conditionally import KuzuBackend (Bighorn)
try:
    from .memory import KuzuBackend, BighornBackend
except ImportError:
    KuzuBackend = None  # type: ignore
    BighornBackend = None  # type: ignore

# Opik Exporter (TEA-BUILTIN-005.1)
# Conditionally import - requires 'pip install opik'
try:
    from .exporters import OpikExporter
except ImportError:
    OpikExporter = None  # type: ignore

# Bug Reporting (TEA-REPORT-001d)
from . import report_cli

# DOT Parser and StateGraph Integration (TEA-GAME-001)
from .dot_parser import (
    dot_to_stategraph,
    run_dot,
    parse_dot,
    parse_dot_string,
    analyze_graph,
    dot_to_yaml,
)

__all__ = [
    "StateGraph",
    "START",
    "END",
    "YAMLEngine",
    "MemoryCheckpointer",
    # TEA-ARCH-001: Core exceptions
    "HTTPResponse",
    # Parallel Execution Reliability (TD.13)
    "ParallelConfig",
    "ParallelFlowResult",
    "RetryPolicy",
    "CircuitBreaker",
    "CircuitBreakerConfig",
    "CircuitBreakerRegistry",
    "CircuitState",
    "CircuitOpenError",
    "RetryExhaustedError",
    "CancellationToken",
    "ParallelFlowCallback",
    "ParallelFlowContext",
    "CallbackManager",
    # Parallel Executors (TEA-PARALLEL-001.1)
    "ParallelExecutor",
    "ThreadExecutor",
    "ProcessExecutor",
    "FlowTask",
    "ExecutorResult",
    "PickleValidationError",
    "get_executor",
    "register_executor",
    "available_strategies",
    # Tracing (TEA-BUILTIN-001.3)
    "TraceContext",
    "TraceExporter",
    "ConsoleExporter",
    "FileExporter",
    "CallbackExporter",
    # Observability (TEA-OBS-001.1)
    "ObservabilityContext",
    "EventStream",
    "EventStreamHandler",
    "ConsoleHandler",
    "FileHandler",
    "CallbackHandler",
    # Memory (TEA-BUILTIN-001.1)
    "MemoryBackend",
    "InMemoryBackend",
    # Long-Term Memory (TEA-BUILTIN-001.4)
    "LongTermMemoryBackend",
    "SQLiteBackend",
    "GraphBackend",
    "CozoBackend",
    "COZO_AVAILABLE",
    # Graph Database - Kuzu/Bighorn (TEA-BUILTIN-001.4 Bighorn Extension)
    "KuzuBackend",
    "BighornBackend",
    "KUZU_AVAILABLE",
    # Opik Exporter (TEA-BUILTIN-005.1)
    "OpikExporter",
    # DOT Parser and StateGraph Integration (TEA-GAME-001)
    "dot_to_stategraph",
    "run_dot",
    "parse_dot",
    "parse_dot_string",
    "analyze_graph",
    "dot_to_yaml",
    # Version
    "__version__",
]
__version__ = "0.9.90"
