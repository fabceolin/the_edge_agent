"""
Observability Infrastructure for YAMLEngine (TEA-OBS-001.1).

This module provides flow-scoped observability with structured logging,
event buffering, and declarative configuration. It composes (not replaces)
the existing TraceContext infrastructure to provide:

1. Flow-scoped logging: Each workflow execution gets a unique flow ID
2. Event buffering: Configurable ring buffer for log retention
3. Declarative configuration: YAML-based handler and level configuration
4. Programmatic access: API for retrieving complete flow logs

Example:
    >>> from the_edge_agent.observability import ObservabilityContext, EventStream
    >>>
    >>> # Create context with console output
    >>> ctx = ObservabilityContext(
    ...     flow_id="my-flow-123",
    ...     config={'level': 'info', 'handlers': [{'type': 'console'}]}
    ... )
    >>>
    >>> # Log events for a node
    >>> span_id = ctx.start_node_span("process", metadata={"input": "data"})
    >>> ctx.log("process", "info", "metric", message="Processing", metrics={"items": 42})
    >>> ctx.end_node_span("process", status="ok")
    >>>
    >>> # Get complete flow log
    >>> flow_log = ctx.get_flow_log()
    >>> print(f"Events: {flow_log['metrics']['event_count']}")

Handlers:
    - ConsoleHandler: Print events to stdout (verbose or summary mode)
    - FileHandler: Write events to JSON lines file
    - CallbackHandler: Call user-provided function with each event

Shared Log Schema (cross-runtime parity with Rust):
    {
        "flow_id": "uuid-string",
        "span_id": "uuid-string",
        "parent_id": "optional-uuid",
        "node": "node-name",
        "level": "info",  # debug | info | warn | error
        "timestamp": 1703347200.123,  # Unix timestamp (float)
        "event_type": "entry",  # entry | exit | error | metric
        "message": "Optional message",
        "data": {},
        "metrics": {"duration_ms": 123.45}
    }
"""

import fnmatch
import json
import threading
import time
import uuid
from collections import deque
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Protocol

from .tracing import TraceContext, ConsoleExporter, FileExporter, CallbackExporter


# =============================================================================
# Protocols
# =============================================================================


class EventStreamHandler(Protocol):
    """Protocol for event stream handlers."""

    def handle(self, event: Dict[str, Any]) -> None:
        """Handle a log event."""
        ...


# =============================================================================
# Handler Implementations
# =============================================================================


class ConsoleHandler:
    """Print events to console with optional verbose mode."""

    def __init__(self, verbose: bool = False):
        """
        Initialize console handler.

        Args:
            verbose: If True, print full event details. If False, print summary.
        """
        self.verbose = verbose

    def handle(self, event: Dict[str, Any]) -> None:
        """Print event to console."""
        node = event.get("node", "unknown")
        level = event.get("level", "info").upper()
        event_type = event.get("event_type", "?")
        message = event.get("message", "")

        if self.verbose:
            print(f"[{level}] {node} ({event_type}): {message}")
            if event.get("data"):
                print(f"  data: {event['data']}")
            if event.get("metrics"):
                print(f"  metrics: {event['metrics']}")
        else:
            icon = {
                "entry": "\u2192",  # →
                "exit": "\u2190",  # ←
                "error": "\u2717",  # ✗
                "metric": "\ud83d\udcca",  # 📊 as bytes
            }.get(
                event_type, "\u2022"
            )  # •
            print(f"[{level}] {icon} {node}: {message}")


class FileHandler:
    """Write events to JSON lines file."""

    def __init__(self, path: str):
        """
        Initialize file handler.

        Args:
            path: File path to write events to (JSON lines format).
                  Supports {flow_id} template substitution in path.
        """
        self.path = Path(path)
        self._lock = threading.Lock()
        # Ensure parent directory exists
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def handle(self, event: Dict[str, Any]) -> None:
        """Append event to file as JSON line."""
        with self._lock:
            with open(self.path, "a", encoding="utf-8") as f:
                f.write(json.dumps(event, default=str) + "\n")


class CallbackHandler:
    """Call user-provided function with each event."""

    def __init__(self, callback: Callable[[Dict[str, Any]], None]):
        """
        Initialize callback handler.

        Args:
            callback: Function to call with each event.
        """
        self.callback = callback

    def handle(self, event: Dict[str, Any]) -> None:
        """Call the callback with event data."""
        try:
            self.callback(event)
        except Exception:
            # Swallow errors to not crash workflow
            pass


# =============================================================================
# EventStream
# =============================================================================


class EventStream:
    """
    Thread-safe ring buffer for flow events.

    Uses collections.deque with maxlen for bounded buffer behavior.
    Oldest events are evicted when capacity is reached.
    """

    def __init__(
        self, max_size: int = 1000, handlers: Optional[List[EventStreamHandler]] = None
    ):
        """
        Initialize event stream.

        Args:
            max_size: Maximum number of events to retain (default: 1000).
            handlers: List of handlers to forward events to.
        """
        self._lock = threading.Lock()
        self._buffer: deque = deque(maxlen=max_size)
        self._handlers: List[EventStreamHandler] = handlers or []
        self.max_size = max_size

    def append(self, event: Dict[str, Any]) -> None:
        """
        Add event to buffer and forward to handlers.

        Args:
            event: Event dictionary to add.
        """
        with self._lock:
            self._buffer.append(event)

        # Handler calls outside lock to prevent deadlocks
        for handler in self._handlers:
            try:
                handler.handle(event)
            except Exception:
                pass  # Swallow errors to not crash workflow

    def get_all(self) -> List[Dict[str, Any]]:
        """
        Return all events as list (thread-safe copy).

        Returns:
            List of all buffered events in order.
        """
        with self._lock:
            return list(self._buffer)

    def query(self, filters: Dict[str, Any]) -> List[Dict[str, Any]]:
        """
        Filter events by criteria.

        Supported filters:
            - node: Glob pattern to match node name (e.g., "llm.*")
            - level: Exact level match (debug, info, warn, error)
            - event_type: Exact event type match (entry, exit, error, metric)
            - start_time: Minimum timestamp (inclusive)
            - end_time: Maximum timestamp (inclusive)

        Args:
            filters: Dictionary of filter criteria.

        Returns:
            List of events matching all filters.
        """
        events = self.get_all()

        if "node" in filters:
            pattern = filters["node"]
            events = [e for e in events if fnmatch.fnmatch(e.get("node", ""), pattern)]

        if "level" in filters:
            events = [e for e in events if e.get("level") == filters["level"]]

        if "event_type" in filters:
            events = [e for e in events if e.get("event_type") == filters["event_type"]]

        if "start_time" in filters:
            events = [
                e for e in events if e.get("timestamp", 0) >= filters["start_time"]
            ]

        if "end_time" in filters:
            events = [e for e in events if e.get("timestamp", 0) <= filters["end_time"]]

        return events

    def clear(self) -> None:
        """Clear all events from buffer."""
        with self._lock:
            self._buffer.clear()


# =============================================================================
# ObservabilityContext
# =============================================================================


class ObservabilityContext:
    """
    Flow-scoped observability context that wraps TraceContext.

    This class COMPOSES (not extends) TraceContext to add:
    - Flow-level event aggregation
    - Ring buffer for bounded log retention
    - Structured log schema for cross-runtime parity with Rust

    Example:
        >>> ctx = ObservabilityContext(
        ...     flow_id="flow-123",
        ...     config={'level': 'info', 'handlers': [{'type': 'console'}]}
        ... )
        >>> span_id = ctx.start_node_span("process")
        >>> ctx.log("process", "info", "entry", "Starting process")
        >>> ctx.end_node_span("process", status="ok")
        >>> flow_log = ctx.get_flow_log()
    """

    def __init__(
        self,
        flow_id: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
        trace_context: Optional[TraceContext] = None,
    ):
        """
        Initialize observability context.

        Args:
            flow_id: Unique identifier for this flow execution.
                    If None, generates a UUID.
            config: Configuration dictionary with optional keys:
                    - level: Log level threshold (debug, info, warn, error)
                    - buffer_size: Maximum events to retain (default: 1000)
                    - handlers: List of handler configurations
            trace_context: Optional existing TraceContext to wrap.
                          If None, creates a new one.
        """
        self.flow_id = flow_id or str(uuid.uuid4())
        self.config = config or {}
        self._level = self.config.get("level", "info")

        # COMPOSITION: Wrap existing TraceContext, don't replace it
        self._trace_context = trace_context or TraceContext(exporters=[])

        # Event stream with configurable buffer size
        buffer_size = self.config.get("buffer_size", 1000)
        handlers = self._create_handlers(self.config.get("handlers", []))
        self._event_stream = EventStream(max_size=buffer_size, handlers=handlers)

        # Track node-to-span mapping for proper span management
        self._node_spans: Dict[str, str] = {}

        # Lock for thread-safe access to node-spans
        self._lock = threading.Lock()

    def _create_handlers(
        self, handler_configs: List[Dict[str, Any]]
    ) -> List[EventStreamHandler]:
        """
        Create handler instances from configuration.

        Args:
            handler_configs: List of handler configuration dictionaries.

        Returns:
            List of handler instances.
        """
        handlers: List[EventStreamHandler] = []
        for cfg in handler_configs:
            handler_type = cfg.get("type")
            if handler_type == "console":
                handlers.append(ConsoleHandler(verbose=cfg.get("verbose", False)))
            elif handler_type == "file":
                path = cfg.get("path", "./logs/flow.jsonl")
                # Template substitution for {flow_id}
                path = path.replace("{flow_id}", self.flow_id)
                handlers.append(FileHandler(path=path))
            elif handler_type == "callback":
                if "callback" in cfg:
                    handlers.append(CallbackHandler(callback=cfg["callback"]))
        return handlers

    def _create_event(
        self,
        node: str,
        level: str,
        event_type: str,
        message: Optional[str],
        data: Optional[Dict[str, Any]],
        metrics: Optional[Dict[str, Any]],
    ) -> Dict[str, Any]:
        """
        Create a log event conforming to shared schema.

        Args:
            node: Name of the node generating the event.
            level: Log level (debug, info, warn, error).
            event_type: Type of event (entry, exit, error, metric).
            message: Optional message text.
            data: Optional data dictionary.
            metrics: Optional metrics dictionary.

        Returns:
            Event dictionary with all required fields.
        """
        current_span = self._trace_context.current_span()
        return {
            "flow_id": self.flow_id,
            "span_id": current_span["span_id"] if current_span else None,
            "parent_id": current_span.get("parent_id") if current_span else None,
            "node": node,
            "level": level,
            "timestamp": time.time(),
            "event_type": event_type,
            "message": message,
            "data": data or {},
            "metrics": metrics or {},
        }

    def _should_log(self, level: str) -> bool:
        """
        Check if event level meets configured threshold.

        Args:
            level: Level of the event to check.

        Returns:
            True if the event should be logged.
        """
        levels = {"debug": 0, "info": 1, "warn": 2, "error": 3}
        return levels.get(level, 1) >= levels.get(self._level, 1)

    def log(
        self,
        node: str,
        level: str,
        event_type: str,
        message: Optional[str] = None,
        data: Optional[Dict[str, Any]] = None,
        metrics: Optional[Dict[str, Any]] = None,
    ) -> None:
        """
        Log a structured event to the event stream.

        Args:
            node: Name of the node generating the event.
            level: Log level (debug, info, warn, error).
            event_type: Type of event (entry, exit, error, metric).
            message: Optional message text.
            data: Optional data dictionary.
            metrics: Optional metrics dictionary.
        """
        event = self._create_event(node, level, event_type, message, data, metrics)
        if self._should_log(level):
            self._event_stream.append(event)
            # Also log to TraceContext for span correlation
            self._trace_context.log_event(message=message, metrics=metrics)

    def start_node_span(
        self, node: str, metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Start a span for a node and emit entry event.

        Args:
            node: Name of the node.
            metadata: Optional metadata to attach to the span.

        Returns:
            The span ID as a string.
        """
        # Delegate to TraceContext
        span = self._trace_context.start_span(name=node, metadata=metadata)
        span_id = span["span_id"]

        with self._lock:
            self._node_spans[node] = span_id

        # Emit entry event
        self.log(node, "info", "entry", f"Starting {node}", data=metadata)
        return span_id

    def end_node_span(
        self, node: str, status: str = "ok", error: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        End a node's span and emit exit/error event.

        Args:
            node: Name of the node.
            status: Status of the span ("ok" or "error").
            error: Optional error message if status is "error".

        Returns:
            The completed span dictionary, or None if no span was active.
        """
        # Delegate to TraceContext
        completed = self._trace_context.end_span(status=status, error=error)

        # Emit exit or error event
        if status == "error":
            self.log(node, "error", "error", f"Error in {node}: {error}")
        else:
            duration = completed.get("duration_ms", 0) if completed else 0
            self.log(
                node,
                "info",
                "exit",
                f"Completed {node}",
                metrics={"duration_ms": duration},
            )

        # Clean up node-span mapping
        with self._lock:
            self._node_spans.pop(node, None)

        return completed

    def get_flow_log(self) -> Dict[str, Any]:
        """
        Return complete flow trace with events, spans, and metrics.

        Returns:
            Dictionary containing:
            - flow_id: The flow identifier
            - events: All buffered events in order
            - spans: All completed spans from TraceContext
            - metrics: Aggregate metrics (total_duration_ms, node_count, etc.)
            - timeline: Ordered timeline of all activity
        """
        events = self._event_stream.get_all()
        spans = self._trace_context.completed_spans.copy()

        # Build unified timeline
        timeline = sorted(
            [{"type": "event", **e} for e in events]
            + [{"type": "span", **s} for s in spans],
            key=lambda x: x.get("timestamp", x.get("start_time", 0)),
        )

        # Calculate aggregate metrics
        error_count = sum(1 for e in events if e.get("event_type") == "error")
        node_count = len(set(e.get("node") for e in events if e.get("node")))
        total_duration = sum(s.get("duration_ms", 0) for s in spans)

        return {
            "flow_id": self.flow_id,
            "events": events,
            "spans": spans,
            "metrics": {
                "total_duration_ms": total_duration,
                "node_count": node_count,
                "error_count": error_count,
                "event_count": len(events),
            },
            "timeline": timeline,
        }

    @property
    def trace_context(self) -> TraceContext:
        """Access the underlying TraceContext for direct manipulation."""
        return self._trace_context

    @property
    def event_stream(self) -> EventStream:
        """Access the underlying EventStream for direct queries."""
        return self._event_stream
