"""
Tracing Infrastructure for YAMLEngine (TEA-BUILTIN-001.3).

This module provides distributed tracing capabilities for observing agent
workflow execution. It includes a TraceContext for managing hierarchical
spans and multiple exporter backends for outputting trace data.

Tracing is thread-safe and supports parallel execution through thread-local
span stacks. Each span captures timing, metadata, events, and metrics.

Example:
    >>> from the_edge_agent.tracing import TraceContext, ConsoleExporter
    >>>
    >>> # Create context with console output
    >>> ctx = TraceContext(exporters=[ConsoleExporter(verbose=True)])
    >>>
    >>> # Start a span
    >>> span = ctx.start_span("process_request", metadata={"user": "alice"})
    >>>
    >>> # Log events during processing
    >>> ctx.log_event("validation started")
    >>> ctx.log_event(metrics={"items_processed": 42})
    >>>
    >>> # End the span
    >>> ctx.end_span(status="ok")
    >>>
    >>> # Access completed spans
    >>> print(len(ctx.completed_spans))
    1

Exporters:
    - ConsoleExporter: Print spans to stdout (verbose or summary mode)
    - FileExporter: Write spans to JSON lines file
    - CallbackExporter: Call user-provided function with each span

Custom Exporter:
    >>> class MyExporter:
    ...     def export(self, span: dict) -> None:
    ...         # Send to your observability platform
    ...         pass
"""

import copy
import json
import threading
import time
import uuid
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Protocol


class TraceExporter(Protocol):
    """Protocol for trace exporters."""

    def export(self, span: Dict[str, Any]) -> None:
        """Export a completed span."""
        ...


class ConsoleExporter:
    """Export spans to console (stdout)."""

    def __init__(self, verbose: bool = False):
        """
        Initialize console exporter.

        Args:
            verbose: If True, print full span details. If False, print summary only.
        """
        self.verbose = verbose

    def export(self, span: Dict[str, Any]) -> None:
        """Print span to console."""
        duration = span.get("duration_ms", 0)
        status = span.get("status", "ok")
        name = span.get("name", "unknown")

        if self.verbose:
            # Full details
            print(f"[TRACE] {name}")
            print(f"  span_id: {span.get('span_id', 'unknown')}")
            if span.get("parent_id"):
                print(f"  parent_id: {span['parent_id']}")
            print(f"  duration: {duration:.2f}ms")
            print(f"  status: {status}")
            if span.get("error"):
                print(f"  error: {span['error']}")
            if span.get("metadata"):
                print(f"  metadata: {span['metadata']}")
            if span.get("events"):
                print(f"  events: {len(span['events'])} event(s)")
                for event in span["events"]:
                    print(f"    - {event.get('message', event)}")
            if span.get("metrics"):
                print(f"  metrics: {span['metrics']}")
        else:
            # Summary only
            status_icon = "\u2713" if status == "ok" else "\u2717"
            print(f"[TRACE] {status_icon} {name} ({duration:.2f}ms) - {status}")


class FileExporter:
    """Export spans to a JSON lines file."""

    def __init__(self, path: str):
        """
        Initialize file exporter.

        Args:
            path: File path to write spans to (JSON lines format).
        """
        self.path = Path(path)
        self._lock = threading.Lock()
        # Ensure parent directory exists
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def export(self, span: Dict[str, Any]) -> None:
        """Append span to file as JSON line."""
        with self._lock:
            with open(self.path, 'a', encoding='utf-8') as f:
                f.write(json.dumps(span, default=str) + '\n')


class CallbackExporter:
    """Export spans via user-provided callback function."""

    def __init__(self, callback: Callable[[Dict[str, Any]], None]):
        """
        Initialize callback exporter.

        Args:
            callback: Function to call with each completed span.
        """
        self.callback = callback

    def export(self, span: Dict[str, Any]) -> None:
        """Call the callback with span data."""
        try:
            self.callback(span)
        except Exception:
            # Swallow errors to not break graph execution
            pass


class TraceContext:
    """
    Thread-safe context manager for distributed tracing.

    Manages a stack of spans for hierarchical tracing. Each thread maintains
    its own span stack to support parallel execution.

    Example:
        >>> ctx = TraceContext(exporters=[ConsoleExporter()])
        >>> span = ctx.start_span("operation", metadata={"key": "value"})
        >>> ctx.log_event("processing started")
        >>> ctx.end_span(status="ok")
    """

    def __init__(self, exporters: Optional[List[Any]] = None):
        """
        Initialize trace context.

        Args:
            exporters: List of TraceExporter instances for span export.
                      Defaults to empty list (spans collected but not exported).
        """
        self.exporters = exporters or []
        self._lock = threading.Lock()
        # Thread-local storage for span stacks (supports parallel execution)
        self._local = threading.local()
        # Completed spans (for inspection/testing)
        self.completed_spans: List[Dict[str, Any]] = []

    def _get_span_stack(self) -> List[Dict[str, Any]]:
        """Get the span stack for the current thread."""
        if not hasattr(self._local, 'span_stack'):
            self._local.span_stack = []
        return self._local.span_stack

    def start_span(
        self,
        name: str,
        metadata: Optional[Dict[str, Any]] = None,
        parent_id: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Start a new span.

        Args:
            name: Name of the span (operation being traced).
            metadata: Optional metadata to attach to the span.
            parent_id: Optional explicit parent span ID. If not provided,
                      auto-parents to the current span.

        Returns:
            The created span dictionary.
        """
        span_stack = self._get_span_stack()

        # Auto-parent to current span if no explicit parent
        if parent_id is None and span_stack:
            parent_id = span_stack[-1]["span_id"]

        span = {
            "span_id": str(uuid.uuid4()),
            "parent_id": parent_id,
            "name": name,
            "start_time": time.time(),
            "end_time": None,
            "duration_ms": None,
            "status": "ok",
            "error": None,
            "metadata": metadata or {},
            "events": [],
            "metrics": {}
        }

        span_stack.append(span)
        return span

    def current_span(self) -> Optional[Dict[str, Any]]:
        """Get the current active span, or None if no span is active."""
        span_stack = self._get_span_stack()
        return span_stack[-1] if span_stack else None

    def log_event(
        self,
        message: Optional[str] = None,
        event: Optional[Dict[str, Any]] = None,
        metrics: Optional[Dict[str, Any]] = None,
        snapshot_state: bool = False,
        state: Optional[Dict[str, Any]] = None,
        sanitize_keys: Optional[List[str]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Log an event or metrics to the current span.

        Args:
            message: Optional message to log.
            event: Optional event dictionary with additional data.
            metrics: Optional metrics to merge into span's metrics.
            snapshot_state: If True, snapshot the current state.
            state: State to snapshot (required if snapshot_state=True).
            sanitize_keys: Keys to exclude from state snapshot (e.g., secrets).

        Returns:
            The current span, or None if no span is active.
        """
        current = self.current_span()
        if current is None:
            return None

        timestamp = time.time()

        # Build event record
        event_record = {"timestamp": timestamp}
        if message:
            event_record["message"] = message
        if event:
            event_record.update(event)

        # State snapshot
        if snapshot_state and state is not None:
            snapshot = copy.deepcopy(state)
            # Sanitize sensitive keys
            if sanitize_keys:
                for key in sanitize_keys:
                    if key in snapshot:
                        snapshot[key] = "[REDACTED]"
            event_record["state_snapshot"] = snapshot

        current["events"].append(event_record)

        # Merge metrics
        if metrics:
            current["metrics"].update(metrics)

        # Auto-capture token usage if present in state
        if state and "usage" in state:
            usage = state["usage"]
            if isinstance(usage, dict):
                for key in ("prompt_tokens", "completion_tokens", "total_tokens"):
                    if key in usage:
                        current["metrics"][key] = current["metrics"].get(key, 0) + usage[key]

        return current

    def end_span(
        self,
        status: str = "ok",
        error: Optional[str] = None
    ) -> Optional[Dict[str, Any]]:
        """
        End the current span and export it.

        Args:
            status: Span status ("ok" or "error").
            error: Optional error message if status is "error".

        Returns:
            The completed span, or None if no span was active.
        """
        span_stack = self._get_span_stack()
        if not span_stack:
            return None

        span = span_stack.pop()
        span["end_time"] = time.time()
        span["duration_ms"] = (span["end_time"] - span["start_time"]) * 1000
        span["status"] = status
        if error:
            span["error"] = error

        # Store completed span (thread-safe)
        with self._lock:
            self.completed_spans.append(span)

        # Export to all exporters (errors don't crash graph execution)
        for exporter in self.exporters:
            try:
                exporter.export(span)
            except Exception:
                # Silently ignore exporter errors
                pass

        return span

    def clear(self) -> None:
        """Clear all completed spans and reset state."""
        with self._lock:
            self.completed_spans.clear()
        self._local.span_stack = []
