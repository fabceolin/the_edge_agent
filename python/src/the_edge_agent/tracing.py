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
import gzip
import json
import logging
import os
import queue
import threading
import time
import uuid
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Protocol, TypedDict, Union

logger = logging.getLogger(__name__)

# TEA-OBS-003.1: LLM payload capture metadata key.
# When llm.call is invoked under a capture-enabled node, the action injects
# a dict under metadata[LLM_PAYLOAD_KEY] containing the documented fields
# (messages_input, response_content, tokens_input, tokens_output, model,
# stop_reason, cost_usd). The slim FileExporter strips this key before
# writing; LlmPayloadFileExporter only writes spans containing it.
LLM_PAYLOAD_KEY = "llm_payload"

# TEA-OBS-003.1 (NFR-AC-3): First line of every *.llm.jsonl file is a
# # WARNING comment so downstream consumers (and humans grepping the file)
# see immediately that the contents may include PII. JSONL parsers that do
# not understand comments will need to skip lines starting with '#'.
PII_WARNING_HEADER = (
    "# WARNING: contains LLM request/response payloads. May contain PII "
    "(names, financial data, identifiers). Treat as sensitive.\n"
)


class LlmPayloadSpan(TypedDict, total=False):
    """Documented field schema (TEA-OBS-003.1 AC-4) for the dict written
    under span['metadata'][LLM_PAYLOAD_KEY] by the llm.call action when
    payload capture is active for the parent node."""

    messages_input: List[Dict[str, Any]]
    response_content: Optional[str]
    tokens_input: Optional[int]
    tokens_output: Optional[int]
    model: Optional[str]
    stop_reason: Optional[str]
    cost_usd: Optional[float]


def replace_binary_payloads(value: Any) -> Any:
    """Recursively replace bytes-like entries with documented placeholders.

    Per AC-5, binary attachments (PDF/image bytes) referenced by message
    content are not captured verbatim. Each bytes/bytearray/memoryview is
    replaced with ``{"type": "binary_omitted", "size_bytes": N}``. Scalars,
    strings, and other JSON-friendly types pass through unchanged. Tuples
    are converted to lists for JSON parity.
    """
    if isinstance(value, (bytes, bytearray, memoryview)):
        try:
            size = len(value)
        except TypeError:
            size = 0
        return {"type": "binary_omitted", "size_bytes": size}
    if isinstance(value, dict):
        return {k: replace_binary_payloads(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [replace_binary_payloads(item) for item in value]
    return value


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
    """Export spans to a JSON lines file.

    By default, strips the ``metadata[LLM_PAYLOAD_KEY]`` field (TEA-OBS-003.1)
    so that the spans-only file remains slim and never contains captured LLM
    request/response payloads. Pass ``strip_llm_payload=False`` to disable
    the projection (e.g., for legacy callers who never set the key anyway).
    """

    def __init__(self, path: str, strip_llm_payload: bool = True):
        """
        Initialize file exporter.

        Args:
            path: File path to write spans to (JSON lines format).
            strip_llm_payload: When True (default), removes the
                ``metadata[LLM_PAYLOAD_KEY]`` field before writing so that
                LLM payloads do not bleed into the slim spans file.
        """
        self.path = Path(path)
        self.strip_llm_payload = strip_llm_payload
        self._lock = threading.Lock()
        # Ensure parent directory exists
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def _project_for_slim(self, span: Dict[str, Any]) -> Dict[str, Any]:
        """Return a shallow copy with the LLM payload key stripped.

        Only allocates a new dict when the payload key is present, so the
        common (no payload) path stays cheap.
        """
        metadata = span.get("metadata")
        if not isinstance(metadata, dict) or LLM_PAYLOAD_KEY not in metadata:
            return span
        slim_metadata = {k: v for k, v in metadata.items() if k != LLM_PAYLOAD_KEY}
        slim_span = dict(span)
        slim_span["metadata"] = slim_metadata
        return slim_span

    def export(self, span: Dict[str, Any]) -> None:
        """Append span to file as JSON line."""
        if self.strip_llm_payload:
            span = self._project_for_slim(span)
        with self._lock:
            with open(self.path, 'a', encoding='utf-8') as f:
                f.write(json.dumps(span, default=str) + '\n')


class LlmPayloadFileExporter:
    """Export only spans containing an LLM payload to a separate JSONL file.

    Filters at the exporter boundary so non-LLM spans incur no I/O. The
    first record written to a fresh file is :data:`PII_WARNING_HEADER`,
    which is a JSONL comment line (downstream parsers must skip lines that
    start with ``#``).

    Used by Story TEA-OBS-003.1. When TEA-OBS-003.3 is in effect the
    constructor honours ``compress="gzip"`` to write ``.jsonl.gz`` instead.
    """

    def __init__(
        self,
        path: str,
        compress: Optional[str] = None,
    ):
        """
        Args:
            path: File path to write payload-bearing spans to.
            compress: Optional compression codec. Currently only ``"gzip"``
                is recognised; any other value is rejected. ``None`` (default)
                writes plain JSONL.
        """
        if compress is not None and compress != "gzip":
            raise ValueError(
                f"Unsupported compression codec: {compress!r}. "
                "Only 'gzip' is currently supported."
            )
        self.path = Path(path)
        self.compress = compress
        self._lock = threading.Lock()
        self._header_written = False
        self.path.parent.mkdir(parents=True, exist_ok=True)

    def _open(self):
        if self.compress == "gzip":
            return gzip.open(self.path, "at", encoding="utf-8")
        return open(self.path, "a", encoding="utf-8")

    def _file_is_empty(self) -> bool:
        try:
            return (not self.path.exists()) or self.path.stat().st_size == 0
        except OSError:
            return True

    def export(self, span: Dict[str, Any]) -> None:
        """Append span if (and only if) it carries an LLM payload."""
        metadata = span.get("metadata")
        if not isinstance(metadata, dict) or not metadata.get(LLM_PAYLOAD_KEY):
            return
        with self._lock:
            need_header = (not self._header_written) and self._file_is_empty()
            try:
                with self._open() as f:
                    if need_header:
                        f.write(PII_WARNING_HEADER)
                        self._header_written = True
                    try:
                        line = json.dumps(span, default=str) + "\n"
                    except (TypeError, ValueError) as exc:
                        logger.warning(
                            "LlmPayloadFileExporter: failed to serialize span: %s",
                            exc,
                        )
                        return
                    f.write(line)
            except OSError as exc:
                # AC: trace exporter exception must not break the LLM call.
                logger.error(
                    "LlmPayloadFileExporter: write failed for %s: %s",
                    self.path,
                    exc,
                )


class AsyncFileExporter:
    """Async writer wrapper for an underlying span exporter (TEA-OBS-003.3).

    Spans are placed on a bounded queue and a single background worker
    thread drains the queue, writing in batches. ``export()`` returns
    immediately after enqueue (target: <1ms), so the LLM call return path
    is no longer blocked on disk I/O on slow storage (NFS, cloud disk).

    The worker batches up to ``batch_size`` spans before flushing, or
    flushes after ``flush_interval_s`` seconds, whichever comes first. Each
    flush is a single ``open/write/close`` cycle on the wrapped exporter.

    ``overflow_policy='drop_newest'`` (default) drops new spans when the
    queue is full and increments :attr:`drops`. ``'block'`` causes
    ``export()`` to block until a slot is available — defeats the latency
    benefit but guarantees no data loss.

    Trade-off: spans buffered in the queue between flushes are lost on
    SIGKILL / power loss. Document this in observability.md (AC-19).
    """

    _SHUTDOWN_SENTINEL = object()

    def __init__(
        self,
        wrapped_exporter: Any,
        queue_size: int = 1000,
        flush_interval_s: float = 5.0,
        batch_size: int = 50,
        overflow_policy: str = "drop_newest",
        shutdown_timeout_s: float = 30.0,
    ):
        if queue_size < 1:
            raise ValueError("queue_size must be >= 1")
        # BOUNDS-001 (TEA-OBS-003.3 QA): cap queue_size at a sane upper
        # bound (~2.5GB worst-case at 25KB/span) and warn well before that
        # so misconfigurations surface at construction rather than at OOM.
        if queue_size > 100_000:
            raise ValueError(
                f"queue_size must be <= 100000 (got {queue_size}); "
                "values above this can exhaust memory at ~25KB/span."
            )
        if queue_size > 10_000:
            logger.warning(
                "AsyncFileExporter queue_size=%d is unusually large; "
                "memory usage may grow to ~%d MB at 25KB/span.",
                queue_size,
                (queue_size * 25) // 1024,
            )
        if flush_interval_s <= 0:
            raise ValueError("flush_interval_s must be > 0")
        if flush_interval_s < 0.01:
            raise ValueError(
                f"flush_interval_s must be >= 0.01s (got {flush_interval_s}); "
                "values below this thrash the worker thread."
            )
        if flush_interval_s > 3600:
            raise ValueError(
                f"flush_interval_s must be <= 3600s (got {flush_interval_s}); "
                "values above an hour delay flushes past most useful retention."
            )
        if batch_size < 1:
            raise ValueError("batch_size must be >= 1")
        if overflow_policy not in ("drop_newest", "block"):
            raise ValueError(
                f"Invalid overflow_policy {overflow_policy!r}; "
                "expected 'drop_newest' or 'block'"
            )
        self._wrapped = wrapped_exporter
        self.queue_size = queue_size
        self.flush_interval_s = flush_interval_s
        self.batch_size = batch_size
        self.overflow_policy = overflow_policy
        self.shutdown_timeout_s = shutdown_timeout_s

        # Introspection counters (proposed AC-20)
        self.drops = 0
        self.flush_count = 0
        self.last_flush_ts: Optional[float] = None

        self._queue: "queue.Queue[Any]" = queue.Queue(maxsize=queue_size)
        self._stop_event = threading.Event()
        self._worker_started = False
        self._worker_lock = threading.Lock()
        self._worker_thread: Optional[threading.Thread] = None
        self._last_overflow_warn_ts: float = 0.0
        self._closed = False

    @property
    def queue_depth(self) -> int:
        return self._queue.qsize()

    @property
    def worker_alive(self) -> bool:
        return self._worker_thread is not None and self._worker_thread.is_alive()

    def _ensure_worker(self) -> None:
        if self._worker_started:
            return
        with self._worker_lock:
            if self._worker_started:
                return
            self._worker_thread = threading.Thread(
                target=self._run_worker,
                name="AsyncFileExporter-worker",
                daemon=True,
            )
            self._worker_thread.start()
            self._worker_started = True

    def export(self, span: Dict[str, Any]) -> None:
        if self._closed:
            return
        self._ensure_worker()
        if self.overflow_policy == "drop_newest":
            try:
                self._queue.put_nowait(span)
            except queue.Full:
                self.drops += 1
                now = time.time()
                # Throttle WARN logging to ~1/min so we don't flood logs.
                if now - self._last_overflow_warn_ts > 60.0:
                    logger.warning(
                        "AsyncFileExporter queue full (size=%d); dropped %d spans so far.",
                        self.queue_size,
                        self.drops,
                    )
                    self._last_overflow_warn_ts = now
        else:
            # 'block' policy
            self._queue.put(span)

    def _flush_batch(self, batch: List[Dict[str, Any]]) -> None:
        for span in batch:
            try:
                self._wrapped.export(span)
            except Exception as exc:
                logger.error(
                    "AsyncFileExporter: wrapped exporter raised: %s", exc
                )
        self.flush_count += 1
        self.last_flush_ts = time.time()

    def _run_worker(self) -> None:
        batch: List[Dict[str, Any]] = []
        deadline = time.time() + self.flush_interval_s
        while True:
            timeout = max(0.0, deadline - time.time())
            try:
                item = self._queue.get(timeout=timeout)
            except queue.Empty:
                if batch:
                    self._flush_batch(batch)
                    batch = []
                deadline = time.time() + self.flush_interval_s
                if self._stop_event.is_set() and self._queue.empty():
                    return
                continue
            if item is self._SHUTDOWN_SENTINEL:
                if batch:
                    self._flush_batch(batch)
                    batch = []
                # Drain any remaining real spans queued before the sentinel.
                while True:
                    try:
                        leftover = self._queue.get_nowait()
                    except queue.Empty:
                        break
                    if leftover is self._SHUTDOWN_SENTINEL:
                        continue
                    batch.append(leftover)
                    if len(batch) >= self.batch_size:
                        self._flush_batch(batch)
                        batch = []
                if batch:
                    self._flush_batch(batch)
                return
            batch.append(item)
            if len(batch) >= self.batch_size:
                self._flush_batch(batch)
                batch = []
                deadline = time.time() + self.flush_interval_s

    def close(self, timeout: Optional[float] = None) -> None:
        """Drain queue and stop the worker thread.

        Idempotent: a second call is a no-op.
        """
        if self._closed:
            return
        self._closed = True
        if not self._worker_started or self._worker_thread is None:
            return
        self._stop_event.set()
        try:
            self._queue.put_nowait(self._SHUTDOWN_SENTINEL)
        except queue.Full:
            # Queue full of real spans — block to ensure sentinel lands so
            # the worker eventually drains. We use a short put with timeout
            # to avoid hanging forever.
            try:
                self._queue.put(self._SHUTDOWN_SENTINEL, timeout=1.0)
            except queue.Full:
                pass
        join_timeout = (
            timeout if timeout is not None else self.shutdown_timeout_s
        )
        drain_started_at = time.time()
        self._worker_thread.join(timeout=join_timeout)
        drain_duration = time.time() - drain_started_at
        if self._worker_thread.is_alive():
            # NFR-AC-22 / TECH-004: structured WARN with dropped, queue depth,
            # flush count, drain duration so post-mortems have all the fields
            # they need without log scraping or string regex.
            remaining = self._queue.qsize()
            logger.warning(
                "AsyncFileExporter: shutdown timeout exceeded (%.1fs); "
                "approximately %d spans may have been lost. "
                "drops=%d, flush_count=%d, queue_depth=%d, drain_s=%.3f",
                join_timeout,
                remaining,
                self.drops,
                self.flush_count,
                remaining,
                drain_duration,
                extra={
                    "async_exporter_event": "shutdown_timeout",
                    "dropped": self.drops,
                    "queue_depth": remaining,
                    "flush_count": self.flush_count,
                    "drain_duration_s": drain_duration,
                    "shutdown_timeout_s": join_timeout,
                },
            )


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
