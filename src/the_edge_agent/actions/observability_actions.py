"""
Observability Actions for YAMLEngine (TEA-BUILTIN-001.3).

This module provides tracing and observability actions for YAMLEngine workflows.
Actions integrate with the TraceContext for distributed tracing support.

Actions:
    - trace.start: Start a new trace span
    - trace.log: Log events, metrics, or state snapshots to current span
    - trace.end: End the current trace span

Example:
    >>> # Start a span
    >>> result = registry['trace.start'](
    ...     state={},
    ...     name="process_request",
    ...     metadata={"user_id": "123"}
    ... )
    >>> print(result['span_id'])

    >>> # Log events
    >>> registry['trace.log'](
    ...     state={"processed": 42},
    ...     message="Processing complete",
    ...     metrics={"items_processed": 42}
    ... )

    >>> # End the span
    >>> result = registry['trace.end'](state={}, status="ok")
    >>> print(f"Duration: {result['duration_ms']}ms")
"""

from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register observability actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing trace context
    """

    def trace_start(state, name, metadata=None, parent_id=None, **kwargs):
        """
        Start a new trace span.

        Args:
            state: Current state dictionary
            name: Name of the span (operation being traced)
            metadata: Optional metadata to attach to the span
            parent_id: Optional explicit parent span ID. If not provided,
                      auto-parents to the current span.

        Returns:
            {"span_id": str, "name": str, "parent_id": Optional[str], "success": True}
            Or {"error": str, "success": False} if tracing is disabled
        """
        if not engine._enable_tracing or engine._trace_context is None:
            return {
                "success": False,
                "error": "Tracing is not enabled"
            }

        span = engine._trace_context.start_span(
            name=name,
            metadata=metadata,
            parent_id=parent_id
        )

        return {
            "span_id": span["span_id"],
            "name": span["name"],
            "parent_id": span["parent_id"],
            "success": True
        }

    registry['trace.start'] = trace_start
    registry['actions.trace_start'] = trace_start

    def trace_log(
        state,
        message=None,
        event=None,
        metrics=None,
        snapshot_state=False,
        sanitize_keys=None,
        **kwargs
    ):
        """
        Log an event, metrics, or state snapshot to the current span.

        Args:
            state: Current state dictionary
            message: Optional message to log
            event: Optional event dictionary with additional data
            metrics: Optional metrics dictionary to merge into span
            snapshot_state: If True, capture current state in the event
            sanitize_keys: List of state keys to redact in snapshot

        Returns:
            {"logged": True, "span_id": str, "event_count": int, "success": True}
            Or {"success": False, "error": str} if no active span or tracing disabled
        """
        if not engine._enable_tracing or engine._trace_context is None:
            return {
                "success": False,
                "error": "Tracing is not enabled"
            }

        current = engine._trace_context.log_event(
            message=message,
            event=event,
            metrics=metrics,
            snapshot_state=snapshot_state,
            state=state,
            sanitize_keys=sanitize_keys
        )

        if current is None:
            return {
                "success": False,
                "error": "No active span to log to",
                "logged": False
            }

        return {
            "logged": True,
            "span_id": current["span_id"],
            "event_count": len(current["events"]),
            "success": True
        }

    registry['trace.log'] = trace_log
    registry['actions.trace_log'] = trace_log

    def trace_end(state, status="ok", error=None, **kwargs):
        """
        End the current trace span.

        Args:
            state: Current state dictionary
            status: Span status - "ok" or "error" (default: "ok")
            error: Optional error message if status is "error"

        Returns:
            {"span_id": str, "duration_ms": float, "status": str, "success": True}
            Or {"success": False, "error": str} if no active span or tracing disabled
        """
        if not engine._enable_tracing or engine._trace_context is None:
            return {
                "success": False,
                "error": "Tracing is not enabled"
            }

        span = engine._trace_context.end_span(status=status, error=error)

        if span is None:
            return {
                "success": False,
                "error": "No active span to end"
            }

        return {
            "span_id": span["span_id"],
            "duration_ms": span["duration_ms"],
            "status": span["status"],
            "success": True
        }

    registry['trace.end'] = trace_end
    registry['actions.trace_end'] = trace_end
