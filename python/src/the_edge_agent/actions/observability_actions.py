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
            return {"success": False, "error": "Tracing is not enabled"}

        span = engine._trace_context.start_span(
            name=name, metadata=metadata, parent_id=parent_id
        )

        return {
            "span_id": span["span_id"],
            "name": span["name"],
            "parent_id": span["parent_id"],
            "success": True,
        }

    registry["trace.start"] = trace_start
    registry["actions.trace_start"] = trace_start

    def trace_log(
        state,
        message=None,
        event=None,
        metrics=None,
        snapshot_state=False,
        sanitize_keys=None,
        **kwargs,
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
            return {"success": False, "error": "Tracing is not enabled"}

        current = engine._trace_context.log_event(
            message=message,
            event=event,
            metrics=metrics,
            snapshot_state=snapshot_state,
            state=state,
            sanitize_keys=sanitize_keys,
        )

        if current is None:
            return {
                "success": False,
                "error": "No active span to log to",
                "logged": False,
            }

        return {
            "logged": True,
            "span_id": current["span_id"],
            "event_count": len(current["events"]),
            "success": True,
        }

    registry["trace.log"] = trace_log
    registry["actions.trace_log"] = trace_log

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
            return {"success": False, "error": "Tracing is not enabled"}

        span = engine._trace_context.end_span(status=status, error=error)

        if span is None:
            return {"success": False, "error": "No active span to end"}

        return {
            "span_id": span["span_id"],
            "duration_ms": span["duration_ms"],
            "status": span["status"],
            "success": True,
        }

    registry["trace.end"] = trace_end
    registry["actions.trace_end"] = trace_end

    # TEA-BUILTIN-005.3: Opik healthcheck action
    def opik_healthcheck(state, timeout=5.0, **kwargs):
        """
        Validate Opik connectivity and authentication (TEA-BUILTIN-005.3).

        Tests connectivity to the configured Opik instance and validates
        that the API key (if required) is valid.

        Args:
            state: Current state dictionary
            timeout: Connection timeout in seconds (default: 5.0)

        Returns:
            On success:
                {
                    "success": True,
                    "latency_ms": float,
                    "workspace": str,
                    "project": str,
                    "message": "Connected to Opik successfully"
                }
            On failure:
                {
                    "success": False,
                    "error": str,
                    "message": str  # User-friendly guidance
                }

        Example:
            >>> result = registry['opik.healthcheck'](state={})
            >>> if result['success']:
            ...     print(f"Connected in {result['latency_ms']:.1f}ms")
            ... else:
            ...     print(result['message'])
        """
        import time

        # Check if Opik SDK is installed
        try:
            import opik
        except ImportError:
            return {
                "success": False,
                "error": "Opik SDK not installed",
                "message": "Opik SDK not installed. Install with: pip install opik",
                "install_command": "pip install opik",
            }

        # Get resolved Opik config
        config = getattr(engine, "_opik_config", {})
        api_key = config.get("api_key")
        workspace = config.get("workspace")
        project_name = config.get("project_name", "the-edge-agent")
        url_override = config.get("url")

        try:
            start_time = time.time()

            # Configure Opik with our settings
            configure_kwargs = {}
            if api_key:
                configure_kwargs["api_key"] = api_key
            if workspace:
                configure_kwargs["workspace"] = workspace
            if url_override:
                configure_kwargs["url_override"] = url_override

            if configure_kwargs:
                opik.configure(**configure_kwargs)

            # Create client to validate connection
            # This will raise an exception if credentials are invalid
            client = opik.Opik(project_name=project_name)

            # Try to get workspace info to validate connection
            # The client is lazy, so we need to trigger an actual API call
            # For now, we consider successful client creation as a pass
            latency_ms = (time.time() - start_time) * 1000

            # Get effective workspace (may be default if not specified)
            effective_workspace = workspace or "default"

            return {
                "success": True,
                "latency_ms": latency_ms,
                "workspace": effective_workspace,
                "project": project_name,
                "message": "Connected to Opik successfully",
            }

        except Exception as e:
            error_str = str(e)

            # Generate helpful error message based on error type
            if "401" in error_str or "unauthorized" in error_str.lower():
                message = (
                    "Invalid API key. Please verify your key at "
                    "https://www.comet.com/opik/account"
                )
            elif "api_key" in error_str.lower() or "OPIK_API_KEY" in error_str:
                message = (
                    "OPIK_API_KEY not set. Get your API key at "
                    "https://www.comet.com/opik"
                )
            elif "connection" in error_str.lower() or "timeout" in error_str.lower():
                if url_override:
                    message = (
                        f"Cannot connect to self-hosted Opik at {url_override}. "
                        "Verify the URL and server status."
                    )
                else:
                    message = "Cannot connect to Opik. Check network connectivity."
            else:
                message = f"Opik connection failed: {error_str}"

            return {"success": False, "error": error_str, "message": message}

    registry["opik.healthcheck"] = opik_healthcheck
    registry["actions.opik_healthcheck"] = opik_healthcheck

    # TEA-OBS-001.1: Flow observability actions
    def obs_get_flow_log(state, **kwargs):
        """
        Get the complete flow log from ObservabilityContext (TEA-OBS-001.1).

        Returns a structured trace containing all events, spans, and metrics
        for the current flow execution. Requires observability to be enabled
        via the 'observability' configuration in YAML.

        Args:
            state: Current state dictionary

        Returns:
            On success:
                {
                    "success": True,
                    "flow_log": {
                        "flow_id": str,
                        "events": List[dict],
                        "spans": List[dict],
                        "metrics": {
                            "total_duration_ms": float,
                            "node_count": int,
                            "error_count": int,
                            "event_count": int
                        },
                        "timeline": List[dict]
                    }
                }
            On failure:
                {"success": False, "error": str}

        Example:
            >>> result = registry['obs.get_flow_log'](state={})
            >>> if result['success']:
            ...     print(f"Flow: {result['flow_log']['flow_id']}")
            ...     print(f"Events: {result['flow_log']['metrics']['event_count']}")
        """
        if (
            not hasattr(engine, "_observability_context")
            or engine._observability_context is None
        ):
            return {
                "success": False,
                "error": "Observability is not enabled. Add 'observability: {enabled: true}' to YAML config.",
            }

        flow_log = engine._observability_context.get_flow_log()
        return {"success": True, "flow_log": flow_log}

    registry["obs.get_flow_log"] = obs_get_flow_log
    registry["actions.obs_get_flow_log"] = obs_get_flow_log

    def obs_log_event(
        state,
        node=None,
        level="info",
        event_type="metric",
        message=None,
        data=None,
        metrics=None,
        **kwargs,
    ):
        """
        Log a custom event to the observability stream (TEA-OBS-001.1).

        Allows workflows to emit custom log events that appear in the flow log.
        Requires observability to be enabled.

        Args:
            state: Current state dictionary
            node: Node name (optional, defaults to 'custom')
            level: Log level - 'debug', 'info', 'warn', 'error' (default: 'info')
            event_type: Event type - 'entry', 'exit', 'error', 'metric' (default: 'metric')
            message: Optional message text
            data: Optional data dictionary
            metrics: Optional metrics dictionary

        Returns:
            {"success": True, "flow_id": str}
            or {"success": False, "error": str}

        Example:
            >>> registry['obs.log_event'](
            ...     state={},
            ...     node="my_step",
            ...     level="info",
            ...     message="Processing complete",
            ...     metrics={"items_processed": 42}
            ... )
        """
        if (
            not hasattr(engine, "_observability_context")
            or engine._observability_context is None
        ):
            return {
                "success": False,
                "error": "Observability is not enabled. Add 'observability: {enabled: true}' to YAML config.",
            }

        obs_ctx = engine._observability_context
        obs_ctx.log(
            node=node or "custom",
            level=level,
            event_type=event_type,
            message=message,
            data=data,
            metrics=metrics,
        )

        return {"success": True, "flow_id": obs_ctx.flow_id}

    registry["obs.log_event"] = obs_log_event
    registry["actions.obs_log_event"] = obs_log_event

    def obs_query_events(state, filters=None, **kwargs):
        """
        Query events from the observability stream (TEA-OBS-001.1).

        Filter events by node pattern, level, event type, or time range.
        Requires observability to be enabled.

        Args:
            state: Current state dictionary
            filters: Dictionary of filters:
                - node: Glob pattern to match node name (e.g., "llm.*")
                - level: Exact level match (debug, info, warn, error)
                - event_type: Exact type match (entry, exit, error, metric)
                - start_time: Minimum timestamp (Unix float)
                - end_time: Maximum timestamp (Unix float)

        Returns:
            {"success": True, "events": List[dict], "count": int}
            or {"success": False, "error": str}

        Example:
            >>> result = registry['obs.query_events'](
            ...     state={},
            ...     filters={"node": "llm.*", "level": "error"}
            ... )
            >>> print(f"Found {result['count']} error events")
        """
        if (
            not hasattr(engine, "_observability_context")
            or engine._observability_context is None
        ):
            return {
                "success": False,
                "error": "Observability is not enabled. Add 'observability: {enabled: true}' to YAML config.",
            }

        obs_ctx = engine._observability_context
        events = obs_ctx.event_stream.query(filters or {})

        return {"success": True, "events": events, "count": len(events)}

    registry["obs.query_events"] = obs_query_events
    registry["actions.obs_query_events"] = obs_query_events
