"""
OpikExporter for The Edge Agent (TEA-BUILTIN-005.1).

This module provides an exporter for sending trace spans to Comet Opik,
enabling visualization and analysis of agent execution in the Opik dashboard.

The exporter implements the TraceExporter protocol and integrates with
YAMLEngine's tracing infrastructure.

Example:
    >>> from the_edge_agent import YAMLEngine
    >>>
    >>> # Use shorthand configuration
    >>> engine = YAMLEngine(trace_exporter="opik")
    >>>
    >>> # Or configure with custom settings
    >>> from the_edge_agent.exporters import OpikExporter
    >>> exporter = OpikExporter(
    ...     project_name="my-agent",
    ...     workspace="my-workspace"
    ... )
    >>> engine = YAMLEngine(trace_exporter=[exporter])

Environment Variables:
    OPIK_API_KEY: API key for Opik Cloud (optional for self-hosted)
    OPIK_PROJECT_NAME: Project name in Opik (default: "the-edge-agent")
    OPIK_WORKSPACE: Workspace name (default: user's default workspace)
    OPIK_URL_OVERRIDE: Custom Opik URL for self-hosted instances
"""

import logging
import os
from datetime import datetime
from typing import Any, Dict, Optional

logger = logging.getLogger(__name__)


class OpikExporter:
    """
    Export trace spans to Comet Opik platform.

    This exporter converts TEA trace spans to Opik format and sends them
    to the Opik platform for visualization and analysis.

    Features:
        - Lazy configuration (only connects on first export)
        - Graceful degradation (export failures don't crash graph execution)
        - Parent-child span relationship preservation
        - Token usage metrics forwarding
        - Environment variable configuration

    Attributes:
        project_name: The Opik project to log spans to
        workspace: The Opik workspace (optional)
        api_key: API key for Opik Cloud (optional for self-hosted)
        url_override: Custom URL for self-hosted Opik

    Example:
        >>> exporter = OpikExporter(project_name="my-agent")
        >>> span = {
        ...     "span_id": "abc123",
        ...     "name": "llm_call",
        ...     "duration_ms": 150.0,
        ...     "status": "ok",
        ...     "metrics": {"prompt_tokens": 100}
        ... }
        >>> exporter.export(span)
    """

    def __init__(
        self,
        api_key: Optional[str] = None,
        project_name: Optional[str] = None,
        workspace: Optional[str] = None,
        url_override: Optional[str] = None
    ):
        """
        Initialize the Opik exporter.

        Args:
            api_key: API key for Opik Cloud. If not provided, reads from
                    OPIK_API_KEY environment variable.
            project_name: Project name in Opik. If not provided, reads from
                         OPIK_PROJECT_NAME env var (default: "the-edge-agent").
            workspace: Workspace name. If not provided, reads from
                      OPIK_WORKSPACE environment variable.
            url_override: Custom URL for self-hosted Opik. If not provided,
                         reads from OPIK_URL_OVERRIDE environment variable.
        """
        self._configured = False
        self._client = None

        # Configuration with environment variable fallbacks
        self._api_key = api_key or os.getenv("OPIK_API_KEY")
        self._project_name = project_name or os.getenv(
            "OPIK_PROJECT_NAME", "the-edge-agent"
        )
        self._workspace = workspace or os.getenv("OPIK_WORKSPACE")
        self._url_override = url_override or os.getenv("OPIK_URL_OVERRIDE")

        # Track active traces for parent-child relationships
        self._trace_map: Dict[str, Any] = {}

    def _check_opik_available(self) -> None:
        """
        Check if the opik package is installed.

        Raises:
            ImportError: If opik is not installed, with clear install instructions.
        """
        try:
            import opik  # noqa: F401
        except ImportError:
            raise ImportError(
                "Opik SDK not installed. Install with: pip install opik\n"
                "Or install The Edge Agent with Opik support: "
                "pip install the-edge-agent[opik]"
            )

    def _ensure_configured(self) -> None:
        """
        Lazy configuration on first export.

        Initializes the Opik client with the configured settings.
        Only called once, on the first span export.
        """
        if self._configured:
            return

        self._check_opik_available()

        import opik

        # Configure Opik with our settings
        # Note: opik.configure() handles defaults for missing values
        configure_kwargs = {}
        if self._api_key:
            configure_kwargs["api_key"] = self._api_key
        if self._workspace:
            configure_kwargs["workspace"] = self._workspace
        if self._url_override:
            configure_kwargs["url_override"] = self._url_override

        if configure_kwargs:
            opik.configure(**configure_kwargs)

        # Create client for logging spans
        # Note: Opik SDK auto-creates the project if it doesn't exist
        # when the first trace/span is exported
        self._client = opik.Opik(project_name=self._project_name)
        self._configured = True

        # Log at info level for project creation awareness (TEA-BUILTIN-005.3)
        logger.info(
            f"OpikExporter initialized for project '{self._project_name}' "
            f"(workspace: {self._workspace or 'default'}). "
            f"Project will be auto-created if it doesn't exist."
        )
        logger.debug(
            f"OpikExporter configured: project={self._project_name}, "
            f"workspace={self._workspace or 'default'}"
        )

    def _convert_span_to_opik(self, span: Dict[str, Any]) -> Dict[str, Any]:
        """
        Convert a TEA span to Opik span format.

        Args:
            span: TEA span dictionary with fields like span_id, name, etc.

        Returns:
            Dictionary with Opik-compatible span data.
        """
        # Convert Unix timestamps to datetime objects
        start_time = None
        end_time = None

        if span.get("start_time"):
            start_time = datetime.fromtimestamp(span["start_time"])
        if span.get("end_time"):
            end_time = datetime.fromtimestamp(span["end_time"])

        # Build metadata combining TEA metadata and events
        metadata = dict(span.get("metadata", {}))

        # Add events to metadata as a list
        events = span.get("events", [])
        if events:
            metadata["tea_events"] = events

        # Add error info to metadata if present
        if span.get("error"):
            metadata["error_message"] = span["error"]

        # Build the Opik span data
        opik_span = {
            "id": span.get("span_id"),
            "name": span.get("name", "unknown"),
            "start_time": start_time,
            "end_time": end_time,
            "metadata": metadata if metadata else None,
        }

        # Handle parent relationship
        parent_id = span.get("parent_id")
        if parent_id:
            opik_span["parent_span_id"] = parent_id

        # Handle metrics - particularly token usage
        metrics = span.get("metrics", {})
        if metrics:
            # Token usage is special - Opik expects specific fields
            usage = {}
            if "prompt_tokens" in metrics:
                usage["prompt_tokens"] = metrics["prompt_tokens"]
            if "completion_tokens" in metrics:
                usage["completion_tokens"] = metrics["completion_tokens"]
            if "total_tokens" in metrics:
                usage["total_tokens"] = metrics["total_tokens"]

            if usage:
                opik_span["usage"] = usage

            # Other metrics go to metadata
            other_metrics = {
                k: v for k, v in metrics.items()
                if k not in ("prompt_tokens", "completion_tokens", "total_tokens")
            }
            if other_metrics:
                if opik_span["metadata"] is None:
                    opik_span["metadata"] = {}
                opik_span["metadata"]["tea_metrics"] = other_metrics

        return opik_span

    def export(self, span: Dict[str, Any]) -> None:
        """
        Export a completed span to Opik.

        This method implements the TraceExporter protocol. It converts the
        TEA span format to Opik format and sends it to the Opik platform.

        Export failures are handled gracefully and logged at debug level.
        They will NOT crash graph execution.

        Args:
            span: Completed span dictionary from TraceContext.end_span().
                 Expected fields:
                 - span_id: Unique identifier
                 - parent_id: Parent span ID (optional)
                 - name: Span name
                 - start_time: Unix timestamp
                 - end_time: Unix timestamp
                 - duration_ms: Duration in milliseconds
                 - status: "ok" or "error"
                 - error: Error message (if status is "error")
                 - metadata: Additional key-value data
                 - events: List of events logged during span
                 - metrics: Metrics including token usage
        """
        try:
            self._ensure_configured()

            # Convert span to Opik format
            opik_span = self._convert_span_to_opik(span)

            # Determine if this is a trace root or a child span
            parent_id = span.get("parent_id")

            if parent_id is None:
                # This is a root span - create a trace
                trace = self._client.trace(
                    name=opik_span["name"],
                    start_time=opik_span["start_time"],
                    end_time=opik_span["end_time"],
                    metadata=opik_span.get("metadata"),
                )

                # Store trace reference for potential child spans
                self._trace_map[span["span_id"]] = trace

                logger.debug(
                    f"OpikExporter: created trace '{opik_span['name']}' "
                    f"(id={span['span_id']})"
                )
            else:
                # This is a child span - create a span under the parent's trace
                # Find the root trace for this span hierarchy
                trace = self._find_trace_for_parent(parent_id)

                if trace is not None:
                    trace.span(
                        name=opik_span["name"],
                        start_time=opik_span["start_time"],
                        end_time=opik_span["end_time"],
                        metadata=opik_span.get("metadata"),
                        parent_span_id=parent_id,
                        usage=opik_span.get("usage"),
                    )

                    logger.debug(
                        f"OpikExporter: created span '{opik_span['name']}' "
                        f"under parent {parent_id}"
                    )
                else:
                    # Parent trace not found - create as standalone trace
                    trace = self._client.trace(
                        name=opik_span["name"],
                        start_time=opik_span["start_time"],
                        end_time=opik_span["end_time"],
                        metadata=opik_span.get("metadata"),
                    )
                    self._trace_map[span["span_id"]] = trace

                    logger.debug(
                        f"OpikExporter: created standalone trace '{opik_span['name']}' "
                        f"(parent {parent_id} not found)"
                    )

        except ImportError:
            # Re-raise ImportError so users know they need to install opik
            raise
        except Exception as e:
            # Graceful degradation - log and continue
            logger.debug(f"OpikExporter: export failed: {e}")

    def _find_trace_for_parent(self, parent_id: str) -> Optional[Any]:
        """
        Find the trace object for a given parent span ID.

        Traverses up the span hierarchy to find the root trace.

        Args:
            parent_id: The parent span ID to find a trace for.

        Returns:
            The trace object if found, None otherwise.
        """
        # Check if parent is a root trace
        if parent_id in self._trace_map:
            return self._trace_map[parent_id]

        # Parent might be a child span - we stored its trace when created
        # For now, return None if not directly in trace_map
        # A more sophisticated implementation would maintain full hierarchy
        return None

    def flush(self) -> None:
        """
        Flush any pending exports.

        Called to ensure all spans are sent before shutdown.
        """
        if self._client is not None:
            try:
                self._client.flush()
                logger.debug("OpikExporter: flushed pending spans")
            except Exception as e:
                logger.debug(f"OpikExporter: flush failed: {e}")

    def end(self) -> None:
        """
        End the exporter and cleanup resources.

        Called during shutdown to properly close the Opik client.
        """
        if self._client is not None:
            try:
                self._client.end()
                logger.debug("OpikExporter: client ended")
            except Exception as e:
                logger.debug(f"OpikExporter: end failed: {e}")

        self._trace_map.clear()
        self._configured = False
        self._client = None
