"""
Trace Exporters for The Edge Agent.

This module provides exporters for sending trace data to external
observability platforms. Exporters implement the TraceExporter protocol
defined in tracing.py.

Available Exporters:
    - OpikExporter: Export spans to Comet Opik platform

Example:
    >>> from the_edge_agent.exporters import OpikExporter
    >>> from the_edge_agent import YAMLEngine
    >>>
    >>> # Use Opik exporter directly
    >>> exporter = OpikExporter(project_name="my-agent")
    >>> engine = YAMLEngine(trace_exporter=[exporter])
    >>>
    >>> # Or use shorthand configuration
    >>> engine = YAMLEngine(trace_exporter="opik")
"""

from .opik_exporter import OpikExporter

__all__ = ["OpikExporter"]
