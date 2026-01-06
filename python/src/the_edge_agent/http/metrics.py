"""
Prometheus Metrics Collection for TEA Server.

Story: TEA-BUILTIN-015.8 (Health & Metadata Endpoints)

Provides metrics collection and Prometheus-format export for agent executions.
Tracks execution counts, durations, and errors by agent and type.

Example:
    >>> from the_edge_agent.http import metrics
    >>>
    >>> # Record an execution
    >>> metrics.record_execution("research_agent", "success", 1.5)
    >>>
    >>> # Record an error
    >>> metrics.record_error("research_agent", "timeout")
    >>>
    >>> # Export as Prometheus format
    >>> print(metrics.to_prometheus())
"""

import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional

# Try to import FastAPI, but provide fallback if not available
try:
    from fastapi import Response

    FASTAPI_AVAILABLE = True
except ImportError:
    FASTAPI_AVAILABLE = False
    Response = None  # type: ignore


@dataclass
class MetricsCollector:
    """
    Collects agent execution metrics for Prometheus export.

    Thread-safe metrics collection with support for:
    - Execution counts by agent and status (success/error)
    - Duration tracking for histogram calculation
    - Error counts by agent and error type

    Attributes:
        executions: Dict mapping agent name to status counts
        durations: Dict mapping agent name to list of durations (seconds)
        errors: Dict mapping agent name to error type counts

    Example:
        >>> collector = MetricsCollector()
        >>> collector.record_execution("agent1", "success", 0.5)
        >>> collector.record_execution("agent1", "error", 1.2)
        >>> collector.record_error("agent1", "timeout")
        >>> print(collector.to_prometheus())
    """

    executions: Dict[str, Dict[str, int]] = field(default_factory=dict)
    durations: Dict[str, List[float]] = field(default_factory=dict)
    errors: Dict[str, Dict[str, int]] = field(default_factory=dict)
    _lock: threading.Lock = field(default_factory=threading.Lock)

    def record_execution(
        self,
        agent: str,
        status: str,
        duration_seconds: float,
    ) -> None:
        """
        Record an agent execution.

        Args:
            agent: Name of the agent that executed
            status: Execution status ("success" or "error")
            duration_seconds: Execution duration in seconds
        """
        with self._lock:
            if agent not in self.executions:
                self.executions[agent] = {}
                self.durations[agent] = []

            current = self.executions[agent].get(status, 0)
            self.executions[agent][status] = current + 1
            self.durations[agent].append(duration_seconds)

    def record_error(self, agent: str, error_type: str) -> None:
        """
        Record an error by type.

        Args:
            agent: Name of the agent that had the error
            error_type: Type/category of the error (e.g., "timeout", "llm_error")
        """
        with self._lock:
            if agent not in self.errors:
                self.errors[agent] = {}
            current = self.errors[agent].get(error_type, 0)
            self.errors[agent][error_type] = current + 1

    def get_execution_count(
        self,
        agent: Optional[str] = None,
        status: Optional[str] = None,
    ) -> int:
        """
        Get execution count, optionally filtered by agent and/or status.

        Args:
            agent: Filter by agent name (None = all agents)
            status: Filter by status (None = all statuses)

        Returns:
            Total execution count matching the filters.
        """
        with self._lock:
            total = 0
            agents = [agent] if agent else list(self.executions.keys())
            for a in agents:
                if a in self.executions:
                    if status:
                        total += self.executions[a].get(status, 0)
                    else:
                        total += sum(self.executions[a].values())
            return total

    def get_error_count(
        self,
        agent: Optional[str] = None,
        error_type: Optional[str] = None,
    ) -> int:
        """
        Get error count, optionally filtered by agent and/or error type.

        Args:
            agent: Filter by agent name (None = all agents)
            error_type: Filter by error type (None = all types)

        Returns:
            Total error count matching the filters.
        """
        with self._lock:
            total = 0
            agents = [agent] if agent else list(self.errors.keys())
            for a in agents:
                if a in self.errors:
                    if error_type:
                        total += self.errors[a].get(error_type, 0)
                    else:
                        total += sum(self.errors[a].values())
            return total

    def get_duration_stats(
        self,
        agent: Optional[str] = None,
    ) -> Dict[str, float]:
        """
        Get duration statistics for an agent.

        Args:
            agent: Agent name (None = aggregate all agents)

        Returns:
            Dictionary with sum, count, min, max, avg keys.
        """
        with self._lock:
            all_durations: List[float] = []

            if agent:
                all_durations = self.durations.get(agent, []).copy()
            else:
                for durations in self.durations.values():
                    all_durations.extend(durations)

            if not all_durations:
                return {"sum": 0.0, "count": 0, "min": 0.0, "max": 0.0, "avg": 0.0}

            return {
                "sum": sum(all_durations),
                "count": len(all_durations),
                "min": min(all_durations),
                "max": max(all_durations),
                "avg": sum(all_durations) / len(all_durations),
            }

    def reset(self) -> None:
        """Reset all metrics to initial state."""
        with self._lock:
            self.executions.clear()
            self.durations.clear()
            self.errors.clear()

    def to_prometheus(self) -> str:
        """
        Export metrics in Prometheus text format.

        Returns:
            Multi-line string in Prometheus text exposition format.

        Example output:
            # HELP tea_agent_executions_total Total agent executions
            # TYPE tea_agent_executions_total counter
            tea_agent_executions_total{agent="research",status="success"} 100
            tea_agent_executions_total{agent="research",status="error"} 5

            # HELP tea_agent_duration_seconds Agent execution duration
            # TYPE tea_agent_duration_seconds summary
            tea_agent_duration_seconds_sum{agent="research"} 150.5
            tea_agent_duration_seconds_count{agent="research"} 105
        """
        lines: List[str] = []

        with self._lock:
            # Executions counter
            lines.append("# HELP tea_agent_executions_total Total agent executions")
            lines.append("# TYPE tea_agent_executions_total counter")
            for agent, counts in sorted(self.executions.items()):
                for status, count in sorted(counts.items()):
                    lines.append(
                        f'tea_agent_executions_total{{agent="{agent}",status="{status}"}} {count}'
                    )

            # Duration summary (sum and count for now)
            if self.durations:
                lines.append("")
                lines.append(
                    "# HELP tea_agent_duration_seconds Agent execution duration"
                )
                lines.append("# TYPE tea_agent_duration_seconds summary")
                for agent, durations in sorted(self.durations.items()):
                    if durations:
                        duration_sum = sum(durations)
                        duration_count = len(durations)
                        lines.append(
                            f'tea_agent_duration_seconds_sum{{agent="{agent}"}} {duration_sum:.3f}'
                        )
                        lines.append(
                            f'tea_agent_duration_seconds_count{{agent="{agent}"}} {duration_count}'
                        )

            # Errors counter
            if self.errors:
                lines.append("")
                lines.append("# HELP tea_agent_errors_total Total agent errors by type")
                lines.append("# TYPE tea_agent_errors_total counter")
                for agent, error_counts in sorted(self.errors.items()):
                    for error_type, count in sorted(error_counts.items()):
                        lines.append(
                            f'tea_agent_errors_total{{agent="{agent}",error_type="{error_type}"}} {count}'
                        )

        return "\n".join(lines)


# Global metrics instance
metrics = MetricsCollector()


def get_metrics_endpoint():
    """
    Get a FastAPI endpoint handler for Prometheus metrics.

    Returns:
        Async function that returns metrics in Prometheus text format.

    Example:
        >>> from fastapi import FastAPI
        >>> app = FastAPI()
        >>> app.add_api_route("/metrics", get_metrics_endpoint(), methods=["GET"])
    """

    async def metrics_handler():
        """Return metrics in Prometheus text format."""
        if FASTAPI_AVAILABLE:
            return Response(
                content=metrics.to_prometheus(),
                media_type="text/plain",
            )
        else:
            return metrics.to_prometheus()

    return metrics_handler
