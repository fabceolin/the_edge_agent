"""
Execution Mode Framework for TEA.

This module provides configurable execution modes for multi-task workflows:
- Sequential: Tasks execute one at a time in order
- Parallel: Tasks execute concurrently with isolated git worktrees
- Graph: LLM analyzes dependencies and generates DOT for optimal parallelization

Story: TEA-RALPHY-001.6 (Execution Modes)

Usage:
    >>> from the_edge_agent.execution import (
    ...     ExecutionMode,
    ...     ExecutionConfig,
    ...     ExecutionOrchestrator,
    ... )
    >>>
    >>> # Parse config from YAML settings
    >>> config = ExecutionConfig.from_yaml(settings)
    >>>
    >>> # Create orchestrator and run
    >>> orchestrator = ExecutionOrchestrator(config)
    >>> result = await orchestrator.run(tasks, state)
"""

from .mode import ExecutionMode, ExecutionConfig
from .orchestrator import ExecutionOrchestrator, BaseExecutor
from .sequential import SequentialExecutor
from .parallel import ParallelExecutor
from .graph import GraphExecutor

__all__ = [
    "ExecutionMode",
    "ExecutionConfig",
    "ExecutionOrchestrator",
    "BaseExecutor",
    "SequentialExecutor",
    "ParallelExecutor",
    "GraphExecutor",
]
