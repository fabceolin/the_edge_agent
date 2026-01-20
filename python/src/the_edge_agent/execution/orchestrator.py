"""
Execution Orchestrator for TEA.

This module provides the BaseExecutor abstract class and ExecutionOrchestrator
factory that selects the appropriate execution strategy based on configuration.

Story: TEA-RALPHY-001.6 (Execution Modes)

Example:
    >>> from the_edge_agent.execution import ExecutionOrchestrator, ExecutionConfig
    >>>
    >>> config = ExecutionConfig.from_yaml(settings)
    >>> orchestrator = ExecutionOrchestrator(config)
    >>> result = await orchestrator.run(tasks, state)
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, List

from .mode import ExecutionConfig, ExecutionMode


class BaseExecutor(ABC):
    """
    Abstract base class for execution strategies.

    Each execution mode (sequential, parallel, graph) implements this interface
    to provide consistent execution behavior.
    """

    def __init__(self, config: ExecutionConfig):
        """
        Initialize the executor with configuration.

        Args:
            config: ExecutionConfig with mode-specific settings
        """
        self.config = config

    @abstractmethod
    async def execute(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute tasks according to the strategy.

        Args:
            tasks: List of task dictionaries with at least 'id' and 'description'
            state: Initial state dictionary for execution context

        Returns:
            Dictionary with execution results:
            {
                "mode": str,
                "status": "success" | "conflict" | "error",
                "tasks_completed": int,
                "results": List[Dict],
                ...  # mode-specific fields
            }
        """
        pass

    def validate_tasks(self, tasks: List[Dict[str, Any]]) -> None:
        """
        Validate task list structure.

        Args:
            tasks: List of task dictionaries

        Raises:
            ValueError: If tasks are invalid
        """
        if not tasks:
            raise ValueError("Task list cannot be empty")

        for i, task in enumerate(tasks):
            if not isinstance(task, dict):
                raise ValueError(f"Task {i} must be a dictionary, got {type(task)}")
            if "id" not in task:
                raise ValueError(f"Task {i} must have an 'id' field")


class ExecutionOrchestrator:
    """
    Factory for selecting and managing execution strategies.

    The orchestrator creates the appropriate executor based on the configured
    mode and provides a unified interface for running tasks.
    """

    def __init__(self, config: ExecutionConfig):
        """
        Initialize the orchestrator with configuration.

        Args:
            config: ExecutionConfig specifying mode and settings
        """
        self.config = config
        self._executor = self._create_executor()

    def _create_executor(self) -> BaseExecutor:
        """
        Create the appropriate executor for the configured mode.

        Returns:
            BaseExecutor subclass instance
        """
        if self.config.mode == ExecutionMode.SEQUENTIAL:
            from .sequential import SequentialExecutor

            return SequentialExecutor(self.config)
        elif self.config.mode == ExecutionMode.PARALLEL:
            from .parallel import ParallelExecutor

            return ParallelExecutor(self.config)
        elif self.config.mode == ExecutionMode.GRAPH:
            from .graph import GraphExecutor

            return GraphExecutor(self.config)
        else:
            raise ValueError(f"Unknown execution mode: {self.config.mode}")

    @property
    def executor(self) -> BaseExecutor:
        """Get the underlying executor instance."""
        return self._executor

    async def run(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute tasks using the configured strategy.

        Args:
            tasks: List of task dictionaries
            state: Initial state for execution

        Returns:
            Execution results dictionary
        """
        # Validate tasks
        self._executor.validate_tasks(tasks)

        # Execute with the selected strategy
        return await self._executor.execute(tasks, state)

    def get_mode(self) -> ExecutionMode:
        """Get the current execution mode."""
        return self.config.mode

    def get_config_summary(self) -> Dict[str, Any]:
        """
        Get a summary of the current configuration.

        Returns:
            Dictionary with key configuration values for logging
        """
        return {
            "mode": self.config.mode.value,
            "target_branch": self.config.target_branch,
            "max_parallel": (
                self.config.max_parallel
                if self.config.mode == ExecutionMode.PARALLEL
                else None
            ),
            "dot_output_path": (
                self.config.dot_output_path
                if self.config.mode == ExecutionMode.GRAPH
                else None
            ),
        }
