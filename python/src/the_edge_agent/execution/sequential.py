"""
Sequential Executor for TEA.

This module implements the SequentialExecutor that executes tasks one at a time
in the order specified. No git worktrees are created - all work happens in
the current working directory.

Story: TEA-RALPHY-001.6 (Execution Modes)
Acceptance Criteria: 10, 11, 12

Example:
    >>> from the_edge_agent.execution import SequentialExecutor, ExecutionConfig
    >>>
    >>> config = ExecutionConfig(mode=ExecutionMode.SEQUENTIAL)
    >>> executor = SequentialExecutor(config)
    >>> result = await executor.execute(tasks, initial_state)
"""

import logging
import os
import subprocess
from typing import Any, Callable, Dict, List, Optional

from .orchestrator import BaseExecutor
from .mode import ExecutionConfig

logger = logging.getLogger(__name__)


class SequentialExecutor(BaseExecutor):
    """
    Execute tasks one at a time in specified order.

    Characteristics:
    - Tasks execute sequentially in the order provided
    - All tasks share the single working directory (no worktrees)
    - Single branch used for all changes
    - State accumulates across tasks (each task can access previous results)
    """

    def __init__(
        self,
        config: ExecutionConfig,
        task_executor: Optional[Callable] = None,
    ):
        """
        Initialize the sequential executor.

        Args:
            config: ExecutionConfig with execution settings
            task_executor: Optional custom task execution function.
                If not provided, uses default shell-based execution.
        """
        super().__init__(config)
        self._task_executor = task_executor or self._default_task_executor

    async def execute(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute tasks sequentially in order.

        Args:
            tasks: List of task dictionaries with 'id' and 'description'
            state: Initial state dictionary

        Returns:
            {
                "mode": "sequential",
                "status": "success" | "error",
                "tasks_completed": int,
                "results": List[Dict],
                "final_state": Dict
            }
        """
        results = []
        current_state = state.copy()
        tasks_completed = 0
        error_info = None

        # Get working directory
        working_dir = self.config.working_directory or os.getcwd()

        logger.info(
            f"Starting sequential execution of {len(tasks)} tasks in {working_dir}"
        )

        for i, task in enumerate(tasks):
            task_id = task.get("id", f"task-{i}")
            logger.info(f"Executing task {i+1}/{len(tasks)}: {task_id}")

            try:
                # Execute the task
                result = await self._execute_task(task, current_state, working_dir)

                results.append(
                    {
                        "task_id": task_id,
                        "success": result.get("success", True),
                        "output": result,
                    }
                )

                # Update state with task results
                if result.get("success", True):
                    tasks_completed += 1
                    # Merge task output into state for next task
                    current_state = {**current_state, **result}
                else:
                    # Task failed - record error but continue
                    error_info = {
                        "task_id": task_id,
                        "error": result.get("error", "Unknown error"),
                    }
                    logger.warning(f"Task {task_id} failed: {error_info['error']}")
                    # Optionally break on first error (configurable)
                    # For now, continue with remaining tasks

            except Exception as e:
                logger.error(f"Exception executing task {task_id}: {e}")
                results.append(
                    {
                        "task_id": task_id,
                        "success": False,
                        "error": str(e),
                    }
                )
                error_info = {"task_id": task_id, "error": str(e)}

        status = "success" if error_info is None else "partial"
        if tasks_completed == 0 and len(tasks) > 0:
            status = "error"

        logger.info(
            f"Sequential execution complete: {tasks_completed}/{len(tasks)} tasks succeeded"
        )

        return {
            "mode": "sequential",
            "status": status,
            "tasks_completed": tasks_completed,
            "tasks_total": len(tasks),
            "results": results,
            "final_state": current_state,
            "error": error_info,
        }

    async def _execute_task(
        self,
        task: Dict[str, Any],
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Execute a single task.

        Args:
            task: Task dictionary with execution details
            state: Current state
            working_dir: Directory to execute in

        Returns:
            Task execution result
        """
        return await self._task_executor(task, state, working_dir)

    async def _default_task_executor(
        self,
        task: Dict[str, Any],
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Default task executor using shell commands or YAMLEngine.

        This is the fallback when no custom executor is provided.
        Tasks can specify execution method via:
        - 'command': Shell command to execute
        - 'workflow': YAML workflow file to run
        - 'steps': List of steps to execute

        Args:
            task: Task dictionary
            state: Current state
            working_dir: Working directory

        Returns:
            Execution result dictionary
        """
        task_id = task.get("id", "unknown")

        # Check for shell command
        if "command" in task:
            return await self._execute_shell_command(
                task["command"], working_dir, task_id
            )

        # Check for workflow file
        if "workflow" in task:
            return await self._execute_workflow(task["workflow"], state, working_dir)

        # Check for steps
        if "steps" in task:
            return await self._execute_steps(task["steps"], state, working_dir)

        # No execution method specified - just mark as complete
        logger.debug(f"Task {task_id} has no execution method, marking complete")
        return {
            "success": True,
            "task_id": task_id,
            "message": "Task completed (no execution required)",
        }

    async def _execute_shell_command(
        self,
        command: str,
        working_dir: str,
        task_id: str,
    ) -> Dict[str, Any]:
        """
        Execute a shell command.

        Args:
            command: Shell command to execute
            working_dir: Working directory
            task_id: Task identifier for logging

        Returns:
            Execution result
        """
        logger.debug(f"Executing command for {task_id}: {command[:100]}...")

        try:
            result = subprocess.run(
                command,
                shell=True,
                cwd=working_dir,
                capture_output=True,
                text=True,
                timeout=3600,  # 1 hour timeout
            )

            success = result.returncode == 0

            return {
                "success": success,
                "task_id": task_id,
                "returncode": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "error": result.stderr if not success else None,
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "task_id": task_id,
                "error": "Command timed out after 1 hour",
            }
        except Exception as e:
            return {
                "success": False,
                "task_id": task_id,
                "error": str(e),
            }

    async def _execute_workflow(
        self,
        workflow_path: str,
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Execute a YAML workflow file.

        Args:
            workflow_path: Path to workflow YAML file
            state: Current state to pass to workflow
            working_dir: Working directory

        Returns:
            Workflow execution result
        """
        try:
            # Import here to avoid circular imports
            from ..yaml_engine import YAMLEngine

            engine = YAMLEngine()
            graph = engine.load_from_file(workflow_path)
            compiled = graph.compile()

            # Execute and collect results
            final_state = None
            for event in compiled.invoke(state):
                final_state = event

            return {
                "success": True,
                "workflow": workflow_path,
                "final_state": final_state,
            }

        except Exception as e:
            logger.error(f"Workflow execution failed: {e}")
            return {
                "success": False,
                "workflow": workflow_path,
                "error": str(e),
            }

    async def _execute_steps(
        self,
        steps: List[Dict[str, Any]],
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Execute a list of steps sequentially.

        Args:
            steps: List of step dictionaries
            state: Current state
            working_dir: Working directory

        Returns:
            Combined execution result
        """
        step_results = []
        current_state = state.copy()

        for i, step in enumerate(steps):
            step_name = step.get("name", f"step-{i}")

            if "run" in step:
                result = await self._execute_shell_command(
                    step["run"], working_dir, step_name
                )
            else:
                result = {"success": True, "step": step_name, "message": "No-op"}

            step_results.append(result)

            if not result.get("success", True):
                return {
                    "success": False,
                    "steps_completed": i,
                    "step_results": step_results,
                    "error": f"Step {step_name} failed: {result.get('error')}",
                }

            # Update state
            current_state.update(result)

        return {
            "success": True,
            "steps_completed": len(steps),
            "step_results": step_results,
        }
