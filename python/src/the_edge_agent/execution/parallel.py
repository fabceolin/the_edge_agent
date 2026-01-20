"""
Parallel Executor for TEA.

This module implements the ParallelExecutor that executes tasks concurrently
with isolated git worktrees. Each task gets its own worktree and branch for
complete isolation.

Story: TEA-RALPHY-001.6 (Execution Modes)
Acceptance Criteria: 13, 14, 15, 16

Example:
    >>> from the_edge_agent.execution import ParallelExecutor, ExecutionConfig
    >>>
    >>> config = ExecutionConfig(mode=ExecutionMode.PARALLEL, max_parallel=4)
    >>> executor = ParallelExecutor(config)
    >>> result = await executor.execute(tasks, initial_state)
"""

import asyncio
import logging
import os
import subprocess
from typing import Any, Callable, Dict, List, Optional

from .orchestrator import BaseExecutor
from .mode import ExecutionConfig
from ..actions.git_actions import (
    git_worktree_create,
    git_worktree_remove,
    git_worktree_merge,
)

logger = logging.getLogger(__name__)


class ParallelExecutor(BaseExecutor):
    """
    Execute tasks in parallel, optionally with isolated git worktrees.

    When use_worktrees=True (default):
    - Each task gets an isolated worktree with a dedicated branch
    - Tasks execute concurrently (limited by max_parallel)
    - Results are merged back to target branch in dependency order
    - Merge conflicts halt execution and return conflict info

    When use_worktrees=False:
    - Tasks execute concurrently in the same working directory
    - No git worktree creation or merging
    - Simpler and faster for tasks that don't conflict on files
    """

    def __init__(
        self,
        config: ExecutionConfig,
        task_executor: Optional[Callable] = None,
    ):
        """
        Initialize the parallel executor.

        Args:
            config: ExecutionConfig with parallel settings
            task_executor: Optional custom task execution function
        """
        super().__init__(config)
        self._task_executor = task_executor or self._default_task_executor

    async def execute(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute tasks in parallel with isolated worktrees.

        Args:
            tasks: List of task dictionaries with 'id' and 'description'
            state: Initial state dictionary

        Returns:
            {
                "mode": "parallel",
                "status": "success" | "conflict" | "error",
                "tasks_completed": int,
                "results": List[Dict],
                "merge_results": List[Dict],
                "conflicts": Optional[List[Dict]]
            }
        """
        results = []
        merge_results = []
        conflicts = None

        # Get repo root
        repo_root = self.config.working_directory or os.getcwd()

        use_worktrees = self.config.use_worktrees
        logger.info(
            f"Starting parallel execution of {len(tasks)} tasks "
            f"(max_parallel={self.config.max_parallel}, use_worktrees={use_worktrees})"
        )

        try:
            if use_worktrees:
                # Phase 1: Create worktrees for each task
                worktrees = await self._create_worktrees(tasks, repo_root)

                # Phase 2: Execute tasks in parallel (in worktrees)
                results = await self._execute_parallel(worktrees, state)

                # Phase 3: Merge results back (in order)
                merge_results, conflicts = await self._merge_results(
                    worktrees, results, repo_root
                )
            else:
                # Simple parallel: no worktrees, run in same directory
                results = await self._execute_parallel_simple(tasks, state, repo_root)

        except Exception as e:
            logger.error(f"Parallel execution failed: {e}")
            return {
                "mode": "parallel",
                "status": "error",
                "error": str(e),
                "tasks_completed": 0,
                "results": results,
            }

        # Determine status
        tasks_completed = sum(1 for r in results if r.get("success", False))
        if conflicts:
            status = "conflict"
        elif tasks_completed == len(tasks):
            status = "success"
        else:
            status = "partial"

        logger.info(
            f"Parallel execution complete: {tasks_completed}/{len(tasks)} tasks, "
            f"status={status}"
        )

        return {
            "mode": "parallel",
            "use_worktrees": use_worktrees,
            "status": status,
            "tasks_completed": tasks_completed,
            "tasks_total": len(tasks),
            "results": results,
            "merge_results": merge_results if use_worktrees else None,
            "conflicts": conflicts,
        }

    async def _create_worktrees(
        self,
        tasks: List[Dict[str, Any]],
        repo_root: str,
    ) -> List[Dict[str, Any]]:
        """
        Create a worktree for each task.

        Args:
            tasks: List of tasks
            repo_root: Repository root directory

        Returns:
            List of worktree info dicts with 'task' field added
        """
        worktrees = []

        for task in tasks:
            task_id = task.get("id", f"task-{len(worktrees)}")

            # Generate branch and worktree path
            branch_name = f"{self.config.branch_prefix}{task_id}"
            worktree_path = os.path.join(repo_root, self.config.worktree_base, task_id)

            logger.debug(f"Creating worktree for {task_id}: {worktree_path}")

            result = git_worktree_create(
                branch_name=branch_name,
                worktree_path=worktree_path,
                repo_root=repo_root,
            )

            if not result.get("success", False):
                logger.error(f"Failed to create worktree for {task_id}: {result}")
                raise RuntimeError(
                    f"Failed to create worktree for task {task_id}: "
                    f"{result.get('error', 'Unknown error')}"
                )

            worktrees.append(
                {
                    **result,
                    "task": task,
                    "task_id": task_id,
                }
            )

        logger.info(f"Created {len(worktrees)} worktrees")
        return worktrees

    async def _execute_parallel(
        self,
        worktrees: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> List[Dict[str, Any]]:
        """
        Execute tasks in parallel with semaphore limiting concurrency.

        Args:
            worktrees: List of worktree info dicts
            state: Initial state

        Returns:
            List of execution results
        """
        semaphore = asyncio.Semaphore(self.config.max_parallel)

        async def execute_with_limit(wt_info: Dict[str, Any]) -> Dict[str, Any]:
            async with semaphore:
                return await self._execute_in_worktree(wt_info, state)

        # Run all tasks concurrently (limited by semaphore)
        results = await asyncio.gather(
            *[execute_with_limit(wt) for wt in worktrees],
            return_exceptions=True,
        )

        # Process results, converting exceptions to error dicts
        processed = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                processed.append(
                    {
                        "task_id": worktrees[i]["task_id"],
                        "success": False,
                        "error": str(result),
                    }
                )
            else:
                processed.append(result)

        return processed

    async def _execute_parallel_simple(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
        working_dir: str,
    ) -> List[Dict[str, Any]]:
        """
        Execute tasks in parallel without worktrees (simple mode).

        All tasks run in the same working directory. Use this when tasks
        don't conflict on files or when git isolation isn't needed.

        Args:
            tasks: List of tasks
            state: Initial state
            working_dir: Working directory for all tasks

        Returns:
            List of execution results
        """
        semaphore = asyncio.Semaphore(self.config.max_parallel)

        async def execute_task(task: Dict[str, Any]) -> Dict[str, Any]:
            task_id = task.get("id", f"task-{tasks.index(task)}")
            async with semaphore:
                logger.debug(f"Executing task {task_id} in {working_dir} (simple mode)")
                try:
                    result = await self._task_executor(task, state, working_dir)
                    return {
                        "task_id": task_id,
                        **result,
                    }
                except Exception as e:
                    logger.error(f"Task {task_id} failed: {e}")
                    return {
                        "task_id": task_id,
                        "success": False,
                        "error": str(e),
                    }

        # Run all tasks concurrently
        results = await asyncio.gather(
            *[execute_task(task) for task in tasks],
            return_exceptions=True,
        )

        # Convert exceptions to error dicts
        processed = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                task_id = tasks[i].get("id", f"task-{i}")
                processed.append(
                    {
                        "task_id": task_id,
                        "success": False,
                        "error": str(result),
                    }
                )
            else:
                processed.append(result)

        return processed

    async def _execute_in_worktree(
        self,
        wt_info: Dict[str, Any],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute a task within its worktree.

        Args:
            wt_info: Worktree info dict with 'task' and 'worktree_path'
            state: State to pass to task

        Returns:
            Task execution result
        """
        task = wt_info["task"]
        task_id = wt_info["task_id"]
        worktree_path = wt_info["worktree_path"]

        logger.debug(f"Executing task {task_id} in {worktree_path}")

        try:
            result = await self._task_executor(task, state, worktree_path)
            return {
                "task_id": task_id,
                "worktree_path": worktree_path,
                "branch": wt_info.get("branch"),
                **result,
            }
        except Exception as e:
            logger.error(f"Task {task_id} execution failed: {e}")
            return {
                "task_id": task_id,
                "worktree_path": worktree_path,
                "success": False,
                "error": str(e),
            }

    async def _merge_results(
        self,
        worktrees: List[Dict[str, Any]],
        results: List[Dict[str, Any]],
        repo_root: str,
    ) -> tuple:
        """
        Merge successful task results back to target branch.

        Args:
            worktrees: List of worktree info dicts
            results: List of execution results
            repo_root: Repository root

        Returns:
            Tuple of (merge_results, conflicts)
        """
        merge_results = []
        conflicts = None

        for wt_info, result in zip(worktrees, results):
            task_id = wt_info["task_id"]
            worktree_path = wt_info["worktree_path"]

            if not result.get("success", False):
                logger.debug(f"Skipping merge for failed task {task_id}")
                # Clean up worktree anyway
                git_worktree_remove(worktree_path=worktree_path, force=True)
                continue

            logger.debug(f"Merging task {task_id} to {self.config.target_branch}")

            merge_result = git_worktree_merge(
                worktree_path=worktree_path,
                target_branch=self.config.target_branch,
                delete_after_merge=True,
                repo_root=repo_root,
            )

            merge_results.append(
                {
                    "task_id": task_id,
                    **merge_result,
                }
            )

            if merge_result.get("conflicts"):
                logger.warning(f"Merge conflict for task {task_id}")
                conflicts = merge_result["conflicts"]
                # Stop merging on first conflict
                break

        return merge_results, conflicts

    async def _default_task_executor(
        self,
        task: Dict[str, Any],
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Default task executor using shell commands.

        Args:
            task: Task dictionary
            state: Current state
            working_dir: Worktree path to execute in

        Returns:
            Execution result dictionary
        """
        task_id = task.get("id", "unknown")

        # Check for shell command
        if "command" in task:
            return await self._execute_shell_command(
                task["command"], working_dir, task_id
            )

        # Check for workflow
        if "workflow" in task:
            return await self._execute_workflow(task["workflow"], state, working_dir)

        # No execution method - just mark complete
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
        Execute a shell command in the worktree.

        Args:
            command: Shell command
            working_dir: Worktree path
            task_id: Task identifier

        Returns:
            Execution result
        """
        logger.debug(f"Executing command for {task_id}: {command[:100]}...")

        try:
            # Run in subprocess to not block event loop
            loop = asyncio.get_event_loop()
            result = await loop.run_in_executor(
                None,
                lambda: subprocess.run(
                    command,
                    shell=True,
                    cwd=working_dir,
                    capture_output=True,
                    text=True,
                    timeout=3600,
                ),
            )

            success = result.returncode == 0

            # Commit any changes made
            if success:
                await self._commit_changes(working_dir, task_id)

            return {
                "success": success,
                "returncode": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "error": result.stderr if not success else None,
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Command timed out after 1 hour",
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
            }

    async def _commit_changes(
        self,
        working_dir: str,
        task_id: str,
    ) -> None:
        """
        Commit any changes in the worktree.

        Args:
            working_dir: Worktree path
            task_id: Task identifier for commit message
        """
        try:
            # Check if there are changes
            status = subprocess.run(
                ["git", "status", "--porcelain"],
                cwd=working_dir,
                capture_output=True,
                text=True,
            )

            if status.stdout.strip():
                # Stage and commit
                subprocess.run(
                    ["git", "add", "-A"],
                    cwd=working_dir,
                    check=True,
                )
                subprocess.run(
                    ["git", "commit", "-m", f"Task {task_id}: automated changes"],
                    cwd=working_dir,
                    check=True,
                )
                logger.debug(f"Committed changes for task {task_id}")

        except subprocess.CalledProcessError as e:
            logger.warning(f"Failed to commit changes for {task_id}: {e}")

    async def _execute_workflow(
        self,
        workflow_path: str,
        state: Dict[str, Any],
        working_dir: str,
    ) -> Dict[str, Any]:
        """
        Execute a YAML workflow in the worktree.

        Args:
            workflow_path: Path to workflow file
            state: Current state
            working_dir: Worktree path

        Returns:
            Workflow execution result
        """
        try:
            from ..yaml_engine import YAMLEngine

            engine = YAMLEngine()
            graph = engine.load_from_file(workflow_path)
            compiled = graph.compile()

            final_state = None
            for event in compiled.invoke(state):
                final_state = event

            # Commit any changes
            await self._commit_changes(working_dir, f"workflow-{workflow_path}")

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
