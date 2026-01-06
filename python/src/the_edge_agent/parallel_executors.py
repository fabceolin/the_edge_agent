"""
Parallel executor abstractions for The Edge Agent.

This module provides pluggable executor strategies for parallel flow execution,
enabling different execution backends (threads, processes, remote) based on workload
characteristics.

Strategies:
- ThreadExecutor: Thread-based execution (default), suitable for I/O-bound tasks
- ProcessExecutor: Process-based execution, suitable for CPU-bound tasks that
  need to bypass Python's GIL
- RemoteExecutor: SSH/GNU Parallel execution for distributed workflows (TEA-PARALLEL-001.3)

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

import asyncio
import copy
import json
import os
import pickle
import shutil
import subprocess
import tempfile
import time
import logging
from abc import abstractmethod
from concurrent.futures import (
    ThreadPoolExecutor,
    ProcessPoolExecutor,
    Future,
    TimeoutError as FuturesTimeoutError,
)
from dataclasses import dataclass, field
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Optional,
    Protocol,
    Tuple,
    Union,
    runtime_checkable,
)

from the_edge_agent.parallel import (
    ParallelConfig,
    ParallelFlowResult,
)

logger = logging.getLogger(__name__)


@dataclass
class FlowTask:
    """
    Represents a single parallel flow task to be executed.

    Attributes:
        branch: The branch/flow name (starting node)
        flow_func: The callable to execute the flow
        state: The state dict for this flow (should be a deep copy)
        config: Configuration dict for the flow
        fan_in_node: The target fan-in node
        parallel_config: Parallel execution configuration
        start_time: Time when the flow was submitted
    """

    branch: str
    flow_func: Callable[..., Any]
    state: Dict[str, Any]
    config: Dict[str, Any]
    fan_in_node: str
    parallel_config: ParallelConfig
    start_time: float


@dataclass
class ExecutorResult:
    """
    Result from executor submission.

    Attributes:
        future: The Future object for the submitted task
        branch: The branch name
        parallel_config: The parallel config for this flow
        start_time: The start time
    """

    future: Future
    branch: str
    parallel_config: ParallelConfig
    start_time: float


class PickleValidationError(Exception):
    """Raised when state contains non-picklable objects."""

    def __init__(self, key: str, flow_index: int, original_error: Exception):
        self.key = key
        self.flow_index = flow_index
        self.original_error = original_error
        super().__init__(
            f"State key '{key}' in flow {flow_index} is not picklable: {original_error}"
        )


@runtime_checkable
class ParallelExecutor(Protocol):
    """
    Protocol defining the interface for parallel execution strategies.

    Implementations must provide methods to submit tasks and collect results.
    The executor is responsible for managing the underlying execution backend
    (thread pool, process pool, etc.).

    Usage:
        executor = get_executor("thread", max_workers=4)
        with executor:
            futures = executor.submit_flows(tasks, flow_runner)
            results = executor.collect_results(futures, timeout=30.0)

    Note:
        Executors must be used as context managers to ensure proper cleanup.
    """

    @property
    def strategy(self) -> str:
        """Return the strategy name ('thread' or 'process')."""
        ...

    @property
    def max_workers(self) -> Optional[int]:
        """Return the maximum number of workers."""
        ...

    def __enter__(self) -> "ParallelExecutor":
        """Enter the context manager."""
        ...

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """Exit the context manager and cleanup resources."""
        ...

    def submit(
        self,
        fn: Callable[..., Any],
        *args: Any,
        **kwargs: Any,
    ) -> Future:
        """
        Submit a single task for execution.

        Args:
            fn: The function to execute
            *args: Positional arguments for the function
            **kwargs: Keyword arguments for the function

        Returns:
            Future object for the submitted task
        """
        ...

    def submit_flow(
        self,
        task: FlowTask,
        flow_runner: Callable[..., Any],
    ) -> ExecutorResult:
        """
        Submit a parallel flow task for execution.

        Args:
            task: The FlowTask describing the flow to execute
            flow_runner: The function that executes the flow

        Returns:
            ExecutorResult containing the Future and metadata
        """
        ...

    def validate_state(self, state: Dict[str, Any], flow_index: int = 0) -> None:
        """
        Validate that state is compatible with this executor.

        For ProcessExecutor, this validates that state is picklable.
        ThreadExecutor may perform no validation.

        Args:
            state: The state dict to validate
            flow_index: Index of the flow (for error messages)

        Raises:
            PickleValidationError: If state is not valid for this executor
        """
        ...


class ThreadExecutor:
    """
    Thread-based parallel executor.

    This executor uses Python's ThreadPoolExecutor for parallel execution.
    It's suitable for I/O-bound tasks and is the default execution strategy.

    Thread Safety:
        State is passed directly to threads (with deep copy handled by caller).
        This executor does not modify the state dict.

    Attributes:
        max_workers: Maximum number of worker threads
        _executor: The underlying ThreadPoolExecutor

    Example:
        executor = ThreadExecutor(max_workers=4)
        with executor:
            future = executor.submit(my_function, arg1, arg2)
            result = future.result()
    """

    def __init__(self, max_workers: Optional[int] = None):
        """
        Initialize the ThreadExecutor.

        Args:
            max_workers: Maximum number of worker threads. If None, uses
                Python's default: min(32, os.cpu_count() + 4)
        """
        self._max_workers = max_workers
        self._executor: Optional[ThreadPoolExecutor] = None

    @property
    def strategy(self) -> str:
        """Return the strategy name."""
        return "thread"

    @property
    def max_workers(self) -> Optional[int]:
        """Return the maximum number of workers."""
        return self._max_workers

    def __enter__(self) -> "ThreadExecutor":
        """Enter the context manager and create the thread pool."""
        self._executor = ThreadPoolExecutor(max_workers=self._max_workers)
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """Exit the context manager and shutdown the thread pool."""
        if self._executor is not None:
            self._executor.shutdown(wait=True)
            self._executor = None

    def submit(
        self,
        fn: Callable[..., Any],
        *args: Any,
        **kwargs: Any,
    ) -> Future:
        """
        Submit a single task for execution.

        Args:
            fn: The function to execute
            *args: Positional arguments for the function
            **kwargs: Keyword arguments for the function

        Returns:
            Future object for the submitted task

        Raises:
            RuntimeError: If executor is not initialized (not in context)
        """
        if self._executor is None:
            raise RuntimeError("ThreadExecutor must be used as a context manager")
        return self._executor.submit(fn, *args, **kwargs)

    def submit_flow(
        self,
        task: FlowTask,
        flow_runner: Callable[..., Any],
    ) -> ExecutorResult:
        """
        Submit a parallel flow task for execution.

        Args:
            task: The FlowTask describing the flow to execute
            flow_runner: The function that executes the flow

        Returns:
            ExecutorResult containing the Future and metadata
        """
        future = self.submit(
            flow_runner,
            task.branch,
            task.state,
            task.config,
            task.fan_in_node,
            task.parallel_config,
            task.start_time,
        )
        return ExecutorResult(
            future=future,
            branch=task.branch,
            parallel_config=task.parallel_config,
            start_time=task.start_time,
        )

    def validate_state(self, state: Dict[str, Any], flow_index: int = 0) -> None:
        """
        Validate state for thread execution.

        ThreadExecutor does not require picklable state, so this is a no-op.

        Args:
            state: The state dict to validate
            flow_index: Index of the flow (for error messages)
        """
        # No validation needed for threads - any Python object is allowed
        pass


def _process_flow_wrapper(
    flow_runner: Callable[..., Any],
    branch: str,
    state: Dict[str, Any],
    config: Dict[str, Any],
    fan_in_node: str,
    parallel_config_dict: Dict[str, Any],
    start_time: float,
) -> Any:
    """
    Wrapper function for process execution.

    This function is picklable and deserializes the ParallelConfig
    before calling the actual flow runner.

    Note:
        This must be a module-level function to be picklable.
    """
    # Reconstruct ParallelConfig from dict
    parallel_config = ParallelConfig(**parallel_config_dict)
    return flow_runner(branch, state, config, fan_in_node, parallel_config, start_time)


class ProcessExecutor:
    """
    Process-based parallel executor.

    This executor uses Python's ProcessPoolExecutor for parallel execution,
    which bypasses the Global Interpreter Lock (GIL). It's suitable for
    CPU-bound tasks that benefit from true parallelism.

    Serialization:
        State must be picklable for process execution. Non-picklable objects
        (lambdas, open files, database connections, etc.) will cause errors.
        Use validate_state() to check before submission.

    Attributes:
        max_workers: Maximum number of worker processes
        _executor: The underlying ProcessPoolExecutor

    Example:
        executor = ProcessExecutor(max_workers=4)
        with executor:
            # Validate state before submission
            executor.validate_state(state)
            future = executor.submit(my_function, arg1, arg2)
            result = future.result()
    """

    def __init__(self, max_workers: Optional[int] = None):
        """
        Initialize the ProcessExecutor.

        Args:
            max_workers: Maximum number of worker processes. If None, uses
                the number of processors on the machine.
        """
        self._max_workers = max_workers
        self._executor: Optional[ProcessPoolExecutor] = None

    @property
    def strategy(self) -> str:
        """Return the strategy name."""
        return "process"

    @property
    def max_workers(self) -> Optional[int]:
        """Return the maximum number of workers."""
        return self._max_workers

    def __enter__(self) -> "ProcessExecutor":
        """Enter the context manager and create the process pool."""
        self._executor = ProcessPoolExecutor(max_workers=self._max_workers)
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """Exit the context manager and shutdown the process pool."""
        if self._executor is not None:
            self._executor.shutdown(wait=True)
            self._executor = None

    def submit(
        self,
        fn: Callable[..., Any],
        *args: Any,
        **kwargs: Any,
    ) -> Future:
        """
        Submit a single task for execution.

        Note:
            The function and all arguments must be picklable for process
            execution. Use validate_state() to check state dicts.

        Args:
            fn: The function to execute (must be picklable)
            *args: Positional arguments for the function (must be picklable)
            **kwargs: Keyword arguments for the function (must be picklable)

        Returns:
            Future object for the submitted task

        Raises:
            RuntimeError: If executor is not initialized (not in context)
        """
        if self._executor is None:
            raise RuntimeError("ProcessExecutor must be used as a context manager")
        return self._executor.submit(fn, *args, **kwargs)

    def submit_flow(
        self,
        task: FlowTask,
        flow_runner: Callable[..., Any],
    ) -> ExecutorResult:
        """
        Submit a parallel flow task for execution.

        The state is validated for picklability before submission.

        Args:
            task: The FlowTask describing the flow to execute
            flow_runner: The function that executes the flow (must be picklable)

        Returns:
            ExecutorResult containing the Future and metadata

        Raises:
            PickleValidationError: If state contains non-picklable objects
        """
        # Validate state before submission
        self.validate_state(task.state)

        # Convert ParallelConfig to dict for pickling
        parallel_config_dict = {
            "timeout_seconds": task.parallel_config.timeout_seconds,
            "timeout_total": task.parallel_config.timeout_total,
            "fail_fast": task.parallel_config.fail_fast,
            "retry_policy": task.parallel_config.retry_policy,
            "circuit_breaker": task.parallel_config.circuit_breaker,
            "include_traceback": task.parallel_config.include_traceback,
        }

        # Submit using the wrapper function for better picklability
        future = self.submit(
            _process_flow_wrapper,
            flow_runner,
            task.branch,
            task.state,
            task.config,
            task.fan_in_node,
            parallel_config_dict,
            task.start_time,
        )
        return ExecutorResult(
            future=future,
            branch=task.branch,
            parallel_config=task.parallel_config,
            start_time=task.start_time,
        )

    def validate_state(self, state: Dict[str, Any], flow_index: int = 0) -> None:
        """
        Validate that state is picklable for process execution.

        This method attempts to pickle each value in the state dict and
        provides clear error messages for non-picklable objects.

        Args:
            state: The state dict to validate
            flow_index: Index of the flow (for error messages)

        Raises:
            PickleValidationError: If any value is not picklable
        """
        for key, value in state.items():
            try:
                pickle.dumps(value)
            except (pickle.PicklingError, TypeError, AttributeError) as e:
                raise PickleValidationError(key, flow_index, e)


class RemoteExecutionError(Exception):
    """Raised when remote execution fails."""

    def __init__(self, host: str, message: str, stderr: Optional[str] = None):
        self.host = host
        self.stderr = stderr
        super().__init__(f"Remote execution failed on {host}: {message}")


class SCPTransferError(Exception):
    """Raised when SCP file transfer fails."""

    def __init__(self, host: str, file_path: str, message: str):
        self.host = host
        self.file_path = file_path
        super().__init__(f"SCP transfer failed for {file_path} to {host}: {message}")


@dataclass
class RemoteConfig:
    """
    Configuration for remote execution via SSH/GNU Parallel.

    This dataclass defines the settings for distributing workflow execution
    across multiple SSH hosts. Supports both GNU Parallel (preferred) and
    direct SSH fallback for systems without GNU Parallel installed.

    Attributes:
        hosts: List of SSH host strings (e.g., ["user@server1", "user@server2"])
        basefile: Path to the tea binary to copy to remote hosts
        workdir: Remote working directory for execution (default: /tmp/tea-jobs)
        cleanup: Whether to remove remote files after execution (default: True)
        yaml_path: Path to the YAML workflow file to transfer (set during execution)
        env_vars: Environment variable transfer configuration (optional)

    Example:
        >>> config = RemoteConfig(
        ...     hosts=["user@server1", "user@server2"],
        ...     basefile="./tea",
        ...     workdir="/tmp/tea-remote",
        ...     cleanup=True
        ... )
    """

    hosts: List[str] = field(default_factory=list)
    basefile: str = "./tea"
    workdir: str = "/tmp/tea-jobs"
    cleanup: bool = True
    yaml_path: Optional[str] = None
    env_vars: Optional[Dict[str, Any]] = None

    def __post_init__(self) -> None:
        """Validate configuration."""
        if not self.hosts:
            raise ValueError("At least one host must be specified for remote execution")


class RemoteExecutor:
    """
    Remote SSH-based parallel executor with GNU Parallel support.

    This executor distributes parallel flows across remote hosts via SSH,
    preferring GNU Parallel for efficient distribution when available,
    with async SSH fallback for systems without it.

    Features:
    - GNU Parallel detection and command generation (preferred)
    - Async SSH fallback when GNU Parallel is unavailable
    - Full YAML + state file transfer via SCP
    - Entry-point/exit-point support for targeted node execution
    - Result collection and JSON deserialization
    - Configurable remote file cleanup
    - Timeout and retry integration with ParallelConfig

    Security:
        - Environment variables are transferred securely via whitelist
        - Only explicitly listed variables are sent to remote hosts
        - Backend warnings are logged for incompatible configurations

    Attributes:
        config: Remote execution configuration
        settings: Full settings dict for backend warning checks
        has_parallel: True if GNU Parallel is available on local system
        _env_handler: Handler for environment variable transfer
        _temp_files: List of temporary files to cleanup

    Example:
        config = RemoteConfig(
            hosts=["user@host1", "user@host2"],
            basefile="./tea",
            yaml_path="./workflow.yaml"
        )
        executor = RemoteExecutor(config, settings={"secrets": {"backend": "env"}})
        with executor:
            # Use execute() for full remote flow execution
            results = executor.execute(flows, ParallelConfig(timeout_seconds=30))
    """

    def __init__(
        self,
        config: RemoteConfig,
        settings: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize the RemoteExecutor.

        Args:
            config: Remote execution configuration
            settings: Full YAML settings dict for backend warnings
        """
        self._config = config
        self._settings = settings or {}
        self._env_handler: Optional["RemoteEnvHandler"] = None
        self._temp_files: List[str] = []
        self._host_index = 0
        self._executor: Optional[ThreadPoolExecutor] = None

        # Check for GNU Parallel availability
        self.has_parallel = shutil.which("parallel") is not None

        # Initialize environment handler if env_vars config provided
        if config.env_vars is not None:
            try:
                from .remote_env import RemoteEnvHandler, parse_env_vars_config

                env_config = parse_env_vars_config(config.env_vars)
                self._env_handler = RemoteEnvHandler(env_config)
            except ImportError:
                logger.warning(
                    "remote_env module not available, env_vars config ignored"
                )

        logger.debug(
            f"RemoteExecutor initialized: {len(config.hosts)} hosts, "
            f"GNU Parallel: {self.has_parallel}"
        )

    @property
    def strategy(self) -> str:
        """Return the strategy name."""
        return "remote"

    @property
    def max_workers(self) -> Optional[int]:
        """Return the maximum number of workers (hosts)."""
        return len(self._config.hosts)

    @property
    def hosts(self) -> List[str]:
        """Return the list of remote hosts."""
        return self._config.hosts

    def __enter__(self) -> "RemoteExecutor":
        """Enter the context manager and emit backend warnings."""
        self._emit_backend_warnings()
        # Use a thread pool to manage remote SSH calls
        self._executor = ThreadPoolExecutor(max_workers=len(self._config.hosts))
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """Exit the context manager and cleanup resources."""
        if self._executor is not None:
            self._executor.shutdown(wait=True)
            self._executor = None

        # Cleanup temporary files
        for temp_file in self._temp_files:
            try:
                if os.path.exists(temp_file):
                    os.remove(temp_file)
            except OSError as e:
                logger.warning(f"Failed to cleanup temp file {temp_file}: {e}")
        self._temp_files.clear()

    def _emit_backend_warnings(self) -> None:
        """
        Emit warnings for backend configurations that may not work well with remote.

        Checks:
        - secrets.backend != 'env': Remote hosts need cloud provider credentials
        - ltm.backend == 'sqlite' with local path: Not shared across hosts
        - checkpoint.backend in ('memory', 'file'): Not shared across hosts
        """
        # Secrets backend warning
        secrets_backend = self._settings.get("secrets", {}).get("backend", "env")
        if secrets_backend != "env":
            logger.info(
                f"Remote hosts should have cloud provider credentials configured locally. "
                f"secrets.backend='{secrets_backend}' requires remote hosts to have "
                f"appropriate IAM roles or credentials."
            )

        # LTM backend warning
        ltm_settings = self._settings.get("ltm", {})
        ltm_backend = ltm_settings.get("backend", "sqlite")
        ltm_path = ltm_settings.get("path", "")

        if ltm_backend == "sqlite" and not str(ltm_path).startswith(
            ("s3://", "gs://", "az://")
        ):
            logger.warning(
                f"LTM backend 'sqlite' with local path is not shared across remote hosts. "
                f"Each remote host will have isolated LTM state. "
                f"Consider using 'duckdb' with cloud storage for shared LTM."
            )

        # Checkpoint backend warning
        checkpoint_settings = self._settings.get("checkpoint", {})
        checkpoint_backend = checkpoint_settings.get("backend", "memory")
        if checkpoint_backend in ("memory", "file"):
            logger.warning(
                f"Checkpoint backend '{checkpoint_backend}' is not shared across remote hosts. "
                f"Checkpoints created on remote hosts will be local to that host. "
                f"Consider using a distributed checkpoint backend (S3/GCS) for shared checkpoints."
            )

    def _get_next_host(self) -> str:
        """Get the next host in round-robin fashion."""
        host = self._config.hosts[self._host_index % len(self._config.hosts)]
        self._host_index += 1
        return host

    def _build_ssh_command(
        self,
        host: str,
        remote_command: str,
    ) -> List[str]:
        """
        Build the SSH command with environment variable handling.

        Args:
            host: The remote host
            remote_command: Command to execute on remote

        Returns:
            List of command arguments for subprocess
        """
        cmd = ["ssh"]

        # Add SendEnv options if using ssh_env mode
        if self._env_handler is not None:
            ssh_opts = self._env_handler.build_ssh_options()
            cmd.extend(ssh_opts)

        cmd.append(host)

        # Prepend source command if using export_file mode
        if self._env_handler is not None:
            source_cmd = self._env_handler.get_source_command(
                f"{self._config.workdir}/env.sh"
            )
            remote_command = source_cmd + remote_command

        cmd.append(remote_command)
        return cmd

    def _prepare_remote_env(self, host: str) -> None:
        """
        Prepare environment on remote host.

        For export_file mode, generates and transfers the env script.
        """
        if self._env_handler is None:
            return

        if self._env_handler.config.mode == "export_file":
            # Generate local script
            local_script = tempfile.mktemp(suffix=".sh")
            self._env_handler.generate_export_script(local_script)
            self._temp_files.append(local_script)

            # Transfer to remote
            remote_path = f"{self._config.workdir}/env.sh"
            try:
                subprocess.run(
                    ["ssh", host, f"mkdir -p {self._config.workdir}"],
                    check=True,
                    capture_output=True,
                )
                subprocess.run(
                    ["scp", local_script, f"{host}:{remote_path}"],
                    check=True,
                    capture_output=True,
                )
            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to transfer env script to {host}: {e}")
                raise

    def submit(
        self,
        fn: Callable[..., Any],
        *args: Any,
        **kwargs: Any,
    ) -> Future:
        """
        Submit a single task for execution on a remote host.

        Note:
            Remote execution serializes the task to JSON and transfers
            via SSH. The callable must be a workflow that can be executed
            by the tea CLI on the remote host.

        Args:
            fn: The function to execute (must be serializable)
            *args: Positional arguments for the function
            **kwargs: Keyword arguments for the function

        Returns:
            Future object for the submitted task
        """
        if self._executor is None:
            raise RuntimeError("RemoteExecutor must be used as a context manager")

        # For remote execution, we need to serialize and transfer
        # This is a simplified implementation - actual remote execution
        # would involve more complex state serialization
        host = self._get_next_host()

        def remote_task():
            return fn(*args, **kwargs)

        return self._executor.submit(remote_task)

    def submit_flow(
        self,
        task: FlowTask,
        flow_runner: Callable[..., Any],
    ) -> ExecutorResult:
        """
        Submit a parallel flow task for execution on a remote host.

        Args:
            task: The FlowTask describing the flow to execute
            flow_runner: The function that executes the flow

        Returns:
            ExecutorResult containing the Future and metadata
        """
        if self._executor is None:
            raise RuntimeError("RemoteExecutor must be used as a context manager")

        host = self._get_next_host()

        def execute_remote():
            """Execute the flow on a remote host via SSH."""
            try:
                # Prepare environment on remote
                self._prepare_remote_env(host)

                # Serialize state to JSON for transfer
                state_json = json.dumps(task.state)

                # Build remote command
                remote_cmd = (
                    f"cd {self._config.workdir} && "
                    f"echo '{state_json}' | "
                    f"{self._config.basefile} run --input -"
                )

                # Execute via SSH
                ssh_cmd = self._build_ssh_command(host, remote_cmd)
                result = subprocess.run(
                    ssh_cmd,
                    capture_output=True,
                    text=True,
                    timeout=task.parallel_config.timeout_seconds or 300,
                )

                if result.returncode != 0:
                    raise RuntimeError(
                        f"Remote execution failed on {host}: {result.stderr}"
                    )

                # Parse result
                output = json.loads(result.stdout) if result.stdout else {}
                return output

            except subprocess.TimeoutExpired:
                raise TimeoutError(f"Remote execution timed out on {host}")
            except json.JSONDecodeError as e:
                raise RuntimeError(f"Failed to parse remote output: {e}")

        future = self._executor.submit(execute_remote)
        return ExecutorResult(
            future=future,
            branch=task.branch,
            parallel_config=task.parallel_config,
            start_time=task.start_time,
        )

    def validate_state(self, state: Dict[str, Any], flow_index: int = 0) -> None:
        """
        Validate state for remote execution.

        State must be JSON-serializable for remote transfer.

        Args:
            state: The state dict to validate
            flow_index: Index of the flow (for error messages)

        Raises:
            ValueError: If state is not JSON-serializable
        """
        try:
            json.dumps(state)
        except (TypeError, ValueError) as e:
            raise ValueError(
                f"State in flow {flow_index} is not JSON-serializable: {e}"
            )

    def execute(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
    ) -> List[ParallelFlowResult]:
        """
        Execute flows on remote hosts.

        This is the main entry point for remote execution. It distributes
        flows across configured SSH hosts, executing each flow using the
        tea CLI with --entry-point and --exit-point flags.

        Args:
            flows: List of flow dicts, each containing:
                - entry_point: Starting node for the flow
                - exit_point: Ending node for the flow
                - state: State dict for this flow
            config: ParallelConfig with timeout, retry, etc.

        Returns:
            List of ParallelFlowResult, one per flow

        Example:
            >>> flows = [
            ...     {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
            ...     {"entry_point": "node_b", "exit_point": "fan_in", "state": {"value": 2}},
            ... ]
            >>> results = executor.execute(flows, ParallelConfig(timeout_seconds=30))
        """
        # Validate yaml_path is set
        if not self._config.yaml_path:
            raise ValueError("RemoteConfig.yaml_path must be set for execute()")

        if self.has_parallel:
            return self._execute_with_gnu_parallel(flows, config)
        else:
            logger.info("GNU Parallel not available, using SSH fallback")
            return asyncio.run(self._execute_with_ssh_fallback(flows, config))

    def _execute_with_gnu_parallel(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
    ) -> List[ParallelFlowResult]:
        """
        Execute flows using GNU Parallel for efficient distribution.

        GNU Parallel provides efficient job distribution across hosts with
        built-in file transfer, cleanup, and progress tracking.

        Args:
            flows: List of flow dicts with entry_point, exit_point, state
            config: ParallelConfig with timeout settings

        Returns:
            List of ParallelFlowResult
        """
        results: List[ParallelFlowResult] = []
        temp_files: List[str] = []

        try:
            # Write input files for each flow
            for i, flow in enumerate(flows):
                input_path = tempfile.mktemp(suffix=f"_tea_input_{i}.json")
                with open(input_path, "w") as f:
                    json.dump(flow["state"], f)
                temp_files.append(input_path)

            # Create flow arguments file for GNU Parallel
            args_file = tempfile.mktemp(suffix="_tea_args.txt")
            with open(args_file, "w") as f:
                for i, flow in enumerate(flows):
                    # Write: input_file entry_point exit_point
                    f.write(
                        f"{temp_files[i]} {flow['entry_point']} {flow['exit_point']}\n"
                    )
            temp_files.append(args_file)

            # Build GNU Parallel command
            cmd = self.generate_gnu_parallel_command(flows, config, args_file)

            logger.debug(f"GNU Parallel command: {' '.join(cmd)}")

            # Execute
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.timeout_total,
            )

            timing_ms = (time.time() - start_time) * 1000

            if result.returncode != 0:
                logger.warning(f"GNU Parallel returned non-zero: {result.stderr}")

            # Collect results
            for i, (flow, input_path) in enumerate(zip(flows, temp_files[:-1])):
                result_path = f"{input_path}.result.json"
                try:
                    with open(result_path) as f:
                        state = json.load(f)
                    results.append(
                        ParallelFlowResult.from_success(
                            branch=flow["entry_point"],
                            state=state,
                            timing_ms=timing_ms,
                        )
                    )
                except FileNotFoundError:
                    results.append(
                        ParallelFlowResult(
                            branch=flow["entry_point"],
                            success=False,
                            error=f"Result file not found: {result_path}",
                            error_type="FileNotFoundError",
                            timing_ms=timing_ms,
                        )
                    )
                except json.JSONDecodeError as e:
                    results.append(
                        ParallelFlowResult(
                            branch=flow["entry_point"],
                            success=False,
                            error=f"Invalid JSON in result: {e}",
                            error_type="JSONDecodeError",
                            timing_ms=timing_ms,
                        )
                    )

        except subprocess.TimeoutExpired:
            timing_ms = (config.timeout_total or 0) * 1000
            for flow in flows:
                results.append(
                    ParallelFlowResult.from_timeout(
                        branch=flow["entry_point"],
                        state=flow["state"],
                        timing_ms=timing_ms,
                        timeout_seconds=config.timeout_total or 0,
                    )
                )
        finally:
            # Cleanup local temp files
            for temp_file in temp_files:
                try:
                    Path(temp_file).unlink(missing_ok=True)
                    # Also try to remove result files
                    Path(f"{temp_file}.result.json").unlink(missing_ok=True)
                except Exception:
                    pass

        return results

    async def _execute_with_ssh_fallback(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
    ) -> List[ParallelFlowResult]:
        """
        Execute flows using direct SSH when GNU Parallel is unavailable.

        This fallback uses asyncio to run SSH connections concurrently,
        distributing flows across hosts in round-robin fashion.

        Args:
            flows: List of flow dicts with entry_point, exit_point, state
            config: ParallelConfig with timeout settings

        Returns:
            List of ParallelFlowResult
        """
        tasks = []
        for i, flow in enumerate(flows):
            # Round-robin host assignment
            host = self._config.hosts[i % len(self._config.hosts)]
            task = self._ssh_execute_single(
                host=host,
                entry_point=flow["entry_point"],
                exit_point=flow["exit_point"],
                state=flow["state"],
                config=config,
            )
            tasks.append(task)

        # Execute all tasks concurrently
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # Convert results to ParallelFlowResult
        flow_results: List[ParallelFlowResult] = []
        for i, (flow, result) in enumerate(zip(flows, results)):
            if isinstance(result, Exception):
                flow_results.append(
                    ParallelFlowResult(
                        branch=flow["entry_point"],
                        success=False,
                        error=str(result),
                        error_type=type(result).__name__,
                        state=flow["state"],
                        timing_ms=0.0,
                    )
                )
            else:
                flow_results.append(result)

        return flow_results

    async def _ssh_execute_single(
        self,
        host: str,
        entry_point: str,
        exit_point: str,
        state: Dict[str, Any],
        config: ParallelConfig,
    ) -> ParallelFlowResult:
        """
        Execute a single flow on a remote host via SSH.

        Transfers the tea binary, YAML file, and state JSON to the remote
        host, executes with entry/exit-point flags, and retrieves the result.

        Args:
            host: SSH host string (user@server)
            entry_point: Starting node for the flow
            exit_point: Ending node for the flow
            state: State dict to transfer
            config: ParallelConfig with timeout settings

        Returns:
            ParallelFlowResult with execution result
        """
        start_time = time.time()
        input_file = None
        result_file = None

        try:
            # Create temp input file
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".json", delete=False
            ) as f:
                json.dump(state, f)
                input_file = f.name

            result_file = f"{input_file}.result.json"

            # 1. Create remote workdir
            await self._ssh_run(host, f"mkdir -p {self._config.workdir}")

            # 2. SCP files to remote
            files_to_transfer = [self._config.basefile, input_file]
            if self._config.yaml_path:
                files_to_transfer.append(self._config.yaml_path)
            await self._scp_to_remote(host, files_to_transfer)

            # 3. Make tea executable and execute remotely
            remote_input = f"{self._config.workdir}/{Path(input_file).name}"
            remote_output = f"{remote_input}.result.json"
            tea_name = Path(self._config.basefile).name
            yaml_name = (
                Path(self._config.yaml_path).name if self._config.yaml_path else ""
            )

            cmd = (
                f"cd {self._config.workdir} && "
                f"chmod +x {tea_name} && "
                f"./{tea_name} run {yaml_name} "
                f"--entry-point {entry_point} "
                f"--exit-point {exit_point} "
                f"--input {remote_input} "
                f"--output {remote_output}"
            )

            await self._ssh_run(host, cmd, timeout=config.timeout_seconds)

            # 4. SCP result back
            await self._scp_from_remote(host, remote_output, result_file)

            # 5. Read result
            with open(result_file) as f:
                result_state = json.load(f)

            timing_ms = (time.time() - start_time) * 1000

            # 6. Cleanup remote files if configured
            if self._config.cleanup:
                try:
                    await self._ssh_run(
                        host,
                        f"rm -rf {self._config.workdir}/*",
                    )
                except Exception as e:
                    # Cleanup failure is logged but doesn't fail the flow
                    logger.warning(f"Remote cleanup failed on {host}: {e}")

            return ParallelFlowResult.from_success(
                branch=entry_point,
                state=result_state,
                timing_ms=timing_ms,
            )

        except asyncio.TimeoutError:
            timing_ms = (time.time() - start_time) * 1000
            return ParallelFlowResult.from_timeout(
                branch=entry_point,
                state=state,
                timing_ms=timing_ms,
                timeout_seconds=config.timeout_seconds or 0,
            )
        except Exception as e:
            timing_ms = (time.time() - start_time) * 1000
            return ParallelFlowResult.from_error(
                branch=entry_point,
                exception=e,
                state=state,
                timing_ms=timing_ms,
                include_traceback=config.include_traceback,
            )
        finally:
            # Cleanup local temp files
            if input_file:
                Path(input_file).unlink(missing_ok=True)
            if result_file:
                Path(result_file).unlink(missing_ok=True)

    async def _ssh_run(
        self,
        host: str,
        command: str,
        timeout: Optional[float] = None,
    ) -> str:
        """
        Run a command on a remote host via SSH.

        Args:
            host: SSH host string (user@server)
            command: Command to execute
            timeout: Optional timeout in seconds

        Returns:
            Command stdout

        Raises:
            RemoteExecutionError: If SSH command fails
            asyncio.TimeoutError: If timeout is exceeded
        """
        ssh_cmd = [
            "ssh",
            "-o",
            "BatchMode=yes",
            "-o",
            "StrictHostKeyChecking=accept-new",
            host,
            command,
        ]

        proc = await asyncio.create_subprocess_exec(
            *ssh_cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )

        try:
            stdout, stderr = await asyncio.wait_for(
                proc.communicate(),
                timeout=timeout,
            )
        except asyncio.TimeoutError:
            proc.kill()
            await proc.wait()
            raise asyncio.TimeoutError(f"SSH command timed out after {timeout}s")

        if proc.returncode != 0:
            raise RemoteExecutionError(
                host=host,
                message=f"Command failed with exit code {proc.returncode}",
                stderr=stderr.decode() if stderr else None,
            )

        return stdout.decode()

    async def _scp_to_remote(self, host: str, files: List[str]) -> None:
        """
        Copy files to a remote host via SCP.

        Args:
            host: SSH host string (user@server)
            files: List of local file paths to copy

        Raises:
            SCPTransferError: If SCP transfer fails
        """
        for file_path in files:
            cmd = [
                "scp",
                "-o",
                "BatchMode=yes",
                "-o",
                "StrictHostKeyChecking=accept-new",
                file_path,
                f"{host}:{self._config.workdir}/",
            ]

            proc = await asyncio.create_subprocess_exec(
                *cmd,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout, stderr = await proc.communicate()

            if proc.returncode != 0:
                raise SCPTransferError(
                    host=host,
                    file_path=file_path,
                    message=stderr.decode() if stderr else "Unknown error",
                )

    async def _scp_from_remote(
        self,
        host: str,
        remote_path: str,
        local_path: str,
    ) -> None:
        """
        Copy a file from a remote host via SCP.

        Args:
            host: SSH host string (user@server)
            remote_path: Remote file path
            local_path: Local destination path

        Raises:
            SCPTransferError: If SCP transfer fails
        """
        cmd = [
            "scp",
            "-o",
            "BatchMode=yes",
            "-o",
            "StrictHostKeyChecking=accept-new",
            f"{host}:{remote_path}",
            local_path,
        ]

        proc = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout, stderr = await proc.communicate()

        if proc.returncode != 0:
            raise SCPTransferError(
                host=host,
                file_path=remote_path,
                message=stderr.decode() if stderr else "Unknown error",
            )

    def generate_gnu_parallel_command(
        self,
        flows: List[Dict[str, Any]],
        config: ParallelConfig,
        args_file: Optional[str] = None,
    ) -> List[str]:
        """
        Generate the GNU Parallel command for the given flows.

        This method is useful for debugging and testing the generated command
        without actually executing it.

        Args:
            flows: List of flow dicts with entry_point, exit_point, state
            config: ParallelConfig with timeout settings
            args_file: Path to the arguments file (optional, for actual execution)

        Returns:
            List of command parts that would be passed to subprocess.run
        """
        if not self._config.yaml_path:
            raise ValueError("RemoteConfig.yaml_path must be set")

        hosts = ",".join(self._config.hosts)
        yaml_name = os.path.basename(self._config.yaml_path)
        tea_name = os.path.basename(self._config.basefile)

        cmd = [
            "parallel",
            "--sshlogin",
            hosts,
            "--basefile",
            self._config.basefile,
            "--basefile",
            self._config.yaml_path,
            "--transferfile",
            "{1}",
            "--return",
            "{1}.result.json",
        ]

        if self._config.cleanup:
            cmd.append("--cleanup")

        if config.timeout_seconds:
            cmd.extend(["--timeout", str(int(config.timeout_seconds))])

        # {1} = input file, {2} = entry_point, {3} = exit_point
        remote_cmd = (
            f"cd {self._config.workdir} && "
            f"./{tea_name} run {yaml_name} "
            f"--entry-point {{2}} "
            f"--exit-point {{3}} "
            f"--input {{1}} "
            f"--output {{1}}.result.json"
        )
        cmd.append(remote_cmd)

        # Add colsep for argument parsing
        cmd.extend(["--colsep", " "])

        if args_file:
            cmd.extend(["::::", args_file])

        return cmd


# Registry of available executors
_EXECUTOR_REGISTRY: Dict[str, type] = {
    "thread": ThreadExecutor,
    "process": ProcessExecutor,
    "remote": RemoteExecutor,
}


def get_executor(
    strategy: str = "thread",
    max_workers: Optional[int] = None,
) -> ParallelExecutor:
    """
    Factory function to get an executor by strategy name.

    Args:
        strategy: The execution strategy ("thread" or "process")
        max_workers: Maximum number of workers (optional)

    Returns:
        A ParallelExecutor instance for the requested strategy

    Raises:
        ValueError: If strategy is not recognized

    Example:
        executor = get_executor("thread", max_workers=4)
        with executor:
            future = executor.submit(my_function, arg1)
            result = future.result()
    """
    if strategy not in _EXECUTOR_REGISTRY:
        valid_strategies = ", ".join(sorted(_EXECUTOR_REGISTRY.keys()))
        raise ValueError(
            f"Unknown parallel strategy: '{strategy}'. "
            f"Valid strategies are: {valid_strategies}"
        )

    executor_class = _EXECUTOR_REGISTRY[strategy]
    return executor_class(max_workers=max_workers)


def register_executor(name: str, executor_class: type) -> None:
    """
    Register a custom executor implementation.

    Args:
        name: The strategy name for the executor
        executor_class: The executor class (must implement ParallelExecutor)

    Example:
        class MyCustomExecutor:
            # ... implementation ...

        register_executor("custom", MyCustomExecutor)
    """
    _EXECUTOR_REGISTRY[name] = executor_class


def available_strategies() -> List[str]:
    """
    Get a list of available execution strategies.

    Returns:
        List of strategy names
    """
    return sorted(_EXECUTOR_REGISTRY.keys())
