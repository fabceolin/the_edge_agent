import inspect
import logging
import time
import traceback
from typing import Any, Callable, Dict, List, Optional, Union, Generator, Tuple
import networkx as nx
from concurrent.futures import (
    Future,
    TimeoutError as FuturesTimeoutError,
)
import threading
import copy
from queue import Queue, Empty

# TEA-BUILTIN-015.5: Import HTTPResponse for early termination
# TEA-ARCH-001: Moved to exceptions module to avoid transitive dependency on actions package
from the_edge_agent.exceptions import HTTPResponse

from the_edge_agent.checkpoint import CheckpointMixin
from the_edge_agent.visualization import VisualizationMixin
from the_edge_agent.parallel import (
    ParallelConfig,
    ParallelFlowResult,
    RetryPolicy,
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerRegistry,
    CircuitState,
    CircuitOpenError,
    RetryExhaustedError,
    CancellationToken,
    ParallelFlowCallback,
    ParallelFlowContext,
    CallbackManager,
)

# TEA-PARALLEL-001.1: Executor abstraction for parallel strategies
from the_edge_agent.parallel_executors import get_executor

# Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin

START = "__start__"
END = "__end__"


class StateGraph(CheckpointMixin, VisualizationMixin):
    """
    A graph-based state machine for managing complex workflows.

    This class allows defining states, transitions, and conditions for state changes,
    as well as executing the workflow based on the defined graph structure.

    Attributes:
        state_schema (Dict[str, Any]): The schema defining the structure of the state.
        graph (nx.DiGraph): The directed graph representing the state machine.
        interrupt_before (List[str]): Nodes to interrupt before execution.
        interrupt_after (List[str]): Nodes to interrupt after execution.
        logger (logging.Logger): Logger instance for observability.
        log_state_values (bool): Whether to log full state values.

    Logging:
        The StateGraph uses Python's logging module for observability. Configure via:
        - log_level: Set to logging.DEBUG for detailed trace, logging.INFO for flow info
        - log_state_values: Set to True to log state contents (security-sensitive)

        Log levels used:
        - DEBUG: Node entry/exit, edge evaluation, state keys, transitions
        - INFO: Node completion, parallel flow start/join, execution complete
        - WARNING: Fallback paths, no valid next node situations
        - ERROR: Exceptions in node execution

        To capture logs, configure a handler on the logger:
            >>> import logging
            >>> logging.basicConfig(level=logging.DEBUG)
            >>> graph = StateGraph({"value": int}, log_level=logging.DEBUG)

    Example:
        >>> graph = StateGraph({"value": int})
        >>> graph.add_node("start", run=lambda state: {"value": state["value"] + 1})
        >>> graph.add_node("end", run=lambda state: {"result": f"Final value: {state['value']}"})
        >>> graph.set_entry_point("start")
        >>> graph.set_finish_point("end")
        >>> graph.add_edge("start", "end")
        >>> result = list(graph.invoke({"value": 1}))
        >>> print(result[-1]["state"]["result"])
        Final value: 2
    """

    def __init__(
        self,
        state_schema: Dict[str, Any],
        raise_exceptions: bool = False,
        max_workers: Optional[int] = None,
        log_level: int = logging.WARNING,
        log_state_values: bool = False,
        parallel_config: Optional[ParallelConfig] = None,
    ):
        """
        Initialize the StateGraph.

        Args:
            state_schema (Dict[str, Any]): The schema defining the structure of the state.
            raise_exceptions (bool): If True, exceptions in node functions will be raised instead of being handled internally.
            max_workers (Optional[int]): Maximum number of worker threads for parallel execution.
                If None, uses Python's default: min(32, os.cpu_count() + 4).
            log_level (int): Logging level for the graph execution. Defaults to logging.WARNING.
                Use logging.DEBUG for detailed trace, logging.INFO for high-level flow.
            log_state_values (bool): If True, log full state values. Defaults to False for security
                (state may contain secrets, API keys, or PII). Only enable in trusted environments.
            parallel_config (Optional[ParallelConfig]): Default configuration for parallel flows.
                Can be overridden per-edge via add_parallel_edge() or at compile time.
        """
        self.state_schema = state_schema
        self.graph = nx.DiGraph()
        self.graph.add_node(START, run=None)
        self.graph.add_node(END, run=None)
        self.interrupt_before: List[str] = []
        self.interrupt_after: List[str] = []
        self.raise_exceptions = raise_exceptions
        self.parallel_sync = {}
        self.max_workers = max_workers
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(log_level)
        self.log_state_values = log_state_values
        self.checkpoint_dir: Optional[str] = None
        self.checkpointer: Optional[Any] = None  # MemoryCheckpointer or compatible
        # Parallel execution reliability (TD.13)
        self.parallel_config: ParallelConfig = parallel_config or ParallelConfig()
        self.circuit_registry: CircuitBreakerRegistry = CircuitBreakerRegistry(
            scope="graph"
        )
        self.callback_manager: Optional[CallbackManager] = None
        self._max_active_threads: Optional[int] = (
            None  # TECH-001: thread pool exhaustion prevention
        )

    def add_node(self, node: str, run: Optional[Callable[..., Any]] = None) -> None:
        """
        Add a node to the graph.

        Args:
            node (str): The name of the node.
            run (Optional[Callable[..., Any]]): The function to run when this node is active.

        Raises:
            ValueError: If the node already exists in the graph.
        """
        if node in self.graph.nodes:
            raise ValueError(f"Node '{node}' already exists in the graph.")
        self.graph.add_node(node, run=run)

    def add_edge(self, in_node: str, out_node: str) -> None:
        """
        Add an unconditional edge between two nodes.

        Args:
            in_node (str): The source node.
            out_node (str): The target node.

        Raises:
            ValueError: If either node doesn't exist in the graph.
        """
        if in_node not in self.graph.nodes or out_node not in self.graph.nodes:
            raise ValueError("Both nodes must exist in the graph.")
        self.graph.add_edge(
            in_node, out_node, cond=lambda **kwargs: True, cond_map={True: out_node}
        )

    def add_conditional_edges(
        self, in_node: str, func: Callable[..., Any], cond: Dict[Any, str]
    ) -> None:
        """
        Add conditional edges from a node based on a function's output.

        Args:
            in_node (str): The source node.
            func (Callable[..., Any]): The function to determine the next node.
            cond (Dict[Any, str]): Mapping of function outputs to target nodes.

        Raises:
            ValueError: If the source node doesn't exist or if any target node is invalid.
        """
        if in_node not in self.graph.nodes:
            raise ValueError(f"Node '{in_node}' does not exist in the graph.")
        for cond_value, out_node in cond.items():
            if out_node not in self.graph.nodes:
                raise ValueError(
                    f"Target node '{out_node}' does not exist in the graph."
                )
            self.graph.add_edge(in_node, out_node, cond=func, cond_map=cond)

    def add_parallel_edge(
        self,
        in_node: str,
        out_node: str,
        fan_in_node: str,
        config: Optional[ParallelConfig] = None,
    ) -> None:
        """
        Add an unconditional parallel edge between two nodes.

        Args:
            in_node (str): The source node.
            out_node (str): The target node.
            fan_in_node (str): The fan-in node that this parallel flow will reach.
            config (Optional[ParallelConfig]): Per-edge configuration for this parallel flow.
                If None, uses the graph-level parallel_config.

        Raises:
            ValueError: If either node doesn't exist in the graph.

        Example:
            >>> # With custom timeout for slow flow
            >>> graph.add_parallel_edge(
            ...     "start", "slow_api", "fan_in",
            ...     config=ParallelConfig(timeout_seconds=60.0)
            ... )
        """
        if (
            in_node not in self.graph.nodes
            or out_node not in self.graph.nodes
            or fan_in_node not in self.graph.nodes
        ):
            raise ValueError("All nodes must exist in the graph.")
        self.graph.add_edge(
            in_node,
            out_node,
            cond=lambda **kwargs: True,
            cond_map={True: out_node},
            parallel=True,
            fan_in_node=fan_in_node,
            parallel_config=config,  # Store per-edge config
        )

    def add_fanin_node(
        self, node: str, run: Optional[Callable[..., Any]] = None
    ) -> None:
        """
        Add a fan-in node to the graph.

        Args:
            node (str): The name of the node.
            run (Optional[Callable[..., Any]]): The function to run when this node is active.

        Raises:
            ValueError: If the node already exists in the graph.
        """
        if node in self.graph.nodes:
            raise ValueError(f"Node '{node}' already exists in the graph.")
        self.graph.add_node(node, run=run, fan_in=True)

    def invoke(
        self,
        input_state: Optional[Dict[str, Any]] = None,
        config: Optional[Dict[str, Any]] = None,
        checkpoint: Optional[str] = None,
    ) -> Generator[Dict[str, Any], None, None]:
        """
        Execute the graph, yielding interrupts and the final state.

        Args:
            input_state (Optional[Dict[str, Any]]): The initial state. Pass None to resume
                from a checkpoint (requires checkpoint parameter).
            config (Optional[Dict[str, Any]]): Configuration for the execution.
            checkpoint (Optional[str]): Checkpoint identifier to resume from. Can be:
                - File path (when using checkpoint_dir)
                - Memory key (when using MemoryCheckpointer)

        Yields:
            Dict[str, Any]: Events during execution. Possible types:
                - {"type": "interrupt", "node": str, "state": dict, "checkpoint_path": str}: Interrupt
                - {"type": "error", "node": str, "error": str, "state": dict}: Error occurred
                - {"type": "final", "state": dict}: Execution completed successfully

        Raises:
            RuntimeError: If raise_exceptions=True and an error occurs in any node
                (including parallel flows and fan-in nodes).
            ValueError: If input_state is None without checkpoint parameter.
            FileNotFoundError: If checkpoint file doesn't exist.
            KeyError: If checkpoint key doesn't exist in MemoryCheckpointer.

        Resume Pattern:
            Interrupts STOP execution. To continue, call invoke(None, checkpoint=...).

        Example:
            >>> # First invoke: runs until interrupt
            >>> events = list(graph.invoke({"x": 1}))
            >>> checkpoint_path = events[-1]["checkpoint_path"]
            >>> # Resume from checkpoint
            >>> events = list(graph.invoke(None, checkpoint=checkpoint_path))
        """
        # Resume case: input_state is None signals resume
        if input_state is None:
            if checkpoint is None:
                raise ValueError(
                    "checkpoint parameter required when input_state is None. "
                    "Use invoke(None, checkpoint='...') to resume from an interrupt."
                )
            yield from self.resume_from_checkpoint(checkpoint, config)
            return

        # Legacy support: checkpoint provided with input_state (checkpoint takes precedence)
        if checkpoint is not None:
            # If input_state is provided with checkpoint, treat it as a state update
            yield from self.resume_from_checkpoint(
                checkpoint, config, state_update=input_state
            )
            return

        if config is None:
            config = {}

        current_node = START
        state = input_state.copy()
        config = config.copy()

        # TEA-BUILTIN-015.4: Validate input against schema if attached
        input_schema = getattr(self, "_input_schema", None)
        if input_schema is not None:
            from the_edge_agent.validation import validate_input, ValidationError

            try:
                validated_state = validate_input(state, input_schema)
                state = validated_state
            except ValidationError as e:
                # Return 422-style validation error event
                yield {
                    "type": "validation_error",
                    "status_code": 422,
                    "errors": [err.to_dict() for err in e.errors],
                    "state": input_state.copy(),
                }
                return

        # Log execution start
        self.logger.debug(f"Starting execution with state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Initial state: {state}")

        # TEA-PARALLEL-001.1: Use executor abstraction for configurable parallel strategy
        # Allow runtime override via config, fallback to instance default
        max_workers = config.get("max_workers", self.max_workers)
        # Get strategy from parallel_config (default: "thread")
        strategy = self.parallel_config.strategy
        with get_executor(strategy, max_workers=max_workers) as executor:
            # Mapping from fan-in nodes to list of futures
            fanin_futures: Dict[str, List[Future]] = {}
            # Lock for thread-safe operations on fanin_futures
            fanin_lock = threading.Lock()

            while current_node != END:
                # Check for interrupt before
                if current_node in self.interrupt_before:
                    checkpoint_path = self._auto_save_checkpoint(
                        state, current_node, config
                    )
                    yield {
                        "type": "interrupt",
                        "node": current_node,
                        "state": state.copy(),
                        "checkpoint_path": checkpoint_path,
                    }
                    return  # STOP execution - must resume via invoke(None, checkpoint=...)

                # Get node data
                node_data = self.node(current_node)
                run_func = node_data.get("run")

                # Execute node's run function if present
                if run_func:
                    self.logger.debug(f"Entering node: {current_node}")
                    if self.log_state_values:
                        self.logger.debug(f"Node '{current_node}' input state: {state}")
                    try:
                        result = self._execute_node_function(
                            run_func, state, config, current_node
                        )
                        state.update(result)
                        self.logger.info(
                            f"Node '{current_node}' completed successfully"
                        )
                        if self.log_state_values:
                            self.logger.debug(
                                f"Node '{current_node}' output state: {state}"
                            )
                    except HTTPResponse as http_response:
                        # TEA-BUILTIN-015.5: Handle http.respond early termination
                        self.logger.info(
                            f"HTTP response from node '{current_node}': status={http_response.status}"
                        )
                        yield {
                            "type": "http_response",
                            "node": current_node,
                            "status": http_response.status,
                            "body": http_response.body,
                            "headers": http_response.headers,
                            "state": state.copy(),
                        }
                        return
                    except Exception as e:
                        self.logger.error(f"Error in node '{current_node}': {e}")
                        if self.raise_exceptions:
                            raise RuntimeError(
                                f"Error in node '{current_node}': {str(e)}"
                            ) from e
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": str(e),
                                "state": state.copy(),
                            }
                            return

                # Check for interrupt after
                if current_node in self.interrupt_after:
                    checkpoint_path = self._auto_save_checkpoint(
                        state, current_node, config
                    )
                    yield {
                        "type": "interrupt",
                        "node": current_node,
                        "state": state.copy(),
                        "checkpoint_path": checkpoint_path,
                    }
                    return  # STOP execution - must resume via invoke(None, checkpoint=...)

                # Determine next node
                successors = self.successors(current_node)

                # Separate parallel and normal edges
                parallel_edges = []
                normal_successors = []

                for successor in successors:
                    edge_data = self.edge(current_node, successor)
                    if edge_data.get("parallel", False):
                        fan_in_node = edge_data.get("fan_in_node", None)
                        if fan_in_node is None:
                            error_msg = f"Parallel edge from '{current_node}' to '{successor}' must have 'fan_in_node' specified"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {
                                    "type": "error",
                                    "node": current_node,
                                    "error": error_msg,
                                    "state": state.copy(),
                                }
                                return
                        parallel_edges.append((successor, fan_in_node))
                    else:
                        normal_successors.append(successor)

                # Start parallel flows
                if parallel_edges:
                    self.logger.info(
                        f"Starting {len(parallel_edges)} parallel flow(s) from node '{current_node}'"
                    )
                for successor, fan_in_node in parallel_edges:
                    # Get per-edge config (TD.13)
                    edge_data = self.edge(current_node, successor)
                    edge_config = self._get_parallel_config_for_edge(edge_data)

                    # Start a new thread for the flow starting from successor
                    self.logger.debug(
                        f"Launching parallel flow to node '{successor}' (fan-in: '{fan_in_node}')"
                    )
                    start_time = time.time()

                    # Submit with enhanced metadata for timeout/retry/circuit breaker handling
                    future = executor.submit(
                        self._execute_flow_with_reliability,
                        successor,
                        copy.deepcopy(state),
                        config.copy(),
                        fan_in_node,
                        edge_config,
                        start_time,
                    )
                    # Register the future with the corresponding fan-in node
                    with fanin_lock:
                        if fan_in_node not in fanin_futures:
                            fanin_futures[fan_in_node] = []
                        fanin_futures[fan_in_node].append(
                            (future, successor, edge_config, start_time)
                        )

                # Handle normal successors
                if normal_successors:
                    # For simplicity, take the first valid normal successor
                    next_node = None
                    for successor in normal_successors:
                        edge_data = self.edge(current_node, successor)
                        cond_func = edge_data.get("cond", lambda **kwargs: True)
                        cond_map = edge_data.get("cond_map", None)
                        available_params = {
                            "state": state,
                            "config": config,
                            "node": current_node,
                            "graph": self,
                        }
                        cond_params = self._prepare_function_params(
                            cond_func, available_params
                        )
                        cond_result = cond_func(**cond_params)
                        self.logger.debug(
                            f"Edge '{current_node}' -> '{successor}': condition result = {cond_result}"
                        )

                        if cond_map:
                            next_node_candidate = cond_map.get(cond_result, None)
                            if next_node_candidate:
                                next_node = next_node_candidate
                                self.logger.debug(
                                    f"Transitioning from '{current_node}' to '{next_node}'"
                                )
                                break
                        else:
                            if cond_result:
                                next_node = successor
                                self.logger.debug(
                                    f"Transitioning from '{current_node}' to '{next_node}'"
                                )
                                break
                    if next_node:
                        current_node = next_node
                    else:
                        self.logger.warning(
                            f"No valid next node found from node '{current_node}'"
                        )
                        error_msg = (
                            f"No valid next node found from node '{current_node}'"
                        )
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": error_msg,
                                "state": state.copy(),
                            }
                            return
                else:
                    # No normal successors
                    # Check if there is a fan-in node with pending futures
                    # Use lock for thread-safe access to fanin_futures
                    with fanin_lock:
                        if fanin_futures:
                            # Proceed to the fan-in node
                            current_node = list(fanin_futures.keys())[0]
                            # pop() atomically retrieves AND removes - prevents double processing
                            futures = fanin_futures.pop(current_node, [])
                        else:
                            futures = None

                    if futures is not None:
                        # Wait for futures outside lock to avoid blocking other threads
                        self.logger.info(
                            f"Joining {len(futures)} parallel flow(s) at fan-in node '{current_node}'"
                        )
                        results: List[ParallelFlowResult] = []
                        has_failure = False
                        fail_fast_triggered = False

                        for future, branch, edge_config, start_time in futures:
                            timeout = edge_config.timeout_seconds
                            fail_fast = edge_config.fail_fast

                            try:
                                # Get result with optional timeout
                                raw_result = future.result(timeout=timeout)
                                elapsed_ms = (time.time() - start_time) * 1000

                                # Check if result is an error dict from _execute_flow_with_reliability
                                if isinstance(raw_result, ParallelFlowResult):
                                    result = raw_result
                                elif (
                                    isinstance(raw_result, dict)
                                    and raw_result.get("type") == "error"
                                ):
                                    # Convert legacy error dict to ParallelFlowResult
                                    result = ParallelFlowResult(
                                        branch=branch,
                                        success=False,
                                        state=raw_result.get("state"),
                                        error=raw_result.get("error"),
                                        error_type="ExecutionError",
                                        timing_ms=elapsed_ms,
                                    )
                                    has_failure = True
                                else:
                                    # Success - raw_result is the state dict
                                    result = ParallelFlowResult.from_success(
                                        branch=branch,
                                        state=raw_result,
                                        timing_ms=elapsed_ms,
                                    )
                                results.append(result)

                                # Fail-fast check
                                if not result.success and fail_fast:
                                    fail_fast_triggered = True
                                    self.logger.warning(
                                        f"Fail-fast triggered by branch '{branch}'"
                                    )
                                    break

                            except FuturesTimeoutError:
                                # Timeout handling
                                elapsed_ms = (time.time() - start_time) * 1000
                                self.logger.warning(
                                    f"Parallel flow '{branch}' timed out after {timeout}s"
                                )
                                future.cancel()  # Best effort cancellation
                                result = ParallelFlowResult.from_timeout(
                                    branch=branch,
                                    state=None,  # State unknown after timeout
                                    timing_ms=elapsed_ms,
                                    timeout_seconds=timeout,
                                )
                                results.append(result)
                                has_failure = True

                                # Fire timeout callback if registered
                                if self.callback_manager:
                                    context = ParallelFlowContext(
                                        branch=branch,
                                        fan_in_node=current_node,
                                        state_snapshot=state.copy(),
                                        start_time=start_time,
                                        config=edge_config,
                                    )
                                    self.callback_manager.fire_flow_timeout(
                                        context, timeout
                                    )

                                if fail_fast:
                                    fail_fast_triggered = True
                                    self.logger.warning(
                                        f"Fail-fast triggered by timeout on branch '{branch}'"
                                    )
                                    break

                            except Exception as e:
                                # Unexpected error during result collection
                                elapsed_ms = (time.time() - start_time) * 1000
                                self.logger.error(
                                    f"Error collecting result from branch '{branch}': {e}"
                                )
                                result = ParallelFlowResult.from_error(
                                    branch=branch,
                                    exception=e,
                                    state=None,
                                    timing_ms=elapsed_ms,
                                    include_traceback=edge_config.include_traceback,
                                )
                                results.append(result)
                                has_failure = True

                                if fail_fast:
                                    fail_fast_triggered = True
                                    break

                        self.logger.debug(
                            f"All parallel flows joined at '{current_node}'"
                        )

                        # Check if we should abort due to failures (when fail_fast is True)
                        if fail_fast_triggered and self.raise_exceptions:
                            failed_branches = [
                                r.branch for r in results if not r.success
                            ]
                            raise RuntimeError(
                                f"Parallel execution aborted due to failures in: {failed_branches}"
                            )

                        # Check for errors in parallel flows
                        # When raise_exceptions=True, propagate the first error
                        # When raise_exceptions=False, continue with partial results
                        for result in results:
                            if isinstance(result, ParallelFlowResult):
                                if not result.success:
                                    if self.raise_exceptions:
                                        # Propagate the error as RuntimeError
                                        error_msg = f"Error in node '{result.branch}': {result.error}"
                                        raise RuntimeError(error_msg)
                                    else:
                                        # Yield error event and stop (backwards compatibility)
                                        yield {
                                            "type": "error",
                                            "node": result.branch,
                                            "error": result.error,
                                            "state": result.state or state.copy(),
                                        }
                                        return
                            elif (
                                isinstance(result, dict)
                                and result.get("type") == "error"
                            ):
                                # Legacy error dict path
                                self.logger.error(
                                    f"Error from parallel flow: {result.get('error')}"
                                )
                                if self.raise_exceptions:
                                    raise RuntimeError(
                                        f"Error in node '{result.get('node')}': {result.get('error')}"
                                    )
                                yield result
                                return

                        # Collect the results in the state
                        state["parallel_results"] = results

                        # Check for interrupt before fan-in execution
                        if current_node in self.interrupt_before:
                            checkpoint_path = self._auto_save_checkpoint(
                                state, current_node, config
                            )
                            yield {
                                "type": "interrupt",
                                "node": current_node,
                                "state": state.copy(),
                                "checkpoint_path": checkpoint_path,
                            }
                            return  # STOP execution - must resume via invoke(None, checkpoint=...)

                        # Execute the fan-in node's run function
                        node_data = self.node(current_node)
                        run_func = node_data.get("run")
                        if run_func:
                            self.logger.debug(f"Entering fan-in node: {current_node}")
                            try:
                                result = self._execute_node_function(
                                    run_func, state, config, current_node
                                )
                                state.update(result)
                                self.logger.info(
                                    f"Fan-in node '{current_node}' completed successfully"
                                )
                            except Exception as e:
                                self.logger.error(
                                    f"Error in fan-in node '{current_node}': {e}"
                                )
                                if self.raise_exceptions:
                                    raise RuntimeError(
                                        f"Error in node '{current_node}': {str(e)}"
                                    ) from e
                                else:
                                    yield {
                                        "type": "error",
                                        "node": current_node,
                                        "error": str(e),
                                        "state": state.copy(),
                                    }
                                    return

                        # Check for interrupt after fan-in execution
                        if current_node in self.interrupt_after:
                            checkpoint_path = self._auto_save_checkpoint(
                                state, current_node, config
                            )
                            yield {
                                "type": "interrupt",
                                "node": current_node,
                                "state": state.copy(),
                                "checkpoint_path": checkpoint_path,
                            }
                            return  # STOP execution - must resume via invoke(None, checkpoint=...)

                        # Continue to next node
                        next_node = self._get_next_node(current_node, state, config)
                        if not next_node:
                            self.logger.warning(
                                f"No valid next node found from fan-in node '{current_node}'"
                            )
                            error_msg = (
                                f"No valid next node found from node '{current_node}'"
                            )
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {
                                    "type": "error",
                                    "node": current_node,
                                    "error": error_msg,
                                    "state": state.copy(),
                                }
                                return
                        current_node = next_node
                    else:
                        error_msg = (
                            f"No valid next node found from node '{current_node}'"
                        )
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": error_msg,
                                "state": state.copy(),
                            }
                            return
        # Once END is reached, yield final state
        self.logger.info("Execution completed successfully")
        self.logger.debug(f"Final state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Final state: {state}")

        # TEA-BUILTIN-015.5: Auto-apply output transformation if output_schema attached
        final_state = state.copy()
        output = None
        if hasattr(self, "_output_schema") and self._output_schema is not None:
            try:
                from the_edge_agent.transformation import transform_output

                engine = getattr(self, "_yaml_engine", None)
                if engine is not None:
                    output = transform_output(
                        final_state,
                        self._output_schema,
                        engine._jinja_env,
                    )
                    self.logger.debug(
                        f"Applied output_schema transformation: {list(output.keys())}"
                    )
            except Exception as e:
                self.logger.warning(f"Error applying output transformation: {e}")
                # Don't fail the whole execution, just skip transformation
                output = None

        yield {"type": "final", "state": final_state, "output": output}

    def _execute_flow(self, current_node, state, config, fan_in_node):
        """
        Execute a flow starting from current_node until it reaches fan_in_node.

        This method is used for parallel execution paths. Each parallel flow runs
        in its own thread and executes independently until reaching the fan-in node.

        Args:
            current_node (str): The starting node of the flow.
            state (Dict[str, Any]): The state of the flow (deep copied for thread safety).
            config (Dict[str, Any]): Configuration for the execution.
            fan_in_node (str): The fan-in node where this flow should stop.

        Returns:
            Dict[str, Any]: Either:
                - The final state when flow reaches fan_in_node successfully
                - Error dict {"type": "error", "node": str, "error": str, "state": dict}
                  when raise_exceptions=False and an error occurs

        Raises:
            RuntimeError: If raise_exceptions=True and an error occurs in any node.

        Error Handling:
            When raise_exceptions=False (default):
                - Returns {"type": "error", "node": <node>, "error": <msg>, "state": <state>}
                - invoke() will detect this and yield the error event
            When raise_exceptions=True:
                - Raises RuntimeError with message: "Error in node '<node>': <msg>"
        """
        self.logger.info(
            f"Parallel flow started at node '{current_node}' (target fan-in: '{fan_in_node}')"
        )
        while current_node != END:
            # Check if current node is the fan-in node
            if current_node == fan_in_node:
                # Return the state to be collected at fan-in node
                self.logger.info(f"Parallel flow reached fan-in node '{fan_in_node}'")
                return state

            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                self.logger.debug(f"[Parallel] Entering node: {current_node}")
                if self.log_state_values:
                    self.logger.debug(
                        f"[Parallel] Node '{current_node}' input state: {state}"
                    )
                try:
                    result = self._execute_node_function(
                        run_func, state, config, current_node
                    )
                    state.update(result)
                    self.logger.debug(f"[Parallel] Node '{current_node}' completed")
                    if self.log_state_values:
                        self.logger.debug(
                            f"[Parallel] Node '{current_node}' output state: {state}"
                        )
                except Exception as e:
                    self.logger.error(f"[Parallel] Error in node '{current_node}': {e}")
                    if self.raise_exceptions:
                        raise RuntimeError(
                            f"Error in node '{current_node}': {str(e)}"
                        ) from e
                    else:
                        # Return consistent error dict structure
                        return {
                            "type": "error",
                            "node": current_node,
                            "error": str(e),
                            "state": state.copy(),
                        }

            # Determine next node using _get_next_node
            try:
                next_node = self._get_next_node(current_node, state, config)
            except Exception as e:
                self.logger.error(
                    f"[Parallel] Error getting next node from '{current_node}': {e}"
                )
                if self.raise_exceptions:
                    raise
                else:
                    return {
                        "type": "error",
                        "node": current_node,
                        "error": str(e),
                        "state": state.copy(),
                    }

            if next_node:
                self.logger.debug(
                    f"[Parallel] Transitioning from '{current_node}' to '{next_node}'"
                )
                current_node = next_node
            else:
                error_msg = f"No valid next node found from node '{current_node}' in parallel flow"
                self.logger.warning(f"[Parallel] {error_msg}")
                if self.raise_exceptions:
                    raise RuntimeError(error_msg)
                else:
                    return {
                        "type": "error",
                        "node": current_node,
                        "error": error_msg,
                        "state": state.copy(),
                    }

        # Reached END
        self.logger.debug(f"[Parallel] Flow reached END")
        return state

    def _execute_flow_with_reliability(
        self,
        start_node: str,
        state: Dict[str, Any],
        config: Dict[str, Any],
        fan_in_node: str,
        parallel_config: ParallelConfig,
        start_time: float,
    ) -> Union[ParallelFlowResult, Dict[str, Any]]:
        """
        Execute a flow with retry, circuit breaker, and timeout support.

        This method wraps _execute_flow with reliability patterns from TD.13:
        - Circuit breaker: Fail fast if circuit is open
        - Retry policy: Retry on failure with exponential backoff
        - Callback integration: Fire lifecycle events

        Args:
            start_node: The starting node of the parallel flow
            state: The state dict (deep copied for thread safety)
            config: Configuration for execution
            fan_in_node: The fan-in node where this flow should stop
            parallel_config: Configuration for timeout, retry, circuit breaker
            start_time: Time when flow was submitted (for timing calculations)

        Returns:
            ParallelFlowResult on success or failure
            (May return raw dict for backwards compatibility when no reliability features used)
        """
        branch = start_node
        attempt_errors: List[Dict[str, Any]] = []
        retry_policy = parallel_config.retry_policy
        cb_config = parallel_config.circuit_breaker
        circuit_state_str: Optional[str] = None

        # Circuit breaker check
        circuit: Optional[CircuitBreaker] = None
        if cb_config is not None:
            circuit = self.circuit_registry.get_or_create(branch, cb_config)
            circuit_state_str = circuit.state.value

            if not circuit.allow_request():
                self.logger.warning(f"Circuit breaker open for branch '{branch}'")
                return ParallelFlowResult.from_circuit_open(branch, branch)

        # Fire on_flow_start callback
        if self.callback_manager:
            context = ParallelFlowContext(
                branch=branch,
                fan_in_node=fan_in_node,
                state_snapshot=state.copy(),
                start_time=start_time,
                config=parallel_config,
            )
            self.callback_manager.fire_flow_start(context)

        # Determine max retries
        max_retries = retry_policy.max_retries if retry_policy else 0
        max_stored_errors = retry_policy.max_stored_errors if retry_policy else 5

        last_exception: Optional[Exception] = None
        attempt = 0

        while attempt <= max_retries:
            try:
                # Execute the flow
                result = self._execute_flow(
                    start_node, state.copy(), config, fan_in_node
                )
                elapsed_ms = (time.time() - start_time) * 1000

                # Check if result is an error dict
                if isinstance(result, dict) and result.get("type") == "error":
                    # Flow returned an error (not an exception)
                    error_msg = result.get("error", "Unknown error")
                    exc = RuntimeError(error_msg)

                    # Record failure for circuit breaker
                    if circuit:
                        circuit.record_failure()
                        circuit_state_str = circuit.state.value

                    # Store error for retry
                    if len(attempt_errors) < max_stored_errors:
                        attempt_errors.append(
                            {
                                "attempt": attempt,
                                "error": error_msg,
                                "error_type": "ExecutionError",
                            }
                        )

                    # Check if we should retry
                    if retry_policy and retry_policy.should_retry(exc, attempt):
                        delay = retry_policy.get_delay(attempt)
                        self.logger.info(
                            f"Retrying branch '{branch}' after {delay}s (attempt {attempt + 1}/{max_retries + 1})"
                        )

                        # Fire retry callback
                        if self.callback_manager:
                            context = ParallelFlowContext(
                                branch=branch,
                                fan_in_node=fan_in_node,
                                state_snapshot=state.copy(),
                                start_time=start_time,
                                config=parallel_config,
                            )
                            self.callback_manager.fire_flow_retry(
                                context, attempt + 1, delay, exc
                            )

                        time.sleep(delay)
                        attempt += 1
                        last_exception = exc
                        continue

                    # No more retries - return failure result
                    flow_result = ParallelFlowResult(
                        branch=branch,
                        success=False,
                        state=result.get("state"),
                        error=error_msg,
                        error_type="ExecutionError",
                        timing_ms=elapsed_ms,
                        retry_count=attempt,
                        attempt_errors=attempt_errors,
                        circuit_state=circuit_state_str,
                    )

                    # Fire completion callback
                    if self.callback_manager:
                        context = ParallelFlowContext(
                            branch=branch,
                            fan_in_node=fan_in_node,
                            state_snapshot=state.copy(),
                            start_time=start_time,
                            config=parallel_config,
                        )
                        self.callback_manager.fire_flow_complete(context, flow_result)

                    return flow_result

                # Success!
                if circuit:
                    circuit.record_success()
                    circuit_state_str = circuit.state.value

                flow_result = ParallelFlowResult.from_success(
                    branch=branch,
                    state=result,
                    timing_ms=elapsed_ms,
                    retry_count=attempt,
                    circuit_state=circuit_state_str,
                )

                # Fire completion callback
                if self.callback_manager:
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=(
                            result.copy() if isinstance(result, dict) else {}
                        ),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_complete(context, flow_result)

                return flow_result

            except Exception as e:
                elapsed_ms = (time.time() - start_time) * 1000
                self.logger.error(
                    f"Exception in branch '{branch}' (attempt {attempt + 1}): {e}"
                )

                # Record failure for circuit breaker
                if circuit:
                    circuit.record_failure()
                    circuit_state_str = circuit.state.value

                # Store error
                if len(attempt_errors) < max_stored_errors:
                    attempt_errors.append(
                        {
                            "attempt": attempt,
                            "error": str(e),
                            "error_type": type(e).__name__,
                        }
                    )

                # Fire error callback
                if self.callback_manager:
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=state.copy(),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_error(context, e, attempt)

                # Check if we should retry
                if retry_policy and retry_policy.should_retry(e, attempt):
                    delay = retry_policy.get_delay(attempt)
                    self.logger.info(
                        f"Retrying branch '{branch}' after {delay}s (attempt {attempt + 1}/{max_retries + 1})"
                    )

                    # Fire retry callback
                    if self.callback_manager:
                        context = ParallelFlowContext(
                            branch=branch,
                            fan_in_node=fan_in_node,
                            state_snapshot=state.copy(),
                            start_time=start_time,
                            config=parallel_config,
                        )
                        self.callback_manager.fire_flow_retry(
                            context, attempt + 1, delay, e
                        )

                    time.sleep(delay)
                    attempt += 1
                    last_exception = e
                    continue

                # No more retries
                flow_result = ParallelFlowResult.from_error(
                    branch=branch,
                    exception=e,
                    state=state.copy(),
                    timing_ms=elapsed_ms,
                    include_traceback=parallel_config.include_traceback,
                    retry_count=attempt,
                    attempt_errors=attempt_errors,
                    circuit_state=circuit_state_str,
                )

                # Fire completion callback
                if self.callback_manager:
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=state.copy(),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_complete(context, flow_result)

                return flow_result

        # Retries exhausted
        elapsed_ms = (time.time() - start_time) * 1000
        error_msg = f"All {max_retries + 1} attempts exhausted for branch '{branch}'"
        self.logger.error(error_msg)

        flow_result = ParallelFlowResult(
            branch=branch,
            success=False,
            state=state.copy(),
            error=str(last_exception) if last_exception else error_msg,
            error_type=(
                type(last_exception).__name__
                if last_exception
                else "RetryExhaustedError"
            ),
            timing_ms=elapsed_ms,
            retry_count=attempt,
            attempt_errors=attempt_errors,
            circuit_state=circuit_state_str,
        )

        # Fire completion callback
        if self.callback_manager:
            context = ParallelFlowContext(
                branch=branch,
                fan_in_node=fan_in_node,
                state_snapshot=state.copy(),
                start_time=start_time,
                config=parallel_config,
            )
            self.callback_manager.fire_flow_complete(context, flow_result)

        return flow_result

    def _stream_parallel_flow(
        self,
        start_node: str,
        state: Dict[str, Any],
        config: Dict[str, Any],
        fan_in_node: str,
        result_queue: Queue,
    ) -> None:
        """
        Execute a parallel flow and put yields into a queue for streaming.

        This method is used for parallel streaming execution. Each parallel flow
        runs in its own thread and puts events into the shared queue.

        Args:
            start_node (str): The starting node of the parallel flow.
            state (Dict[str, Any]): The state (deep copied for thread safety).
            config (Dict[str, Any]): Configuration for the execution.
            fan_in_node (str): The fan-in node where this flow should stop.
            result_queue (Queue): Queue to put events into for the main thread.

        Queue Events:
            - {"type": "parallel_state", "branch": str, "node": str, "state": dict}
            - {"type": "parallel_error", "branch": str, "node": str, "error": str, "state": dict}
            - {"type": "branch_complete", "branch": str, "state": dict}
        """
        current_node = start_node
        flow_state = state  # Already deep copied by caller

        self.logger.info(
            f"[Parallel Stream] Flow started at node '{start_node}' (target fan-in: '{fan_in_node}')"
        )

        while current_node != END:
            # Check if current node is the fan-in node
            if current_node == fan_in_node:
                self.logger.info(
                    f"[Parallel Stream] Flow '{start_node}' reached fan-in node '{fan_in_node}'"
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": start_node,
                        "state": flow_state.copy(),
                    }
                )
                return

            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                self.logger.debug(f"[Parallel Stream] Entering node: {current_node}")
                if self.log_state_values:
                    self.logger.debug(
                        f"[Parallel Stream] Node '{current_node}' input state: {flow_state}"
                    )
                try:
                    result = self._execute_node_function(
                        run_func, flow_state, config, current_node
                    )
                    flow_state.update(result)
                    self.logger.debug(
                        f"[Parallel Stream] Node '{current_node}' completed"
                    )
                    if self.log_state_values:
                        self.logger.debug(
                            f"[Parallel Stream] Node '{current_node}' output state: {flow_state}"
                        )
                    # Put state event into queue
                    result_queue.put(
                        {
                            "type": "parallel_state",
                            "branch": start_node,
                            "node": current_node,
                            "state": flow_state.copy(),
                        }
                    )
                except Exception as e:
                    self.logger.error(
                        f"[Parallel Stream] Error in node '{current_node}': {e}"
                    )
                    result_queue.put(
                        {
                            "type": "parallel_error",
                            "branch": start_node,
                            "node": current_node,
                            "error": str(e),
                            "state": flow_state.copy(),
                        }
                    )
                    # Signal branch completion with error state
                    result_queue.put(
                        {
                            "type": "branch_complete",
                            "branch": start_node,
                            "state": flow_state.copy(),
                            "error": True,
                        }
                    )
                    return

            # Determine next node
            try:
                next_node = self._get_next_node(current_node, flow_state, config)
            except Exception as e:
                self.logger.error(
                    f"[Parallel Stream] Error getting next node from '{current_node}': {e}"
                )
                result_queue.put(
                    {
                        "type": "parallel_error",
                        "branch": start_node,
                        "node": current_node,
                        "error": str(e),
                        "state": flow_state.copy(),
                    }
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": start_node,
                        "state": flow_state.copy(),
                        "error": True,
                    }
                )
                return

            if next_node:
                self.logger.debug(
                    f"[Parallel Stream] Transitioning from '{current_node}' to '{next_node}'"
                )
                current_node = next_node
            else:
                error_msg = f"No valid next node found from node '{current_node}' in parallel flow"
                self.logger.warning(f"[Parallel Stream] {error_msg}")
                result_queue.put(
                    {
                        "type": "parallel_error",
                        "branch": start_node,
                        "node": current_node,
                        "error": error_msg,
                        "state": flow_state.copy(),
                    }
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": start_node,
                        "state": flow_state.copy(),
                        "error": True,
                    }
                )
                return

        # Reached END (should not normally happen for parallel flows)
        self.logger.debug(f"[Parallel Stream] Flow '{start_node}' reached END")
        result_queue.put(
            {
                "type": "branch_complete",
                "branch": start_node,
                "state": flow_state.copy(),
            }
        )

    def _stream_parallel_flow_with_reliability(
        self,
        start_node: str,
        state: Dict[str, Any],
        config: Dict[str, Any],
        fan_in_node: str,
        result_queue: Queue,
        parallel_config: ParallelConfig,
        start_time: float,
    ) -> None:
        """
        Execute a parallel streaming flow with retry and circuit breaker support.

        This method wraps _stream_parallel_flow with reliability patterns from TD.13.

        Args:
            start_node: The starting node of the parallel flow
            state: The state dict (deep copied for thread safety)
            config: Configuration for execution
            fan_in_node: The fan-in node where this flow should stop
            result_queue: Queue to put events into
            parallel_config: Configuration for timeout, retry, circuit breaker
            start_time: Time when flow was submitted (for timing calculations)
        """
        branch = start_node
        retry_policy = parallel_config.retry_policy
        cb_config = parallel_config.circuit_breaker
        attempt_errors: List[Dict[str, Any]] = []
        circuit_state_str: Optional[str] = None

        # Circuit breaker check
        circuit: Optional[CircuitBreaker] = None
        if cb_config is not None:
            circuit = self.circuit_registry.get_or_create(branch, cb_config)
            circuit_state_str = circuit.state.value

            if not circuit.allow_request():
                self.logger.warning(
                    f"[Parallel Stream] Circuit breaker open for branch '{branch}'"
                )
                result_queue.put(
                    {
                        "type": "parallel_error",
                        "branch": branch,
                        "node": branch,
                        "error": f"Circuit breaker '{branch}' is open",
                        "state": state.copy(),
                        "circuit_state": CircuitState.OPEN.value,
                    }
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": branch,
                        "state": state.copy(),
                        "error": True,
                        "circuit_state": CircuitState.OPEN.value,
                    }
                )
                return

        # Fire on_flow_start callback
        if self.callback_manager:
            context = ParallelFlowContext(
                branch=branch,
                fan_in_node=fan_in_node,
                state_snapshot=state.copy(),
                start_time=start_time,
                config=parallel_config,
            )
            self.callback_manager.fire_flow_start(context)

        # Determine max retries
        max_retries = retry_policy.max_retries if retry_policy else 0
        max_stored_errors = retry_policy.max_stored_errors if retry_policy else 5

        attempt = 0
        while attempt <= max_retries:
            try:
                # Use a sub-queue to capture events from the flow
                # and check for errors at the end
                flow_error = [None]
                flow_state = [state.copy()]

                def wrapped_flow():
                    """Execute flow and track completion status."""
                    try:
                        current_node = start_node
                        while current_node != END:
                            if current_node == fan_in_node:
                                flow_state[0] = flow_state[0].copy()
                                return True  # Success

                            node_data = self.node(current_node)
                            run_func = node_data.get("run")

                            if run_func:
                                self.logger.debug(
                                    f"[Parallel Stream Retry] Entering node: {current_node}"
                                )
                                result = self._execute_node_function(
                                    run_func, flow_state[0], config, current_node
                                )
                                flow_state[0].update(result)
                                result_queue.put(
                                    {
                                        "type": "parallel_state",
                                        "branch": start_node,
                                        "node": current_node,
                                        "state": flow_state[0].copy(),
                                    }
                                )

                            next_node = self._get_next_node(
                                current_node, flow_state[0], config
                            )
                            if next_node:
                                current_node = next_node
                            else:
                                flow_error[0] = (
                                    f"No valid next node from '{current_node}'"
                                )
                                return False
                        return True
                    except Exception as e:
                        flow_error[0] = str(e)
                        return False

                success = wrapped_flow()
                elapsed_ms = (time.time() - start_time) * 1000

                if success:
                    # Record success for circuit breaker
                    if circuit:
                        circuit.record_success()
                        circuit_state_str = circuit.state.value

                    result_queue.put(
                        {
                            "type": "branch_complete",
                            "branch": branch,
                            "state": flow_state[0].copy(),
                            "timing_ms": elapsed_ms,
                            "retry_count": attempt,
                            "circuit_state": circuit_state_str,
                        }
                    )

                    # Fire completion callback
                    if self.callback_manager:
                        flow_result = ParallelFlowResult.from_success(
                            branch=branch,
                            state=flow_state[0],
                            timing_ms=elapsed_ms,
                            retry_count=attempt,
                            circuit_state=circuit_state_str,
                        )
                        context = ParallelFlowContext(
                            branch=branch,
                            fan_in_node=fan_in_node,
                            state_snapshot=flow_state[0].copy(),
                            start_time=start_time,
                            config=parallel_config,
                        )
                        self.callback_manager.fire_flow_complete(context, flow_result)
                    return

                # Flow failed
                error_msg = flow_error[0] or "Unknown error"
                exc = RuntimeError(error_msg)

                if circuit:
                    circuit.record_failure()
                    circuit_state_str = circuit.state.value

                if len(attempt_errors) < max_stored_errors:
                    attempt_errors.append(
                        {
                            "attempt": attempt,
                            "error": error_msg,
                            "error_type": "ExecutionError",
                        }
                    )

                # Check if we should retry
                if retry_policy and retry_policy.should_retry(exc, attempt):
                    delay = retry_policy.get_delay(attempt)
                    self.logger.info(
                        f"[Parallel Stream] Retrying branch '{branch}' after {delay}s (attempt {attempt + 1})"
                    )

                    if self.callback_manager:
                        context = ParallelFlowContext(
                            branch=branch,
                            fan_in_node=fan_in_node,
                            state_snapshot=state.copy(),
                            start_time=start_time,
                            config=parallel_config,
                        )
                        self.callback_manager.fire_flow_retry(
                            context, attempt + 1, delay, exc
                        )

                    time.sleep(delay)
                    attempt += 1
                    continue

                # No more retries
                result_queue.put(
                    {
                        "type": "parallel_error",
                        "branch": branch,
                        "node": branch,
                        "error": error_msg,
                        "state": flow_state[0].copy(),
                    }
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": branch,
                        "state": flow_state[0].copy(),
                        "error": True,
                        "timing_ms": elapsed_ms,
                        "retry_count": attempt,
                        "attempt_errors": attempt_errors,
                        "circuit_state": circuit_state_str,
                    }
                )

                if self.callback_manager:
                    flow_result = ParallelFlowResult(
                        branch=branch,
                        success=False,
                        state=flow_state[0].copy(),
                        error=error_msg,
                        error_type="ExecutionError",
                        timing_ms=elapsed_ms,
                        retry_count=attempt,
                        attempt_errors=attempt_errors,
                        circuit_state=circuit_state_str,
                    )
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=flow_state[0].copy(),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_complete(context, flow_result)
                return

            except Exception as e:
                elapsed_ms = (time.time() - start_time) * 1000
                self.logger.error(
                    f"[Parallel Stream] Exception in branch '{branch}' (attempt {attempt + 1}): {e}"
                )

                if circuit:
                    circuit.record_failure()
                    circuit_state_str = circuit.state.value

                if len(attempt_errors) < max_stored_errors:
                    attempt_errors.append(
                        {
                            "attempt": attempt,
                            "error": str(e),
                            "error_type": type(e).__name__,
                        }
                    )

                if self.callback_manager:
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=state.copy(),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_error(context, e, attempt)

                if retry_policy and retry_policy.should_retry(e, attempt):
                    delay = retry_policy.get_delay(attempt)
                    self.logger.info(
                        f"[Parallel Stream] Retrying branch '{branch}' after {delay}s"
                    )

                    if self.callback_manager:
                        context = ParallelFlowContext(
                            branch=branch,
                            fan_in_node=fan_in_node,
                            state_snapshot=state.copy(),
                            start_time=start_time,
                            config=parallel_config,
                        )
                        self.callback_manager.fire_flow_retry(
                            context, attempt + 1, delay, e
                        )

                    time.sleep(delay)
                    attempt += 1
                    continue

                result_queue.put(
                    {
                        "type": "parallel_error",
                        "branch": branch,
                        "node": branch,
                        "error": str(e),
                        "state": state.copy(),
                    }
                )
                result_queue.put(
                    {
                        "type": "branch_complete",
                        "branch": branch,
                        "state": state.copy(),
                        "error": True,
                        "timing_ms": elapsed_ms,
                        "retry_count": attempt,
                        "attempt_errors": attempt_errors,
                        "circuit_state": circuit_state_str,
                    }
                )

                if self.callback_manager:
                    flow_result = ParallelFlowResult.from_error(
                        branch=branch,
                        exception=e,
                        state=state.copy(),
                        timing_ms=elapsed_ms,
                        include_traceback=parallel_config.include_traceback,
                        retry_count=attempt,
                        attempt_errors=attempt_errors,
                        circuit_state=circuit_state_str,
                    )
                    context = ParallelFlowContext(
                        branch=branch,
                        fan_in_node=fan_in_node,
                        state_snapshot=state.copy(),
                        start_time=start_time,
                        config=parallel_config,
                    )
                    self.callback_manager.fire_flow_complete(context, flow_result)
                return

        # Retries exhausted
        elapsed_ms = (time.time() - start_time) * 1000
        error_msg = f"All {max_retries + 1} attempts exhausted for branch '{branch}'"
        self.logger.error(f"[Parallel Stream] {error_msg}")

        result_queue.put(
            {
                "type": "parallel_error",
                "branch": branch,
                "node": branch,
                "error": error_msg,
                "state": state.copy(),
            }
        )
        result_queue.put(
            {
                "type": "branch_complete",
                "branch": branch,
                "state": state.copy(),
                "error": True,
                "timing_ms": elapsed_ms,
                "retry_count": attempt,
                "attempt_errors": attempt_errors,
                "circuit_state": circuit_state_str,
            }
        )

        if self.callback_manager:
            flow_result = ParallelFlowResult(
                branch=branch,
                success=False,
                state=state.copy(),
                error=error_msg,
                error_type="RetryExhaustedError",
                timing_ms=elapsed_ms,
                retry_count=attempt,
                attempt_errors=attempt_errors,
                circuit_state=circuit_state_str,
            )
            context = ParallelFlowContext(
                branch=branch,
                fan_in_node=fan_in_node,
                state_snapshot=state.copy(),
                start_time=start_time,
                config=parallel_config,
            )
            self.callback_manager.fire_flow_complete(context, flow_result)

    def _execute_node_function(
        self,
        func: Callable[..., Any],
        state: Dict[str, Any],
        config: Dict[str, Any],
        node: str,
    ) -> Dict[str, Any]:
        """
        Execute the function associated with a node.

        Args:
            func (Callable[..., Any]): The function to execute.
            state (Dict[str, Any]): The current state.
            config (Dict[str, Any]): The configuration.
            node (str): The current node name.

        Returns:
            Dict[str, Any]: The result of the function execution.

        Raises:
            Exception: If an exception occurs during function execution.
        """
        available_params = {
            "state": state,
            "config": config,
            "node": node,
            "graph": self,
        }
        if "parallel_results" in state:
            available_params["parallel_results"] = state["parallel_results"]
        function_params = self._prepare_function_params(func, available_params)
        result = func(**function_params)
        if isinstance(result, dict):
            return result
        else:
            # If result is not a dict, wrap it in a dict
            return {"result": result}

    def set_entry_point(self, init_state: str) -> None:
        """
        Set the entry point of the graph.

        Args:
            init_state (str): The initial state node.

        Raises:
            ValueError: If the initial state node doesn't exist in the graph.
        """
        if init_state not in self.graph.nodes:
            raise ValueError(f"Node '{init_state}' does not exist in the graph.")
        self.graph.add_edge(
            START, init_state, cond=lambda **kwargs: True, cond_map={True: init_state}
        )

    def set_finish_point(self, final_state: str) -> None:
        """
        Set the finish point of the graph.

        Args:
            final_state (str): The final state node.

        Raises:
            ValueError: If the final state node doesn't exist in the graph.
        """
        if final_state not in self.graph.nodes:
            raise ValueError(f"Node '{final_state}' does not exist in the graph.")
        self.graph.add_edge(
            final_state, END, cond=lambda **kwargs: True, cond_map={True: END}
        )

    def compile(
        self,
        interrupt_before: List[str] = [],
        interrupt_after: List[str] = [],
        checkpoint_dir: Optional[str] = None,
        checkpointer: Optional[Any] = None,
        parallel_config: Optional[ParallelConfig] = None,
        parallel_callbacks: Optional[List[ParallelFlowCallback]] = None,
        max_active_threads: Optional[int] = None,
        circuit_breaker_scope: str = "graph",
    ) -> "StateGraph":
        """
        Compile the graph and set interruption points.

        Args:
            interrupt_before (List[str]): Nodes to interrupt before execution.
            interrupt_after (List[str]): Nodes to interrupt after execution.
            checkpoint_dir (Optional[str]): Directory for file-based checkpoint storage.
                When set, checkpoints are automatically saved before yielding interrupt events.
                Files are named: `{node}_{timestamp}.pkl`
            checkpointer (Optional[Any]): A checkpointer instance (e.g., MemoryCheckpointer)
                for in-memory or custom checkpoint storage.
            parallel_config (Optional[ParallelConfig]): Default config for parallel flows.
                Overrides constructor-level config. Per-edge config takes precedence.
            parallel_callbacks (Optional[List[ParallelFlowCallback]]): Callbacks for
                parallel flow lifecycle events (start, complete, error, timeout, retry).
            max_active_threads (Optional[int]): Maximum active threads for parallel execution.
                Prevents thread pool exhaustion from zombie threads (TECH-001).
            circuit_breaker_scope (str): "graph" (reset on new instance) or "global"
                (persist across instances). Default: "graph".

        Returns:
            StateGraph: The compiled graph instance.

        Raises:
            ValueError: If any interrupt node doesn't exist in the graph.
            ValueError: If interrupts are defined without a checkpointer.

        Note:
            When using interrupt_before or interrupt_after, you MUST provide either
            checkpoint_dir (for file-based storage) or checkpointer (for in-memory/custom storage).
            Interrupts STOP execution until explicitly resumed via invoke(None, checkpoint=...).

        Example:
            >>> from the_edge_agent import MemoryCheckpointer, ParallelConfig, RetryPolicy
            >>> checkpointer = MemoryCheckpointer()
            >>> graph.compile(
            ...     interrupt_before=["node_a"],
            ...     checkpointer=checkpointer,
            ...     parallel_config=ParallelConfig(
            ...         timeout_seconds=30.0,
            ...         retry_policy=RetryPolicy(max_retries=3)
            ...     ),
            ... )
        """
        for node in interrupt_before + interrupt_after:
            if node not in self.graph.nodes:
                raise ValueError(
                    f"Interrupt node '{node}' does not exist in the graph."
                )

        # Validate: interrupts require a checkpointer
        has_interrupts = bool(interrupt_before or interrupt_after)
        has_checkpointer = bool(checkpoint_dir or checkpointer)

        if has_interrupts and not has_checkpointer:
            raise ValueError(
                "A checkpointer is required when using interrupt_before or interrupt_after. "
                "Provide either checkpoint_dir='/path' for file-based storage "
                "or checkpointer=MemoryCheckpointer() for in-memory storage."
            )

        self.interrupt_before = interrupt_before
        self.interrupt_after = interrupt_after
        self.checkpoint_dir = checkpoint_dir
        self.checkpointer = checkpointer

        # Parallel execution reliability (TD.13)
        if parallel_config is not None:
            self.parallel_config = parallel_config
        if parallel_callbacks is not None:
            self.callback_manager = CallbackManager(callbacks=parallel_callbacks)
        if max_active_threads is not None:
            self._max_active_threads = max_active_threads
        if circuit_breaker_scope != "graph":
            self.circuit_registry = CircuitBreakerRegistry(scope=circuit_breaker_scope)

        return self

    def execute_scoped(
        self,
        initial_state: Dict[str, Any],
        entry_point: str,
        exit_point: str,
        config: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Execute a portion of the graph from entry_point to exit_point.

        This method enables scoped execution for remote parallel branch execution,
        where branches can run independently on different hosts and stop before
        the fan-in node (which runs on the main host).

        Args:
            initial_state: Starting state (typically loaded from --input JSON)
            entry_point: Node to start execution at (instead of __start__)
            exit_point: Node to stop BEFORE (not executed)
            config: Optional execution configuration

        Returns:
            Final state after executing the scoped portion of the graph

        Raises:
            ValueError: If entry_point doesn't exist in graph
            ValueError: If exit_point doesn't exist in graph
            ValueError: If no path exists from entry_point to exit_point
            ValueError: If entry_point and exit_point are the same
            RuntimeError: If raise_exceptions=True and an error occurs

        Example:
            >>> # Execute a branch of a parallel workflow
            >>> result = graph.execute_scoped(
            ...     initial_state={"value": 42},
            ...     entry_point="branch_a",
            ...     exit_point="merge"
            ... )
            >>> # Executes: branch_a → step_a1 → step_a2 → (stops before merge)

        Note:
            The exit_point node is NOT executed. This is critical for remote
            parallel execution where fan-in nodes must run on the main host.
        """
        # Validate entry_point exists
        if entry_point != START and entry_point not in self.graph.nodes:
            raise ValueError(f"Entry point '{entry_point}' not found in graph")

        # Validate exit_point exists
        if exit_point != END and exit_point not in self.graph.nodes:
            raise ValueError(f"Exit point '{exit_point}' not found in graph")

        # Validate entry and exit are different
        if entry_point == exit_point:
            raise ValueError(
                f"Entry point and exit point cannot be the same: '{entry_point}'"
            )

        # Validate path exists from entry to exit
        if not self._path_exists(entry_point, exit_point):
            raise ValueError(
                f"No execution path from '{entry_point}' to '{exit_point}'"
            )

        # Execute the scoped portion
        return self._run_scoped(
            state=initial_state.copy(),
            start_node=entry_point,
            stop_before=exit_point,
            config=config or {},
        )

    def _path_exists(self, start: str, end: str) -> bool:
        """
        Check if there's a path from start to end using BFS.

        This considers all edges including conditional edges, treating them
        as potentially traversable for path validation purposes.

        Args:
            start: Starting node
            end: Target node

        Returns:
            True if a path exists from start to end, False otherwise
        """
        if start == end:
            return False  # Can't have same entry and exit

        visited = set()
        queue = [start]

        while queue:
            current = queue.pop(0)
            if current == end:
                return True
            if current in visited:
                continue
            visited.add(current)

            # Get successors from all edges (including conditional/parallel)
            if current in self.graph.nodes:
                for successor in self.graph.successors(current):
                    if successor not in visited:
                        queue.append(successor)

        return False

    def _get_nodes_between(self, start: str, end: str) -> List[str]:
        """
        Get all nodes reachable from start before reaching end.

        Uses BFS to find all nodes that could be traversed in any execution
        path from start to end. This is used for interrupt validation in
        remote execution scope.

        Args:
            start: Starting node
            end: Target node (not included in result)

        Returns:
            List of node names between start and end (excluding end)
        """
        visited = set()
        result = []
        queue = [start]

        while queue:
            current = queue.pop(0)

            # Stop at end node (don't include it)
            if current == end or current in visited:
                continue

            visited.add(current)
            result.append(current)

            # Get all successors (including conditional edges)
            if current in self.graph.nodes:
                for successor in self.graph.successors(current):
                    if successor not in visited:
                        queue.append(successor)

        return result

    def validate_remote_scope(
        self,
        entry_point: str,
        exit_point: str,
    ) -> List[str]:
        """
        Validate that remote execution scope doesn't contain interrupt points.

        Interrupt points require human interaction, which can't happen on
        a remote host that's executing a branch. This method checks for
        interrupt_before and interrupt_after flags on nodes within the scope.

        Args:
            entry_point: Starting node of remote scope
            exit_point: Ending node of remote scope (typically fan-in)

        Returns:
            List of error messages (empty if valid)

        Example:
            >>> graph.compile(interrupt_before=["human_review"])
            >>> errors = graph.validate_remote_scope("branch_a", "merge")
            >>> if errors:
            ...     raise ValueError("\\n".join(errors))

        Note:
            Call this method before starting remote execution to ensure
            no interrupt points will cause indefinite hangs on remote hosts.
        """
        errors = []

        # Get all nodes in the execution path
        nodes_in_scope = self._get_nodes_between(entry_point, exit_point)

        for node_name in nodes_in_scope:
            # Check interrupt_before
            if node_name in self.interrupt_before:
                errors.append(
                    f"Node '{node_name}' has interrupt_before=True, which is not "
                    f"supported in remote execution scope. Move interrupt points "
                    f"before the parallel fan-out."
                )

            # Check interrupt_after
            if node_name in self.interrupt_after:
                errors.append(
                    f"Node '{node_name}' has interrupt_after=True, which is not "
                    f"supported in remote execution scope. Move interrupt points "
                    f"after the parallel fan-in."
                )

        return errors

    def _run_scoped(
        self,
        state: Dict[str, Any],
        start_node: str,
        stop_before: str,
        config: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute graph from start_node, stopping before stop_before.

        This is a simplified execution path for scoped runs that doesn't
        support parallel edges (which would require the full invoke machinery).

        Args:
            state: Current state dictionary
            start_node: Node to begin execution at
            stop_before: Node to stop before (not executed)
            config: Execution configuration

        Returns:
            Final state after execution
        """
        current_node = start_node

        self.logger.debug(
            f"Scoped execution: {start_node} → (stop before {stop_before})"
        )

        while current_node != stop_before and current_node != END:
            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                self.logger.debug(f"[Scoped] Entering node: {current_node}")
                if self.log_state_values:
                    self.logger.debug(
                        f"[Scoped] Node '{current_node}' input state: {state}"
                    )
                try:
                    result = self._execute_node_function(
                        run_func, state, config, current_node
                    )
                    state.update(result)
                    self.logger.info(f"[Scoped] Node '{current_node}' completed")
                    if self.log_state_values:
                        self.logger.debug(
                            f"[Scoped] Node '{current_node}' output state: {state}"
                        )
                except Exception as e:
                    self.logger.error(f"[Scoped] Error in node '{current_node}': {e}")
                    if self.raise_exceptions:
                        raise RuntimeError(
                            f"Error in node '{current_node}': {str(e)}"
                        ) from e
                    else:
                        # Return state with error info for caller to handle
                        state["_scoped_error"] = {
                            "node": current_node,
                            "error": str(e),
                        }
                        return state

            # Get next node
            next_node = self._get_next_node(current_node, state, config)

            if next_node is None:
                error_msg = f"No valid next node found from '{current_node}' in scoped execution"
                self.logger.warning(f"[Scoped] {error_msg}")
                if self.raise_exceptions:
                    raise RuntimeError(error_msg)
                else:
                    state["_scoped_error"] = {
                        "node": current_node,
                        "error": error_msg,
                    }
                    return state

            self.logger.debug(
                f"[Scoped] Transitioning from '{current_node}' to '{next_node}'"
            )
            current_node = next_node

        self.logger.info(f"Scoped execution complete: stopped before '{stop_before}'")
        return state

    def node(self, node_name: str) -> Dict[str, Any]:
        """
        Get the attributes of a specific node.

        Args:
            node_name (str): The name of the node.

        Returns:
            Dict[str, Any]: The node's attributes.

        Raises:
            KeyError: If the node is not found in the graph.
        """
        if node_name not in self.graph.nodes:
            raise KeyError(f"Node '{node_name}' not found in the graph")
        return self.graph.nodes[node_name]

    def edge(self, in_node: str, out_node: str) -> Dict[str, Any]:
        """
        Get the attributes of a specific edge.

        Args:
            in_node (str): The source node.
            out_node (str): The target node.

        Returns:
            Dict[str, Any]: The edge's attributes.

        Raises:
            KeyError: If the edge is not found in the graph.
        """
        if not self.graph.has_edge(in_node, out_node):
            raise KeyError(
                f"Edge from '{in_node}' to '{out_node}' not found in the graph"
            )
        return self.graph.edges[in_node, out_node]

    def successors(self, node: str) -> List[str]:
        """
        Get the list of successors for a given node.

        Args:
            node (str): The name of the node.

        Returns:
            List[str]: A list of successor node names.

        Raises:
            KeyError: If the node is not found in the graph.
        """
        if node not in self.graph.nodes:
            raise KeyError(f"Node '{node}' not found in the graph")
        return list(self.graph.successors(node))

    def stream(
        self,
        input_state: Optional[Dict[str, Any]] = None,
        config: Optional[Dict[str, Any]] = None,
        checkpoint: Optional[str] = None,
    ) -> Generator[Dict[str, Any], None, None]:
        """
        Execute the graph, yielding results at each node execution, including interrupts.

        Supports parallel execution with intermediate state streaming from all branches.

        Args:
            input_state (Optional[Dict[str, Any]]): The initial state.
            config (Optional[Dict[str, Any]]): Configuration for the execution.
            checkpoint (Optional[str]): Path to a checkpoint file to resume from.
                If provided, execution resumes BY RE-EXECUTING the saved node
                with the saved state. input_state is ignored when checkpoint is provided.

        Yields:
            Dict[str, Any]: Events during execution. Possible types:
                - {"type": "interrupt_before", "node": str, "state": dict}: Interrupt before node
                - {"type": "interrupt_after", "node": str, "state": dict}: Interrupt after node
                - {"type": "state", "node": str, "state": dict}: Intermediate state after node execution
                - {"type": "parallel_state", "branch": str, "node": str, "state": dict}: State from parallel branch
                - {"type": "parallel_error", "branch": str, "node": str, "error": str, "state": dict}: Error in parallel branch
                - {"type": "error", "node": str, "error": str, "state": dict}: Error occurred
                - {"type": "final", "state": dict}: Execution completed successfully

        Raises:
            RuntimeError: If raise_exceptions=True and an error occurs in any node.
            FileNotFoundError: If checkpoint file doesn't exist.
            ValueError: If checkpoint file is corrupt or incompatible.

        Error Handling:
            When raise_exceptions=False (default):
                - Errors yield {"type": "error", "node": <node>, "error": <msg>, "state": <state>}
                - Parallel errors yield {"type": "parallel_error", ...} but don't stop other branches
                - Execution stops after yielding a main-flow error
            When raise_exceptions=True:
                - Errors raise RuntimeError with message: "Error in node '<node>': <msg>"

        Note:
            Parallel branch events may interleave non-deterministically. The order of
            parallel_state events from different branches is not guaranteed.

        Example:
            >>> # First stream: runs until interrupt
            >>> events = list(graph.stream({"x": 1}))
            >>> checkpoint_path = events[-1]["checkpoint_path"]
            >>> # Resume from checkpoint
            >>> events = list(graph.stream(None, checkpoint=checkpoint_path))
        """
        # Resume case: input_state is None signals resume
        if input_state is None:
            if checkpoint is None:
                raise ValueError(
                    "checkpoint parameter required when input_state is None. "
                    "Use stream(None, checkpoint='...') to resume from an interrupt."
                )
            yield from self._stream_from_checkpoint(checkpoint, config)
            return

        # Legacy support: checkpoint provided with input_state (checkpoint takes precedence)
        if checkpoint is not None:
            # If input_state is provided with checkpoint, treat it as a state update
            yield from self._stream_from_checkpoint(
                checkpoint, config, state_update=input_state
            )
            return

        if config is None:
            config = {}

        current_node = START
        state = input_state.copy()
        config = config.copy()

        # TEA-BUILTIN-015.1: State injection - load session data if session_id is present
        state = self._maybe_inject_session_state(state)

        # Log execution start
        self.logger.debug(
            f"Starting stream execution with state keys: {list(state.keys())}"
        )
        if self.log_state_values:
            self.logger.debug(f"Initial state: {state}")

        # TEA-PARALLEL-001.1: Use executor abstraction for configurable parallel strategy
        max_workers = config.get("max_workers", self.max_workers)
        strategy = self.parallel_config.strategy
        with get_executor(strategy, max_workers=max_workers) as executor:
            # Queue for collecting events from parallel flows
            result_queue: Queue = Queue()
            # Track active branches per fan-in node
            active_branches: Dict[str, set] = {}
            # Track completed branch states per fan-in node
            branch_states: Dict[str, List[Dict[str, Any]]] = {}
            # Lock for thread-safe operations
            stream_lock = threading.Lock()

            while current_node != END:
                # Check for interrupt before
                if current_node in self.interrupt_before:
                    checkpoint_path = self._auto_save_checkpoint(
                        state, current_node, config
                    )
                    yield {
                        "type": "interrupt_before",
                        "node": current_node,
                        "state": state.copy(),
                        "checkpoint_path": checkpoint_path,
                    }
                    return  # STOP execution - must resume via stream(None, checkpoint=...)

                # Get node data
                node_data = self.node(current_node)
                run_func = node_data.get("run")

                # Execute node's run function if present
                if run_func:
                    self.logger.debug(f"Entering node: {current_node}")
                    if self.log_state_values:
                        self.logger.debug(f"Node '{current_node}' input state: {state}")
                    try:
                        result = self._execute_node_function(
                            run_func, state, config, current_node
                        )
                        state.update(result)
                        self.logger.info(
                            f"Node '{current_node}' completed successfully"
                        )
                        if self.log_state_values:
                            self.logger.debug(
                                f"Node '{current_node}' output state: {state}"
                            )
                        # Yield intermediate state after execution
                        yield {
                            "type": "state",
                            "node": current_node,
                            "state": state.copy(),
                        }
                    except Exception as e:
                        self.logger.error(f"Error in node '{current_node}': {e}")
                        if self.raise_exceptions:
                            raise RuntimeError(
                                f"Error in node '{current_node}': {str(e)}"
                            ) from e
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": str(e),
                                "state": state.copy(),
                            }
                            return

                # Check for interrupt after
                if current_node in self.interrupt_after:
                    checkpoint_path = self._auto_save_checkpoint(
                        state, current_node, config
                    )
                    yield {
                        "type": "interrupt_after",
                        "node": current_node,
                        "state": state.copy(),
                        "checkpoint_path": checkpoint_path,
                    }
                    return  # STOP execution - must resume via stream(None, checkpoint=...)

                # Determine next node - check for parallel edges
                successors = self.successors(current_node)

                # Separate parallel and normal edges
                parallel_edges = []
                normal_successors = []

                for successor in successors:
                    edge_data = self.edge(current_node, successor)
                    if edge_data.get("parallel", False):
                        fan_in_node = edge_data.get("fan_in_node", None)
                        if fan_in_node is None:
                            error_msg = f"Parallel edge from '{current_node}' to '{successor}' must have 'fan_in_node' specified"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {
                                    "type": "error",
                                    "node": current_node,
                                    "error": error_msg,
                                    "state": state.copy(),
                                }
                                return
                        parallel_edges.append((successor, fan_in_node))
                    else:
                        normal_successors.append(successor)

                # Start parallel flows
                if parallel_edges:
                    self.logger.info(
                        f"Starting {len(parallel_edges)} parallel flow(s) from node '{current_node}'"
                    )
                    for successor, fan_in_node in parallel_edges:
                        # Get per-edge config (TD.13)
                        edge_data = self.edge(current_node, successor)
                        edge_config = self._get_parallel_config_for_edge(edge_data)

                        self.logger.debug(
                            f"Launching parallel stream flow to node '{successor}' (fan-in: '{fan_in_node}')"
                        )
                        # Register branch with fan-in node
                        with stream_lock:
                            if fan_in_node not in active_branches:
                                active_branches[fan_in_node] = set()
                                branch_states[fan_in_node] = []
                            active_branches[fan_in_node].add(successor)
                        # Submit thread with reliability wrapper (TD.13)
                        start_time = time.time()
                        executor.submit(
                            self._stream_parallel_flow_with_reliability,
                            successor,
                            copy.deepcopy(state),
                            config.copy(),
                            fan_in_node,
                            result_queue,
                            edge_config,
                            start_time,
                        )

                # Handle normal successors
                if normal_successors:
                    # Find first valid normal successor
                    next_node = None
                    for successor in normal_successors:
                        edge_data = self.edge(current_node, successor)
                        cond_func = edge_data.get("cond", lambda **kwargs: True)
                        cond_map = edge_data.get("cond_map", None)
                        available_params = {
                            "state": state,
                            "config": config,
                            "node": current_node,
                            "graph": self,
                        }
                        cond_params = self._prepare_function_params(
                            cond_func, available_params
                        )
                        cond_result = cond_func(**cond_params)
                        self.logger.debug(
                            f"Edge '{current_node}' -> '{successor}': condition result = {cond_result}"
                        )

                        if cond_map:
                            next_node_candidate = cond_map.get(cond_result, None)
                            if next_node_candidate:
                                next_node = next_node_candidate
                                self.logger.debug(
                                    f"Transitioning from '{current_node}' to '{next_node}'"
                                )
                                break
                        else:
                            if cond_result:
                                next_node = successor
                                self.logger.debug(
                                    f"Transitioning from '{current_node}' to '{next_node}'"
                                )
                                break

                    if next_node:
                        current_node = next_node
                    else:
                        self.logger.warning(
                            f"No valid next node found from node '{current_node}'"
                        )
                        error_msg = (
                            f"No valid next node found from node '{current_node}'"
                        )
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": error_msg,
                                "state": state.copy(),
                            }
                            return
                else:
                    # No normal successors - check for pending parallel flows
                    with stream_lock:
                        pending_fan_ins = [
                            fn for fn, branches in active_branches.items() if branches
                        ]

                    if pending_fan_ins:
                        # Wait for parallel flows and yield their events
                        fan_in_node = pending_fan_ins[0]
                        self.logger.info(
                            f"Waiting for parallel flows to complete at fan-in '{fan_in_node}'"
                        )

                        # Drain queue until all branches complete
                        while True:
                            with stream_lock:
                                remaining = len(active_branches.get(fan_in_node, set()))
                            if remaining == 0:
                                break

                            try:
                                event = result_queue.get(timeout=0.1)
                                event_type = event.get("type")
                                event_branch = event.get("branch")

                                if event_type == "parallel_state":
                                    yield event
                                elif event_type == "parallel_error":
                                    yield event
                                elif event_type == "branch_complete":
                                    with stream_lock:
                                        if event_branch in active_branches.get(
                                            fan_in_node, set()
                                        ):
                                            active_branches[fan_in_node].discard(
                                                event_branch
                                            )
                                            # Only collect state if no error
                                            if not event.get("error"):
                                                branch_states[fan_in_node].append(
                                                    event.get("state", {})
                                                )
                                            self.logger.debug(
                                                f"Branch '{event_branch}' completed, {len(active_branches[fan_in_node])} remaining"
                                            )
                            except Empty:
                                continue

                        # All branches complete - execute fan-in node
                        self.logger.info(
                            f"All parallel flows joined at fan-in node '{fan_in_node}'"
                        )
                        with stream_lock:
                            parallel_results = branch_states.pop(fan_in_node, [])
                            active_branches.pop(fan_in_node, None)

                        state["parallel_results"] = parallel_results
                        current_node = fan_in_node

                        # Execute the fan-in node
                        node_data = self.node(current_node)
                        run_func = node_data.get("run")

                        if current_node in self.interrupt_before:
                            checkpoint_path = self._auto_save_checkpoint(
                                state, current_node, config
                            )
                            yield {
                                "type": "interrupt_before",
                                "node": current_node,
                                "state": state.copy(),
                                "checkpoint_path": checkpoint_path,
                            }
                            return  # STOP execution - must resume via stream(None, checkpoint=...)

                        if run_func:
                            self.logger.debug(f"Entering fan-in node: {current_node}")
                            try:
                                result = self._execute_node_function(
                                    run_func, state, config, current_node
                                )
                                state.update(result)
                                self.logger.info(
                                    f"Fan-in node '{current_node}' completed successfully"
                                )
                                yield {
                                    "type": "state",
                                    "node": current_node,
                                    "state": state.copy(),
                                }
                            except Exception as e:
                                self.logger.error(
                                    f"Error in fan-in node '{current_node}': {e}"
                                )
                                if self.raise_exceptions:
                                    raise RuntimeError(
                                        f"Error in node '{current_node}': {str(e)}"
                                    ) from e
                                else:
                                    yield {
                                        "type": "error",
                                        "node": current_node,
                                        "error": str(e),
                                        "state": state.copy(),
                                    }
                                    return

                        if current_node in self.interrupt_after:
                            checkpoint_path = self._auto_save_checkpoint(
                                state, current_node, config
                            )
                            yield {
                                "type": "interrupt_after",
                                "node": current_node,
                                "state": state.copy(),
                                "checkpoint_path": checkpoint_path,
                            }
                            return  # STOP execution - must resume via stream(None, checkpoint=...)

                        # Continue to next node after fan-in
                        next_node = self._get_next_node(current_node, state, config)
                        if not next_node:
                            self.logger.warning(
                                f"No valid next node found from fan-in node '{current_node}'"
                            )
                            error_msg = (
                                f"No valid next node found from node '{current_node}'"
                            )
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {
                                    "type": "error",
                                    "node": current_node,
                                    "error": error_msg,
                                    "state": state.copy(),
                                }
                                return
                        current_node = next_node
                    else:
                        error_msg = (
                            f"No valid next node found from node '{current_node}'"
                        )
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {
                                "type": "error",
                                "node": current_node,
                                "error": error_msg,
                                "state": state.copy(),
                            }
                            return

        # Once END is reached, yield final state
        self.logger.info("Stream execution completed successfully")
        self.logger.debug(f"Final state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Final state: {state}")

        # TEA-BUILTIN-015.1: Auto-save session if configured
        self._maybe_auto_save_session(state)

        yield {"type": "final", "state": state.copy()}

    def _maybe_inject_session_state(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Inject session data into state if session_id is present.

        TEA-BUILTIN-015.1 AC8: When `session_id` is in initial state,
        session data is loaded and merged into state before execution.

        The session data is merged under state (not replacing existing keys
        unless they're from a previous session). The session_id is preserved.

        Args:
            state: Initial state that may contain session_id.

        Returns:
            State with session data merged in (if available).
        """
        # Check if session backend is attached by YAMLEngine
        session_backend = getattr(self, "_session_backend", None)
        if session_backend is None:
            return state

        # Check if session_id is in state
        session_id = state.get("session_id")
        if not session_id:
            return state

        # Load session data
        try:
            session_data = session_backend.load(session_id)
            if session_data is None:
                self.logger.debug(f"No session data found for {session_id}")
                return state

            # Merge session data into state (session data has lower priority)
            # This means explicit initial state values take precedence
            merged = session_data.copy()
            merged.update(state)  # Initial state overrides session data

            self.logger.debug(f"Injected session data for {session_id}")
            return merged

        except Exception as e:
            self.logger.warning(f"Failed to inject session state for {session_id}: {e}")
            return state

    def _maybe_auto_save_session(self, state: Dict[str, Any]) -> None:
        """
        Auto-save session state if auto_save is enabled in session settings.

        TEA-BUILTIN-015.1: This method is called after graph execution completes.
        It checks if session backend and settings are attached to the graph
        (by YAMLEngine) and if auto_save is enabled.

        Args:
            state: Final state to save to session.
        """
        # Check if session backend and settings are attached by YAMLEngine
        session_backend = getattr(self, "_session_backend", None)
        session_settings = getattr(self, "_session_settings", None)

        if session_backend is None or session_settings is None:
            return

        # Check if auto_save is enabled
        if not getattr(session_settings, "auto_save", False):
            return

        # Get session_id from state
        session_id = state.get("session_id")
        if not session_id:
            self.logger.debug("Auto-save skipped: no session_id in state")
            return

        # Determine which fields to persist
        persist_fields = getattr(session_settings, "persist_fields", None)
        if persist_fields is not None:
            # Save only specified fields
            data = {k: state.get(k) for k in persist_fields if k in state}
        else:
            # Save entire state, excluding internal fields
            data = {k: v for k, v in state.items() if not k.startswith("_")}

        # Get TTL from settings
        ttl = getattr(session_settings, "ttl", 0)
        effective_ttl = ttl if ttl > 0 else None

        # Save to backend
        try:
            success = session_backend.save(session_id, data, ttl=effective_ttl)
            if success:
                self.logger.debug(f"Auto-saved session {session_id}")
            else:
                self.logger.warning(f"Failed to auto-save session {session_id}")
        except Exception as e:
            self.logger.warning(f"Error during auto-save for session {session_id}: {e}")

    def _prepare_function_params(
        self, func: Callable[..., Any], available_params: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Prepare the parameters for a node function based on its signature.

        Args:
            func (Callable[..., Any]): The function to prepare parameters for.
            available_params (Dict[str, Any]): Dictionary of available parameters.

        Returns:
            Dict[str, Any]: The prepared parameters for the function.

        Raises:
            ValueError: If required parameters for the function are not provided.
        """
        sig = inspect.signature(func)
        function_params = {}

        if len(sig.parameters) == 0:
            return {}

        for param_name, param in sig.parameters.items():
            if param_name in available_params:
                function_params[param_name] = available_params[param_name]
            elif param.default is not inspect.Parameter.empty:
                function_params[param_name] = param.default
            elif param.kind == inspect.Parameter.VAR_KEYWORD:
                function_params.update(
                    {
                        k: v
                        for k, v in available_params.items()
                        if k not in function_params
                    }
                )
                break
            else:
                raise ValueError(
                    f"Required parameter '{param_name}' not provided for function '{func.__name__}'"
                )

        return function_params

    def _get_next_node(
        self, current_node: str, state: Dict[str, Any], config: Dict[str, Any]
    ) -> Optional[str]:
        """
        Determine the next node based on the current node's successors and conditions.

        Args:
            current_node (str): The current node.
            state (Dict[str, Any]): The current state.
            config (Dict[str, Any]): The configuration.

        Returns:
            Optional[str]: The name of the next node, or None if no valid next node is found.
        """
        successors = self.successors(current_node)

        for successor in successors:
            edge_data = self.edge(current_node, successor)
            cond_func = edge_data.get("cond", lambda **kwargs: True)
            cond_map = edge_data.get("cond_map", None)
            available_params = {
                "state": state,
                "config": config,
                "node": current_node,
                "graph": self,
            }
            cond_params = self._prepare_function_params(cond_func, available_params)
            cond_result = cond_func(**cond_params)

            if cond_map:
                # cond_map is a mapping from condition results to nodes
                next_node = cond_map.get(cond_result, None)
                if next_node:
                    return next_node
            else:
                # cond_result is treated as boolean
                if cond_result:
                    return successor

        # No valid next node found
        return None

    # --- Circuit Breaker Management APIs (TECH-002 mitigation) ---

    def reset_circuit(self, branch: Optional[str] = None) -> None:
        """
        Reset a circuit breaker or all circuit breakers.

        Args:
            branch: Branch/circuit name to reset, or None to reset all.

        Example:
            >>> graph.reset_circuit("slow_api")  # Reset specific circuit
            >>> graph.reset_circuit()  # Reset all circuits
        """
        self.circuit_registry.reset_circuit(branch)

    def reset_all_circuits(self) -> None:
        """Reset all circuit breakers to CLOSED state."""
        self.circuit_registry.reset_all_circuits()

    def get_circuit_states(self) -> Dict[str, Dict[str, Any]]:
        """
        Get the state of all circuit breakers.

        Returns:
            Dict mapping circuit names to their state info, including:
            - name: Circuit identifier
            - state: Current state (closed/open/half_open)
            - failure_count: Current failure count
            - last_failure_time: Timestamp of last failure
            - half_open_calls: Number of test calls in half-open state

        Example:
            >>> states = graph.get_circuit_states()
            >>> for name, info in states.items():
            ...     print(f"{name}: {info['state']}")
        """
        return self.circuit_registry.get_circuit_states()

    def _get_parallel_config_for_edge(
        self, edge_data: Dict[str, Any]
    ) -> ParallelConfig:
        """
        Get the effective ParallelConfig for an edge.

        Per-edge config takes precedence over graph-level config.

        Args:
            edge_data: Edge attributes from the graph

        Returns:
            ParallelConfig to use for this edge
        """
        edge_config = edge_data.get("parallel_config")
        if edge_config is not None:
            return edge_config
        return self.parallel_config
