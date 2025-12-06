import inspect
import logging
from typing import Any, Callable, Dict, List, Optional, Union, Generator
import networkx as nx
from concurrent.futures import ThreadPoolExecutor, Future
import threading
import copy
from queue import Queue, Empty

from the_edge_agent.checkpoint import CheckpointMixin
from the_edge_agent.visualization import VisualizationMixin

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

    def __init__(self, state_schema: Dict[str, Any], raise_exceptions: bool = False, max_workers: Optional[int] = None, log_level: int = logging.WARNING, log_state_values: bool = False):
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
        self.graph.add_edge(in_node, out_node, cond=lambda **kwargs: True, cond_map={True: out_node})

    def add_conditional_edges(self, in_node: str, func: Callable[..., Any], cond: Dict[Any, str]) -> None:
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
                raise ValueError(f"Target node '{out_node}' does not exist in the graph.")
            self.graph.add_edge(in_node, out_node, cond=func, cond_map=cond)

    def add_parallel_edge(self, in_node: str, out_node: str, fan_in_node: str) -> None:
        """
        Add an unconditional parallel edge between two nodes.

        Args:
            in_node (str): The source node.
            out_node (str): The target node.
            fan_in_node (str): The fan-in node that this parallel flow will reach.

        Raises:
            ValueError: If either node doesn't exist in the graph.
        """
        if in_node not in self.graph.nodes or out_node not in self.graph.nodes or fan_in_node not in self.graph.nodes:
            raise ValueError("All nodes must exist in the graph.")
        self.graph.add_edge(in_node, out_node, cond=lambda **kwargs: True, cond_map={True: out_node}, parallel=True, fan_in_node=fan_in_node)

    def add_fanin_node(self, node: str, run: Optional[Callable[..., Any]] = None) -> None:
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

    def invoke(self, input_state: Optional[Dict[str, Any]] = None, config: Optional[Dict[str, Any]] = None, checkpoint: Optional[str] = None) -> Generator[Dict[str, Any], None, None]:
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
            yield from self.resume_from_checkpoint(checkpoint, config, state_update=input_state)
            return

        if config is None:
            config = {}

        current_node = START
        state = input_state.copy()
        config = config.copy()

        # Log execution start
        self.logger.debug(f"Starting execution with state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Initial state: {state}")

        # Create a ThreadPoolExecutor for parallel flows
        # Allow runtime override via config, fallback to instance default
        max_workers = config.get('max_workers', self.max_workers)
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Mapping from fan-in nodes to list of futures
            fanin_futures: Dict[str, List[Future]] = {}
            # Lock for thread-safe operations on fanin_futures
            fanin_lock = threading.Lock()

            while current_node != END:
                # Check for interrupt before
                if current_node in self.interrupt_before:
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
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
                        result = self._execute_node_function(run_func, state, config, current_node)
                        state.update(result)
                        self.logger.info(f"Node '{current_node}' completed successfully")
                        if self.log_state_values:
                            self.logger.debug(f"Node '{current_node}' output state: {state}")
                    except Exception as e:
                        self.logger.error(f"Error in node '{current_node}': {e}")
                        if self.raise_exceptions:
                            raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                        else:
                            yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                            return

                # Check for interrupt after
                if current_node in self.interrupt_after:
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                    return  # STOP execution - must resume via invoke(None, checkpoint=...)

                # Determine next node
                successors = self.successors(current_node)

                # Separate parallel and normal edges
                parallel_edges = []
                normal_successors = []

                for successor in successors:
                    edge_data = self.edge(current_node, successor)
                    if edge_data.get('parallel', False):
                        fan_in_node = edge_data.get('fan_in_node', None)
                        if fan_in_node is None:
                            error_msg = f"Parallel edge from '{current_node}' to '{successor}' must have 'fan_in_node' specified"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                                return
                        parallel_edges.append((successor, fan_in_node))
                    else:
                        normal_successors.append(successor)

                # Start parallel flows
                if parallel_edges:
                    self.logger.info(f"Starting {len(parallel_edges)} parallel flow(s) from node '{current_node}'")
                for successor, fan_in_node in parallel_edges:
                    # Start a new thread for the flow starting from successor
                    self.logger.debug(f"Launching parallel flow to node '{successor}' (fan-in: '{fan_in_node}')")
                    future = executor.submit(self._execute_flow, successor, copy.deepcopy(state), config.copy(), fan_in_node)
                    # Register the future with the corresponding fan-in node
                    with fanin_lock:
                        if fan_in_node not in fanin_futures:
                            fanin_futures[fan_in_node] = []
                        fanin_futures[fan_in_node].append(future)

                # Handle normal successors
                if normal_successors:
                    # For simplicity, take the first valid normal successor
                    next_node = None
                    for successor in normal_successors:
                        edge_data = self.edge(current_node, successor)
                        cond_func = edge_data.get("cond", lambda **kwargs: True)
                        cond_map = edge_data.get("cond_map", None)
                        available_params = {"state": state, "config": config, "node": current_node, "graph": self}
                        cond_params = self._prepare_function_params(cond_func, available_params)
                        cond_result = cond_func(**cond_params)
                        self.logger.debug(f"Edge '{current_node}' -> '{successor}': condition result = {cond_result}")

                        if cond_map:
                            next_node_candidate = cond_map.get(cond_result, None)
                            if next_node_candidate:
                                next_node = next_node_candidate
                                self.logger.debug(f"Transitioning from '{current_node}' to '{next_node}'")
                                break
                        else:
                            if cond_result:
                                next_node = successor
                                self.logger.debug(f"Transitioning from '{current_node}' to '{next_node}'")
                                break
                    if next_node:
                        current_node = next_node
                    else:
                        self.logger.warning(f"No valid next node found from node '{current_node}'")
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
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
                        self.logger.info(f"Joining {len(futures)} parallel flow(s) at fan-in node '{current_node}'")
                        results = [future.result() for future in futures]
                        self.logger.debug(f"All parallel flows joined at '{current_node}'")
                        # Check for errors in parallel flow results
                        for result in results:
                            if isinstance(result, dict) and result.get("type") == "error":
                                # Yield the error from the parallel flow
                                self.logger.error(f"Error from parallel flow: {result.get('error')}")
                                yield result
                                return
                        # Collect the results in the state
                        state['parallel_results'] = results

                        # Check for interrupt before fan-in execution
                        if current_node in self.interrupt_before:
                            checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                            return  # STOP execution - must resume via invoke(None, checkpoint=...)

                        # Execute the fan-in node's run function
                        node_data = self.node(current_node)
                        run_func = node_data.get("run")
                        if run_func:
                            self.logger.debug(f"Entering fan-in node: {current_node}")
                            try:
                                result = self._execute_node_function(run_func, state, config, current_node)
                                state.update(result)
                                self.logger.info(f"Fan-in node '{current_node}' completed successfully")
                            except Exception as e:
                                self.logger.error(f"Error in fan-in node '{current_node}': {e}")
                                if self.raise_exceptions:
                                    raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                                else:
                                    yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                                    return

                        # Check for interrupt after fan-in execution
                        if current_node in self.interrupt_after:
                            checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                            return  # STOP execution - must resume via invoke(None, checkpoint=...)

                        # Continue to next node
                        next_node = self._get_next_node(current_node, state, config)
                        if not next_node:
                            self.logger.warning(f"No valid next node found from fan-in node '{current_node}'")
                            error_msg = f"No valid next node found from node '{current_node}'"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                                return
                        current_node = next_node
                    else:
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return
        # Once END is reached, yield final state
        self.logger.info("Execution completed successfully")
        self.logger.debug(f"Final state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Final state: {state}")
        yield {"type": "final", "state": state.copy()}


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
        self.logger.info(f"Parallel flow started at node '{current_node}' (target fan-in: '{fan_in_node}')")
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
                    self.logger.debug(f"[Parallel] Node '{current_node}' input state: {state}")
                try:
                    result = self._execute_node_function(run_func, state, config, current_node)
                    state.update(result)
                    self.logger.debug(f"[Parallel] Node '{current_node}' completed")
                    if self.log_state_values:
                        self.logger.debug(f"[Parallel] Node '{current_node}' output state: {state}")
                except Exception as e:
                    self.logger.error(f"[Parallel] Error in node '{current_node}': {e}")
                    if self.raise_exceptions:
                        raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                    else:
                        # Return consistent error dict structure
                        return {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}

            # Determine next node using _get_next_node
            try:
                next_node = self._get_next_node(current_node, state, config)
            except Exception as e:
                self.logger.error(f"[Parallel] Error getting next node from '{current_node}': {e}")
                if self.raise_exceptions:
                    raise
                else:
                    return {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}

            if next_node:
                self.logger.debug(f"[Parallel] Transitioning from '{current_node}' to '{next_node}'")
                current_node = next_node
            else:
                error_msg = f"No valid next node found from node '{current_node}' in parallel flow"
                self.logger.warning(f"[Parallel] {error_msg}")
                if self.raise_exceptions:
                    raise RuntimeError(error_msg)
                else:
                    return {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}

        # Reached END
        self.logger.debug(f"[Parallel] Flow reached END")
        return state

    def _stream_parallel_flow(self, start_node: str, state: Dict[str, Any], config: Dict[str, Any], fan_in_node: str, result_queue: Queue) -> None:
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

        self.logger.info(f"[Parallel Stream] Flow started at node '{start_node}' (target fan-in: '{fan_in_node}')")

        while current_node != END:
            # Check if current node is the fan-in node
            if current_node == fan_in_node:
                self.logger.info(f"[Parallel Stream] Flow '{start_node}' reached fan-in node '{fan_in_node}'")
                result_queue.put({
                    "type": "branch_complete",
                    "branch": start_node,
                    "state": flow_state.copy()
                })
                return

            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                self.logger.debug(f"[Parallel Stream] Entering node: {current_node}")
                if self.log_state_values:
                    self.logger.debug(f"[Parallel Stream] Node '{current_node}' input state: {flow_state}")
                try:
                    result = self._execute_node_function(run_func, flow_state, config, current_node)
                    flow_state.update(result)
                    self.logger.debug(f"[Parallel Stream] Node '{current_node}' completed")
                    if self.log_state_values:
                        self.logger.debug(f"[Parallel Stream] Node '{current_node}' output state: {flow_state}")
                    # Put state event into queue
                    result_queue.put({
                        "type": "parallel_state",
                        "branch": start_node,
                        "node": current_node,
                        "state": flow_state.copy()
                    })
                except Exception as e:
                    self.logger.error(f"[Parallel Stream] Error in node '{current_node}': {e}")
                    result_queue.put({
                        "type": "parallel_error",
                        "branch": start_node,
                        "node": current_node,
                        "error": str(e),
                        "state": flow_state.copy()
                    })
                    # Signal branch completion with error state
                    result_queue.put({
                        "type": "branch_complete",
                        "branch": start_node,
                        "state": flow_state.copy(),
                        "error": True
                    })
                    return

            # Determine next node
            try:
                next_node = self._get_next_node(current_node, flow_state, config)
            except Exception as e:
                self.logger.error(f"[Parallel Stream] Error getting next node from '{current_node}': {e}")
                result_queue.put({
                    "type": "parallel_error",
                    "branch": start_node,
                    "node": current_node,
                    "error": str(e),
                    "state": flow_state.copy()
                })
                result_queue.put({
                    "type": "branch_complete",
                    "branch": start_node,
                    "state": flow_state.copy(),
                    "error": True
                })
                return

            if next_node:
                self.logger.debug(f"[Parallel Stream] Transitioning from '{current_node}' to '{next_node}'")
                current_node = next_node
            else:
                error_msg = f"No valid next node found from node '{current_node}' in parallel flow"
                self.logger.warning(f"[Parallel Stream] {error_msg}")
                result_queue.put({
                    "type": "parallel_error",
                    "branch": start_node,
                    "node": current_node,
                    "error": error_msg,
                    "state": flow_state.copy()
                })
                result_queue.put({
                    "type": "branch_complete",
                    "branch": start_node,
                    "state": flow_state.copy(),
                    "error": True
                })
                return

        # Reached END (should not normally happen for parallel flows)
        self.logger.debug(f"[Parallel Stream] Flow '{start_node}' reached END")
        result_queue.put({
            "type": "branch_complete",
            "branch": start_node,
            "state": flow_state.copy()
        })

    def _execute_node_function(self, func: Callable[..., Any], state: Dict[str, Any], config: Dict[str, Any], node: str) -> Dict[str, Any]:
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
        available_params = {"state": state, "config": config, "node": node, "graph": self}
        if 'parallel_results' in state:
            available_params['parallel_results'] = state['parallel_results']
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
        self.graph.add_edge(START, init_state, cond=lambda **kwargs: True, cond_map={True: init_state})

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
        self.graph.add_edge(final_state, END, cond=lambda **kwargs: True, cond_map={True: END})

    def compile(self, interrupt_before: List[str] = [], interrupt_after: List[str] = [],
                checkpoint_dir: Optional[str] = None, checkpointer: Optional[Any] = None) -> 'StateGraph':
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
            >>> from the_edge_agent import MemoryCheckpointer
            >>> checkpointer = MemoryCheckpointer()
            >>> graph.compile(interrupt_before=["node_a"], checkpointer=checkpointer)
            >>> # Or use file-based storage:
            >>> graph.compile(interrupt_before=["node_a"], checkpoint_dir="/tmp/checkpoints")
        """
        for node in interrupt_before + interrupt_after:
            if node not in self.graph.nodes:
                raise ValueError(f"Interrupt node '{node}' does not exist in the graph.")

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
        return self

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
            raise KeyError(f"Edge from '{in_node}' to '{out_node}' not found in the graph")
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

    def stream(self, input_state: Optional[Dict[str, Any]] = None, config: Optional[Dict[str, Any]] = None, checkpoint: Optional[str] = None) -> Generator[Dict[str, Any], None, None]:
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
            yield from self._stream_from_checkpoint(checkpoint, config, state_update=input_state)
            return

        if config is None:
            config = {}

        current_node = START
        state = input_state.copy()
        config = config.copy()

        # Log execution start
        self.logger.debug(f"Starting stream execution with state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Initial state: {state}")

        # Create ThreadPoolExecutor for parallel flows
        max_workers = config.get('max_workers', self.max_workers)
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
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
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt_before", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
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
                        result = self._execute_node_function(run_func, state, config, current_node)
                        state.update(result)
                        self.logger.info(f"Node '{current_node}' completed successfully")
                        if self.log_state_values:
                            self.logger.debug(f"Node '{current_node}' output state: {state}")
                        # Yield intermediate state after execution
                        yield {"type": "state", "node": current_node, "state": state.copy()}
                    except Exception as e:
                        self.logger.error(f"Error in node '{current_node}': {e}")
                        if self.raise_exceptions:
                            raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                        else:
                            yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                            return

                # Check for interrupt after
                if current_node in self.interrupt_after:
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt_after", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                    return  # STOP execution - must resume via stream(None, checkpoint=...)

                # Determine next node - check for parallel edges
                successors = self.successors(current_node)

                # Separate parallel and normal edges
                parallel_edges = []
                normal_successors = []

                for successor in successors:
                    edge_data = self.edge(current_node, successor)
                    if edge_data.get('parallel', False):
                        fan_in_node = edge_data.get('fan_in_node', None)
                        if fan_in_node is None:
                            error_msg = f"Parallel edge from '{current_node}' to '{successor}' must have 'fan_in_node' specified"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                                return
                        parallel_edges.append((successor, fan_in_node))
                    else:
                        normal_successors.append(successor)

                # Start parallel flows
                if parallel_edges:
                    self.logger.info(f"Starting {len(parallel_edges)} parallel flow(s) from node '{current_node}'")
                    for successor, fan_in_node in parallel_edges:
                        self.logger.debug(f"Launching parallel stream flow to node '{successor}' (fan-in: '{fan_in_node}')")
                        # Register branch with fan-in node
                        with stream_lock:
                            if fan_in_node not in active_branches:
                                active_branches[fan_in_node] = set()
                                branch_states[fan_in_node] = []
                            active_branches[fan_in_node].add(successor)
                        # Submit thread
                        executor.submit(
                            self._stream_parallel_flow,
                            successor,
                            copy.deepcopy(state),
                            config.copy(),
                            fan_in_node,
                            result_queue
                        )

                # Handle normal successors
                if normal_successors:
                    # Find first valid normal successor
                    next_node = None
                    for successor in normal_successors:
                        edge_data = self.edge(current_node, successor)
                        cond_func = edge_data.get("cond", lambda **kwargs: True)
                        cond_map = edge_data.get("cond_map", None)
                        available_params = {"state": state, "config": config, "node": current_node, "graph": self}
                        cond_params = self._prepare_function_params(cond_func, available_params)
                        cond_result = cond_func(**cond_params)
                        self.logger.debug(f"Edge '{current_node}' -> '{successor}': condition result = {cond_result}")

                        if cond_map:
                            next_node_candidate = cond_map.get(cond_result, None)
                            if next_node_candidate:
                                next_node = next_node_candidate
                                self.logger.debug(f"Transitioning from '{current_node}' to '{next_node}'")
                                break
                        else:
                            if cond_result:
                                next_node = successor
                                self.logger.debug(f"Transitioning from '{current_node}' to '{next_node}'")
                                break

                    if next_node:
                        current_node = next_node
                    else:
                        self.logger.warning(f"No valid next node found from node '{current_node}'")
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return
                else:
                    # No normal successors - check for pending parallel flows
                    with stream_lock:
                        pending_fan_ins = [fn for fn, branches in active_branches.items() if branches]

                    if pending_fan_ins:
                        # Wait for parallel flows and yield their events
                        fan_in_node = pending_fan_ins[0]
                        self.logger.info(f"Waiting for parallel flows to complete at fan-in '{fan_in_node}'")

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
                                        if event_branch in active_branches.get(fan_in_node, set()):
                                            active_branches[fan_in_node].discard(event_branch)
                                            # Only collect state if no error
                                            if not event.get("error"):
                                                branch_states[fan_in_node].append(event.get("state", {}))
                                            self.logger.debug(f"Branch '{event_branch}' completed, {len(active_branches[fan_in_node])} remaining")
                            except Empty:
                                continue

                        # All branches complete - execute fan-in node
                        self.logger.info(f"All parallel flows joined at fan-in node '{fan_in_node}'")
                        with stream_lock:
                            parallel_results = branch_states.pop(fan_in_node, [])
                            active_branches.pop(fan_in_node, None)

                        state['parallel_results'] = parallel_results
                        current_node = fan_in_node

                        # Execute the fan-in node
                        node_data = self.node(current_node)
                        run_func = node_data.get("run")

                        if current_node in self.interrupt_before:
                            checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt_before", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                            return  # STOP execution - must resume via stream(None, checkpoint=...)

                        if run_func:
                            self.logger.debug(f"Entering fan-in node: {current_node}")
                            try:
                                result = self._execute_node_function(run_func, state, config, current_node)
                                state.update(result)
                                self.logger.info(f"Fan-in node '{current_node}' completed successfully")
                                yield {"type": "state", "node": current_node, "state": state.copy()}
                            except Exception as e:
                                self.logger.error(f"Error in fan-in node '{current_node}': {e}")
                                if self.raise_exceptions:
                                    raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                                else:
                                    yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                                    return

                        if current_node in self.interrupt_after:
                            checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt_after", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                            return  # STOP execution - must resume via stream(None, checkpoint=...)

                        # Continue to next node after fan-in
                        next_node = self._get_next_node(current_node, state, config)
                        if not next_node:
                            self.logger.warning(f"No valid next node found from fan-in node '{current_node}'")
                            error_msg = f"No valid next node found from node '{current_node}'"
                            if self.raise_exceptions:
                                raise RuntimeError(error_msg)
                            else:
                                yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                                return
                        current_node = next_node
                    else:
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return

        # Once END is reached, yield final state
        self.logger.info("Stream execution completed successfully")
        self.logger.debug(f"Final state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Final state: {state}")
        yield {"type": "final", "state": state.copy()}

    def _prepare_function_params(self, func: Callable[..., Any], available_params: Dict[str, Any]) -> Dict[str, Any]:
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
                function_params.update({k: v for k, v in available_params.items() if k not in function_params})
                break
            else:
                raise ValueError(f"Required parameter '{param_name}' not provided for function '{func.__name__}'")

        return function_params

    def _get_next_node(self, current_node: str, state: Dict[str, Any], config: Dict[str, Any]) -> Optional[str]:
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
            available_params = {"state": state, "config": config, "node": current_node, "graph": self}
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

