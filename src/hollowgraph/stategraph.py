import time
import inspect
from typing import Any, Callable, Dict, List, Optional, Union, Generator, Tuple
import networkx as nx
from networkx.drawing.nx_agraph import to_agraph
from concurrent.futures import ThreadPoolExecutor
import threading
import copy
from dataclasses import dataclass
from collections import deque

@dataclass
class DebugConfig:
    """Configuration for debug mode in StateGraph."""
    enabled: bool = False
    max_history_size: int = 1000  # Maximum number of states to store per node
    track_timestamps: bool = True  # Whether to track execution timestamps


# Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin

START = "__start__"
END = "__end__"

class StateGraph:
    """
    A graph-based state machine for managing complex workflows.

    This class allows defining states, transitions, and conditions for state changes,
    as well as executing the workflow based on the defined graph structure.

    Attributes:
        state_schema (Dict[str, Any]): The schema defining the structure of the state.
        graph (nx.DiGraph): The directed graph representing the state machine.
        interrupt_before (List[str]): Nodes to interrupt before execution.
        interrupt_after (List[str]): Nodes to interrupt after execution.

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

    def __init__(self, state_schema: Dict[str, Any], raise_exceptions: bool = False, debug_mode: bool = False):
        """
        Initialize the StateGraph with optional debug mode.

        Args:
            state_schema (Dict[str, Any]): The schema defining the structure of the state.
            raise_exceptions (bool): If True, exceptions will be raised instead of being handled internally.
            debug_mode (bool): If True, enables state tracking for debugging.
        """
        self.state_schema = state_schema
        self.graph = nx.DiGraph()
        self.debug_mode = debug_mode

        # Initialize START and END nodes with debug attributes if needed
        debug_attrs = {'before_run': [], 'after_run': [], 'timestamps': [], 'executions': 0}
        self.graph.add_node(START, run=None, **debug_attrs)
        self.graph.add_node(END, run=None, **debug_attrs)

        self.interrupt_before: List[str] = []
        self.interrupt_after: List[str] = []
        self.raise_exceptions = raise_exceptions
        self.parallel_sync = {}

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
        debug_attrs = {'before_run': [], 'after_run': [], 'timestamps': [], 'executions': 0}
        self.graph.add_node(node, run=run, **debug_attrs)


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
        debug_attrs = {'before_run': [], 'after_run': [], 'timestamps': [], 'executions': 0}
        self.graph.add_node(node, run=run, fan_in=True, **debug_attrs)

    def invoke(self, input_state: Dict[str, Any] = {}, config: Dict[str, Any] = {}) -> Generator[Dict[str, Any], None, None]:
        """
        Execute the graph, yielding interrupts and the final state.

        Args:
            input_state (Dict[str, Any]): The initial state.
            config (Dict[str, Any]): Configuration for the execution.

        Yields:
            Dict[str, Any]: Interrupts and the final state during execution.
        """

        current_node = START
        state = input_state.copy()
        config = config.copy()
        # Create a ThreadPoolExecutor for parallel flows
        executor = ThreadPoolExecutor()
        # Mapping from fan-in nodes to list of futures
        fanin_futures: Dict[str, List[Future]] = {}
        # Lock for thread-safe operations on fanin_futures
        fanin_lock = threading.Lock()

        try:
            while current_node != END:
                # Check for interrupt before
                if current_node in self.interrupt_before:
                    yield {"type": "interrupt", "node": current_node, "state": state.copy()}

                # Get node data
                node_data = self.node(current_node)
                run_func = node_data.get("run")

                # Execute node's run function if present
                if run_func:
                    try:
                        result = self._execute_node_function(run_func, state, config, current_node)
                        state.update(result)
                    except Exception as e:
                        if self.raise_exceptions:
                            raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                        else:
                            yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                            return

                # Check for interrupt after
                if current_node in self.interrupt_after:
                    yield {"type": "interrupt", "node": current_node, "state": state.copy()}

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
                for successor, fan_in_node in parallel_edges:
                    # Start a new thread for the flow starting from successor
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

                        if cond_map:
                            next_node_candidate = cond_map.get(cond_result, None)
                            if next_node_candidate:
                                next_node = next_node_candidate
                                break
                        else:
                            if cond_result:
                                next_node = successor
                                break
                    if next_node:
                        current_node = next_node
                    else:
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return
                else:
                    # No normal successors
                    # Check if there is a fan-in node with pending futures
                    if fanin_futures:
                        # Proceed to the fan-in node
                        current_node = list(fanin_futures.keys())[0]
                        # Wait for all futures corresponding to this fan-in node
                        futures = fanin_futures.get(current_node, [])
                        results = [future.result() for future in futures]
                        # Collect the results in the state
                        state['parallel_results'] = results

                        # Execute the fan-in node's run function
                        node_data = self.node(current_node)
                        run_func = node_data.get("run")
                        if run_func:
                            try:
                                result = self._execute_node_function(run_func, state, config, current_node)
                                state.update(result)
                            except Exception as e:
                                if self.raise_exceptions:
                                    raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                                else:
                                    yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                                    return

                        # Continue to next node
                        next_node = self._get_next_node(current_node, state, config)
                        if not next_node:
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
        finally:
            executor.shutdown(wait=True)
        # Once END is reached, yield final state
        yield {"type": "final", "state": state.copy()}


    def _execute_flow(self, current_node, state, config, fan_in_node):
        """
        Execute a flow starting from current_node until it reaches fan_in_node.

        Args:
            current_node (str): The starting node of the flow.
            state (Dict[str, Any]): The state of the flow.
            config (Dict[str, Any]): Configuration for the execution.
            fan_in_node (str): The fan-in node where this flow should stop.

        Returns:
            Dict[str, Any]: The final state of the flow when it reaches the fan-in node.
        """
        while current_node != END:
            # Check if current node is the fan-in node
            if current_node == fan_in_node:
                # Return the state to be collected at fan-in node
                return state

            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                try:
                    result = self._execute_node_function(run_func, state, config, current_node)
                    state.update(result)
                except Exception as e:
                    if self.raise_exceptions:
                        raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                    else:
                        # Return error in state
                        state['error'] = str(e)
                        return state

            # Determine next node using _get_next_node
            try:
                next_node = self._get_next_node(current_node, state, config)
            except Exception as e:
                if self.raise_exceptions:
                    raise
                else:
                    state['error'] = str(e)
                    return state

            if next_node:
                current_node = next_node
            else:
                error_msg = f"No valid next node found from node '{current_node}' in parallel flow"
                if self.raise_exceptions:
                    raise RuntimeError(error_msg)
                else:
                    state['error'] = error_msg
                    return state

        # Reached END
        return state

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

         # Get node data and update tracking attributes
        node_data = self.node(node)
        timestamp = time.time()

        # Store state before execution
        # Track debug information only if debug mode is enabled
        if self.debug_mode:
            node_data['before_run'].append(state.copy())
            node_data['timestamps'].append(time.time())
            node_data['executions'] += 1

        available_params = {"state": state, "config": config, "node": node, "graph": self}
        if 'parallel_results' in state:
            available_params['parallel_results'] = state['parallel_results']
        function_params = self._prepare_function_params(func, available_params)
        result = func(**function_params)
        if isinstance(result, dict):
            state.update(result)
        else:
            state["result"] = result

        if self.debug_mode:
            node_data['after_run'].append(state.copy())

        return result

    def get_node_execution_history(self, node_name: str) -> Dict[str, Any]:
        """
        Get the execution history of a specific node.

        Args:
            node_name (str): The name of the node.

        Returns:
            Dict[str, Any]: A dictionary containing the node's execution history:
                - before_run: List of states before each execution
                - after_run: List of states after each execution
                - timestamps: List of execution timestamps
                - executions: Total number of executions
                - run: The node's run function
                - other attributes of the node

        Raises:
            KeyError: If the node is not found in the graph.
        """
        if node_name not in self.graph.nodes:
            raise KeyError(f"Node '{node_name}' not found in the graph")

        node_data = self.node(node_name)
        return {
            'before_run': node_data.get('before_run', []),
            'after_run': node_data.get('after_run', []),
            'timestamps': node_data.get('timestamps', []),
            'executions': node_data.get('executions', 0),
            'run': node_data.get('run'),
            **{k: v for k, v in node_data.items()
               if k not in ['before_run', 'after_run', 'timestamps', 'executions', 'run']}
        }

    def get_node_state_at_execution(self, node_name: str, execution_index: int) -> Tuple[Dict[str, Any], Dict[str, Any], float]:
        """
        Get the before and after states of a node for a specific execution.

        Args:
            node_name (str): The name of the node.
            execution_index (int): The index of the execution (0-based).

        Returns:
            Tuple[Dict[str, Any], Dict[str, Any], float]: A tuple containing:
                - The state before execution
                - The state after execution
                - The timestamp of the execution

        Raises:
            KeyError: If the node is not found in the graph.
            IndexError: If the execution_index is out of range.
        """
        if node_name not in self.graph.nodes:
            raise KeyError(f"Node '{node_name}' not found in the graph")

        node_data = self.node(node_name)
        before_states = node_data.get('before_run', [])
        after_states = node_data.get('after_run', [])
        timestamps = node_data.get('timestamps', [])

        if execution_index < 0 or execution_index >= len(before_states):
            raise IndexError(f"Execution index {execution_index} out of range for node '{node_name}'")

        return (
            before_states[execution_index],
            after_states[execution_index],
            timestamps[execution_index]
        )

    def clear_node_history(self, node_name: str) -> None:
        """
        Clear the execution history of a specific node.

        Args:
            node_name (str): The name of the node.

        Raises:
            KeyError: If the node is not found in the graph.
        """
        if node_name not in self.graph.nodes:
            raise KeyError(f"Node '{node_name}' not found in the graph")

        node_data = self.node(node_name)
        node_data['before_run'] = []
        node_data['after_run'] = []
        node_data['timestamps'] = []
        node_data['executions'] = 0

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

    def compile(self, interrupt_before: List[str] = [], interrupt_after: List[str] = []) -> 'StateGraph':
        """
        Compile the graph and set interruption points.

        Args:
            interrupt_before (List[str]): Nodes to interrupt before execution.
            interrupt_after (List[str]): Nodes to interrupt after execution.

        Returns:
            StateGraph: The compiled graph instance.

        Raises:
            ValueError: If any interrupt node doesn't exist in the graph.
        """
        for node in interrupt_before + interrupt_after:
            if node not in self.graph.nodes:
                raise ValueError(f"Interrupt node '{node}' does not exist in the graph.")

        self.interrupt_before = interrupt_before
        self.interrupt_after = interrupt_after
        return self

    def enable_debug_mode(self) -> None:
        """Enable debug mode."""
        self.debug_mode = True

    def disable_debug_mode(self) -> None:
        """Disable debug mode."""
        self.debug_mode = False

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

    def stream(self, input_state: Dict[str, Any] = {}, config: Dict[str, Any] = {}) -> Generator[Dict[str, Any], None, None]:
        """
        Execute the graph, yielding results at each node execution, including interrupts.

        Args:
            input_state (Dict[str, Any]): The initial state.
            config (Dict[str, Any]): Configuration for the execution.

        Yields:
            Dict[str, Any]: Intermediate states, interrupts, errors, and the final state during execution.
        """
        current_node = START
        state = input_state.copy()
        config = config.copy()

        while current_node != END:
            # Check for interrupt before
            if current_node in self.interrupt_before:
                yield {"type": "interrupt_before", "node": current_node, "state": state.copy()}

            # Get node data
            node_data = self.node(current_node)
            run_func = node_data.get("run")

            # Execute node's run function if present
            if run_func:
                try:
                    result = self._execute_node_function(run_func, state, config, current_node)
                    state.update(result)
                    # Yield intermediate state after execution
                    yield {"type": "state", "node": current_node, "state": state.copy()}
                except Exception as e:
                    if self.raise_exceptions:
                        raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
                    else:
                        yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
                        return

            # Check for interrupt after
            if current_node in self.interrupt_after:
                yield {"type": "interrupt_after", "node": current_node, "state": state.copy()}

            # Determine next node
            next_node = self._get_next_node(current_node, state, config)
            if not next_node:
                error_msg = f"No valid next node found from node '{current_node}'"
                if self.raise_exceptions:
                    raise RuntimeError(error_msg)
                else:
                    yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                    return

            current_node = next_node

        # Once END is reached, yield final state
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

    def render_graphviz(self):
        """
        Render the graph using NetworkX and Graphviz.

        Returns:
            pygraphviz.AGraph: A PyGraphviz graph object representing the StateGraph.
        """
        # Create a new directed graph
        G = nx.DiGraph()

        # Add nodes with attributes
        for node in self.graph.nodes():
            label = f"{node}\n"
            label += f"interrupt_before: {node in self.interrupt_before}\n"
            label += f"interrupt_after: {node in self.interrupt_after}"
            G.add_node(node, label=label)

        # Add edges with attributes
        for u, v, data in self.graph.edges(data=True):
            edge_label = ""
            if 'cond' in data:
                cond = data['cond']
                if callable(cond) and cond.__name__ != '<lambda>':
                    edge_label = "condition"
                elif isinstance(cond, dict) and len(cond) > 1:
                    edge_label = "condition"
            G.add_edge(u, v, label=edge_label)

        # Convert to a PyGraphviz graph
        A = to_agraph(G)

        # Set graph attributes
        A.graph_attr.update(rankdir="TB", size="8,8")
        A.node_attr.update(shape="rectangle", style="filled", fillcolor="white")
        A.edge_attr.update(color="black")

        return A


    def save_graph_image(self, filename="state_graph.png"):
        """
        Save the graph as an image file.

        Args:
            filename (str): The name of the file to save the graph image to.
        """
        A = self.render_graphviz()
        A.layout(prog='dot')
        A.draw(filename)
