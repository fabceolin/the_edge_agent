"""
Checkpoint persistence mixin for StateGraph.

This module provides the CheckpointMixin class that adds checkpoint save/load
functionality to the StateGraph class. Checkpoints allow saving execution state
to disk and resuming later from where execution left off.

The mixin pattern keeps these methods on the StateGraph class while organizing
the implementation in a separate file for maintainability.

Example:
    >>> from the_edge_agent import StateGraph
    >>> graph = StateGraph({"value": int})
    >>> # ... add nodes and edges ...
    >>> graph.save_checkpoint("/tmp/checkpoint.pkl", state, "node_a", config)
    >>> # Later...
    >>> for event in graph.resume_from_checkpoint("/tmp/checkpoint.pkl"):
    ...     print(event)
"""

import copy
import logging
import os
import pickle
import threading
import time
from concurrent.futures import ThreadPoolExecutor, Future
from queue import Queue, Empty
from typing import Any, Callable, Dict, Generator, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    pass  # For future type hints if needed


class CheckpointMixin:
    """
    Mixin providing checkpoint persistence functionality for StateGraph.

    This mixin expects the following attributes to be provided by the StateGraph class:
        - self.graph: nx.DiGraph - The underlying graph structure
        - self.logger: logging.Logger - Logger instance
        - self.raise_exceptions: bool - Whether to raise exceptions
        - self.checkpoint_dir: Optional[str] - Directory for auto-save
        - self.log_state_values: bool - Whether to log state values
        - self.max_workers: Optional[int] - Max workers for parallel execution
        - self.interrupt_before: List[str] - Nodes to interrupt before
        - self.interrupt_after: List[str] - Nodes to interrupt after

    Methods provided by StateGraph used by this mixin:
        - self.node(node_name) -> Dict[str, Any]
        - self.edge(in_node, out_node) -> Dict[str, Any]
        - self.successors(node) -> List[str]
        - self._execute_node_function(func, state, config, node) -> Dict[str, Any]
        - self._prepare_function_params(func, available_params) -> Dict[str, Any]
        - self._get_next_node(current_node, state, config) -> Optional[str]
        - self._execute_flow(current_node, state, config, fan_in_node) -> Dict[str, Any]
        - self._stream_parallel_flow(start_node, state, config, fan_in_node, result_queue) -> None
    """

    # Type hints for attributes provided by StateGraph (for IDE support)
    graph: Any
    logger: logging.Logger
    raise_exceptions: bool
    checkpoint_dir: Optional[str]
    checkpointer: Optional[Any]  # MemoryCheckpointer or compatible
    log_state_values: bool
    max_workers: Optional[int]
    interrupt_before: List[str]
    interrupt_after: List[str]

    def save_checkpoint(self, file_path: str, state: Dict[str, Any], node: str, config: Dict[str, Any]) -> None:
        """
        Save execution context to a pickle file for later resumption.

        Args:
            file_path (str): Path where the checkpoint file will be saved.
            state (Dict[str, Any]): Current state dictionary to save.
            node (str): Node name to resume from.
            config (Dict[str, Any]): Configuration dictionary at checkpoint time.

        Raises:
            ValueError: If node doesn't exist in the graph.
            RuntimeError: If raise_exceptions=True and file I/O or pickle error occurs.

        Note:
            **Parallel Flow Limitation:** Checkpoints capture main thread state only.
            If checkpoint is taken during parallel execution, only the main thread's
            view of state is saved. Parallel branches' intermediate states are NOT
            captured. If checkpoint is taken at a fan-in node after parallel flows
            complete, the state WILL include `parallel_results` from all branches.

        Example:
            >>> graph.save_checkpoint("/tmp/checkpoint.pkl", state, "node_a", config)
        """
        if node not in self.graph.nodes:
            raise ValueError(f"Node '{node}' does not exist in the graph.")

        checkpoint = {
            "state": state.copy(),
            "node": node,
            "config": config.copy() if config else {},
            "timestamp": time.time(),
            "version": "1.0",
        }

        try:
            # Ensure directory exists
            dir_path = os.path.dirname(file_path)
            if dir_path:
                os.makedirs(dir_path, exist_ok=True)
            with open(file_path, 'wb') as f:
                pickle.dump(checkpoint, f)
            self.logger.info(f"Checkpoint saved to '{file_path}' at node '{node}'")
        except (IOError, OSError, pickle.PicklingError) as e:
            error_msg = f"Failed to save checkpoint to '{file_path}': {str(e)}"
            self.logger.error(error_msg)
            if self.raise_exceptions:
                raise RuntimeError(error_msg) from e

    def _auto_save_checkpoint(self, state: Dict[str, Any], node: str, config: Dict[str, Any]) -> Optional[str]:
        """
        Auto-save checkpoint using configured checkpointer or checkpoint_dir.

        Called internally before yielding interrupt events. Supports both:
        - File-based storage (when checkpoint_dir is set)
        - In-memory storage (when checkpointer is set, e.g., MemoryCheckpointer)

        Args:
            state (Dict[str, Any]): Current state dictionary.
            node (str): Current node name.
            config (Dict[str, Any]): Current configuration.

        Returns:
            Optional[str]: Checkpoint identifier (file path or memory key),
                or None if no checkpointer is configured.
        """
        timestamp_ms = int(time.time() * 1000)

        # Check for in-memory checkpointer first
        if self.checkpointer is not None:
            checkpoint_id = f"{node}_{timestamp_ms}"
            self.checkpointer.save(checkpoint_id, state, node, config)
            self.logger.info(f"Checkpoint saved to memory: '{checkpoint_id}'")
            return checkpoint_id

        # Fall back to file-based storage
        if self.checkpoint_dir is not None:
            filename = f"{node}_{timestamp_ms}.pkl"
            file_path = os.path.join(self.checkpoint_dir, filename)
            self.save_checkpoint(file_path, state, node, config)
            return file_path

        return None

    @classmethod
    def load_checkpoint(cls, file_path: str) -> Dict[str, Any]:
        """
        Load a checkpoint from a pickle file.

        Args:
            file_path (str): Path to the checkpoint file.

        Returns:
            Dict[str, Any]: Checkpoint data containing:
                - "state": dict - The state dictionary
                - "node": str - Node name to resume from
                - "config": dict - Configuration dictionary
                - "timestamp": float - When checkpoint was saved
                - "version": str - Checkpoint format version

        Raises:
            FileNotFoundError: If checkpoint file doesn't exist.
            ValueError: If checkpoint file is corrupt or incompatible.

        Example:
            >>> checkpoint = StateGraph.load_checkpoint("/tmp/checkpoint.pkl")
            >>> print(checkpoint["node"], checkpoint["state"])
        """
        try:
            with open(file_path, 'rb') as f:
                checkpoint = pickle.load(f)
        except FileNotFoundError:
            raise FileNotFoundError(f"Checkpoint file not found: '{file_path}'")
        except (IOError, OSError) as e:
            raise ValueError(f"Failed to read checkpoint file '{file_path}': {str(e)}") from e
        except (pickle.UnpicklingError, EOFError, AttributeError, ImportError) as e:
            raise ValueError(f"Corrupt or incompatible checkpoint file '{file_path}': {str(e)}") from e

        # Validate checkpoint structure
        required_keys = {"state", "node", "config", "timestamp", "version"}
        if not isinstance(checkpoint, dict):
            raise ValueError(f"Invalid checkpoint format in '{file_path}': expected dict, got {type(checkpoint).__name__}")
        missing_keys = required_keys - set(checkpoint.keys())
        if missing_keys:
            raise ValueError(f"Incompatible checkpoint file '{file_path}': missing keys {missing_keys}")

        return checkpoint

    def resume_from_checkpoint(self, checkpoint_id: str, config: Optional[Dict[str, Any]] = None) -> Generator[Dict[str, Any], None, None]:
        """
        Resume execution from a saved checkpoint.

        Loads the checkpoint and continues execution starting BY RE-EXECUTING the saved node
        (not after it). The saved state is restored, and execution proceeds normally from there.

        Args:
            checkpoint_id (str): Checkpoint identifier. Can be:
                - Memory key (when using MemoryCheckpointer)
                - File path (when using checkpoint_dir)
            config (Optional[Dict[str, Any]]): Optional config override. If provided, merges
                with saved config (provided config takes precedence).

        Yields:
            Dict[str, Any]: Events during execution (same as invoke()):
                - {"type": "interrupt", "node": str, "state": dict, "checkpoint_path": str}: Interrupt
                - {"type": "error", "node": str, "error": str, "state": dict}: Error occurred
                - {"type": "final", "state": dict}: Execution completed successfully

        Raises:
            KeyError: If checkpoint not found in MemoryCheckpointer.
            FileNotFoundError: If checkpoint file doesn't exist.
            ValueError: If checkpoint is corrupt, incompatible, or node doesn't exist.
            RuntimeError: If raise_exceptions=True and an error occurs.

        Example:
            >>> # Resume from saved checkpoint
            >>> for event in graph.resume_from_checkpoint(checkpoint_path):
            ...     print(event["type"])
        """
        # Try MemoryCheckpointer first
        if self.checkpointer is not None:
            checkpoint = self.checkpointer.load(checkpoint_id)
        else:
            # Fall back to file-based loading
            checkpoint = self.load_checkpoint(checkpoint_id)

        # Validate saved node exists in this graph
        saved_node = checkpoint["node"]
        if saved_node not in self.graph.nodes:
            raise ValueError(f"Checkpoint node '{saved_node}' does not exist in the graph.")

        # Merge configs (provided config overrides saved config)
        merged_config = checkpoint["config"].copy()
        if config:
            merged_config.update(config)

        self.logger.info(f"Resuming from checkpoint at node '{saved_node}'")

        yield from self._invoke_from_node(
            checkpoint["state"].copy(),
            merged_config,
            saved_node
        )

    def _invoke_from_node(self, state: Dict[str, Any], config: Dict[str, Any], start_node: str,
                          skip_first_interrupt: bool = True) -> Generator[Dict[str, Any], None, None]:
        """
        Internal method to execute the graph starting from a specific node.

        This is the core implementation used by both invoke(checkpoint=...) and
        resume_from_checkpoint(). The execution starts BY RE-EXECUTING the specified
        node (not after it).

        Args:
            state (Dict[str, Any]): The state to resume with.
            config (Dict[str, Any]): Configuration for the execution.
            start_node (str): The node to start execution from (will be re-executed).
            skip_first_interrupt (bool): If True, skip interrupt_before check on start_node.
                This is used when resuming from an interrupt to avoid re-triggering it.

        Yields:
            Dict[str, Any]: Events during execution.
        """
        # Import END constant locally to avoid circular imports
        END = "__end__"

        current_node = start_node
        is_first_node = skip_first_interrupt  # Track if we're on the first node

        # Log execution start
        self.logger.debug(f"Resuming execution from node '{start_node}' with state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Resume state: {state}")

        # Create a ThreadPoolExecutor for parallel flows
        max_workers = config.get('max_workers', self.max_workers)
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            fanin_futures: Dict[str, List[Future]] = {}
            fanin_lock = threading.Lock()

            while current_node != END:
                # Check for interrupt before (skip on first node when resuming)
                if current_node in self.interrupt_before and not is_first_node:
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                    return  # STOP execution - must resume via invoke(None, checkpoint=...)

                # For interrupt_after resumes: skip execution and go to next node
                # (the node was already executed before the interrupt was saved)
                if is_first_node and current_node in self.interrupt_after:
                    is_first_node = False
                    next_node = self._get_next_node(current_node, state, config)
                    if not next_node:
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return
                    current_node = next_node
                    continue

                # After first node, enable interrupt checks
                is_first_node = False

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
                    self.logger.debug(f"Launching parallel flow to node '{successor}' (fan-in: '{fan_in_node}')")
                    future = executor.submit(self._execute_flow, successor, copy.deepcopy(state), config.copy(), fan_in_node)
                    with fanin_lock:
                        if fan_in_node not in fanin_futures:
                            fanin_futures[fan_in_node] = []
                        fanin_futures[fan_in_node].append(future)

                # Handle normal successors
                if normal_successors:
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
                    with fanin_lock:
                        if fanin_futures:
                            current_node = list(fanin_futures.keys())[0]
                            futures = fanin_futures.pop(current_node, [])
                        else:
                            futures = None

                    if futures is not None:
                        self.logger.info(f"Joining {len(futures)} parallel flow(s) at fan-in node '{current_node}'")
                        results = [future.result() for future in futures]
                        self.logger.debug(f"All parallel flows joined at '{current_node}'")
                        for result in results:
                            if isinstance(result, dict) and result.get("type") == "error":
                                self.logger.error(f"Error from parallel flow: {result.get('error')}")
                                yield result
                                return
                        state['parallel_results'] = results

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

    def _stream_from_checkpoint(self, checkpoint_id: str, config: Optional[Dict[str, Any]] = None) -> Generator[Dict[str, Any], None, None]:
        """
        Resume streaming execution from a saved checkpoint.

        Internal method that loads checkpoint and streams with intermediate states.
        Uses streaming event types (interrupt_before/after, state) rather than invoke types.

        Args:
            checkpoint_id (str): Checkpoint identifier (memory key or file path).
            config (Optional[Dict[str, Any]]): Optional config override.

        Yields:
            Dict[str, Any]: Streaming events during execution.
        """
        # Try MemoryCheckpointer first
        if self.checkpointer is not None:
            checkpoint = self.checkpointer.load(checkpoint_id)
        else:
            # Fall back to file-based loading
            checkpoint = self.load_checkpoint(checkpoint_id)

        # Validate saved node exists
        saved_node = checkpoint["node"]
        if saved_node not in self.graph.nodes:
            raise ValueError(f"Checkpoint node '{saved_node}' does not exist in the graph.")

        # Merge configs
        merged_config = checkpoint["config"].copy()
        if config:
            merged_config.update(config)

        self.logger.info(f"Resuming stream from checkpoint at node '{saved_node}'")

        yield from self._stream_from_node(
            checkpoint["state"].copy(),
            merged_config,
            saved_node
        )

    def _stream_from_node(self, state: Dict[str, Any], config: Dict[str, Any], start_node: str,
                          skip_first_interrupt: bool = True) -> Generator[Dict[str, Any], None, None]:
        """
        Internal method to stream the graph starting from a specific node.

        Similar to _invoke_from_node but yields streaming event types.
        The execution starts BY RE-EXECUTING the specified node (not after it).

        Args:
            state (Dict[str, Any]): The state to resume with.
            config (Dict[str, Any]): Configuration for the execution.
            start_node (str): The node to start execution from (will be re-executed).
            skip_first_interrupt (bool): If True, skip interrupt_before check on start_node.
                This is used when resuming from an interrupt to avoid re-triggering it.

        Yields:
            Dict[str, Any]: Streaming events during execution.
        """
        # Import END constant locally to avoid circular imports
        END = "__end__"

        current_node = start_node
        is_first_node = skip_first_interrupt  # Track if we're on the first node

        self.logger.debug(f"Resuming stream from node '{start_node}' with state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Resume state: {state}")

        max_workers = config.get('max_workers', self.max_workers)
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            result_queue: Queue = Queue()
            active_branches: Dict[str, set] = {}
            branch_states: Dict[str, List[Dict[str, Any]]] = {}
            stream_lock = threading.Lock()

            while current_node != END:
                # Check for interrupt before (skip on first node when resuming)
                if current_node in self.interrupt_before and not is_first_node:
                    checkpoint_path = self._auto_save_checkpoint(state, current_node, config)
                    yield {"type": "interrupt_before", "node": current_node, "state": state.copy(), "checkpoint_path": checkpoint_path}
                    return  # STOP execution - must resume via stream(None, checkpoint=...)

                # For interrupt_after resumes: skip execution and go to next node
                # (the node was already executed before the interrupt was saved)
                if is_first_node and current_node in self.interrupt_after:
                    is_first_node = False
                    next_node = self._get_next_node(current_node, state, config)
                    if not next_node:
                        error_msg = f"No valid next node found from node '{current_node}'"
                        if self.raise_exceptions:
                            raise RuntimeError(error_msg)
                        else:
                            yield {"type": "error", "node": current_node, "error": error_msg, "state": state.copy()}
                            return
                    current_node = next_node
                    continue

                # After first node, enable interrupt checks
                is_first_node = False

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

                # Determine next node
                successors = self.successors(current_node)

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
                        with stream_lock:
                            if fan_in_node not in active_branches:
                                active_branches[fan_in_node] = set()
                                branch_states[fan_in_node] = []
                            active_branches[fan_in_node].add(successor)
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
                        fan_in_node = pending_fan_ins[0]
                        self.logger.info(f"Waiting for parallel flows to complete at fan-in '{fan_in_node}'")

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
                                            if not event.get("error"):
                                                branch_states[fan_in_node].append(event.get("state", {}))
                                            self.logger.debug(f"Branch '{event_branch}' completed, {len(active_branches[fan_in_node])} remaining")
                            except Empty:
                                continue

                        self.logger.info(f"All parallel flows joined at fan-in node '{fan_in_node}'")
                        with stream_lock:
                            parallel_results = branch_states.pop(fan_in_node, [])
                            active_branches.pop(fan_in_node, None)

                        state['parallel_results'] = parallel_results
                        current_node = fan_in_node

                        node_data = self.node(current_node)
                        run_func = node_data.get("run")

                        if current_node in self.interrupt_before:
                            self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt_before", "node": current_node, "state": state.copy()}

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
                            self._auto_save_checkpoint(state, current_node, config)
                            yield {"type": "interrupt_after", "node": current_node, "state": state.copy()}

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

        self.logger.info("Stream execution completed successfully")
        self.logger.debug(f"Final state keys: {list(state.keys())}")
        if self.log_state_values:
            self.logger.debug(f"Final state: {state}")
        yield {"type": "final", "state": state.copy()}
