"""
Checkpointer classes for StateGraph checkpoint storage.

This module provides different checkpointer implementations for storing
and retrieving checkpoint data. Checkpointers allow StateGraph to save
execution state at interrupt points and resume later.

Available checkpointers:
    - MemoryCheckpointer: In-memory storage for testing and simple use cases

Example:
    >>> from the_edge_agent.checkpointers import MemoryCheckpointer
    >>> checkpointer = MemoryCheckpointer()
    >>> graph.compile(interrupt_before=["approval"], checkpointer=checkpointer)
"""

import time
from typing import Any, Dict, List, Optional


class MemoryCheckpointer:
    """
    In-memory checkpoint storage for testing and simple use cases.

    Stores checkpoints in a dictionary keyed by checkpoint ID. This is useful
    for testing and scenarios where persistence is not required.

    Note:
        Checkpoints are lost when the process exits. For persistent storage,
        use file-based checkpoints with `checkpoint_dir` instead.

    Attributes:
        _storage (Dict[str, Dict[str, Any]]): Internal storage dictionary.

    Example:
        >>> checkpointer = MemoryCheckpointer()
        >>> graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)
        >>> events = list(graph.invoke({"x": 1}))
        >>> checkpoint_id = events[-1]["checkpoint_path"]
        >>> # Resume later
        >>> events = list(graph.invoke(None, checkpoint=checkpoint_id))
    """

    def __init__(self) -> None:
        """Initialize an empty in-memory checkpointer."""
        self._storage: Dict[str, Dict[str, Any]] = {}

    def save(self, checkpoint_id: str, state: Dict[str, Any], node: str,
             config: Optional[Dict[str, Any]] = None) -> str:
        """
        Save checkpoint to memory.

        Args:
            checkpoint_id (str): Unique identifier for this checkpoint.
            state (Dict[str, Any]): State dictionary to save.
            node (str): Node name to resume from.
            config (Optional[Dict[str, Any]]): Configuration dictionary.

        Returns:
            str: The checkpoint_id (for consistency with file-based storage).

        Example:
            >>> checkpointer.save("cp_123", {"x": 1}, "node_a", {"key": "val"})
            'cp_123'
        """
        self._storage[checkpoint_id] = {
            "state": state.copy(),
            "node": node,
            "config": config.copy() if config else {},
            "timestamp": time.time(),
            "version": "1.0",
        }
        return checkpoint_id

    def load(self, checkpoint_id: str) -> Dict[str, Any]:
        """
        Load checkpoint from memory.

        Args:
            checkpoint_id (str): The checkpoint identifier to load.

        Returns:
            Dict[str, Any]: Checkpoint data containing:
                - "state": dict - The saved state
                - "node": str - Node name to resume from
                - "config": dict - Saved configuration
                - "timestamp": float - When checkpoint was saved
                - "version": str - Checkpoint format version

        Raises:
            KeyError: If checkpoint_id is not found.

        Example:
            >>> data = checkpointer.load("cp_123")
            >>> print(data["node"], data["state"])
        """
        if checkpoint_id not in self._storage:
            raise KeyError(f"Checkpoint not found: {checkpoint_id}")
        # Return a copy to prevent external modification
        checkpoint = self._storage[checkpoint_id]
        return {
            "state": checkpoint["state"].copy(),
            "node": checkpoint["node"],
            "config": checkpoint["config"].copy(),
            "timestamp": checkpoint["timestamp"],
            "version": checkpoint["version"],
        }

    def list(self) -> List[str]:
        """
        List all checkpoint IDs.

        Returns:
            List[str]: List of all checkpoint identifiers.

        Example:
            >>> checkpointer.save("cp_1", {}, "node_a")
            >>> checkpointer.save("cp_2", {}, "node_b")
            >>> checkpointer.list()
            ['cp_1', 'cp_2']
        """
        return list(self._storage.keys())

    def delete(self, checkpoint_id: str) -> None:
        """
        Delete a checkpoint.

        Args:
            checkpoint_id (str): The checkpoint identifier to delete.

        Raises:
            KeyError: If checkpoint_id is not found.

        Example:
            >>> checkpointer.delete("cp_123")
        """
        if checkpoint_id not in self._storage:
            raise KeyError(f"Checkpoint not found: {checkpoint_id}")
        del self._storage[checkpoint_id]

    def clear(self) -> None:
        """
        Delete all checkpoints.

        Example:
            >>> checkpointer.clear()
            >>> checkpointer.list()
            []
        """
        self._storage.clear()

    def __len__(self) -> int:
        """Return the number of stored checkpoints."""
        return len(self._storage)

    def __bool__(self) -> bool:
        """Checkpointer is always truthy (even when empty)."""
        return True

    def __contains__(self, checkpoint_id: str) -> bool:
        """Check if a checkpoint exists."""
        return checkpoint_id in self._storage
