"""
Core Actions for YAMLEngine.

This module provides fundamental utility actions for YAMLEngine workflows
including HTTP requests, file operations, notifications, and checkpoint
management.

Actions:
    - http.get: HTTP GET request
    - http.post: HTTP POST request
    - file.read: Read content from a file
    - file.write: Write content to a file
    - notify: Send notifications (placeholder)
    - checkpoint.save: Save workflow checkpoint
    - checkpoint.load: Load workflow checkpoint

Example:
    >>> # HTTP GET
    >>> result = registry['http.get'](state={}, url="https://api.example.com/data")
    >>> print(result)

    >>> # File operations
    >>> registry['file.write'](state={}, path="output.txt", content="Hello World")
    >>> result = registry['file.read'](state={}, path="output.txt")
    >>> print(result['content'])
"""

from pathlib import Path
from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register core utility actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """
    # Import StateGraph for checkpoint operations
    from ..stategraph import StateGraph

    # HTTP actions
    def http_get(state, url, headers=None, **kwargs):
        """Make HTTP GET request."""
        import requests
        response = requests.get(url, headers=headers or {})
        response.raise_for_status()
        return response.json()

    def http_post(state, url, json=None, headers=None, **kwargs):
        """Make HTTP POST request."""
        import requests
        response = requests.post(url, json=json, headers=headers or {})
        response.raise_for_status()
        return response.json()

    registry['http.get'] = http_get
    registry['actions.http_get'] = http_get
    registry['http.post'] = http_post
    registry['actions.http_post'] = http_post

    # File actions
    def file_write(state, path, content, **kwargs):
        """Write content to a file."""
        path_obj = Path(path)
        path_obj.parent.mkdir(parents=True, exist_ok=True)
        path_obj.write_text(content)
        return {'path': str(path_obj)}

    def file_read(state, path, **kwargs):
        """Read content from a file."""
        return {'content': Path(path).read_text()}

    registry['file.write'] = file_write
    registry['actions.file_write'] = file_write
    registry['file.read'] = file_read
    registry['actions.file_read'] = file_read

    # Notify action (placeholder)
    def notify(state, channel, message, **kwargs):
        """Send a notification."""
        print(f"[{channel.upper()}] {message}")
        return {'sent': True}

    registry['actions.notify'] = notify
    registry['notify'] = notify

    # Checkpoint actions
    def checkpoint_save(state, path, graph=None, node=None, config=None, **kwargs):
        """
        Save checkpoint to specified path.

        Args:
            state: Current state dictionary
            path: File path where checkpoint will be saved
            graph: StateGraph instance (injected via context)
            node: Current node name (injected via context)
            config: Current config dict (injected via context)

        Returns:
            {"checkpoint_path": str, "saved": True} on success
            {"checkpoint_path": str, "saved": False, "error": str} on failure
        """
        # Use injected graph or fall back to engine's current graph
        target_graph = graph or engine._current_graph

        if target_graph is None:
            return {
                "checkpoint_path": path,
                "saved": False,
                "error": "No graph available for checkpoint"
            }

        try:
            # Ensure parent directory exists
            path_obj = Path(path)
            path_obj.parent.mkdir(parents=True, exist_ok=True)

            # Save checkpoint
            target_graph.save_checkpoint(
                str(path_obj),
                state,
                node or "unknown",
                config or {}
            )

            # Track as last checkpoint
            engine._last_checkpoint_path = str(path_obj)

            return {
                "checkpoint_path": str(path_obj),
                "saved": True
            }
        except Exception as e:
            return {
                "checkpoint_path": path,
                "saved": False,
                "error": str(e)
            }

    def checkpoint_load(state, path, **kwargs):
        """
        Load checkpoint from specified path.

        Args:
            state: Current state (for template processing, not used)
            path: File path to checkpoint

        Returns:
            {
                "checkpoint_state": dict,
                "checkpoint_node": str,
                "checkpoint_config": dict,
                "checkpoint_timestamp": float,
                "checkpoint_version": str
            }
            Or {"error": str} on failure
        """
        try:
            checkpoint = StateGraph.load_checkpoint(path)
            return {
                "checkpoint_state": checkpoint["state"],
                "checkpoint_node": checkpoint["node"],
                "checkpoint_config": checkpoint.get("config", {}),
                "checkpoint_timestamp": checkpoint.get("timestamp"),
                "checkpoint_version": checkpoint.get("version")
            }
        except FileNotFoundError:
            return {"error": f"Checkpoint file not found: {path}"}
        except ValueError as e:
            return {"error": f"Invalid checkpoint file: {e}"}
        except Exception as e:
            return {"error": f"Failed to load checkpoint: {e}"}

    registry['checkpoint.save'] = checkpoint_save
    registry['checkpoint.load'] = checkpoint_load
