"""
Core Actions for YAMLEngine.

This module provides fundamental utility actions for YAMLEngine workflows
including HTTP requests, file operations (local and remote via fsspec),
notifications, and checkpoint management.

Actions:
    - http.get: HTTP GET request
    - http.post: HTTP POST request
    - file.read: Read content from a file (local or remote via URI scheme)
    - file.write: Write content to a file (local or remote via URI scheme)
    - notify: Send notifications (placeholder)
    - checkpoint.save: Save workflow checkpoint
    - checkpoint.load: Load workflow checkpoint

Supported URI Schemes (via fsspec):
    - file:///path/to/file - Local filesystem
    - s3://bucket/path - AWS S3 (requires s3fs)
    - gs://bucket/path - Google Cloud Storage (requires gcsfs)
    - az://container/path - Azure Blob Storage (requires adlfs)
    - memory://path - In-memory filesystem (for testing)
    - http://... / https://... - HTTP/HTTPS (read-only)

Example:
    >>> # HTTP GET
    >>> result = registry['http.get'](state={}, url="https://api.example.com/data")
    >>> print(result)

    >>> # Local file operations
    >>> registry['file.write'](state={}, path="output.txt", content="Hello World")
    >>> result = registry['file.read'](state={}, path="output.txt")
    >>> print(result['content'])

    >>> # Remote file operations
    >>> result = registry['file.read'](state={}, path="s3://my-bucket/data.json")
    >>> registry['file.write'](state={}, path="gs://my-bucket/out.json", content='{}')
"""

from pathlib import Path
from typing import Any, Callable, Dict, Optional, Tuple, Union
import fsspec


# Backend package names for helpful error messages
_BACKEND_PACKAGES = {
    's3': 's3fs',
    'gs': 'gcsfs',
    'gcs': 'gcsfs',
    'az': 'adlfs',
    'abfs': 'adlfs',
    'abfss': 'adlfs',
    'wasb': 'adlfs',
    'wasbs': 'adlfs',
    'sftp': 'sshfs',
    'ssh': 'sshfs',
}


def _get_filesystem(path: str) -> Tuple[Any, str, Optional[Dict[str, Any]]]:
    """
    Get fsspec filesystem and path from a URI.

    Handles both URI schemes (s3://bucket/path) and local paths (/path/to/file).
    Returns appropriate error dict if backend package is missing.

    Args:
        path: File path or URI (e.g., 's3://bucket/key', '/local/path', 'relative/path')

    Returns:
        Tuple of (filesystem, resolved_path, error_dict)
        - filesystem: fsspec filesystem instance (or None on error)
        - resolved_path: Path within the filesystem
        - error_dict: Error information if failed, None on success

    Example:
        >>> fs, fs_path, err = _get_filesystem("s3://my-bucket/data.json")
        >>> if err:
        ...     return err
        >>> content = fs.read_text(fs_path)
    """
    try:
        # fsspec.url_to_fs handles all URI parsing
        fs, fs_path = fsspec.url_to_fs(path)
        return fs, fs_path, None
    except ImportError as e:
        # Extract the protocol from the path for better error messages
        protocol = path.split('://')[0] if '://' in path else 'file'
        package = _BACKEND_PACKAGES.get(protocol, protocol)
        return None, '', {
            'success': False,
            'error': f"Backend package not installed for '{protocol}://'. Install with: pip install {package}",
            'error_type': 'backend_not_installed'
        }
    except Exception as e:
        return None, '', {
            'success': False,
            'error': str(e),
            'error_type': 'filesystem_error'
        }


def _normalize_path(path: str) -> str:
    """
    Normalize path for consistent handling.

    - Strips 'file://' prefix for local files
    - Returns other URIs unchanged
    - Converts Path objects to strings

    Args:
        path: File path or URI

    Returns:
        Normalized path string
    """
    if isinstance(path, Path):
        path = str(path)
    # Handle file:// URIs by stripping the prefix for local operations
    if path.startswith('file://'):
        return path[7:]  # Strip 'file://' prefix
    return path


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

    # File actions - now with fsspec for remote storage support
    def file_write(state, path, content, stream=False, **kwargs):
        """
        Write content to a file (local or remote via fsspec).

        Supports local paths, file:// URIs, and remote URIs (s3://, gs://, az://, etc.).
        Automatically creates parent directories for both local and remote paths.

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., '/tmp/file.txt', 's3://bucket/key')
            content: Content to write (string)
            stream: If True and content is an iterator, stream write (default: False)
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'path': str, 'success': True} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            # Create parent directories
            parent = fs._parent(fs_path) if hasattr(fs, '_parent') else str(Path(fs_path).parent)
            if parent and parent != fs_path:
                try:
                    fs.makedirs(parent, exist_ok=True)
                except (NotImplementedError, AttributeError):
                    # Some filesystems don't support makedirs (e.g., memory://)
                    pass

            # Write content
            if stream and hasattr(content, '__iter__') and not isinstance(content, (str, bytes)):
                # Stream write for iterators
                with fs.open(fs_path, 'w') as f:
                    for chunk in content:
                        f.write(chunk)
            else:
                # Standard write
                if hasattr(fs, 'write_text'):
                    fs.write_text(fs_path, content)
                else:
                    with fs.open(fs_path, 'w') as f:
                        f.write(content)

            return {'path': path, 'success': True}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except FileNotFoundError:
            return {'success': False, 'error': f"Path not found: {path}", 'error_type': 'not_found'}
        except Exception as e:
            error_msg = str(e)
            # Detect common cloud errors
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if '404' in error_msg or 'NoSuchBucket' in error_msg or 'not found' in error_msg.lower():
                return {'success': False, 'error': f"Not found: {error_msg}", 'error_type': 'not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'write_error'}

    def file_read(state, path, stream=False, encoding='utf-8', cache=None, **kwargs):
        """
        Read content from a file (local or remote via fsspec).

        Supports local paths, file:// URIs, and remote URIs (s3://, gs://, az://, etc.).
        Supports fsspec's caching via protocol chaining (e.g., 'simplecache::s3://...').

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., '/tmp/file.txt', 's3://bucket/key')
            stream: If True, returns a file-like object instead of content (default: False)
            encoding: Text encoding (default: 'utf-8')
            cache: Caching mode - 'simple', 'file', 'block', or None (default: None)
                   - 'simple': Cache entire file locally (simplecache::)
                   - 'file': Cache to specific temp directory (filecache::)
                   - 'block': Cache in blocks for partial reads (blockcache::)
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'content': str, 'success': True} on success (when stream=False)
            {'file': file-like, 'success': True} on success (when stream=True)
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)

        # Apply cache prefix if specified and path has a remote protocol
        if cache and '://' in path and not path.startswith(('simplecache::', 'filecache::', 'blockcache::')):
            cache_prefix = {
                'simple': 'simplecache::',
                'file': 'filecache::',
                'block': 'blockcache::'
            }.get(cache, '')
            if cache_prefix:
                path = cache_prefix + path
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            if stream:
                # Return file-like object for streaming
                return {'file': fs.open(fs_path, 'r', encoding=encoding), 'success': True}
            else:
                # Read entire content
                if hasattr(fs, 'read_text'):
                    content = fs.read_text(fs_path, encoding=encoding)
                else:
                    with fs.open(fs_path, 'r', encoding=encoding) as f:
                        content = f.read()
                return {'content': content, 'success': True}
        except FileNotFoundError:
            return {'success': False, 'error': f"File not found: {path}", 'error_type': 'not_found'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except Exception as e:
            error_msg = str(e)
            # Detect common cloud errors
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if '404' in error_msg or 'NoSuchKey' in error_msg or 'not found' in error_msg.lower():
                return {'success': False, 'error': f"Not found: {error_msg}", 'error_type': 'not_found'}
            if 'NoSuchBucket' in error_msg:
                return {'success': False, 'error': f"Bucket not found: {error_msg}", 'error_type': 'bucket_not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'read_error'}

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
