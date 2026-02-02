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
    - github://user/repo@ref/path - GitHub repository files (TEA-CLI-001)
    - gitlab://user/repo@ref/path - GitLab repository files (TEA-CLI-001)

Authentication (via environment variables):
    - GITHUB_TOKEN: GitHub API authentication
    - GITLAB_TOKEN: GitLab API authentication
    - GIT_TOKEN: Generic fallback for git protocols

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

    >>> # Git repository files (TEA-CLI-001)
    >>> result = registry['file.read'](state={}, path="github://user/repo@main/file.yaml")
    >>> print(result['content'])
"""

import os
from pathlib import Path
from typing import Any, Callable, Dict, Optional, Tuple
import fsspec


# Backend package names for helpful error messages
_BACKEND_PACKAGES = {
    "s3": "s3fs",
    "gs": "gcsfs",
    "gcs": "gcsfs",
    "az": "adlfs",
    "abfs": "adlfs",
    "abfss": "adlfs",
    "wasb": "adlfs",
    "wasbs": "adlfs",
    "sftp": "sshfs",
    "ssh": "sshfs",
    "github": "requests",
    "gitlab": "requests",
}


def _parse_git_url(url: str) -> Optional[Dict[str, str]]:
    """
    Parse github:// or gitlab:// URLs into components.

    Supported formats:
    - github://user/repo@ref/path/to/file
    - gitlab://user/repo@ref/path/to/file

    Args:
        url: Git URL to parse

    Returns:
        Dict with keys: provider, owner, repo, ref, path
        None if not a valid git URL
    """
    for provider in ("github", "gitlab"):
        prefix = f"{provider}://"
        if url.startswith(prefix):
            remainder = url[len(prefix) :]

            # Split on @ to get ref
            if "@" in remainder:
                repo_part, rest = remainder.split("@", 1)
                if "/" in rest:
                    ref, file_path = rest.split("/", 1)
                else:
                    ref = rest
                    file_path = ""
            else:
                # No ref specified, default to main
                ref = "main"
                if "/" in remainder:
                    parts = remainder.split("/", 2)
                    if len(parts) >= 3:
                        repo_part = f"{parts[0]}/{parts[1]}"
                        file_path = parts[2]
                    else:
                        repo_part = remainder
                        file_path = ""
                else:
                    repo_part = remainder
                    file_path = ""

            # Parse owner/repo
            if "/" in repo_part:
                owner, repo = repo_part.split("/", 1)
            else:
                return None  # Invalid format

            return {
                "provider": provider,
                "owner": owner,
                "repo": repo,
                "ref": ref,
                "path": file_path,
            }

    return None


def _get_git_token(provider: str) -> Optional[str]:
    """
    Get authentication token for git provider from environment.

    Checks in order:
    1. Provider-specific token (GITHUB_TOKEN, GITLAB_TOKEN)
    2. Generic fallback (GIT_TOKEN)

    Args:
        provider: 'github' or 'gitlab'

    Returns:
        Token string or None if not found
    """
    if provider == "github":
        return os.environ.get("GITHUB_TOKEN") or os.environ.get("GIT_TOKEN")
    elif provider == "gitlab":
        return os.environ.get("GITLAB_TOKEN") or os.environ.get("GIT_TOKEN")
    return os.environ.get("GIT_TOKEN")


class _GitFileSystem:
    """
    Minimal filesystem-like interface for github:// and gitlab:// URLs.

    This provides read-only access to files in git repositories via their
    respective APIs (GitHub Raw, GitLab Raw).

    Note: This is intentionally minimal - only implements methods needed
    by file_read action. For full fsspec compatibility, a proper
    AbstractFileSystem subclass would be needed.
    """

    def __init__(
        self,
        provider: str,
        owner: str,
        repo: str,
        ref: str,
        token: Optional[str] = None,
    ):
        """
        Initialize git filesystem.

        Args:
            provider: 'github' or 'gitlab'
            owner: Repository owner/namespace
            repo: Repository name
            ref: Branch, tag, or commit SHA
            token: Optional authentication token
        """
        self.provider = provider
        self.owner = owner
        self.repo = repo
        self.ref = ref
        self.token = token

    def _get_raw_url(self, file_path: str) -> str:
        """Get raw file URL for the provider."""
        if self.provider == "github":
            return f"https://raw.githubusercontent.com/{self.owner}/{self.repo}/{self.ref}/{file_path}"
        elif self.provider == "gitlab":
            # GitLab raw file URL format
            return f"https://gitlab.com/{self.owner}/{self.repo}/-/raw/{self.ref}/{file_path}"
        else:
            raise ValueError(f"Unknown provider: {self.provider}")

    def open(self, path: str, mode: str = "r", encoding: str = "utf-8"):
        """
        Open a file from the git repository.

        Args:
            path: Path within repository
            mode: Must be 'r' or 'rb' (read-only)
            encoding: Text encoding (for 'r' mode)

        Returns:
            File-like object
        """
        import requests
        from io import BytesIO, StringIO

        if mode not in ("r", "rb"):
            raise ValueError("Git filesystems are read-only. Mode must be 'r' or 'rb'.")

        url = self._get_raw_url(path)
        headers = {}
        if self.token:
            if self.provider == "github":
                headers["Authorization"] = f"token {self.token}"
            elif self.provider == "gitlab":
                headers["PRIVATE-TOKEN"] = self.token

        response = requests.get(url, headers=headers, timeout=30)

        if response.status_code == 404:
            raise FileNotFoundError(
                f"File not found: {self.provider}://{self.owner}/{self.repo}@{self.ref}/{path}"
            )
        elif response.status_code == 401:
            token_env = "GITHUB_TOKEN" if self.provider == "github" else "GITLAB_TOKEN"
            raise PermissionError(
                f"Authentication required for private repository. Set {token_env} environment variable."
            )
        elif response.status_code == 403:
            raise PermissionError(
                f"Access denied to {self.provider}://{self.owner}/{self.repo}@{self.ref}/{path}"
            )

        response.raise_for_status()

        if mode == "rb":
            return BytesIO(response.content)
        else:
            return StringIO(response.content.decode(encoding))

    def info(self, path: str) -> Dict[str, Any]:
        """Get file info (limited - size not available without fetch)."""
        return {"name": path, "type": "file"}


def _get_git_filesystem(path: str) -> Tuple[Any, str, Optional[Dict[str, Any]]]:
    """
    Get filesystem for github:// or gitlab:// URLs.

    Args:
        path: Git URL (github://user/repo@ref/path or gitlab://user/repo@ref/path)

    Returns:
        Tuple of (filesystem, file_path, error_dict)
    """
    git_parts = _parse_git_url(path)
    if not git_parts:
        return (
            None,
            "",
            {
                "success": False,
                "error": f"Invalid git URL format: {path}. Expected: github://user/repo@ref/path",
                "error_type": "invalid_url",
            },
        )

    provider = git_parts["provider"]
    token = _get_git_token(provider)

    try:
        fs = _GitFileSystem(
            provider=provider,
            owner=git_parts["owner"],
            repo=git_parts["repo"],
            ref=git_parts["ref"],
            token=token,
        )
        return fs, git_parts["path"], None
    except ImportError:
        return (
            None,
            "",
            {
                "success": False,
                "error": "requests package required for git:// URLs. Install with: pip install requests",
                "error_type": "backend_not_installed",
            },
        )
    except Exception as e:
        return (
            None,
            "",
            {"success": False, "error": str(e), "error_type": "filesystem_error"},
        )


def _get_filesystem(path: str) -> Tuple[Any, str, Optional[Dict[str, Any]]]:
    """
    Get fsspec filesystem and path from a URI.

    Handles both URI schemes (s3://bucket/path) and local paths (/path/to/file).
    Returns appropriate error dict if backend package is missing.

    Supports git protocols (github://, gitlab://) via a custom filesystem
    implementation that uses the provider's raw file API.

    Args:
        path: File path or URI (e.g., 's3://bucket/key', '/local/path', 'relative/path',
              'github://user/repo@ref/path', 'gitlab://user/repo@ref/path')

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

        >>> # Git repository access
        >>> fs, fs_path, err = _get_filesystem("github://user/repo@main/file.yaml")
    """
    # Handle git:// protocols specially (TEA-CLI-001)
    if path.startswith(("github://", "gitlab://")):
        return _get_git_filesystem(path)

    try:
        # fsspec.url_to_fs handles all URI parsing
        fs, fs_path = fsspec.url_to_fs(path)
        return fs, fs_path, None
    except ImportError:
        # Extract the protocol from the path for better error messages
        protocol = path.split("://")[0] if "://" in path else "file"
        package = _BACKEND_PACKAGES.get(protocol, protocol)
        return (
            None,
            "",
            {
                "success": False,
                "error": f"Backend package not installed for '{protocol}://'. Install with: pip install {package}",
                "error_type": "backend_not_installed",
            },
        )
    except Exception as e:
        return (
            None,
            "",
            {"success": False, "error": str(e), "error_type": "filesystem_error"},
        )


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
    if path.startswith("file://"):
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

    registry["http.get"] = http_get
    registry["actions.http_get"] = http_get
    registry["http.post"] = http_post
    registry["actions.http_post"] = http_post

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
            parent = (
                fs._parent(fs_path)
                if hasattr(fs, "_parent")
                else str(Path(fs_path).parent)
            )
            if parent and parent != fs_path:
                try:
                    fs.makedirs(parent, exist_ok=True)
                except (NotImplementedError, AttributeError):
                    # Some filesystems don't support makedirs (e.g., memory://)
                    pass

            # Write content
            if (
                stream
                and hasattr(content, "__iter__")
                and not isinstance(content, (str, bytes))
            ):
                # Stream write for iterators
                with fs.open(fs_path, "w") as f:
                    for chunk in content:
                        f.write(chunk)
            else:
                # Standard write
                if hasattr(fs, "write_text"):
                    fs.write_text(fs_path, content)
                else:
                    with fs.open(fs_path, "w") as f:
                        f.write(content)

            return {"path": path, "success": True}
        except PermissionError:
            return {
                "success": False,
                "error": f"Permission denied: {path}",
                "error_type": "permission_denied",
            }
        except FileNotFoundError:
            return {
                "success": False,
                "error": f"Path not found: {path}",
                "error_type": "not_found",
            }
        except Exception as e:
            error_msg = str(e)
            # Detect common cloud errors
            if "403" in error_msg or "Forbidden" in error_msg:
                return {
                    "success": False,
                    "error": f"Permission denied: {error_msg}",
                    "error_type": "permission_denied",
                }
            if (
                "404" in error_msg
                or "NoSuchBucket" in error_msg
                or "not found" in error_msg.lower()
            ):
                return {
                    "success": False,
                    "error": f"Not found: {error_msg}",
                    "error_type": "not_found",
                }
            if "credential" in error_msg.lower() or "auth" in error_msg.lower():
                return {
                    "success": False,
                    "error": f"Missing credentials: {error_msg}",
                    "error_type": "missing_credentials",
                }
            return {"success": False, "error": error_msg, "error_type": "write_error"}

    def file_read(state, path, stream=False, encoding="utf-8", cache=None, **kwargs):
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
        if (
            cache
            and "://" in path
            and not path.startswith(("simplecache::", "filecache::", "blockcache::"))
        ):
            cache_prefix = {
                "simple": "simplecache::",
                "file": "filecache::",
                "block": "blockcache::",
            }.get(cache, "")
            if cache_prefix:
                path = cache_prefix + path
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            if stream:
                # Return file-like object for streaming
                return {
                    "file": fs.open(fs_path, "r", encoding=encoding),
                    "success": True,
                }
            else:
                # Read entire content
                if hasattr(fs, "read_text"):
                    content = fs.read_text(fs_path, encoding=encoding)
                else:
                    with fs.open(fs_path, "r", encoding=encoding) as f:
                        content = f.read()
                return {"content": content, "success": True}
        except FileNotFoundError:
            return {
                "success": False,
                "error": f"File not found: {path}",
                "error_type": "not_found",
            }
        except PermissionError:
            return {
                "success": False,
                "error": f"Permission denied: {path}",
                "error_type": "permission_denied",
            }
        except Exception as e:
            error_msg = str(e)
            # Detect common cloud errors
            if "403" in error_msg or "Forbidden" in error_msg:
                return {
                    "success": False,
                    "error": f"Permission denied: {error_msg}",
                    "error_type": "permission_denied",
                }
            if (
                "404" in error_msg
                or "NoSuchKey" in error_msg
                or "not found" in error_msg.lower()
            ):
                return {
                    "success": False,
                    "error": f"Not found: {error_msg}",
                    "error_type": "not_found",
                }
            if "NoSuchBucket" in error_msg:
                return {
                    "success": False,
                    "error": f"Bucket not found: {error_msg}",
                    "error_type": "bucket_not_found",
                }
            if "credential" in error_msg.lower() or "auth" in error_msg.lower():
                return {
                    "success": False,
                    "error": f"Missing credentials: {error_msg}",
                    "error_type": "missing_credentials",
                }
            return {"success": False, "error": error_msg, "error_type": "read_error"}

    registry["file.write"] = file_write
    registry["actions.file_write"] = file_write
    registry["file.read"] = file_read
    registry["actions.file_read"] = file_read

    # Notify action (placeholder)
    def notify(state, channel, message, **kwargs):
        """Send a notification."""
        print(f"[{channel.upper()}] {message}")
        return {"sent": True}

    registry["actions.notify"] = notify
    registry["notify"] = notify

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
                "error": "No graph available for checkpoint",
            }

        try:
            # Ensure parent directory exists
            path_obj = Path(path)
            path_obj.parent.mkdir(parents=True, exist_ok=True)

            # Save checkpoint
            target_graph.save_checkpoint(
                str(path_obj), state, node or "unknown", config or {}
            )

            # Track as last checkpoint
            engine._last_checkpoint_path = str(path_obj)

            return {"checkpoint_path": str(path_obj), "saved": True}
        except Exception as e:
            return {"checkpoint_path": path, "saved": False, "error": str(e)}

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
                "checkpoint_version": checkpoint.get("version"),
            }
        except FileNotFoundError:
            return {"error": f"Checkpoint file not found: {path}"}
        except ValueError as e:
            return {"error": f"Invalid checkpoint file: {e}"}
        except Exception as e:
            return {"error": f"Failed to load checkpoint: {e}"}

    registry["checkpoint.save"] = checkpoint_save
    registry["checkpoint.load"] = checkpoint_load
