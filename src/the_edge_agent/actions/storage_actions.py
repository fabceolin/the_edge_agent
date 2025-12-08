"""
Storage Actions for YAMLEngine.

This module provides cloud storage operations via fsspec, enabling
YAML agents to list, copy, delete, and manage files across multiple
storage backends (S3, GCS, Azure, local filesystem, etc.).

Actions:
    - storage.list: List objects with optional prefix filtering
    - storage.exists: Check if object exists
    - storage.delete: Delete an object
    - storage.copy: Copy between locations (same or cross-provider)
    - storage.info: Get file/object metadata
    - storage.mkdir: Create directory/prefix
    - storage.native: Access provider-specific operations

Supported URI Schemes (via fsspec):
    - file:///path - Local filesystem
    - s3://bucket/path - AWS S3 (requires s3fs)
    - gs://bucket/path - Google Cloud Storage (requires gcsfs)
    - az://container/path - Azure Blob Storage (requires adlfs)
    - memory://path - In-memory filesystem (for testing)
    - http://... / https://... - HTTP/HTTPS (read-only)

Example:
    >>> # List files
    >>> result = registry['storage.list'](state={}, path="s3://my-bucket/data/")
    >>> print(result['files'])

    >>> # Check existence
    >>> result = registry['storage.exists'](state={}, path="gs://bucket/file.json")
    >>> print(result['exists'])

    >>> # Copy across providers
    >>> registry['storage.copy'](
    ...     state={},
    ...     source="s3://source-bucket/file.json",
    ...     destination="gs://dest-bucket/file.json"
    ... )
"""

from typing import Any, Callable, Dict, List, Optional
import fsspec

from .core_actions import _get_filesystem, _normalize_path, _BACKEND_PACKAGES


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register storage actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def storage_list(state, path, detail=False, max_results=None, **kwargs):
        """
        List files/objects at the given path.

        Args:
            state: Current state (for template processing)
            path: Directory/prefix path or URI (e.g., 's3://bucket/prefix/')
            detail: If True, return full file info; if False, just names (default: False)
            max_results: Maximum number of results to return (default: None = all)
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'files': list, 'success': True} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            # List directory contents
            files = fs.ls(fs_path, detail=detail)

            # Apply max_results limit
            if max_results is not None and len(files) > max_results:
                files = files[:max_results]

            return {'files': files, 'success': True, 'count': len(files)}
        except FileNotFoundError:
            return {'success': False, 'error': f"Path not found: {path}", 'error_type': 'not_found'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if 'NoSuchBucket' in error_msg:
                return {'success': False, 'error': f"Bucket not found: {error_msg}", 'error_type': 'bucket_not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'list_error'}

    def storage_exists(state, path, **kwargs):
        """
        Check if a file/object exists.

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., 's3://bucket/key')
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'exists': bool, 'success': True} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            exists = fs.exists(fs_path)
            return {'exists': exists, 'success': True, 'path': path}
        except Exception as e:
            error_msg = str(e)
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'exists_error'}

    def storage_delete(state, path, recursive=False, **kwargs):
        """
        Delete a file/object or directory.

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., 's3://bucket/key')
            recursive: If True, delete directory contents recursively (default: False)
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'deleted': True, 'success': True, 'path': str} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            fs.rm(fs_path, recursive=recursive)
            return {'deleted': True, 'success': True, 'path': path}
        except FileNotFoundError:
            return {'success': False, 'error': f"File not found: {path}", 'error_type': 'not_found'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if '404' in error_msg or 'NoSuchKey' in error_msg:
                return {'success': False, 'error': f"Not found: {error_msg}", 'error_type': 'not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'delete_error'}

    def storage_copy(state, source, destination, **kwargs):
        """
        Copy a file/object to another location.

        Supports same-provider and cross-provider copies (e.g., S3 to GCS).

        Args:
            state: Current state (for template processing)
            source: Source path or URI (e.g., 's3://bucket/src.json')
            destination: Destination path or URI (e.g., 'gs://bucket/dst.json')
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'copied': True, 'success': True, 'source': str, 'destination': str} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        source = _normalize_path(source)
        destination = _normalize_path(destination)

        # Get source filesystem
        src_fs, src_path, err = _get_filesystem(source)
        if err:
            return err

        # Get destination filesystem
        dst_fs, dst_path, err = _get_filesystem(destination)
        if err:
            return err

        try:
            # Check if same filesystem (can use native copy)
            if type(src_fs).__name__ == type(dst_fs).__name__ and hasattr(src_fs, 'protocol'):
                # Same provider - use native copy if available
                try:
                    src_fs.copy(src_path, dst_path)
                    return {'copied': True, 'success': True, 'source': source, 'destination': destination}
                except (NotImplementedError, AttributeError):
                    pass  # Fall through to generic copy

            # Cross-provider copy: read from source, write to destination
            with src_fs.open(src_path, 'rb') as f_src:
                content = f_src.read()

            # Create parent directories on destination
            parent = dst_fs._parent(dst_path) if hasattr(dst_fs, '_parent') else None
            if parent:
                try:
                    dst_fs.makedirs(parent, exist_ok=True)
                except (NotImplementedError, AttributeError):
                    pass

            with dst_fs.open(dst_path, 'wb') as f_dst:
                f_dst.write(content)

            return {'copied': True, 'success': True, 'source': source, 'destination': destination}
        except FileNotFoundError:
            return {'success': False, 'error': f"Source not found: {source}", 'error_type': 'not_found'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied", 'error_type': 'permission_denied'}
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if '404' in error_msg or 'NoSuchKey' in error_msg:
                return {'success': False, 'error': f"Not found: {error_msg}", 'error_type': 'not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'copy_error'}

    def storage_info(state, path, **kwargs):
        """
        Get metadata/info about a file/object.

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., 's3://bucket/key')
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'info': dict, 'success': True} on success - info contains:
                - name: Full path name
                - size: File size in bytes
                - type: 'file' or 'directory'
                - modified: Last modified time (if available)
                - Additional provider-specific metadata
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            info = fs.info(fs_path)
            return {'info': info, 'success': True, 'path': path}
        except FileNotFoundError:
            return {'success': False, 'error': f"File not found: {path}", 'error_type': 'not_found'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if '404' in error_msg or 'NoSuchKey' in error_msg:
                return {'success': False, 'error': f"Not found: {error_msg}", 'error_type': 'not_found'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'info_error'}

    def storage_mkdir(state, path, exist_ok=True, **kwargs):
        """
        Create a directory/prefix.

        Args:
            state: Current state (for template processing)
            path: Directory path or URI (e.g., 's3://bucket/prefix/')
            exist_ok: If True, don't error if directory exists (default: True)
            **kwargs: Additional arguments passed to filesystem

        Returns:
            {'created': True, 'success': True, 'path': str} on success
            {'success': False, 'error': str, 'error_type': str} on failure
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        try:
            fs.makedirs(fs_path, exist_ok=exist_ok)
            return {'created': True, 'success': True, 'path': path}
        except FileExistsError:
            if exist_ok:
                return {'created': False, 'success': True, 'path': path, 'already_exists': True}
            return {'success': False, 'error': f"Directory already exists: {path}", 'error_type': 'exists'}
        except PermissionError:
            return {'success': False, 'error': f"Permission denied: {path}", 'error_type': 'permission_denied'}
        except NotImplementedError:
            # Some filesystems don't support makedirs (object stores don't need them)
            return {'created': True, 'success': True, 'path': path, 'note': 'Directory creation not required for this backend'}
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'mkdir_error'}

    def storage_native(state, path, operation, **kwargs):
        """
        Execute a native filesystem operation not exposed by standard fsspec API.

        This is an escape hatch for provider-specific operations like:
        - S3: put_object_acl, get_object_tagging, etc.
        - GCS: compose, rewrite, etc.
        - Azure: set_blob_metadata, etc.

        Args:
            state: Current state (for template processing)
            path: File path or URI (e.g., 's3://bucket/key')
            operation: Name of the native filesystem method to call
            **kwargs: Arguments passed to the native method

        Returns:
            {'result': any, 'success': True, 'operation': str} on success
            {'success': False, 'error': str, 'error_type': str} on failure

        Example:
            >>> # Set S3 object ACL
            >>> registry['storage.native'](
            ...     state={},
            ...     path="s3://my-bucket/file.json",
            ...     operation="put_object_acl",
            ...     ACL="public-read"
            ... )
        """
        path = _normalize_path(path)
        fs, fs_path, err = _get_filesystem(path)
        if err:
            return err

        # Helper to safely check if attr is callable (some properties may raise)
        def _is_callable_attr(obj, name):
            try:
                attr = getattr(obj, name, None)
                return callable(attr)
            except Exception:
                return False

        try:
            # Check if the operation exists on the filesystem
            if not hasattr(fs, operation):
                available_ops = [m for m in dir(fs) if not m.startswith('_') and _is_callable_attr(fs, m)]
                return {
                    'success': False,
                    'error': f"Operation '{operation}' not available on {type(fs).__name__}",
                    'error_type': 'operation_not_found',
                    'available_operations': available_ops[:20]  # Limit to avoid huge output
                }

            # Get and call the method
            method = getattr(fs, operation)

            # Verify it's actually callable
            if not callable(method):
                return {
                    'success': False,
                    'error': f"'{operation}' is not a callable method on {type(fs).__name__}",
                    'error_type': 'operation_not_callable'
                }

            # Some operations need the path as first argument, some don't
            # Try with path first, then without
            try:
                result = method(fs_path, **kwargs)
            except TypeError:
                # Maybe the operation doesn't take a path
                result = method(**kwargs)

            return {'result': result, 'success': True, 'operation': operation, 'path': path}
        except AttributeError as e:
            # Handle case where getattr fails
            available_ops = [m for m in dir(fs) if not m.startswith('_') and _is_callable_attr(fs, m)]
            return {
                'success': False,
                'error': f"Operation '{operation}' not available on {type(fs).__name__}",
                'error_type': 'operation_not_found',
                'available_operations': available_ops[:20]
            }
        except Exception as e:
            error_msg = str(e)
            if '403' in error_msg or 'Forbidden' in error_msg:
                return {'success': False, 'error': f"Permission denied: {error_msg}", 'error_type': 'permission_denied'}
            if 'credential' in error_msg.lower() or 'auth' in error_msg.lower():
                return {'success': False, 'error': f"Missing credentials: {error_msg}", 'error_type': 'missing_credentials'}
            return {'success': False, 'error': error_msg, 'error_type': 'native_error'}

    # Register all storage actions with dual namespace
    registry['storage.list'] = storage_list
    registry['actions.storage_list'] = storage_list

    registry['storage.exists'] = storage_exists
    registry['actions.storage_exists'] = storage_exists

    registry['storage.delete'] = storage_delete
    registry['actions.storage_delete'] = storage_delete

    registry['storage.copy'] = storage_copy
    registry['actions.storage_copy'] = storage_copy

    registry['storage.info'] = storage_info
    registry['actions.storage_info'] = storage_info

    registry['storage.mkdir'] = storage_mkdir
    registry['actions.storage_mkdir'] = storage_mkdir

    registry['storage.native'] = storage_native
    registry['actions.storage_native'] = storage_native
