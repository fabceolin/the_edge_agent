"""
BlobStorage Abstract Base Class (TEA-BUILTIN-006).

Defines the interface for cloud blob storage backends.
Extracted from firebase/functions-agents/actions/cloud_memory.py.

The BlobStorage ABC supports:
- Upload/download content to/from cloud storage
- Delete objects
- List objects with prefix filtering
- Copy objects between locations
- Move operations (copy + delete)

Error Response Format:
    All methods return dictionaries with consistent format:
    - Success: {"success": True, ...additional fields...}
    - Failure: {"success": False, "error": str, "error_type": str}

Error Types:
    - not_found: Object not found
    - already_exists: Object already exists (for conditional uploads)
    - validation_error: Invalid input parameters
    - permission_denied: Authorization failure
    - connection_error: Backend connection issues
    - storage_full: Quota exceeded
    - backend_not_installed: Required library not available
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Optional, Union


@dataclass
class BlobInfo:
    """
    Metadata about a blob/object.

    Attributes:
        name: Object name/path
        size: Size in bytes
        content_type: MIME type
        created: Creation timestamp
        updated: Last modified timestamp
        etag: Entity tag for versioning
        metadata: Custom metadata dict
    """
    name: str
    size: int
    content_type: str
    created: Optional[datetime] = None
    updated: Optional[datetime] = None
    etag: Optional[str] = None
    metadata: Optional[Dict[str, str]] = None


class BlobStorage(ABC):
    """
    Abstract base class for blob storage backends.

    Implementations must be thread-safe for use in parallel execution.
    All methods return dictionaries with consistent error format.
    """

    # =========================================================================
    # UPLOAD OPERATIONS
    # =========================================================================

    @abstractmethod
    def upload(
        self,
        path: str,
        content: Union[str, bytes],
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """
        Upload content to storage.

        Args:
            path: Object path in storage
            content: Content to upload (str or bytes)
            content_type: MIME type (auto-detected if not provided)
            metadata: Custom metadata to attach
            overwrite: If False, fail if object exists

        Returns:
            {
                "success": True,
                "path": str,
                "uri": str (full URI like gs://bucket/path),
                "size": int,
                "etag": str
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def upload_file(
        self,
        path: str,
        local_path: str,
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """
        Upload a file from local filesystem.

        Args:
            path: Object path in storage
            local_path: Path to local file
            content_type: MIME type (auto-detected if not provided)
            metadata: Custom metadata to attach
            overwrite: If False, fail if object exists

        Returns:
            Same as upload()
        """
        pass

    # =========================================================================
    # DOWNLOAD OPERATIONS
    # =========================================================================

    @abstractmethod
    def download(
        self,
        path: str
    ) -> Dict[str, Any]:
        """
        Download content from storage.

        Args:
            path: Object path in storage

        Returns:
            {
                "success": True,
                "content": str|bytes,
                "content_type": str,
                "size": int
            }
            or {"success": False, "error": str, "error_type": "not_found"}
        """
        pass

    @abstractmethod
    def download_file(
        self,
        path: str,
        local_path: str
    ) -> Dict[str, Any]:
        """
        Download to local filesystem.

        Args:
            path: Object path in storage
            local_path: Path to save locally

        Returns:
            {"success": True, "path": str, "local_path": str, "size": int}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    # =========================================================================
    # DELETE OPERATIONS
    # =========================================================================

    @abstractmethod
    def delete(
        self,
        path: str
    ) -> Dict[str, Any]:
        """
        Delete an object.

        Args:
            path: Object path in storage

        Returns:
            {"success": True, "path": str, "deleted": True}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def delete_many(
        self,
        paths: List[str]
    ) -> Dict[str, Any]:
        """
        Delete multiple objects.

        Args:
            paths: List of object paths

        Returns:
            {
                "success": True,
                "deleted": int (count),
                "failed": int (count),
                "errors": list (failed paths with errors)
            }

        Default implementation calls delete() in loop.
        Override for batch optimization.
        """
        deleted = 0
        failed = 0
        errors = []

        for path in paths:
            result = self.delete(path)
            if result.get("success"):
                deleted += 1
            else:
                failed += 1
                errors.append({"path": path, "error": result.get("error")})

        return {
            "success": failed == 0,
            "deleted": deleted,
            "failed": failed,
            "errors": errors
        }

    # =========================================================================
    # QUERY OPERATIONS
    # =========================================================================

    @abstractmethod
    def exists(
        self,
        path: str
    ) -> Dict[str, Any]:
        """
        Check if an object exists.

        Args:
            path: Object path in storage

        Returns:
            {"success": True, "exists": bool, "path": str}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def info(
        self,
        path: str
    ) -> Dict[str, Any]:
        """
        Get object metadata.

        Args:
            path: Object path in storage

        Returns:
            {
                "success": True,
                "info": BlobInfo (as dict),
                "path": str
            }
            or {"success": False, "error": str, "error_type": "not_found"}
        """
        pass

    @abstractmethod
    def list(
        self,
        prefix: str = "",
        delimiter: Optional[str] = None,
        max_results: int = 1000
    ) -> Dict[str, Any]:
        """
        List objects with optional prefix filtering.

        Args:
            prefix: Path prefix to filter by
            delimiter: Delimiter for hierarchical listing (e.g., "/")
            max_results: Maximum objects to return

        Returns:
            {
                "success": True,
                "objects": list of BlobInfo (as dicts),
                "prefixes": list of common prefixes (if delimiter used),
                "count": int
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    # =========================================================================
    # COPY/MOVE OPERATIONS
    # =========================================================================

    @abstractmethod
    def copy(
        self,
        source_path: str,
        dest_path: str,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """
        Copy an object to a new location.

        Args:
            source_path: Source object path
            dest_path: Destination object path
            overwrite: If False, fail if destination exists

        Returns:
            {
                "success": True,
                "source": str,
                "destination": str,
                "size": int
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def move(
        self,
        source_path: str,
        dest_path: str,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """
        Move an object (copy + delete).

        Args:
            source_path: Source object path
            dest_path: Destination object path
            overwrite: If False, fail if destination exists

        Returns:
            {
                "success": True,
                "source": str,
                "destination": str,
                "size": int
            }
            or {"success": False, "error": str, "error_type": str}

        Default implementation uses copy() then delete().
        Override for atomic move if backend supports it.
        """
        # Copy first
        copy_result = self.copy(source_path, dest_path, overwrite)
        if not copy_result.get("success"):
            return copy_result

        # Delete source
        delete_result = self.delete(source_path)
        if not delete_result.get("success"):
            # Log warning but don't fail - copy succeeded
            import logging
            logging.warning(
                f"Move: copy succeeded but delete failed for {source_path}: "
                f"{delete_result.get('error')}"
            )

        return {
            "success": True,
            "source": source_path,
            "destination": dest_path,
            "size": copy_result.get("size", 0)
        }

    # =========================================================================
    # UTILITIES
    # =========================================================================

    @abstractmethod
    def get_uri(self, path: str) -> str:
        """
        Get full URI for an object path.

        Args:
            path: Object path

        Returns:
            Full URI (e.g., "gs://bucket/path")
        """
        pass

    @abstractmethod
    def parse_uri(self, uri: str) -> Dict[str, Any]:
        """
        Parse a storage URI into components.

        Args:
            uri: Full storage URI (e.g., "gs://bucket/path")

        Returns:
            {"bucket": str, "path": str}
            or {"error": str} if invalid
        """
        pass

    def detect_content_type(self, path: str) -> str:
        """
        Detect content type from path extension.

        Args:
            path: File path

        Returns:
            MIME type string
        """
        import mimetypes
        content_type, _ = mimetypes.guess_type(path)
        return content_type or "application/octet-stream"

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    @abstractmethod
    def close(self) -> None:
        """
        Close the storage client and release resources.

        Should be called when the storage is no longer needed.
        Safe to call multiple times.
        """
        pass

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False
