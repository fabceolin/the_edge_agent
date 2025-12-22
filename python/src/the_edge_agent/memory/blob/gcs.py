"""
GCSBlobStorage Implementation (TEA-BUILTIN-006).

Implements BlobStorage ABC using Google Cloud Storage.
Migrated from firebase/functions-agents/actions/cloud_memory.py.

Requirements:
    pip install google-cloud-storage

Environment:
    GOOGLE_APPLICATION_CREDENTIALS: Path to service account JSON
    FIREBASE_STORAGE_BUCKET: Default bucket name (optional)
    FIREBASE_STORAGE_EMULATOR_HOST: Emulator host for local dev (optional)

Usage:
    >>> from the_edge_agent.memory.blob import GCSBlobStorage
    >>>
    >>> storage = GCSBlobStorage(bucket="my-bucket")
    >>> result = storage.upload("path/to/file.yaml", content)
    >>> result = storage.download("path/to/file.yaml")
"""

import os
import logging
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Union

try:
    from google.cloud import storage
    GCS_AVAILABLE = True
except ImportError:
    GCS_AVAILABLE = False
    storage = None  # type: ignore

from tenacity import retry, stop_after_attempt, wait_exponential, retry_if_exception

from .base import BlobStorage, BlobInfo


logger = logging.getLogger(__name__)


def _is_transient_error(exception: Exception) -> bool:
    """Check if an error is transient and should be retried."""
    error_msg = str(exception).lower()
    transient_indicators = [
        "503", "service unavailable",
        "timeout", "timed out",
        "temporarily unavailable",
        "retry", "try again",
        "connection reset", "connection refused",
        "network", "dns"
    ]
    return any(indicator in error_msg for indicator in transient_indicators)


class GCSBlobStorage(BlobStorage):
    """
    Google Cloud Storage implementation of BlobStorage.

    Supports both production GCS and Firebase Storage Emulator.
    Thread-safe via GCS client's internal connection pooling.

    Attributes:
        bucket_name: Default bucket name
        project_id: GCP project ID
        _client: GCS client instance
    """

    def __init__(
        self,
        bucket: Optional[str] = None,
        project_id: Optional[str] = None
    ):
        """
        Initialize GCS blob storage.

        Args:
            bucket: Default bucket name.
                   Falls back to FIREBASE_STORAGE_BUCKET env var.
            project_id: GCP project ID.
                       Falls back to FIREBASE_PROJECT_ID env var.

        Raises:
            ImportError: If google-cloud-storage is not installed
        """
        if not GCS_AVAILABLE:
            raise ImportError(
                "google-cloud-storage is required for GCSBlobStorage. "
                "Install with: pip install google-cloud-storage"
            )

        self.bucket_name = bucket or os.environ.get(
            "FIREBASE_STORAGE_BUCKET",
            "rankellix-law.firebasestorage.app"
        )
        self.project_id = project_id or os.environ.get(
            "FIREBASE_PROJECT_ID",
            "demo-rankellix-dev"
        )
        self._client = None
        self._bucket = None

    def _get_client(self):
        """Get GCS client with emulator support."""
        if self._client is None:
            # Check for Storage emulator
            emulator_host = os.environ.get("FIREBASE_STORAGE_EMULATOR_HOST")
            if emulator_host:
                # Replace 0.0.0.0 with localhost for client connections
                emulator_host = emulator_host.replace("0.0.0.0", "localhost")
                os.environ["STORAGE_EMULATOR_HOST"] = f"http://{emulator_host}"

            self._client = storage.Client(project=self.project_id)

        return self._client

    def _get_bucket(self):
        """Get bucket reference."""
        if self._bucket is None:
            self._bucket = self._get_client().bucket(self.bucket_name)
        return self._bucket

    # =========================================================================
    # UPLOAD OPERATIONS
    # =========================================================================

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=1, max=10),
        retry=retry_if_exception(_is_transient_error)
    )
    def upload(
        self,
        path: str,
        content: Union[str, bytes],
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """Upload content to GCS."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            # Check if exists when overwrite=False
            if not overwrite and blob.exists():
                return {
                    "success": False,
                    "error": f"Object already exists: {path}",
                    "error_type": "already_exists"
                }

            # Detect content type if not provided
            if content_type is None:
                content_type = self.detect_content_type(path)

            # Convert string to bytes
            if isinstance(content, str):
                content_bytes = content.encode('utf-8')
            else:
                content_bytes = content

            # Set custom metadata
            if metadata:
                blob.metadata = metadata

            blob.upload_from_string(content_bytes, content_type=content_type)

            logger.debug(f"Uploaded {len(content_bytes)} bytes to {path}")

            return {
                "success": True,
                "path": path,
                "uri": self.get_uri(path),
                "size": len(content_bytes),
                "etag": blob.etag
            }

        except Exception as e:
            logger.error(f"Upload failed for {path}: {e}")
            if _is_transient_error(e):
                raise  # Let retry handle it
            return {
                "success": False,
                "error": f"Upload failed: {str(e)}",
                "error_type": "connection_error"
            }

    def upload_file(
        self,
        path: str,
        local_path: str,
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """Upload a file from local filesystem."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            # Check if exists when overwrite=False
            if not overwrite and blob.exists():
                return {
                    "success": False,
                    "error": f"Object already exists: {path}",
                    "error_type": "already_exists"
                }

            # Detect content type if not provided
            if content_type is None:
                content_type = self.detect_content_type(local_path)

            # Set custom metadata
            if metadata:
                blob.metadata = metadata

            blob.upload_from_filename(local_path, content_type=content_type)

            # Get file size
            size = os.path.getsize(local_path)

            logger.debug(f"Uploaded file {local_path} to {path} ({size} bytes)")

            return {
                "success": True,
                "path": path,
                "uri": self.get_uri(path),
                "size": size,
                "etag": blob.etag
            }

        except FileNotFoundError:
            return {
                "success": False,
                "error": f"Local file not found: {local_path}",
                "error_type": "not_found"
            }
        except Exception as e:
            logger.error(f"File upload failed for {local_path} -> {path}: {e}")
            return {
                "success": False,
                "error": f"Upload failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # DOWNLOAD OPERATIONS
    # =========================================================================

    def download(
        self,
        path: str
    ) -> Dict[str, Any]:
        """Download content from GCS."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            if not blob.exists():
                return {
                    "success": False,
                    "error": f"Object not found: {path}",
                    "error_type": "not_found"
                }

            content = blob.download_as_bytes()

            # Try to decode as UTF-8, otherwise return bytes
            try:
                content_str = content.decode('utf-8')
                return {
                    "success": True,
                    "content": content_str,
                    "content_type": blob.content_type,
                    "size": len(content)
                }
            except UnicodeDecodeError:
                return {
                    "success": True,
                    "content": content,
                    "content_type": blob.content_type,
                    "size": len(content)
                }

        except Exception as e:
            logger.error(f"Download failed for {path}: {e}")
            return {
                "success": False,
                "error": f"Download failed: {str(e)}",
                "error_type": "connection_error"
            }

    def download_file(
        self,
        path: str,
        local_path: str
    ) -> Dict[str, Any]:
        """Download to local filesystem."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            if not blob.exists():
                return {
                    "success": False,
                    "error": f"Object not found: {path}",
                    "error_type": "not_found"
                }

            blob.download_to_filename(local_path)

            size = os.path.getsize(local_path)

            logger.debug(f"Downloaded {path} to {local_path} ({size} bytes)")

            return {
                "success": True,
                "path": path,
                "local_path": local_path,
                "size": size
            }

        except Exception as e:
            logger.error(f"File download failed for {path} -> {local_path}: {e}")
            return {
                "success": False,
                "error": f"Download failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # DELETE OPERATIONS
    # =========================================================================

    def delete(
        self,
        path: str
    ) -> Dict[str, Any]:
        """Delete an object from GCS."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            blob.delete()

            logger.debug(f"Deleted {path}")

            return {
                "success": True,
                "path": path,
                "deleted": True
            }

        except Exception as e:
            error_str = str(e).lower()
            if "not found" in error_str or "404" in error_str:
                return {
                    "success": False,
                    "error": f"Object not found: {path}",
                    "error_type": "not_found"
                }
            logger.error(f"Delete failed for {path}: {e}")
            return {
                "success": False,
                "error": f"Delete failed: {str(e)}",
                "error_type": "connection_error"
            }

    def delete_many(
        self,
        paths: List[str]
    ) -> Dict[str, Any]:
        """Delete multiple objects using batch API."""
        if not paths:
            return {"success": True, "deleted": 0, "failed": 0, "errors": []}

        try:
            bucket = self._get_bucket()

            # GCS supports batch delete
            with self._get_client().batch():
                for path in paths:
                    blob = bucket.blob(path)
                    blob.delete()

            logger.debug(f"Batch deleted {len(paths)} objects")

            return {
                "success": True,
                "deleted": len(paths),
                "failed": 0,
                "errors": []
            }

        except Exception as e:
            # Fall back to sequential delete
            logger.warning(f"Batch delete failed, falling back to sequential: {e}")
            return super().delete_many(paths)

    # =========================================================================
    # QUERY OPERATIONS
    # =========================================================================

    def exists(
        self,
        path: str
    ) -> Dict[str, Any]:
        """Check if an object exists in GCS."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            return {
                "success": True,
                "exists": blob.exists(),
                "path": path
            }

        except Exception as e:
            logger.error(f"Exists check failed for {path}: {e}")
            return {
                "success": False,
                "error": f"Exists check failed: {str(e)}",
                "error_type": "connection_error"
            }

    def info(
        self,
        path: str
    ) -> Dict[str, Any]:
        """Get object metadata from GCS."""
        try:
            bucket = self._get_bucket()
            blob = bucket.blob(path)

            # Reload to get metadata
            blob.reload()

            if not blob.exists():
                return {
                    "success": False,
                    "error": f"Object not found: {path}",
                    "error_type": "not_found"
                }

            info = BlobInfo(
                name=blob.name,
                size=blob.size or 0,
                content_type=blob.content_type or "application/octet-stream",
                created=blob.time_created,
                updated=blob.updated,
                etag=blob.etag,
                metadata=blob.metadata
            )

            return {
                "success": True,
                "info": {
                    "name": info.name,
                    "size": info.size,
                    "content_type": info.content_type,
                    "created": info.created.isoformat() if info.created else None,
                    "updated": info.updated.isoformat() if info.updated else None,
                    "etag": info.etag,
                    "metadata": info.metadata
                },
                "path": path
            }

        except Exception as e:
            error_str = str(e).lower()
            if "not found" in error_str or "404" in error_str:
                return {
                    "success": False,
                    "error": f"Object not found: {path}",
                    "error_type": "not_found"
                }
            logger.error(f"Info failed for {path}: {e}")
            return {
                "success": False,
                "error": f"Info failed: {str(e)}",
                "error_type": "connection_error"
            }

    def list(
        self,
        prefix: str = "",
        delimiter: Optional[str] = None,
        max_results: int = 1000
    ) -> Dict[str, Any]:
        """List objects with optional prefix filtering."""
        try:
            bucket = self._get_bucket()

            blobs = bucket.list_blobs(
                prefix=prefix,
                delimiter=delimiter,
                max_results=max_results
            )

            objects = []
            prefixes = []

            for blob in blobs:
                objects.append({
                    "name": blob.name,
                    "size": blob.size or 0,
                    "content_type": blob.content_type or "application/octet-stream",
                    "updated": blob.updated.isoformat() if blob.updated else None
                })

            # Get common prefixes (for hierarchical listing)
            if delimiter and hasattr(blobs, 'prefixes'):
                prefixes = list(blobs.prefixes)

            logger.debug(f"Listed {len(objects)} objects with prefix '{prefix}'")

            return {
                "success": True,
                "objects": objects,
                "prefixes": prefixes,
                "count": len(objects)
            }

        except Exception as e:
            logger.error(f"List failed for prefix '{prefix}': {e}")
            return {
                "success": False,
                "error": f"List failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # COPY/MOVE OPERATIONS
    # =========================================================================

    def copy(
        self,
        source_path: str,
        dest_path: str,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """Copy an object within GCS."""
        try:
            bucket = self._get_bucket()
            source_blob = bucket.blob(source_path)
            dest_blob = bucket.blob(dest_path)

            # Check if source exists
            if not source_blob.exists():
                return {
                    "success": False,
                    "error": f"Source not found: {source_path}",
                    "error_type": "not_found"
                }

            # Check if dest exists when overwrite=False
            if not overwrite and dest_blob.exists():
                return {
                    "success": False,
                    "error": f"Destination already exists: {dest_path}",
                    "error_type": "already_exists"
                }

            # Perform copy
            bucket.copy_blob(source_blob, bucket, dest_path)

            # Get size
            source_blob.reload()
            size = source_blob.size or 0

            logger.debug(f"Copied {source_path} to {dest_path} ({size} bytes)")

            return {
                "success": True,
                "source": source_path,
                "destination": dest_path,
                "size": size
            }

        except Exception as e:
            logger.error(f"Copy failed {source_path} -> {dest_path}: {e}")
            return {
                "success": False,
                "error": f"Copy failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # UTILITIES
    # =========================================================================

    def get_uri(self, path: str) -> str:
        """Get full GCS URI for path."""
        return f"gs://{self.bucket_name}/{path}"

    def parse_uri(self, uri: str) -> Dict[str, Any]:
        """Parse GCS URI into components."""
        if not uri.startswith("gs://"):
            return {"error": f"Invalid GCS URI: {uri}"}

        parts = uri.replace("gs://", "").split("/", 1)
        if len(parts) != 2:
            return {"error": f"Invalid GCS URI format: {uri}"}

        return {
            "bucket": parts[0],
            "path": parts[1]
        }

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    def close(self) -> None:
        """Close the GCS client."""
        # GCS client doesn't require explicit cleanup
        self._client = None
        self._bucket = None
        logger.debug("GCSBlobStorage closed")
