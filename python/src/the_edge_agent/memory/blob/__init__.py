"""
Blob Storage Backend Module (TEA-BUILTIN-006).

Provides abstract interface and implementations for cloud blob storage.
Primary implementation: GCSBlobStorage for Google Cloud Storage.

The BlobStorage ABC defines operations for:
- Upload/download content to/from cloud storage
- Delete objects
- List objects with prefix filtering
- Copy objects between locations

Example:
    >>> from the_edge_agent.memory.blob import create_blob_storage
    >>>
    >>> # GCS (default)
    >>> storage = create_blob_storage("gcs", bucket="my-bucket")
    >>>
    >>> # Use the storage
    >>> result = storage.upload("path/to/file.yaml", content)
    >>> result = storage.download("path/to/file.yaml")
    >>> storage.close()
"""

from .base import BlobStorage, BlobInfo

# Availability flags
GCS_AVAILABLE = False

try:
    from .gcs import GCSBlobStorage
    GCS_AVAILABLE = True
except ImportError:
    GCSBlobStorage = None  # type: ignore


# Registry of blob storage classes
_BLOB_REGISTRY: dict = {}


def register_blob_storage(name: str, storage_class: type) -> None:
    """Register a blob storage class."""
    _BLOB_REGISTRY[name.lower()] = storage_class


def get_registered_blob_storages() -> list:
    """Get list of registered blob storage names."""
    return list(_BLOB_REGISTRY.keys())


def create_blob_storage(
    storage_type: str = "gcs",
    **kwargs
) -> BlobStorage:
    """
    Factory function to create a blob storage instance.

    Args:
        storage_type: Type of storage ("gcs")
        **kwargs: Storage-specific configuration

    Returns:
        BlobStorage instance

    Raises:
        ValueError: If storage_type is not registered
        ImportError: If required dependencies are not installed
    """
    storage_name = storage_type.lower()

    if storage_name not in _BLOB_REGISTRY:
        available = ", ".join(get_registered_blob_storages()) or "none"
        raise ValueError(
            f"Unknown blob storage type: '{storage_type}'. "
            f"Available storages: {available}"
        )

    storage_class = _BLOB_REGISTRY[storage_name]
    return storage_class(**kwargs)


# Auto-register available storages
if GCS_AVAILABLE:
    register_blob_storage("gcs", GCSBlobStorage)
    register_blob_storage("firebase", GCSBlobStorage)  # Alias for Firebase Storage


__all__ = [
    "BlobStorage",
    "BlobInfo",
    "create_blob_storage",
    "register_blob_storage",
    "get_registered_blob_storages",
    "GCS_AVAILABLE",
]

if GCS_AVAILABLE:
    __all__.append("GCSBlobStorage")
