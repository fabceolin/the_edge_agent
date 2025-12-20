"""
Metadata Storage Backend Module (TEA-BUILTIN-006).

Provides abstract interface and implementations for metadata/catalog storage.
Primary implementation: FirestoreMetadataStore for Firebase Firestore.

The MetadataStore ABC defines operations for:
- Document CRUD (set, get, update, delete)
- Collection queries with filtering, ordering, and pagination
- Transactional operations for atomic multi-document updates

Example:
    >>> from the_edge_agent.memory.metadata import create_metadata_store
    >>>
    >>> # Firestore (default)
    >>> store = create_metadata_store("firestore")
    >>>
    >>> # Use the store
    >>> result = store.set_document("collection", "doc_id", {"field": "value"})
    >>> result = store.get_document("collection", "doc_id")
    >>> store.close()
"""

from .base import MetadataStore, MetadataQuery, OrderDirection

# Availability flags
FIRESTORE_AVAILABLE = False

try:
    from .firestore import FirestoreMetadataStore
    FIRESTORE_AVAILABLE = True
except ImportError:
    FirestoreMetadataStore = None  # type: ignore


# Registry of metadata store classes
_METADATA_REGISTRY: dict = {}


def register_metadata_store(name: str, store_class: type) -> None:
    """Register a metadata store class."""
    _METADATA_REGISTRY[name.lower()] = store_class


def get_registered_metadata_stores() -> list:
    """Get list of registered metadata store names."""
    return list(_METADATA_REGISTRY.keys())


def create_metadata_store(
    store_type: str = "firestore",
    **kwargs
) -> MetadataStore:
    """
    Factory function to create a metadata store instance.

    Args:
        store_type: Type of store ("firestore")
        **kwargs: Store-specific configuration

    Returns:
        MetadataStore instance

    Raises:
        ValueError: If store_type is not registered
        ImportError: If required dependencies are not installed
    """
    store_name = store_type.lower()

    if store_name not in _METADATA_REGISTRY:
        available = ", ".join(get_registered_metadata_stores()) or "none"
        raise ValueError(
            f"Unknown metadata store type: '{store_type}'. "
            f"Available stores: {available}"
        )

    store_class = _METADATA_REGISTRY[store_name]
    return store_class(**kwargs)


# Auto-register available stores
if FIRESTORE_AVAILABLE:
    register_metadata_store("firestore", FirestoreMetadataStore)


__all__ = [
    "MetadataStore",
    "MetadataQuery",
    "OrderDirection",
    "create_metadata_store",
    "register_metadata_store",
    "get_registered_metadata_stores",
    "FIRESTORE_AVAILABLE",
]

if FIRESTORE_AVAILABLE:
    __all__.append("FirestoreMetadataStore")
