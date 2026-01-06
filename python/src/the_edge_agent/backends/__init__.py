"""
Backend client factories for external services.

TEA-BUILTIN-015.2: Firestore client factory for direct document operations.

Provides lazy-initialized, cached clients for:
- Firestore (Google Cloud Firestore / Firebase)

Usage:
    >>> from the_edge_agent.backends import get_firestore_client
    >>> client = get_firestore_client(project="my-project")
    >>> doc = client.collection("users").document("user123").get()
"""

from .firestore_client import (
    get_firestore_client,
    FirestoreClientWrapper,
    clear_firestore_cache,
    FIRESTORE_AVAILABLE,
)

__all__ = [
    "get_firestore_client",
    "FirestoreClientWrapper",
    "clear_firestore_cache",
    "FIRESTORE_AVAILABLE",
]
