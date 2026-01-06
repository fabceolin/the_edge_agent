"""
Firestore Client Factory (TEA-BUILTIN-015.2).

Provides lazy-initialized, cached Firestore client instances for YAML agent
workflows. Supports both production Firebase and the Firestore emulator.

Features:
- Lazy initialization: Client is only created on first use
- Caching: One client per project ID, shared across actions
- Emulator support: Auto-detects FIRESTORE_EMULATOR_HOST environment variable
- Credentials support: Uses GOOGLE_APPLICATION_CREDENTIALS or explicit path

Requirements:
    pip install firebase-admin

Environment Variables:
    FIRESTORE_EMULATOR_HOST: Firestore emulator address (e.g., "localhost:8080")
    GOOGLE_APPLICATION_CREDENTIALS: Path to service account JSON file
    FIREBASE_PROJECT_ID: Default project ID if not specified

Usage:
    >>> from the_edge_agent.backends import get_firestore_client
    >>> client = get_firestore_client(project="my-project")
    >>> doc = client.collection("users").document("user123").get()

Example YAML:
    settings:
      firestore:
        project: "${FIREBASE_PROJECT_ID}"
        emulator_host: "${FIRESTORE_EMULATOR_HOST:-}"
        credentials_path: "${GOOGLE_APPLICATION_CREDENTIALS:-}"
"""

import logging
import os
from threading import Lock
from typing import Any, Dict, Optional

logger = logging.getLogger(__name__)

# Check if firebase-admin is available
try:
    import firebase_admin
    from firebase_admin import credentials, firestore

    FIRESTORE_AVAILABLE = True
except ImportError:
    FIRESTORE_AVAILABLE = False
    firebase_admin = None  # type: ignore
    credentials = None  # type: ignore
    firestore = None  # type: ignore

# Client cache: project_id -> FirestoreClientWrapper
_client_cache: Dict[str, "FirestoreClientWrapper"] = {}
_cache_lock = Lock()


class FirestoreClientWrapper:
    """
    Lazy-initialized Firestore client wrapper.

    This wrapper provides lazy initialization of the Firestore client,
    creating the client only when first accessed. It supports:
    - Multiple Firebase apps via unique project IDs
    - Emulator detection via environment variable
    - Explicit credentials path configuration

    Attributes:
        _project_id: Google Cloud/Firebase project ID
        _emulator_host: Optional emulator host address
        _credentials_path: Optional path to service account JSON
        _client: Lazily-initialized Firestore client
        _app: Firebase app instance for this project
        _initialized: Whether the client has been initialized
    """

    def __init__(
        self,
        project_id: Optional[str] = None,
        emulator_host: Optional[str] = None,
        credentials_path: Optional[str] = None,
    ):
        """
        Initialize the Firestore client wrapper.

        Args:
            project_id: Google Cloud project ID. If None, uses
                       FIREBASE_PROJECT_ID env var or default from credentials.
            emulator_host: Firestore emulator address (e.g., "localhost:8080").
                          If None, checks FIRESTORE_EMULATOR_HOST env var.
            credentials_path: Path to service account JSON file.
                             If None, uses GOOGLE_APPLICATION_CREDENTIALS.

        Raises:
            ImportError: If firebase-admin is not installed
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin is required for Firestore operations. "
                "Install with: pip install firebase-admin"
            )

        # Resolve project ID
        self._project_id = project_id or os.environ.get("FIREBASE_PROJECT_ID")

        # Resolve emulator host
        self._emulator_host = emulator_host or os.environ.get("FIRESTORE_EMULATOR_HOST")

        # Resolve credentials path
        self._credentials_path = credentials_path or os.environ.get(
            "GOOGLE_APPLICATION_CREDENTIALS"
        )

        self._client = None
        self._app = None
        self._initialized = False
        self._init_lock = Lock()

    def _ensure_client(self) -> Any:
        """
        Lazily initialize the Firestore client.

        Creates a Firebase app and Firestore client on first access.
        Thread-safe via lock.

        Returns:
            Firestore client instance
        """
        if self._initialized:
            return self._client

        with self._init_lock:
            # Double-check after acquiring lock
            if self._initialized:
                return self._client

            # Set emulator host environment variable if specified
            # This must be done BEFORE creating the client
            if self._emulator_host:
                os.environ["FIRESTORE_EMULATOR_HOST"] = self._emulator_host
                logger.info(f"Using Firestore emulator at: {self._emulator_host}")

            # Create app name unique to this project
            app_name = self._project_id or "default"
            app_suffix = f"_firestore_{id(self)}"
            full_app_name = f"{app_name}{app_suffix}"

            # Check if app already exists
            try:
                self._app = firebase_admin.get_app(full_app_name)
                logger.debug(f"Reusing existing Firebase app: {full_app_name}")
            except ValueError:
                # App doesn't exist, create it
                cred = None
                options = {}

                # Configure credentials
                if self._credentials_path and os.path.exists(self._credentials_path):
                    cred = credentials.Certificate(self._credentials_path)
                    logger.debug(f"Using credentials from: {self._credentials_path}")
                elif self._emulator_host:
                    # For emulator, no credentials needed
                    cred = None
                    logger.debug("Using emulator without credentials")
                else:
                    # Try default credentials
                    try:
                        cred = credentials.ApplicationDefault()
                        logger.debug("Using application default credentials")
                    except Exception as e:
                        logger.warning(f"No credentials available: {e}")

                # Set project ID in options if specified
                if self._project_id:
                    options["projectId"] = self._project_id

                # Initialize app
                self._app = firebase_admin.initialize_app(
                    credential=cred,
                    options=options if options else None,
                    name=full_app_name,
                )
                logger.info(
                    f"Initialized Firebase app: {full_app_name} "
                    f"(project: {self._project_id or 'default'})"
                )

            # Create Firestore client from app
            self._client = firestore.client(app=self._app)
            self._initialized = True
            logger.debug("Firestore client initialized")

            return self._client

    @property
    def client(self) -> Any:
        """
        Get the Firestore client, initializing if needed.

        Returns:
            Firestore client instance
        """
        return self._ensure_client()

    @property
    def project_id(self) -> Optional[str]:
        """Get the project ID."""
        return self._project_id

    @property
    def emulator_host(self) -> Optional[str]:
        """Get the emulator host if configured."""
        return self._emulator_host

    @property
    def is_emulator(self) -> bool:
        """Check if using the Firestore emulator."""
        return bool(self._emulator_host)

    def collection(self, name: str) -> Any:
        """
        Get a collection reference.

        Args:
            name: Collection name (can include subcollection path)

        Returns:
            Firestore CollectionReference
        """
        return self.client.collection(name)

    def document(self, path: str) -> Any:
        """
        Get a document reference by full path.

        Args:
            path: Document path (e.g., "users/user123" or
                  "users/user123/posts/post456")

        Returns:
            Firestore DocumentReference
        """
        return self.client.document(path)

    def batch(self) -> Any:
        """
        Create a new write batch.

        Returns:
            Firestore WriteBatch
        """
        return self.client.batch()

    def transaction(self) -> Any:
        """
        Create a new transaction.

        Returns:
            Firestore Transaction
        """
        return self.client.transaction()

    def close(self) -> None:
        """
        Close the client and cleanup resources.

        Safe to call multiple times.
        """
        if self._app is not None:
            try:
                firebase_admin.delete_app(self._app)
                logger.debug(f"Deleted Firebase app: {self._app.name}")
            except Exception as e:
                logger.warning(f"Error closing Firebase app: {e}")
            finally:
                self._app = None
                self._client = None
                self._initialized = False


def get_firestore_client(
    project: Optional[str] = None,
    emulator_host: Optional[str] = None,
    credentials_path: Optional[str] = None,
) -> FirestoreClientWrapper:
    """
    Get or create a cached Firestore client.

    Creates a new client if one doesn't exist for the given project,
    otherwise returns the cached client. Thread-safe.

    Args:
        project: Google Cloud project ID. Uses FIREBASE_PROJECT_ID if None.
        emulator_host: Firestore emulator address. Uses
                      FIRESTORE_EMULATOR_HOST if None.
        credentials_path: Path to service account JSON. Uses
                         GOOGLE_APPLICATION_CREDENTIALS if None.

    Returns:
        FirestoreClientWrapper instance

    Raises:
        ImportError: If firebase-admin is not installed

    Example:
        >>> client = get_firestore_client(project="my-project")
        >>> doc = client.collection("users").document("user123").get()
    """
    if not FIRESTORE_AVAILABLE:
        raise ImportError(
            "firebase-admin is required for Firestore operations. "
            "Install with: pip install firebase-admin"
        )

    # Create cache key from parameters
    resolved_project = project or os.environ.get("FIREBASE_PROJECT_ID", "default")
    cache_key = f"{resolved_project}:{emulator_host or ''}:{credentials_path or ''}"

    with _cache_lock:
        if cache_key not in _client_cache:
            _client_cache[cache_key] = FirestoreClientWrapper(
                project_id=project,
                emulator_host=emulator_host,
                credentials_path=credentials_path,
            )
            logger.debug(f"Created new Firestore client for: {cache_key}")
        else:
            logger.debug(f"Reusing cached Firestore client for: {cache_key}")

        return _client_cache[cache_key]


def clear_firestore_cache() -> int:
    """
    Clear all cached Firestore clients.

    Closes all clients and removes them from the cache.
    Useful for testing or when reconfiguring connections.

    Returns:
        Number of clients cleared
    """
    with _cache_lock:
        count = len(_client_cache)
        for client in _client_cache.values():
            try:
                client.close()
            except Exception as e:
                logger.warning(f"Error closing client during cache clear: {e}")
        _client_cache.clear()
        logger.info(f"Cleared {count} cached Firestore clients")
        return count
