"""
Google Cloud Secret Manager Backend (TEA-BUILTIN-012.2).

This module provides a secrets backend for Google Cloud Secret Manager.

Uses Application Default Credentials (ADC) for authentication, which supports:
    - Service account key file (GOOGLE_APPLICATION_CREDENTIALS env var)
    - Workload Identity (on GKE)
    - Compute Engine default service account
    - Cloud Run/Cloud Functions service account
    - gcloud CLI credentials (gcloud auth application-default login)

Example:
    >>> from the_edge_agent.secrets import create_secrets_backend
    >>>
    >>> backend = create_secrets_backend("gcp", project_id="my-project")
    >>> api_key = backend.get("api_key")
    >>>
    >>> # With prefix filtering
    >>> backend = create_secrets_backend("gcp", project_id="my-project", secret_prefix="myapp-")
    >>> api_key = backend.get("api_key")  # Retrieves myapp-api_key
"""

import logging
from typing import Any, Dict, Optional

from .base import SecretsBackend

logger = logging.getLogger(__name__)


class GCPSecretsBackend(SecretsBackend):
    """
    Google Cloud Secret Manager backend.

    Uses Application Default Credentials (ADC) for authentication.
    ADC automatically finds credentials based on the environment:
        1. GOOGLE_APPLICATION_CREDENTIALS environment variable
        2. Workload Identity (on GKE)
        3. Default service account (on Compute Engine, Cloud Run, etc.)
        4. gcloud CLI credentials

    Args:
        project_id: The GCP project ID containing the secrets
        secret_prefix: Optional prefix to filter/namespace secrets

    Raises:
        ImportError: If google-cloud-secret-manager is not installed

    Example:
        >>> backend = GCPSecretsBackend(project_id="my-project")
        >>> backend.get("api_key")
        'sk-...'

        >>> # With prefix
        >>> backend = GCPSecretsBackend(project_id="my-project", secret_prefix="myapp-")
        >>> backend.get("api_key")  # Retrieves myapp-api_key
        'sk-...'
    """

    def __init__(self, project_id: str, secret_prefix: str = ""):
        """Initialize Google Cloud Secret Manager backend."""
        try:
            from google.cloud import secretmanager
            from google.api_core.exceptions import (
                NotFound,
                PermissionDenied,
                Unauthenticated,
            )
        except ImportError:
            raise ImportError(
                "google-cloud-secret-manager is required for GCP Secret Manager backend. "
                "Install with: pip install the-edge-agent[gcp]"
            )

        # Store exception types for later use
        self._NotFound = NotFound
        self._PermissionDenied = PermissionDenied
        self._Unauthenticated = Unauthenticated

        if not project_id:
            raise ValueError("project_id is required for GCP Secret Manager backend")

        self._project_id = project_id
        self._secret_prefix = secret_prefix
        self._cache: Dict[str, Any] = {}
        self._all_loaded = False

        try:
            self._client = secretmanager.SecretManagerServiceClient()
        except Exception as e:
            raise RuntimeError(
                "GCP authentication failed. "
                "Configure via GOOGLE_APPLICATION_CREDENTIALS environment variable, "
                "gcloud CLI (gcloud auth application-default login), "
                "or default service account."
            ) from e

    def _build_secret_name(self, key: str) -> str:
        """Build the full secret name path for GCP Secret Manager."""
        secret_id = f"{self._secret_prefix}{key}" if self._secret_prefix else key
        return f"projects/{self._project_id}/secrets/{secret_id}/versions/latest"

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a secret value by key.

        Retrieves the latest version of the secret.

        Args:
            key: Secret name (without prefix if using prefix mode)
            default: Default value if secret not found

        Returns:
            The secret value as a UTF-8 string, or default if not found
        """
        if key in self._cache:
            return self._cache[key]

        name = self._build_secret_name(key)
        try:
            response = self._client.access_secret_version(request={"name": name})
            value = response.payload.data.decode("UTF-8")
            self._cache[key] = value
            return value
        except self._NotFound:
            return default
        except self._Unauthenticated as e:
            raise RuntimeError(
                "GCP authentication failed. "
                "Configure via GOOGLE_APPLICATION_CREDENTIALS environment variable, "
                "gcloud CLI (gcloud auth application-default login), "
                "or default service account."
            ) from e
        except self._PermissionDenied as e:
            raise RuntimeError(
                f"Access denied to secret '{key}'. "
                "Check that the service account has 'Secret Manager Secret Accessor' role."
            ) from e
        except Exception as e:
            # Generic error - don't expose internal details
            logger.warning(f"Failed to access secret {key}: {type(e).__name__}")
            return default

    def get_all(self) -> Dict[str, Any]:
        """
        Get all secrets matching the prefix from the project.

        Note: This requires 'Secret Manager Secret Accessor' role and
        'secretmanager.secrets.list' permission.

        Returns:
            Copy of all secrets {key: value}
        """
        if not self._all_loaded:
            parent = f"projects/{self._project_id}"
            try:
                for secret in self._client.list_secrets(request={"parent": parent}):
                    # Extract secret name from full path
                    # Format: projects/{project}/secrets/{secret_name}
                    full_name = secret.name
                    secret_name = full_name.split("/")[-1]

                    # Filter by prefix if specified
                    if self._secret_prefix:
                        if not secret_name.startswith(self._secret_prefix):
                            continue
                        # Remove prefix for the cache key
                        key = secret_name[len(self._secret_prefix) :]
                    else:
                        key = secret_name

                    # Only load if not already in cache
                    if key and key not in self._cache:
                        try:
                            self.get(key)  # This will cache the value
                        except RuntimeError:
                            # Skip secrets we can't access
                            pass

                self._all_loaded = True
            except self._Unauthenticated as e:
                raise RuntimeError(
                    "GCP authentication failed. "
                    "Configure via GOOGLE_APPLICATION_CREDENTIALS environment variable, "
                    "gcloud CLI (gcloud auth application-default login), "
                    "or default service account."
                ) from e
            except self._PermissionDenied as e:
                raise RuntimeError(
                    "Access denied to list secrets. "
                    "Check that the service account has 'secretmanager.secrets.list' permission."
                ) from e

        return self._cache.copy()

    def has(self, key: str) -> bool:
        """
        Check if a secret exists.

        Args:
            key: Secret name (without prefix if using prefix mode)

        Returns:
            True if secret exists and is accessible
        """
        if key in self._cache:
            return True

        # Try to access the secret
        value = self.get(key)
        return value is not None
