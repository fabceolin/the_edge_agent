"""
Azure Key Vault Backend (TEA-BUILTIN-012.2).

This module provides a secrets backend for Azure Key Vault.

Uses Azure's DefaultAzureCredential for authentication, which supports:
    - Environment variables (AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID)
    - Managed Identity (on Azure VMs, App Service, Functions, AKS)
    - Azure CLI (az login)
    - Visual Studio Code credentials

Example:
    >>> from the_edge_agent.secrets import create_secrets_backend
    >>>
    >>> backend = create_secrets_backend(
    ...     "azure",
    ...     vault_url="https://myvault.vault.azure.net"
    ... )
    >>> api_key = backend.get("api-key")
"""

import logging
from typing import Any, Dict, Optional

from .base import SecretsBackend

logger = logging.getLogger(__name__)


class AzureSecretsBackend(SecretsBackend):
    """
    Azure Key Vault secrets backend.

    Uses DefaultAzureCredential for authentication, which automatically
    tries multiple authentication methods in order:
        1. Environment variables (AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID)
        2. Managed Identity
        3. Azure CLI credentials
        4. VS Code credentials
        5. Interactive browser login (if enabled)

    Args:
        vault_url: The URL of the Azure Key Vault (e.g., "https://myvault.vault.azure.net")

    Raises:
        ImportError: If azure-identity or azure-keyvault-secrets is not installed

    Example:
        >>> backend = AzureSecretsBackend(vault_url="https://myvault.vault.azure.net")
        >>> backend.get("api-key")
        'sk-...'
        >>> backend.get_all()
        {'api-key': 'sk-...', 'db-password': 'secret'}
    """

    def __init__(self, vault_url: str):
        """Initialize Azure Key Vault backend."""
        try:
            from azure.identity import DefaultAzureCredential
            from azure.keyvault.secrets import SecretClient
            from azure.core.exceptions import (
                ClientAuthenticationError,
                HttpResponseError,
                ResourceNotFoundError,
            )
        except ImportError:
            raise ImportError(
                "azure-identity and azure-keyvault-secrets are required for Azure Key Vault backend. "
                "Install with: pip install the-edge-agent[azure]"
            )

        # Store exception types for later use
        self._ClientAuthenticationError = ClientAuthenticationError
        self._HttpResponseError = HttpResponseError
        self._ResourceNotFoundError = ResourceNotFoundError

        if not vault_url:
            raise ValueError("vault_url is required for Azure Key Vault backend")

        # Validate vault URL format
        if not vault_url.startswith("https://") or ".vault.azure.net" not in vault_url:
            raise ValueError(
                f"Invalid vault_url: {vault_url}. "
                "Expected format: https://<vault-name>.vault.azure.net"
            )

        self._vault_url = vault_url

        try:
            credential = DefaultAzureCredential()
            self._client = SecretClient(vault_url=vault_url, credential=credential)
        except ClientAuthenticationError as e:
            raise RuntimeError(
                "Azure authentication failed. "
                "Configure via environment variables (AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID), "
                "Azure CLI (az login), or Managed Identity."
            ) from e

        self._cache: Dict[str, Any] = {}
        self._all_loaded = False

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a secret value by key.

        Args:
            key: Secret name in Azure Key Vault
            default: Default value if secret not found

        Returns:
            The secret value, or default if not found
        """
        if key in self._cache:
            return self._cache[key]

        try:
            secret = self._client.get_secret(key)
            self._cache[key] = secret.value
            return secret.value
        except self._ResourceNotFoundError:
            return default
        except self._ClientAuthenticationError as e:
            raise RuntimeError(
                "Azure authentication failed. "
                "Configure via environment variables (AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID), "
                "Azure CLI (az login), or Managed Identity."
            ) from e
        except self._HttpResponseError as e:
            # Handle other HTTP errors without exposing internal details
            status_code = getattr(e, "status_code", "unknown")
            if status_code == 403:
                raise RuntimeError(
                    "Access denied to Azure Key Vault. "
                    "Check that the identity has 'Secret > Get' permission."
                ) from e
            # Generic error
            raise RuntimeError(
                f"Failed to access Azure Key Vault: HTTP {status_code}"
            ) from e

    def get_all(self) -> Dict[str, Any]:
        """
        Get all secrets from the Key Vault.

        Note: This requires 'Secret > List' permission in addition to 'Secret > Get'.

        Returns:
            Copy of all secrets {name: value}
        """
        if not self._all_loaded:
            try:
                for prop in self._client.list_properties_of_secrets():
                    if prop.name not in self._cache:
                        try:
                            secret = self._client.get_secret(prop.name)
                            self._cache[prop.name] = secret.value
                        except self._ResourceNotFoundError:
                            # Secret deleted between list and get - skip
                            pass
                        except self._HttpResponseError as e:
                            # Log but continue - some secrets may be inaccessible
                            logger.warning(
                                f"Skipping secret {prop.name}: {e.status_code}"
                            )
                self._all_loaded = True
            except self._ClientAuthenticationError as e:
                raise RuntimeError(
                    "Azure authentication failed. "
                    "Configure via environment variables (AZURE_CLIENT_ID, AZURE_CLIENT_SECRET, AZURE_TENANT_ID), "
                    "Azure CLI (az login), or Managed Identity."
                ) from e
            except self._HttpResponseError as e:
                status_code = getattr(e, "status_code", "unknown")
                if status_code == 403:
                    raise RuntimeError(
                        "Access denied to Azure Key Vault. "
                        "Check that the identity has 'Secret > List' permission."
                    ) from e
                raise RuntimeError(
                    f"Failed to list Azure Key Vault secrets: HTTP {status_code}"
                ) from e

        return self._cache.copy()

    def has(self, key: str) -> bool:
        """
        Check if a secret exists in the Key Vault.

        Args:
            key: Secret name

        Returns:
            True if secret exists
        """
        if key in self._cache:
            return True

        try:
            self._client.get_secret(key)
            return True
        except self._ResourceNotFoundError:
            return False
        except (self._ClientAuthenticationError, self._HttpResponseError):
            return False
