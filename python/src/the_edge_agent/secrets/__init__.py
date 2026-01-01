"""
Secrets Backend Module (TEA-BUILTIN-012.1, TEA-BUILTIN-012.2).

This module provides unified access to secrets from various sources.
All backends implement the SecretsBackend abstract base class.

Available Backends:
    - env: Environment variables with optional prefix filtering (default)
    - aws: AWS Secrets Manager (requires boto3)
    - azure: Azure Key Vault (requires azure-keyvault-secrets)
    - gcp: Google Cloud Secret Manager (requires google-cloud-secret-manager)

Example:
    >>> from the_edge_agent.secrets import create_secrets_backend, EnvSecretsBackend
    >>>
    >>> # Create env backend directly
    >>> backend = EnvSecretsBackend(prefix="MYAPP_")
    >>>
    >>> # Or use factory function
    >>> backend = create_secrets_backend("env", prefix="MYAPP_")
    >>>
    >>> # Cloud backends (TEA-BUILTIN-012.2)
    >>> backend = create_secrets_backend("aws", secret_name="myapp/secrets")
    >>> backend = create_secrets_backend("azure", vault_url="https://myvault.vault.azure.net")
    >>> backend = create_secrets_backend("gcp", project_id="my-project")
    >>>
    >>> # Access secrets
    >>> api_key = backend.get("API_KEY")
    >>> all_secrets = backend.get_all()
"""

from typing import Any, Dict, List, Optional, Type

from .base import SecretsBackend
from .env import EnvSecretsBackend

__all__ = [
    "SecretsBackend",
    "EnvSecretsBackend",
    "AWSSecretsBackend",
    "AzureSecretsBackend",
    "GCPSecretsBackend",
    "create_secrets_backend",
    "get_registered_backends",
]


# =============================================================================
# LAZY IMPORTS FOR CLOUD BACKENDS
# =============================================================================


def __getattr__(name: str):
    """Lazy import cloud backends to avoid requiring their dependencies."""
    if name == "AWSSecretsBackend":
        from .aws import AWSSecretsBackend

        return AWSSecretsBackend
    elif name == "AzureSecretsBackend":
        from .azure import AzureSecretsBackend

        return AzureSecretsBackend
    elif name == "GCPSecretsBackend":
        from .gcp import GCPSecretsBackend

        return GCPSecretsBackend
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


# =============================================================================
# BACKEND REGISTRY AND FACTORY
# =============================================================================


# Registry of backend classes by name
_BACKEND_REGISTRY: Dict[str, Type[SecretsBackend]] = {
    "env": EnvSecretsBackend,
}


def _get_aws_backend() -> Type[SecretsBackend]:
    """Lazily load AWS Secrets Manager backend."""
    try:
        from .aws import AWSSecretsBackend

        return AWSSecretsBackend
    except ImportError as e:
        raise ImportError(
            "AWS Secrets Manager backend requires 'boto3'. "
            "Install with: pip install the-edge-agent[aws]"
        ) from e


def _get_azure_backend() -> Type[SecretsBackend]:
    """Lazily load Azure Key Vault backend."""
    try:
        from .azure import AzureSecretsBackend

        return AzureSecretsBackend
    except ImportError as e:
        raise ImportError(
            "Azure Key Vault backend requires 'azure-keyvault-secrets' and 'azure-identity'. "
            "Install with: pip install the-edge-agent[azure]"
        ) from e


def _get_gcp_backend() -> Type[SecretsBackend]:
    """Lazily load Google Cloud Secret Manager backend."""
    try:
        from .gcp import GCPSecretsBackend

        return GCPSecretsBackend
    except ImportError as e:
        raise ImportError(
            "Google Cloud Secret Manager backend requires 'google-cloud-secret-manager'. "
            "Install with: pip install the-edge-agent[gcp]"
        ) from e


# Lazy loaders for cloud backends
_LAZY_BACKENDS: Dict[str, callable] = {
    "aws": _get_aws_backend,
    "azure": _get_azure_backend,
    "gcp": _get_gcp_backend,
}


def get_registered_backends() -> List[str]:
    """
    Get list of all available backend names.

    Returns:
        List of backend names (e.g., ["env", "aws", "azure", "gcp"])

    Note:
        Cloud backends may require additional dependencies to be installed.
    """
    return list(_BACKEND_REGISTRY.keys()) + list(_LAZY_BACKENDS.keys())


def create_secrets_backend(
    backend_type: str = "env",
    **kwargs: Any,
) -> SecretsBackend:
    """
    Factory function to create a secrets backend instance.

    Args:
        backend_type: Type of backend to create. Options:
            - "env": Environment variables (default)
            - "aws": AWS Secrets Manager
            - "azure": Azure Key Vault
            - "gcp": Google Cloud Secret Manager
        **kwargs: Backend-specific configuration options:
            - EnvSecretsBackend: prefix (str) - env var prefix to filter
            - AWSSecretsBackend: region (str), secret_name (str), secret_prefix (str)
            - AzureSecretsBackend: vault_url (str)
            - GCPSecretsBackend: project_id (str), secret_prefix (str)

    Returns:
        SecretsBackend instance

    Raises:
        ValueError: If backend_type is not recognized
        ImportError: If required dependencies for cloud backends are not installed

    Example:
        >>> # Environment variables with prefix
        >>> backend = create_secrets_backend("env", prefix="MYAPP_")
        >>>
        >>> # AWS Secrets Manager (single JSON secret)
        >>> backend = create_secrets_backend("aws", secret_name="my/secret")
        >>>
        >>> # AWS Secrets Manager (prefix-based)
        >>> backend = create_secrets_backend("aws", secret_prefix="myapp/")
        >>>
        >>> # Azure Key Vault
        >>> backend = create_secrets_backend("azure", vault_url="https://myvault.vault.azure.net")
        >>>
        >>> # Google Cloud Secret Manager
        >>> backend = create_secrets_backend("gcp", project_id="my-project")
    """
    backend_name = backend_type.lower()

    # Check direct registry first
    if backend_name in _BACKEND_REGISTRY:
        backend_class = _BACKEND_REGISTRY[backend_name]
        return backend_class(**kwargs)

    # Check lazy loaders for cloud backends
    if backend_name in _LAZY_BACKENDS:
        loader = _LAZY_BACKENDS[backend_name]
        backend_class = loader()  # May raise ImportError
        return backend_class(**kwargs)

    # Unknown backend type
    available = ", ".join(get_registered_backends())
    raise ValueError(
        f"Unknown secrets backend type: '{backend_type}'. "
        f"Available backends: {available}"
    )
