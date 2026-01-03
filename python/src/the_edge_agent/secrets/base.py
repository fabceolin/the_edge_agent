"""
Secrets Backend Abstract Base Class (TEA-BUILTIN-012.1).

This module provides the abstract base class for secrets backends.
Secrets backends provide unified access to secrets from various sources
(environment variables, cloud providers, vaults).

Supported Backends:
    - env: Environment variables with optional prefix filtering (default)
    - aws: AWS Secrets Manager (TEA-BUILTIN-012.2)
    - azure: Azure Key Vault (TEA-BUILTIN-012.2)
    - gcp: Google Cloud Secret Manager (TEA-BUILTIN-012.2)

Example:
    >>> from the_edge_agent.secrets import create_secrets_backend
    >>>
    >>> # Environment variables with prefix
    >>> backend = create_secrets_backend("env", prefix="MYAPP_")
    >>> api_key = backend.get("API_KEY")  # Gets MYAPP_API_KEY
    >>>
    >>> # Get all secrets for template context
    >>> secrets = backend.get_all()
    >>> # {"API_KEY": "value", "DB_PASSWORD": "secret"}
"""

from abc import ABC, abstractmethod
from typing import Any, Dict, Optional


class SecretsBackend(ABC):
    """
    Abstract base class for secrets backends.

    All secrets backends must implement this interface to ensure consistent
    behavior across different secret sources (env vars, cloud providers, vaults).

    Thread Safety:
        Implementations SHOULD be thread-safe for use in parallel execution.

    Security:
        - Secrets are never logged
        - Backends should support lazy loading where possible
        - Cloud backends should use short-lived tokens

    Example:
        >>> class MyBackend(SecretsBackend):
        ...     def get(self, key, default=None):
        ...         return self._secrets.get(key, default)
        ...     def get_all(self):
        ...         return self._secrets.copy()
        ...     def has(self, key):
        ...         return key in self._secrets
        ...     def close(self):
        ...         pass
    """

    @abstractmethod
    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a secret value by key.

        Args:
            key: The secret key name (without any backend-specific prefix)
            default: Default value if secret not found

        Returns:
            The secret value, or default if not found

        Example:
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
            >>> # Environment has MYAPP_API_KEY=secret123
            >>> backend.get("API_KEY")
            'secret123'
            >>> backend.get("MISSING", default="fallback")
            'fallback'
        """
        pass

    @abstractmethod
    def get_all(self) -> Dict[str, Any]:
        """
        Get all secrets as a dictionary.

        This is used to inject secrets into Jinja2 template context.
        Keys should be normalized (e.g., prefix stripped for env backend).

        Returns:
            Dictionary of all secrets {key: value}

        Security Note:
            The returned dict is typically used for template rendering.
            Avoid logging this dict to prevent secret leakage.

        Example:
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
            >>> # Environment has MYAPP_KEY1=val1, MYAPP_KEY2=val2
            >>> backend.get_all()
            {'KEY1': 'val1', 'KEY2': 'val2'}
        """
        pass

    @abstractmethod
    def has(self, key: str) -> bool:
        """
        Check if a secret exists.

        Args:
            key: The secret key name

        Returns:
            True if secret exists, False otherwise

        Example:
            >>> backend.has("API_KEY")
            True
            >>> backend.has("NONEXISTENT")
            False
        """
        pass

    def close(self) -> None:
        """
        Close the backend and release resources.

        Optional cleanup method. Default implementation is a no-op.
        Override if your backend needs cleanup (e.g., close connections).

        Should be safe to call multiple times.
        """
        pass

    def __enter__(self) -> "SecretsBackend":
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> bool:
        """Context manager exit - close the backend."""
        self.close()
        return False
