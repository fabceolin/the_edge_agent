"""
Environment Variables Secrets Backend (TEA-BUILTIN-012.1).

This module provides a secrets backend that reads secrets from environment
variables. It supports optional prefix filtering to isolate application-specific
secrets.

Example:
    >>> import os
    >>> os.environ["MYAPP_API_KEY"] = "secret123"
    >>> os.environ["MYAPP_DB_PASSWORD"] = "password456"
    >>>
    >>> from the_edge_agent.secrets import EnvSecretsBackend
    >>>
    >>> backend = EnvSecretsBackend(prefix="MYAPP_")
    >>> backend.get("API_KEY")  # Returns "secret123"
    >>> backend.get_all()  # Returns {"API_KEY": "secret123", "DB_PASSWORD": "password456"}
"""

import os
from typing import Any, Dict, Optional

from .base import SecretsBackend


class EnvSecretsBackend(SecretsBackend):
    """
    Secrets backend that reads from environment variables.

    This backend provides access to environment variables as secrets.
    When a prefix is specified, only variables starting with that prefix
    are accessible, and the prefix is stripped from the key names.

    Attributes:
        _prefix: The prefix filter for environment variables

    Thread Safety:
        This implementation is thread-safe as it only reads from os.environ.

    Example:
        >>> import os
        >>> os.environ["MYAPP_API_KEY"] = "secret123"
        >>> os.environ["OTHER_VAR"] = "other"
        >>>
        >>> # Without prefix - access any env var
        >>> backend = EnvSecretsBackend()
        >>> backend.get("MYAPP_API_KEY")
        'secret123'
        >>>
        >>> # With prefix - keys are stripped
        >>> backend = EnvSecretsBackend(prefix="MYAPP_")
        >>> backend.get("API_KEY")  # Looks up MYAPP_API_KEY
        'secret123'
        >>> backend.has("OTHER_VAR")  # Not accessible
        False
    """

    def __init__(self, prefix: str = ""):
        """
        Initialize the environment variables secrets backend.

        Args:
            prefix: Optional prefix to filter environment variables.
                   When specified, only env vars starting with this prefix
                   are accessible, and the prefix is stripped from keys.
                   Default is empty string (no filtering).

        Example:
            >>> # Access all env vars
            >>> backend = EnvSecretsBackend()
            >>>
            >>> # Only access env vars starting with MYAPP_
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
        """
        self._prefix = prefix

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a secret value by key.

        If a prefix was configured, the key is automatically prefixed
        when looking up the environment variable.

        Args:
            key: The secret key name (without prefix)
            default: Default value if secret not found

        Returns:
            The secret value (env var value), or default if not found

        Example:
            >>> os.environ["MYAPP_API_KEY"] = "secret123"
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
            >>> backend.get("API_KEY")
            'secret123'
            >>> backend.get("MISSING")
            None
            >>> backend.get("MISSING", default="fallback")
            'fallback'
        """
        full_key = f"{self._prefix}{key}"
        return os.environ.get(full_key, default)

    def get_all(self) -> Dict[str, Any]:
        """
        Get all secrets as a dictionary.

        If a prefix was configured, only environment variables starting
        with that prefix are returned, and the prefix is stripped from
        the keys in the returned dictionary.

        Returns:
            Dictionary of secrets {key: value}. Keys are stripped of prefix.

        Example:
            >>> os.environ["MYAPP_KEY1"] = "val1"
            >>> os.environ["MYAPP_KEY2"] = "val2"
            >>> os.environ["OTHER"] = "other"
            >>>
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
            >>> backend.get_all()
            {'KEY1': 'val1', 'KEY2': 'val2'}
            >>>
            >>> # Without prefix, returns all env vars
            >>> backend = EnvSecretsBackend()
            >>> secrets = backend.get_all()
            >>> "PATH" in secrets  # Contains all env vars
            True
        """
        if not self._prefix:
            return dict(os.environ)

        prefix_len = len(self._prefix)
        return {
            key[prefix_len:]: value
            for key, value in os.environ.items()
            if key.startswith(self._prefix)
        }

    def has(self, key: str) -> bool:
        """
        Check if a secret exists.

        If a prefix was configured, the key is automatically prefixed
        when checking the environment.

        Args:
            key: The secret key name (without prefix)

        Returns:
            True if secret exists, False otherwise

        Example:
            >>> os.environ["MYAPP_API_KEY"] = "secret123"
            >>> backend = EnvSecretsBackend(prefix="MYAPP_")
            >>> backend.has("API_KEY")
            True
            >>> backend.has("MISSING")
            False
        """
        full_key = f"{self._prefix}{key}"
        return full_key in os.environ

    def close(self) -> None:
        """
        Close the backend.

        No-op for environment variables backend as there are no resources
        to release.
        """
        pass

    def __repr__(self) -> str:
        """String representation for debugging."""
        return f"EnvSecretsBackend(prefix={self._prefix!r})"
