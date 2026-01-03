"""
Secrets Actions for YAMLEngine (TEA-BUILTIN-012.3).

This module provides actions for accessing secrets from the configured
secrets backend. These actions allow YAML workflows to retrieve secrets
programmatically during execution.

Available Actions:
    - secrets.get: Get a secret value by key
    - secrets.has: Check if a secret exists

Example YAML usage:
    nodes:
      - name: get_credentials
        steps:
          - uses: secrets.get
            with:
              key: DATABASE_URL
            output: db_url
          - uses: secrets.has
            with:
              key: OPTIONAL_FEATURE_KEY
            output: has_feature

Security Note:
    Secrets retrieved via these actions are stored in the workflow state.
    However, the secrets backend itself is NOT serialized in checkpoints,
    providing defense-in-depth against secret leakage.
"""

from typing import Any, Callable, Dict, Optional

import logging

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register secrets actions with the action registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing secrets backend

    Actions registered:
        - secrets.get: Get a secret value by key
        - secrets.has: Check if a secret exists
    """

    def secrets_get(
        state: Dict[str, Any],
        key: str,
        default: Any = None,
        **kwargs: Any,
    ) -> Dict[str, Any]:
        """
        Get a secret value by key.

        This action retrieves a secret from the configured secrets backend.
        If no secrets backend is configured, falls back to the engine's
        secrets dictionary (populated from EnvSecretsBackend by default).

        Args:
            state: Current workflow state (unused but required by action interface)
            key: Secret key to retrieve
            default: Default value if secret not found (default: None)
            **kwargs: Additional parameters (ignored)

        Returns:
            Dictionary with 'value' key containing the secret value or default.
            Designed to be used with 'output' to store in state.

        Example YAML:
            - uses: secrets.get
              with:
                key: API_KEY
              output: api_key

            - uses: secrets.get
              with:
                key: OPTIONAL_KEY
                default: "fallback_value"
              output: optional_value
        """
        # Try secrets backend first
        if engine._secrets_backend is not None:
            value = engine._secrets_backend.get(key, default)
        else:
            # Fallback to engine.secrets dict (populated at load time)
            value = engine.secrets.get(key, default)

        logger.debug(f"secrets.get: key={key}, found={value is not None}")
        return {"value": value}

    def secrets_has(
        state: Dict[str, Any],
        key: str,
        **kwargs: Any,
    ) -> Dict[str, Any]:
        """
        Check if a secret exists.

        This action checks whether a secret key exists in the configured
        secrets backend. Useful for conditional logic based on optional
        secrets.

        Args:
            state: Current workflow state (unused but required by action interface)
            key: Secret key to check
            **kwargs: Additional parameters (ignored)

        Returns:
            Dictionary with 'exists' key containing boolean.
            Designed to be used with 'output' to store in state.

        Example YAML:
            - uses: secrets.has
              with:
                key: OPTIONAL_FEATURE_KEY
              output: has_feature

            - name: use_feature
              when: "{{ state.has_feature }}"
              run: |
                # Feature is available
                pass
        """
        # Try secrets backend first
        if engine._secrets_backend is not None:
            exists = engine._secrets_backend.has(key)
        else:
            # Fallback to engine.secrets dict
            exists = key in engine.secrets

        logger.debug(f"secrets.has: key={key}, exists={exists}")
        return {"exists": exists}

    # Register actions with both naming conventions
    registry["secrets.get"] = secrets_get
    registry["secrets.has"] = secrets_has

    # Also register with legacy 'actions.' prefix for backward compatibility
    registry["actions.secrets_get"] = secrets_get
    registry["actions.secrets_has"] = secrets_has

    logger.debug("Registered secrets actions: secrets.get, secrets.has")
