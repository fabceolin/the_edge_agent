"""
AWS Secrets Manager Backend (TEA-BUILTIN-012.2).

This module provides a secrets backend for AWS Secrets Manager.

Supports two patterns:
    1. Single JSON secret: One secret containing multiple key-value pairs as JSON
    2. Prefix-based multi-secret: Multiple secrets with a common prefix

Example:
    >>> from the_edge_agent.secrets import create_secrets_backend
    >>>
    >>> # Single JSON secret (recommended for related secrets)
    >>> backend = create_secrets_backend("aws", secret_name="myapp/secrets")
    >>> api_key = backend.get("api_key")  # From JSON: {"api_key": "..."}
    >>>
    >>> # Prefix-based (for separate secrets)
    >>> backend = create_secrets_backend("aws", secret_prefix="myapp/")
    >>> api_key = backend.get("api_key")  # From secret: myapp/api_key
"""

import json
import logging
from typing import Any, Dict, Optional

from .base import SecretsBackend

logger = logging.getLogger(__name__)


class AWSSecretsBackend(SecretsBackend):
    """
    AWS Secrets Manager backend.

    Uses boto3's default credential chain for authentication:
        1. Environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)
        2. Shared credential file (~/.aws/credentials)
        3. IAM role for EC2 instances
        4. IAM role for ECS tasks

    Args:
        region: AWS region name (optional, uses boto3 default if not specified)
        secret_name: Name of a single JSON secret containing multiple key-value pairs
        secret_prefix: Prefix for multiple individual secrets (mutually exclusive with secret_name)

    Raises:
        ImportError: If boto3 is not installed

    Example:
        >>> # Single JSON secret
        >>> backend = AWSSecretsBackend(secret_name="myapp/secrets", region="us-east-1")
        >>> backend.get("api_key")
        'sk-...'

        >>> # Prefix-based secrets
        >>> backend = AWSSecretsBackend(secret_prefix="myapp/", region="us-west-2")
        >>> backend.get("api_key")  # Retrieves myapp/api_key
        'sk-...'
    """

    def __init__(
        self,
        region: Optional[str] = None,
        secret_name: Optional[str] = None,
        secret_prefix: Optional[str] = None,
    ):
        """Initialize AWS Secrets Manager backend."""
        try:
            import boto3
            from botocore.exceptions import ClientError, NoCredentialsError
        except ImportError:
            raise ImportError(
                "boto3 is required for AWS Secrets Manager backend. "
                "Install with: pip install the-edge-agent[aws]"
            )

        # Store exception types for later use
        self._ClientError = ClientError
        self._NoCredentialsError = NoCredentialsError

        # Validate configuration
        if secret_name and secret_prefix:
            raise ValueError(
                "Cannot specify both 'secret_name' and 'secret_prefix'. "
                "Use 'secret_name' for a single JSON secret or 'secret_prefix' for multiple secrets."
            )

        if not secret_name and not secret_prefix:
            raise ValueError(
                "Must specify either 'secret_name' (for a single JSON secret) "
                "or 'secret_prefix' (for multiple secrets with a common prefix)."
            )

        try:
            self._client = boto3.client("secretsmanager", region_name=region)
        except NoCredentialsError as e:
            raise RuntimeError(
                "AWS credentials not configured. "
                "Configure via environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY), "
                "AWS CLI (aws configure), or IAM role."
            ) from e

        self._secret_name = secret_name
        self._secret_prefix = secret_prefix
        self._region = region
        self._cache: Dict[str, Any] = {}
        self._loaded = False

    def _load_secrets(self) -> None:
        """Load secrets from AWS Secrets Manager."""
        if self._loaded:
            return

        try:
            if self._secret_name:
                # Single JSON secret mode
                self._load_json_secret()
            elif self._secret_prefix:
                # Prefix-based multi-secret mode
                self._load_prefix_secrets()
            self._loaded = True
        except self._NoCredentialsError as e:
            raise RuntimeError(
                "AWS credentials not configured. "
                "Configure via environment variables (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY), "
                "AWS CLI (aws configure), or IAM role."
            ) from e
        except self._ClientError as e:
            error_code = e.response.get("Error", {}).get("Code", "")
            if error_code == "ResourceNotFoundException":
                secret_id = self._secret_name or f"{self._secret_prefix}*"
                raise RuntimeError(
                    f"Secret not found: {secret_id}. "
                    "Verify the secret exists in AWS Secrets Manager."
                ) from e
            elif error_code == "AccessDeniedException":
                raise RuntimeError(
                    "Access denied to AWS Secrets Manager. "
                    "Check IAM permissions for secretsmanager:GetSecretValue."
                ) from e
            else:
                # Generic error - don't expose internal details
                raise RuntimeError(
                    f"Failed to access AWS Secrets Manager: {error_code}"
                ) from e

    def _load_json_secret(self) -> None:
        """Load a single JSON secret containing multiple key-value pairs."""
        response = self._client.get_secret_value(SecretId=self._secret_name)

        # AWS can store secrets as string or binary
        if "SecretString" in response:
            secret_string = response["SecretString"]
            try:
                self._cache = json.loads(secret_string)
            except json.JSONDecodeError:
                # Not JSON - treat as single value
                self._cache = {"_value": secret_string}
        else:
            # Binary secret - store as bytes
            self._cache = {"_binary": response["SecretBinary"]}

    def _load_prefix_secrets(self) -> None:
        """Load multiple secrets that share a common prefix."""
        paginator = self._client.get_paginator("list_secrets")

        for page in paginator.paginate():
            for secret_entry in page.get("SecretList", []):
                name = secret_entry["Name"]
                if name.startswith(self._secret_prefix):
                    # Extract the key name without prefix
                    key = name[len(self._secret_prefix) :]
                    if key:  # Skip if prefix is the entire name
                        try:
                            value_response = self._client.get_secret_value(
                                SecretId=name
                            )
                            if "SecretString" in value_response:
                                self._cache[key] = value_response["SecretString"]
                            else:
                                self._cache[key] = value_response["SecretBinary"]
                        except self._ClientError as e:
                            # Log but continue - some secrets may be inaccessible
                            error_code = e.response.get("Error", {}).get("Code", "")
                            logger.warning(f"Skipping secret {name}: {error_code}")

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get a secret value by key.

        Args:
            key: Secret key name (without prefix if using prefix mode)
            default: Default value if secret not found

        Returns:
            The secret value, or default if not found
        """
        self._load_secrets()
        return self._cache.get(key, default)

    def get_all(self) -> Dict[str, Any]:
        """
        Get all secrets as a dictionary.

        Returns:
            Copy of all secrets {key: value}
        """
        self._load_secrets()
        return self._cache.copy()

    def has(self, key: str) -> bool:
        """
        Check if a secret exists.

        Args:
            key: Secret key name

        Returns:
            True if secret exists
        """
        self._load_secrets()
        return key in self._cache
