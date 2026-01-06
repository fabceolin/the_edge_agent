"""
Authentication Module for TEA YAMLEngine.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides configurable authentication via YAML settings, supporting:
- Firebase ID token verification
- Generic JWT verification with configurable secret/public key
- Simple API key authentication

Example YAML:
    settings:
      auth:
        provider: firebase         # Required: firebase | jwt | api_key | none
        token_header: "X-Firebase-Token"
        required: true             # 401 if auth fails (default: true)
        inject_user: true          # Inject user into state (default: true)
        user_state_key: "__user__"

Usage:
    >>> from the_edge_agent.auth import create_auth_provider, AuthProvider, UserInfo
    >>>
    >>> # Create provider from config
    >>> provider = create_auth_provider({"provider": "firebase"})
    >>>
    >>> # Verify token
    >>> result = await provider.verify_token(token)
    >>> if result.success:
    ...     print(f"User: {result.user.uid}")
"""

from .settings import (
    AuthSettings,
    AuthProviderType,
    TokenSourceType,
    parse_auth_settings,
    build_auth_config_dict,
)
from .base import AuthProvider, UserInfo, AuthResult
from .firebase_provider import FirebaseAuthProvider
from .jwt_provider import JWTAuthProvider
from .apikey_provider import APIKeyAuthProvider
from .middleware import (
    AuthMiddleware,
    AuthenticateResult,
    AuthenticationError,
    create_auth_middleware,
)


def create_auth_provider(config: dict) -> AuthProvider | None:
    """
    Factory function for auth providers.

    Creates an appropriate AuthProvider instance based on the provider type
    specified in the configuration dictionary.

    Args:
        config: Authentication configuration dictionary with keys:
            - provider: Provider type (firebase, jwt, api_key, none)
            - firebase: Firebase-specific config (project_id)
            - jwt: JWT-specific config (secret, algorithms, issuer)
            - api_key: API key config (keys, keys_file)

    Returns:
        AuthProvider instance for the specified provider type,
        or None if provider is "none".

    Raises:
        ValueError: If an unknown provider type is specified.

    Example:
        >>> provider = create_auth_provider({
        ...     "provider": "firebase",
        ...     "firebase": {"project_id": "my-project"}
        ... })
    """
    provider_type = config.get("provider", "none")

    if provider_type == "firebase":
        firebase_config = config.get("firebase", {})
        return FirebaseAuthProvider(
            project_id=firebase_config.get("project_id"),
        )

    elif provider_type == "jwt":
        jwt_config = config.get("jwt", {})
        return JWTAuthProvider(
            secret=jwt_config.get("secret"),
            public_key_path=jwt_config.get("public_key_path"),
            algorithms=jwt_config.get("algorithms", ["HS256"]),
            issuer=jwt_config.get("issuer"),
            audience=jwt_config.get("audience"),
        )

    elif provider_type == "api_key":
        api_key_config = config.get("api_key", {})
        return APIKeyAuthProvider(
            keys=api_key_config.get("keys", []),
            keys_file=api_key_config.get("keys_file"),
        )

    elif provider_type == "none":
        return None

    else:
        raise ValueError(f"Unknown auth provider: {provider_type}")


__all__ = [
    # Settings
    "AuthSettings",
    "AuthProviderType",
    "TokenSourceType",
    "parse_auth_settings",
    "build_auth_config_dict",
    # Base classes
    "AuthProvider",
    "UserInfo",
    "AuthResult",
    # Provider implementations
    "FirebaseAuthProvider",
    "JWTAuthProvider",
    "APIKeyAuthProvider",
    # Middleware
    "AuthMiddleware",
    "AuthenticateResult",
    "AuthenticationError",
    "create_auth_middleware",
    # Factory
    "create_auth_provider",
]
