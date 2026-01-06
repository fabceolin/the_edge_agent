"""
Auth Settings Schema for YAML-based Authentication Configuration.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides Pydantic models for authentication configuration via YAML settings.
Supports Firebase, JWT, and API Key providers with configurable behavior.

Example YAML:
    settings:
      auth:
        provider: firebase         # Required: firebase | jwt | api_key | none
        token_header: "X-Firebase-Token"  # Header containing token
        required: true             # 401 if auth fails (default: true)
        inject_user: true          # Inject user into state (default: true)
        user_state_key: "__user__" # State key for user info (default: __user__)

        claims_mapping:            # Map token claims to user fields
          user_id: uid
          user_email: email
          roles: custom_claims.roles

        firebase:
          project_id: "${FIREBASE_PROJECT_ID}"

        jwt:
          secret: "${JWT_SECRET}"
          algorithms: ["HS256"]
          issuer: "https://your-issuer.com"

        api_key:
          keys:
            - "${API_KEY_1}"
            - "${API_KEY_2}"
"""

from enum import Enum
from typing import Dict, List, Optional, Any

from pydantic import BaseModel, Field, field_validator


class AuthProviderType(str, Enum):
    """Supported authentication provider types."""

    FIREBASE = "firebase"
    JWT = "jwt"
    API_KEY = "api_key"
    NONE = "none"


class TokenSourceType(str, Enum):
    """Token source location types."""

    HEADER = "header"
    QUERY = "query"


class FirebaseAuthConfig(BaseModel):
    """Firebase authentication provider configuration."""

    project_id: Optional[str] = Field(
        default=None,
        description="Firebase project ID for token verification",
    )


class JWTAuthConfig(BaseModel):
    """JWT authentication provider configuration."""

    secret: Optional[str] = Field(
        default=None,
        description="Secret key for HS256/HS384/HS512 algorithms",
    )

    public_key_path: Optional[str] = Field(
        default=None,
        description="Path to public key file for RS256/RS384/RS512 algorithms",
    )

    algorithms: List[str] = Field(
        default=["HS256"],
        description="List of accepted JWT algorithms",
    )

    issuer: Optional[str] = Field(
        default=None,
        description="Expected token issuer (iss claim)",
    )

    audience: Optional[str] = Field(
        default=None,
        description="Expected token audience (aud claim)",
    )


class APIKeyAuthConfig(BaseModel):
    """API Key authentication provider configuration."""

    keys: List[str] = Field(
        default_factory=list,
        description="List of valid API keys",
    )

    keys_file: Optional[str] = Field(
        default=None,
        description="Path to file containing API keys (one per line)",
    )


class AuthSettings(BaseModel):
    """
    Pydantic model for authentication settings.

    This model validates the auth configuration from YAML files and
    provides typed access to authentication options.

    Attributes:
        provider: Authentication provider type (firebase, jwt, api_key, none)
        token_header: HTTP header name containing the token
        token_query_param: Query parameter name containing the token (alternative)
        required: Whether authentication is required (401 on failure)
        inject_user: Whether to inject user info into state
        user_state_key: State key for user info injection
        claims_mapping: Mapping of user fields to token claim paths
        firebase: Firebase-specific configuration
        jwt: JWT-specific configuration
        api_key: API key-specific configuration

    Example:
        >>> config = {
        ...     "provider": "firebase",
        ...     "token_header": "Authorization",
        ...     "required": True
        ... }
        >>> settings = AuthSettings(**config)
        >>> settings.provider
        <AuthProviderType.FIREBASE: 'firebase'>
    """

    provider: AuthProviderType = Field(
        default=AuthProviderType.NONE,
        description="Authentication provider type",
    )

    token_header: str = Field(
        default="Authorization",
        description="HTTP header name containing the auth token",
    )

    token_query_param: Optional[str] = Field(
        default=None,
        description="Query parameter name for token (alternative to header)",
    )

    required: bool = Field(
        default=True,
        description="Whether authentication is required. If True, returns 401 on failure.",
    )

    inject_user: bool = Field(
        default=True,
        description="Whether to inject authenticated user into state",
    )

    user_state_key: str = Field(
        default="__user__",
        description="State key where user info is injected",
    )

    claims_mapping: Dict[str, str] = Field(
        default_factory=dict,
        description="Mapping of user fields to token claim paths (e.g., user_id: uid)",
    )

    firebase: Optional[FirebaseAuthConfig] = Field(
        default=None,
        description="Firebase-specific configuration",
    )

    jwt: Optional[JWTAuthConfig] = Field(
        default=None,
        description="JWT-specific configuration",
    )

    api_key: Optional[APIKeyAuthConfig] = Field(
        default=None,
        description="API Key-specific configuration",
    )

    @field_validator("provider", mode="before")
    @classmethod
    def validate_provider(cls, v):
        """Accept string values and convert to enum."""
        if isinstance(v, str):
            try:
                return AuthProviderType(v.lower())
            except ValueError:
                valid = [e.value for e in AuthProviderType]
                raise ValueError(f"Invalid provider '{v}'. Valid options: {valid}")
        return v

    @field_validator("claims_mapping", mode="before")
    @classmethod
    def validate_claims_mapping(cls, v):
        """Ensure claims_mapping is a dict with string keys and values."""
        if v is None:
            return {}
        if isinstance(v, dict):
            for key, value in v.items():
                if not isinstance(key, str) or not isinstance(value, str):
                    raise ValueError(
                        f"claims_mapping must have string keys and values, "
                        f"got {type(key).__name__}: {type(value).__name__}"
                    )
            return v
        raise ValueError(f"claims_mapping must be a dict, got {type(v).__name__}")

    @field_validator("firebase", mode="before")
    @classmethod
    def validate_firebase(cls, v):
        """Convert dict to FirebaseAuthConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            return FirebaseAuthConfig(**v)
        return v

    @field_validator("jwt", mode="before")
    @classmethod
    def validate_jwt(cls, v):
        """Convert dict to JWTAuthConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            return JWTAuthConfig(**v)
        return v

    @field_validator("api_key", mode="before")
    @classmethod
    def validate_api_key(cls, v):
        """Convert dict to APIKeyAuthConfig."""
        if v is None:
            return None
        if isinstance(v, dict):
            return APIKeyAuthConfig(**v)
        return v

    model_config = {"use_enum_values": True}


def parse_auth_settings(config: dict) -> Optional[AuthSettings]:
    """
    Parse auth settings from a configuration dictionary.

    Args:
        config: Configuration dictionary, typically from YAML settings.
                Expected to have an 'auth' key with auth configuration.

    Returns:
        AuthSettings instance if auth configuration is present,
        None otherwise.

    Example:
        >>> config = {"settings": {"auth": {"provider": "firebase"}}}
        >>> settings = parse_auth_settings(config.get("settings", {}))
        >>> settings.provider
        'firebase'
    """
    auth_config = config.get("auth")
    if auth_config is None:
        return None

    if not isinstance(auth_config, dict):
        return None

    try:
        return AuthSettings(**auth_config)
    except Exception:
        # Return None for invalid configurations to maintain backward compatibility
        return None


def build_auth_config_dict(settings: AuthSettings) -> Dict[str, Any]:
    """
    Build a provider configuration dictionary from AuthSettings.

    This is used by create_auth_provider() to construct the appropriate
    provider instance.

    Args:
        settings: Validated AuthSettings instance

    Returns:
        Configuration dictionary suitable for create_auth_provider()
    """
    config: Dict[str, Any] = {
        "provider": settings.provider,
        "required": settings.required,
        "inject_user": settings.inject_user,
        "user_state_key": settings.user_state_key,
        "claims_mapping": settings.claims_mapping,
        "token_header": settings.token_header,
    }

    if settings.token_query_param:
        config["token_query_param"] = settings.token_query_param

    if settings.firebase:
        config["firebase"] = settings.firebase.model_dump()

    if settings.jwt:
        config["jwt"] = settings.jwt.model_dump()

    if settings.api_key:
        config["api_key"] = settings.api_key.model_dump()

    return config
