"""
Authentication Middleware for YAMLEngine.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides the AuthMiddleware class that handles:
- Token extraction from requests
- User verification via providers
- User injection into state
- Required vs optional auth behavior
"""

import logging
from typing import Any, Dict, Optional

from .base import AuthProvider, AuthResult, UserInfo
from .settings import AuthSettings

logger = logging.getLogger(__name__)


class AuthenticationError(Exception):
    """
    Raised when authentication fails and auth is required.

    Attributes:
        status_code: HTTP status code (401 for unauthorized)
        message: Error description
    """

    def __init__(self, message: str, status_code: int = 401):
        self.message = message
        self.status_code = status_code
        super().__init__(message)


class AuthMiddleware:
    """
    Authentication middleware for YAMLEngine workflows.

    Handles authentication verification and user injection into state
    based on the AuthSettings configuration.

    Usage:
        >>> from the_edge_agent.auth import (
        ...     AuthMiddleware, AuthSettings, create_auth_provider
        ... )
        >>>
        >>> settings = AuthSettings(provider="firebase", required=True)
        >>> provider = create_auth_provider({"provider": "firebase"})
        >>> middleware = AuthMiddleware(settings, provider)
        >>>
        >>> # Process authentication
        >>> state = {"input": "hello"}
        >>> headers = {"Authorization": "Bearer <token>"}
        >>> result = await middleware.authenticate(state, headers=headers)
        >>> if result.success:
        ...     state = result.state  # State now has __user__
    """

    def __init__(
        self,
        settings: AuthSettings,
        provider: Optional[AuthProvider],
    ):
        """
        Initialize auth middleware.

        Args:
            settings: Auth configuration from YAML
            provider: Auth provider instance (may be None for provider="none")
        """
        self._settings = settings
        self._provider = provider

    @property
    def settings(self) -> AuthSettings:
        """Get the auth settings."""
        return self._settings

    @property
    def provider(self) -> Optional[AuthProvider]:
        """Get the auth provider."""
        return self._provider

    @property
    def is_enabled(self) -> bool:
        """Check if authentication is configured and enabled."""
        return self._settings.provider != "none" and self._provider is not None

    async def authenticate(
        self,
        state: Dict[str, Any],
        headers: Optional[Dict[str, str]] = None,
        query_params: Optional[Dict[str, str]] = None,
        token: Optional[str] = None,
    ) -> "AuthenticateResult":
        """
        Authenticate a request and optionally inject user into state.

        This method:
        1. Extracts the token from headers/query params (or uses provided token)
        2. Verifies the token via the auth provider
        3. Applies claims mapping to user info
        4. Injects user into state if configured
        5. Handles required vs optional auth

        Args:
            state: Current workflow state dictionary
            headers: HTTP headers (for token extraction)
            query_params: URL query parameters (for token extraction)
            token: Optional pre-extracted token (bypasses extraction)

        Returns:
            AuthenticateResult with success/failure and updated state

        Raises:
            AuthenticationError: If auth is required and fails
        """
        if not self.is_enabled:
            # Auth not configured, pass through
            return AuthenticateResult(success=True, state=state, user=None)

        headers = headers or {}
        query_params = query_params or {}

        # Extract token
        if token is None:
            token = self._provider.extract_token(
                headers=headers,
                query_params=query_params,
                token_header=self._settings.token_header,
                token_query_param=self._settings.token_query_param,
            )

        if token is None:
            return self._handle_no_token(state)

        # Verify token
        auth_result = await self._provider.verify_token(token)

        if not auth_result.success:
            return self._handle_auth_failure(
                state, auth_result.error or "Unknown error"
            )

        # Apply claims mapping
        user = auth_result.user
        if user and self._settings.claims_mapping:
            user = user.apply_claims_mapping(self._settings.claims_mapping)

        # Inject user into state
        if self._settings.inject_user and user:
            state = state.copy()
            state[self._settings.user_state_key] = user.to_state_dict()

        return AuthenticateResult(success=True, state=state, user=user)

    def _handle_no_token(self, state: Dict[str, Any]) -> "AuthenticateResult":
        """Handle case when no token is provided."""
        if self._settings.required:
            raise AuthenticationError("Authentication required: no token provided")

        # Optional auth: inject None user and continue
        if self._settings.inject_user:
            state = state.copy()
            state[self._settings.user_state_key] = None

        return AuthenticateResult(success=True, state=state, user=None)

    def _handle_auth_failure(
        self, state: Dict[str, Any], error: str
    ) -> "AuthenticateResult":
        """Handle case when token verification fails."""
        if self._settings.required:
            raise AuthenticationError(f"Authentication failed: {error}")

        # Optional auth: inject None user and continue
        if self._settings.inject_user:
            state = state.copy()
            state[self._settings.user_state_key] = None

        return AuthenticateResult(success=False, state=state, user=None, error=error)


class AuthenticateResult:
    """
    Result of authentication attempt.

    Attributes:
        success: Whether authentication succeeded
        state: Updated state dict (may include __user__)
        user: Authenticated user info (if success)
        error: Error message (if failure)
    """

    def __init__(
        self,
        success: bool,
        state: Dict[str, Any],
        user: Optional[UserInfo] = None,
        error: Optional[str] = None,
    ):
        self.success = success
        self.state = state
        self.user = user
        self.error = error

    def __bool__(self) -> bool:
        return self.success


def create_auth_middleware(
    auth_config: Dict[str, Any],
) -> Optional[AuthMiddleware]:
    """
    Create auth middleware from configuration dictionary.

    This factory function creates both the AuthSettings and AuthProvider,
    then wraps them in an AuthMiddleware instance.

    Args:
        auth_config: Auth configuration from YAML settings.auth section

    Returns:
        AuthMiddleware instance, or None if provider is "none"
    """
    from .settings import parse_auth_settings, build_auth_config_dict
    from . import create_auth_provider

    # Parse settings
    # Wrap in dict for parse_auth_settings which expects settings-level dict
    settings = parse_auth_settings({"auth": auth_config})
    if settings is None:
        return None

    if settings.provider == "none":
        return None

    # Build provider config and create provider
    provider_config = build_auth_config_dict(settings)
    provider = create_auth_provider(provider_config)

    if provider is None:
        return None

    return AuthMiddleware(settings, provider)
