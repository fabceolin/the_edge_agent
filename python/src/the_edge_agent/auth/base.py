"""
Base classes for authentication providers.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides abstract base class for auth providers and data models
for user information and authentication results.
"""

from abc import ABC, abstractmethod
from typing import Optional, Dict, Any

from pydantic import BaseModel, Field


class UserInfo(BaseModel):
    """
    Authenticated user information.

    This model represents the user information extracted from a verified token.
    It is injected into the workflow state as `__user__` by default.

    Attributes:
        uid: Unique user identifier (always present)
        email: User's email address (if available)
        name: User's display name (if available)
        provider: Authentication provider used (firebase, jwt, api_key)
        claims: Raw token claims dictionary
        Additional fields may be added via claims_mapping

    Example:
        >>> user = UserInfo(
        ...     uid="abc123",
        ...     email="user@example.com",
        ...     provider="firebase",
        ...     claims={"custom_claims": {"role": "admin"}}
        ... )
    """

    uid: str = Field(description="Unique user identifier")

    email: Optional[str] = Field(
        default=None,
        description="User's email address",
    )

    name: Optional[str] = Field(
        default=None,
        description="User's display name",
    )

    provider: str = Field(description="Authentication provider used")

    claims: Dict[str, Any] = Field(
        default_factory=dict,
        description="Raw token claims",
    )

    model_config = {"extra": "allow"}

    def apply_claims_mapping(self, mapping: Dict[str, str]) -> "UserInfo":
        """
        Apply claims mapping to add custom fields from token claims.

        Args:
            mapping: Dictionary mapping target field names to claim paths.
                    Dot notation is supported for nested claims
                    (e.g., "custom_claims.roles").

        Returns:
            New UserInfo instance with mapped fields added.

        Example:
            >>> user = UserInfo(
            ...     uid="123", provider="firebase",
            ...     claims={"custom_claims": {"roles": ["admin"]}}
            ... )
            >>> mapped = user.apply_claims_mapping({"roles": "custom_claims.roles"})
            >>> mapped.model_dump()["roles"]
            ['admin']
        """
        extra_fields = {}

        for target_field, claim_path in mapping.items():
            value = self._get_nested_claim(claim_path)
            if value is not None:
                extra_fields[target_field] = value

        if not extra_fields:
            return self

        # Create new instance with extra fields
        return UserInfo(**{**self.model_dump(), **extra_fields})

    def _get_nested_claim(self, path: str) -> Any:
        """
        Get a nested claim value by dot-notation path.

        Args:
            path: Dot-separated path (e.g., "custom_claims.roles")

        Returns:
            Claim value or None if not found
        """
        parts = path.split(".")
        value: Any = self.claims

        for part in parts:
            if isinstance(value, dict):
                value = value.get(part)
            else:
                return None

            if value is None:
                return None

        return value

    def to_state_dict(self) -> Dict[str, Any]:
        """
        Convert to dictionary suitable for state injection.

        Returns:
            Dictionary with all user fields including mapped claims.
        """
        return self.model_dump()


class AuthResult(BaseModel):
    """
    Result of token verification.

    Attributes:
        success: Whether authentication succeeded
        user: UserInfo if authentication succeeded
        error: Error message if authentication failed

    Example:
        >>> result = AuthResult(
        ...     success=True,
        ...     user=UserInfo(uid="123", provider="firebase")
        ... )
        >>> if result.success:
        ...     print(f"Authenticated as {result.user.uid}")
    """

    success: bool = Field(description="Whether authentication succeeded")

    user: Optional[UserInfo] = Field(
        default=None,
        description="User information if authentication succeeded",
    )

    error: Optional[str] = Field(
        default=None,
        description="Error message if authentication failed",
    )

    @classmethod
    def ok(cls, user: UserInfo) -> "AuthResult":
        """Create a successful result."""
        return cls(success=True, user=user)

    @classmethod
    def fail(cls, error: str) -> "AuthResult":
        """Create a failed result."""
        return cls(success=False, error=error)


class AuthProvider(ABC):
    """
    Abstract base class for authentication providers.

    Implementations must provide token verification and optionally
    user profile retrieval.

    Example implementation:
        >>> class MyAuthProvider(AuthProvider):
        ...     async def verify_token(self, token: str) -> AuthResult:
        ...         # Verify token and return result
        ...         return AuthResult.ok(UserInfo(uid="123", provider="my"))
        ...
        ...     async def get_user(self, uid: str) -> Optional[UserInfo]:
        ...         # Fetch full user profile
        ...         return UserInfo(uid=uid, provider="my", name="John")
    """

    @abstractmethod
    async def verify_token(self, token: str) -> AuthResult:
        """
        Verify an authentication token.

        Args:
            token: The token to verify (ID token, JWT, or API key)

        Returns:
            AuthResult with success=True and user info if valid,
            or success=False and error message if invalid.
        """
        pass

    @abstractmethod
    async def get_user(self, uid: str) -> Optional[UserInfo]:
        """
        Get full user profile by UID.

        This method is used by the auth.get_user action to fetch
        additional user information beyond what's in the token.

        Args:
            uid: User's unique identifier

        Returns:
            UserInfo with full profile, or None if user not found.
        """
        pass

    def extract_token(
        self,
        headers: Dict[str, str],
        query_params: Dict[str, str],
        token_header: str = "Authorization",
        token_query_param: Optional[str] = None,
    ) -> Optional[str]:
        """
        Extract authentication token from request.

        Checks header first, then query param if configured.
        Handles Bearer token prefix automatically.

        Args:
            headers: Request headers (case-insensitive lookup)
            query_params: URL query parameters
            token_header: Header name to check (default: Authorization)
            token_query_param: Query param name to check (optional)

        Returns:
            Extracted token or None if not found.
        """
        # Check header first (case-insensitive)
        headers_lower = {k.lower(): v for k, v in headers.items()}
        header_value = headers_lower.get(token_header.lower())

        if header_value:
            # Handle "Bearer <token>" format
            if header_value.lower().startswith("bearer "):
                return header_value[7:]
            return header_value

        # Check query param if configured
        if token_query_param and token_query_param in query_params:
            return query_params[token_query_param]

        return None
