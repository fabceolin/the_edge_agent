"""
JWT Authentication Provider.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides generic JWT token verification using PyJWT library.
"""

import logging
from typing import List, Optional

from .base import AuthProvider, AuthResult, UserInfo

logger = logging.getLogger(__name__)


class JWTAuthProvider(AuthProvider):
    """
    JWT Authentication provider using PyJWT library.

    Verifies JWT tokens with configurable secret/public key,
    algorithms, issuer, and audience validation.

    Requires:
        pip install PyJWT

    Example YAML:
        settings:
          auth:
            provider: jwt
            token_header: "Authorization"
            jwt:
              secret: "${JWT_SECRET}"
              algorithms: ["HS256"]
              issuer: "https://auth.example.com"
              audience: "my-api"

        # Or with RS256 public key:
        settings:
          auth:
            provider: jwt
            jwt:
              public_key_path: "/path/to/public.pem"
              algorithms: ["RS256"]
    """

    # Security: Explicitly disallow 'none' algorithm to prevent bypass attacks
    FORBIDDEN_ALGORITHMS = {"none", "None", "NONE"}

    def __init__(
        self,
        secret: Optional[str] = None,
        public_key_path: Optional[str] = None,
        algorithms: Optional[List[str]] = None,
        issuer: Optional[str] = None,
        audience: Optional[str] = None,
    ):
        """
        Initialize JWT auth provider.

        Args:
            secret: Secret key for symmetric algorithms (HS256, HS384, HS512)
            public_key_path: Path to public key file for asymmetric algorithms
            algorithms: List of accepted algorithms (default: ["HS256"])
            issuer: Expected token issuer (iss claim)
            audience: Expected token audience (aud claim)
        """
        self._secret = secret
        self._public_key_path = public_key_path
        self._algorithms = algorithms or ["HS256"]
        self._issuer = issuer
        self._audience = audience
        self._public_key: Optional[str] = None

        # Security: Validate algorithms to prevent 'none' algorithm attack
        for alg in self._algorithms:
            if alg in self.FORBIDDEN_ALGORITHMS:
                raise ValueError(
                    f"Algorithm '{alg}' is not allowed for security reasons"
                )

    def _get_verification_key(self) -> Optional[str]:
        """
        Get the key used for token verification.

        Returns:
            Secret string for symmetric algorithms,
            public key content for asymmetric algorithms.
        """
        if self._secret:
            return self._secret

        if self._public_key_path:
            if self._public_key is None:
                try:
                    with open(self._public_key_path, "r") as f:
                        self._public_key = f.read()
                except Exception as e:
                    logger.error(f"Failed to read public key: {e}")
                    return None
            return self._public_key

        return None

    async def verify_token(self, token: str) -> AuthResult:
        """
        Verify a JWT token.

        Args:
            token: JWT token string

        Returns:
            AuthResult with user info on success, error on failure.
        """
        if not token:
            return AuthResult.fail("No token provided")

        key = self._get_verification_key()
        if not key:
            return AuthResult.fail("JWT verification key not configured")

        try:
            import jwt as pyjwt
        except ImportError:
            logger.warning("PyJWT not installed. Install with: pip install PyJWT")
            return AuthResult.fail("JWT library not available")

        try:
            # Build verification options
            options = {}
            if self._issuer:
                options["require"] = options.get("require", []) + ["iss"]
            if self._audience:
                options["require"] = options.get("require", []) + ["aud"]

            # Decode and verify the token
            decoded = pyjwt.decode(
                token,
                key,
                algorithms=self._algorithms,
                issuer=self._issuer,
                audience=self._audience,
                options=options if options else None,
            )

            # Extract standard claims for user info
            uid = decoded.get("sub") or decoded.get("uid") or decoded.get("user_id")
            if not uid:
                return AuthResult.fail("Token missing subject (sub) claim")

            user = UserInfo(
                uid=str(uid),
                email=decoded.get("email"),
                name=decoded.get("name") or decoded.get("preferred_username"),
                provider="jwt",
                claims=decoded,
            )

            return AuthResult.ok(user)

        except Exception as e:
            error_msg = str(e)

            # Sanitize error messages
            if "expired" in error_msg.lower():
                return AuthResult.fail("Token expired")
            elif "invalid" in error_msg.lower():
                return AuthResult.fail("Invalid token")
            elif "signature" in error_msg.lower():
                return AuthResult.fail("Invalid token signature")
            elif "issuer" in error_msg.lower():
                return AuthResult.fail("Invalid token issuer")
            elif "audience" in error_msg.lower():
                return AuthResult.fail("Invalid token audience")
            else:
                logger.debug(f"JWT verification failed: {error_msg}")
                return AuthResult.fail("Token verification failed")

    async def get_user(self, uid: str) -> Optional[UserInfo]:
        """
        Get user by UID.

        Note: JWT provider doesn't have a user database, so this
        returns minimal info based on UID only.

        Args:
            uid: User identifier from token

        Returns:
            Basic UserInfo with just UID, or None if not provided.
        """
        if not uid:
            return None

        return UserInfo(
            uid=uid,
            provider="jwt",
            claims={},
        )
