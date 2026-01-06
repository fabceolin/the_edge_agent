"""
Firebase Authentication Provider.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides Firebase ID token verification using firebase-admin SDK.
"""

import logging
from typing import Optional

from .base import AuthProvider, AuthResult, UserInfo

logger = logging.getLogger(__name__)


class FirebaseAuthProvider(AuthProvider):
    """
    Firebase Authentication provider using firebase-admin SDK.

    Verifies Firebase ID tokens and retrieves user profiles from
    Firebase Authentication.

    Requires:
        pip install firebase-admin

    Configuration:
        - GOOGLE_APPLICATION_CREDENTIALS environment variable, or
        - Firebase Admin SDK initialized before provider creation

    Example YAML:
        settings:
          auth:
            provider: firebase
            token_header: "X-Firebase-Token"
            firebase:
              project_id: "my-firebase-project"
    """

    def __init__(self, project_id: Optional[str] = None):
        """
        Initialize Firebase auth provider.

        Args:
            project_id: Firebase project ID. If None, uses default
                       from initialized Firebase Admin SDK.
        """
        self._project_id = project_id
        self._app = None
        self._initialized = False

    def _ensure_initialized(self) -> bool:
        """
        Ensure Firebase Admin SDK is initialized.

        Returns:
            True if initialized successfully, False otherwise.
        """
        if self._initialized:
            return True

        try:
            import firebase_admin
            from firebase_admin import credentials

            # Check if already initialized
            try:
                self._app = firebase_admin.get_app()
                self._initialized = True
                return True
            except ValueError:
                # Not initialized, try to initialize
                pass

            # Try default credentials
            try:
                cred = credentials.ApplicationDefault()
                options = {}
                if self._project_id:
                    options["projectId"] = self._project_id
                self._app = firebase_admin.initialize_app(cred, options)
                self._initialized = True
                return True
            except Exception as e:
                logger.warning(
                    f"Failed to initialize Firebase with default credentials: {e}"
                )
                return False

        except ImportError:
            logger.warning(
                "firebase-admin not installed. "
                "Install with: pip install firebase-admin"
            )
            return False

    async def verify_token(self, token: str) -> AuthResult:
        """
        Verify a Firebase ID token.

        Args:
            token: Firebase ID token from client SDK

        Returns:
            AuthResult with user info on success, error on failure.
        """
        if not token:
            return AuthResult.fail("No token provided")

        if not self._ensure_initialized():
            return AuthResult.fail("Firebase Admin SDK not initialized")

        try:
            from firebase_admin import auth

            # Verify the ID token
            decoded = auth.verify_id_token(token)

            # Extract user info from decoded token
            user = UserInfo(
                uid=decoded["uid"],
                email=decoded.get("email"),
                name=decoded.get("name"),
                provider="firebase",
                claims=decoded,
            )

            return AuthResult.ok(user)

        except Exception as e:
            error_msg = str(e)

            # Sanitize error message to avoid exposing sensitive info
            if "expired" in error_msg.lower():
                return AuthResult.fail("Token expired")
            elif "invalid" in error_msg.lower():
                return AuthResult.fail("Invalid token")
            elif "revoked" in error_msg.lower():
                return AuthResult.fail("Token revoked")
            else:
                logger.debug(f"Firebase token verification failed: {error_msg}")
                return AuthResult.fail("Token verification failed")

    async def get_user(self, uid: str) -> Optional[UserInfo]:
        """
        Get full user profile from Firebase Authentication.

        Args:
            uid: Firebase user UID

        Returns:
            UserInfo with full profile, or None if user not found.
        """
        if not uid:
            return None

        if not self._ensure_initialized():
            return None

        try:
            from firebase_admin import auth

            # Get user record from Firebase Auth
            user_record = auth.get_user(uid)

            return UserInfo(
                uid=user_record.uid,
                email=user_record.email,
                name=user_record.display_name,
                provider="firebase",
                claims=user_record.custom_claims or {},
            )

        except Exception as e:
            logger.debug(f"Failed to get Firebase user {uid}: {e}")
            return None
