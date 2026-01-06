"""
API Key Authentication Provider.

Story: TEA-BUILTIN-015.3 (Auth Middleware in YAML)

Provides simple API key authentication by matching against a list
of valid keys.
"""

import hashlib
import logging
import secrets
from typing import List, Optional, Set

from .base import AuthProvider, AuthResult, UserInfo

logger = logging.getLogger(__name__)


class APIKeyAuthProvider(AuthProvider):
    """
    API Key Authentication provider.

    Validates API keys against a configured list of valid keys.
    Keys can be provided directly or loaded from a file.

    Security considerations:
    - Keys are stored hashed in memory for comparison
    - Constant-time comparison is used to prevent timing attacks
    - Keys file should have restrictive permissions (600)

    Example YAML:
        settings:
          auth:
            provider: api_key
            token_header: "X-API-Key"
            api_key:
              keys:
                - "${API_KEY_1}"
                - "${API_KEY_2}"

        # Or load from file:
        settings:
          auth:
            provider: api_key
            api_key:
              keys_file: "/path/to/api_keys.txt"
    """

    def __init__(
        self,
        keys: Optional[List[str]] = None,
        keys_file: Optional[str] = None,
    ):
        """
        Initialize API key auth provider.

        Args:
            keys: List of valid API keys
            keys_file: Path to file containing keys (one per line)
        """
        self._key_hashes: Set[str] = set()
        self._keys_file = keys_file
        self._loaded = False

        # Load keys provided directly
        if keys:
            for key in keys:
                if key and key.strip():
                    self._key_hashes.add(self._hash_key(key.strip()))

    def _hash_key(self, key: str) -> str:
        """
        Hash an API key for secure storage/comparison.

        Uses SHA-256 for consistent hashing.
        """
        return hashlib.sha256(key.encode("utf-8")).hexdigest()

    def _load_keys_from_file(self) -> None:
        """
        Load API keys from configured file.

        Keys file should have one key per line.
        Empty lines and lines starting with # are ignored.
        """
        if self._loaded or not self._keys_file:
            return

        try:
            with open(self._keys_file, "r") as f:
                for line in f:
                    line = line.strip()
                    # Skip empty lines and comments
                    if line and not line.startswith("#"):
                        self._key_hashes.add(self._hash_key(line))

            self._loaded = True
            logger.debug(f"Loaded {len(self._key_hashes)} API keys from file")

        except Exception as e:
            logger.error(f"Failed to load API keys from {self._keys_file}: {e}")

    def _verify_key(self, provided_key: str) -> bool:
        """
        Verify an API key using constant-time comparison.

        Args:
            provided_key: The API key to verify

        Returns:
            True if the key is valid, False otherwise.
        """
        # Load keys from file if configured
        self._load_keys_from_file()

        if not self._key_hashes:
            logger.warning("No API keys configured")
            return False

        provided_hash = self._hash_key(provided_key)

        # Check against all hashes using constant-time comparison
        for valid_hash in self._key_hashes:
            if secrets.compare_digest(provided_hash, valid_hash):
                return True

        return False

    async def verify_token(self, token: str) -> AuthResult:
        """
        Verify an API key.

        Args:
            token: API key to verify

        Returns:
            AuthResult with success if key is valid.
        """
        if not token:
            return AuthResult.fail("No API key provided")

        if not self._verify_key(token):
            return AuthResult.fail("Invalid API key")

        # API keys don't have user info, generate a pseudo-UID from key hash
        # (partial hash to avoid exposing the full key)
        key_hash = self._hash_key(token)
        pseudo_uid = f"apikey_{key_hash[:16]}"

        user = UserInfo(
            uid=pseudo_uid,
            provider="api_key",
            claims={"key_prefix": key_hash[:8]},
        )

        return AuthResult.ok(user)

    async def get_user(self, uid: str) -> Optional[UserInfo]:
        """
        Get user by UID.

        Note: API key provider doesn't maintain a user database,
        so this just returns the UID as-is.

        Args:
            uid: User identifier (pseudo-UID from key hash)

        Returns:
            Basic UserInfo with the provided UID.
        """
        if not uid:
            return None

        return UserInfo(
            uid=uid,
            provider="api_key",
            claims={},
        )
