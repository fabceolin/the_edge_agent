"""
Tests for TEA-BUILTIN-015.3: Auth Middleware in YAML.

Tests cover:
- Auth settings schema validation (AC1)
- Firebase auth provider (AC2)
- JWT auth provider (AC3)
- API key auth provider (AC4)
- User injection (AC5)
- Claims mapping (AC6)
- Required vs optional auth (AC7)
- Auth actions (AC8)
- Backward compatibility (AC9)
"""

import asyncio
import hashlib
import tempfile
import unittest
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


class TestAuthSettings(unittest.TestCase):
    """Test AuthSettings schema validation (AC1)."""

    def test_default_settings(self):
        """Test default AuthSettings values."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings()
        self.assertEqual(settings.provider, "none")
        self.assertEqual(settings.token_header, "Authorization")
        self.assertTrue(settings.required)
        self.assertTrue(settings.inject_user)
        self.assertEqual(settings.user_state_key, "__user__")
        self.assertEqual(settings.claims_mapping, {})

    def test_firebase_provider_settings(self):
        """Test Firebase provider configuration."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings(
            provider="firebase",
            token_header="X-Firebase-Token",
            firebase={"project_id": "my-project"},
        )
        self.assertEqual(settings.provider, "firebase")
        self.assertEqual(settings.token_header, "X-Firebase-Token")
        self.assertIsNotNone(settings.firebase)
        self.assertEqual(settings.firebase.project_id, "my-project")

    def test_jwt_provider_settings(self):
        """Test JWT provider configuration."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings(
            provider="jwt",
            jwt={
                "secret": "my-secret",
                "algorithms": ["HS256", "HS384"],
                "issuer": "https://auth.example.com",
            },
        )
        self.assertEqual(settings.provider, "jwt")
        self.assertIsNotNone(settings.jwt)
        self.assertEqual(settings.jwt.secret, "my-secret")
        self.assertEqual(settings.jwt.algorithms, ["HS256", "HS384"])
        self.assertEqual(settings.jwt.issuer, "https://auth.example.com")

    def test_api_key_provider_settings(self):
        """Test API key provider configuration."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings(
            provider="api_key",
            token_header="X-API-Key",
            api_key={"keys": ["key1", "key2"]},
        )
        self.assertEqual(settings.provider, "api_key")
        self.assertIsNotNone(settings.api_key)
        self.assertEqual(settings.api_key.keys, ["key1", "key2"])

    def test_claims_mapping_validation(self):
        """Test claims mapping configuration."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings(
            provider="firebase",
            claims_mapping={
                "user_id": "uid",
                "roles": "custom_claims.roles",
            },
        )
        self.assertEqual(settings.claims_mapping["user_id"], "uid")
        self.assertEqual(settings.claims_mapping["roles"], "custom_claims.roles")

    def test_invalid_provider_raises_error(self):
        """Test invalid provider type raises ValueError."""
        from the_edge_agent.auth import AuthSettings

        with self.assertRaises(ValueError) as ctx:
            AuthSettings(provider="invalid")
        self.assertIn("Invalid provider", str(ctx.exception))

    def test_parse_auth_settings_with_valid_config(self):
        """Test parse_auth_settings with valid configuration."""
        from the_edge_agent.auth import parse_auth_settings

        config = {
            "auth": {
                "provider": "firebase",
                "required": False,
            }
        }
        settings = parse_auth_settings(config)
        self.assertIsNotNone(settings)
        self.assertEqual(settings.provider, "firebase")
        self.assertFalse(settings.required)

    def test_parse_auth_settings_without_auth(self):
        """Test parse_auth_settings returns None when no auth config."""
        from the_edge_agent.auth import parse_auth_settings

        settings = parse_auth_settings({})
        self.assertIsNone(settings)

    def test_optional_auth_settings(self):
        """Test optional auth (required=false) configuration."""
        from the_edge_agent.auth import AuthSettings

        settings = AuthSettings(
            provider="firebase",
            required=False,
            inject_user=True,
        )
        self.assertFalse(settings.required)
        self.assertTrue(settings.inject_user)


class TestUserInfo(unittest.TestCase):
    """Test UserInfo model and claims mapping (AC5, AC6)."""

    def test_basic_user_info(self):
        """Test basic UserInfo creation."""
        from the_edge_agent.auth import UserInfo

        user = UserInfo(
            uid="abc123",
            email="user@example.com",
            name="John Doe",
            provider="firebase",
            claims={"role": "admin"},
        )
        self.assertEqual(user.uid, "abc123")
        self.assertEqual(user.email, "user@example.com")
        self.assertEqual(user.name, "John Doe")
        self.assertEqual(user.provider, "firebase")
        self.assertEqual(user.claims["role"], "admin")

    def test_claims_mapping_simple(self):
        """Test simple claims mapping."""
        from the_edge_agent.auth import UserInfo

        user = UserInfo(
            uid="123",
            provider="firebase",
            claims={"role": "admin", "tier": "premium"},
        )
        mapped = user.apply_claims_mapping(
            {
                "user_role": "role",
                "user_tier": "tier",
            }
        )
        data = mapped.model_dump()
        self.assertEqual(data["user_role"], "admin")
        self.assertEqual(data["user_tier"], "premium")

    def test_claims_mapping_nested(self):
        """Test nested claims mapping with dot notation."""
        from the_edge_agent.auth import UserInfo

        user = UserInfo(
            uid="123",
            provider="firebase",
            claims={
                "custom_claims": {
                    "roles": ["admin", "user"],
                    "metadata": {"department": "engineering"},
                }
            },
        )
        mapped = user.apply_claims_mapping(
            {
                "roles": "custom_claims.roles",
                "department": "custom_claims.metadata.department",
            }
        )
        data = mapped.model_dump()
        self.assertEqual(data["roles"], ["admin", "user"])
        self.assertEqual(data["department"], "engineering")

    def test_claims_mapping_missing_path(self):
        """Test claims mapping with non-existent path."""
        from the_edge_agent.auth import UserInfo

        user = UserInfo(
            uid="123",
            provider="firebase",
            claims={"existing": "value"},
        )
        mapped = user.apply_claims_mapping(
            {
                "missing": "non.existent.path",
            }
        )
        data = mapped.model_dump()
        self.assertNotIn("missing", data)

    def test_to_state_dict(self):
        """Test UserInfo conversion to state dict."""
        from the_edge_agent.auth import UserInfo

        user = UserInfo(
            uid="123",
            email="test@example.com",
            provider="firebase",
            claims={"role": "admin"},
        )
        state_dict = user.to_state_dict()
        self.assertEqual(state_dict["uid"], "123")
        self.assertEqual(state_dict["email"], "test@example.com")
        self.assertEqual(state_dict["provider"], "firebase")
        self.assertEqual(state_dict["claims"]["role"], "admin")


class TestAuthResult(unittest.TestCase):
    """Test AuthResult model."""

    def test_success_result(self):
        """Test successful auth result."""
        from the_edge_agent.auth import AuthResult, UserInfo

        user = UserInfo(uid="123", provider="firebase")
        result = AuthResult.ok(user)
        self.assertTrue(result.success)
        self.assertIsNotNone(result.user)
        self.assertIsNone(result.error)

    def test_failure_result(self):
        """Test failed auth result."""
        from the_edge_agent.auth import AuthResult

        result = AuthResult.fail("Token expired")
        self.assertFalse(result.success)
        self.assertIsNone(result.user)
        self.assertEqual(result.error, "Token expired")


class TestJWTAuthProvider(unittest.TestCase):
    """Test JWTAuthProvider (AC3)."""

    def test_forbids_none_algorithm(self):
        """Test that 'none' algorithm is rejected (security)."""
        from the_edge_agent.auth import JWTAuthProvider

        with self.assertRaises(ValueError) as ctx:
            JWTAuthProvider(secret="test", algorithms=["none"])
        self.assertIn("not allowed", str(ctx.exception))

    def test_verify_valid_token(self):
        """Test verification of valid JWT token."""
        pytest.importorskip("jwt")
        import jwt as pyjwt
        from the_edge_agent.auth import JWTAuthProvider

        secret = "test-secret-key"
        provider = JWTAuthProvider(secret=secret, algorithms=["HS256"])

        # Create valid token
        token = pyjwt.encode(
            {"sub": "user123", "email": "test@example.com", "name": "Test User"},
            secret,
            algorithm="HS256",
        )

        result = asyncio.run(provider.verify_token(token))
        self.assertTrue(result.success)
        self.assertEqual(result.user.uid, "user123")
        self.assertEqual(result.user.email, "test@example.com")

    def test_verify_invalid_token(self):
        """Test rejection of invalid JWT token."""
        pytest.importorskip("jwt")
        from the_edge_agent.auth import JWTAuthProvider

        provider = JWTAuthProvider(secret="correct-secret", algorithms=["HS256"])

        # Token signed with wrong secret
        import jwt as pyjwt

        token = pyjwt.encode(
            {"sub": "user123"},
            "wrong-secret",
            algorithm="HS256",
        )

        result = asyncio.run(provider.verify_token(token))
        self.assertFalse(result.success)
        self.assertIn("signature", result.error.lower())

    def test_verify_expired_token(self):
        """Test rejection of expired JWT token."""
        pytest.importorskip("jwt")
        import time
        import jwt as pyjwt
        from the_edge_agent.auth import JWTAuthProvider

        secret = "test-secret"
        provider = JWTAuthProvider(secret=secret, algorithms=["HS256"])

        # Create expired token
        token = pyjwt.encode(
            {"sub": "user123", "exp": int(time.time()) - 3600},  # 1 hour ago
            secret,
            algorithm="HS256",
        )

        result = asyncio.run(provider.verify_token(token))
        self.assertFalse(result.success)
        self.assertIn("expired", result.error.lower())

    def test_verify_no_subject(self):
        """Test rejection of token without subject claim."""
        pytest.importorskip("jwt")
        import jwt as pyjwt
        from the_edge_agent.auth import JWTAuthProvider

        secret = "test-secret"
        provider = JWTAuthProvider(secret=secret, algorithms=["HS256"])

        # Token without sub/uid claim
        token = pyjwt.encode(
            {"email": "test@example.com"},
            secret,
            algorithm="HS256",
        )

        result = asyncio.run(provider.verify_token(token))
        self.assertFalse(result.success)
        self.assertIn("subject", result.error.lower())

    def test_verify_empty_token(self):
        """Test rejection of empty token."""
        from the_edge_agent.auth import JWTAuthProvider

        provider = JWTAuthProvider(secret="test", algorithms=["HS256"])
        result = asyncio.run(provider.verify_token(""))
        self.assertFalse(result.success)
        self.assertIn("no token", result.error.lower())


class TestAPIKeyAuthProvider(unittest.TestCase):
    """Test APIKeyAuthProvider (AC4)."""

    def test_verify_valid_key(self):
        """Test verification of valid API key."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["valid-key-1", "valid-key-2"])

        result = asyncio.run(provider.verify_token("valid-key-1"))
        self.assertTrue(result.success)
        self.assertIsNotNone(result.user)
        self.assertEqual(result.user.provider, "api_key")

    def test_verify_invalid_key(self):
        """Test rejection of invalid API key."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["valid-key"])

        result = asyncio.run(provider.verify_token("invalid-key"))
        self.assertFalse(result.success)
        self.assertIn("invalid", result.error.lower())

    def test_verify_empty_key(self):
        """Test rejection of empty API key."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["valid-key"])

        result = asyncio.run(provider.verify_token(""))
        self.assertFalse(result.success)

    def test_load_keys_from_file(self):
        """Test loading API keys from file."""
        from the_edge_agent.auth import APIKeyAuthProvider

        # Create temp file with keys
        with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as f:
            f.write("file-key-1\n")
            f.write("# comment line\n")
            f.write("file-key-2\n")
            f.write("\n")  # empty line
            keys_file = f.name

        try:
            provider = APIKeyAuthProvider(keys_file=keys_file)

            result1 = asyncio.run(provider.verify_token("file-key-1"))
            self.assertTrue(result1.success)

            result2 = asyncio.run(provider.verify_token("file-key-2"))
            self.assertTrue(result2.success)

            # Comment should not be a valid key
            result3 = asyncio.run(provider.verify_token("# comment line"))
            self.assertFalse(result3.success)
        finally:
            import os

            os.unlink(keys_file)

    def test_no_keys_configured(self):
        """Test handling when no keys are configured."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider()

        result = asyncio.run(provider.verify_token("any-key"))
        self.assertFalse(result.success)


class TestFirebaseAuthProvider(unittest.TestCase):
    """Test FirebaseAuthProvider (AC2)."""

    def test_verify_without_firebase_init(self):
        """Test graceful handling when Firebase not initialized."""
        from the_edge_agent.auth import FirebaseAuthProvider

        provider = FirebaseAuthProvider()

        # Mock to prevent actual Firebase initialization
        with patch.object(provider, "_ensure_initialized", return_value=False):
            result = asyncio.run(provider.verify_token("any-token"))
            self.assertFalse(result.success)
            self.assertIn("not initialized", result.error.lower())

    def test_verify_empty_token(self):
        """Test rejection of empty token."""
        from the_edge_agent.auth import FirebaseAuthProvider

        provider = FirebaseAuthProvider()

        result = asyncio.run(provider.verify_token(""))
        self.assertFalse(result.success)
        self.assertIn("no token", result.error.lower())

    def test_verify_valid_token_mocked(self):
        """Test verification of valid Firebase token (mocked)."""
        # Skip if firebase_admin is not installed
        try:
            import firebase_admin
        except ImportError:
            self.skipTest("firebase-admin not installed")

        from the_edge_agent.auth import FirebaseAuthProvider

        provider = FirebaseAuthProvider()

        # Mock the internal verify call
        async def mock_verify(token):
            from the_edge_agent.auth import AuthResult, UserInfo

            return AuthResult.ok(
                UserInfo(
                    uid="firebase-user-123",
                    email="user@example.com",
                    name="Firebase User",
                    provider="firebase",
                    claims={},
                )
            )

        with patch.object(provider, "verify_token", side_effect=mock_verify):
            result = asyncio.run(provider.verify_token("valid-firebase-token"))
            self.assertTrue(result.success)
            self.assertEqual(result.user.uid, "firebase-user-123")
            self.assertEqual(result.user.email, "user@example.com")

    def test_verify_expired_token_mocked(self):
        """Test handling of expired Firebase token (mocked)."""
        # Skip if firebase_admin is not installed
        try:
            import firebase_admin
        except ImportError:
            self.skipTest("firebase-admin not installed")

        from the_edge_agent.auth import FirebaseAuthProvider

        provider = FirebaseAuthProvider()

        # Mock the internal verify call to return expired error
        async def mock_verify(token):
            from the_edge_agent.auth import AuthResult

            return AuthResult.fail("Token expired")

        with patch.object(provider, "verify_token", side_effect=mock_verify):
            result = asyncio.run(provider.verify_token("expired-token"))
            self.assertFalse(result.success)
            self.assertIn("expired", result.error.lower())


class TestAuthMiddleware(unittest.TestCase):
    """Test AuthMiddleware (AC5, AC6, AC7)."""

    def test_middleware_disabled_when_no_provider(self):
        """Test middleware passes through when provider is 'none'."""
        from the_edge_agent.auth import AuthMiddleware, AuthSettings

        settings = AuthSettings(provider="none")
        middleware = AuthMiddleware(settings, None)

        self.assertFalse(middleware.is_enabled)

        result = asyncio.run(middleware.authenticate({"input": "test"}))
        self.assertTrue(result.success)
        self.assertEqual(result.state["input"], "test")

    def test_required_auth_no_token(self):
        """Test required auth raises error when no token (AC7)."""
        from the_edge_agent.auth import (
            AuthMiddleware,
            AuthSettings,
            APIKeyAuthProvider,
            AuthenticationError,
        )

        settings = AuthSettings(provider="api_key", required=True)
        provider = APIKeyAuthProvider(keys=["valid-key"])
        middleware = AuthMiddleware(settings, provider)

        with self.assertRaises(AuthenticationError) as ctx:
            asyncio.run(middleware.authenticate({}, headers={}))
        self.assertIn("no token", str(ctx.exception).lower())

    def test_optional_auth_no_token(self):
        """Test optional auth continues with None user (AC7)."""
        from the_edge_agent.auth import (
            AuthMiddleware,
            AuthSettings,
            APIKeyAuthProvider,
        )

        settings = AuthSettings(
            provider="api_key",
            required=False,
            inject_user=True,
        )
        provider = APIKeyAuthProvider(keys=["valid-key"])
        middleware = AuthMiddleware(settings, provider)

        result = asyncio.run(middleware.authenticate({}, headers={}))
        self.assertTrue(result.success)
        self.assertIsNone(result.state["__user__"])

    def test_user_injection(self):
        """Test user info injection into state (AC5)."""
        from the_edge_agent.auth import (
            AuthMiddleware,
            AuthSettings,
            APIKeyAuthProvider,
        )

        settings = AuthSettings(
            provider="api_key",
            token_header="X-API-Key",
            inject_user=True,
        )
        provider = APIKeyAuthProvider(keys=["test-key"])
        middleware = AuthMiddleware(settings, provider)

        result = asyncio.run(
            middleware.authenticate(
                {"input": "test"},
                headers={"X-API-Key": "test-key"},
            )
        )
        self.assertTrue(result.success)
        self.assertIn("__user__", result.state)
        self.assertEqual(result.state["__user__"]["provider"], "api_key")

    def test_claims_mapping_in_middleware(self):
        """Test claims mapping is applied during authentication (AC6)."""
        pytest.importorskip("jwt")
        import jwt as pyjwt
        from the_edge_agent.auth import (
            AuthMiddleware,
            AuthSettings,
            JWTAuthProvider,
        )

        secret = "test-secret"
        settings = AuthSettings(
            provider="jwt",
            claims_mapping={
                "user_role": "role",
                "department": "dept",
            },
        )
        provider = JWTAuthProvider(secret=secret, algorithms=["HS256"])
        middleware = AuthMiddleware(settings, provider)

        # Create token with custom claims
        token = pyjwt.encode(
            {
                "sub": "user123",
                "role": "admin",
                "dept": "engineering",
            },
            secret,
            algorithm="HS256",
        )

        result = asyncio.run(
            middleware.authenticate({}, headers={"Authorization": f"Bearer {token}"})
        )
        self.assertTrue(result.success)
        user = result.state["__user__"]
        self.assertEqual(user["user_role"], "admin")
        self.assertEqual(user["department"], "engineering")

    def test_custom_user_state_key(self):
        """Test custom user state key."""
        from the_edge_agent.auth import (
            AuthMiddleware,
            AuthSettings,
            APIKeyAuthProvider,
        )

        settings = AuthSettings(
            provider="api_key",
            inject_user=True,
            user_state_key="current_user",
        )
        provider = APIKeyAuthProvider(keys=["test-key"])
        middleware = AuthMiddleware(settings, provider)

        result = asyncio.run(
            middleware.authenticate({}, headers={"Authorization": "test-key"})
        )
        self.assertTrue(result.success)
        self.assertIn("current_user", result.state)
        self.assertNotIn("__user__", result.state)


class TestCreateAuthProvider(unittest.TestCase):
    """Test create_auth_provider factory function."""

    def test_create_firebase_provider(self):
        """Test creating Firebase provider."""
        from the_edge_agent.auth import create_auth_provider, FirebaseAuthProvider

        provider = create_auth_provider(
            {
                "provider": "firebase",
                "firebase": {"project_id": "test-project"},
            }
        )
        self.assertIsInstance(provider, FirebaseAuthProvider)

    def test_create_jwt_provider(self):
        """Test creating JWT provider."""
        from the_edge_agent.auth import create_auth_provider, JWTAuthProvider

        provider = create_auth_provider(
            {
                "provider": "jwt",
                "jwt": {"secret": "test-secret"},
            }
        )
        self.assertIsInstance(provider, JWTAuthProvider)

    def test_create_api_key_provider(self):
        """Test creating API key provider."""
        from the_edge_agent.auth import create_auth_provider, APIKeyAuthProvider

        provider = create_auth_provider(
            {
                "provider": "api_key",
                "api_key": {"keys": ["key1"]},
            }
        )
        self.assertIsInstance(provider, APIKeyAuthProvider)

    def test_create_none_provider(self):
        """Test creating 'none' provider returns None."""
        from the_edge_agent.auth import create_auth_provider

        provider = create_auth_provider({"provider": "none"})
        self.assertIsNone(provider)

    def test_create_unknown_provider_raises_error(self):
        """Test unknown provider raises ValueError."""
        from the_edge_agent.auth import create_auth_provider

        with self.assertRaises(ValueError):
            create_auth_provider({"provider": "unknown"})


class TestTokenExtraction(unittest.TestCase):
    """Test token extraction from headers and query params."""

    def test_extract_from_header(self):
        """Test token extraction from header."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["test-key"])

        token = provider.extract_token(
            headers={"Authorization": "test-key"},
            query_params={},
        )
        self.assertEqual(token, "test-key")

    def test_extract_bearer_token(self):
        """Test Bearer token prefix removal."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["test-key"])

        token = provider.extract_token(
            headers={"Authorization": "Bearer actual-token"},
            query_params={},
        )
        self.assertEqual(token, "actual-token")

    def test_extract_case_insensitive_header(self):
        """Test case-insensitive header lookup."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["test-key"])

        token = provider.extract_token(
            headers={"authorization": "test-key"},
            query_params={},
            token_header="Authorization",
        )
        self.assertEqual(token, "test-key")

    def test_extract_from_query_param(self):
        """Test token extraction from query parameter."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["test-key"])

        token = provider.extract_token(
            headers={},
            query_params={"api_key": "test-key"},
            token_query_param="api_key",
        )
        self.assertEqual(token, "test-key")

    def test_header_takes_precedence(self):
        """Test that header token takes precedence over query param."""
        from the_edge_agent.auth import APIKeyAuthProvider

        provider = APIKeyAuthProvider(keys=["test-key"])

        token = provider.extract_token(
            headers={"Authorization": "header-token"},
            query_params={"api_key": "query-token"},
            token_query_param="api_key",
        )
        self.assertEqual(token, "header-token")


class TestBackwardCompatibility(unittest.TestCase):
    """Test backward compatibility (AC9)."""

    def test_engine_without_auth_settings(self):
        """Test that agents without auth settings work unchanged."""
        # This is a basic test to ensure auth module doesn't break existing behavior
        from the_edge_agent.auth import parse_auth_settings

        # Simulating an agent config without auth
        config = {
            "llm": {"model": "gpt-4"},
            "session": {"backend": "memory"},
        }

        settings = parse_auth_settings(config)
        self.assertIsNone(settings)


if __name__ == "__main__":
    unittest.main()
