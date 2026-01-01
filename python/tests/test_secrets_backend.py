"""
Unit tests for Secrets Backend Module (TEA-BUILTIN-012.1).

Tests cover:
- SecretsBackend protocol compliance (AC-1)
- EnvSecretsBackend with prefix filtering (AC-2)
- Factory function create_secrets_backend (AC-3)
- YAMLEngine integration with settings.secrets (AC-4)
- get_all() for Jinja2 context (AC-5)
- Lazy import behavior for cloud backends (AC-6)
"""

import os
import pytest
from unittest import TestCase


class TestSecretsBackendProtocol(TestCase):
    """Test SecretsBackend abstract base class (AC-1)."""

    def test_protocol_is_abstract(self):
        """GIVEN SecretsBackend ABC, WHEN subclassed without implementing methods, THEN TypeError."""
        from the_edge_agent.secrets import SecretsBackend

        with self.assertRaises(TypeError):
            # Cannot instantiate abstract class
            SecretsBackend()

    def test_protocol_requires_get(self):
        """GIVEN SecretsBackend ABC, WHEN get() not implemented, THEN TypeError."""
        from the_edge_agent.secrets import SecretsBackend

        class IncompleteBackend(SecretsBackend):
            def get_all(self):
                return {}

            def has(self, key):
                return False

        with self.assertRaises(TypeError):
            IncompleteBackend()

    def test_protocol_requires_get_all(self):
        """GIVEN SecretsBackend ABC, WHEN get_all() not implemented, THEN TypeError."""
        from the_edge_agent.secrets import SecretsBackend

        class IncompleteBackend(SecretsBackend):
            def get(self, key, default=None):
                return default

            def has(self, key):
                return False

        with self.assertRaises(TypeError):
            IncompleteBackend()

    def test_protocol_requires_has(self):
        """GIVEN SecretsBackend ABC, WHEN has() not implemented, THEN TypeError."""
        from the_edge_agent.secrets import SecretsBackend

        class IncompleteBackend(SecretsBackend):
            def get(self, key, default=None):
                return default

            def get_all(self):
                return {}

        with self.assertRaises(TypeError):
            IncompleteBackend()

    def test_protocol_close_has_default(self):
        """GIVEN SecretsBackend ABC, WHEN close() not implemented, THEN default no-op works."""
        from the_edge_agent.secrets import SecretsBackend

        class MinimalBackend(SecretsBackend):
            def get(self, key, default=None):
                return default

            def get_all(self):
                return {}

            def has(self, key):
                return False

        backend = MinimalBackend()
        # Should not raise - default close() is no-op
        backend.close()


class TestEnvSecretsBackend(TestCase):
    """Test EnvSecretsBackend implementation (AC-2, AC-5)."""

    def setUp(self):
        """Set up test environment variables."""
        # Store original env vars to restore later
        self._original_env = {}
        self._test_keys = [
            "MYAPP_API_KEY",
            "MYAPP_DB_PASSWORD",
            "OTHER_KEY",
            "TEST_PREFIX_VAR1",
            "TEST_PREFIX_VAR2",
        ]
        for key in self._test_keys:
            if key in os.environ:
                self._original_env[key] = os.environ[key]

    def tearDown(self):
        """Restore original environment."""
        for key in self._test_keys:
            if key in self._original_env:
                os.environ[key] = self._original_env[key]
            elif key in os.environ:
                del os.environ[key]

    def test_get_with_prefix(self):
        """GIVEN env vars with prefix, WHEN get() called, THEN returns value (AC-2)."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["MYAPP_API_KEY"] = "secret123"
        backend = EnvSecretsBackend(prefix="MYAPP_")

        result = backend.get("API_KEY")
        self.assertEqual(result, "secret123")

    def test_get_without_prefix(self):
        """GIVEN env vars, WHEN get() without prefix, THEN uses full key."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["MYAPP_API_KEY"] = "secret123"
        backend = EnvSecretsBackend()

        result = backend.get("MYAPP_API_KEY")
        self.assertEqual(result, "secret123")

    def test_get_missing_returns_default(self):
        """GIVEN missing key, WHEN get() called, THEN returns default."""
        from the_edge_agent.secrets import EnvSecretsBackend

        backend = EnvSecretsBackend(prefix="MYAPP_")

        result = backend.get("NONEXISTENT", default="fallback")
        self.assertEqual(result, "fallback")

    def test_get_missing_returns_none_default(self):
        """GIVEN missing key, WHEN get() called without default, THEN returns None."""
        from the_edge_agent.secrets import EnvSecretsBackend

        backend = EnvSecretsBackend(prefix="MYAPP_")

        result = backend.get("NONEXISTENT")
        self.assertIsNone(result)

    def test_get_all_filters_by_prefix(self):
        """GIVEN mixed env vars, WHEN get_all() called, THEN only prefixed returned (AC-2, AC-5)."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["MYAPP_API_KEY"] = "val1"
        os.environ["MYAPP_DB_PASSWORD"] = "val2"
        os.environ["OTHER_KEY"] = "other"

        backend = EnvSecretsBackend(prefix="MYAPP_")
        secrets = backend.get_all()

        self.assertEqual(secrets, {"API_KEY": "val1", "DB_PASSWORD": "val2"})
        self.assertNotIn("OTHER_KEY", secrets)

    def test_get_all_without_prefix_returns_all(self):
        """GIVEN env vars, WHEN get_all() without prefix, THEN returns all env vars (AC-5)."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["TEST_PREFIX_VAR1"] = "val1"
        backend = EnvSecretsBackend()
        secrets = backend.get_all()

        # Should contain PATH and our test var
        self.assertIn("TEST_PREFIX_VAR1", secrets)
        self.assertEqual(secrets["TEST_PREFIX_VAR1"], "val1")

    def test_has_returns_true_for_existing(self):
        """GIVEN existing key, WHEN has() called, THEN returns True."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["MYAPP_API_KEY"] = "secret"
        backend = EnvSecretsBackend(prefix="MYAPP_")

        self.assertTrue(backend.has("API_KEY"))

    def test_has_returns_false_for_missing(self):
        """GIVEN missing key, WHEN has() called, THEN returns False."""
        from the_edge_agent.secrets import EnvSecretsBackend

        backend = EnvSecretsBackend(prefix="MYAPP_")

        self.assertFalse(backend.has("NONEXISTENT"))

    def test_close_is_noop(self):
        """GIVEN EnvSecretsBackend, WHEN close() called, THEN no error."""
        from the_edge_agent.secrets import EnvSecretsBackend

        backend = EnvSecretsBackend()
        backend.close()  # Should not raise

    def test_context_manager(self):
        """GIVEN EnvSecretsBackend, WHEN used as context manager, THEN works."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["MYAPP_API_KEY"] = "secret"

        with EnvSecretsBackend(prefix="MYAPP_") as backend:
            result = backend.get("API_KEY")
            self.assertEqual(result, "secret")

    def test_repr(self):
        """GIVEN EnvSecretsBackend, WHEN repr() called, THEN returns string."""
        from the_edge_agent.secrets import EnvSecretsBackend

        backend = EnvSecretsBackend(prefix="MYAPP_")
        repr_str = repr(backend)

        self.assertIn("EnvSecretsBackend", repr_str)
        self.assertIn("MYAPP_", repr_str)


class TestSecretsFactory(TestCase):
    """Test create_secrets_backend factory function (AC-3, AC-6)."""

    def test_factory_creates_env_backend(self):
        """GIVEN 'env' type, WHEN factory called, THEN EnvSecretsBackend returned (AC-3)."""
        from the_edge_agent.secrets import create_secrets_backend, EnvSecretsBackend

        backend = create_secrets_backend("env", prefix="TEST_")

        self.assertIsInstance(backend, EnvSecretsBackend)

    def test_factory_default_is_env(self):
        """GIVEN no type, WHEN factory called, THEN EnvSecretsBackend returned."""
        from the_edge_agent.secrets import create_secrets_backend, EnvSecretsBackend

        backend = create_secrets_backend()

        self.assertIsInstance(backend, EnvSecretsBackend)

    def test_factory_unknown_type_raises(self):
        """GIVEN unknown type, WHEN factory called, THEN ValueError raised."""
        from the_edge_agent.secrets import create_secrets_backend

        with self.assertRaises(ValueError) as ctx:
            create_secrets_backend("unknown")

        self.assertIn("Unknown secrets backend type", str(ctx.exception))
        self.assertIn("unknown", str(ctx.exception))

    def test_factory_case_insensitive(self):
        """GIVEN uppercase type, WHEN factory called, THEN works."""
        from the_edge_agent.secrets import create_secrets_backend, EnvSecretsBackend

        backend = create_secrets_backend("ENV")

        self.assertIsInstance(backend, EnvSecretsBackend)

    def test_factory_forwards_config(self):
        """GIVEN config kwargs, WHEN factory called, THEN forwarded to backend (AC-3)."""
        from the_edge_agent.secrets import create_secrets_backend

        os.environ["CUSTOM_KEY"] = "value"
        backend = create_secrets_backend("env", prefix="CUSTOM_")

        result = backend.get("KEY")
        self.assertEqual(result, "value")

    def test_factory_aws_requires_config(self):
        """GIVEN 'aws' type without required args, WHEN factory called, THEN ValueError (TEA-BUILTIN-012.2)."""
        from the_edge_agent.secrets import create_secrets_backend
        from unittest.mock import patch

        # Mock boto3 to avoid real AWS calls
        with patch("boto3.client"):
            with self.assertRaises(ValueError) as ctx:
                create_secrets_backend("aws")  # Missing secret_name or secret_prefix

            self.assertIn("Must specify either", str(ctx.exception))

    def test_factory_azure_requires_config(self):
        """GIVEN 'azure' type without vault_url, WHEN factory called, THEN ValueError (TEA-BUILTIN-012.2)."""
        from the_edge_agent.secrets import create_secrets_backend

        # The AzureSecretsBackend constructor checks vault_url before importing azure modules
        with self.assertRaises(ValueError) as ctx:
            create_secrets_backend("azure", vault_url="")  # Empty vault_url

        self.assertIn("vault_url is required", str(ctx.exception))

    def test_factory_gcp_requires_config(self):
        """GIVEN 'gcp' type without project_id, WHEN factory called, THEN ValueError (TEA-BUILTIN-012.2)."""
        from the_edge_agent.secrets import create_secrets_backend

        # The GCPSecretsBackend constructor checks project_id before using the client
        with self.assertRaises(ValueError) as ctx:
            create_secrets_backend("gcp", project_id="")  # Empty project_id

        self.assertIn("project_id is required", str(ctx.exception))

    def test_get_registered_backends_includes_all(self):
        """GIVEN registry, WHEN get_registered_backends() called, THEN includes all."""
        from the_edge_agent.secrets import get_registered_backends

        backends = get_registered_backends()

        self.assertIn("env", backends)
        self.assertIn("aws", backends)
        self.assertIn("azure", backends)
        self.assertIn("gcp", backends)


class TestLazyImports(TestCase):
    """Test lazy import behavior (AC-6)."""

    def test_import_secrets_module_succeeds(self):
        """GIVEN secrets module, WHEN imported without cloud SDKs, THEN no ImportError (AC-6)."""
        # This should not raise even without boto3, azure-*, google-cloud-*
        from the_edge_agent.secrets import SecretsBackend, EnvSecretsBackend
        from the_edge_agent.secrets import (
            create_secrets_backend,
            get_registered_backends,
        )

        # Verify exports work
        self.assertIsNotNone(SecretsBackend)
        self.assertIsNotNone(EnvSecretsBackend)
        self.assertIsNotNone(create_secrets_backend)
        self.assertIsNotNone(get_registered_backends)

    def test_env_backend_works_without_cloud_deps(self):
        """GIVEN no cloud SDKs, WHEN EnvSecretsBackend used, THEN works (AC-6)."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["LAZY_TEST"] = "value"
        backend = EnvSecretsBackend()

        self.assertEqual(backend.get("LAZY_TEST"), "value")


class TestYAMLEngineIntegration(TestCase):
    """Test YAMLEngine integration with settings.secrets (AC-4)."""

    def setUp(self):
        """Set up test environment."""
        self._original_env = {}
        self._test_keys = ["MYAPP_API_KEY", "MYAPP_DB_PASSWORD"]
        for key in self._test_keys:
            if key in os.environ:
                self._original_env[key] = os.environ[key]

        os.environ["MYAPP_API_KEY"] = "secret123"
        os.environ["MYAPP_DB_PASSWORD"] = "password456"

    def tearDown(self):
        """Restore original environment."""
        for key in self._test_keys:
            if key in self._original_env:
                os.environ[key] = self._original_env[key]
            elif key in os.environ:
                del os.environ[key]

    def test_yaml_engine_loads_secrets_from_settings(self):
        """GIVEN settings.secrets config, WHEN engine loads, THEN secrets populated (AC-4)."""
        from the_edge_agent import YAMLEngine

        config = {
            "settings": {"secrets": {"backend": "env", "prefix": "MYAPP_"}},
            "state_schema": {"msg": str},
            "nodes": [{"name": "test", "run": 'return {"msg": "ok"}'}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        engine = YAMLEngine()
        engine.load_from_dict(config)

        self.assertEqual(
            engine.secrets, {"API_KEY": "secret123", "DB_PASSWORD": "password456"}
        )

    def test_yaml_engine_creates_secrets_backend(self):
        """GIVEN settings.secrets config, WHEN engine loads, THEN _secrets_backend set (AC-4)."""
        from the_edge_agent import YAMLEngine
        from the_edge_agent.secrets import EnvSecretsBackend

        config = {
            "settings": {"secrets": {"backend": "env", "prefix": "MYAPP_"}},
            "state_schema": {},
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        engine = YAMLEngine()
        engine.load_from_dict(config)

        self.assertIsNotNone(engine._secrets_backend)
        self.assertIsInstance(engine._secrets_backend, EnvSecretsBackend)

    def test_yaml_engine_close_closes_secrets_backend(self):
        """GIVEN engine with secrets backend, WHEN close() called, THEN backend closed (AC-4)."""
        from the_edge_agent import YAMLEngine

        config = {
            "settings": {"secrets": {"backend": "env", "prefix": "MYAPP_"}},
            "state_schema": {},
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        engine = YAMLEngine()
        engine.load_from_dict(config)
        self.assertIsNotNone(engine._secrets_backend)

        engine.close()
        self.assertIsNone(engine._secrets_backend)

    def test_yaml_engine_without_secrets_config(self):
        """GIVEN no settings.secrets, WHEN engine loads, THEN no secrets backend."""
        from the_edge_agent import YAMLEngine

        config = {
            "state_schema": {},
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        engine = YAMLEngine()
        engine.load_from_dict(config)

        self.assertIsNone(engine._secrets_backend)
        self.assertEqual(engine.secrets, {})

    def test_yaml_engine_secrets_default_backend_is_env(self):
        """GIVEN settings.secrets without backend type, WHEN engine loads, THEN uses env."""
        from the_edge_agent import YAMLEngine
        from the_edge_agent.secrets import EnvSecretsBackend

        config = {
            "settings": {"secrets": {"prefix": "MYAPP_"}},
            "state_schema": {},
            "nodes": [{"name": "test", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "test"},
                {"from": "test", "to": "__end__"},
            ],
        }

        engine = YAMLEngine()
        engine.load_from_dict(config)

        self.assertIsInstance(engine._secrets_backend, EnvSecretsBackend)


class TestSecretsForJinja2(TestCase):
    """Test get_all() returns Jinja2-compatible dict (AC-5)."""

    def test_get_all_returns_dict(self):
        """GIVEN secrets, WHEN get_all() called, THEN returns dict."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["JINJA_KEY1"] = "val1"
        os.environ["JINJA_KEY2"] = "val2"
        backend = EnvSecretsBackend(prefix="JINJA_")

        result = backend.get_all()

        self.assertIsInstance(result, dict)

    def test_get_all_dict_usable_in_jinja2(self):
        """GIVEN get_all() result, WHEN used in Jinja2, THEN works (AC-5)."""
        from jinja2 import Template
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["TPL_API_KEY"] = "secret123"
        backend = EnvSecretsBackend(prefix="TPL_")
        secrets = backend.get_all()

        template = Template("API Key: {{ secrets.API_KEY }}")
        result = template.render(secrets=secrets)

        self.assertEqual(result, "API Key: secret123")

    def test_get_all_returns_copy(self):
        """GIVEN get_all() result, WHEN modified, THEN original unchanged."""
        from the_edge_agent.secrets import EnvSecretsBackend

        os.environ["COPY_KEY"] = "original"
        backend = EnvSecretsBackend(prefix="COPY_")

        secrets = backend.get_all()
        secrets["NEW_KEY"] = "added"

        # Second call should not include "NEW_KEY"
        secrets2 = backend.get_all()
        self.assertNotIn("NEW_KEY", secrets2)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
