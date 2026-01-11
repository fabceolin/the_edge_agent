"""
Tests for ducklake backend alias (TEA-LTM-010).

The ducklake alias provides a simplified configuration for DuckDB storage
with pluggable catalog backends and sensible defaults.
"""

import unittest
import tempfile
import shutil
import os
from unittest.mock import patch

from the_edge_agent.memory import (
    expand_ltm_config,
    parse_ltm_config,
    create_ltm_backend,
)


class TestDucklakeConfigExpansion(unittest.TestCase):
    """Unit tests for ducklake config expansion (AC1, AC2, AC3)."""

    def test_ducklake_expands_to_duckdb(self):
        """Ducklake should expand to duckdb backend."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["backend"], "duckdb")

    def test_ducklake_default_catalog_type(self):
        """Ducklake should default to sqlite catalog."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["catalog"]["type"], "sqlite")

    def test_ducklake_default_catalog_path(self):
        """Ducklake should default catalog path to ./ltm_catalog.db."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["catalog"]["path"], "./ltm_catalog.db")

    def test_ducklake_default_storage_uri(self):
        """Ducklake should default storage uri to ./ltm_data/."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["storage"]["uri"], "./ltm_data/")

    def test_ducklake_default_lazy(self):
        """Ducklake should default lazy to True."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertTrue(config["lazy"])

    def test_ducklake_default_inline_threshold(self):
        """Ducklake should default inline_threshold to 4096."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["inline_threshold"], 4096)

    def test_ducklake_all_defaults(self):
        """Ducklake with no options should apply all defaults (AC3)."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["backend"], "duckdb")
        self.assertEqual(config["catalog"]["type"], "sqlite")
        self.assertEqual(config["catalog"]["path"], "./ltm_catalog.db")
        self.assertEqual(config["storage"]["uri"], "./ltm_data/")
        self.assertTrue(config["lazy"])
        self.assertEqual(config["inline_threshold"], 4096)


class TestDucklakeCatalogOverride(unittest.TestCase):
    """Tests for catalog type override (AC2)."""

    def test_ducklake_sqlite_catalog_explicit(self):
        """Ducklake should allow explicit sqlite catalog config."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "sqlite", "path": "./custom.db"},
                }
            }
        )
        self.assertEqual(config["catalog"]["type"], "sqlite")
        self.assertEqual(config["catalog"]["path"], "./custom.db")

    def test_ducklake_duckdb_catalog(self):
        """Ducklake should allow duckdb catalog type override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "duckdb", "shared": True},
                }
            }
        )
        self.assertEqual(config["catalog"]["type"], "duckdb")
        self.assertTrue(config["catalog"]["shared"])

    def test_ducklake_firestore_catalog(self):
        """Ducklake should allow firestore catalog type override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "firestore", "project_id": "my-project"},
                }
            }
        )
        self.assertEqual(config["catalog"]["type"], "firestore")
        self.assertEqual(config["catalog"]["project_id"], "my-project")

    def test_ducklake_postgres_catalog(self):
        """Ducklake should allow postgres catalog type override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "postgres", "dsn": "postgresql://..."},
                }
            }
        )
        self.assertEqual(config["catalog"]["type"], "postgres")
        self.assertEqual(config["catalog"]["dsn"], "postgresql://...")

    def test_ducklake_supabase_catalog(self):
        """Ducklake should allow supabase catalog type override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "supabase", "url": "https://..."},
                }
            }
        )
        self.assertEqual(config["catalog"]["type"], "supabase")
        self.assertEqual(config["catalog"]["url"], "https://...")

    def test_ducklake_catalog_passthrough(self):
        """Ducklake should pass through all catalog-specific options."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {
                        "type": "firestore",
                        "project_id": "my-project",
                        "collection": "ltm_entries",
                        "custom_option": "value",
                    },
                }
            }
        )
        self.assertEqual(config["catalog"]["project_id"], "my-project")
        self.assertEqual(config["catalog"]["collection"], "ltm_entries")
        self.assertEqual(config["catalog"]["custom_option"], "value")


class TestDucklakeStorageOverride(unittest.TestCase):
    """Tests for storage configuration override."""

    def test_ducklake_storage_uri_override(self):
        """Ducklake should allow storage uri override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "storage": {"uri": "gs://my-bucket/ltm/"},
                }
            }
        )
        self.assertEqual(config["storage"]["uri"], "gs://my-bucket/ltm/")

    def test_ducklake_options_override(self):
        """Ducklake should allow lazy and inline_threshold override."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "lazy": False,
                    "inline_threshold": 8192,
                }
            }
        )
        self.assertFalse(config["lazy"])
        self.assertEqual(config["inline_threshold"], 8192)


class TestDucklakeBackwardCompatibility(unittest.TestCase):
    """Tests for backward compatibility with explicit duckdb config (AC4)."""

    def test_explicit_duckdb_unchanged(self):
        """Explicit backend: duckdb should work unchanged."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "duckdb",
                    "catalog": {"type": "sqlite", "path": "./my_catalog.db"},
                    "storage": {"uri": "./my_data/"},
                    "lazy": False,
                    "inline_threshold": 2048,
                }
            }
        )
        self.assertEqual(config["backend"], "duckdb")
        self.assertEqual(config["catalog"]["type"], "sqlite")
        self.assertEqual(config["catalog"]["path"], "./my_catalog.db")
        self.assertEqual(config["storage"]["uri"], "./my_data/")
        self.assertFalse(config["lazy"])
        self.assertEqual(config["inline_threshold"], 2048)

    def test_explicit_duckdb_no_defaults_added(self):
        """Explicit duckdb should not have ducklake defaults added."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "duckdb",
                }
            }
        )
        self.assertEqual(config["backend"], "duckdb")
        # These should NOT be present for explicit duckdb
        self.assertNotIn("catalog", config)
        self.assertNotIn("storage", config)
        self.assertNotIn("lazy", config)
        self.assertNotIn("inline_threshold", config)

    def test_sqlite_backend_unchanged(self):
        """Explicit backend: sqlite should work unchanged."""
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "sqlite",
                    "path": "./memory.db",
                }
            }
        )
        self.assertEqual(config["backend"], "sqlite")
        self.assertEqual(config["path"], "./memory.db")


class TestDucklakeCaseInsensitive(unittest.TestCase):
    """Tests for case-insensitive backend name handling."""

    def test_ducklake_lowercase(self):
        """ducklake in lowercase should work."""
        config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
        self.assertEqual(config["backend"], "duckdb")

    def test_ducklake_uppercase(self):
        """DUCKLAKE in uppercase should work."""
        config = expand_ltm_config({"ltm": {"backend": "DUCKLAKE"}})
        self.assertEqual(config["backend"], "duckdb")

    def test_ducklake_mixed_case(self):
        """DuckLake in mixed case should work."""
        config = expand_ltm_config({"ltm": {"backend": "DuckLake"}})
        self.assertEqual(config["backend"], "duckdb")


class TestDucklakeEnvVarExpansion(unittest.TestCase):
    """Tests for environment variable expansion in ducklake config."""

    def test_env_var_in_storage_uri(self):
        """Environment variables should be expanded in storage uri."""
        with patch.dict(os.environ, {"LTM_STORAGE": "gs://my-bucket/"}):
            config = expand_ltm_config(
                {
                    "ltm": {
                        "backend": "ducklake",
                        "storage": {"uri": "${LTM_STORAGE}"},
                    }
                }
            )
            self.assertEqual(config["storage"]["uri"], "gs://my-bucket/")

    def test_env_var_with_default(self):
        """Environment variables with defaults should work."""
        # Ensure the var is not set
        os.environ.pop("MISSING_VAR", None)
        config = expand_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "storage": {"uri": "${MISSING_VAR:-./fallback/}"},
                }
            }
        )
        self.assertEqual(config["storage"]["uri"], "./fallback/")


class TestDucklakeIntegration(unittest.TestCase):
    """Integration tests for ducklake with actual backend creation."""

    def setUp(self):
        """Set up test fixtures."""
        self.test_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up test fixtures."""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def test_ducklake_creates_duckdb_backend(self):
        """Ducklake should create a DuckDBLTMBackend instance."""
        try:
            from the_edge_agent.memory import DuckDBLTMBackend
        except ImportError:
            self.skipTest("DuckDB not available")

        catalog_path = os.path.join(self.test_dir, "catalog.db")
        storage_uri = os.path.join(self.test_dir, "storage/")

        backend = parse_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "sqlite", "path": catalog_path},
                    "storage": {"uri": storage_uri},
                }
            }
        )

        try:
            self.assertIsInstance(backend, DuckDBLTMBackend)
        finally:
            backend.close()

    def test_ducklake_store_retrieve(self):
        """Ducklake backend should support store/retrieve operations."""
        try:
            from the_edge_agent.memory import DuckDBLTMBackend
        except ImportError:
            self.skipTest("DuckDB not available")

        catalog_path = os.path.join(self.test_dir, "catalog.db")
        storage_uri = os.path.join(self.test_dir, "storage/")

        backend = parse_ltm_config(
            {
                "ltm": {
                    "backend": "ducklake",
                    "catalog": {"type": "sqlite", "path": catalog_path},
                    "storage": {"uri": storage_uri},
                }
            }
        )

        try:
            # Store
            result = backend.store("test_key", {"value": 42})
            self.assertTrue(result.get("success"))

            # Retrieve
            result = backend.retrieve("test_key")
            self.assertTrue(result.get("success"))
            self.assertTrue(result.get("found"))
            self.assertEqual(result["value"]["value"], 42)
        finally:
            backend.close()


if __name__ == "__main__":
    unittest.main()
