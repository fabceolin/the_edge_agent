"""
Tests for LTM Factory Functions (TEA-BUILTIN-001.6.4).

Tests cover:
- Factory function registration
- Backend creation
- Configuration parsing
- Environment variable expansion
- Error handling
"""

import os
import pytest
import tempfile
from unittest.mock import patch


class TestExpandEnvVars:
    """Tests for expand_env_vars helper function."""

    def test_expand_simple_env_var(self):
        """Test expansion of ${VAR} pattern."""
        from the_edge_agent.memory import expand_env_vars

        with patch.dict(os.environ, {"MY_VAR": "test_value"}):
            result = expand_env_vars("${MY_VAR}")
            assert result == "test_value"

    def test_expand_env_var_with_default(self):
        """Test expansion of ${VAR:-default} pattern."""
        from the_edge_agent.memory import expand_env_vars

        # Variable not set - should use default
        with patch.dict(os.environ, {}, clear=True):
            result = expand_env_vars("${MISSING_VAR:-fallback}")
            assert result == "fallback"

        # Variable set - should use value
        with patch.dict(os.environ, {"MISSING_VAR": "actual"}):
            result = expand_env_vars("${MISSING_VAR:-fallback}")
            assert result == "actual"

    def test_expand_nested_dict(self):
        """Test expansion in nested dicts."""
        from the_edge_agent.memory import expand_env_vars

        with patch.dict(os.environ, {"VAR1": "val1", "VAR2": "val2"}):
            config = {
                "outer": "${VAR1}",
                "nested": {"inner": "${VAR2}", "default": "${MISSING:-default}"},
            }
            result = expand_env_vars(config)
            assert result["outer"] == "val1"
            assert result["nested"]["inner"] == "val2"
            assert result["nested"]["default"] == "default"

    def test_expand_list(self):
        """Test expansion in lists."""
        from the_edge_agent.memory import expand_env_vars

        with patch.dict(os.environ, {"VAR": "value"}):
            config = ["${VAR}", "literal", "${MISSING:-default}"]
            result = expand_env_vars(config)
            assert result == ["value", "literal", "default"]

    def test_non_string_passthrough(self):
        """Test that non-string types pass through unchanged."""
        from the_edge_agent.memory import expand_env_vars

        assert expand_env_vars(123) == 123
        assert expand_env_vars(True) is True
        assert expand_env_vars(None) is None
        assert expand_env_vars(3.14) == 3.14


class TestLTMBackendRegistry:
    """Tests for LTM backend registry and factory."""

    def test_registered_backends(self):
        """Test that expected backends are registered."""
        from the_edge_agent.memory import get_registered_backends

        backends = get_registered_backends()
        assert "sqlite" in backends
        assert "duckdb" in backends
        assert "litestream" in backends
        assert "blob-sqlite" in backends

    def test_create_sqlite_backend(self):
        """Test creating SQLite backend."""
        from the_edge_agent.memory import create_ltm_backend

        backend = create_ltm_backend("sqlite", db_path=":memory:")
        assert backend is not None
        backend.close()

    def test_create_duckdb_backend(self):
        """Test creating DuckDB backend with catalog config."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=True,
                enable_fts=False,
            )
            assert backend is not None
            assert type(backend).__name__ == "DuckDBLTMBackend"
            backend.close()

    def test_create_unknown_backend_raises(self):
        """Test that unknown backend raises ValueError."""
        from the_edge_agent.memory import create_ltm_backend

        with pytest.raises(ValueError) as exc_info:
            create_ltm_backend("unknown_backend")

        assert "unknown_backend" in str(exc_info.value).lower()


class TestCatalogBackendRegistry:
    """Tests for catalog backend registry and factory."""

    def test_registered_catalog_backends(self):
        """Test that expected catalog backends are registered."""
        from the_edge_agent.memory import get_registered_catalog_backends

        catalogs = get_registered_catalog_backends()
        assert "sqlite" in catalogs

    def test_create_sqlite_catalog(self):
        """Test creating SQLite catalog."""
        from the_edge_agent.memory import create_catalog_backend

        catalog = create_catalog_backend("sqlite", path=":memory:")
        assert catalog is not None
        assert type(catalog).__name__ == "SQLiteCatalog"

    def test_create_unknown_catalog_raises(self):
        """Test that unknown catalog raises ValueError."""
        from the_edge_agent.memory import create_catalog_backend

        with pytest.raises(ValueError) as exc_info:
            create_catalog_backend("unknown_catalog")

        assert "unknown_catalog" in str(exc_info.value).lower()


class TestParseBackendConfig:
    """Tests for parse_backend_config function."""

    def test_parse_sqlite_config(self):
        """Test parsing SQLite configuration."""
        from the_edge_agent.memory import parse_backend_config

        config = {"ltm_backend": "sqlite", "ltm_path": "./test.db"}
        backend_type, kwargs = parse_backend_config(config)
        assert backend_type == "sqlite"
        assert kwargs.get("db_path") == "./test.db"

    def test_parse_duckdb_config(self):
        """Test parsing DuckDB configuration with nested catalog."""
        from the_edge_agent.memory import parse_backend_config

        config = {
            "ltm_backend": "duckdb",
            "catalog": {"type": "sqlite", "path": ":memory:"},
            "storage": {"uri": "./ltm_data/"},
            "inline_threshold": 512,
            "lazy": True,
            "enable_fts": False,
        }
        backend_type, kwargs = parse_backend_config(config)
        assert backend_type == "duckdb"
        assert kwargs.get("catalog_config") == {"type": "sqlite", "path": ":memory:"}
        assert kwargs.get("storage_uri") == "./ltm_data/"
        assert kwargs.get("inline_threshold") == 512
        assert kwargs.get("lazy") is True
        assert kwargs.get("enable_fts") is False

    def test_parse_storage_string(self):
        """Test parsing storage as simple string."""
        from the_edge_agent.memory import parse_backend_config

        config = {
            "ltm_backend": "duckdb",
            "catalog": {"type": "sqlite"},
            "storage": "/path/to/storage/",
        }
        backend_type, kwargs = parse_backend_config(config)
        assert kwargs.get("storage_uri") == "/path/to/storage/"

    def test_default_backend_type(self):
        """Test that default backend type is sqlite."""
        from the_edge_agent.memory import parse_backend_config

        config = {}
        backend_type, kwargs = parse_backend_config(config)
        assert backend_type == "sqlite"


class TestParseLTMConfig:
    """Tests for parse_ltm_config convenience function."""

    def test_parse_complete_config(self):
        """Test parsing complete LTM settings."""
        from the_edge_agent.memory import parse_ltm_config

        settings = {
            "ltm": {
                "backend": "sqlite",
            }
        }
        backend = parse_ltm_config(settings)
        assert backend is not None
        assert type(backend).__name__ == "SQLiteBackend"
        backend.close()

    def test_parse_duckdb_config(self):
        """Test parsing DuckDB LTM settings."""
        from the_edge_agent.memory import parse_ltm_config

        with tempfile.TemporaryDirectory() as tmpdir:
            settings = {
                "ltm": {
                    "backend": "duckdb",
                    "catalog": {"type": "sqlite", "path": ":memory:"},
                    "storage": {"uri": tmpdir + "/"},
                    "lazy": True,
                    "enable_fts": False,
                }
            }
            backend = parse_ltm_config(settings)
            assert type(backend).__name__ == "DuckDBLTMBackend"
            backend.close()

    def test_parse_with_env_vars(self):
        """Test that environment variables are expanded."""
        from the_edge_agent.memory import parse_ltm_config

        with patch.dict(os.environ, {"TEST_STORAGE": "/tmp/test_ltm/"}):
            os.makedirs("/tmp/test_ltm/", exist_ok=True)
            settings = {
                "ltm": {
                    "backend": "duckdb",
                    "catalog": {"type": "sqlite", "path": ":memory:"},
                    "storage": {"uri": "${TEST_STORAGE}"},
                    "lazy": True,
                    "enable_fts": False,
                }
            }
            backend = parse_ltm_config(settings)
            assert "/tmp/test_ltm/" in backend._storage_uri
            backend.close()


class TestDuckDBLTMBackendLazyInit:
    """Tests for DuckDB LTM Backend lazy initialization."""

    def test_lazy_catalog_creation(self):
        """Test that catalog is created lazily when lazy=True."""
        from the_edge_agent.memory import DuckDBLTMBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = DuckDBLTMBackend(
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=True,
                enable_fts=False,
            )
            # Catalog should be None before first access
            assert backend._catalog is None

            # Accessing catalog property should create it
            catalog = backend.catalog
            assert catalog is not None
            assert backend._catalog is not None

            backend.close()

    def test_store_triggers_lazy_init(self):
        """Test that store triggers lazy initialization."""
        from the_edge_agent.memory import DuckDBLTMBackend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = DuckDBLTMBackend(
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=True,
                enable_fts=False,
            )
            # Store should work and trigger lazy init
            result = backend.store("key", {"value": "test"})
            assert result["success"] is True
            assert backend._catalog is not None

            backend.close()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
