"""
Unit tests for HierarchicalLTMBackend YAML config transformation (BUG.001).

Tests the transformation of nested YAML config format to flat parameters
expected by HierarchicalLTMBackend.__init__().

Test Categories:
- P0: Core transformation tests (catalog.url, storage.uri, hierarchy.levels, etc.)
- P0: Error message tests (missing required fields)
- P1: Backward compatibility tests (flat params still work)

Test IDs follow the test design document:
docs/qa/assessments/BUG.001-test-design-20260118.md
"""

import os
import pytest
from unittest import TestCase


class TestParseHierarchicalLTMConfig(TestCase):
    """Unit tests for _parse_hierarchical_ltm_config() function (BUG.001-UNIT-001 to 016)."""

    def setUp(self):
        """Import function under test."""
        from the_edge_agent.memory.base import _parse_hierarchical_ltm_config

        self.parse = _parse_hierarchical_ltm_config

    # =========================================================================
    # P0: Core Transformation Tests (BUG.001-UNIT-001 to 006)
    # =========================================================================

    def test_catalog_url_transformation(self):
        """BUG.001-UNIT-001: catalog.url -> catalog_url"""
        config = {
            "catalog": {"url": "postgresql://user:pass@localhost/db"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertEqual(result["catalog_url"], "postgresql://user:pass@localhost/db")

    def test_storage_uri_transformation(self):
        """BUG.001-UNIT-002: storage.uri -> storage_uri"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "gs://my-bucket/ltm/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertEqual(result["storage_uri"], "gs://my-bucket/ltm/")

    def test_hierarchy_levels_transformation(self):
        """BUG.001-UNIT-003: hierarchy.levels -> hierarchy_levels"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "project", "user", "session"]},
        }
        result = self.parse(config)
        self.assertEqual(
            result["hierarchy_levels"], ["org", "project", "user", "session"]
        )

    def test_hierarchy_defaults_transformation(self):
        """BUG.001-UNIT-004: hierarchy.defaults -> hierarchy_defaults"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {
                "levels": ["org", "user"],
                "defaults": {"org": "default_org", "user": "_anonymous"},
            },
        }
        result = self.parse(config)
        self.assertEqual(
            result["hierarchy_defaults"],
            {"org": "default_org", "user": "_anonymous"},
        )

    def test_catalog_pool_size_extraction(self):
        """BUG.001-UNIT-005: catalog.pool_size -> pool_size"""
        config = {
            "catalog": {"url": "sqlite:///:memory:", "pool_size": 20},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertEqual(result["pool_size"], 20)

    def test_catalog_lazy_extraction(self):
        """BUG.001-UNIT-006: catalog.lazy -> lazy"""
        config = {
            "catalog": {"url": "sqlite:///:memory:", "lazy": True},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertTrue(result["lazy"])

    # =========================================================================
    # P1: Index Config Transformation Tests (BUG.001-UNIT-007 to 009)
    # =========================================================================

    def test_index_config_transformation(self):
        """BUG.001-UNIT-007: index block -> index_config dict"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
            "index": {
                "format": "parquet",
                "row_group_size": 10000,
                "compression": "snappy",
            },
        }
        result = self.parse(config)
        self.assertIn("index_config", result)
        self.assertEqual(result["index_config"]["format"], "parquet")
        self.assertEqual(result["index_config"]["row_group_size"], 10000)
        self.assertEqual(result["index_config"]["compression"], "snappy")

    def test_index_config_defaults(self):
        """BUG.001-UNIT-008: index block uses defaults for missing fields"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
            "index": {"format": "parquet"},  # Only format specified
        }
        result = self.parse(config)
        self.assertEqual(result["index_config"]["format"], "parquet")
        self.assertEqual(result["index_config"]["row_group_size"], 122880)  # default
        self.assertEqual(result["index_config"]["compression"], "zstd")  # default

    def test_no_index_config_when_not_specified(self):
        """BUG.001-UNIT-009: No index_config when index block absent"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertNotIn("index_config", result)

    # =========================================================================
    # P1: Performance Config Transformation Tests (BUG.001-UNIT-010)
    # =========================================================================

    def test_performance_config_transformation(self):
        """BUG.001-UNIT-010: performance block -> performance_config dict"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
            "performance": {
                "metadata_cache": {"enabled": True, "ttl_seconds": 300},
                "parallel_reads": {"threads": 4},
            },
        }
        result = self.parse(config)
        self.assertIn("performance_config", result)
        self.assertEqual(result["performance_config"]["metadata_cache_ttl"], 300)
        self.assertEqual(result["performance_config"]["threads"], 4)

    # =========================================================================
    # P0: Backward Compatibility Tests (BUG.001-UNIT-011, 012)
    # =========================================================================

    def test_flat_params_still_work(self):
        """BUG.001-UNIT-011: Flat params are passed through create_ltm_backend"""
        from the_edge_agent.memory import create_ltm_backend, HIERARCHICAL_AVAILABLE

        if not HIERARCHICAL_AVAILABLE:
            self.skipTest("HierarchicalLTMBackend dependencies not available")

        # Flat params should work directly with create_ltm_backend
        backend = create_ltm_backend(
            "hierarchical",
            catalog_url="sqlite:///:memory:",
            storage_uri="./test_ltm_flat/",
            hierarchy_levels=["org", "user", "session"],
            lazy=True,
        )
        try:
            self.assertEqual(backend.hierarchy_levels, ["org", "user", "session"])
        finally:
            backend.close()

    def test_factory_with_flat_kwargs(self):
        """BUG.001-UNIT-012: Factory accepts flat kwargs directly"""
        from the_edge_agent.memory.base import _is_hierarchical_nested_config

        # Config with flat keys should not be detected as nested
        flat_config = {
            "catalog_url": "sqlite:///:memory:",
            "storage_uri": "./test_data/",
            "hierarchy_levels": ["org", "user"],
        }
        self.assertFalse(_is_hierarchical_nested_config(flat_config))

    # =========================================================================
    # P1: Inline Threshold Test (BUG.001-UNIT-013)
    # =========================================================================

    def test_inline_threshold_extraction(self):
        """BUG.001-UNIT-013: inline_threshold is extracted correctly"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
            "inline_threshold": 2048,
        }
        result = self.parse(config)
        self.assertEqual(result["inline_threshold"], 2048)

    def test_inline_threshold_default(self):
        """inline_threshold uses default 1024 when not specified"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        result = self.parse(config)
        self.assertEqual(result["inline_threshold"], 1024)

    # =========================================================================
    # P0: Error Message Tests (BUG.001-UNIT-014 to 016)
    # =========================================================================

    def test_missing_catalog_url_error(self):
        """BUG.001-UNIT-014: Missing catalog.url raises ValueError with clear message"""
        config = {
            "catalog": {},  # Missing url
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        with self.assertRaises(ValueError) as ctx:
            self.parse(config)
        self.assertIn("catalog.url", str(ctx.exception))
        self.assertIn("HierarchicalLTMBackend", str(ctx.exception))

    def test_missing_storage_uri_error(self):
        """BUG.001-UNIT-015: Missing storage.uri raises ValueError with clear message"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {},  # Missing uri
            "hierarchy": {"levels": ["org", "user"]},
        }
        with self.assertRaises(ValueError) as ctx:
            self.parse(config)
        self.assertIn("storage.uri", str(ctx.exception))
        self.assertIn("HierarchicalLTMBackend", str(ctx.exception))

    def test_missing_hierarchy_levels_error(self):
        """BUG.001-UNIT-016: Missing hierarchy.levels raises ValueError with clear message"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {},  # Missing levels
        }
        with self.assertRaises(ValueError) as ctx:
            self.parse(config)
        self.assertIn("hierarchy.levels", str(ctx.exception))
        self.assertIn("HierarchicalLTMBackend", str(ctx.exception))

    def test_empty_hierarchy_levels_error(self):
        """Empty hierarchy.levels list raises ValueError"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": []},  # Empty levels
        }
        with self.assertRaises(ValueError) as ctx:
            self.parse(config)
        self.assertIn("hierarchy.levels", str(ctx.exception))


class TestIsHierarchicalNestedConfig(TestCase):
    """Unit tests for _is_hierarchical_nested_config() helper."""

    def setUp(self):
        """Import function under test."""
        from the_edge_agent.memory.base import _is_hierarchical_nested_config

        self.is_nested = _is_hierarchical_nested_config

    def test_nested_config_detected(self):
        """Nested config with catalog/storage/hierarchy is detected"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            "hierarchy": {"levels": ["org", "user"]},
        }
        self.assertTrue(self.is_nested(config))

    def test_flat_config_not_detected(self):
        """Flat config with catalog_url/storage_uri is not detected as nested"""
        config = {
            "catalog_url": "sqlite:///:memory:",
            "storage_uri": "./test_data/",
            "hierarchy_levels": ["org", "user"],
        }
        self.assertFalse(self.is_nested(config))

    def test_mixed_config_prefers_flat(self):
        """Config with both nested and flat keys prefers flat (backward compat)"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},  # nested
            "catalog_url": "postgresql://...",  # flat
            "storage_uri": "./test_data/",
            "hierarchy_levels": ["org", "user"],
        }
        # Has flat keys, so should not be treated as nested
        self.assertFalse(self.is_nested(config))

    def test_partial_nested_detected(self):
        """Partial nested config is still detected"""
        config = {
            "catalog": {"url": "sqlite:///:memory:"},
            "storage": {"uri": "./test_data/"},
            # hierarchy might come from defaults
        }
        self.assertTrue(self.is_nested(config))


class TestExpandEnvVarsInHierarchicalConfig(TestCase):
    """Unit tests for environment variable expansion in hierarchical config."""

    def setUp(self):
        """Set up test environment variables."""
        self.original_env = os.environ.copy()
        os.environ["TEST_DB_URL"] = "postgresql://test:pass@localhost/testdb"
        os.environ["TEST_STORAGE_URI"] = "gs://test-bucket/ltm/"

    def tearDown(self):
        """Restore original environment."""
        os.environ.clear()
        os.environ.update(self.original_env)

    def test_env_var_expansion_in_catalog_url(self):
        """Environment variables are expanded in catalog.url"""
        from the_edge_agent.memory.base import expand_env_vars

        config = {
            "catalog": {"url": "${TEST_DB_URL}"},
            "storage": {"uri": "./test_data/"},
        }
        expanded = expand_env_vars(config)
        self.assertEqual(
            expanded["catalog"]["url"],
            "postgresql://test:pass@localhost/testdb",
        )

    def test_env_var_with_default_expansion(self):
        """Environment variables with defaults are expanded correctly"""
        from the_edge_agent.memory.base import expand_env_vars

        config = {
            "catalog": {"url": "${MISSING_VAR:-sqlite:///:memory:}"},
            "storage": {"uri": "${TEST_STORAGE_URI:-./default/}"},
        }
        expanded = expand_env_vars(config)
        self.assertEqual(expanded["catalog"]["url"], "sqlite:///:memory:")
        self.assertEqual(expanded["storage"]["uri"], "gs://test-bucket/ltm/")


class TestParseLTMConfigWithHierarchical(TestCase):
    """Unit tests for parse_ltm_config() with hierarchical backend."""

    def test_parse_ltm_config_creates_hierarchical_backend(self):
        """parse_ltm_config() creates HierarchicalLTMBackend from nested config"""
        from the_edge_agent.memory import parse_ltm_config, HIERARCHICAL_AVAILABLE

        if not HIERARCHICAL_AVAILABLE:
            self.skipTest("HierarchicalLTMBackend dependencies not available")

        settings = {
            "ltm": {
                "backend": "hierarchical",
                "catalog": {"url": "sqlite:///:memory:"},
                "storage": {"uri": "./test_parse_ltm/"},
                "hierarchy": {"levels": ["org", "user", "session"]},
            }
        }

        backend = parse_ltm_config(settings)
        try:
            self.assertEqual(backend.__class__.__name__, "HierarchicalLTMBackend")
            self.assertEqual(backend.hierarchy_levels, ["org", "user", "session"])
        finally:
            backend.close()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
