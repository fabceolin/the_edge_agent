"""
Integration Tests for LTM Backend (TEA-BUILTIN-001.6.4).

Tests cover:
- Full flow with SQLiteCatalog
- cache.wrap integration with DuckDB
- YAML engine LTM configuration
- End-to-end caching scenarios
"""

import os
import pytest
import tempfile
from unittest.mock import patch


class TestDuckDBWithSQLiteCatalog:
    """Integration tests for DuckDB backend with SQLite catalog."""

    def test_full_store_retrieve_cycle(self):
        """Test complete store and retrieve cycle."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            # Store small data (should be inlined)
            result = backend.store(
                "small_key", {"data": "small"}, metadata={"type": "test"}
            )
            assert result["success"] is True
            assert result["inlined"] is True

            # Retrieve
            retrieve_result = backend.retrieve("small_key")
            assert retrieve_result["success"] is True
            assert retrieve_result["found"] is True
            assert retrieve_result["value"] == {"data": "small"}
            assert retrieve_result["metadata"]["type"] == "test"

            # Delete
            delete_result = backend.delete("small_key")
            assert delete_result["success"] is True
            assert delete_result["deleted"] is True

            # Verify deleted
            after_delete = backend.retrieve("small_key")
            assert after_delete["found"] is False

            backend.close()

    def test_large_data_cloud_storage(self):
        """Test that large data is uploaded to cloud storage."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                inline_threshold=50,  # Very small threshold
                lazy=False,
                enable_fts=False,
            )

            # Store large data (should go to cloud storage)
            large_value = {"data": "x" * 100}
            result = backend.store("large_key", large_value)
            assert result["success"] is True
            assert result["inlined"] is False
            assert "storage_uri" in result

            # Retrieve from cloud
            retrieve_result = backend.retrieve("large_key")
            assert retrieve_result["success"] is True
            assert retrieve_result["value"] == large_value
            assert retrieve_result["inlined"] is False

            backend.close()

    def test_content_deduplication(self):
        """Test that identical content is deduplicated."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            value = {"data": "test"}

            # First store
            result1 = backend.store("key1", value)
            assert result1["success"] is True
            assert result1["stored"] is True
            hash1 = result1["content_hash"]

            # Store same content again with same key
            result2 = backend.store("key1", value)
            assert result2["success"] is True
            assert result2.get("deduplicated") is True
            assert result2["content_hash"] == hash1

            backend.close()


class TestCacheWrapIntegration:
    """Integration tests for cache.wrap with DuckDB backend."""

    def test_cache_hit_miss_flow(self):
        """Test cache hit and miss behavior."""
        from the_edge_agent import YAMLEngine

        with tempfile.TemporaryDirectory() as tmpdir:
            engine = YAMLEngine(
                ltm_backend_type="duckdb",
                ltm_config={
                    "catalog_config": {"type": "sqlite", "path": ":memory:"},
                    "storage_uri": tmpdir + "/",
                    "enable_fts": False,
                },
            )

            registry = engine.actions_registry

            # Register test action
            call_count = [0]

            def test_action(state, **kwargs):
                call_count[0] += 1
                return {"success": True, "result": f"call_{call_count[0]}"}

            registry["test.action"] = test_action

            # First call - cache miss
            result1 = registry["cache.wrap"](
                state={},
                action="test.action",
                args={},
                key="test-cache-key",
                ttl_seconds=300,
            )
            assert result1["_cache_hit"] is False
            assert call_count[0] == 1

            # Second call - cache hit
            result2 = registry["cache.wrap"](
                state={},
                action="test.action",
                args={},
                key="test-cache-key",
                ttl_seconds=300,
            )
            assert result2["_cache_hit"] is True
            assert call_count[0] == 1  # Should not increment

            # Results should be identical
            assert result1["result"] == result2["result"]

            engine.close()

    def test_cache_with_different_keys(self):
        """Test that different keys maintain separate cache entries."""
        from the_edge_agent import YAMLEngine

        with tempfile.TemporaryDirectory() as tmpdir:
            engine = YAMLEngine(
                ltm_backend_type="duckdb",
                ltm_config={
                    "catalog_config": {"type": "sqlite", "path": ":memory:"},
                    "storage_uri": tmpdir + "/",
                    "enable_fts": False,
                },
            )

            registry = engine.actions_registry
            call_count = [0]

            def test_action(state, value=None, **kwargs):
                call_count[0] += 1
                return {"success": True, "result": value or "default"}

            registry["test.action"] = test_action

            # Store with key1
            result1 = registry["cache.wrap"](
                state={}, action="test.action", args={"value": "value1"}, key="key1"
            )
            assert call_count[0] == 1

            # Store with key2
            result2 = registry["cache.wrap"](
                state={}, action="test.action", args={"value": "value2"}, key="key2"
            )
            assert call_count[0] == 2  # Should call action again

            # Retrieve key1 - should be cached
            result1_again = registry["cache.wrap"](
                state={}, action="test.action", args={"value": "value1"}, key="key1"
            )
            assert result1_again["_cache_hit"] is True
            assert call_count[0] == 2  # Should not increment

            engine.close()


class TestYAMLEngineLTMConfig:
    """Integration tests for YAML engine LTM configuration from settings."""

    def test_yaml_ltm_settings_applied(self):
        """Test that LTM settings from YAML are applied."""
        from the_edge_agent import YAMLEngine
        import tempfile

        yaml_content = """
name: test-ltm-settings
state_schema:
  result: str

settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: ":memory:"
    storage:
      uri: "/tmp/yaml_test_ltm/"
    inline_threshold: 256
    lazy: true
    enable_fts: false

nodes:
  - name: start
    run: |
      return {"result": "done"}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
        os.makedirs("/tmp/yaml_test_ltm/", exist_ok=True)

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(yaml_content)
            yaml_path = f.name

        try:
            engine = YAMLEngine()
            graph = engine.load_from_file(yaml_path)

            # Verify DuckDB backend was configured
            assert type(engine._ltm_backend).__name__ == "DuckDBLTMBackend"
            assert "/tmp/yaml_test_ltm/" in engine._ltm_backend._storage_uri

            engine.close()
        finally:
            os.unlink(yaml_path)

    def test_yaml_env_var_expansion(self):
        """Test that environment variables in YAML settings are expanded."""
        from the_edge_agent import YAMLEngine
        import tempfile

        with patch.dict(os.environ, {"TEST_STORAGE_PATH": "/tmp/env_test_ltm/"}):
            os.makedirs("/tmp/env_test_ltm/", exist_ok=True)

            yaml_content = """
name: test-env-expansion
state_schema:
  result: str

settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: ":memory:"
    storage:
      uri: "${TEST_STORAGE_PATH}"
    lazy: true
    enable_fts: false

nodes:
  - name: start
    run: |
      return {"result": "done"}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".yaml", delete=False
            ) as f:
                f.write(yaml_content)
                yaml_path = f.name

            try:
                engine = YAMLEngine()
                graph = engine.load_from_file(yaml_path)

                # Verify env var was expanded
                assert "/tmp/env_test_ltm/" in engine._ltm_backend._storage_uri

                engine.close()
            finally:
                os.unlink(yaml_path)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
