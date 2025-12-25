"""
Performance Tests for LTM Backend (TEA-BUILTIN-001.6.4).

Tests cover:
- Cold start latency (lazy initialization)
- Store/retrieve latency
- Batch operation throughput
- Comparison with SQLite backend
"""

import os
import pytest
import tempfile
import time


class TestColdStartPerformance:
    """Performance tests for cold start latency."""

    def test_lazy_init_faster_than_eager(self):
        """Test that lazy initialization is faster than eager."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            # Measure eager init time
            start = time.perf_counter()
            eager_backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/eager/",
                lazy=False,
                enable_fts=False,
            )
            eager_time = time.perf_counter() - start
            eager_backend.close()

            # Measure lazy init time
            start = time.perf_counter()
            lazy_backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/lazy/",
                lazy=True,
                enable_fts=False,
            )
            lazy_time = time.perf_counter() - start

            # Lazy should be significantly faster (at least 2x)
            assert (
                lazy_time < eager_time * 0.5
            ), f"Lazy init ({lazy_time:.4f}s) should be faster than eager ({eager_time:.4f}s)"

            lazy_backend.close()


class TestStoreRetrieveLatency:
    """Performance tests for store/retrieve operations."""

    def test_inline_store_latency(self):
        """Test that inline store is fast (<10ms)."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            # Warm up
            backend.store("warmup", {"data": "warmup"})

            # Measure 100 stores
            latencies = []
            for i in range(100):
                start = time.perf_counter()
                backend.store(f"key_{i}", {"data": f"value_{i}"})
                latencies.append(time.perf_counter() - start)

            avg_latency = sum(latencies) / len(latencies)
            max_latency = max(latencies)

            # Average should be under 10ms, max under 50ms
            assert (
                avg_latency < 0.010
            ), f"Average latency {avg_latency*1000:.2f}ms exceeds 10ms"
            assert (
                max_latency < 0.050
            ), f"Max latency {max_latency*1000:.2f}ms exceeds 50ms"

            backend.close()

    def test_retrieve_latency(self):
        """Test that retrieve is fast (<5ms)."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            # Pre-populate
            for i in range(100):
                backend.store(f"key_{i}", {"data": f"value_{i}"})

            # Measure 100 retrieves
            latencies = []
            for i in range(100):
                start = time.perf_counter()
                backend.retrieve(f"key_{i}")
                latencies.append(time.perf_counter() - start)

            avg_latency = sum(latencies) / len(latencies)

            # Average should be under 5ms
            assert (
                avg_latency < 0.005
            ), f"Average latency {avg_latency*1000:.2f}ms exceeds 5ms"

            backend.close()


class TestBatchThroughput:
    """Performance tests for batch operations."""

    def test_batch_store_throughput(self):
        """Test batch store throughput (>100 entries/sec)."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            entries = [
                {"key": f"batch_key_{i}", "value": {"data": f"batch_value_{i}"}}
                for i in range(100)
            ]

            start = time.perf_counter()
            result = backend.store_batch(entries, atomic=True)
            elapsed = time.perf_counter() - start

            assert result["success"] is True
            throughput = 100 / elapsed
            assert throughput > 100, f"Throughput {throughput:.0f}/s below 100/s"

            backend.close()


class TestBackendComparison:
    """Compare DuckDB backend with SQLite backend."""

    def test_duckdb_comparable_to_sqlite(self):
        """Test that DuckDB is not significantly slower than SQLite."""
        from the_edge_agent.memory import create_ltm_backend

        with tempfile.TemporaryDirectory() as tmpdir:
            # SQLite backend
            sqlite_backend = create_ltm_backend("sqlite", db_path=":memory:")

            # DuckDB backend
            duckdb_backend = create_ltm_backend(
                "duckdb",
                catalog_config={"type": "sqlite", "path": ":memory:"},
                storage_uri=tmpdir + "/",
                lazy=False,
                enable_fts=False,
            )

            # Measure 50 store/retrieve cycles for SQLite
            start = time.perf_counter()
            for i in range(50):
                sqlite_backend.store(f"key_{i}", {"data": f"value_{i}"})
                sqlite_backend.retrieve(f"key_{i}")
            sqlite_time = time.perf_counter() - start

            # Measure 50 store/retrieve cycles for DuckDB
            start = time.perf_counter()
            for i in range(50):
                duckdb_backend.store(f"key_{i}", {"data": f"value_{i}"})
                duckdb_backend.retrieve(f"key_{i}")
            duckdb_time = time.perf_counter() - start

            # DuckDB should be within 5x of SQLite (reasonable overhead for catalog layer)
            ratio = duckdb_time / sqlite_time if sqlite_time > 0 else 999
            assert ratio < 5, f"DuckDB {ratio:.1f}x slower than SQLite"

            sqlite_backend.close()
            duckdb_backend.close()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
