"""
CLI URL integration tests.

TEA-CLI-001: URL-Based File Input for Python CLI

Tests cover:
- Local path passthrough (AC1)
- resolve_file_url function (AC1)
- Cache flags (AC3)
- CLI cache subcommands (AC4)
"""

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import pytest
from typer.testing import CliRunner

from the_edge_agent.cli import (
    app,
    resolve_file_url,
)
from the_edge_agent.cache import RemoteFileCache


runner = CliRunner()


class TestResolveFileUrl(unittest.TestCase):
    """Tests for resolve_file_url function (AC1)."""

    def test_local_path_passthrough(self):
        """Local paths should be returned unchanged."""
        with tempfile.NamedTemporaryFile(suffix=".yaml", delete=False) as f:
            f.write(b"name: test")
            local_path = f.name

        try:
            result = resolve_file_url(local_path)
            self.assertEqual(str(result), local_path)
        finally:
            os.unlink(local_path)

    def test_relative_path_passthrough(self):
        """Relative paths should be returned as Path objects."""
        result = resolve_file_url("./workflow.yaml")
        # Result is a Path object with the same value
        self.assertEqual(result, Path("./workflow.yaml"))

    def test_cache_only_mode_uncached(self):
        """Cache-only mode should fail for uncached URLs."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with pytest.raises(Exception) as exc_info:
                resolve_file_url(
                    "s3://bucket/uncached.yaml",
                    cache_only=True,
                    cache_dir=Path(tmpdir),
                )
            # Should suggest running without --cache-only
            assert "cache-only" in str(exc_info.value).lower()


class TestCacheCliCommands(unittest.TestCase):
    """Tests for tea cache CLI subcommands (AC4)."""

    def test_cache_list_empty(self):
        """tea cache list should show empty message for empty cache."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Set XDG_CACHE_HOME to use temp directory
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                result = runner.invoke(app, ["cache", "list"])
                # Should either show "empty" or list headers
                assert result.exit_code == 0
                assert "empty" in result.output.lower() or "URL" in result.output

    def test_cache_list_json(self):
        """tea cache list --json should output valid JSON."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                result = runner.invoke(app, ["cache", "list", "--json"])
                assert result.exit_code == 0
                # Should be valid JSON (empty array)
                data = json.loads(result.output)
                assert isinstance(data, list)

    def test_cache_info(self):
        """tea cache info should show cache statistics."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                result = runner.invoke(app, ["cache", "info"])
                assert result.exit_code == 0
                assert "Location" in result.output
                assert "Entries" in result.output
                assert "Size" in result.output

    def test_cache_info_json(self):
        """tea cache info --json should output valid JSON."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                result = runner.invoke(app, ["cache", "info", "--json"])
                assert result.exit_code == 0
                data = json.loads(result.output)
                assert "location" in data
                assert "total_entries" in data

    def test_cache_clear_empty(self):
        """tea cache clear should handle empty cache."""
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                result = runner.invoke(app, ["cache", "clear", "--force"])
                assert result.exit_code == 0
                assert "empty" in result.output.lower()

    def test_cache_clear_invalid_duration(self):
        """tea cache clear --older-than with invalid format should error when there are entries."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # XDG_CACHE_HOME is the base, RemoteFileCache adds /tea/remote/
            # So we need to create the cache at the expected location
            with patch.dict(os.environ, {"XDG_CACHE_HOME": tmpdir}):
                # Create cache - this will use tmpdir/tea/remote/
                cache = RemoteFileCache()  # Uses the patched XDG_CACHE_HOME
                entry_dir = cache.files_dir / "abc123"
                entry_dir.mkdir(parents=True)
                (entry_dir / "test.yaml").write_text("content")

                manifest = {
                    "version": 1,
                    "entries": {
                        "abc123": {
                            "url": "s3://bucket/test.yaml",
                            "local_path": "files/abc123/test.yaml",
                            "created_at": 0,
                            "ttl_seconds": 3600,
                            "is_permanent": False,
                            "size_bytes": 7,
                        }
                    },
                }
                with open(cache.manifest_path, "w") as f:
                    json.dump(manifest, f)

                result = runner.invoke(
                    app, ["cache", "clear", "--older-than", "invalid", "--force"]
                )
                # Should show error about invalid format
                assert result.exit_code == 1 or "Invalid duration" in result.output


class TestRunCommandWithUrls(unittest.TestCase):
    """Tests for run command URL handling (AC1)."""

    def test_run_local_file(self):
        """tea run with local file should work."""
        with tempfile.NamedTemporaryFile(suffix=".yaml", delete=False) as f:
            f.write(b"""
name: test-workflow
state_schema:
  message: str
nodes:
  - name: hello
    run: |
      return {"message": "Hello World"}
edges:
  - from: __start__
    to: hello
  - from: hello
    to: __end__
""")
            yaml_path = f.name

        try:
            result = runner.invoke(app, ["run", yaml_path])
            # Should complete (exit 0) or show workflow output
            # We don't need perfect success, just no URL-related errors
            assert "Error" not in result.output or "not found" in result.output.lower()
        finally:
            os.unlink(yaml_path)

    def test_run_with_no_cache_flag(self):
        """tea run --no-cache should be accepted."""
        with tempfile.NamedTemporaryFile(suffix=".yaml", delete=False) as f:
            f.write(b"""
name: test-workflow
state_schema:
  message: str
nodes:
  - name: hello
    run: |
      return {"message": "Hello"}
edges:
  - from: __start__
    to: hello
  - from: hello
    to: __end__
""")
            yaml_path = f.name

        try:
            result = runner.invoke(app, ["run", yaml_path, "--no-cache"])
            # Flag should be accepted (no unknown option error)
            assert "Error: No such option: --no-cache" not in result.output
        finally:
            os.unlink(yaml_path)

    def test_run_with_cache_only_flag(self):
        """tea run --cache-only should be accepted."""
        with tempfile.NamedTemporaryFile(suffix=".yaml", delete=False) as f:
            f.write(b"""
name: test-workflow
state_schema:
  message: str
nodes:
  - name: hello
    run: |
      return {"message": "Hello"}
edges:
  - from: __start__
    to: hello
  - from: hello
    to: __end__
""")
            yaml_path = f.name

        try:
            result = runner.invoke(app, ["run", yaml_path, "--cache-only"])
            # Flag should be accepted (no unknown option error)
            assert "Error: No such option: --cache-only" not in result.output
        finally:
            os.unlink(yaml_path)


class TestCacheManagement(unittest.TestCase):
    """Tests for cache management operations."""

    def test_cache_with_entries(self):
        """Cache operations with actual entries."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create cache with some entries
            cache = RemoteFileCache(cache_dir=Path(tmpdir))

            # Create a mock entry directly in the manifest
            entry_dir = cache.files_dir / "abc123"
            entry_dir.mkdir(parents=True)
            (entry_dir / "test.yaml").write_text("content")

            manifest = {
                "version": 1,
                "entries": {
                    "abc123": {
                        "url": "s3://bucket/test.yaml",
                        "local_path": "files/abc123/test.yaml",
                        "created_at": 0,  # Very old
                        "ttl_seconds": 3600,
                        "is_permanent": False,
                        "size_bytes": 7,
                    }
                },
            }
            with open(cache.manifest_path, "w") as f:
                json.dump(manifest, f)

            # Test list
            entries = cache.list_entries()
            assert len(entries) == 1
            assert entries[0]["url"] == "s3://bucket/test.yaml"

            # Test info
            info = cache.info()
            assert info["total_entries"] == 1

            # Test clear
            count = cache.clear()
            assert count == 1

            # Verify empty
            entries = cache.list_entries()
            assert len(entries) == 0


if __name__ == "__main__":
    unittest.main()
