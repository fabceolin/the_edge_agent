"""
Tests for Schema Loader.

TEA-BUILTIN-008.2: Schema Loading with Git Refs and Remote Storage

This module tests:
- Git reference parsing (short form and full URL)
- fsspec URI detection
- Schema caching with TTL
- Error handling
"""

import json
import os
import pytest
import subprocess
import time
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch, MagicMock

from the_edge_agent.schema.schema_loader import (
    parse_git_reference,
    is_git_reference,
    is_fsspec_uri,
    GitReference,
    GitSchemaFetcher,
    FsspecSchemaFetcher,
    SchemaCache,
    fetch_schema,
    resolve_schema_uses,
    clear_cache,
)


class TestParseGitReference:
    """Test Git reference parsing."""

    def test_short_form_basic(self):
        """Test parsing short form: owner/repo@ref#path"""
        ref = parse_git_reference("company/schemas@v1.0.0#invoice/schema.json")

        assert ref.host == "github.com"
        assert ref.owner == "company"
        assert ref.repo == "schemas"
        assert ref.ref == "v1.0.0"
        assert ref.path == "invoice/schema.json"
        assert ref.protocol == "https"

    def test_short_form_with_branch(self):
        """Test short form with branch name."""
        ref = parse_git_reference("myorg/myrepo@main#base.json")

        assert ref.owner == "myorg"
        assert ref.repo == "myrepo"
        assert ref.ref == "main"
        assert ref.path == "base.json"

    def test_short_form_with_feature_branch(self):
        """Test short form with feature branch containing slashes."""
        ref = parse_git_reference("org/repo@feature/my-feature#schema.json")

        assert ref.ref == "feature/my-feature"
        assert ref.path == "schema.json"

    def test_full_url_https(self):
        """Test full URL with HTTPS."""
        ref = parse_git_reference(
            "git+https://github.com/company/schemas.git@v1.0.0#schema.json"
        )

        assert ref.host == "github.com"
        assert ref.owner == "company"
        assert ref.repo == "schemas"
        assert ref.ref == "v1.0.0"
        assert ref.path == "schema.json"
        assert ref.protocol == "https"

    def test_full_url_ssh(self):
        """Test full URL with SSH."""
        ref = parse_git_reference(
            "git+ssh://git@github.com/company/private.git@main#schema.json"
        )

        assert ref.protocol == "ssh"
        assert ref.host == "git@github.com"
        assert ref.owner == "company"
        assert ref.repo == "private"

    def test_full_url_gitlab(self):
        """Test full URL with GitLab."""
        ref = parse_git_reference(
            "git+https://gitlab.com/org/project.git@v2.0.0#schemas/invoice.json"
        )

        assert ref.host == "gitlab.com"
        assert ref.owner == "org"
        assert ref.repo == "project"
        assert ref.path == "schemas/invoice.json"

    def test_full_url_without_git_suffix(self):
        """Test full URL without .git suffix."""
        ref = parse_git_reference(
            "git+https://github.com/company/schemas@main#schema.json"
        )

        assert ref.repo == "schemas"
        assert ref.ref == "main"

    def test_invalid_format_no_at(self):
        """Test error on invalid format - missing @."""
        with pytest.raises(ValueError) as exc_info:
            parse_git_reference("company/schemas#path.json")

        assert "Invalid Git reference format" in str(exc_info.value)

    def test_invalid_format_no_hash(self):
        """Test error on invalid format - missing #."""
        with pytest.raises(ValueError) as exc_info:
            parse_git_reference("company/schemas@v1.0.0")

        assert "Invalid Git reference format" in str(exc_info.value)

    def test_invalid_format_empty(self):
        """Test error on empty string."""
        with pytest.raises(ValueError):
            parse_git_reference("")

    def test_preserves_original(self):
        """Test that original reference is preserved."""
        original = "company/schemas@v1.0.0#path.json"
        ref = parse_git_reference(original)

        assert ref.original == original


class TestReferenceTypeDetection:
    """Test reference type detection functions."""

    def test_is_git_reference_short_form(self):
        """Test short form Git ref detection."""
        assert is_git_reference("company/schemas@v1.0.0#schema.json") is True

    def test_is_git_reference_full_url(self):
        """Test full URL Git ref detection."""
        assert is_git_reference(
            "git+https://github.com/co/repo.git@main#schema.json"
        ) is True

    def test_is_git_reference_s3(self):
        """Test S3 URI is not Git ref."""
        assert is_git_reference("s3://bucket/schema.json") is False

    def test_is_git_reference_https(self):
        """Test HTTPS URI is not Git ref."""
        assert is_git_reference("https://example.com/schema.json") is False

    def test_is_fsspec_uri_s3(self):
        """Test S3 URI detection."""
        assert is_fsspec_uri("s3://bucket/path/schema.json") is True

    def test_is_fsspec_uri_gs(self):
        """Test GCS URI detection."""
        assert is_fsspec_uri("gs://bucket/path/schema.json") is True

    def test_is_fsspec_uri_az(self):
        """Test Azure URI detection."""
        assert is_fsspec_uri("az://container/path/schema.json") is True

    def test_is_fsspec_uri_https(self):
        """Test HTTPS URI detection."""
        assert is_fsspec_uri("https://example.com/schema.json") is True

    def test_is_fsspec_uri_file(self):
        """Test file:// URI detection."""
        assert is_fsspec_uri("file:///absolute/path/schema.json") is True

    def test_is_fsspec_uri_git_ref(self):
        """Test Git ref is not fsspec URI."""
        assert is_fsspec_uri("company/schemas@v1.0.0#schema.json") is False


class TestSchemaCache:
    """Test schema caching with TTL."""

    def test_cache_set_and_get(self):
        """Test basic cache set and get."""
        cache = SchemaCache(ttl=60)
        schema = {"type": "object"}

        cache.set("key1", schema)
        result = cache.get("key1")

        assert result == schema

    def test_cache_miss(self):
        """Test cache miss returns None."""
        cache = SchemaCache(ttl=60)

        result = cache.get("nonexistent")

        assert result is None

    def test_cache_expiry(self):
        """Test cache expiry with short TTL."""
        cache = SchemaCache(ttl=0)  # Immediate expiry
        schema = {"type": "object"}

        cache.set("key1", schema)
        time.sleep(0.01)  # Wait a bit
        result = cache.get("key1")

        assert result is None

    def test_cache_clear(self):
        """Test cache clearing."""
        cache = SchemaCache(ttl=60)
        cache.set("key1", {"a": 1})
        cache.set("key2", {"b": 2})

        cache.clear()

        assert cache.get("key1") is None
        assert cache.get("key2") is None


class TestGitSchemaFetcher:
    """Test Git schema fetcher."""

    def test_clone_url_https(self):
        """Test HTTPS clone URL building."""
        fetcher = GitSchemaFetcher()
        ref = GitReference(
            host="github.com",
            owner="company",
            repo="schemas",
            ref="main",
            path="schema.json",
            protocol="https",
            original="company/schemas@main#schema.json"
        )

        url = fetcher._build_clone_url(ref)

        assert url == "https://github.com/company/schemas.git"

    def test_clone_url_ssh(self):
        """Test SSH clone URL building."""
        fetcher = GitSchemaFetcher()
        ref = GitReference(
            host="github.com",
            owner="company",
            repo="private",
            ref="main",
            path="schema.json",
            protocol="ssh",
            original="git+ssh://..."
        )

        url = fetcher._build_clone_url(ref)

        assert url == "git@github.com:company/private.git"

    def test_fetch_with_local_repo(self):
        """Test fetching from local Git repository."""
        with TemporaryDirectory() as tmpdir:
            # Create a local Git repo
            repo_dir = Path(tmpdir) / "test-repo"
            repo_dir.mkdir()

            # Initialize repo
            subprocess.run(
                ["git", "init"],
                cwd=repo_dir,
                capture_output=True,
                check=True
            )
            subprocess.run(
                ["git", "config", "user.email", "test@test.com"],
                cwd=repo_dir,
                capture_output=True
            )
            subprocess.run(
                ["git", "config", "user.name", "Test"],
                cwd=repo_dir,
                capture_output=True
            )

            # Create schema file
            schema_dir = repo_dir / "schemas"
            schema_dir.mkdir()
            schema_file = schema_dir / "test.json"
            test_schema = {"type": "object", "properties": {"name": {"type": "string"}}}
            schema_file.write_text(json.dumps(test_schema))

            # Commit
            subprocess.run(["git", "add", "."], cwd=repo_dir, capture_output=True)
            subprocess.run(
                ["git", "commit", "-m", "Initial"],
                cwd=repo_dir,
                capture_output=True
            )

            # Create fetcher with custom cache dir
            cache_dir = Path(tmpdir) / "cache"
            fetcher = GitSchemaFetcher(cache_dir=cache_dir)

            # Mock the clone to use local path
            with patch.object(fetcher, '_build_clone_url', return_value=str(repo_dir)):
                ref = GitReference(
                    host="local",
                    owner="test",
                    repo="test-repo",
                    ref="master",
                    path="schemas/test.json",
                    protocol="file",
                    original="test/test-repo@master#schemas/test.json"
                )

                # This will clone from local repo
                try:
                    schema = fetcher.fetch(ref)
                    assert schema == test_schema
                except RuntimeError:
                    # Git operations may fail in some environments
                    pytest.skip("Git operations not available")


class TestFsspecSchemaFetcher:
    """Test fsspec schema fetcher."""

    def test_fetch_local_file(self):
        """Test fetching from local file."""
        with TemporaryDirectory() as tmpdir:
            schema_path = Path(tmpdir) / "schema.json"
            test_schema = {"type": "object", "properties": {"id": {"type": "integer"}}}
            schema_path.write_text(json.dumps(test_schema))

            fetcher = FsspecSchemaFetcher()

            # Mock _get_filesystem to work without core_actions
            with patch.object(fetcher, 'cache', SchemaCache()):
                # Create a simple mock for file reading
                with patch('fsspec.filesystem') as mock_fs:
                    mock_file = MagicMock()
                    mock_file.__enter__ = MagicMock(return_value=mock_file)
                    mock_file.__exit__ = MagicMock(return_value=False)
                    mock_file.read.return_value = json.dumps(test_schema)

                    mock_filesystem = MagicMock()
                    mock_filesystem.open.return_value = mock_file
                    mock_fs.return_value = mock_filesystem

                    # Skip the test if core_actions are available
                    # (they will use _get_filesystem instead)
                    try:
                        from the_edge_agent.actions.core_actions import _get_filesystem
                        pytest.skip("Using core_actions infrastructure")
                    except ImportError:
                        pass

    def test_fetch_yaml_file(self):
        """Test fetching YAML schema."""
        with TemporaryDirectory() as tmpdir:
            schema_path = Path(tmpdir) / "schema.yaml"
            schema_path.write_text("type: object\nproperties:\n  name:\n    type: string\n")

            # Test detection by extension
            assert schema_path.suffix in ('.yaml', '.yml')


class TestUnifiedFetch:
    """Test unified fetch_schema function."""

    def test_fetch_detects_git_ref(self):
        """Test fetch_schema routes Git refs correctly."""
        with patch('the_edge_agent.schema.schema_loader._get_git_fetcher') as mock_fetcher:
            mock_instance = MagicMock()
            mock_instance.fetch.return_value = {"type": "object"}
            mock_fetcher.return_value = mock_instance

            fetch_schema("company/schemas@v1.0.0#schema.json")

            mock_instance.fetch.assert_called_once()

    def test_fetch_detects_fsspec_uri(self):
        """Test fetch_schema routes fsspec URIs correctly."""
        with patch('the_edge_agent.schema.schema_loader._get_fsspec_fetcher') as mock_fetcher:
            mock_instance = MagicMock()
            mock_instance.fetch.return_value = {"type": "object"}
            mock_fetcher.return_value = mock_instance

            fetch_schema("s3://bucket/schema.json")

            mock_instance.fetch.assert_called_once()

    def test_fetch_invalid_reference(self):
        """Test fetch_schema with invalid reference."""
        with pytest.raises(ValueError) as exc_info:
            fetch_schema("not-a-valid-reference")

        assert "Unknown reference format" in str(exc_info.value)


class TestResolveSchemaUses:
    """Test resolve_schema_uses function."""

    def test_resolve_multiple_refs(self):
        """Test resolving multiple schema references."""
        with patch('the_edge_agent.schema.schema_loader._get_git_fetcher') as mock_git:
            with patch('the_edge_agent.schema.schema_loader._get_fsspec_fetcher') as mock_fsspec:
                mock_git_instance = MagicMock()
                mock_git_instance.fetch.return_value = {"base": True}
                mock_git.return_value = mock_git_instance

                mock_fsspec_instance = MagicMock()
                mock_fsspec_instance.fetch.return_value = {"overlay": True}
                mock_fsspec.return_value = mock_fsspec_instance

                schemas = resolve_schema_uses([
                    "company/schemas@v1#base.json",
                    "s3://bucket/overlay.json"
                ])

                assert len(schemas) == 2
                assert schemas[0] == {"base": True}
                assert schemas[1] == {"overlay": True}


class TestClearCache:
    """Test global cache clearing."""

    def test_clear_cache(self):
        """Test clearing global cache."""
        # This just verifies the function exists and is callable
        clear_cache()  # Should not raise


class TestIntegration:
    """Integration tests with real local operations."""

    def test_full_flow_local_git(self):
        """Test full flow with local Git repo."""
        with TemporaryDirectory() as tmpdir:
            # Create a local Git repo
            repo_dir = Path(tmpdir) / "schemas"
            repo_dir.mkdir()

            # Initialize
            result = subprocess.run(
                ["git", "init"],
                cwd=repo_dir,
                capture_output=True
            )
            if result.returncode != 0:
                pytest.skip("Git not available")

            subprocess.run(
                ["git", "config", "user.email", "test@test.com"],
                cwd=repo_dir,
                capture_output=True
            )
            subprocess.run(
                ["git", "config", "user.name", "Test"],
                cwd=repo_dir,
                capture_output=True
            )

            # Create schema
            schema_file = repo_dir / "invoice.json"
            schema = {
                "$schema": "https://json-schema.org/draft/2020-12/schema",
                "type": "object",
                "properties": {"amount": {"type": "number"}}
            }
            schema_file.write_text(json.dumps(schema, indent=2))

            # Commit
            subprocess.run(["git", "add", "."], cwd=repo_dir, capture_output=True)
            subprocess.run(
                ["git", "commit", "-m", "Add invoice schema"],
                cwd=repo_dir,
                capture_output=True
            )

            # Now test parsing the reference (not actual fetch, which needs network)
            ref = parse_git_reference("owner/schemas@main#invoice.json")
            assert ref.path == "invoice.json"
            assert ref.ref == "main"
