# TEA-CLI-001: URL-Based File Input for Python CLI

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-CLI-001 |
| **Title** | URL-Based File Input for Python CLI |
| **Epic** | CLI Enhancement |
| **Priority** | High |
| **Estimate** | 5 points |
| **Blocks** | TEA-CLI-002 (Rust implementation) |
| **Validation Status** | ✅ PO Approved |
| **Created** | 2026-02-01 |

---

## User Story

**As a** workflow developer using the Python CLI,
**I want to** specify file inputs using URLs (file://, s3://, gs://, github://, etc.),
**So that** I can run workflows directly from remote storage or git repositories without manual downloads.

---

## Background & Context

The `uses:` YAML syntax already supports fsspec-based URL resolution via `_get_filesystem()` in `core_actions.py`. This story extends that capability to all Python CLI file input arguments.

```bash
# Run workflow directly from GitHub
tea run github://fabceolin/the_edge_agent@main/examples/workflows/agent.yaml

# Use S3-hosted workflow with local DOT file
tea run --from-dot workflow.dot --dot-workflow s3://my-bucket/workflows/handler.yaml

# Resume from cloud-stored checkpoint
tea run workflow.yaml --checkpoint gs://checkpoints/run-123.pkl
```

---

## Acceptance Criteria

### AC1: CLI URL Support

- [ ] All file input flags accept URLs with any fsspec-supported protocol
- [ ] Affected flags: positional `file`, `--checkpoint`, `--checkpoint-dir`, `--from-dot`, `--dot-workflow`, `--input @path`
- [ ] Uses existing `_get_filesystem()` from `core_actions.py`
- [ ] Relative paths in remote YAMLs resolve relative to remote base URL
- [ ] Local paths work unchanged (passthrough)

### AC2: Git Protocol Support

- [ ] Format: `github://user/repo@branch/path/to/file.yaml`
- [ ] Support branch names: `@main`, `@feature/xyz`
- [ ] Support commit SHA: `@a1b2c3d`
- [ ] Support tags: `@v1.0.0`
- [ ] Private repository support via `GITHUB_TOKEN` / `GIT_TOKEN` env vars
- [ ] GitLab support: `gitlab://user/repo@branch/path`

### AC3: Caching System

- [ ] Remote files cached locally in `~/.cache/tea/remote/` (XDG compliant)
- [ ] Cache key: SHA256 hash of (URL + ref for git)
- [ ] Cache TTL configurable via `TEA_CACHE_TTL` env var (default: 1 hour)
- [ ] `--no-cache` flag to force fresh fetch
- [ ] `--cache-only` flag for offline mode (fail if not cached)
- [ ] `--cache-dir <path>` flag to override default cache location
- [ ] Git refs: branches re-fetch on TTL expiry, SHA/tags cached indefinitely
- [ ] Cache size limit via `TEA_CACHE_MAX_SIZE` (default: 1GB, TTL-based eviction)
- [ ] Debug logging for cache hits/misses when `--verbose` enabled

### AC4: Cache Management CLI

- [ ] `tea cache list` - Show cached files with URL, size, age
- [ ] `tea cache clear` - Clear all cached files
- [ ] `tea cache clear --older-than <duration>` - Clear files older than duration (e.g., `7d`, `24h`)
- [ ] `tea cache info` - Show cache location, total size, entry count

### AC5: Authentication via Environment Variables

- [ ] AWS: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`
- [ ] GCS: `GOOGLE_APPLICATION_CREDENTIALS`
- [ ] Azure: `AZURE_STORAGE_ACCOUNT_NAME`, `AZURE_STORAGE_ACCOUNT_KEY`
- [ ] Git: `GITHUB_TOKEN`, `GITLAB_TOKEN`, `GIT_TOKEN` (generic fallback)
- [ ] Clear error messages when credentials missing

### AC6: Error Handling

- [ ] Missing backend package: suggest install command (`pip install s3fs`)
- [ ] Authentication failure: identify which credential is missing
- [ ] Network failure: suggest `--cache-only` if cached version exists
- [ ] Invalid URL format: show expected format with examples

### AC7: Test Infrastructure

- [ ] Use `moto` for S3 mocking, `responses` for HTTP mocking
- [ ] Use local git repos (via `tempfile`) for git:// tests
- [ ] All tests run offline (no real network calls)
- [ ] Minimum 90% code coverage on new modules

### AC8: Documentation

- [ ] Update CLI `--help` text with URL examples
- [ ] Add `docs/shared/remote-files.md` with full protocol reference
- [ ] Inline code comments for cache key generation algorithm
- [ ] Inline code comments for URL parsing logic

---

## Technical Design

### New Module: `cache.py`

```python
# python/src/the_edge_agent/cache.py
"""Remote file cache manager.

Cache key algorithm:
- For non-git URLs: SHA256(canonical_url)[:16]
- For git URLs: SHA256(f"{repo}@{ref}")[:16]
- Branch refs include TTL check, SHA/tags are permanent
"""

import hashlib
import json
import os
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any

class RemoteFileCache:
    def __init__(self, cache_dir: Optional[Path] = None):
        self.cache_dir = cache_dir or self._default_cache_dir()
        self.manifest_path = self.cache_dir / "manifest.json"

    def _default_cache_dir(self) -> Path:
        """XDG-compliant cache directory."""
        xdg_cache = os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache")
        return Path(xdg_cache) / "tea" / "remote"

    def _cache_key(self, url: str) -> str:
        """Generate cache key from URL."""
        return hashlib.sha256(url.encode()).hexdigest()[:16]

    def has_valid(self, url: str, ttl: Optional[int] = None) -> bool:
        """Check if valid (non-expired) cache entry exists."""
        ...

    def get_path(self, url: str) -> Path:
        """Get local path for cached URL."""
        ...

    def fetch_and_cache(self, fs, fs_path: str, url: str) -> Path:
        """Fetch from remote and cache locally."""
        ...

    def clear(self, older_than: Optional[timedelta] = None) -> int:
        """Clear cache entries, optionally filtering by age."""
        ...

    def list_entries(self) -> list[Dict[str, Any]]:
        """List all cache entries with metadata."""
        ...

    def info(self) -> Dict[str, Any]:
        """Get cache statistics."""
        ...
```

### CLI Changes: `cli.py`

```python
# Add to existing cli.py

def resolve_file_url(
    path: str,
    cache: bool = True,
    cache_dir: Optional[Path] = None,
    verbose: bool = False
) -> Path:
    """Resolve local path or remote URL to local file path."""
    if not _is_url(path):
        return Path(path)

    from .actions.core_actions import _get_filesystem
    from .cache import RemoteFileCache

    cache_manager = RemoteFileCache(cache_dir=cache_dir)

    if cache and cache_manager.has_valid(path):
        if verbose:
            typer.echo(f"Cache HIT: {path}", err=True)
        return cache_manager.get_path(path)

    if verbose:
        typer.echo(f"Cache MISS: {path}", err=True)

    fs, fs_path, err = _get_filesystem(path)
    if err:
        if cache_manager.has_expired(path):
            raise typer.BadParameter(
                f"{err['error']}\nTip: A cached version exists. Use --cache-only to use it."
            )
        raise typer.BadParameter(err['error'])

    return cache_manager.fetch_and_cache(fs, fs_path, path)


# New cache subcommand group
@app.command()
def cache():
    """Manage remote file cache."""
    pass

@cache.command("list")
def cache_list():
    """Show cached files with URL, size, age."""
    ...

@cache.command("clear")
def cache_clear(older_than: Optional[str] = None):
    """Clear cached files."""
    ...

@cache.command("info")
def cache_info():
    """Show cache statistics."""
    ...
```

### Git Protocol in `core_actions.py`

```python
# Add git:// protocol support to _get_filesystem()

def _get_filesystem(path: str) -> Tuple[Any, str, Optional[Dict[str, Any]]]:
    """Get fsspec filesystem and path from a URI."""

    # Handle git:// protocols specially
    if path.startswith(("github://", "gitlab://")):
        return _get_git_filesystem(path)

    # Existing fsspec logic...
    try:
        fs, fs_path = fsspec.url_to_fs(path)
        return fs, fs_path, None
    except ImportError as e:
        # ... existing error handling
```

### Cache Structure

```
~/.cache/tea/remote/
├── manifest.json          # URL -> cache entry mapping with TTL
├── github/
│   └── fabceolin_the_edge_agent_main_abc123/
│       └── examples/workflows/agent.yaml
├── s3/
│   └── my-bucket_workflows_def456/
│       └── handler.yaml
└── gs/
    └── checkpoints_run-123_ghi789/
        └── checkpoint.pkl
```

### Manifest Format

```json
{
  "version": 1,
  "entries": {
    "abc123def456": {
      "url": "github://fabceolin/the_edge_agent@main/examples/agent.yaml",
      "local_path": "github/fabceolin_the_edge_agent_main_abc123/examples/agent.yaml",
      "created_at": 1706745600,
      "ttl_seconds": 3600,
      "is_permanent": false
    }
  }
}
```

---

## Files to Modify

| File | Changes |
|------|---------|
| `python/src/the_edge_agent/cli.py` | Add URL resolution, cache flags, `tea cache` subcommands |
| `python/src/the_edge_agent/cache.py` | **NEW** - Remote file cache manager |
| `python/src/the_edge_agent/actions/core_actions.py` | Add git:// protocol to `_get_filesystem()` |
| `python/src/the_edge_agent/yaml_engine.py` | Use cache for remote subgraphs |
| `python/tests/test_cache.py` | **NEW** - Cache unit tests |
| `python/tests/test_cli_urls.py` | **NEW** - CLI URL integration tests |
| `python/tests/test_git_protocol.py` | **NEW** - Git protocol tests |
| `docs/shared/remote-files.md` | **NEW** - Full protocol reference |

---

## Dependencies

| Package | Purpose | Install |
|---------|---------|---------|
| `fsspec` | Core URL resolution | Already installed |
| `gcsfs` | GCS support | Optional: `pip install gcsfs` |
| `s3fs` | S3 support | Optional: `pip install s3fs` |
| `adlfs` | Azure support | Optional: `pip install adlfs` |
| `moto[s3]` | S3 mocking (dev) | `pip install moto[s3]` |
| `responses` | HTTP mocking (dev) | `pip install responses` |

---

## Rollback Procedure

1. **Code Rollback**: Revert commits introducing URL support
2. **Cache Cleanup**: Optional - `~/.cache/tea/remote/` can remain or be deleted
3. **No Breaking Changes**: Local path usage unchanged

---

## Test Cases

| Test | Mock Strategy |
|------|---------------|
| `test_local_path_passthrough` | None |
| `test_s3_url_resolution` | `moto` |
| `test_github_branch_fetch` | Local git repo |
| `test_github_sha_fetch` | Local git repo |
| `test_cache_hit` | Filesystem only |
| `test_cache_ttl_expiry` | Mock time |
| `test_cache_only_mode` | None |
| `test_no_cache_mode` | `moto` |
| `test_private_repo_auth` | Local git repo + env |
| `test_missing_credentials` | Mock 401 |
| `test_cache_clear_command` | Filesystem |
| `test_cache_list_command` | Filesystem |

---

## Out of Scope (Deferred to TEA-CLI-002)

- Rust CLI implementation
- `object_store` crate integration
- Rust git2 integration

---

## Definition of Done

- [ ] All acceptance criteria met
- [ ] Unit tests pass with mocked backends
- [ ] Integration tests pass with local git repos
- [ ] `docs/shared/remote-files.md` created
- [ ] CLI `--help` shows URL examples
- [ ] `tea cache` subcommands functional
- [ ] Code reviewed and merged
