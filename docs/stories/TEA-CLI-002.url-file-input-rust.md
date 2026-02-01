# TEA-CLI-002: URL-Based File Input for Rust CLI

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-CLI-002 |
| **Title** | URL-Based File Input for Rust CLI |
| **Epic** | CLI Enhancement |
| **Priority** | Medium |
| **Estimate** | 5 points |
| **Blocked By** | TEA-CLI-001 (Python implementation) |
| **Validation Status** | ✅ PO Approved |
| **Created** | 2026-02-01 |

---

## User Story

**As a** workflow developer using the Rust CLI,
**I want to** specify file inputs using URLs (file://, s3://, gs://, github://, etc.),
**So that** I have feature parity with the Python CLI for remote workflow execution.

---

## Background & Context

TEA-CLI-001 delivers URL support for Python CLI. This story provides equivalent functionality for the Rust CLI, leveraging `object_store` crate for cloud storage and `git2` for git protocol support.

The Rust implementation should match Python behavior exactly, using the same:
- URL formats (`github://user/repo@branch/path`)
- Cache structure (`~/.cache/tea/remote/`)
- Environment variables for authentication
- CLI flags (`--no-cache`, `--cache-only`, `--cache-dir`)

---

## Acceptance Criteria

### AC1: CLI URL Support

- [ ] All file input flags accept URLs matching Python's supported protocols
- [ ] Affected flags: positional `file`, `--checkpoint`, `--checkpoint-dir`, `--from-dot`, `--dot-workflow`
- [ ] Implement using `object_store` crate (^0.10) for cloud storage
- [ ] Local paths work unchanged (passthrough)
- [ ] Behavior matches Python CLI exactly

### AC2: Git Protocol Support

- [ ] Format: `github://user/repo@branch/path/to/file.yaml` (matches Python)
- [ ] Implement using `git2` crate (^0.18)
- [ ] Support branch names, commit SHA, tags
- [ ] Private repository support via `GITHUB_TOKEN` / `GIT_TOKEN` env vars
- [ ] GitLab support: `gitlab://user/repo@branch/path`

### AC3: Caching System

- [ ] Share cache directory with Python (`~/.cache/tea/remote/`)
- [ ] Read/write compatible manifest format (JSON)
- [ ] Same cache key algorithm as Python (SHA256)
- [ ] `--no-cache`, `--cache-only`, `--cache-dir` flags
- [ ] TTL via `TEA_CACHE_TTL` env var
- [ ] Debug logging when `--verbose` enabled

### AC4: Cache Management CLI

- [ ] `tea cache list` - Show cached files
- [ ] `tea cache clear` - Clear all cached files
- [ ] `tea cache clear --older-than <duration>` - Filter by age
- [ ] `tea cache info` - Show cache statistics

### AC5: Trait-Based Design for Testing

- [ ] Define `RemoteFileSystem` trait for URL resolution
- [ ] Implement mock version for unit tests
- [ ] Use `wiremock` for HTTP integration tests
- [ ] All tests run offline

### AC6: Error Handling

- [ ] Match Python error messages exactly
- [ ] Suggest `--cache-only` on network failure if cache exists
- [ ] Clear credential error messages

### AC7: Documentation

- [ ] Update `docs/rust/actions-reference.md` with URL support
- [ ] Update Rust CLI `--help` text
- [ ] Inline code comments matching Python implementation

---

## Technical Design

### New Module Structure

```
rust/src/remote/
├── mod.rs          # Public API, RemoteFile enum
├── cache.rs        # Cache manager (compatible with Python)
├── git.rs          # Git protocol handler
├── cloud.rs        # object_store wrapper
└── traits.rs       # RemoteFileSystem trait for mocking
```

### Trait Definition

```rust
// rust/src/remote/traits.rs

use std::path::PathBuf;
use crate::error::TeaResult;

/// Trait for remote file resolution - enables mocking in tests
pub trait RemoteFileSystem: Send + Sync {
    /// Fetch remote URL to local path, using cache if available
    fn fetch(&self, url: &str) -> TeaResult<PathBuf>;

    /// Check if protocol is supported
    fn supports_protocol(&self, protocol: &str) -> bool;

    /// Force refresh, bypassing cache
    fn fetch_no_cache(&self, url: &str) -> TeaResult<PathBuf>;

    /// Use cache only, fail if not cached
    fn fetch_cache_only(&self, url: &str) -> TeaResult<PathBuf>;
}

/// Production implementation using object_store + git2
pub struct DefaultRemoteFileSystem {
    cache: Cache,
    // ...
}

/// Mock implementation for testing
#[cfg(test)]
pub struct MockRemoteFileSystem {
    responses: HashMap<String, PathBuf>,
}
```

### RemoteFile Enum

```rust
// rust/src/remote/mod.rs

/// Parsed remote file URL
pub enum RemoteFile {
    Local(PathBuf),
    S3 { bucket: String, key: String },
    Gcs { bucket: String, key: String },
    Azure { container: String, path: String },
    Http { url: String },
    Git {
        host: GitHost,  // GitHub, GitLab
        repo: String,
        ref_: GitRef,   // Branch, Tag, Sha
        path: String
    },
}

pub enum GitHost {
    GitHub,
    GitLab,
}

pub enum GitRef {
    Branch(String),
    Tag(String),
    Sha(String),
}

impl RemoteFile {
    /// Parse URL string into RemoteFile
    ///
    /// # Examples
    /// - `github://user/repo@main/path/file.yaml`
    /// - `s3://bucket/key/path`
    /// - `file:///absolute/path`
    pub fn parse(url: &str) -> TeaResult<Self> { ... }

    /// Fetch to local path using provided filesystem
    pub fn fetch<F: RemoteFileSystem>(&self, fs: &F) -> TeaResult<PathBuf> { ... }
}
```

### Cache Compatibility

```rust
// rust/src/remote/cache.rs

/// Cache manifest format - compatible with Python implementation
#[derive(Serialize, Deserialize)]
struct CacheManifest {
    version: u32,  // Must be 1 for Python compatibility
    entries: HashMap<String, CacheEntry>,
}

#[derive(Serialize, Deserialize)]
struct CacheEntry {
    url: String,
    local_path: String,
    created_at: i64,  // Unix timestamp
    ttl_seconds: Option<i64>,
    is_permanent: bool,  // true for SHA/tags
}

impl Cache {
    /// Use same directory as Python
    fn default_dir() -> PathBuf {
        dirs::cache_dir()
            .unwrap_or_else(|| PathBuf::from(".cache"))
            .join("tea")
            .join("remote")
    }

    /// Same hash algorithm as Python
    fn cache_key(url: &str) -> String {
        use sha2::{Sha256, Digest};
        let hash = Sha256::digest(url.as_bytes());
        hex::encode(&hash[..8])  // First 16 hex chars
    }
}
```

### Git Protocol Handler

```rust
// rust/src/remote/git.rs

use git2::{Repository, RemoteCallbacks, Cred};

pub struct GitFetcher {
    cache: Cache,
}

impl GitFetcher {
    /// Fetch file from GitHub/GitLab repository
    ///
    /// URL format: github://user/repo@ref/path/to/file
    ///
    /// Authentication:
    /// - Uses GITHUB_TOKEN or GIT_TOKEN from environment
    /// - Falls back to unauthenticated for public repos
    pub fn fetch(&self, url: &str) -> TeaResult<PathBuf> {
        let parsed = GitUrl::parse(url)?;

        // Check cache first
        if let Some(cached) = self.cache.get(&url) {
            if !cached.is_expired() || parsed.ref_.is_permanent() {
                return Ok(cached.path);
            }
        }

        // Clone/fetch repository
        let repo_dir = self.cache.repo_dir(&parsed);
        let repo = self.clone_or_fetch(&parsed, &repo_dir)?;

        // Checkout specific ref
        let commit = self.resolve_ref(&repo, &parsed.ref_)?;
        repo.checkout_tree(&commit.as_object(), None)?;

        // Copy file to cache
        let file_path = repo_dir.join(&parsed.path);
        let cached_path = self.cache.store(&url, &file_path)?;

        Ok(cached_path)
    }

    fn get_credentials(&self) -> Option<Cred> {
        let token = std::env::var("GITHUB_TOKEN")
            .or_else(|_| std::env::var("GITLAB_TOKEN"))
            .or_else(|_| std::env::var("GIT_TOKEN"))
            .ok()?;

        Cred::userpass_plaintext("git", &token).ok()
    }
}
```

---

## Files to Create/Modify

| File | Changes |
|------|---------|
| `rust/src/remote/mod.rs` | **NEW** - RemoteFile enum, public API |
| `rust/src/remote/cache.rs` | **NEW** - Python-compatible cache |
| `rust/src/remote/git.rs` | **NEW** - Git protocol with git2 |
| `rust/src/remote/cloud.rs` | **NEW** - object_store wrapper |
| `rust/src/remote/traits.rs` | **NEW** - RemoteFileSystem trait |
| `rust/src/bin/tea.rs` | Add URL resolution, cache subcommands |
| `rust/src/lib.rs` | Export remote module |
| `rust/Cargo.toml` | Add dependencies |
| `rust/tests/remote_test.rs` | **NEW** - Integration tests |
| `docs/rust/actions-reference.md` | Update with URL support |

---

## Dependencies

| Crate | Version | Purpose |
|-------|---------|---------|
| `object_store` | ^0.10 | S3, GCS, Azure abstraction |
| `git2` | ^0.18 | Git repository access |
| `directories` | ^5.0 | XDG cache paths |
| `sha2` | ^0.10 | Cache key hashing |
| `hex` | ^0.4 | Hash encoding |
| `wiremock` | ^0.5 (dev) | HTTP mocking |

### Cargo.toml Addition

```toml
[dependencies]
object_store = { version = "0.10", features = ["aws", "gcp", "azure"] }
git2 = "0.18"
directories = "5.0"
sha2 = "0.10"
hex = "0.4"

[dev-dependencies]
wiremock = "0.5"
tempfile = "3.0"
```

---

## Rollback Procedure

1. **Code Rollback**: Revert Rust remote module commits
2. **Cache**: Shared with Python - no separate cleanup needed
3. **No Breaking Changes**: Local paths unchanged

---

## Test Cases

| Test | Strategy |
|------|----------|
| `test_local_path_passthrough` | Direct |
| `test_s3_url_resolution` | MockRemoteFileSystem |
| `test_github_fetch` | Local git repo via tempfile |
| `test_cache_hit` | Filesystem |
| `test_cache_python_compatible` | Read Python-written manifest |
| `test_cache_write_python_readable` | Write manifest, verify with Python |
| `test_trait_mock` | MockRemoteFileSystem |

---

## Cross-Story Validation

| Check | Requirement |
|-------|-------------|
| URL format parity | `github://user/repo@ref/path` identical |
| Cache key parity | Same SHA256 algorithm |
| Manifest compatibility | Rust can read Python cache, vice versa |
| Error message parity | Same wording for common errors |
| Flag parity | `--no-cache`, `--cache-only`, `--cache-dir` |

---

## Integration Test: Cross-Language Cache

```bash
# Python writes cache entry
python -c "
from the_edge_agent.cache import RemoteFileCache
cache = RemoteFileCache()
cache.store('github://test/repo@main/file.yaml', '/tmp/test.yaml')
"

# Rust reads same entry
cargo run -- cache list | grep "github://test/repo@main/file.yaml"

# Rust writes cache entry
cargo run -- run github://test/repo@main/other.yaml --dry-run

# Python reads Rust entry
python -c "
from the_edge_agent.cache import RemoteFileCache
cache = RemoteFileCache()
entry = cache.get('github://test/repo@main/other.yaml')
assert entry is not None
"
```

---

## Definition of Done

- [ ] All acceptance criteria met
- [ ] Tests pass with mocked backends
- [ ] Cache interoperability with Python verified
- [ ] Documentation updated
- [ ] Rust CLI `--help` matches Python examples
- [ ] `tea cache` subcommands functional
- [ ] Code reviewed and merged
