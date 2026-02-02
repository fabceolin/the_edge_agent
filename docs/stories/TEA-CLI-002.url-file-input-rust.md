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

---

## QA Notes - Risk Profile

**Assessment Date:** 2026-02-01
**Reviewer:** Quinn (Test Architect)
**Full Report:** [`docs/qa/assessments/TEA-CLI-002-risk-20260201.md`](../qa/assessments/TEA-CLI-002-risk-20260201.md)

### Risk Level: MODERATE-HIGH (Score: 55/100)

### Identified Risks Summary

| Risk ID   | Description                           | Score | Priority |
|-----------|---------------------------------------|-------|----------|
| SEC-001   | Credential exposure in git auth       | 9     | Critical |
| SEC-002   | Path traversal in cache paths         | 6     | High     |
| TECH-001  | Cache manifest format incompatibility | 6     | High     |
| TECH-002  | object_store API version mismatch     | 4     | Medium   |
| TECH-003  | git2 ref resolution edge cases        | 4     | Medium   |
| DATA-001  | Cache corruption on concurrent access | 3     | Low      |

### Required Mitigations (Must Fix)

1. **SEC-001**: Never log tokens; implement token redaction in errors; use secure credential handling
2. **SEC-002**: Validate all paths within cache directory; canonicalize paths; reject traversal sequences
3. **TECH-001**: Implement manifest version checking; add Python-Rust interop tests

### Testing Priorities

1. **Critical**: Credential handling tests, log output scanning for token leakage
2. **High**: Path traversal vectors, Python-Rust cache round-trip tests
3. **Medium**: Cloud provider mocks, git ref resolution edge cases
4. **Low**: Concurrent cache access, large repo behavior

### Gate Recommendation

**CONCERNS** - Story may proceed to development with mandatory security mitigations tracked. Critical risk SEC-001 must be addressed in implementation with explicit security review before merge.

---

## QA Notes - NFR Assessment

**Assessment Date:** 2026-02-01
**Reviewer:** Quinn (Test Architect)
**Full Report:** [`docs/qa/assessments/TEA-CLI-002-nfr-20260201.md`](../qa/assessments/TEA-CLI-002-nfr-20260201.md)

### NFR Coverage Summary

| NFR | Status | Key Findings |
|-----|--------|--------------|
| Security | CONCERNS | Token redaction needed; path traversal protection required |
| Performance | CONCERNS | No target latency defined; cache size limits unspecified |
| Reliability | CONCERNS | Concurrent cache access risks; partial download recovery unclear |
| Maintainability | PASS | Trait-based design; clear module structure; offline testing |

**Overall NFR Status:** CONCERNS
**Quality Score:** 70/100

### Missing Considerations

1. **Credential Security:** No explicit requirement to redact tokens from error messages/logs (including `--verbose` mode)
2. **Path Validation:** Cache paths derived from URLs need canonicalization to prevent traversal attacks
3. **Performance Targets:** No defined latency requirements for cache hit/miss operations
4. **Concurrent Access:** Cache manifest lacks file locking specification for parallel runs
5. **Large Files:** No handling specified for files >100MB or large repo clones
6. **Network Timeouts:** Default timeouts undocumented; no configuration option

### Test Recommendations

**Critical Priority:**
- Token redaction verification tests (ensure no credential leakage in any output)
- Path traversal attack vector tests (malicious URL patterns)

**High Priority:**
- Cross-language cache round-trip tests (Python writes, Rust reads, and vice versa)
- Concurrent cache write tests (10+ parallel threads)
- Network error classification tests (distinguish timeout vs auth vs 404)

**Medium Priority:**
- Cache hit latency benchmarks (<100ms target suggested)
- Large file download handling (>100MB)
- Incomplete download recovery tests

### Recommended Acceptance Criteria Additions

**AC8: Security Hardening**
- [ ] Credential values MUST be redacted from all error messages and logs
- [ ] Cache paths MUST be canonicalized and validated within cache directory
- [ ] Cache directory MUST be created with 0700 permissions

**AC9: Performance Requirements**
- [ ] Cache hit latency MUST be under 100ms for local filesystem
- [ ] Document default network timeouts in CLI help

**AC10: Reliability Requirements**
- [ ] Cache manifest writes MUST be atomic (temp file + rename pattern)
- [ ] Interrupted downloads MUST NOT corrupt cache entries
- [ ] Implement retry with backoff for transient network errors

### Gate Integration

NFR validation block ready for gate file:

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Token redaction and path traversal protection needed'
  performance:
    status: CONCERNS
    notes: 'Target latency unknown; cache limits unspecified'
  reliability:
    status: CONCERNS
    notes: 'Concurrent access risks; partial download recovery unclear'
  maintainability:
    status: PASS
    notes: 'Trait-based mocking design; clear module structure'
```

---

## QA Notes - Test Design

**Design Date:** 2026-02-01
**Designer:** Quinn (Test Architect)
**Full Document:** [`docs/qa/assessments/TEA-CLI-002-test-design-20260201.md`](../qa/assessments/TEA-CLI-002-test-design-20260201.md)

### Test Coverage Matrix

| Acceptance Criteria | Unit | Integration | E2E | Total |
|---------------------|------|-------------|-----|-------|
| AC1: CLI URL Support | 3 | 4 | 1 | 8 |
| AC2: Git Protocol Support | 4 | 5 | 0 | 9 |
| AC3: Caching System | 4 | 9 | 1 | 14 |
| AC4: Cache Management CLI | 0 | 4 | 1 | 5 |
| AC5: Trait-Based Design | 4 | 1 | 0 | 5 |
| AC6: Error Handling | 2 | 1 | 1 | 4 |
| AC7: Documentation | 2 | 0 | 0 | 2 |
| Security (Risk Profile) | 1 | 4 | 1 | 6 |
| Cross-Language Parity | 1 | 3 | 2 | 6 |
| **TOTAL** | **21 (40%)** | **24 (46%)** | **7 (14%)** | **52** |

### Priority Distribution

| Priority | Count | Focus Areas |
|----------|-------|-------------|
| **P0 - Critical** | 18 | Security (path traversal, credential masking), cache key parity, core URL resolution, cross-language manifest compatibility |
| **P1 - High** | 20 | Git protocol variants, cache TTL, CLI commands, error messages, trait mocking |
| **P2 - Medium** | 11 | Azure integration, custom cache dir, selective clear, concurrent writes |
| **P3 - Low** | 3 | Documentation validation, offline test verification |

### Key Test Scenarios with Expected Results

#### Security Tests (P0 - Must Pass)

| Scenario | Input | Expected Result |
|----------|-------|-----------------|
| Path traversal blocked | `github://../../../etc/passwd` | Rejected before filesystem access |
| Cache path containment | Any URL resolving to `../outside` | Resolved path must be within `~/.cache/tea/remote/` |
| Credentials not in logs | `--verbose` with `GITHUB_TOKEN=secret` | Output shows `***` instead of token |
| Credentials not in errors | Auth failure with token set | Error message shows masked token |

#### Cross-Language Parity Tests (P0 - Must Pass)

| Scenario | Input | Expected Result |
|----------|-------|-----------------|
| Rust reads Python manifest | Python-generated `manifest.json` | Rust parses successfully, finds entries |
| Python reads Rust manifest | Rust-generated `manifest.json` | Python parses successfully, finds entries |
| Cache key identical | Same URL in both languages | Identical SHA256-based cache key |
| Mixed language workflow | Python caches `github://...` | Rust uses cached entry without refetch |

#### Core Functionality Tests (P0/P1)

| Scenario | Input | Expected Result |
|----------|-------|-----------------|
| Local path passthrough | `/home/user/workflow.yaml` | Returns unchanged `PathBuf` |
| S3 URL resolution | `s3://bucket/workflow.yaml` | Downloads via object_store mock, returns cached path |
| Cache hit | Previously fetched URL | Returns cached path, no network call |
| Cache miss | New URL | Fetches, caches, returns path |
| `--no-cache` | Any URL with cache | Forces fresh fetch, updates cache |
| `--cache-only` (uncached) | Unknown URL | Returns error with suggestion |
| Git branch fetch | `github://user/repo@main/file.yaml` | Clones via git2, checks out main, extracts file |
| Git SHA fetch | `github://user/repo@a1b2c3d/file.yaml` | Checks out specific commit |

### Test Data Requirements

| Fixture | Description | Setup Method |
|---------|-------------|--------------|
| `mock_remote_fs` | MockRemoteFileSystem with preconfigured responses | Rust test fixture |
| `wiremock_server` | HTTP mock server for cloud protocols | `wiremock` crate |
| `local_git_repo` | Bare git repo with branches/tags/commits | `tempfile` + `git2` |
| `cache_dir` | Isolated cache directory per test | `tempfile::tempdir()` |
| `python_manifest` | Pre-generated Python cache manifest | Static JSON fixture |
| `frozen_time` | Mock system time for TTL tests | Custom mock or `faketime` |

### Test Environment Configuration

```rust
// Required environment variables (set in test setup)
std::env::set_var("XDG_CACHE_HOME", temp_dir.path());
std::env::set_var("TEA_CACHE_TTL", "3600");

// For authentication tests
std::env::set_var("GITHUB_TOKEN", "ghp_test_token_redacted");
std::env::set_var("AWS_ACCESS_KEY_ID", "testing");
std::env::set_var("AWS_SECRET_ACCESS_KEY", "testing");
```

### Risk Mitigation Coverage

| Risk ID | Description | Test IDs |
|---------|-------------|----------|
| SEC-001 | Credential exposure | CLI-002-INT-026, INT-027, E2E-005 |
| SEC-002 | Path traversal | CLI-002-UNIT-020, INT-025 |
| TECH-001 | Cache manifest incompatibility | CLI-002-INT-028, INT-029, INT-030 |
| TECH-002 | object_store API version | CLI-002-INT-001, INT-002, INT-003 |
| TECH-003 | git2 ref resolution | CLI-002-UNIT-004/005/006, INT-005/006/007 |
| DATA-001 | Cache corruption | CLI-002-INT-018 |

### Gate Integration

```yaml
test_design:
  scenarios_total: 52
  by_level: { unit: 21, integration: 24, e2e: 7 }
  by_priority: { p0: 18, p1: 20, p2: 11, p3: 3 }
  coverage_gaps: []
  risk_coverage: [SEC-001, SEC-002, TECH-001, TECH-002, TECH-003, DATA-001]
  cross_language_tests: 6
```

---

## QA Notes - Requirements Trace

**Trace Date:** 2026-02-01
**Analyst:** Quinn (Test Architect)
**Full Report:** [`docs/qa/assessments/TEA-CLI-002-trace-20260201.md`](../qa/assessments/TEA-CLI-002-trace-20260201.md)

### Requirements Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Requirements** | 35 |
| **Fully Covered** | 30 (86%) |
| **Partially Covered** | 5 (14%) |
| **Not Covered** | 0 (0%) |

### Traceability Matrix (Acceptance Criteria)

| AC | Coverage | Test Count | Key Tests |
|----|----------|------------|-----------|
| AC1: CLI URL Support | FULL | 8 | UNIT-001/002, INT-001/002, E2E-001 |
| AC2: Git Protocol Support | FULL | 9 | UNIT-004/005/006/007, INT-005/006/007/008/009 |
| AC3: Caching System | FULL | 14 | UNIT-008/009/010/011, INT-010 through INT-018, E2E-002 |
| AC4: Cache Management CLI | FULL | 5 | INT-019/020/021/022, E2E-003 |
| AC5: Trait-Based Design | FULL | 5 | UNIT-012/013/014/015, INT-023 |
| AC6: Error Handling | FULL | 4 | UNIT-016/017, INT-024, E2E-004 |
| AC7: Documentation | PARTIAL | 2 | UNIT-018/019 (manual review needed) |

### Cross-Language Parity Traceability

| Requirement | Status | Tests |
|-------------|--------|-------|
| URL format identical | Covered | UNIT-004 |
| Cache key algorithm parity | Covered | UNIT-009, INT-030 |
| Manifest interop (Rust to Python) | Covered | INT-029 |
| Manifest interop (Python to Rust) | Covered | INT-028 |
| Error message parity | Covered | UNIT-016 |
| Flag behavior parity | Covered | INT-013/014/015/016 |

### Risk Mitigation Traceability

| Risk ID | Description | Status | Test IDs |
|---------|-------------|--------|----------|
| SEC-001 | Credential exposure | Covered | INT-026, INT-027, E2E-005 |
| SEC-002 | Path traversal | Covered | UNIT-020, INT-025 |
| TECH-001 | Manifest incompatibility | Covered | INT-028/029/030, UNIT-021 |
| TECH-002 | object_store API | Covered | INT-001/002/003 |
| TECH-003 | git2 ref resolution | Covered | UNIT-004/005/006, INT-005/006/007 |
| DATA-001 | Cache corruption | Covered | INT-018 |

### Gaps Identified

| Gap | Severity | Recommendation |
|-----|----------|----------------|
| No performance benchmark test | Medium | Add CLI-002-PERF-001: Cache hit latency under 100ms |
| No interrupted download test | Medium | Add test for partial download corruption prevention |
| No retry backoff test | Low | Add test for transient error retry behavior |
| No cache dir permission test | Low | Add test for 0700 directory permissions |

### Gate Integration

```yaml
trace:
  totals:
    requirements: 35
    full: 30
    partial: 5
    none: 0
  coverage_percentage: 86
  planning_ref: 'docs/qa/assessments/TEA-CLI-002-test-design-20260201.md'
  risk_coverage: [SEC-001, SEC-002, TECH-001, TECH-002, TECH-003, DATA-001]
  cross_language_tests: 6
  notes: 'docs/qa/assessments/TEA-CLI-002-trace-20260201.md'
```

### Trace Recommendation

**Status:** PASS

All acceptance criteria have mapped test coverage. Security risks are fully traced to P0 tests. Cross-language parity requirements have comprehensive test mappings. Minor NFR gaps can be addressed during development without blocking.

---

## SM Validation

**Validation Date:** 2026-02-01
**Validator:** Bob (Scrum Master)

### Definition of Ready Checklist

| Criterion | Status | Notes |
|-----------|--------|-------|
| Clear title and description | ✅ PASS | "URL-Based File Input for Rust CLI" with comprehensive user story |
| Acceptance criteria defined and testable | ✅ PASS | 7 ACs with checkboxes, mapped to 52 test scenarios |
| Dependencies identified | ✅ PASS | "Blocked By: TEA-CLI-001 (Python implementation)" |
| Technical approach documented | ✅ PASS | Full module structure, trait definitions, code examples |
| Story properly sized | ✅ PASS | 5 points, appropriate for scope |
| Risk Profile present | ✅ PASS | Score 55/100, 6 risks identified with mitigations |
| NFR Assessment present | ✅ PASS | Quality Score 70/100, security/performance/reliability reviewed |
| Test Design present | ✅ PASS | 52 scenarios (21 unit, 24 integration, 7 E2E) |
| Requirements Trace present | ✅ PASS | 86% full coverage, all risks traced to tests |
| No blocking issues or unknowns | ✅ PASS | Concerns documented with clear mitigations |

### Validation Summary

| Category | Status |
|----------|--------|
| Goal & Context Clarity | **PASS** |
| Technical Implementation Guidance | **PASS** |
| Reference Effectiveness | **PASS** |
| Self-Containment Assessment | **PASS** |
| Testing Guidance | **PASS** |

**Clarity Score:** 9/10

### Verdict

**✅ READY FOR DEVELOPMENT**

This story provides exceptional developer handoff documentation:
- Complete Rust code examples for key traits and structs
- Explicit cross-language parity requirements with Python CLI
- Comprehensive QA coverage with all 4 assessment types
- 52 test scenarios with expected results and priority levels
- Clear security mitigations for critical risks (SEC-001, SEC-002)

---

## Status

**Ready for Development**

*Validated by SM on 2026-02-01. All Definition of Ready criteria passed.*
