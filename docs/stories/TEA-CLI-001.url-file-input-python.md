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

---

## QA Notes - Risk Profile

**Analysis Date:** 2026-02-01
**Reviewer:** Quinn (Test Architect)
**Risk Score:** 42/100 (Medium-High Risk)

### Risk Summary

| Level | Count | Risk IDs |
|-------|-------|----------|
| Critical | 0 | - |
| High | 3 | SEC-001, SEC-002, PERF-001 |
| Medium | 4 | SEC-003, DATA-002, TECH-002 |
| Low | 4 | DATA-001, TECH-001, PERF-002, OPS-001, OPS-002 |

### Identified Risks

| Risk ID | Description | Score |
|---------|-------------|-------|
| **SEC-001** | Credential exposure in cache/logs - tokens may leak via verbose output or cache manifest | 6 |
| **SEC-002** | Path traversal via crafted URLs - malicious URLs could escape cache directory | 6 |
| **SEC-003** | SSRF via arbitrary URL fetch - could probe internal networks | 4 |
| **DATA-001** | Cache poisoning via manifest tampering | 3 |
| **DATA-002** | Concurrent cache writes cause corruption | 4 |
| **TECH-001** | fsspec backend import failures at runtime | 3 |
| **TECH-002** | Git ref resolution complexity (branch/tag/SHA disambiguation) | 4 |
| **PERF-001** | Large file downloads block CLI without feedback | 6 |
| **PERF-002** | Cache TTL eviction causes unexpected delays | 2 |
| **OPS-001** | Unclear error messages for authentication failures | 3 |
| **OPS-002** | Cache directory permissions on shared/multi-user systems | 2 |

### Required Mitigations (Must-Fix Before Merge)

1. **SEC-001/SEC-002**: Implement URL sanitization and path traversal prevention using `pathlib.Path.resolve()` with containment check. Mask all credentials in log output.
2. **PERF-001**: Add progress indicator for downloads >1MB and configurable timeout via `TEA_FETCH_TIMEOUT`.
3. **SEC-003**: Whitelist allowed protocols; reject file:// URLs pointing to localhost/internal IPs in production mode.

### Testing Priorities

1. **Priority 1 - Security (Must Pass)**
   - Path traversal attack vectors (`github://../../../etc/passwd`)
   - Credential masking verification in `--verbose` mode
   - SSRF prevention for internal IP ranges

2. **Priority 2 - Performance**
   - Large file download behavior (10MB+)
   - Concurrent cache access from parallel processes
   - Cache TTL boundary conditions

3. **Priority 3 - Functional**
   - All protocol variants work correctly
   - Git branch vs SHA vs tag resolution
   - Cache hit/miss/expiry scenarios
   - Graceful degradation when backends missing

### Gate Recommendation

**Preliminary Gate: CONCERNS** - Three high-risk items (SEC-001, SEC-002, PERF-001) require explicit mitigation before PASS can be granted. Implementation should prioritize security controls.

---

## QA Notes - NFR Assessment

**Assessment Date:** 2026-02-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (Core Four NFRs)
**Assessment File:** `docs/qa/assessments/TEA-CLI-001-nfr-20260201.md`

### NFR Coverage Summary

| NFR | Status | Coverage |
|-----|--------|----------|
| **Security** | CONCERNS | AC5 covers authentication; SEC-001/002/003 risks identified in Risk Profile but mitigations not yet in Technical Design |
| **Performance** | CONCERNS | Cache system well-designed (AC3); missing progress indicator and timeout configuration |
| **Reliability** | PASS | AC6 provides comprehensive error handling; cache-only fallback for offline mode |
| **Maintainability** | PASS | AC7 mandates 90% coverage with mocking strategy; AC8 requires documentation |

**Quality Score:** 80/100

### Missing Considerations

1. **Security - Not in Technical Design:**
   - Path traversal prevention code (use `pathlib.Path.resolve()` with containment check)
   - Credential masking before logging in `--verbose` mode
   - Protocol whitelist for SSRF protection

2. **Performance - Missing from AC5:**
   - `TEA_FETCH_TIMEOUT` environment variable for network timeouts
   - Progress indicator for large downloads (>1MB)

3. **Usability - Not Assessed:**
   - Error message clarity for multi-hop failures (e.g., git clone + file read)

### Test Recommendations

| Priority | Test Category | Required Tests |
|----------|--------------|----------------|
| **P1 - Security** | Path Traversal | `github://../../../etc/passwd`, `s3://bucket/../../etc/passwd` |
| **P1 - Security** | Credential Masking | Verify tokens not in `--verbose` output |
| **P1 - Security** | SSRF | Reject `file://localhost/etc/passwd`, `http://169.254.169.254/` |
| **P2 - Performance** | Timeout | Network hang handling with `TEA_FETCH_TIMEOUT` |
| **P2 - Performance** | Progress | Indicator displays for 10MB+ downloads |
| **P3 - Reliability** | Offline | `--cache-only` with cached vs uncached URLs |

### Acceptance Criteria Gaps

**Recommended additions to story:**

1. **AC1 Amendment:** Add "Progress indicator for downloads exceeding 1MB"
2. **AC5 Amendment:** Add `TEA_FETCH_TIMEOUT` (default: 30s) for network timeout control
3. **New AC9 - Security Controls:**
   - [ ] Path traversal prevention: All resolved paths must be within cache directory
   - [ ] Credential masking: URLs logged in verbose mode must have tokens redacted
   - [ ] Protocol whitelist: Configurable allowed protocols (default: `s3://`, `gs://`, `github://`, `gitlab://`, `https://`)

### Gate Integration

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Path traversal, credential masking, SSRF protection not in technical design'
  performance:
    status: CONCERNS
    notes: 'No progress indicator; no TEA_FETCH_TIMEOUT; targets unknown'
  reliability:
    status: PASS
    notes: 'Comprehensive error handling in AC6; graceful degradation'
  maintainability:
    status: PASS
    notes: '90% coverage target; mocking strategy; docs required'
```

### Verdict

**NFR Gate: CONCERNS** - Story is well-structured with good reliability and maintainability coverage, but security mitigations need to be added to Technical Design section and two performance gaps should be addressed before implementation begins.

---

## QA Notes - Test Design

**Design Date:** 2026-02-01
**Designer:** Quinn (Test Architect)
**Full Document:** `docs/qa/assessments/TEA-CLI-001-test-design-20260201.md`

### Test Coverage Matrix

| Acceptance Criteria | Unit | Integration | E2E | Total |
|---------------------|------|-------------|-----|-------|
| AC1: CLI URL Support | 3 | 4 | 1 | 8 |
| AC2: Git Protocol Support | 4 | 5 | 0 | 9 |
| AC3: Caching System | 5 | 9 | 1 | 15 |
| AC4: Cache Management CLI | 0 | 4 | 1 | 5 |
| AC5: Authentication | 1 | 3 | 1 | 5 |
| AC6: Error Handling | 2 | 1 | 1 | 4 |
| AC7: Test Infrastructure | 3 | 0 | 0 | 3 |
| Security (Risk Profile) | 1 | 2 | 2 | 5 |
| Performance (Risk Profile) | 0 | 0 | 1 | 1 |
| **TOTAL** | **19 (40%)** | **20 (43%)** | **8 (17%)** | **47** |

### Priority Distribution

| Priority | Count | Focus Areas |
|----------|-------|-------------|
| **P0 - Critical** | 15 | Security (path traversal, SSRF, credential masking), cache key algorithm, core URL resolution, authentication |
| **P1 - High** | 18 | Git protocol variants, cache TTL, CLI commands, error messages, offline mode |
| **P2 - Medium** | 11 | Azure integration, custom cache dir, selective clear, concurrent writes |
| **P3 - Low** | 3 | Test infrastructure meta-validation |

### Key Test Scenarios with Expected Results

#### Security Tests (P0 - Must Pass)

| Scenario | Input | Expected Result |
|----------|-------|-----------------|
| Path traversal blocked | `github://../../../etc/passwd` | Raise `SecurityError`, path rejected before filesystem access |
| SSRF localhost blocked | `file://localhost/etc/passwd` | Reject with "localhost URLs not permitted" |
| SSRF metadata blocked | `http://169.254.169.254/latest/meta-data` | Reject with "cloud metadata endpoints blocked" |
| Credentials masked | `--verbose` with `GITHUB_TOKEN=secret123` | Output shows `***` instead of token value |
| Cache path containment | Any URL resolving to `../outside` | Resolved path must be within `~/.cache/tea/remote/` |

#### Core Functionality Tests (P0/P1)

| Scenario | Input | Expected Result |
|----------|-------|-----------------|
| Local path passthrough | `/home/user/workflow.yaml` | Returns unchanged `Path` object |
| S3 URL resolution | `s3://bucket/workflow.yaml` | Downloads via moto, returns cached local path |
| Cache hit | Previously fetched URL | Returns cached path, no network call |
| Cache miss | New URL | Fetches, caches, returns path |
| `--no-cache` | Any URL with cache | Forces fresh fetch, updates cache |
| `--cache-only` (uncached) | Unknown URL | Raises `typer.BadParameter` with suggestion |
| `--cache-only` (cached) | Known URL | Returns cached path, no network |
| Git branch fetch | `github://user/repo@main/file.yaml` | Clones, checks out main, extracts file |
| Git SHA fetch | `github://user/repo@a1b2c3d/file.yaml` | Checks out specific commit |
| Missing backend | `s3://...` without s3fs installed | Error: "Install s3fs: pip install s3fs" |

### Test Data Requirements

| Fixture | Description | Setup |
|---------|-------------|-------|
| `moto_s3` | Mock S3 bucket with `test_workflow.yaml` | `@pytest.fixture` using moto library |
| `local_git_repo` | Bare git repo with branches, tags, commits | `tempfile` + `git init --bare` |
| `cache_dir` | Isolated cache directory per test | `pytest.tmp_path` fixture |
| `large_file` | 10MB binary for progress indicator test | Generated in fixture |
| `mock_time` | Frozen time for TTL expiry tests | `freezegun.freeze_time` |

### Test Environment Configuration

```python
# Required environment variables (set in conftest.py fixtures)
TEA_CACHE_TTL=3600          # 1 hour default
TEA_CACHE_MAX_SIZE=100MB    # Size limit
XDG_CACHE_HOME=/tmp/test    # Isolated cache location

# For auth tests
AWS_ACCESS_KEY_ID=testing
AWS_SECRET_ACCESS_KEY=testing
GITHUB_TOKEN=ghp_test123
```

### Execution Order

1. **P0 Unit** → Fail fast on algorithm bugs
2. **P0 Integration** → Verify component boundaries
3. **P0 E2E** → Validate security controls
4. **P1 in order** → Core functionality
5. **P2+ as time permits** → Secondary features

### Risk Mitigation Coverage

| Risk ID | Description | Test IDs |
|---------|-------------|----------|
| SEC-001 | Credential exposure | TEA-CLI-001-INT-028 |
| SEC-002 | Path traversal | TEA-CLI-001-UNIT-019, INT-027 |
| SEC-003 | SSRF vulnerability | TEA-CLI-001-E2E-006, E2E-007 |
| DATA-002 | Concurrent corruption | TEA-CLI-001-INT-018 |
| TECH-002 | Git ref complexity | TEA-CLI-001-UNIT-004/005/006 |
| PERF-001 | Large download UX | TEA-CLI-001-E2E-008 |

### Gate Integration

```yaml
test_design:
  scenarios_total: 47
  by_level: { unit: 19, integration: 20, e2e: 8 }
  by_priority: { p0: 15, p1: 18, p2: 11, p3: 3 }
  coverage_gaps: []
  risk_coverage: [SEC-001, SEC-002, SEC-003, DATA-001, DATA-002, TECH-001, TECH-002, PERF-001, OPS-001]
```

---

## QA Notes - Requirements Trace

**Trace Date:** 2026-02-01
**Traced By:** Quinn (Test Architect)
**Full Report:** `docs/qa/assessments/TEA-CLI-001-trace-20260201.md`

### Requirements Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Requirements** | 45 (8 ACs + 37 sub-requirements) |
| **Fully Covered** | 41 (91%) |
| **Partially Covered** | 4 (9%) |
| **Not Covered** | 0 (0%) |

### Traceability Matrix (Summary)

| Acceptance Criteria | Requirements | Tests | Coverage |
|---------------------|--------------|-------|----------|
| AC1: CLI URL Support | 5 | 8 | FULL |
| AC2: Git Protocol Support | 6 | 9 | FULL |
| AC3: Caching System | 11 | 14 | FULL |
| AC4: Cache Management CLI | 4 | 5 | FULL |
| AC5: Authentication | 5 | 5 | FULL |
| AC6: Error Handling | 4 | 4 | FULL |
| AC7: Test Infrastructure | 4 | 3 | PARTIAL |
| AC8: Documentation | 4 | 0 | PARTIAL (manual) |
| Security (Risk Profile) | 3 | 5 | FULL |
| Performance (Risk Profile) | 2 | 1 | PARTIAL |

### Identified Gaps

| Gap ID | Description | Severity | Action |
|--------|-------------|----------|--------|
| GAP-001 | AC7: No automated offline test verification | Low | Add CI check blocking real network calls |
| GAP-002 | AC8: Documentation requires manual review | Low | PR checklist item |
| GAP-003 | `TEA_FETCH_TIMEOUT` not in acceptance criteria | Medium | **Add to AC5** |
| GAP-004 | Progress indicator 1MB threshold not tested | Medium | Add boundary test |

### Key Test Mappings (Given-When-Then)

**Security (P0 - Must Pass):**
- **Path Traversal (SEC-002):** Given URL `github://../../../etc/passwd` → When resolution attempted → Then `SecurityError` raised before filesystem access
- **SSRF (SEC-003):** Given URL `http://169.254.169.254/` → When fetched → Then rejected with "cloud metadata endpoints blocked"
- **Credential Masking (SEC-001):** Given `GITHUB_TOKEN=secret` with `--verbose` → When logged → Then output shows `***` not token

**Core Functionality (P0/P1):**
- **Cache Hit:** Given URL in cache with valid TTL → When resolved → Then returns local path, no network call
- **Git Branch:** Given `github://user/repo@main/file.yaml` → When fetched → Then extracts from main branch HEAD
- **Local Passthrough:** Given local path `/home/user/file.yaml` → When resolved → Then returns unchanged `Path` object

### Recommendations

1. **Must-Fix:** Add `TEA_FETCH_TIMEOUT` environment variable to AC5 (identified in NFR Assessment, no test exists)
2. **Should-Fix:** Add progress indicator threshold test at 1MB boundary
3. **Should-Fix:** Add CI network isolation to verify offline test execution

### Gate Integration

```yaml
trace:
  totals:
    requirements: 45
    full: 41
    partial: 4
    none: 0
    coverage_pct: 91
  uncovered:
    - ac: AC5
      reason: 'TEA_FETCH_TIMEOUT env var missing from AC'
    - ac: Performance
      reason: 'Progress threshold boundary (1MB) not tested'
  notes: 'See docs/qa/assessments/TEA-CLI-001-trace-20260201.md'
```

### Verdict

**Requirements Trace: PASS with GAPS** - 91% coverage achieved. All security requirements fully traced. Two medium-severity gaps identified (GAP-003, GAP-004) recommend AC amendments before implementation begins.

---

## SM Validation

**Validation Date:** 2026-02-01
**Validated By:** Bob (Scrum Master)
**Mode:** YOLO

### Definition of Ready Checklist

| Criteria | Status | Notes |
|----------|--------|-------|
| Clear title and description | ✅ PASS | User story format with background context |
| Testable acceptance criteria | ✅ PASS | 8 ACs with checkbox items |
| Dependencies identified | ✅ PASS | Package and story dependencies documented |
| Technical approach documented | ✅ PASS | Code snippets, file list, cache structure |
| Properly sized | ✅ PASS | 5 points, scope bounded, out-of-scope defined |
| QA notes present | ✅ PASS | Risk Profile, NFR, Test Design, Trace all present |
| No blocking issues | ⚠️ CONCERNS | Security mitigations need Technical Design updates |

### Concerns Noted

1. **SEC-001/SEC-002 Mitigations:** Path traversal prevention and credential masking code not yet in Technical Design section (Risk Profile recommends, but code not shown)
2. **PERF-001 Mitigation:** Progress indicator for large downloads mentioned but not in code examples
3. **AC5 Gap:** `TEA_FETCH_TIMEOUT` environment variable recommended by NFR Assessment but not in acceptance criteria

### Recommendation

Story is **Ready for Development** with the following conditions:
- Developer MUST implement SEC-001/SEC-002 mitigations per Risk Profile before merge
- Developer SHOULD add `TEA_FETCH_TIMEOUT` to implementation per NFR Assessment
- PR review MUST verify security controls are in place

### Verdict

**SM Gate: PASS (Conditional)** - Story meets Definition of Ready. Documented QA concerns provide clear implementation guidance.

---

## Status

**Ready for Development**

Story has passed SM validation checklist. Implementation should prioritize security mitigations identified in QA Notes Risk Profile section (SEC-001, SEC-002, PERF-001). All acceptance criteria are testable and technical approach is well-documented.
