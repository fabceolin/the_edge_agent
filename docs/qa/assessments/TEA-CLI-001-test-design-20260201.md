# Test Design: Story TEA-CLI-001

**Date:** 2026-02-01
**Designer:** Quinn (Test Architect)
**Story:** URL-Based File Input for Python CLI

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 47 |
| **Unit tests** | 19 (40%) |
| **Integration tests** | 20 (43%) |
| **E2E tests** | 8 (17%) |
| **Priority distribution** | P0: 15, P1: 18, P2: 11, P3: 3 |

### Strategy Rationale

This story involves security-sensitive URL handling, caching infrastructure, and CLI integration. The test strategy emphasizes:

1. **Security-first testing** - Path traversal, SSRF, credential exposure tests are P0
2. **Heavy unit testing** - Cache key generation, URL parsing are pure functions
3. **Integration focus** - fsspec backend integration, git protocol handling
4. **Minimal E2E** - Critical CLI workflows only (offline mocking)

---

## Test Scenarios by Acceptance Criteria

### AC1: CLI URL Support

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-001 | Unit | P0 | `_is_url()` correctly identifies URL vs local path | Pure classification logic | - |
| TEA-CLI-001-UNIT-002 | Unit | P0 | Local paths pass through unchanged | No transformation on non-URLs | - |
| TEA-CLI-001-UNIT-003 | Unit | P1 | Relative paths in remote YAML resolve to remote base | URL path resolution logic | - |
| TEA-CLI-001-INT-001 | Integration | P0 | `resolve_file_url()` returns local path for file:// URLs | fsspec local filesystem integration | - |
| TEA-CLI-001-INT-002 | Integration | P1 | S3 URL resolution via moto mock | s3fs backend integration | TECH-001 |
| TEA-CLI-001-INT-003 | Integration | P1 | GCS URL resolution via mock | gcsfs backend integration | TECH-001 |
| TEA-CLI-001-INT-004 | Integration | P2 | Azure URL resolution via mock | adlfs backend integration | TECH-001 |
| TEA-CLI-001-E2E-001 | E2E | P1 | `tea run s3://bucket/workflow.yaml` executes workflow | Full CLI path with mocked S3 | - |

### AC2: Git Protocol Support

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-004 | Unit | P0 | Parse `github://user/repo@branch/path` correctly | URL parsing logic | TECH-002 |
| TEA-CLI-001-UNIT-005 | Unit | P0 | Parse `github://user/repo@a1b2c3d/path` (SHA) | Commit SHA parsing | TECH-002 |
| TEA-CLI-001-UNIT-006 | Unit | P1 | Parse `github://user/repo@v1.0.0/path` (tag) | Tag parsing | TECH-002 |
| TEA-CLI-001-UNIT-007 | Unit | P1 | Parse `gitlab://user/repo@branch/path` | GitLab variant parsing | - |
| TEA-CLI-001-INT-005 | Integration | P0 | Fetch file from local bare git repo (branch) | Git checkout integration | - |
| TEA-CLI-001-INT-006 | Integration | P1 | Fetch file from local git repo (specific SHA) | SHA resolution | - |
| TEA-CLI-001-INT-007 | Integration | P1 | Fetch file from local git repo (tag) | Tag resolution | - |
| TEA-CLI-001-INT-008 | Integration | P0 | Private repo auth with GITHUB_TOKEN env var | Token authentication | - |
| TEA-CLI-001-INT-009 | Integration | P2 | Private repo auth with GIT_TOKEN fallback | Generic token fallback | - |

### AC3: Caching System

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-008 | Unit | P0 | Cache key generation: SHA256(URL)[:16] | Algorithm correctness | DATA-001 |
| TEA-CLI-001-UNIT-009 | Unit | P0 | Git cache key includes ref: SHA256(repo@ref)[:16] | Git-specific key generation | DATA-001 |
| TEA-CLI-001-UNIT-010 | Unit | P1 | Default cache dir is XDG compliant | XDG_CACHE_HOME handling | - |
| TEA-CLI-001-UNIT-011 | Unit | P1 | TTL expiry calculation for branches | Time-based validity | - |
| TEA-CLI-001-UNIT-012 | Unit | P1 | SHA/tags cached indefinitely (no TTL) | Immutable ref handling | - |
| TEA-CLI-001-INT-010 | Integration | P0 | Cache hit returns local file without network | Cache retrieval path | - |
| TEA-CLI-001-INT-011 | Integration | P1 | Cache miss fetches and stores file | Cache population path | - |
| TEA-CLI-001-INT-012 | Integration | P1 | `--no-cache` bypasses cache, fetches fresh | Force fetch behavior | - |
| TEA-CLI-001-INT-013 | Integration | P1 | `--cache-only` fails if not cached | Offline mode enforcement | - |
| TEA-CLI-001-INT-014 | Integration | P1 | `--cache-only` succeeds if cached | Offline mode success | - |
| TEA-CLI-001-INT-015 | Integration | P2 | `--cache-dir <path>` overrides default location | Custom cache location | - |
| TEA-CLI-001-INT-016 | Integration | P1 | TEA_CACHE_TTL env var configures expiry | Environment config | - |
| TEA-CLI-001-INT-017 | Integration | P2 | TEA_CACHE_MAX_SIZE triggers eviction | Size-based eviction | - |
| TEA-CLI-001-INT-018 | Integration | P2 | Concurrent cache writes don't corrupt manifest | File locking behavior | DATA-002 |
| TEA-CLI-001-E2E-002 | E2E | P1 | `--verbose` shows cache HIT/MISS messages | Debug output verification | - |

### AC4: Cache Management CLI

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-INT-019 | Integration | P1 | `tea cache list` shows cached entries | CLI command integration | - |
| TEA-CLI-001-INT-020 | Integration | P1 | `tea cache clear` removes all entries | Full cache clear | - |
| TEA-CLI-001-INT-021 | Integration | P2 | `tea cache clear --older-than 7d` filters by age | Selective clear | - |
| TEA-CLI-001-INT-022 | Integration | P2 | `tea cache info` shows size/count stats | Statistics output | - |
| TEA-CLI-001-E2E-003 | E2E | P2 | Cache commands output is human-readable | UX validation | - |

### AC5: Authentication via Environment Variables

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-013 | Unit | P1 | Credential loading respects AWS env var precedence | Config logic | - |
| TEA-CLI-001-INT-023 | Integration | P0 | S3 auth with AWS_ACCESS_KEY_ID + AWS_SECRET_ACCESS_KEY | AWS credential flow | - |
| TEA-CLI-001-INT-024 | Integration | P2 | GCS auth with GOOGLE_APPLICATION_CREDENTIALS | GCP credential flow | - |
| TEA-CLI-001-INT-025 | Integration | P2 | Azure auth with AZURE_STORAGE_ACCOUNT_* vars | Azure credential flow | - |
| TEA-CLI-001-E2E-004 | E2E | P1 | Missing credentials produce clear error message | UX for auth failures | OPS-001 |

### AC6: Error Handling

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-014 | Unit | P1 | Error message suggests `pip install s3fs` for missing backend | Helpful error text | TECH-001 |
| TEA-CLI-001-UNIT-015 | Unit | P1 | Invalid URL format error shows expected format | User guidance | - |
| TEA-CLI-001-INT-026 | Integration | P0 | Network failure suggests `--cache-only` if cached version exists | Graceful degradation | - |
| TEA-CLI-001-E2E-005 | E2E | P1 | Full error flow: missing package → install suggestion | End-to-end UX | - |

### AC7: Test Infrastructure (Meta-tests)

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-016 | Unit | P3 | Verify moto fixtures available for S3 tests | Test infra validation | - |
| TEA-CLI-001-UNIT-017 | Unit | P3 | Verify local git repo fixture works | Test infra validation | - |
| TEA-CLI-001-UNIT-018 | Unit | P3 | Coverage report shows ≥90% on cache.py | Coverage gate | - |

### Security Tests (From Risk Profile)

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-UNIT-019 | Unit | P0 | Path traversal blocked: `github://../../../etc/passwd` | Security: path escape | SEC-002 |
| TEA-CLI-001-INT-027 | Integration | P0 | Resolved paths contained within cache directory | Containment verification | SEC-002 |
| TEA-CLI-001-INT-028 | Integration | P0 | Credentials masked in `--verbose` output | Token leak prevention | SEC-001 |
| TEA-CLI-001-E2E-006 | E2E | P0 | SSRF blocked: `file://localhost/etc/passwd` rejected | Internal network protection | SEC-003 |
| TEA-CLI-001-E2E-007 | E2E | P0 | SSRF blocked: `http://169.254.169.254/` metadata rejected | Cloud metadata protection | SEC-003 |

### Performance Tests (From Risk Profile)

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TEA-CLI-001-E2E-008 | E2E | P1 | Large file (10MB) download shows progress indicator | UX for long operations | PERF-001 |

---

## Risk Coverage Matrix

| Risk ID | Description | Test Coverage |
|---------|-------------|---------------|
| SEC-001 | Credential exposure | TEA-CLI-001-INT-028 |
| SEC-002 | Path traversal | TEA-CLI-001-UNIT-019, TEA-CLI-001-INT-027 |
| SEC-003 | SSRF vulnerability | TEA-CLI-001-E2E-006, TEA-CLI-001-E2E-007 |
| DATA-001 | Cache poisoning | TEA-CLI-001-UNIT-008, TEA-CLI-001-UNIT-009 |
| DATA-002 | Concurrent write corruption | TEA-CLI-001-INT-018 |
| TECH-001 | Backend import failures | TEA-CLI-001-INT-002/003/004, TEA-CLI-001-UNIT-014 |
| TECH-002 | Git ref complexity | TEA-CLI-001-UNIT-004/005/006 |
| PERF-001 | Large download blocks | TEA-CLI-001-E2E-008 |
| OPS-001 | Unclear auth errors | TEA-CLI-001-E2E-004 |

---

## Test Environment Requirements

### Required Test Fixtures

| Fixture | Purpose | Implementation |
|---------|---------|----------------|
| `moto_s3` | Mock S3 bucket with test files | `@pytest.fixture` with moto |
| `mock_gcs` | Mock GCS bucket | `responses` library for HTTP |
| `local_git_repo` | Bare git repo with branches/tags | `tempfile` + `subprocess` |
| `cache_dir` | Isolated cache directory per test | `tmp_path` fixture |
| `mock_time` | Control TTL expiry | `freezegun` or `unittest.mock` |

### Test Data Requirements

| Data Item | Purpose | Location |
|-----------|---------|----------|
| `test_workflow.yaml` | Valid workflow for URL tests | `tests/fixtures/test_workflow.yaml` |
| `large_file.bin` | 10MB file for progress tests | Generated in fixture |
| `private_repo_token` | Mock token for auth tests | `GIT_TOKEN` env var in fixture |

### Environment Variables for Tests

```python
# conftest.py
@pytest.fixture
def env_vars(monkeypatch):
    monkeypatch.setenv("TEA_CACHE_TTL", "3600")
    monkeypatch.setenv("TEA_CACHE_MAX_SIZE", "100MB")
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path / ".cache"))
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - `test_cache_key_generation`
   - `test_url_parsing`
   - `test_path_traversal_blocked`
   - `test_local_path_passthrough`

2. **P0 Integration tests** (verify component integration)
   - `test_cache_hit_no_network`
   - `test_s3_auth_with_credentials`
   - `test_git_branch_fetch`
   - `test_credentials_masked_verbose`
   - `test_paths_contained_in_cache`

3. **P0 E2E tests** (critical security paths)
   - `test_ssrf_localhost_blocked`
   - `test_ssrf_metadata_blocked`

4. **P1 tests** (core functionality)
   - All cache operations
   - Git protocol variants
   - Error handling
   - CLI commands

5. **P2+ tests** (secondary features, as time permits)
   - Azure integration
   - Cache management CLI
   - Selective clear

---

## Gate YAML Block

```yaml
test_design:
  date: '2026-02-01'
  designer: 'Quinn (Test Architect)'
  scenarios_total: 47
  by_level:
    unit: 19
    integration: 20
    e2e: 8
  by_priority:
    p0: 15
    p1: 18
    p2: 11
    p3: 3
  risk_coverage:
    covered: [SEC-001, SEC-002, SEC-003, DATA-001, DATA-002, TECH-001, TECH-002, PERF-001, OPS-001]
    uncovered: [PERF-002, OPS-002]  # Low risk, accepted
  coverage_gaps: []
  ac_coverage:
    AC1: 8 tests
    AC2: 9 tests
    AC3: 14 tests
    AC4: 5 tests
    AC5: 5 tests
    AC6: 4 tests
    AC7: 3 tests
    Security: 5 tests
    Performance: 1 test
```

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-CLI-001-test-design-20260201.md`
P0 tests identified: 15
Security tests: 5
All ACs covered: Yes

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations from Risk Profile addressed
- [x] Security tests are P0
