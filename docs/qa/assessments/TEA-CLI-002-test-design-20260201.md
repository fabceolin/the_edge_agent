# Test Design: Story TEA-CLI-002

**Story:** TEA-CLI-002 - URL-Based File Input for Rust CLI
**Date:** 2026-02-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 52 |
| **Unit tests** | 21 (40%) |
| **Integration tests** | 24 (46%) |
| **E2E tests** | 7 (14%) |
| **Priority distribution** | P0: 18, P1: 20, P2: 11, P3: 3 |

### Strategy Rationale

This test design mirrors TEA-CLI-001 (Python) to ensure behavioral parity while addressing Rust-specific concerns:

1. **Trait-based mocking** - Leverages `RemoteFileSystem` trait for offline testing
2. **Cross-language cache compatibility** - Critical tests for Python-Rust manifest interoperability
3. **Security-first** - Path traversal and credential leakage tests are P0
4. **wiremock over moto** - Uses Rust HTTP mocking for cloud protocol tests

---

## Test Scenarios by Acceptance Criteria

### AC1: CLI URL Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-001 | Unit | P0 | `test_local_path_passthrough` - Verify local paths return unchanged `PathBuf` | Core logic, pure function |
| CLI-002-UNIT-002 | Unit | P0 | `test_url_protocol_detection` - Correctly identify s3://, gs://, github://, file:// | Pure parsing logic |
| CLI-002-UNIT-003 | Unit | P1 | `test_relative_path_handling` - Relative paths resolve against CWD | Edge case for passthrough |
| CLI-002-INT-001 | Integration | P0 | `test_s3_url_resolution` - S3 URL downloads via object_store mock | Component integration |
| CLI-002-INT-002 | Integration | P0 | `test_gcs_url_resolution` - GCS URL downloads via object_store mock | Component integration |
| CLI-002-INT-003 | Integration | P1 | `test_azure_url_resolution` - Azure blob URL handling | Component integration |
| CLI-002-INT-004 | Integration | P1 | `test_https_url_resolution` - HTTP/HTTPS file fetch | Component integration |
| CLI-002-E2E-001 | E2E | P1 | `test_cli_run_with_remote_yaml` - Full `tea run s3://...` execution | User journey validation |

### AC2: Git Protocol Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-004 | Unit | P0 | `test_github_url_parse_branch` - Parse `github://user/repo@main/path` | Complex parsing logic |
| CLI-002-UNIT-005 | Unit | P0 | `test_github_url_parse_sha` - Parse `github://user/repo@a1b2c3d/path` | SHA vs branch detection |
| CLI-002-UNIT-006 | Unit | P0 | `test_github_url_parse_tag` - Parse `github://user/repo@v1.0.0/path` | Tag pattern recognition |
| CLI-002-UNIT-007 | Unit | P1 | `test_gitlab_url_parse` - Parse `gitlab://user/repo@ref/path` | Alternate git host |
| CLI-002-INT-005 | Integration | P0 | `test_github_branch_fetch` - Clone and checkout branch via local git repo | Git2 integration |
| CLI-002-INT-006 | Integration | P0 | `test_github_sha_fetch` - Checkout specific commit | Git2 ref resolution |
| CLI-002-INT-007 | Integration | P1 | `test_github_tag_fetch` - Checkout tag | Git2 tag handling |
| CLI-002-INT-008 | Integration | P1 | `test_private_repo_auth` - GITHUB_TOKEN authentication | Credential flow |
| CLI-002-INT-009 | Integration | P1 | `test_gitlab_private_auth` - GITLAB_TOKEN authentication | Alternate token |

### AC3: Caching System

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-008 | Unit | P0 | `test_cache_key_sha256` - Cache key uses SHA256 of URL | Algorithm correctness |
| CLI-002-UNIT-009 | Unit | P0 | `test_cache_key_parity_with_python` - Same URL produces same key as Python | Cross-language parity |
| CLI-002-UNIT-010 | Unit | P1 | `test_cache_ttl_calculation` - TTL expiry logic | Time-based logic |
| CLI-002-UNIT-011 | Unit | P1 | `test_permanent_cache_for_sha_tags` - SHA/tags never expire | Business rule |
| CLI-002-INT-010 | Integration | P0 | `test_cache_hit` - Cached URL returns local path without network | Core cache behavior |
| CLI-002-INT-011 | Integration | P0 | `test_cache_miss` - Uncached URL triggers fetch | Core cache behavior |
| CLI-002-INT-012 | Integration | P1 | `test_cache_ttl_expiry` - Expired entry triggers refetch | TTL enforcement |
| CLI-002-INT-013 | Integration | P1 | `test_no_cache_flag` - `--no-cache` forces fresh fetch | Flag behavior |
| CLI-002-INT-014 | Integration | P1 | `test_cache_only_flag_hit` - `--cache-only` with cached entry succeeds | Flag behavior |
| CLI-002-INT-015 | Integration | P1 | `test_cache_only_flag_miss` - `--cache-only` without cache fails | Flag behavior |
| CLI-002-INT-016 | Integration | P2 | `test_custom_cache_dir` - `--cache-dir` overrides default | Flag behavior |
| CLI-002-INT-017 | Integration | P2 | `test_verbose_cache_logging` - `--verbose` shows cache hit/miss | Debug output |
| CLI-002-INT-018 | Integration | P2 | `test_concurrent_cache_writes` - Parallel runs don't corrupt manifest | Concurrency safety |
| CLI-002-E2E-002 | E2E | P1 | `test_cache_persists_across_runs` - Cached file available in subsequent run | User journey |

### AC4: Cache Management CLI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-INT-019 | Integration | P1 | `test_cache_list_command` - `tea cache list` shows entries | CLI subcommand |
| CLI-002-INT-020 | Integration | P1 | `test_cache_clear_command` - `tea cache clear` removes all | CLI subcommand |
| CLI-002-INT-021 | Integration | P2 | `test_cache_clear_older_than` - `tea cache clear --older-than 7d` | Selective clear |
| CLI-002-INT-022 | Integration | P2 | `test_cache_info_command` - `tea cache info` shows stats | CLI subcommand |
| CLI-002-E2E-003 | E2E | P2 | `test_cache_workflow` - Full list→clear→verify empty | User journey |

### AC5: Trait-Based Design for Testing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-012 | Unit | P1 | `test_mock_filesystem_fetch` - MockRemoteFileSystem returns configured paths | Trait implementation |
| CLI-002-UNIT-013 | Unit | P1 | `test_mock_supports_protocol` - Protocol support check | Trait method |
| CLI-002-UNIT-014 | Unit | P2 | `test_mock_fetch_no_cache` - Force refresh method | Trait method |
| CLI-002-INT-023 | Integration | P1 | `test_wiremock_http_integration` - HTTP mock with wiremock | Integration test infrastructure |
| CLI-002-UNIT-015 | Unit | P3 | `test_all_tests_run_offline` - Meta: verify no real network calls | Test infrastructure validation |

### AC6: Error Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-016 | Unit | P1 | `test_error_message_format` - Errors match Python format | Cross-language parity |
| CLI-002-INT-024 | Integration | P1 | `test_network_failure_suggests_cache_only` - Shows cache-only hint | UX improvement |
| CLI-002-UNIT-017 | Unit | P1 | `test_credential_error_message` - Clear auth failure message | Error clarity |
| CLI-002-E2E-004 | E2E | P2 | `test_missing_file_error_flow` - 404 produces actionable error | User journey |

### AC7: Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-UNIT-018 | Unit | P3 | `test_help_text_contains_url_examples` - Verify --help mentions URLs | Documentation validation |
| CLI-002-UNIT-019 | Unit | P3 | `test_rustdoc_compiles` - Inline doc examples compile | Documentation quality |

---

## Security Test Scenarios (From Risk Profile)

| ID | Level | Priority | Test | Risk ID | Justification |
|----|-------|----------|------|---------|---------------|
| CLI-002-UNIT-020 | Unit | P0 | `test_path_traversal_blocked` - `../` sequences rejected | SEC-002 | Path escape prevention |
| CLI-002-INT-025 | Integration | P0 | `test_cache_path_containment` - All resolved paths within cache dir | SEC-002 | Defense in depth |
| CLI-002-INT-026 | Integration | P0 | `test_credential_not_in_logs` - Token values never logged | SEC-001 | Credential protection |
| CLI-002-INT-027 | Integration | P0 | `test_credential_not_in_errors` - Token redacted from error messages | SEC-001 | Credential protection |
| CLI-002-E2E-005 | E2E | P0 | `test_verbose_masks_credentials` - `--verbose` output safe | SEC-001 | User-facing security |

---

## Cross-Language Parity Tests (Critical)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| CLI-002-INT-028 | Integration | P0 | `test_rust_reads_python_manifest` - Rust can read Python-written manifest.json | Cross-language interop |
| CLI-002-INT-029 | Integration | P0 | `test_python_reads_rust_manifest` - Python can read Rust-written manifest.json | Cross-language interop |
| CLI-002-INT-030 | Integration | P0 | `test_cache_key_identical` - Same URL → same key in both languages | Algorithm parity |
| CLI-002-UNIT-021 | Unit | P1 | `test_manifest_version_check` - Rust validates manifest version field | Format compatibility |
| CLI-002-E2E-006 | E2E | P1 | `test_mixed_language_cache_workflow` - Python caches, Rust uses | Full interop validation |
| CLI-002-E2E-007 | E2E | P1 | `test_rust_caches_python_uses` - Rust caches, Python uses | Bidirectional interop |

---

## Risk Coverage Matrix

| Risk ID | Description | Test Coverage |
|---------|-------------|---------------|
| SEC-001 | Credential exposure | CLI-002-INT-026, INT-027, E2E-005 |
| SEC-002 | Path traversal | CLI-002-UNIT-020, INT-025 |
| TECH-001 | Cache manifest incompatibility | CLI-002-INT-028, INT-029, INT-030 |
| TECH-002 | object_store API version | CLI-002-INT-001, INT-002, INT-003 |
| TECH-003 | git2 ref resolution | CLI-002-UNIT-004/005/006, INT-005/006/007 |
| DATA-001 | Cache corruption | CLI-002-INT-018 |

---

## Test Data Requirements

| Fixture | Description | Setup Method |
|---------|-------------|--------------|
| `mock_remote_fs` | MockRemoteFileSystem with preconfigured responses | Rust fixture |
| `wiremock_server` | HTTP mock server for cloud protocols | wiremock crate |
| `local_git_repo` | Bare git repo with branches/tags/commits | tempfile + git2 |
| `cache_dir` | Isolated cache directory per test | tempfile::tempdir() |
| `python_manifest` | Pre-generated Python cache manifest | Static JSON fixture |
| `rust_manifest` | Generated Rust manifest for Python validation | Test output |
| `frozen_time` | Mock system time for TTL tests | faketime or mock |

---

## Test Environment Configuration

```rust
// Required environment variables (set in test setup)
std::env::set_var("XDG_CACHE_HOME", temp_dir.path());
std::env::set_var("TEA_CACHE_TTL", "3600");

// For authentication tests
std::env::set_var("GITHUB_TOKEN", "ghp_test_token_redacted");
std::env::set_var("AWS_ACCESS_KEY_ID", "testing");
std::env::set_var("AWS_SECRET_ACCESS_KEY", "testing");
```

---

## Recommended Execution Order

1. **P0 Unit tests** (UNIT-001 to UNIT-021) - Fail fast on algorithm bugs
2. **P0 Integration tests** (INT-001, INT-005/006, INT-010/011, INT-025-030) - Verify critical paths
3. **P0 E2E tests** (E2E-005) - Validate security controls
4. **P1 tests in order** - Core functionality
5. **P2+ tests as time permits** - Secondary features

---

## Gate Integration Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 21
    integration: 24
    e2e: 7
  by_priority:
    p0: 18
    p1: 20
    p2: 11
    p3: 3
  coverage_gaps: []
  risk_coverage:
    - SEC-001
    - SEC-002
    - TECH-001
    - TECH-002
    - TECH-003
    - DATA-001
  cross_language_tests: 6
  parity_with_python: true
```

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-CLI-002-test-design-20260201.md`
P0 tests identified: 18
Cross-language parity tests: 6
Risk mitigations covered: SEC-001, SEC-002, TECH-001, TECH-002, TECH-003, DATA-001

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components)
- [x] No duplicate coverage across levels
- [x] Priorities align with risk profile (security tests are P0)
- [x] Test IDs follow naming convention (CLI-002-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Cross-language parity tests included
- [x] Security tests cover identified risks (SEC-001, SEC-002)
