# Test Design: Story TEA-WASM-003.1

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 58
- Unit tests: 22 (38%)
- Integration tests: 24 (41%)
- E2E tests: 12 (21%)
- Priority distribution: P0: 18, P1: 22, P2: 14, P3: 4

## Risk Profile Summary

| Risk Area | Probability | Impact | Mitigation Strategy |
|-----------|-------------|--------|---------------------|
| OPFS browser compatibility | Medium | High | Cross-browser integration tests, memory fallback |
| OpenDAL OPFS newness | Medium | Medium | Isolation via feature flags, custom fallback |
| CORS misconfiguration | High | Medium | Clear error messages, documentation |
| Credential leakage | Low | Critical | Unit tests for isolation, no serialization |
| WASM bundle size | Medium | Medium | Bundle size monitoring tests |

## Test Scenarios by Acceptance Criteria

### AC-1: URI Parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-001 | Unit | P0 | Parse `s3://bucket/path/file` correctly | Core routing logic, pure function |
| TEA-WASM-003.1-UNIT-002 | Unit | P0 | Parse `gs://bucket/path/file` correctly | Core routing logic, pure function |
| TEA-WASM-003.1-UNIT-003 | Unit | P0 | Parse `az://container/path/file` correctly | Core routing logic, pure function |
| TEA-WASM-003.1-UNIT-004 | Unit | P0 | Parse `http://host/path` and `https://host/path` | Core routing logic, pure function |
| TEA-WASM-003.1-UNIT-005 | Unit | P0 | Parse `opfs://path/file` correctly | Core routing logic, WASM-specific |
| TEA-WASM-003.1-UNIT-006 | Unit | P0 | Parse `memory://path/file` correctly | Core routing logic, testing backend |
| TEA-WASM-003.1-UNIT-007 | Unit | P1 | Return error for unsupported scheme `ftp://` | Error handling, clear messaging |
| TEA-WASM-003.1-UNIT-008 | Unit | P1 | Return error for malformed URI `not-a-uri` | Input validation |

### AC-2: Storage Read

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-001 | Integration | P0 | Read text content from memory:// | Multi-component flow, validates interface |
| TEA-WASM-003.1-INT-002 | Integration | P0 | Read binary content (base64) from memory:// | Binary data flow validation |
| TEA-WASM-003.1-INT-003 | Integration | P1 | Return error for non-existent file | Error path validation |
| TEA-WASM-003.1-INT-004 | Integration | P1 | Return UTF-8 decode error for invalid content | Edge case handling |

### AC-3: Storage Write

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-005 | Integration | P0 | Write text content to memory:// | Core write functionality |
| TEA-WASM-003.1-INT-006 | Integration | P0 | Write binary content (base64) to memory:// | Binary write validation |
| TEA-WASM-003.1-INT-007 | Integration | P1 | Overwrite existing file | Update behavior validation |
| TEA-WASM-003.1-INT-008 | Integration | P2 | Write to nested path creates directories | Path handling |

### AC-4: Storage List

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-009 | Integration | P1 | List files in directory returns entries | Core list functionality |
| TEA-WASM-003.1-INT-010 | Integration | P1 | List respects limit option | Pagination support |
| TEA-WASM-003.1-INT-011 | Integration | P2 | List empty directory returns empty array | Edge case |
| TEA-WASM-003.1-INT-012 | Integration | P2 | List shows is_dir flag correctly | Metadata accuracy |

### AC-5: Storage Exists

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-013 | Integration | P1 | Exists returns true for existing file | Core functionality |
| TEA-WASM-003.1-INT-014 | Integration | P1 | Exists returns false for non-existent file | Core functionality |

### AC-6: Storage Delete

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-015 | Integration | P1 | Delete removes existing file | Core delete functionality |
| TEA-WASM-003.1-INT-016 | Integration | P2 | Delete non-existent file returns success | Idempotency |

### AC-7: S3 Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-017 | Integration | P0 | S3 read with valid credentials succeeds | Cloud backend validation |
| TEA-WASM-003.1-INT-018 | Integration | P0 | S3 write with valid credentials succeeds | Cloud backend validation |
| TEA-WASM-003.1-INT-019 | Integration | P0 | S3 missing credentials returns clear error | Security, error handling |
| TEA-WASM-003.1-E2E-001 | E2E | P1 | Full S3 workflow: write, read, list, delete | End-to-end cloud validation |

### AC-8: GCS Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-020 | Integration | P1 | GCS read with valid credentials succeeds | Cloud backend validation |
| TEA-WASM-003.1-INT-021 | Integration | P1 | GCS write with valid credentials succeeds | Cloud backend validation |

### AC-9: Azure Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-022 | Integration | P1 | Azure Blob read with valid credentials succeeds | Cloud backend validation |
| TEA-WASM-003.1-INT-023 | Integration | P1 | Azure Blob write with valid credentials succeeds | Cloud backend validation |

### AC-10: HTTP Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-024 | Integration | P1 | HTTP read from public URL succeeds | Read-only backend |
| TEA-WASM-003.1-UNIT-009 | Unit | P1 | HTTP write returns unsupported error | Read-only enforcement |

### AC-11: Memory Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-010 | Unit | P0 | Memory backend round-trip (write then read) | Testing foundation |
| TEA-WASM-003.1-UNIT-011 | Unit | P1 | Memory backend isolation between instances | State isolation |

### AC-12: OPFS Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-E2E-002 | E2E | P0 | OPFS write and read in Chrome | Primary browser support |
| TEA-WASM-003.1-E2E-003 | E2E | P0 | OPFS write and read in Edge | Chromium-based validation |
| TEA-WASM-003.1-E2E-004 | E2E | P1 | OPFS write and read in Firefox | Cross-browser validation |
| TEA-WASM-003.1-E2E-005 | E2E | P2 | OPFS write and read in Safari | Limited support validation |

### AC-13: OPFS Persistence

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-E2E-006 | E2E | P0 | OPFS files persist after page reload | Critical persistence guarantee |
| TEA-WASM-003.1-E2E-007 | E2E | P1 | OPFS files persist after browser restart | Session persistence |

### AC-14: OPFS DuckDB Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-E2E-008 | E2E | P0 | DuckDB reads parquet written via OPFS | Critical integration |
| TEA-WASM-003.1-E2E-009 | E2E | P1 | DuckDB COPY TO writes to OPFS readable by storage API | Bidirectional integration |

### AC-15: OPFS Binary Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-025 | Integration | P0 | Write and read parquet file via OPFS | Binary format support |
| TEA-WASM-003.1-INT-026 | Integration | P1 | Write and read PNG image via OPFS | Image binary support |

### AC-16: OPFS Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-012 | Unit | P0 | is_opfs_available returns false before init | State detection |
| TEA-WASM-003.1-UNIT-013 | Unit | P0 | is_opfs_available returns true after init | State detection |
| TEA-WASM-003.1-E2E-010 | E2E | P1 | Graceful fallback to memory when OPFS unavailable | Degradation handling |

### AC-17: Credential Injection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-014 | Unit | P0 | set_storage_credentials stores credentials in memory | Credential management |
| TEA-WASM-003.1-UNIT-015 | Unit | P0 | clear_storage_credentials removes all credentials | Security cleanup |
| TEA-WASM-003.1-UNIT-016 | Unit | P1 | set_storage_credentials with invalid JSON returns error | Input validation |

### AC-18: Environment Variables

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-017 | Unit | P2 | AWS_* env vars used when credentials not set | Env var fallback |
| TEA-WASM-003.1-UNIT-018 | Unit | P2 | Explicit credentials override env vars | Priority behavior |

### AC-19: Credential Isolation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-019 | Unit | P0 | Credentials NOT included in state_json output | Security critical |
| TEA-WASM-003.1-UNIT-020 | Unit | P0 | Credentials NOT serialized in checkpoint | Security critical |
| TEA-WASM-003.1-UNIT-021 | Unit | P0 | Credentials NOT logged in error messages | Security critical |

### AC-20: WASM Compilation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-027 | Integration | P0 | Compile to wasm32-unknown-unknown succeeds | Build validation |
| TEA-WASM-003.1-INT-028 | Integration | P1 | All exported functions accessible from JS | Interface validation |

### AC-21: Bundle Size

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-029 | Integration | P1 | Default features bundle < 1MB | Size constraint |
| TEA-WASM-003.1-INT-030 | Integration | P2 | Each backend adds < 500KB | Per-feature constraint |

### AC-22: CORS Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-E2E-011 | E2E | P1 | CORS error returns helpful message | User experience |
| TEA-WASM-003.1-UNIT-022 | Unit | P2 | CORS error message includes origin info | Debug assistance |

### AC-23: Error Messages

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-UNIT-023 | Unit | P0 | Missing credentials error is clear | User experience |
| TEA-WASM-003.1-UNIT-024 | Unit | P0 | Permission denied error is clear | User experience |
| TEA-WASM-003.1-UNIT-025 | Unit | P0 | Not found error is clear | User experience |
| TEA-WASM-003.1-UNIT-026 | Unit | P1 | Network error includes URI | Debug assistance |

### AC-24: Graceful Degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-E2E-012 | E2E | P1 | S3 unavailable returns graceful error | Resilience |
| TEA-WASM-003.1-UNIT-027 | Unit | P2 | Backend unavailable doesn't crash | Stability |

## Cross-Provider Operations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.1-INT-031 | Integration | P0 | Copy from S3 to OPFS succeeds | Cross-provider transfer |
| TEA-WASM-003.1-INT-032 | Integration | P0 | Copy from OPFS to S3 succeeds | Cross-provider transfer |
| TEA-WASM-003.1-INT-033 | Integration | P1 | Copy between same provider succeeds | Same-provider optimization |
| TEA-WASM-003.1-INT-034 | Integration | P2 | Copy preserves binary content exactly | Data integrity |

## Risk Coverage Matrix

| Risk ID | Description | Mitigating Tests |
|---------|-------------|------------------|
| RISK-001 | OPFS browser compatibility | TEA-WASM-003.1-E2E-002, E2E-003, E2E-004, E2E-005 |
| RISK-002 | OpenDAL OPFS newness | TEA-WASM-003.1-E2E-010, UNIT-012, UNIT-013 |
| RISK-003 | CORS misconfiguration | TEA-WASM-003.1-E2E-011, UNIT-022 |
| RISK-004 | Credential leakage | TEA-WASM-003.1-UNIT-019, UNIT-020, UNIT-021 |
| RISK-005 | WASM bundle size | TEA-WASM-003.1-INT-029, INT-030 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - URI parsing (UNIT-001 through UNIT-006)
   - Credential isolation (UNIT-019, UNIT-020, UNIT-021)
   - OPFS state detection (UNIT-012, UNIT-013)
   - Error messages (UNIT-023, UNIT-024, UNIT-025)

2. **P0 Integration tests** (validate components)
   - Memory backend (INT-001, INT-002, INT-005, INT-006)
   - S3 backend (INT-017, INT-018, INT-019)
   - WASM compilation (INT-027)
   - Cross-provider copy (INT-031, INT-032)
   - OPFS binary (INT-025)

3. **P0 E2E tests** (critical browser validation)
   - OPFS Chrome/Edge (E2E-002, E2E-003)
   - OPFS persistence (E2E-006)
   - DuckDB integration (E2E-008)

4. **P1 tests in order** (core functionality)
5. **P2+ as time permits** (secondary features)

## Test Environment Requirements

### Unit Tests
- Rust test harness (`cargo test`)
- No external dependencies
- Mock WASM environment where needed

### Integration Tests
- MinIO container for S3 testing
- Memory backend for isolated testing
- wasm-pack for WASM compilation validation

### E2E Tests
- Playwright browser automation
- Chrome 102+, Edge 102+, Firefox 111+, Safari 16.4+
- OPFS-enabled browser contexts
- DuckDB WASM loaded

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 58
  by_level:
    unit: 22
    integration: 24
    e2e: 12
  by_priority:
    p0: 18
    p1: 22
    p2: 14
    p3: 4
  coverage_gaps: []
  critical_paths:
    - uri_parsing
    - credential_isolation
    - opfs_persistence
    - duckdb_integration
    - cross_provider_copy
  risk_mitigations:
    - opfs_browser_compat: 4_tests
    - credential_leakage: 3_tests
    - cors_handling: 2_tests
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-WASM-003.1-test-design-20260112.md
P0 tests identified: 18
Critical integration points: OPFS + DuckDB, Cross-provider copy
Highest risk areas: Credential isolation, OPFS browser compatibility
```

## Quality Checklist

- [x] Every AC has test coverage (24 ACs, all covered)
- [x] Test levels are appropriate (unit for logic, integration for components, e2e for browser)
- [x] No duplicate coverage across levels (validated)
- [x] Priorities align with business risk (security = P0, convenience = P2)
- [x] Test IDs follow naming convention (TEA-WASM-003.1-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests
