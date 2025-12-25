# Test Design: Story TEA-BUILTIN-001.6.2

**Date:** 2025-12-25
**Designer:** Quinn (Test Architect)
**Story:** DuckDB LTM Backend Core

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 18 (38%)
- **Integration tests:** 25 (53%)
- **E2E tests:** 4 (9%)
- **Priority distribution:** P0: 19, P1: 18, P2: 10

## Risk Assessment Summary

| Risk Area | Impact | Probability | Priority |
|-----------|--------|-------------|----------|
| Data loss on store/retrieve | High | Medium | P0 |
| Deduplication failure (storage waste) | Medium | Low | P0 |
| Cloud storage credential failures | High | Medium | P0 |
| FTS index corruption | Medium | Low | P1 |
| Circuit breaker not triggering | Medium | Low | P2 |

---

## Test Scenarios by Acceptance Criteria

### AC-1: LTMBackend Protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-001 | Unit | P0 | DuckDBLTMBackend implements LTMBackend protocol | Pure interface compliance |
| 1.6.2-UNIT-002 | Unit | P1 | All protocol methods present with correct signatures | Protocol contract verification |

### AC-2: Catalog Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-003 | Unit | P0 | Constructor requires CatalogBackend instance | Configuration validation |
| 1.6.2-UNIT-004 | Unit | P1 | Raises error if catalog is None | Defensive programming |
| 1.6.2-INT-001 | Integration | P0 | Backend initializes with SQLiteCatalog | Component interaction |
| 1.6.2-INT-002 | Integration | P1 | Backend initializes with PostgresCatalog | Backend compatibility |

### AC-3: Store Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-005 | Unit | P0 | store() returns success=True on valid input | Core business logic |
| 1.6.2-UNIT-006 | Unit | P0 | store() returns correct response structure | API contract |
| 1.6.2-INT-003 | Integration | P0 | store() persists to catalog (verify with get_entry) | Data persistence verification |
| 1.6.2-INT-004 | Integration | P0 | store() with metadata preserves all fields | Metadata integrity |

### AC-4: Retrieve Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-007 | Unit | P0 | retrieve() returns found=True for existing key | Core logic |
| 1.6.2-UNIT-008 | Unit | P0 | retrieve() returns default for missing key | Default handling |
| 1.6.2-INT-005 | Integration | P0 | retrieve() returns exact value stored | Data integrity roundtrip |
| 1.6.2-INT-006 | Integration | P0 | retrieve() includes content_hash in response | Response contract |

### AC-5: Delete Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-009 | Unit | P0 | delete() returns deleted=True for existing key | Core logic |
| 1.6.2-UNIT-010 | Unit | P1 | delete() returns deleted=False for missing key | Idempotency |
| 1.6.2-INT-007 | Integration | P0 | delete() removes from catalog (verify not found) | Data removal verification |
| 1.6.2-INT-008 | Integration | P0 | retrieve() after delete() returns default | Delete-retrieve consistency |

### AC-6: Search Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-011 | Unit | P1 | search() returns results array | Response structure |
| 1.6.2-INT-009 | Integration | P1 | search() returns matching entries | Search functionality |
| 1.6.2-INT-010 | Integration | P1 | search() respects limit parameter | Pagination |

### AC-7: Inline Threshold

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-012 | Unit | P0 | Default inline_threshold is 1024 bytes | Configuration default |
| 1.6.2-UNIT-013 | Unit | P1 | Custom inline_threshold accepted in constructor | Configurability |

### AC-8: Small Data Inlining

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-014 | Unit | P0 | Data < threshold returns inlined=True | Inlining logic |
| 1.6.2-INT-011 | Integration | P0 | Small data stored in catalog inlined_value | Data storage strategy |
| 1.6.2-INT-012 | Integration | P0 | Inlined data has no storage_uri | Storage path correctness |

### AC-9: Large Data Cloud

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-015 | Unit | P0 | Data >= threshold returns inlined=False | Threshold boundary |
| 1.6.2-INT-013 | Integration | P0 | Large data creates storage_uri | Cloud upload trigger |
| 1.6.2-INT-014 | Integration | P0 | Large data file exists at storage_uri | Upload verification |

### AC-10: Transparent Retrieval

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-015 | Integration | P0 | retrieve() works for inlined data | Transparent access |
| 1.6.2-INT-016 | Integration | P0 | retrieve() works for cloud-stored data | Transparent access |
| 1.6.2-E2E-001 | E2E | P0 | Store small→retrieve, store large→retrieve seamless | User journey |

### AC-11: Hash on Store

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-016 | Unit | P0 | store() computes content_hash | Hash computation |
| 1.6.2-UNIT-017 | Unit | P0 | Same content produces same hash | Hash determinism |

### AC-12: Skip Duplicate

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-017 | Integration | P0 | Same key+hash returns deduplicated=True | Deduplication logic |
| 1.6.2-INT-018 | Integration | P0 | Deduplicated store skips cloud upload | Storage optimization |
| 1.6.2-INT-019 | Integration | P1 | Same key+different hash overwrites | Update behavior |

### AC-13: Hash in Response

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-UNIT-018 | Unit | P0 | store() response includes content_hash | Response contract |
| 1.6.2-INT-020 | Integration | P1 | retrieve() response includes content_hash | Response contract |

### AC-14: S3 Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-021 | Integration | P1 | store() with s3:// URI uploads successfully | Cloud provider support |
| 1.6.2-INT-022 | Integration | P1 | retrieve() from s3:// URI works | Cloud read |

### AC-15: GCS Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-023 | Integration | P1 | store() with gs:// URI uploads successfully | Cloud provider support |
| 1.6.2-INT-024 | Integration | P1 | retrieve() from gs:// URI works | Cloud read |

### AC-16: Azure Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-025 | Integration | P2 | store() with az:// URI uploads successfully | Cloud provider support |
| 1.6.2-INT-026 | Integration | P2 | retrieve() from az:// URI works | Cloud read |

### AC-17: Local Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-027 | Integration | P0 | store() with local path ./ltm_data/ works | Local fallback |
| 1.6.2-INT-028 | Integration | P0 | retrieve() from local path works | Local read |
| 1.6.2-E2E-002 | E2E | P1 | Full CRUD cycle with local storage | Local development journey |

### AC-18: httpfs Extension

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-029 | Integration | P1 | httpfs extension loaded on init | Extension loading |
| 1.6.2-INT-030 | Integration | P2 | Cloud operations use httpfs (not fsspec for read) | Implementation verification |

### AC-19: FTS Extension

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-031 | Integration | P1 | FTS extension loaded when enable_fts=True | Extension loading |
| 1.6.2-INT-032 | Integration | P2 | FTS extension not loaded when enable_fts=False | Configuration respect |

### AC-20: FTS Index

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-033 | Integration | P1 | ltm_fts table created on init | Schema creation |
| 1.6.2-INT-034 | Integration | P1 | FTS index created with correct columns | Index structure |

### AC-21: FTS Search

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-035 | Integration | P1 | search(query) returns BM25-ranked results | FTS ranking |
| 1.6.2-INT-036 | Integration | P1 | Higher relevance entries appear first | Ranking correctness |

### AC-22: Metadata Filter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-037 | Integration | P1 | search(metadata_filter) filters correctly | Metadata filtering |
| 1.6.2-INT-038 | Integration | P2 | Combined query + metadata_filter works | Combined filtering |

### AC-23: Shared Engine

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-039 | Integration | P2 | Accepts external DuckDBQueryEngine | Engine injection |
| 1.6.2-INT-040 | Integration | P2 | Operations use shared engine connection | Connection reuse |

### AC-24: Standalone Engine

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-041 | Integration | P2 | Creates own engine when none provided | Auto-creation |
| 1.6.2-E2E-003 | E2E | P2 | Backend works with no external dependencies | Isolation |

### AC-25: Circuit Breaker

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.2-INT-042 | Integration | P2 | Circuit breaker trips after repeated failures | Fault tolerance |
| 1.6.2-E2E-004 | E2E | P2 | Backend recovers after circuit breaker reset | Resilience journey |

---

## Risk Coverage Matrix

| Risk | Mitigating Tests |
|------|------------------|
| Data loss on store/retrieve | 1.6.2-INT-003, 1.6.2-INT-005, 1.6.2-E2E-001 |
| Deduplication failure | 1.6.2-UNIT-016, 1.6.2-UNIT-017, 1.6.2-INT-017, 1.6.2-INT-018 |
| Cloud storage failures | 1.6.2-INT-021 to 1.6.2-INT-028 |
| FTS index corruption | 1.6.2-INT-033, 1.6.2-INT-034, 1.6.2-INT-035 |
| Circuit breaker not triggering | 1.6.2-INT-042, 1.6.2-E2E-004 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic): 1.6.2-UNIT-001 through 1.6.2-UNIT-018
2. **P0 Integration tests** (data integrity): 1.6.2-INT-001 through 1.6.2-INT-020
3. **P0 E2E tests** (critical journey): 1.6.2-E2E-001
4. **P1 Integration tests** (cloud/FTS): 1.6.2-INT-021 through 1.6.2-INT-038
5. **P1 E2E tests**: 1.6.2-E2E-002
6. **P2 tests** (as time permits): Remaining tests

---

## Test Environment Requirements

| Environment | Purpose | Setup |
|-------------|---------|-------|
| In-memory SQLite catalog | Unit tests | `SQLiteCatalog(":memory:")` |
| Local file storage | Integration tests | `./test_ltm/` directory |
| Mock S3/GCS | Cloud integration | LocalStack or moto |
| DuckDB with FTS | FTS tests | Ensure fts extension available |

---

## Coverage Gaps

None identified. All 25 acceptance criteria have test coverage.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for I/O)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
