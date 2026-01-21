# Test Design: Story TEA-LTM-015

**Date:** 2025-01-18
**Designer:** Quinn (Test Architect)
**Story:** TEA-LTM-015 - Hierarchical LTM Backend

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 47 | 100% |
| **Unit tests** | 16 | 34% |
| **Integration tests** | 23 | 49% |
| **E2E tests** | 5 | 11% |
| **Performance tests** | 3 | 6% |

### Priority Distribution

| Priority | Count | Focus |
|----------|-------|-------|
| **P0** | 18 | Core functionality, data integrity, tenant isolation |
| **P1** | 19 | Error handling, edge cases, performance targets |
| **P2** | 10 | Cleanup, monitoring, nice-to-have |

---

## Test Scenarios by Acceptance Criteria

### AC-1: YAML Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-UNIT-001 | Unit | P1 | Parse valid YAML with all config options | Config parsing is pure logic |
| TEA-LTM-015-UNIT-002 | Unit | P1 | Validate required fields (catalog.url, storage.uri) present | Validation logic |
| TEA-LTM-015-UNIT-003 | Unit | P1 | Expand environment variables (${DATABASE_URL}) | Template expansion logic |
| TEA-LTM-015-UNIT-004 | Unit | P2 | Use defaults for optional fields (metadata_cache_ttl, parallel_reads) | Default value logic |
| TEA-LTM-015-INT-001 | Integration | P1 | Initialize backend from YAML config file | Full config loading flow |

---

### AC-2: Backend Factory Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-UNIT-005 | Unit | P0 | `create_ltm_backend("hierarchical", ...)` returns HierarchicalLTMBackend | Factory registration |
| TEA-LTM-015-INT-002 | Integration | P0 | Factory creates backend with PostgreSQL connection | End-to-end factory creation |
| TEA-LTM-015-UNIT-006 | Unit | P1 | Factory raises error for missing required params | Error handling |

---

### AC-3: Store with Entity Association

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-003 | Integration | P0 | Store creates blob at correct hierarchical path | Core write flow |
| TEA-LTM-015-INT-004 | Integration | P0 | Store updates PostgreSQL catalog with entry metadata | Catalog integration |
| TEA-LTM-015-INT-005 | Integration | P0 | Store creates entry-owner association in database | Entity association |
| TEA-LTM-015-UNIT-007 | Unit | P0 | Generate correct blob path from entity tuple | Path generation logic |
| TEA-LTM-015-INT-006 | Integration | P1 | Store with optional metadata includes metadata in catalog | Optional params handling |
| TEA-LTM-015-INT-007 | Integration | P1 | Store with expires_at sets TTL in catalog | TTL handling |

---

### AC-4: Retrieve by Key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-008 | Integration | P0 | Retrieve returns stored entry by key | Core read flow |
| TEA-LTM-015-INT-009 | Integration | P0 | Retrieve returns None for non-existent key | Negative case |
| TEA-LTM-015-INT-010 | Integration | P1 | Retrieve uses metadata cache on repeated calls | Cache behavior |
| TEA-LTM-015-UNIT-008 | Unit | P1 | Inlined entries returned directly (no blob fetch) | Inline optimization |

---

### AC-5: Retrieve by Entity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-011 | Integration | P0 | Retrieve by session returns session entries | Direct entity query |
| TEA-LTM-015-INT-012 | Integration | P0 | Retrieve by user includes all user's sessions (descendants) | Closure table query |
| TEA-LTM-015-INT-013 | Integration | P0 | Retrieve by project includes all users and sessions | Multi-level descendants |
| TEA-LTM-015-INT-014 | Integration | P0 | Retrieve by org includes entire hierarchy | Full hierarchy query |
| TEA-LTM-015-INT-015 | Integration | P1 | Retrieve with `include_descendants=False` returns direct only | Flag behavior |
| TEA-LTM-015-INT-016 | Integration | P1 | Pagination (limit/offset) works correctly | Pagination |
| TEA-LTM-015-UNIT-009 | Unit | P1 | Returns correct `total_count` and `has_more` flag | Response structure |

---

### AC-6: Path Generation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-UNIT-010 | Unit | P0 | Path includes all hierarchy levels from closure table | Path generation is pure logic |
| TEA-LTM-015-UNIT-011 | Unit | P0 | Path escapes special characters in entity IDs | Security/correctness |
| TEA-LTM-015-UNIT-012 | Unit | P1 | Path format matches: `{storage_uri}/{type:id}/{type:id}/.../` | Format validation |

---

### AC-7: Parquet Index Management (Delta Files)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-017 | Integration | P1 | Write creates delta file at each hierarchy level | Delta pattern |
| TEA-LTM-015-INT-018 | Integration | P1 | Delta file named with UUID for uniqueness | Concurrency safety |
| TEA-LTM-015-INT-019 | Integration | P1 | Delete creates tombstone entry | Soft delete pattern |
| TEA-LTM-015-UNIT-013 | Unit | P1 | Reads union all delta files correctly | Read logic |

---

### AC-8: Index Compaction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-020 | Integration | P1 | Compaction merges delta files into main index | Core compaction |
| TEA-LTM-015-INT-021 | Integration | P1 | Compaction removes tombstoned entries | Tombstone processing |
| TEA-LTM-015-INT-022 | Integration | P1 | Compaction cleans up processed delta files | Cleanup |
| TEA-LTM-015-INT-023 | Integration | P1 | Auto-triggers when delta count exceeds threshold | Threshold trigger |
| TEA-LTM-015-UNIT-014 | Unit | P2 | Returns compacted_paths and entries_processed | Response structure |

---

### AC-9: Metadata Cache (DuckDB)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-024 | Integration | P1 | DuckDB settings applied on initialization | Config application |
| TEA-LTM-015-PERF-001 | Performance | P1 | Repeated queries faster with cache enabled | Cache effectiveness |

---

### AC-10: Parallel Retrieval

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-025 | Integration | P1 | Batch retrieve returns all requested entries | Batch functionality |
| TEA-LTM-015-PERF-002 | Performance | P1 | Parallel retrieval faster than sequential for >10 entries | Parallelization benefit |
| TEA-LTM-015-UNIT-015 | Unit | P2 | Thread count respects configuration | Config honoring |

---

### AC-11: Tenant Isolation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-E2E-001 | E2E | P0 | Org A cannot query entries from Org B | **Critical security** |
| TEA-LTM-015-E2E-002 | E2E | P0 | Org A cannot access Org B's blob storage paths | Storage isolation |
| TEA-LTM-015-INT-026 | Integration | P0 | PostgreSQL queries always filtered by root entity | Query filtering |

---

### AC-12: Access Control Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-027 | Integration | P0 | `allowed_ancestors` blocks unauthorized access | Authorization |
| TEA-LTM-015-INT-028 | Integration | P0 | `allowed_ancestors` permits authorized access | Authorization positive |
| TEA-LTM-015-UNIT-016 | Unit | P1 | Entity ancestry validated against allowed list | Validation logic |

---

### AC-13: Default Entity Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-029 | Integration | P1 | Missing org uses configured default | Default resolution |
| TEA-LTM-015-INT-030 | Integration | P1 | Missing project uses configured default | Default resolution |
| TEA-LTM-015-INT-031 | Integration | P1 | Default entities auto-created if not exist | Auto-creation |
| TEA-LTM-015-INT-032 | Integration | P1 | Full path resolved with partial parents | Partial resolution |

---

### AC-14: Blob Write Failure Recovery

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-033 | Integration | P0 | Blob write failure does NOT update catalog | Consistency guarantee |
| TEA-LTM-015-INT-034 | Integration | P0 | Catalog failure after blob success logs orphan | Orphan tracking |
| TEA-LTM-015-INT-035 | Integration | P1 | StorageError raised on blob write failure | Error type |
| TEA-LTM-015-INT-036 | Integration | P1 | CatalogError includes blob_path for retry | Error context |

---

### AC-15: Connection Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-037 | Integration | P1 | Connection pool uses configured size | Config honoring |
| TEA-LTM-015-INT-038 | Integration | P1 | Retry 3 times with exponential backoff on failure | Retry behavior |
| TEA-LTM-015-INT-039 | Integration | P1 | ConnectionError raised after retries exhausted | Error escalation |

---

### AC-16: Concurrent Index Write Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-040 | Integration | P1 | Concurrent writes create separate delta files | Concurrency handling |
| TEA-LTM-015-INT-041 | Integration | P1 | No data loss under concurrent writes | Data integrity |

---

### AC-17: Index Compaction Failure Recovery

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-042 | Integration | P1 | Interrupted compaction recoverable by re-running | Idempotency |
| TEA-LTM-015-INT-043 | Integration | P2 | Atomic rename uses generation-match | Atomicity mechanism |

---

### AC-18: Orphan Cleanup Job

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-INT-044 | Integration | P1 | Cleanup removes blobs older than max_age without catalog entry | Core cleanup |
| TEA-LTM-015-INT-045 | Integration | P1 | Cleanup preserves blobs younger than max_age | Age filtering |
| TEA-LTM-015-INT-046 | Integration | P1 | Cleanup preserves blobs with catalog entries | Catalog check |
| TEA-LTM-015-INT-047 | Integration | P2 | dry_run mode reports without deleting | Dry run |
| TEA-LTM-015-INT-048 | Integration | P2 | Returns scanned_count, orphan_count, deleted_paths | Response structure |

---

### End-to-End Critical Paths

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-E2E-003 | E2E | P0 | Full write → read → query cycle works | Critical path |
| TEA-LTM-015-E2E-004 | E2E | P0 | Multi-tenant isolation end-to-end | Security critical |
| TEA-LTM-015-E2E-005 | E2E | P1 | Large dataset (1M entries) query performance | Scale validation |

---

### Performance Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-LTM-015-PERF-001 | Perf | P1 | Metadata cache improves repeated query latency | Cache effectiveness |
| TEA-LTM-015-PERF-002 | Perf | P1 | Parallel batch retrieval outperforms sequential | Parallelization |
| TEA-LTM-015-PERF-003 | Perf | P1 | Session query <100ms at 1M entries | Target validation |

---

## Risk Coverage

| Risk ID | Test Coverage | Tests |
|---------|---------------|-------|
| DATA-001 (Blob-catalog consistency) | **Covered** | INT-033, INT-034, INT-035, INT-036 |
| DATA-002 (Orphan accumulation) | **Covered** | INT-044, INT-045, INT-046, INT-047 |
| TECH-001 (Concurrent move corruption) | **Partial** | INT-040, INT-041 (need explicit move concurrency test) |
| SEC-001 (Tenant isolation) | **Covered** | E2E-001, E2E-002, INT-026, INT-027, INT-028 |
| PERF-001 (Compaction blocking) | **Partial** | INT-020-023 (need latency under compaction test) |
| PERF-002 (Stale cache) | **Not covered** | Add test for stale read within TTL |

### Recommended Additional Tests (Risk Mitigation)

| ID | Level | Priority | Test | Mitigates Risk |
|----|-------|----------|------|----------------|
| TEA-LTM-015-CHAOS-001 | Chaos | P0 | Kill process between blob write and catalog update | DATA-001 |
| TEA-LTM-015-CONC-001 | Concurrency | P1 | Parallel move_entity on overlapping subtrees | TECH-001 |
| TEA-LTM-015-PERF-004 | Performance | P1 | Write latency during active compaction | PERF-001 |
| TEA-LTM-015-CACHE-001 | Integration | P2 | Stale read after delete within cache TTL | PERF-002 |

---

## Recommended Execution Order

### Phase 1: Fast Fail (P0 Unit)
1. TEA-LTM-015-UNIT-005 (Factory registration)
2. TEA-LTM-015-UNIT-007 (Path generation)
3. TEA-LTM-015-UNIT-010, 011 (Path correctness)
4. TEA-LTM-015-UNIT-016 (Authorization validation)

### Phase 2: Core Integration (P0 Integration)
5. TEA-LTM-015-INT-002 (Factory with PostgreSQL)
6. TEA-LTM-015-INT-003, 004, 005 (Store flow)
7. TEA-LTM-015-INT-008, 009 (Retrieve flow)
8. TEA-LTM-015-INT-011 through 014 (Entity queries)
9. TEA-LTM-015-INT-026, 027, 028 (Authorization)
10. TEA-LTM-015-INT-033, 034 (Consistency guarantees)

### Phase 3: Security Critical (P0 E2E)
11. TEA-LTM-015-E2E-001, 002 (Tenant isolation)
12. TEA-LTM-015-E2E-003, 004 (Critical paths)

### Phase 4: Error Handling (P1)
13. INT-035 through 039 (Error handling)
14. INT-040, 041 (Concurrency)
15. INT-042, 043 (Recovery)

### Phase 5: Performance & Edge Cases (P1-P2)
16. PERF-001, 002, 003 (Performance targets)
17. Remaining P1 and P2 tests

---

## Test Environment Requirements

### Unit Tests
- No external dependencies
- Mock catalog and storage

### Integration Tests
- PostgreSQL (Docker: `postgres:15`)
- Local file storage (`file:///tmp/ltm_test/`)
- DuckDB in-memory

### E2E Tests
- PostgreSQL with test data seeded
- GCS emulator (optional) or local storage
- Multiple tenant fixtures (org:tenant_a, org:tenant_b)

### Performance Tests
- PostgreSQL with 1M+ entries
- GCS or S3 with real network latency (or emulated)
- Benchmark baseline from existing backends

---

## Quality Checklist

- [x] Every AC has test coverage (18 ACs → 48 tests)
- [x] Test levels are appropriate (shift-left: 34% unit, 49% integration)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for security, data integrity)
- [x] Test IDs follow naming convention (TEA-LTM-015-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed (4 additional tests recommended)
