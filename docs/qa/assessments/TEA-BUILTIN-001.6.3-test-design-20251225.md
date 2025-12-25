# Test Design: Story TEA-BUILTIN-001.6.3 Serverless Optimization

**Date:** 2025-12-25
**Designer:** Quinn (Test Architect)
**Story:** Serverless Optimization for DuckDB LTM Backend

## Test Strategy Overview

- **Total test scenarios:** 48
- **Unit tests:** 22 (46%)
- **Integration tests:** 19 (40%)
- **E2E tests:** 7 (14%)
- **Priority distribution:** P0: 18, P1: 20, P2: 10

### Risk Context

This story addresses **serverless deployment optimization** which is critical for:
- Cloud Functions / Lambda / Edge deployment scenarios
- Production cold start performance
- Batch operation efficiency at scale
- Data lifecycle management (TTL/cleanup)

The high number of P0 tests reflects the performance-critical nature of serverless deployments where cold start times and batch efficiency directly impact user experience and cost.

---

## Test Scenarios by Acceptance Criteria

### Cold Start Optimization (AC-1 to AC-6)

#### AC-1: Lazy Initialization

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-001 | Unit | P0 | Verify catalog property returns None before first access | Pure logic: lazy initialization pattern |
| 1.6.3-UNIT-002 | Unit | P0 | Verify catalog property creates backend on first access | Pure logic: singleton pattern |
| 1.6.3-UNIT-003 | Unit | P1 | Verify subsequent catalog accesses return same instance | Pure logic: caching behavior |
| 1.6.3-UNIT-004 | Unit | P0 | Verify engine property is lazily initialized | Pure logic: engine lazy pattern |
| 1.6.3-INT-001 | Integration | P1 | Verify import does not trigger catalog connection | Component interaction: module loading |

#### AC-2: Connection Pooling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-005 | Unit | P1 | Verify pool configuration parameters validated | Pure validation logic |
| 1.6.3-INT-002 | Integration | P0 | Verify connections reused from pool | DB pooling behavior |
| 1.6.3-INT-003 | Integration | P1 | Verify pool exhaustion handling | Edge case: resource limits |

#### AC-3: Firestore Pool

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-006 | Unit | P0 | Verify Firestore client singleton returns same instance | Pure logic: singleton pattern |
| 1.6.3-INT-004 | Integration | P0 | Verify Firestore client reused across invocations | Firestore client behavior |
| 1.6.3-INT-005 | Integration | P1 | Verify Firestore client handles project parameter | Service configuration |

#### AC-4: Postgres Pool

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-007 | Unit | P0 | Verify pool config: min_size=1, max_size=10 | Pure configuration validation |
| 1.6.3-INT-006 | Integration | P0 | Verify asyncpg pool creation and connection acquisition | DB pool integration |
| 1.6.3-INT-007 | Integration | P1 | Verify pool handles concurrent connection requests | Concurrency behavior |
| 1.6.3-INT-008 | Integration | P2 | Verify pool command_timeout=30 enforced | Timeout configuration |

#### AC-5: Cold Start Target

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-009 | Integration | P0 | Benchmark catalog initialization < 100ms | Performance critical |
| 1.6.3-INT-010 | Integration | P0 | Benchmark SQLiteCatalog init < 5ms | Performance critical |
| 1.6.3-INT-011 | Integration | P1 | Benchmark FirestoreCatalog init < 100ms | Performance validation |
| 1.6.3-INT-012 | Integration | P1 | Benchmark PostgresCatalog init < 50ms | Performance validation |

#### AC-6: Warm Start Target

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-013 | Integration | P0 | Benchmark warm start < 10ms with connection reuse | Performance critical |
| 1.6.3-UNIT-008 | Unit | P1 | Verify no re-initialization on subsequent calls | Pure logic: caching |

---

### Batch Operations (AC-7 to AC-12)

#### AC-7: store_batch Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-009 | Unit | P0 | Verify store_batch accepts list of entries | Pure input validation |
| 1.6.3-UNIT-010 | Unit | P0 | Verify store_batch separates inlined vs cloud entries by threshold | Pure logic: decision tree |
| 1.6.3-INT-014 | Integration | P0 | Verify store_batch writes multiple entries atomically | DB transaction behavior |
| 1.6.3-E2E-001 | E2E | P1 | Store 100 entries and verify retrieval | Full workflow validation |

#### AC-8: retrieve_batch Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-011 | Unit | P0 | Verify retrieve_batch accepts list of keys | Pure input validation |
| 1.6.3-UNIT-012 | Unit | P1 | Verify retrieve_batch returns {results, missing} structure | Pure output format |
| 1.6.3-INT-015 | Integration | P0 | Verify retrieve_batch fetches multiple entries efficiently | DB batch query |
| 1.6.3-INT-016 | Integration | P1 | Verify retrieve_batch handles mix of inlined and cloud entries | Multi-source retrieval |

#### AC-9: Batch Catalog Writes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-017 | Integration | P0 | Verify Firestore WriteBatch commits multiple documents | Firestore batch API |
| 1.6.3-INT-018 | Integration | P0 | Verify Postgres executemany batches inserts | Postgres batch SQL |
| 1.6.3-INT-019 | Integration | P1 | Verify SQLite batch insert performance | SQLite batch behavior |

#### AC-10: Batch Cloud Uploads

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-020 | Integration | P1 | Verify parallel uploads for large data entries | Async upload behavior |
| 1.6.3-E2E-002 | E2E | P2 | Upload 10 large files in parallel < 2s | Performance validation |

#### AC-11: Batch Atomicity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-013 | Unit | P0 | Verify batch result reports success/failed counts | Pure output validation |
| 1.6.3-INT-021 | Integration | P0 | Verify failed batch rolls back all entries | Transaction atomicity |
| 1.6.3-INT-022 | Integration | P1 | Verify partial failure in cloud upload handled correctly | Error recovery |

#### AC-12: Batch Size Limit

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-014 | Unit | P1 | Verify batch_size parameter defaults to 500 | Configuration default |
| 1.6.3-UNIT-015 | Unit | P1 | Verify entries chunked by batch_size | Pure chunking logic |
| 1.6.3-INT-023 | Integration | P2 | Verify Firestore 500 ops per batch limit respected | Firestore constraint |

---

### TTL and Cleanup (AC-13 to AC-18)

#### AC-13: expires_at Tracking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-016 | Unit | P0 | Verify expires_at stored in catalog entry | Pure data structure |
| 1.6.3-INT-024 | Integration | P1 | Verify expires_at persisted to all catalog backends | DB schema validation |

#### AC-14: cleanup_expired Method

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-017 | Unit | P0 | Verify cleanup_expired accepts limit parameter | Pure input validation |
| 1.6.3-INT-025 | Integration | P0 | Verify cleanup_expired removes expired entries | DB deletion behavior |
| 1.6.3-E2E-003 | E2E | P1 | Cleanup 50 expired entries with cloud files | Full lifecycle test |

#### AC-15: Cleanup Returns Count

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-018 | Unit | P0 | Verify cleanup returns {deleted: int, errors: list} | Pure output format |
| 1.6.3-INT-026 | Integration | P1 | Verify deleted count matches actual deletions | Count accuracy |

#### AC-16: Cleanup Removes Cloud

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-027 | Integration | P0 | Verify cleanup deletes associated cloud files | File deletion behavior |
| 1.6.3-E2E-004 | E2E | P1 | Verify cloud storage empty after cleanup | Full cleanup validation |

#### AC-17: Probabilistic Cleanup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-019 | Unit | P1 | Verify cleanup triggered at configured probability | Pure random logic |
| 1.6.3-INT-028 | Integration | P2 | Verify cache.wrap calls cleanup_expired on cache miss | Integration behavior |

#### AC-18: Cleanup Efficiency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-020 | Unit | P1 | Verify query uses indexed expires_at filter | Query structure |
| 1.6.3-INT-029 | Integration | P2 | Benchmark cleanup query with index < 50ms | Performance validation |

---

### Graceful Degradation (AC-19 to AC-21)

#### AC-19: Catalog Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-021 | Unit | P0 | Verify catalog unavailable exception caught and logged | Error handling logic |
| 1.6.3-INT-030 | Integration | P0 | Verify operation proceeds without caching when catalog fails | Fallback behavior |
| 1.6.3-E2E-005 | E2E | P2 | Agent completes workflow when catalog unavailable | Resilience validation |

#### AC-20: Cloud Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-UNIT-022 | Unit | P1 | Verify exponential backoff parameters (max retries, delay) | Configuration logic |
| 1.6.3-INT-031 | Integration | P1 | Verify retry with backoff on cloud upload failure | Retry behavior |
| 1.6.3-E2E-006 | E2E | P2 | Recover from transient cloud storage failure | Full recovery test |

#### AC-21: Partial Batch Success

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.6.3-INT-032 | Integration | P0 | Verify batch reports partial success with {stored, failed, errors} | Partial result handling |
| 1.6.3-E2E-007 | E2E | P1 | Process batch with 1 failing entry, verify others succeed | Resilience validation |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Cold start exceeds 100ms | Medium | High | 1.6.3-INT-009, 1.6.3-INT-010-012 |
| Connection pool exhaustion | Low | High | 1.6.3-INT-003, 1.6.3-INT-007 |
| Batch atomicity failure | Low | Critical | 1.6.3-INT-021, 1.6.3-INT-022 |
| Cleanup leaves orphaned cloud files | Medium | Medium | 1.6.3-INT-027, 1.6.3-E2E-004 |
| Catalog unavailable in production | Medium | High | 1.6.3-INT-030, 1.6.3-E2E-005 |
| Firestore 500 ops limit exceeded | Low | Medium | 1.6.3-INT-023 |

---

## Recommended Execution Order

1. **P0 Unit tests** - Fail fast on core logic (11 tests)
2. **P0 Integration tests** - Validate critical integrations (12 tests)
3. **P1 Unit tests** - Secondary logic validation (8 tests)
4. **P1 Integration tests** - Secondary integrations (8 tests)
5. **P1 E2E tests** - Core workflow validation (4 tests)
6. **P2 tests** - As time permits (10 tests)

---

## Test Implementation Notes

### Mocking Strategy

- **Unit tests**: Mock all external dependencies (catalog backends, cloud storage, DuckDB)
- **Integration tests**: Use real local backends (SQLite, in-memory DuckDB) with mocked cloud
- **E2E tests**: Use full stack with test cloud storage (local MinIO or emulator)

### Performance Benchmarks

Benchmarks should be implemented as integration tests with configurable thresholds:

```python
@pytest.mark.benchmark
def test_cold_start_under_100ms():
    start = time.perf_counter()
    backend = DuckDBLTMBackend(catalog_config, storage_uri)
    _ = backend.catalog  # Trigger lazy init
    elapsed = (time.perf_counter() - start) * 1000
    assert elapsed < 100, f"Cold start {elapsed:.2f}ms exceeds 100ms target"
```

### Fixture Strategy

```python
@pytest.fixture
def lazy_backend():
    """Backend configured for lazy init testing."""
    return DuckDBLTMBackend(
        catalog_config={"backend": "sqlite", "path": ":memory:"},
        storage_uri="memory://test/"
    )

@pytest.fixture
def batch_entries():
    """Generate test entries for batch operations."""
    return [{"key": f"key-{i}", "value": {"data": i}} for i in range(100)]
```

---

## Quality Checklist

- [x] Every AC has test coverage (21 ACs mapped to 48 tests)
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (performance = P0)
- [x] Test IDs follow naming convention (1.6.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-001.6.3
  story_title: Serverless Optimization
  designer: Quinn (Test Architect)
  date: 2025-12-25
  scenarios_total: 48
  by_level:
    unit: 22
    integration: 19
    e2e: 7
  by_priority:
    p0: 18
    p1: 20
    p2: 10
  coverage_gaps: []
  key_risks:
    - cold_start_performance
    - batch_atomicity
    - cleanup_orphaned_files
  recommendations:
    - Implement performance benchmarks with CI thresholds
    - Use mocked cloud storage for unit/integration tests
    - Add chaos testing for graceful degradation scenarios
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.6.3-test-design-20251225.md
P0 tests identified: 18
P1 tests identified: 20
P2 tests identified: 10
Total scenarios: 48
Coverage: 100% of acceptance criteria
```
