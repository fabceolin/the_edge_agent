# Test Design: Story TEA-BUILTIN-010

**Date:** 2024-12-24
**Designer:** Quinn (Test Architect)
**Story:** Cache and Memoization Actions

## Test Strategy Overview

- **Total test scenarios:** 48
- **Unit tests:** 28 (58%)
- **Integration tests:** 15 (31%)
- **E2E tests:** 5 (11%)
- **Priority distribution:** P0: 18, P1: 20, P2: 10

## Test Scenarios by Acceptance Criteria

### AC-1: cache.wrap action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-001 | Unit | P0 | cache.wrap calls wrapped action and returns result | Core functionality - pure action wrapping logic |
| 010-UNIT-002 | Unit | P0 | cache.wrap passes args correctly to wrapped action | Argument forwarding is critical |
| 010-INT-001 | Integration | P0 | cache.wrap integrates with ltm.retrieve and ltm.store | Multi-component interaction with LTM |

### AC-2: Cache Key Generation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-003 | Unit | P0 | key_strategy=sha256 generates correct hash | Pure hashing logic |
| 010-UNIT-004 | Unit | P0 | key_strategy=args hashes arguments deterministically | Pure hashing logic |
| 010-UNIT-005 | Unit | P0 | key_strategy=custom evaluates Jinja expression | Template evaluation logic |
| 010-UNIT-006 | Unit | P1 | key_strategy=file_content loads file and hashes | File I/O with hashing |
| 010-UNIT-007 | Unit | P1 | Same args produce same cache key (deterministic) | Consistency verification |
| 010-UNIT-008 | Unit | P1 | Different args produce different cache keys | Collision avoidance |

### AC-3: Cache Lookup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-INT-002 | Integration | P0 | cache.wrap checks LTM before executing action | LTM integration point |
| 010-UNIT-009 | Unit | P1 | Cache lookup uses correct key format | Key formatting logic |

### AC-4: Cache Hit Return

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-010 | Unit | P0 | Cache hit returns cached value without executing action | Core cache hit logic |
| 010-UNIT-011 | Unit | P0 | Cache hit includes _cache_hit=true in response | Metadata correctness |
| 010-UNIT-012 | Unit | P1 | Cache hit includes _cache_created_at from metadata | Observability requirement |
| 010-INT-003 | Integration | P1 | Second call returns cached result from LTM | End-to-end cache hit flow |

### AC-5: Cache Miss Execute

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-013 | Unit | P0 | Cache miss executes wrapped action normally | Core cache miss logic |
| 010-UNIT-014 | Unit | P1 | Cache miss sets _cache_hit=false in response | Metadata correctness |

### AC-6: Cache Store

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-INT-004 | Integration | P0 | Successful execution stores result in LTM | LTM storage integration |
| 010-UNIT-015 | Unit | P1 | Failed action does not store in cache | Prevent caching errors |
| 010-UNIT-016 | Unit | P1 | Cache store includes all required metadata fields | Metadata completeness |

### AC-7: Configurable TTL

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-017 | Unit | P1 | ttl_days sets correct expiration timestamp | TTL calculation logic |
| 010-UNIT-018 | Unit | P1 | ttl_hours overrides ttl_days | TTL precedence logic |
| 010-UNIT-019 | Unit | P1 | ttl_seconds overrides ttl_hours and ttl_days | TTL precedence logic |
| 010-UNIT-020 | Unit | P0 | Expired cache entry treated as miss | Expiration enforcement |

### AC-8: Skip Cache

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-021 | Unit | P0 | skip_cache=true bypasses cache lookup | Bypass logic |
| 010-UNIT-022 | Unit | P1 | skip_cache=true still stores result in cache | Partial bypass behavior |

### AC-9: Cache Disabled

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-023 | Unit | P1 | cache_enabled=false skips lookup and store | Full disable logic |
| 010-UNIT-024 | Unit | P1 | cache_enabled=false executes action normally | Fallback behavior |

### AC-10: cache.invalidate action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-025 | Unit | P1 | cache.invalidate deletes entry by exact key | Key deletion logic |
| 010-INT-005 | Integration | P1 | cache.invalidate with pattern deletes matching entries | Pattern matching with LTM |
| 010-INT-006 | Integration | P2 | cache.invalidate with metadata_filter deletes by filter | Metadata filter integration |
| 010-UNIT-026 | Unit | P2 | cache.invalidate returns deleted_count and deleted_keys | Response format |

### AC-11: cache.get action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-027 | Unit | P2 | cache.get retrieves value by key | Retrieval logic |
| 010-UNIT-028 | Unit | P2 | cache.get returns found=false for missing key | Not found handling |
| 010-UNIT-029 | Unit | P2 | cache.get returns expired=true for expired entry | Expiration detection |
| 010-UNIT-030 | Unit | P2 | cache.get includes metadata when include_metadata=true | Optional metadata |

### AC-12: storage.hash action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-031 | Unit | P0 | storage.hash computes SHA256 of local file | Core hashing functionality |
| 010-UNIT-032 | Unit | P1 | storage.hash supports MD5 algorithm | Algorithm selection |
| 010-UNIT-033 | Unit | P1 | storage.hash supports Blake2b algorithm | Algorithm selection |
| 010-INT-007 | Integration | P1 | storage.hash works with S3 URIs | Cloud storage integration |
| 010-INT-008 | Integration | P1 | storage.hash works with GCS URIs | Cloud storage integration |
| 010-INT-009 | Integration | P2 | storage.hash works with Azure URIs | Cloud storage integration |
| 010-UNIT-034 | Unit | P1 | storage.hash returns size_bytes | File size in response |

### AC-13: sha256 Jinja filter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-035 | Unit | P1 | sha256 filter hashes string correctly | Filter logic |
| 010-UNIT-036 | Unit | P2 | sha256 filter handles bytes input | Input type handling |
| 010-UNIT-037 | Unit | P2 | sha256 filter is available in Jinja environment | Registration verification |

### AC-14: Probabilistic Cleanup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-038 | Unit | P1 | Cleanup runs ~5% of cache misses (statistical test) | Probability verification |
| 010-UNIT-039 | Unit | P2 | cleanup_probability=0 never runs cleanup | Disable cleanup |
| 010-UNIT-040 | Unit | P2 | cleanup_probability=1 always runs cleanup | Force cleanup |

### AC-15: Cleanup Logic

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-INT-010 | Integration | P1 | Cleanup searches LTM for expired entries | LTM search integration |
| 010-INT-011 | Integration | P1 | Cleanup deletes only expired entries | Correct filtering |

### AC-16: Cleanup Limit

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-041 | Unit | P1 | Cleanup deletes at most cleanup_limit entries | Limit enforcement |
| 010-UNIT-042 | Unit | P2 | Default cleanup_limit is 5 | Default value |

### AC-17: LTM Failure Graceful

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-INT-012 | Integration | P0 | LTM retrieve failure proceeds with action execution | Graceful degradation |
| 010-INT-013 | Integration | P0 | LTM store failure does not fail the action | Graceful degradation |

### AC-18: Hash Failure Graceful

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-043 | Unit | P1 | File not found proceeds without caching | Graceful degradation |
| 010-UNIT-044 | Unit | P1 | Permission denied proceeds without caching | Graceful degradation |

### AC-19: Cache Metadata

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-045 | Unit | P1 | Cached entry includes _cache_type metadata | Metadata completeness |
| 010-UNIT-046 | Unit | P1 | Cached entry includes _cache_action metadata | Metadata completeness |
| 010-UNIT-047 | Unit | P1 | Cached entry includes _cache_expires_at metadata | Metadata completeness |

### AC-20: Response Indicators

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-048 | Unit | P0 | Response includes _cache_hit boolean | Already covered in AC-4 |

### AC-21: Uses LTM Backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-INT-014 | Integration | P0 | All cache ops use ltm.* actions | Architecture compliance |

### AC-22: Dual Namespace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-UNIT-049 | Unit | P1 | Actions accessible as cache.* | Namespace registration |
| 010-UNIT-050 | Unit | P1 | Actions accessible as actions.cache_* | Namespace registration |

### AC-23: Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-E2E-001 | E2E | P2 | Example YAML agent with cache works end-to-end | Documentation accuracy |

## E2E Test Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 010-E2E-002 | E2E | P0 | Full cache flow: miss → store → hit | Critical user journey |
| 010-E2E-003 | E2E | P1 | Cache with file_content strategy on real file | Real-world usage |
| 010-E2E-004 | E2E | P1 | cache.wrap with llm.call action | Common integration |
| 010-E2E-005 | E2E | P2 | Cache invalidation followed by fresh execution | Cache management flow |

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Cache key collisions | 010-UNIT-007, 010-UNIT-008 |
| Stale data returned | 010-UNIT-020, 010-UNIT-021 |
| LTM backend unavailable | 010-INT-012, 010-INT-013 |
| Memory exhaustion from large cached results | Manual/load testing |
| Cleanup never runs | 010-UNIT-038 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - 010-UNIT-001 through 010-UNIT-003
   - 010-UNIT-010, 010-UNIT-011, 010-UNIT-013
   - 010-UNIT-020, 010-UNIT-021, 010-UNIT-031

2. **P0 Integration tests**
   - 010-INT-001, 010-INT-002, 010-INT-004
   - 010-INT-012, 010-INT-013, 010-INT-014

3. **P0 E2E tests**
   - 010-E2E-002

4. **P1 tests in order**
   - All P1 unit tests
   - All P1 integration tests
   - P1 E2E tests

5. **P2+ as time permits**

## Quality Gate Summary

```yaml
test_design:
  scenarios_total: 48
  by_level:
    unit: 28
    integration: 15
    e2e: 5
  by_priority:
    p0: 18
    p1: 20
    p2: 10
  coverage_gaps: []
```

## Notes

- **High unit test ratio (58%)** is appropriate due to many pure logic functions (key generation, TTL calculation, metadata formatting)
- **Integration tests (31%)** focus on LTM interactions and fsspec file access
- **E2E tests (11%)** validate complete cache flows with real backends
- Recommend using pytest fixtures for mocking LTM backend in unit tests
- Consider property-based testing for key generation determinism
