# Test Design: Story TEA-BUILTIN-008.7

**Date:** 2024-12-24
**Designer:** Quinn (Test Architect)
**Story:** ScrapeGraphAI Result Caching

## Test Strategy Overview

- **Total test scenarios:** 24
- **Unit tests:** 14 (58%)
- **Integration tests:** 7 (29%)
- **E2E tests:** 3 (13%)
- **Priority distribution:** P0: 8, P1: 10, P2: 6

## Test Scenarios by Acceptance Criteria

### AC-1: Cache Parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-001 | Unit | P0 | web.ai_scrape accepts cache dict parameter | Parameter acceptance |
| 008.7-UNIT-002 | Unit | P1 | web.ai_scrape works without cache parameter | Backward compatibility |
| 008.7-UNIT-003 | Unit | P2 | Invalid cache parameter structure raises clear error | Input validation |

### AC-2: Cache Enabled

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-004 | Unit | P0 | cache.enabled=true enables caching behavior | Core enable logic |
| 008.7-UNIT-005 | Unit | P0 | cache.enabled=false (default) skips all caching | Default behavior |

### AC-3: Cache TTL

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-006 | Unit | P1 | cache.ttl_days sets correct expiration | TTL calculation |
| 008.7-UNIT-007 | Unit | P1 | Default TTL is 60 days when not specified | Default value |
| 008.7-UNIT-008 | Unit | P1 | Expired cache entry triggers fresh scrape | Expiration enforcement |

### AC-4: Cache Key Strategy

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-009 | Unit | P0 | key_strategy=url hashes URL only | Key strategy logic |
| 008.7-UNIT-010 | Unit | P1 | key_strategy=url_schema includes schema in hash | Key strategy logic |
| 008.7-UNIT-011 | Unit | P1 | key_strategy=url_prompt includes prompt in hash | Key strategy logic |
| 008.7-UNIT-012 | Unit | P2 | Same URL produces same cache key | Deterministic hashing |
| 008.7-UNIT-013 | Unit | P2 | Different URLs produce different cache keys | Collision avoidance |

### AC-5: Skip Cache

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-014 | Unit | P0 | cache.skip=true bypasses lookup, forces scrape | Bypass logic |
| 008.7-INT-001 | Integration | P1 | skip=true still stores result for next request | Partial bypass behavior |

### AC-6: Cache Hit

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-002 | Integration | P0 | Cached scrape returns _cache_hit=true | Cache hit flow |
| 008.7-INT-003 | Integration | P0 | Cache hit does not call ScrapeGraphAI API | API cost savings |
| 008.7-UNIT-015 | Unit | P1 | Cache hit response includes original data | Data integrity |

### AC-7: Cache Miss

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-004 | Integration | P0 | Cache miss executes ScrapeGraphAI scrape | Normal execution |
| 008.7-INT-005 | Integration | P1 | Successful scrape stores result in LTM | Storage after scrape |
| 008.7-INT-006 | Integration | P2 | Failed scrape does not store in cache | Don't cache errors |

### AC-8: Cache Indicators

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-016 | Unit | P1 | Response includes _cache_hit boolean | Metadata presence |
| 008.7-UNIT-017 | Unit | P1 | Response includes _cache_key string | Metadata presence |
| 008.7-UNIT-018 | Unit | P2 | Cache hit includes _cache_created_at timestamp | Metadata presence |

### AC-9: Default Off

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-007 | Integration | P0 | No cache parameter means no caching behavior | Backward compatibility |

### AC-10: Existing Tests Pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-E2E-001 | E2E | P0 | All TEA-BUILTIN-008.4 tests still pass | Regression prevention |

## E2E Test Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-E2E-002 | E2E | P1 | Full cache flow: miss → store → hit with real URL | Critical user journey |
| 008.7-E2E-003 | E2E | P2 | url_schema strategy: same URL, different schemas get different cache | Real-world scenario |

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Stale scrape data returned | 008.7-UNIT-008, 008.7-UNIT-014 |
| Breaking existing 008.4 functionality | 008.7-E2E-001, 008.7-INT-007 |
| ScrapeGraphAI API still called on cache hit | 008.7-INT-003 |
| Different schemas sharing same cache | 008.7-UNIT-010, 008.7-E2E-003 |
| Cache key collisions | 008.7-UNIT-012, 008.7-UNIT-013 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - 008.7-UNIT-001, 008.7-UNIT-004, 008.7-UNIT-005
   - 008.7-UNIT-009, 008.7-UNIT-014

2. **P0 Integration tests**
   - 008.7-INT-002, 008.7-INT-003, 008.7-INT-004
   - 008.7-INT-007

3. **P0 E2E tests**
   - 008.7-E2E-001 (regression suite)

4. **P1 tests in order**
   - All P1 unit tests
   - All P1 integration tests
   - 008.7-E2E-002

5. **P2+ as time permits**

## Quality Gate Summary

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 14
    integration: 7
    e2e: 3
  by_priority:
    p0: 8
    p1: 10
    p2: 6
  coverage_gaps: []
```

## Notes

- **Dependency on TEA-BUILTIN-010**: Many cache mechanics are provided by the cache.wrap framework; tests here focus on web.ai_scrape-specific behavior
- **E2E-001 is critical**: Ensures backward compatibility with all 27 existing 008.4 tests
- **Mock ScrapeGraphAI API** in integration tests to avoid rate limits and costs
- **URL normalization** should be tested if implemented (not in current ACs)
- Consider adding performance test to verify cache hit is <50ms vs 2-10s scrape time

## Test Environment Requirements

```yaml
test_environment:
  unit_tests:
    mocks_required:
      - ltm_retrieve
      - ltm_store
      - scrapegraphai_client

  integration_tests:
    requires:
      - LTM backend (in-memory SQLite)
      - Mocked ScrapeGraphAI responses

  e2e_tests:
    requires:
      - Full LTM backend
      - Real or mocked ScrapeGraphAI (consider vcr/cassette pattern)
      - Test URLs with stable content
```

## Trace References

```
Test design matrix: the_edge_agent/docs/qa/assessments/TEA-BUILTIN-008.7-test-design-20251224.md
P0 tests identified: 8
Dependency: TEA-BUILTIN-010 must be complete first
```
