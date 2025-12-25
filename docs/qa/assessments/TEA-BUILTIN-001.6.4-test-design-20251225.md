# Test Design: Story TEA-BUILTIN-001.6.4 - Integration & Testing

**Date:** 2024-12-25
**Designer:** Quinn (Test Architect)
**Story:** TEA-BUILTIN-001.6.4 Integration & Testing
**Parent Epic:** TEA-BUILTIN-001.6 DuckDB Long-Term Memory Backend with DuckLake Catalog

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 57 |
| **Unit Tests** | 22 (39%) |
| **Integration Tests** | 28 (49%) |
| **E2E Tests** | 7 (12%) |
| **Priority Distribution** | P0: 18, P1: 24, P2: 12, P3: 3 |

### Test Level Rationale

This story is heavily integration-focused because it centers on:
1. Factory patterns connecting multiple catalog backends
2. YAML configuration parsing with environment variable expansion
3. Cross-module compatibility (cache.wrap integration)
4. Documentation accuracy verification

Unit tests validate pure logic (env var parsing, config validation). Integration tests dominate due to the multi-component nature of factory registration and backend instantiation. E2E tests cover critical documentation accuracy and migration path verification.

---

## Test Scenarios by Acceptance Criteria

### AC-1: LTM Backend Factory

> `create_ltm_backend("duckdb", ...)` creates `DuckDBLTMBackend`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-001 | Unit | P0 | Factory returns correct type for "duckdb" | Type safety validation |
| 001.6.4-UNIT-002 | Unit | P0 | Factory raises ValueError for unknown backend | Error handling for invalid input |
| 001.6.4-INT-001 | Integration | P0 | Created backend has functional catalog attached | Multi-component wiring verification |
| 001.6.4-INT-002 | Integration | P1 | Backend instance exposes correct storage URI | Configuration propagation |

### AC-2: Catalog Backend Factory

> `create_catalog_backend(type, ...)` creates appropriate catalog

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-003 | Unit | P0 | Factory creates SQLiteCatalog for "sqlite" | Registry correctness |
| 001.6.4-UNIT-004 | Unit | P0 | Factory creates FirestoreCatalog for "firestore" | Registry correctness |
| 001.6.4-UNIT-005 | Unit | P0 | Factory creates PostgresCatalog for "postgres" | Registry correctness |
| 001.6.4-UNIT-006 | Unit | P0 | Factory creates SupabaseCatalog for "supabase" | Registry correctness |
| 001.6.4-UNIT-007 | Unit | P1 | Factory raises ValueError for unknown catalog type | Error boundary |
| 001.6.4-INT-003 | Integration | P1 | Each catalog type implements CatalogBackend protocol | Contract compliance |

### AC-3: YAML Config Parsing

> Factory parses nested YAML configuration

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-008 | Unit | P0 | parse_ltm_config extracts backend type correctly | Configuration parsing core |
| 001.6.4-UNIT-009 | Unit | P1 | Nested catalog config passed to catalog factory | Config delegation |
| 001.6.4-UNIT-010 | Unit | P1 | Nested storage config sets storage_uri | Config mapping |
| 001.6.4-INT-004 | Integration | P0 | Full YAML config creates working DuckDB backend | End-to-end config flow |
| 001.6.4-INT-005 | Integration | P1 | Missing optional fields use sensible defaults | Default value propagation |

### AC-4: Environment Variables

> Config supports `${ENV_VAR}` substitution

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-011 | Unit | P0 | expand_env_vars replaces ${VAR} with env value | Core substitution logic |
| 001.6.4-UNIT-012 | Unit | P0 | expand_env_vars applies ${VAR:-default} fallback | Default value handling |
| 001.6.4-UNIT-013 | Unit | P1 | Nested dict/list structures are recursively expanded | Deep expansion |
| 001.6.4-UNIT-014 | Unit | P1 | Missing env var without default returns empty string | Edge case handling |
| 001.6.4-INT-006 | Integration | P0 | Full config with env vars creates working backend | Integration with factory |

### AC-5: Default Catalog

> If no catalog specified, defaults to SQLite (`:memory:`)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-015 | Unit | P0 | create_ltm_backend without catalog uses sqlite default | Default value correctness |
| 001.6.4-INT-007 | Integration | P1 | Default catalog is functional in-memory SQLite | Default usability |

### AC-6 to AC-9: YAML Configuration Documentation

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-E2E-001 | E2E | P1 | YAML_REFERENCE.md full config example is valid YAML | Documentation accuracy |
| 001.6.4-E2E-002 | E2E | P1 | Minimal config example creates working backend | Minimal path validation |
| 001.6.4-INT-008 | Integration | P1 | inline_threshold config affects inlining behavior | Config effectiveness |

### AC-10 to AC-12: Cache Integration

> Works with TEA-BUILTIN-010 cache.wrap action

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-009 | Integration | P0 | cache.wrap stores via DuckDB backend | Cross-feature integration |
| 001.6.4-INT-010 | Integration | P0 | cache.wrap retrieves cached value (cache hit) | Core cache behavior |
| 001.6.4-INT-011 | Integration | P0 | cache.wrap executes on cache miss | Cache miss flow |
| 001.6.4-INT-012 | Integration | P1 | No code changes needed in cache.wrap | API stability |
| 001.6.4-INT-013 | Integration | P1 | cache.wrap works with each catalog backend type | Backend agnosticism |

### AC-13 to AC-16: Documentation

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-E2E-003 | E2E | P2 | CLAUDE.md contains backend selection guide section | Doc completeness |
| 001.6.4-E2E-004 | E2E | P2 | YAML_REFERENCE.md contains LTM configuration section | Doc completeness |
| 001.6.4-E2E-005 | E2E | P2 | Migration guide exists and covers sqlite migration | Migration path documented |
| 001.6.4-E2E-006 | E2E | P2 | Troubleshooting section exists | Support documentation |

### AC-17: Unit Test Coverage

> >90% coverage for all modules

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-UNIT-016 | Unit | P1 | memory/__init__.py factory functions >90% coverage | Coverage gate |
| 001.6.4-UNIT-017 | Unit | P1 | yaml_engine.py env expansion >90% coverage | Coverage gate |
| 001.6.4-UNIT-018 | Unit | P2 | catalog_sqlite.py >90% coverage | Coverage gate |

### AC-18: Catalog Backend Tests

> Each catalog backend has comprehensive tests

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-014 | Integration | P1 | SQLiteCatalog track/get/delete/list works | Backend completeness |
| 001.6.4-INT-015 | Integration | P1 | FirestoreCatalog track/get/delete/list works (emulator) | Backend completeness |
| 001.6.4-INT-016 | Integration | P1 | PostgresCatalog track/get/delete/list works (Docker) | Backend completeness |
| 001.6.4-INT-017 | Integration | P1 | SupabaseCatalog track/get/delete/list works (mock) | Backend completeness |

### AC-19: Integration Tests

> Full flow tests with each catalog

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-018 | Integration | P0 | Full store/retrieve/delete with SQLiteCatalog | Critical path |
| 001.6.4-INT-019 | Integration | P1 | Full store/retrieve/delete with FirestoreCatalog | Backend coverage |
| 001.6.4-INT-020 | Integration | P1 | Full store/retrieve/delete with PostgresCatalog | Backend coverage |
| 001.6.4-INT-021 | Integration | P1 | Full store/retrieve/delete with SupabaseCatalog | Backend coverage |

### AC-20: Firebase Emulator Tests

> FirestoreCatalog tested with emulator

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-022 | Integration | P1 | Firestore emulator connection established | Emulator setup |
| 001.6.4-INT-023 | Integration | P1 | CRUD operations work against emulator | Realistic testing |
| 001.6.4-INT-024 | Integration | P2 | Collection prefix isolation works | Multi-tenant safety |

### AC-21: Cache Integration Tests

> Tests with cache.wrap

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-025 | Integration | P0 | cache.wrap + DuckDB + SQLite catalog works | Primary use case |
| 001.6.4-INT-026 | Integration | P1 | cache.wrap + DuckDB + Firestore catalog works | GCP use case |
| 001.6.4-INT-027 | Integration | P2 | cache.wrap cleanup with DuckDB backend works | Resource management |

### AC-22: Performance Tests

> Benchmark tests for cold start and operations

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.6.4-INT-028 | Integration | P2 | Cold start time < 500ms (SQLite catalog) | Performance gate |
| 001.6.4-UNIT-019 | Unit | P2 | store operation < 100ms for small data | Latency baseline |
| 001.6.4-UNIT-020 | Unit | P2 | retrieve operation < 50ms for inlined data | Latency baseline |
| 001.6.4-UNIT-021 | Unit | P3 | Batch operations throughput > 100 ops/sec | Throughput baseline |
| 001.6.4-UNIT-022 | Unit | P3 | Memory usage < 50MB for 1000 entries | Resource baseline |
| 001.6.4-E2E-007 | E2E | P3 | Performance benchmarks documented in docs | Documentation gate |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| **Factory returns wrong backend type** | 001.6.4-UNIT-001, 001.6.4-UNIT-003-006 |
| **Env var expansion fails silently** | 001.6.4-UNIT-011-014, 001.6.4-INT-006 |
| **cache.wrap breaks with DuckDB** | 001.6.4-INT-009-013, 001.6.4-INT-025-026 |
| **Documentation examples don't work** | 001.6.4-E2E-001, 001.6.4-E2E-002 |
| **Default catalog unusable** | 001.6.4-UNIT-015, 001.6.4-INT-007 |
| **Firestore emulator tests flaky** | 001.6.4-INT-022-024 (explicit emulator checks) |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit + Integration)
1. 001.6.4-UNIT-001 through 001.6.4-UNIT-007 (factory correctness)
2. 001.6.4-UNIT-008, 001.6.4-UNIT-011-012 (config parsing core)
3. 001.6.4-UNIT-015 (default catalog)
4. 001.6.4-INT-001, 001.6.4-INT-004, 001.6.4-INT-006-007 (factory integration)
5. 001.6.4-INT-009-011, 001.6.4-INT-018 (cache + primary flow)

### Phase 2: Core Coverage (P1)
1. 001.6.4-UNIT-009-010, 001.6.4-UNIT-013-014 (config edge cases)
2. 001.6.4-INT-002-003, 001.6.4-INT-005, 001.6.4-INT-008 (config integration)
3. 001.6.4-INT-012-017 (backend coverage)
4. 001.6.4-INT-019-023, 001.6.4-INT-025-026 (catalog + emulator)
5. 001.6.4-E2E-001-002 (documentation accuracy)

### Phase 3: Extended (P2)
1. 001.6.4-UNIT-016-018 (coverage gates)
2. 001.6.4-INT-024, 001.6.4-INT-027-028 (edge cases + performance)
3. 001.6.4-UNIT-019-020 (latency baselines)
4. 001.6.4-E2E-003-006 (documentation completeness)

### Phase 4: Nice-to-Have (P3)
1. 001.6.4-UNIT-021-022 (throughput + memory)
2. 001.6.4-E2E-007 (performance docs)

---

## Test Environment Requirements

### Unit Tests
- No external dependencies
- Environment variable mocking via `monkeypatch` or `os.environ`

### Integration Tests
- **SQLiteCatalog**: In-memory (`:memory:`)
- **FirestoreCatalog**: Firebase Emulator (`FIRESTORE_EMULATOR_HOST`)
- **PostgresCatalog**: Docker container (postgres:15-alpine)
- **SupabaseCatalog**: Mock responses or Supabase local dev

### E2E Tests
- Full YAML agent execution
- Documentation file parsing
- Actual file existence checks

---

## Quality Checklist

- [x] Every AC has at least one test (22 ACs mapped)
- [x] Test levels are appropriate (no over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-001.6.4
  scenarios_total: 57
  by_level:
    unit: 22
    integration: 28
    e2e: 7
  by_priority:
    p0: 18
    p1: 24
    p2: 12
    p3: 3
  coverage_gaps: []
  special_requirements:
    - Firebase Emulator for AC-20
    - Docker PostgreSQL for AC-19
    - Mock/local Supabase for AC-18
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.6.4-test-design-20251225.md
P0 tests identified: 18
AC coverage: 22/22 (100%)
```
