# Test Design: Story BUG.001

Date: 2026-01-18
Designer: Quinn (Test Architect)

## Summary

**Bug:** HierarchicalLTMBackend YAML Config Mismatch
**Root Cause:** `parse_backend_config()` does not transform nested YAML config (`catalog.url`, `hierarchy.levels`) to flat parameters (`catalog_url`, `hierarchy_levels`) expected by `HierarchicalLTMBackend.__init__()`

## Test Strategy Overview

- Total test scenarios: 48
- Unit tests: 28 (58%)
- Integration tests: 16 (33%)
- E2E tests: 4 (8%)
- Priority distribution: P0: 12, P1: 18, P2: 14, P3: 4

## Test Scenarios by Acceptance Criteria

### AC-1: YAML config with nested blocks correctly instantiates HierarchicalLTMBackend

The documented YAML format with `catalog:`, `storage:`, `hierarchy:` blocks must work.

#### Scenarios

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-UNIT-001   | Unit        | P0       | parse_backend_config transforms `catalog.url` to `catalog_url` | Core transformation logic                         |
| BUG.001-UNIT-002   | Unit        | P0       | parse_backend_config transforms `storage.uri` to `storage_uri` | Core transformation logic                         |
| BUG.001-UNIT-003   | Unit        | P0       | parse_backend_config transforms `hierarchy.levels` to `hierarchy_levels` | Core transformation logic                         |
| BUG.001-UNIT-004   | Unit        | P0       | parse_backend_config transforms `hierarchy.defaults` to `hierarchy_defaults` | Core transformation logic                         |
| BUG.001-UNIT-005   | Unit        | P1       | parse_backend_config extracts `catalog.pool_size` to `pool_size` | Secondary config parameter                        |
| BUG.001-UNIT-006   | Unit        | P1       | parse_backend_config extracts `catalog.lazy` to `lazy` | Secondary config parameter                        |
| BUG.001-UNIT-007   | Unit        | P1       | parse_backend_config transforms `index` block to `index_config` | Optional config block                             |
| BUG.001-UNIT-008   | Unit        | P1       | parse_backend_config transforms `performance` block to `performance_config` | Optional config block                             |
| BUG.001-UNIT-009   | Unit        | P2       | parse_backend_config handles `performance.metadata_cache.ttl_seconds` | Nested performance config                         |
| BUG.001-UNIT-010   | Unit        | P2       | parse_backend_config handles `performance.parallel_reads.threads` | Nested performance config                         |
| BUG.001-INT-001    | Integration | P0       | YAMLEngine loads agent with full A3 hierarchical config | End-to-end config parsing                         |
| BUG.001-INT-002    | Integration | P0       | HierarchicalLTMBackend instantiates from parsed YAML config | Backend creation succeeds                         |
| BUG.001-INT-003    | Integration | P1       | YAMLEngine.ltm_backend property returns HierarchicalLTMBackend | Property access works                             |
| BUG.001-E2E-001    | E2E         | P0       | Load documented CLAUDE.md A3 example YAML and verify backend type | Ensures documentation accuracy                    |

### AC-2: Flat parameter API remains backward-compatible for programmatic use

Direct instantiation with flat parameters must continue to work.

#### Scenarios

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-UNIT-011   | Unit        | P0       | HierarchicalLTMBackend accepts flat parameters directly  | Core API backward compatibility                   |
| BUG.001-UNIT-012   | Unit        | P0       | create_ltm_backend("hierarchical") with flat kwargs works | Factory backward compatibility                    |
| BUG.001-UNIT-013   | Unit        | P1       | Existing tests for HierarchicalLTMBackend continue to pass | Regression prevention                             |
| BUG.001-INT-004    | Integration | P0       | Programmatic creation matches documented examples in docstrings | API contract preserved                            |
| BUG.001-INT-005    | Integration | P1       | Backend from flat params and YAML config behave identically | Functional equivalence                            |

### AC-3: Error message is clear if required config is missing

Missing required parameters should produce actionable error messages.

#### Scenarios

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-UNIT-014   | Unit        | P0       | Missing `catalog.url` raises ValueError with clear message | User-facing error quality                         |
| BUG.001-UNIT-015   | Unit        | P0       | Missing `storage.uri` raises ValueError with clear message | User-facing error quality                         |
| BUG.001-UNIT-016   | Unit        | P0       | Missing `hierarchy.levels` raises ValueError with clear message | User-facing error quality                         |
| BUG.001-UNIT-017   | Unit        | P1       | Error message includes parameter name and expected format | Actionable error                                  |
| BUG.001-UNIT-018   | Unit        | P1       | Error message suggests correct YAML structure            | Helpful error                                     |
| BUG.001-INT-006    | Integration | P1       | YAMLEngine logs warning with config issue details        | Observability for debugging                       |

### AC-4: Unit tests cover both YAML-style and programmatic initialization

Comprehensive test coverage for both initialization paths.

#### Scenarios

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-UNIT-019   | Unit        | P1       | Test YAML config with minimal required fields            | Boundary case                                     |
| BUG.001-UNIT-020   | Unit        | P1       | Test YAML config with all optional fields                | Full config coverage                              |
| BUG.001-UNIT-021   | Unit        | P1       | Test flat params with minimal required fields            | Boundary case                                     |
| BUG.001-UNIT-022   | Unit        | P1       | Test flat params with all optional fields                | Full config coverage                              |
| BUG.001-UNIT-023   | Unit        | P2       | Test environment variable expansion in catalog.url       | ${VAR} substitution                               |
| BUG.001-UNIT-024   | Unit        | P2       | Test environment variable expansion in storage.uri       | ${VAR} substitution                               |
| BUG.001-UNIT-025   | Unit        | P2       | Test environment variable expansion with defaults        | ${VAR:-default} substitution                      |
| BUG.001-INT-007    | Integration | P1       | Test round-trip: YAML → backend → operations → verify    | Full lifecycle test                               |
| BUG.001-INT-008    | Integration | P1       | Test round-trip: flat params → backend → operations → verify | Full lifecycle test                               |

### AC-5: CLAUDE.md documentation matches actual supported config format

Documentation must be accurate after fix is applied.

#### Scenarios

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-INT-009    | Integration | P1       | Parse CLAUDE.md A3 example YAML and instantiate backend  | Doc accuracy verification                         |
| BUG.001-INT-010    | Integration | P2       | Parse CLAUDE.md Python API example and verify equivalence | Doc accuracy verification                         |
| BUG.001-E2E-002    | E2E         | P1       | Full agent YAML from docs works end-to-end               | User experience validation                        |

## Additional Test Scenarios (Non-AC Derived)

### Edge Cases and Error Handling

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-UNIT-026   | Unit        | P2       | Empty hierarchy.levels array raises error                | Validation edge case                              |
| BUG.001-UNIT-027   | Unit        | P2       | Non-string hierarchy level raises error                  | Type validation                                   |
| BUG.001-UNIT-028   | Unit        | P3       | Unknown keys in config are ignored (forward compatibility) | Robustness                                        |
| BUG.001-INT-011    | Integration | P2       | SQLAlchemy connection failure produces clear error       | Dependency failure handling                       |
| BUG.001-INT-012    | Integration | P2       | fsspec storage URI parsing handles edge protocols        | Storage URI variations (file://, gs://, s3://)    |
| BUG.001-INT-013    | Integration | P3       | Invalid catalog.type raises clear error                  | Type validation                                   |

### Backend Registration and Factory

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-INT-014    | Integration | P1       | "hierarchical" is registered in backend registry         | Factory availability                              |
| BUG.001-INT-015    | Integration | P2       | get_registered_backends() includes "hierarchical"        | Discovery API                                     |
| BUG.001-INT-016    | Integration | P3       | check_hierarchical_available() reflects dependencies     | Availability check                                |

### Performance and Lazy Initialization

| ID                 | Level       | Priority | Test                                                     | Justification                                     |
| ------------------ | ----------- | -------- | -------------------------------------------------------- | ------------------------------------------------- |
| BUG.001-E2E-003    | E2E         | P2       | lazy=true defers SQLAlchemy/fsspec init until first use  | Cold start optimization                           |
| BUG.001-E2E-004    | E2E         | P3       | Backend closes cleanly without leaking connections       | Resource cleanup                                  |

## Risk Coverage

| Risk ID   | Risk Description                                    | Mitigated By                                      |
| --------- | --------------------------------------------------- | ------------------------------------------------- |
| RISK-001  | Breaking change to existing programmatic API        | BUG.001-UNIT-011, BUG.001-UNIT-012, BUG.001-INT-004 |
| RISK-002  | YAML config parsing fails silently and falls back   | BUG.001-INT-001, BUG.001-E2E-001                  |
| RISK-003  | Environment variable expansion breaks in nested config | BUG.001-UNIT-023, BUG.001-UNIT-024, BUG.001-UNIT-025 |
| RISK-004  | Documentation remains out of sync with implementation | BUG.001-INT-009, BUG.001-INT-010, BUG.001-E2E-002 |
| RISK-005  | Missing required config silently uses defaults      | BUG.001-UNIT-014 through BUG.001-UNIT-018        |

## Recommended Execution Order

1. **P0 Unit tests** (BUG.001-UNIT-001 through 016) - Core transformation logic
2. **P0 Integration tests** (BUG.001-INT-001, 002, 004) - End-to-end config parsing
3. **P0 E2E test** (BUG.001-E2E-001) - Documentation accuracy
4. **P1 tests** in order - Extended coverage
5. **P2+ tests** as time permits - Edge cases and robustness

## Test File Locations

Tests should be added to:
- `python/tests/test_hierarchical_ltm.py` - Existing backend tests
- `python/tests/test_yaml_engine_ltm.py` - New file for YAML config parsing tests
- `python/tests/test_ltm_config_parsing.py` - New file for `parse_backend_config` tests

## Fixtures Required

### YAML Fixture: `tests/fixtures/hierarchical_ltm_config.yaml`
```yaml
settings:
  ltm:
    backend: hierarchical
    catalog:
      type: sqlalchemy
      url: "sqlite:///:memory:"  # Use in-memory for testing
      pool_size: 5
      lazy: true
    storage:
      uri: "${TEST_LTM_STORAGE:-./test_ltm_data/}"
    hierarchy:
      enabled: true
      levels: [org, project, user, session]
      root_entity:
        type: org
        id: "test_org"
      defaults:
        org: "default_org"
        project: "_unassigned"
    performance:
      metadata_cache:
        enabled: true
        ttl_seconds: 300
      parallel_reads:
        threads: 4
    index:
      format: parquet
      row_group_size: 10000
      compression: zstd
```

### YAML Fixture: `tests/fixtures/hierarchical_ltm_minimal.yaml`
```yaml
settings:
  ltm:
    backend: hierarchical
    catalog:
      url: "sqlite:///:memory:"
    storage:
      uri: "./test_ltm_data/"
    hierarchy:
      levels: [org, user, session]
```

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 48
  by_level:
    unit: 28
    integration: 16
    e2e: 4
  by_priority:
    p0: 12
    p1: 18
    p2: 14
    p3: 4
  coverage_gaps: []
  risks_addressed: 5
  fixtures_required: 2
```

## Quality Checklist

- [x] Every AC has test coverage (AC-1: 14, AC-2: 5, AC-3: 6, AC-4: 10, AC-5: 4)
- [x] Test levels are appropriate (unit for logic, integration for config flow, E2E for user experience)
- [x] No duplicate coverage across levels (each scenario tests different aspect)
- [x] Priorities align with business risk (P0 for breaking changes, P1 for core functionality)
- [x] Test IDs follow naming convention (BUG.001-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent (each can run in isolation)
- [x] Risk mitigations are addressed (5 identified risks covered by 15 tests)

## Key Principles Applied

- **Shift left**: 58% unit tests for fast feedback on transformation logic
- **Risk-based**: P0 tests cover backward compatibility and core functionality
- **Efficient coverage**: Each level tests appropriate scope without overlap
- **Maintainability**: Clear naming, fixtures for reuse, grouped by AC

## Trace References

```text
Test design matrix: docs/qa/assessments/BUG.001-test-design-20260118.md
P0 tests identified: 12
Total scenarios: 48
Acceptance criteria covered: 5/5
```
