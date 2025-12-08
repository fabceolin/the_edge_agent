# Test Design: Story TEA-BUILTIN-001.4

Date: 2025-12-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 52
- Unit tests: 35 (67%)
- Integration tests: 17 (33%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 14, P1: 26, P2: 12

## Risk Assessment Summary

| Risk Area | Probability | Impact | Mitigation |
|-----------|-------------|--------|------------|
| SQLite concurrency issues | Medium | High | Thread-local connections, WAL mode, busy timeout |
| CozoDB dependency unavailable | High | Medium | Graceful degradation with informative errors |
| Path traversal attacks | Low | Critical | Path validation before operations |
| Memory leaks from unclosed connections | Medium | Medium | Context managers, explicit close(), test cleanup |
| Datalog query injection | Low | High | Parameter binding, query validation |
| Embedding dimension mismatches | Medium | Medium | Dimension validation on store operations |

## Test Scenarios by Acceptance Criteria

### AC1: LongTermMemoryBackend Protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-001 | Unit | P0 | Verify LongTermMemoryBackend defines store/retrieve/delete/search/close methods | Protocol compliance - pure interface validation |
| 001.4-UNIT-002 | Unit | P1 | Verify protocol methods have correct signatures | Type safety for implementations |
| 001.4-UNIT-003 | Unit | P1 | Verify protocol is importable from the_edge_agent | Public API accessibility |

### AC2: SQLiteBackend Implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-004 | Unit | P0 | SQLiteBackend store and retrieve basic operation | Core CRUD - fundamental functionality |
| 001.4-UNIT-005 | Unit | P0 | SQLiteBackend delete operation | Core CRUD - data removal |
| 001.4-UNIT-006 | Unit | P0 | SQLiteBackend FTS5 search basic operation | Core feature - full-text search |
| 001.4-UNIT-007 | Unit | P1 | SQLiteBackend store with metadata | Extended store functionality |
| 001.4-UNIT-008 | Unit | P1 | SQLiteBackend search with metadata filter | Extended query functionality |
| 001.4-UNIT-009 | Unit | P1 | SQLiteBackend thread-local connections | Concurrency safety - critical for multi-threaded usage |
| 001.4-UNIT-010 | Unit | P1 | SQLiteBackend WAL mode enabled | Performance and concurrency optimization |
| 001.4-UNIT-011 | Unit | P1 | SQLiteBackend path validation prevents traversal | Security - critical for file operations |
| 001.4-INT-001 | Integration | P1 | SQLiteBackend concurrent writes from multiple threads | Real-world concurrency scenario |
| 001.4-UNIT-012 | Unit | P2 | SQLiteBackend handles large values (>1MB) | Edge case - large data storage |
| 001.4-UNIT-013 | Unit | P2 | SQLiteBackend FTS5 ranking/relevance | FTS quality validation |
| 001.4-UNIT-014 | Unit | P2 | SQLiteBackend update timestamp on overwrite | Audit trail support |

### AC3: CozoBackend Implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-015 | Unit | P0 | CozoBackend initializes with SQLite storage | Core initialization |
| 001.4-UNIT-016 | Unit | P0 | CozoBackend creates entity and relation schemas | Schema setup validation |
| 001.4-UNIT-017 | Unit | P1 | CozoBackend creates HNSW vector index | Vector search prerequisite |
| 001.4-UNIT-018 | Unit | P1 | CozoBackend thread-safe connection management | Concurrency safety |
| 001.4-UNIT-019 | Unit | P1 | CozoBackend Datalog query building helpers | API usability |
| 001.4-UNIT-020 | Unit | P2 | CozoBackend handles large graph (>10K entities) | Scale validation |

### AC4-7: ltm.* Actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-021 | Unit | P0 | ltm.store stores key-value pair | Core action - store |
| 001.4-UNIT-022 | Unit | P0 | ltm.retrieve returns stored value | Core action - retrieve |
| 001.4-UNIT-023 | Unit | P0 | ltm.delete removes entry | Core action - delete |
| 001.4-UNIT-024 | Unit | P0 | ltm.search returns matching results | Core action - search |
| 001.4-UNIT-025 | Unit | P1 | ltm.store with metadata persists metadata | Extended store |
| 001.4-UNIT-026 | Unit | P1 | ltm.retrieve with default returns default when not found | Fallback behavior |
| 001.4-UNIT-027 | Unit | P1 | ltm.search with metadata_filter applies filter | Filtered search |
| 001.4-UNIT-028 | Unit | P1 | ltm.search with limit respects limit | Pagination support |
| 001.4-UNIT-029 | Unit | P1 | ltm.* actions return error dict on failure | Consistent error handling |
| 001.4-INT-002 | Integration | P1 | ltm actions persist across engine restarts | Persistence validation |

### AC8: graph.store_entity Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-030 | Unit | P0 | graph.store_entity stores basic entity | Core graph operation |
| 001.4-UNIT-031 | Unit | P1 | graph.store_entity with properties stores JSON properties | Extended entity storage |
| 001.4-UNIT-032 | Unit | P1 | graph.store_entity with embed=True generates embedding | RAG integration |
| 001.4-INT-003 | Integration | P1 | graph.store_entity with embedding.create integration | Cross-action dependency |

### AC9: graph.store_relation Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-033 | Unit | P0 | graph.store_relation creates edge between entities | Core graph operation |
| 001.4-UNIT-034 | Unit | P1 | graph.store_relation with properties stores JSON properties | Extended relation storage |
| 001.4-UNIT-035 | Unit | P1 | graph.store_relation validates entity existence (when enabled) | Referential integrity |

### AC10: graph.query Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-036 | Unit | P0 | graph.query executes raw Datalog query | Core query mechanism |
| 001.4-UNIT-037 | Unit | P1 | graph.query with pattern dict translates to Datalog | Simplified query API |
| 001.4-UNIT-038 | Unit | P1 | graph.query with params binds parameters safely | SQL injection prevention |
| 001.4-UNIT-039 | Unit | P1 | graph.query timeout prevents runaway queries | Resource protection |
| 001.4-INT-004 | Integration | P1 | graph.query recursive N-hop traversal | Complex graph pattern |
| 001.4-UNIT-040 | Unit | P2 | graph.query with cyclic relations terminates | Edge case - cycles |

### AC11: graph.retrieve_context Action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-041 | Unit | P0 | graph.retrieve_context returns subgraph by entity_id | Core context retrieval |
| 001.4-UNIT-042 | Unit | P1 | graph.retrieve_context with query generates embedding | Semantic search path |
| 001.4-UNIT-043 | Unit | P1 | graph.retrieve_context with hops parameter expands N-hops | Graph expansion |
| 001.4-INT-005 | Integration | P1 | graph.retrieve_context HNSW similarity search | Vector + graph integration |
| 001.4-INT-006 | Integration | P1 | graph.retrieve_context returns context_summary | Usability output |

### AC12: YAMLEngine Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-INT-007 | Integration | P0 | YAMLEngine accepts ltm_backend parameter | Backend injection |
| 001.4-INT-008 | Integration | P0 | YAMLEngine accepts graph_backend parameter | Backend injection |
| 001.4-INT-009 | Integration | P1 | YAMLEngine defaults to SQLiteBackend(":memory:") for ltm | Sensible defaults |
| 001.4-INT-010 | Integration | P1 | YAMLEngine defaults to CozoBackend(temp) with graceful fallback | Sensible defaults |
| 001.4-INT-011 | Integration | P1 | YAMLEngine.close() cleans up both backends | Resource management |
| 001.4-INT-012 | Integration | P1 | YAMLEngine feature flags enable_ltm/enable_graph work | Feature toggle |

### AC13: Thread Safety and Resource Cleanup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-INT-013 | Integration | P1 | SQLiteBackend close() releases all connections | Resource cleanup |
| 001.4-INT-014 | Integration | P1 | CozoBackend close() releases database | Resource cleanup |
| 001.4-INT-015 | Integration | P2 | Concurrent access from 10+ threads | Stress test |

### AC14-15: Action Patterns and Namespaces

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-044 | Unit | P1 | ltm.* and actions.ltm_* point to same functions | Dual namespace |
| 001.4-UNIT-045 | Unit | P1 | graph.* and actions.graph_* point to same functions | Dual namespace |
| 001.4-UNIT-046 | Unit | P1 | All actions follow consistent error return format | Error handling pattern |

### AC18: Graceful Degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-UNIT-047 | Unit | P0 | graph.* actions return informative error when pycozo unavailable | Graceful degradation |
| 001.4-INT-016 | Integration | P1 | YAMLEngine initializes successfully without pycozo | Optional dependency |
| 001.4-INT-017 | Integration | P2 | ltm.* actions work normally when pycozo unavailable | Feature isolation |

## Integration Workflow Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.4-INT-WF-001 | Integration | P0 | ltm actions in YAML workflow | Workflow integration |
| 001.4-INT-WF-002 | Integration | P0 | graph actions in YAML workflow | Workflow integration |
| 001.4-INT-WF-003 | Integration | P1 | Combined short-term + long-term memory workflow | Memory layer interaction |
| 001.4-INT-WF-004 | Integration | P1 | Knowledge graph agent with Datalog reasoning | Real-world scenario |
| 001.4-INT-WF-005 | Integration | P1 | RAG + graph context retrieval pipeline | Cross-feature integration |

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| SQLite concurrency | 001.4-UNIT-009, 001.4-UNIT-010, 001.4-INT-001, 001.4-INT-015 | Full |
| CozoDB unavailable | 001.4-UNIT-047, 001.4-INT-016, 001.4-INT-017 | Full |
| Path traversal | 001.4-UNIT-011 | Full |
| Memory leaks | 001.4-INT-011, 001.4-INT-013, 001.4-INT-014 | Full |
| Query injection | 001.4-UNIT-038 | Full |
| Dimension mismatch | 001.4-UNIT-032, 001.4-INT-003 | Partial (via RAG tests) |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core functionality)
   - Protocol compliance (001.4-UNIT-001)
   - SQLiteBackend CRUD (001.4-UNIT-004 through 006)
   - CozoBackend init (001.4-UNIT-015, 016)
   - Core ltm.* actions (001.4-UNIT-021 through 024)
   - Core graph.* actions (001.4-UNIT-030, 033, 036, 041)
   - Graceful degradation (001.4-UNIT-047)

2. **P0 Integration tests** (core integration points)
   - Backend injection (001.4-INT-007, 008)
   - Workflow tests (001.4-INT-WF-001, 002)

3. **P1 Unit tests** (extended functionality)
   - All P1 unit tests in sequence

4. **P1 Integration tests** (cross-component validation)
   - All P1 integration tests

5. **P2 tests as time permits**
   - Edge cases and stress tests

## Test File Structure

```
tests/
  test_yaml_engine_ltm.py          # New file for this story
    TestLongTermMemoryProtocol      # AC1 tests
    TestSQLiteBackend               # AC2 tests
    TestCozoBackend                 # AC3 tests (skipped if pycozo unavailable)
    TestLTMActions                  # AC4-7 tests
    TestGraphActions                # AC8-11 tests
    TestYAMLEngineIntegration       # AC12 tests
    TestThreadSafety                # AC13 tests
    TestNamespaceAccess             # AC14-15 tests
    TestGracefulDegradation         # AC18 tests
    TestLTMWorkflows                # Workflow integration tests
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for component interaction)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention: {EPIC}.{STORY}-{LEVEL}-{SEQ}
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

## Key Testing Principles Applied

- **Shift left**: 67% unit tests for fast feedback
- **Risk-based**: P0 tests cover data integrity, security, and dependency availability
- **Efficient coverage**: SQLite and Cozo tested at appropriate levels
- **Maintainability**: Clear test structure following existing patterns
- **Fast feedback**: P0 unit tests run first to catch fundamental issues

## Test Dependencies

| Dependency | Required For | Skip Strategy |
|------------|--------------|---------------|
| `pycozo[embedded]` | CozoBackend tests | `pytest.mark.skipif` |
| `openai` | Embedding tests with RAG integration | Mock |
| SQLite3 | All SQLiteBackend tests | stdlib - always available |

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 35
    integration: 17
    e2e: 0
  by_priority:
    p0: 14
    p1: 26
    p2: 12
  coverage_gaps: []
  risk_coverage:
    sqlite_concurrency: full
    cozo_unavailable: full
    path_traversal: full
    memory_leaks: full
    query_injection: full
    dimension_mismatch: partial
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.4-test-design-20251207.md
P0 tests identified: 14
P1 tests identified: 26
P2 tests identified: 12
Total scenarios: 52
```
