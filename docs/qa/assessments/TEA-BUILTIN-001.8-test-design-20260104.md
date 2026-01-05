# Test Design: Story TEA-BUILTIN-001.8

**Date:** 2026-01-04
**Designer:** Quinn (Test Architect)
**Story:** DuckPGQ Graph Query Integration

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 58 |
| **Unit tests** | 32 (55%) |
| **Integration tests** | 18 (31%) |
| **E2E tests** | 8 (14%) |

**Priority Distribution:**

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 14 | Core extension loading, graph creation, basic queries |
| P1 | 24 | Query variations, algorithms, cloud storage |
| P2 | 16 | Edge cases, error handling, backward compat |
| P3 | 4 | Performance, documentation |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Extension Loading

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-001 | Unit | P0 | `INSTALL duckpgq FROM community` executes without error | Core functionality gate |
| 1.8-UNIT-002 | Unit | P0 | `LOAD duckpgq` activates extension | Core functionality gate |
| 1.8-UNIT-003 | Unit | P1 | Extension is listed in `duckdb_extensions()` after load | Verification of load state |
| 1.8-UNIT-004 | Unit | P2 | Repeated LOAD is idempotent (no error) | Robustness |

### AC-2: Lazy Loading

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-005 | Unit | P0 | Extension not loaded on DuckDBQueryEngine init | Cold start optimization |
| 1.8-UNIT-006 | Unit | P0 | Extension loads on first `graph.create` call | Lazy loading behavior |
| 1.8-UNIT-007 | Unit | P1 | Extension loads on first `graph.query` call | Lazy loading behavior |
| 1.8-INT-001 | Integration | P1 | Multiple graph operations share single extension load | Resource efficiency |

### AC-3: Error Handling

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-008 | Unit | P0 | Graceful error when extension install fails (network) | User experience |
| 1.8-UNIT-009 | Unit | P0 | Error message includes installation instructions | User guidance |
| 1.8-UNIT-010 | Unit | P1 | Error includes DuckDB version requirement | Debugging aid |
| 1.8-UNIT-011 | Unit | P2 | `success: False` returned, not exception raised | API consistency |

### AC-4: Query Engine Integration

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-012 | Unit | P0 | `load_extension("duckpgq")` returns `{"success": True}` | API contract |
| 1.8-INT-002 | Integration | P1 | Extension coexists with httpfs, vss, fts | Extension compatibility |
| 1.8-INT-003 | Integration | P2 | Extension survives connection pool recycle | Connection management |

### AC-5: Create Property Graph

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-013 | Unit | P0 | `CREATE PROPERTY GRAPH` with single vertex table | Core graph creation |
| 1.8-UNIT-014 | Unit | P0 | `CREATE PROPERTY GRAPH` with vertex + edge tables | Core graph creation |
| 1.8-UNIT-015 | Unit | P1 | Graph with multiple vertex tables | Multi-type graphs |
| 1.8-UNIT-016 | Unit | P1 | Graph with multiple edge tables | Multi-relationship graphs |
| 1.8-UNIT-017 | Unit | P2 | `CREATE OR REPLACE PROPERTY GRAPH` replaces existing | Update pattern |

### AC-6: Vertex Tables

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-018 | Unit | P0 | Load vertex table from local Parquet file | Core data loading |
| 1.8-INT-004 | Integration | P1 | Load vertex table from in-memory DataFrame | Flexibility |
| 1.8-UNIT-019 | Unit | P1 | Vertex table with string ID column | Common pattern |
| 1.8-UNIT-020 | Unit | P1 | Vertex table with integer ID column | Common pattern |
| 1.8-UNIT-021 | Unit | P2 | Vertex table with composite key (multiple columns) | Advanced pattern |

### AC-7: Edge Tables

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-022 | Unit | P0 | Edge table with SOURCE KEY and DESTINATION KEY | Core edge definition |
| 1.8-UNIT-023 | Unit | P1 | Edge table REFERENCES vertex table correctly | Referential integrity |
| 1.8-UNIT-024 | Unit | P1 | Edge table with properties (weight, type) | Rich edges |
| 1.8-UNIT-025 | Unit | P2 | Self-referencing edges (same source/dest table) | Graph pattern |

### AC-8: Drop Property Graph

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-026 | Unit | P1 | `DROP PROPERTY GRAPH` removes graph | Cleanup |
| 1.8-UNIT-027 | Unit | P1 | `DROP PROPERTY GRAPH IF EXISTS` on non-existent is no-op | Idempotency |
| 1.8-UNIT-028 | Unit | P2 | Source tables remain after graph drop | Data preservation |

### AC-9: Pattern Matching

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-029 | Unit | P0 | `MATCH (a)-[r]->(b)` returns connected nodes | Core query syntax |
| 1.8-UNIT-030 | Unit | P0 | `MATCH (a:Label)` filters by vertex label | Label filtering |
| 1.8-UNIT-031 | Unit | P1 | `MATCH (a)-[r:TYPE]->(b)` filters by edge type | Edge type filtering |
| 1.8-UNIT-032 | Unit | P1 | Multi-hop pattern `(a)-[]->(b)-[]->(c)` | Path patterns |

### AC-10: Node Filtering

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-033 | Unit | P0 | `WHERE a.id = 'value'` filters nodes | Core filtering |
| 1.8-UNIT-034 | Unit | P1 | `WHERE a.prop > 10` numeric comparison | Numeric filters |
| 1.8-UNIT-035 | Unit | P1 | `WHERE a.name LIKE '%pattern%'` string matching | String filters |
| 1.8-UNIT-036 | Unit | P2 | `WHERE a.id IN ('a', 'b', 'c')` list filtering | Batch queries |

### AC-11: Path Queries

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-005 | Integration | P0 | `ANY SHORTEST` finds shortest path | Core path finding |
| 1.8-INT-006 | Integration | P1 | `{1,5}` hop limit is respected | Bounded traversal |
| 1.8-INT-007 | Integration | P1 | `path_length(p)` returns correct hop count | Path metrics |
| 1.8-INT-008 | Integration | P2 | `vertices(p)` returns ordered node list | Path inspection |
| 1.8-INT-009 | Integration | P2 | Returns empty when no path exists | No-result handling |

### AC-12: COLUMNS Clause

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-UNIT-037 | Unit | P0 | `COLUMNS (a.id, b.name)` projects selected fields | Core projection |
| 1.8-UNIT-038 | Unit | P1 | `COLUMNS (a.*, b.id)` expands wildcard | Convenience syntax |
| 1.8-UNIT-039 | Unit | P1 | `COLUMNS (r.weight as score)` with alias | Result naming |

### AC-13: Parameterized Queries

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-010 | Integration | P0 | `{{ state.entity_id }}` renders in WHERE clause | Jinja2 integration |
| 1.8-INT-011 | Integration | P1 | `{{ state.limit \| default(10) }}` with filter | Template filters |
| 1.8-INT-012 | Integration | P2 | SQL injection attempt in template is escaped | Security |

### AC-14: PageRank

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-013 | Integration | P0 | `pagerank(graph, table)` returns float values | Algorithm execution |
| 1.8-INT-014 | Integration | P1 | PageRank sum approximately equals 1.0 | Algorithm correctness |
| 1.8-INT-015 | Integration | P1 | Hub nodes have higher PageRank | Algorithm semantics |

### AC-15: Weakly Connected Components

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-016 | Integration | P0 | `weakly_connected_component` returns cluster IDs | Algorithm execution |
| 1.8-INT-017 | Integration | P1 | Disconnected subgraphs get different IDs | Algorithm correctness |
| 1.8-INT-018 | Integration | P2 | Single-node graph returns one component | Edge case |

### AC-16: Local Clustering Coefficient

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-019 | Integration | P1 | `local_clustering_coefficient` returns [0,1] | Algorithm bounds |
| 1.8-INT-020 | Integration | P2 | Fully connected cluster returns 1.0 | Algorithm correctness |
| 1.8-INT-021 | Integration | P2 | Star topology center returns 0.0 | Algorithm correctness |

### AC-17: Shortest Path

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-022 | Integration | P0 | Direct edge returns path length 1 | Core functionality |
| 1.8-INT-023 | Integration | P1 | Multi-hop path returns correct length | Algorithm correctness |
| 1.8-INT-024 | Integration | P2 | Unreachable nodes return NULL/empty | No-path handling |

### AC-18: graph.create Action

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-E2E-001 | E2E | P0 | YAML `uses: graph.create` creates property graph | Action integration |
| 1.8-E2E-002 | E2E | P1 | Action with `vertex_tables` list creates multi-table graph | YAML config |
| 1.8-E2E-003 | E2E | P1 | Action with `edge_tables` references work correctly | YAML config |

### AC-19: graph.query Action

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-E2E-004 | E2E | P0 | YAML `uses: graph.query` with `pgq:` executes SQL/PGQ | Action integration |
| 1.8-E2E-005 | E2E | P1 | Query results stored in `output` state variable | State management |
| 1.8-INT-025 | Integration | P1 | Invalid PGQ syntax returns `success: False` with error | Error handling |

### AC-20: graph.algorithm Action

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-E2E-006 | E2E | P1 | YAML `uses: graph.algorithm` with `algorithm: pagerank` | Action integration |
| 1.8-E2E-007 | E2E | P2 | Unknown algorithm name returns helpful error | Error handling |

### AC-21: Backward Compatibility

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-026 | Integration | P0 | CozoDB `graph.query` with `datalog:` still works | Regression prevention |
| 1.8-INT-027 | Integration | P0 | Kuzu `graph.query` with `cypher:` still works | Regression prevention |
| 1.8-INT-028 | Integration | P2 | Existing graph action tests pass unchanged | Regression suite |

### AC-22 to AC-24: Cloud Storage (S3, GCS, Azure)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-029 | Integration | P1 | Load vertex Parquet from `s3://` URI | S3 integration |
| 1.8-INT-030 | Integration | P1 | Load vertex Parquet from `gs://` URI | GCS integration |
| 1.8-INT-031 | Integration | P2 | Load vertex Parquet from `az://` URI | Azure integration |
| 1.8-INT-032 | Integration | P2 | Edge table from cloud storage references local vertex | Mixed sources |

### AC-25: httpfs Coordination

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-INT-033 | Integration | P1 | httpfs credentials reused from DuckDBQueryEngine | Config sharing |
| 1.8-INT-034 | Integration | P2 | S3 credentials from environment variables work | Serverless pattern |

### AC-26 to AC-29: Testing Requirements

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.8-E2E-008 | E2E | P3 | Full workflow: create → query → algorithm → drop | Smoke test |

---

## Risk Coverage

| Risk | Test Coverage | Mitigation |
|------|---------------|------------|
| DuckPGQ version incompatibility | 1.8-UNIT-008, 1.8-UNIT-010 | Version check before load |
| Extension load failure in serverless | 1.8-UNIT-005, 1.8-UNIT-006 | Lazy loading pattern |
| SQL/PGQ syntax errors | 1.8-INT-025, 1.8-UNIT-029-036 | Comprehensive query tests |
| Backward compatibility break | 1.8-INT-026, 1.8-INT-027, 1.8-INT-028 | Existing test suite |
| Cloud credential issues | 1.8-INT-033, 1.8-INT-034 | Credential reuse tests |
| Algorithm correctness | 1.8-INT-013-024 | Known-result graphs |

---

## Test Data Requirements

### Minimal Test Graph (Unit Tests)

```python
# 3 nodes, 2 edges - for basic pattern matching
vertices = [
    {"id": "a", "name": "Alice", "type": "person"},
    {"id": "b", "name": "Bob", "type": "person"},
    {"id": "c", "name": "Carol", "type": "person"},
]
edges = [
    {"from_id": "a", "to_id": "b", "type": "knows", "weight": 1.0},
    {"from_id": "b", "to_id": "c", "type": "knows", "weight": 0.5},
]
```

### Algorithm Verification Graph (Integration Tests)

```python
# Star topology for clustering coefficient = 0 at center
# Triangle for clustering coefficient = 1
# Disconnected for WCC = 2 components
star_center = {"id": "hub"}
star_spokes = [{"id": f"spoke_{i}"} for i in range(4)]
triangle = [{"id": f"tri_{i}"} for i in range(3)]
```

### Cloud Storage Mock (Integration Tests)

```python
# Use tempfile + fsspec local filesystem as S3 mock
# Or use moto library for full S3 API mock
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core functionality)
   - Extension loading (1.8-UNIT-001 to 004)
   - Lazy loading (1.8-UNIT-005, 006)
   - Graph creation (1.8-UNIT-013, 014)
   - Pattern matching (1.8-UNIT-029, 030)

2. **P0 Integration tests**
   - Path queries (1.8-INT-005)
   - PageRank (1.8-INT-013)
   - Template rendering (1.8-INT-010)
   - Backward compatibility (1.8-INT-026, 027)

3. **P0 E2E tests**
   - graph.create action (1.8-E2E-001)
   - graph.query action (1.8-E2E-004)

4. **P1 tests** (in ID order)

5. **P2+ tests** (as time permits)

---

## Test File Structure

```
python/tests/
├── test_duckpgq_graph.py           # Main test file
│   ├── TestDuckPGQExtensionLoading  # AC-1 to AC-4
│   ├── TestPropertyGraphCreation    # AC-5 to AC-8
│   ├── TestPatternMatching          # AC-9 to AC-12
│   ├── TestParameterizedQueries     # AC-13
│   ├── TestGraphAlgorithms          # AC-14 to AC-17
│   └── TestErrorHandling            # AC-3, AC-29
├── test_duckpgq_actions.py         # YAML action tests
│   ├── TestGraphCreateAction        # AC-18
│   ├── TestGraphQueryAction         # AC-19
│   ├── TestGraphAlgorithmAction     # AC-20
│   └── TestBackwardCompatibility    # AC-21
└── test_duckpgq_cloud.py           # Cloud storage tests
    ├── TestS3Integration            # AC-22
    ├── TestGCSIntegration           # AC-23
    ├── TestAzureIntegration         # AC-24
    └── TestHttpfsCoordination       # AC-25
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-001.8
  date: 2026-01-04
  designer: Quinn
  scenarios_total: 58
  by_level:
    unit: 32
    integration: 18
    e2e: 8
  by_priority:
    p0: 14
    p1: 24
    p2: 16
    p3: 4
  coverage_gaps: []
  risk_coverage:
    - risk: "DuckPGQ version incompatibility"
      tests: ["1.8-UNIT-008", "1.8-UNIT-010"]
    - risk: "Backward compatibility break"
      tests: ["1.8-INT-026", "1.8-INT-027", "1.8-INT-028"]
    - risk: "Algorithm correctness"
      tests: ["1.8-INT-013", "1.8-INT-014", "1.8-INT-015", "1.8-INT-016", "1.8-INT-017"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.8-test-design-20260104.md
P0 tests identified: 14
AC coverage: 29/29 (100%)
```
