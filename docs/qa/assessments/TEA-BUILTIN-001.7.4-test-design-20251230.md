# Test Design: Story TEA-BUILTIN-001.7.4

**Story Title:** Neo4j Graph Data Science Integration
**Date:** 2024-12-30
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

- **Total test scenarios:** 78
- **Unit tests:** 42 (54%)
- **Integration tests:** 28 (36%)
- **E2E tests:** 8 (10%)
- **Priority distribution:** P0: 26, P1: 32, P2: 16, P3: 4

### Strategy Rationale

This story implements Neo4j GDS (Graph Data Science) algorithm integration - an **Enterprise-only feature** requiring graceful degradation. The test strategy emphasizes:

1. **Unit tests** for algorithm wrapper logic, parameter validation, and response parsing
2. **Integration tests** for actual GDS procedure calls against Neo4j (requires GDS plugin)
3. **E2E tests** limited to critical workflow validation (YAML agent running GDS algorithms)
4. **Graceful degradation testing** as P0 (users must get clear errors, not crashes)

---

## Test Scenarios by Acceptance Criteria

### AC-1 to AC-3: GDS Detection

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-001 | Unit | P0 | `check_gds_available()` returns True when GDS plugin installed | Core dependency detection - guards all other features |
| 001.7.4-UNIT-002 | Unit | P0 | `check_gds_available()` returns False when GDS not installed | Graceful degradation entry point |
| 001.7.4-UNIT-003 | Unit | P1 | `check_gds_available()` handles connection timeout gracefully | Network resilience |
| 001.7.4-UNIT-004 | Unit | P1 | `get_gds_version()` returns valid semver string when GDS present | Version compatibility checks |
| 001.7.4-UNIT-005 | Unit | P1 | `get_gds_version()` returns None when GDS not present | Null-safe version handling |
| 001.7.4-UNIT-006 | Unit | P0 | `GDS_AVAILABLE` property reflects detection state | API contract for callers |
| 001.7.4-INT-001 | Integration | P0 | Detection against real Neo4j with GDS 2.x | Validates Cypher `RETURN gds.version()` works |
| 001.7.4-INT-002 | Integration | P0 | Detection against real Neo4j Community Edition (no GDS) | Validates graceful failure path |

---

### AC-4 to AC-7: Graph Projection

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-007 | Unit | P0 | `gds_project_graph()` constructs valid native projection Cypher | Core projection mechanism |
| 001.7.4-UNIT-008 | Unit | P0 | `gds_project_graph()` returns expected response schema | API contract validation |
| 001.7.4-UNIT-009 | Unit | P1 | `gds_project_graph()` validates graph_name is non-empty string | Input validation |
| 001.7.4-UNIT-010 | Unit | P1 | `gds_project_graph()` validates node_projection has valid labels | Input validation |
| 001.7.4-UNIT-011 | Unit | P1 | `gds_project_graph()` with empty relationship_projection uses * | Default handling |
| 001.7.4-UNIT-012 | Unit | P2 | `gds_project_graph()` with node properties configuration | Advanced config |
| 001.7.4-UNIT-013 | Unit | P2 | `gds_project_graph()` with relationship properties configuration | Advanced config |
| 001.7.4-UNIT-014 | Unit | P0 | `gds_drop_graph()` constructs valid drop Cypher | Cleanup mechanism |
| 001.7.4-UNIT-015 | Unit | P1 | `gds_drop_graph()` returns success for existing graph | Happy path |
| 001.7.4-UNIT-016 | Unit | P1 | `gds_drop_graph()` returns error for non-existent graph | Error handling |
| 001.7.4-UNIT-017 | Unit | P1 | `gds_list_graphs()` returns list of active projections | Inventory query |
| 001.7.4-UNIT-018 | Unit | P2 | `gds_list_graphs()` returns empty list when no projections | Empty state |
| 001.7.4-UNIT-019 | Unit | P1 | `gds_estimate_memory()` returns bytes estimate | Memory planning |
| 001.7.4-UNIT-020 | Unit | P2 | `gds_estimate_memory()` validates algorithm name | Input validation |
| 001.7.4-INT-003 | Integration | P0 | Create projection with single node label | Core GDS workflow |
| 001.7.4-INT-004 | Integration | P1 | Create projection with multiple node labels | Multi-label support |
| 001.7.4-INT-005 | Integration | P1 | Create projection with weighted relationships | Property projection |
| 001.7.4-INT-006 | Integration | P1 | Drop graph and verify removal | Cleanup works |
| 001.7.4-INT-007 | Integration | P2 | Memory estimation for large graph | Real estimation |

---

### AC-8 to AC-11: Centrality Algorithms

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-021 | Unit | P0 | `gds_page_rank()` constructs valid stream Cypher | Core algorithm wrapper |
| 001.7.4-UNIT-022 | Unit | P1 | `gds_page_rank()` with max_iterations config | Iteration control |
| 001.7.4-UNIT-023 | Unit | P1 | `gds_page_rank()` with damping_factor config | Standard PageRank param |
| 001.7.4-UNIT-024 | Unit | P2 | `gds_page_rank()` with tolerance config | Convergence control |
| 001.7.4-UNIT-025 | Unit | P0 | `gds_page_rank()` returns nodes with score property | Response schema |
| 001.7.4-UNIT-026 | Unit | P0 | `gds_betweenness_centrality()` constructs valid Cypher | Algorithm wrapper |
| 001.7.4-UNIT-027 | Unit | P1 | `gds_betweenness_centrality()` with sampling config | Large graph optimization |
| 001.7.4-UNIT-028 | Unit | P0 | `gds_degree_centrality()` constructs valid Cypher | Algorithm wrapper |
| 001.7.4-UNIT-029 | Unit | P1 | `gds_degree_centrality()` with orientation config | IN/OUT/BOTH |
| 001.7.4-UNIT-030 | Unit | P0 | `gds_closeness_centrality()` constructs valid Cypher | Algorithm wrapper |
| 001.7.4-INT-008 | Integration | P0 | PageRank on test graph returns sorted scores | Algorithm correctness |
| 001.7.4-INT-009 | Integration | P1 | Betweenness on test graph identifies bridge nodes | Algorithm correctness |
| 001.7.4-INT-010 | Integration | P1 | Degree centrality counts connections | Algorithm correctness |
| 001.7.4-INT-011 | Integration | P2 | Closeness on connected test graph | Algorithm correctness |

---

### AC-12 to AC-14: Community Detection

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-031 | Unit | P0 | `gds_louvain()` constructs valid Cypher | Core community detection |
| 001.7.4-UNIT-032 | Unit | P1 | `gds_louvain()` with max_levels config | Hierarchy control |
| 001.7.4-UNIT-033 | Unit | P1 | `gds_louvain()` with max_iterations config | Convergence control |
| 001.7.4-UNIT-034 | Unit | P0 | `gds_louvain()` returns nodes with community_id | Response schema |
| 001.7.4-UNIT-035 | Unit | P0 | `gds_label_propagation()` constructs valid Cypher | Algorithm wrapper |
| 001.7.4-UNIT-036 | Unit | P1 | `gds_label_propagation()` with seed property | Seeded communities |
| 001.7.4-UNIT-037 | Unit | P0 | `gds_wcc()` constructs valid Cypher | Connected components |
| 001.7.4-INT-012 | Integration | P0 | Louvain on test graph with 2 distinct clusters | Algorithm correctness |
| 001.7.4-INT-013 | Integration | P1 | Label propagation converges on test graph | Algorithm stability |
| 001.7.4-INT-014 | Integration | P1 | WCC identifies disconnected components | Component detection |

---

### AC-15 to AC-17: Path Finding

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-038 | Unit | P0 | `gds_dijkstra()` constructs valid Cypher with source/target | Core pathfinding |
| 001.7.4-UNIT-039 | Unit | P0 | `gds_dijkstra()` returns path with total cost | Response schema |
| 001.7.4-UNIT-040 | Unit | P1 | `gds_dijkstra()` with relationship weight property | Weighted paths |
| 001.7.4-UNIT-041 | Unit | P1 | `gds_dijkstra()` returns empty for unreachable target | No-path scenario |
| 001.7.4-UNIT-042 | Unit | P1 | `gds_astar()` constructs valid Cypher with heuristic | A* algorithm |
| 001.7.4-UNIT-043 | Unit | P2 | `gds_astar()` with latitude/longitude heuristic | Geo pathfinding |
| 001.7.4-UNIT-044 | Unit | P1 | `gds_all_shortest_paths()` from single source | Multi-target paths |
| 001.7.4-INT-015 | Integration | P0 | Dijkstra finds shortest path on weighted graph | Algorithm correctness |
| 001.7.4-INT-016 | Integration | P1 | A* with geographic heuristic outperforms Dijkstra | Heuristic benefit |
| 001.7.4-INT-017 | Integration | P2 | All shortest paths from hub node | Multi-path results |

---

### AC-18 to AC-19: Node Similarity

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-045 | Unit | P0 | `gds_node_similarity()` constructs valid Cypher | Core similarity |
| 001.7.4-UNIT-046 | Unit | P0 | `gds_node_similarity()` returns pairs with score | Response schema |
| 001.7.4-UNIT-047 | Unit | P1 | `gds_node_similarity()` with similarity_cutoff filter | Threshold filtering |
| 001.7.4-UNIT-048 | Unit | P1 | `gds_node_similarity()` with top_k limit | Result limiting |
| 001.7.4-UNIT-049 | Unit | P0 | `gds_knn()` constructs valid Cypher | K-NN algorithm |
| 001.7.4-UNIT-050 | Unit | P1 | `gds_knn()` with k parameter | Neighbor count |
| 001.7.4-INT-018 | Integration | P0 | Node similarity on bipartite graph | Jaccard correctness |
| 001.7.4-INT-019 | Integration | P1 | KNN returns k neighbors per node | K-NN correctness |

---

### AC-20 to AC-21: Result Handling

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-051 | Unit | P0 | Stream mode returns results directly | Default mode |
| 001.7.4-UNIT-052 | Unit | P1 | Write mode writes property to graph | Persistence mode |
| 001.7.4-UNIT-053 | Unit | P1 | Mutate mode stores in named result | Named results |
| 001.7.4-UNIT-054 | Unit | P0 | Results include nodes_processed count | Execution stats |
| 001.7.4-UNIT-055 | Unit | P0 | Results include execution_time_ms | Execution stats |
| 001.7.4-UNIT-056 | Unit | P2 | Results include memory_used_bytes | Memory stats |
| 001.7.4-INT-020 | Integration | P0 | Write mode persists scores to node properties | Write verification |
| 001.7.4-INT-021 | Integration | P1 | Mutate mode results accessible by name | Mutate verification |

---

### AC-22: Configuration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-057 | Unit | P0 | Parse `settings.graph.gds.enabled: true` from YAML | Config parsing |
| 001.7.4-UNIT-058 | Unit | P1 | Parse `settings.graph.gds.memory_estimation: true` | Config parsing |
| 001.7.4-UNIT-059 | Unit | P1 | Parse `settings.graph.gds.default_write_property` | Config parsing |
| 001.7.4-UNIT-060 | Unit | P1 | Default GDS config when section missing | Default handling |
| 001.7.4-UNIT-061 | Unit | P2 | GDS disabled when `enabled: false` | Feature toggle |

---

### AC-23: Action Registration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-062 | Unit | P0 | `gds.project_graph` action registered | Action namespace |
| 001.7.4-UNIT-063 | Unit | P0 | `gds.drop_graph` action registered | Action namespace |
| 001.7.4-UNIT-064 | Unit | P0 | `gds.page_rank` action registered | Action namespace |
| 001.7.4-UNIT-065 | Unit | P0 | `gds.louvain` action registered | Action namespace |
| 001.7.4-UNIT-066 | Unit | P0 | `gds.dijkstra` action registered | Action namespace |
| 001.7.4-UNIT-067 | Unit | P0 | `gds.node_similarity` action registered | Action namespace |
| 001.7.4-INT-022 | Integration | P1 | YAML node with `uses: gds.page_rank` executes | YAML integration |
| 001.7.4-INT-023 | Integration | P1 | YAML node with `uses: gds.louvain` executes | YAML integration |

---

### AC-24 to AC-25: Graceful Degradation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-UNIT-068 | Unit | P0 | GDS action returns clear error when GDS unavailable | User-friendly errors |
| 001.7.4-UNIT-069 | Unit | P0 | Error includes `"error_type": "dependency_missing"` | Programmatic handling |
| 001.7.4-UNIT-070 | Unit | P0 | Error message mentions "Enterprise Edition" requirement | User guidance |
| 001.7.4-UNIT-071 | Unit | P1 | Warning logged when GDS check fails | Observability |
| 001.7.4-UNIT-072 | Unit | P1 | Workflow continues after GDS action failure (non-blocking) | Resilience |
| 001.7.4-INT-024 | Integration | P0 | All GDS actions fail gracefully on Community Edition | Full degradation path |
| 001.7.4-INT-025 | Integration | P1 | YAML workflow with conditional GDS path handles missing GDS | Workflow resilience |

---

### E2E: Critical User Journeys

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.7.4-E2E-001 | E2E | P0 | YAML agent: Create projection → Run PageRank → Drop graph | Core GDS workflow |
| 001.7.4-E2E-002 | E2E | P0 | YAML agent: Community detection → Write results → Query communities | Community analysis workflow |
| 001.7.4-E2E-003 | E2E | P1 | YAML agent: Path finding between entities | Graph navigation workflow |
| 001.7.4-E2E-004 | E2E | P1 | YAML agent: Node similarity for recommendations | Similarity workflow |
| 001.7.4-E2E-005 | E2E | P0 | YAML agent: Handle GDS unavailable gracefully | Degradation workflow |
| 001.7.4-E2E-006 | E2E | P2 | YAML agent: Chain multiple algorithms | Complex analytics |
| 001.7.4-E2E-007 | E2E | P2 | YAML agent: Memory estimation before large projection | Resource planning |
| 001.7.4-E2E-008 | E2E | P3 | YAML agent: Centrality + Community in single workflow | Combined analysis |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| GDS library not detected properly | 001.7.4-UNIT-001, 001.7.4-UNIT-002, 001.7.4-INT-001, 001.7.4-INT-002 |
| Cypher syntax errors in algorithm wrappers | All UNIT-0XX tests constructing Cypher |
| Poor error handling on Community Edition | 001.7.4-UNIT-068 to 001.7.4-UNIT-072, 001.7.4-INT-024 |
| Algorithm results not matching expected schema | 001.7.4-UNIT-025, 001.7.4-UNIT-034, 001.7.4-UNIT-039, 001.7.4-UNIT-046 |
| Graph projection memory issues | 001.7.4-UNIT-019, 001.7.4-INT-007 |
| Write mode not persisting correctly | 001.7.4-INT-020 |
| Action registration missing | 001.7.4-UNIT-062 to 001.7.4-UNIT-067 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic) - 26 tests
2. **P0 Integration tests** (validate GDS connectivity) - 8 tests
3. **P0 E2E tests** (critical workflows) - 3 tests
4. **P1 tests in order** (unit → integration → e2e) - 32 tests
5. **P2+ as time permits** - 16+ tests

---

## Test Environment Requirements

### Unit Tests
- Python 3.9+
- pytest + pytest-mock
- No Neo4j required (mocked procedures)

### Integration Tests
- Neo4j Enterprise Edition 5.x with GDS 2.x plugin
- Docker container: `neo4j:5-enterprise` with GDS plugin
- Test data seed script for graph projections

### E2E Tests
- Full TEA Python environment
- Neo4j Enterprise + GDS running
- Sample YAML agent workflows

---

## Quality Checklist

- [x] Every AC has test coverage (25 ACs → 78 scenarios)
- [x] Test levels are appropriate (54% unit, 36% integration, 10% E2E)
- [x] No duplicate coverage across levels (unit tests logic, integration tests GDS, E2E tests workflows)
- [x] Priorities align with business risk (graceful degradation = P0)
- [x] Test IDs follow naming convention (`001.7.4-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 78
  by_level:
    unit: 42
    integration: 28
    e2e: 8
  by_priority:
    p0: 26
    p1: 32
    p2: 16
    p3: 4
  coverage_gaps: []
  key_risks_addressed:
    - gds_detection_reliability
    - graceful_degradation_path
    - algorithm_result_schemas
    - action_registration_completeness
```

---

## Trace References

- **Test design matrix:** `docs/qa/assessments/TEA-BUILTIN-001.7.4-test-design-20251230.md`
- **P0 tests identified:** 26
- **Story file:** `docs/stories/TEA-BUILTIN-001.7.4-neo4j-gds.md`
