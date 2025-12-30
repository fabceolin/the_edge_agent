# Story TEA-BUILTIN-001.7.4: Neo4j Graph Data Science Integration

## Status

**Ready for Development**

**QA Validated:** 2024-12-30 - All checklist criteria passed. Test design comprehensive with 78 scenarios (P0:26, P1:32, P2:16, P3:4). All 25 acceptance criteria have test coverage.

## Story

**As a** YAML agent developer building analytics-driven applications,
**I want** access to Neo4j Graph Data Science (GDS) algorithms,
**so that** I can leverage graph algorithms for centrality analysis, community detection, path finding, and node similarity to enrich agent decision-making and knowledge extraction.

## Story Context

**Existing System Integration:**

- Integrates with: `Neo4jBackend` from TEA-BUILTIN-001.7.1
- Technology: Neo4j GDS Library 2.x (Enterprise feature)
- Follows pattern: Optional feature with graceful degradation
- Touch points: `memory/graph.py`, `actions/graph_actions.py`, new `actions/gds_actions.py`

**Prerequisites:**
- Neo4j Enterprise Edition
- GDS Library 2.x installed

## Acceptance Criteria

### GDS Detection

1. **AC-1**: `check_gds_available()` detects GDS library presence
2. **AC-2**: `get_gds_version()` returns installed GDS version
3. **AC-3**: `GDS_AVAILABLE` property on Neo4jBackend

### Graph Projection

4. **AC-4**: `gds_project_graph(graph_name, node_projection, relationship_projection, config)` creates in-memory projection
   - `node_projection`: Labels and properties to include
   - `relationship_projection`: Types and properties
   - Returns `{"success": True, "graph_name": str, "node_count": int, "relationship_count": int}`
5. **AC-5**: `gds_drop_graph(graph_name)` removes projection
6. **AC-6**: `gds_list_graphs()` shows active projections
7. **AC-7**: `gds_estimate_memory(algorithm, graph_name, config)` estimates memory requirements

### Centrality Algorithms

8. **AC-8**: `gds_page_rank(graph_name, config)` runs PageRank
   - Config: `max_iterations`, `damping_factor`, `tolerance`
   - Returns nodes with `score` property
9. **AC-9**: `gds_betweenness_centrality(graph_name, config)` runs betweenness
10. **AC-10**: `gds_degree_centrality(graph_name, config)` runs degree centrality
11. **AC-11**: `gds_closeness_centrality(graph_name, config)` runs closeness

### Community Detection

12. **AC-12**: `gds_louvain(graph_name, config)` runs Louvain community detection
    - Returns nodes with `community_id` property
    - Config: `max_levels`, `max_iterations`
13. **AC-13**: `gds_label_propagation(graph_name, config)` runs Label Propagation
14. **AC-14**: `gds_wcc(graph_name, config)` runs Weakly Connected Components

### Path Finding

15. **AC-15**: `gds_dijkstra(graph_name, source_id, target_id, config)` finds shortest weighted path
    - Returns path with total cost
16. **AC-16**: `gds_astar(graph_name, source_id, target_id, config)` runs A* with heuristic
17. **AC-17**: `gds_all_shortest_paths(graph_name, source_id, config)` from single source

### Node Similarity

18. **AC-18**: `gds_node_similarity(graph_name, config)` computes Jaccard similarity
    - Returns pairs with similarity score
    - Config: `similarity_cutoff`, `top_k`
19. **AC-19**: `gds_knn(graph_name, config)` runs K-Nearest Neighbors

### Result Handling

20. **AC-20**: Algorithm results can be:
    - Returned directly (stream mode)
    - Written back to graph (write mode)
    - Stored in named result (mutate mode)
21. **AC-21**: Results include execution statistics (nodes processed, time)

### Configuration

22. **AC-22**: YAML configuration for GDS:
    ```yaml
    settings:
      graph:
        backend: neo4j
        gds:
          enabled: true
          memory_estimation: true
          default_write_property: "gds_result"
    ```

### Action Registration

23. **AC-23**: Register GDS actions namespace:
    - `gds.project_graph`
    - `gds.drop_graph`
    - `gds.page_rank`
    - `gds.louvain`
    - `gds.dijkstra`
    - `gds.node_similarity`
    - (and others)

### Graceful Degradation

24. **AC-24**: Clear error when GDS not available
25. **AC-25**: Warning about Enterprise-only features

## Tasks / Subtasks

- [ ] **Task 1: Implement GDS detection** (AC: 1-3)
  - [ ] Add `check_gds_available()` procedure check
  - [ ] Add `get_gds_version()`
  - [ ] Add `GDS_AVAILABLE` property

- [ ] **Task 2: Implement graph projection** (AC: 4-7)
  - [ ] Add `gds_project_graph()` with native projection
  - [ ] Add `gds_drop_graph()` and `gds_list_graphs()`
  - [ ] Add memory estimation

- [ ] **Task 3: Implement centrality algorithms** (AC: 8-11)
  - [ ] Add PageRank wrapper
  - [ ] Add Betweenness, Degree, Closeness

- [ ] **Task 4: Implement community detection** (AC: 12-14)
  - [ ] Add Louvain wrapper
  - [ ] Add Label Propagation and WCC

- [ ] **Task 5: Implement path finding** (AC: 15-17)
  - [ ] Add Dijkstra wrapper
  - [ ] Add A* with heuristic support
  - [ ] Add all shortest paths

- [ ] **Task 6: Implement node similarity** (AC: 18-19)
  - [ ] Add Node Similarity wrapper
  - [ ] Add KNN

- [ ] **Task 7: Add result handling** (AC: 20-21)
  - [ ] Support stream/write/mutate modes
  - [ ] Include execution statistics

- [ ] **Task 8: Register actions** (AC: 23)
  - [ ] Create `gds_actions.py` module
  - [ ] Register all GDS actions

- [ ] **Task 9: Add configuration** (AC: 22)
  - [ ] Parse GDS settings from YAML
  - [ ] Apply defaults

- [ ] **Task 10: Add unit tests**
  - [ ] Test GDS detection
  - [ ] Test algorithm wrappers (mocked)
  - [ ] Test graceful degradation

## Dev Notes

### GDS Cypher Patterns

```cypher
// Check GDS availability
RETURN gds.version() AS version

// Create graph projection
CALL gds.graph.project(
  'myGraph',
  'Entity',
  'RELATES_TO',
  {nodeProperties: ['properties'], relationshipProperties: []}
)

// Run PageRank (stream mode)
CALL gds.pageRank.stream('myGraph')
YIELD nodeId, score
RETURN gds.util.asNode(nodeId).id AS entity_id, score
ORDER BY score DESC

// Run Louvain (write mode)
CALL gds.louvain.write('myGraph', {writeProperty: 'community'})
YIELD communityCount, modularity
```

### Python Wrapper Pattern

```python
def gds_page_rank(self, graph_name: str, config: dict = None) -> dict:
    """Run PageRank on projected graph."""
    if not self.check_gds_available():
        return {
            "success": False,
            "error": "GDS library not available. Requires Neo4j Enterprise with GDS plugin.",
            "error_type": "dependency_missing"
        }

    config = config or {}
    mode = config.pop("mode", "stream")

    query = f"""
        CALL gds.pageRank.{mode}($graph_name, $config)
        YIELD nodeId, score
        RETURN gds.util.asNode(nodeId).id AS entity_id, score
        ORDER BY score DESC
    """

    return self._execute_query(query, {
        "graph_name": graph_name,
        "config": config
    })
```

### Testing

- Test file: `python/tests/test_neo4j_gds.py`
- Mock GDS procedures
- Test each algorithm category

## Definition of Done

- [ ] GDS detection working
- [ ] Graph projection CRUD working
- [ ] At least 2 algorithms per category implemented
- [ ] Stream/Write/Mutate modes supported
- [ ] Actions registered and documented
- [ ] Graceful degradation on Community Edition
- [ ] Unit tests with >90% coverage

---

## QA Notes

**Test Design Review Date:** 2024-12-30
**Test Architect:** Quinn

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 78 |
| Unit Tests | 42 (54%) |
| Integration Tests | 28 (36%) |
| E2E Tests | 8 (10%) |
| P0 (Critical) | 26 |
| P1 (High) | 32 |
| P2 (Medium) | 16 |
| P3 (Low) | 4 |

**Coverage Assessment:** All 25 acceptance criteria have test coverage. Test level distribution is appropriate for this story - heavy unit testing for algorithm wrapper logic with integration tests validating actual GDS procedure calls.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **GDS library not detected properly** | HIGH | P0 tests cover detection on both Enterprise (with GDS) and Community Edition (without GDS) |
| **Poor error handling on Community Edition** | HIGH | 5 unit tests + 2 integration tests specifically for graceful degradation path |
| **Cypher syntax errors in algorithm wrappers** | MEDIUM | Each algorithm has dedicated Cypher construction unit tests |
| **Algorithm results not matching expected schema** | MEDIUM | Response schema validation for PageRank, Louvain, Dijkstra, and NodeSimilarity |
| **Graph projection memory issues** | LOW | Memory estimation tests included but may need production tuning |
| **Write mode not persisting correctly** | MEDIUM | Integration test 001.7.4-INT-020 validates write persistence |

### Recommended Test Scenarios

**P0 Critical Path (26 scenarios):**
- GDS detection reliability (UNIT-001, UNIT-002, UNIT-006, INT-001, INT-002)
- Graph projection core workflow (UNIT-007, UNIT-008, UNIT-014, INT-003)
- Algorithm wrappers for each category (PageRank, Louvain, Dijkstra, NodeSimilarity)
- Graceful degradation (UNIT-068, UNIT-069, UNIT-070, INT-024)
- Action registration (UNIT-062 through UNIT-067)
- E2E critical workflows (E2E-001, E2E-002, E2E-005)

**Integration Test Requirements:**
- Requires Neo4j Enterprise Edition 5.x with GDS 2.x plugin
- Docker container: `neo4j:5-enterprise` with GDS plugin
- Test data seed script for graph projections

### Concerns / Blockers

1. **Enterprise-Only Dependency:** GDS is an Enterprise feature. Integration and E2E tests require Neo4j Enterprise Edition with GDS plugin - this may limit CI/CD automation for open-source contributors.

2. **Test Infrastructure:** Integration tests require a properly configured Neo4j Enterprise container with GDS. Consider documenting a Docker Compose setup for local testing.

3. **No Coverage Gaps Detected:** All ACs mapped to test scenarios.

### QA Recommendation

**Status:** Ready for Development

The test design is comprehensive and appropriately prioritizes graceful degradation as P0. Recommend proceeding with implementation. Key quality gates:
- All P0 tests must pass before PR approval
- Integration tests require Neo4j Enterprise + GDS environment
- E2E tests validate complete YAML agent workflows

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-001.7.4-test-design-20251230.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.1 | Initial story creation | PO (Sarah) |
