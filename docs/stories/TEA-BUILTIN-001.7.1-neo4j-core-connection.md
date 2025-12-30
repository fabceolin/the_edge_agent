# Story TEA-BUILTIN-001.7.1: Neo4j Core Connection & Authentication

## Status

**Ready for Development**

> ✅ QA Validated: 2025-12-30 | 67 test scenarios | 100% AC coverage | Security-critical tests defined

## Story

**As a** YAML agent developer,
**I want** a Neo4j graph backend with robust connection handling and flexible authentication,
**so that** I can connect my agents to remote Neo4j databases using various connection protocols and authentication methods while leveraging the existing GraphBackend interface.

## Story Context

**Existing System Integration:**

- Integrates with: `GraphBackend` protocol from `memory/graph.py`
- Technology: Neo4j 5.4+ with official `neo4j` Python driver
- Follows pattern: `KuzuBackend` class structure and error handling
- Touch points: `memory/graph.py`, `memory/__init__.py`

## Acceptance Criteria

### Connection Management

1. **AC-1**: `Neo4jBackend` class implements `GraphBackend` protocol
2. **AC-2**: Constructor accepts `uri` parameter with all Neo4j URI schemes:
   - `bolt://host:port` - Unencrypted Bolt protocol
   - `bolt+s://host:port` - Bolt with TLS (certificate verification)
   - `bolt+ssc://host:port` - Bolt with TLS (self-signed, skip verification)
   - `neo4j://host:port` - Neo4j scheme with routing support
   - `neo4j+s://host:port` - Neo4j with TLS
   - `neo4j+ssc://host:port` - Neo4j with TLS (self-signed)
3. **AC-3**: Constructor accepts `database` parameter (default: "neo4j")
4. **AC-4**: Connection pooling with configurable `max_connection_pool_size` (default: 50)
5. **AC-5**: Configurable `max_connection_lifetime` in seconds (default: 3600)
6. **AC-6**: Configurable `connection_acquisition_timeout` in seconds (default: 60)
7. **AC-7**: Automatic reconnection on transient failures

### Authentication

8. **AC-8**: Basic authentication with `username` and `password` parameters
9. **AC-9**: Bearer token authentication with `bearer_token` parameter
10. **AC-10**: Credentials can be provided via environment variables with `${VAR}` syntax
11. **AC-11**: No credentials logged in error messages or debug output
12. **AC-12**: Clear error message when authentication fails

### GraphBackend Protocol Implementation

13. **AC-13**: `store_entity(entity_id, entity_type, properties, embedding)` creates/updates nodes
    - Uses `MERGE` for upsert semantics
    - Node label from `entity_type`
    - Properties stored as node properties
    - Embedding stored in `_embedding` property (if provided)
14. **AC-14**: `store_relation(from_entity, to_entity, relation_type, properties)` creates relationships
    - Uses `MERGE` for upsert semantics
    - Relationship type from `relation_type`
15. **AC-15**: `query(cypher, datalog, pattern, params, limit, timeout)` executes queries
    - `cypher` parameter for raw Cypher queries
    - `datalog` returns error suggesting Cypher (Neo4j doesn't support Datalog)
    - `pattern` dict converted to Cypher query
    - `params` passed as query parameters
    - `limit` appended if not in query
16. **AC-16**: `retrieve_context(query, embedding, entity_id, hops, limit)` traverses graph
    - `entity_id` starts N-hop expansion
    - Returns entities and relations in subgraph
    - Builds context summary
17. **AC-17**: `close()` properly closes driver and releases connections

### Error Handling

18. **AC-18**: Returns consistent error dict format:
    ```python
    {"success": False, "error": str, "error_type": str}
    ```
19. **AC-19**: Error types include:
    - `connection_error` - Network/connection issues
    - `authentication_error` - Auth failures
    - `validation_error` - Invalid parameters
    - `query_error` - Cypher execution failures
    - `dependency_missing` - neo4j driver not installed
20. **AC-20**: Graceful handling of Neo4j server unavailability

### Integration

21. **AC-21**: Availability check function `_check_neo4j_available()` returns bool
22. **AC-22**: `NEO4J_AVAILABLE` constant exported from `memory/__init__.py`
23. **AC-23**: `Neo4jBackend` conditionally imported in `memory/__init__.py`
24. **AC-24**: Thread-safe operations using driver's built-in session management

## Tasks / Subtasks

- [ ] **Task 1: Create Neo4jBackend class skeleton** (AC: 1, 17)
  - [ ] Add `Neo4jBackend` class to `memory/graph.py`
  - [ ] Implement `GraphBackend` protocol methods as stubs
  - [ ] Add `_check_neo4j_available()` function
  - [ ] Add `NEO4J_AVAILABLE` constant

- [ ] **Task 2: Implement connection management** (AC: 2-7, 20)
  - [ ] Parse URI schemes and configure driver accordingly
  - [ ] Configure connection pool settings
  - [ ] Implement `close()` method
  - [ ] Add reconnection logic for transient failures

- [ ] **Task 3: Implement authentication** (AC: 8-12)
  - [ ] Implement Basic Auth with `neo4j.basic_auth()`
  - [ ] Implement Bearer Token with `neo4j.bearer_auth()`
  - [ ] Support environment variable expansion
  - [ ] Ensure no credential leakage in logs

- [ ] **Task 4: Implement store_entity** (AC: 13)
  - [ ] Generate MERGE Cypher for node creation/update
  - [ ] Handle properties serialization (JSON for complex types)
  - [ ] Store embedding in `_embedding` property
  - [ ] Return success dict with created/updated status

- [ ] **Task 5: Implement store_relation** (AC: 14)
  - [ ] Generate MERGE Cypher for relationship
  - [ ] Handle properties serialization
  - [ ] Return success dict

- [ ] **Task 6: Implement query** (AC: 15, 18-19)
  - [ ] Execute raw Cypher queries with parameters
  - [ ] Convert pattern dict to Cypher
  - [ ] Handle datalog parameter with helpful error
  - [ ] Parse results to list of dicts

- [ ] **Task 7: Implement retrieve_context** (AC: 16)
  - [ ] Implement N-hop expansion from entity_id
  - [ ] Build subgraph response (entities + relations)
  - [ ] Generate context summary

- [ ] **Task 8: Update memory module exports** (AC: 21-23)
  - [ ] Add conditional import in `memory/__init__.py`
  - [ ] Export `Neo4jBackend` and `NEO4J_AVAILABLE`
  - [ ] Update `__all__` list

- [ ] **Task 9: Add unit tests** (AC: 24)
  - [ ] Test connection with mocked driver
  - [ ] Test both auth methods
  - [ ] Test all GraphBackend methods
  - [ ] Test error handling scenarios
  - [ ] Test thread safety

## Dev Notes

### Neo4j Driver Pattern

```python
from neo4j import GraphDatabase, basic_auth, bearer_auth

# Basic auth
driver = GraphDatabase.driver(
    "bolt://localhost:7687",
    auth=basic_auth("neo4j", "password"),
    max_connection_pool_size=50,
    max_connection_lifetime=3600,
)

# Bearer auth
driver = GraphDatabase.driver(
    "neo4j+s://aura.neo4j.io",
    auth=bearer_auth("eyJ...token"),
)

# Execute query
with driver.session(database="neo4j") as session:
    result = session.run("MATCH (n) RETURN n LIMIT 10")
    records = list(result)
```

### Cypher for Entity Storage

```cypher
// MERGE creates or updates node
MERGE (e:Entity {id: $entity_id})
SET e.type = $entity_type,
    e.properties = $properties,
    e._embedding = $embedding,
    e.updated_at = datetime()
RETURN e.id, e.type
```

### Source Tree Reference

```
python/src/the_edge_agent/memory/
├── graph.py          # Add Neo4jBackend here (alongside CozoBackend, KuzuBackend)
├── __init__.py       # Add exports
└── ...
```

### Testing

- Test file: `python/tests/test_neo4j_backend.py`
- Use `unittest.mock` to mock `neo4j.GraphDatabase`
- Test patterns:
  - Mock driver creation
  - Mock session execution
  - Verify Cypher query generation
  - Test error scenarios

## Definition of Done

- [ ] `Neo4jBackend` implements full `GraphBackend` protocol
- [ ] All URI schemes supported and tested
- [ ] Both auth methods working
- [ ] Unit tests with >90% coverage
- [ ] No credential leakage in logs
- [ ] Existing `KuzuBackend`/`CozoBackend` tests still pass
- [ ] Documentation updated

---

## QA Notes

**Test Design Review Date:** 2025-12-30
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Scenarios** | 67 |
| **Unit Tests** | 42 (63%) |
| **Integration Tests** | 19 (28%) |
| **E2E Tests** | 6 (9%) |
| **AC Coverage** | 24/24 (100%) |

**Priority Distribution:**
- P0 (Critical): 24 scenarios
- P1 (High): 28 scenarios
- P2 (Medium): 12 scenarios
- P3 (Low): 3 scenarios

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| **Credential leakage** | High | 4 tests covering `__repr__`, `str()`, error messages |
| **Authentication misconfiguration** | High | 10 tests covering env var expansion, auth methods |
| **Connection failures in production** | High | 6 tests for retry logic, transient failure recovery |
| **Query injection** | Medium | 1 test verifying parameterized queries |
| **Resource exhaustion** | Medium | 3 tests for pool limits, high-concurrency |
| **Breaking GraphBackend contract** | High | 3 tests for protocol compliance |

### Recommended Test Scenarios

**Must-Have (P0 - 24 scenarios):**
1. Protocol compliance verification (AC-1)
2. URI scheme validation for all 6 supported schemes (AC-2)
3. Basic and bearer authentication (AC-8, AC-9)
4. Credential security - no leakage in logs/errors (AC-11)
5. Automatic reconnection on transient failures (AC-7)
6. Error dict format consistency (AC-18/19)
7. Dependency availability check (AC-21)
8. Core CRUD operations: `store_entity`, `store_relation`, `query` (AC-13, 14, 15)
9. Thread safety under concurrent access (AC-24)

**High Priority (P1 - 28 scenarios):**
1. Connection pool configuration (AC-4)
2. Database parameter handling (AC-3)
3. Environment variable expansion for credentials (AC-10)
4. Pattern-to-Cypher conversion in `query()` (AC-15)
5. N-hop context retrieval (AC-16)
6. Module exports and conditional imports (AC-22, 23)

### Concerns or Blockers

1. **Integration Test Infrastructure**: Tests require access to a Neo4j instance. Recommend:
   - Docker container for CI/CD (neo4j:5.4-community)
   - Mock driver for unit tests
   - Skip integration tests when `NEO4J_TEST_URI` not set

2. **Security Testing**: Credential leakage tests (001.7.1-UNIT-036 to 039) are critical security gates. These MUST pass before any release.

3. **Environment Variable Edge Cases**: Test 001.7.1-UNIT-034 covers missing env vars - ensure error messages don't reveal which vars are expected (security consideration).

4. **Thread Safety Validation**: Integration test 001.7.1-INT-019 requires careful design to avoid flaky results. Consider using `threading.Barrier` for synchronized concurrent access.

### QA Recommendation

**Status: Ready for Development**

The story has comprehensive acceptance criteria covering all aspects of Neo4j integration. Test design provides 100% AC coverage with appropriate prioritization. Security-critical areas (authentication, credential handling) have defense-in-depth testing.

**Pre-Implementation Checklist:**
- [ ] Ensure `pytest-docker` available for integration tests
- [ ] Create test fixtures for mock Neo4j driver
- [ ] Set up CI environment variables for integration testing

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.1 | Initial story creation | PO (Sarah) |
