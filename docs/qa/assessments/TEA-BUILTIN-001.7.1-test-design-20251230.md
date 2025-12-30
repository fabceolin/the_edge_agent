# Test Design: Story TEA-BUILTIN-001.7.1

**Story Title:** Neo4j Core Connection & Authentication
**Date:** 2025-12-30
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 67 |
| **Unit tests** | 42 (63%) |
| **Integration tests** | 19 (28%) |
| **E2E tests** | 6 (9%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 24 | Critical - Authentication, connection, error handling |
| **P1** | 28 | High - Core GraphBackend operations |
| **P2** | 12 | Medium - Edge cases, secondary configs |
| **P3** | 3 | Low - Advanced configurations |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Neo4jBackend implements GraphBackend protocol

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-001 | Unit | P0 | Verify Neo4jBackend class has all required protocol methods | Protocol compliance is foundation |
| 001.7.1-UNIT-002 | Unit | P0 | Verify method signatures match GraphBackend protocol | Type safety ensures interoperability |
| 001.7.1-UNIT-003 | Unit | P1 | Verify Neo4jBackend can be used wherever GraphBackend is expected | Duck typing compatibility |

---

### AC-2: URI schemes support (bolt://, bolt+s://, bolt+ssc://, neo4j://, neo4j+s://, neo4j+ssc://)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-004 | Unit | P0 | Accept `bolt://host:port` unencrypted connection | Basic connectivity |
| 001.7.1-UNIT-005 | Unit | P0 | Accept `bolt+s://host:port` with TLS verification | Production security |
| 001.7.1-UNIT-006 | Unit | P1 | Accept `bolt+ssc://host:port` with self-signed cert | Dev environment support |
| 001.7.1-UNIT-007 | Unit | P0 | Accept `neo4j://host:port` with routing | Cluster connectivity |
| 001.7.1-UNIT-008 | Unit | P0 | Accept `neo4j+s://host:port` with TLS | Aura/cloud connectivity |
| 001.7.1-UNIT-009 | Unit | P1 | Accept `neo4j+ssc://host:port` with self-signed | Hybrid environments |
| 001.7.1-UNIT-010 | Unit | P0 | Reject invalid URI scheme (e.g., `http://`) | Input validation |
| 001.7.1-UNIT-011 | Unit | P1 | Reject malformed URI (missing port, invalid host) | Input validation |
| 001.7.1-INT-001 | Integration | P1 | Connect to real Neo4j with `bolt://` scheme | Real connectivity validation |
| 001.7.1-INT-002 | Integration | P1 | Connect to real Neo4j with `neo4j://` scheme | Routing protocol validation |

---

### AC-3: Database parameter (default: "neo4j")

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-012 | Unit | P1 | Default database is "neo4j" when not specified | Sensible defaults |
| 001.7.1-UNIT-013 | Unit | P1 | Custom database name accepted | Multi-database support |
| 001.7.1-UNIT-014 | Unit | P2 | Empty database string handled gracefully | Edge case |
| 001.7.1-INT-003 | Integration | P2 | Session created with correct database parameter | Database selection verification |

---

### AC-4: Connection pooling with max_connection_pool_size (default: 50)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-015 | Unit | P1 | Default max_connection_pool_size is 50 | Sensible defaults |
| 001.7.1-UNIT-016 | Unit | P1 | Custom pool size passed to driver | Configuration flexibility |
| 001.7.1-UNIT-017 | Unit | P2 | Pool size of 0 or negative rejected | Input validation |
| 001.7.1-INT-004 | Integration | P2 | Pool limits concurrent connections appropriately | Resource management |

---

### AC-5: max_connection_lifetime (default: 3600)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-018 | Unit | P2 | Default max_connection_lifetime is 3600 seconds | Sensible defaults |
| 001.7.1-UNIT-019 | Unit | P2 | Custom lifetime passed to driver | Configuration flexibility |

---

### AC-6: connection_acquisition_timeout (default: 60)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-020 | Unit | P2 | Default connection_acquisition_timeout is 60 seconds | Sensible defaults |
| 001.7.1-UNIT-021 | Unit | P2 | Custom timeout passed to driver | Configuration flexibility |

---

### AC-7: Automatic reconnection on transient failures

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-022 | Unit | P0 | Retry on ServiceUnavailable exception | Resilience |
| 001.7.1-UNIT-023 | Unit | P0 | Retry on SessionExpired exception | Resilience |
| 001.7.1-UNIT-024 | Unit | P1 | Respect max retry attempts (fail after N retries) | Prevent infinite loops |
| 001.7.1-UNIT-025 | Unit | P1 | Exponential backoff between retries | Resource protection |
| 001.7.1-INT-005 | Integration | P1 | Recovery after transient network failure | Real-world resilience |

---

### AC-8: Basic authentication (username/password)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-026 | Unit | P0 | Basic auth with username and password | Core authentication |
| 001.7.1-UNIT-027 | Unit | P0 | Driver created with basic_auth() helper | Correct API usage |
| 001.7.1-INT-006 | Integration | P0 | Successful authentication with valid credentials | Real auth flow |
| 001.7.1-INT-007 | Integration | P0 | Authentication failure with invalid credentials | Security validation |

---

### AC-9: Bearer token authentication

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-028 | Unit | P0 | Bearer auth with token | Cloud/Aura support |
| 001.7.1-UNIT-029 | Unit | P0 | Driver created with bearer_auth() helper | Correct API usage |
| 001.7.1-INT-008 | Integration | P1 | Successful bearer token authentication | Real auth flow |
| 001.7.1-INT-009 | Integration | P1 | Authentication failure with invalid token | Security validation |

---

### AC-10: Environment variable expansion (${VAR} syntax)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-030 | Unit | P0 | Expand `${NEO4J_PASSWORD}` in password field | Secure config |
| 001.7.1-UNIT-031 | Unit | P0 | Expand `${NEO4J_USER}` in username field | Secure config |
| 001.7.1-UNIT-032 | Unit | P1 | Expand `${NEO4J_TOKEN}` in bearer_token field | Secure config |
| 001.7.1-UNIT-033 | Unit | P1 | Expand `${NEO4J_URI}` in uri field | Flexible deployment |
| 001.7.1-UNIT-034 | Unit | P1 | Error when referenced env var is not set | Clear failure mode |
| 001.7.1-UNIT-035 | Unit | P2 | Literal `$${VAR}` escaped (not expanded) | Edge case handling |

---

### AC-11: No credentials in logs/error messages

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-036 | Unit | P0 | Password not in __repr__ output | Security |
| 001.7.1-UNIT-037 | Unit | P0 | Password not in str() output | Security |
| 001.7.1-UNIT-038 | Unit | P0 | Token not in error messages | Security |
| 001.7.1-UNIT-039 | Unit | P0 | Credentials redacted in exception messages | Security |

---

### AC-12: Clear error message on authentication failure

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-040 | Unit | P0 | Authentication error includes "authentication_error" type | Error categorization |
| 001.7.1-UNIT-041 | Unit | P1 | Error message suggests checking credentials | User guidance |

---

### AC-13: store_entity() with MERGE semantics

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-042 | Unit | P0 | Generate valid MERGE Cypher for new entity | Core functionality |
| 001.7.1-UNIT-043 | Unit | P0 | Update existing entity (MERGE upsert) | Core functionality |
| 001.7.1-UNIT-044 | Unit | P1 | Node label from entity_type | Schema correctness |
| 001.7.1-UNIT-045 | Unit | P1 | Properties stored as node properties | Data storage |
| 001.7.1-UNIT-046 | Unit | P1 | Embedding stored in `_embedding` property | Vector storage |
| 001.7.1-UNIT-047 | Unit | P2 | Handle None embedding (optional field) | Edge case |
| 001.7.1-UNIT-048 | Unit | P1 | Return success dict with entity_id | API contract |
| 001.7.1-INT-010 | Integration | P0 | Create and verify entity in real Neo4j | Data persistence |
| 001.7.1-INT-011 | Integration | P1 | Update existing entity and verify | Upsert behavior |

---

### AC-14: store_relation() with MERGE semantics

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-049 | Unit | P0 | Generate valid MERGE Cypher for relationship | Core functionality |
| 001.7.1-UNIT-050 | Unit | P1 | Relationship type from relation_type | Schema correctness |
| 001.7.1-UNIT-051 | Unit | P1 | Properties stored on relationship | Data storage |
| 001.7.1-UNIT-052 | Unit | P1 | Return success dict | API contract |
| 001.7.1-INT-012 | Integration | P0 | Create relationship between existing nodes | Data persistence |
| 001.7.1-INT-013 | Integration | P1 | Error when source/target entity missing | Referential integrity |

---

### AC-15: query() with cypher/datalog/pattern support

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-053 | Unit | P0 | Execute raw Cypher query | Core query capability |
| 001.7.1-UNIT-054 | Unit | P0 | Pass params as query parameters | Security (injection prevention) |
| 001.7.1-UNIT-055 | Unit | P1 | Append LIMIT when not in query | Result limiting |
| 001.7.1-UNIT-056 | Unit | P1 | Datalog parameter returns helpful error | API compatibility |
| 001.7.1-UNIT-057 | Unit | P1 | Pattern dict converted to Cypher MATCH | Pattern query support |
| 001.7.1-UNIT-058 | Unit | P2 | Handle timeout parameter | Query management |
| 001.7.1-INT-014 | Integration | P0 | Execute Cypher and parse results | Real query execution |
| 001.7.1-E2E-001 | E2E | P1 | Complex multi-hop query returns correct results | Full workflow |

---

### AC-16: retrieve_context() with N-hop expansion

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-059 | Unit | P1 | Generate N-hop expansion Cypher from entity_id | Graph traversal |
| 001.7.1-UNIT-060 | Unit | P1 | Return entities and relations in subgraph | Context structure |
| 001.7.1-UNIT-061 | Unit | P2 | Build context summary string | LLM integration |
| 001.7.1-INT-015 | Integration | P1 | Retrieve 2-hop context from real graph | Real traversal |
| 001.7.1-E2E-002 | E2E | P2 | Context retrieval with large subgraph | Performance validation |

---

### AC-17: close() releases connections

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-062 | Unit | P0 | close() calls driver.close() | Resource cleanup |
| 001.7.1-UNIT-063 | Unit | P1 | Operations after close() raise error | State validation |
| 001.7.1-INT-016 | Integration | P1 | Connection pool released after close() | Resource management |

---

### AC-18/19: Consistent error dict format and error types

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-064 | Unit | P0 | Error dict has success=False, error, error_type keys | API contract |
| 001.7.1-UNIT-065 | Unit | P0 | connection_error type for network issues | Error categorization |
| 001.7.1-UNIT-066 | Unit | P0 | authentication_error type for auth failures | Error categorization |
| 001.7.1-UNIT-067 | Unit | P1 | validation_error type for invalid params | Error categorization |
| 001.7.1-UNIT-068 | Unit | P1 | query_error type for Cypher failures | Error categorization |
| 001.7.1-UNIT-069 | Unit | P0 | dependency_missing when neo4j not installed | Clear failure mode |

---

### AC-20: Graceful handling of server unavailability

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-INT-017 | Integration | P0 | Return error dict when server unreachable | Graceful degradation |
| 001.7.1-INT-018 | Integration | P1 | Timeout after configured period | Resource protection |
| 001.7.1-E2E-003 | E2E | P2 | Agent workflow handles Neo4j downtime gracefully | Real-world resilience |

---

### AC-21/22/23: Availability check and module exports

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-UNIT-070 | Unit | P0 | `_check_neo4j_available()` returns True when neo4j installed | Dependency check |
| 001.7.1-UNIT-071 | Unit | P0 | `_check_neo4j_available()` returns False when neo4j missing | Dependency check |
| 001.7.1-UNIT-072 | Unit | P1 | `NEO4J_AVAILABLE` constant exported from memory/__init__.py | Module API |
| 001.7.1-UNIT-073 | Unit | P1 | `Neo4jBackend` conditionally imported when available | Graceful import |
| 001.7.1-UNIT-074 | Unit | P2 | No ImportError when neo4j not installed | Optional dependency |

---

### AC-24: Thread-safe operations

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.7.1-INT-019 | Integration | P0 | Concurrent store_entity calls succeed | Thread safety |
| 001.7.1-E2E-004 | E2E | P1 | Parallel agent workflows share Neo4j backend | Real concurrency |
| 001.7.1-E2E-005 | E2E | P2 | High-concurrency stress test (50 threads) | Load validation |
| 001.7.1-E2E-006 | E2E | P3 | Connection pool exhaustion handled gracefully | Edge case |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| **Credential leakage** | 001.7.1-UNIT-036 to 039 |
| **Connection failures in production** | 001.7.1-UNIT-022 to 025, 001.7.1-INT-005 |
| **Authentication misconfiguration** | 001.7.1-UNIT-030 to 035, 001.7.1-INT-006 to 009 |
| **Query injection** | 001.7.1-UNIT-054 |
| **Resource exhaustion** | 001.7.1-INT-004, 001.7.1-E2E-005, 001.7.1-E2E-006 |
| **Breaking GraphBackend contract** | 001.7.1-UNIT-001 to 003 |

---

## Recommended Execution Order

1. **P0 Unit tests (fail fast on critical issues)**
   - Protocol compliance (001, 002)
   - URI validation (004, 005, 007, 008, 010)
   - Authentication (026-029, 036-041)
   - Reconnection (022, 023)
   - Error handling (064-066, 069)
   - Env var expansion (030, 031)
   - Core operations (042, 043, 049, 053, 054, 062)
   - Availability check (070, 071)

2. **P0 Integration tests**
   - Real authentication (INT-006, INT-007)
   - Entity persistence (INT-010)
   - Relationship creation (INT-012)
   - Query execution (INT-014)
   - Server unavailability (INT-017)
   - Thread safety (INT-019)

3. **P1 Unit tests**
   - URI schemes (006, 009, 011)
   - Database config (012, 013)
   - Pool size (015, 016)
   - Retry behavior (024, 025)
   - Remaining auth tests
   - All store/query operations
   - Module exports (072, 073)

4. **P1 Integration tests**
   - Connection tests (INT-001 to INT-005)
   - Entity updates (INT-011)
   - Relation errors (INT-013)
   - Context retrieval (INT-015)
   - Connection cleanup (INT-016)
   - Timeout (INT-018)

5. **P1 E2E tests**
   - Complex queries (E2E-001)
   - Parallel workflows (E2E-004)

6. **P2 tests as time permits**

7. **P3 tests only in full regression cycles**

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 67
  by_level:
    unit: 42
    integration: 19
    e2e: 6
  by_priority:
    p0: 24
    p1: 28
    p2: 12
    p3: 3
  coverage_gaps: []
  risk_mitigations:
    - credential_leakage: 4 tests
    - connection_resilience: 6 tests
    - authentication_security: 10 tests
    - query_injection: 1 test
    - resource_exhaustion: 3 tests
    - protocol_compliance: 3 tests
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.7.1-test-design-20251230.md
P0 tests identified: 24
P1 tests identified: 28
Total ACs covered: 24/24 (100%)
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for DB, E2E for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (auth/security = P0)
- [x] Test IDs follow naming convention (001.7.1-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have defense in depth
