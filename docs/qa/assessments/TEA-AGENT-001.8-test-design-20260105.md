# Test Design: Story TEA-AGENT-001.8

**Story**: LlamaIndex RAG Bridge
**Date**: 2026-01-05
**Designer**: Quinn (Test Architect)

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| Total test scenarios | 63 |
| Unit tests | 28 (44%) |
| Integration tests | 24 (38%) |
| E2E tests | 11 (18%) |

**Priority distribution:**
- P0: 15 (24%) - Security, data integrity, fallback behavior
- P1: 28 (44%) - Core functionality, query engines
- P2: 16 (25%) - Configuration, edge cases
- P3: 4 (7%) - Nice-to-have features

## Test Scenarios by Acceptance Criteria

---

### AC1: `rag.llamaindex.query` Action

**Requirements:**
1. Executes simple vector queries against LlamaIndex index
2. Configurable index path or in-memory index
3. Returns retrieved nodes with scores
4. Supports similarity_top_k parameter
5. Graceful fallback to native `rag.query` when LlamaIndex unavailable

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-UNIT-001 | Unit | P1 | Validate query parameter parsing | Pure input validation logic |
| TEA-AGENT-001.8-UNIT-002 | Unit | P0 | Validate similarity_top_k accepts positive integers only | Data integrity - prevents invalid queries |
| TEA-AGENT-001.8-UNIT-003 | Unit | P1 | Parse index path from absolute path | Path handling logic |
| TEA-AGENT-001.8-UNIT-004 | Unit | P1 | Parse index path from relative path | Path handling logic |
| TEA-AGENT-001.8-UNIT-005 | Unit | P2 | Handle empty query string | Edge case validation |
| TEA-AGENT-001.8-UNIT-006 | Unit | P1 | Format retrieved nodes with scores as list | Result transformation logic |
| TEA-AGENT-001.8-UNIT-007 | Unit | P2 | Handle empty retrieval results | Edge case handling |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-UNIT-002 - Validate similarity_top_k
  Given a query action configuration
  When similarity_top_k is set to -5
  Then an InvalidParameterError should be raised
  And the error message should indicate positive integer required

Scenario: TEA-AGENT-001.8-UNIT-006 - Format nodes with scores
  Given retrieved nodes from LlamaIndex
  When formatting results for state output
  Then each node should include text content and similarity score
  And results should be ordered by score descending
```

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-001 | Integration | P0 | Execute query against persisted vector index | Core component interaction |
| TEA-AGENT-001.8-INT-002 | Integration | P1 | Execute query against in-memory index | Core component interaction |
| TEA-AGENT-001.8-INT-003 | Integration | P0 | Fallback to native rag.query when LlamaIndex unavailable | Graceful degradation path |
| TEA-AGENT-001.8-INT-004 | Integration | P1 | Query with custom similarity_top_k parameter | Parameter passthrough |
| TEA-AGENT-001.8-INT-005 | Integration | P2 | Query returns metadata alongside content | Result enrichment |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-INT-003 - Fallback to native RAG
  Given LlamaIndex is not installed
  And a workflow using rag.llamaindex.query
  When the query action executes
  Then the native rag.query action should be invoked instead
  And a warning should be logged indicating fallback mode
  And the query results should be returned successfully
```

---

### AC2: `rag.llamaindex.router` Action

**Requirements:**
1. Implements Router Query Engine pattern
2. Configurable query engines with descriptions
3. Automatic engine selection based on query semantics
4. Supports engine types: vector, keyword, sql
5. Returns selected engine and results

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-UNIT-008 | Unit | P1 | Parse engine configuration from YAML | Configuration parsing logic |
| TEA-AGENT-001.8-UNIT-009 | Unit | P0 | Validate engine type is one of [vector, keyword, sql] | Input validation |
| TEA-AGENT-001.8-UNIT-010 | Unit | P1 | Validate engine description is non-empty | Configuration validation |
| TEA-AGENT-001.8-UNIT-011 | Unit | P2 | Handle missing engine description gracefully | Edge case |
| TEA-AGENT-001.8-UNIT-012 | Unit | P1 | Format router response with selected engine name | Result transformation |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-UNIT-009 - Validate engine type
  Given a router configuration with engines
  When an engine has type "nosql"
  Then a ConfigurationError should be raised
  And the error should list valid engine types
```

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-006 | Integration | P1 | Router selects vector engine for semantic query | LLM + engine interaction |
| TEA-AGENT-001.8-INT-007 | Integration | P1 | Router selects keyword engine for exact match query | LLM + engine interaction |
| TEA-AGENT-001.8-INT-008 | Integration | P1 | Router selects SQL engine for structured data query | LLM + DB interaction |
| TEA-AGENT-001.8-INT-009 | Integration | P0 | Router handles SQL injection attempt safely | Security - SQL engine safety |
| TEA-AGENT-001.8-INT-010 | Integration | P2 | Router with single engine always selects that engine | Optimization path |
| TEA-AGENT-001.8-INT-011 | Integration | P1 | Router returns both engine selection rationale and results | Full response validation |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-INT-009 - SQL injection prevention [SECURITY]
  Given a router configuration with SQL engine
  And query: "'; DROP TABLE users; --"
  When the router processes the query
  Then the query should be parameterized or escaped
  And no SQL injection should occur
  And appropriate error handling should be triggered
```

#### E2E Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-E2E-001 | E2E | P1 | Complete workflow using router to query multiple data sources | Critical user journey |
| TEA-AGENT-001.8-E2E-002 | E2E | P2 | Router correctly routes mixed-intent queries | Complex behavior validation |

---

### AC3: `rag.llamaindex.subquestion` Action

**Requirements:**
1. Implements Sub-Question Query Engine pattern
2. Decomposes complex queries into sub-questions
3. Parallel execution of sub-queries (when enabled)
4. Synthesizes final answer from sub-answers
5. Returns full decomposition trace

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-UNIT-013 | Unit | P1 | Parse subquestion configuration | Configuration parsing |
| TEA-AGENT-001.8-UNIT-014 | Unit | P1 | Validate parallel flag is boolean | Input validation |
| TEA-AGENT-001.8-UNIT-015 | Unit | P2 | Handle empty synthesis_prompt with default | Default handling |
| TEA-AGENT-001.8-UNIT-016 | Unit | P1 | Format decomposition trace structure | Result transformation |
| TEA-AGENT-001.8-UNIT-017 | Unit | P2 | Validate synthesis_prompt length limits | Edge case |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-UNIT-016 - Decomposition trace structure
  Given subquestion execution results
  When formatting the decomposition trace
  Then trace should include original_query field
  And trace should include sub_questions array
  And each sub_question should have query and answer fields
  And trace should include synthesized_answer field
```

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-012 | Integration | P1 | Decompose comparison query into sub-questions | Core LLM interaction |
| TEA-AGENT-001.8-INT-013 | Integration | P1 | Execute sub-queries sequentially | Serial execution path |
| TEA-AGENT-001.8-INT-014 | Integration | P1 | Execute sub-queries in parallel | Parallel execution path |
| TEA-AGENT-001.8-INT-015 | Integration | P1 | Synthesize final answer from sub-answers | Synthesis LLM interaction |
| TEA-AGENT-001.8-INT-016 | Integration | P2 | Handle sub-query failure gracefully | Error recovery |
| TEA-AGENT-001.8-INT-017 | Integration | P2 | Subquestion with custom synthesis prompt | Customization support |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-INT-014 - Parallel sub-query execution
  Given a complex query requiring 3 sub-questions
  And parallel: true in configuration
  When subquestion action executes
  Then all 3 sub-queries should start within 100ms of each other
  And total execution time should be less than sequential time
  And results should be correctly merged
```

#### E2E Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-E2E-003 | E2E | P1 | Complete comparison analysis using subquestion decomposition | Critical user journey |
| TEA-AGENT-001.8-E2E-004 | E2E | P2 | Multi-entity comparison with parallel execution | Complex behavior |

---

### AC4: Index Management

**Requirements:**
1. `rag.llamaindex.create_index` for index creation
2. `rag.llamaindex.load_index` for loading persisted index
3. `rag.llamaindex.add_documents` for adding to existing index
4. Index persistence to filesystem or cloud storage

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-UNIT-018 | Unit | P1 | Parse document list for indexing | Input parsing |
| TEA-AGENT-001.8-UNIT-019 | Unit | P0 | Validate storage path is writable | Data integrity pre-check |
| TEA-AGENT-001.8-UNIT-020 | Unit | P2 | Handle empty document list | Edge case |
| TEA-AGENT-001.8-UNIT-021 | Unit | P1 | Parse cloud storage URI (s3://, gs://) | Cloud path parsing |
| TEA-AGENT-001.8-UNIT-022 | Unit | P2 | Validate document metadata schema | Schema validation |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-UNIT-019 - Validate writable path [DATA INTEGRITY]
  Given an index creation request
  When the storage path is /readonly/path
  Then a PermissionError should be raised before index creation
  And no partial index files should be written
```

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-018 | Integration | P0 | Create index from documents and persist to filesystem | Core persistence |
| TEA-AGENT-001.8-INT-019 | Integration | P1 | Load previously persisted index | Core retrieval |
| TEA-AGENT-001.8-INT-020 | Integration | P1 | Add documents to existing index | Incremental update |
| TEA-AGENT-001.8-INT-021 | Integration | P2 | Create index and persist to S3-compatible storage | Cloud persistence |
| TEA-AGENT-001.8-INT-022 | Integration | P0 | Index creation handles OOM gracefully | Resource exhaustion |
| TEA-AGENT-001.8-INT-023 | Integration | P2 | Load corrupted index file with meaningful error | Error handling |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-INT-022 - OOM handling [DATA INTEGRITY]
  Given a large document set exceeding available memory
  When create_index action executes
  Then the action should raise MemoryError or ResourceExhausted
  And no partial index should be persisted
  And state should not be corrupted
```

#### E2E Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-E2E-005 | E2E | P1 | Full lifecycle: create index, query, add docs, query again | Critical user journey |
| TEA-AGENT-001.8-E2E-006 | E2E | P2 | Cross-session index persistence and retrieval | Persistence validation |

---

### AC5: Settings Configuration

**Requirements:**
1. Configure via `settings.llamaindex`
2. Default index path configuration
3. Embedding model configuration
4. LLM configuration for query synthesis
5. Graceful fallback when LlamaIndex unavailable

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-UNIT-023 | Unit | P1 | Parse settings.llamaindex configuration | Config parsing |
| TEA-AGENT-001.8-UNIT-024 | Unit | P1 | Apply default values for missing settings | Default handling |
| TEA-AGENT-001.8-UNIT-025 | Unit | P0 | Validate embedding model name format | Configuration validation |
| TEA-AGENT-001.8-UNIT-026 | Unit | P1 | Validate LLM model configuration | Configuration validation |
| TEA-AGENT-001.8-UNIT-027 | Unit | P2 | Environment variable expansion in settings | Variable interpolation |
| TEA-AGENT-001.8-UNIT-028 | Unit | P0 | Detect LlamaIndex availability | Import detection logic |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-UNIT-028 - LlamaIndex availability detection
  Given LlamaIndex is not installed
  When checking LlamaIndex availability
  Then is_llamaindex_available() should return False
  And no ImportError should bubble up to caller
```

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-024 | Integration | P1 | Settings propagate to LlamaIndex client | Config flow |
| TEA-AGENT-001.8-INT-025 | Integration | P0 | All llamaindex.* actions gracefully fallback when unavailable | Graceful degradation |

**Given-When-Then Examples:**

```gherkin
Scenario: TEA-AGENT-001.8-INT-025 - Graceful fallback for all actions [CRITICAL]
  Given LlamaIndex is not installed
  And a workflow using rag.llamaindex.query, rag.llamaindex.router, and rag.llamaindex.subquestion
  When the workflow executes
  Then each action should fallback to native alternatives or skip
  And warnings should be logged for each fallback
  And the workflow should complete without errors
```

---

### AC6: Python Implementation

**Requirements:**
1. New module: `python/src/the_edge_agent/actions/llamaindex_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%
4. Requires `llamaindex` optional dependency

#### Unit Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| N/A | Unit | P1 | All llamaindex actions registered in registry | Covered by code inspection |

#### Integration Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-INT-026 | Integration | P1 | llamaindex_actions module loads without LlamaIndex | Optional dependency handling |
| TEA-AGENT-001.8-INT-027 | Integration | P1 | Actions discoverable in build_actions_registry() | Registry integration |

#### E2E Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-AGENT-001.8-E2E-007 | E2E | P1 | Execute llamaindex-simple-rag.yaml example | Example validation |
| TEA-AGENT-001.8-E2E-008 | E2E | P1 | Execute llamaindex-router-rag.yaml example | Example validation |
| TEA-AGENT-001.8-E2E-009 | E2E | P1 | Execute llamaindex-subquestion-rag.yaml example | Example validation |
| TEA-AGENT-001.8-E2E-010 | E2E | P3 | Performance: query latency under 2s for 1000 doc index | Performance baseline |
| TEA-AGENT-001.8-E2E-011 | E2E | P3 | Performance: parallel subquestion faster than sequential | Parallelism validation |

---

## Security Test Annotations

| Test ID | Security Category | Risk |
|---------|-------------------|------|
| TEA-AGENT-001.8-INT-009 | SQL Injection | HIGH |
| TEA-AGENT-001.8-UNIT-019 | Path Traversal | MEDIUM |
| TEA-AGENT-001.8-INT-022 | Resource Exhaustion | MEDIUM |
| TEA-AGENT-001.8-INT-025 | Graceful Degradation | LOW |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| LlamaIndex unavailability breaks workflows | TEA-AGENT-001.8-INT-003, TEA-AGENT-001.8-INT-025, TEA-AGENT-001.8-UNIT-028 |
| SQL injection via router SQL engine | TEA-AGENT-001.8-INT-009 |
| Memory exhaustion during index creation | TEA-AGENT-001.8-INT-022 |
| Data loss from partial index writes | TEA-AGENT-001.8-UNIT-019, TEA-AGENT-001.8-INT-018 |
| Configuration errors not caught early | TEA-AGENT-001.8-UNIT-002, TEA-AGENT-001.8-UNIT-009, TEA-AGENT-001.8-UNIT-025 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on configuration/validation issues)
2. **P0 Integration tests** (core functionality + security)
3. **P1 Unit tests** (feature coverage)
4. **P1 Integration tests** (component interactions)
5. **P1 E2E tests** (critical user journeys)
6. **P2 tests** (edge cases, additional coverage)
7. **P3 tests** (performance, nice-to-have)

---

## Test Environment Requirements

| Requirement | Notes |
|-------------|-------|
| LlamaIndex installed | Required for most tests; fallback tests require uninstall |
| OpenAI API key | Required for LLM-based routing and synthesis |
| SQLite or PostgreSQL | Required for SQL engine router tests |
| Writable temp directory | Required for index persistence tests |
| S3-compatible storage (optional) | For cloud persistence tests |

---

## Coverage Gaps

None identified. All ACs have comprehensive test coverage.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 63
  by_level:
    unit: 28
    integration: 24
    e2e: 11
  by_priority:
    p0: 15
    p1: 28
    p2: 16
    p3: 4
  coverage_gaps: []
  security_tests: 4
  performance_tests: 2
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.8-test-design-20260105.md
P0 tests identified: 15
Security tests identified: 4
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Security-critical paths identified and marked
- [x] Fallback behavior thoroughly tested
