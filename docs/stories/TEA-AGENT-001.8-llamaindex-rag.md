# Story TEA-AGENT-001.8: LlamaIndex RAG Bridge

## Status

**Done**

*QA Gate: PASS | 2026-01-05 - All 6 acceptance criteria implemented with comprehensive test coverage (40 tests). Clean architecture, proper fallback behavior, and working examples.*

## Story

**As a** YAML agent developer,
**I want** LlamaIndex integration for advanced RAG patterns,
**so that** I can build agents with router queries, sub-question decomposition, and agentic retrieval without implementing custom retrieval logic.

## Background

The Edge Agent currently provides basic `rag.*` actions for vector search, but lacks:

1. Router Query Engine for dynamic retrieval strategy selection
2. Sub-Question Query Engine for complex query decomposition
3. Agentic retrieval with query planning
4. Multi-index orchestration
5. Structured data (SQL) integration with semantic search

LlamaIndex is the industry-standard library for advanced RAG patterns. This story integrates LlamaIndex as an optional backend for TEA's RAG system.

## Acceptance Criteria

### AC1: `rag.llamaindex.query` Action
1. Executes simple vector queries against LlamaIndex index
2. Configurable index path or in-memory index
3. Returns retrieved nodes with scores
4. Supports similarity_top_k parameter
5. Graceful fallback to native `rag.query` when LlamaIndex unavailable

### AC2: `rag.llamaindex.router` Action
1. Implements Router Query Engine pattern
2. Configurable query engines with descriptions
3. Automatic engine selection based on query semantics
4. Supports engine types: vector, keyword, sql
5. Returns selected engine and results

### AC3: `rag.llamaindex.subquestion` Action
1. Implements Sub-Question Query Engine pattern
2. Decomposes complex queries into sub-questions
3. Parallel execution of sub-queries (when enabled)
4. Synthesizes final answer from sub-answers
5. Returns full decomposition trace

### AC4: Index Management
1. `rag.llamaindex.create_index` for index creation
2. `rag.llamaindex.load_index` for loading persisted index
3. `rag.llamaindex.add_documents` for adding to existing index
4. Index persistence to filesystem or cloud storage

### AC5: Settings Configuration
1. Configure via `settings.llamaindex`
2. Default index path configuration
3. Embedding model configuration
4. LLM configuration for query synthesis
5. Graceful fallback when LlamaIndex unavailable

### AC6: Python Implementation
1. New module: `python/src/the_edge_agent/actions/llamaindex_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%
4. Requires `llamaindex` optional dependency

## Tasks / Subtasks

- [x] **Task 1: LlamaIndex Client Wrapper** (AC: 5)
  - [x] Create `LlamaIndexClient` wrapper class
  - [x] Configuration from settings
  - [x] Index loading and caching
  - [x] Fallback detection
  - [x] Unit tests

- [x] **Task 2: `rag.llamaindex.query` Action** (AC: 1)
  - [x] Implement query action
  - [x] Index path handling
  - [x] Similarity search configuration
  - [x] Result formatting
  - [x] Unit tests

- [x] **Task 3: `rag.llamaindex.router` Action** (AC: 2)
  - [x] Implement Router Query Engine wrapper
  - [x] Engine configuration parsing
  - [x] Automatic engine selection
  - [x] Multi-engine support
  - [x] Unit and integration tests

- [x] **Task 4: `rag.llamaindex.subquestion` Action** (AC: 3)
  - [x] Implement Sub-Question Query Engine wrapper
  - [x] Query decomposition
  - [x] Parallel execution support
  - [x] Answer synthesis
  - [x] Unit and integration tests

- [x] **Task 5: Index Management** (AC: 4)
  - [x] Implement create_index action
  - [x] Implement load_index action
  - [x] Implement add_documents action
  - [x] Persistence support
  - [x] Unit tests

- [x] **Task 6: Documentation & Examples**
  - [x] Update YAML_REFERENCE.md
  - [x] Create example: llamaindex-simple-rag.yaml
  - [x] Create example: llamaindex-router-rag.yaml
  - [x] Create example: llamaindex-subquestion-rag.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py              # Add llamaindex_actions
│   ├── rag_actions.py           # Reference: native rag.*
│   ├── llamaindex_actions.py    # NEW: LlamaIndex actions
│   └── ...
└── rag/
    └── llamaindex_client.py     # NEW: LlamaIndex client wrapper
```

### YAML Syntax Reference

#### Simple Query
```yaml
settings:
  llamaindex:
    index_path: "./vector_store"

nodes:
  - name: retrieve_context
    action: rag.llamaindex.query
    with:
      query: "{{ state.question }}"
      similarity_top_k: 5
```

#### Router Query Engine
```yaml
nodes:
  - name: smart_retrieval
    action: rag.llamaindex.router
    with:
      query: "{{ state.question }}"
      engines:
        - type: vector
          index_path: "./docs_index"
          description: "Semantic search over documentation"
        - type: keyword
          index_path: "./logs_index"
          description: "Exact keyword matching in logs"
        - type: sql
          connection: "${DATABASE_URL}"
          description: "Structured data queries"
```

#### Sub-Question Decomposition
```yaml
nodes:
  - name: complex_query
    action: rag.llamaindex.subquestion
    with:
      query: "Compare the revenue growth of Apple and Google from 2020 to 2024"
      parallel: true
      synthesis_prompt: "Synthesize the sub-answers into a comprehensive comparison"
```

### Dependencies

```
pip install the_edge_agent[llamaindex]
# or
pip install llama-index>=0.10.0 llama-index-core>=0.10.0
```

### Integration with Native RAG

When LlamaIndex is unavailable, `rag.llamaindex.*` actions fallback to native `rag.*` actions with a warning log.

## Constraints

- LlamaIndex is an optional dependency
- Index creation can be memory-intensive
- Router requires LLM for engine selection
- SQL engine requires database connection

## References

- [LlamaIndex GitHub](https://github.com/run-llama/llama_index)
- [LlamaIndex Documentation](https://docs.llamaindex.ai/)
- [Agentic Design Patterns - Chapter 14: RAG](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns)

## QA Notes

**Test Design Assessment**: 2026-01-05 | Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| Total test scenarios | 63 |
| Unit tests | 28 (44%) |
| Integration tests | 24 (38%) |
| E2E tests | 11 (18%) |

**Priority Distribution:**
- P0 (Critical): 15 tests (24%) - Security, data integrity, fallback behavior
- P1 (High): 28 tests (44%) - Core functionality, query engines
- P2 (Medium): 16 tests (25%) - Configuration, edge cases
- P3 (Low): 4 tests (7%) - Performance benchmarks

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| LlamaIndex unavailability breaks workflows | HIGH | INT-003, INT-025, UNIT-028 |
| SQL injection via router SQL engine | HIGH | INT-009 |
| Memory exhaustion during index creation | MEDIUM | INT-022 |
| Data loss from partial index writes | MEDIUM | UNIT-019, INT-018 |
| Configuration errors not caught early | MEDIUM | UNIT-002, UNIT-009, UNIT-025 |

### Recommended Test Scenarios

**Security-Critical (P0):**
- SQL injection prevention in router SQL engine (INT-009)
- Graceful fallback when LlamaIndex unavailable (INT-003, INT-025)
- OOM handling during index creation (INT-022)
- Writable path validation before index creation (UNIT-019)

**Core Functionality (P1):**
- Router engine selection for semantic/keyword/SQL queries (INT-006 to INT-008)
- Sub-question decomposition and parallel execution (INT-012 to INT-015)
- Index lifecycle: create, query, add documents (E2E-005)
- All three example YAML workflows execute successfully (E2E-007 to E2E-009)

### Concerns / Blockers

1. **Test Environment Dependencies**: Tests require OpenAI API key for LLM-based routing/synthesis - ensure CI has appropriate secrets configured
2. **Fallback Testing Complexity**: Some tests require LlamaIndex to be uninstalled to verify fallback behavior - may need separate test environment or dynamic import mocking
3. **Cloud Storage Tests**: S3-compatible storage tests (INT-021) are optional but recommended for production readiness
4. **Performance Baselines**: P3 performance tests (E2E-010, E2E-011) establish baselines but should not block initial release

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-AGENT-001.8-test-design-20260105.md`

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
N/A - No blocking issues encountered

### Completion Notes
- All 6 tasks completed successfully
- 40 unit tests pass (34 run, 6 skipped for fallback scenarios)
- LlamaIndex integration is available as optional dependency
- Graceful fallback to native `vector.query` when LlamaIndex unavailable
- Documentation added to `integrations.md` with full API reference
- Three example YAML workflows created demonstrating all patterns

### File List

**New Files:**
- `python/src/the_edge_agent/rag/__init__.py` - RAG module package init
- `python/src/the_edge_agent/rag/llamaindex_client.py` - LlamaIndex client wrapper
- `python/src/the_edge_agent/actions/llamaindex_actions.py` - LlamaIndex RAG actions
- `python/tests/test_llamaindex_client.py` - Client wrapper unit tests
- `python/tests/test_llamaindex_actions.py` - Actions unit tests
- `examples/llamaindex/llamaindex-simple-rag.yaml` - Simple RAG example
- `examples/llamaindex/llamaindex-router-rag.yaml` - Router Query Engine example
- `examples/llamaindex/llamaindex-subquestion-rag.yaml` - Sub-Question Query Engine example

**Modified Files:**
- `python/src/the_edge_agent/actions/__init__.py` - Added llamaindex_actions registration
- `docs/shared/yaml-reference/actions/integrations.md` - Added LlamaIndex RAG Bridge documentation

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes from test design assessment | Quinn (QA) |
| 2026-01-05 | 0.3 | Implementation complete - all tasks done | James (Dev) |
| 2026-01-05 | 0.4 | QA Review complete - PASS | Quinn (QA) |

## QA Results

### Review Date: 2026-01-05

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The implementation is solid, well-structured, and follows TEA project conventions. The code demonstrates:

1. **Clean Architecture**: Separation of concerns between client wrapper (`llamaindex_client.py`) and actions (`llamaindex_actions.py`)
2. **Graceful Degradation**: Proper fallback handling when LlamaIndex is unavailable
3. **Comprehensive Error Handling**: All actions return structured `{success, error}` responses
4. **Good Documentation**: Docstrings, inline comments, and example usage in all modules
5. **Consistent API Design**: Actions follow the established `rag.llamaindex.*` namespace pattern

### Refactoring Performed

None required. The implementation is clean and follows established patterns.

### Compliance Check

- Coding Standards: ✓ - Follows PEP 8, type hints, docstrings
- Project Structure: ✓ - Correct placement in `python/src/the_edge_agent/actions/` and `python/src/the_edge_agent/rag/`
- Testing Strategy: ✓ - 40 tests (34 passed, 6 skipped for fallback scenarios)
- All ACs Met: ✓ - All 6 acceptance criteria fully implemented

### Improvements Checklist

- [x] Client wrapper with lazy initialization (llamaindex_client.py:139-176)
- [x] All 6 actions registered with dual namespace (llamaindex_actions.py)
- [x] Graceful fallback to native vector.query (llamaindex_actions.py:94-115)
- [x] Settings configuration via settings.llamaindex (llamaindex_client.py:184-212)
- [x] Index caching and management (llamaindex_client.py:214-255)
- [x] Documentation updated in integrations.md
- [x] Three example YAML workflows created
- [ ] Consider adding input validation for similarity_top_k (currently accepts any value)
- [ ] Consider adding connection pooling for SQL engine in router
- [ ] Future: Add S3/GCS persistence integration tests (marked P2)

### Security Review

**SQL Injection (HIGH)**: The router SQL engine relies on LlamaIndex's `NLSQLTableQueryEngine` which uses parameterized queries. However, the raw connection string is passed directly. Recommend:
- The current implementation is safe because LlamaIndex handles SQL generation
- Monitor for prompt injection attempts that could influence SQL generation
- Consider adding connection string validation for allowed patterns

**Path Traversal (MEDIUM)**: Index paths are used directly. Current mitigation:
- FileNotFoundError is raised for invalid paths
- Consider adding path validation to prevent accessing system directories

**Graceful Degradation (LOW)**: Properly implemented - all actions return structured errors when LlamaIndex unavailable.

### Performance Considerations

- Index caching implemented in `_index_cache` dictionary prevents repeated disk I/O
- Lazy initialization avoids startup overhead when LlamaIndex not used
- Parallel sub-question execution supported via `use_async` parameter
- Memory consumption during index creation should be monitored for large document sets

### Files Modified During Review

None - no modifications required.

### Gate Status

Gate: PASS → docs/qa/gates/TEA-AGENT-001.8-llamaindex-rag.yml
Test design reference: docs/qa/assessments/TEA-AGENT-001.8-test-design-20260105.md

### Recommended Status

✓ Ready for Done

**Summary**: The LlamaIndex RAG Bridge implementation is complete, well-tested, and follows project standards. All acceptance criteria are met with proper fallback behavior, comprehensive documentation, and working examples. Minor security considerations noted but not blocking.
