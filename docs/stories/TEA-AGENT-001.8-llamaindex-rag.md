# Story TEA-AGENT-001.8: LlamaIndex RAG Bridge

## Status

**Draft**

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

- [ ] **Task 1: LlamaIndex Client Wrapper** (AC: 5)
  - [ ] Create `LlamaIndexClient` wrapper class
  - [ ] Configuration from settings
  - [ ] Index loading and caching
  - [ ] Fallback detection
  - [ ] Unit tests

- [ ] **Task 2: `rag.llamaindex.query` Action** (AC: 1)
  - [ ] Implement query action
  - [ ] Index path handling
  - [ ] Similarity search configuration
  - [ ] Result formatting
  - [ ] Unit tests

- [ ] **Task 3: `rag.llamaindex.router` Action** (AC: 2)
  - [ ] Implement Router Query Engine wrapper
  - [ ] Engine configuration parsing
  - [ ] Automatic engine selection
  - [ ] Multi-engine support
  - [ ] Unit and integration tests

- [ ] **Task 4: `rag.llamaindex.subquestion` Action** (AC: 3)
  - [ ] Implement Sub-Question Query Engine wrapper
  - [ ] Query decomposition
  - [ ] Parallel execution support
  - [ ] Answer synthesis
  - [ ] Unit and integration tests

- [ ] **Task 5: Index Management** (AC: 4)
  - [ ] Implement create_index action
  - [ ] Implement load_index action
  - [ ] Implement add_documents action
  - [ ] Persistence support
  - [ ] Unit tests

- [ ] **Task 6: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: llamaindex-simple-rag.yaml
  - [ ] Create example: llamaindex-router-rag.yaml
  - [ ] Create example: llamaindex-subquestion-rag.yaml

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

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial story from Sprint Change Proposal | Sarah (PO) |
