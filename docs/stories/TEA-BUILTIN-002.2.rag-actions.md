# Story TEA-BUILTIN-002.2: RAG Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in RAG actions (embedding create, vector store, vector query),
**so that** I can build knowledge-augmented agents with semantic search without writing Python code.

## Acceptance Criteria

1. `embedding.create` action generates embeddings from text using configurable models
2. `vector.store` action stores embeddings with metadata in a vector database
3. `vector.query` action performs semantic similarity search with filtering
4. Embedding providers are pluggable (OpenAI default, local models optional)
5. Vector stores are pluggable (in-memory default, Chroma/Pinecone optional)
6. Batch operations supported for efficiency with large documents
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `embedding.*`, `vector.*` and aliases
9. Comprehensive unit tests cover all RAG operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**:
- TEA-BUILTIN-001.2 (LLM Enhanced) - shares OpenAI client pattern

**Blocks**: None

**Internal Dependencies**:
- `vector.store` can auto-call `embedding.create` internally
- Uses same OpenAI client pattern as `llm.call`

## User Prerequisites

- [ ] **Required**: Obtain `OPENAI_API_KEY` from https://platform.openai.com (for embeddings)
- [ ] **Optional**: `pip install chromadb` (for Chroma vector store)
- [ ] **Optional**: `pip install numpy` (for efficient similarity calculations)

## Tasks / Subtasks

- [ ] Task 1: Implement embedding provider abstraction (AC: 1, 4)
  - [ ] Create `EmbeddingProvider` protocol/interface
  - [ ] Implement `OpenAIEmbeddingProvider` as default
  - [ ] Support `text-embedding-3-small`, `text-embedding-3-large`, `text-embedding-ada-002`
  - [ ] Add configuration for provider selection in YAML
  - [ ] Support batch embedding for efficiency

- [ ] Task 2: Implement vector store abstraction (AC: 2, 3, 5)
  - [ ] Create `VectorStore` protocol/interface
  - [ ] Implement `InMemoryVectorStore` as default (no dependencies)
  - [ ] Design document schema:
    ```python
    {
        "id": str,
        "text": str,
        "embedding": List[float],
        "metadata": dict
    }
    ```
  - [ ] Support cosine similarity search
  - [ ] Support metadata filtering

- [ ] Task 3: Implement `embedding.create` action (AC: 1, 6, 7, 8)
  - [ ] Define function signature: `embedding_create(state, text, model="text-embedding-3-small", batch=False, **kwargs)`
  - [ ] Handle single text or list of texts
  - [ ] Use configured embedding provider
  - [ ] Return `{"embedding": List[float], "model": str, "dimensions": int}` for single
  - [ ] Return `{"embeddings": List[List[float]], "model": str, "count": int}` for batch
  - [ ] Register in actions dict with namespaces

- [ ] Task 4: Implement `vector.store` action (AC: 2, 6, 7, 8)
  - [ ] Define function signature: `vector_store(state, texts, embeddings=None, ids=None, metadata=None, collection="default", **kwargs)`
  - [ ] Auto-generate embeddings if not provided
  - [ ] Auto-generate IDs if not provided (UUID)
  - [ ] Support batch storage
  - [ ] Return `{"stored": int, "collection": str, "ids": List[str]}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 5: Implement `vector.query` action (AC: 3, 7, 8)
  - [ ] Define function signature: `vector_query(state, query, k=5, collection="default", filter=None, include_embeddings=False, **kwargs)`
  - [ ] Generate embedding for query text
  - [ ] Perform similarity search
  - [ ] Apply metadata filters if provided
  - [ ] Return:
    ```python
    {
        "results": [
            {"id": str, "text": str, "score": float, "metadata": dict}
        ],
        "query": str,
        "collection": str,
        "k": int
    }
    ```
  - [ ] Register in actions dict with namespaces

- [ ] Task 6: In-memory vector store implementation (AC: 5)
  - [ ] Implement cosine similarity calculation
  - [ ] Implement efficient search with numpy (optional dep)
  - [ ] Fallback to pure Python if numpy not available
  - [ ] Support persistence via pickle for checkpoints
  - [ ] Implement metadata filtering with simple predicates

- [ ] Task 7: Optional Chroma integration (AC: 5)
  - [ ] Implement `ChromaVectorStore` wrapper
  - [ ] Handle Chroma client initialization
  - [ ] Make Chroma an optional dependency
  - [ ] Document setup in YAML

- [ ] Task 8: Write tests (AC: 9)
  - [ ] Test embedding.create with mock OpenAI
  - [ ] Test embedding.create batch mode
  - [ ] Test vector.store single and batch
  - [ ] Test vector.query similarity search
  - [ ] Test vector.query with metadata filters
  - [ ] Test in-memory store persistence
  - [ ] Test collection isolation

- [ ] Task 9: Update documentation (AC: 10)
  - [ ] Add RAG actions to CLAUDE.md
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Document vector store configuration
  - [ ] Create example YAML showing RAG agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: None (in-memory store is pure Python)
- **Recommended**: `numpy` (for efficient similarity calculation)
- **Optional**: `openai` (for OpenAI embeddings)
- **Optional**: `chromadb` (for Chroma vector store)

### Vector Store Configuration
```yaml
settings:
  rag:
    embedding_model: text-embedding-3-small
    vector_store: memory  # or chroma, pinecone
    chroma_path: ./chroma_db  # for persistent Chroma

secrets:
  OPENAI_API_KEY: ${OPENAI_API_KEY}
```

### Key Constraints
- In-memory store is default - zero external dependencies
- Embedding dimensions must match across operations
- Large batch operations should stream to avoid memory issues
- Vector store state should be serializable for checkpoints

### Cosine Similarity (Pure Python)
```python
def cosine_similarity(a: List[float], b: List[float]) -> float:
    dot = sum(x * y for x, y in zip(a, b))
    norm_a = sum(x * x for x in a) ** 0.5
    norm_b = sum(x * x for x in b) ** 0.5
    return dot / (norm_a * norm_b) if norm_a and norm_b else 0.0
```

### Metadata Filter Syntax
```yaml
- name: query_with_filter
  uses: vector.query
  with:
    query: "{{ state.question }}"
    k: 5
    filter:
      type: "article"
      date_gte: "2024-01-01"
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Priority Levels**:
- **P0**: Critical - Basic embedding/store/query operations
- **P1**: Core - Batch operations, filters, abstractions
- **P2**: Advanced - Auto-embed, optional integrations

**Testing Standards**:
- Mock OpenAI for embedding tests
- Use deterministic embeddings for search tests
- Test both with and without numpy

**Unit Test Cases**:
```python
class TestRAGActions(unittest.TestCase):
    # P0 - Critical
    def test_embedding_create_single(self): ...  # (P0)
    def test_vector_store_single(self): ...  # (P0)
    def test_vector_query_basic(self): ...  # (P0)
    def test_embedding_create_handles_api_error(self): ...  # (P0) - Handle OpenAI API failures

    # P1 - Core functionality
    def test_embedding_create_batch(self): ...  # (P1)
    def test_vector_store_batch(self): ...  # (P1)
    def test_vector_query_with_filter(self): ...  # (P1)
    def test_vector_query_top_k(self): ...  # (P1)
    def test_collection_isolation(self): ...  # (P1)
    def test_inmemory_persistence(self): ...  # (P1)
    def test_vector_store_rejects_mismatched_dimensions(self): ...  # (P1) - Validate embedding dimensions
    def test_vector_query_handles_empty_store(self): ...  # (P1) - Query on empty collection
    def test_metadata_filter_invalid_syntax(self): ...  # (P1) - Reject malformed filter expressions
    def test_embedding_provider_protocol_compliance(self): ...  # (P1) - AC4: Verify provider abstraction
    def test_vector_store_protocol_compliance(self): ...  # (P1) - AC5: Verify store abstraction
    def test_action_registration_namespaces(self): ...  # (P1) - AC8: Verify both namespaces work
    def test_vector_store_with_checkpoint(self): ...  # (P1) - Persistence across checkpoint save/load

    # P2 - Advanced features
    def test_vector_store_auto_embed(self): ...  # (P2)
    def test_vector_store_handles_duplicate_ids(self): ...  # (P2) - Handle duplicate document IDs
    def test_cosine_similarity_pure_python(self): ...  # (P2) - Fallback when numpy unavailable
    @unittest.skipUnless(has_chroma, "Chroma not installed")
    def test_chroma_integration(self): ...  # (P2)
```

**Integration Test Cases**:
```python
class TestRAGActionsIntegration(unittest.TestCase):
    def test_rag_pipeline_in_yaml_workflow(self): ...  # (P1)
    def test_embedding_with_llm_call(self): ...  # (P1)
```

**Test Summary**: 24 tests (21 unit + 3 integration) | P0: 4 | P1: 16 | P2: 4

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Code follows existing patterns in yaml_engine.py
- [ ] In-memory store works with zero dependencies

## Rollback Procedure

If RAG actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['embedding.create'] = embedding_create
   # actions['vector.store'] = vector_store
   # actions['vector.query'] = vector_query
   ```

2. **State Cleanup**:
   - In-memory vector store data is lost (by design)
   - Chroma DB files remain but can be deleted
   - No impact on workflow state

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify LLM actions still work

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_rag=False)`
   - Enable embedding.create first
   - Enable vector.store/query with in-memory backend
   - Enable Chroma integration last

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
