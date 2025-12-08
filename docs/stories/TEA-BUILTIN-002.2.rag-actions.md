# Story TEA-BUILTIN-002.2: RAG Actions

## Status

Done

## Story

**As a** YAML agent developer,
**I want** built-in RAG actions (embedding create, vector store, vector query),
**so that** I can build knowledge-augmented agents with semantic search without writing Python code.

## Acceptance Criteria

1. `embedding.create` action generates embeddings from text using configurable models
2. `vector.store` action stores embeddings with metadata in a vector database
3. `vector.query` action performs semantic similarity search with filtering
4. Embedding providers are pluggable (OpenAI local/remote compatible API, Ollama local) with provider-agnostic interface
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

- [ ] **Optional**: Obtain `OPENAI_API_KEY` from https://platform.openai.com (for OpenAI remote embeddings)
- [ ] **Optional**: OpenAI-compatible local API (e.g., LocalAI, vLLM) with custom `base_url`
- [ ] **Optional**: Install Ollama from https://ollama.ai (for Ollama local embeddings)
- [ ] **Optional**: Pull Ollama embedding model: `ollama pull nomic-embed-text`
- [ ] **Optional**: `pip install chromadb` (for Chroma vector store)
- [ ] **Optional**: `pip install numpy` (for efficient similarity calculations)

## Tasks / Subtasks

- [x] Task 1: Implement embedding provider abstraction (AC: 1, 4)
  - [x] Create `EmbeddingProvider` protocol/interface
  - [x] Implement `OpenAIEmbeddingProvider` (local/remote compatible API)
    - [x] Support `text-embedding-3-small` (1536 dims), `text-embedding-3-large` (3072 dims), `text-embedding-ada-002`
    - [x] Support custom `base_url` for local OpenAI-compatible APIs (LocalAI, vLLM, etc.)
    - [x] Support custom dimensions for text-embedding-3-* models
  - [x] Implement `OllamaEmbeddingProvider` (local)
    - [x] Support `nomic-embed-text` (768 dims, 8K context)
    - [x] Support `mxbai-embed-large` (1024 dims)
    - [x] Support `all-minilm` (384 dims, lightweight)
    - [x] Support `bge-m3` (1024 dims, highest accuracy)
    - [x] Configurable `base_url` (default: http://localhost:11434)
    - [x] Configurable timeout
  - [x] Add configuration for provider selection in YAML
  - [x] Support batch embedding for efficiency

- [x] Task 2: Implement vector store abstraction (AC: 2, 3, 5)
  - [x] Create `VectorStore` protocol/interface
  - [x] Implement `InMemoryVectorStore` as default (no dependencies)
  - [x] Design document schema:
    ```python
    {
        "id": str,
        "text": str,
        "embedding": List[float],
        "metadata": dict
    }
    ```
  - [x] Support cosine similarity search
  - [x] Support metadata filtering

- [x] Task 3: Implement `embedding.create` action (AC: 1, 6, 7, 8)
  - [x] Define function signature: `embedding_create(state, text, model="text-embedding-3-small", batch=False, **kwargs)`
  - [x] Handle single text or list of texts
  - [x] Use configured embedding provider
  - [x] Return `{"embedding": List[float], "model": str, "dimensions": int}` for single
  - [x] Return `{"embeddings": List[List[float]], "model": str, "count": int}` for batch
  - [x] Register in actions dict with namespaces

- [x] Task 4: Implement `vector.store` action (AC: 2, 6, 7, 8)
  - [x] Define function signature: `vector_store(state, texts, embeddings=None, ids=None, metadata=None, collection="default", **kwargs)`
  - [x] Auto-generate embeddings if not provided
  - [x] Auto-generate IDs if not provided (UUID)
  - [x] Support batch storage
  - [x] Return `{"stored": int, "collection": str, "ids": List[str]}`
  - [x] Register in actions dict with namespaces

- [x] Task 5: Implement `vector.query` action (AC: 3, 7, 8)
  - [x] Define function signature: `vector_query(state, query, k=5, collection="default", filter=None, include_embeddings=False, **kwargs)`
  - [x] Generate embedding for query text
  - [x] Perform similarity search
  - [x] Apply metadata filters if provided
  - [x] Return:
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
  - [x] Register in actions dict with namespaces

- [x] Task 6: In-memory vector store implementation (AC: 5)
  - [x] Implement cosine similarity calculation
  - [x] Implement efficient search with numpy (optional dep)
  - [x] Fallback to pure Python if numpy not available
  - [x] Support persistence via pickle for checkpoints
  - [x] Implement metadata filtering with simple predicates

- [x] Task 7: Optional Chroma integration (AC: 5)
  - [x] Implement `ChromaVectorStore` wrapper
  - [x] Handle Chroma client initialization
  - [x] Make Chroma an optional dependency
  - [x] Document setup in YAML

- [x] Task 8: Write tests (AC: 9)
  - [x] Test embedding.create with mock OpenAI
  - [x] Test embedding.create batch mode
  - [x] Test vector.store single and batch
  - [x] Test vector.query similarity search
  - [x] Test vector.query with metadata filters
  - [x] Test in-memory store persistence
  - [x] Test collection isolation

- [x] Task 9: Update documentation (AC: 10)
  - [x] Add RAG actions to CLAUDE.md
  - [x] Add examples in docs/YAML_AGENTS.md
  - [x] Document vector store configuration
  - [x] Create example YAML showing RAG agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: None (in-memory store is pure Python)
- **Recommended**: `numpy` (for efficient similarity calculation)
- **Optional**: `openai` (for OpenAI embeddings - local/remote compatible API)
- **Optional**: Ollama runtime (for Ollama local embeddings - no pip package needed)
- **Optional**: `chromadb` (for Chroma vector store)

### Vector Store Configuration
```yaml
settings:
  rag:
    # Provider selection: "openai" (default) or "ollama"
    embedding_provider: openai

    # OpenAI settings (when provider=openai)
    embedding_model: text-embedding-3-small
    # openai_base_url: https://api.openai.com/v1  # default, or custom for local APIs

    # Ollama settings (when provider=ollama)
    # embedding_provider: ollama
    # embedding_model: nomic-embed-text
    # ollama_base_url: http://localhost:11434
    # ollama_timeout: 60.0

    vector_store: memory  # or chroma, pinecone
    chroma_path: ./chroma_db  # for persistent Chroma

secrets:
  OPENAI_API_KEY: ${OPENAI_API_KEY}  # Required for OpenAI remote provider
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

### Ollama Embedding Models Reference

| Model | Dimensions | Context | Use Case |
|-------|------------|---------|----------|
| `nomic-embed-text` | 768 | 8,192 tokens | Long documents, balanced |
| `mxbai-embed-large` | 1024 | 512 tokens | High accuracy, compact |
| `all-minilm` | 384 | 256 tokens | Lightweight, fast |
| `bge-m3` | 1024 | 8,192 tokens | Highest retrieval accuracy |

**Ollama API Endpoint**: `POST /api/embeddings`
```python
# Request
{"model": "nomic-embed-text", "prompt": "text to embed"}

# Response
{"embedding": [0.1, 0.2, ...]}
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
- Mock Ollama HTTP responses for Ollama provider tests
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

    # P1 - Core functionality (OpenAI Provider)
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
    def test_openai_custom_base_url(self): ...  # (P1) - OpenAI local/compatible API support

    # P1 - Core functionality (Ollama Provider)
    def test_ollama_embedding_create_single(self): ...  # (P1)
    def test_ollama_embedding_create_batch(self): ...  # (P1)
    def test_ollama_connection_error_handling(self): ...  # (P1)
    def test_ollama_model_not_found_error(self): ...  # (P1)
    def test_provider_switching_openai_to_ollama(self): ...  # (P1)

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

**Test Summary**: 29 tests (26 unit + 3 integration) | P0: 4 | P1: 21 | P2: 4

## Definition of Done

- [x] All acceptance criteria verified
- [x] All tasks completed
- [x] Tests pass (existing and new)
- [x] No regressions in existing YAML engine functionality
- [x] Documentation updated
- [x] Code follows existing patterns in yaml_engine.py
- [x] In-memory store works with zero dependencies

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
| 2025-12-06 | 0.3 | Added Ollama embedding provider support, OpenAI local/remote API support, updated tasks and tests | Sarah (PO Agent) |
| 2025-12-07 | 1.0 | Implementation complete - all tasks done | James (Dev Agent) |
| 2025-12-07 | 1.0 | QA Review - PASS | Quinn (Test Architect) |

## QA Results

### Review Date: 2025-12-07

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The RAG actions implementation is **excellent**. The code demonstrates:

1. **Clean Architecture**: Clear separation between embedding providers (Protocol-based abstraction), vector stores (ABC-based abstraction), and action registration
2. **Provider Abstraction (AC4)**: `EmbeddingProvider` protocol properly defines the interface with `embed()`, `dimensions`, and `model_name` properties
3. **Store Abstraction (AC5)**: `VectorStore` ABC with `add()`, `query()`, `get_state()`, `restore_state()` methods enables pluggability
4. **Error Handling**: All actions catch exceptions and return structured error responses with `{"error": str, "success": False}`
5. **Lazy Client Initialization**: OpenAI client is lazily created, reducing startup overhead
6. **Pure Python Fallback**: Cosine similarity works with or without numpy via conditional import

### Requirements Traceability

| AC# | Acceptance Criteria | Test Coverage | Status |
|-----|---------------------|---------------|--------|
| 1 | embedding.create generates embeddings from text | `test_embedding_create_single`, `test_embedding_create_batch` | ✓ PASS |
| 2 | vector.store stores embeddings with metadata | `test_vector_store_single`, `test_vector_store_batch` | ✓ PASS |
| 3 | vector.query performs similarity search with filtering | `test_vector_query_basic`, `test_vector_query_with_filter`, `test_vector_query_top_k` | ✓ PASS |
| 4 | Embedding providers pluggable (OpenAI, Ollama) | `test_embedding_provider_protocol_compliance`, `test_ollama_embedding_create_single`, `test_provider_switching_openai_to_ollama` | ✓ PASS |
| 5 | Vector stores pluggable (in-memory, Chroma) | `test_vector_store_protocol_compliance`, `test_chroma_integration` | ✓ PASS |
| 6 | Batch operations supported | `test_embedding_create_batch`, `test_vector_store_batch` | ✓ PASS |
| 7 | Actions follow `_setup_builtin_actions()` pattern | Actions registered via `register_actions(registry, engine)` | ✓ PASS |
| 8 | Dual namespace registration | `test_action_registration_namespaces` | ✓ PASS |
| 9 | Comprehensive unit tests | 32 tests covering P0/P1/P2 priorities | ✓ PASS |
| 10 | Documentation updated | CLAUDE.md and docs/YAML_AGENTS.md updated | ✓ PASS |

### Refactoring Performed

No refactoring required. The implementation follows best practices and existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, proper docstrings, type hints
- Project Structure: ✓ Actions module follows established pattern in `src/the_edge_agent/actions/`
- Testing Strategy: ✓ Tests organized by priority (P0/P1/P2), uses mocks appropriately
- All ACs Met: ✓ All 10 acceptance criteria verified with test coverage

### Test Architecture Assessment

**Strengths:**
- Well-organized test classes by priority level (P0, P1, P2)
- Fixtures for mock OpenAI client and sample embeddings
- Integration tests verify end-to-end RAG pipeline
- Chroma tests properly skipped when not installed
- 32 tests cover all critical paths

**Test Coverage:**
- P0 (Critical): 4 tests - Basic operations, error handling
- P1 (Core): 23 tests - OpenAI/Ollama providers, filters, checkpoints
- P2 (Advanced): 5 tests - Auto-embed, Chroma, pure Python fallback

### Security Review

✓ **No security concerns identified**

- API keys are loaded from environment variables (not hardcoded)
- No eval/exec of user input in RAG actions
- Filter syntax validation prevents injection
- Error messages don't leak sensitive information

### Performance Considerations

✓ **Well-designed for performance**

- Numpy-accelerated batch similarity calculation when available
- Pure Python fallback maintains correctness
- Lazy provider/client initialization
- Batch embedding support reduces API calls
- InMemoryVectorStore efficient for small-medium datasets

**Note:** For production with large datasets (>100k vectors), Chroma or other optimized vector stores recommended.

### Files Modified During Review

None - implementation meets quality standards.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-002.2-rag-actions.yml

### Recommended Status

✓ **Ready for Done**

All acceptance criteria verified, tests passing (32/32), documentation complete, and implementation follows established patterns.

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No issues encountered during implementation

### Completion Notes
- All 9 tasks completed successfully
- 32 tests written (30 unit + 2 Chroma integration), all passing
- 378 total tests in suite pass with no regressions
- Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md
- RAG actions follow existing action module patterns in `src/the_edge_agent/actions/`
- Fixed ChromaVectorStore to handle empty metadata dicts (Chroma requirement)

### File List

**New Files**:
- `src/the_edge_agent/actions/rag_actions.py` - Main RAG actions implementation
- `tests/test_yaml_engine_rag.py` - RAG actions test suite

**Modified Files**:
- `src/the_edge_agent/actions/__init__.py` - Added RAG actions registration
- `setup.py` - Added optional extras for rag, rag-chroma, llm, web, all
- `requirements.txt` - Updated with core deps and optional deps documentation
- `CLAUDE.md` - Added RAG actions documentation
- `docs/YAML_AGENTS.md` - Added RAG actions section with examples
- `docs/stories/TEA-BUILTIN-002.2.rag-actions.md` - Updated task checkboxes and status
