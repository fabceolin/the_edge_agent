# Story TEA-BUILTIN-002.4: model2vec Embedding Provider

## Status

**Ready for Development**

_Validated: 2026-01-02 - All story checklist criteria passed. QA test design complete._

## Story

**As a** YAML agent developer,
**I want** a local embedding provider using model2vec,
**so that** I can generate embeddings without API calls or external services.

## Story Context

**Existing System Integration:**
- Extends: TEA-BUILTIN-002.2 (RAG Actions) - adds new embedding provider
- Integrates with: `EmbeddingProvider` protocol, `embedding.create`, `vector.store`
- Technology: Python 3.9+, model2vec library
- Pattern: Follows `OpenAIEmbeddingProvider`, `OllamaEmbeddingProvider` patterns
- Touch points: `actions/rag_actions.py`

**Reference Implementation:** [semtools](https://github.com/run-llama/semtools)
- Model usage: `minishlab/potion-multilingual-128M`
- Encoding: `model.encode(texts)` returns `np.ndarray` shape `(n, 128)`

**Rationale:** For semantic file search, local embeddings are preferred because:
1. No API costs for embedding large codebases
2. No network latency
3. Works offline
4. Consistent results (no model version drift)

## Acceptance Criteria

**Functional Requirements:**

1. `Model2VecEmbeddingProvider` implements `EmbeddingProvider` protocol
2. Supports `minishlab/potion-multilingual-128M` model (128 dims)
3. Lazy model loading on first use (cached)
4. Batch embedding for efficiency
5. Model downloaded from HuggingFace Hub on first use
6. Cached in `~/.cache/huggingface/` (standard location)

**Integration Requirements:**

7. Configurable via `settings.rag.embedding_provider: model2vec`
8. Works with existing `embedding.create` action
9. Works with existing `vector.store` and `vector.query` actions
10. Default provider for `vector.index_files` when indexing files
11. Optional dependency - core package works without model2vec

**Quality Requirements:**

12. Unit tests with mocked model
13. Integration test with real model (optional, slow)
14. Clear error message if model2vec not installed
15. Type hints throughout (mypy compatible)

## Dependencies

**Blocked By:**
- TEA-BUILTIN-002.2 (RAG Actions) - must be complete (Done)

**Blocks:**
- None

**Related:**
- TEA-BUILTIN-002.3 (vector.index_files) - uses this as default provider

## User Prerequisites

- [ ] `pip install model2vec` (or `pip install the_edge_agent[search]`)
- [ ] `pip install numpy`
- [ ] ~500MB disk space for model download (first use)
- [ ] Internet connection for initial model download

## Tasks / Subtasks

- [ ] **Task 1: Implement Model2VecEmbeddingProvider** (AC: 1, 2, 3, 4)
  - [ ] Create `Model2VecEmbeddingProvider` class
  - [ ] Implement `EmbeddingProvider` protocol:
    - [ ] `embed(texts: Union[str, List[str]]) -> List[List[float]]`
    - [ ] `dimensions: int` property (returns 128)
    - [ ] `model_name: str` property
  - [ ] Lazy model loading via module-level cache
  - [ ] Batch encoding using `model.encode(texts)`

- [ ] **Task 2: Add to provider factory** (AC: 7, 8, 9)
  - [ ] Update `create_embedding_provider()` to handle `provider="model2vec"`
  - [ ] Add `model2vec` to provider selection logic
  - [ ] Default to `model2vec` for file indexing operations

- [ ] **Task 3: Handle optional dependency** (AC: 11, 14)
  - [ ] Try/except import of `model2vec`
  - [ ] Clear error message: "model2vec not installed. Install with: pip install model2vec"
  - [ ] Optional dependency in setup.py `[search]` extra

- [ ] **Task 4: Configure as default for file indexing** (AC: 10)
  - [ ] In `vector.index_files`, default to `model2vec` if available
  - [ ] Fall back to configured provider if model2vec not installed
  - [ ] Document this behavior

- [ ] **Task 5: Add dependencies** (AC: 11)
  - [ ] Add `model2vec` to optional `[search]` extra in setup.py
  - [ ] Ensure `numpy` is already a dependency (or add)

- [ ] **Task 6: Testing** (AC: 12, 13, 15)
  - [ ] Test provider protocol compliance
  - [ ] Test single text embedding
  - [ ] Test batch embedding
  - [ ] Test lazy loading behavior
  - [ ] Test error when model2vec not installed (mocked)
  - [ ] Integration test with real model (marked slow)

## Dev Notes

### Model2Vec Usage

```python
from model2vec import StaticModel

# Load model (downloads on first use, ~500MB)
model = StaticModel.from_pretrained("minishlab/potion-multilingual-128M")

# Embed single or batch
embeddings = model.encode(["text 1", "text 2"])  # shape: (2, 128)

# Single text
embedding = model.encode(["single text"])[0]  # shape: (128,)
```

### Provider Implementation

```python
from typing import List, Union, Optional
import numpy as np

class Model2VecEmbeddingProvider:
    """
    Local embedding provider using model2vec (AC: 1, 2).

    Uses minishlab/potion-multilingual-128M model (128 dimensions).
    Model is lazy-loaded and cached at module level.
    """

    MODEL_NAME = "minishlab/potion-multilingual-128M"
    DIMENSIONS = 128

    _model = None  # Module-level cache

    def __init__(self, model: Optional[str] = None):
        """
        Initialize model2vec provider.

        Args:
            model: Model name (default: minishlab/potion-multilingual-128M)
        """
        self._model_name = model or self.MODEL_NAME

    def _get_model(self):
        """Lazy load model (AC: 3)."""
        if Model2VecEmbeddingProvider._model is None:
            try:
                from model2vec import StaticModel
            except ImportError:
                raise ImportError(
                    "model2vec not installed. Install with: pip install model2vec"
                )

            Model2VecEmbeddingProvider._model = StaticModel.from_pretrained(
                self._model_name
            )

        return Model2VecEmbeddingProvider._model

    def embed(self, texts: Union[str, List[str]]) -> List[List[float]]:
        """Generate embeddings using model2vec (AC: 4)."""
        model = self._get_model()

        # Normalize to list
        if isinstance(texts, str):
            texts = [texts]

        # Encode batch
        embeddings = model.encode(texts)

        # Convert numpy to list
        if isinstance(embeddings, np.ndarray):
            return embeddings.tolist()
        return embeddings

    @property
    def dimensions(self) -> int:
        return self.DIMENSIONS

    @property
    def model_name(self) -> str:
        return self._model_name
```

### Settings Configuration

```yaml
settings:
  rag:
    # Use model2vec for local embeddings (no API)
    embedding_provider: model2vec
    embedding_model: minishlab/potion-multilingual-128M  # default

    # Or use OpenAI/Ollama as before
    # embedding_provider: openai
    # embedding_provider: ollama
```

### Provider Factory Update

```python
def create_embedding_provider(
    provider: str = "openai",
    model: Optional[str] = None,
    ...
) -> EmbeddingProvider:
    """Factory function to create embedding providers."""

    if provider == "openai":
        return OpenAIEmbeddingProvider(...)
    elif provider == "ollama":
        return OllamaEmbeddingProvider(...)
    elif provider == "model2vec":
        return Model2VecEmbeddingProvider(model=model)
    else:
        raise ValueError(f"Unknown embedding provider: {provider}")
```

### Default for File Indexing

In `vector.index_files`, prefer model2vec for file operations:

```python
def vector_index_files(state, paths, **kwargs):
    # Use model2vec as default for file indexing if available
    provider_type = kwargs.get("provider")
    if provider_type is None:
        try:
            import model2vec
            provider_type = "model2vec"
        except ImportError:
            # Fall back to configured provider
            provider_type = None  # Will use settings default

    # ... rest of implementation
```

## Testing

**Test File Location:** `python/tests/test_model2vec_provider.py`

| Test Case | Priority | Description |
|-----------|----------|-------------|
| `test_protocol_compliance` | P0 | Implements EmbeddingProvider protocol |
| `test_embed_single_text` | P0 | Single text returns embedding |
| `test_embed_batch` | P0 | Batch returns list of embeddings |
| `test_dimensions` | P0 | Returns 128 dimensions |
| `test_model_name` | P1 | Returns correct model name |
| `test_lazy_loading` | P1 | Model not loaded until first embed |
| `test_model_cached` | P1 | Model reused across calls |
| `test_import_error` | P1 | Clear error when model2vec missing |
| `test_factory_creates_provider` | P1 | Factory handles model2vec |
| `test_integration_real_model` | P2 | Real embedding with actual model |

**Mock Strategy:**
- Mock `model2vec.StaticModel` for unit tests
- Use real model only for P2 integration test (marked slow)

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] Provider implements `EmbeddingProvider` protocol
- [ ] Lazy model loading verified
- [ ] Works with existing `embedding.create`, `vector.store`, `vector.query`
- [ ] Optional dependency properly configured
- [ ] Clear error message when not installed
- [ ] Unit tests pass (mocked)
- [ ] Integration test passes (real model)
- [ ] Type hints pass mypy check
- [ ] Existing RAG tests still pass
- [ ] Dependencies added to setup.py `[search]` extra

## QA Notes

**Assessment Date:** 2026-01-02
**Assessed By:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total Test Scenarios** | 21 |
| **Unit Tests** | 12 (57%) |
| **Integration Tests** | 7 (33%) |
| **E2E Tests** | 3 (14%) |
| **P0 (Critical)** | 8 |
| **P1 (High)** | 8 |
| **P2 (Medium)** | 5 |

**Coverage Assessment:** All 15 acceptance criteria have mapped test coverage. Protocol compliance (AC1) and core functionality (AC2-4) have strong P0 test coverage.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| Protocol violation breaks RAG actions | HIGH | P0 unit tests for `EmbeddingProvider` protocol compliance |
| Dimension mismatch with vector stores | HIGH | Explicit dimension verification in 002.4-UNIT-005 |
| Memory leak from cached model | MEDIUM | Lazy loading + module-level cache tested in 002.4-UNIT-009 |
| Confusing error for missing dependency | MEDIUM | Clear ImportError message tested in 002.4-UNIT-012 |
| Network failure during model download | LOW | Gap - not explicitly tested; HuggingFace hub handles retries |

### Recommended Test Scenarios

**Priority 0 (Must Pass Before Merge):**
1. `002.4-UNIT-001-004`: Protocol compliance - embed signature, return type, properties
2. `002.4-UNIT-005`: Dimensions returns exactly 128
3. `002.4-UNIT-010-011`: Single and batch embedding correctness
4. `002.4-UNIT-012`: ImportError with clear message
5. `002.4-INT-005`: Factory creates provider for `provider="model2vec"`

**Priority 1 (Should Pass):**
1. `002.4-UNIT-006-009`: Model name, lazy loading, caching behavior
2. `002.4-INT-002`: Large batch (100+ texts) performance
3. `002.4-INT-006-007`: RAG action integration, default provider logic
4. `002.4-E2E-001`: YAML agent with model2vec setting

**Priority 2 (Nice to Have):**
1. `002.4-INT-001`: Real model 128-dim verification
2. `002.4-INT-003-004`: HuggingFace download and cache persistence
3. `002.4-E2E-002-003`: File indexing workflow, mypy check

### Concerns and Notes

1. **Network Dependency:** Model download from HuggingFace Hub on first use requires network. Integration tests with real model should be marked `@pytest.mark.slow` and optionally skipped in CI.

2. **Module-Level Cache:** The `_model` class variable cache pattern is simple but means model cannot be unloaded. Acceptable for intended use case.

3. **Optional Dependency:** Ensure `[search]` extra is properly documented. Users without `model2vec` should get clear error, not cryptic ImportError.

4. **No Blockers:** Story is well-structured with clear implementation guidance. Ready for development.

### Test Design Reference

Full test design: `docs/qa/assessments/TEA-BUILTIN-002.4-test-design-20260102.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial draft (unified from TEA-BUILTIN-014.1) | Sarah (PO) |
| 2026-01-02 | 0.2 | Added QA Notes section | Quinn (QA) |
