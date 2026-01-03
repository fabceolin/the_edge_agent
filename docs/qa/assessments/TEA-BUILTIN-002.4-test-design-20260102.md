# Test Design: Story TEA-BUILTIN-002.4

Date: 2026-01-02
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 21
- **Unit tests:** 11 (52%)
- **Integration tests:** 7 (33%)
- **E2E tests:** 3 (14%)
- **Priority distribution:** P0: 8, P1: 8, P2: 5

## Story Summary

**Title:** model2vec Embedding Provider
**Objective:** Implement a local embedding provider using model2vec library that enables embedding generation without API calls or external services.

## Test Scenarios by Acceptance Criteria

### AC1: Model2VecEmbeddingProvider implements EmbeddingProvider protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-001 | Unit | P0 | Verify `embed()` method signature accepts `Union[str, List[str]]` | Core protocol compliance - type contract |
| 002.4-UNIT-002 | Unit | P0 | Verify `embed()` returns `List[List[float]]` | Core protocol compliance - return type |
| 002.4-UNIT-003 | Unit | P0 | Verify `dimensions` property exists and returns `int` | Protocol property compliance |
| 002.4-UNIT-004 | Unit | P1 | Verify `model_name` property exists and returns `str` | Protocol property compliance |

### AC2: Supports minishlab/potion-multilingual-128M model (128 dims)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-005 | Unit | P0 | Verify `dimensions` returns exactly 128 | Critical dimension contract |
| 002.4-UNIT-006 | Unit | P1 | Verify `model_name` returns "minishlab/potion-multilingual-128M" by default | Model configuration correctness |
| 002.4-INT-001 | Integration | P2 | Verify real model produces 128-dimensional embeddings | Integration with actual model2vec library |

### AC3: Lazy model loading on first use (cached)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-007 | Unit | P1 | Verify model not loaded on `__init__()` | Lazy loading - no side effects at construction |
| 002.4-UNIT-008 | Unit | P1 | Verify model loaded on first `embed()` call | Lazy loading triggers on first use |
| 002.4-UNIT-009 | Unit | P1 | Verify subsequent `embed()` calls reuse cached model | Module-level caching prevents reload |

### AC4: Batch embedding for efficiency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-010 | Unit | P0 | Single text input returns list with one embedding | Batch API handles single correctly |
| 002.4-UNIT-011 | Unit | P0 | Multiple texts return correct number of embeddings | Batch processing correctness |
| 002.4-INT-002 | Integration | P1 | Large batch (100+ texts) processes efficiently | Performance under realistic load |

### AC5-6: Model downloaded from HuggingFace Hub, cached in ~/.cache/huggingface/

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-003 | Integration | P2 | First embed() call downloads model if not cached | HuggingFace integration |
| 002.4-INT-004 | Integration | P2 | Subsequent calls use cached model (no download) | Cache persistence verification |

### AC7-9: Configurable via settings, works with existing actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-005 | Integration | P0 | Factory creates Model2VecEmbeddingProvider for `provider="model2vec"` | Provider factory integration |
| 002.4-INT-006 | Integration | P1 | `embedding.create` action works with model2vec provider | RAG action integration |
| 002.4-E2E-001 | E2E | P1 | YAML agent with `embedding_provider: model2vec` setting executes correctly | Full settings integration |

### AC10: Default provider for vector.index_files when indexing files

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-007 | Integration | P1 | `vector.index_files` uses model2vec by default when available | Default provider selection logic |
| 002.4-E2E-002 | E2E | P2 | File indexing workflow completes with model2vec provider | End-to-end file indexing |

### AC11, 14: Optional dependency with clear error message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-012 | Unit | P0 | ImportError raised with clear message when model2vec not installed | Error handling for missing dependency |

### AC12-15: Testing and type hints

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-E2E-003 | E2E | P2 | Mypy type check passes on provider module | Type hint completeness |

## Test Implementation Details

### Unit Test Suite (Mocked)

**File:** `python/tests/test_model2vec_provider.py`

```python
# Mock strategy for unit tests
@pytest.fixture
def mock_model2vec(mocker):
    """Mock model2vec.StaticModel for unit tests."""
    mock_model = mocker.Mock()
    mock_model.encode.return_value = np.array([[0.1] * 128])
    mock_static = mocker.patch("model2vec.StaticModel")
    mock_static.from_pretrained.return_value = mock_model
    return mock_model
```

### Integration Test Suite

**File:** `python/tests/integration/test_model2vec_integration.py`

```python
# Mark slow tests for real model interaction
@pytest.mark.slow
@pytest.mark.integration
def test_real_model_embedding():
    """Test with actual model2vec model."""
    ...
```

### E2E Test Suite

**File:** `python/tests/e2e/test_model2vec_e2e.py`

```python
@pytest.mark.e2e
def test_yaml_agent_with_model2vec():
    """Test complete YAML agent workflow with model2vec."""
    ...
```

## Risk Mitigation Coverage

| Risk | Test Coverage | Mitigating Tests |
|------|---------------|------------------|
| Protocol violation breaks RAG actions | High | 002.4-UNIT-001 through 002.4-UNIT-004 |
| Dimension mismatch with vector stores | High | 002.4-UNIT-005, 002.4-INT-001 |
| Memory leak from model loading | Medium | 002.4-UNIT-009 |
| Confusing error for missing dependency | High | 002.4-UNIT-012 |
| Batch processing failure | High | 002.4-UNIT-010, 002.4-UNIT-011 |

## Recommended Execution Order

1. **P0 Unit tests** (002.4-UNIT-001 through 005, 010, 011, 012) - Fail fast on protocol/core issues
2. **P0 Integration tests** (002.4-INT-005) - Verify factory integration
3. **P1 Unit tests** (002.4-UNIT-006 through 009) - Lazy loading behavior
4. **P1 Integration tests** (002.4-INT-002, 006, 007) - Component interactions
5. **P1 E2E tests** (002.4-E2E-001) - YAML configuration
6. **P2+ tests** (002.4-INT-001, 003, 004, E2E-002, E2E-003) - As time permits

## Coverage Gap Analysis

| Acceptance Criteria | Test Coverage | Gap |
|---------------------|---------------|-----|
| AC1: Protocol implementation | Full | None |
| AC2: Model support | Full | None |
| AC3: Lazy loading | Full | None |
| AC4: Batch embedding | Full | None |
| AC5-6: Model caching | Medium | Network failure scenarios not covered |
| AC7-9: Settings integration | Full | None |
| AC10: Default provider | Full | None |
| AC11, 14: Optional dependency | Full | None |
| AC12-15: Quality | Full | None |

## Test Data Requirements

### Mock Data

- Sample texts: `["Hello world", "Test embedding", "Multiple texts"]`
- Mock embedding output: `np.array([[0.1] * 128, [0.2] * 128, [0.3] * 128])`

### Real Model Test Data

- Unicode: `"Bonjour le monde"` (French)
- Long text: 1000+ character paragraph
- Edge cases: empty string, whitespace-only, special characters

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for boundaries)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (protocol compliance = P0)
- [x] Test IDs follow naming convention (002.4-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 21
  by_level:
    unit: 12
    integration: 7
    e2e: 3
  by_priority:
    p0: 8
    p1: 8
    p2: 5
  coverage_gaps:
    - "Network failure scenarios during model download not explicitly tested"
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-002.4-test-design-20260102.md
P0 tests identified: 8
```
