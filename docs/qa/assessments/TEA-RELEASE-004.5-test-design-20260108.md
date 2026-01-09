# Test Design: Story TEA-RELEASE-004.5

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Story:** Python Local LLM Actions Integration

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 26 (62%)
- **Integration tests:** 12 (29%)
- **E2E tests:** 4 (9%)
- **Priority distribution:** P0: 14, P1: 16, P2: 10, P3: 2

## Test Pyramid Rationale

This story implements local LLM actions which are primarily **business logic and API abstraction** layers. The test strategy emphasizes:

1. **Heavy unit testing** - Pure logic in LlmBackend, model path resolution, and parameter handling
2. **Focused integration tests** - llama-cpp-python interaction, yaml_engine action dispatch
3. **Minimal E2E tests** - Critical user journeys only (YAML workflow execution)

---

## Test Scenarios by Acceptance Criteria

### AC-1: `llm.call` action generates text using local llama-cpp-python backend

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-001 | Unit | P0 | `LocalLlmBackend.call()` returns LlmCallResult with content | Core functionality - pure method testing |
| 004.5-UNIT-002 | Unit | P1 | `call()` passes prompt, max_tokens, temperature correctly | Parameter validation |
| 004.5-UNIT-003 | Unit | P1 | `call()` handles stop sequences correctly | Stop token boundary |
| 004.5-UNIT-004 | Unit | P1 | `call()` returns token usage when available | Observability feature |
| 004.5-INT-001 | Integration | P0 | `llm.call` action executes via yaml_engine | Action dispatch integration |

### AC-2: `llm.chat` action generates text using OpenAI-compatible chat format

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-005 | Unit | P0 | `LocalLlmBackend.chat()` accepts OpenAI message format | Core chat interface |
| 004.5-UNIT-006 | Unit | P0 | `chat()` handles system, user, assistant roles | Role handling is critical |
| 004.5-UNIT-007 | Unit | P1 | `chat()` extracts content from response correctly | Response parsing |
| 004.5-UNIT-008 | Unit | P1 | `chat()` passes optional kwargs (max_tokens, temperature, stop) | Optional parameter passthrough |
| 004.5-INT-002 | Integration | P0 | `llm.chat` action with message list in YAML | Primary use case for Phi-4-mini |

### AC-3: `llm.embed` action generates embeddings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-009 | Unit | P0 | `LocalLlmBackend.embed()` returns list of floats | Core embedding functionality |
| 004.5-UNIT-010 | Unit | P1 | `embed()` handles empty text input | Edge case |
| 004.5-UNIT-011 | Unit | P2 | `embed()` returns consistent dimension | Embedding consistency |
| 004.5-INT-003 | Integration | P1 | `llm.embed` action stores embeddings in state | YAML workflow integration |

### AC-4: `llm.stream` action supports streaming generation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-012 | Unit | P0 | `stream()` invokes callback for each chunk | Callback mechanism |
| 004.5-UNIT-013 | Unit | P1 | `stream()` accumulates full content in result | Final result validation |
| 004.5-UNIT-014 | Unit | P1 | `stream_chat()` streams chat completion with callbacks | Chat streaming variant |
| 004.5-UNIT-015 | Unit | P2 | `stream()` handles early termination (stop tokens) | Boundary condition |
| 004.5-INT-004 | Integration | P1 | `llm.stream` action outputs to state correctly | Action integration |

### AC-5: Model path configurable via YAML settings or environment variable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-016 | Unit | P0 | `resolve_model_path()` returns TEA_MODEL_PATH when set | Priority 1 path |
| 004.5-UNIT-017 | Unit | P0 | `resolve_model_path()` returns YAML settings path when set | Priority 2 path |
| 004.5-UNIT-018 | Unit | P1 | `resolve_model_path()` warns when TEA_MODEL_PATH file not found | Error handling |

### AC-6: Graceful fallback to API-based LLM if local model not found

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-019 | Unit | P0 | `create_llm_backend()` falls back to API when model not found | Critical fallback |
| 004.5-UNIT-020 | Unit | P0 | `create_llm_backend()` falls back to API on ImportError | No llama-cpp-python |
| 004.5-UNIT-021 | Unit | P1 | `create_llm_backend()` logs fallback reason | Observability |
| 004.5-INT-005 | Integration | P0 | Fallback to API backend executes successfully | End-to-end fallback |

### AC-7: Auto-detect model configuration from filename

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-022 | Unit | P0 | `get_model_info()` returns 128K ctx for Phi models | Auto-detection logic |
| 004.5-UNIT-023 | Unit | P0 | `get_model_info()` returns 32K ctx for Gemma models | Auto-detection logic |
| 004.5-UNIT-024 | Unit | P1 | `get_model_info()` returns chatml format for Phi | Chat format detection |
| 004.5-UNIT-025 | Unit | P1 | `get_model_info()` returns gemma format for Gemma | Chat format detection |
| 004.5-UNIT-026 | Unit | P2 | `get_model_info()` returns safe defaults for unknown models | Unknown model handling |

### AC-8: Optional dependency `[llm-local]` enables local LLM support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-INT-006 | Integration | P0 | Install with `[llm-local]` includes llama-cpp-python | Dependency verification |
| 004.5-INT-007 | Integration | P0 | Install without extras excludes llama-cpp-python | Slim install verification |

### AC-9: YAML settings support `llm.backend: local` configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-INT-008 | Integration | P1 | YamlEngine parses `llm.backend: local` setting | Settings parsing |
| 004.5-INT-009 | Integration | P1 | YamlEngine parses `llm.backend: api` setting | API mode selection |

### AC-10: `llm.model_path` setting specifies GGUF file location

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-027 | Unit | P1 | YAML settings `llm.model_path` passed to resolve_model_path | Settings propagation |

### AC-11: Default model search paths: `$APPDIR`, `~/.cache/tea/models/`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-028 | Unit | P1 | `resolve_model_path()` searches APPDIR when set | AppImage support |
| 004.5-UNIT-029 | Unit | P1 | `resolve_model_path()` searches ~/.cache/tea/models/ | Default cache location |
| 004.5-UNIT-030 | Unit | P2 | `resolve_model_path()` searches for DEFAULT_MODELS in order | Model preference order |

### AC-12: Support both Phi-4-mini (128K ctx) and Gemma (32K ctx) models

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-INT-010 | Integration | P1 | Load Phi-4-mini model with auto-detected 128K context | Model-specific config |
| 004.5-INT-011 | Integration | P1 | Load Gemma model with auto-detected 32K context | Model-specific config |

### AC-13: Unit tests for LlmBackend class

*(Covered by test scenarios above)*

### AC-14: Integration test with small test model (TinyLlama ~500MB)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-INT-012 | Integration | P0 | Execute `llm.call` with TinyLlama test model | Real model integration |

### AC-15: Build without `llm-local` extras excludes llama-cpp-python dependency

*(Covered by 004.5-INT-007)*

### AC-16: No regressions in existing Python tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-E2E-001 | E2E | P0 | Full pytest suite passes after implementation | Regression prevention |

### End-to-End Test Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-E2E-002 | E2E | P0 | YAML workflow with `llm.call` action completes | Critical user journey |
| 004.5-E2E-003 | E2E | P1 | YAML workflow with `llm.chat` multi-turn conversation | Multi-turn chat flow |
| 004.5-E2E-004 | E2E | P2 | YAML workflow with all LLM actions (call, chat, embed, stream) | Comprehensive action test |

---

## Risk Coverage Matrix

| Risk | Impact | Tests Mitigating |
|------|--------|------------------|
| llama-cpp-python API changes | High | 004.5-UNIT-001 to 004.5-UNIT-015 (mocked tests isolate API) |
| Model file not found | Medium | 004.5-UNIT-016 to 004.5-UNIT-021 (path resolution + fallback) |
| Incorrect model config | Medium | 004.5-UNIT-022 to 004.5-UNIT-026 (auto-detection tests) |
| Optional dependency missing | High | 004.5-INT-006, 004.5-INT-007, 004.5-UNIT-020 |
| YAML settings malformed | Medium | 004.5-INT-008, 004.5-INT-009 |
| Breaking existing tests | High | 004.5-E2E-001 (regression suite) |
| Memory issues with large models | Low | 004.5-INT-012 (use small TinyLlama) |

---

## Test Implementation Files

| File | Purpose | Coverage |
|------|---------|----------|
| `python/tests/test_llm_backend.py` | LlmBackend ABC, dataclasses | AC-1,2,3,4 |
| `python/tests/test_llm_local.py` | LocalLlmBackend (mocked) | AC-1,2,3,4,7,12 |
| `python/tests/test_llm_path_resolution.py` | resolve_model_path, get_model_info | AC-5,7,10,11 |
| `python/tests/test_llm_backend_factory.py` | create_llm_backend, fallback | AC-6,9 |
| `python/tests/test_llm_local_integration.py` | Integration with TinyLlama | AC-14 |
| `python/tests/test_yaml_engine_llm.py` | YAML action dispatch | AC-1,2,3,4,9 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - LlmBackend ABC tests
   - Path resolution tests
   - Auto-detection tests
   - Fallback logic tests

2. **P0 Integration tests** (validate component boundaries)
   - Optional dependency installation
   - TinyLlama integration
   - YAML action dispatch

3. **P0 E2E tests** (confirm critical paths)
   - Full pytest regression suite
   - Basic YAML workflow

4. **P1 tests** (core functionality)
   - Parameter handling
   - Chat completion
   - Streaming
   - Model-specific configs

5. **P2+ tests** (as time permits)
   - Edge cases
   - Unknown model handling
   - Comprehensive workflow

---

## Mocking Strategy

### Unit Tests (Mocked)

```python
@patch("the_edge_agent.actions.llm_local.Llama")
def test_local_llm_call(mock_llama):
    mock_instance = Mock()
    mock_instance.return_value = {
        "choices": [{"text": "Hello, world!"}],
        "usage": {"total_tokens": 10},
    }
    mock_llama.return_value = mock_instance
    # ...
```

### Integration Tests (Real llama-cpp-python, small model)

```python
@pytest.mark.skipif(not LLAMA_CPP_AVAILABLE, reason="llama-cpp-python not installed")
@pytest.mark.skipif(not TEST_MODEL_PATH.exists(), reason="TinyLlama model not available")
def test_local_llm_integration():
    backend = LocalLlmBackend(str(TEST_MODEL_PATH))
    result = backend.call(LlmCallParams(prompt="Hello", max_tokens=10))
    assert result.content
```

---

## Test Data Requirements

| Data | Location | Size | Purpose |
|------|----------|------|---------|
| TinyLlama GGUF | `tests/fixtures/models/` | ~500MB | Integration testing |
| Sample YAML workflows | `tests/fixtures/yaml/` | <1KB | Action dispatch testing |

**Note:** TinyLlama model should be downloaded separately and added to `.gitignore`.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Mocking strategy defined
- [x] Risk coverage mapped

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 26
    integration: 12
    e2e: 4
  by_priority:
    p0: 14
    p1: 16
    p2: 10
    p3: 2
  coverage_gaps: []
  files:
    - python/tests/test_llm_backend.py
    - python/tests/test_llm_local.py
    - python/tests/test_llm_path_resolution.py
    - python/tests/test_llm_backend_factory.py
    - python/tests/test_llm_local_integration.py
    - python/tests/test_yaml_engine_llm.py
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.5-test-design-20260108.md
P0 tests identified: 14
Total scenarios: 42
```
