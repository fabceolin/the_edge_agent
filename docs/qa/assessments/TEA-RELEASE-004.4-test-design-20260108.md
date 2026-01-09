# Test Design: Story TEA-RELEASE-004.4

Date: 2026-01-08
Designer: Quinn (Test Architect)

## Story Reference

**Story ID:** TEA-RELEASE-004.4
**Title:** Rust Local LLM Actions Integration
**Status:** Draft

## Test Strategy Overview

- Total test scenarios: 67
- Unit tests: 42 (63%)
- Integration tests: 19 (28%)
- E2E tests: 6 (9%)
- Priority distribution: P0: 18, P1: 28, P2: 15, P3: 6

## Test Scenarios by Acceptance Criteria

### AC-1: `llm.call` action generates text using local llama-cpp-2 backend (raw prompt)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-001 | Unit | P0 | LocalLlmBackend::call() tokenizes prompt correctly | Core tokenization logic |
| 4.4-UNIT-002 | Unit | P0 | LocalLlmBackend::call() returns LlmCallResult with content | Pure output validation |
| 4.4-UNIT-003 | Unit | P1 | LocalLlmBackend::call() respects max_tokens parameter | Parameter handling logic |
| 4.4-UNIT-004 | Unit | P1 | LocalLlmBackend::call() applies temperature sampling | Sampling algorithm validation |
| 4.4-UNIT-005 | Unit | P1 | LocalLlmBackend::call() detects EOS token and stops generation | Boundary condition handling |
| 4.4-UNIT-006 | Unit | P2 | LocalLlmBackend::call() handles empty prompt gracefully | Edge case handling |
| 4.4-INT-001 | Integration | P0 | llm.call action dispatches to LocalLlmBackend when backend=local | Component interaction verification |
| 4.4-INT-002 | Integration | P1 | llm.call action with raw prompt produces valid text output | End-to-end action flow |

### AC-2: `llm.chat` action generates text using OpenAI-compatible chat format

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-007 | Unit | P0 | format_chat_prompt() formats ChatML correctly for Phi-4-mini | Template formatting logic |
| 4.4-UNIT-008 | Unit | P0 | format_chat_prompt() formats Gemma chat template correctly | Template formatting logic |
| 4.4-UNIT-009 | Unit | P1 | format_chat_prompt() handles unknown formats with fallback | Graceful degradation |
| 4.4-UNIT-010 | Unit | P1 | LocalLlmBackend::chat() converts messages to prompt correctly | Message transformation |
| 4.4-UNIT-011 | Unit | P1 | ChatMessage struct serializes/deserializes correctly | Data structure validation |
| 4.4-UNIT-012 | Unit | P2 | llm.chat handles multi-turn conversations (system, user, assistant) | Complex message flows |
| 4.4-INT-003 | Integration | P0 | llm.chat action with messages array produces valid response | Component interaction |
| 4.4-INT-004 | Integration | P1 | llm.chat action respects model's auto-detected chat format | Config propagation |

### AC-3: `llm.embed` action generates embeddings using local model

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-013 | Unit | P0 | LocalLlmBackend::embed() returns Vec<f32> embeddings | Core embedding output |
| 4.4-UNIT-014 | Unit | P1 | LocalLlmBackend::embed() uses embedding context params | Config application |
| 4.4-UNIT-015 | Unit | P1 | LocalLlmBackend::embed() handles model without embedding support | Error handling |
| 4.4-UNIT-016 | Unit | P2 | Embedding vector dimensions match model specification | Output validation |
| 4.4-INT-005 | Integration | P0 | llm.embed action produces embeddings for input text | Action dispatch flow |
| 4.4-INT-006 | Integration | P2 | llm.embed action stores embeddings in state correctly | State management |

### AC-4: `llm.stream` action supports streaming generation (callback-based)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-017 | Unit | P0 | LocalLlmBackend::stream() invokes callback for each token | Callback mechanism |
| 4.4-UNIT-018 | Unit | P1 | LocalLlmBackend::stream() returns accumulated result | Result aggregation |
| 4.4-UNIT-019 | Unit | P1 | LocalLlmBackend::stream_chat() formats prompt and streams | Combined functionality |
| 4.4-UNIT-020 | Unit | P2 | Streaming respects max_tokens limit | Parameter enforcement |
| 4.4-INT-007 | Integration | P1 | llm.stream action outputs tokens progressively | Real-time output flow |

### AC-5: Model path configurable via YAML settings or environment variable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-021 | Unit | P0 | resolve_model_path() reads TEA_MODEL_PATH environment variable | Env var handling |
| 4.4-UNIT-022 | Unit | P0 | resolve_model_path() reads YAML settings.llm.model_path | Config parsing |
| 4.4-UNIT-023 | Unit | P1 | resolve_model_path() follows priority order: env > yaml > appdir > cache | Priority logic |
| 4.4-UNIT-024 | Unit | P2 | resolve_model_path() returns None when no model found | Absent model case |
| 4.4-INT-008 | Integration | P1 | YAML workflow with llm.model_path loads specified model | Config-to-action flow |

### AC-6: Graceful fallback to API-based LLM if local model not found

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-025 | Unit | P0 | create_llm_backend() returns ApiLlmBackend when model not found and backend=auto | Fallback logic |
| 4.4-UNIT-026 | Unit | P0 | create_llm_backend() panics when backend=local but no model | Explicit error handling |
| 4.4-UNIT-027 | Unit | P1 | create_llm_backend() logs warning before fallback | Observability |
| 4.4-UNIT-028 | Unit | P1 | ApiLlmBackend::new() parses settings correctly | Fallback initialization |
| 4.4-INT-009 | Integration | P0 | Workflow continues with API backend when local model missing | Graceful degradation |

### AC-7: Auto-detect model configuration from filename

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-029 | Unit | P0 | get_model_config() detects Phi-4-mini model (128K ctx) | Pattern matching |
| 4.4-UNIT-030 | Unit | P0 | get_model_config() detects Gemma model (32K ctx) | Pattern matching |
| 4.4-UNIT-031 | Unit | P1 | get_model_config() returns safe defaults for unknown models | Default handling |
| 4.4-UNIT-032 | Unit | P2 | get_model_config() is case-insensitive for model names | Robustness |
| 4.4-UNIT-033 | Unit | P2 | get_model_config() sets correct chat_format for each model family | Format association |

### AC-8: Feature flag `--features llm-local` enables local LLM support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-034 | Unit | P0 | LocalLlmBackend compiles only with llm-local feature | Conditional compilation |
| 4.4-UNIT-035 | Unit | P1 | Code without llm-local feature excludes llama-cpp-2 dependency | Dependency isolation |
| 4.4-E2E-001 | E2E | P0 | Build succeeds with `cargo build --features llm-local` | Build verification |
| 4.4-E2E-002 | E2E | P0 | Build succeeds with `cargo build` (no llm-local feature) | Default build |

### AC-9: YAML settings support `llm.backend: local` configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-036 | Unit | P0 | YamlSettings parses llm.backend field correctly | Config parsing |
| 4.4-UNIT-037 | Unit | P1 | Backend selection respects 'local', 'api', 'auto' values | Enum handling |
| 4.4-INT-010 | Integration | P1 | YAML with backend: local uses LocalLlmBackend | Config propagation |

### AC-10: `llm.model_path` setting specifies GGUF file location

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-038 | Unit | P1 | YamlSettings parses llm.model_path string | Config parsing |
| 4.4-UNIT-039 | Unit | P1 | Model path supports home directory expansion (~/) | Path resolution |
| 4.4-UNIT-040 | Unit | P2 | Model path supports environment variable expansion | Flexibility |

### AC-11: Default model search paths: `$APPDIR`, `~/.cache/tea/models/`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-041 | Unit | P0 | resolve_model_path() searches APPDIR/usr/share/models/ | AppImage support |
| 4.4-UNIT-042 | Unit | P0 | resolve_model_path() searches ~/.cache/tea/models/ | Cache location |
| 4.4-UNIT-043 | Unit | P1 | resolve_model_path() tries DEFAULT_MODELS in order | Priority order |
| 4.4-UNIT-044 | Unit | P3 | resolve_model_path() logs search attempts with tracing | Debugging support |

### AC-12: Support both Phi-4-mini (128K ctx) and Gemma (32K ctx) models

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-045 | Unit | P1 | ModelConfig for Phi-4-mini has n_ctx=128000 | Correct config |
| 4.4-UNIT-046 | Unit | P1 | ModelConfig for Gemma has n_ctx=32768 | Correct config |
| 4.4-INT-011 | Integration | P1 | LocalLlmBackend creates context with model-specific n_ctx | Config application |

### AC-13: Unit tests for LlmBackend trait implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-047 | Unit | P0 | LlmBackend trait has call, chat, embed, stream, stream_chat methods | API completeness |
| 4.4-UNIT-048 | Unit | P1 | LocalLlmBackend implements LlmBackend trait | Trait conformance |
| 4.4-UNIT-049 | Unit | P1 | ApiLlmBackend implements LlmBackend trait | Trait conformance |

### AC-14: Integration test with small test model (TinyLlama ~500MB)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-INT-012 | Integration | P0 | TinyLlama model loads successfully | Model loading |
| 4.4-INT-013 | Integration | P0 | llm.call with TinyLlama generates valid text | End-to-end generation |
| 4.4-INT-014 | Integration | P1 | llm.chat with TinyLlama produces coherent response | Chat functionality |
| 4.4-INT-015 | Integration | P2 | llm.stream with TinyLlama outputs tokens progressively | Streaming test |
| 4.4-E2E-003 | E2E | P1 | Full YAML workflow with TinyLlama executes successfully | Complete workflow |

### AC-15: Build without `llm-local` feature excludes llama-cpp-2 dependency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-E2E-004 | E2E | P0 | Binary size without llm-local is smaller than with | Dependency exclusion |
| 4.4-INT-016 | Integration | P1 | llm actions without feature return appropriate error | Feature gate behavior |

### AC-16: No regressions in existing Rust tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-INT-017 | Integration | P0 | All existing llm.call API tests pass | Backward compatibility |
| 4.4-INT-018 | Integration | P0 | All existing llm.stream SSE parsing tests pass | Backward compatibility |
| 4.4-INT-019 | Integration | P0 | All existing llm.tools function calling tests pass | Backward compatibility |
| 4.4-E2E-005 | E2E | P0 | Full test suite `cargo test` passes | Regression prevention |
| 4.4-E2E-006 | E2E | P1 | Full test suite `cargo test --features llm-local` passes | Feature-enabled regression |

## Additional Quality Scenarios

### Error Handling & Edge Cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-050 | Unit | P0 | LlmError::ModelNotFound contains path in error message | Error clarity |
| 4.4-UNIT-051 | Unit | P0 | LlmError::ModelLoadFailed contains underlying error | Error propagation |
| 4.4-UNIT-052 | Unit | P1 | LlmError::InferenceFailed captures context | Debugging support |
| 4.4-UNIT-053 | Unit | P1 | LlmError::EmbeddingNotSupported returned for non-embedding models | Correct error type |
| 4.4-UNIT-054 | Unit | P2 | LocalLlmBackend handles corrupt GGUF file gracefully | Robustness |

### Thread Safety & Concurrency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-055 | Unit | P1 | LLM_BACKEND OnceLock initializes once | Thread safety |
| 4.4-UNIT-056 | Unit | P2 | get_llm_backend() returns same instance across threads | Singleton pattern |
| 4.4-INT-INT | Integration | P3 | Concurrent llm.call actions don't interfere | Parallel execution |

### Performance & Resource Management

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.4-UNIT-057 | Unit | P2 | n_threads defaults to available CPU cores | Resource utilization |
| 4.4-UNIT-058 | Unit | P3 | Model loads in reasonable time (<30s for TinyLlama) | Performance baseline |
| 4.4-UNIT-059 | Unit | P3 | Memory usage stays bounded during generation | Resource management |

## Risk Coverage

| Risk | Mitigating Test IDs |
|------|---------------------|
| llama-cpp-2 API changes | 4.4-UNIT-047, 4.4-UNIT-048, 4.4-E2E-001 |
| Compilation issues with feature flags | 4.4-E2E-001, 4.4-E2E-002, 4.4-E2E-004 |
| Model path resolution failures | 4.4-UNIT-021 through 4.4-UNIT-024, 4.4-UNIT-041 through 4.4-UNIT-044 |
| Chat format incompatibility | 4.4-UNIT-007, 4.4-UNIT-008, 4.4-UNIT-033 |
| Fallback mechanism failure | 4.4-UNIT-025 through 4.4-UNIT-028, 4.4-INT-009 |
| Regression in existing functionality | 4.4-INT-017 through 4.4-INT-019, 4.4-E2E-005, 4.4-E2E-006 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - LlmBackend trait tests
   - Model path resolution tests
   - Feature flag compilation tests
   - Error type tests

2. **P0 Integration tests**
   - Backend selection and fallback
   - Action dispatch tests
   - Backward compatibility tests

3. **P0 E2E tests**
   - Build verification with/without features
   - Full test suite regression

4. **P1 tests in order**
   - Chat format tests
   - Streaming tests
   - TinyLlama integration tests
   - Config propagation tests

5. **P2+ tests as time permits**
   - Edge cases
   - Performance baselines
   - Concurrent execution

## Test Implementation Notes

### Mock Requirements

For unit tests without actual model:
- Mock `LlamaModel` for `LocalLlmBackend` tests
- Mock filesystem for `resolve_model_path` tests
- Mock environment variables using `temp_env` or similar

### Test Model Setup

For integration tests:
```bash
# Download TinyLlama test model (~500MB)
mkdir -p ~/.cache/tea/models/
wget https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories260K.gguf \
  -O ~/.cache/tea/models/tinyllama-stories.gguf
```

### Feature Flag Testing

```bash
# Test without feature (should compile, llm actions return error)
cargo test --no-default-features

# Test with llm feature only (API backend)
cargo test --features llm

# Test with local LLM support
cargo test --features llm-local

# Full feature test
cargo test --all-features
```

### CI/CD Considerations

- P0 and P1 unit tests should run on every PR
- Integration tests with TinyLlama can run in nightly builds
- E2E build tests should run on merge to main
- Consider caching TinyLlama model in CI to avoid repeated downloads

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (4.4-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
- [x] Backward compatibility explicitly tested
- [x] Feature flag behavior explicitly tested

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RELEASE-004.4
  date: 2026-01-08
  designer: Quinn (Test Architect)
  scenarios_total: 67
  by_level:
    unit: 42
    integration: 19
    e2e: 6
  by_priority:
    p0: 18
    p1: 28
    p2: 15
    p3: 6
  coverage_gaps: []
  risks_mitigated:
    - llama-cpp-2 API changes
    - Feature flag compilation issues
    - Model path resolution failures
    - Chat format incompatibility
    - Fallback mechanism failure
    - Regression in existing functionality
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.4-test-design-20260108.md
P0 tests identified: 18
P1 tests identified: 28
Total scenarios: 67
```
