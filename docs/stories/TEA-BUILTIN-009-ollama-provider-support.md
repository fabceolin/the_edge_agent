# Story TEA-BUILTIN-009: Ollama Provider Support - Brownfield Addition

## Status

Done

## Story

**As a** developer using The Edge Agent,
**I want** to use Ollama as an LLM provider in YAML agents,
**so that** I can run local LLMs without requiring OpenAI API keys or cloud dependencies.

## Story Context

**Existing System Integration:**

- Integrates with: `llm_actions.py` (Python), `llm.rs` (Rust)
- Technology: Python OpenAI SDK, Rust reqwest HTTP client
- Follows pattern: Provider abstraction similar to Azure OpenAI detection
- Touch points: `llm.call`, `llm.stream`, `llm.tools` actions

**Current State:**

| Runtime | llm.call | llm.stream | llm.tools | Provider Param |
|---------|----------|------------|-----------|----------------|
| Rust    | ✅ Has Ollama | ❌ N/A | ❌ N/A | ✅ Yes |
| Python  | ❌ OpenAI/Azure only | ❌ OpenAI/Azure only | ❌ OpenAI/Azure only | ❌ No |

## Acceptance Criteria

**Functional Requirements:**

1. Python `llm.call` supports Ollama via explicit `provider: "ollama"` parameter
2. Python `llm.call` supports Ollama via `OLLAMA_API_BASE` environment variable fallback
3. Python `llm.stream` supports Ollama with same provider detection logic
4. Python `llm.tools` supports Ollama for models with tool-calling capability
5. Rust `llm.call` existing Ollama support continues to work (no regression)
6. Default `api_base` for Ollama is `http://localhost:11434/v1` when not specified

**Integration Requirements:**

7. Existing OpenAI and Azure OpenAI functionality continues unchanged
8. Provider selection priority: explicit param > environment variable > default (OpenAI)
9. No API key required for Ollama provider (unlike OpenAI)
10. YAML syntax is consistent across Python and Rust runtimes

**Quality Requirements:**

11. Unit tests for Ollama provider selection logic (without live API calls)
12. Integration test with mock server or skip-if-unavailable pattern
13. Documentation updated in `docs/python/actions-reference.md` and `docs/rust/actions-reference.md`

## Technical Notes

**Provider Detection Priority (Python):**

```python
# 1. Explicit provider parameter (highest priority)
if provider == "ollama":
    use_ollama()
# 2. Environment variable detection
elif os.getenv("OLLAMA_API_BASE"):
    use_ollama()
# 3. Azure OpenAI detection (existing)
elif os.getenv("AZURE_OPENAI_API_KEY"):
    use_azure()
# 4. Default to OpenAI
else:
    use_openai()
```

**YAML Syntax Examples:**

```yaml
# Explicit provider
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1  # optional, has default
    messages:
      - role: user
        content: "{{ state.question }}"

# Environment variable fallback (OLLAMA_API_BASE set)
- name: ask_llm
  uses: llm.call
  with:
    model: llama3.2
    messages:
      - role: user
        content: "{{ state.question }}"
```

**Ollama API Compatibility:**

- Ollama exposes OpenAI-compatible API at `/v1/chat/completions`
- No authentication required (no Bearer token)
- Streaming supported via SSE
- Tool calling depends on model (llama3.1+, mistral-nemo, etc.)

**Key Constraints:**

- Ollama must be running locally or accessible at specified `api_base`
- Tool calling only works with Ollama models that support it
- No cost tracking for Ollama (local, free)

## Tasks / Subtasks

- [x] **Task 1: Add provider parameter to Python llm.call** (AC: 1, 2, 6, 7, 8, 9)
  - [x] Add `provider` parameter with default "auto"
  - [x] Implement Ollama client initialization (no API key)
  - [x] Add `OLLAMA_API_BASE` environment variable detection
  - [x] Ensure Azure/OpenAI detection unchanged
  - [x] Handle provider priority: explicit > env var > default

- [x] **Task 2: Add Ollama support to Python llm.stream** (AC: 3, 7, 9)
  - [x] Add same provider detection logic as llm.call
  - [x] Verify streaming works with Ollama's SSE format
  - [x] No cost_usd calculation for Ollama provider

- [x] **Task 3: Add Ollama support to Python llm.tools** (AC: 4, 7, 9)
  - [x] Add provider detection logic
  - [x] Document which Ollama models support tool calling
  - [x] Graceful handling if model doesn't support tools

- [x] **Task 4: Add unit tests** (AC: 11)
  - [x] Test provider selection with explicit param
  - [x] Test provider selection with env var
  - [x] Test provider priority order
  - [x] Test no API key required for Ollama

- [x] **Task 5: Add integration tests** (AC: 12)
  - [x] Add Ollama integration test with `@pytest.mark.skipif` if Ollama unavailable
  - [x] Test basic completion
  - [x] Test streaming
  - [x] Test tool calling (if model supports it)

- [x] **Task 6: Update documentation** (AC: 10, 13)
  - [x] Update `docs/python/actions-reference.md` with Ollama examples
  - [x] Update `docs/rust/actions-reference.md` to document existing Ollama support
  - [x] Add provider configuration section to both docs

- [x] **Task 7: Verify Rust parity** (AC: 5, 10)
  - [x] Verify Rust llm.call Ollama support still works
  - [x] Consider adding `OLLAMA_API_BASE` env var support to Rust for consistency

## Definition of Done

- [x] Functional requirements met (llm.call, llm.stream, llm.tools support Ollama)
- [x] Integration requirements verified (no regression, consistent syntax)
- [x] Existing functionality regression tested
- [x] Code follows existing patterns and standards
- [x] Tests pass (existing and new)
- [x] Documentation updated

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Breaking existing OpenAI/Azure detection logic
- **Mitigation:** Provider priority ensures explicit param wins; extensive test coverage for existing paths
- **Rollback:** Revert to previous llm_actions.py version

**Compatibility Verification:**

- [x] No breaking changes to existing APIs (provider param has default)
- [x] Database changes: N/A
- [x] UI changes: N/A
- [x] Performance impact: Negligible (one additional env var check)

## Dev Notes

**Relevant Source Tree:**

```
python/src/the_edge_agent/actions/
├── llm_actions.py          # Main file to modify
└── __init__.py

rust/src/actions/
└── llm.rs                  # Already has Ollama support (reference)

python/tests/
└── test_llm_actions.py     # Add tests here (create if needed)
```

**Existing Pattern Reference:**

The Azure OpenAI detection in `llm_actions.py:169-187` shows the pattern:

```python
azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
azure_endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")

if azure_api_key and azure_endpoint:
    client = AzureOpenAI(...)
else:
    client = OpenAI()
```

**Ollama-specific considerations:**

1. OpenAI SDK can connect to Ollama by setting `base_url`:
   ```python
   client = OpenAI(base_url="http://localhost:11434/v1", api_key="ollama")
   ```
   Note: Ollama ignores API key but OpenAI SDK requires one, so use dummy value "ollama"

2. Ollama model names: `llama3.2`, `mistral`, `codellama`, `llama3.1:70b`, etc.

3. Tool calling support varies by model - document in actions-reference.md

**Recommended Test Models:**

- **CPU-friendly (basic completion):** `phi4-mini` - lightweight, runs on CPU
  - https://ollama.com/library/phi4-mini
- **Tool calling:** `mistral-nemo` - supports function/tool calling
  - https://ollama.com/library/mistral-nemo

### Testing

**Test file location:** `python/tests/test_llm_actions.py`

**Test standards:**
- Use pytest
- Mock HTTP calls for unit tests (no live API dependency)
- Use `@pytest.mark.skipif` for integration tests requiring live Ollama
- Follow existing test patterns in `python/tests/`

**Testing frameworks:**
- pytest
- pytest-mock or unittest.mock
- responses library for HTTP mocking (if available)

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/llm_actions.py` | Modified | Added provider/api_base params to llm.call, llm.stream, llm.tools |
| `python/tests/test_llm_actions.py` | Created | 15 unit tests + 3 integration tests for Ollama provider |
| `docs/python/actions-reference.md` | Modified | Added LLM Provider Configuration section with Ollama examples |
| `docs/rust/actions-reference.md` | Modified | Documented existing Rust Ollama support |
| `docs/shared/YAML_REFERENCE.md` | Modified | Added LLM Provider Configuration section with Ollama examples |
| `docs/qa/gates/TEA-BUILTIN-009-ollama-provider-support.yml` | Created | QA gate file - PASS |

### Completion Notes

1. **Provider Priority Implementation**: Implemented provider detection priority as specified: explicit param > OLLAMA_API_BASE env var > AZURE env vars > default OpenAI
2. **No Cost for Ollama**: cost_usd calculation is skipped for Ollama provider (local/free)
3. **Rust Parity**: Rust already has Ollama support via explicit `provider: ollama` param. Python now has feature parity plus env var detection. Documented the difference in docs.
4. **Tool Calling**: Documented that tool calling with Ollama requires compatible models (llama3.1+, mistral-nemo, qwen2.5)
5. **Integration Tests Verified**: All 3 integration tests passed against live Ollama (phi4-mini model)

### Debug Log References
N/A - No significant debugging issues encountered

### Test Results
- **Unit tests**: 15 passed (mocked)
- **Regression tests**: 41 passed (llm_call_consolidation + opik_tracing)
- **Integration tests**: 3 passed (live Ollama with phi4-mini)
- **Rust tests**: 20 passed (llm module)

## QA Results

### Review Date: 2024-12-23

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Excellent implementation.** The Ollama provider support follows existing patterns (Azure detection) and maintains backward compatibility. The code is clean, well-documented, and follows DRY principles by reusing the same provider detection logic across all three LLM actions.

**Strengths:**
- Provider detection logic is consistent across `llm.call`, `llm.stream`, and `llm.tools`
- Backward compatible - new `provider` and `api_base` params have sensible defaults
- Cost calculation correctly skipped for Ollama (local/free provider)
- Comprehensive test coverage with proper mocking strategy
- Integration tests use `@pytest.mark.skipif` pattern for CI-friendliness

### Refactoring Performed

None required. Code quality is high and follows existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, proper docstrings
- Project Structure: ✓ Tests in correct location, docs updated
- Testing Strategy: ✓ Unit tests (mocked) + integration tests (skipif pattern)
- All ACs Met: ✓ All 13 acceptance criteria verified

### Improvements Checklist

- [x] Provider detection implemented for all three LLM actions
- [x] Environment variable fallback (OLLAMA_API_BASE) working
- [x] Cost calculation skipped for Ollama
- [x] Documentation updated in both Python and Rust docs
- [x] 18 tests total (15 unit + 3 integration)
- [x] Regression tests passing (37 total LLM tests)
- [ ] Consider extracting provider detection to shared helper function (future improvement, not blocking)
- [ ] Consider adding Rust `OLLAMA_API_BASE` env var parity (documented as future work)

### Security Review

✓ **No security concerns.**
- Ollama uses dummy API key "ollama" which is intentional (Ollama ignores auth)
- No sensitive data exposed
- No new attack vectors introduced

### Performance Considerations

✓ **Negligible impact.**
- Single additional environment variable check (`OLLAMA_API_BASE`)
- No additional network calls or blocking operations

### Files Modified During Review

- `docs/shared/YAML_REFERENCE.md` - Added LLM Provider Configuration section with Ollama examples
- `docs/qa/gates/TEA-BUILTIN-009-ollama-provider-support.yml` - Created gate file

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-BUILTIN-009-ollama-provider-support.yml`

### Recommended Status

**✓ Done** - All acceptance criteria met, tests passing, documentation complete.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2024-12-23 | 1.0 | Implementation complete - all ACs met | James (Dev Agent) |
| 2024-12-23 | 1.1 | QA Review - PASS | Quinn (Test Architect) |
| 2024-12-23 | 1.2 | Added YAML_REFERENCE.md docs, status → Done | Quinn (Test Architect) |
