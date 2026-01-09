# Test Design: Story TEA-RELEASE-004.3d - WASM Bundled LLM (Batteries Included)

Date: 2026-01-09
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 14 (33%)
- **Integration tests:** 16 (38%)
- **E2E tests:** 12 (29%)
- **Priority distribution:** P0: 18, P1: 16, P2: 8

## Story Context

This story refactors tea-wasm-llm from a callback bridge pattern to a "batteries included" architecture that bundles wllama internally. The key architectural change is embedding wllama.wasm (~5-10MB) directly in the package, eliminating external npm dependencies.

### Key Testing Challenges

1. **WASM-in-WASM complexity**: Testing wllama loading inside the tea-wasm-llm module
2. **Browser environment**: Tests require SharedArrayBuffer detection, IndexedDB, and Service Workers
3. **Multi-threading**: COOP/COEP headers required for multi-thread tests
4. **Large model downloads**: Need mock/stub strategies for 1.9GB model tests
5. **Demo app validation**: GitHub Pages-specific testing (Service Worker for headers)

## Test Scenarios by Acceptance Criteria

### AC-1: Single WASM bundle includes wllama engine internally

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-001 | Unit | P0 | Bundle contains wllama-single.wasm asset | Verify static asset presence in build |
| 004.3d-UNIT-002 | Unit | P0 | Bundle contains wllama-multi.wasm asset | Verify multi-thread variant bundled |
| 004.3d-INT-001 | Integration | P0 | wllama.wasm loads from bundled assets | Validates internal loading mechanism |
| 004.3d-INT-002 | Integration | P1 | wllama loads without external URL fetch | Confirm no CDN/npm dependency |

### AC-2: `initLlm()` initializes LLM without external callback registration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-003 | Unit | P0 | `initLlm()` returns Promise that resolves | Core API function signature |
| 004.3d-UNIT-004 | Unit | P0 | `initLlm()` accepts optional config object | API flexibility test |
| 004.3d-INT-003 | Integration | P0 | `initLlm()` loads wllama engine internally | Critical initialization path |
| 004.3d-UNIT-005 | Unit | P1 | `initLlm()` called twice is idempotent | Prevent double initialization bugs |
| 004.3d-UNIT-006 | Unit | P1 | `initLlm()` throws descriptive error on failure | Error handling for initialization |

### AC-3: `chat(prompt)` generates completions using embedded wllama

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-007 | Unit | P0 | `chat()` returns object with `content` property | API contract verification |
| 004.3d-UNIT-008 | Unit | P0 | `chat()` accepts prompt string | Basic function signature |
| 004.3d-UNIT-009 | Unit | P1 | `chat()` accepts options (maxTokens, temperature) | Parameter passing |
| 004.3d-INT-004 | Integration | P0 | `chat()` produces coherent response from wllama | End-to-end completion flow |
| 004.3d-INT-005 | Integration | P1 | `chat()` respects maxTokens limit | Token limiting behavior |
| 004.3d-UNIT-010 | Unit | P1 | `chat()` throws if called before `initLlm()` | Usage error handling |

### AC-4: `embed(text)` generates embeddings using embedded wllama

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-011 | Unit | P1 | `embed()` returns object with `vector` property | API contract |
| 004.3d-UNIT-012 | Unit | P1 | `embed()` vector is Float32Array | Correct data type |
| 004.3d-INT-006 | Integration | P1 | `embed()` produces 384+ dimension vector | Valid embedding dimensions |
| 004.3d-INT-007 | Integration | P2 | Similar texts produce similar embeddings | Semantic correctness |

### AC-5: Model lazy-loads from configured URL on first use

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-008 | Integration | P0 | Model downloads from configured modelUrl | Core model loading path |
| 004.3d-INT-009 | Integration | P0 | `onProgress` callback fires during download | Progress reporting contract |
| 004.3d-INT-010 | Integration | P1 | Model load is deferred until first `chat()`/`embed()` call | Lazy loading behavior |
| 004.3d-UNIT-013 | Unit | P2 | Default modelUrl points to valid HuggingFace path | Config defaults |

### AC-6: IndexedDB cache from 004.3b continues to work

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-011 | Integration | P0 | Cached model is retrieved from IndexedDB | Cache hit path |
| 004.3d-INT-012 | Integration | P0 | Cache miss triggers download | Cache miss path |
| 004.3d-INT-013 | Integration | P1 | `onProgress` shows "cached" status on cache hit | UX feedback |
| 004.3d-E2E-001 | E2E | P1 | Second page load uses cached model (no network) | Real browser caching |

### AC-7: wllama WASM files embedded as static assets or loaded internally

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-014 | Unit | P0 | Assets directory contains wllama-*.wasm files | Build artifact verification |
| 004.3d-INT-014 | Integration | P0 | wllama WASM loads from package assets, not CDN | Network isolation test |

### AC-8: Build produces single distributable package

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-015 | Integration | P0 | `npm pack` produces single .tgz | Package integrity |
| 004.3d-E2E-002 | E2E | P1 | Package installs and works in fresh project | End-user installation flow |

### AC-9: Package size ~5-10MB (excluding model)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-016 | Integration | P0 | Package size is between 5MB and 12MB | Size budget compliance |
| 004.3d-INT-017 | Integration | P2 | Package size breakdown shows expected components | Debug/optimization aid |

### AC-10: ES module build for browser usage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-E2E-003 | E2E | P0 | Package loads via `<script type="module">` | Browser ESM compatibility |
| 004.3d-E2E-004 | E2E | P1 | Named exports work (`import { initLlm, chat }`) | ESM export validation |

### AC-11: Zero npm peer dependencies for LLM functionality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-UNIT-015 | Unit | P0 | package.json has no peerDependencies with "llama" or "wllama" | Dependency audit |
| 004.3d-E2E-005 | E2E | P0 | LLM works without @wllama/wllama installed | Zero-dependency validation |

### AC-12: Existing tests continue to pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-018 | Integration | P0 | All pre-existing tea-wasm-llm tests pass | Regression prevention |

### AC-13: E2E test with real model works in browser

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-E2E-006 | E2E | P0 | Playwright: initLlm → chat → response in Chrome | Critical browser validation |
| 004.3d-E2E-007 | E2E | P1 | Playwright: chatStream fires token callbacks | Streaming in browser |

### AC-14: Backward compatible API (old callback pattern still works)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-019 | Integration | P0 | `set_llm_handler()` accepts callback | Legacy API preserved |
| 004.3d-INT-020 | Integration | P0 | Callback pattern triggers LLM via registered handler | Legacy flow works |
| 004.3d-INT-021 | Integration | P1 | Deprecation warning logged when using callback API | Migration guidance |

### AC-15: Demo app hosted on GitHub Pages with working LLM chat

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-E2E-008 | E2E | P0 | Demo loads at fabceolin.github.io/the_edge_agent/wasm-demo/ | Deployment validation |
| 004.3d-E2E-009 | E2E | P0 | Demo chat input and response work | Core demo functionality |

### AC-16: Demo works with multi-threading via COOP/COEP service worker

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-E2E-010 | E2E | P0 | SharedArrayBuffer is available in demo (multi-thread) | COOP/COEP working |
| 004.3d-INT-022 | Integration | P1 | coi-serviceworker.js injects required headers | Service worker mechanism |

### AC-17: Demo shows model loading progress and cache status

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-E2E-011 | E2E | P1 | Progress bar updates during model download | UX validation |
| 004.3d-E2E-012 | E2E | P1 | Cache status indicator shows "cached" on reload | Cache UX feedback |

### AC-18: Demo includes YAML workflow execution example

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-INT-023 | Integration | P1 | Demo YAML tab executes workflow with LLM node | YAML integration demo |
| 004.3d-E2E-013 | E2E | P2 | YAML execution shows result in demo UI | Full demo feature |

### AC-19: Article in `docs/articles/` following preprint format

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-DOC-001 | Integration | P0 | `docs/articles/wasm-llm-browser-inference.md` exists | File presence |
| 004.3d-DOC-002 | Integration | P1 | Article has Abstract, Introduction, Architecture sections | Format compliance |

### AC-20: Article includes link to live demo

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-DOC-003 | Integration | P0 | Article contains link to `../wasm-demo/index.html` | Demo link present |
| 004.3d-DOC-004 | Integration | P1 | Demo link resolves in built docs | Link validity |

### AC-21: Article added to `docs/_toc.yml`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3d-DOC-005 | Integration | P0 | `_toc.yml` contains `articles/wasm-llm-browser-inference` entry | TOC inclusion |
| 004.3d-DOC-006 | Integration | P0 | `sphinx-build` completes without errors | Build validation |
| 004.3d-DOC-007 | Integration | P2 | Mermaid diagrams render in built HTML | Visual content |

## Risk Coverage

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| WASM-in-WASM loading issues | Medium | High | 004.3d-INT-001, 004.3d-INT-003, 004.3d-INT-014 |
| Package size too large | Low | Medium | 004.3d-INT-016, 004.3d-INT-017 |
| Multi-threading detection | Low | Medium | 004.3d-E2E-010, 004.3d-INT-022 |
| Breaking existing users | Low | High | 004.3d-INT-018, 004.3d-INT-019, 004.3d-INT-020 |
| GitHub Pages COOP/COEP | Medium | Medium | 004.3d-E2E-010, 004.3d-INT-022 |
| Model caching regression | Low | High | 004.3d-INT-011, 004.3d-INT-012, 004.3d-E2E-001 |

## Test Environment Requirements

### Unit Tests
- **Framework:** Vitest
- **Environment:** Node.js with WASM support
- **Mocking:** wllama responses for fast tests

### Integration Tests
- **Framework:** Vitest with fake-indexeddb
- **Environment:** jsdom with WASM support
- **Data:** Small test model or mock model loader

### E2E Tests
- **Framework:** Playwright
- **Browser:** Chrome with COOP/COEP flags
- **Model:** Phi-4-mini-Q3_K_S.gguf (or smaller test model)
- **Headers:** Custom server or coi-serviceworker

### Documentation Tests
- **Tool:** Sphinx with myst-parser
- **Validation:** sphinx-build exit code, link checker

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. 004.3d-UNIT-001 through 004.3d-UNIT-003 (bundle verification)
2. 004.3d-UNIT-007, 004.3d-UNIT-008 (API contracts)
3. 004.3d-UNIT-014, 004.3d-UNIT-015 (build artifacts, dependencies)

### Phase 2: P0 Integration Tests
1. 004.3d-INT-001, 004.3d-INT-003 (wllama loading)
2. 004.3d-INT-004 (chat flow)
3. 004.3d-INT-008, 004.3d-INT-011, 004.3d-INT-012 (model loading/caching)
4. 004.3d-INT-015, 004.3d-INT-016 (package integrity)
5. 004.3d-INT-018, 004.3d-INT-019, 004.3d-INT-020 (backward compatibility)
6. 004.3d-DOC-001, 004.3d-DOC-003, 004.3d-DOC-005, 004.3d-DOC-006 (docs)

### Phase 3: P0 E2E Tests
1. 004.3d-E2E-003 (ESM in browser)
2. 004.3d-E2E-005 (zero dependencies)
3. 004.3d-E2E-006 (real model in browser)
4. 004.3d-E2E-008, 004.3d-E2E-009 (demo app)
5. 004.3d-E2E-010 (multi-threading)

### Phase 4: P1 Tests
All P1 tests in order by AC

### Phase 5: P2+ Tests
As time permits

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RELEASE-004.3d
  scenarios_total: 42
  by_level:
    unit: 15
    integration: 23
    e2e: 13
    doc: 7
  by_priority:
    p0: 18
    p1: 16
    p2: 8
  coverage_gaps: []
  risks_covered:
    - WASM-in-WASM loading
    - Package size budget
    - Multi-threading detection
    - Backward compatibility
    - GitHub Pages COOP/COEP
    - Model caching regression
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.3d-test-design-20260109.md
P0 tests identified: 18
Story ID: TEA-RELEASE-004.3d
Epic: TEA-RELEASE-004 (LLM Bundled Distributions)
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (`004.3d-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Risk coverage mapped to test scenarios
- [x] Execution order optimizes for fail-fast

## Notes for Implementation

1. **Mock Strategy for Large Models**: Create a small test GGUF (~50MB) for unit/integration tests; use real Phi-4-mini only for final E2E validation
2. **CI Considerations**: E2E tests with real model should run only on release branches or manual trigger due to model size
3. **Browser Matrix**: Primary target is Chrome; Safari limitations (no multi-thread) should be documented, not blocked
4. **Demo Test Frequency**: Demo E2E tests should run on docs deploy, not every PR
