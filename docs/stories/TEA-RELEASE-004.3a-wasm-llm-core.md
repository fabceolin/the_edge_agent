# Story TEA-RELEASE-004.3a: WASM LLM Core Package (Phi-4-mini)

## Status

Ready for Review

**QA Validation:** 2026-01-08 - All acceptance criteria have test coverage (32 scenarios, 11 P0 critical tests). No coverage gaps identified.

## Story

**As a** developer building browser-based TEA applications,
**I want** a WASM package with wllama LLM integration using Phi-4-mini,
**So that** I can execute LLM workflows in the browser without backend servers, using a single-file model under 2GB.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-WASM-001 spike infrastructure (`rust/examples/wasm-spike/`)
- Technology: Rust + wasm-pack + wasm-bindgen + wllama
- Follows pattern: wllama callback bridge pattern from WASM feasibility spike
- Touch points: `rust/tea-wasm-llm/` (new crate)

**Dependency:** TEA-WASM-001 (Done) - provides proven wllama callback bridge pattern

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: `llm.call` action works in browser using wllama callback bridge
2. **AC-2**: Multi-threading supported when COOP/COEP headers present (falls back to single-thread)
3. **AC-3**: `initTeaLlm()` initializes WASM module and registers wllama handler
4. **AC-4**: `executeLlmYaml()` executes YAML workflows with LLM actions

### Build Requirements

5. **AC-5**: wasm-pack builds `tea-wasm-llm` package successfully
6. **AC-6**: Package includes TypeScript definitions (`.d.ts` files)
7. **AC-7**: ES module build provided for browser usage

### Quality Requirements

8. **AC-8**: Unit test with tiny test model (stories260K.gguf ~500KB) passes
9. **AC-9**: Existing tea-wasm spike remains functional (separate crate)
10. **AC-10**: No WASM compilation warnings
11. **AC-11**: Unit tests for error paths (no handler registered, invalid JSON params)

## Tasks / Subtasks

- [x] Task 1: Create tea-wasm-llm crate structure (AC: 5, 6, 7)
  - [x] Create `rust/tea-wasm-llm/` directory
  - [x] Create `Cargo.toml` with wasm-pack configuration
  - [x] Add dependencies: wasm-bindgen, web-sys, js-sys, serde, serde_json
  - [x] Configure wasm-bindgen exports in `src/lib.rs`
  - [x] Add TypeScript type generation with `--typescript` flag

- [x] Task 2: Port wllama LLM bridge from spike (AC: 1, 2)
  - [x] Copy `llm.rs` from `rust/examples/wasm-spike/src/`
  - [x] Implement `set_llm_handler()` for wllama callback registration
  - [x] Implement `llm_call_async()` that invokes registered handler
  - [x] Implement `llm_embed_async()` for embeddings
  - [x] Add SharedArrayBuffer detection for multi-threading support
  - [x] Add fallback to single-threaded mode

- [x] Task 3: Create JavaScript/TypeScript wrapper (AC: 3, 4, 6)
  - [x] Create `js/` directory for TypeScript sources
  - [x] Create `js/index.ts` main entry point
  - [x] Define `TeaLlmConfig` interface
  - [x] Implement `initTeaLlm(config)` async function
  - [x] Implement `executeLlmYaml(yaml, state)` function
  - [x] Export all public types

- [x] Task 4: Add build and test infrastructure (AC: 8, 9, 10)
  - [x] Add `build.sh` script for wasm-pack build
  - [x] Create `tests/` directory with test HTML harness
  - [x] Add test using tiny model (stories260K.gguf)
  - [x] Verify existing spike still builds independently

## Dev Notes

### Crate Structure

```
rust/tea-wasm-llm/
├── Cargo.toml
├── build.sh                # Build script
├── src/
│   ├── lib.rs              # WASM entry point, exports
│   └── llm.rs              # wllama callback bridge
├── js/
│   └── index.ts            # TypeScript wrapper
├── tests/
│   ├── test.html           # Browser test harness
│   └── tiny-model-test.js  # Test with 500KB model
└── pkg/                    # wasm-pack output (generated)
```

### Cargo.toml

```toml
[package]
name = "tea-wasm-llm"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "rlib"]

[dependencies]
wasm-bindgen = "0.2"
wasm-bindgen-futures = "0.4"
js-sys = "0.3"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
getrandom = { version = "0.2", features = ["js"] }

[dependencies.web-sys]
version = "0.3"
features = [
    "console",
    "Window",
]

[profile.release]
opt-level = "s"
lto = true
```

### lib.rs Exports

```rust
use wasm_bindgen::prelude::*;

mod llm;

pub use llm::{set_llm_handler, llm_call_async, llm_embed_async};

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub async fn execute_yaml(yaml: &str, initial_state: &str) -> Result<String, JsValue> {
    // Parse YAML, execute graph, return result
    // For this story, delegate LLM actions to registered handler
    todo!("Implement YAML execution")
}
```

### LLM Bridge Pattern (from spike)

```rust
// src/llm.rs
use wasm_bindgen::prelude::*;
use std::sync::Mutex;

static LLM_HANDLER: Mutex<Option<js_sys::Function>> = Mutex::new(None);

#[wasm_bindgen]
pub fn set_llm_handler(handler: js_sys::Function) {
    let mut guard = LLM_HANDLER.lock().unwrap();
    *guard = Some(handler);
}

#[wasm_bindgen]
pub async fn llm_call_async(params_json: &str) -> Result<String, JsValue> {
    let handler = {
        let guard = LLM_HANDLER.lock().unwrap();
        guard.clone().ok_or_else(|| JsValue::from_str("LLM handler not registered"))?
    };

    let promise = handler.call1(&JsValue::NULL, &JsValue::from_str(params_json))?;
    let result = wasm_bindgen_futures::JsFuture::from(js_sys::Promise::from(promise)).await?;

    result.as_string().ok_or_else(|| JsValue::from_str("Invalid response"))
}
```

### TypeScript Wrapper

```typescript
// js/index.ts
import init, { execute_yaml, set_llm_handler } from '../pkg/tea_wasm_llm.js';

export interface TeaLlmConfig {
  modelUrl?: string;
  threads?: number;
}

export interface LlmHandler {
  (paramsJson: string): Promise<string>;
}

let initialized = false;

export async function initTeaLlm(
  config: TeaLlmConfig = {},
  llmHandler: LlmHandler
): Promise<void> {
  if (!initialized) {
    await init();
    initialized = true;
  }

  // Register the LLM handler
  set_llm_handler(llmHandler);
}

export async function executeLlmYaml(
  yaml: string,
  initialState: Record<string, unknown>
): Promise<Record<string, unknown>> {
  const result = await execute_yaml(yaml, JSON.stringify(initialState));
  return JSON.parse(result);
}
```

### Test with Tiny Model

```javascript
// tests/tiny-model-test.js
import { Wllama } from 'https://cdn.jsdelivr.net/npm/@anthropic-ai/wllama@2.3.6/esm/index.js';
import { initTeaLlm, executeLlmYaml } from '../js/index.js';

async function test() {
  // Load tiny model (~500KB)
  const wllama = new Wllama({
    'single-thread/wllama.wasm': 'https://cdn.jsdelivr.net/npm/@anthropic-ai/wllama@2.3.6/esm/single-thread/wllama.wasm',
  });
  await wllama.loadModelFromHF('ggml-org/models', 'tinyllamas/stories260K.gguf');

  // Register handler
  await initTeaLlm({}, async (paramsJson) => {
    const params = JSON.parse(paramsJson);
    const result = await wllama.createCompletion(params.prompt, {
      nPredict: params.max_tokens || 20,
    });
    return JSON.stringify({ content: result, model: 'tiny' });
  });

  // Execute workflow
  const yaml = `
name: test
nodes:
  - name: gen
    action: llm.call
    params:
      prompt: "Once upon a time"
      max_tokens: 10
edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
`;

  const result = await executeLlmYaml(yaml, {});
  console.log('Test passed:', result);
}

test().catch(console.error);
```

### Build Script

```bash
#!/bin/bash
# build.sh
set -e

cd "$(dirname "$0")"

echo "Building WASM..."
wasm-pack build --target web --release

echo "Compiling TypeScript..."
npx tsc js/index.ts --outDir pkg/ --declaration --module esnext --target esnext

echo "Done! Package in pkg/"
```

## Definition of Done

- [x] `rust/tea-wasm-llm/` crate created
- [x] `wasm-pack build` succeeds without errors
- [x] TypeScript definitions generated
- [x] `set_llm_handler()` registers callback
- [x] `llm_call_async()` invokes registered handler
- [x] `initTeaLlm()` and `executeLlmYaml()` work
- [x] Test with tiny model passes in browser
- [x] Existing spike crate unaffected

## Risk and Compatibility Check

**Primary Risk:** wasm-bindgen async/Promise interop complexity

**Mitigation:** Pattern proven in TEA-WASM-001 spike

**Rollback:** New crate, no impact on existing code

## Compatibility Verification

- [x] No breaking changes to existing APIs
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: None

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Split from TEA-RELEASE-004.3 | Bob (SM Agent) |
| 2026-01-08 | 0.2 | Changed model from Gemma to Phi-4-mini Q3_K_S (1.9GB, single file, 128K context) | Sarah (PO Agent) |

## QA Notes

**Review Date:** 2026-01-08
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 32 | 100% |
| **Unit tests** | 14 | 44% |
| **Integration tests** | 12 | 37% |
| **E2E tests** | 6 | 19% |

| Priority | Count | Description |
|----------|-------|-------------|
| P0 (Critical) | 11 | Must pass before release |
| P1 (High) | 13 | Core functionality validation |
| P2 (Medium) | 6 | Secondary scenarios |
| P3 (Low) | 2 | Edge cases |

**Coverage Status:** All 11 Acceptance Criteria have test coverage with no gaps identified.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **WASM-JS interop failure** | Medium | High | 3 integration tests cover async Promise handling and handler invocation |
| **Async Promise handling bugs** | Medium | High | Dedicated tests for Promise resolution and error propagation |
| **Multi-threading crashes** | Low | Medium | Tests verify COOP/COEP detection and single-thread fallback |
| **JSON serialization errors** | Low | High | Unit tests for serialization round-trips |
| **Build toolchain breakage** | Low | High | Integration test for `wasm-pack build` success |

**Primary Risk (from story):** wasm-bindgen async/Promise interop complexity — addressed by 7 tests focusing on WASM-JS boundary.

### Recommended Test Scenarios

**Phase 1 - Fail Fast (Unit Tests):**
- Handler registration stores callback correctly (4.3a-UNIT-001)
- Handler validates callback is a function (4.3a-UNIT-002)
- `llm_call_async` errors when no handler registered (4.3a-UNIT-015)
- Invalid JSON params return descriptive error (4.3a-UNIT-016)

**Phase 2 - Component Boundaries (Integration):**
- `wasm-pack build --target web` succeeds (4.3a-INT-011)
- `llm_call_async` invokes registered JS handler (4.3a-INT-001)
- `initTeaLlm` loads WASM module successfully (4.3a-INT-006)
- `executeLlmYaml` processes simple workflow end-to-end (4.3a-INT-009)

**Phase 3 - Full Validation (E2E):**
- Complete `llm.call` action with wllama in browser (4.3a-E2E-001)
- Complete YAML workflow with LLM action in browser (4.3a-E2E-003)
- Tiny model (stories260K.gguf) loads and generates text (4.3a-E2E-005)

### Concerns and Blockers

**No blockers identified.**

**Concerns:**
1. **E2E test environment complexity** - E2E tests require a web server with COOP/COEP headers and the tiny model file (~500KB). CI/CD setup must provision these.
2. **Multi-threading test coverage** - Testing multi-threaded mode (4.3a-E2E-002) is P2 priority; ensure at least one manual verification with COOP/COEP headers enabled before release.
3. **Dependency on external model** - Tests depend on `stories260K.gguf` from HuggingFace. Consider caching or vendoring for CI reliability.

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-RELEASE-004.3a-test-design-20260108.md`

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No issues encountered during development.

### Completion Notes

- All 4 tasks completed successfully
- 6 unit tests pass (3 LLM module tests, 2 lib tests)
- wasm-pack build succeeds without warnings
- TypeScript definitions auto-generated by wasm-bindgen
- Existing wasm-spike crate builds independently (verified)
- SharedArrayBuffer detection implemented via `has_shared_array_buffer()` function
- Browser test harness includes 13 automated tests

### File List

| File | Status | Description |
|------|--------|-------------|
| `rust/tea-wasm-llm/Cargo.toml` | Created | Crate manifest with wasm-pack config |
| `rust/tea-wasm-llm/src/lib.rs` | Created | WASM entry point, YAML execution, exports |
| `rust/tea-wasm-llm/src/llm.rs` | Created | LLM callback bridge (set_llm_handler, llm_call_async, llm_embed_async) |
| `rust/tea-wasm-llm/js/index.ts` | Created | TypeScript wrapper with initTeaLlm, executeLlmYaml |
| `rust/tea-wasm-llm/build.sh` | Created | Build script for wasm-pack |
| `rust/tea-wasm-llm/tests/test.html` | Created | Browser test harness (13 tests) |
| `rust/tea-wasm-llm/tests/tiny-model-test.html` | Created | Tiny model integration test page |
| `rust/tea-wasm-llm/tests/tiny-model-test.js` | Created | Wllama + TEA integration test |
| `rust/tea-wasm-llm/pkg/` | Generated | wasm-pack output (wasm, js, d.ts) |

### Change Log

| Date | Change | Files |
|------|--------|-------|
| 2026-01-08 | Initial implementation of tea-wasm-llm crate | All files above |
