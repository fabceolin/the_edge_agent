# Story TEA-RELEASE-004.3: WASM Web Package with Bundled Gemma 3n E4B

## Status

**SUPERSEDED** - Split into smaller stories

## Superseded By

This story was split into three smaller, more manageable stories:

1. **[TEA-RELEASE-004.3a](TEA-RELEASE-004.3a-wasm-llm-core.md)** - WASM LLM Core Package
   - Create tea-wasm-llm crate
   - Port wllama LLM bridge
   - TypeScript wrapper

2. **[TEA-RELEASE-004.3b](TEA-RELEASE-004.3b-wasm-model-bundling.md)** - Model Bundling and Caching
   - Model chunking for GitHub limits
   - IndexedDB caching
   - Model loader

3. **[TEA-RELEASE-004.3c](TEA-RELEASE-004.3c-wasm-release-testing.md)** - Release and Testing
   - GitHub Actions workflow
   - Playwright browser tests
   - Documentation

**Reason for split:** Original story had 8 tasks with complex dependencies, making it too large for a single development session.

---

## Original Story (Preserved for Reference)

## Story

**As a** developer deploying TEA in the browser,
**I want** a WASM package with bundled Gemma 3n E4B model,
**So that** users can run LLM workflows without downloading models at runtime.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-WASM-001 spike infrastructure (`rust/examples/wasm-spike/`)
- Technology: Rust + wasm-pack + wasm-bindgen + wllama
- Follows pattern: wllama callback bridge pattern from WASM feasibility spike
- Touch points: `rust/examples/wasm-spike/`, new `tea-wasm-llm` npm package

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: GitHub Release includes `tea-wasm-llm-{version}.tar.gz` with bundled Gemma 3n E4B Q4_K_M model
2. **AC-2**: `llm.call` action works in browser using wllama with bundled model
3. **AC-3**: Model loads from bundled assets without external network requests
4. **AC-4**: IndexedDB caching for model persistence across browser sessions
5. **AC-5**: Multi-threading supported when COOP/COEP headers present

### Build Requirements

6. **AC-6**: Model split into chunks <2GB for GitHub Release compatibility
7. **AC-7**: wasm-pack builds `tea-wasm-llm` package with bundled assets
8. **AC-8**: Package includes TypeScript definitions
9. **AC-9**: ES module build provided for browser usage
10. **AC-10**: GitHub Actions workflow uploads to GitHub Releases on tag

### Quality Requirements

11. **AC-11**: Browser test: Load package and verify model initialization
12. **AC-12**: Functional test: Run simple LLM workflow in headless browser
13. **AC-13**: Package size documented in README
14. **AC-14**: Existing tea-wasm spike remains functional (separate package)

## Tasks / Subtasks

- [ ] Task 1: Create tea-wasm-llm crate structure (AC: 7, 8, 9)
  - [ ] Create `rust/tea-wasm-llm/` directory
  - [ ] Set up Cargo.toml with wasm-pack configuration
  - [ ] Configure wasm-bindgen exports
  - [ ] Add TypeScript type generation

- [ ] Task 2: Integrate wllama LLM bridge (AC: 2, 5)
  - [ ] Port `llm.rs` from wasm-spike to tea-wasm-llm
  - [ ] Implement `set_llm_handler()` for wllama callback
  - [ ] Support multi-threading with SharedArrayBuffer detection
  - [ ] Add `llm_call_async()` and `llm_embed_async()` exports

- [ ] Task 3: Bundle model into package (AC: 1, 3, 6)
  - [ ] Download `gemma-3n-E4B-it-Q4_K_M.gguf` (~4.5GB)
  - [ ] Split model into chunks <2GB (npm limit)
  - [ ] Create model manifest JSON with chunk URLs
  - [ ] Add model loading JavaScript wrapper

- [ ] Task 4: Implement IndexedDB caching (AC: 4)
  - [ ] Create `model-cache.ts` utility
  - [ ] Store model chunks in IndexedDB after first load
  - [ ] Check cache before loading from bundled assets
  - [ ] Add cache invalidation based on model version

- [ ] Task 5: Create JavaScript/TypeScript wrapper (AC: 8)
  - [ ] Create `index.ts` entry point
  - [ ] Export `initTeaLlm()` async initialization function
  - [ ] Export `executeLlmYaml()` workflow execution
  - [ ] Add TypeScript interfaces for params and results

- [ ] Task 6: Add GitHub Actions release workflow (AC: 10)
  - [ ] Create workflow triggered on release tag
  - [ ] Build WASM with wasm-pack
  - [ ] Bundle model chunks into package
  - [ ] Create tarball `tea-wasm-llm-{version}.tar.gz`
  - [ ] Upload to GitHub Releases using `gh release upload`

- [ ] Task 7: Add browser tests (AC: 11, 12)
  - [ ] Create test HTML harness
  - [ ] Add Playwright tests for package loading
  - [ ] Test LLM workflow execution in headless Chrome
  - [ ] Test IndexedDB caching behavior

- [ ] Task 8: Update documentation (AC: 13)
  - [ ] Create package README with usage examples
  - [ ] Document COOP/COEP header requirements
  - [ ] Add size breakdown table
  - [ ] Include CDN usage examples

## Dev Notes

### Package Structure

```
rust/tea-wasm-llm/
├── Cargo.toml
├── src/
│   ├── lib.rs              # WASM entry point
│   └── llm.rs              # wllama bridge (from spike)
├── pkg/                    # wasm-pack output
│   ├── tea_wasm_llm.js
│   ├── tea_wasm_llm_bg.wasm
│   ├── tea_wasm_llm.d.ts
│   └── package.json
├── models/                 # Bundled model chunks
│   ├── gemma-3n-E4B-it-Q4_K_M.chunk-00.bin
│   ├── gemma-3n-E4B-it-Q4_K_M.chunk-01.bin
│   ├── gemma-3n-E4B-it-Q4_K_M.chunk-02.bin
│   └── manifest.json
└── js/
    ├── index.ts            # Main entry point
    ├── model-loader.ts     # Model chunk reassembly
    └── model-cache.ts      # IndexedDB caching
```

### Model Chunking Strategy

GitHub Releases has a **2GB per file limit**. The model is ~4.5GB. Strategy:

**Split into multiple release assets:**
```
tea-wasm-llm-{version}.tar.gz         # ~50MB - WASM + JS
gemma-3n-E4B-it-Q4_K_M.chunk-00.bin   # ~2GB - model chunk 1
gemma-3n-E4B-it-Q4_K_M.chunk-01.bin   # ~2GB - model chunk 2
gemma-3n-E4B-it-Q4_K_M.chunk-02.bin   # ~500MB - model chunk 3
model-manifest.json                    # Chunk metadata
```

**User downloads all assets and extracts to same directory.**

### JavaScript Entry Point

```typescript
// js/index.ts
import init, { execute_yaml, set_llm_handler } from '../pkg/tea_wasm_llm.js';
import { loadModel, cacheModel } from './model-loader';
import { Wllama } from '@anthropic-ai/wllama'; // Note: wllama is from @anthropic-ai, but TEA package is @the-edge-agent

export interface TeaLlmConfig {
  modelUrl?: string;           // Override default bundled model
  useCache?: boolean;          // Use IndexedDB cache (default: true)
  threads?: number;            // Worker threads (default: navigator.hardwareConcurrency)
}

export async function initTeaLlm(config: TeaLlmConfig = {}): Promise<void> {
  await init();

  // Initialize wllama
  const wllama = new Wllama({
    'single-thread/wllama.wasm': bundledWllamaUrl,
    'multi-thread/wllama.wasm': bundledWllamaMultiUrl,
  });

  // Load model (from cache or bundled)
  const modelPath = config.modelUrl || await loadBundledModel(config.useCache);
  await wllama.loadModel(modelPath);

  // Register LLM handler
  set_llm_handler(async (paramsJson: string) => {
    const params = JSON.parse(paramsJson);
    const result = await wllama.createCompletion(params.prompt, {
      nPredict: params.max_tokens || 100,
      sampling: { temp: params.temperature || 0.7 },
    });
    return JSON.stringify({ content: result, model: 'gemma-3n-e4b-local' });
  });
}

export async function executeLlmYaml(yaml: string, initialState: object): Promise<object> {
  const result = await execute_yaml(yaml, JSON.stringify(initialState));
  return JSON.parse(result);
}
```

### IndexedDB Model Cache

```typescript
// js/model-cache.ts
const DB_NAME = 'tea-llm-cache';
const STORE_NAME = 'models';
const MODEL_VERSION = 'gemma-3n-e4b-q4km-v1';

export async function getCachedModel(): Promise<Uint8Array | null> {
  const db = await openDB();
  const tx = db.transaction(STORE_NAME, 'readonly');
  const store = tx.objectStore(STORE_NAME);
  const cached = await store.get(MODEL_VERSION);
  return cached?.data || null;
}

export async function cacheModel(data: Uint8Array): Promise<void> {
  const db = await openDB();
  const tx = db.transaction(STORE_NAME, 'readwrite');
  const store = tx.objectStore(STORE_NAME);
  await store.put({ version: MODEL_VERSION, data, timestamp: Date.now() });
}
```

### COOP/COEP Headers for Multi-threading

For multi-threaded wllama inference, server must send:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Without these headers, package falls back to single-threaded mode.

### wasm-pack Build Command

```bash
cd rust/tea-wasm-llm
wasm-pack build --target web --release
```

### GitHub Release Workflow

```yaml
- name: Build WASM LLM package
  run: |
    cd rust/tea-wasm-llm
    wasm-pack build --target web --release

- name: Package WASM
  run: |
    cd rust/tea-wasm-llm
    tar -czvf tea-wasm-llm-${{ github.ref_name }}.tar.gz pkg/ js/

- name: Split model into chunks
  run: |
    split -b 2000000000 models/gemma-3n-E4B-it-Q4_K_M.gguf \
      gemma-3n-E4B-it-Q4_K_M.chunk-

- name: Upload to GitHub Release
  run: |
    gh release upload ${{ github.ref_name }} \
      tea-wasm-llm-${{ github.ref_name }}.tar.gz \
      gemma-3n-E4B-it-Q4_K_M.chunk-* \
      model-manifest.json
  env:
    GH_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Browser Usage Example

```html
<!DOCTYPE html>
<html>
<head>
  <title>TEA WASM LLM Demo</title>
</head>
<body>
<script type="module">
// Download from GitHub Releases and serve locally
import { initTeaLlm, executeLlmYaml } from './tea-wasm-llm/index.js';

async function run() {
  // Initialize (loads model, may take time on first load)
  console.log('Loading model...');
  await initTeaLlm({ useCache: true });
  console.log('Model loaded!');

  // Execute LLM workflow
  const yaml = `
name: simple-chat
nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.question }}"
      max_tokens: 100
edges:
  - from: __start__
    to: generate
  - from: generate
    to: __end__
`;

  const result = await executeLlmYaml(yaml, { question: "What is 2+2?" });
  console.log('Result:', result);
}

run();
</script>
</body>
</html>
```

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Build test | In workflow | wasm-pack build succeeds |
| Load test | Playwright | Package loads in browser |
| LLM test | Playwright | Simple generation works |
| Cache test | Playwright | IndexedDB caching works |

## Definition of Done

- [ ] `tea-wasm-llm` package builds with wasm-pack
- [ ] Model bundled or accessible from package
- [ ] `initTeaLlm()` initializes wllama with model
- [ ] `executeLlmYaml()` runs LLM workflows
- [ ] IndexedDB caching implemented
- [ ] Browser tests pass in Playwright
- [ ] Package uploaded to GitHub Releases
- [ ] Documentation includes COOP/COEP requirements

## Risk and Compatibility Check

**Primary Risk:** GitHub Release 2GB per file limit vs model size (4.5GB)

**Mitigation:** Split model into <2GB chunks; provide model-manifest.json for reassembly

**Rollback:** Keep existing tea-wasm spike; new package is additive

## Compatibility Verification

- [x] No breaking changes to existing APIs (new package)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: Large initial model download, cached after

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
