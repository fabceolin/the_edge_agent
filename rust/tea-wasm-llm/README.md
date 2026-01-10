# TEA WASM LLM

**Batteries-included browser LLM with zero external dependencies.**

Run local LLM inference directly in the browser with a single import. No npm peer dependencies required - wllama engine is bundled internally.

## Features

- **Single import** - No external wllama dependency to install
- **Simple API** - `initLlm()`, `chat()`, `chatStream()`, `embed()`
- Model loading with IndexedDB caching
- Progress tracking during model download
- Automatic cache invalidation on version change
- Multi-threaded inference (with SharedArrayBuffer/COOP/COEP)
- YAML workflow execution with LLM actions
- Backward compatible with legacy callback API

## Quick Start (Batteries-Included API)

```typescript
import { initLlm, chat, chatStream, embed } from 'tea-wasm-llm';

// Initialize and load model (one-time)
await initLlm({
  modelUrl: 'https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf',
  onProgress: (loaded, total) => {
    console.log(`Loading: ${Math.round(loaded / total * 100)}%`);
  },
  onReady: () => console.log('Ready!'),
});

// Simple chat
const response = await chat("What is the capital of France?", {
  maxTokens: 100,
  temperature: 0.7,
});
console.log(response.content);

// Streaming chat
await chatStream("Tell me a story", (token) => {
  process.stdout.write(token);
});

// Generate embeddings
const embedding = await embed("Hello world");
console.log(embedding.vector); // Float32Array
```

## HTML Example

```html
<!DOCTYPE html>
<html>
<head>
  <script src="coi-serviceworker.js"></script> <!-- For multi-threading -->
</head>
<body>
  <div id="output"></div>
  <script type="module">
    import { initLlm, chat } from './pkg/index.js';

    const output = document.getElementById('output');

    await initLlm({
      modelUrl: './models/Phi-4-mini-Q3_K_S.gguf',
      onProgress: (loaded, total) => {
        output.textContent = `Loading: ${Math.round(loaded / total * 100)}%`;
      },
    });

    const response = await chat("Hello!");
    output.textContent = response.content;
  </script>
</body>
</html>
```

## Installation

### From GitHub Releases

Download from [GitHub Releases](https://github.com/fabceolin/the_edge_agent/releases):

1. `tea-wasm-llm-{version}.tar.gz` - WASM package + TypeScript wrapper
2. `microsoft_Phi-4-mini-instruct-Q3_K_S.gguf` - Model file (~1.9GB)
3. `manifest.json` - Model metadata
4. `SHA256SUMS.wasm` - Checksums for verification

Extract and deploy to your web server:

```bash
# Download and extract
wget https://github.com/fabceolin/the_edge_agent/releases/download/v{version}/tea-wasm-llm-{version}.tar.gz
tar -xzf tea-wasm-llm-{version}.tar.gz

# Download model
wget https://github.com/fabceolin/the_edge_agent/releases/download/v{version}/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf
wget https://github.com/fabceolin/the_edge_agent/releases/download/v{version}/manifest.json

# Move model to models directory
mkdir -p models
mv microsoft_Phi-4-mini-instruct-Q3_K_S.gguf manifest.json models/
```

### From npm (coming soon)

```bash
npm install tea-wasm-llm
```

## Server Configuration

**CRITICAL:** Multi-threaded WASM requires specific HTTP headers for SharedArrayBuffer support.

### Required HTTP Headers

```http
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

### Nginx Configuration

```nginx
location / {
    add_header Cross-Origin-Opener-Policy "same-origin" always;
    add_header Cross-Origin-Embedder-Policy "require-corp" always;
    add_header Cross-Origin-Resource-Policy "cross-origin" always;

    # Optional: Enable compression for WASM
    gzip on;
    gzip_types application/wasm application/javascript application/json;
}
```

### Apache Configuration

```apache
<Directory "/var/www/html">
    Header set Cross-Origin-Opener-Policy "same-origin"
    Header set Cross-Origin-Embedder-Policy "require-corp"
    Header set Cross-Origin-Resource-Policy "cross-origin"
</Directory>
```

### Caddy Configuration

```caddyfile
example.com {
    header Cross-Origin-Opener-Policy "same-origin"
    header Cross-Origin-Embedder-Policy "require-corp"
    header Cross-Origin-Resource-Policy "cross-origin"
}
```

### Local Development Server

Use the included test server:

```bash
node tests/server.js
# Server runs at http://localhost:8080 with COOP/COEP headers
```

## Usage

### Legacy Callback API (Backward Compatible)

> **Note:** For new projects, use the batteries-included API shown in Quick Start above.
> The callback API is maintained for backward compatibility and advanced use cases.

#### Basic Usage (Legacy)

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>TEA WASM LLM</title>
</head>
<body>
    <script type="module">
        import { initTeaLlm, executeLlmYaml, loadBundledModel } from './pkg/index.js';
        import { Wllama } from '@anthropic-ai/wllama';

        async function main() {
            // Load model with caching
            const modelData = await loadBundledModel({
                modelBasePath: './models',
                useCache: true,
                onProgress: (loaded, total) => {
                    const percent = Math.round(loaded / total * 100);
                    console.log(`Loading: ${percent}%`);
                }
            });

            // Initialize wllama with loaded model
            const wllama = new Wllama();
            await wllama.loadModel(modelData);

            // Initialize TEA LLM with wllama handler
            await initTeaLlm({}, async (paramsJson) => {
                const params = JSON.parse(paramsJson);
                const result = await wllama.createCompletion(params.prompt, {
                    nPredict: params.max_tokens || 100,
                    temperature: params.temperature || 0.7,
                });
                return JSON.stringify({
                    content: result,
                    model: 'phi-4-mini',
                });
            });

            // Execute YAML workflow
            const result = await executeLlmYaml(`
name: chat
nodes:
  - name: respond
    action: llm.call
    with:
      prompt: "{{ state.question }}"
      max_tokens: 100
edges:
  - from: __start__
    to: respond
  - from: respond
    to: __end__
`, { question: "What is the capital of France?" });

            console.log(result.llm_response.content);
        }

        main().catch(console.error);
    </script>
</body>
</html>
```

### TypeScript Usage

```typescript
import {
    initTeaLlm,
    executeLlmYaml,
    loadBundledModel,
    isModelCached,
    clearModelCache,
    getModelCacheStats,
    type TeaLlmConfig,
    type LlmParams,
    type LlmResponse,
} from 'tea-wasm-llm';

// Check cache status
const cached = await isModelCached('v1');
console.log('Model cached:', cached);

// Get cache statistics
const stats = await getModelCacheStats();
console.log('Cache stats:', stats);

// Clear cache if needed
await clearModelCache();
```

## Supported Models

| Model | Size | Use Case | Model URL |
|-------|------|----------|-----------|
| **Gemma 3 1B Q8_0** | ~1.07 GB | Edge devices, demos, Safari | [unsloth/gemma-3-1b-it-GGUF](https://huggingface.co/unsloth/gemma-3-1b-it-GGUF) |
| Phi-4-mini Q3_K_S | ~1.9 GB | General purpose, extended context | [bartowski/Phi-4-mini-instruct-GGUF](https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF) |

### Gemma 3 1B (Recommended for Edge)

Ultra-lightweight 1B parameter model ideal for:
- Resource-constrained edge devices
- Safari IndexedDB compatibility (fits 1GB limit)
- Fast prototyping and demos
- Low-memory environments (~2GB RAM)

```typescript
await initLlm({
  modelUrl: 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q8_0.gguf',
  onProgress: (loaded, total) => console.log(`${Math.round(loaded/total*100)}%`),
});
```

### Phi-4-mini (Higher Quality)

Larger 4B parameter model for:
- Better quality responses
- Extended 128K context window
- Complex reasoning tasks

```typescript
await initLlm({
  modelUrl: 'https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf',
  onProgress: (loaded, total) => console.log(`${Math.round(loaded/total*100)}%`),
});
```

## Package Size

| Asset | Size | Description |
|-------|------|-------------|
| WASM + JS | ~50 MB | Core WASM package and TypeScript wrapper |
| Model (Gemma 3 1B Q8_0) | ~1.07 GB | Google Gemma 3 1B Instruct (ultra-lightweight) |
| Model (Phi-4-mini Q3_K_S) | ~1.9 GB | Microsoft Phi-4-mini-instruct (full-size) |

> **Note:** Both model files fit within GitHub's 2GB release asset limit without chunking.

## Memory Requirements

| Mode | Minimum RAM | Recommended RAM |
|------|-------------|-----------------|
| Loading | 2 GB | 4 GB |
| Inference | 3 GB | 6 GB |

> The model requires approximately 3GB RAM during inference. Ensure sufficient memory is available.

## API Reference

### `initTeaLlm(config, llmHandler)`

Initialize TEA LLM with an LLM handler.

```typescript
await initTeaLlm(
    { verbose: true },
    async (paramsJson: string) => {
        // Handle LLM call and return response JSON
        return JSON.stringify({ content: '...' });
    }
);
```

### `executeLlmYaml(yaml, initialState)`

Execute a YAML workflow.

```typescript
const result = await executeLlmYaml(yamlString, { input: 'hello' });
```

### `loadBundledModel(config)`

Load model with optional caching.

```typescript
const modelData = await loadBundledModel({
    modelBasePath: './models',
    useCache: true,
    onProgress: (loaded, total) => { /* ... */ },
});
```

### Cache Management

```typescript
// Check if model is cached
const cached = await isModelCached('v1');

// Get cache statistics
const stats = await getModelCacheStats();
// { totalSize: 1900000000, modelCount: 1, oldestEntry: Date }

// Clear all cached models
await clearModelCache();
```

## Scripting Bridges (Lua & Prolog)

TEA WASM LLM supports Lua and Prolog scripting through JavaScript bridges. This enables `lua.eval` and `prolog.query` actions in YAML workflows.

### Lua Integration (with wasmoon)

Use [wasmoon](https://github.com/ceifa/wasmoon) (~200KB) for Lua 5.4 scripting in the browser:

```typescript
import { LuaFactory } from 'wasmoon';
import {
  initTeaLlm,
  executeLlmYaml,
  registerLuaCallback,
  evalLua,
  isLuaCallbackRegistered,
  clearLuaCallback,
} from 'tea-wasm-llm';

// Initialize wasmoon Lua engine
const lua = await (new LuaFactory()).createEngine();

// Register Lua callback
registerLuaCallback(async (code, stateJson) => {
  const state = JSON.parse(stateJson);
  lua.global.set('state', state);
  try {
    const result = await lua.doString(code);
    return JSON.stringify({ result });
  } catch (e) {
    return JSON.stringify({ result: null, error: String(e) });
  }
});

// Check registration
console.log('Lua registered:', isLuaCallbackRegistered());

// Use in YAML workflows
const result = await executeLlmYaml(`
name: lua-validation
nodes:
  - name: validate
    action: lua.eval
    with:
      code: |
        return state.value > 0 and state.value < 100
    output: is_valid
edges:
  - from: __start__
    to: validate
  - from: validate
    to: __end__
`, { value: 42 });

console.log(result.is_valid); // true

// Or use directly
const evalResult = await evalLua('return state.x * 2', { x: 21 });
console.log(evalResult.lua_result); // 42

// Clean up
clearLuaCallback();
```

### Prolog Integration (with swipl-wasm)

Use [swipl-wasm](https://npmjs.com/package/swipl-wasm) for full SWI-Prolog with CLP(FD), constraint solving, and all standard predicates:

```typescript
import SWIPL from 'swipl-wasm';
import {
  initTeaLlm,
  executeLlmYaml,
  registerPrologHandler,
  queryProlog,
  isPrologHandlerRegistered,
  clearPrologHandler,
} from 'tea-wasm-llm';

// Initialize SWI-Prolog WASM
const swipl = await SWIPL();

// Register Prolog handler
registerPrologHandler(async (queryJson) => {
  const { code, facts } = JSON.parse(queryJson);
  try {
    // Assert facts if provided
    if (facts) {
      for (const fact of facts.split('.').filter(f => f.trim())) {
        swipl.call(`assertz(${fact.trim()})`);
      }
    }
    // Execute query
    const result = swipl.call(code);
    return JSON.stringify({
      bindings: result ? [result] : [],
      success: !!result
    });
  } catch (e) {
    return JSON.stringify({ bindings: [], success: false, error: String(e) });
  }
});

// Check registration
console.log('Prolog registered:', isPrologHandlerRegistered());

// Use in YAML workflows
const result = await executeLlmYaml(`
name: prolog-query
nodes:
  - name: find_path
    action: prolog.query
    with:
      facts: |
        edge(a, b).
        edge(b, c).
        edge(c, d).
        path(X, Y) :- edge(X, Y).
        path(X, Y) :- edge(X, Z), path(Z, Y)
      code: "path(a, X)"
    output: paths
edges:
  - from: __start__
    to: find_path
  - from: find_path
    to: __end__
`, {});

console.log(result.paths); // { bindings: [{X: 'b'}], success: true }

// Or use directly
const queryResult = await queryProlog(
  'member(X, [1,2,3])',
  undefined, // no facts needed
  {}
);
console.log(queryResult.prolog_result.bindings); // [{X: 1}]

// Clean up
clearPrologHandler();
```

### CLP(FD) Example with swipl-wasm

```typescript
// Use constraint logic programming for puzzle solving
const yaml = `
name: sudoku-solver
nodes:
  - name: solve
    action: prolog.query
    with:
      code: |
        use_module(library(clpfd)),
        X in 1..9,
        Y in 1..9,
        X + Y #= 10,
        label([X, Y])
    output: solution
edges:
  - from: __start__
    to: solve
  - from: solve
    to: __end__
`;

const result = await executeLlmYaml(yaml, {});
console.log(result.solution.bindings); // Solutions where X + Y = 10
```

### Alternative: trealla (Lightweight)

For minimal bundle size (~500KB), use [trealla](https://github.com/trealla-prolog/trealla):

```typescript
import { Prolog } from 'trealla';
import { registerPrologHandler } from 'tea-wasm-llm';

const pl = new Prolog();

registerPrologHandler(async (queryJson) => {
  const { code, facts } = JSON.parse(queryJson);
  try {
    if (facts) await pl.consultText(facts);
    const results = await pl.queryOnce(code);
    return JSON.stringify({
      bindings: results ? [results] : [],
      success: !!results
    });
  } catch (e) {
    return JSON.stringify({ bindings: [], success: false, error: String(e) });
  }
});
```

### Combined Example: LLM + Lua + Prolog

This example shows how to use all three runtimes in a single workflow:

```yaml
name: intelligent-agent
description: Uses LLM, Lua validation, and Prolog reasoning

nodes:
  - name: generate
    action: llm.call
    with:
      prompt: "Generate a number between 1 and 100: "
      max_tokens: 10
    output: generated

  - name: validate
    action: lua.eval
    with:
      code: |
        local num = tonumber(state.generated.content)
        return num and num > 0 and num <= 100
    output: is_valid

  - name: classify
    action: prolog.query
    with:
      facts: |
        small(X) :- X < 33.
        medium(X) :- X >= 33, X < 66.
        large(X) :- X >= 66.
      code: "small({{ state.generated.content }})"
    output: classification

edges:
  - from: __start__
    to: generate
  - from: generate
    to: validate
  - from: validate
    to: classify
  - from: classify
    to: __end__
```

### Runtime Comparison

| Runtime | Library | Bundle Size | Use Case |
|---------|---------|-------------|----------|
| Lua | [wasmoon](https://github.com/ceifa/wasmoon) | ~200KB | Validation, data transformation |
| **Prolog** | [**swipl-wasm**](https://npmjs.com/package/swipl-wasm) | ~5-10MB | **Full SWI-Prolog with CLP(FD) (Recommended)** |
| Prolog | [trealla](https://github.com/trealla-prolog/trealla) | ~500KB | Lightweight, basic logic |

## Troubleshooting

### "SharedArrayBuffer is not defined"

**Cause:** Your server is missing COOP/COEP headers.

**Solution:** Add the required HTTP headers (see Server Configuration above).

**Verification:** Open browser console and check:
```javascript
console.log(typeof SharedArrayBuffer); // Should be 'function'
```

### Model fails to load

1. **Verify model file is present:**
   ```bash
   ls -la models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf
   ```

2. **Check manifest.json matches model:**
   ```bash
   cat models/manifest.json
   # Verify "file" matches actual filename
   ```

3. **Clear IndexedDB cache:**
   ```javascript
   indexedDB.deleteDatabase('tea-llm-cache');
   ```

4. **Verify checksums:**
   ```bash
   sha256sum models/*.gguf
   # Compare with SHA256SUMS.wasm
   ```

### Out of memory

**Symptoms:** Browser crashes or shows "Aw, Snap!" error.

**Solutions:**
- Close other browser tabs
- Use a device with more memory (minimum 3GB free)
- Use Chrome/Chromium (better WASM memory handling)
- Check Chrome's memory usage: `chrome://memory-internals`

### WASM module fails to load

1. **Check browser console for errors**
2. **Verify MIME types are correct:**
   - `.wasm` files should be served as `application/wasm`
   - `.js` files should be served as `application/javascript`

3. **Check file paths are correct:**
   ```javascript
   // The WASM module looks for files relative to the HTML page
   import init from './pkg/tea_wasm_llm.js';
   ```

### Slow model loading

**First load:** Downloads model (~1.9GB). This can take several minutes depending on network speed.

**Subsequent loads:** Uses IndexedDB cache (near-instant).

To check cache status:
```javascript
const stats = await getModelCacheStats();
console.log('Cached:', stats.modelCount > 0);
```

## Browser Compatibility

| Browser | Status | Notes |
|---------|--------|-------|
| Chrome 89+ | Supported | Full SharedArrayBuffer support |
| Edge 89+ | Supported | Chromium-based |
| Firefox 79+ | Partial | Requires COOP/COEP headers |
| Safari 15.2+ | Partial | Limited SharedArrayBuffer support |

> **Recommendation:** Use Chrome or Chromium-based browsers for best performance.

## Development

### Building from Source

```bash
# Install dependencies
npm install

# Build WASM package
./build.sh

# Run tests
npm run test:e2e
```

### Running the Test Server

```bash
npm run test:server
# Open http://localhost:8080/tests/test.html
```

### Running E2E Tests

```bash
# Install Playwright browsers
npx playwright install chromium

# Run tests
npm run test:e2e

# Run with UI
npm run test:e2e:ui
```

## License

MIT License - see [LICENSE](../../LICENSE) for details.

## Related

- [The Edge Agent](https://github.com/fabceolin/the_edge_agent) - Main project
- [wllama](https://github.com/anthropic-ai/wllama) - llama.cpp WASM port
- [llama.cpp](https://github.com/ggerganov/llama.cpp) - LLM inference engine
