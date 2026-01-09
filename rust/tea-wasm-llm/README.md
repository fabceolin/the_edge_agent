# TEA WASM LLM

Run TEA YAML workflows with local LLM inference in the browser.

This package provides WebAssembly-based LLM execution for The Edge Agent, enabling offline AI inference directly in web browsers without requiring a server backend.

## Features

- YAML workflow execution with LLM actions
- Model loading with IndexedDB caching
- Progress tracking during model download
- Automatic cache invalidation on version change
- Multi-threaded inference (with SharedArrayBuffer)

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

### Basic Usage

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

## Package Size

| Asset | Size | Description |
|-------|------|-------------|
| WASM + JS | ~50 MB | Core WASM package and TypeScript wrapper |
| Model (Phi-4-mini Q3_K_S) | ~1.9 GB | Microsoft Phi-4-mini-instruct quantized |
| **Total** | **~2.0 GB** | Complete package |

> **Note:** The model file (~1.9GB) fits within GitHub's 2GB release asset limit without chunking.

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
