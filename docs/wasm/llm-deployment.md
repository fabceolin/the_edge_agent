# WASM LLM Deployment Guide

Deploy TEA with bundled LLM in the browser using WebAssembly.

## Overview

The WASM LLM package allows you to run TEA workflows with local LLM inference entirely in the browser. This enables:

- **Offline operation**: No internet required after initial load
- **Privacy**: Data never leaves the user's device
- **Low latency**: No network round-trips for inference

## Requirements

### HTTP Headers (Required for Multi-threading)

Your web server **MUST** send these headers for multi-threaded inference using SharedArrayBuffer:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Without these headers, the WASM module will fall back to single-threaded mode with significantly reduced performance.

### Browser Support

| Browser | Version | SharedArrayBuffer | Status |
|---------|---------|-------------------|--------|
| Chrome | 91+ | Yes (with headers) | Full support |
| Firefox | 79+ | Yes (with headers) | Full support |
| Safari | 15.2+ | Yes (with headers) | Full support |
| Edge | 91+ | Yes (with headers) | Full support |

## Server Configuration

### Nginx Configuration

```nginx
server {
    listen 443 ssl http2;
    server_name your-domain.com;

    # SSL configuration
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    root /var/www/tea-wasm;

    location / {
        # Required headers for SharedArrayBuffer
        add_header Cross-Origin-Opener-Policy same-origin;
        add_header Cross-Origin-Embedder-Policy require-corp;

        # Cache static assets
        location ~* \.(wasm|js|gguf)$ {
            expires 1y;
            add_header Cache-Control "public, immutable";
            add_header Cross-Origin-Opener-Policy same-origin;
            add_header Cross-Origin-Embedder-Policy require-corp;
        }
    }
}
```

### Apache Configuration

```apache
<IfModule mod_headers.c>
    Header set Cross-Origin-Opener-Policy "same-origin"
    Header set Cross-Origin-Embedder-Policy "require-corp"
</IfModule>

<IfModule mod_mime.c>
    AddType application/wasm .wasm
    AddType application/octet-stream .gguf
</IfModule>

<IfModule mod_expires.c>
    ExpiresActive On
    ExpiresByType application/wasm "access plus 1 year"
    ExpiresByType application/octet-stream "access plus 1 year"
</IfModule>
```

### Caddy Configuration

```caddyfile
your-domain.com {
    root * /var/www/tea-wasm
    file_server

    header {
        Cross-Origin-Opener-Policy same-origin
        Cross-Origin-Embedder-Policy require-corp
    }
}
```

### Development Server (Python)

For local development, use the included script:

```bash
#!/usr/bin/env python3
from http.server import HTTPServer, SimpleHTTPRequestHandler
import sys

class CORSHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        super().end_headers()

if __name__ == '__main__':
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
    print(f"Serving at http://localhost:{port}")
    HTTPServer(('', port), CORSHandler).serve_forever()
```

Save as `serve.py` and run:

```bash
python serve.py 8080
```

## Download and Setup

### 1. Download from GitHub Releases

```bash
# Download WASM package
curl -L https://github.com/fabceolin/the_edge_agent/releases/download/v0.9.5/tea-wasm-llm-0.9.5.tar.gz -o tea-wasm-llm.tar.gz

# Extract
tar -xzf tea-wasm-llm.tar.gz
```

### 2. Package Contents

```
tea-wasm-llm/
├── tea-wasm-llm.js        # Main JavaScript module
├── tea-wasm-llm.wasm      # WebAssembly binary
├── tea-wasm-llm.d.ts      # TypeScript definitions
├── models/
│   └── README.md          # Model download instructions
└── examples/
    ├── index.html         # Demo page
    └── chat.html          # Chat interface example
```

### 3. Download Model (Separate Download)

Models are distributed separately due to size. Download and place in `models/`:

```bash
# Phi-4-mini (smaller, ~2GB)
curl -L https://github.com/fabceolin/the_edge_agent/releases/download/v0.9.5/phi-4-mini-instruct-Q3_K_S.gguf \
  -o tea-wasm-llm/models/model.gguf

# Or Gemma (higher quality, ~4.5GB)
curl -L https://github.com/fabceolin/the_edge_agent/releases/download/v0.9.5/gemma-3n-E4B-it-Q4_K_M.gguf \
  -o tea-wasm-llm/models/model.gguf
```

## Usage

### Basic HTML Integration

```html
<!DOCTYPE html>
<html>
<head>
    <title>TEA WASM LLM Demo</title>
</head>
<body>
    <h1>TEA Local LLM</h1>
    <textarea id="input" placeholder="Enter your question..."></textarea>
    <button onclick="runChat()">Ask</button>
    <div id="output"></div>

    <script type="module">
        import { initTeaLlm, executeLlmYaml, loadModel } from './tea-wasm-llm/tea-wasm-llm.js';

        // Initialize on page load
        let initialized = false;

        window.runChat = async function() {
            const output = document.getElementById('output');
            const input = document.getElementById('input').value;

            if (!initialized) {
                output.textContent = 'Loading model... (this may take a minute)';
                await initTeaLlm();
                await loadModel('./tea-wasm-llm/models/model.gguf');
                initialized = true;
            }

            output.textContent = 'Generating...';

            const yaml = `
name: browser-chat
state_schema:
  question: str
  answer: str

nodes:
  - name: chat
    action: llm.chat
    params:
      backend: local
      prompt: "{{ state.question }}"
      system: "You are a helpful assistant."
      max_tokens: 200

edges:
  - from: __start__
    to: chat
  - from: chat
    to: __end__
`;

            const result = await executeLlmYaml(yaml, { question: input });
            output.textContent = result.answer || result.error || 'No response';
        };
    </script>
</body>
</html>
```

### TypeScript Integration

```typescript
import { initTeaLlm, executeLlmYaml, loadModel, TeaLlmConfig } from './tea-wasm-llm';

const config: TeaLlmConfig = {
    numThreads: navigator.hardwareConcurrency || 4,
    contextSize: 2048,
    batchSize: 512,
};

async function main() {
    // Initialize WASM module
    await initTeaLlm(config);

    // Load model (shows progress)
    await loadModel('./models/model.gguf', (progress) => {
        console.log(`Loading: ${(progress * 100).toFixed(1)}%`);
    });

    // Execute workflow
    const result = await executeLlmYaml(yamlContent, {
        question: "What is the capital of France?"
    });

    console.log(result.answer);
}
```

### React Integration

```tsx
import { useEffect, useState, useCallback } from 'react';
import { initTeaLlm, executeLlmYaml, loadModel } from './tea-wasm-llm';

function useTealLlm(modelPath: string) {
    const [ready, setReady] = useState(false);
    const [loading, setLoading] = useState(true);
    const [progress, setProgress] = useState(0);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        async function init() {
            try {
                await initTeaLlm();
                await loadModel(modelPath, setProgress);
                setReady(true);
            } catch (e) {
                setError(e instanceof Error ? e.message : 'Failed to load');
            } finally {
                setLoading(false);
            }
        }
        init();
    }, [modelPath]);

    const chat = useCallback(async (question: string) => {
        if (!ready) throw new Error('Model not ready');

        const yaml = `
name: chat
state_schema:
  question: str
  answer: str
nodes:
  - name: respond
    action: llm.chat
    params:
      backend: local
      prompt: "{{ state.question }}"
      max_tokens: 200
edges:
  - from: __start__
    to: respond
  - from: respond
    to: __end__
`;
        return executeLlmYaml(yaml, { question });
    }, [ready]);

    return { ready, loading, progress, error, chat };
}

export function ChatComponent() {
    const { ready, loading, progress, error, chat } = useTealLlm('./models/model.gguf');
    const [input, setInput] = useState('');
    const [response, setResponse] = useState('');

    const handleSubmit = async () => {
        setResponse('Thinking...');
        const result = await chat(input);
        setResponse(result.answer);
    };

    if (loading) return <div>Loading model: {(progress * 100).toFixed(0)}%</div>;
    if (error) return <div>Error: {error}</div>;

    return (
        <div>
            <input value={input} onChange={(e) => setInput(e.target.value)} />
            <button onClick={handleSubmit} disabled={!ready}>Ask</button>
            <p>{response}</p>
        </div>
    );
}
```

## IndexedDB Caching

The WASM module automatically caches loaded models in IndexedDB for faster subsequent loads:

| Event | Behavior |
|-------|----------|
| First load | Downloads model, stores in IndexedDB (~30s-5min depending on size) |
| Subsequent loads | Loads from IndexedDB cache (~5-15s) |
| Cache invalidation | Automatic when model URL changes |

### Manual Cache Management

```javascript
import { clearModelCache, getCacheSize, getCachedModels } from './tea-wasm-llm';

// List cached models
const models = await getCachedModels();
console.log(models); // ['model.gguf']

// Get cache size
const sizeBytes = await getCacheSize();
console.log(`Cache: ${(sizeBytes / 1e9).toFixed(2)} GB`);

// Clear cache
await clearModelCache();
```

## Performance Tuning

### Thread Configuration

```javascript
await initTeaLlm({
    numThreads: navigator.hardwareConcurrency || 4, // Use all available cores
    contextSize: 2048,  // Reduce for lower memory usage
    batchSize: 512,     // Reduce for lower memory, increase for speed
});
```

### Memory Requirements

| Model | RAM Required | Recommended |
|-------|--------------|-------------|
| Phi-4-mini Q3_K_S | ~2.5GB | 4GB+ total system RAM |
| Gemma 3n E4B | ~6GB | 8GB+ total system RAM |

### Performance Tips

1. **Use Phi-4-mini for web apps**: Smaller, faster to load, good for interactive use
2. **Reduce context size**: Lower `contextSize` reduces memory but limits conversation length
3. **Enable caching**: IndexedDB caching dramatically improves subsequent loads
4. **Consider Web Workers**: Run inference in a worker to avoid blocking UI

## Troubleshooting

### Common Issues

#### "SharedArrayBuffer is not defined"

**Cause**: Missing COOP/COEP headers.

**Solution**: Ensure your server sends both headers:
```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

#### "Out of memory"

**Cause**: Model too large for available RAM.

**Solutions**:
1. Use Phi-4-mini instead of Gemma
2. Reduce `contextSize` in config
3. Close other browser tabs
4. Use a device with more RAM

#### "Model loading hangs at 100%"

**Cause**: Model initialization taking longer than expected.

**Solutions**:
1. Wait longer (initialization can take 30-60 seconds)
2. Check browser console for errors
3. Try a smaller model

#### Slow inference

**Cause**: Single-threaded fallback mode.

**Solutions**:
1. Verify COOP/COEP headers are set
2. Check `navigator.hardwareConcurrency` is available
3. Update to a newer browser

### Debugging

Enable debug logging:

```javascript
await initTeaLlm({
    debug: true,
    onLog: (message) => console.log('[TEA]', message),
});
```

### Browser DevTools

1. **Network tab**: Verify .wasm and .gguf files load correctly
2. **Console**: Check for initialization errors
3. **Memory**: Monitor heap usage during inference
4. **Performance**: Profile to identify bottlenecks

## Security Considerations

- **Model integrity**: Verify model checksums after download
- **Content Security Policy**: Ensure CSP allows WASM execution
- **User data**: All processing happens client-side, no data sent to servers
- **Memory isolation**: Each tab runs in isolated memory space

## API Reference

### Functions

| Function | Description |
|----------|-------------|
| `initTeaLlm(config?)` | Initialize WASM module |
| `loadModel(url, onProgress?)` | Load GGUF model |
| `executeLlmYaml(yaml, input)` | Execute TEA workflow |
| `clearModelCache()` | Clear IndexedDB cache |
| `getCachedModels()` | List cached model names |
| `getCacheSize()` | Get cache size in bytes |

### Types

```typescript
interface TeaLlmConfig {
    numThreads?: number;      // Default: navigator.hardwareConcurrency
    contextSize?: number;     // Default: 2048
    batchSize?: number;       // Default: 512
    debug?: boolean;          // Default: false
    onLog?: (msg: string) => void;
}

interface TeaLlmResult {
    [key: string]: any;       // Output state from workflow
    error?: string;           // Error message if failed
}
```

## Next Steps

- [Local LLM Examples](../examples/llm/) - Example YAML workflows
- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete YAML syntax
- [Actions Reference](../python/actions-reference.md) - Available actions
