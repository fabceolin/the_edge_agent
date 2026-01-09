# Browser-Based LLM Inference with TEA WASM

**The Edge Agent Team**

*Open Source Project*

https://github.com/fabceolin/the_edge_agent

---

## Abstract

This article presents TEA WASM LLM, a batteries-included WebAssembly package
for running large language models directly in the browser. We describe the
architecture that bundles the wllama engine internally, eliminating external
npm dependencies. The package provides a simple API (`initLlm`, `chat`, `embed`)
and leverages IndexedDB for model caching, enabling offline-capable LLM inference
with a single import. A live demo demonstrates practical browser-based AI
applications without server-side components.

**Keywords:** WebAssembly, LLM, Browser, wllama, Offline AI, TEA, Edge Computing

---

## 1. Introduction

The deployment of Large Language Models (LLMs) traditionally requires significant server-side infrastructure, including GPUs, API endpoints, and network connectivity. This creates barriers for privacy-sensitive applications, offline use cases, and edge computing scenarios where network latency is unacceptable.

WebAssembly (WASM) offers an alternative: compile the model runtime to WASM and execute inference directly in the user's browser. This approach provides:

- **Privacy**: Data never leaves the user's device
- **Offline capability**: No network required after initial model download
- **Zero infrastructure**: No servers to maintain
- **Instant deployment**: Ship via static hosting (e.g., GitHub Pages)

TEA WASM LLM builds on this foundation with a "batteries-included" philosophy: bundle the wllama engine (llama.cpp for WASM) internally, eliminating the need for users to install or configure external dependencies.

## 2. Architecture

### 2.1 Batteries-Included Design

The traditional approach to browser-based LLMs requires multiple moving parts:

```{mermaid}
flowchart LR
    subgraph Browser["Browser/Host Page"]
        direction LR
        APP["Your App"] -->|"callback"| BRIDGE["JS Bridge"]
        BRIDGE -->|"API calls"| WLLAMA["wllama\n(npm pkg)\nEXTERNAL"]
    end

    NPM["npm install\n@wllama/wllama"] -.->|"User must install"| WLLAMA
```

TEA WASM LLM simplifies this to a single import:

```{mermaid}
flowchart TB
    subgraph Package["tea-wasm-llm (~5-10MB)"]
        WASM["TEA WASM Core"]
        WLLAMA["wllama.wasm (embedded)"]
        LOADER["Model Loader"]
        CACHE["IndexedDB Cache"]
    end

    subgraph Browser["Browser"]
        APP["Your App"]
        IDB[(IndexedDB)]
    end

    MODEL["Gemma-3-1b.gguf (~1.3GB)"]

    APP -->|"import"| Package
    WASM --> WLLAMA
    LOADER --> CACHE
    CACHE --> IDB
    MODEL -.->|"lazy load"| LOADER
```

### 2.2 Component Overview

| Component | Purpose | Size |
|-----------|---------|------|
| TEA WASM Core | YAML execution, state management | ~500KB |
| wllama.wasm | llama.cpp WASM runtime | ~3-4MB |
| Model Loader | HTTP fetch with progress | ~10KB |
| IndexedDB Cache | Persistent model storage | ~10KB |
| **Total Package** | All dependencies bundled | **~5-8MB** |

### 2.3 Multi-Threading Support

Modern browsers support multi-threaded WASM via SharedArrayBuffer, but this requires specific HTTP headers:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

For hosting platforms like GitHub Pages that don't support custom headers, TEA WASM LLM includes a service worker (`coi-serviceworker.js`) that intercepts requests and adds these headers.

The package automatically detects threading capability:

```javascript
import { hasCoopCoep } from 'tea-wasm-llm';

if (hasCoopCoep()) {
  console.log('Multi-threaded WASM available');
} else {
  console.log('Falling back to single-threaded mode');
}
```

## 3. Quick Start

### 3.1 Installation

```html
<script type="module">
import { initLlm, chat, chatStream, embed } from './pkg/index.js';

// Initialize with model URL
await initLlm({
  modelUrl: 'https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q8_0.gguf',
  onProgress: (loaded, total) => {
    console.log(`Loading: ${Math.round(loaded / total * 100)}%`);
  },
});

// Simple chat
const response = await chat("What is the capital of France?");
console.log(response.content);
</script>
```

### 3.2 Streaming Responses

For real-time token-by-token output:

```javascript
await chatStream("Tell me a story about a brave knight", (token) => {
  document.getElementById('output').textContent += token;
}, {
  maxTokens: 200,
  temperature: 0.8,
});
```

### 3.3 Embeddings

Generate vector embeddings for semantic search:

```javascript
const embedding = await embed("Hello world");
console.log(embedding.vector);  // Float32Array
console.log(embedding.dimension);  // e.g., 384
```

## 4. API Reference

### 4.1 Core Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `initLlm(config)` | Initialize engine and load model | `Promise<void>` |
| `chat(prompt, options?)` | Generate completion | `Promise<ChatResponse>` |
| `chatStream(prompt, onToken, options?)` | Stream tokens | `Promise<ChatResponse>` |
| `embed(text)` | Generate embeddings | `Promise<EmbedResponse>` |
| `isLlmReady()` | Check if initialized | `boolean` |
| `disposeLlm()` | Free resources | `Promise<void>` |

### 4.2 Configuration Options

```typescript
interface InitLlmConfig {
  modelUrl?: string;           // URL to GGUF model file
  modelBasePath?: string;      // Base path for manifest-based loading
  onProgress?: (loaded: number, total: number) => void;
  onReady?: () => void;
  useCache?: boolean;          // Default: true
  singleThread?: boolean;      // Force single-thread mode
  nCtx?: number;               // Context size (default: 2048)
  verbose?: boolean;           // Enable logging
}
```

### 4.3 Chat Options

```typescript
interface ChatOptions {
  maxTokens?: number;    // Default: 100
  temperature?: number;  // Default: 0.7
  topP?: number;         // Default: 0.9
  topK?: number;         // Default: 0 (disabled)
  stop?: string[];       // Stop sequences
  system?: string;       // System prompt
}
```

## 5. Demo Application

A live demo is available at: [TEA WASM LLM Demo](https://fabceolin.github.io/the_edge_agent/wasm-demo/)

### 5.1 Features

- **Chat Interface**: Interactive conversation with Gemma 3 1B
- **Model Loading Progress**: Real-time download progress indicator
- **Cache Status**: Shows cached model size and status
- **YAML Workflow**: Execute TEA YAML workflows with LLM actions
- **Multi-threading Indicator**: Shows threading mode

### 5.2 Demo Architecture

```
docs/wasm-demo/
├── index.html          # Main demo page with chat UI
├── app.js              # Demo application logic
├── style.css           # Responsive styling
├── coi-serviceworker.js # COOP/COEP headers for GitHub Pages
└── README.md           # Local development instructions
```

## 6. Browser Compatibility

| Browser | Single-thread | Multi-thread | Notes |
|---------|---------------|--------------|-------|
| Chrome 90+ | Yes | Yes | Full support |
| Firefox 90+ | Yes | Yes | Full support |
| Safari 15+ | Yes | No | No SharedArrayBuffer |
| Edge 90+ | Yes | Yes | Full support |

### 6.1 Memory Requirements

| Mode | Minimum RAM | Recommended RAM |
|------|-------------|-----------------|
| Loading | 2 GB | 4 GB |
| Inference | 3 GB | 6 GB |

## 7. Comparison with Alternatives

### 7.1 vs. Traditional Callback Bridge

| Aspect | Callback Bridge | Batteries Included |
|--------|-----------------|-------------------|
| Dependencies | @wllama/wllama npm | None |
| Setup code | ~20 lines | ~3 lines |
| Failure points | Multiple | Single |
| Bundle size | Smaller (but split) | Larger (but unified) |

### 7.2 vs. Server-Based APIs

| Aspect | Server API | Browser WASM |
|--------|------------|--------------|
| Privacy | Data leaves device | Data stays local |
| Latency | Network dependent | Zero network |
| Cost | API charges | Free after download |
| Offline | No | Yes |
| Setup | API keys required | Just import |

## 8. Performance Considerations

### 8.1 Initial Load

- First visit: Download ~1.3GB model (cached after)
- Subsequent visits: Load from IndexedDB (~5-10 seconds)
- WASM compilation: ~2-3 seconds

### 8.2 Inference Speed

Approximate token generation speeds on consumer hardware:

| CPU | Single-thread | Multi-thread |
|-----|---------------|--------------|
| Intel i7 (8th gen) | ~5 tok/s | ~15 tok/s |
| Apple M1 | ~8 tok/s | ~25 tok/s |
| AMD Ryzen 5 | ~6 tok/s | ~18 tok/s |

*Speeds vary based on model size, context length, and browser.*

## 9. Conclusion

TEA WASM LLM provides a simple, batteries-included solution for browser-based
LLM inference. The bundled architecture eliminates dependency management while
IndexedDB caching enables efficient repeated use. The combination of
privacy-preserving local inference and offline capability opens new
possibilities for edge AI applications.

### 9.1 Future Work

- Smaller quantized models for faster initial load
- WebGPU acceleration when broadly available
- Persistent conversation memory
- Model fine-tuning in browser

## 10. References

- [The Edge Agent](https://github.com/fabceolin/the_edge_agent)
- [wllama - llama.cpp for WebAssembly](https://github.com/ngxson/wllama)
- [llama.cpp](https://github.com/ggerganov/llama.cpp)
- [WebAssembly](https://webassembly.org/)
- [SharedArrayBuffer and Cross-Origin Isolation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer)
