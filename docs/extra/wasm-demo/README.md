# TEA WASM LLM Demo

Interactive demo of the TEA WASM LLM batteries-included browser LLM.

## Live Demo

Visit: https://fabceolin.github.io/the_edge_agent/wasm-demo/

## Features

- **YAML Workflow Engine**: Full TEA YAML workflow execution in the browser
- **Tera Templates**: Jinja2-compatible template engine with filters
- **Conditional Routing**: Dynamic workflow branching with `goto`/`when` conditions
- **Simulated Parallel Execution**: Fan-out/fan-in patterns for concurrent workflows
- **Browser LLM**: Gemma 3 1B running locally via WebGPU/WASM
- **Model Caching**: IndexedDB-based caching for faster subsequent loads
- **Multi-threading**: Automatic detection and use of multi-threaded WASM

## Example Workflows

The demo includes 5 example workflows showcasing different capabilities:

### 1. Tera Templates & Filters (No LLM)
Demonstrates the Jinja2-compatible template engine:
- Variable interpolation: `{{ state.key }}`
- Filters: `upper`, `lower`, `tojson`, `length`, `default`
- Nested access: `{{ state.user.name }}`

### 2. Conditional Routing (No LLM)
Shows dynamic workflow branching:
- `goto` with `when` conditions for branching
- Score-based routing logic
- Multiple conditions evaluated in order
- Default fallback paths

### 3. Variables & Loops (No LLM)
Demonstrates advanced template features:
- `settings.variables` for reusable values
- Loop iteration with `{% for %}`
- Conditional blocks with `{% if %}`

### 4. Parallel Fan-Out (No LLM)
Shows simulated parallel execution:
- Fan-out: single node spawns multiple parallel paths
- Fan-in: collect results from parallel paths
- Merge strategies: `isolated`, `merge_deep`, `last_write_wins`

### 5. Neurosymbolic Q&A (Requires LLM)
Advanced workflow combining multiple engines:
- LLM for natural language understanding
- Lua for answer extraction and normalization
- Prolog for knowledge base validation
- Complete world capitals database (195 countries)

## Local Development

### Prerequisites

- Node.js 18+
- wasm-pack (for building the WASM package)

### Build the WASM Package

```bash
cd rust/tea-wasm-llm
./build.sh
```

### Serve Locally

The demo requires COOP/COEP headers for multi-threading. Use the included test server:

```bash
cd rust/tea-wasm-llm
npm run test:server
# Opens at http://localhost:8080
```

Or use a custom server with headers:

```bash
cd docs/wasm-demo
python3 -c "
from http.server import HTTPServer, SimpleHTTPRequestHandler
class Handler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        super().end_headers()
HTTPServer(('', 8080), Handler).serve_forever()
"
```

### Build Demo Package

Before running the demo, build the bundled package:

```bash
cd rust/tea-wasm-llm
node scripts/bundle-for-demo.cjs
```

This creates `docs/wasm-demo/pkg/` with all required files.

## How It Works

### Service Worker for COOP/COEP

GitHub Pages doesn't support custom HTTP headers. The `coi-serviceworker.js` intercepts requests and adds the required headers:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

This enables `SharedArrayBuffer`, which is required for multi-threaded WASM execution.

### Model Loading

On first visit:
1. The ~1.3GB Gemma 3 1B model is downloaded from Hugging Face
2. Progress is shown in the UI
3. Model is cached in IndexedDB for future visits

On subsequent visits:
1. Model loads directly from IndexedDB cache
2. No network request needed

### Architecture

```
docs/wasm-demo/
├── index.html          # Main demo page
├── app.js              # Demo application logic
├── style.css           # Styling
├── coi-serviceworker.js # COOP/COEP headers workaround
├── README.md           # This file
└── pkg/                # Built WASM package (copied from rust/tea-wasm-llm/pkg)
```

## Browser Compatibility

| Browser | Single-thread | Multi-thread |
|---------|---------------|--------------|
| Chrome 90+ | Yes | Yes |
| Firefox 90+ | Yes | Yes |
| Safari 15+ | Yes | No |
| Edge 90+ | Yes | Yes |

Safari doesn't support `SharedArrayBuffer`, so multi-threading is disabled.

## Troubleshooting

### "SharedArrayBuffer is not defined"

The COOP/COEP service worker may not be active. Try:
1. Clear browser cache
2. Hard refresh (Ctrl+Shift+R)
3. Check DevTools > Application > Service Workers

### Model download fails

Check:
1. Network connectivity to Hugging Face
2. Sufficient storage space (~1.5GB needed)
3. Browser DevTools console for errors

### Slow performance

- First load is slow due to model download
- Ensure multi-threading is active (check status bar)
- Close other browser tabs to free memory
