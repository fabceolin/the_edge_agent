# Rust Actions Reference

This document lists all built-in actions available in the Rust implementation of The Edge Agent.

## Available Actions

The Rust implementation currently provides 6 action modules with the following actions:

### LLM Actions (`actions/llm.rs`)

| Action | Description |
|--------|-------------|
| `llm.call` | Call OpenAI-compatible LLM API |
| `llm.stream` | Stream LLM response via SSE |
| `llm.tools` | LLM with function/tool calling support |
| `llm.chat` | Local LLM chat (requires `llm-local` feature) |
| `llm.embed` | Generate vector embeddings (requires `llm-local` feature) |
| `memory.embed` | Alias for `llm.embed` (Python parity) |

#### llm.call

**Parameters:**

```yaml
action: llm.call
with:
  model: gpt-4                    # Required: Model name
  prompt: "{{ state.input }}"     # Required: Prompt text (or use messages)
  messages:                       # Alternative: Message array
    - role: system
      content: "You are helpful"
    - role: user
      content: "{{ state.input }}"
  api_key: "{{ secrets.OPENAI_KEY }}"  # Optional: API key
  temperature: 0.7                # Optional: Sampling temperature
  max_tokens: 1000                # Optional: Maximum tokens
```

**Returns:** `content`, `response`, `model`, `finish_reason`, `usage`, `id`

#### llm.stream

Stream LLM responses via Server-Sent Events (SSE). Aggregates all chunks and returns complete content with streaming metadata.

**Parameters:**

```yaml
- name: stream_response
  uses: llm.stream
  with:
    provider: ollama              # "openai" (default) or "ollama"
    model: phi4-mini              # Model name
    prompt: "{{ state.question }}"  # Or use messages array
    temperature: 0.7              # Optional
    max_tokens: 500               # Optional
    api_base: http://localhost:11434/v1  # Optional custom endpoint
```

**Returns:**

```json
{
  "content": "Full aggregated response text",
  "response": "Full aggregated response text",
  "streamed": true,
  "chunk_count": 15,
  "model": "phi4-mini",
  "usage": { "prompt_tokens": 10, "completion_tokens": 50, "total_tokens": 60 }
}
```

#### llm.tools

LLM with tool/function calling support. Handles multi-turn tool use loops.

**Parameters:**

```yaml
- name: agent_with_tools
  uses: llm.tools
  with:
    provider: openai              # "openai" (default) or "ollama"
    model: gpt-4                  # Model name (must support tool calling)
    prompt: "What's the weather in Boston?"  # Or use messages
    tools:                        # YAML-style tool definitions
      - name: get_weather
        description: Get current weather
        parameters:
          location:
            type: string
            description: City name
            required: true
          unit:
            type: string
            enum: [celsius, fahrenheit]
        action: weather.get       # Optional: maps to registered action
    tool_choice: auto             # "auto" (default), "none", or specific tool
    max_tool_rounds: 10           # Maximum tool call rounds (default: 10)
    temperature: 0.7
```

**Returns:**

```json
{
  "content": "Final response after tool calls",
  "tool_calls": [
    { "id": "call_abc", "name": "get_weather", "arguments": {"location": "Boston"} }
  ],
  "tool_results": [
    { "tool_call_id": "call_abc", "name": "get_weather", "result": "..." }
  ],
  "rounds": 1,
  "model": "gpt-4"
}
```

**Note:** Action dispatch (`action: weather.get`) records the mapping but does not execute in Rust (returns placeholder result for manual handling). Full action dispatch is available in Python.

#### LLM Provider Configuration

The Rust LLM actions support multiple providers: **OpenAI** and **Ollama**.

**Provider Selection:**
- Default provider is `openai`
- Use `provider: ollama` for local Ollama models

**Ollama Example:**

```yaml
# Explicit Ollama provider
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1  # optional, this is the default
    prompt: "{{ state.question }}"
```

**Ollama Tool Calling:**

Tool calling with Ollama requires models that support function calling:
- `mistral-nemo` (recommended)
- `qwen2.5`
- `llama3.1+`

**Key Differences from Python:**
- Rust uses explicit `provider` parameter only (no env var auto-detection)
- No `OLLAMA_API_BASE` environment variable support yet (planned for parity)
- Tool action dispatch returns placeholder (does not execute actions)

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key (required for OpenAI) |

Note: Ollama does not require an API key.

#### Local LLM Provider (TEA-RELEASE-004)

Run LLM inference locally using bundled GGUF models via llama-cpp-2. No API keys or network required.

**Build with local LLM support:**

```bash
cd rust
cargo build --release --features llm-local
```

**Or use an LLM-bundled AppImage** (recommended):

```bash
./tea-rust-llm-gemma-0.9.5-x86_64.AppImage run workflow.yaml
```

##### llm.chat (Local Backend)

Generate text using a local llama.cpp model.

**Parameters:**

```yaml
- name: generate
  uses: llm.chat
  with:
    backend: local                    # "local", "api", or "auto"
    prompt: "{{ state.question }}"    # Text prompt
    system: "You are helpful."        # Optional system prompt
    max_tokens: 200                   # Max tokens to generate
    temperature: 0.7                  # Sampling temperature
    model_path: /path/to/model.gguf   # Optional: override model
```

**Returns:**

```json
{
  "content": "Generated response text",
  "backend": "local",
  "model": "gemma-3n-E4B"
}
```

**Settings:**

Configure in workflow `settings.llm`:

```yaml
settings:
  llm:
    backend: auto           # "local", "api", or "auto"
    model_path: auto        # Path to GGUF model
    n_ctx: 2048             # Context window size
    n_gpu_layers: 0         # GPU layers (0=CPU, -1=all GPU)
```

##### llm.embed / memory.embed (Local Backend) - TEA-RUST-045

Generate vector embeddings using a local GGUF model. Both `llm.embed` and `memory.embed` are available for cross-runtime parity with Python.

**Requires:** `--features llm-local` or an LLM-bundled AppImage.

**Parameters:**

```yaml
- name: embed_query
  uses: llm.embed  # or memory.embed (alias)
  with:
    text: "{{ state.query }}"           # Required: Text to embed
    model_path: /path/to/model.gguf     # Optional: Override model path
    n_ctx: 4096                         # Optional: Context window size
    n_threads: 8                        # Optional: CPU threads
    n_gpu_layers: 0                     # Optional: GPU layers (0=CPU)
  outputs:
    query_embedding: embedding
    embedding_dim: dimensions
```

**Returns:**

```json
{
  "embedding": [0.123, -0.456, 0.789, ...],
  "model": "gemma-3-1b-it",
  "dimensions": 768,
  "tokens_used": 42
}
```

**Example - RAG Query Embedding:**

```yaml
nodes:
  - name: embed_user_query
    uses: memory.embed
    with:
      text: "{{ state.user_question }}"
    outputs:
      query_vector: embedding
      vector_dim: dimensions

  - name: embed_document
    uses: llm.embed
    with:
      text: "{{ state.document_chunk }}"
    outputs:
      doc_vector: embedding
```

**Notes:**
- Both `llm.embed` and `memory.embed` call the same underlying implementation
- Embedding dimensions depend on the model (e.g., Gemma 3 1B: 768, Phi-4-mini: 3072)
- Model must be loaded with embedding support (most GGUF models support this)
- For best results, use the same model for query and document embeddings

##### llm.stream (Local Backend)

Stream LLM responses with aggregated output.

**Parameters:**

```yaml
- name: stream_response
  uses: llm.stream
  with:
    backend: local
    prompt: "{{ state.question }}"
    max_tokens: 500
```

**Returns:**

```json
{
  "content": "Full aggregated response",
  "backend": "local",
  "streamed": true,
  "chunk_count": 15
}
```

##### Model Path Resolution

Models are discovered in this order:

1. `TEA_MODEL_PATH` environment variable
2. `params.model_path` in action
3. `settings.llm.model_path` in YAML
4. `$APPDIR/usr/share/models/` (AppImage bundle)
5. `~/.cache/tea/models/` (default cache)

##### Backend Selection

| Backend | Behavior |
|---------|----------|
| `auto` | Prefer local if model available, fallback to API |
| `local` | Force local model, error if not found |
| `api` | Force API call (requires OPENAI_API_KEY) |

### HTTP Actions (`actions/http.rs`)

| Action | Description |
|--------|-------------|
| `http.get` | HTTP GET request |
| `http.post` | HTTP POST request |

**Parameters:**

```yaml
action: http.get
with:
  url: "https://api.example.com/data"
  headers:
    Authorization: "Bearer {{ secrets.API_KEY }}"
  output_key: api_response

action: http.post
with:
  url: "https://api.example.com/submit"
  body: "{{ state.payload | json }}"
  headers:
    Content-Type: application/json
  output_key: post_result
```

### File Actions (`actions/file.rs`)

| Action | Description |
|--------|-------------|
| `file.read` | Read file contents |
| `file.write` | Write file contents |

**Parameters:**

```yaml
action: file.read
with:
  path: "./data/input.txt"
  output_key: file_content

action: file.write
with:
  path: "./output/result.txt"
  content: "{{ state.result }}"
```

### Data Actions (`actions/data.rs`)

| Action | Description |
|--------|-------------|
| `json.parse` | Parse JSON string to object |
| `json.transform` | Transform JSON using JMESPath |
| `data.validate` | Validate against JSON Schema |

**Parameters:**

```yaml
action: json.parse
with:
  input: "{{ state.json_string }}"
  output_key: parsed_data

action: json.transform
with:
  input: "{{ state.data }}"
  query: "items[?status=='active'].name"
  output_key: active_names

action: data.validate
with:
  data: "{{ state.user_input }}"
  schema:
    type: object
    required: [name, email]
    properties:
      name: { type: string }
      email: { type: string, format: email }
  output_key: validation_result
```

### Rate Limit Actions (`actions/ratelimit.rs`)

| Action | Description |
|--------|-------------|
| `ratelimit.wrap` | Wrap action with rate limiting |
| `actions.ratelimit_wrap` | Alias for ratelimit.wrap |

#### ratelimit.wrap

Wraps another action with rate limiting to prevent API throttling when making concurrent calls to rate-limited services.

**Parameters:**

```yaml
- name: call_api
  uses: ratelimit.wrap
  with:
    action: http.get                # Required: Action to wrap
    limiter: api_provider           # Required: Named limiter
    rpm: 60                         # Optional: Requests per minute (default: 60)
    rps: 1                          # Optional: Requests per second (takes precedence over rpm)
    timeout: 5000                   # Optional: Max wait time in ms (fails fast if exceeded)
    args:                           # Optional: Arguments for the wrapped action
      url: "https://api.example.com/data"
```

**Returns:** Original action result with additional metadata:

```json
{
  "_ratelimit_waited_ms": 150.5,    // Time spent waiting for rate limit
  "_ratelimit_limiter": "api_provider"  // Name of the limiter used
}
```

**Pre-configuration via Settings:**

Rate limiters can be pre-configured in the workflow settings:

```yaml
name: api-workflow
settings:
  rate_limiters:
    openai:
      rpm: 60
    anthropic:
      rps: 2

nodes:
  - name: call_openai
    uses: ratelimit.wrap
    with:
      action: llm.call
      limiter: openai              # Uses pre-configured 60 rpm
      args:
        model: gpt-4
        prompt: "{{ state.input }}"
```

**Error Handling:**

If timeout is specified and the wait would exceed it, returns a `RateLimitTimeout` error:

```
Rate limit timeout for limiter 'api_provider': wait would exceed 5000ms (estimated 10000ms)
```

### Memory Actions (`actions/memory.rs`)

| Action | Description |
|--------|-------------|
| `memory.store` | Store value in session memory |
| `memory.retrieve` | Retrieve value from session memory |
| `memory.delete` | Delete value from session memory |
| `memory.list` | List all memory keys |
| `memory.embed` | Generate vector embeddings (alias for `llm.embed`, requires `llm-local`) |

**Parameters:**

```yaml
action: memory.store
with:
  key: "user_preference"
  value: "{{ state.preference }}"

action: memory.retrieve
with:
  key: "user_preference"
  output_key: stored_preference

action: memory.delete
with:
  key: "user_preference"

action: memory.list
with:
  output_key: all_keys
```

## Comparison with Python

| Feature | Python | Rust |
|---------|--------|------|
| Action modules | 20+ | 7 |
| LLM actions | Full (call, stream, tools, chat, embed) | Full (call, stream, tools, chat, embed) |
| Local LLM (llama.cpp) | Yes (llama-cpp-python) | Yes (llama-cpp-2) |
| HTTP actions | Full | Full |
| File actions | Local + remote (S3, GCS, Azure) | Local + remote (S3, GCS, Azure, GitHub, GitLab) |
| Data actions | Full (JSON, CSV, validate) | Full |
| Memory actions | Session + LTM + Cloud | Session only |
| Rate limiting | Yes | Yes |
| Vector/RAG | Yes | Not yet |
| Web scraping | Yes | Not yet |
| Graph DB | Yes | Not yet |
| Observability | Yes (Opik) | Not yet |
| Tool action dispatch | Yes (auto-execute) | Mapping only (placeholder results) |

## Remote File URL Support (TEA-CLI-002)

The Rust CLI supports loading workflows from remote URLs with intelligent caching. This provides feature parity with the Python implementation (TEA-CLI-001).

### Supported URL Schemes

| Scheme | Format | Example |
|--------|--------|---------|
| S3 | `s3://bucket/path/file.yaml` | `s3://my-bucket/workflows/agent.yaml` |
| GCS | `gs://bucket/path/file.yaml` or `gcs://...` | `gs://my-bucket/workflows/agent.yaml` |
| Azure | `az://container/path/file.yaml` or `azure://...` | `az://my-container/workflows/agent.yaml` |
| HTTP(S) | `https://example.com/file.yaml` | `https://example.com/workflows/agent.yaml` |
| GitHub | `github://owner/repo@ref/path` | `github://user/repo@main/workflows/agent.yaml` |
| GitLab | `gitlab://owner/repo@ref/path` | `gitlab://group/project@v1.0.0/config.yaml` |

### CLI Usage

```bash
# Run workflow from S3
tea run s3://my-bucket/workflows/agent.yaml

# Run workflow from GitHub (uses raw.githubusercontent.com)
tea run github://user/repo@main/workflows/agent.yaml

# Run without caching (always fetch fresh)
tea run --no-cache github://user/repo@main/workflow.yaml

# Use only cached version (fail if not cached)
tea run --cache-only github://user/repo@main/workflow.yaml

# Custom cache directory
tea run --cache-dir /tmp/my-cache github://user/repo@main/workflow.yaml
```

### Cache Management

```bash
# List all cached files
tea cache list

# Show cache statistics
tea cache info

# Clear all cache entries
tea cache clear

# Clear entries older than 1 hour
tea cache clear --older-than 1h

# Clear entries older than 7 days
tea cache clear --older-than 7d
```

### Cache Behavior

- **TTL-based**: Branch refs (`@main`, `@develop`) expire after 1 hour
- **Permanent**: Tags (`@v1.0.0`) and SHA refs are cached indefinitely
- **Python-compatible**: Manifest format is compatible with Python implementation
- **Cache key**: SHA256 hash of URL (first 16 hex characters)

### Cache Location

Default cache directory:
- `~/.cache/tea/remote_files/` (Linux/macOS)
- Can be overridden with `--cache-dir`

### Manifest Format

The cache uses a JSON manifest (`manifest.json`) compatible with Python:

```json
{
  "version": 1,
  "entries": {
    "abc123def456...": {
      "url": "github://user/repo@main/file.yaml",
      "local_path": "files/abc123def456.../file.yaml",
      "created_at": 1706745600,
      "ttl_seconds": 3600,
      "is_permanent": false,
      "size_bytes": 1024
    }
  }
}
```

### Security Mitigations

The remote file module implements several security measures:

| ID | Mitigation | Description |
|----|------------|-------------|
| SEC-001 | Credential masking | API keys, tokens, and Bearer tokens are masked in logs |
| SEC-002 | Path containment | Validates paths stay within cache directory |
| SEC-003 | SSRF protection | Blocks internal IPs (localhost, 127.0.0.1, 169.254.x.x) |

### Environment Variables

| Variable | Description |
|----------|-------------|
| `AWS_ACCESS_KEY_ID` | AWS access key for S3 |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key for S3 |
| `GOOGLE_APPLICATION_CREDENTIALS` | GCP service account JSON for GCS |
| `AZURE_STORAGE_ACCOUNT` | Azure storage account name |
| `AZURE_STORAGE_KEY` | Azure storage account key |
| `GITHUB_TOKEN` | GitHub personal access token (for private repos) |
| `GITLAB_TOKEN` | GitLab personal access token (for private repos) |

### Programmatic API

```rust
use the_edge_agent::remote::{
    RemoteFile, RemoteFileCache, RemoteFileSystem,
    DefaultRemoteFileSystem, MockRemoteFileSystem,
};

// Parse a URL
let remote_file = RemoteFile::parse("github://user/repo@main/file.yaml")?;

// Create cache
let cache = RemoteFileCache::new(None)?; // Uses default location

// Create filesystem with cache
let fs = DefaultRemoteFileSystem::new(Some(cache));

// Fetch file (uses cache if valid)
let local_path = fs.fetch("github://user/repo@main/file.yaml")?;

// Check if cached
if cache.has_valid("github://user/repo@main/file.yaml") {
    println!("Using cached version");
}
```

### Mock Filesystem for Testing

```rust
use the_edge_agent::remote::MockRemoteFileSystem;

let mut mock = MockRemoteFileSystem::new();
mock.add_response("s3://bucket/file.yaml", PathBuf::from("/tmp/cached.yaml"));
mock.add_error("s3://bucket/private.yaml", "Access denied");

// Use in tests
let result = mock.fetch("s3://bucket/file.yaml")?;
assert_eq!(result, PathBuf::from("/tmp/cached.yaml"));
```

## Adding Custom Actions

See [Development Guide](development-guide.md#adding-a-new-action) for instructions on implementing custom actions.

## Source Files

```
rust/src/actions/
├── mod.rs           # Action registry
├── llm.rs           # LLM API actions (~1800 lines, includes stream/tools)
├── llm_backend.rs   # LLM backend abstraction (local/api/auto)
├── llm_local.rs     # Local llama.cpp backend
├── http.rs          # HTTP actions (~200 lines)
├── file.rs          # File actions (~250 lines)
├── data.rs          # Data actions (~1000 lines)
├── memory.rs        # Memory actions (~700 lines)
└── ratelimit.rs     # Rate limiting actions (~500 lines)

rust/src/remote/      # Remote file support (TEA-CLI-002)
├── mod.rs           # Main module, RemoteFile enum, DefaultRemoteFileSystem
├── traits.rs        # RemoteFileSystem trait, MockRemoteFileSystem
├── cache.rs         # Python-compatible cache manager
├── git.rs           # GitHub/GitLab protocol handler
└── cloud.rs         # S3/GCS/Azure/HTTP fetcher
```
