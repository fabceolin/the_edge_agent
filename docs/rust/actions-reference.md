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
| Action modules | 20+ | 6 |
| LLM actions | Full (call, stream, tools, retry) | Full (call, stream, tools) |
| HTTP actions | Full | Full |
| File actions | Local + remote (S3, GCS, Azure) | Local only |
| Data actions | Full (JSON, CSV, validate) | Full |
| Memory actions | Session + LTM + Cloud | Session only |
| Rate limiting | Yes | Yes |
| Vector/RAG | Yes | Not yet |
| Web scraping | Yes | Not yet |
| Graph DB | Yes | Not yet |
| Observability | Yes (Opik) | Not yet |
| Tool action dispatch | Yes (auto-execute) | Mapping only (placeholder results) |

## Adding Custom Actions

See [Development Guide](development-guide.md#adding-a-new-action) for instructions on implementing custom actions.

## Source Files

```
rust/src/actions/
├── mod.rs        # Action registry
├── llm.rs        # LLM actions (~1800 lines, includes stream/tools)
├── http.rs       # HTTP actions (~200 lines)
├── file.rs       # File actions (~250 lines)
├── data.rs       # Data actions (~1000 lines)
├── memory.rs     # Memory actions (~700 lines)
└── ratelimit.rs  # Rate limiting actions (~500 lines)
```
