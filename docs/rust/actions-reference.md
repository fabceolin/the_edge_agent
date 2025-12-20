# Rust Actions Reference

This document lists all built-in actions available in the Rust implementation of The Edge Agent.

## Available Actions

The Rust implementation currently provides 5 action modules with the following actions:

### LLM Actions (`actions/llm.rs`)

| Action | Description |
|--------|-------------|
| `llm.call` | Call OpenAI-compatible LLM API |
| `llm.stream` | Stream LLM response chunks |

**Parameters:**

```yaml
action: llm.call
with:
  model: gpt-4                    # Required: Model name
  prompt: "{{ state.input }}"     # Required: Prompt text
  api_key: "{{ secrets.OPENAI_KEY }}"  # Optional: API key
  temperature: 0.7                # Optional: Sampling temperature
  max_tokens: 1000                # Optional: Maximum tokens
  output_key: response            # Required: State key for result
```

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
| Action modules | 20+ | 5 |
| LLM actions | Full (call, stream, tools, retry) | Basic (call, stream) |
| HTTP actions | Full | Full |
| File actions | Local + remote (S3, GCS, Azure) | Local only |
| Data actions | Full (JSON, CSV, validate) | Full |
| Memory actions | Session + LTM + Cloud | Session only |
| Vector/RAG | Yes | Not yet |
| Web scraping | Yes | Not yet |
| Graph DB | Yes | Not yet |
| Observability | Yes (Opik) | Not yet |

## Adding Custom Actions

See [Development Guide](development-guide.md#adding-a-new-action) for instructions on implementing custom actions.

## Source Files

```
rust/src/actions/
├── mod.rs      # Action registry
├── llm.rs      # LLM actions (~500 lines)
├── http.rs     # HTTP actions (~200 lines)
├── file.rs     # File actions (~250 lines)
├── data.rs     # Data actions (~1000 lines)
└── memory.rs   # Memory actions (~700 lines)
```
