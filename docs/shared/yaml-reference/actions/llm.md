# LLM Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** [DOC-002](../../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

LLM actions provide integration with language models from multiple providers. All actions support OpenAI, Azure OpenAI, Ollama, and LiteLLM providers.

---

## Table of Contents

- [llm.call](#llmcall)
- [LLM Provider Configuration](#llm-provider-configuration)
  - [Provider Detection](#provider-detection)
  - [Ollama Example](#ollama-example)
  - [LiteLLM Provider](#litellm-provider)
  - [Shell Provider](#shell-provider)
- [llm.stream](#llmstream)
- [llm.retry](#llmretry)
- [llm.tools](#llmtools)

---

## `llm.call`

Call OpenAI-compatible LLM API:

```yaml
- name: generate
  uses: llm.call
  with:
    model: gpt-4                    # Required
    messages:                       # Required
      - role: system
        content: You are helpful
      - role: user
        content: "{{ state.prompt }}"
    temperature: 0.7                # Optional (default: 0.7)
  output: llm_response
```

**Returns:**
```python
{"content": "LLM response text", "usage": {"prompt_tokens": N, "completion_tokens": N}}
```

---

## LLM Provider Configuration

LLM actions support multiple providers: **OpenAI**, **Azure OpenAI**, **Ollama**, and **LiteLLM**.

### Provider Detection

**Detection Priority:**
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection:
   - `OLLAMA_API_BASE` → Ollama
   - `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` → Azure OpenAI
3. Default → OpenAI

**Provider Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `provider` | Provider selection: `auto`, `openai`, `azure`, `ollama`, `litellm` | `auto` |
| `api_base` | Custom API base URL | Provider default |

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `OLLAMA_API_BASE` | Ollama | Ollama API URL (default: `http://localhost:11434/v1`) |

### Ollama Example

```yaml
# Explicit provider parameter
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama                # Use local Ollama
    model: llama3.2                 # Ollama model name
    api_base: http://localhost:11434/v1  # Optional, this is the default
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Environment variable fallback (set OLLAMA_API_BASE)
- name: ask_llm
  uses: llm.call
  with:
    model: llama3.2
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

**Ollama Notes:**
- No API key required (uses dummy value internally)
- No cost calculation (local/free)
- Tool calling requires compatible models: `llama3.1+`, `mistral-nemo`, `qwen2.5`

### LiteLLM Provider

LiteLLM provides access to 100+ LLM providers through a unified OpenAI-compatible interface.

**Installation:**
```bash
pip install the_edge_agent[litellm]
```

**Example:**

```yaml
# Use Anthropic Claude via LiteLLM
- name: ask_claude
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Use Google Gemini via LiteLLM
- name: ask_gemini
  uses: llm.call
  with:
    provider: litellm
    model: gemini/gemini-pro
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response

# Use AWS Bedrock via LiteLLM
- name: ask_bedrock
  uses: llm.call
  with:
    provider: litellm
    model: bedrock/anthropic.claude-v2
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

**LiteLLM Model Format:**

LiteLLM uses `provider/model-name` format:

| Provider | Model Example |
|----------|---------------|
| Anthropic | `anthropic/claude-3-opus-20240229` |
| AWS Bedrock | `bedrock/anthropic.claude-v2` |
| Google Gemini | `gemini/gemini-pro` |
| Azure OpenAI | `azure/gpt-4` |
| Ollama (via LiteLLM) | `ollama/llama3.2` |
| Cohere | `cohere/command-r-plus` |
| Mistral | `mistral/mistral-large-latest` |

**LiteLLM Environment Variables:**

| Variable | Provider |
|----------|----------|
| `ANTHROPIC_API_KEY` | Anthropic Claude |
| `GOOGLE_API_KEY` | Google Gemini |
| `COHERE_API_KEY` | Cohere |
| `MISTRAL_API_KEY` | Mistral AI |
| `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` | AWS Bedrock |

See [LiteLLM Providers](https://docs.litellm.ai/docs/providers) for complete list.

**LiteLLM Features:**
- Built-in cost tracking via `cost_usd` in response
- Automatic retry with exponential backoff (`max_retries` parameter)
- Opik observability integration (`opik_trace=True`)
- Streaming support (`llm.stream`)
- Tool calling support (`llm.tools`) for compatible models

**LiteLLM with Opik Tracing:**

```yaml
- name: traced_call
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    opik_trace: true  # Enable Opik logging
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

### Shell Provider

The Shell provider allows you to execute local CLI commands for LLM calls. This is useful for leveraging CLI tools like `claude`, `gemini`, or `qwen` that you already have installed, avoiding API costs while using familiar command-line tools.

**Basic Usage:**

```yaml
- name: ask_claude
  uses: llm.call
  with:
    provider: shell
    shell_provider: claude          # Which shell provider config to use
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

**Built-in Shell Providers:**

Three shell providers are pre-configured:

| Provider | Command | Default Args |
|----------|---------|--------------|
| `claude` | `claude` | `["-p"]` |
| `gemini` | `gemini` | `["prompt"]` |
| `qwen` | `qwen` | `[]` |

**Custom Shell Providers:**

Configure custom CLI tools in `settings.llm.shell_providers`:

```yaml
settings:
  llm:
    shell_providers:
      my_local_llm:
        command: /usr/local/bin/my-llm
        args: ["--model", "mistral-7b", "--input", "-"]
        stdin_mode: pipe              # pipe (default) or file
        timeout: 600                  # seconds
        env:                          # Optional extra env vars
          MY_API_KEY: "${MY_API_KEY}"
```

**Shell Provider Parameters:**

| Parameter | Description | Default |
|-----------|-------------|---------|
| `command` | CLI command to execute | Required |
| `args` | Command arguments | `[]` |
| `stdin_mode` | How to send input: `pipe` or `file` | `pipe` |
| `timeout` | Max execution time in seconds | `300` |
| `env` | Additional environment variables | `{}` |

**Environment Variable Expansion:**

Config values support `${VAR}` syntax for environment variable expansion:

```yaml
settings:
  llm:
    shell_providers:
      secure_llm:
        command: secure-llm-cli
        args: []
        env:
          API_KEY: "${SECRET_API_KEY}"
          MODEL_PATH: "${HOME}/models/mistral"
```

**File Mode for Large Contexts:**

For very large prompts that may exceed stdin buffer limits, use `stdin_mode: file`:

```yaml
settings:
  llm:
    shell_providers:
      large_context_llm:
        command: my-llm
        args: ["--input-file", "{input_file}"]  # {input_file} is replaced with temp file path
        stdin_mode: file
        timeout: 600
```

**Streaming with Shell Provider:**

Shell provider also supports `llm.stream` with line-by-line output aggregation:

```yaml
- name: stream_claude
  uses: llm.stream
  with:
    provider: shell
    shell_provider: claude
    messages:
      - role: user
        content: "Write a poem about coding"
  output: poem
```

**Message Formatting:**

Messages are formatted for CLI stdin as plain text:
- System messages: `System: <content>`
- Assistant messages: `Assistant: <content>`
- User messages: `<content>` (no prefix)

Messages are joined with double newlines.

**Error Handling:**

Shell provider returns appropriate error responses for:
- Command not found: `{"error": "Shell command not found: ...", "success": false}`
- Timeout: `{"error": "Shell command timed out after Ns", "success": false}`
- Non-zero exit: `{"error": "Shell command failed (exit N): stderr...", "success": false}`

---

## `llm.stream`

Stream LLM responses with chunk aggregation:

```yaml
- name: stream_response
  uses: llm.stream
  with:
    model: gpt-4
    messages:
      - role: user
        content: "{{ state.query }}"
    temperature: 0.7
  output: stream_result
```

**Returns:**
```python
{"content": str, "usage": dict, "streamed": true, "chunk_count": int}
```

---

## `llm.retry`

LLM calls with exponential backoff retry logic:

```yaml
- name: resilient_call
  uses: llm.retry
  with:
    model: gpt-4
    messages:
      - role: user
        content: "{{ state.query }}"
    max_retries: 3          # Optional (default: 3)
    base_delay: 1.0         # Optional (default: 1.0)
    max_delay: 60.0         # Optional (default: 60.0)
  output: retry_result
```

**Returns:**
- Success: `{"content": str, "usage": dict, "attempts": int, "total_delay": float}`
- Failure: `{"error": str, "success": false, "attempts": int, "total_delay": float}`

**Retry behavior:**
- Retryable: HTTP 429 (rate limit), HTTP 5xx, timeouts, connection errors
- Non-retryable: HTTP 4xx (except 429)
- Respects `Retry-After` header when present

---

## `llm.tools`

Function/tool calling with automatic action dispatch:

```yaml
- name: agent_with_tools
  uses: llm.tools
  with:
    model: gpt-4
    messages:
      - role: system
        content: You are a helpful assistant with access to tools.
      - role: user
        content: "{{ state.query }}"
    tools:
      - name: search_web
        description: Search the web for information
        parameters:
          query:
            type: string
            description: Search query
            required: true
        action: http.get            # Maps to registered action
    tool_choice: auto               # Optional: "auto", "none", or tool name
    max_tool_rounds: 10             # Optional (default: 10)
  output: tools_result
```

**Returns:**
- Success: `{"content": str, "tool_calls": list, "tool_results": list, "rounds": int}`
- Failure: `{"error": str, "success": false, "tool_calls": list, "tool_results": list}`

---

## Dual Namespace

All LLM actions are available via dual namespaces: `llm.*` and `actions.llm_*`.

---

## See Also

- [Actions Overview](./README.md)
- [I/O Actions](./io.md) - HTTP and file operations
- [Integrations](./integrations.md) - Observability and tracing
