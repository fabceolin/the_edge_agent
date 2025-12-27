# LLM Orchestration

> Access 100+ LLM providers through a unified interface with streaming, tool calling, and automatic retries.

## Why This Matters

Modern AI applications need flexibility - the ability to switch between cloud providers like OpenAI and Anthropic, or run locally with Ollama. TEA abstracts provider differences behind a single, consistent API. You write your YAML once, then swap providers by changing one line.

## Quick Example

```yaml
name: intelligent-assistant
description: LLM with tool calling and structured output

nodes:
  - name: analyze_request
    uses: llm.tools
    with:
      provider: openai              # or: ollama, litellm
      model: gpt-4o
      messages:
        - role: system
          content: You are a helpful assistant with access to tools.
        - role: user
          content: "{{ state.user_query }}"
      tools:
        - name: search_documents
          description: Search internal knowledge base
          parameters:
            query:
              type: string
              required: true
    output: response
```

## Supported Providers

| Provider | Model Examples | Features | Configuration |
|----------|----------------|----------|---------------|
| **OpenAI** | gpt-4o, gpt-4-turbo, o1-preview | Full (streaming, tools, vision) | `OPENAI_API_KEY` |
| **Ollama** | llama3.2, mistral-nemo, qwen2.5 | Local, offline, tools (select models) | `provider: ollama` |
| **Azure OpenAI** | Deployed models | Enterprise, compliance | `AZURE_OPENAI_*` vars |
| **Anthropic** | claude-3-opus, claude-3-sonnet | Via LiteLLM | `provider: litellm` |
| **Google Gemini** | gemini-pro, gemini-ultra | Via LiteLLM | `provider: litellm` |
| **AWS Bedrock** | anthropic.claude-v2, ai21.j2-ultra | Via LiteLLM | `provider: litellm` |
| **Cohere** | command-r-plus | Via LiteLLM | `provider: litellm` |
| **Mistral AI** | mistral-large-latest | Via LiteLLM | `provider: litellm` |

**LiteLLM Integration**: Access 100+ providers through the unified LiteLLM interface. Install with:

```bash
pip install the_edge_agent[litellm]
```

## Available Actions

| Action | Description | Streaming | Tools |
|--------|-------------|-----------|-------|
| `llm.call` | Call any LLM provider | No | No |
| `llm.stream` | Stream response chunks | Yes | No |
| `llm.tools` | LLM with function calling | Optional | Yes |

[Full Actions Reference (Python)](../python/actions-reference.md) | [Full Actions Reference (Rust)](../rust/actions-reference.md)

## Key Features

| Feature | Description |
|---------|-------------|
| **Provider Abstraction** | Switch providers by changing one parameter |
| **Automatic Retries** | Exponential backoff for rate limits and transient errors |
| **Cost Tracking** | LiteLLM returns `cost_usd` for billing visibility |
| **Tool Calling** | Native function calling with automatic action dispatch |
| **Streaming** | Real-time response streaming with chunk aggregation |
| **Observability** | Opik integration for tracing LLM calls |

## Provider Examples

### OpenAI (Default)

```yaml
- name: generate
  uses: llm.call
  with:
    model: gpt-4o
    messages:
      - role: user
        content: "{{ state.prompt }}"
  output: response
```

### Ollama (Local)

```yaml
- name: local_inference
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1
    messages:
      - role: user
        content: "{{ state.prompt }}"
  output: response
```

### Anthropic via LiteLLM

```yaml
- name: claude_response
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    messages:
      - role: user
        content: "{{ state.prompt }}"
  output: response
```

### Streaming with Retry

```yaml
- name: stream_with_retry
  uses: llm.stream
  with:
    model: gpt-4o
    messages:
      - role: user
        content: "{{ state.prompt }}"
    max_retries: 3
    timeout: 60
  output: response
```

## Tool Calling

Define tools inline and let the LLM decide when to use them:

```yaml
- name: agent_with_tools
  uses: llm.tools
  with:
    model: gpt-4o
    messages:
      - role: system
        content: You can use tools to help answer questions.
      - role: user
        content: "{{ state.question }}"
    tools:
      - name: get_weather
        description: Get current weather for a location
        parameters:
          location:
            type: string
            description: City name
            required: true
      - name: search_web
        description: Search the web for information
        parameters:
          query:
            type: string
            required: true
    max_tool_rounds: 10
  output: agent_result
```

**Tool-capable Ollama models**: llama3.1+, mistral-nemo, qwen2.5

## Environment Variables

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `AZURE_OPENAI_DEPLOYMENT` | Azure | Deployment name |
| `OLLAMA_API_BASE` | Ollama | Ollama URL (default: `http://localhost:11434/v1`) |
| `ANTHROPIC_API_KEY` | LiteLLM | Anthropic API key |
| `GEMINI_API_KEY` | LiteLLM | Google Gemini API key |

## Examples

- [Customer Support Agent](../../examples/yaml_customer_support_example.yaml) - Multi-path workflow with intent classification
- [Perplexity Research](../../examples/yaml_perplexity_example.yaml) - Web search integration
- [Streaming Example](../../examples/stream_example.py) - Python streaming demonstration

## Learn More

- [YAML Reference - LLM Actions](../shared/YAML_REFERENCE.md#llm-actions)
- [Python Actions Reference](../python/actions-reference.md#llm-actions)
- [Rust Actions Reference](../rust/actions-reference.md#llm-actions)
- [LiteLLM Provider Docs](https://docs.litellm.ai/docs/providers)
- [Ollama Support Story](../stories/TEA-BUILTIN-009-ollama-provider-support.md)
- [LiteLLM Integration Story](../stories/TEA-LLM-003-litellm-provider-integration.md)
