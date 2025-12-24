# Python Actions Reference

This document lists all built-in actions available in the Python implementation of The Edge Agent.

For complete action documentation including parameters and examples, see the [YAML Reference](../shared/YAML_REFERENCE.md#built-in-actions).

## Action Modules

The Python implementation provides 20+ action modules organized by domain:

### Core Actions

| Action | Module | Description |
|--------|--------|-------------|
| `llm.call` | `llm_actions.py` | Call OpenAI-compatible LLM |
| `llm.stream` | `llm_actions.py` | Stream LLM response |
| `llm.tools` | `llm_actions.py` | LLM with tool calling |

#### LLM Provider Configuration

The LLM actions support multiple providers: **OpenAI**, **Azure OpenAI**, and **Ollama**.

**Provider Detection Priority:**
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection:
   - `OLLAMA_API_BASE` → Ollama
   - `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` → Azure OpenAI
3. Default → OpenAI

**Ollama Example:**

```yaml
# Explicit provider parameter
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1  # optional, this is the default
    messages:
      - role: user
        content: "{{ state.question }}"

# Environment variable fallback (set OLLAMA_API_BASE)
- name: ask_llm
  uses: llm.call
  with:
    model: llama3.2
    messages:
      - role: user
        content: "{{ state.question }}"
```

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `AZURE_OPENAI_DEPLOYMENT` | Azure | Deployment name (optional) |
| `OLLAMA_API_BASE` | Ollama | Ollama API URL (default: `http://localhost:11434/v1`) |

**Ollama Tool Calling:**

Tool calling with Ollama requires models that support it:
- `llama3.1+`
- `mistral-nemo`
- `qwen2.5`

```yaml
- name: llm_with_tools
  uses: llm.tools
  with:
    provider: ollama
    model: mistral-nemo
    messages:
      - role: user
        content: "What's the weather in London?"
    tools:
      - name: get_weather
        description: Get weather for a location
        parameters:
          location:
            type: string
            required: true
```

### HTTP Actions

| Action | Module | Description |
|--------|--------|-------------|
| `http.get` | `http_actions.py` | HTTP GET request |
| `http.post` | `http_actions.py` | HTTP POST request |

### File Actions

| Action | Module | Description |
|--------|--------|-------------|
| `file.read` | `file_actions.py` | Read local/remote file |
| `file.write` | `file_actions.py` | Write local/remote file |

### Storage Actions

| Action | Module | Description |
|--------|--------|-------------|
| `storage.list` | `storage_actions.py` | List files in directory |
| `storage.copy` | `storage_actions.py` | Copy files |
| `storage.delete` | `storage_actions.py` | Delete files |

### Data Actions

| Action | Module | Description |
|--------|--------|-------------|
| `json.parse` | `data_actions.py` | Parse JSON string |
| `json.transform` | `data_actions.py` | Transform JSON with JMESPath |
| `csv.parse` | `data_actions.py` | Parse CSV data |
| `data.validate` | `data_actions.py` | Validate against JSON Schema |

### Code Execution

| Action | Module | Description |
|--------|--------|-------------|
| `code.execute` | `code_actions.py` | Execute Python code (sandboxed) |
| `code.sandbox` | `code_actions.py` | Run in RestrictedPython sandbox |

### Memory Actions

| Action | Module | Description |
|--------|--------|-------------|
| `memory.store` | `memory_actions.py` | Store in session memory |
| `memory.retrieve` | `memory_actions.py` | Retrieve from session memory |
| `ltm.store` | `ltm_actions.py` | Store in long-term memory |
| `ltm.retrieve` | `ltm_actions.py` | Retrieve from long-term memory |

### Vector/RAG Actions

| Action | Module | Description |
|--------|--------|-------------|
| `embedding.create` | `vector_actions.py` | Create text embeddings |
| `vector.store` | `vector_actions.py` | Store in vector database |
| `vector.query` | `vector_actions.py` | Query vector database |

### Web Actions

| Action | Module | Description |
|--------|--------|-------------|
| `web.scrape` | `web_actions.py` | Scrape web page (Firecrawl) |
| `web.crawl` | `web_actions.py` | Crawl multiple pages (Firecrawl) |
| `web.search` | `web_actions.py` | Web search (Perplexity) |
| `web.ai_scrape` | `web_actions.py` | AI-powered structured extraction (ScrapeGraphAI) |

### Graph Actions

| Action | Module | Description |
|--------|--------|-------------|
| `graph.store_entity` | `graph_actions.py` | Store entity in graph DB |
| `graph.query` | `graph_actions.py` | Query graph database |

### Observability

| Action | Module | Description |
|--------|--------|-------------|
| `trace.start` | `observability_actions.py` | Start trace span |
| `trace.log` | `observability_actions.py` | Log trace event |
| `trace.end` | `observability_actions.py` | End trace span |

## Source Location

All action modules are in:

```
python/src/the_edge_agent/actions/
├── __init__.py
├── llm_actions.py
├── http_actions.py
├── file_actions.py
├── storage_actions.py
├── data_actions.py
├── code_actions.py
├── memory_actions.py
├── ltm_actions.py
├── cloud_memory_actions.py
├── vector_actions.py
├── graph_actions.py
├── web_actions.py
├── tools_actions.py
├── observability_actions.py
└── context_actions.py
```

## Custom Actions

Register custom actions via the `imports:` section in YAML:

```yaml
imports:
  - path: ./my_actions.py
    actions:
      - my_custom_action
```

Your module must implement `register_actions()`:

```python
def register_actions(registry, engine):
    registry['my_custom_action'] = my_action_function
```
