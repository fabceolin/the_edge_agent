# Built-in Actions Reference

> **Parent document:** [YAML Reference](../../YAML_REFERENCE.md)
> **Related:** [Node Specification](../nodes.md) | [Template Syntax](../templates.md)
> **Epic:** [DOC-002](../../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

The Edge Agent provides **55+ built-in actions** organized into 6 categories. Actions are the building blocks for YAML agent workflows, providing capabilities from LLM calls to file I/O, data processing, memory persistence, and external integrations.

---

## Quick Reference

| Action | Description | Document |
|--------|-------------|----------|
| `llm.call` | Call LLM API | [LLM](./llm.md#llmcall) |
| `llm.stream` | Stream LLM response | [LLM](./llm.md#llmstream) |
| `llm.tools` | Function/tool calling | [LLM](./llm.md#llmtools) |
| `http.get` | HTTP GET request | [I/O](./io.md#httpget) |
| `http.post` | HTTP POST request | [I/O](./io.md#httppost) |
| `file.read` | Read file | [I/O](./io.md#fileread) |
| `file.write` | Write file | [I/O](./io.md#filewrite) |
| `json.parse` | Parse JSON string | [Data](./data.md#jsonparse) |
| `json.transform` | JMESPath/JSONPath transform | [Data](./data.md#jsontransform) |
| `memory.store` | Store in session memory | [Memory](./memory.md#memorystore) |
| `ltm.store` | Long-term memory store | [Memory](./memory.md#ltmstore) |
| `vector.query` | Semantic search | [Integrations](./integrations.md#vectorquery) |
| `web.scrape` | Web scraping | [Integrations](./integrations.md#webscrape) |
| `retry.loop` | Retry with correction | [Specialized](./specialized.md#retryloop) |
| `ratelimit.wrap` | Rate limit any action | [Specialized](./specialized.md#ratelimitwrap) |
| `secrets.get` | Get secret value | [Specialized](./specialized.md#secretsget) |
| `secrets.has` | Check secret exists | [Specialized](./specialized.md#secretshas) |

---

## Action Categories

### [LLM Actions](./llm.md) (4 actions)

Language model integration with multiple providers.

| Action | Description |
|--------|-------------|
| `llm.call` | Call OpenAI-compatible LLM API |
| `llm.stream` | Stream LLM responses with chunk aggregation |
| `llm.retry` | LLM calls with exponential backoff retry |
| `llm.tools` | Function/tool calling with action dispatch |

**Providers:** OpenAI, Azure OpenAI, Ollama, LiteLLM (100+ models)

---

### [I/O Actions](./io.md) (10 actions)

HTTP requests, file operations, and cloud storage.

| Action | Description |
|--------|-------------|
| `http.get` | HTTP GET request |
| `http.post` | HTTP POST request |
| `file.read` | Read local or remote files |
| `file.write` | Write local or remote files |
| `storage.list` | List files in directory |
| `storage.exists` | Check if file exists |
| `storage.info` | Get file metadata |
| `storage.copy` | Copy files (cross-cloud) |
| `storage.delete` | Delete files |
| `storage.mkdir` | Create directories |

**Cloud Support:** S3, GCS, Azure Blob via fsspec

---

### [Data Processing Actions](./data.md) (15 actions)

JSON/CSV parsing, validation, transformation, and code execution.

| Action | Description |
|--------|-------------|
| `json.parse` | Parse JSON string to object |
| `json.transform` | Transform with JMESPath/JSONPath |
| `json.stringify` | Convert object to JSON string |
| `csv.parse` | Parse CSV to list of dicts |
| `csv.stringify` | Convert list to CSV |
| `data.validate` | Validate against JSON Schema |
| `data.merge` | Deep merge dictionaries |
| `data.filter` | Filter list with predicates |
| `code.execute` | Execute sandboxed Python |
| `code.sandbox` | Manage persistent sandbox sessions |
| `data.create_table` | Create tabular data table |
| `data.insert` | Insert rows into table |
| `data.update` | Update rows by primary key |
| `data.delete` | Delete rows (tombstone) |
| `data.query` | SQL query with LWW merge |

---

### [Memory Actions](./memory.md) (20 actions)

Session memory, long-term memory, caching, and graph storage.

| Action | Description |
|--------|-------------|
| `memory.store` | Store in session memory |
| `memory.retrieve` | Retrieve from session |
| `memory.summarize` | Summarize with LLM |
| `ltm.store` | Store in long-term memory |
| `ltm.retrieve` | Retrieve from LTM |
| `ltm.delete` | Delete from LTM |
| `ltm.search` | Search LTM by pattern |
| `cache.wrap` | Cache action results |
| `cache.get` | Get cached value |
| `cache.invalidate` | Invalidate cache entries |
| `memory.cloud_store` | Firebase Agent Memory store |
| `memory.cloud_retrieve` | Firebase retrieve |
| `memory.cloud_list` | Firebase list memories |
| `memory.grep` | Text search with ranking |
| `memory.sql_query` | SQL query via DuckDB |
| `memory.embed` | Generate embeddings |
| `memory.vector_search` | Vector similarity search |
| `graph.store_entity` | Store graph entity |
| `graph.store_relation` | Store graph relationship |
| `graph.query` | Query graph with Cypher/Datalog |

**Backends:** SQLite, DuckDB, Firebase, CozoDB, Kuzu

---

### [Integration Actions](./integrations.md) (15 actions)

External services, web scraping, RAG, and observability.

| Action | Description |
|--------|-------------|
| `trace.start` | Start trace span |
| `trace.log` | Log events/metrics |
| `trace.end` | End trace span |
| `opik.healthcheck` | Validate Opik connection |
| `web.scrape` | Scrape single page (Firecrawl) |
| `web.crawl` | Crawl multiple pages |
| `web.search` | Web search (Perplexity) |
| `web.ai_scrape` | AI-powered extraction (ScrapeGraphAI) |
| `embedding.create` | Generate embeddings |
| `vector.store` | Store with embeddings |
| `vector.query` | Semantic similarity search |
| `tools.crewai` | Execute CrewAI tools |
| `tools.mcp` | Execute MCP server tools |
| `tools.langchain` | Execute LangChain tools |
| `tools.discover` | Discover available tools |

**External APIs:** Firecrawl, Perplexity, ScrapeGraphAI, Opik

---

### [Specialized Actions](./specialized.md) (15 actions)

Checkpoints, schema manipulation, document extraction, validation, rate limiting, and secrets management.

| Action | Description |
|--------|-------------|
| `checkpoint.save` | Save workflow checkpoint |
| `checkpoint.load` | Load checkpoint |
| `schema.merge` | Deep merge JSON Schemas |
| `llamaextract.extract` | Extract structured data from documents |
| `llamaextract.upload_agent` | Create LlamaExtract agent |
| `llamaextract.list_agents` | List extraction agents |
| `llamaextract.get_agent` | Get agent details |
| `llamaextract.delete_agent` | Delete extraction agent |
| `validate.extraction` | 3-layer extraction validation |
| `validate.generate_prompt` | Generate extraction prompt |
| `retry.loop` | Retry with correction node |
| `ratelimit.wrap` | Rate limit any action (RPM/RPS) |
| `secrets.get` | Get secret from cloud provider |
| `secrets.has` | Check if secret exists |
| Custom Actions | Register Python functions |

**Features:** Prolog constraints, semantic probes, checkpoint persistence, rate limiting, cloud secrets (AWS, Azure, GCP)

---

## Dual Namespace Convention

All actions support two naming conventions:

```yaml
# Dotted namespace (recommended)
- uses: llm.call
- uses: file.read
- uses: memory.store

# Underscore namespace (alternative)
- uses: actions.llm_call
- uses: actions.file_read
- uses: actions.memory_store
```

Both forms are functionally identical. The dotted namespace is preferred for readability.

---

## Action Anatomy

Every action follows a consistent structure:

```yaml
- name: step_name           # Unique identifier for this step
  uses: category.action     # Action to execute
  with:                     # Parameters (action-specific)
    param1: value1
    param2: "{{ state.dynamic }}"
  output: result_key        # Where to store results in state
  condition: "{{ state.should_run }}"  # Optional: skip if false
```

**Common Return Fields:**

| Field | Type | Description |
|-------|------|-------------|
| `success` | bool | Whether action succeeded |
| `error` | string | Error message (on failure) |
| `error_type` | string | Error category for handling |

---

## Error Handling

Actions return structured errors for conditional handling:

```yaml
- name: risky_operation
  uses: http.get
  with:
    url: "{{ state.url }}"
  output: result

- name: handle_error
  condition: "{{ not result.success }}"
  uses: llm.call
  with:
    model: gpt-4
    messages:
      - role: user
        content: "Handle error: {{ result.error }}"
```

---

## See Also

- [Node Specification](../nodes.md) - Complete node structure
- [Template Syntax](../templates.md) - Jinja2 expressions
- [Advanced Runtimes](../advanced-runtimes.md) - Lua and Prolog nodes
- [YAML Reference](../../YAML_REFERENCE.md) - Complete reference
