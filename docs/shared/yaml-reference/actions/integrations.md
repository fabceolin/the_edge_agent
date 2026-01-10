# Integration Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

Integration actions provide web scraping, search, RAG (Retrieval-Augmented Generation), external tools bridge, observability/tracing, and notification capabilities.

---

## Table of Contents

- [Observability Actions](#observability-actions)
  - [trace.start](#tracestart)
  - [trace.log](#tracelog)
  - [trace.end](#traceend)
  - [Auto-Instrumentation](#auto-instrumentation)
  - [Opik Integration](#opik-integration)
- [Web Actions](#web-actions)
  - [web.scrape](#webscrape)
  - [web.crawl](#webcrawl)
  - [web.search](#websearch)
  - [web.ai_scrape](#webai_scrape)
- [RAG Actions](#rag-actions)
  - [embedding.create](#embeddingcreate)
  - [vector.store](#vectorstore)
  - [vector.query](#vectorquery)
- [LlamaIndex RAG Bridge](#llamaindex-rag-bridge)
  - [rag.llamaindex.query](#ragllamaindexquery)
  - [rag.llamaindex.router](#ragllamaindexrouter)
  - [rag.llamaindex.subquestion](#ragllamaindexsubquestion)
  - [rag.llamaindex.create_index](#ragllamaindexcreate_index)
  - [rag.llamaindex.load_index](#ragllamaindexload_index)
  - [rag.llamaindex.add_documents](#ragllamaindexadd_documents)
- [Tools Bridge Actions](#tools-bridge-actions)
  - [tools.crewai](#toolscrewai)
  - [tools.mcp](#toolsmcp)
  - [tools.langchain](#toolslangchain)
  - [tools.discover](#toolsdiscover)
- [Notification Actions](#notification-actions)
  - [actions.notify](#actionsnotify)

---

## Observability Actions

### `trace.start`

Start a new trace span:

```yaml
- name: start_trace
  uses: trace.start
  with:
    name: "process_data"                  # Required
    metadata:                             # Optional
      user_id: "{{ state.user_id }}"
      operation: "data_processing"
    parent_id: "{{ state.parent_span }}"  # Optional
  output: span_info
```

**Returns:** `{"span_id": str, "name": str, "parent_id": str | null, "success": true}`

### `trace.log`

Log events, metrics, or state snapshots:

```yaml
# Log message
- name: log_progress
  uses: trace.log
  with:
    message: "Processing step completed"

# Log metrics
- name: log_metrics
  uses: trace.log
  with:
    metrics:
      items_processed: 100
      duration_ms: 250

# Snapshot state
- name: log_state
  uses: trace.log
  with:
    message: "Before API call"
    snapshot_state: true
    sanitize_keys: ["api_key", "password"]
```

**Returns:** `{"logged": true, "span_id": str, "event_count": int, "success": true}`

### `trace.end`

End current trace span:

```yaml
- name: end_trace
  uses: trace.end
  with:
    status: ok                            # "ok" or "error"
    error: "{{ state.error_message }}"    # Optional (for status: error)
```

**Returns:** `{"span_id": str, "duration_ms": float, "status": str, "success": true}`

### Auto-Instrumentation

Enable automatic tracing via YAML settings:

```yaml
settings:
  auto_trace: true         # Auto-wrap all nodes with tracing
  trace_exporter: console  # "console", "file"
  trace_file: ./traces.jsonl
```

### Opik Integration

Export traces to [Comet Opik](https://www.comet.com/site/products/opik/) for visualization and LLM observability.

**Required:** `pip install opik` or `pip install the-edge-agent[opik]`

**Basic Configuration:**

```yaml
settings:
  trace_exporter: opik
  opik:
    enabled: true
    api_key: "${OPIK_API_KEY}"
    workspace: my-team
    project_name: my-agent-production
    url: https://opik.mycompany.com/api  # Optional: Self-hosted
    llm_tracing: true                    # Wrap OpenAI with track_openai()
    trace_export: true                   # Export TEA spans to Opik
```

**Environment Variables:**

| Variable | Description | Default |
|----------|-------------|---------|
| `OPIK_API_KEY` | API key for Opik Cloud | None (required for Cloud) |
| `OPIK_WORKSPACE` | Workspace/organization name | User's default workspace |
| `OPIK_PROJECT_NAME` | Project for grouping traces | `"the-edge-agent"` |
| `OPIK_URL_OVERRIDE` | Self-hosted Opik endpoint URL | Opik Cloud URL |

**`opik.healthcheck`:**

```yaml
- name: validate_opik
  uses: opik.healthcheck
  output: opik_status
```

**Returns:**
- Success: `{"success": true, "latency_ms": float, "project": str, "workspace": str}`
- Failure: `{"success": false, "message": str, "error_type": str}`

---

## Web Actions

Web scraping and search via external APIs.

**Required environment variables:**
- `FIRECRAWL_API_KEY` - For web.scrape and web.crawl
- `PERPLEXITY_API_KEY` - For web.search
- `SCRAPEGRAPH_API_KEY` - For web.ai_scrape

### `web.scrape`

Scrape web content via Firecrawl API:

```yaml
- name: fetch_article
  uses: web.scrape
  with:
    url: "{{ state.target_url }}"         # Required
    formats: ["markdown", "links"]        # Optional
    only_main_content: true               # Optional
    timeout: 30000                        # Optional (ms)
  output: scraped_content
```

**Returns:** `{"success": true, "url": str, "markdown": str, "links": list, "metadata": dict}`

### `web.crawl`

Crawl multiple pages:

```yaml
- name: crawl_docs
  uses: web.crawl
  with:
    url: "https://docs.example.com"       # Required
    max_depth: 2                          # Optional (default: 2)
    limit: 20                             # Optional (default: 10)
    include_paths: ["/api/*"]             # Optional
    exclude_paths: ["/admin/*"]           # Optional
  output: crawled_pages
```

**Returns:** `{"success": true, "pages": list, "total_pages": int, "job_id": str}`

### `web.search`

Web search via Perplexity API:

```yaml
- name: search_topic
  uses: web.search
  with:
    query: "{{ state.topic }}"            # Required
    num_results: 10                       # Optional
  output: search_results
```

**Returns:** `{"success": true, "results": list, "query": str, "total_results": int, "answer": str}`

### `web.ai_scrape`

AI-powered structured data extraction via ScrapeGraphAI API.

**Required:** `SCRAPEGRAPH_API_KEY` environment variable.

```yaml
# Basic usage with inline schema
- name: extract_products
  uses: web.ai_scrape
  with:
    url: "{{ state.target_url }}"         # Required
    prompt: "Extract all products"        # Required
    output_schema:                         # Schema (inline dict)
      type: object
      properties:
        products:
          type: array
          items:
            type: object
            properties:
              name: { type: string }
              price: { type: string }
    max_retries: 3                         # Optional (default: 3)
  output: extracted_data

# Load schema from Git reference
- name: extract_invoice
  uses: web.ai_scrape
  with:
    url: "{{ state.invoice_url }}"
    prompt: "Extract invoice data"
    schema:
      uses: company/schemas@v1.0.0#invoice/schema.json
  output: invoice_data

# With caching
- name: extract_cached
  uses: web.ai_scrape
  with:
    url: "{{ state.url }}"
    prompt: "Extract product data"
    output_schema:
      type: object
      properties:
        name: { type: string }
        price: { type: string }
    cache:
      enabled: true
      ttl_days: 30
      key_strategy: "url"
  output: product_data
```

**Cache Key Strategies:**
- `"url"` - Hash URL only
- `"url+prompt"` - Hash URL + prompt
- `"url+schema"` - Hash URL + schema
- `"url+prompt+schema"` - Hash all three (most granular)

**Returns:**
```python
{
    "success": True,
    "data": {...},
    "url": str,
    "schema_used": {...},
    "_cache_hit": bool,
    "_cache_key": str
}
```

---

## RAG Actions

Retrieval-Augmented Generation with embeddings and vector search.

**Providers:**
- OpenAI: `text-embedding-3-small`, `text-embedding-3-large`, `text-embedding-ada-002`
- Ollama: `nomic-embed-text`, `mxbai-embed-large`, `all-minilm`, `bge-m3`

### `embedding.create`

Generate embeddings from text:

```yaml
# Single text
- name: embed_query
  uses: embedding.create
  with:
    text: "{{ state.query }}"             # Required
    model: text-embedding-3-small         # Optional
    provider: openai                      # Optional: "openai" or "ollama"
  output: embedding_result

# Batch embedding
- name: embed_documents
  uses: embedding.create
  with:
    text: "{{ state.documents }}"         # List of texts
  output: embeddings_result
```

**Returns:**
- Single: `{"embedding": list[float], "model": str, "dimensions": int}`
- Batch: `{"embeddings": list[list[float]], "model": str, "count": int, "dimensions": int}`

### `vector.store`

Store documents with embeddings:

```yaml
- name: store_docs
  uses: vector.store
  with:
    texts:                                # Required
      - "First document content"
      - "Second document content"
    metadata:                             # Optional
      - type: article
      - type: blog
    collection: my_knowledge_base         # Optional
  output: store_result
```

**Returns:** `{"stored": int, "collection": str, "ids": list[str]}`

### `vector.query`

Semantic similarity search:

```yaml
- name: search_knowledge
  uses: vector.query
  with:
    query: "{{ state.question }}"         # Required
    k: 5                                  # Optional (default: 5)
    collection: my_knowledge_base         # Optional
    filter:                               # Optional
      type: article
  output: search_results
```

**Returns:** `{"results": [{"id": str, "text": str, "score": float, "metadata": dict}], "query": str, "collection": str, "k": int}`

**Filter operators:** `field` (exact), `field_gte`, `field_lte`, `field_gt`, `field_lt`, `field_ne`, `field_in`

---

## LlamaIndex RAG Bridge

Advanced RAG patterns via LlamaIndex integration. Provides router queries, sub-question decomposition, and agentic retrieval without implementing custom retrieval logic.

**Required:** `pip install the-edge-agent[llamaindex]` or `pip install llama-index>=0.10.0 llama-index-core>=0.10.0`

**Fallback:** When LlamaIndex is not available, `rag.llamaindex.query` falls back to native `vector.query`. Other actions return an error with `success: false`.

### Settings Configuration

```yaml
settings:
  llamaindex:
    index_path: "./vector_store"              # Default index path
    embedding_model: text-embedding-3-small   # OpenAI embedding model
    embedding_base_url: null                  # Custom API URL (for local/compatible)
    llm_model: gpt-4o-mini                    # LLM for routing/synthesis
    llm_base_url: null                        # Custom LLM API URL
    api_key: "${OPENAI_API_KEY}"              # API key (env var)
```

### `rag.llamaindex.query`

Execute simple vector query against LlamaIndex index:

```yaml
- name: retrieve_context
  uses: rag.llamaindex.query
  with:
    query: "{{ state.question }}"             # Required
    index_path: "./vector_store"              # Optional (uses settings default)
    similarity_top_k: 5                       # Optional (default: 5)
    response_mode: compact                    # Optional: compact, refine, tree_summarize
  output: rag_result
```

**Returns:**
```python
{
    "response": "Synthesized answer from context...",
    "nodes": [
        {"id": str, "text": str, "metadata": dict}
    ],
    "scores": [0.95, 0.87, ...],
    "success": True
}
```

### `rag.llamaindex.router`

Router Query Engine for dynamic retrieval strategy selection:

```yaml
- name: smart_retrieval
  uses: rag.llamaindex.router
  with:
    query: "{{ state.question }}"             # Required
    engines:                                   # Required: list of engines
      - type: vector                          # Engine type
        index_path: "./docs_index"            # Path for vector/keyword
        description: "Semantic search over documentation"
        name: docs_engine                     # Optional identifier
      - type: keyword
        index_path: "./logs_index"
        description: "Exact keyword matching in logs"
      - type: sql
        connection: "sqlite:///sales.db"      # Connection string for SQL
        description: "Structured data queries"
    verbose: false                            # Optional: enable logging
  output: router_result
```

**Engine Types:**
- `vector`: Semantic similarity search using embeddings
- `keyword`: BM25-style keyword matching
- `sql`: Natural language to SQL queries (requires SQLAlchemy)

**Returns:**
```python
{
    "response": "Answer based on selected engine...",
    "selected_engine": {"name": "docs_engine", "reason": "..."},
    "nodes": [...],
    "scores": [...],
    "success": True
}
```

### `rag.llamaindex.subquestion`

Sub-Question Query Engine for complex query decomposition:

```yaml
- name: complex_query
  uses: rag.llamaindex.subquestion
  with:
    query: "Compare the revenue growth of Apple and Google from 2020 to 2024"
    engines:                                   # List of query engines
      - type: vector
        index_path: "./financials"
        description: "Financial data and reports"
    parallel: true                            # Optional: parallel sub-question execution
    synthesis_prompt: "Synthesize a comparison"  # Optional: custom synthesis
    verbose: false
  output: subq_result
```

**Simpler form with single index:**

```yaml
- name: complex_query
  uses: rag.llamaindex.subquestion
  with:
    query: "What are the pros and cons of Python vs Rust?"
    index_path: "./programming_docs"          # Single index shorthand
    parallel: true
  output: analysis
```

**Returns:**
```python
{
    "response": "Comprehensive comparison...",
    "sub_questions": [
        "What was Apple revenue in 2020?",
        "What was Apple revenue in 2024?",
        "What was Google revenue in 2020?",
        "What was Google revenue in 2024?"
    ],
    "sub_answers": [
        "Apple revenue in 2020 was...",
        "..."
    ],
    "success": True
}
```

### `rag.llamaindex.create_index`

Create a new LlamaIndex vector index:

```yaml
# From documents
- name: create_from_docs
  uses: rag.llamaindex.create_index
  with:
    documents:                                # List of document dicts
      - text: "Document 1 content..."
        metadata: {type: article, date: "2024-01-15"}
      - text: "Document 2 content..."
        metadata: {type: blog}
    persist_path: "./my_index"                # Optional: persist to disk
  output: create_result

# From directory
- name: create_from_dir
  uses: rag.llamaindex.create_index
  with:
    directory: "./documents/"                 # Load all files from directory
    persist_path: "./my_index"
  output: create_result
```

**Returns:** `{"success": true, "persist_path": str, "document_count": int}`

### `rag.llamaindex.load_index`

Load a persisted index:

```yaml
- name: load_existing
  uses: rag.llamaindex.load_index
  with:
    index_path: "./my_index"                  # Required
    force_reload: false                       # Optional: bypass cache
  output: load_result
```

**Returns:** `{"success": true, "index_path": str, "cached": bool}`

### `rag.llamaindex.add_documents`

Add documents to an existing index:

```yaml
- name: add_new_docs
  uses: rag.llamaindex.add_documents
  with:
    documents:
      - text: "New document content"
        metadata: {added: "2024-06-01"}
    index_path: "./my_index"                  # Optional (uses settings default)
  output: add_result
```

**Returns:** `{"success": true, "added": int, "index_path": str}`

---

## Tools Bridge Actions

Access external tool ecosystems (CrewAI, MCP, LangChain).

**Dependencies (all optional):**
```bash
pip install crewai crewai-tools     # For CrewAI
pip install mcp                      # For MCP
pip install langchain langchain-community  # For LangChain
```

### `tools.crewai`

Execute CrewAI tools:

```yaml
- name: search_web
  uses: tools.crewai
  with:
    tool: SerperDevTool                   # Required
    query: "{{ state.search_query }}"     # Tool-specific params
    timeout: 30.0                         # Optional
  output: search_result
```

**Returns:** `{"result": any, "tool": str, "success": true}`

### `tools.mcp`

Execute MCP server tools:

```yaml
- name: read_file
  uses: tools.mcp
  with:
    server:                               # Required
      command: npx
      args: ["-y", "@anthropic/mcp-server-filesystem"]
    tool: read_file                       # Required
    path: "/tmp/data.txt"                 # Tool-specific params
  output: file_result
```

**Returns:** `{"result": any, "tool": str, "server": str, "success": true}`

### `tools.langchain`

Execute LangChain tools:

```yaml
- name: wiki_search
  uses: tools.langchain
  with:
    tool: WikipediaQueryRun               # Required
    query: "{{ state.query }}"            # Tool-specific params
  output: wiki_result
```

**Returns:** `{"result": any, "tool": str, "success": true}`

### `tools.discover`

Discover available tools:

```yaml
- name: list_tools
  uses: tools.discover
  with:
    source: all                           # "crewai", "mcp", "langchain", or "all"
    filter: search                        # Optional
  output: available_tools
```

**Returns:** `{"tools": list, "sources": list, "count": int, "success": true}`

---

## Notification Actions

### `actions.notify`

```yaml
- name: alert
  uses: actions.notify
  with:
    channel: slack                       # Required
    message: "Task completed!"           # Required
```

**Returns:** `{"sent": true}`

---

## Dual Namespace

All trace actions: `trace.*` and `actions.trace_*`.
All web actions: `web.*` and `actions.web_*`.
All RAG actions: `embedding.*`, `vector.*` and `actions.embedding_*`, `actions.vector_*`.
All tools bridge actions: `tools.*` and `actions.tools_*`.
All Opik actions: `opik.*` and `actions.opik_*`.

---

## See Also

- [Actions Overview](./README.md)
- [LLM Actions](./llm.md) - LLM integration
- [Memory Actions](./memory.md) - Vector storage
- [Specialized Actions](./specialized.md) - LlamaExtract and validation
