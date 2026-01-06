# RAG & Memory

> Build agents with persistent memory and semantic search - from session-scoped caching to cloud-synced long-term knowledge bases.

## Why This Matters

AI agents often need to remember context across conversations, retrieve relevant knowledge from large document collections, and persist important information between sessions. TEA provides a comprehensive memory and RAG (Retrieval-Augmented Generation) stack that works locally for development and scales to cloud-native deployments without code changes.

## Quick Example

```yaml
name: rag-memory-agent
version: "1.0"

settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: ":memory:"
    storage:
      uri: "./ltm_data/"
    inline_threshold: 1024

nodes:
  - id: remember
    action: memory.store
    key: "user_preference"
    value: "{{ state.preference }}"
    ttl: 3600  # expires in 1 hour

  - id: search
    action: vector.query
    query: "{{ state.question }}"
    k: 5
    collection: "knowledge_base"

  - id: answer
    action: llm.call
    model: gpt-4
    messages:
      - role: system
        content: |
          Use this context: {{ state.search.results | tojson }}
      - role: user
        content: "{{ state.question }}"

edges:
  - from: __start__
    to: remember
  - from: remember
    to: search
  - from: search
    to: answer
  - from: answer
    to: __end__
```

## Memory Types

| Type | Backend | Persistence | Use Case |
|------|---------|-------------|----------|
| **Short-Term** | In-Memory | Session only | Working memory, TTL-based caching |
| **Long-Term (Local)** | SQLite | Survives restarts | Local knowledge base, development |
| **Long-Term (Cloud)** | DuckDB + Catalog | Cloud-synced | Production, serverless, multi-agent |
| **Graph Memory** | CozoDB / Kuzu | Local or cloud | Entity relationships, reasoning |

## LTM Backends

| Backend | Use Case | Install |
|---------|----------|---------|
| **sqlite** (default) | Local development, single-node | Built-in |
| **duckdb** | Analytics-heavy, catalog-aware, cloud storage | `pip install duckdb fsspec` |
| **litestream** | SQLite with S3 replication | `pip install litestream` |
| **blob-sqlite** | Distributed with blob storage | `pip install fsspec` |

### Catalog Backends (for DuckDB LTM)

| Catalog | Use Case | Install |
|---------|----------|---------|
| **sqlite** (default) | Local, development | Built-in |
| **firestore** | Serverless, Firebase ecosystem | `pip install firebase-admin` |
| **postgres** | Self-hosted, SQL compatibility | `pip install psycopg2` |
| **supabase** | Edge, REST API, managed Postgres | `pip install requests` |

## Available Actions

### Memory Actions

| Action | Description |
|--------|-------------|
| `memory.store` | Store key-value pairs with optional TTL (session-scoped) |
| `memory.retrieve` | Retrieve values by key with fallback default |
| `memory.summarize` | Condense conversation history using LLM |

### Long-Term Memory Actions

| Action | Description |
|--------|-------------|
| `ltm.store` | Persist key-value pairs to durable storage |
| `ltm.retrieve` | Fetch values from long-term storage |
| `ltm.delete` | Remove entries from storage |
| `ltm.search` | Full-text search with FTS5 and metadata filtering |

### RAG Actions

| Action | Description |
|--------|-------------|
| `embedding.create` | Generate embeddings (OpenAI, Ollama, or custom) |
| `vector.store` | Store documents with embeddings and metadata |
| `vector.query` | Semantic similarity search with filtering |

### Graph Memory Actions

| Action | Description |
|--------|-------------|
| `graph.store_entity` | Store nodes with type, properties, and embeddings |
| `graph.store_relation` | Create edges between entities |
| `graph.query` | Datalog/Cypher queries for graph traversals |
| `graph.retrieve_context` | Get relevant subgraph for a query (N-hop expansion) |

## Key Features

| Feature | Description |
|---------|-------------|
| **Zero Dependencies** | In-memory backends work with pure Python |
| **Pluggable Providers** | OpenAI, Ollama, or custom embedding providers |
| **Serverless Ready** | DuckDB with Firestore/Supabase catalog for cloud functions |
| **Content Deduplication** | SHA-256 hashing prevents duplicate storage |
| **Small Data Inlining** | Entries <1KB stored directly in catalog (fast access) |
| **Cloud Storage** | Direct S3/GCS/Azure I/O via httpfs extension |

## Configuration Examples

### Local Development

```yaml
settings:
  ltm:
    backend: sqlite
    path: ./agent_memory.db
  rag:
    embedding_provider: ollama
    embedding_model: nomic-embed-text
    vector_store: memory
```

### Serverless Production (Firebase)

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
      project: my-tea-project
    storage:
      uri: "gs://my-bucket/agents/ltm/"
    inline_threshold: 1024
  rag:
    embedding_provider: openai
    embedding_model: text-embedding-3-small
```

### Self-Hosted Production (PostgreSQL)

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      connection_string: "${POSTGRES_URL}"
    storage:
      uri: "s3://my-bucket/agents/ltm/"
      s3_region: us-east-1
```

## Embedding Providers

### OpenAI (Remote or Local-Compatible API)

```yaml
settings:
  rag:
    embedding_provider: openai
    embedding_model: text-embedding-3-small  # 1536 dims
    # openai_base_url: http://localhost:8000/v1  # For local APIs
```

### Ollama (Local)

```yaml
settings:
  rag:
    embedding_provider: ollama
    embedding_model: nomic-embed-text  # 768 dims, 8K context
    ollama_base_url: http://localhost:11434
```

| Ollama Model | Dimensions | Context | Use Case |
|--------------|------------|---------|----------|
| `nomic-embed-text` | 768 | 8,192 tokens | Long documents, balanced |
| `mxbai-embed-large` | 1024 | 512 tokens | High accuracy |
| `all-minilm` | 384 | 256 tokens | Lightweight, fast |
| `bge-m3` | 1024 | 8,192 tokens | Highest retrieval accuracy |

## Examples

- [Knowledge Graph Agent](https://github.com/fabceolin/the_edge_agent/tree/main/examples/prolog/neurosymbolic) - Graph memory with Datalog reasoning
- [Conversational Agent](https://github.com/fabceolin/the_edge_agent/tree/main/examples) - Short-term memory with summarization
- [RAG Pipeline](https://github.com/fabceolin/the_edge_agent/tree/main/examples) - Embedding + vector search workflow

## Learn More

- [Memory Actions Reference](../python/actions-reference.md) - Full API documentation
- [LTM Backend Guide](https://github.com/fabceolin/the_edge_agent/blob/main/CLAUDE.md#ltm-backend-selection-guide) - Backend selection and configuration
- [Checkpoint Guide](../shared/architecture/checkpoint-guide.md) - Save/resume with memory state
- [TEA-BUILTIN-001.1: Memory Actions](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-001.1.memory-actions.md) - Implementation story
- [TEA-BUILTIN-001.4: Long-Term Memory](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-001.4.long-term-memory.md) - LTM architecture
- [TEA-BUILTIN-001.6: DuckDB LTM](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-001.6.duckdb-ltm-backend.md) - Cloud-native backend
- [TEA-BUILTIN-002.2: RAG Actions](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-002.2.rag-actions.md) - Embedding and vector search
