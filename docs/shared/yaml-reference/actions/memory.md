# Memory Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** [DOC-002](../../../stories/DOC-002-yaml-reference-modularization.md)

## Overview

Memory actions provide session storage, persistent long-term memory, caching, and cloud-native agent memory with vector search. Includes graph database integration for entity-relationship storage.

---

## Table of Contents

- [Session Memory Actions](#session-memory-actions)
  - [memory.store](#memorystore)
  - [memory.retrieve](#memoryretrieve)
  - [memory.summarize](#memorysummarize)
- [Long-Term Memory Configuration](#long-term-memory-configuration)
  - [Backend Types](#backend-types)
  - [Catalog Types](#catalog-types)
- [Long-Term Memory Actions](#long-term-memory-actions)
  - [ltm.store](#ltmstore)
  - [ltm.retrieve](#ltmretrieve)
  - [ltm.delete](#ltmdelete)
  - [ltm.search](#ltmsearch)
- [Cache and Memoization Actions](#cache-and-memoization-actions)
  - [cache.wrap](#cachewrap)
  - [cache.get](#cacheget)
  - [cache.invalidate](#cacheinvalidate)
- [Firebase Agent Memory Actions](#firebase-agent-memory-actions)
  - [memory.cloud_store](#memorycloud_store)
  - [memory.cloud_retrieve](#memorycloud_retrieve)
  - [memory.cloud_list](#memorycloud_list)
  - [memory.grep](#memorygrep)
  - [memory.sql_query](#memorysql_query)
  - [memory.embed](#memoryembed)
  - [memory.vector_search](#memoryvector_search)
- [Session Actions](#session-actions)
  - [session.create](#sessioncreate)
  - [session.end](#sessionend)
  - [session.restore](#sessionrestore)
- [Catalog Actions](#catalog-actions)
  - [catalog.register_table](#catalogregister_table)
  - [catalog.create_snapshot](#catalogcreate_snapshot)
- [Graph Database Actions](#graph-database-actions)
  - [graph.store_entity](#graphstore_entity)
  - [graph.store_relation](#graphstore_relation)
  - [graph.query](#graphquery)
  - [graph.retrieve_context](#graphretrieve_context)

---

## Session Memory Actions

Session memory for storing data across graph invocations within the same engine instance.

> **Warning: Global State**
>
> Memory actions use a **process-global** in-memory store:
>
> - All workflows in the same process share the same memory store
> - `memory.clear` wipes ALL stored data, affecting all concurrent workflows
> - Data does **not** persist across process restarts
> - Use key prefixes (e.g., `session_123:user_name`) to namespace data
>
> For workflow-scoped storage, use state variables. For persistent storage, use [Long-Term Memory Actions](#long-term-memory-actions).

### `memory.store`

Store key-value pair with optional TTL:

```yaml
- name: remember_user
  uses: memory.store
  with:
    key: "user_name"                      # Required
    value: "{{ state.name }}"             # Required
    ttl: 3600                             # Optional (seconds, null = no expiration)
    namespace: "session_123"              # Optional
  output: store_result
```

**Returns:** `{"stored": true, "key": str, "namespace": str}`

### `memory.retrieve`

Retrieve value from memory:

```yaml
- name: recall_user
  uses: memory.retrieve
  with:
    key: "user_name"                      # Required
    default: "Guest"                      # Optional
    namespace: "session_123"              # Optional
  output: retrieved_value
```

**Returns:** `{"value": any, "found": bool, "key": str}`

### `memory.summarize`

Summarize conversation history using LLM:

```yaml
- name: compress_history
  uses: memory.summarize
  with:
    messages_key: "conversation"          # Required (state key with messages)
    max_tokens: 1000                      # Optional
    model: "gpt-3.5-turbo"                # Optional
  output: summary_result
```

**Returns:** `{"summary": str, "original_count": int, "token_estimate": int, "success": true}`

---

## Long-Term Memory Configuration

Configure LTM backend in the `settings.ltm` section.

### Basic Configuration

```yaml
settings:
  ltm:
    backend: duckdb              # "sqlite" (default), "duckdb", "litestream", "blob-sqlite"
    catalog:
      type: sqlite               # "sqlite", "firestore", "postgres", "supabase"
      path: ":memory:"           # For sqlite catalog
    storage:
      uri: "${LTM_STORAGE:-./ltm_data/}"  # Cloud or local storage path
    inline_threshold: 1024       # Inline data < this size in catalog (bytes)
    lazy: true                   # Lazy initialization for serverless
    enable_fts: true             # Full-text search (default: true)
```

### Backend Types

| Backend | Description | Best For |
|---------|-------------|----------|
| `sqlite` | Local SQLite with FTS5 | Development, single-node |
| `duckdb` | DuckDB + catalog + cloud storage | Analytics, cloud storage, caching |
| `litestream` | SQLite with S3 replication | Disaster recovery, edge sync |
| `blob-sqlite` | SQLite on blob storage | Distributed, multi-node |

### Catalog Types

| Catalog | Description | Best For |
|---------|-------------|----------|
| `sqlite` | Local SQLite | Development, testing |
| `firestore` | Firebase Firestore | Serverless, Firebase ecosystem |
| `postgres` | PostgreSQL | Self-hosted, SQL compatibility |
| `supabase` | Supabase REST API | Edge, managed Postgres |

### Environment Variable Expansion

LTM configuration supports `${VAR}` and `${VAR:-default}` syntax:

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: "${CATALOG_TYPE:-sqlite}"
      path: "${CATALOG_PATH:-:memory:}"
    storage:
      uri: "${STORAGE_URI:-./ltm_data/}"
```

---

## Long-Term Memory Actions

Persistent storage using the configured LTM backend with full-text search.

### `ltm.store`

Store key-value pair persistently:

```yaml
- name: store_knowledge
  uses: ltm.store
  with:
    key: "user_profile"                   # Required
    value: "{{ state.profile_data }}"     # Required
    metadata:                             # Optional
      type: "profile"
      source: "onboarding"
  output: store_result
```

**Returns:** `{"success": true, "stored": true, "key": str, "created": bool}`

### `ltm.retrieve`

Retrieve value from persistent storage:

```yaml
- name: load_knowledge
  uses: ltm.retrieve
  with:
    key: "user_profile"                   # Required
    default: {}                           # Optional
  output: retrieved_value
```

**Returns:** `{"success": true, "value": any, "found": bool, "metadata": dict}`

### `ltm.delete`

Delete key from persistent storage:

```yaml
- name: remove_data
  uses: ltm.delete
  with:
    key: "deprecated_key"                 # Required
  output: delete_result
```

**Returns:** `{"success": true, "deleted": bool, "key": str}`

### `ltm.search`

Full-text search across stored values:

```yaml
- name: search_knowledge
  uses: ltm.search
  with:
    query: "coding preferences"           # Required
    limit: 10                             # Optional (default: 10)
    metadata_filter:                      # Optional
      type: "profile"
  output: search_results
```

**Returns:** `{"success": true, "results": [{"key": str, "value": any, "metadata": dict, "score": float}], "count": int}`

---

## Cache and Memoization Actions

Automatic caching of action results in Long-Term Memory with configurable TTL.

**Required dependencies:**
- LTM backend configured (SQLite by default)
- `pip install fsspec` - For remote file hashing

### `cache.wrap`

Wrap any action with automatic caching:

```yaml
# Cache LLM call by arguments hash
- name: translate_cached
  uses: cache.wrap
  with:
    action: llm.call
    key_strategy: args                     # Hash all arguments
    ttl_days: 30                           # Cache for 30 days
    args:
      model: gpt-4o
      messages:
        - role: user
          content: "Translate to Spanish: {{ state.text }}"
  output: translation_result

# Cache document extraction by file content hash
- name: extract_cached
  uses: cache.wrap
  with:
    action: llamaextract.extract
    key_strategy: file_content             # Hash file content
    key_source: file                       # Argument containing file path
    ttl_days: 60
    args:
      file: "{{ state.file_path }}"
      agent_name: "{{ state.agent_name }}"
  output: extraction_result

# Custom cache key with Jinja expression
- name: search_cached
  uses: cache.wrap
  with:
    action: web.search
    key: "search:{{ state.query | lower | sha256 }}"
    ttl_hours: 24
    args:
      query: "{{ state.query }}"
  output: search_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `action` | string | Yes | - | Action to wrap (e.g., `llm.call`) |
| `args` | dict | Yes | - | Arguments to pass to wrapped action |
| `key` | string | No | - | Custom cache key or Jinja expression |
| `key_strategy` | string | No | `args` | One of: `sha256`, `args`, `custom`, `file_content` |
| `key_source` | string | No | - | Argument name for `file_content`/`sha256` strategy |
| `ttl_days` | int | No | 60 | Cache TTL in days |
| `ttl_hours` | int | No | - | TTL in hours (overrides `ttl_days`) |
| `ttl_seconds` | int | No | - | TTL in seconds (overrides `ttl_hours`) |
| `skip_cache` | bool | No | false | Bypass cache lookup, force fresh execution |
| `cleanup_probability` | float | No | 0.05 | Probability of cleanup after cache miss |
| `cleanup_limit` | int | No | 5 | Max expired entries to delete per cleanup |

**Returns:**
```json
{
  "success": true,
  "result": {...},
  "_cache_hit": true,
  "_cache_key": "cache:llm.call:abc123...",
  "_cache_created_at": "2025-01-15T10:30:00+00:00"
}
```

### `cache.get`

Retrieve cached value without executing action:

```yaml
- name: check_cache
  uses: cache.get
  with:
    key: "cache:llm.call:abc123..."        # Cache key to retrieve
    include_metadata: true                  # Include cache metadata
  output: cache_entry
```

**Returns:** `{"success": true, "found": bool, "value": any, "expired": bool, "metadata": dict}`

### `cache.invalidate`

Delete cached entries by key or pattern:

```yaml
# Invalidate by exact key
- name: clear_entry
  uses: cache.invalidate
  with:
    key: "cache:llm.call:abc123..."
  output: invalidate_result

# Invalidate by pattern
- name: clear_all_llm_cache
  uses: cache.invalidate
  with:
    pattern: "cache:llm.call:*"
  output: bulk_invalidate

# Invalidate by metadata filter
- name: clear_extraction_cache
  uses: cache.invalidate
  with:
    metadata_filter:
      _cache_action: llamaextract.extract
  output: filtered_invalidate
```

**Returns:** `{"success": true, "deleted_count": int, "deleted_keys": list}`

---

## Firebase Agent Memory Actions

Cloud-native agent memory layer with DuckDB search and vector similarity.

**Required dependencies:**
- `pip install firebase-admin` - For Firestore/GCS backends
- `pip install duckdb` - For query engine and vector search
- `pip install sqlglot tiktoken` - For SQL validation and token counting

Or install with: `pip install the-edge-agent[firebase]`

### `memory.cloud_store`

Store content to cloud storage with metadata:

```yaml
- name: store_document
  uses: memory.cloud_store
  with:
    path: "firms/acme/profile.yaml"        # Required
    content: "{{ state.yaml_content }}"     # Required
    metadata:                               # Optional
      status: "active"
      summary: "Company profile"
    skip_embedding: false                   # Optional (default: false)
  output: store_result
```

**Returns:** `{"success": true, "storage_uri": str, "content_hash": "sha256:...", "doc_id": str}`

### `memory.cloud_retrieve`

Retrieve content from cloud storage:

```yaml
- name: load_document
  uses: memory.cloud_retrieve
  with:
    path: "firms/acme/profile.yaml"        # Required
  output: retrieved_doc
```

**Returns:** `{"success": true, "content": str, "metadata": dict}`

### `memory.cloud_list`

List files with filtering:

```yaml
- name: list_firm_docs
  uses: memory.cloud_list
  with:
    prefix: "firms/"                        # Optional
    limit: 100                              # Optional (default: 100)
  output: file_list
```

**Returns:** `{"success": true, "files": list, "count": int}`

### `memory.grep`

Deterministic text search across memory files:

```yaml
- name: search_todos
  uses: memory.grep
  with:
    pattern: "TODO"                         # Required
    path_filter: "*.yaml"                   # Optional
    case_sensitive: false                   # Optional (default: true)
  output: grep_results
```

**Returns:** `{"success": true, "results": list, "count": int}`

### `memory.sql_query`

SQL query with safety controls (SELECT only):

```yaml
- name: query_active_docs
  uses: memory.sql_query
  with:
    query: "SELECT file_path, summary FROM agent_memory WHERE status = 'active' LIMIT 10"
    path_filter: "firms/*"                  # Optional
  output: query_results
```

**Returns:** `{"success": true, "results": list, "count": int, "columns": list}`

**Security:** Only SELECT queries allowed. Dangerous functions (read_csv, etc.) blocked.

### `memory.embed`

Generate embedding for content:

```yaml
- name: embed_content
  uses: memory.embed
  with:
    content: "{{ state.document_text }}"    # Required
    model: "text-embedding-3-small"         # Optional (default)
  output: embedding_result
```

**Returns:** `{"success": true, "embedding": list, "model": str, "dimensions": 1536}`

### `memory.vector_search`

Semantic similarity search:

```yaml
- name: semantic_search
  uses: memory.vector_search
  with:
    query: "legal contract analysis"        # Required
    top_k: 10                               # Optional (default: 10)
    threshold: 0.7                          # Optional (default: 0.0)
    content_type: "yaml"                    # Optional filter
  output: search_results
```

**Returns:** `{"success": true, "results": [{"id": str, "score": float, "content": str, "metadata": dict}], "count": int}`

---

## Session Actions

### `session.create`

Create session with TTL:

```yaml
- name: start_session
  uses: session.create
  with:
    session_id: "interview-123"             # Required
    user_id: "user-456"                     # Required
    ttl_hours: 24                           # Optional (default: 24)
    metadata:                               # Optional
      type: "interview"
      firm: "acme"
  output: session_result
```

**Returns:** `{"success": true, "session_id": str, "expires_at": datetime}`

### `session.end`

End session and archive its memory:

```yaml
- name: end_session
  uses: session.end
  with:
    session_id: "interview-123"             # Required
  output: archive_result
```

**Returns:** `{"success": true, "archived": true, "archive_path": str}`

### `session.restore`

Restore archived session:

```yaml
- name: restore_session
  uses: session.restore
  with:
    session_id: "interview-123"             # Required
  output: restore_result
```

**Returns:** `{"success": true, "restored": true, "session_id": str}`

---

## Catalog Actions

### `catalog.register_table`

Register table in DuckLake catalog:

```yaml
- name: register_memory_table
  uses: catalog.register_table
  with:
    name: "agent_memory"                    # Required
    table_type: "memory"                    # Required (memory | tabular)
    source_prefix: "agent-memory/"          # Required
    schema:                                 # Required
      file_path: "VARCHAR"
      content: "VARCHAR"
      embedding: "FLOAT[1536]"
  output: table_result
```

**Returns:** `{"success": true, "table_id": str, "name": str}`

### `catalog.create_snapshot`

Create point-in-time snapshot:

```yaml
- name: snapshot_catalog
  uses: catalog.create_snapshot
  with:
    table: "agent_memory"                   # Required
  output: snapshot_result
```

**Returns:** `{"success": true, "snapshot_id": str, "created_at": datetime}`

---

## Graph Database Actions

Entity-relationship storage using CozoDB or Kuzu backends.

**Required (optional):**
- `pip install 'pycozo[embedded]'` - For CozoDB backend
- `pip install kuzu` - For Kuzu backend

### `graph.store_entity`

Store entity with properties:

```yaml
- name: store_user
  uses: graph.store_entity
  with:
    entity_id: "{{ state.user_id }}"      # Required
    entity_type: "User"                   # Required
    properties:                           # Optional
      name: "{{ state.user_name }}"
      role: "{{ state.user_role }}"
  output: entity_result
```

**Returns:** `{"success": true, "entity_id": str, "type": str, "created": bool}`

### `graph.store_relation`

Create relationship between entities:

```yaml
- name: create_ownership
  uses: graph.store_relation
  with:
    from_entity: "{{ state.user_id }}"    # Required
    to_entity: "{{ state.project_id }}"   # Required
    relation_type: "owns"                 # Required
    properties:                           # Optional
      since: "{{ state.created_date }}"
  output: relation_result
```

**Returns:** `{"success": true, "from": str, "to": str, "type": str}`

### `graph.query`

Execute graph queries:

```yaml
# Cypher query (Kuzu backend)
- name: find_projects
  uses: graph.query
  with:
    cypher: |
      MATCH (u:Entity {id: '{{ state.user_id }}'})
      -[r:owns]->(p:Entity)
      RETURN p.id, p.properties
  output: query_result

# Pattern query (works with both backends)
- name: find_users
  uses: graph.query
  with:
    pattern:
      entity_type: "User"
  output: pattern_result
```

**Returns:** `{"success": true, "results": list, "count": int, "query": str}`

### `graph.retrieve_context`

Retrieve contextual information for entity:

```yaml
- name: get_context
  uses: graph.retrieve_context
  with:
    entity_id: "{{ state.user_id }}"      # Required
    hops: 2                               # Optional (default: 2)
    limit: 20                             # Optional
  output: context_result
```

**Returns:** `{"success": true, "entities": list, "relations": list, "context_summary": str}`

---

## Dual Namespace

All memory actions use dual namespaces: `memory.*` and `actions.memory_*`.
All LTM actions: `ltm.*` and `actions.ltm_*`.
All cache actions: `cache.*` and `actions.cache_*`.
All graph actions: `graph.*` and `actions.graph_*`.

---

## See Also

- [Actions Overview](./README.md)
- [Data Processing](./data.md) - Tabular data actions
- [Integrations](./integrations.md) - RAG and vector search
