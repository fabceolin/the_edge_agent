# Memory Actions

> **Parent document:** [Actions Overview](./README.md)
> **Related:** [Node Specification](../nodes.md)
> **Epic:** DOC-002 (YAML Reference Modularization)

## Overview

Memory actions provide session storage, persistent long-term memory, caching, and cloud-native agent memory with vector search. Includes graph database integration for entity-relationship storage.

---

## Table of Contents

- [Session Memory Actions](#session-memory-actions)
  - [memory.store](#memorystore)
  - [memory.retrieve](#memoryretrieve)
  - [memory.summarize](#memorysummarize)
- [Mem0 Universal Memory Actions](#mem0-universal-memory-actions)
  - [memory.mem0.add](#memorymem0add)
  - [memory.mem0.search](#memorymem0search)
  - [memory.mem0.get_all](#memorymem0get_all)
  - [memory.mem0.get](#memorymem0get)
  - [memory.mem0.update](#memorymem0update)
  - [memory.mem0.delete](#memorymem0delete)
  - [memory.mem0.test](#memorymem0test)
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
- [Session Persistence Actions](#session-persistence-actions-tea-builtin-0151)
  - [session.load](#sessionload)
  - [session.save](#sessionsave)
  - [session.delete](#sessiondelete)
  - [session.exists](#sessionexists)
- [Catalog Actions](#catalog-actions)
  - [catalog.register_table](#catalogregister_table)
  - [catalog.create_snapshot](#catalogcreate_snapshot)
- [Firestore Actions (TEA-BUILTIN-015.2)](#firestore-actions-tea-builtin-0152)
  - [firestore.get](#firestoreget)
  - [firestore.set](#firestoreset)
  - [firestore.query](#firestorequery)
  - [firestore.delete](#firestoredelete)
  - [firestore.batch](#firestorebatch)
- [Graph Database Actions](#graph-database-actions)
  - [graph.store_entity](#graphstore_entity)
  - [graph.store_relation](#graphstore_relation)
  - [graph.query](#graphquery)
  - [graph.retrieve_context](#graphretrieve_context)
- [DuckPGQ Graph Actions (TEA-BUILTIN-001.8)](#duckpgq-graph-actions-tea-builtin-0018)
  - [graph.create](#graphcreate)
  - [graph.drop](#graphdrop)
  - [graph.algorithm](#graphalgorithm)
  - [graph.shortest_path](#graphshortest_path)
  - [graph.list_graphs](#graphlist_graphs)

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

## Mem0 Universal Memory Actions

> **Story:** TEA-AGENT-001.6 (Mem0 Memory)

Mem0 integration for universal memory management with automatic fact extraction, semantic search, and user/session/agent scoped memories. Optionally supports graph memory for entity-relationship extraction.

**Required dependencies:**
- `pip install mem0ai` or `pip install the-edge-agent[mem0]`

### Configuration

Configure Mem0 backend in YAML settings:

```yaml
settings:
  memory:
    backend: mem0                              # Enable Mem0 backend
    api_key: "${MEM0_API_KEY}"                 # Optional: Mem0 cloud API key
    endpoint: "${MEM0_ENDPOINT}"               # Optional: self-hosted endpoint
    user_id: "{{ state.user_id }}"             # Optional: default user scope
    session_id: "{{ state.session_id }}"       # Optional: default session scope
    agent_id: "agent_abc"                      # Optional: default agent scope
    graph: true                                # Optional: enable graph memory
```

**Cloud vs Local Mode:**
- With `api_key`: Uses Mem0 cloud service (recommended for production)
- Without `api_key`: Uses local Mem0 instance (requires additional setup)

### `memory.mem0.add`

Store conversation messages with automatic fact extraction:

```yaml
- name: remember_preferences
  uses: memory.mem0.add
  with:
    messages:                                   # Required
      - role: user
        content: "I prefer dark mode and coding in Python"
      - role: assistant
        content: "I'll remember your preferences!"
    user_id: "{{ state.user_id }}"             # Optional (at least one scope required)
    session_id: "{{ state.session_id }}"       # Optional
    agent_id: "{{ state.agent_id }}"           # Optional
    metadata:                                   # Optional
      source: "onboarding"
      priority: "high"
  output: memory_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `messages` | str/list/dict | Yes | - | Single message or conversation history |
| `user_id` | string | No* | - | User scope for memory |
| `session_id` | string | No* | - | Session scope for memory |
| `agent_id` | string | No* | - | Agent scope for memory |
| `metadata` | dict | No | - | Additional metadata to store |

\* At least one scope (user_id, session_id, or agent_id) is required.

**Returns:** `{"success": true, "memory_id": str, "memories": list}`

### `memory.mem0.search`

Semantic search over stored memories:

```yaml
- name: recall_preferences
  uses: memory.mem0.search
  with:
    query: "What are my coding preferences?"   # Required
    user_id: "{{ state.user_id }}"             # Optional
    session_id: "{{ state.session_id }}"       # Optional
    agent_id: "{{ state.agent_id }}"           # Optional
    limit: 5                                   # Optional (default: 5)
    include_relations: false                   # Optional (requires graph: true)
  output: search_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `query` | string | Yes | - | Semantic search query |
| `user_id` | string | No | - | Filter by user scope |
| `session_id` | string | No | - | Filter by session scope |
| `agent_id` | string | No | - | Filter by agent scope |
| `limit` | int | No | 5 | Maximum results to return |
| `include_relations` | bool | No | false | Include graph relations (graph mode) |

**Returns:**
```text
{
  "success": true,
  "results": [
    {"id": "mem_abc123", "memory": "User prefers dark mode", "score": 0.95, "metadata": {...}},
    {"id": "mem_def456", "memory": "User codes in Python", "score": 0.89, "metadata": {...}}
  ],
  "relations": []
}
```

### `memory.mem0.get_all`

Retrieve all memories for a scope with pagination:

```yaml
- name: list_user_memories
  uses: memory.mem0.get_all
  with:
    user_id: "{{ state.user_id }}"             # Optional
    session_id: "{{ state.session_id }}"       # Optional
    agent_id: "{{ state.agent_id }}"           # Optional
    limit: 20                                  # Optional
    offset: 0                                  # Optional (for pagination)
  output: memories_result
```

**Returns:** `{"success": true, "memories": list, "total": int}`

### `memory.mem0.get`

Retrieve a specific memory by ID:

```yaml
- name: get_specific_memory
  uses: memory.mem0.get
  with:
    memory_id: "{{ state.memory_id }}"         # Required
  output: memory_result
```

**Returns:** `{"success": true, "memory": {"id": str, "memory": str, "metadata": dict}}`

### `memory.mem0.update`

Update an existing memory's content or metadata:

```yaml
- name: update_memory
  uses: memory.mem0.update
  with:
    memory_id: "{{ state.memory_id }}"         # Required
    text: "Updated preference: VS Code"        # Optional
    metadata:                                  # Optional
      verified: true
      updated_at: "{{ state.timestamp }}"
  output: update_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `memory_id` | string | Yes | - | Memory ID to update |
| `text` | string | No* | - | New memory content |
| `metadata` | dict | No* | - | Metadata to merge with existing |

\* At least one of `text` or `metadata` is required.

**Returns:** `{"success": true, "memory": {...}}`

### `memory.mem0.delete`

Delete memories by ID or bulk delete by scope:

```yaml
# Delete single memory
- name: delete_memory
  uses: memory.mem0.delete
  with:
    memory_id: "{{ state.memory_id }}"         # Required for single delete
  output: delete_result

# Bulk delete all user memories
- name: delete_user_memories
  uses: memory.mem0.delete
  with:
    user_id: "{{ state.user_id }}"             # Scope for bulk delete
    delete_all: true                           # Required for bulk delete (safety flag)
  output: bulk_delete_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `memory_id` | string | No* | - | Delete specific memory by ID |
| `user_id` | string | No | - | Scope for bulk delete |
| `session_id` | string | No | - | Scope for bulk delete |
| `agent_id` | string | No | - | Scope for bulk delete |
| `delete_all` | bool | No | false | Safety flag for bulk delete |

\* Either `memory_id` or (`scope` + `delete_all: true`) is required.

**Returns:** `{"success": true, "deleted_count": int}`

### `memory.mem0.test`

Test Mem0 connection and configuration:

```yaml
- name: check_mem0
  uses: memory.mem0.test
  output: test_result
```

**Returns:** `{"success": true, "message": "Mem0 connection successful"}` or error details.

### Graceful Fallback

When Mem0 is unavailable (not installed or connection error), actions gracefully fall back:
- `memory.mem0.add`: Falls back to `memory.store` with serialized messages
- `memory.mem0.search`: Returns error (semantic search requires Mem0)
- `memory.mem0.get_all`: Falls back to `memory.retrieve` where possible

All fallback responses include `"fallback": true` in the result.

### Graph Memory

Enable graph memory for entity-relationship extraction by setting `graph: true` in settings:

```yaml
settings:
  memory:
    backend: mem0
    graph: true                                # Enable Mem0g
    user_id: "{{ state.user_id }}"

nodes:
  - name: store_relationship
    uses: memory.mem0.add
    with:
      messages:
        - role: user
          content: "Alice works at Acme Corp and reports to Bob"
      user_id: "{{ state.user_id }}"
    # Mem0 automatically extracts: Alice -> works_at -> Acme Corp
    #                              Alice -> reports_to -> Bob

  - name: query_relationships
    uses: memory.mem0.search
    with:
      query: "Who does Alice report to?"
      user_id: "{{ state.user_id }}"
      include_relations: true                  # Returns graph relations
    output: relationship_result
```

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
| `ducklake` | Alias for DuckDB with sensible defaults (TEA-LTM-010) | Quick setup, DuckDB with catalog |
| `litestream` | SQLite with S3 replication | Disaster recovery, edge sync |
| `blob-sqlite` | SQLite on blob storage | Distributed, multi-node |

### Ducklake Backend (TEA-LTM-010)

The `ducklake` backend is an alias that expands to DuckDB with sensible defaults, providing a simplified configuration for DuckDB storage with pluggable catalog backends.

**Defaults applied when using `backend: ducklake`:**
- `catalog.type: sqlite` (default catalog)
- `catalog.path: ./ltm_catalog.db`
- `storage.uri: ./ltm_data/`
- `lazy: true`
- `inline_threshold: 4096`

**Minimal configuration:**
```yaml
settings:
  ltm:
    backend: ducklake    # Expands to duckdb with all defaults
```

**With custom catalog:**
```yaml
settings:
  ltm:
    backend: ducklake
    catalog:
      type: duckdb
      shared: true       # Single file for storage and catalog
    storage:
      uri: ./my_data/
```

**With cloud storage:**
```yaml
settings:
  ltm:
    backend: ducklake
    catalog:
      type: firestore
      project_id: my-project
    storage:
      uri: gs://my-bucket/ltm/
```

All catalog types are supported: `sqlite` (default), `duckdb`, `firestore`, `postgres`, `supabase`.

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

### Agent-Prefixed Cache Keys

Cache keys are automatically prefixed with the agent name to enable agent-specific cache management:

```
cache:{agent_name}:{user_key}
```

**Agent name resolution (in priority order):**
1. `settings.name` from YAML config
2. Top-level `name` field from YAML
3. YAML filename (without extension)
4. Fallback to `unknown_agent`

This enables targeted cache invalidation without affecting other agents:

```yaml
# Invalidate only file_extraction_agent cache
- name: clear_agent_cache
  uses: cache.invalidate
  with:
    pattern: "cache:file_extraction_agent:*"
```

Use `key_prefix: false` to disable automatic prefixing for shared caches.

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
  # Final key: cache:my_agent:llm.call:abc123...

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

# Shared cache without agent prefix
- name: shared_translate
  uses: cache.wrap
  with:
    action: llm.call
    key: "shared:translate:{{ state.text | sha256 }}"
    key_prefix: false                      # Disable agent prefix
    args:
      model: gpt-4o
      messages:
        - role: user
          content: "Translate: {{ state.text }}"
  output: shared_result
  # Final key: cache:shared:translate:xyz789 (no agent prefix)
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `action` | string | Yes | - | Action to wrap (e.g., `llm.call`) |
| `args` | dict | Yes | - | Arguments to pass to wrapped action |
| `key` | string | No | - | Custom cache key or Jinja expression |
| `key_strategy` | string | No | `args` | One of: `sha256`, `args`, `custom`, `file_content` |
| `key_source` | string | No | - | Argument name for `file_content`/`sha256` strategy |
| `key_prefix` | bool | No | true | Auto-prefix keys with agent name |
| `ttl_days` | int | No | 60 | Cache TTL in days |
| `ttl_hours` | int | No | - | TTL in hours (overrides `ttl_days`) |
| `ttl_seconds` | int | No | - | TTL in seconds (overrides `ttl_hours`) |
| `skip_cache` | bool | No | false | Bypass cache lookup, force fresh execution |
| `cleanup_probability` | float | No | 0.05 | Probability of cleanup after cache miss |
| `cleanup_limit` | int | No | 5 | Max expired entries to delete per cleanup |

**Returns:**
```text
{
  "success": true,
  "result": {...},
  "_cache_hit": true,
  "_cache_key": "cache:my_agent:llm.call:abc123...",
  "_cache_created_at": "2025-01-15T10:30:00+00:00"
}
```

### `cache.get`

Retrieve cached value without executing action:

```yaml
- name: check_cache
  uses: cache.get
  with:
    key: "cache:my_agent:llm.call:abc123..."  # Full cache key with agent prefix
    include_metadata: true                     # Include cache metadata
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
    key: "cache:my_agent:llm.call:abc123..."
  output: invalidate_result

# Invalidate all cache for a specific agent
- name: clear_agent_cache
  uses: cache.invalidate
  with:
    pattern: "cache:file_extraction_agent:*"
  output: agent_invalidate

# Invalidate by pattern (all agents)
- name: clear_all_llm_cache
  uses: cache.invalidate
  with:
    pattern: "cache:*:llm.call:*"
  output: bulk_invalidate

# Invalidate ALL cache entries
- name: clear_everything
  uses: cache.invalidate
  with:
    pattern: "cache:*"
  output: full_invalidate

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

## Session Persistence Actions (TEA-BUILTIN-015.1)

Session persistence provides stateful conversation support via YAML settings. Configure a session backend to automatically load and save session state across agent executions.

### Configuration

Configure session persistence in the `settings.session` block:

```yaml
settings:
  session:
    backend: memory              # Required: memory | firestore
    collection: "agent_sessions" # Firestore collection name
    auto_save: true              # Auto-save state after execution
    ttl: 3600                    # Session TTL in seconds (0 = never expires)
    persist_fields:              # Optional: specific fields to persist
      - conversation_history
      - user_context
```

**Settings Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `backend` | string | `"memory"` | Storage backend: `memory` (in-process) or `firestore` |
| `collection` | string | `"agent_sessions"` | Firestore collection name |
| `auto_save` | boolean | `false` | Automatically save state after each execution |
| `ttl` | integer | `0` | Session expiry in seconds (0 = never expires) |
| `persist_fields` | list | `null` | Specific state fields to persist (null = all) |

### State Injection

When `session_id` is present in the initial state, session data is automatically loaded and merged before execution:

```python
# First turn
events = graph.stream({"session_id": "conv_123", "message": "Hello"})

# Second turn (session data auto-loaded)
events = graph.stream({"session_id": "conv_123", "message": "Follow-up"})
```

Initial state values take precedence over session data.

### `session.load`

Explicitly load session data:

```yaml
- name: load_context
  uses: session.load
  with:
    session_id: "{{ state.session_id }}"  # Optional: uses state.session_id if omitted
    default: {}                           # Default if session not found
  output: session_data
```

**Returns:** Session data dictionary or default value.

### `session.save`

Explicitly save current state:

```yaml
- name: save_progress
  uses: session.save
  with:
    session_id: "{{ state.session_id }}"  # Optional: uses state.session_id if omitted
    fields:                               # Optional: specific fields to save
      - conversation_history
      - last_question
    ttl: 7200                             # Optional: override TTL for this save
  output: save_result
```

**Returns:** `{"success": true, "session_id": str}` or `{"success": false, "error": str}`

### `session.delete`

Delete a session:

```yaml
- name: cleanup
  uses: session.delete
  with:
    session_id: "{{ state.session_id }}"
  output: delete_result
```

**Returns:** `{"success": true}`

### `session.exists`

Check if a session exists:

```yaml
- name: check_session
  uses: session.exists
  with:
    session_id: "{{ state.session_id }}"
  output: exists_result
```

**Returns:** `{"exists": true}` or `{"exists": false}`

### Complete Example

```yaml
name: stateful-conversation
state_schema:
  session_id: str
  messages: list
  turn_count: int

settings:
  session:
    backend: memory
    auto_save: true
    ttl: 3600
    persist_fields:
      - messages
      - turn_count

nodes:
  - name: process_message
    run: |
      messages = state.get("messages", [])
      messages.append({"role": "user", "content": state.get("input", "")})
      turn_count = state.get("turn_count", 0) + 1
      return {"messages": messages, "turn_count": turn_count}

edges:
  - from: __start__
    to: process_message
  - from: process_message
    to: __end__
```

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

## Firestore Actions (TEA-BUILTIN-015.2)

Direct Firestore document CRUD operations for YAML agents. Provides document-level access to Google Cloud Firestore without requiring custom Python code.

**Required:**
- `pip install firebase-admin`

**Environment Variables:**
| Variable | Description |
|----------|-------------|
| `FIREBASE_PROJECT_ID` | Google Cloud/Firebase project ID |
| `GOOGLE_APPLICATION_CREDENTIALS` | Path to service account JSON |
| `FIRESTORE_EMULATOR_HOST` | Emulator address (e.g., `localhost:8080`) |

### Configuration

Configure Firestore settings in `settings.firestore`:

```yaml
settings:
  firestore:
    project: "${FIREBASE_PROJECT_ID}"
    emulator_host: "${FIRESTORE_EMULATOR_HOST:-}"        # Optional, for local dev
    credentials_path: "${GOOGLE_APPLICATION_CREDENTIALS:-}"  # Optional
```

### `firestore.get`

Retrieve a document by ID:

```yaml
- name: get_user
  uses: firestore.get
  with:
    collection: "users"                    # Required
    document: "{{ state.user_id }}"        # Required
    default: {name: "Unknown", active: false}  # Optional fallback
  output: user_data
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection path (supports nested: `users/uid/posts`) |
| `document` | string | Yes | - | Document ID |
| `default` | any | No | `null` | Value if document doesn't exist |

**Returns:** `{"success": true, "data": dict, "exists": bool, "doc_id": str, "path": str}`

### `firestore.set`

Create or update a document:

```yaml
- name: save_result
  uses: firestore.set
  with:
    collection: "results"
    document: "{{ state.session_id }}"     # Optional (auto-generates UUID if omitted)
    data:                                  # Required
      answer: "{{ state.answer }}"
      timestamp: "{{ now() }}"
    merge: true                            # Optional (preserve existing fields)
  output: doc_ref
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection path |
| `document` | string | No | auto-gen | Document ID (UUID generated if omitted) |
| `data` | dict | Yes | - | Document data to write |
| `merge` | bool | No | `false` | If true, merge with existing doc |

**Returns:** `{"success": true, "doc_id": str, "path": str, "created": bool}`

### `firestore.query`

Query documents with filters:

```yaml
- name: get_history
  uses: firestore.query
  with:
    collection: "history"
    where:                                 # Optional filters
      - field: user_id
        op: "=="
        value: "{{ state.user_id }}"
      - field: created_at
        op: ">="
        value: "{{ state.since_date }}"
    order_by: "-created_at"                # Optional ("-" for descending)
    limit: 10                              # Optional (default: 100)
    offset: 0                              # Optional
  output: history_items
```

**Where Operators:**
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Array: `in`, `not-in`, `array-contains`, `array-contains-any`

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `collection` | string | Yes | - | Collection path |
| `where` | list | No | - | Filter conditions |
| `order_by` | string/list | No | - | Field(s) to sort by |
| `limit` | int | No | 100 | Max documents to return |
| `offset` | int | No | 0 | Documents to skip |

**Returns:** `{"success": true, "documents": list, "count": int}`

### `firestore.delete`

Delete a document:

```yaml
- name: cleanup
  uses: firestore.delete
  with:
    collection: "temp"
    document: "{{ state.temp_id }}"
  output: delete_result
```

**Returns:** `{"success": true, "doc_id": str, "path": str, "deleted": true}`

### `firestore.batch`

Execute multiple operations atomically:

```yaml
- name: batch_update
  uses: firestore.batch
  with:
    operations:
      - type: set
        collection: "users"
        document: "{{ state.user_id }}"
        data: {last_active: "{{ now() }}"}
        merge: true
      - type: delete
        collection: "temp"
        document: "{{ state.temp_id }}"
  output: batch_result
```

**Operation Types:**
- `set`: Create or update document (supports `merge: true`)
- `delete`: Delete document

**Returns:** `{"success": true, "count": int, "results": list}`

### Error Codes

All Firestore actions return structured errors on failure:

```yaml
# Error response structure
{
  "success": false,
  "error": {
    "code": "NOT_FOUND",           # Error code
    "message": "Document not found",
    "collection": "users",         # Context if applicable
    "document": "missing_id"
  }
}
```

| Code | Description |
|------|-------------|
| `NOT_FOUND` | Document does not exist |
| `PERMISSION_DENIED` | Insufficient permissions |
| `INVALID_ARGUMENT` | Invalid parameter value |
| `ALREADY_EXISTS` | Document already exists (when expected not to) |
| `ABORTED` | Operation aborted (transaction contention) |
| `UNAVAILABLE` | Service unavailable |
| `INTERNAL` | Internal error |
| `IMPORT_ERROR` | firebase-admin not installed |

### Nested Collections

Access subcollections using path notation:

```yaml
- name: get_post
  uses: firestore.get
  with:
    collection: "users/{{ state.user_id }}/posts"
    document: "{{ state.post_id }}"
  output: post_data

# Or provide full path in collection
- name: get_comment
  uses: firestore.get
  with:
    collection: "users/{{ state.user_id }}/posts/{{ state.post_id }}/comments/{{ state.comment_id }}"
  output: comment_data
```

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

## DuckPGQ Graph Actions (TEA-BUILTIN-001.8)

SQL/PGQ graph queries via the DuckDB DuckPGQ extension. Provides property graphs, pattern matching, path finding, and graph algorithms (PageRank, clustering, connected components) using standard SQL/PGQ syntax (ISO SQL:2023).

> **Story:** TEA-BUILTIN-001.8 (DuckPGQ Graph Queries)

**Required:**
- DuckDB with DuckPGQ extension (auto-installs from community)
- `pip install duckdb` - For DuckDB

**Optional cloud storage:**
- `pip install fsspec s3fs` - For S3 URIs
- `pip install fsspec gcsfs` - For GCS URIs
- `pip install fsspec adlfs` - For Azure URIs

### Configuration

Configure DuckPGQ via DuckDB settings:

```yaml
settings:
  duckdb:
    extensions:
      - httpfs    # For cloud storage access
      - duckpgq   # SQL/PGQ graph queries (lazy-loaded on first use)
```

### `graph.create`

Create a property graph from vertex and edge tables (Parquet files):

```yaml
- name: setup_knowledge_graph
  uses: graph.create
  with:
    name: knowledge_graph                      # Required: graph name
    vertex_tables:                             # Required: vertex tables
      - name: entities
        source: "s3://bucket/graph/entities.parquet"
        key: id                                # Primary key column
      - name: documents
        source: "s3://bucket/graph/documents.parquet"
        key: doc_id
    edge_tables:                               # Required: edge tables
      - name: relations
        source: "s3://bucket/graph/relations.parquet"
        source_key: from_id                    # Foreign key to source vertex
        destination_key: to_id                 # Foreign key to destination vertex
        references: entities                   # Vertex table reference
  output: graph_result
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `name` | string | Yes | - | Property graph name |
| `vertex_tables` | list | Yes | - | Vertex table definitions |
| `edge_tables` | list | Yes | - | Edge table definitions |

**Vertex Table Definition:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Table name in graph |
| `source` | string | Yes | Parquet file path (local or cloud URI) |
| `key` | string | Yes | Primary key column |

**Edge Table Definition:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Table name in graph |
| `source` | string | Yes | Parquet file path (local or cloud URI) |
| `source_key` | string | Yes | Column referencing source vertex |
| `destination_key` | string | Yes | Column referencing destination vertex |
| `references` | string | Yes | Vertex table name |

**Returns:** `{"success": true, "graph": str, "vertex_tables": list, "edge_tables": list}`

### `graph.drop`

Drop a property graph:

```yaml
- name: cleanup_graph
  uses: graph.drop
  with:
    name: knowledge_graph                      # Required
    if_exists: true                            # Optional (default: true)
  output: drop_result
```

**Returns:** `{"success": true, "graph": str, "dropped": bool}`

### SQL/PGQ Query Support

The existing `graph.query` action supports SQL/PGQ via the `pgq` parameter:

```yaml
# Pattern matching with SQL/PGQ
- name: find_related_entities
  uses: graph.query
  with:
    pgq: |
      FROM GRAPH_TABLE (knowledge_graph
        MATCH (e:entities WHERE e.id = '{{ state.entity_id }}')
              -[r:relations]->(related:entities)
        COLUMNS (related.id, related.name, r.type, r.weight)
      )
      ORDER BY weight DESC
      LIMIT {{ state.limit | default(10) }}
  output: related_entities

# Shortest path query
- name: find_path
  uses: graph.query
  with:
    pgq: |
      FROM GRAPH_TABLE (knowledge_graph
        MATCH path = ANY SHORTEST
          (a:entities WHERE a.id = '{{ state.start_id }}')
          -[r:relations]->{1,5}
          (b:entities WHERE b.id = '{{ state.end_id }}')
        COLUMNS (path_length(path) as hops, vertices(path) as nodes)
      )
  output: path_result
```

**Returns:** `{"success": true, "results": list, "count": int, "query": str}`

### `graph.algorithm`

Run graph algorithms (PageRank, clustering, connected components):

```yaml
# PageRank for entity importance
- name: compute_pagerank
  uses: graph.algorithm
  with:
    algorithm: pagerank                        # Required
    graph: knowledge_graph                     # Required
    table: entities                            # Required: vertex table
    limit: 100                                 # Optional
  output: important_entities

# Find clusters
- name: find_clusters
  uses: graph.algorithm
  with:
    algorithm: weakly_connected_component
    graph: knowledge_graph
    table: entities
  output: clustered_entities

# Connectivity analysis
- name: analyze_connectivity
  uses: graph.algorithm
  with:
    algorithm: local_clustering_coefficient
    graph: knowledge_graph
    table: entities
    filter: "clustering > 0.5"                 # Optional WHERE filter
  output: connected_entities
```

**Supported Algorithms:**

| Algorithm | Description | Output Column |
|-----------|-------------|---------------|
| `pagerank` | PageRank centrality score | `importance` |
| `weakly_connected_component` | Cluster/component ID | `cluster` |
| `local_clustering_coefficient` | Node connectivity score | `clustering` |

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `algorithm` | string | Yes | - | Algorithm name |
| `graph` | string | Yes | - | Property graph name |
| `table` | string | Yes | - | Vertex table name |
| `limit` | int | No | - | Max results to return |
| `filter` | string | No | - | WHERE clause filter |

**Returns:** `{"success": true, "results": list, "count": int, "algorithm": str}`

### `graph.shortest_path`

Find shortest path between two entities:

```yaml
- name: find_connection
  uses: graph.shortest_path
  with:
    graph: knowledge_graph                     # Required
    from_entity: "{{ state.start_id }}"        # Required
    to_entity: "{{ state.end_id }}"            # Required
    max_hops: 5                                # Optional (default: 5)
    edge_table: relations                      # Optional
  output: path_result
```

**Returns:**
```json
{
  "success": true,
  "path_found": true,
  "hops": 3,
  "nodes": ["entity_a", "entity_b", "entity_c", "entity_d"],
  "edges": [
    {"from": "entity_a", "to": "entity_b", "type": "knows"},
    {"from": "entity_b", "to": "entity_c", "type": "works_with"},
    {"from": "entity_c", "to": "entity_d", "type": "reports_to"}
  ]
}
```

### `graph.list_graphs`

List created property graphs:

```yaml
- name: list_all_graphs
  uses: graph.list_graphs
  output: graphs_list
```

**Returns:** `{"success": true, "graphs": list, "count": int}`

### Complete Example: Knowledge Graph Analysis

```yaml
name: knowledge-graph-analysis
description: Analyze entity relationships using DuckPGQ

nodes:
  # Create property graph from cloud storage
  - name: setup_graph
    uses: graph.create
    with:
      name: company_graph
      vertex_tables:
        - name: people
          source: "s3://data/graph/people.parquet"
          key: person_id
        - name: departments
          source: "s3://data/graph/departments.parquet"
          key: dept_id
      edge_tables:
        - name: works_in
          source: "s3://data/graph/works_in.parquet"
          source_key: person_id
          destination_key: dept_id
          references: people
        - name: reports_to
          source: "s3://data/graph/reports_to.parquet"
          source_key: subordinate_id
          destination_key: manager_id
          references: people

  # Find team members
  - name: find_team
    uses: graph.query
    with:
      pgq: |
        FROM GRAPH_TABLE (company_graph
          MATCH (p:people)-[w:works_in]->(d:departments WHERE d.name = '{{ state.department }}')
          COLUMNS (p.person_id, p.name, p.role)
        )
    output: team_members

  # Compute influence scores
  - name: find_influencers
    uses: graph.algorithm
    with:
      algorithm: pagerank
      graph: company_graph
      table: people
      limit: 10
    output: top_influencers

  # Find reporting chain
  - name: find_chain
    uses: graph.shortest_path
    with:
      graph: company_graph
      from_entity: "{{ state.employee_id }}"
      to_entity: "{{ state.ceo_id }}"
      max_hops: 10
      edge_table: reports_to
    output: reporting_chain

edges:
  - from: __start__
    to: setup_graph
  - from: setup_graph
    to: [find_team, find_influencers, find_chain]
  - from: [find_team, find_influencers, find_chain]
    to: __end__
```

### Cloud Storage URIs

DuckPGQ supports loading Parquet files from cloud storage via httpfs:

| Cloud | URI Format | Authentication |
|-------|------------|----------------|
| S3 | `s3://bucket/path/file.parquet` | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` |
| GCS | `gs://bucket/path/file.parquet` | `GOOGLE_APPLICATION_CREDENTIALS` |
| Azure | `az://container/path/file.parquet` | `AZURE_STORAGE_ACCOUNT`, `AZURE_STORAGE_KEY` |

### Backend Selection

DuckPGQ is automatically used when:
- The `pgq` parameter is provided to `graph.query`
- The `graph.create`, `graph.drop`, `graph.algorithm`, `graph.shortest_path`, or `graph.list_graphs` actions are called

For other graph operations (`graph.store_entity`, `graph.store_relation`, `graph.retrieve_context`), the configured CozoDB or Kuzu backend is used.

---

## Dual Namespace

All memory actions use dual namespaces: `memory.*` and `actions.memory_*`.
All LTM actions: `ltm.*` and `actions.ltm_*`.
All cache actions: `cache.*` and `actions.cache_*`.
All graph actions: `graph.*` and `actions.graph_*`.
All Firestore actions: `firestore.*` and `actions.firestore_*`.

---

## See Also

- [Actions Overview](./README.md)
- [Data Processing](./data.md) - Tabular data actions
- [Integrations](./integrations.md) - RAG and vector search
