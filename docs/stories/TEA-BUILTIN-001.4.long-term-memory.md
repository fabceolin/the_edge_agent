# Story TEA-BUILTIN-001.4: Long-Term Memory Actions

## Status

**DONE** - All tasks complete including Bighorn (Kuzu) cloud-native graph backend extension

## Story

**As a** YAML agent developer,
**I want** long-term memory actions with SQLite for key-value storage and pluggable graph backends (CozoDB for local, Bighorn for cloud-native),
**so that** I can build agents with persistent knowledge that survives across sessions and can model complex entity relationships with both local performance (Datalog) and cloud-native storage (Cypher with direct S3/GCS I/O).

## Acceptance Criteria

1. `LongTermMemoryBackend` protocol defines interface separate from short-term `MemoryBackend`
2. `SQLiteBackend` implements persistent key-value storage with full CRUD operations and FTS5 search
3. `CozoBackend` implements graph-based storage for entities and relationships using Datalog
4. `ltm.store` action stores key-value pairs persistently with optional metadata
5. `ltm.retrieve` action retrieves values by key with optional default fallback
6. `ltm.delete` action removes entries from long-term storage
7. `ltm.search` action searches across stored values with FTS5 and metadata filtering
8. `graph.store_entity` action stores nodes with type, properties, and optional embeddings
9. `graph.store_relation` action creates edges between entities with relationship type
10. `graph.query` action performs Datalog queries for graph traversals and reasoning
11. `graph.retrieve_context` action returns relevant subgraph for a given query/embedding using HNSW
12. Backends are injectable via `YAMLEngine(ltm_backend=..., graph_backend=...)`
13. Both SQLite and CozoDB backends support thread-safety and proper resource cleanup
14. All actions follow existing `_setup_builtin_actions()` pattern with consistent error handling
15. Actions are accessible via `ltm.*`, `graph.*` and `actions.ltm_*`, `actions.graph_*` namespaces
16. Comprehensive unit tests cover all long-term memory operations
17. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md
18. Graceful degradation when CozoDB is not installed (graph.* actions return informative error)

### Bighorn Extension (Cloud-Native Graph)

19. `BighornBackend` implements graph storage with direct cloud blob I/O via `httpfs` extension
20. Bighorn supports reading graph data directly from S3 (`s3://`), GCS (`gs://`), and Azure (`az://`)
21. Bighorn supports writing graph data directly to S3 and GCS (Azure read-only due to protocol limitations)
22. Graph storage uses URI scheme consistent with TEA-BUILTIN-004.1:
    - `file:///path/to/graph.db` or `./graph.db` - Local filesystem
    - `s3://bucket/graph/` - AWS S3
    - `gs://bucket/graph/` - Google Cloud Storage (via HMAC)
    - `az://container/graph/` - Azure Blob (read-only)
23. Bighorn uses Cypher query language (complementary to CozoDB's Datalog)
24. Graceful degradation when Bighorn extension packages not installed
25. Both CozoDB and Bighorn can coexist - user selects based on deployment target (local vs serverless)

## Dependencies

**Blocked By**:
- TEA-BUILTIN-001.1 (Memory Actions) - establishes memory patterns
- TEA-BUILTIN-002.2 (RAG Actions) - for embedding integration in graph.store_entity

**Blocks**:
- Future agent memory consolidation features
- Knowledge graph reasoning agents

**Internal Dependencies**:
- `graph.store_entity` optionally uses `embedding.create` for semantic storage
- `graph.retrieve_context` optionally uses `embedding.create` for semantic search

## User Prerequisites

- [ ] **Required**: SQLite3 (included in Python stdlib)
- [ ] **Required for CozoDB (local graph)**: Install CozoDB: `pip install "pycozo[embedded]"`
- [ ] **Required for Bighorn (cloud graph)**: Install Bighorn: `pip install kuzu` (Bighorn package)
- [ ] **Optional for Bighorn S3**: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`
- [ ] **Optional for Bighorn GCS**: HMAC keys from GCS Interoperability settings
- [ ] **Optional**: `OPENAI_API_KEY` for embedding-enhanced graph storage

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           MEMORY ARCHITECTURE                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  SHORT-TERM MEMORY (001.1)          LONG-TERM MEMORY (001.4)               │
│  ─────────────────────────          ────────────────────────────           │
│                                                                             │
│  ┌─────────────────────┐           ┌────────────────────────────┐          │
│  │  InMemoryBackend    │           │    LongTermMemoryBackend   │          │
│  │  - Key-value store  │           │    (Protocol)              │          │
│  │  - TTL support      │           └─────────────┬──────────────┘          │
│  │  - Session-scoped   │                         │                         │
│  └─────────────────────┘           ┌─────────────┴─────────────┐           │
│                                    │                           │           │
│  Actions:                   ┌──────┴──────┐          ┌─────────┴────────┐  │
│  - memory.store             │SQLiteBackend│          │   CozoBackend    │  │
│  - memory.retrieve          │             │          │  (SQLite storage)│  │
│  - memory.summarize         │- Key-value  │          │                  │  │
│                             │- FTS5 search│          │- Entity nodes    │  │
│                             │- Metadata   │          │- Relations/edges │  │
│                             │- CRUD ops   │          │- Datalog queries │  │
│                             └─────────────┘          │- HNSW vectors    │  │
│                                                      │- Time travel     │  │
│                                                      └──────────────────┘  │
│                                                                             │
│  LTM Actions:                    Graph Actions:                            │
│  - ltm.store                     - graph.store_entity                      │
│  - ltm.retrieve                  - graph.store_relation                    │
│  - ltm.delete                    - graph.query                             │
│  - ltm.search                    - graph.retrieve_context                  │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Graph Backend Comparison: CozoDB vs Bighorn

| Aspect | CozoDB | Bighorn (KuzuDB Fork) |
|--------|--------|----------------------|
| **Project Status** | Active, stable | Community fork by Kineviz (Oct 2025) |
| **Query Language** | Datalog | Cypher |
| **Storage Backend** | SQLite (local) | Local + **Direct Cloud I/O** |
| **Cloud Read** | ❌ | ✅ S3, GCS, Azure |
| **Cloud Write** | ❌ | ✅ S3, GCS (Azure read-only) |
| **Serverless Compatible** | ❌ (needs filesystem) | ✅ (via httpfs) |
| **Vector Search** | Native HNSW | Native |
| **Time Travel** | Yes | No |
| **Binary Size** | ~15MB | ~50MB |
| **Best For** | Local/Container, reasoning | Cloud-native, serverless |

### When to Use CozoDB

1. **Local/Container deployments** - Maximum performance, no network latency
2. **Complex reasoning** - Datalog excels at recursive queries and logic
3. **Time travel** - Query historical states for debugging/auditing
4. **Small footprint** - 15MB binary, SQLite storage
5. **Edge computing** - Runs in restricted environments

### When to Use Bighorn

1. **Firebase Cloud Functions** - No local filesystem available
2. **AWS Lambda / Azure Functions** - Serverless compute
3. **Data Lake integration** - Read/write directly to S3/GCS Parquet files
4. **Existing Cypher knowledge** - Familiar query language
5. **Multi-region** - Store graph in central cloud location

### Architecture Decision

```
┌─────────────────────────────────────────────────────────────────┐
│                    DEPLOYMENT TARGET                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  LOCAL/CONTAINER                    SERVERLESS/CLOUD             │
│  ───────────────                    ────────────────             │
│                                                                  │
│  ┌─────────────────┐               ┌─────────────────┐          │
│  │    CozoDB       │               │    Bighorn      │          │
│  │                 │               │                 │          │
│  │ - Datalog       │               │ - Cypher        │          │
│  │ - SQLite local  │               │ - httpfs ext    │          │
│  │ - HNSW vectors  │               │ - S3/GCS direct │          │
│  │ - Time travel   │               │ - Parquet I/O   │          │
│  │ - 15MB binary   │               │ - 50MB binary   │          │
│  └─────────────────┘               └─────────────────┘          │
│                                                                  │
│  graph_backend="cozo"              graph_backend="bighorn"       │
│  graph_path="./graph.db"           graph_storage="s3://bucket/"  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Tasks / Subtasks

- [x] Task 1: Design LongTermMemory abstraction (AC: 1, 12) ✓
  - [x] Create `LongTermMemoryBackend` protocol with methods: `store()`, `retrieve()`, `delete()`, `search()`, `close()`
  - [x] Define serialization format for values (JSON with metadata)
  - [x] Add connection lifecycle management (open/close)
  - [x] Implement thread-safe access patterns
  - [x] Define consistent error return format: `{"success": False, "error": str, "error_type": str}`

- [x] Task 2: Implement SQLiteBackend for ltm.* (AC: 2, 13) ✓
  - [x] Create SQLite schema with FTS5:
    ```sql
    CREATE TABLE ltm_store (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL,
        metadata TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );
    CREATE VIRTUAL TABLE ltm_fts USING fts5(key, value, metadata, content='ltm_store');
    ```
  - [x] Implement connection pooling with thread-local connections
  - [x] Enable WAL mode for concurrent access
  - [x] Implement `store()`, `retrieve()`, `delete()`, `search()` methods
  - [x] Add path validation to prevent directory traversal attacks
  - [x] Handle concurrent writes safely

- [x] Task 3: Implement `ltm.*` actions (AC: 4, 5, 6, 7, 14, 15) ✓
  - [x] `ltm.store(state, key, value, metadata=None, **kwargs)` → `{"success": True, "stored": True, "key": str}`
  - [x] `ltm.retrieve(state, key, default=None, **kwargs)` → `{"success": True, "value": any, "found": bool, "metadata": dict}`
  - [x] `ltm.delete(state, key, **kwargs)` → `{"success": True, "deleted": bool, "key": str}`
  - [x] `ltm.search(state, query=None, metadata_filter=None, limit=10, **kwargs)` → `{"success": True, "results": list, "count": int}`
  - [x] Register with dual namespaces: `ltm.*` and `actions.ltm_*`
  - [x] Return error dict on failure: `{"success": False, "error": str}`

- [x] Task 4: Implement CozoBackend for graph.* (AC: 3, 13, 18) ✓
  - [x] Check for pycozo availability with graceful degradation
  - [x] Initialize CozoDB with SQLite storage engine
  - [x] Create entity relation schema:
    ```datalog
    :create entity {
        id: String =>
        type: String,
        properties: String,
        embedding: [Float]?,
        created_at: Float default now()
    }
    ```
  - [x] Create relation schema:
    ```datalog
    :create relation {
        from_id: String,
        to_id: String,
        rel_type: String =>
        properties: String?,
        created_at: Float default now()
    }
    ```
  - [x] Create HNSW vector index for semantic search
  - [x] Implement thread-safe connection management
  - [x] Add helper methods for Datalog query building

- [x] Task 5: Implement `graph.store_entity` action (AC: 8, 14, 15) ✓
  - [x] Define signature: `graph_store_entity(state, entity_id, entity_type, properties=None, text=None, embed=False, **kwargs)`
  - [x] If `embed=True` and `text` provided, generate embedding via `embedding.create`
  - [x] Store entity with Datalog `:put` operation
  - [x] Return `{"success": True, "entity_id": str, "type": str, "has_embedding": bool}`

- [x] Task 6: Implement `graph.store_relation` action (AC: 9, 14, 15) ✓
  - [x] Define signature: `graph_store_relation(state, from_entity, to_entity, relation_type, properties=None, **kwargs)`
  - [x] Validate both entities exist (optional, configurable)
  - [x] Store relation with Datalog `:put` operation
  - [x] Return `{"success": True, "from": str, "to": str, "type": str}`

- [x] Task 7: Implement `graph.query` action (AC: 10, 14, 15) ✓
  - [x] Define signature: `graph_query(state, datalog=None, pattern=None, params=None, limit=100, **kwargs)`
  - [x] Support raw Datalog queries via `datalog` parameter
  - [x] Support simplified pattern matching via `pattern` dict (translated to Datalog)
  - [x] Implement query timeout to prevent runaway traversals
  - [x] Return `{"success": True, "results": list, "count": int, "query": str}`

- [x] Task 8: Implement `graph.retrieve_context` action (AC: 11, 14, 15) ✓
  - [x] Define signature: `graph_retrieve_context(state, query=None, embedding=None, entity_id=None, hops=2, limit=20, **kwargs)`
  - [x] If `query` provided and embeddings enabled, convert to embedding for HNSW similarity search
  - [x] If `entity_id` provided, expand N-hop neighborhood via recursive Datalog
  - [x] Return subgraph as `{"success": True, "entities": list, "relations": list, "context_summary": str}`

- [x] Task 9: Add YAMLEngine integration (AC: 12, 18) ✓
  - [x] Add `ltm_backend` parameter to `YAMLEngine.__init__()`
  - [x] Add `graph_backend` parameter to `YAMLEngine.__init__()`
  - [x] Default to `SQLiteBackend(":memory:")` for ltm if not specified
  - [x] Default to `CozoBackend(temp_dir)` for graph if not specified (with graceful fallback)
  - [x] Add `close()` method for proper cleanup of both backends
  - [x] Add feature flags: `enable_ltm=True`, `enable_graph=True`

- [x] Task 10: Write tests (AC: 16) ✓
  - [x] Test SQLiteBackend CRUD operations
  - [x] Test SQLiteBackend FTS5 search
  - [x] Test SQLiteBackend thread safety
  - [x] Test CozoBackend entity storage
  - [x] Test CozoBackend relation creation
  - [x] Test CozoBackend Datalog queries
  - [x] Test CozoBackend HNSW vector search (mocked embeddings)
  - [x] Test graph.retrieve_context with N-hop expansion
  - [x] Test graceful degradation when pycozo not installed
  - [x] Test integration in YAML workflows
  - [x] Test backend injection and lifecycle

- [x] Task 11: Update documentation (AC: 17) ✓
  - [x] Add Long-Term Memory section to CLAUDE.md
  - [x] Add Graph Memory section to CLAUDE.md with Datalog examples
  - [x] Add examples in docs/YAML_AGENTS.md
  - [x] Create example YAML showing knowledge graph agent with reasoning

### Bighorn Extension Tasks (NEW)

- [x] **Task 12: Implement BighornBackend** (AC: 19, 20, 21, 24) ✓
  - [x] Create `BighornBackend` class implementing `GraphBackend` protocol
  - [x] Load `httpfs` extension dynamically with graceful fallback
  - [x] Implement URI scheme detection (`s3://`, `gs://`, `az://`, `file://`)
  - [x] Implement entity schema in Cypher (compatible with CozoDB schema)
  - [x] Implement relation schema in Cypher
  - [x] Handle extension not installed gracefully
  - [x] Unit tests with mocked httpfs

- [x] **Task 13: Implement cloud read operations** (AC: 20) ✓
  - [x] `LOAD FROM` with S3 URIs (Parquet, CSV)
  - [x] Configure S3 credentials (env vars, IAM roles)
  - [x] `LOAD FROM` with GCS URIs (HMAC mode)
  - [x] `LOAD FROM` with Azure URIs (read-only)
  - [x] Test globbing patterns (`s3://bucket/data/*.parquet`)
  - [x] Test projection pushdown for Parquet

- [x] **Task 14: Implement cloud write operations** (AC: 21) ✓
  - [x] `COPY TO` with S3 URIs (Multipart Upload)
  - [x] `COPY TO` with GCS URIs (via HMAC compatibility)
  - [x] Document Azure write limitations (requires AzCopy workaround)
  - [x] Test atomic writes and error recovery

- [x] **Task 15: Implement Cypher query translation** (AC: 23) ✓
  - [x] Map `graph.store_entity` to Cypher `CREATE` or `MERGE`
  - [x] Map `graph.store_relation` to Cypher relationship creation
  - [x] Map `graph.query` to raw Cypher execution
  - [x] Provide pattern-to-Cypher helper for simple queries
  - [x] Document Cypher vs Datalog differences for users

- [x] **Task 16: Add YAMLEngine Bighorn integration** (AC: 22, 25) ✓
  - [x] Add `graph_backend_type` parameter: `"cozo"`, `"kuzu"`, or `"bighorn"`
  - [x] Add cloud credentials configuration options
  - [x] Ensure both backends can coexist (user selects one)
  - [x] Auto-select backend if not specified

- [x] **Task 17: Write Bighorn tests** (AC: 19-25) ✓
  - [x] Test KuzuBackend with local file paths
  - [x] Test BighornBackend alias (KuzuBackend == BighornBackend)
  - [x] Test graceful degradation when Kuzu not installed
  - [x] Test Cypher query execution
  - [x] Test cloud read/write operations (with httpfs fallback)
  - [x] Integration test in YAML workflow (29 tests passing)

- [x] **Task 18: Update documentation for Bighorn** (AC: 17) ✓
  - [x] Add Kuzu/Bighorn section to CLAUDE.md
  - [x] Document CozoDB vs Kuzu backend selection
  - [x] Add Cypher query examples
  - [x] Document cloud storage configuration

## Dev Notes

### Short-Term vs Long-Term Memory Distinction

| Aspect | Short-Term (001.1) | Long-Term (001.4) |
|--------|-------------------|-------------------|
| **Backend** | InMemoryBackend | SQLiteBackend / CozoBackend |
| **Persistence** | Session only | Survives restarts |
| **TTL** | Supported | Not applicable (permanent) |
| **Data Model** | Key-value | Key-value (SQLite) + Graph (CozoDB) |
| **Query Language** | Key lookup | SQL/FTS5 (ltm) + Datalog (graph) |
| **Use Case** | Working memory, cache | Knowledge base, entity memory |
| **Actions** | `memory.*` | `ltm.*`, `graph.*` |

### CozoDB Integration Pattern

CozoDB is an embedded database combining:
- **Datalog** - Declarative logic language for powerful queries
- **SQLite storage** - Battle-tested persistence
- **HNSW vectors** - Native semantic search

```python
from pycozo import Client

# Initialize with SQLite storage
db = Client('sqlite', './agent_graph.db')

# Create entity schema
db.run("""
    :create entity {
        id: String =>
        type: String,
        properties: String,
        embedding: [Float]?,
        created_at: Float default now()
    }
""")

# Create relation schema
db.run("""
    :create relation {
        from_id: String,
        to_id: String,
        rel_type: String =>
        properties: String?,
        created_at: Float default now()
    }
""")

# Create HNSW index for vector search
db.run("""
    ::hnsw create entity:semantic_idx {
        dim: 1536,
        m: 50,
        ef_construction: 200,
        fields: [embedding],
        distance: Cosine
    }
""")

# Store entity
db.run("""
    ?[id, type, properties, embedding, created_at] <- [[$id, $type, $props, $emb, now()]]
    :put entity {id, type, properties, embedding, created_at}
""", {
    'id': 'person_1',
    'type': 'Person',
    'props': '{"name": "John", "age": 30}',
    'emb': [0.1, 0.2, ...]  # 1536-dim embedding
})

# Store relation
db.run("""
    ?[from_id, to_id, rel_type, properties, created_at] <- [[$from, $to, $rel, $props, now()]]
    :put relation {from_id, to_id, rel_type, properties, created_at}
""", {'from': 'person_1', 'to': 'person_2', 'rel': 'KNOWS', 'props': '{}'})

# Query: Find all people known by person_1 (1-hop)
result = db.run("""
    ?[id, type, properties] :=
        relation[from_id, to_id, rel_type, _, _],
        from_id = 'person_1',
        rel_type = 'KNOWS',
        entity[to_id, type, properties, _, _],
        id = to_id
""")

# Query: Recursive N-hop traversal
result = db.run("""
    connected[a, b] := relation[a, b, _, _, _]
    connected[a, c] := connected[a, b], relation[b, c, _, _, _]

    ?[connected_id, type, properties] :=
        connected['person_1', connected_id],
        entity[connected_id, type, properties, _, _]

    :limit 20
""")

# Semantic search with HNSW
result = db.run("""
    ?[id, type, properties, score] :=
        ~entity:semantic_idx {id, type, properties |
            query: $query_embedding,
            k: 10,
            ef: 50,
            bind_distance: score
        }
""", {'query_embedding': [0.1, 0.2, ...]})
```

### Bighorn Integration Pattern (Cloud-Native)

Bighorn is a fork of KuzuDB maintained by Kineviz with direct cloud storage I/O:

```python
# Note: Bighorn uses Cypher, not Datalog
# This is pseudocode based on KuzuDB API

import kuzu  # Bighorn package

# Initialize with cloud storage
db = kuzu.Database("s3://my-bucket/agent_graph/")

# Load httpfs extension for cloud access
conn = kuzu.Connection(db)
conn.execute("INSTALL httpfs; LOAD httpfs;")

# Configure S3 credentials
conn.execute("""
    CREATE SECRET s3_secret (
        TYPE s3,
        KEY_ID '{{ secrets.aws_access_key }}',
        SECRET '{{ secrets.aws_secret_key }}',
        REGION 'us-east-1'
    );
""")

# Create node table
conn.execute("""
    CREATE NODE TABLE Entity (
        id STRING PRIMARY KEY,
        type STRING,
        properties STRING,
        embedding DOUBLE[],
        created_at TIMESTAMP DEFAULT current_timestamp()
    )
""")

# Create relationship table
conn.execute("""
    CREATE REL TABLE RELATES_TO (
        FROM Entity TO Entity,
        rel_type STRING,
        properties STRING,
        created_at TIMESTAMP DEFAULT current_timestamp()
    )
""")

# Store entity (Cypher)
conn.execute("""
    CREATE (e:Entity {
        id: $id,
        type: $type,
        properties: $props
    })
""", {"id": "person_1", "type": "Person", "props": '{"name": "John"}'})

# Store relation (Cypher)
conn.execute("""
    MATCH (a:Entity {id: $from_id}), (b:Entity {id: $to_id})
    CREATE (a)-[:RELATES_TO {rel_type: $rel_type}]->(b)
""", {"from_id": "person_1", "to_id": "person_2", "rel_type": "KNOWS"})

# Query: N-hop traversal (Cypher)
result = conn.execute("""
    MATCH (start:Entity {id: $start_id})-[:RELATES_TO*1..2]-(connected:Entity)
    RETURN connected.id, connected.type, connected.properties
    LIMIT 20
""", {"start_id": "person_1"})

# Load data directly from S3 Parquet
conn.execute("""
    COPY Entity FROM 's3://my-bucket/data/entities/*.parquet'
""")

# Export graph to S3
conn.execute("""
    COPY (MATCH (e:Entity) RETURN e.*) TO 's3://my-bucket/exports/entities.parquet'
""")
```

### Bighorn Cloud Storage Support

| URI Scheme | Read | Write | Auth Method |
|------------|------|-------|-------------|
| `file://` | ✅ | ✅ | Filesystem |
| `s3://` | ✅ | ✅ | AWS credentials, IAM role |
| `gs://` | ✅ | ✅ | HMAC keys (S3 compatibility) |
| `az://` | ✅ | ❌ | Azure AD / Connection string |
| `http://` | ✅ | ❌ | None (public) |

### Bighorn Credential Configuration

```python
# S3 Credentials (multiple methods)
# 1. Environment variables
#    AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN

# 2. IAM Role (EC2, ECS, Lambda)
conn.execute("CREATE SECRET (TYPE s3, PROVIDER credential_chain);")

# 3. Explicit credentials
conn.execute("""
    CREATE SECRET s3_creds (
        TYPE s3,
        KEY_ID 'AKIA...',
        SECRET '...',
        REGION 'us-east-1'
    );
""")

# GCS Credentials (HMAC mode for S3 compatibility)
conn.execute("""
    CREATE SECRET gcs_creds (
        TYPE gcs,
        KEY_ID 'GOOG...',
        SECRET '...'
    );
""")

# Azure Credentials (read-only)
conn.execute("""
    CREATE SECRET azure_creds (
        TYPE azure,
        CONNECTION_STRING '...'
    );
""")
```

### Bighorn Performance Tips

1. **Use Parquet format** - Columnar storage with projection pushdown
2. **Partition data** - Use Hive-style partitioning (`/year=2024/month=01/`)
3. **File size** - Target 100MB-1GB per file (avoid small files)
4. **Caching** - Use `simplecache::s3://...` for repeated reads

### Bighorn vs CozoDB Query Comparison

| Operation | CozoDB (Datalog) | Bighorn (Cypher) |
|-----------|------------------|------------------|
| Find entity | `?[id] := entity[id, 'Person', _, _, _]` | `MATCH (e:Entity {type: 'Person'}) RETURN e.id` |
| 1-hop | `?[b] := relation['a', b, _, _, _]` | `MATCH ({id: 'a'})-[]->(n) RETURN n` |
| N-hop | Recursive rules | `MATCH (a)-[*1..3]-(b) RETURN b` |
| Aggregation | `?[count(x)] := ...` | `MATCH (n) RETURN count(n)` |

### SQLite Schema Design (for ltm.*)

```sql
-- Long-term key-value store
CREATE TABLE IF NOT EXISTS ltm_store (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,  -- JSON serialized
    metadata TEXT,        -- JSON serialized
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Full-text search virtual table
CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(
    key, value, metadata,
    content='ltm_store',
    content_rowid='rowid'
);

-- Triggers to keep FTS in sync
CREATE TRIGGER IF NOT EXISTS ltm_ai AFTER INSERT ON ltm_store BEGIN
    INSERT INTO ltm_fts(rowid, key, value, metadata)
    VALUES (new.rowid, new.key, new.value, new.metadata);
END;

CREATE TRIGGER IF NOT EXISTS ltm_ad AFTER DELETE ON ltm_store BEGIN
    INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
    VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
END;

CREATE TRIGGER IF NOT EXISTS ltm_au AFTER UPDATE ON ltm_store BEGIN
    INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
    VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
    INSERT INTO ltm_fts(rowid, key, value, metadata)
    VALUES (new.rowid, new.key, new.value, new.metadata);
END;
```

### Thread Safety Considerations

```python
import threading
from contextlib import contextmanager

class SQLiteBackend:
    """Thread-safe SQLite backend for long-term key-value storage."""

    def __init__(self, db_path: str):
        self.db_path = self._validate_path(db_path)
        self._local = threading.local()
        self._lock = threading.Lock()

    def _validate_path(self, path: str) -> str:
        """Prevent path traversal attacks."""
        import os
        real_path = os.path.realpath(path)
        # Add additional validation as needed
        return real_path

    @contextmanager
    def _get_connection(self):
        """Thread-local connection with WAL mode."""
        if not hasattr(self._local, 'conn'):
            import sqlite3
            self._local.conn = sqlite3.connect(
                self.db_path,
                check_same_thread=False,
                isolation_level=None  # Autocommit
            )
            self._local.conn.execute("PRAGMA journal_mode=WAL")
            self._local.conn.execute("PRAGMA busy_timeout=5000")
            self._local.conn.row_factory = sqlite3.Row
        yield self._local.conn


class CozoBackend:
    """Thread-safe CozoDB backend for graph storage."""

    COZO_AVAILABLE = False

    def __init__(self, db_path: str):
        try:
            from pycozo import Client
            CozoBackend.COZO_AVAILABLE = True
        except ImportError:
            CozoBackend.COZO_AVAILABLE = False
            return

        self.db_path = db_path
        self._client = Client('sqlite', db_path)
        self._lock = threading.Lock()
        self._init_schema()

    def _init_schema(self):
        """Initialize graph schema."""
        # Entity table
        self._client.run("""
            :create entity {
                id: String =>
                type: String,
                properties: String,
                embedding: [Float]?,
                created_at: Float default now()
            }
        """)
        # Relation table
        self._client.run("""
            :create relation {
                from_id: String,
                to_id: String,
                rel_type: String =>
                properties: String?,
                created_at: Float default now()
            }
        """)
```

### Graceful Degradation Pattern

```python
def _check_cozo_available():
    """Check if CozoDB is available."""
    try:
        from pycozo import Client
        return True
    except ImportError:
        return False

COZO_AVAILABLE = _check_cozo_available()

def graph_store_entity(state, entity_id, entity_type, **kwargs):
    """Store entity in graph database."""
    if not COZO_AVAILABLE:
        return {
            "success": False,
            "error": "CozoDB not installed. Install with: pip install 'pycozo[embedded]'",
            "error_type": "dependency_missing"
        }
    # ... implementation
```

### Error Handling Pattern

All actions return consistent error format:

```python
# Success response
{"success": True, "stored": True, "key": "my_key", ...}

# Error response
{
    "success": False,
    "error": "Human-readable error message",
    "error_type": "validation_error|connection_error|query_error|dependency_missing"
}
```

### Key Constraints

- SQLite backend requires no external dependencies (stdlib)
- CozoDB requires `pip install "pycozo[embedded]"` (~15MB)
- Embedding integration is optional (works without OpenAI)
- Both backends must implement `close()` for resource cleanup
- Graph queries have configurable timeout to prevent runaway traversals
- Path inputs are validated to prevent directory traversal

### Version Requirements

- Python: >=3.9
- pycozo[embedded]: >=0.7.0
- sqlite3: stdlib (included)
- openai: >=1.0.0 (optional, for embeddings)

### File Locations

- **Backend implementations**: `src/the_edge_agent/memory.py` (extend existing file)
- **Actions**: `src/the_edge_agent/actions/memory_actions.py` (add ltm/graph actions)
- **Tests**: `tests/test_yaml_engine_ltm.py` (new file)

## Testing

**Test File Location**: `tests/test_yaml_engine_ltm.py`

**Priority Levels**:
- **P0**: Critical - Must pass for basic functionality
- **P1**: Core - Required for production readiness
- **P2**: Edge cases - Important but not blocking

**Testing Standards**:
- Use `unittest` framework consistent with existing tests
- Mock CozoDB for unit tests where pycozo not installed
- Use temporary directories for SQLite/CozoDB test databases
- Test both success and error paths
- Verify graceful degradation

**Unit Test Cases**:
```python
class TestSQLiteBackend(unittest.TestCase):
    # P0 - Critical
    def test_sqlite_store_retrieve(self): ...  # (P0)
    def test_sqlite_delete(self): ...  # (P0)
    def test_sqlite_search_basic(self): ...  # (P0)

    # P1 - Core functionality
    def test_sqlite_store_with_metadata(self): ...  # (P1)
    def test_sqlite_search_fts5(self): ...  # (P1)
    def test_sqlite_search_metadata_filter(self): ...  # (P1)
    def test_sqlite_thread_safety(self): ...  # (P1)
    def test_sqlite_wal_mode(self): ...  # (P1)
    def test_sqlite_path_validation(self): ...  # (P1) - Security

    # P2 - Edge cases
    def test_sqlite_large_value(self): ...  # (P2)
    def test_sqlite_concurrent_writes(self): ...  # (P2)

class TestCozoBackend(unittest.TestCase):
    # P0 - Critical
    def test_cozo_store_entity(self): ...  # (P0)
    def test_cozo_store_relation(self): ...  # (P0)
    def test_cozo_query_basic(self): ...  # (P0)
    def test_cozo_graceful_degradation(self): ...  # (P0)

    # P1 - Core functionality
    def test_cozo_entity_with_properties(self): ...  # (P1)
    def test_cozo_datalog_pattern_match(self): ...  # (P1)
    def test_cozo_retrieve_context_by_entity(self): ...  # (P1)
    def test_cozo_retrieve_context_by_embedding(self): ...  # (P1) - mock embeddings
    def test_cozo_multi_hop_traversal(self): ...  # (P1)
    def test_cozo_hnsw_vector_search(self): ...  # (P1)
    def test_cozo_query_timeout(self): ...  # (P1)

    # P2 - Edge cases
    def test_cozo_large_graph(self): ...  # (P2)
    def test_cozo_cyclic_relations(self): ...  # (P2)
    def test_cozo_time_travel_query(self): ...  # (P2)

class TestLTMActions(unittest.TestCase):
    # P0 - Critical
    def test_ltm_store_action(self): ...  # (P0)
    def test_ltm_retrieve_action(self): ...  # (P0)
    def test_graph_store_entity_action(self): ...  # (P0)

    # P1 - Core functionality
    def test_ltm_dual_namespace_access(self): ...  # (P1)
    def test_graph_dual_namespace_access(self): ...  # (P1)
    def test_ltm_search_action(self): ...  # (P1)
    def test_graph_query_action(self): ...  # (P1)
    def test_error_response_format(self): ...  # (P1)

    # P2 - Edge cases
    def test_backend_injection(self): ...  # (P2)
    def test_backend_cleanup(self): ...  # (P2)
```

**Integration Test Cases**:
```python
class TestLTMIntegration(unittest.TestCase):
    def test_ltm_in_yaml_workflow(self): ...  # (P0)
    def test_graph_in_yaml_workflow(self): ...  # (P0)
    def test_combined_memory_workflow(self): ...  # (P1) - short-term + long-term
    def test_knowledge_graph_agent(self): ...  # (P1) - realistic agent scenario
    def test_rag_with_graph_context(self): ...  # (P1) - embedding + graph
```

**Test Summary**: 32 tests (27 unit + 5 integration) | P0: 10 | P1: 17 | P2: 5

## Definition of Done

- [x] All acceptance criteria verified ✓
- [x] All tasks completed ✓
- [x] Tests pass (existing and new) ✓ - 475 passed, 4 skipped
- [x] No regressions in existing memory functionality ✓
- [x] Documentation updated ✓ - CLAUDE.md and docs/YAML_AGENTS.md
- [x] Code follows existing patterns in yaml_engine.py ✓
- [x] CozoDB dependency is optional (graceful degradation if not installed) ✓
- [x] Error handling returns consistent format ✓

## Rollback Procedure

If long-term memory actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['ltm.store'] = ltm_store
   # actions['ltm.retrieve'] = ltm_retrieve
   # ... etc
   # actions['graph.store_entity'] = graph_store_entity
   # ... etc
   ```

2. **State Cleanup**:
   - SQLite databases remain on disk (can be manually deleted)
   - CozoDB databases remain on disk (can be manually deleted)
   - Existing short-term memory (001.1) continues to work

3. **Verification**:
   - Run existing test suite: `pytest tests/test_yaml_engine.py`
   - Verify short-term memory actions still work

4. **Gradual Rollout** (Recommended):
   - Use feature flags: `YAMLEngine(enable_ltm=False, enable_graph=False)`
   - Enable per-environment before full release

## Example Usage

### YAML Agent with Knowledge Graph and Datalog Reasoning

```yaml
name: knowledge_agent
version: "1.0"

settings:
  ltm:
    backend: sqlite
    path: ./agent_memory.db
  graph:
    backend: cozo
    path: ./agent_graph.db

nodes:
  - id: extract_entities
    action: llm.tools
    model: gpt-4
    messages:
      - role: system
        content: |
          Extract entities and relationships from the user's message.
          Use store_entity for each entity and store_relation for connections.
    tools:
      - name: store_entity
        action: graph.store_entity
        parameters:
          embed: true  # Generate embedding for semantic search
      - name: store_relation
        action: graph.store_relation

  - id: retrieve_context
    action: graph.retrieve_context
    query: "{{ state.user_query }}"
    hops: 2
    limit: 20

  - id: reason_about_context
    action: graph.query
    datalog: |
      # Find all entities connected to the query context
      # with transitive closure (recursive)
      connected[a, b] := relation[a, b, _, _, _]
      connected[a, c] := connected[a, b], relation[b, c, _, _, _]

      ?[id, type, properties] :=
          connected[$start_entity, id],
          entity[id, type, properties, _, _]

      :limit 50
    params:
      start_entity: "{{ state.primary_entity }}"

  - id: respond
    action: llm.call
    model: gpt-4
    messages:
      - role: system
        content: |
          Use this context from the knowledge graph:

          Entities: {{ state.entities | json }}
          Relations: {{ state.relations | json }}
          Reasoning results: {{ state.reason_about_context.results | json }}
      - role: user
        content: "{{ state.user_query }}"

edges:
  - from: START
    to: extract_entities
  - from: extract_entities
    to: retrieve_context
  - from: retrieve_context
    to: reason_about_context
  - from: reason_about_context
    to: respond
  - from: respond
    to: END
```

### Datalog Query Examples

```yaml
# Simple pattern match - find all Person entities
- id: find_people
  action: graph.query
  datalog: |
    ?[id, properties] :=
        entity[id, type, properties, _, _],
        type = 'Person'

# N-hop traversal - find all entities within 2 hops
- id: find_connected
  action: graph.query
  datalog: |
    hop1[id] := relation[$start, id, _, _, _]
    hop2[id] := hop1[mid], relation[mid, id, _, _, _]
    all_connected[id] := hop1[id]
    all_connected[id] := hop2[id]

    ?[id, type, properties] :=
        all_connected[id],
        entity[id, type, properties, _, _]
  params:
    start: "{{ state.entity_id }}"

# Semantic search with graph filter
- id: semantic_graph_search
  action: graph.retrieve_context
  query: "{{ state.user_query }}"
  hops: 1
  limit: 10
```

## QA Results

**Review Date**: 2025-12-07
**Reviewer**: Quinn (Test Architect)
**Review Type**: Test Design

### Test Design Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 52 |
| Unit Tests | 35 (67%) |
| Integration Tests | 17 (33%) |
| E2E Tests | 0 (0%) |
| P0 (Critical) | 14 |
| P1 (Core) | 26 |
| P2 (Edge Cases) | 12 |

### Risk Assessment

| Risk Area | Probability | Impact | Mitigation |
|-----------|-------------|--------|------------|
| SQLite concurrency issues | Medium | High | Thread-local connections, WAL mode, busy timeout |
| CozoDB dependency unavailable | High | Medium | Graceful degradation with informative errors |
| Path traversal attacks | Low | Critical | Path validation before operations |
| Memory leaks from unclosed connections | Medium | Medium | Context managers, explicit close(), test cleanup |
| Datalog query injection | Low | High | Parameter binding, query validation |
| Embedding dimension mismatches | Medium | Medium | Dimension validation on store operations |

### AC Coverage

All 18 acceptance criteria have test coverage:
- AC1-3: Backend protocols and implementations (9 tests)
- AC4-7: ltm.* actions (10 tests)
- AC8-11: graph.* actions (13 tests)
- AC12: YAMLEngine integration (6 tests)
- AC13: Thread safety and resource cleanup (3 tests)
- AC14-15: Action patterns and namespaces (3 tests)
- AC16: Unit test coverage (addressed by all tests)
- AC17: Documentation (out of test scope)
- AC18: Graceful degradation (3 tests)

### Key Test Decisions

1. **No E2E tests** - Backend/library feature without direct user-facing components; integration tests sufficient
2. **Heavy unit focus (67%)** - SQLite/CozoDB involve significant pure logic best tested in isolation
3. **CozoDB tests use `pytest.mark.skipif`** - Gracefully skip when pycozo not installed
4. **Security tests included** - Path traversal (001.4-UNIT-011) and query injection (001.4-UNIT-038)

### Test Design Document

Full test design: `docs/qa/assessments/TEA-BUILTIN-001.4-test-design-20251207.md`

### Recommendations

1. **Implement P0 tests first** - Validate core CRUD and graceful degradation before extended features
2. **Use temporary directories** - All SQLite/CozoDB tests should use temp dirs for isolation
3. **Mock embeddings** - RAG integration tests should mock embedding.create to avoid API calls
4. **Stress test concurrency** - P2 test with 10+ threads validates real-world usage patterns

### Implementation Review (2025-12-07)

**Reviewed By**: Quinn (Test Architect)
**Review Type**: Implementation QA Review

#### Code Quality Assessment

**Overall Assessment**: Excellent implementation quality. The code follows established patterns from TEA-BUILTIN-001.1 (Memory Actions) with clean separation of concerns and comprehensive error handling.

**Strengths**:
- Clean protocol-based design (`LongTermMemoryBackend`, `GraphBackend`) enables pluggable backends
- Thread-safe implementation with proper locking patterns
- Consistent error response format across all operations
- Graceful degradation when CozoDB not installed
- Comprehensive docstrings with examples
- FTS5 full-text search with automatic sync triggers

**Architecture Highlights**:
- SQLiteBackend uses shared cache for in-memory databases (`file:ltm_mem_{id}?mode=memory&cache=shared`)
- WAL mode enabled for file-based databases with busy timeout
- CozoBackend schema properly handles entity/relation tables with HNSW vector index
- Action module separation (`ltm_actions.py`, `graph_actions.py`) follows established patterns

#### Compliance Check

- Coding Standards: ✓ - Follows project patterns
- Project Structure: ✓ - Files in correct locations
- Testing Strategy: ✓ - 37 unit/integration tests
- All ACs Met: ✓ - All 18 acceptance criteria verified

#### Test Coverage Analysis

| Test Category | Count | Status |
|---------------|-------|--------|
| SQLiteBackend CRUD | 6 | ✓ All Pass |
| SQLiteBackend Search/Filter | 4 | ✓ All Pass |
| SQLiteBackend Thread Safety | 2 | ✓ All Pass |
| SQLiteBackend Edge Cases | 3 | ✓ All Pass |
| LTM Actions | 7 | ✓ All Pass |
| Graph Actions (graceful degradation) | 4 | ✓ All Pass |
| CozoBackend (when installed) | 4 | ✓ All Pass |
| Backend Injection/Lifecycle | 3 | ✓ All Pass |
| Integration | 1 | ✓ All Pass |
| **Total** | **37** | **37 Pass, 1 Skip** |

**Note**: 1 test skipped (`test_graph_graceful_degradation`) when CozoDB is installed - expected behavior.

#### AC-to-Test Traceability

| AC | Description | Test(s) |
|----|-------------|---------|
| 1 | LongTermMemoryBackend protocol | Protocol in memory.py, used by SQLiteBackend |
| 2 | SQLiteBackend with FTS5 | test_search_basic, test_search_fts5_multiple_results |
| 3 | CozoBackend with Datalog | test_query_basic (CozoBackend class) |
| 4 | ltm.store action | test_ltm_store_action |
| 5 | ltm.retrieve action | test_ltm_retrieve_action, test_ltm_retrieve_default |
| 6 | ltm.delete action | test_ltm_delete_action |
| 7 | ltm.search action | test_ltm_search_action |
| 8 | graph.store_entity | test_store_entity |
| 9 | graph.store_relation | test_store_relation |
| 10 | graph.query | test_query_basic |
| 11 | graph.retrieve_context | test_retrieve_context_by_entity |
| 12 | Backend injection | test_custom_ltm_backend_injection |
| 13 | Thread-safety & cleanup | test_thread_safety_*, test_engine_close_cleanup |
| 14 | _setup_builtin_actions pattern | Verified in ltm_actions.py, graph_actions.py |
| 15 | Dual namespace access | test_ltm_dual_namespace_access, test_graph_dual_namespace_access |
| 16 | Unit tests | 37 tests in test_yaml_engine_ltm.py |
| 17 | Documentation | CLAUDE.md updated, story complete |
| 18 | Graceful degradation | test_graph_graceful_degradation |

#### Security Review

- **Path Traversal Protection**: ✓ Implemented in `_validate_path()` - rejects `..` in paths
- **SQL Injection**: ✓ Uses parameterized queries throughout
- **JSON Serialization**: ✓ Safe JSON encode/decode with error handling
- **Thread Safety**: ✓ Locking on all critical operations

#### Performance Considerations

- **SQLite WAL Mode**: Enabled for concurrent read/write performance
- **Busy Timeout**: 5000ms to handle lock contention
- **Shared Cache**: Used for in-memory databases to allow multi-thread access
- **FTS5 Triggers**: Automatic sync keeps search index up-to-date

#### Regression Check

Full test suite executed: **478 passed, 1 skipped** - No regressions detected.

#### Gate Status

**Gate: PASS** → docs/qa/gates/TEA-BUILTIN-001.4-long-term-memory.yml

#### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, all tests pass, documentation complete.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-07 | 0.1 | Initial draft with Kuzu | Sarah (PO Agent) |
| 2025-12-07 | 0.2 | Rewrote with CozoDB architecture per strategic analysis | Sarah (PO Agent) |
| 2025-12-07 | 0.3 | Added QA Results - Test Design review | Quinn (Test Architect) |
| 2025-12-07 | 0.4 | Added QA Results - Implementation review (PASS) | Quinn (Test Architect) |
| 2025-12-07 | 0.5 | Extended with Bighorn (KuzuDB fork) for cloud-native graph with direct S3/GCS I/O | SM Agent |
| 2025-12-07 | 0.6 | Implemented KuzuBackend (Bighorn), Cypher queries, httpfs cloud ops, 29 tests passing | James (Dev Agent) |
