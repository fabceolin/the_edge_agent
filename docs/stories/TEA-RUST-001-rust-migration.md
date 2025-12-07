# TEA-RUST-001: Migrate The Edge Agent to Pure Rust with Lua Scripting

## Epic Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RUST-001 |
| **Type** | Epic |
| **Priority** | High |
| **Estimated Effort** | 18-24 weeks (includes built-in actions + LTM/Graph) |
| **Status** | Draft |

## Description

**As a** developer deploying autonomous workflows to edge computing environments,
**I want** The Edge Agent rewritten in Rust with Lua scripting support,
**So that** I get a single static binary with true parallelism, minimal resource footprint, and resilient unattended execution.

## Background

The Edge Agent (tea) is currently a Python library (~3K LOC) implementing a state graph workflow engine inspired by LangGraph. The migration to Rust will:

- Eliminate Python runtime dependencies
- Enable true parallel execution (no GIL)
- Produce a small (~5-10MB) static binary suitable for edge deployment
- Support autonomous operation without human intervention

**Target deployment:** Embedded Linux systems running autonomously, requiring robust error handling and recovery mechanisms.

---

## Scope

### In Scope

- Static graph execution from YAML configuration
- Node execution with built-in actions (Rust-native)
- Lua 5.4 integration for custom logic and eval expressions
- Conditional edge routing (Lua expressions)
- Parallel fan-out/fan-in execution (rayon)
- Checkpoint persistence (serde + bincode)
- Interrupt before/after with resume capability
- `invoke()` and `stream()` execution modes
- Template variable substitution (`{{ state.key }}`, `{{ variables.key }}`)
- CLI binary for standalone execution
- Library crate for embedding in other Rust projects
- Configurable retry/fallback error handling
- LLM integration (Ollama, OpenAI-compatible APIs)
- Linux x86_64 and aarch64 targets

### Out of Scope

- Dynamic graph mutation at runtime
- Python API or bindings (PyO3)
- Backward compatibility with pickle checkpoints
- Python `exec()`/`eval()` support
- macOS / Windows support
- Non-OpenAI-compatible LLM APIs (Anthropic native, etc.)
- Tools Bridge actions (CrewAI, MCP, LangChain) - Python ecosystem dependent

---

## Acceptance Criteria

### Graph Execution

- [ ] **AC-1**: GIVEN a valid YAML workflow file, WHEN loaded by the Rust engine, THEN an immutable StateGraph is constructed

- [ ] **AC-2**: GIVEN a compiled StateGraph, WHEN `invoke()` is called with initial state, THEN nodes execute in correct order and final state is returned

- [ ] **AC-3**: GIVEN a compiled StateGraph, WHEN `stream()` is called with initial state, THEN an iterator yields events for each node execution

### Parallel Execution

- [ ] **AC-4**: GIVEN parallel edges defined in YAML, WHEN execution reaches fan-out node, THEN branches execute concurrently via rayon and merge at fan-in

- [ ] **AC-5**: GIVEN a parallel branch fails, WHEN retry policy is configured, THEN branch retries up to max_retries before failing

### Checkpointing

- [ ] **AC-6**: GIVEN `interrupt_before` configured for a node, WHEN execution reaches that node, THEN execution stops and checkpoint is saved

- [ ] **AC-7**: GIVEN a saved checkpoint, WHEN `invoke()` is called with checkpoint path and state updates, THEN execution resumes with merged state

### Lua Integration

- [ ] **AC-8**: GIVEN a Lua condition expression, WHEN evaluated against current state, THEN correct edge is selected based on result

- [ ] **AC-9**: GIVEN inline Lua code in a node, WHEN node executes, THEN Lua script runs with state access and returns updated state

### Error Handling

- [ ] **AC-10**: GIVEN a node fails with transient error, WHEN retry policy allows retries, THEN node retries with exponential backoff up to max_retries

- [ ] **AC-11**: GIVEN a node exhausts retries, WHEN fallback node is configured, THEN execution continues to fallback node

- [ ] **AC-12**: GIVEN a node exhausts retries with no fallback, WHEN error_policy is "continue", THEN execution skips to next node with error logged to state

- [ ] **AC-13**: GIVEN unattended execution mode, WHEN any unrecoverable error occurs, THEN checkpoint is saved and process exits with non-zero code

### LLM Actions

- [ ] **AC-14**: GIVEN `llm.call` action with Ollama provider, WHEN node executes, THEN request is sent to local Ollama instance and response stored in state

- [ ] **AC-15**: GIVEN `llm.call` action with OpenAI-compatible provider, WHEN node executes with api_base and api_key, THEN request is sent to specified endpoint and response stored in state

- [ ] **AC-16**: GIVEN LLM request fails, WHEN retry policy is configured, THEN request retries with backoff before falling back

### Built-in Actions (Rust Native)

- [ ] **AC-23**: GIVEN `memory.store` action, WHEN executed with key/value/ttl, THEN data is stored in memory backend with TTL enforcement

- [ ] **AC-24**: GIVEN `memory.retrieve` action, WHEN executed with key, THEN stored value is returned (or default if expired/missing)

- [ ] **AC-25**: GIVEN `trace.start`/`trace.log`/`trace.end` actions, WHEN executed, THEN spans are created, events logged, and exported via configured exporter

- [ ] **AC-26**: GIVEN `json.parse`/`json.transform`/`json.stringify` actions, WHEN executed, THEN JSON data is parsed, transformed via JMESPath, and serialized correctly

- [ ] **AC-27**: GIVEN `csv.parse`/`csv.stringify` actions, WHEN executed, THEN CSV data is parsed/serialized with configurable delimiters and headers

- [ ] **AC-28**: GIVEN `data.validate` action with JSON Schema, WHEN executed, THEN data is validated and errors are returned with paths

- [ ] **AC-29**: GIVEN `data.merge`/`data.filter` actions, WHEN executed, THEN dicts are merged (deep/shallow) and lists are filtered by predicates

- [ ] **AC-30**: GIVEN `web.scrape`/`web.crawl` actions, WHEN executed with Firecrawl API key, THEN content is fetched and returned as LLM-ready markdown

- [ ] **AC-31**: GIVEN `web.search` action, WHEN executed with Perplexity API key, THEN search results are returned

- [ ] **AC-32**: GIVEN `embedding.create` action, WHEN executed, THEN embeddings are generated via OpenAI or Ollama provider

- [ ] **AC-33**: GIVEN `vector.store`/`vector.query` actions, WHEN executed, THEN documents are stored with embeddings and semantic search returns ranked results

- [ ] **AC-34**: GIVEN `code.execute` action is DISABLED by default, WHEN enabled and executed with Lua code, THEN code runs in Lua sandbox (not RestrictedPython)

### Long-Term Memory Actions (Rust Native)

- [ ] **AC-35**: GIVEN `ltm.store` action, WHEN executed with key/value/metadata, THEN data is persisted to SQLite with FTS5 indexing

- [ ] **AC-36**: GIVEN `ltm.retrieve` action, WHEN executed with key, THEN stored value and metadata are returned (or default if not found)

- [ ] **AC-37**: GIVEN `ltm.search` action, WHEN executed with query text, THEN FTS5 full-text search returns matching entries ranked by relevance

- [ ] **AC-38**: GIVEN `graph.store_entity` action, WHEN executed with entity_id/type/properties, THEN entity is stored in graph database with optional embedding

- [ ] **AC-39**: GIVEN `graph.store_relation` action, WHEN executed with source/target/type, THEN relationship edge is created in graph database

- [ ] **AC-40**: GIVEN `graph.query` action, WHEN executed with query pattern, THEN graph traversal returns matching entities and relations

- [ ] **AC-41**: GIVEN `graph.retrieve_context` action, WHEN executed with entity_id or embedding, THEN N-hop subgraph context is returned

### CLI

- [ ] **AC-17**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --input '{"key": "value"}'`, THEN workflow executes and outputs final state as JSON

- [ ] **AC-18**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --stream`, THEN workflow outputs each event as newline-delimited JSON

- [ ] **AC-19**: GIVEN `tea` CLI binary, WHEN run with `tea resume checkpoint.bin --input '{"update": "value"}'`, THEN execution resumes from checkpoint with merged state

- [ ] **AC-20**: GIVEN `tea` CLI binary, WHEN run with `tea validate workflow.yaml`, THEN YAML is parsed and validated without execution

### Library

- [ ] **AC-21**: GIVEN `the_edge_agent` crate as dependency, WHEN `StateGraph::from_yaml()` is called, THEN graph can be used in Rust application

- [ ] **AC-22**: GIVEN library usage, WHEN custom actions are registered via ActionsRegistry, THEN custom Rust functions can be called from YAML nodes

---

## Technical Architecture

### Core Crates

| Component | Crate | Purpose |
|-----------|-------|---------|
| Graph engine | `petgraph` | DiGraph data structure |
| YAML parsing | `serde_yaml` | Configuration loading |
| Templates | `tera` | `{{ variable }}` syntax |
| Lua runtime | `mlua` | Scripting and eval |
| Parallelism | `rayon` | Thread pool |
| Channels | `crossbeam` | Streaming events |
| Serialization | `serde`, `bincode` | Checkpoints |
| HTTP client | `reqwest` | Actions, LLM calls |
| CLI | `clap` | Command-line interface |
| Logging | `tracing` | Observability |

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Static graphs only | Simpler, immutable after parse |
| Lua 5.4 (not LuaJIT) | Modern features, simpler build |
| rayon over tokio | Simpler for CPU-bound graph traversal |
| reqwest blocking | Avoid async complexity |
| No Python interop | Clean break, no FFI overhead |
| bincode checkpoints | Compact binary, no pickle |
| musl static linking | Zero runtime dependencies |

### Build Targets

- `x86_64-unknown-linux-musl` (static binary)
- `aarch64-unknown-linux-musl` (ARM64 edge devices)

---

## Error Handling Configuration

```yaml
# Global defaults
error_policy:
  max_retries: 3
  backoff_base_ms: 1000
  backoff_max_ms: 30000
  jitter: true
  on_failure: checkpoint_and_exit  # or: continue, fallback

nodes:
  fetch_data:
    action: http.get
    config:
      url: "{{ variables.api_url }}"
    retry:
      max_retries: 5  # Override global
    fallback: use_cached_data  # Node to run on failure

  use_cached_data:
    action: file.read
    config:
      path: "/var/cache/last_data.json"
```

---

## CLI Interface

### Commands

| Command | Description |
|---------|-------------|
| `tea run <file>` | Execute a workflow |
| `tea resume <checkpoint>` | Resume from checkpoint |
| `tea validate <file>` | Validate YAML without execution |
| `tea inspect <file>` | Show graph structure |

### Flags

| Flag | Description |
|------|-------------|
| `--input, -i` | Initial state as JSON string or @file.json |
| `--stream, -s` | Output events as NDJSON |
| `--checkpoint-dir, -c` | Directory for checkpoint files |
| `--config, -C` | Config overrides as JSON |
| `--verbose, -v` | Increase log verbosity |
| `--quiet, -q` | Suppress non-error output |

---

## Sub-Stories

| ID | Title | Points |
|----|-------|--------|
| TEA-RUST-002 | Core StateGraph with petgraph | 5 |
| TEA-RUST-003 | YAML parser and tera template engine | 5 |
| TEA-RUST-004 | Node execution and edge traversal | 3 |
| TEA-RUST-005 | Conditional routing with Lua expressions | 3 |
| TEA-RUST-006 | Parallel fan-out/fan-in with rayon | 5 |
| TEA-RUST-007 | Checkpoint persistence and interrupt handling | 3 |
| TEA-RUST-008 | Error handling with retry and fallback | 5 |
| TEA-RUST-009 | Lua integration via mlua | 5 |
| TEA-RUST-010 | Built-in actions - HTTP and file operations | 3 |
| TEA-RUST-011 | Built-in actions - LLM (Ollama, OpenAI-compatible) | 5 |
| TEA-RUST-012 | Stream iterator implementation | 2 |
| TEA-RUST-013 | CLI binary with clap | 3 |
| TEA-RUST-014 | Library crate public API | 2 |
| TEA-RUST-015 | Testing suite and documentation | 5 |
| TEA-RUST-016 | Built-in actions - Memory (store, retrieve, summarize) | 3 |
| TEA-RUST-017 | Built-in actions - Observability (trace start, log, end) | 3 |
| TEA-RUST-018 | Built-in actions - Data Processing (json.*, csv.*, data.*) | 5 |
| TEA-RUST-019 | Built-in actions - Web (scrape, crawl, search via APIs) | 3 |
| TEA-RUST-020 | Built-in actions - RAG (embedding, vector store/query) | 5 |
| TEA-RUST-021 | Built-in actions - Code Execution (Lua sandbox) | 5 |
| TEA-RUST-022 | LLM Enhanced actions (stream, retry, tools) | 5 |
| TEA-RUST-023 | Built-in actions - Long-Term Memory (ltm.*, SQLite/FTS5) | 5 |
| TEA-RUST-024 | Built-in actions - Graph Database (graph.*, CozoDB native Rust) | 5 |

---

## Built-in Actions Migration

The Python implementation includes 27+ built-in actions across 8 categories. This section details the Rust migration strategy for each.

### Migration Strategy by Category

#### P0 - Core Actions (Migrate in Phase 1)

| Action Category | Python Implementation | Rust Implementation | Complexity |
|-----------------|----------------------|---------------------|------------|
| **Memory Actions** | `InMemoryBackend` with TTL | `HashMap` with `Instant` for TTL | Low |
| | `memory.store/retrieve` | Direct Rust implementation | Low |
| | `memory.summarize` | Uses `llm.call` internally | Medium |
| **Observability** | `TraceContext` with exporters | `tracing` crate integration | Low |
| | `trace.start/log/end` | Native Rust spans | Low |
| **Data Processing** | stdlib `json`, `csv` | `serde_json`, `csv` crates | Low |
| | `json.parse/transform/stringify` | `serde_json` + `jmespath` | Low |
| | `csv.parse/stringify` | `csv` crate | Low |
| | `data.validate` | `jsonschema` optional | `jsonschema` crate | Medium |
| | `data.merge/filter` | Pure Python | Pure Rust | Low |
| **Long-Term Memory** | `SQLiteBackend` with FTS5 | `rusqlite` with FTS5 | Low |
| | `ltm.store/retrieve/delete` | Direct Rust implementation | Low |
| | `ltm.search` | FTS5 full-text search | `rusqlite` FTS5 | Low |

#### P1 - Integration Actions (Migrate in Phase 2)

| Action Category | Python Implementation | Rust Implementation | Complexity |
|-----------------|----------------------|---------------------|------------|
| **LLM Enhanced** | OpenAI Python SDK | `reqwest` HTTP calls | Medium |
| | `llm.stream` | Streaming response | `reqwest` + `futures-util` | Medium |
| | `llm.retry` | Exponential backoff | `backoff` crate | Low |
| | `llm.tools` | Function calling dispatch | Action registry dispatch | Medium |
| **Web Actions** | Firecrawl/Perplexity APIs | `reqwest` HTTP calls | Low |
| | `web.scrape/crawl/search` | External API delegation | Same pattern in Rust | Low |
| **RAG Actions** | OpenAI/Ollama embeddings | `reqwest` HTTP calls | Medium |
| | `embedding.create` | Provider abstraction | Rust trait for providers | Medium |
| | `vector.store/query` | `InMemoryVectorStore` | Rust native vector store | Medium |

#### P2 - Advanced Actions (Migrate in Phase 3)

| Action Category | Python Implementation | Rust Implementation | Complexity |
|-----------------|----------------------|---------------------|------------|
| **Code Execution** | RestrictedPython sandbox | **Lua sandbox** (mlua) | High |
| | `code.execute/sandbox` | Lua 5.4 scripting | Different paradigm |
| **Graph Database** | CozoDB via `pycozo` | **CozoDB native Rust** (`cozo` crate) | Medium |
| | `graph.store_entity/relation` | Same Datalog operations | Low |
| | `graph.query` | Datalog queries | Same Datalog queries | Low |
| | `graph.retrieve_context` | HNSW + N-hop traversal | Same (native Rust) | Low |

#### Excluded from Rust Migration

| Action Category | Reason for Exclusion |
|-----------------|---------------------|
| **Tools Bridge** (`tools.crewai`, `tools.mcp`, `tools.langchain`) | Deeply tied to Python ecosystem. Users needing these should use Python version. |

### Built-in Actions Rust Crate Structure

```
src/
├── actions/
│   ├── mod.rs           # Action registry and trait definitions
│   ├── memory.rs        # memory.store, memory.retrieve, memory.summarize
│   ├── ltm.rs           # ltm.store, ltm.retrieve, ltm.delete, ltm.search (rusqlite + FTS5)
│   ├── graph.rs         # graph.store_entity, graph.store_relation, graph.query, graph.retrieve_context (cozo crate)
│   ├── trace.rs         # trace.start, trace.log, trace.end (uses tracing crate)
│   ├── data.rs          # json.*, csv.*, data.* actions
│   ├── web.rs           # web.scrape, web.crawl, web.search
│   ├── rag.rs           # embedding.create, vector.store, vector.query
│   ├── llm.rs           # llm.call, llm.stream, llm.retry, llm.tools
│   └── code.rs          # code.execute (Lua sandbox)
```

### Key Migration Decisions

#### 1. Memory Actions

**Python**: `InMemoryBackend` with `time.monotonic()` for TTL, `threading.Lock` for thread safety.

**Rust**:
```rust
use std::collections::HashMap;
use std::time::{Duration, Instant};
use parking_lot::RwLock;

struct MemoryEntry {
    value: serde_json::Value,
    expires_at: Option<Instant>,
}

struct InMemoryBackend {
    data: RwLock<HashMap<String, MemoryEntry>>,
}
```

#### 2. Observability Actions

**Python**: Custom `TraceContext` with thread-local span stacks.

**Rust**: Use the established `tracing` ecosystem:
```rust
use tracing::{span, Level, event};
use tracing_subscriber::fmt;

// trace.start -> span!
// trace.log -> event!
// trace.end -> span exit
```

#### 3. Data Processing Actions

**Python**: `json`, `csv` stdlib, optional `jmespath`, `jsonschema`.

**Rust**:
- `serde_json` for JSON parsing/serialization
- `csv` crate for CSV operations
- `jmespath` crate for JMESPath expressions
- `jsonschema` crate for validation

#### 4. Web Actions

**Python**: HTTP calls to Firecrawl and Perplexity APIs via `requests`.

**Rust**: Same pattern with `reqwest`:
```rust
use reqwest::blocking::Client;

fn web_scrape(url: &str, api_key: &str) -> Result<ScrapeResult, ActionError> {
    let client = Client::new();
    let resp = client.post("https://api.firecrawl.dev/v1/scrape")
        .bearer_auth(api_key)
        .json(&ScrapeRequest { url, formats: vec!["markdown"] })
        .send()?;
    // ...
}
```

#### 5. RAG Actions

**Python**: `OpenAIEmbeddingProvider`, `OllamaEmbeddingProvider`, `InMemoryVectorStore`.

**Rust**:
```rust
trait EmbeddingProvider {
    fn embed(&self, texts: &[&str]) -> Result<Vec<Vec<f32>>, ActionError>;
    fn dimensions(&self) -> usize;
}

struct InMemoryVectorStore {
    collections: HashMap<String, Vec<Document>>,
}

impl InMemoryVectorStore {
    fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
        // SIMD-optimized with ndarray or nalgebra
    }
}
```

#### 6. Code Execution Actions

**Python**: RestrictedPython bytecode transformation.

**Rust**: **Lua sandbox** via `mlua`:
```rust
use mlua::{Lua, Result};

fn code_execute(code: &str, timeout: Duration) -> Result<ExecutionResult> {
    let lua = Lua::new();
    // Sandbox: remove dangerous globals
    lua.scope(|scope| {
        // Set timeout via custom hook
        lua.set_hook(mlua::HookTriggers::every_line(), move |_lua, _debug| {
            // Check timeout
            Ok(())
        });
        lua.load(code).exec()
    })
}
```

**Key Difference**: Python `code.execute` runs Python; Rust `code.execute` runs Lua. This is a breaking change but provides true sandboxing without GIL concerns.

#### 7. Long-Term Memory Actions

**Python**: `SQLiteBackend` with FTS5 via `sqlite3` stdlib, thread-local connections.

**Rust**: `rusqlite` crate with FTS5 extension:
```rust
use rusqlite::{Connection, params};
use parking_lot::Mutex;

struct SQLiteBackend {
    conn: Mutex<Connection>,
}

impl SQLiteBackend {
    fn new(path: &str) -> Result<Self, rusqlite::Error> {
        let conn = Connection::open(path)?;
        conn.execute_batch("
            PRAGMA journal_mode=WAL;
            CREATE TABLE IF NOT EXISTS ltm_store (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL,
                metadata TEXT,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP
            );
            CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(key, value, metadata);
        ")?;
        Ok(Self { conn: Mutex::new(conn) })
    }

    fn search(&self, query: &str, limit: usize) -> Result<Vec<Entry>, rusqlite::Error> {
        let conn = self.conn.lock();
        let mut stmt = conn.prepare("
            SELECT key, value, metadata FROM ltm_fts WHERE ltm_fts MATCH ?1 LIMIT ?2
        ")?;
        // ...
    }
}
```

**Key Advantage**: Native Rust with same SQLite/FTS5 semantics - no breaking changes.

#### 8. Graph Database Actions

**Python**: CozoDB via `pycozo` Python bindings with SQLite storage backend.

**Rust**: CozoDB native Rust crate (`cozo`) - this is the actual implementation that `pycozo` wraps:
```rust
use cozo::{DbInstance, DataValue, NamedRows};

struct CozoBackend {
    db: DbInstance,
}

impl CozoBackend {
    fn new(path: &str) -> Result<Self, cozo::Error> {
        let db = DbInstance::new("sqlite", path, "")?;
        // Initialize schema - same Datalog as Python version
        db.run_script(r#"
            :create entity {
                id: String =>
                type: String,
                properties: String,
                embedding: [Float]?,
                created_at: Float default now()
            }
        "#, Default::default())?;
        db.run_script(r#"
            :create relation {
                from_id: String,
                to_id: String,
                rel_type: String =>
                properties: String?,
                created_at: Float default now()
            }
        "#, Default::default())?;
        Ok(Self { db })
    }

    fn query(&self, datalog: &str, params: BTreeMap<String, DataValue>) -> Result<NamedRows, cozo::Error> {
        self.db.run_script(datalog, params)
    }
}
```

**Key Advantage**: CozoDB is written in Rust - `pycozo` is just Python bindings. Using native Rust means:
- Same Datalog query language (no breaking changes)
- Same SQLite storage backend
- Same HNSW vector indexing
- Better performance (no Python FFI overhead)
- Smaller binary (no Python runtime)

### Rust Crate Dependencies for Built-in Actions

| Crate | Purpose | Used By |
|-------|---------|---------|
| `serde_json` | JSON parsing/serialization | data.*, web.*, rag.* |
| `csv` | CSV parsing/writing | csv.* |
| `jmespath` | JMESPath expressions | json.transform |
| `jsonschema` | JSON Schema validation | data.validate |
| `reqwest` (blocking) | HTTP client | web.*, rag.*, llm.* |
| `tracing` | Structured logging | trace.* |
| `mlua` | Lua scripting | code.* |
| `parking_lot` | Fast RwLock/Mutex | memory.*, ltm.* |
| `rusqlite` | SQLite with FTS5 | ltm.* |
| `cozo` | Graph DB with Datalog + HNSW | graph.* |

### Feature Flags for Optional Actions

```toml
[features]
default = ["memory", "trace", "data"]
memory = []
trace = ["tracing", "tracing-subscriber"]
data = ["serde_json", "csv"]
web = ["reqwest"]
rag = ["reqwest"]
llm = ["reqwest"]
code = ["mlua"]
ltm = ["rusqlite"]
graph = ["cozo"]
all = ["memory", "trace", "data", "web", "rag", "llm", "code", "ltm", "graph"]
```

### Action Compatibility Matrix

| Action | Python Behavior | Rust Behavior | Breaking Changes |
|--------|----------------|---------------|------------------|
| `memory.store/retrieve` | In-memory with TTL | Same | None |
| `memory.summarize` | Uses OpenAI | Same | None |
| `trace.*` | Custom exporters | `tracing` ecosystem | Exporter config differs |
| `json.*` | stdlib json | `serde_json` | Non-strict mode removed |
| `csv.*` | stdlib csv | `csv` crate | Same behavior |
| `data.validate` | `jsonschema` lib | `jsonschema` crate | Same behavior |
| `data.merge/filter` | Pure Python | Pure Rust | None |
| `web.*` | Firecrawl/Perplexity | Same APIs | None |
| `embedding.create` | OpenAI/Ollama | Same APIs | None |
| `vector.*` | In-memory store | Same | None |
| `llm.*` | OpenAI Python SDK | `reqwest` | None |
| `code.execute` | **RestrictedPython** | **Lua sandbox** | **Different language** |
| `ltm.*` | SQLite + FTS5 | `rusqlite` + FTS5 | None |
| `graph.*` | CozoDB (pycozo) | CozoDB (native Rust) | None |
| `tools.*` | CrewAI/MCP/LangChain | **Not migrated** | **Not available** |

---

## Risks

### High

| Risk | Mitigation |
|------|------------|
| Error handling edge cases in parallel execution | Comprehensive test suite, property-based testing |
| LLM provider API differences despite "OpenAI-compatible" claims | Adapter layer with provider-specific quirks |
| Code execution language change (Python→Lua) | Clear documentation, migration guide |

### Medium

| Risk | Mitigation |
|------|------------|
| Lua syntax migration for existing Python users | Migration guide with examples |
| musl static linking with mlua (Lua C library) | Vendor Lua source, test cross-compilation early |

### Low

| Risk | Mitigation |
|------|------------|
| petgraph API learning curve | Well-documented, established crate |
| tera template syntax differences from Python | Document differences, provide compatibility layer if needed |

---

## Success Metrics

| Metric | Target |
|--------|--------|
| Binary size | < 15MB |
| Startup time | < 50ms |
| Throughput vs Python | 10x+ improvement |
| Runtime dependencies | Zero (static binary) |
| Test coverage | All Python test scenarios passing |

---

## Dependencies

| Type | Dependency |
|------|------------|
| External | None (greenfield Rust project) |
| Runtime (optional) | Ollama for local LLM |
| Runtime (optional) | Firecrawl API key for web.scrape/crawl |
| Runtime (optional) | Perplexity API key for web.search |
| Runtime (optional) | OpenAI API key for embedding.create, llm.* |

## Python TEA-BUILTIN Stories Reference

The following Python built-in action stories have been implemented and inform this Rust migration:

| Story ID | Title | Status | Rust Migration Story |
|----------|-------|--------|---------------------|
| TEA-BUILTIN-001.1 | Memory Actions | ✅ Done | TEA-RUST-016 |
| TEA-BUILTIN-001.2 | LLM Enhanced Actions | ✅ Done | TEA-RUST-022 |
| TEA-BUILTIN-001.3 | Observability Actions | ✅ Done | TEA-RUST-017 |
| TEA-BUILTIN-001.4 | Long-Term Memory & Graph Actions | ✅ Done | TEA-RUST-023, TEA-RUST-024 |
| TEA-BUILTIN-002.1 | Web Actions | ✅ Done | TEA-RUST-019 |
| TEA-BUILTIN-002.2 | RAG Actions | ✅ Done | TEA-RUST-020 |
| TEA-BUILTIN-002.3 | Tools Bridge Actions | ✅ Done | **Not migrated** |
| TEA-BUILTIN-003.1 | Code Execution Actions | ✅ Done | TEA-RUST-021 (Lua) |
| TEA-BUILTIN-003.2 | Data Processing Actions | ✅ Done | TEA-RUST-018 |

**Note**: Tools Bridge Actions (TEA-BUILTIN-002.3) are **not** being migrated to Rust as they depend on Python-specific libraries (CrewAI, LangChain, MCP). Users requiring these capabilities should use the Python version of TEA.

**Key Advantage of TEA-BUILTIN-001.4 Migration**: CozoDB is natively written in Rust - the Python `pycozo` package is just Python bindings. The Rust migration uses CozoDB directly via the `cozo` crate, providing identical Datalog query semantics with better performance and no FFI overhead.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 1.0 | Initial draft - core graph execution, Lua integration, checkpointing | Sarah (PO Agent) |
| 2025-12-07 | 2.0 | Major update: Added built-in actions migration plan (27+ actions across 8 categories). Added 7 new sub-stories (TEA-RUST-016 to TEA-RUST-022). Updated effort estimate to 16-20 weeks. Added Python TEA-BUILTIN reference table. Documented breaking changes (code.execute: Python→Lua, tools.* not migrated). | Sarah (PO Agent) |
| 2025-12-07 | 3.0 | Added TEA-BUILTIN-001.4 Long-Term Memory & Graph actions. Added TEA-RUST-023 (LTM with rusqlite/FTS5) and TEA-RUST-024 (Graph with native CozoDB Rust crate). Updated effort to 18-24 weeks. Added AC-35 to AC-41 for ltm.* and graph.* actions. CozoDB uses native Rust - same Datalog semantics as Python version. | Sarah (PO Agent) |
