# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Edge Agent (tea) is a lightweight, single-app state graph library inspired by LangGraph, designed for edge computing environments. The core architecture is built around a `StateGraph` class that manages state-driven workflows with support for:

- Sequential and conditional node execution
- Parallel fan-out/fan-in patterns using ThreadPoolExecutor
- Checkpoint persistence for save/resume of workflow execution
- LLM-agnostic integration (works with any language model)
- Graph visualization using NetworkX and Graphviz

## Development Commands

### Setup
```bash
# Install system dependencies (required for pygraphviz)
sudo apt-get update
sudo apt-get install libgraphviz-dev graphviz -y

# Install package in development mode
pip install -e .[dev]
```

### Testing
```bash
# Run all tests
pytest

# Run with verbose output
pytest -v

# Run specific test file
pytest tests/test_stategraph.py

# Run specific test
pytest tests/test_stategraph.py::TestStateGraph::test_add_node
```

### Build
```bash
# Build package
python setup.py sdist bdist_wheel
```

## Architecture

### Core Components

**StateGraph** (`src/the_edge_agent/stategraph.py`):
- The main class implementing the state machine
- Uses NetworkX DiGraph internally to represent nodes and edges
- Special nodes: `START` ("__start__") and `END` ("__end__")
- Two execution modes: `invoke()` (returns final state) and `stream()` (yields intermediate states)

**Node Function Signatures**:
- Node functions can accept any combination of: `state`, `config`, `node`, `graph`, `parallel_results`
- Function parameters are introspected using `inspect.signature()` and auto-matched
- Functions must return a dictionary that updates the state
- Non-dict returns are wrapped as `{"result": value}`

**Parallel Execution**:
- Parallel edges defined via `add_parallel_edge(in_node, out_node, fan_in_node, config=ParallelConfig)`
- Fan-in nodes created with `add_fanin_node(node, run)`
- Each parallel flow executes in separate thread via `ThreadPoolExecutor`
- Flows execute until reaching the designated fan-in node
- Results collected in `state['parallel_results']` as `ParallelFlowResult` objects

**Parallel Execution Reliability** (TD.13):
- `ParallelConfig`: Per-edge configuration for timeout, retry, circuit breaker
- `ParallelFlowResult`: Rich result wrapper with timing, retry count, circuit state
- `RetryPolicy`: Exponential backoff with configurable delays and exception filtering
- `CircuitBreaker`: Prevents cascade failures with half-open recovery
- `ParallelFlowCallback`: Protocol for lifecycle event callbacks (start, complete, error, retry)

**Conditional Routing**:
- `add_conditional_edges(in_node, func, cond)` routes based on function output
- `cond` parameter maps function results to target nodes: `{True: "node_a", False: "node_b"}`
- Condition functions evaluated using same parameter introspection as node functions

### Key Design Patterns

1. **Interrupts**: Set via `compile(interrupt_before=[], interrupt_after=[], checkpointer=...)` for human-in-the-loop workflows. Interrupts STOP execution completely and require explicit resume via `invoke(None, checkpoint=...)`. A checkpointer is required when using interrupts.
2. **Exception Handling**: Controlled by `raise_exceptions` parameter (False by default)
3. **Graph Visualization**: `render_graphviz()` and `save_graph_image()` for debugging workflows

## Testing Strategy

Tests use:
- `unittest` framework with parameterized tests
- `hypothesis` for property-based testing
- Mock patches for testing edge cases
- Tests cover: node/edge operations, parallel execution, error handling, interrupts

## Common Patterns

### Basic Sequential Graph
```python
graph = tea.StateGraph({"value": int, "result": str})
graph.add_node("start", run=start_func)
graph.add_node("end", run=end_func)
graph.set_entry_point("start")
graph.add_edge("start", "end")
graph.set_finish_point("end")
compiled = graph.compile()
results = list(compiled.invoke({"value": 1}))
```

### Conditional Routing
```python
graph.add_conditional_edges(
    "start",
    lambda state: state["value"] > 10,
    {True: "end", False: "process"}
)
```

### Parallel Fan-out/Fan-in
```python
graph.add_fanin_node("fan_in", run=aggregate_func)
graph.add_parallel_edge("start", "flow1", "fan_in")
graph.add_parallel_edge("start", "flow2", "fan_in")
graph.add_parallel_edge("start", "flow3", "fan_in")
graph.add_edge("fan_in", "end")
```

### Parallel Execution with Reliability (TD.13)
```python
from the_edge_agent import (
    StateGraph, ParallelConfig, RetryPolicy,
    CircuitBreakerConfig, ParallelFlowResult
)

# Configure parallel flow with timeout and retry
graph.add_parallel_edge(
    "start", "api_call", "fan_in",
    config=ParallelConfig(
        timeout_seconds=30.0,      # Per-attempt timeout
        fail_fast=False,           # Continue with partial results
        retry_policy=RetryPolicy(
            max_retries=3,
            base_delay=1.0,        # Exponential backoff: 1s, 2s, 4s
            backoff_multiplier=2.0,
        ),
        circuit_breaker=CircuitBreakerConfig(
            failure_threshold=5,    # Open after 5 failures
            reset_timeout=30.0,     # Try recovery after 30s
        ),
    )
)

# Graph-level default config (compile time)
graph.compile(
    parallel_config=ParallelConfig(timeout_seconds=60.0),
    circuit_breaker_scope="global",  # Persist across invocations
)

# Access results with ParallelFlowResult
def aggregate(state, parallel_results):
    for result in parallel_results:
        if result.success:
            process(result.state)
        else:
            log_error(result.error, result.timing_ms, result.retry_count)
    return {"done": True}

# Circuit breaker management
graph.reset_circuit("api_call")  # Reset specific circuit
graph.reset_all_circuits()       # Reset all circuits
states = graph.get_circuit_states()  # Get circuit states for monitoring
```

### Parallel Flow Callbacks
```python
from the_edge_agent import (
    ParallelFlowCallback, ParallelFlowContext,
    ParallelFlowResult, CallbackManager
)

class MetricsCallback:
    def on_flow_start(self, context: ParallelFlowContext):
        metrics.increment("parallel_flow_started", tags={"branch": context.branch})

    def on_flow_complete(self, context: ParallelFlowContext, result: ParallelFlowResult):
        metrics.timing("parallel_flow_duration_ms", result.timing_ms)
        if not result.success:
            metrics.increment("parallel_flow_failed", tags={"branch": context.branch})

    def on_flow_retry(self, context, attempt, delay, error):
        metrics.increment("parallel_flow_retry", tags={"attempt": attempt})

    def on_circuit_state_change(self, context, old_state, new_state):
        metrics.gauge("circuit_state", new_state.value)

# Register callbacks at compile time
graph.compile(parallel_callbacks=[MetricsCallback()])
```

## YAML Engine

The Edge Agent supports declarative agent configuration via YAML files (see `docs/YAML_REFERENCE.md` for full documentation).

**YAMLEngine** (`src/the_edge_agent/yaml_engine.py`):
- Creates StateGraph instances from YAML configurations
- Supports inline Python code, built-in actions, multi-step nodes
- External action module imports via `imports:` section
- Template variables: `{{ state.key }}`, `{{ variables.key }}`, `{{ secrets.key }}`
- Filters: `| json`, `| upper`, `| lower`

### External Action Imports

Import custom Python action modules directly from YAML configuration:

```yaml
imports:
  # Local file (relative to YAML file)
  - path: ./actions/my_custom.py
    namespace: custom

  # Installed Python package
  - package: tea_actions_slack
    namespace: slack

nodes:
  - name: process
    uses: custom.transform      # From local file import
    with:
      data: "{{ state.input }}"

  - name: notify
    uses: slack.send_message    # From package import
    with:
      channel: "#alerts"
```

**Module Contract**: External modules MUST expose `register_actions(registry, engine)`:

```python
# my_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""

    def my_action(state, param1, param2=None, **kwargs):
        return {"result": "value", "success": True}

    registry['my_action'] = my_action

# Optional metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My custom actions",
    "actions": ["my_action"],
}
```

**Import Types**:
- `path:` - Local Python file, relative to YAML file location
- `package:` - Installed Python package via `importlib`

**Features**:
- Namespace prefixing prevents action name collisions
- Circular import detection (same module loaded once)
- Clear error messages with file/package name context
- Optional `__tea_actions__` metadata for version/description logging

### Basic Usage
```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("agent.yaml")
for event in graph.stream({"input": "hello"}):
    print(event)
```

### Built-in Actions
- `llm.call` - Call OpenAI-compatible LLM
- `http.get` / `http.post` - HTTP requests
- `file.read` / `file.write` - File operations (local and remote via fsspec)
- `actions.notify` - Notifications
- `checkpoint.save` / `checkpoint.load` - Checkpoint persistence

**Remote Storage Actions** (TEA-BUILTIN-004.1):
- `file.read` / `file.write` - Now support remote URIs (S3, GCS, Azure, etc.) via fsspec
- `storage.list` - List files/objects at path with optional detail
- `storage.exists` - Check if file/object exists
- `storage.delete` - Delete file/object
- `storage.copy` - Copy between locations (same or cross-provider)
- `storage.info` - Get file/object metadata
- `storage.mkdir` - Create directory/prefix
- `storage.native` - Access provider-specific operations

Supported URI schemes (via fsspec):
- `file:///path` - Local filesystem
- `s3://bucket/path` - AWS S3 (requires `s3fs`)
- `gs://bucket/path` - Google Cloud Storage / Firebase (requires `gcsfs`)
- `az://container/path` - Azure Blob Storage (requires `adlfs`)
- `memory://path` - In-memory filesystem (for testing)
- `http://` / `https://` - HTTP/HTTPS (read-only)

Required dependencies:
- `fsspec>=2023.1.0` - Core (included by default)
- `pip install s3fs` - For AWS S3
- `pip install gcsfs` - For GCS/Firebase
- `pip install adlfs` - For Azure

Remote Storage usage:
```python
# Read from S3
result = engine.actions_registry['file.read'](
    state={},
    path="s3://my-bucket/data/file.json"
)
# Returns: {"content": str, "success": True}

# Write to GCS
result = engine.actions_registry['file.write'](
    state={},
    path="gs://my-bucket/output/result.json",
    content='{"key": "value"}'
)
# Returns: {"path": str, "success": True}

# List S3 bucket contents
result = engine.actions_registry['storage.list'](
    state={},
    path="s3://my-bucket/data/",
    detail=True,  # Include file metadata
    max_results=100
)
# Returns: {"files": list, "count": int, "success": True}

# Check if file exists
result = engine.actions_registry['storage.exists'](
    state={},
    path="s3://my-bucket/data/file.json"
)
# Returns: {"exists": bool, "success": True}

# Copy across providers (S3 to GCS)
result = engine.actions_registry['storage.copy'](
    state={},
    source="s3://source-bucket/file.json",
    destination="gs://dest-bucket/file.json"
)
# Returns: {"copied": True, "success": True}

# Use caching for repeated reads
result = engine.actions_registry['file.read'](
    state={},
    path="s3://my-bucket/large-file.csv",
    cache="simple"  # Options: 'simple', 'file', 'block'
)

# Or use fsspec cache prefix directly
result = engine.actions_registry['file.read'](
    state={},
    path="simplecache::s3://my-bucket/large-file.csv"
)
```

Credential resolution (via SDK defaults):
- **AWS S3**: `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY`, IAM role, or `~/.aws/credentials`
- **GCS/Firebase**: `GOOGLE_APPLICATION_CREDENTIALS` or Application Default Credentials
- **Azure**: `AZURE_STORAGE_CONNECTION_STRING` or `AZURE_STORAGE_ACCOUNT`/`AZURE_STORAGE_KEY`

All storage actions are available via dual namespaces: `storage.*` and `actions.storage_*`.

**Data Processing Actions** (TEA-BUILTIN-003.2):
- `json.parse` - Parse JSON strings to Python objects (supports non-strict mode)
- `json.transform` - Transform data with JMESPath/JSONPath expressions
- `json.stringify` - Convert Python objects to JSON strings
- `csv.parse` - Parse CSV from text or file (configurable delimiters/headers)
- `csv.stringify` - Convert lists to CSV strings
- `data.validate` - Validate data against JSON Schema
- `data.merge` - Merge multiple dicts (deep/shallow/replace strategies)
- `data.filter` - Filter lists with predicate expressions

**Code Execution Actions** (TEA-BUILTIN-003.1):
- `code.execute` - Execute Python code in RestrictedPython sandbox
- `code.sandbox` - Manage persistent sandbox sessions for multi-step execution

**SECURITY WARNING**: Code execution is DISABLED by default. Only enable for trusted code patterns.
RestrictedPython provides bytecode transformation, not true process isolation.
Do NOT use for arbitrary LLM-generated code.

Required dependency:
- `pip install RestrictedPython`

Code Execution usage:
```python
# Enable code execution (required - disabled by default)
engine = YAMLEngine(enable_code_execution=True)

# Execute simple Python code
result = engine.actions_registry['code.execute'](
    state={},
    code="x = 1 + 2; result = x * 10",
    timeout=30,  # seconds
    max_output_bytes=65536
)
# Returns: {"success": True, "stdout": str, "stderr": str, "return_value": 30, "execution_time_ms": float}

# Capture print output
result = engine.actions_registry['code.execute'](
    state={},
    code="print('Hello, World!')"
)
# Returns: {"success": True, "stdout": "Hello, World!\n", "return_value": None, ...}

# Persistent sandbox sessions
result = engine.actions_registry['code.sandbox'](
    state={}, action="create"
)
sandbox_id = result['sandbox_id']

# Variables persist across executions
engine.actions_registry['code.sandbox'](
    state={}, action="execute", sandbox_id=sandbox_id,
    code="counter = 10"
)
result = engine.actions_registry['code.sandbox'](
    state={}, action="execute", sandbox_id=sandbox_id,
    code="counter += 5; result = counter"
)
# Returns: {"success": True, "return_value": 15, ...}

# Cleanup
engine.actions_registry['code.sandbox'](
    state={}, action="destroy", sandbox_id=sandbox_id
)
```

Security model (whitelist approach):
- **Allowed**: Math operations, type conversions, iteration, list/dict operations
- **Blocked**: imports, file access, network, exec/eval, dangerous dunders
- Timeout prevents infinite loops
- Output size limits prevent memory exhaustion

All code actions are available via dual namespaces: `code.*` and `actions.code_*`.

**Observability Actions** (TEA-BUILTIN-001.3):
- `trace.start` - Start a trace span with name, metadata, and optional parent
- `trace.log` - Log events, metrics, and state snapshots within a span
- `trace.end` - End the current span with status and optional error details

**Memory Actions** (TEA-BUILTIN-001.1):
- `memory.store` - Store key-value pairs with optional TTL and namespace
- `memory.retrieve` - Retrieve values by key with optional default fallback
- `memory.summarize` - Summarize conversation history using LLM to fit token windows

**Long-Term Memory Actions** (TEA-BUILTIN-001.4):
- `ltm.store` - Store key-value pairs persistently with optional metadata
- `ltm.retrieve` - Retrieve values by key from persistent storage
- `ltm.delete` - Delete a key from persistent storage
- `ltm.search` - Full-text search across stored values using FTS5

Long-term memory provides persistent storage using SQLite with FTS5 full-text search.
Unlike session memory, data persists across engine restarts when using file-based storage.

LTM Backends:
- **SQLiteBackend**: File-based or in-memory SQLite with FTS5 search (default)
- **LitestreamBackend**: SQLite with continuous cloud replication (zero-downtime backup)
- **BlobSQLiteBackend**: SQLite on blob storage with distributed locking (serverless-friendly)

Cloud-Native Backend Decision Matrix:
| Use Case | Backend | Why |
|----------|---------|-----|
| Local development | SQLiteBackend | Simple, no deps |
| Single-region serverless | LitestreamBackend | Continuous backup, fast recovery |
| Multi-region low-concurrency | BlobSQLiteBackend | Download-lock-use-upload pattern |
| High-concurrency serverless | Turso (planned) | HTTP-based libSQL |

LTM Actions usage:
```python
# Store with metadata
result = engine.actions_registry['ltm.store'](
    state={},
    key="user_profile",
    value={"name": "Alice", "preferences": ["coding", "music"]},
    metadata={"type": "profile", "version": 1}
)
# Returns: {"success": True, "stored": True, "key": "user_profile", "created": True}

# Retrieve a value
result = engine.actions_registry['ltm.retrieve'](
    state={},
    key="user_profile"
)
# Returns: {"success": True, "value": {...}, "found": True, "metadata": {...}}

# Retrieve with default for missing key
result = engine.actions_registry['ltm.retrieve'](
    state={},
    key="nonexistent",
    default={"empty": True}
)
# Returns: {"success": True, "value": {"empty": True}, "found": False, "metadata": None}

# Delete a key
result = engine.actions_registry['ltm.delete'](
    state={},
    key="old_data"
)
# Returns: {"success": True, "deleted": True, "key": "old_data"}

# Full-text search
result = engine.actions_registry['ltm.search'](
    state={},
    query="coding music",
    limit=10
)
# Returns: {"success": True, "results": [...], "count": int}

# Search with metadata filter
result = engine.actions_registry['ltm.search'](
    state={},
    metadata_filter={"type": "profile"},
    limit=5
)
# Returns: {"success": True, "results": [...], "count": int}
```

LTM configuration in YAMLEngine:
```python
from the_edge_agent import YAMLEngine, SQLiteBackend

# Default in-memory LTM
engine = YAMLEngine()

# File-based persistent LTM
engine = YAMLEngine(ltm_path="./agent_memory.db")

# Custom backend injection
custom_backend = SQLiteBackend("./custom.db")
engine = YAMLEngine(ltm_backend=custom_backend)

# Disable LTM
engine = YAMLEngine(enable_ltm=False)
```

Cloud-Native LTM Backends:
```python
from the_edge_agent.memory import (
    LitestreamBackend,
    BlobSQLiteBackend,
    create_ltm_backend,
)

# Litestream: SQLite with cloud replication
# Requires: Litestream CLI installed (https://litestream.io)
litestream = LitestreamBackend(
    db_path="./agent_memory.db",
    replica_url="s3://my-bucket/replicas/agent"
)
# Auto-restores from replica on init if local file missing
result = litestream.store("key", {"data": "value"})
config_yaml = litestream.generate_config()  # For litestream daemon

# Blob SQLite: Download-Lock-Use-Upload pattern
# Requires: fsspec + lock backend (firestore or redis)
# pip install fsspec gcsfs firebase-admin  # for GCS + Firestore lock
blob_backend = BlobSQLiteBackend(
    blob_uri="gs://my-bucket/agent_memory.db",
    lock_backend="firestore",  # or "redis"
    lock_ttl=300  # 5 minutes
)
result = blob_backend.store("key", "value")
blob_backend.sync()  # Manual sync (auto on close)
blob_backend.refresh_lock()  # Extend lock TTL during long ops
blob_backend.close()  # Uploads changes and releases lock

# Factory pattern for backend selection
backend = create_ltm_backend("sqlite", db_path="./memory.db")
backend = create_ltm_backend("litestream", db_path="./memory.db", replica_url="s3://...")
backend = create_ltm_backend("blob-sqlite", blob_uri="gs://...", lock_backend="redis")
```

Distributed Locks (for BlobSQLiteBackend):
```python
from the_edge_agent.memory.locks import FirestoreLock, RedisLock, create_lock

# Firestore lock (serverless-friendly, no infrastructure)
# Requires: pip install firebase-admin
lock = FirestoreLock(
    resource_id="agent_memory.db",
    ttl=300,  # 5 minutes
    collection="distributed_locks",
    project_id="my-project"  # Optional, uses ADC default
)

# Redis lock (self-hosted, lower latency)
# Requires: pip install redis
lock = RedisLock(
    resource_id="agent_memory.db",
    ttl=300,
    redis_url="redis://localhost:6379"
)

# Context manager usage
with lock:
    # Exclusive access to resource
    do_work()

# Manual acquire/release
result = lock.acquire(timeout=30.0)
if result['success']:
    try:
        do_work()
        lock.refresh()  # Extend TTL during long operations
    finally:
        lock.release()
```

All LTM actions are available via dual namespaces: `ltm.*` and `actions.ltm_*`.

**Graph Database Actions** (TEA-BUILTIN-001.4):
- `graph.store_entity` - Store entities with type, properties, and optional embeddings
- `graph.store_relation` - Create relationships between entities
- `graph.query` - Execute Cypher (Kuzu) or Datalog (CozoDB) queries against the graph
- `graph.retrieve_context` - Retrieve contextual information for an entity via N-hop traversal

Graph actions provide entity-relationship storage using pluggable backends:
- **CozoBackend**: Datalog queries, HNSW vector search (optional: pip install 'pycozo[embedded]')
- **KuzuBackend (Bighorn)**: Cypher queries, cloud httpfs support (optional: pip install kuzu)

When no graph database is installed, actions return informative error messages with graceful degradation.

Required dependencies (both optional, auto-selected):
- `pip install 'pycozo[embedded]'` - For CozoDB graph backend (Datalog, HNSW)
- `pip install kuzu` - For Kuzu graph backend (Cypher, cloud httpfs)
- Bighorn (Kuzu fork by Kineviz): `pip install git+https://github.com/Kineviz/bighorn.git`

Graph Actions usage:
```python
# Store an entity
result = engine.actions_registry['graph.store_entity'](
    state={},
    entity_id="user_123",
    entity_type="User",
    properties={"name": "Alice", "role": "admin"}
)
# Returns: {"success": True, "entity_id": "user_123", "type": "User", "created": True}

# Store a relation
result = engine.actions_registry['graph.store_relation'](
    state={},
    from_entity="user_123",
    to_entity="project_456",
    relation_type="owns",
    properties={"since": "2024-01-01"}
)
# Returns: {"success": True, "from": "user_123", "to": "project_456", "type": "owns"}

# Cypher query (KuzuBackend)
result = engine.actions_registry['graph.query'](
    state={},
    cypher="MATCH (e:Entity {id: 'user_123'}) RETURN e.id, e.type, e.properties"
)
# Returns: {"success": True, "results": [...], "count": 1, "query": str}

# Datalog query (CozoBackend)
result = engine.actions_registry['graph.query'](
    state={},
    datalog="?[name] := *entity{entity_id: 'user_123', properties: props}, name = get(props, 'name')"
)
# Returns: {"success": True, "results": [{"name": "Alice"}], "count": 1}

# Pattern query (works with both backends)
result = engine.actions_registry['graph.query'](
    state={},
    pattern={"entity_type": "User"}
)
# Returns: {"success": True, "results": [...], "count": int}

# Retrieve entity context with N-hop traversal
result = engine.actions_registry['graph.retrieve_context'](
    state={},
    entity_id="user_123",
    hops=2,
    limit=20
)
# Returns: {"success": True, "entities": [...], "relations": [...], "context_summary": str}
```

Graph configuration in YAMLEngine:
```python
from the_edge_agent import YAMLEngine

# Auto-select backend (CozoDB if available, else Kuzu)
engine = YAMLEngine()

# Explicitly use Kuzu/Bighorn backend
engine = YAMLEngine(graph_backend_type='kuzu')
# Or:
engine = YAMLEngine(graph_backend_type='bighorn')

# Explicitly use CozoDB backend
engine = YAMLEngine(graph_backend_type='cozo')

# File-based persistent graph
engine = YAMLEngine(graph_path="./agent_graph.db")

# Disable graph
engine = YAMLEngine(enable_graph=False)
```

**KuzuBackend Cloud Storage** (Bighorn extension):

KuzuBackend supports direct cloud I/O via the httpfs extension for serverless deployments:
```python
from the_edge_agent import KuzuBackend, BighornBackend

# BighornBackend is an alias for KuzuBackend
backend = KuzuBackend("./graph.kuzu")

# Load data from cloud storage
result = backend.load_from_cloud("s3://bucket/data.parquet")
# Returns: {"success": True, "loaded": True, "uri": str} or {"success": False, "error": str}

# Export to cloud storage
result = backend.save_to_cloud("s3://bucket/output.parquet", query="MATCH (e:Entity) RETURN e.*")
# Returns: {"success": True, "saved": True, "uri": str}

# Note: Azure (az://) is read-only
```

Supported cloud URIs (requires httpfs extension):
- `s3://bucket/path` - AWS S3
- `gs://bucket/path` - Google Cloud Storage (via HMAC)
- `az://container/path` - Azure Blob (read-only)
- `http://`, `https://` - HTTP/HTTPS

Cloud credentials via environment:
- **AWS S3**: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_REGION`
- **GCS**: `GCS_ACCESS_KEY_ID`, `GCS_SECRET_ACCESS_KEY` (HMAC mode)

All graph actions are available via dual namespaces: `graph.*` and `actions.graph_*`.

**LLM Enhanced Actions** (TEA-BUILTIN-001.2):
- `llm.call` - LLM completion call with optional retry logic (max_retries parameter)
- `llm.stream` - Stream LLM responses with chunk aggregation
- `llm.retry` - **DEPRECATED** - Use llm.call with max_retries parameter (will be removed in v0.9.0)
- `llm.tools` - Function/tool calling with automatic action dispatch

**Web Actions** (TEA-BUILTIN-002.1):
- `web.scrape` - Scrape web content via Firecrawl API (returns LLM-ready markdown)
- `web.crawl` - Crawl multiple pages from a URL via Firecrawl API
- `web.search` - Web search via Perplexity API

Web actions use external API delegation for Firebase Cloud Functions compatibility:
- **Firecrawl** (https://firecrawl.dev): Handles scraping/crawling with JS rendering
- **Perplexity** (https://perplexity.ai): Handles web search

Required environment variables:
- `FIRECRAWL_API_KEY` - For web.scrape and web.crawl
- `PERPLEXITY_API_KEY` - For web.search

Web Actions usage:
```python
# Scrape a single page
result = engine.actions_registry['web.scrape'](
    state={},
    url="https://example.com",
    formats=["markdown", "links"],
    only_main_content=True
)
# Returns: {"success": True, "markdown": str, "links": list, "metadata": dict}

# Scrape with structured extraction
result = engine.actions_registry['web.scrape'](
    state={},
    url="https://example.com/products",
    extract_schema={
        "type": "object",
        "properties": {
            "products": {"type": "array", "items": {"type": "object"}}
        }
    }
)
# Returns: {"success": True, "markdown": str, "extract": dict, ...}

# Crawl multiple pages
result = engine.actions_registry['web.crawl'](
    state={},
    url="https://docs.example.com",
    max_depth=2,
    limit=10,
    include_paths=["/docs/*"]
)
# Returns: {"success": True, "pages": list, "total_pages": int, "job_id": str}

# Web search
result = engine.actions_registry['web.search'](
    state={},
    query="AI developments 2025",
    num_results=5
)
# Returns: {"success": True, "results": list, "answer": str, "query": str}
```

All web actions are available via dual namespaces: `web.*` and `actions.web_*`.

**RAG Actions** (TEA-BUILTIN-002.2):
- `embedding.create` - Generate embeddings from text (OpenAI or Ollama)
- `vector.store` - Store documents with embeddings in vector store
- `vector.query` - Semantic similarity search with metadata filtering

RAG actions support pluggable embedding providers:
- **OpenAI** (remote/local compatible API): text-embedding-3-small, text-embedding-3-large, ada-002
- **Ollama** (local): nomic-embed-text, mxbai-embed-large, all-minilm, bge-m3

Vector stores are pluggable:
- **InMemoryVectorStore**: Zero dependencies, pure Python (default)
- **ChromaVectorStore**: Persistent storage (requires: pip install chromadb)

Required environment variables:
- `OPENAI_API_KEY` - For OpenAI embedding provider

RAG Actions usage:
```python
# Create embedding
result = engine.actions_registry['embedding.create'](
    state={},
    text="Hello world",
    model="text-embedding-3-small"
)
# Returns: {"embedding": List[float], "model": str, "dimensions": int}

# Batch embedding
result = engine.actions_registry['embedding.create'](
    state={},
    text=["Text 1", "Text 2", "Text 3"]
)
# Returns: {"embeddings": List[List[float]], "model": str, "count": int}

# Store documents (auto-generates embeddings)
result = engine.actions_registry['vector.store'](
    state={},
    texts=["Doc 1", "Doc 2", "Doc 3"],
    metadata=[{"type": "article"}, {"type": "blog"}, {"type": "article"}],
    collection="my_collection"
)
# Returns: {"stored": int, "collection": str, "ids": List[str]}

# Query with metadata filter
result = engine.actions_registry['vector.query'](
    state={},
    query="search text",
    k=5,
    collection="my_collection",
    filter={"type": "article"}
)
# Returns: {"results": [{"id": str, "text": str, "score": float, "metadata": dict}], ...}

# Use Ollama instead of OpenAI
result = engine.actions_registry['embedding.create'](
    state={},
    text="Hello world",
    provider="ollama",
    model="nomic-embed-text"
)
```

RAG configuration in YAML:
```yaml
settings:
  rag:
    embedding_provider: openai  # or "ollama"
    embedding_model: text-embedding-3-small
    # ollama_base_url: http://localhost:11434  # for Ollama
    vector_store: memory  # or "chroma"
    chroma_path: ./chroma_db  # for persistent Chroma
```

All RAG actions are available via dual namespaces: `embedding.*`, `vector.*` and `actions.embedding_*`, `actions.vector_*`.

**Tools Bridge Actions** (TEA-BUILTIN-002.3):
- `tools.crewai` - Execute CrewAI tools (700+ available)
- `tools.mcp` - Execute MCP (Model Context Protocol) server tools
- `tools.langchain` - Execute LangChain tools
- `tools.discover` - Discover available tools from all sources

Tools bridge actions provide access to external tool ecosystems without writing Python code. All bridges are optional - they gracefully degrade if dependencies are not installed.

Required dependencies (all optional):
- `pip install crewai crewai-tools` - For CrewAI bridge
- `pip install mcp` - For MCP bridge
- `pip install langchain langchain-community` - For LangChain bridge

Tools Bridge Actions usage:
```python
# Use a CrewAI tool
result = engine.actions_registry['tools.crewai'](
    state={},
    tool="SerperDevTool",
    query="AI news 2025"
)
# Returns: {"result": str, "tool": str, "success": True}

# Use an MCP server tool
result = engine.actions_registry['tools.mcp'](
    state={},
    server={"command": "npx", "args": ["-y", "@anthropic/mcp-server-filesystem"]},
    tool="read_file",
    path="/tmp/test.txt"
)
# Returns: {"result": str, "tool": str, "server": str, "success": True}

# Use a LangChain tool
result = engine.actions_registry['tools.langchain'](
    state={},
    tool="DuckDuckGoSearchRun",
    query="Python programming"
)
# Returns: {"result": str, "tool": str, "success": True}

# Discover available tools
result = engine.actions_registry['tools.discover'](
    state={},
    source="all",  # "crewai", "mcp", "langchain", or "all"
    filter="search"  # Optional filter
)
# Returns: {"tools": list, "sources": list, "count": int, "success": True}
```

Tools configuration in YAML:
```yaml
settings:
  tools:
    crewai:
      enabled: true
      tools: [SerperDevTool, ScrapeWebsiteTool]
    mcp:
      servers:
        - name: filesystem
          command: npx
          args: ["-y", "@anthropic/mcp-server-filesystem"]
    langchain:
      enabled: true
      tools: [DuckDuckGoSearchRun, WikipediaQueryRun]
```

All tools bridge actions are available via dual namespaces: `tools.*` and `actions.tools_*`.

LLM Enhanced Actions usage:
```python
# Basic LLM call (no retry)
result = engine.actions_registry['llm.call'](
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}]
)
# Returns: {"content": str, "usage": dict}

# LLM call with retry logic (for standalone/sequential use)
result = engine.actions_registry['llm.call'](
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}],
    max_retries=3,  # Full exponential backoff with Retry-After support
    base_delay=1.0,
    max_delay=60.0
)
# Returns: {"content": str, "usage": dict, "attempts": int, "total_delay": float}

# LLM call in parallel flows (max_retries=0 by default)
# Respects Retry-After header once, then relies on flow-level retry
result = engine.actions_registry['llm.call'](
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}],
    max_retries=0  # Default - single Retry-After attempt, no nested retry
)

# Streaming (aggregates chunks, returns final result)
result = engine.actions_registry['llm.stream'](
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}]
)
# Returns: {"content": str, "usage": dict, "streamed": True, "chunk_count": int}

# Tool/function calling with action dispatch
result = engine.actions_registry['llm.tools'](
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Search for AI news"}],
    tools=[{
        "name": "search",
        "description": "Search the web",
        "parameters": {"query": {"type": "string", "required": True}},
        "action": "web.search"  # Maps to registered action
    }]
)
# Returns: {"content": str, "tool_calls": list, "tool_results": list, "rounds": int}

# DEPRECATED: llm.retry (use llm.call with max_retries instead)
result = engine.actions_registry['llm.retry'](  # Shows deprecation warning
    state={},
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}],
    max_retries=3
)
```

**Migration from llm.retry to llm.call:**
- Standalone retry: Use `llm.call` with `max_retries>0`
- Parallel flows: Use `llm.call` with `max_retries=0` (default) + ParallelConfig.retry_policy

All LLM enhanced actions are available via dual namespaces: `llm.*` and `actions.llm_*`.

Memory backends are pluggable (in-memory default, custom implementations supported):
```python
from the_edge_agent import YAMLEngine, InMemoryBackend

# Default in-memory backend
engine = YAMLEngine()

# Custom backend injection
custom_backend = InMemoryBackend()
engine = YAMLEngine(memory_backend=custom_backend)

# Memory state serialization for checkpoints
memory_state = engine.get_memory_state()
engine.restore_memory_state(memory_state)
```

Tracing is enabled by default. Configure exporters via YAMLEngine:
```python
from the_edge_agent import YAMLEngine, ConsoleExporter, FileExporter

# Console output
engine = YAMLEngine(trace_exporter="console", trace_verbose=True)

# File output (JSON lines)
engine = YAMLEngine(trace_exporter="file", trace_file="./traces.jsonl")

# Custom callback
engine = YAMLEngine(
    trace_exporter="callback",
    trace_callback=lambda span: print(f"Span: {span['name']}")
)

# Disable tracing
engine = YAMLEngine(enable_tracing=False)
```

### Custom Actions
```python
engine = YAMLEngine(actions_registry={
    "custom.action": lambda state, **kwargs: {"result": "custom"}
})
```

### Security Warning
YAML files execute arbitrary Python code via `exec()` and `eval()`. Only load YAML from trusted sources.

## Checkpoint Persistence

The Edge Agent supports checkpoint persistence for saving and resuming workflow execution. Checkpointing is required when using interrupts.

### Checkpointer Types

```python
# In-memory checkpointer (for testing and simple use cases)
from the_edge_agent import MemoryCheckpointer
checkpointer = MemoryCheckpointer()

# File-based checkpoints (for persistent storage)
checkpoint_dir = "/tmp/checkpoints"
```

### Stop/Resume with Interrupts (LangGraph-Compatible)

Interrupts now STOP execution completely and require explicit resume:

```python
from the_edge_agent import StateGraph, MemoryCheckpointer

graph = StateGraph({"value": int, "approved": bool})
graph.add_node("node_a", run=node_a_func)
graph.add_node("node_b", run=node_b_func)  # Needs approval
graph.set_entry_point("node_a")
graph.add_edge("node_a", "node_b")
graph.set_finish_point("node_b")

# Compile with checkpointer - REQUIRED when using interrupts
checkpointer = MemoryCheckpointer()
graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)

# First execution - stops at node_b
events = list(graph.invoke({"value": 1}))
interrupt_event = events[-1]
# interrupt_event = {"type": "interrupt", "node": "node_b", "state": {...}, "checkpoint_path": "..."}

# Resume with state update (human-in-the-loop pattern)
# Pass new state values to merge into checkpoint state
resume_events = list(graph.invoke({"approved": True}, checkpoint=interrupt_event["checkpoint_path"]))
# State now has both value=1 AND approved=True
```

### Auto-Save at Interrupts

```python
# With file-based checkpoints
graph.compile(
    interrupt_before=["node_b"],
    checkpoint_dir="/tmp/checkpoints"
)

# With in-memory checkpointer
graph.compile(
    interrupt_before=["node_b"],
    checkpointer=MemoryCheckpointer()
)

# Checkpoints are saved automatically when interrupt is triggered
# File checkpoint format: {node}_{timestamp_ms}.pkl
```

### Checkpoint with Parallel Flows

- Checkpoints capture main thread state only
- At fan-in nodes, `parallel_results` is included in the saved state
- Parallel branch intermediate states are NOT captured individually

### Resumption Behavior

- `interrupt_before`: Resume re-executes the interrupted node
- `interrupt_after`: Resume continues to the next node (doesn't re-execute)
- **State update on resume**: `invoke({"key": "val"}, checkpoint=path)` merges new state into checkpoint
- Config can be overridden: `invoke({"key": "val"}, checkpoint=path, config={"cfg": "val"})`

### Human-in-the-Loop Pattern

```python
# 1. Run until interrupt (needs human decision)
events = list(graph.invoke({"data": "input"}))
checkpoint = events[-1]["checkpoint_path"]
state = events[-1]["state"]

# 2. Human reviews state and makes decision...
print(f"Review required: {state}")
user_decision = {"approved": True, "feedback": "Looks good"}

# 3. Resume with human input merged into state
result = list(graph.invoke(user_decision, checkpoint=checkpoint))
# Resumed state has original data + user_decision merged in
```

## Important Implementation Details

- Always use `tea.END` constant instead of string "__end__" when checking for END node
- Thread safety: parallel flows get deep copies of state to avoid conflicts
- Fan-in nodes receive `parallel_results` parameter containing list of states from parallel flows
- Edge conditions are re-evaluated on each traversal (allows for dynamic routing)
- Graph compilation is required before execution to set interrupt points
- Checkpoint files use pickle format (version 1.0)
