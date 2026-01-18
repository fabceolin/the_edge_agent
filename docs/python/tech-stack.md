# Tech Stack

This document describes the technology stack and core architecture of The Edge Agent (tea) project.

## Runtime Environment

| Component | Version | Notes |
|-----------|---------|-------|
| Python | >=3.7 | Minimum supported version |
| Package Manager | pip | Standard Python packaging |
| Build System | setuptools | Configured via `setup.py` and `pyproject.toml` |

## Core Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| networkx | 3.3 | Graph data structure for state machine representation |
| pygraphviz | 1.13 | Graph visualization and rendering |
| pyyaml | >=6.0 | YAML parsing for declarative agent configuration |

### NetworkX

The `StateGraph` class uses NetworkX's `DiGraph` (directed graph) internally to represent nodes and edges. This provides:

- Efficient graph traversal algorithms
- Node and edge attribute storage
- Built-in successor/predecessor queries

### PyGraphviz

Used for graph visualization through the `render_graphviz()` and `save_graph_image()` methods. Requires system-level Graphviz installation:

```bash
sudo apt-get install libgraphviz-dev graphviz -y
```

### PyYAML

Enables the YAML engine for declarative agent configuration. Uses `yaml.safe_load()` for parsing.

## Core Components

### StateGraph

**Location:** `src/the_edge_agent/stategraph.py`

The main class implementing the state machine. Key characteristics:

- Uses NetworkX DiGraph internally
- Special nodes: `START` (`"__start__"`) and `END` (`"__end__"`)
- Two execution modes: `invoke()` (returns final state) and `stream()` (yields intermediate states)

**Key Methods:**

| Method | Purpose |
|--------|---------|
| `add_node(node, run)` | Add a node with optional run function |
| `add_edge(in_node, out_node)` | Add unconditional edge |
| `add_conditional_edges(in_node, func, cond)` | Add conditional routing |
| `add_parallel_edge(in_node, out_node, fan_in_node)` | Add parallel execution edge |
| `add_fanin_node(node, run)` | Add fan-in node for collecting parallel results |
| `set_entry_point(node)` | Set starting node |
| `set_finish_point(node)` | Set ending node |
| `compile(interrupt_before, interrupt_after)` | Compile graph with interrupts |
| `invoke(input_state, config)` | Execute graph, yield final result |
| `stream(input_state, config)` | Execute graph, yield intermediate states |
| `render_graphviz()` | Generate visualization |
| `save_graph_image(filename)` | Save graph as image |

### Node Function Signatures

Node functions use flexible parameter introspection:

- Functions can accept any combination of: `state`, `config`, `node`, `graph`, `parallel_results`
- Parameters are introspected using `inspect.signature()` and auto-matched
- Functions must return a dictionary that updates the state
- Non-dict returns are wrapped as `{"result": value}`

```python
# All valid signatures:
def simple(state):
    return {"result": state["value"] * 2}

def with_config(state, config):
    return {"result": state["value"], "timeout": config.get("timeout")}

def fan_in(state, parallel_results):
    return {"combined": [r["data"] for r in parallel_results]}

def full_context(state, config, node, graph):
    return {"node_name": node, "total_nodes": len(graph.nodes)}
```

### Parallel Execution

**Components:**

| Component | Purpose |
|-----------|---------|
| `add_parallel_edge(in, out, fan_in, config)` | Define parallel flow |
| `add_fanin_node(node, run)` | Collect parallel results |
| `ThreadPoolExecutor` | Execute flows concurrently |
| `ParallelFlowResult` | Rich result wrapper |

**Execution Flow:**
1. Parallel edges fan out from source node
2. Each flow executes in separate thread
3. Flows run until reaching designated fan-in node
4. Results collected in `state['parallel_results']`

### Parallel Execution Reliability (TD.13)

Advanced reliability features for parallel flows:

| Component | Purpose |
|-----------|---------|
| `ParallelConfig` | Per-edge timeout, retry, circuit breaker config |
| `ParallelFlowResult` | Result with timing, retry count, circuit state |
| `RetryPolicy` | Exponential backoff with exception filtering |
| `CircuitBreaker` | Prevents cascade failures with half-open recovery |
| `ParallelFlowCallback` | Lifecycle event callbacks |

```python
from the_edge_agent import ParallelConfig, RetryPolicy, CircuitBreakerConfig

graph.add_parallel_edge(
    "start", "api_call", "fan_in",
    config=ParallelConfig(
        timeout_seconds=30.0,
        fail_fast=False,
        retry_policy=RetryPolicy(
            max_retries=3,
            base_delay=1.0,
            backoff_multiplier=2.0,
        ),
        circuit_breaker=CircuitBreakerConfig(
            failure_threshold=5,
            reset_timeout=30.0,
        ),
    )
)
```

### Conditional Routing

Route execution based on function output:

```python
graph.add_conditional_edges(
    "start",
    lambda state: state["value"] > 10,
    {True: "node_a", False: "node_b"}
)
```

- `cond` parameter maps function results to target nodes
- Condition functions use same parameter introspection as node functions
- Edge conditions are re-evaluated on each traversal

### YAMLEngine

**Location:** `src/the_edge_agent/yaml_engine.py`

Creates StateGraph instances from YAML configurations:

| Feature | Description |
|---------|-------------|
| Inline Python code | `run:` blocks with Python |
| Built-in actions | `llm.call`, `http.get`, etc. |
| Multi-step nodes | GitHub Actions-style steps |
| External imports | `imports:` section for custom modules |
| Template variables | `{{ state.key }}`, `{{ variables.key }}` |
| Filters | `| json`, `| upper`, `| lower` |

**Key Classes:**
- `DotDict` - Dictionary with attribute-style access
- `YAMLEngine` - Main engine for YAML parsing and graph creation

## Development Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| pytest | latest | Test runner |
| coverage | latest | Code coverage reporting |
| hypothesis | latest | Property-based testing |
| parameterized | 0.9.0 | Parameterized test cases |

Install development dependencies:

```bash
pip install -e .[dev]
```

## Optional Runtime Dependencies

These are imported dynamically when used:

| Package | Purpose | Required For |
|---------|---------|--------------|
| openai | LLM API integration | `llm.call` action |
| requests | HTTP requests | `http.get`, `http.post` actions |
| fsspec | Remote file access | S3, GCS, Azure storage actions |
| s3fs | AWS S3 support | `s3://` URIs |
| gcsfs | Google Cloud Storage | `gs://` URIs |
| adlfs | Azure Blob Storage | `az://` URIs |
| RestrictedPython | Sandboxed code execution | `code.execute` action |
| firebase-admin | Firebase/Firestore | Cloud memory backends (see [limitations](#firestore-catalog-limitations)) |
| duckdb | SQL queries and vectors | Search and tabular actions |
| opik | Observability | Opik trace exporter |
| chromadb | Vector store | Persistent RAG storage |

## Architecture Decisions

### LLM-Agnostic Design

The library does not bundle any LLM client. Instead:

1. Node functions can import and use any LLM library
2. The YAML engine provides optional `llm.call` action that uses OpenAI-compatible APIs
3. Users can register custom actions for other LLM providers

### Minimal Core Dependencies

The core `StateGraph` class only requires:
- `networkx` for graph representation
- `pygraphviz` for visualization
- Standard library modules: `inspect`, `typing`, `copy`, `threading`, `concurrent.futures`

### Thread Safety

Parallel execution uses `ThreadPoolExecutor` from the standard library:
- Deep copies of state for each parallel flow
- `threading.Lock` for synchronizing fan-in operations
- Configurable `max_workers` parameter

### State Immutability

State is always copied before modification:
- `input_state.copy()` at execution start
- `copy.deepcopy(state)` for parallel flows
- `state.copy()` when yielding events

## Python Version Compatibility

The codebase uses features available in Python 3.7+:

- Type hints (`typing` module)
- `concurrent.futures` for thread pools
- f-strings for string formatting
- Generator functions with `yield`
- `dataclasses` (for newer components)

## Build and Distribution

```bash
# Build package
python setup.py sdist bdist_wheel

# Install from GitHub
pip install git+https://github.com/fabceolin/the_edge_agent.git

# Install with extras
pip install the-edge-agent[firebase,opik]
```

## System Requirements

| Requirement | Details |
|-------------|---------|
| OS | Linux, macOS, Windows (with Graphviz) |
| Graphviz | Required for visualization features |
| Memory | Minimal - designed for edge computing |
| Network | Optional - only for remote actions |

## Module Architecture

```
┌─────────────────┐
│   __init__.py   │ ← Public API
└────────┬────────┘
         │ imports
         ▼
┌─────────────────┐     ┌─────────────────┐
│  stategraph.py  │ ←── │  yaml_engine.py │
│                 │     │                 │
│  - StateGraph   │     │  - YAMLEngine   │
│  - START, END   │     │  - DotDict      │
└─────────────────┘     └─────────────────┘
         │                      │
         ▼                      ▼
┌─────────────────┐     ┌─────────────────┐
│    networkx     │     │     pyyaml      │
│   pygraphviz    │     │   actions/*     │
│   threading     │     │   exporters/*   │
│ concurrent.fut. │     │   memory/*      │
└─────────────────┘     └─────────────────┘
```

## Actions Architecture

Built-in actions are organized by domain:

```
src/the_edge_agent/actions/
├── __init__.py           # Action registry
├── llm_actions.py        # LLM calls (llm.call, llm.stream, llm.tools)
├── http_actions.py       # HTTP requests
├── file_actions.py       # File I/O (local + remote)
├── storage_actions.py    # Cloud storage operations
├── data_actions.py       # JSON, CSV, validation
├── code_actions.py       # Sandboxed code execution
├── memory_actions.py     # Session memory
├── ltm_actions.py        # Long-term memory
├── cloud_memory_actions.py  # Firebase/cloud memory
├── vector_actions.py     # Embeddings and vector search
├── graph_actions.py      # Graph database operations
├── web_actions.py        # Web scraping and search
├── tools_actions.py      # External tool bridges
├── observability_actions.py  # Tracing
└── context_actions.py    # Context management
```

## Memory Backend Architecture

Pluggable backends for different storage needs:

```
src/the_edge_agent/memory/
├── __init__.py
├── blob/                 # Blob storage backends
│   ├── base.py          # Abstract BlobStorage
│   └── gcs.py           # Google Cloud Storage
├── metadata/            # Metadata store backends
│   ├── base.py          # Abstract MetadataStore
│   └── firestore.py     # Firestore implementation
├── query/               # Query engine backends
│   ├── base.py          # Abstract QueryEngine
│   └── duckdb.py        # DuckDB implementation
└── vector/              # Vector index backends
    ├── base.py          # Abstract VectorIndex
    └── duckdb.py        # DuckDB VSS implementation
```

## Firestore Catalog Limitations

When using Firestore as a catalog backend for DuckLake LTM, be aware of these architectural constraints:

| Limitation | Value | Impact |
|------------|-------|--------|
| **Write throughput** | ~1 write/s per document | The "current snapshot" document becomes a hotspot under concurrent writes |
| **Document size** | 1 MB max | Large metadata entries will fail (rare in practice) |

### Workload Suitability

| Workload | Firestore Catalog | Recommendation |
|----------|-------------------|----------------|
| Batch ETL (single writer) | ✅ Works well | Good fit |
| Multi-reader analytics | ✅ Excellent | Scales infinitely |
| Streaming ingestion | ⚠️ Bottleneck | Use SQLAlchemy/PostgreSQL |
| High-concurrency writes | ❌ ~1 TPS limit | Use SQLAlchemy/PostgreSQL |

**Best for:** Read-heavy workloads, serverless deployments, Firebase ecosystem integration.

**Avoid for:** Real-time streaming, concurrent data ingestion pipelines.

### Why This Happens

Firestore uses a consensus algorithm (Paxos/Raft) with synchronous indexing that limits sustained writes to ~1/second on any single document. The DuckLake protocol requires updating a "current snapshot" pointer on every commit, creating a serialization bottleneck when multiple writers compete for the same document.
