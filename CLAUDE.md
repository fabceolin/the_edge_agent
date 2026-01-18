# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Edge Agent (tea) is a lightweight, single-app state graph library inspired by LangGraph, designed for edge computing environments. This is a **polyglot monorepo** with both Python and Rust implementations.

Core features:
- Sequential and conditional node execution
- Parallel fan-out/fan-in patterns
- Checkpoint persistence for save/resume of workflow execution
- LLM-agnostic integration (works with any language model)
- Declarative YAML-based agent configuration

## Repository Structure

```
the_edge_agent/
├── python/                 # Python implementation
│   ├── src/the_edge_agent/ # Source code
│   ├── tests/              # Python tests
│   ├── setup.py
│   └── pyproject.toml
│
├── rust/                   # Rust implementation
│   ├── src/                # Source code
│   ├── tests/              # Rust tests
│   └── Cargo.toml
│
├── examples/               # Shared YAML agents
├── docs/
│   ├── shared/             # Language-agnostic docs
│   ├── python/             # Python-specific docs
│   ├── rust/               # Rust-specific docs
│   ├── stories/            # Feature stories
│   └── qa/                 # QA documents
└── .github/workflows/      # CI/CD (python-tests.yaml, rust-tests.yaml)
```

## Documentation Index

| Topic | Location |
|-------|----------|
| **Core Concepts** | [`docs/shared/architecture/concepts.md`](docs/shared/architecture/concepts.md) |
| **YAML Reference** | [`docs/shared/YAML_REFERENCE.md`](docs/shared/YAML_REFERENCE.md) |
| **Checkpoint Guide** | [`docs/shared/architecture/checkpoint-guide.md`](docs/shared/architecture/checkpoint-guide.md) |
| **Python Getting Started** | [`docs/python/getting-started.md`](docs/python/getting-started.md) |
| **Python Dev Guide** | [`docs/python/development-guide.md`](docs/python/development-guide.md) |
| **Python Actions** | [`docs/python/actions-reference.md`](docs/python/actions-reference.md) |
| **Experiments Guide** | [`docs/python/experiments-guide.md`](docs/python/experiments-guide.md) |
| **Rust Getting Started** | [`docs/rust/getting-started.md`](docs/rust/getting-started.md) |
| **Rust Dev Guide** | [`docs/rust/development-guide.md`](docs/rust/development-guide.md) |
| **Rust Actions** | [`docs/rust/actions-reference.md`](docs/rust/actions-reference.md) |

## Quick Commands

### Python

```bash
# Install in development mode
cd python && pip install -e .[dev]

# Run all tests
cd python && pytest

# Run specific test file
cd python && pytest tests/test_stategraph.py
```

### Rust

```bash
# Build
cd rust && cargo build

# Run all tests
cd rust && cargo test

# Run with release optimizations
cd rust && cargo build --release
```

## Quick Start

### Python API

```python
import the_edge_agent as tea

# Create graph
graph = tea.StateGraph({"value": int, "result": str})
graph.add_node("process", run=lambda state: {"result": f"Processed: {state['value']}"})
graph.set_entry_point("process")
graph.set_finish_point("process")

# Execute
for event in graph.compile().invoke({"value": 42}):
    print(event)
```

### YAML Agent

```yaml
name: simple-agent
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

## Key Concepts

| Concept | Description |
|---------|-------------|
| **StateGraph** | Core class for building state-driven workflows |
| **Nodes** | Workflow steps with run functions |
| **Edges** | Transitions between nodes (simple, conditional, parallel) |
| **Fan-in** | Collects results from parallel flows |
| **Interrupts** | Pause points for human-in-the-loop workflows |
| **YAMLEngine** | Declarative agent configuration from YAML |

## Template Engine

YAML templates use **Jinja2** for variable interpolation (`{{ state.key }}`), providing:
- Familiar syntax from Flask, Ansible, and dbt
- Built-in conditionals (`{% if %}`) and loops (`{% for %}`)
- Filters: `| tojson`, `| fromjson`, `| upper`, `| lower`, `| default`, `| length`
- Object passthrough: single expressions return native Python objects
- Security: `__import__` and dangerous builtins are blocked in templates

## Security Warning

YAML files execute arbitrary Python code via `exec()` in `run:` blocks. Only load YAML from trusted sources. Template expressions (`{{ }}`) use Jinja2's sandboxed environment with improved security.

## Important Notes

- Always use `tea.END` constant instead of string `"__end__"` when checking for END node
- Thread safety: parallel flows get deep copies of state to avoid conflicts
- Fan-in nodes receive `parallel_results` parameter containing list of states from parallel flows
- Edge conditions are re-evaluated on each traversal (allows for dynamic routing)
- Graph compilation is required before execution to set interrupt points
- A checkpointer is required when using interrupts

## LTM Backend Selection Guide

The Edge Agent supports multiple Long-Term Memory (LTM) backends for persistent state storage:

### LTM Backends

| Backend | Use Case | Install |
|---------|----------|---------|
| **sqlite** (default) | Local development, single-node | Built-in |
| **sqlalchemy** | Database-agnostic (PostgreSQL, MySQL, SQLite, etc.) | `pip install sqlalchemy` |
| **duckdb** | Analytics-heavy, catalog-aware, cloud storage | `pip install duckdb fsspec` |
| **ducklake** | DuckDB with sensible defaults (TEA-LTM-010) | `pip install duckdb fsspec` |
| **litestream** | SQLite with S3 replication | `pip install litestream` |
| **blob-sqlite** | Distributed with blob storage | `pip install fsspec` |

### Catalog Backends (for DuckDB LTM)

| Catalog | Use Case | Install |
|---------|----------|---------|
| **sqlite** (default) | Local, development | Built-in |
| **sqlalchemy** | Database-agnostic (PostgreSQL, MySQL, SQLite, etc.) | `pip install sqlalchemy` |
| **duckdb** | Single-file, shared connection | `pip install duckdb` |
| **firestore** | Serverless, Firebase ecosystem | `pip install firebase-admin` |
| **postgres** | Self-hosted, SQL compatibility | `pip install psycopg2` |
| **supabase** | Edge, REST API, managed Postgres | `pip install requests` |

> ⚠️ **Firestore Catalog Limitations:** See [tech-stack.md](docs/python/tech-stack.md#firestore-catalog-limitations) for write throughput and document size constraints.

### Example YAML Configuration

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: ":memory:"
    storage:
      uri: "${LTM_STORAGE_PATH:-./ltm_data/}"
    inline_threshold: 1024
    lazy: true
```

#### Ducklake Backend (TEA-LTM-010)

The `ducklake` backend is an alias that expands to DuckDB with sensible defaults:

```yaml
settings:
  ltm:
    backend: ducklake  # Expands to duckdb with defaults below
    # Defaults applied:
    # catalog.type: sqlite
    # catalog.path: ./ltm_catalog.db
    # storage.uri: ./ltm_data/
    # lazy: true
    # inline_threshold: 4096
```

Override any default by specifying it explicitly:

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

#### Shared DuckDB Catalog (TEA-LTM-011)

For a fully self-contained single-file solution, use the DuckDB catalog with shared mode:

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: duckdb
      shared: true  # Uses same DB as storage
    storage:
      uri: "./ltm.duckdb"  # Single DuckDB file
    inline_threshold: 1024
```

#### SQLAlchemy Backend (TEA-LTM-012)

The `sqlalchemy` backend provides database-agnostic storage using any SQLAlchemy-supported database:

```yaml
# SQLAlchemy with PostgreSQL
settings:
  ltm:
    backend: sqlalchemy
    url: postgresql://user:pass@localhost/dbname
    pool_size: 10
    lazy: true
    catalog:
      type: sqlalchemy
      url: postgresql://user:pass@localhost/dbname

# SQLAlchemy with MySQL
settings:
  ltm:
    backend: sqlalchemy
    url: mysql+pymysql://user:pass@localhost/dbname
    pool_size: 5
    catalog:
      type: sqlalchemy
      url: mysql+pymysql://user:pass@localhost/dbname

# SQLAlchemy with SQLite (local dev)
settings:
  ltm:
    backend: sqlalchemy
    url: sqlite:///./ltm.db
    catalog:
      type: sqlalchemy
      url: sqlite:///./ltm.db
```

SQLAlchemy parameters:
- `url`: SQLAlchemy connection URL (required)
- `pool_size`: Connection pool size (default: 5)
- `echo`: Enable SQL logging (default: false)
- `lazy`: Defer connection until first use (default: false)

### Factory Functions

```python
from the_edge_agent.memory import (
    create_ltm_backend,
    create_catalog_backend,
    expand_env_vars,
    expand_ltm_config,
)

# Create DuckDB backend with SQLite catalog
backend = create_ltm_backend(
    "duckdb",
    catalog_config={"type": "sqlite", "path": ":memory:"},
    storage_uri="./ltm_data/",
    lazy=True
)

# Create catalog directly
catalog = create_catalog_backend("sqlite", path="./catalog.db")

# Create DuckDB catalog (TEA-LTM-011)
duckdb_catalog = create_catalog_backend("duckdb", path="./catalog.duckdb")

# Create SQLAlchemy backend (TEA-LTM-012)
sqlalchemy_backend = create_ltm_backend(
    "sqlalchemy",
    url="postgresql://user:pass@localhost/db",
    pool_size=10,
    lazy=True
)

# Create SQLAlchemy catalog (TEA-LTM-012)
sqlalchemy_catalog = create_catalog_backend(
    "sqlalchemy",
    url="sqlite:///./catalog.db"
)

# Expand ducklake config without creating backend (TEA-LTM-010)
config = expand_ltm_config({"ltm": {"backend": "ducklake"}})
# config["backend"] == "duckdb"
# config["catalog"]["type"] == "sqlite"

# Create SQLAlchemy backend (TEA-LTM-012)
sqlalchemy_backend = create_ltm_backend(
    "sqlalchemy",
    url="postgresql://user:pass@localhost/db",
    pool_size=10,
    lazy=True
)

# Create SQLAlchemy catalog
sqlalchemy_catalog = create_catalog_backend(
    "sqlalchemy",
    url="postgresql://user:pass@localhost/db"
)
```

## Entity Hierarchy (TEA-LTM-013)

The `EntityHierarchy` class provides hierarchical organization for LTM data using a closure table pattern, enabling O(1) queries across multi-tenant hierarchies (e.g., org → project → user → session).

### Why Closure Tables?

Without proper indexing, hierarchical queries require expensive recursive CTEs or multiple round trips. The closure table pre-computes all ancestor-descendant relationships:

```
Entity: org:acme → project:alpha → user:alice → session:s1

Closure Table:
ancestor_id      | descendant_id   | depth
-----------------|-----------------|-------
org:acme         | org:acme        | 0
org:acme         | project:alpha   | 1
org:acme         | user:alice      | 2
org:acme         | session:s1      | 3
...
```

Query "all entries for org:acme" becomes a simple index lookup.

### Quick Start

```python
from the_edge_agent.memory import EntityHierarchy

# Create hierarchy with configured levels
hierarchy = EntityHierarchy(
    levels=["org", "project", "user", "session"],
    url="sqlite:///hierarchy.db",  # or postgresql://...
)

# Register entities
hierarchy.register_entity("org", "acme")
hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
hierarchy.register_entity("user", "alice", parent=("project", "alpha"))
hierarchy.register_entity("session", "s1", parent=("user", "alice"))

# Associate LTM entries with entities
hierarchy.associate_entry("entry_key_123", "session", "s1")

# Query all entries for an org (includes all descendants)
result = hierarchy.get_entries_for_entity("org", "acme")
print(result["total_count"])  # All entries under org:acme

# Get ancestors/descendants
ancestors = hierarchy.get_ancestors("session", "s1")
# [user:alice, project:alpha, org:acme]

descendants = hierarchy.get_descendants("org", "acme")
# [project:alpha, user:alice, session:s1]

# Move entity to new parent (TEA-LTM-014)
# User changes teams: move alice from project:alpha to project:beta
hierarchy.register_entity("project", "beta", parent=("org", "acme"))
hierarchy.move_entity("user", "alice", ("project", "beta"))
# All descendants (sessions) move with alice, closure table is rebuilt atomically
```

### YAML Configuration

```yaml
settings:
  ltm:
    backend: sqlalchemy  # or any LTM backend
    url: postgresql://user:pass@localhost/dbname

    hierarchy:
      enabled: true
      levels: [org, project, user, session]
      root_entity:
        type: org
        id: "${ORG_ID}"
```

### API Methods

| Method | Description |
|--------|-------------|
| `register_entity(type, id, parent, metadata)` | Register entity in hierarchy |
| `associate_entry(entry_key, entity_type, entity_id)` | Link LTM entry to entity |
| `get_entries_for_entity(type, id, include_descendants)` | Query entries with O(1) lookup |
| `list_entities(entity_type, parent, limit)` | List entities with filtering |
| `get_ancestors(type, id)` | Get all ancestors (bottom-up) |
| `get_descendants(type, id)` | Get all descendants (top-down) |
| `move_entity(type, id, new_parent)` | Move entity to new parent, rebuilding closure |
| `delete_entity(type, id, cascade)` | Delete entity and optionally cascade |

### Level Validation

- Entity type must be in configured `levels` list
- Parent type must be exactly one level above child type
- Root entities (first level) cannot have parents
- Non-root entities must have parents

## Hierarchical LTM Backend (TEA-LTM-015)

The `HierarchicalLTMBackend` combines PostgreSQL catalog with hierarchical blob storage for 10GB-100GB+ scale with O(1) hierarchy queries.

### Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         A3 ARCHITECTURE                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    PostgreSQL Catalog                           │   │
│   │  ltm_catalog_entries (metadata, pointers to blob storage)       │   │
│   │  ltm_entities (org, project, user, session)                     │   │
│   │  ltm_entity_closure (hierarchy for O(1) queries)                │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                    │                                    │
│                                    ▼                                    │
│   ┌─────────────────────────────────────────────────────────────────┐   │
│   │                    Blob Storage (fsspec)                        │   │
│   │   gs://bucket/ltm/org:acme/project:alpha/user:alice/...         │   │
│   └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### YAML Configuration

```yaml
settings:
  ltm:
    backend: hierarchical

    catalog:
      type: sqlalchemy
      url: "${DATABASE_URL}"
      pool_size: 10

    storage:
      uri: "gs://my-bucket/ltm/"  # or s3://, file://, az://

    hierarchy:
      levels: [org, project, user, session]
      root_entity:
        type: org
        id: "${ORG_ID}"
      defaults:
        org: "default"
        project: "_unassigned"

    performance:
      metadata_cache:
        enabled: true
        ttl_seconds: 600
      parallel_reads:
        threads: 8

    index:
      format: parquet
      row_group_size: 122880
      compression: zstd
```

### Python API

```python
from the_edge_agent.memory import create_ltm_backend

# Create backend
backend = create_ltm_backend(
    "hierarchical",
    catalog_url="postgresql://user:pass@localhost/db",
    storage_uri="gs://bucket/ltm/",
    hierarchy_levels=["org", "project", "user", "session"],
    hierarchy_defaults={"org": "default", "project": "_unassigned"},
)

# Register entity hierarchy
backend.register_entity_with_defaults(
    "session", "s123",
    parents={"user": "alice", "project": "alpha", "org": "acme"}
)

# Store with entity association
backend.store(
    key="conversation:001",
    value={"messages": [...]},
    entity=("session", "s123"),
    metadata={"type": "chat"},
)

# Retrieve by key
result = backend.retrieve("conversation:001")

# Retrieve all entries for an entity and descendants
result = backend.retrieve_by_entity("project", "alpha")
# Returns all entries for project:alpha and all users/sessions

# Batch retrieval (parallel)
result = backend.retrieve_batch(["key1", "key2", "key3"])

# Compact Parquet indexes
backend.compact_indexes(max_deltas=100)

# Cleanup orphaned blobs
backend.cleanup_orphans(max_age_seconds=3600, dry_run=True)
```

### Performance Targets

| Operation | Target Latency | Mechanism |
|-----------|----------------|-----------|
| Query session entries | <100ms | Direct blob path |
| Query user entries | <200ms | PostgreSQL + blob storage |
| Query project entries | <500ms | Closure table + blob storage |
| Query org entries | <1s | Closure table + blob storage |
| Write new entry | <100ms | Blob + PostgreSQL upsert |

### Storage Flow

**Write:**
1. Resolve entity path from closure table
2. Write blob to storage at hierarchical path (if > inline_threshold)
3. Update PostgreSQL catalog
4. Update Parquet indexes (delta files)

**Read:**
1. Query closure table for all descendants
2. Query entry owners for matching entries
3. Batch retrieve from catalog
4. Fetch blobs from storage (parallel with metadata cache)

### Error Handling

| Exception | Cause | Recovery |
|-----------|-------|----------|
| `StorageError` | Blob write failed | Retry, no catalog update |
| `CatalogError` | PostgreSQL failed after blob success | Log orphan for cleanup |
| `ConnectionError` | Database connection failed | Retry with exponential backoff |

Orphan cleanup job runs periodically to remove blobs without catalog entries.

## Experiment Framework (TEA-BUILTIN-005.4)

TEA provides an experiment framework for systematically evaluating agents using [Comet Opik](https://www.comet.com/docs/opik/).

### Installation

```bash
pip install the-edge-agent[experiments]
```

### Quick Example

```python
from the_edge_agent.experiments import (
    run_tea_experiment,
    create_dataset_from_list,
    BaseTeaMetric,
    f1_score,
)

# Create dataset
create_dataset_from_list("test_cases", [
    {"input": {"text": "..."}, "expected_output": {"result": "..."}},
])

# Define metric
class AccuracyMetric(BaseTeaMetric):
    name = "accuracy"
    def score(self, output, expected_output=None, **kwargs):
        match = output.get("result") == expected_output.get("result")
        return self.make_result(1.0 if match else 0.0)

# Run experiment
run_tea_experiment(
    agent_yaml="agent.yaml",
    dataset_name="test_cases",
    metrics=[AccuracyMetric()],
    experiment_name="agent_v1.0",
)
```

### Scoring Helpers

| Function | Description |
|----------|-------------|
| `jaccard_similarity(set_a, set_b)` | Intersection over union (0.0-1.0) |
| `f1_score(actual, expected)` | Harmonic mean of precision/recall |
| `completeness_score(filled, total)` | Ratio of filled to total |

### CLI Usage

```bash
tea-experiments --agent agent.yaml --dataset test_cases --version 1.0
tea-experiments --agent agent.yaml --dataset test_cases --dry-run
```

For full documentation, see [`docs/python/experiments-guide.md`](docs/python/experiments-guide.md).
