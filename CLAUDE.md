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
