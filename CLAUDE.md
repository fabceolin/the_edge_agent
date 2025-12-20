# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Edge Agent (tea) is a lightweight, single-app state graph library inspired by LangGraph, designed for edge computing environments. The core architecture is built around a `StateGraph` class that manages state-driven workflows with support for:

- Sequential and conditional node execution
- Parallel fan-out/fan-in patterns using ThreadPoolExecutor
- Checkpoint persistence for save/resume of workflow execution
- LLM-agnostic integration (works with any language model)
- Graph visualization using NetworkX and Graphviz
- Declarative YAML-based agent configuration

## Documentation Index

| Topic | Location |
|-------|----------|
| **Development Setup** | [`docs/architecture/development-guide.md`](docs/architecture/development-guide.md) |
| **Tech Stack & Architecture** | [`docs/architecture/tech-stack.md`](docs/architecture/tech-stack.md) |
| **Coding Standards** | [`docs/architecture/coding-standards.md`](docs/architecture/coding-standards.md) |
| **Source Tree** | [`docs/architecture/source-tree.md`](docs/architecture/source-tree.md) |
| **Checkpoint Persistence** | [`docs/architecture/checkpoint-guide.md`](docs/architecture/checkpoint-guide.md) |
| **YAML Agent Reference** | [`docs/YAML_REFERENCE.md`](docs/YAML_REFERENCE.md) |

## Quick Commands

```bash
# Install in development mode
pip install -e .[dev]

# Run all tests
pytest

# Run with verbose output
pytest -v

# Run specific test file
pytest tests/test_stategraph.py

# Build package
python setup.py sdist bdist_wheel
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

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("agent.yaml")
for event in graph.stream({"input": "hello"}):
    print(event)
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

## Built-in Actions (Summary)

Full documentation in [`docs/YAML_REFERENCE.md`](docs/YAML_REFERENCE.md).

| Category | Actions |
|----------|---------|
| **LLM** | `llm.call`, `llm.stream`, `llm.tools` |
| **HTTP** | `http.get`, `http.post` |
| **File** | `file.read`, `file.write` |
| **Storage** | `storage.list`, `storage.copy`, `storage.delete` |
| **Data** | `json.parse`, `json.transform`, `csv.parse`, `data.validate` |
| **Code** | `code.execute`, `code.sandbox` |
| **Memory** | `memory.store`, `memory.retrieve`, `ltm.*` |
| **Vector** | `embedding.create`, `vector.store`, `vector.query` |
| **Web** | `web.scrape`, `web.crawl`, `web.search` |
| **Graph** | `graph.store_entity`, `graph.query` |
| **Trace** | `trace.start`, `trace.log`, `trace.end` |

## Security Warning

YAML files execute arbitrary Python code via `exec()` and `eval()`. Only load YAML from trusted sources.

## Important Notes

- Always use `tea.END` constant instead of string `"__end__"` when checking for END node
- Thread safety: parallel flows get deep copies of state to avoid conflicts
- Fan-in nodes receive `parallel_results` parameter containing list of states from parallel flows
- Edge conditions are re-evaluated on each traversal (allows for dynamic routing)
- Graph compilation is required before execution to set interrupt points
- A checkpointer is required when using interrupts
