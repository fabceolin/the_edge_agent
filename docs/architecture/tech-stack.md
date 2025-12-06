# Tech Stack

This document describes the technology stack used in The Edge Agent (tea) project.

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
| openai | LLM API integration | `llm.call` action in YAML engine |
| requests | HTTP requests | `http.get`, `http.post` actions in YAML engine |

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

## Python Version Compatibility

The codebase uses features available in Python 3.7+:

- Type hints (`typing` module)
- `concurrent.futures` for thread pools
- f-strings for string formatting
- Generator functions with `yield`

## Build and Distribution

```bash
# Build package
python setup.py sdist bdist_wheel

# Install from GitHub
pip install git+https://github.com/fabceolin/the_edge_agent.git
```

## System Requirements

| Requirement | Details |
|-------------|---------|
| OS | Linux, macOS, Windows (with Graphviz) |
| Graphviz | Required for visualization features |
| Memory | Minimal - designed for edge computing |
