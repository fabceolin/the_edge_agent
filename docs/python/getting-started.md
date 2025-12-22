# Getting Started with Python

This guide helps you get started with The Edge Agent Python implementation.

## Installation

### From PyPI

```bash
pip install the-edge-agent
```

### From Source

```bash
cd python/
pip install -e .
```

### With Optional Dependencies

```bash
# LLM support
pip install the-edge-agent[llm]

# RAG with ChromaDB
pip install the-edge-agent[rag-chroma]

# All features
pip install the-edge-agent[all]
```

## Quick Start

### Python API

```python
import the_edge_agent as tea

# Create a simple graph
graph = tea.StateGraph({"value": int, "result": str})

# Add a processing node
graph.add_node("process", run=lambda state: {
    "result": f"Processed: {state['value']}"
})

# Connect nodes
graph.set_entry_point("process")
graph.set_finish_point("process")

# Execute
for event in graph.compile().invoke({"value": 42}):
    print(event)
# Output: {'type': 'final', 'state': {'value': 42, 'result': 'Processed: 42'}}
```

### YAML Agent

Create a file `my_agent.yaml`:

```yaml
name: greeting-agent
state_schema:
  name: str
  greeting: str

nodes:
  - name: greet
    run: |
      return {"greeting": f"Hello, {state['name']}!"}

edges:
  - from: __start__
    to: greet
  - from: greet
    to: __end__
```

Run it:

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("my_agent.yaml")

for event in graph.stream({"name": "World"}):
    print(event)
```

## Next Steps

- [Development Guide](development-guide.md) - Full development setup
- [Coding Standards](coding-standards.md) - Code conventions
- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete YAML syntax
- [Actions Reference](actions-reference.md) - Built-in actions

## System Requirements

- Python 3.7+
- Optional: Graphviz (for visualization)

```bash
# Ubuntu/Debian
sudo apt-get install libgraphviz-dev graphviz -y

# macOS
brew install graphviz
```
