# Optional Dependencies Guide

The Edge Agent is designed with optional dependencies to minimize required installations. This document explains which features are optional and how to enable them.

## Core vs Optional Features

### Core Features (Always Available)
- StateGraph execution engine
- YAML configuration
- Checkpoint persistence
- Memory actions (in-memory backend)
- LLM actions (with API key)
- HTTP actions
- File operations (local filesystem)

### Optional Features

## 1. Graph Visualization (pygraphviz)

**Status**: ✅ **Already Optional**

### When Needed
- Visualizing StateGraph workflows
- Debugging graph structures
- Generating workflow diagrams

### Installation
```bash
# System dependencies (Ubuntu/Debian)
sudo apt-get install libgraphviz-dev graphviz

# Python package
pip install pygraphviz
```

### Usage
```python
from the_edge_agent import StateGraph

graph = StateGraph({"value": int})
# ... configure graph ...

# These methods require pygraphviz
graph.render_graphviz()  # Raises ImportError if not installed
graph.save_graph_image("graph.png")  # Raises ImportError if not installed
```

### Error Handling
- **When installed**: Methods work normally
- **When not installed**: Raises `ImportError` with installation instructions
- **Tests**: Automatically skip with `@pytest.mark.skipif(not HAS_PYGRAPHVIZ)`

### Test Status
```bash
# With pygraphviz installed
pytest tests/test_stategraph_core.py::TestStateGraph::test_render_graphviz -v
# PASSED

# Without pygraphviz installed
pytest tests/test_stategraph_core.py::TestStateGraph::test_render_graphviz -v
# SKIPPED (pygraphviz not installed)
```

---

## 2. Graph Database Backends (Kuzu / CozoDB)

**Status**: ✅ **Already Optional**

### When Needed
- Entity-relationship knowledge graphs
- Semantic search over entities
- Complex graph queries (Cypher/Datalog)
- RAG systems with graph memory

### Backends Available

#### Kuzu (Bighorn) - Cypher Queries
```bash
pip install kuzu
```

**Features**:
- Cypher query language
- Cloud storage support (S3/GCS/Azure via httpfs)
- Serverless-friendly
- High performance

**Usage**:
```python
from the_edge_agent import KuzuBackend

backend = KuzuBackend("./graph.kuzu")
backend.store_entity("e1", "Person", {"name": "Alice"})
backend.store_relation("e1", "e2", "KNOWS")
result = backend.query(cypher="MATCH (p:Entity) RETURN p.id")
backend.close()
```

#### CozoDB - Datalog Queries
```bash
pip install 'pycozo[embedded]'
```

**Features**:
- Datalog query language
- HNSW vector search
- In-memory or persistent
- Rule-based reasoning

**Usage**:
```python
from the_edge_agent import CozoBackend

backend = CozoBackend("./graph.db")
backend.store_entity("e1", "Person", {"name": "Alice"})
backend.store_relation("e1", "e2", "KNOWS")
result = backend.query(datalog="?[id, type] := *entity[id, type, _, _, _]")
backend.close()
```

### YAMLEngine Integration
```python
from the_edge_agent import YAMLEngine

# Auto-select backend (Kuzu preferred, falls back to CozoDB)
engine = YAMLEngine(enable_graph=True)

# Explicit backend selection
engine = YAMLEngine(graph_backend_type='kuzu')  # Use Kuzu
engine = YAMLEngine(graph_backend_type='cozo')  # Use CozoDB

# Disable graph features
engine = YAMLEngine(enable_graph=False)
```

### Error Handling
- **When installed**: Graph actions work normally
- **When not installed**: Actions return `{"success": False, "error": "...", "error_type": "dependency_missing"}`
- **Tests**: Automatically skip when backends not available

### Test Status
```bash
# With Kuzu installed
pytest tests/test_yaml_engine_kuzu.py -v
# 38 PASSED

# Without Kuzu installed
pytest tests/test_yaml_engine_kuzu.py -v
# 38 SKIPPED (kuzu not installed)

# With CozoDB installed
pytest tests/test_yaml_engine_ltm.py::TestCozoBackend -v
# PASSED

# Without CozoDB installed
pytest tests/test_yaml_engine_ltm.py::TestCozoBackend -v
# SKIPPED (CozoDB not installed)
```

### Graceful Degradation Test
```python
# This test verifies graceful degradation
# Skips if ANY graph backend is installed
pytest tests/test_yaml_engine_ltm.py::TestGraphActions::test_graph_graceful_degradation -v
# SKIPPED (Graph backend is installed (CozoDB or Kuzu), skipping graceful degradation test)
```

---

## 3. Other Optional Dependencies

### Code Execution (RestrictedPython)
```bash
pip install RestrictedPython
```

**Usage**:
```python
engine = YAMLEngine(enable_code_execution=True)  # Disabled by default
result = engine.actions_registry['code.execute'](
    {}, code="x = 1 + 2; result = x * 10"
)
```

**Security**: ⚠️ Only enable for trusted code. Not safe for arbitrary LLM-generated code.

### Cloud Storage (fsspec, s3fs, gcsfs, adlfs)
```bash
pip install fsspec s3fs      # AWS S3
pip install fsspec gcsfs      # Google Cloud Storage
pip install fsspec adlfs      # Azure Blob Storage
```

### RAG (ChromaDB)
```bash
pip install chromadb
```

### Web Actions (Firecrawl, Perplexity)
```bash
# Requires API keys, no installation needed
export FIRECRAWL_API_KEY="your_key"
export PERPLEXITY_API_KEY="your_key"
```

### Tools Bridges
```bash
pip install crewai crewai-tools    # CrewAI tools (700+)
pip install mcp                     # Model Context Protocol
pip install langchain langchain-community  # LangChain tools
```

---

## Dependency Matrix

| Feature | Package | Status | Tests Skip When Missing |
|---------|---------|--------|------------------------|
| Core StateGraph | (built-in) | ✅ Required | N/A |
| Graph Visualization | `pygraphviz` | ⚪ Optional | ✅ Yes |
| Kuzu Graph Backend | `kuzu` | ⚪ Optional | ✅ Yes |
| CozoDB Graph Backend | `pycozo[embedded]` | ⚪ Optional | ✅ Yes |
| Code Execution | `RestrictedPython` | ⚪ Optional | ✅ Yes |
| S3 Storage | `s3fs` | ⚪ Optional | ✅ Yes |
| GCS Storage | `gcsfs` | ⚪ Optional | ✅ Yes |
| Azure Storage | `adlfs` | ⚪ Optional | ✅ Yes |
| ChromaDB | `chromadb` | ⚪ Optional | ✅ Yes |
| CrewAI Tools | `crewai` | ⚪ Optional | ✅ Yes |
| MCP Tools | `mcp` | ⚪ Optional | ✅ Yes |
| LangChain Tools | `langchain` | ⚪ Optional | ✅ Yes |

---

## Checking What's Installed

```python
from the_edge_agent import (
    KUZU_AVAILABLE,
    COZO_AVAILABLE,
)
from the_edge_agent.visualization import HAS_PYGRAPHVIZ

print(f"Pygraphviz: {'✓' if HAS_PYGRAPHVIZ else '✗'}")
print(f"Kuzu: {'✓' if KUZU_AVAILABLE else '✗'}")
print(f"CozoDB: {'✓' if COZO_AVAILABLE else '✗'}")
```

---

## Minimal Installation

For the absolute minimum installation (StateGraph only):

```bash
pip install the-edge-agent
```

This installs only:
- `networkx` - Graph structure
- `pyyaml` - YAML parsing
- `jmespath` - JSON querying
- `jsonschema` - Validation
- `fsspec` - File system abstraction

**Total size**: ~5 MB

---

## Recommended Installation

For full features (including graph backends):

```bash
# System dependencies (for pygraphviz)
sudo apt-get install libgraphviz-dev graphviz

# Python packages
pip install the-edge-agent
pip install pygraphviz              # Visualization
pip install kuzu                    # Graph backend (Cypher)
pip install 'pycozo[embedded]'      # Graph backend (Datalog)
```

**Total size**: ~50 MB

---

## Uninstalling Optional Dependencies

```bash
# Remove graph visualization
pip uninstall pygraphviz

# Remove graph backends
pip uninstall kuzu
pip uninstall pycozo

# Tests will automatically skip
pytest -v  # Skips tests for missing dependencies
```

---

## Implementation Details

### How Optional Imports Work

1. **Import Guard Pattern**:
   ```python
   try:
       import kuzu
       HAS_KUZU = True
   except ImportError:
       HAS_KUZU = False
   ```

2. **Runtime Check**:
   ```python
   if not HAS_KUZU:
       raise ImportError("Kuzu not installed. Install with: pip install kuzu")
   ```

3. **Graceful Degradation**:
   ```python
   if not graph_available():
       return {"success": False, "error": "No graph backend installed"}
   ```

4. **Test Skipping**:
   ```python
   @pytest.mark.skipif(not HAS_KUZU, reason="kuzu not installed")
   def test_kuzu_feature():
       ...
   ```

### Files Modified for Optional Dependencies

- ✅ `src/the_edge_agent/visualization.py` - Optional pygraphviz import
- ✅ `src/the_edge_agent/memory/graph.py` - Already has optional Kuzu/Cozo
- ✅ `src/the_edge_agent/actions/graph_actions.py` - Already has graceful degradation
- ✅ `tests/test_stategraph_core.py` - Skip decorators for visualization tests
- ✅ `tests/test_yaml_engine_kuzu.py` - Skip all tests when Kuzu not installed
- ✅ `tests/test_yaml_engine_ltm.py` - Skip CozoDB tests, fixed graceful degradation test

---

## Summary

✅ **All optional dependencies are properly implemented**:
- pygraphviz - Graph visualization
- Kuzu - Graph database (Cypher)
- CozoDB - Graph database (Datalog)

✅ **All tests skip gracefully when dependencies are missing**

✅ **Error messages provide clear installation instructions**

✅ **No breaking changes to existing code**
