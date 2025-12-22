# Source Tree

This document describes the project structure and organization of The Edge Agent (tea) codebase.

## Project Root

```
the_edge_agent/
├── src/                        # Source code
│   └── the_edge_agent/         # Main package
├── tests/                      # Test suite
├── examples/                   # Usage examples
├── docs/                       # Documentation
├── images/                     # Project images
├── setup.py                    # Package configuration
├── pyproject.toml              # Build system config
├── README.md                   # Project overview
├── LICENSE                     # MIT License
└── CLAUDE.md                   # AI assistant instructions
```

## Source Code (`src/the_edge_agent/`)

### `__init__.py`

Package initialization and public API exports.

```python
from .stategraph import StateGraph, START, END
from .yaml_engine import YAMLEngine

__all__ = ["StateGraph", "START", "END", "YAMLEngine"]
__version__ = "0.0.1"
```

**Exports:**
- `StateGraph` - Main class for building state-driven workflows
- `START` - Constant for start node (`"__start__"`)
- `END` - Constant for end node (`"__end__"`)
- `YAMLEngine` - Engine for loading graphs from YAML

### `stategraph.py`

Core state graph implementation (~724 lines).

**Classes:**
- `StateGraph` - Graph-based state machine for managing workflows

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

**Internal Methods:**
- `_execute_node_function()` - Run a node's function with introspected params
- `_execute_flow()` - Execute a parallel flow until fan-in
- `_prepare_function_params()` - Match function signature to available params
- `_get_next_node()` - Determine next node based on conditions

### `yaml_engine.py`

YAML-based declarative agent configuration (~553 lines).

**Classes:**
- `DotDict` - Dictionary with attribute-style access
- `YAMLEngine` - Engine for creating StateGraph from YAML

**Key Methods:**

| Method | Purpose |
|--------|---------|
| `load_from_file(yaml_path)` | Load graph from YAML file |
| `load_from_dict(config)` | Load graph from dictionary |

**Internal Methods:**
- `_add_node_from_config()` - Create node from YAML config
- `_create_run_function()` - Create callable from node config
- `_create_inline_function()` - Create function from Python code string
- `_create_action_function()` - Create function from action reference
- `_create_steps_function()` - Create multi-step function
- `_add_edge_from_config()` - Create edge from YAML config
- `_process_template()` - Replace `{{ }}` variables
- `_setup_builtin_actions()` - Register default actions

**Built-in Actions:**
- `llm.call` - Call OpenAI-compatible LLM
- `http.get` / `http.post` - HTTP requests
- `file.read` / `file.write` - File operations
- `actions.notify` - Notifications (placeholder)

## Tests (`tests/`)

```
tests/
├── __init__.py
├── test_stategraph.py      # StateGraph unit tests
├── test_yaml_engine.py     # YAMLEngine tests
└── stategraph_main.py      # Manual test runner
```

### `test_stategraph.py`

Comprehensive unit tests using:
- `unittest` - Test framework
- `parameterized` - Parameterized test cases
- `hypothesis` - Property-based testing

**Test Categories:**
- Initialization tests
- Node operation tests
- Edge operation tests
- Conditional edge tests
- Entry/finish point tests
- Compilation tests
- Execution tests (invoke/stream)
- Parallel execution tests
- Error handling tests
- Interrupt tests
- Visualization tests

### `test_yaml_engine.py`

Tests for YAML engine functionality:
- Template processing
- Node configuration parsing
- Edge configuration parsing
- Built-in action execution
- Multi-step nodes

## Documentation (`docs/`)

```
docs/
├── architecture/           # Architecture documentation
│   ├── tech-stack.md       # Technology stack details
│   ├── coding-standards.md # Coding conventions
│   └── source-tree.md      # This file
├── stories/                # User stories and tasks
├── qa/                     # QA assessments
└── YAML_REFERENCE.md       # YAML engine reference (unified documentation)
```

## Examples (`examples/`)

Example workflows and generated visualizations:

```
examples/
├── *.yaml                  # YAML agent configurations
├── *.py                    # Python examples
├── *.png                   # Generated graph images
└── env/                    # Example virtual environment
```

## Key Files

### `setup.py`

Package metadata and dependencies:

```python
setup(
    name="the_edge_agent",
    version="0.3.0",
    install_requires=[
        "networkx==3.3",
        "pygraphviz==1.13",
        "pyyaml>=6.0",
    ],
    extras_require={
        "dev": ["pytest", "coverage", "hypothesis", "parameterized==0.9.0"],
    },
)
```

### `pyproject.toml`

Build system configuration:

```toml
[build-system]
requires = ["setuptools>=42", "wheel"]
build-backend = "setuptools.build_meta"
```

### `CLAUDE.md`

Instructions for AI development assistants working on the project.

## Module Dependencies

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
│   pygraphviz    │     │      json       │
│   threading     │     │       re        │
│ concurrent.fut. │     │     pathlib     │
└─────────────────┘     └─────────────────┘
```

## Design Overview

### StateGraph Architecture

```
                    ┌─────────────┐
                    │   START     │  (special node)
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
                    │   Node 1    │  run: Callable
                    └──────┬──────┘
                           │
            ┌──────────────┼──────────────┐
            │              │              │
            ▼              ▼              ▼
     ┌───────────┐  ┌───────────┐  ┌───────────┐
     │ Parallel  │  │ Parallel  │  │ Parallel  │
     │  Flow A   │  │  Flow B   │  │  Flow C   │
     └─────┬─────┘  └─────┬─────┘  └─────┬─────┘
            │              │              │
            └──────────────┼──────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │   Fan-In    │  parallel_results
                    └──────┬──────┘
                           │
                           ▼
                    ┌─────────────┐
                    │    END      │  (special node)
                    └─────────────┘
```

### Event Flow

```
invoke()/stream()
       │
       ▼
┌──────────────────┐
│ Interrupt Before?│──yes──► yield interrupt
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  Execute Node    │──error──► yield error / raise
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ Interrupt After? │──yes──► yield interrupt
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│  Get Next Node   │
└────────┬─────────┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
  END      Continue
    │         │
    ▼         └───► (loop back)
yield final
```
