# Development Guide

This document covers development setup, testing, and build procedures for The Edge Agent (tea) project.

## Prerequisites

### System Dependencies

The project requires Graphviz for visualization features:

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install libgraphviz-dev graphviz -y

# macOS
brew install graphviz

# Windows
# Download from https://graphviz.org/download/
```

### Python Version

- **Minimum**: Python 3.7+
- **Recommended**: Python 3.10+ for best performance

## Installation

### Development Mode

Install the package with development dependencies:

```bash
# Clone the repository
git clone https://github.com/fabceolin/the_edge_agent.git
cd the_edge_agent

# Create virtual environment (recommended)
python -m venv .venv
source .venv/bin/activate  # Linux/macOS
# or: .venv\Scripts\activate  # Windows

# Install in development mode with all extras
pip install -e .[dev]
```

### Optional Dependencies

Install extras based on your needs:

```bash
# Firebase/Cloud memory backends
pip install -e .[firebase]

# Opik observability integration
pip install -e .[opik]

# All optional dependencies
pip install -e .[dev,firebase,opik]
```

## Testing

### Running Tests

```bash
# Run all tests
pytest

# Run with verbose output
pytest -v

# Run with coverage report
pytest --cov=src/the_edge_agent --cov-report=html

# Run specific test file
pytest tests/test_stategraph.py

# Run specific test class
pytest tests/test_stategraph.py::TestStateGraph

# Run specific test method
pytest tests/test_stategraph.py::TestStateGraph::test_add_node

# Run tests matching a pattern
pytest -k "parallel"

# Run with parallel execution (requires pytest-xdist)
pytest -n auto
```

### Test Categories

Tests are organized by component:

| Test File | Component |
|-----------|-----------|
| `test_stategraph.py` | Core StateGraph functionality |
| `test_yaml_engine.py` | YAML configuration parsing |
| `test_yaml_engine_edges.py` | Edge specification handling |
| `test_*_actions.py` | Built-in action modules |
| `test_opik_*.py` | Opik observability integration |

### Testing Frameworks

The project uses:

- **pytest** - Primary test runner
- **unittest** - Test case structure
- **parameterized** - Parameterized test cases
- **hypothesis** - Property-based testing
- **coverage** - Code coverage reporting

### Writing Tests

Follow existing patterns:

```python
import unittest
from parameterized import parameterized
from hypothesis import given, strategies as st
import the_edge_agent as tea

class TestMyFeature(unittest.TestCase):
    def setUp(self):
        self.graph = tea.StateGraph({"value": int})

    def test_basic_functionality(self):
        """Verify basic behavior."""
        self.graph.add_node("test")
        self.assertIn("test", self.graph.graph.nodes)

    @parameterized.expand([
        ("case_1", "input_1", "expected_1"),
        ("case_2", "input_2", "expected_2"),
    ])
    def test_parameterized(self, name, input_val, expected):
        """Test multiple cases."""
        result = self.graph.some_method(input_val)
        self.assertEqual(result, expected)

    @given(st.text(min_size=1))
    def test_property_based(self, node_name):
        """Test with random inputs."""
        # Hypothesis generates random node names
        self.graph.add_node(node_name)
        self.assertIn(node_name, self.graph.graph.nodes)
```

## Build

### Building the Package

```bash
# Build source distribution and wheel
python setup.py sdist bdist_wheel

# Or using build (recommended)
pip install build
python -m build
```

### Build Artifacts

After building, artifacts are in:

```
dist/
├── the_edge_agent-{version}.tar.gz    # Source distribution
└── the_edge_agent-{version}-py3-none-any.whl  # Wheel
```

### Publishing

```bash
# Install twine for publishing
pip install twine

# Upload to PyPI (requires credentials)
twine upload dist/*

# Upload to TestPyPI first (recommended)
twine upload --repository testpypi dist/*
```

## Code Quality

### Linting

```bash
# Run flake8
flake8 src/the_edge_agent

# Run with configuration
flake8 --config=setup.cfg src/
```

### Type Checking

```bash
# Run mypy
mypy src/the_edge_agent
```

### Formatting

```bash
# Format with black
black src/the_edge_agent tests

# Check without modifying
black --check src/the_edge_agent tests
```

## Development Workflow

### Making Changes

1. Create a feature branch: `git checkout -b feature/my-feature`
2. Make changes with tests
3. Run tests: `pytest`
4. Check coverage: `pytest --cov`
5. Commit with descriptive message
6. Push and create PR

### Adding New Actions

1. Create action function in appropriate module under `src/the_edge_agent/actions/`
2. Register in `_setup_builtin_actions()` or via imports
3. Add tests in `tests/test_*_actions.py`
4. Document in `docs/YAML_REFERENCE.md`

### Adding New Features

1. Design the feature (consider YAML and Python APIs)
2. Implement core functionality
3. Add comprehensive tests
4. Update documentation:
   - `CLAUDE.md` if it affects AI assistant usage
   - `docs/YAML_REFERENCE.md` for YAML syntax
   - `docs/architecture/` for architectural changes

## Debugging

### Verbose Logging

```python
import logging
logging.basicConfig(level=logging.DEBUG)

# Or for specific module
logging.getLogger('the_edge_agent').setLevel(logging.DEBUG)
```

### Graph Visualization

```python
graph = tea.StateGraph({"value": int})
# ... configure graph ...

# Render to file
graph.save_graph_image("debug_graph.png")

# Get Graphviz source
dot_source = graph.render_graphviz()
print(dot_source)
```

### Tracing Execution

```python
from the_edge_agent import YAMLEngine

# Enable console tracing
engine = YAMLEngine(trace_exporter="console", trace_verbose=True)

# Or file tracing
engine = YAMLEngine(trace_exporter="file", trace_file="./traces.jsonl")
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `OPENAI_API_KEY` | OpenAI API key for LLM actions | None |
| `FIRECRAWL_API_KEY` | Firecrawl API for web scraping | None |
| `PERPLEXITY_API_KEY` | Perplexity API for web search | None |
| `OPIK_API_KEY` | Opik API for observability | None |
| `OPIK_PROJECT_NAME` | Opik project name | `"the-edge-agent"` |
| `GOOGLE_APPLICATION_CREDENTIALS` | Firebase/GCS credentials | None |

## IDE Setup

### VS Code

Recommended extensions:
- Python (ms-python.python)
- Pylance (ms-python.vscode-pylance)
- Python Test Explorer

`.vscode/settings.json`:
```json
{
  "python.testing.pytestEnabled": true,
  "python.testing.pytestArgs": ["tests"],
  "python.linting.enabled": true,
  "python.linting.flake8Enabled": true
}
```

### PyCharm

1. Mark `src/` as Sources Root
2. Mark `tests/` as Test Sources Root
3. Configure pytest as test runner
