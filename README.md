# The Edge Agent

![The Edge Agent Logo](images/tea.jpg)

The Edge Agent (tea) ☕ is a lightweight, single-app state graph library inspired by LangGraph. It focuses on simplicity, making it ideal for use with local standalone AI agents, including edge computing environments. Tea provides an easy-to-use framework for building state-driven LLM workflows, avoiding unnecessary features to efficiently support local, single-app operations.

## Implementations

This is a **polyglot monorepo** with two implementations:

| Implementation | Status | Best For |
|----------------|--------|----------|
| **[Python](docs/python/getting-started.md)** | Production-ready | Full feature set, rapid prototyping, 20+ built-in actions |
| **[Rust](docs/rust/getting-started.md)** | Active development | Performance-critical deployments, embedded systems |

Both implementations share the same YAML agent syntax and can run the same agent configurations from the `examples/` directory.

### Quick Start by Language

**Python:**
```bash
cd python && pip install -e .
python -c "import the_edge_agent as tea; print(tea.__version__)"
```

**Rust:**
```bash
cd rust && cargo build --release
./target/release/tea --help
```

### Repository Structure

```
the_edge_agent/
├── python/          # Python implementation (full features)
├── rust/            # Rust implementation (performance)
├── examples/        # Shared YAML agents (works with both)
└── docs/
    ├── shared/      # Language-agnostic docs (YAML reference)
    ├── python/      # Python-specific guides
    └── rust/        # Rust-specific guides
```

## Features

- Simple state management
- Easy-to-use graph construction
- Single app focus
- Streamlined workflow creation
- Easy integration with any language models (like GPT)
- LLM library agnostic
- Parallel fan out fan in support
- Visualization of state graphs
- Declarative YAML-based agent configuration

## Installation (Python)

You can install the_edge_agent using pip:

```bash
pip install git+https://github.com/fabceolin/the_edge_agent.git
```

After installation, the `tea-agent` command will be available globally.

# Quick Start

## CLI Usage

The Edge Agent includes a command-line interface for running YAML-defined agent workflows without writing Python code:

```bash
# Run an agent from a YAML file
tea-agent examples/yaml_agent_example.yaml

# Run with initial state as JSON
tea-agent examples/yaml_agent_example.yaml --state '{"query": "artificial intelligence"}'

# Run with initial state from a JSON file
tea-agent examples/yaml_agent_example.yaml --state-file initial_state.json

# Load custom actions from a Python module
tea-agent examples/yaml_agent_example.yaml --actions-module my_company.tea_actions

# Load custom actions from a local Python file
tea-agent examples/yaml_agent_example.yaml --actions-file ./my_custom_actions.py

# Load multiple actions sources (later sources override earlier ones)
tea-agent agent.yaml --actions-module pkg1.actions --actions-module pkg2.actions --actions-file ./overrides.py

# Resume from a checkpoint (human-in-the-loop workflows)
tea-agent agent.yaml --resume ./checkpoints/node_1234567890.pkl

# Auto-continue at interrupts (CI/CD mode)
tea-agent agent.yaml --auto-continue

# Show version
tea-agent --version

# Show help
tea-agent --help
```

### Custom Actions Modules

You can create reusable action modules that can be loaded via the CLI or YAML configuration:

```python
# my_custom_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register custom actions into the provided registry."""

    def custom_search(state, query, **kwargs):
        # Your custom search logic
        return {"results": [...], "success": True}

    def custom_transform(state, data, **kwargs):
        # Your custom transformation logic
        return {"transformed": data, "success": True}

    registry['custom_search'] = custom_search
    registry['custom_transform'] = custom_transform

# Optional metadata for module discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My company's custom actions",
    "actions": ["custom_search", "custom_transform"],
}
```

Then use these actions in your YAML agent:

```yaml
name: my_agent
nodes:
  - name: search
    uses: custom_search
    with:
      query: "{{ state.query }}"
```

Load the actions module when running the agent:

```bash
tea-agent agent.yaml --actions-module my_custom_actions
# or from a file:
tea-agent agent.yaml --actions-file ./my_custom_actions.py
```

**Security Warning:** The `--actions-module` and `--actions-file` flags execute Python code from the specified modules. Only load actions from trusted sources. For production use, prefer installed packages over local files.

**Actions Loading Priority:**
1. Built-in actions (lowest priority)
2. CLI `--actions-module` flags (in order specified)
3. CLI `--actions-file` flags (in order specified)
4. YAML `imports:` section (highest priority - overrides CLI actions)
```

### Interactive Interrupt Workflow (Human-in-the-Loop)

The Edge Agent supports human-in-the-loop workflows via interactive interrupts. When a YAML agent defines `interrupt_before` or `interrupt_after`, execution pauses at those points, allowing you to review state and make decisions before continuing.

#### Basic Usage

```bash
# Run agent with interrupts (interactive mode)
tea-agent examples/customer_support.yaml --state '{"message": "My bill is wrong"}'

# Resume from a saved checkpoint
tea-agent examples/customer_support.yaml --resume ./checkpoints/classify_intent_1734567890.pkl

# Auto-continue mode (skip interactive prompts for CI/CD)
tea-agent examples/customer_support.yaml --auto-continue
```

#### Interactive Prompt Example

When execution reaches an interrupt point:

```
✓ classify_intent

⏸  Interrupt at: classify_intent
   State: {
     "customer_message": "My bill is wrong",
     "intent": "billing",
     "confidence": 0.95
   }

Checkpoint saved: ./checkpoints/classify_intent_1734567890123.pkl

Review the state above. Options:
  [c] Continue with current state
  [u] Update state before continuing
  [a] Abort execution

Choice: u

Enter state updates as JSON (or press Enter to skip):
{"escalate": true, "priority": "high"}

State updated. Resuming execution...

✓ handle_billing
✓ escalate_to_human

================================================================================
✓ Completed
================================================================================
```

#### Configuring Interrupts in YAML

Define interrupts in your YAML agent configuration:

```yaml
name: customer_support_agent

nodes:
  - name: classify_intent
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "Classify this customer message: {{ state.message }}"

  - name: handle_billing
    run: |
      return {"handled": True, "response": "Billing issue processed"}

edges:
  - from: __start__
    to: classify_intent
  - from: classify_intent
    to: handle_billing
  - from: handle_billing
    to: __end__

config:
  checkpoint_dir: ./checkpoints
  interrupt_after: [classify_intent]  # Pause after intent classification
  raise_exceptions: true
```

#### Resume with State Updates

You can resume from a checkpoint and merge in new state:

```bash
# Resume with additional state via --state flag
tea-agent agent.yaml \
  --resume ./checkpoints/classify_intent_123.pkl \
  --state '{"approved": true, "notes": "Verified with supervisor"}'
```

**State Merge Precedence** (highest to lowest):
1. User input from interactive prompt
2. `--state` flag value
3. Checkpoint state
4. YAML initial state defaults

#### Non-Interactive Mode (CI/CD)

For automated environments where interactive prompts would block execution:

```bash
# Auto-continue mode: execution continues at interrupts without pausing
tea-agent agent.yaml --auto-continue

# This also works in Docker, systemd services, and CI pipelines
# where stdin is not a TTY
```

The CLI automatically detects non-TTY environments (Docker, CI/CD) and auto-continues to prevent hanging.

#### Security Warning

⚠️ **Checkpoint files use Python pickle format and should only be loaded from trusted sources.** Do not load checkpoints from untrusted origins as they can execute arbitrary code during unpickling.

### Example Output

```
================================================================================
Running agent from: examples/yaml_agent_example.yaml
================================================================================

Initial state: {
  "query": "artificial intelligence"
}

✓ search
✓ validate_results
✓ summarize
✓ format_output
✓ save_report

================================================================================
✓ Completed
================================================================================
Final state: {...}
```

## Python API Usage

Here's a simple example to get you started:

```
import the_edge_agent as tea

# Initialize the StateGraph
graph = tea.StateGraph({"value": int, "result": str})

# Add nodes with print statements
def start_node(state):
    new_state = {"value": state["value"] + 5}
    print(f"Start node: {state} -> {new_state}")
    return new_state

def process_node(state):
    new_state = {"value": state["value"] * 2}
    print(f"Process node: {state} -> {new_state}")
    return new_state

def end_node(state):
    new_state = {"result": f"Final value: {state['value']}"}
    print(f"End node: {state} -> {new_state}")
    return new_state

graph.add_node("start", start_node)
graph.add_node("process", process_node)
graph.add_node("end", end_node)

# Add edges
graph.set_entry_point("start")
graph.add_conditional_edges(
    "start",
    lambda state: state["value"] > 10,
    {True: "end", False: "process"}
)
graph.add_edge("process", "start")
graph.set_finish_point("end")

# Compile the graph
compiled_graph = graph.compile()

# Run the graph and print results
print("Starting graph execution:")
results = list(compiled_graph.invoke({"value": 1}))

print("\nFinal result:")
for result in results:
    print(result)
```

Graph navigation
```

  ┌──────────────────────────┐
  │                          ▼
┌─────────┐  value <= 10   ┌─────────────┐
│ process │ ◀───────────── │    start    │
└─────────┘                └─────────────┘
                             │
                             │ value > 10
                             ▼
                           ╔═════════════╗
                           ║     end     ║
                           ╚═════════════╝

```
output:
```
Starting graph execution:
Start node: {'value': 1} -> {'value': 6}
Process node: {'value': 6} -> {'value': 12}
Start node: {'value': 12} -> {'value': 17}
End node: {'value': 17} -> {'result': 'Final value: 17'}

Final result:
{'type': 'final', 'state': {'value': 17, 'result': 'Final value: 17'}}
```

A full example with LLM capabilities and fan out fan in examples can be found in the examples directory.

# Contributing
We welcome contributions! Please see our contributing guidelines for more details.

# License
the_edge_agent is released under the MIT License. See the LICENSE file for more details.

# Contributors
We extend our gratitude to external contributors who have supported the_edge_agent:

- **[Fabrício Ceolin](https://www.linkedin.com/in/fabceolin/)** : Implementation
- **[Claudionor Coelho](https://www.linkedin.com/in/claudionor-coelho-jr-b156b01/)** : Contributed to the idealization of the project and provided valuable feedback through test usage.

# Acknowledgements
the_edge_agent is inspired by LangGraph. We thank the LangGraph team for their innovative work in the field of language model workflows.
