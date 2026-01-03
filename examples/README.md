# Examples

This directory contains example workflows for The Edge Agent. All YAML agents in this directory can be run by both the Python and Rust implementations.

## YAML Agents

| File | Description |
|------|-------------|
| `yaml_agent_example.yaml` | Basic YAML agent demonstrating core syntax |
| `yaml_customer_support_example.yaml` | Customer support chatbot with LLM |
| `yaml_perplexity_example.yaml` | Research agent using web search |

### Dynamic Parallel Examples (`yaml/`)

| File | Description |
|------|-------------|
| `yaml/dynamic_parallel_action_mode.yaml` | Fetch multiple URLs in parallel with rate limiting |
| `yaml/dynamic_parallel_steps_mode.yaml` | Process documents through multiple sequential steps |
| `yaml/dynamic_parallel_subgraph_mode.yaml` | Run external YAML workflows per item |
| `yaml/analysis_subgraph.yaml` | Subgraph used by the subgraph mode example |
| `yaml/dynamic_parallel_fail_fast.yaml` | Validation with fail-fast behavior |

## Python Examples

| File | Description |
|------|-------------|
| `example_usage.py` | Basic StateGraph usage |
| `stream_example.py` | Streaming execution with intermediate states |
| `fanout_fanin_example.py` | Parallel execution patterns |
| `nested_fanout_fanin_example.py` | Nested parallel flows |
| `nested_graphs_fanout_fanin_example.py` | Complex nested graph example |
| `two_different_flows_fanout_fanin_example.py` | Multiple parallel branches |
| `llm_customer_support.py` | LLM-powered customer support |
| `perplexity_research_example.py` | Web research workflow |
| `run_yaml_agent.py` | Helper to run YAML agents |

## Running Examples

### With Python

```bash
# Install the package first
cd ../python && pip install -e .

# Run a Python example
cd ../examples
python example_usage.py

# Run a YAML agent
python run_yaml_agent.py yaml_agent_example.yaml
```

### With Rust

```bash
# Build the CLI first
cd ../rust && cargo build --release

# Run a YAML agent
../rust/target/release/tea run yaml_agent_example.yaml --input '{"query": "hello"}'
```

## Environment Setup

Some examples require API keys:

```bash
# For LLM examples
export OPENAI_API_KEY=your-key-here

# For web search examples
export PERPLEXITY_API_KEY=your-key-here
```

## Example: Running the Customer Support Agent

### Python

```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine(secrets={"OPENAI_API_KEY": os.getenv("OPENAI_API_KEY")})
graph = engine.load_from_file("yaml_customer_support_example.yaml")

for event in graph.stream({"user_message": "I need help with my order"}):
    if event.get("type") == "final":
        print(event["state"]["response"])
```

### Rust

```bash
cd ../rust
cargo run --release -- run ../examples/yaml_customer_support_example.yaml \
  --input '{"user_message": "I need help with my order"}' \
  --secret OPENAI_API_KEY=$OPENAI_API_KEY
```

## Creating Your Own Agent

1. Copy an existing YAML file as a template
2. Define your state schema
3. Add nodes with actions or inline code
4. Connect nodes with edges
5. Run with Python or Rust

See the [YAML Reference](../docs/shared/YAML_REFERENCE.md) for complete syntax documentation.
