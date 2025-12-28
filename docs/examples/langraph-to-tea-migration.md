# Migrating from LangGraph to TEA

This guide walks through migrating a LangGraph application to The Edge Agent (TEA) using the Kiroku document writer as a case study.

## Table of Contents

- [Introduction](#introduction)
- [Why Migrate to TEA?](#why-migrate-to-tea)
- [Key Concept Mapping](#key-concept-mapping)
- [Step-by-Step Migration](#step-by-step-migration)
- [Case Study: Kiroku Document Writer](#case-study-kiroku-document-writer)
- [Troubleshooting](#troubleshooting)
- [FAQ](#faq)

## Introduction

TEA (The Edge Agent) is a lightweight state graph library inspired by LangGraph, designed for edge computing environments. While LangGraph uses Python classes and decorators, TEA supports both Python API and declarative YAML configuration.

### What TEA Offers

- **Declarative YAML agents**: Define workflows without writing Python code
- **Smaller footprint**: Designed for resource-constrained edge devices
- **LLM-agnostic**: Works with any language model provider
- **Built-in actions**: HTTP, file I/O, web search, and more
- **Human-in-the-loop**: First-class support for interrupts and user input

## Why Migrate to TEA?

| Feature | LangGraph | TEA |
|---------|-----------|-----|
| Configuration | Python code | Python or YAML |
| Dependencies | Heavy (LangChain) | Minimal |
| Deployment | Server/cloud | Edge/server/cloud |
| Customization | Python classes | YAML + Python |
| Learning curve | Moderate | Low (for YAML) |

## Key Concept Mapping

| LangGraph | TEA (Python) | TEA (YAML) |
|-----------|--------------|------------|
| `StateGraph(TypedDict)` | `StateGraph(schema)` | `state_schema:` |
| `graph.add_node()` | `graph.add_node()` | `nodes:` list |
| `graph.add_edge()` | `graph.add_edge()` | `edges:` list or `goto:` |
| `add_conditional_edges()` | `add_conditional_edges()` | `goto:` with `if:` |
| `@tool` decorator | Custom action functions | `uses:` action |
| `interrupt_before` | `compile(interrupt_before=[])` | `interrupt: before` |
| `MemorySaver` | `checkpointer=` | `settings.checkpointer:` |
| `ChatOpenAI` | User-defined function | `llm.call` action |
| `graph.invoke()` | `graph.invoke()` | `tea run agent.yaml` |

## Step-by-Step Migration

### 1. Define State Schema

**LangGraph (Python):**
```python
from typing import TypedDict, List

class AgentState(TypedDict):
    title: str
    hypothesis: str
    draft: str
    revision_number: int
    messages: List[str]
```

**TEA (YAML):**
```yaml
state_schema:
  title: str
  hypothesis: str
  draft: str
  revision_number: int
  messages: list
```

**TEA (Python):**
```python
import the_edge_agent as tea

graph = tea.StateGraph({
    "title": str,
    "hypothesis": str,
    "draft": str,
    "revision_number": int,
    "messages": list,
})
```

### 2. Convert Nodes

**LangGraph:**
```python
from langchain_openai import ChatOpenAI

llm = ChatOpenAI(model="gpt-4")

def suggest_title(state: AgentState) -> dict:
    prompt = f"Suggest a title for: {state['hypothesis']}"
    response = llm.invoke(prompt)
    return {"title": response.content}

graph.add_node("suggest_title", suggest_title)
```

**TEA (YAML):**
```yaml
nodes:
  - name: suggest_title
    uses: llm.call
    with:
      messages:
        - role: user
          content: "Suggest a title for: {{ state.hypothesis }}"
    output:
      title: "{{ result.content }}"
```

**TEA (Python):**
```python
def suggest_title(state):
    from openai import OpenAI
    client = OpenAI()
    response = client.chat.completions.create(
        model="gpt-4",
        messages=[{"role": "user", "content": f"Suggest a title for: {state['hypothesis']}"}]
    )
    return {"title": response.choices[0].message.content}

graph.add_node("suggest_title", run=suggest_title)
```

### 3. Convert Simple Edges

**LangGraph:**
```python
graph.add_edge("suggest_title", "internet_search")
graph.add_edge("internet_search", "topic_sentence_writer")
```

**TEA (YAML - explicit edges):**
```yaml
edges:
  - from: suggest_title
    to: internet_search
  - from: internet_search
    to: topic_sentence_writer
```

**TEA (YAML - implicit via node order):**
```yaml
# Nodes execute in order unless goto: specifies otherwise
nodes:
  - name: suggest_title
    # ...
  - name: internet_search
    # ...
  - name: topic_sentence_writer
    # ...
```

### 4. Convert Conditional Edges

**LangGraph:**
```python
def should_suggest_title(state: AgentState) -> str:
    if state.get("suggest_title_flag"):
        return "suggest_title"
    return "internet_search"

graph.add_conditional_edges(
    "__start__",
    should_suggest_title,
    {"suggest_title": "suggest_title", "internet_search": "internet_search"}
)
```

**TEA (YAML):**
```yaml
nodes:
  - name: check_suggest_title
    run: |
      return {}
    goto:
      - if: "{{ state.suggest_title_flag }}"
        to: suggest_title
      - to: internet_search  # fallback (no condition)
```

**TEA (Python):**
```python
graph.add_conditional_edges(
    "check_suggest_title",
    lambda state: "suggest_title" if state.get("suggest_title_flag") else "internet_search",
    {"suggest_title": "suggest_title", "internet_search": "internet_search"}
)
```

### 5. Convert Interrupts (Human-in-the-Loop)

**LangGraph:**
```python
graph = graph.compile(
    interrupt_before=["suggest_title_review", "topic_sentence_manual_review"]
)
```

**TEA (YAML):**
```yaml
nodes:
  - name: suggest_title_review
    interrupt: before
    uses: llm.call
    # ...

  - name: topic_sentence_manual_review
    interrupt: before
    uses: llm.call
    # ...
```

**TEA (Python):**
```python
compiled = graph.compile(
    interrupt_before=["suggest_title_review", "topic_sentence_manual_review"]
)
```

### 6. Convert Checkpointing

**LangGraph:**
```python
from langgraph.checkpoint.memory import MemorySaver

memory = MemorySaver()
graph = graph.compile(checkpointer=memory)
```

**TEA (YAML):**
```yaml
settings:
  checkpointer:
    type: memory  # or: sqlite, duckdb, firestore
```

**TEA (Python):**
```python
from the_edge_agent import MemoryCheckpointer

compiled = graph.compile(checkpointer=MemoryCheckpointer())
```

## Case Study: Kiroku Document Writer

The Kiroku document writer is a full academic paper generation workflow originally built with LangGraph. Here's how it was migrated to TEA.

### Original Structure (LangGraph)

```
kiroku/
├── agents/
│   ├── states.py      # AgentState TypedDict
│   ├── prompts.py     # Prompt templates
│   └── nodes.py       # Node functions
├── graph.py           # Graph construction
└── main.py            # Entry point
```

### TEA Structure (YAML)

```
examples/academic/
├── kiroku-document-writer.yaml  # Complete agent definition
└── sample-paper-spec.yaml       # Input specification
```

### State Mapping

| LangGraph Field | TEA Field | Type |
|-----------------|-----------|------|
| `title` | `title` | str |
| `hypothesis` | `hypothesis` | str |
| `area_of_paper` | `area_of_paper` | str |
| `draft` | `draft` | str |
| `plan` | `plan` | str |
| `critique` | `critique` | str |
| `revision_number` | `revision_number` | int |
| `max_revisions` | `max_revisions` | int |
| `suggest_title_flag` | `suggest_title_flag` | bool |

### Node Mapping

| LangGraph Node | TEA Node | Action Type |
|----------------|----------|-------------|
| `suggest_title` | `suggest_title` | `llm.call` |
| `suggest_title_review` | `suggest_title_review` | `llm.call` + interrupt |
| `internet_search` | `internet_search` | `web.search` |
| `topic_sentence_writer` | `topic_sentence_writer` | `llm.call` |
| `paper_writer` | `paper_writer` | `llm.call` |
| `reflection_reviewer` | `reflection_reviewer` | `llm.call` |
| `write_abstract` | `write_abstract` | `llm.call` |
| `generate_references` | `generate_references` | `llm.call` |
| `generate_citations` | `generate_citations` | `text.insert_citations` |

### Prompt Migration

LangGraph prompts used Python f-strings:
```python
PAPER_WRITER_PROMPT = f"""
Each paragraph MUST have at least {sentences_per_paragraph} sentences.
Task: {task}
Content: {content}
"""
```

TEA prompts use Jinja2 templates:
```yaml
paper_writer: |
  Each paragraph MUST have at least {{ state.sentences_per_paragraph }} sentences.
  Task: {{ state.task }}
  Content: {{ state.content | join('\n\n') if state.content else '' }}
```

### Running the Migrated Agent

```bash
# Run with interactive mode (default)
cd python
python -m the_edge_agent.cli run \
  ../examples/academic/kiroku-document-writer.yaml \
  --spec ../examples/academic/sample-paper-spec.yaml

# Run without interrupts (for testing)
python -m the_edge_agent.cli run \
  ../examples/academic/kiroku-document-writer.yaml \
  --spec ../examples/academic/sample-paper-spec.yaml \
  --skip-interrupts
```

## Troubleshooting

### Q: My Jinja2 templates don't render correctly

**Problem:** Template shows `{{ state.value }}` literally instead of the value.

**Solution:** Ensure you're using the correct syntax:
- Use `{{ state.field }}` not `{state.field}` or `${state.field}`
- For lists, use `{{ state.items | join(', ') }}`
- For optional values, use `{{ state.value or 'default' }}`

### Q: Conditional edges aren't working

**Problem:** The workflow always takes the same path regardless of state.

**Solution:** Check your `if:` expressions:
```yaml
# Wrong - comparing strings
goto:
  - if: "{{ state.flag == 'true' }}"
    to: node_a

# Correct - boolean comparison
goto:
  - if: "{{ state.flag }}"
    to: node_a
```

### Q: Interrupts aren't pausing execution

**Problem:** The workflow runs through without stopping at interrupt points.

**Solution:**
1. Ensure `interrupt: before` is on the node definition
2. Don't use `--skip-interrupts` flag
3. Make sure a checkpointer is configured (required for interrupts)

### Q: State not persisting between nodes

**Problem:** Values set in one node are missing in the next.

**Solution:** Ensure your node returns a dictionary that merges with state:
```yaml
# Wrong - returns nothing
run: |
  value = state["input"].upper()
  print(value)

# Correct - returns state update
run: |
  return {"output": state["input"].upper()}
```

### Q: LLM action fails with authentication error

**Problem:** `llm.call` returns authentication error.

**Solution:** Set the required environment variables:
```bash
# For OpenAI
export OPENAI_API_KEY="sk-..."

# For Azure OpenAI
export AZURE_OPENAI_API_KEY="..."
export AZURE_OPENAI_ENDPOINT="https://..."
export AZURE_OPENAI_DEPLOYMENT="gpt-4"
```

### Q: Web search action not working

**Problem:** `web.search` returns no results or errors.

**Solution:** Configure Perplexity API:
```bash
export PERPLEXITY_API_KEY="pplx-..."
```

## FAQ

### Can I mix YAML and Python in TEA?

Yes! TEA supports:
- **Pure YAML**: All logic in YAML with built-in actions
- **YAML + Python blocks**: Inline Python in `run:` sections
- **YAML + Python modules**: Import custom functions via `imports:`
- **Pure Python**: Use the `StateGraph` class directly

### How do I debug my TEA agent?

1. **Console output**: Add print statements in `run:` blocks
2. **Verbose mode**: Run with `RUST_LOG=debug tea run agent.yaml`
3. **Tracing**: Enable trace exporter in settings:
   ```yaml
   settings:
     trace_exporter: console
     trace_verbose: true
   ```

### Can I use TEA with other LLM providers?

Yes, TEA is LLM-agnostic. The `llm.call` action supports OpenAI-compatible APIs, but you can:
- Use custom `run:` blocks with any SDK
- Register custom actions for other providers
- Use `http.post` for direct API calls

### How do I test my TEA agent?

1. **Unit tests**: Test individual nodes with mocked LLM responses
2. **Integration tests**: Test graph compilation and edge routing
3. **E2E tests**: Run full workflow with `--skip-interrupts`

See `python/tests/test_kiroku_workflow.py` for examples.

### What's the performance difference?

TEA is designed for edge computing:
- **Startup time**: ~100ms vs LangGraph's ~2-3s
- **Memory usage**: ~50MB vs LangGraph's ~200-500MB
- **Dependencies**: 5 core packages vs LangGraph's 30+

### Can I migrate incrementally?

Yes! You can:
1. Start with Python API (similar to LangGraph)
2. Gradually convert nodes to YAML
3. Eventually have a fully declarative YAML agent

## Further Reading

- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete YAML syntax guide
- [Actions Reference](../python/actions-reference.md) - Built-in actions documentation
- [Checkpoint Guide](../shared/architecture/checkpoint-guide.md) - Persistence options
- [Examples](../../examples/) - More example agents
