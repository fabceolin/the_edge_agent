# Neurosymbolic AI (Prolog + LLM)

> Combine the pattern recognition of large language models with the logical rigor of symbolic reasoning to build AI systems that are both intelligent and verifiable.

## Why This Matters

LLMs excel at understanding context and generating natural language, but they can hallucinate facts and struggle with complex logical reasoning. Prolog provides deterministic, explainable inference chains that ground LLM outputs in verified facts and rules. By combining both paradigms, you get AI agents that can understand nuance while maintaining logical correctness.

## Quick Example

```yaml
name: reasoning-agent
nodes:
  - name: classify
    uses: llm.call
    with:
      messages:
        - role: user
          content: "Classify: {{ state.input }}"
    output: classification

  - name: reason
    language: prolog
    run: |
      state(classification, Class),
      rule(Class, Action),
      return(action, Action).
```

This simple pattern demonstrates the core idea: an LLM classifies input, then Prolog applies domain rules to determine the correct action. The classification is neural; the decision logic is symbolic and auditable.

## Key Features

| Feature | Description |
|---------|-------------|
| **Knowledge Graphs** | Represent domain knowledge as Prolog facts and query relationships, ancestors, and paths with built-in inference |
| **Constraint Solving** | Use CLP(FD) to solve scheduling, allocation, and optimization problems with declarative constraints |
| **Inference Chains** | Build multi-step reasoning pipelines where each step derives new facts from previous conclusions |
| **Grounding Validation** | Validate LLM extractions against semantic probes to catch hallucinations before they propagate |
| **Thread-Local State** | Parallel execution branches maintain isolated state via Prolog's native thread-local predicates |

## Available Actions

| Action | Description |
|--------|-------------|
| `prolog.query` | Execute a Prolog query and return variable bindings |
| `prolog.assert` | Add facts to the knowledge base dynamically |
| `prolog.retract` | Remove facts from the knowledge base |
| `prolog.consult` | Load external `.pl` rule files |

### Inline Prolog Execution

For most use cases, you define Prolog logic directly in the `run:` block with `language: prolog`:

```yaml
- name: infer_relationships
  language: prolog
  run: |
    % Access workflow state
    state(person, Person),

    % Query knowledge base
    ancestor(Person, Ancestor),

    % Return results to state
    return(ancestors, Ancestor).
```

## Examples

### Neurosymbolic Patterns

- [Family Reasoning with LLM + Prolog](../../examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml) - Full pipeline with 3-layer validation (structural, semantic, grounding)
- [Knowledge Graph Reasoning](../../examples/prolog/neurosymbolic/knowledge-graph.yaml) - Inference over relationships: grandparent, sibling, ancestor
- [Multi-Step Reasoning Chain](../../examples/prolog/neurosymbolic/reasoning-chain.yaml) - Medical symptom checker with chained inference
- [CLP(FD) Scheduling](../../examples/prolog/neurosymbolic/clpfd-scheduling.yaml) - Constraint-based task scheduling

### Basic Prolog

- [Simple Prolog Agent](../../examples/prolog/simple-prolog-agent.yaml) - State access, arithmetic, and inline rules
- [CLP(FD) Constraints](../../examples/prolog/clpfd-constraints.yaml) - Basic constraint satisfaction

## Learn More

- [Prolog Integration Epic](../stories/TEA-PROLOG-001-prolog-integration-epic.md) - Architecture overview and story status
- [Python Prolog Support](../stories/TEA-PY-004-prolog-scripting-support.md) - Python implementation details
- [Rust Prolog Support](../stories/TEA-RUST-035-prolog-scripting-support.md) - Rust implementation details
- [Cross-Runtime Parity](../stories/TEA-PROLOG-002-cross-runtime-parity-tests.md) - Python/Rust compatibility testing

## Installation

Prolog support is optional and requires SWI-Prolog 9.1+ system installation:

```bash
# Ubuntu/Debian
apt install swi-prolog

# macOS
brew install swi-prolog

# Python TEA with Prolog
pip install the-edge-agent[prolog]
```

```toml
# Rust Cargo.toml
[dependencies]
the_edge_agent = { version = "0.8", features = ["prolog"] }
```
