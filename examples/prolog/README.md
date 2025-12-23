# Prolog Examples

This directory contains examples demonstrating Prolog integration with The Edge Agent (TEA).

## Overview

TEA supports Prolog scripting for **neurosymbolic AI** workflows, combining:

- **Neural networks** (pattern recognition, LLM text understanding)
- **Symbolic logic** (formal reasoning, constraint solving)

## Directory Structure

```
prolog/
├── README.md                    # This file
├── simple-prolog-agent.yaml     # Basic Prolog hello world
├── clpfd-constraints.yaml       # CLP(FD) constraint example
│
├── neurosymbolic/               # Neurosymbolic AI examples
│   ├── README.md                # Pattern overview
│   ├── llm-prolog-family-reasoning.yaml  # Flagship: LLM + Prolog
│   ├── classifier-rules.yaml    # Neural classifier → Prolog rules
│   ├── clpfd-scheduling.yaml    # Constraint-based scheduling
│   ├── knowledge-graph.yaml     # KG reasoning
│   └── reasoning-chain.yaml     # Multi-step reasoning
│
├── rules/                       # Reusable Prolog rule libraries
│   ├── family_relationships.pl  # Family inference rules
│   ├── domain_rules.pl          # Generic domain logic
│   └── inference_rules.pl       # Common inference patterns
│
├── parity/                      # Cross-runtime parity tests
│   └── *.yaml                   # Test cases for Python/Rust
│
└── parity-expected/             # Expected test outputs
    └── *.json                   # Expected results
```

## Quick Start

### Running Examples

**Python TEA:**
```bash
cd python
python -m the_edge_agent run ../examples/prolog/simple-prolog-agent.yaml
```

**Rust TEA:**
```bash
cd rust
cargo run --features prolog -- run ../examples/prolog/simple-prolog-agent.yaml
```

### Basic Prolog Node

```yaml
nodes:
  - name: calculate
    language: prolog
    run: |
      state(input, X),
      Y is X * 2,
      return(output, Y).
```

## Examples

### 1. Simple Prolog Agent

Basic example showing state access and return values.

**File:** `simple-prolog-agent.yaml`

### 2. CLP(FD) Constraints

Demonstrates finite domain constraint solving.

**File:** `clpfd-constraints.yaml`

### 3. Neurosymbolic Examples

Advanced examples combining neural and symbolic approaches:

| Example | Description |
|---------|-------------|
| `llm-prolog-family-reasoning.yaml` | LLM extraction + Prolog inference for family relationships |
| `classifier-rules.yaml` | Neural classifier output + domain rule engine |
| `clpfd-scheduling.yaml` | Constraint-based task scheduling |
| `knowledge-graph.yaml` | Prolog-based knowledge graph reasoning |
| `reasoning-chain.yaml` | Multi-step iterative reasoning |

See `neurosymbolic/README.md` for detailed documentation.

## Prolog State Interface

| Predicate | Purpose |
|-----------|---------|
| `state(Key, Value)` | Read value from workflow state |
| `return(Key, Value)` | Set value to return to workflow state |

## CLP(FD) Support

TEA supports SWI-Prolog's CLP(FD) library for constraint solving:

```yaml
- name: solve
  language: prolog
  run: |
    :- use_module(library(clpfd)).
    X in 1..10, Y in 1..10,
    X + Y #= 15, X #< Y,
    label([X, Y]),
    return(solution, [X, Y]).
```

## Requirements

### System Dependencies

- **SWI-Prolog** must be installed on your system

**Ubuntu/Debian:**
```bash
sudo apt install swi-prolog
```

**macOS:**
```bash
brew install swi-prolog
```

**Windows:**
Download from https://www.swi-prolog.org/Download.html

### Python

```bash
pip install 'the-edge-agent[prolog]'
```

### Rust

```bash
cargo build --features prolog
```

## Documentation

- [Python Prolog Guide](../../docs/python/prolog-guide.md)
- [Rust Prolog Guide](../../docs/rust/prolog-guide.md)
- [Neurosymbolic Patterns](../../docs/shared/architecture/neurosymbolic-patterns.md)
- [YAML Reference - Prolog Section](../../docs/shared/YAML_REFERENCE.md#prolog-scripting)
