# Why LLMs Can't Count: Building Neurosymbolic AI Agents with The Edge Agent

**Fabricio Ceolin**

*Independent Researcher*

https://www.linkedin.com/in/fabceolin/

---

## Abstract

Large Language Models excel at natural language understanding but consistently fail at precise mathematical operations due to their tokenization-based architecture. This article demonstrates this limitation using a genealogical reasoning task—computing "great-great-grandmother" relationships—where LLMs must count generations and apply a simple formula. We present a neurosymbolic solution using The Edge Agent (TEA) framework that combines LLM-based natural language parsing with Prolog for logical inference and Lua for precise arithmetic. Our experiments show that while a 3B parameter LLM fails to correctly count 5 generations, the neurosymbolic approach achieves 100% accuracy by delegating computation to symbolic systems designed for mathematical precision.

**Keywords:** Neurosymbolic AI, Large Language Models, Prolog, Symbolic Reasoning, Genealogical Inference

---

## 1. Introduction

Large Language Models (LLMs) have revolutionized natural language processing, enabling sophisticated text generation, summarization, and reasoning tasks. However, as developers push these models into production environments, a fundamental limitation becomes apparent: **LLMs struggle with precise mathematical operations and counting tasks**.

This limitation is particularly problematic when building AI agents that need to perform calculations, count items, or apply mathematical formulas. While LLMs excel at understanding natural language and extracting structured information, they often fail at basic arithmetic — not because of insufficient training data, but due to the fundamental nature of how transformer architectures process information.

This article demonstrates this limitation using a genealogical reasoning task and presents a solution using **neurosymbolic AI** — an approach that combines the natural language understanding capabilities of LLMs with the mathematical precision of symbolic systems like Prolog and Lua.

Following this guide, readers will learn how to:
- Set up a local LLM environment with Ollama
- Install and configure The Edge Agent (TEA)
- Build an LLM-only agent for genealogical reasoning
- Build a neurosymbolic agent combining LLM, Prolog, and Lua
- Compare results to understand when neurosymbolic approaches are necessary

## 2. Audience

This guide is intended for developers and AI practitioners interested in building reliable AI agents. Familiarity with YAML configuration, basic command-line operations, and Python environments is helpful. No prior knowledge of Prolog is required — the examples are self-contained and explained step by step.

## 3. The Problem: LLMs and Mathematical Precision

Consider this simple genealogical task:

> "Alice is the mother of Bob. Bob is the father of Carol. Carol is the mother of Diana. Diana is the father of Eve. Eve is the mother of Frank."
>
> **Question**: What is Alice's relationship to Frank?

The solution requires:
1. **Tracing the lineage**: Alice → Bob → Carol → Diana → Eve → Frank
2. **Counting generations**: 5 generations
3. **Applying the formula**: `number_of_greats = generations - 2 = 3`
4. **Formatting the result**: "Great-Great-Great-Grandmother"

This is straightforward for humans, but LLMs consistently fail at step 3 — they recognize patterns but don't actually compute.

## 4. Software Overview

For this setup, we use:

- **Ollama**: A local LLM runtime that makes it easy to run open-source models
- **Gemma 3N**: Google's efficient small language model optimized for edge deployment
- **The Edge Agent (TEA)**: A lightweight state graph library supporting Python, Lua, and Prolog
- **Prolog**: A logic programming language for symbolic reasoning
- **Lua**: A lightweight scripting language for mathematical calculations

The neurosymbolic architecture separates concerns:
- **LLM** handles natural language understanding (extracting facts from text)
- **Prolog** handles logical reasoning (recursive generation counting)
- **Lua** handles mathematical calculation (applying the greats formula)

## 5. Prerequisites

### Step 1: Install Ollama

Ollama provides a simple way to run LLMs locally.

**Linux:**
```bash
curl -fsSL https://ollama.com/install.sh | sh
```

**macOS:**
```bash
brew install ollama
```

**Start the Ollama server:**
```bash
ollama serve
```

### Step 2: Download the Gemma 3N Model

Pull the Gemma 3N model optimized for efficient inference:

```bash
ollama pull gemma3n:e4b
```

Verify the model is available:
```bash
ollama list
```

Expected output:
```
NAME           ID              SIZE      MODIFIED
gemma3n:e4b    abc123def456    2.0 GB    Just now
```

### Step 3: Install The Edge Agent (TEA)

The easiest way to install TEA is to download the pre-built binary from GitHub releases. The **AppImage** version includes Prolog support out of the box — no additional dependencies required.

**Linux (x86_64) — Recommended:**
```bash
# Download the AppImage (includes Prolog)
wget https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-0.8.17-x86_64.AppImage -O tea
chmod +x tea

# Verify installation
./tea --help
```

**Linux (ARM64):**
```bash
wget https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-0.8.17-aarch64.AppImage -O tea
chmod +x tea
```

**macOS (Apple Silicon):**
```bash
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-python-darwin-arm64-full -o tea
chmod +x tea
```

**Windows:**
```powershell
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-python-windows-x86_64-full.exe -o tea.exe
```

**Alternative: Install from Source**

If you prefer to install from source:

```bash
git clone https://github.com/fabceolin/the_edge_agent.git
cd the_edge_agent/python
python3 -m venv .venv
source .venv/bin/activate
pip install -e .
```

## 6. Detailed Steps

### Step 1: Download the Example Agents

Download the example agents directly from GitHub:

```bash
# Create examples directory
mkdir -p examples/lineage

# Download the LLM-only agent
curl -o examples/lineage/lineage-llm-only.yaml \
  https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/examples/lineage/lineage-llm-only.yaml

# Download the Neurosymbolic agent
curl -o examples/lineage/lineage-neurosymbolic.yaml \
  https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/examples/lineage/lineage-neurosymbolic.yaml
```

### Step 2: Understanding the Agents

**LLM-Only Agent** (`lineage-llm-only.yaml`):
- Single LLM call with Chain-of-Thought prompting
- Asks the model to trace lineage, count generations, and format the title
- Relies entirely on the LLM's ability to count "Great-" prefixes

**Neurosymbolic Agent** (`lineage-neurosymbolic.yaml`):
- **Node 1 (LLM)**: Extracts parent-child relationships as Prolog facts
- **Node 2 (Lua)**: Parses and cleans the extracted facts
- **Node 3 (Prolog)**: Recursively counts generations using logical inference
- **Node 4 (Lua)**: Applies the formula `greats = n - 2` precisely

The key difference is that the neurosymbolic agent uses **Prolog for recursive traversal** and **Lua for mathematical calculation**, ensuring provably correct results.

### Step 3: Run and Compare Both Agents

**Test the LLM-Only Agent:**

```bash
./tea run examples/lineage/lineage-llm-only.yaml --input '{
  "text": "Alice is the mother of Bob. Bob is the father of Carol. Carol is the mother of Diana. Diana is the father of Eve. Eve is the mother of Frank.",
  "ancestor": "Alice",
  "descendant": "Frank"
}'
```

**LLM-Only Result:**
```json
{
  "answer": "Great-Great-Great-Great-Grandmother (5 generations)"
}
```

**Test the Neurosymbolic Agent:**

```bash
./tea run examples/lineage/lineage-neurosymbolic.yaml --input '{
  "text": "Alice is the mother of Bob. Bob is the father of Carol. Carol is the mother of Diana. Diana is the father of Eve. Eve is the mother of Frank.",
  "ancestor": "alice",
  "descendant": "frank"
}'
```

**Neurosymbolic Result:**
```json
{
  "facts": "mother(alice, bob). father(bob, carol). mother(carol, diana). father(diana, eve). mother(eve, frank).",
  "generations": 5,
  "answer": "alice is the Great-Great-Great-Grandmother/Grandfather of frank (5 generations)"
}
```

## 7. Results Analysis

| Metric | LLM-Only | Neurosymbolic | Expected |
|--------|----------|---------------|----------|
| **Trace** | Alice → Bob → Carol → Diana → Eve → Frank | ✓ Extracted correctly | ✓ |
| **Generations** | 5 | 5 | 5 |
| **Greats Count** | 4 | 3 | 3 |
| **Title** | Great⁴-Grandmother | Great³-Grandmother | Great³-Grandmother |
| **Correct?** | ❌ | ✅ | — |

**The mathematical verification:**
```
generations = 5
greats = generations - 2 = 5 - 2 = 3
result = "Great-Great-Great-Grandmother"
```

The LLM-only approach produced **4 greats** instead of the correct **3 greats**. This is a consistent error pattern — LLMs tend to add or subtract one when counting repetitive elements.

## 8. Why LLMs Fail at Counting

LLMs are **statistical pattern matchers**, not calculators. When processing the prompt, the model:

1. Recognizes the genealogical pattern from training data
2. Associates "5 generations" with "many greats"
3. Generates a plausible-looking answer based on pattern matching
4. **Does not actually compute** `5 - 2 = 3`

This is a fundamental architectural limitation. Transformers excel at:
- Natural language understanding
- Pattern recognition
- Contextual reasoning
- Text generation

But struggle with:
- Precise counting
- Mathematical operations
- Recursive logic
- Deterministic calculations

## 9. The Neurosymbolic Solution

The neurosymbolic approach separates concerns effectively:

| Component | Strength | Role in Pipeline |
|-----------|----------|------------------|
| **LLM (Gemma 3N)** | Language understanding | Extract structured facts from natural language |
| **Prolog** | Logical reasoning | Recursively traverse relationships, count generations |
| **Lua** | Mathematical precision | Apply formula `greats = n - 2` exactly |

This architecture ensures:
- **Correctness**: Mathematical operations are deterministic
- **Explainability**: Each step produces verifiable intermediate results
- **Reliability**: The same input always produces the same output

## 10. The Edge Agent (TEA) Features

TEA provides several capabilities that make neurosymbolic AI practical:

- **Polyglot Runtime**: Execute Python, Lua, and Prolog in the same workflow
- **YAML-Based Agents**: Declarative configuration without boilerplate code
- **LLM Agnostic**: Works with Ollama, OpenAI, Anthropic, and other providers
- **Edge-First Design**: Lightweight enough for resource-constrained environments
- **State Management**: Automatic state passing between nodes
- **Checkpoint Persistence**: Save and resume workflow execution

## 11. Next Steps

In future work, I plan to:

1. **Expand neurosymbolic capabilities**: Add more symbolic reasoning backends (Z3, Datalog)
2. **Benchmark reliability**: Systematic comparison across multiple LLM models
3. **Production deployment**: Integrate with monitoring tools for agent observability
4. **Complex reasoning tasks**: Apply to mathematical word problems, planning, and constraint satisfaction

## 12. Conclusion

Building reliable AI agents requires acknowledging the limitations of LLMs. While they excel at understanding and generating natural language, they are not suitable for tasks requiring mathematical precision or logical consistency.

The neurosymbolic approach demonstrated in this article combines the best of both worlds:
- **LLMs** for what they do well (language understanding)
- **Symbolic systems** for what they do well (precise reasoning)

The Edge Agent (TEA) makes this combination practical with its polyglot runtime supporting LLM calls, Prolog reasoning, and Lua calculation in a single YAML-defined workflow.

For developers building AI agents that need to count, calculate, or reason precisely, neurosymbolic AI is not just an option — it's a necessity.

## 13. References

- [The Edge Agent (TEA)](https://github.com/fabceolin/the_edge_agent) - Lightweight state graph library for edge computing
- [Ollama](https://ollama.com) - Run LLMs locally
- [Gemma 3N](https://ai.google.dev/gemma) - Google's efficient language model
- [SWI-Prolog](https://www.swi-prolog.org/) - Logic programming language
- [Neurosymbolic AI: The 3rd Wave](https://arxiv.org/abs/2012.05876) - Academic foundations of the approach
