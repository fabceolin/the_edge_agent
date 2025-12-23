# Neurosymbolic AI Examples

This directory contains examples demonstrating **neurosymbolic AI** patterns with TEA and Prolog.

## What is Neurosymbolic AI?

Neurosymbolic AI combines:

| Approach | Strengths | Used For |
|----------|-----------|----------|
| **Neural** (LLMs, Classifiers) | Pattern recognition, language understanding | Extracting structure from unstructured data |
| **Symbolic** (Prolog) | Formal reasoning, constraint solving | Deterministic inference, explainable decisions |

## The Core Pattern

```
┌─────────────────┐     ┌──────────────────────┐     ┌─────────────────┐
│      LLM        │     │   Premise Logic      │     │     PROLOG      │
│   (Extraction)  │ ──► │   (Transformation)   │ ──► │   (Reasoning)   │
└─────────────────┘     └──────────────────────┘     └─────────────────┘
        │                         │                          │
        ▼                         ▼                          ▼
  Unstructured Text        Resolved Facts             Logical Conclusions
```

## Why Not Just Use LLMs?

LLMs can fail at logical reasoning. Consider this alternative reality scenario:

> "King Charles III is married to Princess Diana... Charles maintained an extramarital relationship with Camilla... Camilla had two children named Thomas and Laura during her affair with Charles."

**Question:** *Who are Thomas's siblings?*

**LLM Response (Gemini 2025-12-22):** "King Charles III is Thomas's brother." :x: **WRONG**

**Prolog Response:**
- Full sibling: Laura (same parents)
- Half-siblings: William, Harry (same father, different mother) :white_check_mark: **CORRECT**

## Examples

### 1. LLM + Prolog Family Reasoning (Flagship Example)

**Files:** `llm-prolog-family-reasoning.yaml`, `llm-prolog-family-reasoning.md`

Demonstrates the complete neurosymbolic pattern:
1. LLM extracts relationships from text
2. Python applies logical premises
3. Prolog performs formal reasoning

This is the **recommended starting point** for understanding neurosymbolic AI.

### 2. Neural Classifier + Rule Engine

**Files:** `classifier-rules.yaml`, `classifier-rules.md`

Pattern: Classification output feeds into domain rules.

```
Text Input ──► Neural Classifier ──► Prolog Rule Engine ──► Decision
              (high_priority)        (if priority + confidence)    (escalate)
```

Use case: Automated ticket routing, content moderation, alert prioritization.

### 3. CLP(FD) Scheduling

**Files:** `clpfd-scheduling.yaml`, `clpfd-scheduling.md`

Pattern: Constraint satisfaction from workflow parameters.

```
Task List ──► Constraint Solver ──► Valid Schedule
              (CLP(FD))             (no conflicts)
```

Use case: Resource allocation, scheduling, planning problems.

### 4. Knowledge Graph Reasoning

**Files:** `knowledge-graph.yaml`, `knowledge-graph.md`

Pattern: Prolog-based inference over knowledge graphs.

```
Query ──► Knowledge Graph ──► Inference Rules ──► Results
          (Prolog facts)      (transitive closure)
```

Use case: Relationship discovery, taxonomy queries, graph analysis.

### 5. Multi-Step Reasoning Chain

**Files:** `reasoning-chain.yaml`, `reasoning-chain.md`

Pattern: Iterative Prolog queries with state accumulation.

```
Initial Query ──► Step 1 ──► Step 2 ──► Step 3 ──► Final Conclusion
                  (facts)    (inference)  (validation)
```

Use case: Complex multi-hop reasoning, proof generation, chain-of-thought.

## Directory Structure

```
neurosymbolic/
├── README.md                              # This file
│
│ # FLAGSHIP EXAMPLE
├── llm-prolog-family-reasoning.yaml       # LLM extraction + Prolog reasoning
├── llm-prolog-family-reasoning.md         # Comprehensive documentation
│
│ # ONTOLOGIES
├── ontologies/
│   ├── family-relationships.md            # Family domain ontology
│   └── family-relationships-schema.json   # JSON Schema for LLM output
│
│ # ADDITIONAL EXAMPLES
├── classifier-rules.yaml                  # Neural → Rules
├── classifier-rules.md
├── clpfd-scheduling.yaml                  # Constraint solving
├── clpfd-scheduling.md
├── knowledge-graph.yaml                   # KG reasoning
├── knowledge-graph.md
├── reasoning-chain.yaml                   # Multi-step reasoning
└── reasoning-chain.md
```

## Running Examples

**Python:**
```bash
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/classifier-rules.yaml
```

**Rust:**
```bash
cd rust
cargo run --features prolog -- run ../examples/prolog/neurosymbolic/classifier-rules.yaml
```

## When to Use Neurosymbolic AI

| Scenario | Pure LLM | Neurosymbolic |
|----------|----------|---------------|
| Simple text extraction | :white_check_mark: | Overkill |
| Multi-hop logical reasoning | :x: Often fails | :white_check_mark: Reliable |
| Constraint satisfaction | :x: Imprecise | :white_check_mark: Exact |
| Explainable decisions | :x: Black box | :white_check_mark: Proof trace |
| Formal rule application | :x: Inconsistent | :white_check_mark: Deterministic |

## Best Practices

1. **Let LLMs do extraction, Prolog do reasoning**
   - LLMs are good at: understanding language, extracting entities
   - Prolog is good at: applying rules, finding solutions

2. **Use schemas for LLM output**
   - Structured JSON output prevents hallucination
   - Schema validation catches errors early

3. **Document your ontology**
   - Define relationships and their semantics
   - Distinguish between extracted facts and derived inferences

4. **Test with parity**
   - Verify examples work identically in Python and Rust
   - Use the parity test framework

## Further Reading

- [Neurosymbolic Patterns Guide](../../../docs/shared/architecture/neurosymbolic-patterns.md)
- [Prolog Guide (Python)](../../../docs/python/prolog-guide.md)
- [Prolog Guide (Rust)](../../../docs/rust/prolog-guide.md)
