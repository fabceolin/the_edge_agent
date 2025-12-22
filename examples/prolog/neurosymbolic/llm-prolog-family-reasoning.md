# LLM + Prolog Family Reasoning

## The Problem: Why LLMs Fail at Logical Reasoning

### Test Scenario

Consider a text describing an **alternative reality** British royal family:

> *"King Charles III is married to Princess Diana... Charles maintained a significant extramarital relationship with his mistress, Camilla... Camilla had two children named Thomas and Laura. Although they were born during the height of her relationship with Charles... their biological father remains unknown."*

### The Question

> "Considering that Camilla only had one partner at a time, who are Thomas's siblings?"

### Wrong Answer from LLM (Gemini, 2025-12-22)

> "**King Charles III** is Thomas's brother."

**Why is this wrong?**
- The LLM confused "biological father" with "brother"
- Charles is Thomas's **father** (inferred from the premise), not his brother

### Correct Answer (via Prolog)

| Type | Names |
|------|-------|
| **Full Sibling** | Laura |
| **Half-Siblings** | William, Harry |

**Why is this correct?**
- Thomas and Laura have the same parents (Charles + Camilla) → siblings
- Thomas, William, and Harry share the same father (Charles), but different mothers → half-siblings

---

## Why LLMs Fail on This Type of Problem

### 1. Training Data Contamination

LLMs were trained with real information about the royal family. When given a text with an "alternative reality" (Diana still alive, married to Charles), they **mix** training knowledge with the provided text.

### 2. Weak Multi-hop Reasoning

To determine half-siblings, reasoning requires multiple steps:

```
1. Thomas is Camilla's child (explicit in text)
2. Premise: Camilla only had one partner → Charles is Thomas's father
3. Charles is also the father of William and Harry (with Diana)
4. Same parents = siblings, only one parent in common = half-siblings
5. Thomas and Laura: Charles + Camilla → siblings
6. Thomas and William/Harry: Charles + (Camilla vs Diana) → half-siblings
```

LLMs frequently "shortcut" this reasoning and reach incorrect conclusions.

### 3. Precise Definitions

The concept of "half-sibling" has a precise logical definition:

```prolog
half_sibling(X, Y) :-
    parent(P, X), parent(P, Y),  % Share ONE parent
    X \= Y,                       % Different people
    \+ sibling(X, Y).             % NOT full siblings
```

LLMs don't consistently apply formal definitions.

---

## The Neurosymbolic Solution

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         INPUT                                    │
│  Narrative text + Question + Premises                           │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    LLM (Extraction)                             │
│  "Extract ONLY the relationships from the provided text"        │
│                                                                  │
│  Output: List of facts                                           │
│    - parent(charles, william)                                    │
│    - parent(diana, william)                                      │
│    - affair(camilla, charles)                                    │
│    - child_of_affair(thomas, charles)                            │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                 Premise Application                             │
│  "Camilla only had one partner" →                               │
│    child_of_affair(thomas, charles) → parent(charles, thomas)   │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    PROLOG (Reasoning)                           │
│                                                                  │
│  % Logical rules                                                 │
│  sibling(X, Y) :-                                                │
│      parent(M, X), parent(M, Y),                                 │
│      parent(F, X), parent(F, Y),                                 │
│      M \= F, X \= Y.                                             │
│                                                                  │
│  half_sibling(X, Y) :-                                           │
│      parent(P, X), parent(P, Y),                                 │
│      X \= Y,                                                     │
│      \+ sibling(X, Y).                                           │
│                                                                  │
│  ?- half_sibling(thomas, X).                                     │
│  X = william ;                                                   │
│  X = harry.                                                      │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                         OUTPUT                                   │
│  "Thomas's half-siblings: William, Harry"                       │
│  "Thomas's sibling: Laura"                                       │
└─────────────────────────────────────────────────────────────────┘
```

### Why This Architecture Works

| Component | Strength | Avoided Weakness |
|-----------|----------|------------------|
| **LLM** | Excellent at extracting information from text | Avoids asking for logical reasoning |
| **Premises** | Deterministic transformation | No ambiguity |
| **Prolog** | Precise logical reasoning | Only receives extracted facts, no contamination |

---

## TEA Implementation

### YAML Agent Flow

```yaml
nodes:
  # 1. LLM extracts relationships from text
  - name: extract_relationships
    action: llm_generate
    config:
      prompt: "Extract ONLY the relationships from the text..."

  # 2. Apply premises to resolve uncertainties
  - name: apply_premise
    run: |
      # If exclusive relationship premise...
      # child_of_affair → parent

  # 3. Transform to Prolog facts
  - name: generate_prolog_facts
    run: |
      # parent(charles, thomas).
      # parent(camilla, thomas).
      # ...

  # 4. Prolog reasons over the facts
  - name: prolog_reasoning
    language: prolog
    run: |
      % Family relationship rules
      sibling(X, Y) :- ...
      half_sibling(X, Y) :- ...

      % Execute query
      findall(H, half_sibling(thomas, H), Results),
      return(query_result, Results).

  # 5. Format answer
  - name: format_answer
    run: |
      # "Thomas's half-siblings: William, Harry"
```

### Generated Prolog Facts

For the alternative royal family text:

```prolog
% Explicit relationships in text
parent(queen_elizabeth_ii, charles).
parent(prince_philip, charles).
married(charles, diana).
parent(charles, william).
parent(charles, harry).
parent(diana, william).
parent(diana, harry).
affair(camilla, charles).

% Inferred from "exclusive relationship" premise
parent(charles, thomas).
parent(charles, laura).
parent(camilla, thomas).
parent(camilla, laura).
```

### Query and Result

```prolog
?- half_sibling(thomas, X).
X = william ;
X = harry.

?- sibling(thomas, X).
X = laura.
```

---

## How to Run

### Python

```bash
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml \
  --state '{
    "text": "The British royal lineage...",
    "query_person": "Thomas",
    "query_type": "half_siblings",
    "premise": "Camilla only had one partner at a time"
  }'
```

### Rust

```bash
cd rust
cargo run --features prolog -- run ../examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml \
  --state '{"text": "...", "query_person": "Thomas", "query_type": "half_siblings", ...}'
```

---

## Query Variations

| Query Type | Question | Result for Thomas |
|------------|----------|-------------------|
| `siblings` | "Who are the siblings?" | Laura |
| `half_siblings` | "Who are the half-siblings?" | William, Harry |
| `all_siblings` | "Who are all siblings?" | Laura, William, Harry |
| `parents` | "Who are the parents?" | Charles, Camilla |

---

## Lessons Learned

### 1. Use LLMs for what they're good at
- Entity and relationship extraction from text
- Natural language understanding
- Unstructured data normalization

### 2. Use Prolog for what it's good at
- Logical reasoning with formal rules
- Queries over relationship graphs
- Consistent application of definitions

### 3. The combination exceeds the parts
- LLM alone: wrong answer
- Prolog alone: couldn't extract from text
- LLM + Prolog: correct answer with explainability

---

## References

- **Neurosymbolic AI**: [Neurosymbolic AI: The 3rd Wave](https://arxiv.org/abs/2012.05876)
- **SWI-Prolog**: [swi-prolog.org](https://www.swi-prolog.org/)
- **TEA Prolog Integration**: [TEA-PROLOG-001 Epic](../../../docs/stories/TEA-PROLOG-001-prolog-integration-epic.md)
