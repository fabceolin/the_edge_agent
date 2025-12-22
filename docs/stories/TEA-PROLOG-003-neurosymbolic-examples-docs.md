# Story: TEA-PROLOG-003 - Neurosymbolic Examples & Documentation

## Status

**Draft**

---

## Story

**As a** developer exploring neurosymbolic AI with TEA,
**I want** comprehensive examples and documentation for Prolog integration,
**So that** I can understand how to combine neural network outputs with symbolic logic reasoning in my workflows.

---

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Dependencies:**
- TEA-PY-004 - Prolog Scripting Support in Python TEA (Must be complete)
- TEA-RUST-035 - Prolog Scripting Support in Rust TEA (Must be complete)
- TEA-PROLOG-002 - Cross-Runtime Parity Tests (Should be complete for verified parity)

**Existing System Integration:**

- **Integrates with:** `docs/shared/YAML_REFERENCE.md`, `examples/` directory
- **Technology:** YAML agents, Prolog (SWI-Prolog), Python/Rust TEA
- **Follows pattern:** Existing examples in `examples/*.yaml`
- **Touch points:** Documentation, example agents, tutorials

**Why Neurosymbolic AI?**

Neurosymbolic AI combines:
- **Neural networks** (pattern recognition, classification, embeddings)
- **Symbolic AI** (logic rules, constraint solving, knowledge graphs)

This enables:
- **Explainable decisions**: Logic rules provide human-readable reasoning chains
- **Constraint satisfaction**: CLP(FD) solves discrete constraints from classifier outputs
- **Knowledge integration**: Domain rules combine with learned patterns
- **Reliable inference**: Symbolic logic provides deterministic conclusions

---

## Motivating Example: LLM Reasoning Failure

### The Problem

Consider a text describing an **alternative reality** British royal family where Diana is still alive and married to Charles, and Camilla had children (Thomas, Laura) during an affair with Charles:

> *"King Charles III is married to Princess Diana... Charles maintained a significant extramarital relationship with his mistress, Camilla... Camilla had two children named Thomas and Laura. Although they were born during the height of her relationship with Charles... their biological father remains unknown."*

**Question:** *"Considering that Camilla only had one partner at a time, who are Thomas's siblings?"*

### LLM Failure (Gemini, 2025-12-22)

> **"King Charles III is Thomas's brother."** ❌

The LLM confused "father" with "brother" — a fundamental logical error.

### Correct Answer via Prolog

| Relationship | Names |
|--------------|-------|
| **Full Sibling** | Laura (same parents: Charles + Camilla) |
| **Half-Siblings** | William, Harry (same father Charles, different mother) |

### Why This Happens

1. **Training Data Contamination**: LLMs mix their training knowledge (real royal family) with the provided alternative reality text
2. **Weak Multi-hop Reasoning**: Determining half-siblings requires chained logical steps
3. **Imprecise Definitions**: LLMs don't consistently apply formal definitions like `half_sibling(X,Y) :- parent(P,X), parent(P,Y), X\=Y, \+ sibling(X,Y)`

### The Neurosymbolic Solution

```
┌─────────────────┐     ┌──────────────────────┐     ┌─────────────────┐
│      LLM        │     │   Premise Logic      │     │     PROLOG      │
│   (Extraction)  │ ──► │   (Transformation)   │ ──► │   (Reasoning)   │
│                 │     │                      │     │                 │
│ "Extract only   │     │ "exclusive relation" │     │ sibling(X,Y):-  │
│  relationships  │     │ → resolve parentage  │     │   parent(M,X),  │
│  from text"     │     │                      │     │   parent(M,Y),  │
└─────────────────┘     └──────────────────────┘     │   parent(F,X),  │
                                                      │   parent(F,Y),  │
        ▼                         ▼                   │   M\=F, X\=Y.   │
                                                      └─────────────────┘
   Extracted Facts:          Resolved Facts:              Query Result:
   affair(camilla,charles)   parent(charles,thomas)       half_sibling(thomas,william)
   child_of_affair(thomas,   parent(camilla,thomas)       half_sibling(thomas,harry)
                   charles)  parent(charles,laura)        sibling(thomas,laura)
```

**Reference Implementation:** `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml`

---

## Acceptance Criteria

### Example Agents

1. **AC-1**: Create a **Neural Classifier → Rule Engine** example showing how to pass neural network classification results to Prolog for rule-based reasoning

2. **AC-2**: Create a **CLP(FD) Constraint Solving** example demonstrating finite domain constraint solving from workflow state

3. **AC-3**: Create a **Knowledge Graph Reasoning** example showing Prolog-based inference over a simple knowledge graph

4. **AC-4**: Create a **Multi-Step Reasoning Chain** example demonstrating iterative Prolog queries with state accumulation

5. **AC-5**: Each example includes both a YAML agent file and a companion README explaining the neurosymbolic pattern

### Documentation

6. **AC-6**: Add **Prolog scripting section** to `docs/shared/YAML_REFERENCE.md` covering syntax, state interface, and CLP libraries

7. **AC-7**: Create **Prolog Getting Started** guide for both Python and Rust (`docs/python/prolog-guide.md`, `docs/rust/prolog-guide.md`)

8. **AC-8**: Create **Neurosymbolic AI Patterns** guide explaining common patterns for combining neural and symbolic approaches

9. **AC-9**: Document **installation requirements** (SWI-Prolog, pyswip, swipl-rs) with platform-specific instructions

10. **AC-10**: Add **troubleshooting section** for common Prolog integration issues

### Tutorial Content

11. **AC-11**: Create a **step-by-step tutorial** walking through building a neurosymbolic agent from scratch

12. **AC-12**: Tutorial includes runnable code that works with both Python and Rust TEA

13. **AC-13**: Tutorial demonstrates the complete pattern: Python node (neural) → Prolog node (symbolic) → output

### Quality

14. **AC-14**: All examples pass in both Python and Rust TEA (verified by parity tests)

15. **AC-15**: Documentation reviewed for clarity and accuracy

16. **AC-16**: Code snippets in documentation are tested and working

---

## Technical Notes

### Implementation Approach

#### 1. Example Directory Structure

```
examples/
└── prolog/
    ├── README.md                           # Overview of Prolog examples
    ├── simple-prolog-agent.yaml            # Basic Prolog hello world
    ├── neurosymbolic/
    │   ├── README.md                       # Neurosymbolic patterns overview
    │   ├── classifier-rules.yaml           # AC-1: Neural → Rules
    │   ├── classifier-rules.md             # Companion explanation
    │   ├── clpfd-scheduling.yaml           # AC-2: Constraint solving
    │   ├── clpfd-scheduling.md
    │   ├── knowledge-graph.yaml            # AC-3: KG reasoning
    │   ├── knowledge-graph.md
    │   ├── reasoning-chain.yaml            # AC-4: Multi-step reasoning
    │   └── reasoning-chain.md
    └── rules/
        ├── domain_rules.pl                 # Reusable Prolog rules
        └── inference_rules.pl
```

#### 2. Example 1: Neural Classifier → Rule Engine (AC-1)

```yaml
# examples/prolog/neurosymbolic/classifier-rules.yaml
name: classifier-rules-agent
description: |
  Demonstrates neurosymbolic pattern: Neural classifier produces
  classification, Prolog applies domain rules for final decision.

state_schema:
  input_text: str
  classification: str
  confidence: float
  domain_rules_applied: list
  final_decision: str
  reasoning: str

nodes:
  # Step 1: Neural classification (simulated)
  - name: classify
    run: |
      # In production, this would call an LLM or classifier
      # Here we simulate classification based on keywords
      text = state["input_text"].lower()

      if "urgent" in text or "critical" in text:
          classification = "high_priority"
          confidence = 0.92
      elif "question" in text or "help" in text:
          classification = "support_request"
          confidence = 0.87
      else:
          classification = "general_inquiry"
          confidence = 0.75

      return {
          "classification": classification,
          "confidence": confidence
      }

  # Step 2: Prolog rule engine
  - name: apply_rules
    language: prolog
    run: |
      % Access neural classification results
      state(classification, Class),
      state(confidence, Conf),

      % Domain rules for decision making
      (
        % Rule 1: High priority with high confidence → escalate
        (Class = 'high_priority', Conf > 0.8) ->
          (Decision = 'escalate_immediately',
           Reason = 'High priority classification with strong confidence')

        % Rule 2: Support request → route to support
        ; (Class = 'support_request', Conf > 0.7) ->
          (Decision = 'route_to_support',
           Reason = 'Support request identified')

        % Rule 3: Low confidence → human review
        ; Conf < 0.6 ->
          (Decision = 'human_review',
           Reason = 'Classification confidence too low for automation')

        % Default: Standard processing
        ; (Decision = 'standard_processing',
           Reason = 'General inquiry with acceptable confidence')
      ),

      % Return the decision
      return(final_decision, Decision),
      return(reasoning, Reason),
      return(domain_rules_applied, ['priority_check', 'confidence_threshold', 'routing']).

edges:
  - from: __start__
    to: classify
  - from: classify
    to: apply_rules
  - from: apply_rules
    to: __end__
```

#### 3. Example 2: CLP(FD) Constraint Solving (AC-2)

```yaml
# examples/prolog/neurosymbolic/clpfd-scheduling.yaml
name: clpfd-scheduling-agent
description: |
  Demonstrates CLP(FD) constraint solving for scheduling.
  Given task durations and constraints, finds valid schedule.

state_schema:
  tasks: list          # List of {name, duration} objects
  total_time: int      # Maximum total time available
  constraints: list    # List of precedence constraints
  schedule: list       # Output: scheduled tasks with start times
  feasible: bool

nodes:
  - name: solve_schedule
    language: prolog
    run: |
      :- use_module(library(clpfd)).

      state(tasks, Tasks),
      state(total_time, MaxTime),

      % Extract task info
      length(Tasks, N),

      % Create start time variables
      length(Starts, N),
      Starts ins 0..MaxTime,

      % For simplicity, assume each task takes 1 time unit
      % and tasks must not overlap (sequential execution)
      chain(Starts, #<),

      % All tasks must complete within time limit
      last(Starts, LastStart),
      LastStart #< MaxTime,

      % Solve
      label(Starts),

      % Build schedule output
      return(schedule, Starts),
      return(feasible, true).

edges:
  - from: __start__
    to: solve_schedule
  - from: solve_schedule
    to: __end__

initial_state:
  tasks:
    - {name: "task_a", duration: 1}
    - {name: "task_b", duration: 1}
    - {name: "task_c", duration: 1}
  total_time: 10
  constraints: []
```

#### 4. Example 3: Knowledge Graph Reasoning (AC-3)

```yaml
# examples/prolog/neurosymbolic/knowledge-graph.yaml
name: knowledge-graph-agent
description: |
  Demonstrates Prolog-based knowledge graph reasoning.
  Infers relationships from a simple family knowledge graph.

state_schema:
  query_person: str
  relationship_type: str
  results: list
  inference_chain: str

nodes:
  - name: reason_over_kg
    language: prolog
    run: |
      % Define the knowledge graph (family relationships)
      parent(alice, bob).
      parent(alice, carol).
      parent(bob, david).
      parent(bob, eve).
      parent(carol, frank).

      % Inference rules
      grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
      sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
      ancestor(X, Y) :- parent(X, Y).
      ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

      % Get query parameters
      state(query_person, Person),
      state(relationship_type, RelType),

      % Execute query based on relationship type
      (
        RelType = 'children' ->
          findall(C, parent(Person, C), Results)
        ; RelType = 'grandchildren' ->
          findall(G, grandparent(Person, G), Results)
        ; RelType = 'siblings' ->
          findall(S, sibling(Person, S), Results)
        ; RelType = 'ancestors' ->
          findall(A, ancestor(A, Person), Results)
        ; Results = []
      ),

      return(results, Results),
      return(inference_chain, 'Prolog logical inference over family KB').

edges:
  - from: __start__
    to: reason_over_kg
  - from: reason_over_kg
    to: __end__

initial_state:
  query_person: alice
  relationship_type: grandchildren
```

#### 5. Documentation Structure

```
docs/
├── shared/
│   ├── YAML_REFERENCE.md              # Update: Add Prolog section
│   └── architecture/
│       └── neurosymbolic-patterns.md  # NEW: Patterns guide (AC-8)
│
├── python/
│   └── prolog-guide.md                # NEW: Python Prolog guide (AC-7)
│
└── rust/
    └── prolog-guide.md                # NEW: Rust Prolog guide (AC-7)
```

#### 6. YAML_REFERENCE.md Prolog Section (AC-6)

```markdown
## Prolog Scripting

The Edge Agent supports Prolog scripting for symbolic AI workflows using SWI-Prolog.

### Installation

**Python:**
```bash
# Install with Prolog support
pip install 'the-edge-agent[prolog]'

# System requirement: SWI-Prolog
# Ubuntu/Debian: sudo apt install swi-prolog
# macOS: brew install swi-prolog
```

**Rust:**
```bash
# Build with Prolog feature
cargo build --features prolog
```

### Basic Syntax

```yaml
nodes:
  - name: prolog_node
    language: prolog
    run: |
      state(input, X),
      Y is X * 2,
      return(output, Y).
```

### State Interface

| Predicate | Purpose |
|-----------|---------|
| `state(Key, Value)` | Read state value by key |
| `return(Key, Value)` | Set return value for state update |

### CLP(FD) Constraint Solving

```yaml
- name: solve
  language: prolog
  run: |
    :- use_module(library(clpfd)).
    X in 1..10, Y in 1..10,
    X + Y #= 15,
    label([X, Y]),
    return(solution, [X, Y]).
```
```

### Key Constraints

- **SWI-Prolog Required**: System installation of SWI-Prolog is required
- **Sandbox Mode**: File/network/shell access blocked by default
- **Timeout**: Default 30 seconds, configurable via `prolog_timeout`
- **First Solution**: Returns first solution only (deterministic mode)
- **Thread-Local**: `state/2` and `return/2` use thread-local predicates for parallel safety

---

## Tasks / Subtasks

- [x] **Task 0: Create LLM + Prolog Family Reasoning example** (AC: 1, 5, 11-13) **[COMPLETE]**
  - [x] Create `llm-prolog-family-reasoning.yaml` - flagship neurosymbolic example
  - [x] Create `llm-prolog-family-reasoning.md` - comprehensive documentation
  - [x] Demonstrates LLM failure on logical reasoning (Gemini example)
  - [x] Shows complete 5-node pipeline: LLM extraction → premise application → Prolog reasoning
  - [x] Documents why neurosymbolic approach succeeds where pure LLM fails
  - **Files created:**
    - `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.yaml`
    - `examples/prolog/neurosymbolic/llm-prolog-family-reasoning.md`

- [ ] **Task 1: Create example directory structure** (AC: 1-5)
  - [x] Create `examples/prolog/` directory if not exists
  - [x] Create `examples/prolog/neurosymbolic/` subdirectory
  - [ ] Create `examples/prolog/rules/` for reusable Prolog rules
  - [ ] Create README.md overview files

- [ ] **Task 2: Create Neural Classifier → Rules example** (AC: 1, 5)
  - [ ] Create `classifier-rules.yaml` agent
  - [ ] Create `classifier-rules.md` companion documentation
  - [ ] Test in Python TEA
  - [ ] Test in Rust TEA
  - [ ] Verify parity

- [ ] **Task 3: Create CLP(FD) Scheduling example** (AC: 2, 5)
  - [ ] Create `clpfd-scheduling.yaml` agent
  - [ ] Create `clpfd-scheduling.md` companion documentation
  - [ ] Test constraint solving in Python TEA
  - [ ] Test constraint solving in Rust TEA
  - [ ] Verify parity

- [ ] **Task 4: Create Knowledge Graph Reasoning example** (AC: 3, 5)
  - [ ] Create `knowledge-graph.yaml` agent
  - [ ] Create `knowledge-graph.md` companion documentation
  - [ ] Test inference in Python TEA
  - [ ] Test inference in Rust TEA
  - [ ] Verify parity

- [ ] **Task 5: Create Multi-Step Reasoning Chain example** (AC: 4, 5)
  - [ ] Create `reasoning-chain.yaml` agent with iterative Prolog queries
  - [ ] Create `reasoning-chain.md` companion documentation
  - [ ] Demonstrate state accumulation across reasoning steps
  - [ ] Test in both runtimes

- [ ] **Task 6: Update YAML_REFERENCE.md** (AC: 6)
  - [ ] Add Prolog Scripting section with syntax reference
  - [ ] Document `state/2` and `return/2` predicates
  - [ ] Document CLP(FD) usage
  - [ ] Document configuration options (`prolog_timeout`, `prolog_sandbox`)
  - [ ] Add security notes

- [ ] **Task 7: Create Python Prolog Guide** (AC: 7, 9)
  - [ ] Create `docs/python/prolog-guide.md`
  - [ ] Document pyswip installation
  - [ ] Document SWI-Prolog system requirements
  - [ ] Add quick start example
  - [ ] Add troubleshooting section

- [ ] **Task 8: Create Rust Prolog Guide** (AC: 7, 9)
  - [ ] Create `docs/rust/prolog-guide.md`
  - [ ] Document `--features prolog` build flag
  - [ ] Document swipl-rs requirements
  - [ ] Add quick start example
  - [ ] Add troubleshooting section

- [ ] **Task 9: Create Neurosymbolic Patterns Guide** (AC: 8)
  - [ ] Create `docs/shared/architecture/neurosymbolic-patterns.md`
  - [ ] Explain neural + symbolic combination benefits
  - [ ] Document common patterns (classifier→rules, constraint solving, KG reasoning)
  - [ ] Provide architectural diagrams
  - [ ] Reference example agents

- [ ] **Task 10: Add Troubleshooting Section** (AC: 10)
  - [ ] Common installation issues
  - [ ] "pyswip not found" resolution
  - [ ] "SWI-Prolog not installed" resolution
  - [ ] Timeout debugging
  - [ ] Sandbox violation handling

- [ ] **Task 11: Create Step-by-Step Tutorial** (AC: 11, 12, 13)
  - [ ] Create `docs/shared/tutorials/neurosymbolic-first-agent.md`
  - [ ] Step 1: Install prerequisites
  - [ ] Step 2: Create Python node for classification
  - [ ] Step 3: Create Prolog node for reasoning
  - [ ] Step 4: Connect with edges
  - [ ] Step 5: Run in Python TEA
  - [ ] Step 6: Run in Rust TEA (verify parity)

- [ ] **Task 12: Verify all examples pass parity** (AC: 14)
  - [ ] Run all examples in Python TEA
  - [ ] Run all examples in Rust TEA
  - [ ] Compare results for parity
  - [ ] Document any differences

- [ ] **Task 13: Review documentation** (AC: 15, 16)
  - [ ] Self-review all documentation for clarity
  - [ ] Test all code snippets
  - [ ] Verify links work
  - [ ] Request peer review if possible

---

## Dev Notes

### Relevant Source Tree

```
the_edge_agent/
├── examples/
│   └── prolog/
│       ├── README.md                      # NEW: Overview
│       ├── simple-prolog-agent.yaml       # NEW: Hello world
│       │
│       ├── neurosymbolic/
│       │   ├── README.md                  # NEW: Pattern overview
│       │   │
│       │   │   # FLAGSHIP EXAMPLE (COMPLETE)
│       │   ├── llm-prolog-family-reasoning.yaml   # ✓ LLM extraction → Prolog reasoning
│       │   ├── llm-prolog-family-reasoning.md     # ✓ Comprehensive documentation
│       │   │
│       │   │   # ONTOLOGIES
│       │   ├── ontologies/
│       │   │   ├── family-relationships.md        # ✓ Family ontology documentation
│       │   │   └── family-relationships-schema.json # ✓ JSON Schema for LLM output
│       │   │
│       │   │   # ADDITIONAL EXAMPLES (TODO)
│       │   ├── classifier-rules.yaml      # TODO
│       │   ├── classifier-rules.md        # TODO
│       │   ├── clpfd-scheduling.yaml      # TODO
│       │   ├── clpfd-scheduling.md        # TODO
│       │   ├── knowledge-graph.yaml       # TODO
│       │   ├── knowledge-graph.md         # TODO
│       │   ├── reasoning-chain.yaml       # TODO
│       │   └── reasoning-chain.md         # TODO
│       │
│       └── rules/
│           ├── family_relationships.pl    # ✓ Prolog rules library (sibling, half_sibling, etc.)
│           ├── domain_rules.pl            # TODO: Generic domain rules
│           └── inference_rules.pl         # TODO: Generic inference rules
│
├── docs/
│   ├── shared/
│   │   ├── YAML_REFERENCE.md              # UPDATE: Add Prolog section
│   │   ├── architecture/
│   │   │   └── neurosymbolic-patterns.md  # NEW
│   │   └── tutorials/
│   │       └── neurosymbolic-first-agent.md # NEW
│   │
│   ├── python/
│   │   └── prolog-guide.md                # NEW
│   │
│   └── rust/
│       └── prolog-guide.md                # NEW
```

### Testing

**Run example agents:**
```bash
# Python
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/classifier-rules.yaml

# Rust
cd rust
cargo run --features prolog -- run ../examples/prolog/neurosymbolic/classifier-rules.yaml
```

### Documentation Standards

- Use **Markdown** for all documentation
- Include **runnable code examples** that users can copy-paste
- Add **diagrams** where helpful (Mermaid preferred)
- Follow existing documentation style in the repo
- Keep language **accessible** to developers new to Prolog

---

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Examples may have subtle bugs not caught in development
- **Mitigation:** All examples verified against parity tests from TEA-PROLOG-002
- **Secondary Risk:** Documentation may become outdated as implementation evolves
- **Mitigation:** Link examples to tests, document version numbers

**Compatibility Verification:**

- [ ] Examples work in both Python and Rust TEA
- [ ] Documentation accurate for current API
- [ ] Installation instructions tested on major platforms

---

## Definition of Done

- [ ] 4 neurosymbolic example agents created (AC 1-4)
- [ ] Each example has companion markdown documentation (AC 5)
- [ ] YAML_REFERENCE.md updated with Prolog section (AC 6)
- [ ] Python and Rust Prolog guides created (AC 7)
- [ ] Neurosymbolic patterns guide created (AC 8)
- [ ] Installation requirements documented (AC 9)
- [ ] Troubleshooting section added (AC 10)
- [ ] Step-by-step tutorial created (AC 11-13)
- [ ] All examples pass in both runtimes (AC 14)
- [ ] Documentation reviewed (AC 15-16)
- [ ] Epic TEA-PROLOG-001 updated with completion status

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-22 | 0.1 | Initial story draft created from epic requirements | Sarah (PO) |
| 2025-12-22 | 0.2 | Added flagship example: LLM+Prolog Family Reasoning (demonstrates LLM failure + neurosymbolic solution) | Sarah (PO) |
| 2025-12-22 | 0.3 | Added Family Relationships Ontology (family-relationships.md, JSON Schema), Prolog rules library (family_relationships.pl) | Sarah (PO) |
