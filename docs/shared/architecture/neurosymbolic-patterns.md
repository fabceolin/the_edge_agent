# Neurosymbolic AI Patterns Guide

This guide covers common patterns for building neurosymbolic AI systems with The Edge Agent (TEA), combining neural networks with symbolic reasoning.

## What is Neurosymbolic AI?

Neurosymbolic AI combines two complementary approaches:

| Approach | Strengths | Weaknesses |
|----------|-----------|------------|
| **Neural Networks** | Pattern recognition, learning from data, handling ambiguity | Black box, inconsistent logic, hallucinations |
| **Symbolic AI (Prolog)** | Precise logic, explainability, constraint satisfaction | Requires explicit rules, brittle to noise |

**Together they provide:**
- Neural perception + symbolic reasoning
- Pattern matching + logical inference
- Learning from data + domain knowledge
- Predictions + explanations

## Core Patterns

### Pattern 1: Neural Classification → Symbolic Rules

The most common pattern: use ML for classification, then apply business rules.

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Neural Net    │     │    Business     │     │     Final       │
│  (Classification)│ ──► │  Rules (Prolog) │ ──► │    Decision     │
└─────────────────┘     └─────────────────┘     └─────────────────┘
       │                        │                        │
       ▼                        ▼                        ▼
   classification          rule matching           action/routing
   + confidence            + thresholds
```

**Example: Customer Support Routing**

```yaml
nodes:
  # Step 1: Classify the ticket
  - name: classify
    language: lua  # or Python in Python TEA
    run: |
      local text = string.lower(state.ticket_text)

      if string.find(text, "urgent") or string.find(text, "critical") then
        return { category = "high_priority", confidence = 0.92 }
      elseif string.find(text, "refund") or string.find(text, "cancel") then
        return { category = "billing", confidence = 0.88 }
      else
        return { category = "general", confidence = 0.75 }
      end

  # Step 2: Apply routing rules
  - name: route
    language: prolog
    run: |
      state(category, Cat),
      state(confidence, Conf),

      (Cat = high_priority, Conf > 0.8 ->
        Route = immediate_escalation
      ; Cat = billing, Conf > 0.7 ->
        Route = billing_team
      ; Conf < 0.5 ->
        Route = human_review
      ;
        Route = general_queue
      ),

      return(routing_decision, Route).
```

**Use cases:**
- Email/ticket routing
- Content moderation
- Fraud detection with rule-based thresholds
- Medical triage systems

### Pattern 2: LLM Extraction → Prolog Reasoning

Use an LLM to extract structured facts, then reason over them with Prolog.

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│       LLM       │     │    Fact Base    │     │     Prolog      │
│   (Extraction)  │ ──► │  (Structured)   │ ──► │   (Reasoning)   │
└─────────────────┘     └─────────────────┘     └─────────────────┘
       │                        │                        │
       ▼                        ▼                        ▼
   "Alice is Bob's          parent(alice,bob)       grandparent(X,Y)
    mother"                 parent(bob,charlie)     sibling(X,Y)
```

**Example: Family Relationship Inference**

```yaml
nodes:
  # Step 1: Extract relationships from text
  - name: extract
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            Extract family relationships from the text.
            Return JSON: {"relationships": [{"type": "parent|spouse|sibling", "person1": "...", "person2": "..."}]}
        - role: user
          content: "{{ state.input_text }}"
    output: extracted

  # Step 2: Reason over extracted facts
  - name: infer
    language: prolog
    run: |
      state(extracted, Data),
      state(query_person, Person),

      % Parse relationships and find relatives
      % (simplified - actual implementation would parse the JSON)

      (Person = alice ->
        Relatives = [bob, carol]
      ; Person = bob ->
        Relatives = [alice, david]
      ;
        Relatives = []
      ),

      return(relatives, Relatives).
```

**Use cases:**
- Knowledge graph construction
- Legal document analysis
- Genealogy research
- Relationship mapping in narratives

### Pattern 3: CLP(FD) Constraint Solving

Use Prolog's Constraint Logic Programming for optimization problems.

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│    Workflow     │     │    CLP(FD)      │     │    Optimal      │
│     State       │ ──► │   Constraints   │ ──► │    Solution     │
└─────────────────┘     └─────────────────┘     └─────────────────┘
       │                        │                        │
       ▼                        ▼                        ▼
   tasks, deadlines        X in 0..10           schedule with
   dependencies            X + Y #= Total       all constraints met
```

**Example: Task Scheduling**

```yaml
nodes:
  - name: schedule
    language: prolog
    run: |
      state(max_time, MaxT),

      % Task variables
      [T1, T2, T3] ins 0..MaxT,

      % Durations
      D1 = 2, D2 = 3, D3 = 2,

      % Precedence: T1 before T2, T2 before T3
      T1 + D1 #=< T2,
      T2 + D2 #=< T3,

      % Must complete in time
      T3 + D3 #=< MaxT,

      % Minimize start time
      T1 #= 0,

      % Find solution
      label([T1, T2, T3]),

      % Build schedule
      E1 is T1 + D1,
      E2 is T2 + D2,
      E3 is T3 + D3,

      return(schedule, [
        _{task: task1, start: T1, end: E1},
        _{task: task2, start: T2, end: E2},
        _{task: task3, start: T3, end: E3}
      ]).
```

**Use cases:**
- Job shop scheduling
- Resource allocation
- Meeting room booking
- Shift planning
- Sudoku/puzzle solving

### Pattern 4: Multi-Step Reasoning Chain

Chain multiple inference steps, building conclusions from premises.

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Initial   │     │   Step 1    │     │   Step 2    │     │   Step 3    │
│    Facts    │ ──► │  Inference  │ ──► │  Inference  │ ──► │ Conclusion  │
└─────────────┘     └─────────────┘     └─────────────┘     └─────────────┘
       │                   │                   │                   │
       ▼                   ▼                   ▼                   ▼
   symptoms           conditions            severity          recommendation
   [fever, cough]     [flu]                 medium            "Rest..."
```

**Example: Medical Symptom Checker**

```yaml
nodes:
  # Step 1: Match symptoms to conditions
  - name: identify
    language: prolog
    run: |
      state(symptoms, Symptoms),

      (member(fever, Symptoms), member(cough, Symptoms) ->
        Condition = flu
      ; member(sneezing, Symptoms), member(itchy_eyes, Symptoms) ->
        Condition = allergies
      ;
        Condition = unknown
      ),

      return(condition, Condition),
      return(trace, ['Step 1: Matched symptoms to condition']).

  # Step 2: Assess severity
  - name: assess
    language: prolog
    run: |
      state(condition, Cond),
      state(age, Age),
      state(trace, PrevTrace),

      ((Cond = flu, Age > 65) ->
        Severity = high
      ; Cond = flu ->
        Severity = medium
      ;
        Severity = low
      ),

      append(PrevTrace, ['Step 2: Assessed severity'], NewTrace),
      return(severity, Severity),
      return(trace, NewTrace).

  # Step 3: Generate recommendation
  - name: recommend
    language: prolog
    run: |
      state(severity, Sev),
      state(trace, PrevTrace),

      (Sev = high ->
        Rec = 'Seek immediate medical attention'
      ; Sev = medium ->
        Rec = 'Rest and monitor symptoms'
      ;
        Rec = 'Over-the-counter treatment'
      ),

      append(PrevTrace, ['Step 3: Generated recommendation'], FinalTrace),
      return(recommendation, Rec),
      return(reasoning_trace, FinalTrace).
```

**Use cases:**
- Medical diagnosis
- Legal reasoning
- Audit trails
- Debugging/troubleshooting
- Explainable AI

### Pattern 5: Knowledge Graph Queries

Query and traverse knowledge graphs using Prolog's built-in graph capabilities.

```
┌─────────────────────┐     ┌─────────────────────┐
│   Knowledge Graph   │     │    Query Result     │
│   (Prolog facts)    │ ──► │   (Inferred facts)  │
└─────────────────────┘     └─────────────────────┘
         │                           │
         ▼                           ▼
   parent(alice, bob)          grandchildren of
   parent(bob, carol)          alice = [carol, david]
```

**Example:**

```yaml
nodes:
  - name: query_kg
    language: prolog
    run: |
      state(query_person, Person),
      state(relationship, Rel),

      % Embedded knowledge graph
      (Rel = children, Person = alice ->
        Results = [bob, carol]
      ; Rel = grandchildren, Person = alice ->
        Results = [david, eve, frank]
      ; Rel = ancestors, Person = david ->
        Results = [bob, alice]
      ;
        Results = []
      ),

      return(results, Results).
```

**Use cases:**
- Organizational hierarchy queries
- Product taxonomy navigation
- Access control checks
- Recommendation systems

## Validation Pattern

Use Prolog to validate LLM outputs before acting on them.

```yaml
nodes:
  - name: generate
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "Generate a schedule for {{ state.meeting_request }}"
    output: llm_schedule

  - name: validate
    language: prolog
    run: |
      state(llm_schedule, Schedule),

      % Validation rules
      % (extract start/end from schedule)
      Start = 10, End = 14,

      % Business rules
      Start >= 9,      % No earlier than 9 AM
      End =< 17,       % No later than 5 PM
      End > Start.     % End after start

  - name: on_valid
    language: lua
    run: |
      return { validated = true, status = "Schedule approved" }

  - name: on_invalid
    language: lua
    run: |
      return { validated = false, status = "Schedule rejected - business rules violated" }

edges:
  - from: generate
    to: validate
  - from: validate
    to: on_valid
    condition: true
  - from: validate
    to: on_invalid
    condition: false
```

## Error Handling Pattern

Graceful degradation when Prolog reasoning fails:

```yaml
nodes:
  - name: try_prolog
    language: prolog
    run: |
      state(value, V),
      V > 0,            % Fails if V <= 0
      Result is V * 2,
      return(result, Result).

  - name: fallback
    language: lua
    run: |
      -- Prolog failed, use default
      return { result = 0, used_fallback = true }

  - name: continue
    language: lua
    run: |
      return { status = "completed" }

edges:
  - from: __start__
    to: try_prolog
  - from: try_prolog
    to: continue     # Prolog succeeded
    condition: true
  - from: try_prolog
    to: fallback     # Prolog failed
    condition: false
  - from: fallback
    to: continue
```

## Best Practices

### 1. Keep Prolog Code Simple

Avoid complex meta-programming. Use straightforward pattern matching:

```prolog
% Good: Simple pattern matching
(Category = urgent, Confidence > 0.8 ->
  Action = escalate
;
  Action = queue
).

% Avoid: Complex meta-predicates
findall(X, (some_complex_goal(X), another_goal(X)), Results).
```

### 2. Use Lua/Python for State Updates (Rust)

Due to Rust TEA limitations, use Lua for state manipulation:

```yaml
# Portable pattern
- name: prolog_logic
  language: prolog
  run: |
    state(value, V),
    V > 0,
    V < 100.

- name: lua_update
  language: lua
  run: |
    return { processed = true }
```

### 3. Provide Reasoning Traces

Build audit trails for explainability:

```prolog
state(trace, PrevTrace),
% ... do reasoning ...
append(PrevTrace, ['Step N: Did X because Y'], NewTrace),
return(trace, NewTrace).
```

### 4. Handle Unknown Cases

Always provide defaults:

```prolog
(known_case(X) ->
  Result = computed_result
;
  Result = default_value  % Fallback
).
```

### 5. Test Prolog Logic Independently

Test Prolog code in `swipl` before embedding in YAML:

```bash
swipl
?- member(x, [a, b, x]).
true.
```

## When to Use Neurosymbolic AI

| Use Case | Recommended Approach |
|----------|---------------------|
| Classification with business rules | Pattern 1: Neural → Rules |
| Information extraction + reasoning | Pattern 2: LLM → Prolog |
| Scheduling/optimization | Pattern 3: CLP(FD) |
| Explainable decisions | Pattern 4: Reasoning Chain |
| Graph queries/traversal | Pattern 5: Knowledge Graph |
| LLM output validation | Validation Pattern |

## Related Documentation

- [Python Prolog Guide](../../python/prolog-guide.md)
- [Rust Prolog Guide](../../rust/prolog-guide.md)
- [YAML Reference - Prolog Section](../YAML_REFERENCE.md#method-2c-prolog-code)
- [Prolog Examples](../../../examples/prolog/)
