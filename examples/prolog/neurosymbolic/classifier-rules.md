# Neural Classifier + Rule Engine

This example demonstrates the **neurosymbolic pattern** where a neural classifier's output feeds into a Prolog rule engine for deterministic decision making.

## The Problem

Pure ML/LLM-based systems suffer from:

1. **Inconsistency**: Same input can produce different outputs
2. **Opacity**: Decisions are hard to explain
3. **Drift**: Model behavior changes over time
4. **Edge cases**: Novel inputs produce unpredictable results

## The Solution

Separate **perception** from **reasoning**:

```
┌─────────────────────┐     ┌─────────────────────┐     ┌─────────────────────┐
│   Neural Classifier │     │   Prolog Rules      │     │   Decision Output   │
│   (Perception)      │ ──► │   (Reasoning)       │ ──► │   (Deterministic)   │
└─────────────────────┘     └─────────────────────┘     └─────────────────────┘
         │                           │                           │
         ▼                           ▼                           ▼
   classification: "high_priority"   Rule: if high_priority      decision: "escalate"
   confidence: 0.88                  AND production_tag          reason: "Production..."
   tags: ["urgent", "production"]    THEN escalate               sla: 4 hours
```

## How It Works

### Step 1: Neural Classification

The classifier (simulated in this example, real LLM/ML in production) analyzes input and produces:

```python
{
    "classification": "critical_incident",  # Category
    "confidence": 0.95,                     # How certain
    "extracted_tags": ["urgent", "production", "outage"]  # Detected features
}
```

### Step 2: Prolog Rule Engine

Rules are explicit, auditable, and deterministic:

```prolog
% Rule 1: Critical incidents always escalate
(Class = 'critical_incident' ->
    Decision = 'immediate_escalation',
    SLA = 1  % 1 hour response time
)

% Rule 2: High priority + production = on-call escalation
; (Class = 'high_priority', member('production', Tags)) ->
    Decision = 'escalate_to_oncall',
    SLA = 4

% Rule 3: Low confidence = human review
; Conf < 0.7 ->
    Decision = 'human_review',
    SLA = 24

% ... more rules
```

## Benefits

| Aspect | Pure ML/LLM | Neurosymbolic |
|--------|-------------|---------------|
| **Consistency** | Variable | Deterministic |
| **Explainability** | "Black box" | Full rule trace |
| **Auditability** | Difficult | Easy - rules are code |
| **Edge cases** | Unpredictable | Controlled fallback |
| **Updates** | Retrain model | Edit rules |
| **Compliance** | Hard to prove | Rule documentation |

## Use Cases

1. **Ticket Routing**: Classify tickets, apply SLA and escalation rules
2. **Content Moderation**: Detect content type, apply policy rules
3. **Alert Prioritization**: Score severity, apply runbook logic
4. **Fraud Detection**: Flag suspicious activity, apply threshold rules
5. **Medical Triage**: Initial assessment, apply clinical protocols

## Running the Example

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

## Expected Output

For the default input ("URGENT: Our production database is down..."):

```json
{
  "classification": "critical_incident",
  "confidence": 0.95,
  "extracted_tags": ["urgent", "production", "database", "outage", "customer_impact"],
  "decision": "immediate_escalation",
  "reason": "Critical incident detected - requires immediate response",
  "sla_hours": 1,
  "applied_rules": ["rule_critical_incident", "rule_sla_critical"]
}
```

## Test Cases

| Input | Classification | Decision | SLA |
|-------|---------------|----------|-----|
| "Production database down" | critical_incident | immediate_escalation | 1h |
| "Urgent API latency issue" | high_priority | escalate_to_oncall | 4h |
| "Something seems wrong" | general_inquiry | human_review | 24h |
| "Customer can't checkout" | support_request | support_priority | 12h |
| "Want dark mode feature" | feature_request | product_backlog | 72h |

## Extending the Example

### Adding New Rules

Add new rule branches in the Prolog node:

```prolog
% New rule: Security-tagged items get immediate escalation
; member('security', Tags) ->
    Decision = 'security_escalation',
    Reason = 'Security concern detected',
    SLA = 2
```

### Using Real ML Classification

Replace the simulated classifier with a real one:

```yaml
- name: classify_input
  action: llm.call
  with:
    model: gpt-4
    messages:
      - role: system
        content: |
          Classify the following support ticket.
          Return JSON: {"classification": "...", "confidence": 0.XX, "tags": [...]}
      - role: user
        content: "{{ state.input_text }}"
```

### Adding Confidence Thresholds

```prolog
% High confidence threshold for automated decisions
; Conf >= 0.9 ->
    (Decision = 'auto_route', ...)

% Medium confidence with constraints
; (Conf >= 0.7, Conf < 0.9, member('low_risk', Tags)) ->
    (Decision = 'auto_route_with_review', ...)

% Low confidence always human review
; Conf < 0.7 ->
    (Decision = 'human_review', ...)
```

## Architecture Diagram

```
                    Input Text
                         │
                         ▼
          ┌──────────────────────────┐
          │    Neural Classifier     │
          │    ───────────────────   │
          │    • Text analysis       │
          │    • Feature extraction  │
          │    • Category prediction │
          │    • Confidence scoring  │
          └──────────────────────────┘
                         │
            classification, confidence, tags
                         │
                         ▼
          ┌──────────────────────────┐
          │    Prolog Rule Engine    │
          │    ───────────────────   │
          │    • Priority rules      │
          │    • Escalation logic    │
          │    • SLA computation     │
          │    • Rule tracing        │
          └──────────────────────────┘
                         │
            decision, reason, sla, applied_rules
                         │
                         ▼
          ┌──────────────────────────┐
          │    Execution System      │
          │    ───────────────────   │
          │    • Route to queue      │
          │    • Create ticket       │
          │    • Send notifications  │
          │    • Start SLA timer     │
          └──────────────────────────┘
```

## Related Examples

- [LLM + Prolog Family Reasoning](llm-prolog-family-reasoning.md) - Flagship example
- [Knowledge Graph Reasoning](knowledge-graph.md) - Graph-based inference
- [Multi-Step Reasoning Chain](reasoning-chain.md) - Iterative reasoning
