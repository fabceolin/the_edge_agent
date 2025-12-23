# Multi-Step Reasoning Chain

This example demonstrates **iterative Prolog reasoning** where each inference step builds on previous conclusions, creating an explicit chain of reasoning.

## The Pattern

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

## How It Works

### Step 1: Symptom → Condition Mapping

```prolog
% Rule: symptom patterns map to conditions
(member(fever, Symptoms), member(cough, Symptoms), member(fatigue, Symptoms),
 Condition = flu)
```

### Step 2: Condition + Demographics → Severity

```prolog
% High risk: flu in elderly
(member(flu, Conditions), Age > 65) ->
    Severity = high
```

### Step 3: Severity → Recommendation

```prolog
Severity = high ->
    Recommendation = 'Seek immediate medical attention'
```

## Why Multi-Step Reasoning?

### 1. Explainability

Each step leaves a trace:
```json
"reasoning_trace": [
    "Step 1: Matched symptoms to conditions",
    "Step 2: Assessed severity based on age and conditions",
    "Step 3: Generated recommendation based on severity"
]
```

### 2. Modularity

Rules can be updated independently:
- Change severity thresholds without touching diagnosis logic
- Add new conditions without changing recommendation rules

### 3. Auditability

For regulated domains (medical, legal, financial), explicit reasoning chains provide audit trails.

## Running the Example

**Python:**
```bash
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/reasoning-chain.yaml
```

**With custom input:**
```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("examples/prolog/neurosymbolic/reasoning-chain.yaml")
compiled = graph.compile()

for event in compiled.invoke({
    "symptoms": ["fever", "cough", "fatigue"],
    "patient_age": 70  # Elderly patient
}):
    print(event)
# Result: high severity, immediate attention recommended
```

## Expected Output

For adult patient (age 35) with flu symptoms:

```json
{
  "symptoms": ["fever", "cough", "fatigue"],
  "patient_age": 35,
  "step1_conditions": ["flu"],
  "step2_severity": "medium",
  "step3_recommendation": "Rest, hydrate, and monitor symptoms. See doctor if worsening.",
  "reasoning_trace": [
    "Step 1: Matched symptoms to conditions",
    "Step 2: Assessed severity based on age and conditions",
    "Step 3: Generated recommendation based on severity"
  ],
  "final_diagnosis": "flu"
}
```

## Use Cases

### 1. Medical Diagnosis

```
Symptoms → Possible Conditions → Risk Factors → Recommendations
```

### 2. Legal Reasoning

```
Facts → Applicable Laws → Legal Analysis → Conclusions
```

### 3. Financial Risk Assessment

```
Indicators → Risk Categories → Aggregate Score → Action Level
```

### 4. Debugging/Troubleshooting

```
Error Logs → Pattern Match → Root Cause → Fix Recommendations
```

## Extending the Example

### Adding More Conditions

```prolog
% COVID-19 pattern
(member(fever, Symptoms), member(cough, Symptoms),
 member(loss_of_taste, Symptoms),
 Condition = covid19)
```

### Adding Risk Factors

```prolog
% Consider comorbidities
state(comorbidities, Comorbidities),
(member(diabetes, Comorbidities) -> RiskMultiplier = 1.5 ; RiskMultiplier = 1.0)
```

### Probabilistic Reasoning

```prolog
% Track confidence scores
ConditionWithConfidence = condition(flu, 0.85),
(Confidence > 0.8 -> HighConfidence = true ; HighConfidence = false)
```

## Key Concepts

### State Accumulation

Each step reads from and writes to workflow state:

```yaml
# Step 1 writes:
step1_conditions: [flu]

# Step 2 reads step1_conditions, writes:
step2_severity: medium

# Step 3 reads both, writes:
step3_recommendation: "..."
```

### Reasoning Trace

The `reasoning_trace` list accumulates explanations:

```prolog
append(PrevTrace, ['Step 2: Assessed severity...'], NewTrace),
return(reasoning_trace, NewTrace).
```

### Fallback Handling

Each step handles unknown cases:

```prolog
% Default if no match
(Conditions = [] -> FinalConditions = [unknown] ; FinalConditions = Conditions)
```

## Architecture Benefits

| Aspect | Single-Step | Multi-Step Chain |
|--------|-------------|------------------|
| **Debuggability** | All-or-nothing | Step-by-step inspection |
| **Maintainability** | Monolithic rules | Modular rule sets |
| **Explainability** | "Black box" | Full trace |
| **Testing** | Integration only | Unit test each step |
| **Reusability** | Copy-paste | Compose steps |

## Related Examples

- [Classifier Rules](classifier-rules.md) - Single-step rule application
- [Knowledge Graph](knowledge-graph.md) - Graph-based inference
- [LLM + Prolog Family Reasoning](llm-prolog-family-reasoning.md) - Complete neurosymbolic pipeline
