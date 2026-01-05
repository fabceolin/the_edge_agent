# Neural Predicate Invention Examples

This directory contains agents demonstrating **Neural Predicate Invention** - a bilevel learning approach where robots automatically discover symbolic abstractions from raw sensor data.

## The Core Insight

> **Inventing predicates = finding the effects of actions**

Instead of manually programming what "empty hand" or "reachable object" means, the system discovers these concepts by observing how sensor readings change when actions are performed.

## Agents

### 1. Effect Discovery (`discover-effects.yaml`)

Analyzes action transitions to discover predicates and their effects.

```bash
./tea run examples/predicate-invention/discover-effects.yaml --input '{
  "transitions": [
    {
      "action": "pick",
      "before": {"gripper": {"aperture": 0.08, "force": 0.01}},
      "after": {"gripper": {"aperture": 0.03, "force": 1.2}}
    }
  ]
}'
```

**Output:** Discovered predicates (`empty_hand`, `holding`) and effect vectors.

### 2. State Classifier (`classify-state.yaml`)

Maps raw sensor readings to predicate truth values.

```bash
./tea run examples/predicate-invention/classify-state.yaml --input '{
  "sensor_state": {
    "gripper": {"position": [0.5, 0.3, 0.2], "aperture": 0.08, "force": 0.01},
    "objects": [
      {"id": "cup", "position": [0.5, 0.3, 0.0], "size": 0.06}
    ]
  }
}'
```

**Output:** `{"empty_hand": true, "holding": null, "reachable": ["cup"]}`

### 3. Action Planner (`plan-actions.yaml`)

Uses discovered predicates to plan action sequences for goals.

```bash
./tea run examples/predicate-invention/plan-actions.yaml --input '{
  "sensor_state": {
    "gripper": {"position": [0.0, 0.0, 0.3], "aperture": 0.08, "force": 0.01},
    "objects": [
      {"id": "alien_artifact", "position": [0.1, 0.0, 0.0], "size": 0.05}
    ]
  },
  "goal": "holding(alien_artifact)"
}'
```

**Output:** `Plan: move_to(alien_artifact) → pick(alien_artifact)`

## Zero-Shot Generalization

The key demonstration: the planner works on **objects never seen during training**.

The predicates `empty_hand`, `reachable(X)`, `holding(X)` are abstract - they apply to ANY object with measurable position and size. Once learned, the robot can plan for cups, bottles, alien artifacts, or moon rocks without retraining.

## Files

| File | Purpose |
|------|---------|
| `discover-effects.yaml` | Effect Discovery agent |
| `classify-state.yaml` | Predicate Classifier agent |
| `plan-actions.yaml` | Action Planning agent |
| `sample-transitions.yaml` | Training data for effect discovery |
| `test-scenarios.yaml` | Test cases for zero-shot generalization |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    BILEVEL LEARNING                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  Transitions ──▶ [discover-effects] ──▶ Predicates     │
│       │               (LLM + Prolog)        │           │
│       │                                     ▼           │
│       │         ┌─────────────────────────────────┐    │
│       │         │         STRIPS Operators        │    │
│       │         │  pick: adds holding, removes    │    │
│       │         │        empty_hand               │    │
│       │         └─────────────────────────────────┘    │
│       │                                     │           │
│       ▼                                     ▼           │
│  Sensor State ──▶ [classify-state] ──▶ [plan-actions]  │
│       │               (LLM + Prolog)    (Prolog)       │
│       │                    │                │           │
│       │                    ▼                ▼           │
│       │              Predicate Values   Action Plan    │
│       │              {empty_hand: true}  [move, pick]  │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## References

- [Bilevel Learning for Neural Predicate Invention](https://arxiv.org/)
- [Learning Symbolic Operators for Task and Motion Planning](https://arxiv.org/abs/2109.13668)
- [STRIPS: A New Approach to Automatic Planning](https://ai.stanford.edu/~nilsson/OnlinePubs-Nils/PublishedPapers/strips.pdf)
