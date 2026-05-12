# ARC-AGI-3 Neurosymbolic Agent

Neurosymbolic agent for the [ARC Prize 2026 / ARC-AGI-3](https://arcprize.org/) competition,
following the same pattern as the [Atari Pinball agent](../../docs/articles/atari-pinball-neurosymbolic.md).

## Architecture

```
sensors  →  worldview (KuzuDB)  →  reasoner  →  actor (arc_agi)
   ↑                                                      │
   └──────────── observe next frame ──────────────────────┘
```

| Layer | Module | Role |
|---|---|---|
| Sensors | `sensors.py` | grid 64×64 → entities via connected components |
| Worldview | `worldview.py` | KuzuDB embedded graph: Observation/Entity/Action/Hypothesis/Rule |
| Induction | `induction.py` | diff frames → candidate hypotheses in DSL |
| Reasoner | `reasoner.py` | Cypher subgraph match + Prolog rules + hypothesis promotion |
| LLM | `llm_hypotheses.py` | Claude fallback when symbolic induction stalls |
| Actor | `agent.yaml` + `run.py` | TEA orchestration |

## Status

First vertical slice targets game `ls20-9607627b` (only ACTION1..ACTION4).
