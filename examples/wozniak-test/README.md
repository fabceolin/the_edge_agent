# Wozniak Coffee Test Examples

This directory contains implementations of the **Wozniak Coffee Test** — Steve Wozniak's benchmark for AI: walking into an unfamiliar house, finding the coffee, locating the coffee maker, and brewing a good cup.

## Files

| File | Description |
|------|-------------|
| `coffee-agent-simulation.yaml` | Simulated coffee test (no real environment) |
| `system1-system2-blending.yaml` | Demo of System 1/System 2 blending |
| `coffee-agent-ai2thor.yaml` | **Real** coffee test using AI2-THOR |
| `ai2thor_helper.py` | Python helper for AI2-THOR integration |
| `test_coffee_agent.py` | Standalone test script (no TEA required) |

## Quick Start

### Prerequisites

```bash
pip install ai2thor
```

### Test AI2-THOR Integration

```bash
# Simple test (with display)
python examples/wozniak-test/test_coffee_agent.py --scene FloorPlan1

# Zero-knowledge test (procedural house)
python examples/wozniak-test/test_coffee_agent.py --scene Procedural
```

### Run with TEA

```bash
# Fixed scene (debugging)
tea run examples/wozniak-test/coffee-agent-ai2thor.yaml \
    --input '{"scene": "FloorPlan1"}'

# Procedural scene (true zero-knowledge)
tea run examples/wozniak-test/coffee-agent-ai2thor.yaml \
    --input '{"scene": "Procedural"}'
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    AI2-THOR Environment                 │
│  ┌─────────────────────────────────────────────────┐   │
│  │            3D Kitchen Scene                      │   │
│  │  ┌─────┐  ┌─────────┐  ┌─────┐  ┌─────────┐    │   │
│  │  │ Mug │  │ Coffee  │  │ Cup │  │ Cabinet │    │   │
│  │  │     │  │ Machine │  │     │  │         │    │   │
│  │  └─────┘  └─────────┘  └─────┘  └─────────┘    │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│                   TEA Agent                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │   SENSORS    │  │  WORLDVIEW   │  │   ACTIONS    │  │
│  │              │  │              │  │              │  │
│  │ get_objects()│─▶│ Prolog KB    │─▶│ navigate()   │  │
│  │ get_visible()│  │ Commonsense  │  │ pickup()     │  │
│  │ get_state()  │  │ Planning     │  │ toggle()     │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Test Phases

1. **Initialize**: Start AI2-THOR, get scene summary
2. **Scan**: Identify coffee-related objects
3. **Explore**: Open cabinets if items missing
4. **Navigate**: Move to coffee machine
5. **Pickup**: Grab mug/cup
6. **Place**: Put container near machine
7. **Activate**: Turn on coffee machine
8. **Verify**: Dona Maria Principle — check before declaring done

## Scenes

### Kitchen Scenes (FloorPlan1-30)

Pre-built kitchens with known layouts. Good for debugging.

```bash
python test_coffee_agent.py --scene FloorPlan1
python test_coffee_agent.py --scene FloorPlan10
python test_coffee_agent.py --scene FloorPlan25
```

### Procedural Scenes (ProcTHOR)

**Zero-knowledge testing**: Each run generates a unique house.

```bash
python test_coffee_agent.py --scene Procedural
```

## Related Articles

- [Building a Neurosymbolic AI for Atari Pinball](../../docs/articles/atari-pinball-neurosymbolic.md)
- [The Wozniak Coffee Test: Why AI Still Can't Make Coffee](../../docs/articles/wozniak-coffee-test.md)
- [Solving the Wozniak Test with AI2-THOR](../../docs/articles/wozniak-ai2thor-zero-knowledge.md)
