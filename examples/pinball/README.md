# Atari Video Pinball - Neurosymbolic AI

This example demonstrates a neurosymbolic AI that plays Atari Video Pinball using:

1. **OCAtari** - Object-centric perception (extracts ball, flippers, bumpers)
2. **Prolog** - Symbolic worldview (physics, timing, strategy rules)
3. **LLM** - Meta-learning (improves rules based on gameplay)

## Quick Start

### 1. Install Dependencies

```bash
# Core dependencies
pip install ocatari gymnasium[atari]

# Accept Atari ROM license
pip install gymnasium[accept-rom-license]

# Install TEA (if not already)
cd ../../python && pip install -e .
```

### 2. Test the Environment

```bash
python atari_env.py
```

This will open Video Pinball and run a few random actions to verify everything works.

### 3. Play with AI Rules

```bash
# Visual mode (watch the AI play)
python -m the_edge_agent.cli run pinball-play.yaml \
  --input '{"max_frames": 5000, "render": true}'

# Headless mode (faster, for training)
python -m the_edge_agent.cli run pinball-play.yaml \
  --input '{"max_frames": 5000, "render": false}'
```

### 4. Run the Learning Loop

```bash
python -m the_edge_agent.cli run pinball-learn.yaml \
  --input '{
    "games_per_iteration": 5,
    "max_iterations": 10,
    "frames_per_game": 5000,
    "render": false
  }'
```

## Files

| File | Description |
|------|-------------|
| `atari_env.py` | OCAtari wrapper with state extraction |
| `pinball_rules_v1.pl` | Initial Prolog rules (worldview) |
| `pinball-play.yaml` | TEA agent that plays pinball |
| `pinball-learn.yaml` | TEA agent that improves rules using LLM |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    GAME LOOP (60 FPS)                       │
│                                                             │
│  OCAtari ──▶ Prolog Rules ──▶ Action                       │
│  (sensors)   (worldview)      (flip_left, flip_right, etc) │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  LEARNING LOOP (Offline)                    │
│                                                             │
│  Traces ──▶ Analysis ──▶ LLM ──▶ New Rules                 │
│  (what happened)  (why)  (how to improve)                   │
└─────────────────────────────────────────────────────────────┘
```

## Customization

### Modify Initial Rules

Edit `pinball_rules_v1.pl` to change the AI's initial strategy:

```prolog
% Example: Make the AI more aggressive with nudging
optimal_action(nudge_left) :-
    ball_position(X, _),
    X > 120,  % Ball on right side
    ball_velocity(VX, _),
    VX > 5.   % Moving fast right
```

### Use a Different LLM

Edit `pinball-learn.yaml` to change the model:

```yaml
- name: improve_rules
  action: llm.call
  config:
    provider: "anthropic"  # or "openai", "ollama"
    model: "claude-sonnet-4-20250514"
```

### Adjust Learning Parameters

```bash
python -m the_edge_agent.cli run pinball-learn.yaml \
  --input '{
    "games_per_iteration": 10,  # More games per iteration
    "max_iterations": 20,       # More learning cycles
    "frames_per_game": 10000,   # Longer games
    "render": false
  }'
```

## Troubleshooting

### "No module named 'ocatari'"

```bash
pip install ocatari
```

### "ROM not found"

```bash
pip install gymnasium[accept-rom-license]
```

### "Display not found" (headless server)

```bash
# Use virtual display
apt-get install xvfb
xvfb-run python -m the_edge_agent.cli run pinball-play.yaml ...
```

### Slow performance

- Set `render: false` for training
- Reduce `frames_per_game`
- Use a faster LLM (e.g., `llama3.2:3b` via Ollama)

## See Also

- [Full article](../../docs/articles/atari-pinball-neurosymbolic.md)
- [OCAtari documentation](https://github.com/k4ntz/OC_Atari)
- [Gymnasium Atari](https://gymnasium.farama.org/environments/atari/)
