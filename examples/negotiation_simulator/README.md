# Neuro-Symbolic Negotiation Simulator — Sprint 1 (CLI)

Reference implementation for the article
[Neuro-Symbolic B2B Negotiation: Gemma 4 + Prolog CLP(FD)](../../docs/articles/neurosymbolic-negotiation-simulator.md).

The agent splits cognition into two layers:

| Layer | Technology | Owns |
|-------|------------|------|
| **System 1** (Neural) | Gemma 4 E2B-it (GGUF via llama.cpp) | Language: extracting offers, writing tone sentences |
| **System 2** (Symbolic) | SWI-Prolog 9.2 + CLP(FD) | Math: margin, BATNA, discount ceiling, counter-price search |

The LLM never invents a number. The solver never writes a sentence.

---

## 1. Install

```bash
# 1. The Edge Agent + Prolog extras + llama.cpp
pip install 'the-edge-agent[prolog]'
sudo apt install swi-prolog                   # needs >= 9.1
CMAKE_ARGS="-DGGML_OPENMP=OFF" pip install llama-cpp-python

# 2. Pull the Gemma 4 E2B-it GGUF (one-time, ~3 GB for Q4_K_M)
mkdir -p ~/.cache/tea/models
huggingface-cli download bartowski/google_gemma-4-E2B-it-GGUF \
    google_gemma-4-E2B-it-Q4_K_M.gguf \
    --local-dir ~/.cache/tea/models
```

> **No GPU?** Set `n_gpu_layers: 0` in `agent.yaml` under `settings.llm`.
> Q4_K_M runs at ~8–15 tok/s on a modern CPU.

---

## 2. Run

### Interactive (recommended)

```bash
cd examples/negotiation_simulator
tea-python run --interactive agent.yaml \
    --input '{"scenario_id": "b2b_laptops", "session_id": "demo01"}'
```

The agent loads the scenario, emits its opening line, then pauses for your
reply after every turn.  Type your counter-offer and hit enter.

### Available scenarios

`scenarios.yaml` ships with three negotiations:

| `scenario_id` | Deal | Floor (min/unit) |
|---------------|------|------------------|
| `b2b_laptops` | 500× Enterprise Laptop ($1200 list) | BATNA $1050 |
| `saas_annual` | 50× Pro Plan, annual ($200 list) | BATNA $115 |
| `wholesale_shoes` | 1,000× Running shoes ($60 list) | BATNA $45 |

Pass any `scenario_id` via `--input`.

### One-shot (for scripting)

```bash
tea-python run agent.yaml --input '{
  "scenario_id": "b2b_laptops",
  "user_message": "I offer $950 per unit, cash, with free shipping."
}'
```

---

## 3. The audit trail

Every turn appends three records to `./audit/<session_id>.jsonl`:

```
{"step": "neural_in",  "turn": 1, "extracted": {"offer_unit_cents": 95000, ...}}
{"step": "symbolic",   "turn": 1, "decision":  {"action": "counter", "target_cents": 105000, ...}}
{"step": "neural_out", "turn": 1, "agent_response": "Given the volume..."}
```

At end-of-game a `finalize` record summarises whether the seller **won**
(closed ≥ floor) or conceded.

Inspect a session:

```bash
jq -c 'select(.step=="symbolic") | .decision' audit/demo01.jsonl
```

---

## 4. Extending the pool

Add a scenario to `scenarios.yaml`:

```yaml
- id: my_deal
  name: "My custom deal"
  product: "Widget Pro"
  quantity: 100
  unit_cost: 20.00
  list_price: 48.00
  min_margin_pct: 20
  max_discount_pct: 30
  shipping_cost_pct: 0
  interest_by_term: { 30: 0.0, 60: 0.03 }
  batna_unit: 34.00
  persona:
    buyer_role: "Procurement lead"
    tone: "terse, data-driven"
    initial_stance: "opens at 40% discount"
    walk_away_threshold_pct: 25
```

The Prolog solver consumes only the top-level numeric fields. The `persona`
block feeds the neural layer flavor.

---

## 5. Files

| File | Story | Role |
|------|-------|------|
| `scenarios.yaml` | 1 | Financial constraints pool |
| `agent.yaml` | 2–5 | State graph wiring all layers |
| `decide.pl` | 4 | Reference Prolog module (same rules as inline node) |
| `audit/*.jsonl` | Guardrail | Three-step reasoning log per turn |

## 6. Backlog (future stories)

### Story 6 — Concession Discipline (anti-ratcheting)

**Problem:** a human who knows they are talking to a bot can drag the session out
with micro-offers ($950 → $952 → $954…) and learn the seller's concession curve.
Sprint 1's only defense is the 10-turn impasse timer.

**Approach (Prolog + state):**
- Track `concession_count`, `last_target_cents`, `buyer_offer_history`.
- *Karrass rule:* each new counter concedes ≤ 50% of the previous concession.
- *Budget:* after 3 concessions the bot locks to the floor — no further movement.
- *Stall detector:* if the buyer repeats the same offer twice, force `action=reject`.
- *Cialdini on the neural side:* inject scarcity ("quote holds through end of week")
  and final-round signaling as separate flavor branches in the generation prompt.

Frameworks: Karrass (*The Negotiating Game*), Cialdini (*Influence*),
Voss (*Never Split the Difference*), Fisher & Ury (*Getting to Yes*).

### Story 7 — Browser UI (Sprint 2)

Port the interactive loop to a browser using
[TEA WASM LLM](../../docs/articles/wasm-llm-browser-inference.md) and
tau-prolog. No server, no API keys, same Prolog rules.
