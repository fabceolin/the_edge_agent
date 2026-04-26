# Neuro-Symbolic Negotiation Simulator — Sprint 1 (CLI)

> **LLM backend**: this example uses **Ollama** (OpenAI-compat endpoint).
> The default `agent.yaml` points at a remote server with Qwen 3.5; swap
> to a local Ollama serving the bundled `Modelfile.gemma4-e2b` (Gemma 4
> E2B GGUF on disk) by editing `settings.llm` — see comments in the YAML.
> Reasoning models (Qwen 3.x, Gemma 4) ship with thinking-mode on by
> default; the agent passes `extra_body: {reasoning_effort: none}` to
> keep the answer in `content` instead of `thinking`.



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
reply after every turn. Type your counter-offer and **press Enter twice**
(the CLI uses double-enter as a send delimiter so multi-line input is
possible). Type `quit` or `exit` to abort.

How it self-aligns with the default CLI flags:

| What the CLI default looks for | What the agent populates | Where |
|---|---|---|
| `--question-key=…,message,…,next_question` | `next_question` | `filter_turn_response`, `emit_opener` |
| `--response-key=response`                  | `entry_router` reads `state.response` and normalizes to `user_message` | `entry_router` |
| `--complete-key=complete,done,finished`    | `complete` | `filter_final_response` |

This means you do **not** need to pass `--question-key`, `--response-key`,
or `--complete-key` — the agent matches the runner's defaults. A
`settings.interview` block in the YAML declares the canonical names for
forward-compatibility with future TEA releases.

To suppress the realistic-latency sleep (handy when scripting / piping
stdin in CI):

```bash
TEA_HUMANIZE=0 tea-python run --interactive agent.yaml \
    --input '{"scenario_id": "b2b_laptops", "session_id": "demo01"}'
```

If the buyer asks a question (e.g. *"What is the current price?"*), the
solver reaches the `clarify` branch — it answers with the list price and
asks for an offer, **without** committing to anything. The agent never
closes a deal on a non-offer (Section 12 of the article).

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

## 6. Persuasion stack & bot-detection resistance (Section 12 of the article)

Sprint 2 ships the full anti-ratcheting + persuasion stack, configured per-scenario
under `negotiation_policy:` in `scenarios.yaml`.

| Mechanism | Where | Tell it neutralizes |
|-----------|-------|---------------------|
| Karrass 50% rule | Prolog `disciplined_counter/10` | concession-curve regularity |
| Concession budget (anchor stickiness) | Prolog `disciplined_counter/10` | tirelessness |
| Stall detector | Prolog `stall_detected_h/3` | tirelessness |
| Quote expiry | state + Prolog | no real-time costs |
| Higher-authority gambit ("let me check with my VP") | Cialdini *authority* prompt branch | determinism |
| Stochastic latency (log-normal) | `humanize_response` node | latency homogeneity |
| Voss tactic rotation (label / mirror / calibrated_q / accusation_audit) | prompt branches | linguistic perfection |
| Cialdini lever rotation (scarcity / authority / social_proof / reciprocity / commitment) | prompt branches | determinism |
| Mood drift (warm → cordial → firm → stern) | prompt persona | tirelessness |
| Side-talk injection | `humanize_response` node | no off-path content |
| Typo injection (off by default) | `parse_generation` | linguistic perfection |
| First-person framing constraint | prompt | bot-style "we"-passive voice |

The architectural rule: **stochastic in the language, deterministic in the numbers**.
Latency, mood, side-talk, Cialdini/Voss rotation all live in the language layer.
Karrass, budget, stall detector, BATNA, floor, ceiling, counter target all live
in the solver. The price clause is always Python composing solver outputs.

Set `TEA_HUMANIZE=0` in the environment to skip the latency sleep
(useful for unit tests and one-shot scripting).

Frameworks: Karrass (*The Negotiating Game*), Cialdini (*Influence*),
Voss (*Never Split the Difference*), Fisher & Ury (*Getting to Yes*).

## 7. Backlog (future stories)

### Story 7 — Browser UI (Sprint 3)

Port the interactive loop to a browser using
[TEA WASM LLM](../../docs/articles/wasm-llm-browser-inference.md) and
tau-prolog. No server, no API keys, same Prolog rules.
