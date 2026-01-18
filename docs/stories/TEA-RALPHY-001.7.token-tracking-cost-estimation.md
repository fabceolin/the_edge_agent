# Story TEA-RALPHY-001.7: Token Tracking & Cost Estimation

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** workflow operator,
**I want** token usage tracking and cost estimation,
**So that** I can monitor AI spending across workflow runs.

## Acceptance Criteria

1. Aggregate `usage` from all `llm.call` responses
2. Track input_tokens and output_tokens separately
3. Apply model-specific pricing (configurable)
4. Report totals at workflow completion
5. Store in workflow state for downstream nodes
6. Optional: persist to LTM for historical tracking

## Tasks / Subtasks

- [ ] Create `TokenTracker` class (AC: 1, 2, 5)
  - [ ] Create `python/src/the_edge_agent/tracking/token_tracker.py`
  - [ ] Implement `TokenTracker` dataclass with `input_tokens`, `output_tokens`, `model`, `timestamp`
  - [ ] Implement `TokenAggregator` class to accumulate across nodes
  - [ ] Add thread-safe accumulation for parallel execution
  - [ ] Store aggregated totals in `state["_token_usage"]`
- [ ] Create pricing configuration schema (AC: 3)
  - [ ] Define `CostConfig` in `python/src/the_edge_agent/tracking/cost_config.py`
  - [ ] Support YAML `settings.cost_tracking.models` configuration
  - [ ] Load default pricing from bundled `data/model_pricing.yaml`
  - [ ] Allow override via environment variable `TEA_COST_CONFIG_PATH`
- [ ] Integrate with `llm.call` action (AC: 1, 2)
  - [ ] Modify `python/src/the_edge_agent/actions/llm_actions.py`
  - [ ] Extract `usage` from API response (OpenAI format)
  - [ ] Extract `usage` from shell provider output (parse JSON if available)
  - [ ] Call `TokenAggregator.add()` after each LLM call
- [ ] Implement cost calculation (AC: 3, 4)
  - [ ] Create `calculate_cost(usage, model, config)` function
  - [ ] Formula: `(input_tokens / 1_000_000) * input_price + (output_tokens / 1_000_000) * output_price`
  - [ ] Handle unknown models with warning and zero cost
- [ ] Add summary reporting (AC: 4, 5)
  - [ ] Create `ralphy.token_summary` action
  - [ ] Output formatted table: model, input_tokens, output_tokens, cost
  - [ ] Store summary in `state["_cost_summary"]`
- [ ] Add LTM persistence (AC: 6)
  - [ ] Create `tracking.persist_to_ltm` action
  - [ ] Schema: `workflow_id`, `run_id`, `timestamp`, `total_cost`, `breakdown`
  - [ ] Support query by date range
- [ ] Add unit tests
  - [ ] Create `python/tests/test_token_tracker.py`
  - [ ] Test aggregation across multiple calls
  - [ ] Test cost calculation accuracy
  - [ ] Test thread-safety in parallel execution

## Dev Notes

### Source Tree

```
python/src/the_edge_agent/
├── tracking/                    # NEW: Token tracking module
│   ├── __init__.py
│   ├── token_tracker.py         # TokenTracker, TokenAggregator
│   └── cost_config.py           # CostConfig, load_pricing()
├── actions/
│   └── llm_actions.py           # MODIFY: Add tracking integration
└── data/
    └── model_pricing.yaml       # NEW: Default pricing table
```

### Token Tracker Implementation

```python
# python/src/the_edge_agent/tracking/token_tracker.py
from dataclasses import dataclass, field
from typing import Dict, List, Optional
from threading import Lock
from datetime import datetime

@dataclass
class TokenUsage:
    model: str
    input_tokens: int
    output_tokens: int
    timestamp: datetime = field(default_factory=datetime.utcnow)
    node_name: Optional[str] = None

@dataclass
class TokenAggregator:
    usages: List[TokenUsage] = field(default_factory=list)
    _lock: Lock = field(default_factory=Lock, repr=False)

    def add(self, usage: TokenUsage) -> None:
        with self._lock:
            self.usages.append(usage)

    def total_by_model(self) -> Dict[str, Dict[str, int]]:
        totals = {}
        for u in self.usages:
            if u.model not in totals:
                totals[u.model] = {"input": 0, "output": 0}
            totals[u.model]["input"] += u.input_tokens
            totals[u.model]["output"] += u.output_tokens
        return totals

    def to_state(self) -> Dict:
        return {
            "usages": [vars(u) for u in self.usages],
            "totals": self.total_by_model(),
        }
```

### Pricing Configuration

```yaml
# python/src/the_edge_agent/data/model_pricing.yaml
models:
  # Anthropic
  claude-sonnet-4-20250514:
    input_per_1m: 3.00
    output_per_1m: 15.00
  claude-opus-4-20250514:
    input_per_1m: 15.00
    output_per_1m: 75.00
  claude-3-5-haiku-20241022:
    input_per_1m: 0.80
    output_per_1m: 4.00

  # OpenAI
  gpt-4o:
    input_per_1m: 2.50
    output_per_1m: 10.00
  gpt-4o-mini:
    input_per_1m: 0.15
    output_per_1m: 0.60

  # Google
  gemini-1.5-pro:
    input_per_1m: 1.25
    output_per_1m: 5.00
  gemini-1.5-flash:
    input_per_1m: 0.075
    output_per_1m: 0.30

# Default for unknown models
default:
  input_per_1m: 1.00
  output_per_1m: 3.00
```

### YAML Integration

```yaml
# Workflow with cost tracking enabled
settings:
  cost_tracking:
    enabled: true
    persist_to_ltm: true
    pricing_override:
      my-custom-model:
        input_per_1m: 2.00
        output_per_1m: 8.00

nodes:
  - name: process_with_llm
    uses: llm.call
    with:
      model: claude-sonnet-4-20250514
      messages:
        - role: user
          content: "Process this: {{ state.input }}"
    output: result

  - name: report_costs
    uses: ralphy.token_summary
    output: cost_report

edges:
  - from: __start__
    to: process_with_llm
  - from: process_with_llm
    to: report_costs
  - from: report_costs
    to: __end__
```

### Output Format

```python
# state["_cost_summary"] after ralphy.token_summary
{
    "total_cost_usd": 0.0234,
    "total_input_tokens": 5420,
    "total_output_tokens": 1230,
    "breakdown": [
        {
            "model": "claude-sonnet-4-20250514",
            "input_tokens": 5420,
            "output_tokens": 1230,
            "cost_usd": 0.0234
        }
    ],
    "formatted": """
┌─────────────────────────┬──────────┬───────────┬──────────┐
│ Model                   │ Input    │ Output    │ Cost     │
├─────────────────────────┼──────────┼───────────┼──────────┤
│ claude-sonnet-4-20250514│ 5,420    │ 1,230     │ $0.0234  │
├─────────────────────────┼──────────┼───────────┼──────────┤
│ TOTAL                   │ 5,420    │ 1,230     │ $0.0234  │
└─────────────────────────┴──────────┴───────────┴──────────┘
"""
}
```

## Testing

**Test Location:** `python/tests/test_token_tracker.py`

```python
import pytest
from the_edge_agent.tracking.token_tracker import TokenUsage, TokenAggregator
from the_edge_agent.tracking.cost_config import calculate_cost, load_pricing

def test_aggregator_thread_safety():
    """Test that aggregator handles concurrent additions."""
    from concurrent.futures import ThreadPoolExecutor

    agg = TokenAggregator()

    def add_usage(i):
        agg.add(TokenUsage(model="test", input_tokens=100, output_tokens=50))

    with ThreadPoolExecutor(max_workers=10) as ex:
        list(ex.map(add_usage, range(100)))

    assert len(agg.usages) == 100
    totals = agg.total_by_model()
    assert totals["test"]["input"] == 10000
    assert totals["test"]["output"] == 5000

def test_cost_calculation():
    """Test cost calculation accuracy."""
    config = load_pricing()

    cost = calculate_cost(
        input_tokens=1_000_000,
        output_tokens=500_000,
        model="claude-sonnet-4-20250514",
        config=config
    )

    # $3.00 for 1M input + $7.50 for 500K output = $10.50
    assert cost == pytest.approx(10.50, rel=0.01)

def test_unknown_model_uses_default():
    """Test that unknown models use default pricing with warning."""
    config = load_pricing()

    cost = calculate_cost(
        input_tokens=1_000_000,
        output_tokens=1_000_000,
        model="unknown-model-xyz",
        config=config
    )

    # Default: $1.00 input + $3.00 output = $4.00
    assert cost == pytest.approx(4.00, rel=0.01)
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_aggregate_usage | Aggregate from multiple calls | 1 |
| test_separate_tokens | Track input/output separately | 2 |
| test_model_pricing | Apply correct pricing | 3 |
| test_summary_report | Generate formatted report | 4 |
| test_store_in_state | Store in `_token_usage` | 5 |
| test_persist_to_ltm | Save to LTM | 6 |
| test_thread_safety | Concurrent additions | 1 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
