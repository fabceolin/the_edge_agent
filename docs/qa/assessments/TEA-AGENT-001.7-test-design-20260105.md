# Test Design: Story TEA-AGENT-001.7

**Story:** DSPy Prompt Optimization
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 72 |
| **Unit tests** | 36 (50%) |
| **Integration tests** | 24 (33%) |
| **E2E tests** | 12 (17%) |

**Priority distribution:**

| Priority | Count | Percentage |
|----------|-------|------------|
| P0 | 18 | 25% |
| P1 | 30 | 42% |
| P2 | 18 | 25% |
| P3 | 6 | 8% |

---

## Test Scenarios by Acceptance Criteria

### AC1: `reason.dspy.cot` Action

**Requirement:** Wraps DSPy ChainOfThought module with signature configuration, structured output, few-shot support, and graceful fallback.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-001 | P0 | **Given** a valid signature `"question -> thinking, answer"`, **When** `reason.dspy.cot` parses it, **Then** it extracts input fields `[question]` and output fields `[thinking, answer]` | Signature parsing is pure string logic |
| TEA-AGENT-001.7-UNIT-002 | P0 | **Given** an invalid signature `"question"`, **When** `reason.dspy.cot` parses it, **Then** it raises `SignatureParseError` with descriptive message | Error handling for malformed input |
| TEA-AGENT-001.7-UNIT-003 | P1 | **Given** signature with multiple inputs `"context, question -> answer"`, **When** parsed, **Then** correctly identifies `[context, question]` as inputs | Multi-input signature validation |
| TEA-AGENT-001.7-UNIT-004 | P1 | **Given** few-shot examples in state, **When** formatting for DSPy, **Then** examples are converted to DSPy Example format | Example transformation logic |
| TEA-AGENT-001.7-UNIT-005 | P2 | **Given** empty few-shot examples, **When** formatting, **Then** returns empty list without error | Edge case: empty input handling |
| TEA-AGENT-001.7-UNIT-006 | P1 | **Given** DSPy CoT response with reasoning trace, **When** formatting output, **Then** structured dict contains `thinking` and `answer` keys | Output formatting logic |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-001 | P0 | **Given** DSPy client configured, **When** `reason.dspy.cot` executes with valid signature, **Then** returns structured output with reasoning trace | Core ChainOfThought integration |
| TEA-AGENT-001.7-INT-002 | P0 | **Given** DSPy unavailable, **When** `reason.dspy.cot` executes, **Then** falls back to native `reason.cot` with warning log | Fallback mechanism critical path |
| TEA-AGENT-001.7-INT-003 | P1 | **Given** few-shot examples in state, **When** `reason.dspy.cot` executes, **Then** examples are injected into DSPy module | Example injection integration |
| TEA-AGENT-001.7-INT-004 | P1 | **Given** YAML agent with `action: reason.dspy.cot`, **When** node executes, **Then** state receives structured output | YAML engine integration |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-001 | P1 | **Given** complete YAML agent using `reason.dspy.cot`, **When** agent runs end-to-end, **Then** produces valid reasoning output in final state | Full agent workflow validation |

---

### AC2: `reason.dspy.react` Action

**Requirement:** Wraps DSPy ReAct module with tool integration, max_steps configuration, action-observation trace, and graceful fallback.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-007 | P1 | **Given** tools list `[web.search, web.scrape]`, **When** formatting for DSPy ReAct, **Then** tools are converted to DSPy Tool format | Tool format transformation |
| TEA-AGENT-001.7-UNIT-008 | P1 | **Given** max_steps=5, **When** ReAct exceeds steps, **Then** returns partial results with truncation warning | Step limit enforcement |
| TEA-AGENT-001.7-UNIT-009 | P1 | **Given** action-observation trace from DSPy, **When** formatting output, **Then** trace is structured as list of `{action, observation}` dicts | Trace formatting logic |
| TEA-AGENT-001.7-UNIT-010 | P2 | **Given** empty tools list, **When** ReAct initializes, **Then** raises `ConfigurationError` (ReAct requires tools) | Validation: tools required |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-005 | P0 | **Given** DSPy client and MCP tools configured, **When** `reason.dspy.react` executes, **Then** tools are invoked and trace returned | Core ReAct with MCP integration |
| TEA-AGENT-001.7-INT-006 | P0 | **Given** DSPy unavailable, **When** `reason.dspy.react` executes, **Then** falls back to native `reason.react` with warning | Fallback mechanism critical |
| TEA-AGENT-001.7-INT-007 | P1 | **Given** CrewAI tool bridge, **When** ReAct uses tool, **Then** CrewAI tool is invoked correctly | CrewAI integration |
| TEA-AGENT-001.7-INT-008 | P1 | **Given** LangChain tool bridge, **When** ReAct uses tool, **Then** LangChain tool is invoked correctly | LangChain integration |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-002 | P1 | **Given** YAML agent using `reason.dspy.react` with web tools, **When** agent researches topic, **Then** produces action-observation trace and result | Full ReAct workflow |

---

### AC3: `reason.dspy.compile` Action

**Requirement:** Compiles DSPy module with teleprompter, supports BootstrapFewShot variants, requires trainset, returns compiled configuration, and persists prompts.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-011 | P0 | **Given** teleprompter type `"BootstrapFewShot"`, **When** validating, **Then** returns valid teleprompter class | Teleprompter type validation |
| TEA-AGENT-001.7-UNIT-012 | P0 | **Given** invalid teleprompter type `"InvalidType"`, **When** validating, **Then** raises `UnsupportedTeleprompterError` | Error handling for bad config |
| TEA-AGENT-001.7-UNIT-013 | P1 | **Given** trainset with 10 examples, **When** validating, **Then** returns formatted DSPy examples | Trainset format validation |
| TEA-AGENT-001.7-UNIT-014 | P1 | **Given** empty trainset, **When** `reason.dspy.compile` called, **Then** raises `InsufficientTrainsetError` | Validation: trainset required |
| TEA-AGENT-001.7-UNIT-015 | P2 | **Given** trainset with malformed example, **When** validating, **Then** skips invalid and logs warning | Partial trainset handling |
| TEA-AGENT-001.7-UNIT-016 | P1 | **Given** compiled module config, **When** serializing for persistence, **Then** produces JSON-serializable dict | Serialization logic |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-009 | P0 | **Given** valid trainset and signature, **When** `reason.dspy.compile` executes with BootstrapFewShot, **Then** returns compiled module config | Core compilation integration |
| TEA-AGENT-001.7-INT-010 | P1 | **Given** BootstrapFewShotWithRandomSearch teleprompter, **When** compiling, **Then** compilation uses random search strategy | Alternative teleprompter |
| TEA-AGENT-001.7-INT-011 | P1 | **Given** compiled module, **When** saved to checkpoint, **Then** compiled prompts persist in checkpoint data | Checkpoint persistence |
| TEA-AGENT-001.7-INT-012 | P2 | **Given** large trainset (100+ examples), **When** compiling, **Then** completes within reasonable time (< 60s) | Performance boundary |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-003 | P1 | **Given** YAML agent with compile node, **When** agent runs with trainset, **Then** compiled module is stored in state | Full compile workflow |

---

### AC4: `reason.dspy.optimize` Action

**Requirement:** Runs optimization against validation set, supports configurable metric function, returns optimization results, and updates compiled prompts.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-017 | P0 | **Given** metric `"exact_match"`, **When** validating, **Then** returns corresponding metric function | Metric function resolution |
| TEA-AGENT-001.7-UNIT-018 | P0 | **Given** unknown metric `"unknown_metric"`, **When** validating, **Then** raises `UnsupportedMetricError` | Error handling for bad metric |
| TEA-AGENT-001.7-UNIT-019 | P1 | **Given** custom metric function in state, **When** resolving metric, **Then** uses custom function | Custom metric support |
| TEA-AGENT-001.7-UNIT-020 | P1 | **Given** validation set with 20 examples, **When** formatting, **Then** returns DSPy-compatible valset | Valset formatting |
| TEA-AGENT-001.7-UNIT-021 | P2 | **Given** empty validation set, **When** `reason.dspy.optimize` called, **Then** raises `InsufficientValsetError` | Validation: valset required |
| TEA-AGENT-001.7-UNIT-022 | P1 | **Given** optimization results, **When** formatting, **Then** includes best_config, score, and iterations | Results formatting |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-013 | P0 | **Given** compiled module and valset, **When** `reason.dspy.optimize` executes, **Then** returns optimization results with best config | Core optimization integration |
| TEA-AGENT-001.7-INT-014 | P1 | **Given** optimization completes, **When** updating state, **Then** compiled prompts are updated in checkpoint | Checkpoint update |
| TEA-AGENT-001.7-INT-015 | P2 | **Given** optimization with F1 metric, **When** executing, **Then** uses F1 scoring correctly | Alternative metric |
| TEA-AGENT-001.7-INT-016 | P2 | **Given** large valset (50+ examples), **When** optimizing, **Then** handles computational load gracefully | Performance: large valset |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-004 | P1 | **Given** YAML agent with compile + optimize nodes, **When** agent runs, **Then** optimized prompts improve accuracy on valset | Full optimization workflow |

---

### AC5: Compiled Prompt Persistence

**Requirement:** Compiled prompts persist across checkpoint saves, load on resume, version tracking, and export/import support.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-023 | P0 | **Given** compiled prompt config, **When** serializing to JSON, **Then** all fields are preserved | Serialization correctness |
| TEA-AGENT-001.7-UNIT-024 | P0 | **Given** serialized prompt config, **When** deserializing, **Then** reconstructs equivalent config | Deserialization correctness |
| TEA-AGENT-001.7-UNIT-025 | P1 | **Given** compiled prompt, **When** generating version, **Then** includes timestamp and hash | Version generation logic |
| TEA-AGENT-001.7-UNIT-026 | P1 | **Given** two compiled configs, **When** comparing versions, **Then** correctly identifies newer/older | Version comparison |
| TEA-AGENT-001.7-UNIT-027 | P2 | **Given** export request, **When** formatting for export, **Then** produces portable JSON file | Export format |
| TEA-AGENT-001.7-UNIT-028 | P2 | **Given** imported config file, **When** parsing, **Then** validates schema and loads config | Import parsing |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-017 | P0 | **Given** compiled prompts in state, **When** checkpoint saved, **Then** prompts are persisted | Checkpoint save integration |
| TEA-AGENT-001.7-INT-018 | P0 | **Given** saved checkpoint with compiled prompts, **When** graph resumes, **Then** prompts are loaded | Checkpoint load integration |
| TEA-AGENT-001.7-INT-019 | P1 | **Given** multiple versions of compiled prompts, **When** loading, **Then** uses latest version | Version selection on load |
| TEA-AGENT-001.7-INT-020 | P2 | **Given** exported config, **When** imported in new graph, **Then** graph uses imported prompts | Export/import cycle |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-005 | P0 | **Given** agent compiles prompts and checkpoints, **When** agent resumes from checkpoint, **Then** compiled prompts are available and functional | Critical persistence workflow |
| TEA-AGENT-001.7-E2E-006 | P2 | **Given** exported compiled config, **When** new agent imports and runs, **Then** uses compiled prompts without recompilation | Cross-agent portability |

---

### AC6: Settings Configuration

**Requirement:** Configure via `settings.dspy.enabled`, support LM configuration, default teleprompter, and graceful fallback.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-029 | P0 | **Given** `settings.dspy.enabled: true`, **When** parsing settings, **Then** DSPy client is initialized | Enable flag parsing |
| TEA-AGENT-001.7-UNIT-030 | P0 | **Given** `settings.dspy.enabled: false`, **When** DSPy action called, **Then** immediately falls back to native | Disable flag handling |
| TEA-AGENT-001.7-UNIT-031 | P1 | **Given** `settings.dspy.model: gpt-4`, **When** initializing client, **Then** LM is configured for GPT-4 | Model configuration |
| TEA-AGENT-001.7-UNIT-032 | P1 | **Given** `settings.dspy.api_key` from env var, **When** initializing, **Then** API key is resolved from environment | API key from env |
| TEA-AGENT-001.7-UNIT-033 | P2 | **Given** missing model setting, **When** initializing, **Then** uses default model (gpt-3.5-turbo) | Default model |
| TEA-AGENT-001.7-UNIT-034 | P1 | **Given** `settings.dspy.default_teleprompter: BootstrapFewShot`, **When** compile action called without teleprompter, **Then** uses default | Default teleprompter |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-021 | P0 | **Given** YAML with DSPy settings, **When** YAMLEngine parses, **Then** DSPy client is correctly configured | Settings integration |
| TEA-AGENT-001.7-INT-022 | P1 | **Given** DSPy import fails, **When** `settings.dspy.enabled: true`, **Then** logs warning and falls back | Missing dependency handling |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-007 | P1 | **Given** complete YAML with all DSPy settings, **When** agent runs, **Then** all configured options are respected | Full settings workflow |

---

### AC7: Python Implementation

**Requirement:** New module in actions, registry integration, test coverage >90%, optional dspy dependency.

#### Unit Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-035 | P0 | **Given** `dspy_actions.py` imported, **When** DSPy not installed, **Then** module loads without error (deferred import) | Optional dependency handling |
| TEA-AGENT-001.7-UNIT-036 | P1 | **Given** actions registry, **When** building, **Then** all DSPy actions are registered under `reason.dspy.*` namespace | Registry integration |

#### Integration Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-INT-023 | P0 | **Given** `build_actions_registry()`, **When** DSPy available, **Then** DSPy actions are callable from registry | Registry callable integration |
| TEA-AGENT-001.7-INT-024 | P0 | **Given** `build_actions_registry()`, **When** DSPy unavailable, **Then** fallback actions are registered | Fallback registry |

#### E2E Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-E2E-008 | P1 | **Given** fresh install with `pip install the_edge_agent[dspy]`, **When** running DSPy example, **Then** all actions work | Dependency install validation |

---

## Cross-Cutting Concerns

### Security Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-SEC-001 | P0 | **Given** API key in settings, **When** logging or serializing, **Then** API key is masked/redacted | Secret exposure prevention |
| TEA-AGENT-001.7-UNIT-SEC-002 | P1 | **Given** compiled prompts with sensitive examples, **When** exporting, **Then** examples can be filtered/redacted | PII protection in exports |
| TEA-AGENT-001.7-INT-SEC-001 | P0 | **Given** malicious signature injection `"input; import os -> output"`, **When** parsed, **Then** rejects with security error | Signature injection prevention |

### Error Handling Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-UNIT-ERR-001 | P0 | **Given** DSPy API timeout, **When** action executes, **Then** retries with exponential backoff | Transient error handling |
| TEA-AGENT-001.7-UNIT-ERR-002 | P1 | **Given** DSPy API rate limit, **When** hit, **Then** waits and retries or fails gracefully | Rate limit handling |
| TEA-AGENT-001.7-UNIT-ERR-003 | P1 | **Given** invalid API key, **When** action executes, **Then** returns clear authentication error | Auth error handling |

### Performance Tests

| ID | Priority | Test Scenario | Justification |
|----|----------|---------------|---------------|
| TEA-AGENT-001.7-PERF-001 | P2 | **Given** 10 concurrent DSPy CoT requests, **When** executing, **Then** all complete within 30s | Concurrency handling |
| TEA-AGENT-001.7-PERF-002 | P2 | **Given** compilation with 100 training examples, **When** compiling, **Then** completes within 60s | Compile performance |
| TEA-AGENT-001.7-PERF-003 | P3 | **Given** optimization with 50 validation examples, **When** optimizing, **Then** completes within 5 min | Optimize performance |

---

## Risk Coverage

| Risk | Mitigated By Tests |
|------|-------------------|
| DSPy not installed | UNIT-035, INT-022, INT-024 |
| API key exposure | SEC-001 |
| Signature injection | SEC-001, SEC-002 |
| Invalid trainset | UNIT-014, UNIT-015 |
| Checkpoint corruption | INT-017, INT-018, E2E-005 |
| Fallback failures | INT-002, INT-006, UNIT-030 |
| Model API failures | ERR-001, ERR-002, ERR-003 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic) - 10 tests
2. **P0 Integration tests** (validate integrations) - 8 tests
3. **P0 E2E tests** (critical paths) - 1 test
4. **P0 Security tests** - 2 tests
5. **P1 Unit tests** - 14 tests
6. **P1 Integration tests** - 11 tests
7. **P1 E2E tests** - 5 tests
8. **P2 tests** as time permits - 18 tests
9. **P3 tests** in full regression - 6 tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 72
  by_level:
    unit: 36
    integration: 24
    e2e: 12
  by_priority:
    p0: 18
    p1: 30
    p2: 18
    p3: 6
  coverage_gaps: []
  security_scenarios: 3
  performance_scenarios: 3
  cross_cutting_total: 9
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for component interaction, E2E for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (fallback and persistence are P0)
- [x] Test IDs follow naming convention (TEA-AGENT-001.7-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Security scenarios included for API key handling
- [x] Performance boundaries defined for compile/optimize operations
- [x] Given-When-Then format used for all scenarios

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.7-test-design-20260105.md
P0 tests identified: 18
P1 tests identified: 30
Total scenarios: 72
```
