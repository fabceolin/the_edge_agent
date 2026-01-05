# Test Design: Story TEA-AGENT-001.4-rust

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 48 |
| **Unit tests** | 28 (58%) |
| **Integration tests** | 14 (29%) |
| **E2E tests** | 6 (13%) |
| **Priority distribution** | P0: 22, P1: 16, P2: 8, P3: 2 |

### Risk-Based Rationale

This story implements core reasoning primitives for edge/embedded deployment. Testing priorities reflect:
- **P0 (22 tests)**: Parsing correctness, loop termination, tool execution - failures here cause silent data corruption or infinite loops
- **P1 (16 tests)**: Multi-model workflows, trace completeness - degraded observability
- **P2 (8 tests)**: Edge cases, format variations - rare conditions
- **P3 (2 tests)**: Cosmetic trace formatting

---

## Test Scenarios by Acceptance Criteria

### AC1: `reason.cot` Action (Chain-of-Thought)

**Requirements:**
1. Wraps LLM call with CoT prompting
2. Structured output: `{thinking: String, answer: Value}`
3. Few-shot examples support (inline in YAML)
4. Thinking formats: `step_by_step`, `pros_cons`, `tree`
5. Output parsed via JSON or custom delimiter

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-001 | Unit | P0 | Parse JSON CoT response with thinking and answer fields | Pure parsing logic, critical for all CoT usage |
| 001.4R-UNIT-002 | Unit | P0 | Parse delimiter-based response (`<thinking>...</thinking>`) | Alternative format, equally critical |
| 001.4R-UNIT-003 | Unit | P0 | Handle malformed JSON with partial thinking | Error recovery path |
| 001.4R-UNIT-004 | Unit | P1 | Apply step_by_step thinking format template | Template injection logic |
| 001.4R-UNIT-005 | Unit | P1 | Apply pros_cons thinking format template | Template variation |
| 001.4R-UNIT-006 | Unit | P1 | Apply tree thinking format template | Template variation |
| 001.4R-UNIT-007 | Unit | P1 | Inject few-shot examples into prompt | Prompt construction logic |
| 001.4R-INT-001 | Integration | P0 | Execute CoT with mock LLM returning valid JSON | End-to-end action flow |
| 001.4R-INT-002 | Integration | P1 | Execute CoT with few-shot examples | Multi-component prompt building |

---

### AC2: `reason.react` Action

**Requirements:**
1. Implements Thought->Action->Observation loop
2. Tools: Rust-native actions only (no MCP/CrewAI)
3. `max_steps` enforced (prevents infinite loops)
4. Early termination on goal achieved
5. Returns full trace and final answer

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-008 | Unit | P0 | Parse ReAct JSON response: thought, action, action_input | Core parsing |
| 001.4R-UNIT-009 | Unit | P0 | Detect "finish" action for early termination | Loop termination logic |
| 001.4R-UNIT-010 | Unit | P0 | Enforce max_steps limit (terminates at exactly N) | Prevents infinite loops |
| 001.4R-UNIT-011 | Unit | P0 | Accumulate trace across multiple steps | State management |
| 001.4R-UNIT-012 | Unit | P1 | Handle malformed action_input with graceful error | Error recovery |
| 001.4R-INT-003 | Integration | P0 | Execute ReAct loop with single tool call | Full action execution |
| 001.4R-INT-004 | Integration | P0 | Execute ReAct with max_steps=3, verify termination | Safety boundary |
| 001.4R-INT-005 | Integration | P0 | Execute ReAct with early finish detection | Goal achievement path |
| 001.4R-INT-006 | Integration | P1 | Execute ReAct with multiple tools in sequence | Multi-tool workflow |
| 001.4R-E2E-001 | E2E | P0 | Complete ReAct workflow: research goal with web.scrape tool | Critical user journey |

---

### AC3: `reason.self_correct` Action

**Requirements:**
1. Generate->Critique->Improve cycle
2. Configurable improvement rounds
3. Same or different models for generate/critique
4. Returns final output with improvement trace

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-013 | Unit | P1 | Parse generate output for critique step | Parsing logic |
| 001.4R-UNIT-014 | Unit | P1 | Accumulate improvement rounds in trace | State management |
| 001.4R-INT-007 | Integration | P0 | Execute single improvement round with same model | Core workflow |
| 001.4R-INT-008 | Integration | P1 | Execute with different generator/critic models | Multi-model support |
| 001.4R-INT-009 | Integration | P1 | Execute 3 improvement rounds | Multi-round flow |
| 001.4R-E2E-002 | E2E | P1 | Complete self-correction on code task | User journey validation |

---

### AC4: `reason.decompose` Action

**Requirements:**
1. Breaks problem into sub-problems
2. Solves sub-problems independently (parallel via rayon)
3. Synthesizes final answer
4. `max_depth` for recursive decomposition

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-015 | Unit | P1 | Parse decomposition response into sub-problems | Parsing logic |
| 001.4R-UNIT-016 | Unit | P1 | Enforce max_depth limit (no infinite recursion) | Safety boundary |
| 001.4R-UNIT-017 | Unit | P2 | Synthesize answer from parallel sub-solutions | Aggregation logic |
| 001.4R-INT-010 | Integration | P0 | Execute decompose with 3 sub-problems | Core workflow |
| 001.4R-INT-011 | Integration | P1 | Verify rayon parallelism (timing check) | Parallel execution |
| 001.4R-E2E-003 | E2E | P2 | Complete decompose workflow on complex problem | User journey |

---

### AC5: Structured Output Parsing

**Requirements:**
1. JSON parsing with fallback to delimiter-based
2. Regex extraction for `<thinking>...</thinking>` blocks
3. Graceful handling of malformed LLM output
4. Error includes raw output for debugging

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-018 | Unit | P0 | Parse valid JSON output | Happy path |
| 001.4R-UNIT-019 | Unit | P0 | Fallback to delimiter parsing when JSON fails | Fallback path |
| 001.4R-UNIT-020 | Unit | P0 | Extract `<thinking>` block via regex | Regex correctness |
| 001.4R-UNIT-021 | Unit | P0 | Return error with raw output on total parse failure | Debuggability |
| 001.4R-UNIT-022 | Unit | P2 | Handle nested JSON in answer field | Edge case |
| 001.4R-UNIT-023 | Unit | P2 | Handle unicode in thinking block | Edge case |

---

### AC6: Reasoning Trace

**Requirements:**
1. All actions return `ReasoningTrace` struct
2. Trace format compatible with tracing crate
3. Optional export to file (newline-delimited JSON)
4. Memory-efficient (streaming to file)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-024 | Unit | P1 | Create ReasoningTrace struct with all fields | Data structure |
| 001.4R-UNIT-025 | Unit | P1 | Serialize trace to NDJSON format | Export format |
| 001.4R-INT-012 | Integration | P1 | Export trace to file during execution | I/O integration |
| 001.4R-INT-013 | Integration | P2 | Verify streaming export (memory usage stable) | Performance |
| 001.4R-E2E-004 | E2E | P2 | Complete workflow with trace file export | Full observability |

---

### AC7: Rust-Native Tool Registry

**Requirements:**
1. ReAct can use any registered Rust action
2. Tool discovery: `tools.list` returns available actions
3. Tool schema: JSON Schema for action parameters
4. Tool execution: Direct function call (no HTTP)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-UNIT-026 | Unit | P0 | Register action in tool registry | Core functionality |
| 001.4R-UNIT-027 | Unit | P0 | Generate JSON Schema for action parameters | Schema accuracy |
| 001.4R-UNIT-028 | Unit | P1 | List all registered tools | Discovery |
| 001.4R-INT-014 | Integration | P0 | Execute tool via registry from ReAct | Full integration |
| 001.4R-E2E-005 | E2E | P1 | ReAct workflow using multiple registered tools | User journey |

---

### AC8: Feature Flag

**Requirements:**
1. Actions behind `--features reasoning` cargo flag
2. Minimal dependencies when disabled

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.4R-INT-015 | Integration | P2 | Build without `reasoning` feature compiles | Conditional compilation |
| 001.4R-INT-016 | Integration | P3 | Verify no reasoning deps when feature disabled | Dependency hygiene |
| 001.4R-E2E-006 | E2E | P3 | Run agent without reasoning feature | Feature isolation |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Infinite ReAct loop | Medium | High | 001.4R-UNIT-010, 001.4R-INT-004 |
| Malformed LLM output crash | High | Medium | 001.4R-UNIT-003, 001.4R-UNIT-012, 001.4R-UNIT-019-021 |
| Tool execution failure in ReAct | Medium | High | 001.4R-INT-003, 001.4R-INT-014 |
| Memory exhaustion on large trace | Low | High | 001.4R-INT-013 |
| Parallel decompose race condition | Low | Medium | 001.4R-INT-011 |

---

## Recommended Execution Order

1. **Phase 1: P0 Unit Tests (Fail Fast)**
   - 001.4R-UNIT-001 through 012 (parsing, loop control)
   - 001.4R-UNIT-018 through 021 (output parsing)
   - 001.4R-UNIT-026, 027 (tool registry)

2. **Phase 2: P0 Integration Tests**
   - 001.4R-INT-001 through 005 (CoT, ReAct flows)
   - 001.4R-INT-007, 010, 014 (self-correct, decompose, tools)

3. **Phase 3: P0 E2E Tests**
   - 001.4R-E2E-001 (ReAct critical path)

4. **Phase 4: P1 Tests**
   - Remaining unit tests (templates, formats)
   - Remaining integration tests (multi-model, trace)
   - 001.4R-E2E-002, 005 (self-correct, tools)

5. **Phase 5: P2+ as Time Permits**
   - Edge cases, feature flag tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 48
  by_level:
    unit: 28
    integration: 14
    e2e: 6
  by_priority:
    p0: 22
    p1: 16
    p2: 8
    p3: 2
  coverage_gaps: []
  high_risk_mitigations:
    - infinite_loop: ["001.4R-UNIT-010", "001.4R-INT-004"]
    - parse_failure: ["001.4R-UNIT-003", "001.4R-UNIT-019-021"]
    - tool_execution: ["001.4R-INT-003", "001.4R-INT-014"]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-AGENT-001.4-rust-test-design-20260105.md
P0 tests identified: 22
Critical paths covered: ReAct loop, Output parsing, Tool registry
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for parsing, integration for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (loop safety = P0)
- [x] Test IDs follow naming convention (001.4R-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Edge cases covered at appropriate priority
- [x] Safety boundaries tested (max_steps, max_depth)
