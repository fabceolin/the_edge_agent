# Test Design: Story TEA-AGENT-001.4-rust

**Story:** Reasoning Techniques (Rust/Embedded)
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Version:** 2.0 (Enhanced with Given-When-Then)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 51 |
| **Unit tests** | 30 (59%) |
| **Integration tests** | 15 (29%) |
| **E2E tests** | 6 (12%) |
| **Priority distribution** | P0: 24, P1: 17, P2: 8, P3: 2 |
| **Security tests** | 3 (included in counts above) |

### Risk-Based Rationale

This story implements core reasoning primitives for edge/embedded deployment. Testing priorities reflect:
- **P0 (24 tests)**: Parsing correctness, loop termination, tool execution, security - failures here cause silent data corruption, infinite loops, or security vulnerabilities
- **P1 (17 tests)**: Multi-model workflows, trace completeness - degraded observability
- **P2 (8 tests)**: Edge cases, format variations - rare conditions
- **P3 (2 tests)**: Cosmetic trace formatting

### Level Distribution Justification

- **Unit (59%)**: Reasoning actions involve complex parsing logic, format handling, and loop control that are pure functions testable in isolation
- **Integration (29%)**: Tool registry integration, LLM provider interaction, and trace file operations require component boundary testing
- **E2E (12%)**: Complete ReAct workflows with real tools and multi-round self-correction cycles validate end-user scenarios

---

## Test Scenarios by Acceptance Criteria

### AC1: `reason.cot` Action (Chain-of-Thought)

**Requirements:**
1. Wraps LLM call with CoT prompting
2. Structured output: `{thinking: String, answer: Value}`
3. Few-shot examples support (inline in YAML)
4. Thinking formats: `step_by_step`, `pros_cons`, `tree`
5. Output parsed via JSON or custom delimiter

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-001 | Unit | P0 | Parse JSON CoT response with thinking and answer fields | Pure parsing logic, critical for all CoT usage | **Given** JSON response `{"thinking": "Step 1: convert...", "answer": 42}` **When** `parse_cot_output()` called **Then** returns `CotOutput{thinking: "Step 1: convert...", answer: 42}` |
| 001.4R-UNIT-002 | Unit | P0 | Parse delimiter-based response (`<thinking>...</thinking>`) | Alternative format, equally critical | **Given** response `<thinking>My reasoning here</thinking>\nAnswer: 42` **When** `parse_cot_output()` called **Then** extracts thinking="My reasoning here", answer=42 |
| 001.4R-UNIT-003 | Unit | P0 | Handle malformed JSON with partial thinking | Error recovery path | **Given** malformed JSON `{"thinking": "partial...` (truncated) **When** `parse_cot_output()` called **Then** returns error with raw output included |
| 001.4R-UNIT-004 | Unit | P1 | Apply step_by_step thinking format template | Template injection logic | **Given** thinking_format=`step_by_step` **When** `format_cot_prompt()` called **Then** prompt contains "Think step by step" instruction |
| 001.4R-UNIT-005 | Unit | P1 | Apply pros_cons thinking format template | Template variation | **Given** thinking_format=`pros_cons` **When** `format_cot_prompt()` called **Then** prompt contains "List pros and cons" instruction |
| 001.4R-UNIT-006 | Unit | P1 | Apply tree thinking format template | Template variation | **Given** thinking_format=`tree` **When** `format_cot_prompt()` called **Then** prompt contains tree structure instructions |
| 001.4R-UNIT-007 | Unit | P1 | Inject few-shot examples into prompt | Prompt construction logic | **Given** few_shot array with 2 examples **When** `format_cot_prompt()` called **Then** prompt includes both examples with problem/thinking/answer |
| 001.4R-INT-001 | Integration | P0 | Execute CoT with mock LLM returning valid JSON | End-to-end action flow | **Given** mock LLM returning valid JSON CoT response **When** `reason.cot` action executed **Then** `CotOutput` stored in state with correct fields |
| 001.4R-INT-002 | Integration | P1 | Execute CoT with few-shot examples | Multi-component prompt building | **Given** mock LLM and 3 few-shot examples **When** `reason.cot` action executed **Then** LLM receives prompt containing all examples |

---

### AC2: `reason.react` Action

**Requirements:**
1. Implements Thought->Action->Observation loop
2. Tools: Rust-native actions only (no MCP/CrewAI)
3. `max_steps` enforced (prevents infinite loops)
4. Early termination on goal achieved
5. Returns full trace and final answer

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-008 | Unit | P0 | Parse ReAct JSON response: thought, action, action_input | Core parsing | **Given** JSON `{"thought": "I need to...", "action": "web.scrape", "action_input": {"url": "..."}}` **When** `parse_react_step()` called **Then** returns valid `ReactStep` struct |
| 001.4R-UNIT-009 | Unit | P0 | Detect "finish" action for early termination | Loop termination logic | **Given** JSON with `"action": "finish"` **When** `is_terminal_action()` called **Then** returns `true` |
| 001.4R-UNIT-010 | Unit | P0 | Enforce max_steps limit (terminates at exactly N) | **CRITICAL SAFETY** | **Given** max_steps=3 and LLM never returns "finish" **When** ReAct loop executes **Then** exactly 3 steps recorded, loop exits gracefully |
| 001.4R-UNIT-011 | Unit | P0 | Accumulate trace across multiple steps | State management | **Given** 3 sequential ReAct steps **When** accumulated **Then** `ReactTrace.steps.len() == 3` and all fields preserved |
| 001.4R-UNIT-012 | Unit | P1 | Handle malformed action_input with graceful error | Error recovery | **Given** JSON with invalid `action_input` structure **When** `parse_react_step()` called **Then** returns error with context (not panic) |
| 001.4R-UNIT-029 | Unit | P0 | Enforce max_steps=1 edge case | Boundary condition | **Given** max_steps=1 **When** ReAct loop starts **Then** executes exactly 1 step regardless of LLM response |
| 001.4R-UNIT-030 | Unit | P1 | Handle max_steps=0 (no execution) | Edge case | **Given** max_steps=0 **When** ReAct loop starts **Then** returns empty trace immediately without LLM call |
| 001.4R-INT-003 | Integration | P0 | Execute ReAct loop with single tool call | Full action execution | **Given** tool `json.transform` registered and mock LLM requesting it **When** ReAct loop runs **Then** tool executed with correct args |
| 001.4R-INT-004 | Integration | P0 | Execute ReAct with max_steps=3, verify termination | Safety boundary | **Given** LLM never says finish **When** ReAct runs with max_steps=3 **Then** terminates after step 3 with trace |
| 001.4R-INT-005 | Integration | P0 | Execute ReAct with early finish detection | Goal achievement path | **Given** LLM returns finish on step 2 of max 5 **When** ReAct runs **Then** terminates at step 2, returns answer |
| 001.4R-INT-006 | Integration | P1 | Execute ReAct with multiple tools in sequence | Multi-tool workflow | **Given** LLM calls memory.store then json.transform **When** 2-step ReAct **Then** both tools executed in sequence |
| 001.4R-E2E-001 | E2E | P0 | Complete ReAct workflow: research goal with web.scrape tool | Critical user journey | **Given** YAML agent with `reason.react` action **When** executed with goal="Find company info" **Then** web.scrape called and final answer returned |

---

### AC3: `reason.self_correct` Action

**Requirements:**
1. Generate->Critique->Improve cycle
2. Configurable improvement rounds
3. Same or different models for generate/critique
4. Returns final output with improvement trace

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-013 | Unit | P1 | Parse generate output for critique step | Parsing logic | **Given** LLM generation output text **When** `prepare_for_critique()` called **Then** output formatted for critic model input |
| 001.4R-UNIT-014 | Unit | P1 | Accumulate improvement rounds in trace | State management | **Given** 3 sequential improvement rounds **When** accumulated **Then** `SelfCorrectOutput.improvement_history.len() == 3` |
| 001.4R-UNIT-031 | Unit | P1 | Execute single improvement round | Core cycle | **Given** improvement_rounds=1 **When** self_correct runs **Then** output has exactly 1 improvement round |
| 001.4R-INT-007 | Integration | P0 | Execute single improvement round with same model | Core workflow | **Given** generator_model=critic_model="ollama:llama3.2" **When** `reason.self_correct` executed **Then** `SelfCorrectOutput` with 1 round |
| 001.4R-INT-008 | Integration | P1 | Execute with different generator/critic models | Multi-model support | **Given** generator_model="ollama:codellama", critic_model="ollama:llama3.2" **When** self_correct runs **Then** correct models called for each phase |
| 001.4R-INT-009 | Integration | P1 | Execute 3 improvement rounds | Multi-round flow | **Given** improvement_rounds=3 **When** self_correct runs **Then** 3 rounds in history with progressive improvements |
| 001.4R-E2E-002 | E2E | P1 | Complete self-correction on code task | User journey validation | **Given** coding_task="Write Rust function to reverse string" **When** self_correct with 2 rounds **Then** improved code returned with critique history |

---

### AC4: `reason.decompose` Action

**Requirements:**
1. Breaks problem into sub-problems
2. Solves sub-problems independently (parallel via rayon)
3. Synthesizes final answer
4. `max_depth` for recursive decomposition

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-015 | Unit | P1 | Parse decomposition response into sub-problems | Parsing logic | **Given** LLM response listing 3 sub-problems **When** `parse_decomposition()` called **Then** returns Vec with 3 sub-problem strings |
| 001.4R-UNIT-016 | Unit | P1 | Enforce max_depth limit (no infinite recursion) | Safety boundary | **Given** max_depth=2 and problem requiring 5 levels **When** decompose runs **Then** stops at depth 2, synthesizes partial |
| 001.4R-UNIT-017 | Unit | P2 | Synthesize answer from parallel sub-solutions | Aggregation logic | **Given** 3 solved sub-problems **When** `synthesize()` called **Then** combined answer includes all sub-solutions |
| 001.4R-UNIT-032 | Unit | P2 | Handle max_depth=1 (no recursion) | Boundary | **Given** max_depth=1 **When** decompose runs **Then** single-level decomposition only, no recursive calls |
| 001.4R-INT-010 | Integration | P0 | Execute decompose with 3 sub-problems | Core workflow | **Given** complex problem **When** `reason.decompose` action executed **Then** 3 sub-problems solved and synthesized |
| 001.4R-INT-011 | Integration | P1 | Verify rayon parallelism (timing check) | Parallel execution | **Given** 3 sub-problems each taking 100ms **When** solved in parallel **Then** total time < 200ms (parallel proof) |
| 001.4R-E2E-003 | E2E | P2 | Complete decompose workflow on complex problem | User journey | **Given** YAML agent with `reason.decompose` action **When** executed with complex math problem **Then** sub-solutions combined into final answer |

---

### AC5: Structured Output Parsing

**Requirements:**
1. JSON parsing with fallback to delimiter-based
2. Regex extraction for `<thinking>...</thinking>` blocks
3. Graceful handling of malformed LLM output
4. Error includes raw output for debugging

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-018 | Unit | P0 | Parse valid JSON output | Happy path | **Given** valid JSON `{"thinking": "...", "answer": {...}}` **When** `parse_structured_output()` called **Then** returns parsed struct |
| 001.4R-UNIT-019 | Unit | P0 | Fallback to delimiter parsing when JSON fails | **CRITICAL for robustness** | **Given** malformed JSON but valid `<thinking>...</thinking>` **When** parse called **Then** delimiter parsing succeeds |
| 001.4R-UNIT-020 | Unit | P0 | Extract `<thinking>` block via regex | Regex correctness | **Given** text with `<thinking>My reasoning</thinking>` **When** regex extraction **Then** "My reasoning" returned |
| 001.4R-UNIT-021 | Unit | P0 | Return error with raw output on total parse failure | Debuggability | **Given** completely unparseable "random garbage" response **When** parse fails **Then** error message includes raw output |
| 001.4R-UNIT-022 | Unit | P2 | Handle nested JSON in answer field | Edge case | **Given** `{"thinking": "...", "answer": {"nested": {"deep": true}}}` **When** parsed **Then** answer is nested object preserved |
| 001.4R-UNIT-023 | Unit | P2 | Handle unicode in thinking block | Edge case | **Given** thinking with unicode chars "思考步骤..." **When** parsed **Then** unicode preserved correctly |
| 001.4R-UNIT-033 | Unit | P1 | Handle empty thinking block | Edge case | **Given** `<thinking></thinking>` **When** parsed **Then** thinking="" (not error) |
| 001.4R-UNIT-034 | Unit | P2 | Handle multiline thinking content | Real LLM output | **Given** multiline `<thinking>\nLine1\nLine2\n</thinking>` **When** parsed **Then** all lines preserved |

---

### AC6: Reasoning Trace

**Requirements:**
1. All actions return `ReasoningTrace` struct
2. Trace format compatible with tracing crate
3. Optional export to file (newline-delimited JSON)
4. Memory-efficient (streaming to file)

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-024 | Unit | P1 | Create ReasoningTrace struct with all fields | Data structure | **Given** trace data from ReAct 3-step execution **When** `ReasoningTrace::new()` **Then** all fields populated (action_type, steps, duration) |
| 001.4R-UNIT-025 | Unit | P1 | Serialize trace to NDJSON format | Export format | **Given** trace with 3 entries **When** `to_ndjson()` called **Then** returns 3 newline-separated JSON strings |
| 001.4R-INT-012 | Integration | P1 | Export trace to file during execution | I/O integration | **Given** ReAct with trace_file="./trace.jsonl" **When** 5-step execution **Then** 5 lines written to file |
| 001.4R-INT-013 | Integration | P2 | Verify streaming export (memory usage stable) | Performance | **Given** trace with 1000 entries **When** streamed to file **Then** peak memory < 2x single entry size |
| 001.4R-INT-017 | Integration | P1 | Integration with tracing crate spans | Observability | **Given** tracing subscriber configured **When** ReAct runs **Then** spans emitted for each step with correct fields |
| 001.4R-E2E-004 | E2E | P2 | Complete workflow with trace file export | Full observability | **Given** YAML agent with trace export enabled **When** workflow completes **Then** trace file contains all steps in NDJSON |

---

### AC7: Rust-Native Tool Registry

**Requirements:**
1. ReAct can use any registered Rust action
2. Tool discovery: `tools.list` returns available actions
3. Tool schema: JSON Schema for action parameters
4. Tool execution: Direct function call (no HTTP)

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-UNIT-026 | Unit | P0 | Register action in tool registry | Core functionality | **Given** Rust action `json.transform` **When** `registry.register()` called **Then** action discoverable via `registry.get("json.transform")` |
| 001.4R-UNIT-027 | Unit | P0 | Generate JSON Schema for action parameters | Schema accuracy | **Given** action with typed params `{input: String, config: Config}` **When** `get_schema()` called **Then** valid JSON Schema with properties returned |
| 001.4R-UNIT-028 | Unit | P1 | List all registered tools | Discovery | **Given** 3 registered actions **When** `tools.list` called **Then** returns 3 tool definitions with names and schemas |
| 001.4R-INT-014 | Integration | P0 | Execute tool via registry from ReAct | Full integration | **Given** ReAct requests tool "json.transform" by name **When** registry lookup + execute **Then** tool runs with correct args, returns result |
| 001.4R-INT-015 | Integration | P1 | Handle unknown tool gracefully | Error path | **Given** ReAct requests nonexistent tool "foo.bar" **When** lookup fails **Then** returns error Result (not panic) |
| 001.4R-E2E-005 | E2E | P1 | ReAct workflow using multiple registered tools | User journey | **Given** YAML agent with ReAct using 3 tools **When** workflow completes **Then** all tools executed via registry |

---

### AC8: Feature Flag

**Requirements:**
1. Actions behind `--features reasoning` cargo flag
2. Minimal dependencies when disabled

| ID | Level | Priority | Test Scenario | Justification | Given-When-Then |
|----|-------|----------|---------------|---------------|-----------------|
| 001.4R-INT-016 | Integration | P2 | Build without `reasoning` feature compiles | Conditional compilation | **Given** Cargo.toml without `reasoning` feature **When** `cargo build` **Then** compiles successfully without reasoning module |
| 001.4R-INT-018 | Integration | P3 | Verify no reasoning deps when feature disabled | Dependency hygiene | **Given** build without `reasoning` feature **When** `cargo tree` checked **Then** no regex/tracing deps from reasoning |
| 001.4R-E2E-006 | E2E | P1 | Full workflow with `--features reasoning` | Release validation | **Given** YAML agent using CoT **When** binary built with `--features reasoning` **Then** workflow completes successfully |

---

## Security Test Scenarios

| ID | Level | Priority | Test Scenario | Risk Mitigated | Given-When-Then |
|----|-------|----------|---------------|----------------|-----------------|
| 001.4R-SEC-001 | Unit | P0 | Reject tool names with path traversal attempts | Command injection | **Given** tool name `../../../etc/passwd` **When** registry lookup **Then** returns error, no file access |
| 001.4R-SEC-002 | Unit | P1 | Sanitize LLM output before file write | File system attacks | **Given** trace file path from config, LLM output with `\0` bytes **When** export to file **Then** null bytes stripped, safe write |
| 001.4R-SEC-003 | Integration | P0 | Tool registry rejects unregistered actions | Unauthorized execution | **Given** ReAct requests action not in registry **When** execution attempted **Then** rejected with error (no arbitrary code exec) |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Severity | Mitigating Tests |
|------|-------------|--------|----------|------------------|
| Infinite ReAct loop | Medium | High | **CRITICAL** | 001.4R-UNIT-010, 001.4R-UNIT-029, 001.4R-INT-004 |
| Malformed LLM output crash | High | Medium | **HIGH** | 001.4R-UNIT-003, 001.4R-UNIT-012, 001.4R-UNIT-019-021 |
| Tool execution failure in ReAct | Medium | High | **HIGH** | 001.4R-INT-003, 001.4R-INT-014, 001.4R-INT-015 |
| Memory exhaustion on large trace | Low | High | **MEDIUM** | 001.4R-INT-013 |
| Parallel decompose race condition | Low | Medium | **LOW** | 001.4R-INT-011 |
| Tool registry injection | Low | Critical | **HIGH** | 001.4R-SEC-001, 001.4R-SEC-003 |
| File system attacks via trace | Low | High | **MEDIUM** | 001.4R-SEC-002 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
- **Parsing Core** (7 tests):
  - 001.4R-UNIT-001, 002, 003 (CoT parsing)
  - 001.4R-UNIT-018, 019, 020, 021 (structured output)
- **ReAct Safety** (5 tests):
  - 001.4R-UNIT-008, 009, 010, 011, 029 (loop control)
- **Tool Registry** (2 tests):
  - 001.4R-UNIT-026, 027 (registration, schema)
- **Security** (2 tests):
  - 001.4R-SEC-001, 001.4R-SEC-003

### Phase 2: P0 Integration Tests
- 001.4R-INT-001 (CoT action flow)
- 001.4R-INT-003, 004, 005 (ReAct execution)
- 001.4R-INT-007 (self-correct core)
- 001.4R-INT-010 (decompose core)
- 001.4R-INT-014 (tool registry integration)

### Phase 3: P0 E2E Tests
- 001.4R-E2E-001 (ReAct critical path)

### Phase 4: P1 Tests
- Remaining unit tests (templates, formats, edge cases)
- Remaining integration tests (multi-model, trace export)
- 001.4R-E2E-002, 005, 006 (self-correct, tools, feature flag)

### Phase 5: P2+ as Time Permits
- Edge cases (unicode, multiline, nested JSON)
- Performance tests (streaming, parallelism)
- Feature flag hygiene

---

## CI Configuration Recommendations

```yaml
# .github/workflows/rust-tests.yaml additions
jobs:
  test-reasoning:
    runs-on: ubuntu-latest
    steps:
      - name: P0 Unit Tests
        run: cargo test --features reasoning reasoning::tests::p0

      - name: Test without reasoning feature
        run: cargo test --no-default-features

      - name: Test with reasoning feature
        run: cargo test --features reasoning

      - name: Check binary size without feature
        run: |
          cargo build --release --no-default-features
          ls -lh target/release/tea
```

---

## Gate YAML Block

```yaml
test_design:
  date: "2026-01-05"
  designer: "Quinn (Test Architect)"
  version: "2.0"
  scenarios_total: 51
  by_level:
    unit: 30
    integration: 15
    e2e: 6
  by_priority:
    p0: 24
    p1: 17
    p2: 8
    p3: 2
  security_tests: 3
  coverage_gaps: []
  high_risk_mitigations:
    - risk: "Infinite ReAct loop"
      severity: CRITICAL
      tests: ["001.4R-UNIT-010", "001.4R-UNIT-029", "001.4R-INT-004"]
    - risk: "Malformed LLM output crash"
      severity: HIGH
      tests: ["001.4R-UNIT-003", "001.4R-UNIT-019", "001.4R-UNIT-021"]
    - risk: "Tool registry injection"
      severity: HIGH
      tests: ["001.4R-SEC-001", "001.4R-SEC-003"]
    - risk: "Tool execution failure"
      severity: HIGH
      tests: ["001.4R-INT-003", "001.4R-INT-014", "001.4R-INT-015"]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-AGENT-001.4-rust-test-design-20260105.md
P0 tests identified: 24
P1 tests identified: 17
Security tests: 3
Total scenarios: 51
Critical paths covered: ReAct loop safety, Output parsing robustness, Tool registry security
```

---

## Quality Checklist

- [x] Every AC has test coverage (AC1: 9, AC2: 12, AC3: 7, AC4: 7, AC5: 8, AC6: 6, AC7: 6, AC8: 3)
- [x] Test levels are appropriate (unit for parsing/logic, integration for boundaries, E2E for workflows)
- [x] No duplicate coverage across levels (each test targets specific aspect)
- [x] Priorities align with business risk (safety-critical paths are P0)
- [x] Test IDs follow naming convention (001.4R-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Edge cases covered at appropriate priority (unicode, multiline, boundary conditions)
- [x] Safety boundaries tested (max_steps, max_depth)
- [x] Security tests included (3 tests for injection, file attacks, unauthorized execution)
- [x] Given-When-Then examples provided for all scenarios
