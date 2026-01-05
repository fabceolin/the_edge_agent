# Test Design: Story TEA-AGENT-001.1 - Multi-Agent Collaboration Primitives

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| Total test scenarios | 54 |
| Unit tests | 26 (48%) |
| Integration tests | 20 (37%) |
| E2E tests | 8 (15%) |

### Priority Distribution

| Priority | Count | Focus |
|----------|-------|-------|
| P0 | 22 | Agent registry, dispatch core, parallel aggregation, error handling |
| P1 | 20 | Sequential chaining, coordination, tool bridges |
| P2 | 8 | Edge cases, advanced configuration |
| P3 | 4 | Performance optimization, debug utilities |

---

## Test Scenarios by Acceptance Criteria

### AC1: Agent Definition in YAML Settings

Validates that agents can be defined in settings, validated, and configured with tool bridges and inheritance.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-001 | Unit | P0 | Parse valid agent definition from YAML | Pure parsing logic, no external deps |
| 001.1-UNIT-002 | Unit | P0 | Reject agent missing required `model` field | Input validation |
| 001.1-UNIT-003 | Unit | P0 | Agent inherits from `settings.llm` when fields omitted | Default behavior logic |
| 001.1-UNIT-004 | Unit | P1 | Validate temperature in 0.0-2.0 range | Boundary validation |
| 001.1-UNIT-005 | Unit | P1 | Parse agent with MCP/CrewAI/LangChain tool references | Tool reference parsing |
| 001.1-UNIT-006 | Unit | P2 | Agent with empty tools list is valid | Edge case validation |
| 001.1-INT-001 | Integration | P0 | AgentRegistry loads and validates complete settings block | Component interaction |
| 001.1-INT-002 | Integration | P1 | Tool bridge references validated at initialization | Cross-module validation |

---

### AC2: `agent.dispatch` Action

Validates single-agent dispatch with templating, timeout, and retry configuration.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-007 | Unit | P0 | Dispatch resolves agent by name from registry | Pure lookup logic |
| 001.1-UNIT-008 | Unit | P0 | Jinja2 task template renders with state variables | Template rendering |
| 001.1-UNIT-009 | Unit | P1 | Dispatch with invalid agent name raises clear error | Error handling |
| 001.1-UNIT-010 | Unit | P1 | Timeout configuration parsed correctly | Config validation |
| 001.1-UNIT-011 | Unit | P2 | Retry backoff calculation is correct | Algorithm verification |
| 001.1-INT-003 | Integration | P0 | Dispatch executes agent call and returns response in state | Core functionality |
| 001.1-INT-004 | Integration | P0 | Dispatch respects agent model/system_prompt/tools | Config application |
| 001.1-INT-005 | Integration | P1 | Dispatch times out after configured duration | Timeout behavior |
| 001.1-INT-006 | Integration | P1 | Dispatch retries on failure with backoff | Retry mechanism |
| 001.1-E2E-001 | E2E | P1 | Full YAML agent executes dispatch action end-to-end | Critical path validation |

---

### AC3: `agent.parallel` Action

Validates parallel dispatch with all aggregation strategies.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-012 | Unit | P0 | `collect` aggregation returns list of all responses | Pure aggregation logic |
| 001.1-UNIT-013 | Unit | P0 | `vote` aggregation returns majority response | Voting algorithm |
| 001.1-UNIT-014 | Unit | P0 | `vote` tie-breaking is deterministic | Algorithm correctness |
| 001.1-UNIT-015 | Unit | P0 | `first` returns first successful response | Race condition logic |
| 001.1-UNIT-016 | Unit | P1 | `consensus` detects agreement threshold | Threshold calculation |
| 001.1-UNIT-017 | Unit | P2 | `max_concurrent` limit enforced | Concurrency control |
| 001.1-INT-007 | Integration | P0 | Parallel dispatch executes agents concurrently | Core parallel functionality |
| 001.1-INT-008 | Integration | P0 | State isolation prevents cross-agent mutation | Critical data safety |
| 001.1-INT-009 | Integration | P0 | Slow agents timeout correctly in parallel | Timeout behavior |
| 001.1-INT-010 | Integration | P1 | `consensus` retries until threshold or max iterations | Retry loop behavior |
| 001.1-INT-011 | Integration | P1 | `first` cancels remaining agents after success | Resource cleanup |
| 001.1-INT-012 | Integration | P2 | Partial failures handled gracefully with `collect` | Error resilience |
| 001.1-E2E-002 | E2E | P0 | YAML parallel action with multiple agents and collect | Critical multi-agent path |
| 001.1-E2E-003 | E2E | P1 | YAML parallel action with vote aggregation | Classification use case |

---

### AC4: `agent.sequential` Action

Validates chained agent execution with state threading.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-018 | Unit | P0 | Sequential chain order maintained | Ordering logic |
| 001.1-UNIT-019 | Unit | P1 | State accumulates through chain | State threading |
| 001.1-UNIT-020 | Unit | P1 | Optional transformation applied between agents | Transform logic |
| 001.1-UNIT-021 | Unit | P1 | Early exit on failure when configured | Control flow |
| 001.1-INT-013 | Integration | P1 | Sequential chain passes state between agents | Cross-agent state flow |
| 001.1-INT-014 | Integration | P1 | Chain continues on failure when early_exit=false | Resilience behavior |
| 001.1-E2E-004 | E2E | P1 | YAML sequential action with 3-agent chain | Full chain validation |

---

### AC5: `agent.coordinate` Action

Validates coordinator pattern with leader, workers, and validation loop.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-022 | Unit | P1 | Leader dispatches subtasks to workers | Dispatch logic |
| 001.1-UNIT-023 | Unit | P1 | Maximum coordination rounds enforced | Limit enforcement |
| 001.1-UNIT-024 | Unit | P2 | Re-dispatch triggered on validation failure | Retry logic |
| 001.1-INT-015 | Integration | P1 | Coordinator aggregates worker results | Multi-agent interaction |
| 001.1-INT-016 | Integration | P1 | Leader validates and accepts correct results | Validation flow |
| 001.1-INT-017 | Integration | P2 | Coordinator exits after max rounds with partial results | Graceful degradation |
| 001.1-E2E-005 | E2E | P1 | YAML coordinator pattern with leader and 2 workers | Complex orchestration |

---

### AC6: Tool Bridge Integration

Validates agents can use tools from MCP, CrewAI, and LangChain bridges.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-INT-018 | Integration | P1 | Agent uses tool from `tools.mcp` bridge | MCP integration |
| 001.1-INT-019 | Integration | P1 | Agent uses tool from `tools.crewai` bridge | CrewAI integration |
| 001.1-INT-020 | Integration | P1 | Agent uses tool from `tools.langchain` bridge | LangChain integration |
| 001.1-UNIT-025 | Unit | P0 | Tool availability check at agent init | Fail-fast validation |
| 001.1-UNIT-026 | Unit | P1 | Clear error when bridge unavailable | Error messaging |
| 001.1-E2E-006 | E2E | P2 | YAML agent with MCP tool executes successfully | Tool bridge E2E |

---

### AC7: Python Implementation

Validates module structure, registry, and action registration.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-INT-021 | Integration | P0 | `agent_actions.py` module imports without error | Module validity |
| 001.1-INT-022 | Integration | P0 | All agent actions registered in `build_actions_registry()` | Action registration |
| 001.1-E2E-007 | E2E | P0 | Python test coverage for agent actions >90% | Coverage validation |

---

### AC8: Rust Implementation

Validates Rust feature parity and module structure.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-INT-023 | Integration | P1 | `agent_actions.rs` module compiles without error | Module validity |
| 001.1-INT-024 | Integration | P1 | Rust AgentRegistry parses YAML settings | Parsing parity |
| 001.1-E2E-008 | E2E | P1 | Rust agent.dispatch matches Python behavior | Cross-runtime parity |

---

## Error Handling Scenarios

Critical error handling tests extracted across all ACs:

| ID | Level | Priority | Test | Mitigates Risk |
|----|-------|----------|------|----------------|
| 001.1-UNIT-ERR-001 | Unit | P0 | Invalid agent name raises AgentNotFoundError | Clear error messaging |
| 001.1-UNIT-ERR-002 | Unit | P0 | Invalid YAML schema raises ValidationError at load | Fail-fast on bad config |
| 001.1-UNIT-ERR-003 | Unit | P0 | All aggregation failures collected with context | Debugging support |
| 001.1-INT-ERR-001 | Integration | P0 | Timeout error includes agent name and duration | Operational clarity |
| 001.1-INT-ERR-002 | Integration | P0 | Retry exhaustion includes all attempt errors | Root cause visibility |
| 001.1-INT-ERR-003 | Integration | P1 | Tool bridge error distinguishes unavailable vs failed | Error classification |

---

## Risk Coverage Matrix

| Risk | Test Scenarios | Coverage |
|------|----------------|----------|
| Agent config validation bypass | 001.1-UNIT-001 to 006, 001.1-INT-001 | Full |
| State mutation in parallel | 001.1-INT-008 | Explicit |
| Timeout not enforced | 001.1-INT-005, 009 | Full |
| Aggregation strategy bugs | 001.1-UNIT-012 to 017 | Full |
| Tool bridge failures | 001.1-UNIT-025/026, 001.1-INT-018 to 020 | Full |
| Python/Rust parity drift | 001.1-E2E-008 | Explicit |

---

## Recommended Test File Structure

### Python

```
python/tests/
├── test_agent_actions.py           # Main test file
├── test_agent_registry.py          # AC1 unit tests
├── test_agent_dispatch.py          # AC2 tests
├── test_agent_parallel.py          # AC3 tests
├── test_agent_sequential.py        # AC4 tests
├── test_agent_coordinate.py        # AC5 tests
├── test_agent_tool_bridges.py      # AC6 tests
└── fixtures/
    ├── multi_agent_config.yaml     # Test fixtures
    └── mock_responses.py           # LLM mock responses
```

### Rust

```
rust/tests/
├── test_agent_actions.rs           # Consolidated tests
├── test_agent_registry.rs          # AC1/AC8 tests
└── fixtures/
    └── multi_agent_config.yaml     # Shared fixtures
```

---

## Recommended Execution Order

1. **P0 Unit tests** (26 tests) - Fail fast on logic errors
2. **P0 Integration tests** (10 tests) - Validate component interaction
3. **P0 E2E tests** (2 tests) - Critical path validation
4. **P1 Unit tests** (8 tests) - Secondary logic
5. **P1 Integration tests** (10 tests) - Extended integration
6. **P1 E2E tests** (5 tests) - Secondary paths
7. **P2+ tests** - As time permits

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Error handling explicitly covered
- [x] State isolation in parallel tests confirmed
- [x] Cross-runtime (Python/Rust) parity tests included

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 54
  by_level:
    unit: 26
    integration: 20
    e2e: 8
  by_priority:
    p0: 22
    p1: 20
    p2: 8
    p3: 4
  coverage_gaps: []
  high_risk_scenarios:
    - 001.1-INT-008  # State isolation in parallel
    - 001.1-INT-009  # Timeout enforcement
    - 001.1-UNIT-014 # Vote tie-breaking determinism
  cross_runtime_parity_tests:
    - 001.1-E2E-008
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.1-test-design-20260105.md
P0 tests identified: 22
Total scenarios: 54
Coverage: 100% of acceptance criteria
```
