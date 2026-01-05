# Test Design: Story TEA-AGENT-001.1-rust

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 48
- Unit tests: 26 (54%)
- Integration tests: 16 (33%)
- E2E tests: 6 (13%)
- Priority distribution: P0: 24, P1: 16, P2: 6, P3: 2

## Test Scenarios by Acceptance Criteria

### AC1: Agent Definition in YAML Settings

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-001 | Unit | P0 | AgentConfig serde deserialization with all fields | Pure struct validation logic |
| 001.1-rust-UNIT-002 | Unit | P0 | AgentConfig validation rejects missing required fields | Input validation, no I/O |
| 001.1-rust-UNIT-003 | Unit | P1 | AgentConfig with optional temperature defaults | Default value logic |
| 001.1-rust-UNIT-004 | Unit | P0 | AgentRegistry lookup by name returns correct agent | HashMap lookup logic |
| 001.1-rust-UNIT-005 | Unit | P0 | AgentRegistry returns AgentError::NotFound for unknown agent | Error path validation |
| 001.1-rust-INT-001 | Integration | P0 | YAML parser extracts settings.agents section correctly | YAML + serde integration |
| 001.1-rust-INT-002 | Integration | P1 | Default agent inherits from settings.llm when not specified | Component interaction |
| 001.1-rust-INT-003 | Integration | P0 | Invalid model format rejected at parse time | Parser validation boundary |

### AC2: `agent.dispatch` Action (Rust Native)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-006 | Unit | P0 | Tera template renders state variables correctly | Pure template logic |
| 001.1-rust-UNIT-007 | Unit | P0 | Template with missing state key returns error | Template error handling |
| 001.1-rust-UNIT-008 | Unit | P1 | Retry backoff calculation is exponential | Algorithm correctness |
| 001.1-rust-INT-004 | Integration | P0 | dispatch_action sends correct HTTP request to endpoint | HTTP client integration |
| 001.1-rust-INT-005 | Integration | P0 | dispatch_action respects configurable timeout | Timeout enforcement |
| 001.1-rust-INT-006 | Integration | P1 | dispatch_action retries on transient HTTP failures | Retry logic integration |
| 001.1-rust-INT-007 | Integration | P0 | dispatch_action returns parsed LLM response | Response parsing |
| 001.1-rust-E2E-001 | E2E | P1 | agent.dispatch completes full request-response cycle | Full action workflow |

### AC3: `agent.parallel` Action (Rayon-based)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-009 | Unit | P0 | State deep clone via serde is correct | Data isolation logic |
| 001.1-rust-UNIT-010 | Unit | P0 | collect aggregation returns Vec with all responses | Aggregation algorithm |
| 001.1-rust-UNIT-011 | Unit | P0 | vote aggregation returns majority response | Voting algorithm |
| 001.1-rust-UNIT-012 | Unit | P0 | vote tie-breaking selects by agent order (deterministic) | Tie-break algorithm |
| 001.1-rust-UNIT-013 | Unit | P0 | consensus aggregation identifies N agreeing agents | Consensus algorithm |
| 001.1-rust-UNIT-014 | Unit | P1 | first aggregation returns earliest successful response | Racing algorithm |
| 001.1-rust-INT-008 | Integration | P0 | parallel_action spawns rayon threads correctly | Rayon integration |
| 001.1-rust-INT-009 | Integration | P0 | Parallel agents receive independent state copies | State isolation in threads |
| 001.1-rust-INT-010 | Integration | P0 | max_concurrent limits active rayon threads | Concurrency control |
| 001.1-rust-INT-011 | Integration | P0 | Per-agent timeout enforced in parallel execution | Timeout per-thread |
| 001.1-rust-INT-012 | Integration | P1 | consensus retries until threshold reached | Retry loop integration |
| 001.1-rust-E2E-002 | E2E | P0 | agent.parallel orchestrates multiple agents end-to-end | Critical multi-agent path |

### AC4: `agent.sequential` Action

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-015 | Unit | P1 | State threading merges Lua tables correctly | Lua table merge logic |
| 001.1-rust-UNIT-016 | Unit | P1 | Early exit abort stops chain immediately | Control flow logic |
| 001.1-rust-UNIT-017 | Unit | P1 | Early exit continue skips failed agent | Control flow logic |
| 001.1-rust-INT-013 | Integration | P1 | sequential_action chains agent outputs as inputs | Agent chaining integration |
| 001.1-rust-INT-014 | Integration | P1 | sequential_action returns accumulated state | State accumulation |
| 001.1-rust-E2E-003 | E2E | P1 | agent.sequential executes full chain workflow | Full sequential workflow |

### AC5: Ollama Integration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-018 | Unit | P1 | Ollama model specification parsing (ollama:model) | Model string parsing |
| 001.1-rust-INT-015 | Integration | P0 | Ollama /api/generate endpoint request format | Ollama API contract |
| 001.1-rust-INT-016 | Integration | P1 | Ollama streaming response handled correctly | Streaming integration |
| 001.1-rust-E2E-004 | E2E | P2 | End-to-end Ollama workflow with real server | Full Ollama path |

### AC6: OpenAI-Compatible Endpoints

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-019 | Unit | P1 | OpenAI model specification parsing (openai:model) | Model string parsing |
| 001.1-rust-UNIT-020 | Unit | P1 | api_key loaded from environment variable | Env var handling |
| 001.1-rust-INT-017 | Integration | P1 | OpenAI-compatible request uses correct endpoint and headers | API contract validation |
| 001.1-rust-E2E-005 | E2E | P2 | End-to-end OpenAI-compatible workflow | Full OpenAI path |

### AC7: Error Handling (Embedded-Safe)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-021 | Unit | P0 | AgentError::NotFound contains agent name | Error data integrity |
| 001.1-rust-UNIT-022 | Unit | P0 | AgentError::Timeout contains duration | Error data integrity |
| 001.1-rust-UNIT-023 | Unit | P0 | AgentError::ConnectionFailed contains retry info | Error data integrity |
| 001.1-rust-UNIT-024 | Unit | P0 | All AgentError variants are recoverable (no panic) | Embedded safety |
| 001.1-rust-INT-018 | Integration | P0 | Checkpoint saved on unrecoverable error | Checkpoint integration |
| 001.1-rust-INT-019 | Integration | P1 | Connection pooling via reqwest::Client reused | Connection management |
| 001.1-rust-E2E-006 | E2E | P2 | Graceful degradation when Ollama unavailable | Error recovery path |

### AC8: Feature Flag

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-025 | Unit | P2 | Actions behind --features agent compile conditionally | Feature flag logic |
| 001.1-rust-UNIT-026 | Unit | P2 | Graceful error when actions used without feature | Feature guard logic |
| 001.1-rust-INT-020 | Integration | P3 | Binary size unchanged when feature disabled | Build verification |
| 001.1-rust-INT-021 | Integration | P3 | Actions registered in registry with feature enabled | Registry integration |

## Coverage Summary by Acceptance Criteria

| AC | Unit | Integration | E2E | Total | Priority Distribution |
|----|------|-------------|-----|-------|----------------------|
| AC1 | 5 | 3 | 0 | 8 | P0: 5, P1: 2, P2: 0, P3: 0 |
| AC2 | 3 | 4 | 1 | 8 | P0: 5, P1: 3, P2: 0, P3: 0 |
| AC3 | 6 | 5 | 1 | 12 | P0: 8, P1: 3, P2: 0, P3: 0 |
| AC4 | 3 | 2 | 1 | 6 | P0: 0, P1: 6, P2: 0, P3: 0 |
| AC5 | 1 | 2 | 1 | 4 | P0: 1, P1: 2, P2: 1, P3: 0 |
| AC6 | 2 | 1 | 1 | 4 | P0: 0, P1: 3, P2: 1, P3: 0 |
| AC7 | 4 | 2 | 1 | 7 | P0: 5, P1: 1, P2: 1, P3: 0 |
| AC8 | 2 | 2 | 0 | 4 | P0: 0, P1: 0, P2: 2, P3: 2 |

## Risk Mitigations

| Risk | Mitigating Tests | Rationale |
|------|-----------------|-----------|
| State corruption in parallel execution | 001.1-rust-UNIT-009, 001.1-rust-INT-009 | Deep clone verification critical for thread safety |
| Non-deterministic voting results | 001.1-rust-UNIT-012 | Deterministic tie-breaking must be verified |
| Unbounded resource consumption | 001.1-rust-INT-010, 001.1-rust-INT-011 | max_concurrent and timeout enforcement |
| Panic in embedded environment | 001.1-rust-UNIT-024 | All error paths must be recoverable |
| Data loss on failure | 001.1-rust-INT-018 | Checkpoint on unrecoverable error |
| Ollama connection flakiness | 001.1-rust-INT-006, 001.1-rust-E2E-006 | Retry and graceful degradation |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - AgentConfig validation (001.1-rust-UNIT-001 to 005)
   - Template rendering (001.1-rust-UNIT-006, 007)
   - Aggregation algorithms (001.1-rust-UNIT-009 to 014)
   - Error types (001.1-rust-UNIT-021 to 024)

2. **P0 Integration tests** (component boundaries)
   - YAML parsing (001.1-rust-INT-001, 003)
   - HTTP dispatch (001.1-rust-INT-004, 005, 007)
   - Parallel execution (001.1-rust-INT-008 to 011)
   - Error handling (001.1-rust-INT-018)

3. **P0 E2E tests** (critical paths)
   - agent.parallel full workflow (001.1-rust-E2E-002)

4. **P1 Unit tests** (secondary logic)
   - Default values, retry backoff, parsing

5. **P1 Integration tests** (secondary flows)
   - Retry logic, consensus, sequential chaining

6. **P1 E2E tests** (secondary journeys)
   - agent.dispatch, agent.sequential full workflows

7. **P2/P3 tests** (as time permits)
   - Feature flag verification, real server E2E

## Test Environment Requirements

| Level | Environment | Notes |
|-------|-------------|-------|
| Unit | cargo test | No external dependencies, mocked HTTP |
| Integration | cargo test --features agent | Mock HTTP server (wiremock-rs) |
| E2E | Real Ollama instance | Requires local Ollama running |

## Test Data Fixtures

### Agent Configuration Fixture
```rust
AgentConfig {
    name: "test_agent".to_string(),
    model: "ollama:llama3.2".to_string(),
    system_prompt: "You are a test agent".to_string(),
    temperature: Some(0.7),
    timeout: Some(60),
}
```

### State Fixture
```rust
json!({
    "research_question": "What is Rust?",
    "context": ["systems programming", "memory safety"]
})
```

### Mock LLM Response Fixture
```json
{
    "response": "Rust is a systems programming language.",
    "done": true,
    "context": [1, 2, 3]
}
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit-heavy for algorithms)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (001.1-rust-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Thread safety scenarios covered
- [x] Error recovery scenarios covered
- [x] Embedded/offline constraints considered

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 48
  by_level:
    unit: 26
    integration: 16
    e2e: 6
  by_priority:
    p0: 24
    p1: 16
    p2: 6
    p3: 2
  coverage_gaps: []
  key_risks_mitigated:
    - state_corruption_parallel
    - nondeterministic_voting
    - unbounded_resources
    - panic_in_embedded
    - data_loss_on_failure
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.1-rust-test-design-20260105.md
P0 tests identified: 24
Coverage: 8 ACs fully covered
```
