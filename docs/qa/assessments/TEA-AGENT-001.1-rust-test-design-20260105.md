# Test Design: Story TEA-AGENT-001.1-rust

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 54
- Unit tests: 28 (52%)
- Integration tests: 18 (33%)
- E2E tests: 8 (15%)
- Priority distribution: P0: 26, P1: 18, P2: 7, P3: 3

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

#### Given-When-Then Examples

**UNIT-001: AgentConfig serde deserialization with all fields**
```gherkin
Given a YAML string with agent configuration:
  """
  name: researcher
  model: ollama:llama3.2
  system_prompt: "You are a research specialist"
  temperature: 0.7
  timeout: 120
  """
When the YAML is deserialized to AgentConfig
Then all fields are populated correctly
And temperature equals 0.7
And timeout equals 120
```

**INT-003: Invalid model format rejected at parse time**
```gherkin
Given a YAML file with invalid agent model:
  """
  settings:
    agents:
      bad_agent:
        model: "invalid-no-prefix"
        system_prompt: "Test"
  """
When the YAML is parsed
Then a validation error is returned
And the error message contains "model must start with 'ollama:' or 'openai:'"
```

### AC2: `agent.dispatch` Action (Rust Native)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-006 | Unit | P0 | Tera template renders state variables correctly | Pure template logic |
| 001.1-rust-UNIT-007 | Unit | P0 | Template with missing state key returns error | Template error handling |
| 001.1-rust-UNIT-008 | Unit | P1 | Retry backoff calculation is exponential | Algorithm correctness |
| 001.1-rust-UNIT-027 | Unit | P0 | **[SECURITY]** Template blocks dangerous expressions | Input validation |
| 001.1-rust-INT-004 | Integration | P0 | dispatch_action sends correct HTTP request to endpoint | HTTP client integration |
| 001.1-rust-INT-005 | Integration | P0 | dispatch_action respects configurable timeout | Timeout enforcement |
| 001.1-rust-INT-006 | Integration | P1 | dispatch_action retries on transient HTTP failures | Retry logic integration |
| 001.1-rust-INT-007 | Integration | P0 | dispatch_action returns parsed LLM response | Response parsing |
| 001.1-rust-E2E-001 | E2E | P1 | agent.dispatch completes full request-response cycle | Full action workflow |

#### Given-When-Then Examples

**UNIT-006: Tera template renders state variables correctly**
```gherkin
Given state contains:
  | key | value |
  | research_question | "What is Rust?" |
  | context | ["memory safety", "systems programming"] |
When template "Research: {{ state.research_question }}" is rendered
Then the result equals "Research: What is Rust?"
```

**UNIT-027: [SECURITY] Template blocks dangerous expressions**
```gherkin
Given a malicious template "{{ __import__('os').system('rm -rf /') }}"
When the template is validated at parse time
Then a TemplateSecurityError is returned
And code execution is prevented
```

**INT-005: dispatch_action respects configurable timeout**
```gherkin
Given an agent configured with timeout: 5
And a mock LLM endpoint that delays 10 seconds
When dispatch_action is invoked
Then AgentError::Timeout is returned
And the error duration is approximately 5 seconds
And the connection is cleanly terminated
```

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
| 001.1-rust-UNIT-028 | Unit | P1 | consensus max_retries prevents infinite loop | Safety bounds |
| 001.1-rust-INT-008 | Integration | P0 | parallel_action spawns rayon threads correctly | Rayon integration |
| 001.1-rust-INT-009 | Integration | P0 | Parallel agents receive independent state copies | State isolation in threads |
| 001.1-rust-INT-010 | Integration | P0 | max_concurrent limits active rayon threads | Concurrency control |
| 001.1-rust-INT-011 | Integration | P0 | Per-agent timeout enforced in parallel execution | Timeout per-thread |
| 001.1-rust-INT-012 | Integration | P1 | consensus retries until threshold reached | Retry loop integration |
| 001.1-rust-INT-022 | Integration | P0 | **[THREAD SAFETY]** AtomicBool cancellation in first aggregation | Race condition prevention |
| 001.1-rust-E2E-002 | E2E | P0 | agent.parallel orchestrates multiple agents end-to-end | Critical multi-agent path |

#### Given-When-Then Examples

**UNIT-012: vote tie-breaking selects by agent order (deterministic)**
```gherkin
Given agents [A, B, C] with responses ["yes", "no", "maybe"]
And agent A is first in definition order
When vote aggregation is applied with tie (all unique responses)
Then agent A's response "yes" is selected
And the result is deterministic across multiple runs
```

**INT-009: Parallel agents receive independent state copies**
```gherkin
Given state = { "counter": 0 }
And 3 agents that each increment counter by 1
When agent.parallel is executed with aggregation: collect
Then each agent sees counter = 0 initially
And no race condition occurs
And results contain [{ "counter": 1 }, { "counter": 1 }, { "counter": 1 }]
```

**INT-022: [THREAD SAFETY] AtomicBool cancellation in first aggregation**
```gherkin
Given 5 agents with varying response times (100ms to 500ms)
When agent.parallel is executed with aggregation: first
Then only the first completing agent's response is returned
And remaining agents are cancelled via AtomicBool signal
And no thread leaks occur (verified via thread count)
```

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

#### Given-When-Then Examples

**UNIT-015: State threading merges Lua tables correctly**
```gherkin
Given agent A returns { "step1": "done", "shared": "A" }
And agent B receives merged state and returns { "step2": "done", "shared": "B" }
When sequential chaining completes
Then final state contains { "step1": "done", "step2": "done", "shared": "B" }
And later values override earlier values for same keys
```

**UNIT-016: Early exit abort stops chain immediately**
```gherkin
Given a chain of 5 agents [A, B, C, D, E]
And agent C fails with AgentError::Timeout
And on_failure is set to "abort"
When agent.sequential is executed
Then only agents A and B are invoked
And AgentError::Timeout is returned
And agents D and E are never called
```

### AC5: Ollama Integration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-018 | Unit | P1 | Ollama model specification parsing (ollama:model) | Model string parsing |
| 001.1-rust-INT-015 | Integration | P0 | Ollama /api/generate endpoint request format | Ollama API contract |
| 001.1-rust-INT-016 | Integration | P1 | Ollama streaming response handled correctly | Streaming integration |
| 001.1-rust-INT-023 | Integration | P1 | Connection pooling via reqwest::Client is reused | Resource efficiency |
| 001.1-rust-E2E-004 | E2E | P2 | End-to-end Ollama workflow with real server | Full Ollama path |

#### Given-When-Then Examples

**INT-015: Ollama /api/generate endpoint request format**
```gherkin
Given agent configured with model: "ollama:llama3.2"
And system_prompt: "You are a helpful assistant"
When dispatch_action is called with task "Hello"
Then HTTP POST is sent to http://localhost:11434/api/generate
And request body contains:
  | field | value |
  | model | llama3.2 |
  | system | You are a helpful assistant |
  | prompt | Hello |
```

### AC6: OpenAI-Compatible Endpoints

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-019 | Unit | P1 | OpenAI model specification parsing (openai:model) | Model string parsing |
| 001.1-rust-UNIT-020 | Unit | P1 | api_key loaded from environment variable | Env var handling |
| 001.1-rust-INT-017 | Integration | P1 | OpenAI-compatible request uses correct endpoint and headers | API contract validation |
| 001.1-rust-E2E-005 | E2E | P2 | End-to-end OpenAI-compatible workflow | Full OpenAI path |

#### Given-When-Then Examples

**INT-017: OpenAI-compatible request uses correct endpoint and headers**
```gherkin
Given agent configured with:
  | field | value |
  | model | openai:gpt-4 |
  | api_base | https://api.example.com/v1 |
And OPENAI_API_KEY environment variable set to "sk-test-123"
When dispatch_action is called
Then HTTP POST is sent to https://api.example.com/v1/chat/completions
And Authorization header equals "Bearer sk-test-123"
And Content-Type header equals "application/json"
```

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
| 001.1-rust-E2E-007 | E2E | P1 | **[RESILIENCE]** Recovery from checkpoint after crash | Crash recovery |

#### Given-When-Then Examples

**UNIT-024: All AgentError variants are recoverable (no panic)**
```gherkin
Given the following error scenarios:
  | error_type | trigger |
  | NotFound | lookup("nonexistent_agent") |
  | Timeout | slow_llm_response(120s) |
  | ConnectionFailed | unreachable_endpoint |
  | ParseError | invalid_llm_response |
When each error is triggered
Then the error is returned as Result::Err
And no panic occurs
And the error can be matched and handled
And std::panic::catch_unwind returns Ok
```

**INT-018: Checkpoint saved on unrecoverable error**
```gherkin
Given a workflow with checkpoint enabled
And an agent.parallel action in progress with 2/3 agents completed
When an unrecoverable network partition occurs
Then the checkpoint is persisted with partial results
And checkpoint contains completed agent responses
And workflow can be resumed from checkpoint
```

**E2E-007: [RESILIENCE] Recovery from checkpoint after crash**
```gherkin
Given a 5-step sequential workflow at step 3
And checkpoint persisted to disk
When the process is killed (SIGKILL)
And the workflow is restarted with same checkpoint
Then execution resumes from step 3
And steps 1-2 are not re-executed
And final result matches expected outcome
```

### AC8: Feature Flag

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-rust-UNIT-025 | Unit | P2 | Actions behind --features agent compile conditionally | Feature flag logic |
| 001.1-rust-UNIT-026 | Unit | P2 | Graceful error when actions used without feature | Feature guard logic |
| 001.1-rust-INT-020 | Integration | P3 | Binary size unchanged when feature disabled | Build verification |
| 001.1-rust-INT-021 | Integration | P3 | Actions registered in registry with feature enabled | Registry integration |
| 001.1-rust-E2E-008 | E2E | P3 | Full workflow with feature flag enabled | Feature integration |

#### Given-When-Then Examples

**UNIT-026: Graceful error when actions used without feature**
```gherkin
Given a workflow YAML using action: agent.dispatch
And the binary is compiled without --features agent
When the workflow is executed
Then EngineError::FeatureNotEnabled is returned
And the error message says "agent actions require --features agent"
And no panic or undefined behavior occurs
```

## Coverage Summary by Acceptance Criteria

| AC | Unit | Integration | E2E | Total | Priority Distribution |
|----|------|-------------|-----|-------|----------------------|
| AC1 | 5 | 3 | 0 | 8 | P0: 5, P1: 2, P2: 0, P3: 0 |
| AC2 | 4 | 4 | 1 | 9 | P0: 6, P1: 3, P2: 0, P3: 0 |
| AC3 | 7 | 6 | 1 | 14 | P0: 9, P1: 4, P2: 0, P3: 0 |
| AC4 | 3 | 2 | 1 | 6 | P0: 0, P1: 6, P2: 0, P3: 0 |
| AC5 | 1 | 3 | 1 | 5 | P0: 1, P1: 3, P2: 1, P3: 0 |
| AC6 | 2 | 1 | 1 | 4 | P0: 0, P1: 3, P2: 1, P3: 0 |
| AC7 | 4 | 2 | 2 | 8 | P0: 5, P1: 2, P2: 1, P3: 0 |
| AC8 | 2 | 2 | 1 | 5 | P0: 0, P1: 0, P2: 2, P3: 3 |

## Risk Mitigations

| Risk | Severity | Mitigating Tests | Rationale |
|------|----------|------------------|-----------|
| State corruption in parallel execution | HIGH | UNIT-009, INT-009, INT-022 | Deep clone verification and thread safety critical |
| Non-deterministic voting results | MEDIUM | UNIT-012 | Deterministic tie-breaking must be verified |
| Unbounded resource consumption | HIGH | UNIT-028, INT-010, INT-011 | max_concurrent, max_retries, and timeout enforcement |
| Panic in embedded environment | CRITICAL | UNIT-024 | All error paths must be recoverable |
| Data loss on failure | HIGH | INT-018, E2E-007 | Checkpoint persistence and recovery |
| Ollama connection flakiness | MEDIUM | INT-006, E2E-006 | Retry and graceful degradation |
| Template injection attack | HIGH | UNIT-027 | Security validation at parse time |
| Thread leak on cancellation | MEDIUM | INT-022 | AtomicBool cancellation verified |

## Security Test Annotations

| Test ID | Security Category | OWASP Category |
|---------|------------------|----------------|
| UNIT-027 | Input Validation | A03:2021-Injection |
| UNIT-024 | Error Handling | A09:2021-Security Logging |
| INT-018 | Data Protection | A07:2021-Software Integrity |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - AgentConfig validation (UNIT-001 to 005)
   - Template rendering & security (UNIT-006, 007, 027)
   - Aggregation algorithms (UNIT-009 to 014)
   - Error types (UNIT-021 to 024)

2. **P0 Integration tests** (component boundaries)
   - YAML parsing (INT-001, 003)
   - HTTP dispatch (INT-004, 005, 007)
   - Parallel execution & thread safety (INT-008 to 011, INT-022)
   - Checkpoint (INT-018)

3. **P0 E2E tests** (critical paths)
   - agent.parallel full workflow (E2E-002)

4. **P1 Unit tests** (secondary logic)
   - Default values, retry backoff, consensus bounds, parsing

5. **P1 Integration tests** (secondary flows)
   - Retry logic, consensus, sequential chaining, connection pooling

6. **P1 E2E tests** (secondary journeys)
   - agent.dispatch, agent.sequential, crash recovery

7. **P2/P3 tests** (as time permits)
   - Feature flag verification, real server E2E, binary size

## Test Environment Requirements

| Level | Environment | Notes |
|-------|-------------|-------|
| Unit | `cargo test` | No external dependencies, mocked HTTP |
| Integration | `cargo test --features agent` | Mock HTTP server (wiremock-rs) |
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

### Malicious Template Fixture (Security Testing)
```rust
let malicious_templates = vec![
    "{{ __import__('os').system('rm -rf /') }}",
    "{{ config.__class__.__mro__[2].__subclasses__() }}",
    "{% include '/etc/passwd' %}",
];
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
- [x] Given-When-Then examples provided for key scenarios
- [x] Security test annotations included
- [x] Crash recovery scenario added

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 54
  by_level:
    unit: 28
    integration: 18
    e2e: 8
  by_priority:
    p0: 26
    p1: 18
    p2: 7
    p3: 3
  coverage_gaps: []
  key_risks_mitigated:
    - state_corruption_parallel
    - nondeterministic_voting
    - unbounded_resources
    - panic_in_embedded
    - data_loss_on_failure
    - template_injection
    - thread_leak_cancellation
  security_tests: 3
  gwt_examples: 15
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.1-rust-test-design-20260105.md
P0 tests identified: 26
P1 tests identified: 18
Coverage: 8 ACs fully covered
Security tests: 3 (OWASP-aligned)
Given-When-Then examples: 15
```
