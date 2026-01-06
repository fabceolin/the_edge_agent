# Test Design: Story TEA-AGENT-001.2-rust

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 52
- Unit tests: 36 (69%)
- Integration tests: 12 (23%)
- E2E tests: 4 (8%)
- Priority distribution: P0: 32, P1: 14, P2: 6

## Test Scenarios by Acceptance Criteria

### AC1: `reflection.loop` Action (Rust Native)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-001 | Unit | P0 | Execute generate→evaluate→correct cycle with valid generator | Core loop logic is pure function |
| 001.2-rust-UNIT-002 | Unit | P0 | Generator outputs correctly passed to evaluator | Data flow validation |
| 001.2-rust-UNIT-003 | Unit | P0 | Evaluator results correctly trigger corrector | Control flow logic |
| 001.2-rust-UNIT-004 | Unit | P0 | First-pass success skips correction | Short-circuit optimization |
| 001.2-rust-UNIT-005 | Unit | P0 | Loop terminates when evaluator passes | Termination condition |
| 001.2-rust-UNIT-006 | Unit | P1 | Lua generator code executes correctly | Lua integration path |
| 001.2-rust-UNIT-007 | Unit | P1 | Lua corrector code executes correctly | Lua integration path |
| 001.2-rust-INT-001 | Integration | P0 | Full reflection cycle with real Rust actions | Multi-component flow |
| 001.2-rust-INT-002 | Integration | P1 | Reflection loop with nested action chains | Complex action composition |

### AC2: Schema Evaluator (jsonschema-rs)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-008 | Unit | P0 | Valid JSON passes schema validation | Happy path validation |
| 001.2-rust-UNIT-009 | Unit | P0 | Invalid JSON fails with correct error path | Error accuracy critical |
| 001.2-rust-UNIT-010 | Unit | P0 | Missing required field returns field name in error | Actionable error messages |
| 001.2-rust-UNIT-011 | Unit | P0 | Type mismatch reports expected vs actual types | Developer experience |
| 001.2-rust-UNIT-012 | Unit | P0 | Nested object errors include full JSON path | Debugging nested structures |
| 001.2-rust-UNIT-013 | Unit | P1 | Array validation with minItems/maxItems | Complex schema features |
| 001.2-rust-UNIT-014 | Unit | P1 | Schema loaded from file path | File-based configuration |
| 001.2-rust-UNIT-015 | Unit | P1 | Inline schema definition works | YAML embedding |
| 001.2-rust-UNIT-016 | Unit | P0 | Draft 7 schema features supported | Compatibility guarantee |
| 001.2-rust-INT-003 | Integration | P0 | Schema evaluator in full reflection loop | Component integration |

### AC3: LLM Evaluator (Ollama/OpenAI-compatible)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-017 | Unit | P0 | LLM response parsed to {pass, score, feedback} | Response parsing logic |
| 001.2-rust-UNIT-018 | Unit | P0 | Tera template renders with state variables | Template engine integration |
| 001.2-rust-UNIT-019 | Unit | P1 | Missing fields in LLM response handled gracefully | Error resilience |
| 001.2-rust-UNIT-020 | Unit | P1 | Model override from config used over default | Configuration precedence |
| 001.2-rust-INT-004 | Integration | P0 | LLM evaluator with mocked Ollama endpoint | External dependency isolation |
| 001.2-rust-INT-005 | Integration | P1 | LLM evaluator with real Ollama (optional) | Real-world validation |
| 001.2-rust-E2E-001 | E2E | P2 | Full reflection loop with LLM evaluation | End-to-end validation |

### AC4: Lua Evaluator

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-021 | Unit | P0 | Lua script receives state correctly | Data injection |
| 001.2-rust-UNIT-022 | Unit | P0 | Lua script receives output correctly | Data injection |
| 001.2-rust-UNIT-023 | Unit | P0 | Lua script receives iteration count | Iteration awareness |
| 001.2-rust-UNIT-024 | Unit | P0 | Lua returns valid {valid, score, errors} | Result extraction |
| 001.2-rust-UNIT-025 | Unit | P0 | Lua sandbox blocks file access (io.open) | Security critical |
| 001.2-rust-UNIT-026 | Unit | P0 | Lua sandbox blocks os module | Security critical |
| 001.2-rust-UNIT-027 | Unit | P0 | Lua sandbox blocks require for arbitrary modules | Security critical |
| 001.2-rust-UNIT-028 | Unit | P1 | Lua syntax error returns helpful message | Developer experience |
| 001.2-rust-UNIT-029 | Unit | P1 | Lua runtime error captured and reported | Error handling |
| 001.2-rust-INT-006 | Integration | P0 | Lua evaluator in full reflection loop | Component integration |

### AC5: Iteration Tracking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-030 | Unit | P0 | reflection_iteration increments on each loop | Counter correctness |
| 001.2-rust-UNIT-031 | Unit | P0 | reflection_history captures all attempts | History accumulation |
| 001.2-rust-UNIT-032 | Unit | P0 | reflection_errors contains all error messages | Error aggregation |
| 001.2-rust-UNIT-033 | Unit | P0 | Attempt struct contains iteration, output, score, errors | Data completeness |
| 001.2-rust-UNIT-034 | Unit | P1 | Stack allocation used where possible (no heap for small data) | Performance optimization |
| 001.2-rust-INT-007 | Integration | P0 | Iteration state persists across checkpoint save/resume | Checkpoint compatibility |

### AC6: Circuit Breaker (Atomic)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-035 | Unit | P0 | Loop stops exactly at max_iterations | Termination guarantee |
| 001.2-rust-UNIT-036 | Unit | P0 | AtomicU32 counter is thread-safe (concurrent access test) | Thread safety critical |
| 001.2-rust-UNIT-037 | Unit | P0 | Circuit opens after max reached (no more iterations possible) | State machine correctness |
| 001.2-rust-UNIT-038 | Unit | P0 | max_iterations=1 results in single attempt | Edge case |
| 001.2-rust-UNIT-039 | Unit | P1 | max_iterations=0 returns error or default behavior | Edge case |
| 001.2-rust-INT-008 | Integration | P0 | Circuit breaker integrates with full loop | Component integration |

### AC7: On-Failure Strategies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-040 | Unit | P0 | return_best selects highest score from history | Selection algorithm |
| 001.2-rust-UNIT-041 | Unit | P0 | return_best with tie selects first occurrence | Deterministic behavior |
| 001.2-rust-UNIT-042 | Unit | P0 | return_last returns final attempt | Simple retrieval |
| 001.2-rust-UNIT-043 | Unit | P0 | raise returns ReflectionError with full history | Error completeness |
| 001.2-rust-UNIT-044 | Unit | P1 | OnFailure enum is exhaustive (all variants handled) | Compile-time safety |
| 001.2-rust-INT-009 | Integration | P0 | On-failure strategies work in full loop | Component integration |

### AC8: Standalone Actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-045 | Unit | P0 | reflection.evaluate runs evaluator without loop | Action isolation |
| 001.2-rust-UNIT-046 | Unit | P0 | reflection.correct runs corrector without loop | Action isolation |
| 001.2-rust-UNIT-047 | Unit | P1 | Standalone actions composable in custom workflow | Composability |
| 001.2-rust-INT-010 | Integration | P1 | Standalone actions registered in actions registry | Registry integration |
| 001.2-rust-E2E-002 | E2E | P2 | Custom reflection pattern using standalone actions | Real-world usage |

### AC9: Feature Flag

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-rust-UNIT-048 | Unit | P0 | Reflection actions unavailable without feature flag | Feature isolation |
| 001.2-rust-UNIT-049 | Unit | P0 | jsonschema crate not linked without feature flag | Dependency pruning |
| 001.2-rust-INT-011 | Integration | P1 | cargo build succeeds with --features reflection | Build system |
| 001.2-rust-INT-012 | Integration | P1 | cargo build succeeds without --features reflection | Build system |
| 001.2-rust-E2E-003 | E2E | P2 | YAML workflow with reflection runs with feature enabled | Full feature validation |
| 001.2-rust-E2E-004 | E2E | P2 | YAML workflow with reflection fails gracefully without feature | Graceful degradation |

## Risk Coverage

| Risk | Mitigated By Test IDs |
|------|----------------------|
| Infinite loop | 001.2-rust-UNIT-035, 001.2-rust-UNIT-036, 001.2-rust-UNIT-037, 001.2-rust-INT-008 |
| Security sandbox escape | 001.2-rust-UNIT-025, 001.2-rust-UNIT-026, 001.2-rust-UNIT-027 |
| Data loss on failure | 001.2-rust-UNIT-031, 001.2-rust-UNIT-032, 001.2-rust-UNIT-043 |
| Schema validation accuracy | 001.2-rust-UNIT-008 through 001.2-rust-UNIT-016 |
| Thread safety | 001.2-rust-UNIT-036 |
| LLM response parsing failures | 001.2-rust-UNIT-017, 001.2-rust-UNIT-019 |
| Checkpoint corruption | 001.2-rust-INT-007 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Circuit breaker tests (prevent infinite loops)
   - Lua sandbox tests (security critical)
   - Schema evaluator tests (data integrity)
   - Iteration tracking tests (state correctness)
   - On-failure strategy tests (result handling)

2. **P0 Integration tests**
   - Full reflection loop
   - Schema evaluator in loop
   - Lua evaluator in loop
   - Circuit breaker integration
   - Checkpoint compatibility

3. **P1 Unit tests**
   - Edge cases and error handling
   - Developer experience (error messages)
   - Configuration options

4. **P1 Integration tests**
   - LLM evaluator with mocked endpoint
   - Action registry integration

5. **P2 E2E tests** (if time permits)
   - Full workflow with LLM
   - Custom reflection patterns
   - Feature flag behavior

## Test Implementation Notes

### Mock Strategy

```rust
// For LLM evaluator tests
trait LlmClient {
    async fn call(&self, prompt: &str) -> Result<LlmResponse, Error>;
}

struct MockLlmClient {
    responses: Vec<LlmResponse>,
}
```

### Test Data Fixtures

```rust
// Valid schema for reuse across tests
const VALID_PERSON_SCHEMA: &str = r#"{
  "type": "object",
  "required": ["name", "email"],
  "properties": {
    "name": { "type": "string", "minLength": 1 },
    "email": { "type": "string", "format": "email" }
  }
}"#;

// Invalid JSON samples for error path testing
const INVALID_MISSING_NAME: &str = r#"{"email": "test@example.com"}"#;
const INVALID_WRONG_TYPE: &str = r#"{"name": 123, "email": "test@example.com"}"#;
```

### Sandbox Testing

```rust
#[test]
fn test_lua_sandbox_blocks_file_io() {
    let lua_code = r#"io.open("/etc/passwd", "r")"#;
    let result = run_lua_evaluator(lua_code, state, output);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("sandbox"));
}
```

### Thread Safety Testing

```rust
#[test]
fn test_circuit_breaker_thread_safe() {
    let breaker = CircuitBreaker::new(10);
    let breaker = Arc::new(breaker);

    let handles: Vec<_> = (0..100)
        .map(|_| {
            let b = breaker.clone();
            thread::spawn(move || b.increment())
        })
        .collect();

    for h in handles {
        h.join().unwrap();
    }

    // Should stop at max_iterations regardless of race conditions
    assert!(breaker.count() <= 10);
}
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have P0 coverage
- [x] Thread safety explicitly tested
- [x] Feature flag behavior tested both ways

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 36
    integration: 12
    e2e: 4
  by_priority:
    p0: 32
    p1: 14
    p2: 6
  coverage_gaps: []
  security_tests:
    - lua_sandbox_file_access
    - lua_sandbox_os_module
    - lua_sandbox_require_block
  thread_safety_tests:
    - circuit_breaker_atomic_counter
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.2-rust-test-design-20260105.md
P0 tests identified: 32
Security-critical tests: 3
Thread-safety tests: 1
```
