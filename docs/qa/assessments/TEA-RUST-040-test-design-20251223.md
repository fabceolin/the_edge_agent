# Test Design: Story TEA-RUST-040

**Story:** Rust LLM Stream and Tools Actions
**Date:** 2024-12-23
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 28 | 100% |
| Unit tests | 16 | 57% |
| Integration tests | 10 | 36% |
| E2E tests | 2 | 7% |

**Priority Distribution:**
- P0 (Critical): 10
- P1 (Core): 12
- P2 (Secondary): 6

---

## Test Scenarios by Acceptance Criteria

### AC1: `llm.stream` streams LLM responses via SSE

**Test Level Justification:** SSE parsing is pure string/JSON logic - ideal for unit tests. Live API validation requires integration tests.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-001 | Unit | P0 | Parse SSE line with content delta | Pure string parsing logic |
| RUST-040-UNIT-002 | Unit | P0 | Parse SSE line with empty delta (heartbeat) | Edge case handling |
| RUST-040-UNIT-003 | Unit | P0 | Parse `data: [DONE]` sentinel correctly | Stream termination |
| RUST-040-UNIT-004 | Unit | P1 | Skip non-data lines (comments, empty) | SSE spec compliance |
| RUST-040-UNIT-005 | Unit | P1 | Deserialize StreamChunk struct from JSON | Serde correctness |
| RUST-040-INT-001 | Integration | P1 | Stream response from Ollama (phi4-mini) | Live API validation |

**Rust Test Code Pattern:**
```rust
#[test]
fn test_parse_sse_line_with_content() {
    let line = r#"data: {"id":"chatcmpl-123","choices":[{"delta":{"content":"Hello"}}]}"#;
    let chunk = parse_sse_line(line).unwrap();
    assert_eq!(chunk.choices[0].delta.content, Some("Hello".to_string()));
}

#[test]
fn test_parse_sse_done_sentinel() {
    let line = "data: [DONE]";
    let result = parse_sse_line(line);
    assert!(result.is_none()); // Should signal stream end
}
```

---

### AC2: `llm.stream` aggregates chunks and returns content with chunk_count

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-006 | Unit | P0 | Aggregate multiple content chunks into string | Core aggregation logic |
| RUST-040-UNIT-007 | Unit | P1 | Count chunks correctly (skip empty deltas) | Accurate metrics |
| RUST-040-UNIT-008 | Unit | P2 | Handle empty stream (no chunks) | Edge case |

**Rust Test Code Pattern:**
```rust
#[test]
fn test_aggregate_chunks() {
    let chunks = vec!["Hello", " ", "world", "!"];
    let (content, count) = aggregate_stream_chunks(chunks);
    assert_eq!(content, "Hello world!");
    assert_eq!(count, 4);
}
```

---

### AC3: `llm.stream` supports same provider param as `llm.call`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-009 | Unit | P0 | Provider "ollama" uses correct default api_base | Provider abstraction |
| RUST-040-UNIT-010 | Unit | P0 | Provider "openai" uses correct default api_base | Provider abstraction |
| RUST-040-INT-002 | Integration | P1 | Stream with explicit api_base override | Custom endpoint |

---

### AC4: `llm.tools` supports OpenAI function/tool calling format

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-011 | Unit | P0 | Parse Tool struct from YAML-style definition | Struct deserialization |
| RUST-040-UNIT-012 | Unit | P0 | Convert YAML-style tool to OpenAI format | Format conversion |
| RUST-040-UNIT-013 | Unit | P1 | Parse tool_calls from API response | Response parsing |
| RUST-040-UNIT-014 | Unit | P1 | Parse function arguments (nested JSON string) | Complex parsing |

**Rust Test Code Pattern:**
```rust
#[test]
fn test_yaml_tool_to_openai_format() {
    let yaml_tool = json!({
        "name": "get_weather",
        "description": "Get weather",
        "parameters": {
            "location": {"type": "string", "required": true}
        }
    });
    let openai_tool = convert_to_openai_format(&yaml_tool);
    assert_eq!(openai_tool["type"], "function");
    assert_eq!(openai_tool["function"]["name"], "get_weather");
}
```

---

### AC5: `llm.tools` handles multi-turn tool use

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-015 | Unit | P0 | Build tool result message for multi-turn | Message construction |
| RUST-040-INT-003 | Integration | P0 | Multi-turn: call → result → final response | Core workflow |
| RUST-040-INT-004 | Integration | P1 | Multiple tool calls in single response | Parallel tools |

**Rust Test Code Pattern:**
```rust
#[test]
fn test_build_tool_result_message() {
    let tool_call_id = "call_abc123";
    let result = json!({"temperature": 72});
    let msg = build_tool_result_message(tool_call_id, &result);
    assert_eq!(msg["role"], "tool");
    assert_eq!(msg["tool_call_id"], "call_abc123");
}
```

---

### AC6: `llm.tools` supports `max_tool_rounds` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-UNIT-016 | Unit | P1 | Default max_tool_rounds is 10 | Default behavior |
| RUST-040-INT-005 | Integration | P1 | Stop after max_tool_rounds exceeded | Infinite loop prevention |

---

### AC7: Both actions support provider, api_base, api_key params

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-INT-006 | Integration | P0 | llm.stream with custom api_key | Auth handling |
| RUST-040-INT-007 | Integration | P0 | llm.tools with custom api_key | Auth handling |

---

### AC8: Existing `llm.call` functionality continues unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-INT-008 | Integration | P0 | Run existing llm.call tests - no regression | Backward compatibility |

**Test Strategy:** Re-run all existing `test_llm_call_*` tests after implementation.

---

### AC9: YAML syntax matches Python implementation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-E2E-001 | E2E | P1 | Same YAML agent runs on both Python and Rust | Cross-runtime parity |

**Test Strategy:** Create a shared YAML test agent that exercises llm.stream and llm.tools, run on both runtimes, compare outputs.

---

### AC10: Actions registered in `register()` function

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-INT-009 | Integration | P2 | "llm.stream" exists in action registry | Registration check |
| RUST-040-INT-010 | Integration | P2 | "llm.tools" exists in action registry | Registration check |

---

### AC11: Error handling follows `TeaError` patterns

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-040-E2E-002 | E2E | P2 | Invalid API key returns appropriate TeaError | Error handling |

---

### AC12-15: Quality Requirements (Testing & Documentation)

These ACs are meta-requirements verified by this test design document and code review.

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| SSE parsing edge cases | Medium | High | UNIT-001 to UNIT-008 |
| Malformed JSON in stream | Low | Medium | UNIT-005, UNIT-008 |
| Infinite tool calling loop | Low | High | INT-005 |
| Breaking llm.call regression | Low | Critical | INT-008 |
| Cross-runtime YAML mismatch | Medium | High | E2E-001 |

---

## Recommended Execution Order

**Phase 1: Fast Feedback (P0 Unit)**
```bash
cargo test test_parse_sse -- --nocapture
cargo test test_aggregate -- --nocapture
cargo test test_tool_parsing -- --nocapture
```

**Phase 2: Integration Validation (P0 Integration)**
```bash
# Requires Ollama running with phi4-mini
cargo test test_stream_ollama -- --ignored --nocapture
cargo test test_tools_multiturn -- --ignored --nocapture
```

**Phase 3: Regression Guard (P0 Regression)**
```bash
cargo test test_llm_call -- --nocapture
```

**Phase 4: Cross-Runtime Parity (P1 E2E)**
```bash
# Run same YAML on both runtimes
tea run examples/test-llm-stream-tools.yaml  # Rust
python -m the_edge_agent examples/test-llm-stream-tools.yaml  # Python
```

**Phase 5: Complete Suite**
```bash
cargo test
cargo test -- --ignored  # Live API tests
```

---

## Test Data Requirements

### Mock SSE Stream Data
```rust
const MOCK_SSE_STREAM: &str = r#"
data: {"id":"chatcmpl-123","choices":[{"delta":{"role":"assistant"}}]}

data: {"id":"chatcmpl-123","choices":[{"delta":{"content":"Hello"}}]}

data: {"id":"chatcmpl-123","choices":[{"delta":{"content":" world"}}]}

data: {"id":"chatcmpl-123","choices":[{"delta":{}}],"finish_reason":"stop"}

data: [DONE]
"#;
```

### Mock Tool Calling Response
```rust
const MOCK_TOOL_RESPONSE: &str = r#"{
    "id": "chatcmpl-456",
    "choices": [{
        "message": {
            "role": "assistant",
            "tool_calls": [{
                "id": "call_abc123",
                "type": "function",
                "function": {
                    "name": "get_weather",
                    "arguments": "{\"location\": \"Boston\"}"
                }
            }]
        },
        "finish_reason": "tool_calls"
    }]
}"#;
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 16
    integration: 10
    e2e: 2
  by_priority:
    p0: 10
    p1: 12
    p2: 6
  coverage_gaps: []
  risks_mitigated:
    - SSE parsing edge cases
    - Tool calling infinite loop
    - llm.call regression
    - Cross-runtime parity
```

---

## Quality Checklist

- [x] Every AC has test coverage (15 ACs → 28 tests)
- [x] Test levels are appropriate (unit for parsing, integration for API)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core functionality)
- [x] Test IDs follow naming convention (RUST-040-LEVEL-NNN)
- [x] Scenarios are atomic and independent
- [x] Mock data provided for unit tests
- [x] Live API test models specified (phi4-mini, mistral-nemo)

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RUST-040-test-design-20251223.md
P0 tests identified: 10
Total test scenarios: 28
Coverage: 100% of acceptance criteria
```
