# Test Design: Story TEA-RUST-045

**Date:** 2025-01-16
**Designer:** Quinn (Test Architect)
**Story:** Rust Memory Embed Action - Cross-Runtime Parity

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 18 |
| Unit tests | 10 (56%) |
| Integration tests | 6 (33%) |
| E2E tests | 2 (11%) |
| Priority distribution | P0: 6, P1: 8, P2: 4 |
| AC Coverage | 100% (14/14) |

### Risk-Based Testing Focus

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Missing `text` parameter crashes | Medium | High | RUST-045-UNIT-001 |
| Feature gate not enforced | Low | High | RUST-045-UNIT-006, RUST-045-INT-003 |
| Model path resolution fails | Medium | Medium | RUST-045-UNIT-004, RUST-045-INT-002 |
| Regression in existing `llm.*` | Low | High | RUST-045-INT-005 |
| Cross-runtime parity mismatch | Medium | Medium | RUST-045-E2E-001 |

## Test Scenarios by Acceptance Criteria

### AC1: `llm.embed` generates vector embeddings using local LLM

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-001 | Unit | P0 | Missing `text` parameter returns `TeaError::InvalidInput` | Input validation - fail fast |
| RUST-045-UNIT-002 | Unit | P1 | Empty string `text` parameter handled | Edge case - empty input |
| RUST-045-INT-001 | Integration | P0 | `llm.embed` returns valid embedding vector | Core functionality with live model |

**Test Details:**

```rust
// RUST-045-UNIT-001: Missing text parameter
#[test]
fn test_llm_embed_missing_text_returns_error() {
    let state = json!({});
    let params: HashMap<String, JsonValue> = HashMap::new(); // No 'text'
    let result = llm_embed(&state, &params);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("text"));
}

// RUST-045-UNIT-002: Empty string handling
#[test]
fn test_llm_embed_empty_text() {
    let state = json!({});
    let params: HashMap<String, JsonValue> = [
        ("text".to_string(), json!("")),
    ].into_iter().collect();
    // Empty text may succeed or fail gracefully - document expected behavior
}
```

---

### AC2: `memory.embed` registered as alias for Python parity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-003 | Unit | P0 | `memory.embed` registered in action registry | Verify alias registration |
| RUST-045-INT-004 | Integration | P1 | `memory.embed` produces identical output to `llm.embed` | Alias equivalence |

**Test Details:**

```rust
// RUST-045-UNIT-003: Alias registration
#[test]
fn test_memory_embed_registered() {
    let registry = ActionRegistry::new();
    llm::register(&registry);
    assert!(registry.get("memory.embed").is_some());
    assert!(registry.get("llm.embed").is_some());
}
```

---

### AC3: Action returns `embedding`, `model`, `dimensions`, `tokens_used`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-INT-001 | Integration | P0 | Output contains all required fields | Output format validation |
| RUST-045-UNIT-007 | Unit | P1 | `dimensions` matches `embedding.len()` | Data consistency |

**Test Details:**

```rust
// RUST-045-INT-001: Output format (with live model)
#[test]
#[ignore]
fn test_llm_embed_output_format() {
    let result = llm_embed(&state, &params).unwrap();
    assert!(result.get("embedding").is_some());
    assert!(result.get("model").is_some());
    assert!(result.get("dimensions").is_some());
    // tokens_used is optional
    let embedding = result["embedding"].as_array().unwrap();
    let dimensions = result["dimensions"].as_u64().unwrap() as usize;
    assert_eq!(embedding.len(), dimensions);
}
```

---

### AC4: Action requires `text` parameter (string to embed)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-001 | Unit | P0 | Missing `text` returns error | Already covered above |
| RUST-045-UNIT-008 | Unit | P1 | Non-string `text` parameter (number) returns error | Type validation |
| RUST-045-UNIT-009 | Unit | P2 | Unicode text handled correctly | i18n support |

---

### AC5: Action supports optional `model_path` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-004 | Unit | P1 | `model_path` parameter extracted correctly | Parameter handling |
| RUST-045-INT-002 | Integration | P1 | Custom `model_path` overrides default | Configuration override |
| RUST-045-UNIT-005 | Unit | P2 | Invalid `model_path` returns clear error | Error messaging |

**Test Details:**

```rust
// RUST-045-UNIT-005: Invalid model path
#[test]
fn test_llm_embed_invalid_model_path() {
    let state = json!({});
    let params: HashMap<String, JsonValue> = [
        ("text".to_string(), json!("test")),
        ("model_path".to_string(), json!("/nonexistent/model.gguf")),
    ].into_iter().collect();
    let result = llm_embed(&state, &params);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found")
         || result.unwrap_err().to_string().contains("model"));
}
```

---

### AC6: Graceful error when `llm-local` feature is not compiled

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-006 | Unit | P0 | Non-llm-local build returns descriptive error | Feature gate UX |
| RUST-045-INT-003 | Integration | P1 | Error message guides user to correct build command | User experience |

**Test Details:**

```rust
// RUST-045-UNIT-006: Feature gate error (test without llm-local)
#[cfg(not(feature = "llm-local"))]
#[test]
fn test_llm_embed_without_feature_returns_error() {
    let state = json!({});
    let params: HashMap<String, JsonValue> = [
        ("text".to_string(), json!("test")),
    ].into_iter().collect();
    let result = llm_embed(&state, &params);
    assert!(result.is_err());
    let err_msg = result.unwrap_err().to_string();
    assert!(err_msg.contains("llm-local") || err_msg.contains("feature"));
}
```

---

### AC7: Existing `llm.*` actions continue unchanged (Regression)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-INT-005 | Integration | P0 | `llm.call` works after adding `llm.embed` | Regression prevention |
| RUST-045-INT-006 | Integration | P1 | `llm.chat` works after adding `llm.embed` | Regression prevention |

---

### AC8: YAML syntax matches Python `memory.embed` (Cross-Runtime Parity)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-E2E-001 | E2E | P1 | Same YAML agent produces equivalent embeddings in Rust and Python | Cross-runtime validation |

**Test Details:**

```yaml
# Cross-runtime parity test YAML
name: embed-parity-test
state_schema:
  text: str
  embedding: list
  dimensions: int

nodes:
  - name: embed
    uses: memory.embed
    with:
      text: "{{ state.text }}"
    outputs:
      embedding: embedding
      dimensions: dimensions

edges:
  - from: __start__
    to: embed
  - from: embed
    to: __end__
```

---

### AC9: Actions registered in `register()` function

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-003 | Unit | P0 | Already covered - registration test | Registration verification |

---

### AC10: Error handling follows `TeaError` patterns

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-010 | Unit | P2 | All errors are `TeaError` variants | Error type consistency |

---

### AC11: Feature-gated behind `llm-local`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-UNIT-006 | Unit | P0 | Already covered - feature gate test | Build configuration |

---

### AC12-13: Unit and Integration tests exist

These ACs are meta-requirements satisfied by implementing the tests above.

---

### AC14: Documentation updated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-045-E2E-002 | E2E | P2 | Documentation includes `llm.embed` / `memory.embed` | Manual review + grep |

---

## Test Scenario Summary Table

| ID | Level | Priority | AC | Description |
|----|-------|----------|----|-|
| RUST-045-UNIT-001 | Unit | P0 | AC1,4 | Missing `text` parameter returns error |
| RUST-045-UNIT-002 | Unit | P1 | AC1 | Empty string `text` handling |
| RUST-045-UNIT-003 | Unit | P0 | AC2,9 | `memory.embed` registered in registry |
| RUST-045-UNIT-004 | Unit | P1 | AC5 | `model_path` parameter extraction |
| RUST-045-UNIT-005 | Unit | P2 | AC5 | Invalid `model_path` error message |
| RUST-045-UNIT-006 | Unit | P0 | AC6,11 | Feature gate error without `llm-local` |
| RUST-045-UNIT-007 | Unit | P1 | AC3 | `dimensions` matches `embedding.len()` |
| RUST-045-UNIT-008 | Unit | P1 | AC4 | Non-string `text` returns error |
| RUST-045-UNIT-009 | Unit | P2 | AC4 | Unicode text handling |
| RUST-045-UNIT-010 | Unit | P2 | AC10 | Errors are `TeaError` variants |
| RUST-045-INT-001 | Integration | P0 | AC1,3 | Live embedding returns valid output |
| RUST-045-INT-002 | Integration | P1 | AC5 | Custom `model_path` override works |
| RUST-045-INT-003 | Integration | P1 | AC6 | Feature gate error is actionable |
| RUST-045-INT-004 | Integration | P1 | AC2 | `memory.embed` = `llm.embed` output |
| RUST-045-INT-005 | Integration | P0 | AC7 | `llm.call` regression test |
| RUST-045-INT-006 | Integration | P1 | AC7 | `llm.chat` regression test |
| RUST-045-E2E-001 | E2E | P1 | AC8 | Cross-runtime YAML parity |
| RUST-045-E2E-002 | E2E | P2 | AC14 | Documentation completeness |

---

## Recommended Execution Order

1. **P0 Unit tests** (fast, no dependencies):
   - RUST-045-UNIT-001, UNIT-003, UNIT-006

2. **P0 Integration tests** (require model):
   - RUST-045-INT-001, INT-005

3. **P1 Unit tests**:
   - RUST-045-UNIT-002, UNIT-004, UNIT-007, UNIT-008

4. **P1 Integration tests**:
   - RUST-045-INT-002, INT-003, INT-004, INT-006

5. **P1 E2E tests**:
   - RUST-045-E2E-001

6. **P2 tests** (as time permits):
   - RUST-045-UNIT-005, UNIT-009, UNIT-010, E2E-002

---

## Test Execution Commands

```bash
# Unit tests (fast, no model required)
cargo test --features llm-local test_llm_embed

# Unit tests for non-llm-local builds
cargo test test_llm_embed_without_feature

# Integration tests (require model)
TEA_MODEL_PATH=/path/to/model.gguf cargo test --features llm-local -- --ignored

# Full test suite
cargo test --features llm-local && \
TEA_MODEL_PATH=/path/to/model.gguf cargo test --features llm-local -- --ignored
```

---

## Quality Checklist

- [x] Every AC has test coverage (14/14)
- [x] Test levels are appropriate (unit for validation, integration for live model)
- [x] No duplicate coverage across levels
- [x] Critical paths have P0 priority
- [x] Test IDs follow naming convention (RUST-045-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 18
  by_level:
    unit: 10
    integration: 6
    e2e: 2
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  ac_coverage: "14/14 (100%)"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RUST-045-test-design-20250116.md
P0 tests identified: 6
Critical paths covered: Input validation, Feature gate, Core embedding, Regression
```
