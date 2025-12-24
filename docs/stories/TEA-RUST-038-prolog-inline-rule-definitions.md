# Story TEA-RUST-038: Support Inline Rule Definitions in Rust Prolog Nodes

## Status

Done

## Parent Epic

[TEA-PROLOG-001-prolog-integration-epic.md](TEA-PROLOG-001-prolog-integration-epic.md)

## Story

**As a** developer using The Edge Agent with Prolog nodes,
**I want** to define inline Prolog rules in my node code,
**so that** I can use the full expressiveness of Prolog including custom predicates within YAML agents.

## Story Context

**Existing System Integration:**

- **Integrates with:** `PrologRuntime` struct in `rust/src/engine/prolog_runtime.rs`
- **Technology:** Rust + swipl-rs 0.3.16 (SWI-Prolog bindings)
- **Follows pattern:** Python's janus-swi implementation handles rules transparently
- **Touch points:** `execute_node_code()` function (currently joins all lines as queries)

**Background:**

When users write Prolog code in YAML nodes, they may include inline rule definitions:

```yaml
- name: apply_rule
  language: prolog
  run: |
    add_ten(X, Y) :- Y is X + 10.
    state(doubled, D),
    add_ten(D, R),
    return(result, R).
```

Currently, `execute_node_code()` joins all lines with `, ` and executes as a single query:
```prolog
add_ten(X, Y) :- Y is X + 10, state(doubled, D), add_ten(D, R), return(result, R)
```

This is invalid Prolog syntax because the rule definition is mixed with query goals.

**Current Error:**
```
Error: Prolog error: Failed to parse Prolog code: Exception
```

**Examples Affected:**
- `examples/prolog/simple-prolog-agent.yaml` (node `apply_rule`)
- `examples/prolog/neurosymbolic/reasoning-chain.yaml` (all nodes using conditionals)

## Acceptance Criteria

**Functional Requirements:**

1. Inline rule definitions (`head :- body.`) are correctly parsed and asserted
2. Rules are asserted using `assertz/1` before query execution
3. Query goals (non-rule lines) execute after rules are asserted
4. Multiple inline rules in a single node are supported
5. Rules with complex bodies (multiple goals, cuts, conditionals) are handled

**Integration Requirements:**

6. Existing nodes without inline rules continue to work unchanged
7. `state/2` and `return/2` predicates continue to work in query portions
8. Rules are cleaned up after node execution (no pollution between nodes)
9. Parallel execution remains thread-safe (rules are per-engine)

**Quality Requirements:**

10. `examples/prolog/simple-prolog-agent.yaml` executes successfully
11. `examples/prolog/neurosymbolic/reasoning-chain.yaml` executes successfully
12. No regression in existing Prolog tests
13. Documentation updated with inline rule examples

## Tasks / Subtasks

- [x] **Task 1: Implement `parse_prolog_code()` following Python pattern** (AC: 1)
  - [x] Port Python's `_parse_code()` logic from `prolog_runtime.py` lines 524-587
  - [x] Categorize statements: directives (starts with `:-`), rules (contains `:-`), queries (else)
  - [x] Handle multi-line statements (accumulate until `.` terminator)

- [x] **Task 2: Implement rule assertion** (AC: 2, 3, 5)
  - [x] For each detected rule, wrap in `assertz((head :- body))`
  - [x] Execute rule assertions before main query
  - [x] Handle complex rule bodies with proper escaping

- [x] **Task 3: Implement query extraction and execution** (AC: 3, 4)
  - [x] Separate query goals from rule definitions
  - [x] Join query goals with `, ` as before
  - [x] Execute queries after all rules are asserted

- [x] **Task 4: Implement rule cleanup** (AC: 8, 9)
  - [x] Track asserted rules during execution
  - [x] Use `retractall/1` to remove rules after execution
  - [x] Ensure cleanup happens even on query failure

- [x] **Task 5: Verify backward compatibility** (AC: 6, 7)
  - [x] Run existing Prolog test suite
  - [x] Verify `state/2` and `return/2` still work
  - [x] Test nodes without inline rules

- [x] **Task 6: Test example files** (AC: 10, 11, 12)
  - [x] Run `simple-prolog-agent.yaml` end-to-end
  - [x] Run `reasoning-chain.yaml` end-to-end
  - [x] Verify all parity tests still pass

- [x] **Task 7: Update documentation** (AC: 13)
  - [x] Add inline rule examples to `docs/rust/prolog-guide.md`
  - [x] Document limitations (if any)

## Dev Notes

### Implementation Approach (Following Python Pattern)

The Python implementation in `python/src/the_edge_agent/prolog_runtime.py` (lines 524-587) uses a simple, proven approach. The key insight is to parse Prolog code into three categories:

1. **Directives**: Statements starting with `:-` (e.g., `:- use_module(library(clpfd)).`)
2. **Rule definitions**: Statements containing `:-` but not starting with it
3. **Query goals**: Everything else

**Why this works:** Prolog conditionals use `->` (if-then), not `:-` (rule operator). Only rule definitions contain `:-` in user code, so no complex parentheses-depth analysis is needed.

```rust
// Port of Python's _parse_code() approach
fn parse_prolog_code(&self, code: &str) -> (Vec<String>, Vec<String>, Vec<String>) {
    let mut directives = Vec::new();
    let mut rules = Vec::new();
    let mut queries = Vec::new();

    for statement in self.split_statements(code) {
        if statement.starts_with(":-") {
            // Directive (e.g., :- use_module(...))
            directives.push(statement);
        } else if statement.contains(":-") {
            // Rule (head :- body)
            rules.push(statement);
        } else {
            // Query goal
            queries.push(statement);
        }
    }

    (directives, rules, queries)
}

// In execute_node_code():
fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
    let (directives, rules, queries) = self.parse_prolog_code(code)?;

    // Create engine context
    let engine = Engine::new();
    let activation = engine.activate();
    let context: Context<_> = activation.into();

    // Set up predicates and state
    self.setup_predicates_in_context(&context);
    self.set_state_facts(&context, state);

    // Handle directives (e.g., :- use_module(...))
    for directive in &directives {
        if let Ok(term) = context.term_from_string(directive) {
            let _ = context.call_term_once(&term);
        }
    }

    // Assert rules using assertz (equivalent to Python's janus.consult())
    let mut asserted_rules = Vec::new();
    for rule in &rules {
        let assert_cmd = format!("assertz(({}))", rule);
        if let Ok(term) = context.term_from_string(&assert_cmd) {
            context.call_term_once(&term)?;
            asserted_rules.push(rule.clone());
        }
    }

    // Execute queries
    let query_code = queries.join(", ");
    let result = self.execute_in_context(&context, &query_code, self.is_sandboxed())?;

    // Clean up asserted rules
    for rule in &asserted_rules {
        if let Some(head) = self.extract_rule_head(rule) {
            let retract_cmd = format!("retractall({})", head);
            if let Ok(term) = context.term_from_string(&retract_cmd) {
                let _ = context.call_term_once(&term);
            }
        }
    }

    Ok(result)
}
```

### Statement Splitting

Split code into complete statements by accumulating lines until a `.` terminator:

```rust
fn split_statements(&self, code: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut current = String::new();

    for line in code.lines() {
        let trimmed = line.trim();

        // Skip empty lines and pure comments
        if trimmed.is_empty() || trimmed.starts_with('%') {
            continue;
        }

        // Remove inline comments (but preserve % in strings)
        let clean = self.strip_inline_comment(trimmed);
        if clean.is_empty() {
            continue;
        }

        if !current.is_empty() {
            current.push(' ');
        }
        current.push_str(&clean);

        // Statement complete when ending with period
        if current.ends_with('.') {
            statements.push(current.trim_end_matches('.').to_string());
            current.clear();
        }
    }

    // Handle remaining content (no trailing period)
    if !current.is_empty() {
        statements.push(current);
    }

    statements
}
```

### Cross-Runtime Parity

This implementation mirrors Python's `_parse_code()` method to ensure YAML agents with Prolog code work identically in both runtimes. Key parity points:

| Aspect | Python | Rust (This Story) |
|--------|--------|-------------------|
| Rule detection | `':-' in statement` | `statement.contains(":-")` |
| Directive detection | `statement.startswith(':-')` | `statement.starts_with(":-")` |
| Rule loading | `janus.consult()` | `assertz()` |
| Statement splitting | Accumulate until `.` | Accumulate until `.` |

### Relevant Source Tree

```
rust/src/engine/prolog_runtime.rs
├── PrologRuntime struct
├── execute() (lines 394-430) - Main entry point
├── execute_node_code() (lines 864-887) - **TARGET FOR CHANGES**
├── execute_in_context() (lines 516-549) - Query execution
└── parse_prolog_code() - **NEW FUNCTION**
```

### Testing

**Test files affected:**
- `examples/prolog/simple-prolog-agent.yaml`
- `examples/prolog/neurosymbolic/reasoning-chain.yaml`

**Run tests:**
```bash
cd rust && LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux cargo test --features prolog

# Test specific example
cd rust && LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux cargo run --features prolog -- \
  run ../examples/prolog/simple-prolog-agent.yaml --input '{"value": 21}'
```

**Expected output for simple-prolog-agent.yaml:**
```json
{
  "value": 21,
  "doubled": 42,
  "result": 52,
  "category": "medium"
}
```

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** Statement splitting may fail on complex multi-line code
- **Mitigation:** Follow Python's proven `_parse_code()` pattern exactly
- **Secondary Risk:** Rule cleanup may miss edge cases
- **Mitigation:** Use `retractall()` with extracted predicate head
- **Rollback:** Revert single function change

**Compatibility Verification:**

- [x] No breaking changes to existing APIs
- [x] No database/config changes
- [x] Follows existing code patterns
- [x] Performance impact negligible (parsing overhead per node)

## Definition of Done

- [x] Inline rule definitions work in Rust Prolog nodes
- [x] `simple-prolog-agent.yaml` executes successfully with correct output
- [x] `reasoning-chain.yaml` executes successfully with correct output
- [x] No regression in existing Prolog tests (244 tests pass)
- [x] Documentation updated with inline rule examples
- [x] Code follows existing patterns and standards

## QA Results

### Review Date: 2025-12-23

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT** - The implementation demonstrates high-quality Rust code with strong adherence to project patterns. Key observations:

1. **Clean Architecture**: New functions (`parse_prolog_code`, `strip_inline_comment`, `extract_rule_head`, `count_arguments`) are well-separated and have clear responsibilities
2. **Cross-Runtime Parity**: Implementation accurately mirrors Python's `_parse_code()` pattern, ensuring YAML agents work identically across runtimes
3. **Defensive Programming**: Proper handling of edge cases (empty code, no rules, multi-line rules, nested parentheses in arguments)
4. **Excellent Documentation**: All new functions have clear doc comments explaining purpose and behavior
5. **Test Coverage**: 20 new unit tests covering parsing, extraction, and integration scenarios

### Refactoring Performed

None required - code quality is already at production level.

### Compliance Check

- Coding Standards: ✓ Follows Rust idioms, proper lifetimes, no clippy errors
- Project Structure: ✓ Changes contained to `prolog_runtime.rs` as expected
- Testing Strategy: ✓ Comprehensive unit tests (parsing) + integration tests (execution) + E2E (example files)
- All ACs Met: ✓ All 13 acceptance criteria verified

### Improvements Checklist

- [x] Parse inline rule definitions correctly (AC 1)
- [x] Assert rules using `assertz/1` (AC 2)
- [x] Execute queries after rules asserted (AC 3)
- [x] Support multiple inline rules (AC 4)
- [x] Handle complex rule bodies (AC 5)
- [x] Backward compatibility maintained (AC 6, 7)
- [x] Rule cleanup after execution (AC 8)
- [x] Thread-safe parallel execution (AC 9)
- [x] Example files execute successfully (AC 10, 11, 12)
- [x] Documentation updated (AC 13)
- [ ] *Optional*: Consider adding escaped quote handling in `strip_inline_comment()` for edge cases like `'%'` in strings

### Security Review

No security concerns. The implementation:
- Uses existing sandbox infrastructure
- Properly cleans up asserted rules after execution
- Does not introduce any new attack vectors

### Performance Considerations

Minimal overhead:
- String parsing is O(n) for code length
- Rule assertion/retraction adds ~1-2ms per rule
- Optimization path: queries-only code takes fast path bypassing assertion logic

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RUST-038-prolog-inline-rule-definitions.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, comprehensive test coverage, excellent code quality.

---

**Previous Test Design Review:** 2025-12-23
**Reviewer:** Quinn (Test Architect)
**Status:** Test Design Complete (Revised)

### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total scenarios | 19 |
| Unit tests | 7 (37%) |
| Integration tests | 9 (47%) |
| E2E tests | 3 (16%) |
| P0 (Critical) | 6 |
| P1 (High) | 9 |
| P2 (Medium) | 4 |

### P0 Tests (Must Pass Before Merge)

| ID | Level | Test Description |
|----|-------|------------------|
| RUST-038-UNIT-001 | Unit | Parse simple rule `add_ten(X, Y) :- Y is X + 10.` |
| RUST-038-UNIT-005 | Unit | Parse multi-line rule spanning multiple lines |
| RUST-038-INT-001 | Integration | Assert rule then use in query |
| RUST-038-INT-002 | Integration | Assert multiple rules, use sequentially |
| RUST-038-INT-007 | Integration | Node without rules - backward compatibility |
| RUST-038-INT-010 | Integration | Rust `parse_prolog_code()` matches Python `_parse_code()` output |

### Key Risks Identified

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Statement splitting fails on complex code | Low | Medium | UNIT-005 |
| Rules leak between node executions | Low | High | INT-009, E2E-001 |
| Cross-runtime parity broken | Low | High | INT-010 |
| Backward compatibility broken | Low | Critical | INT-007 |

### QA Recommendations

1. **Follow Python's `_parse_code()` pattern exactly** - Proven approach, ensures cross-runtime parity
2. **Implement `parse_prolog_code()` as a separate, testable function** - Enables unit testing of parsing logic in isolation
3. **Use explicit rule tracking for reliable cleanup** - Track asserted rules in a Vec for deterministic retraction
4. **Run existing Prolog test suite after implementation** - Verify no regression in 80+ existing tests

### Test Artifacts

- **Test Design:** `docs/qa/assessments/TEA-RUST-038-test-design-20251223.md`
- **Gate File:** `docs/qa/gates/TEA-RUST-038-prolog-inline-rule-definitions.yml`

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Status | Description |
|------|--------|-------------|
| `rust/src/engine/prolog_runtime.rs` | Modified | Added `parse_prolog_code()`, `strip_inline_comment()`, `extract_rule_head()`, `count_arguments()`, updated `execute_node_code()` |
| `docs/rust/prolog-guide.md` | Modified | Added "Inline Rule Definitions" section with examples |

### Debug Log References

None - Implementation completed without issues.

### Completion Notes

1. **Implementation Pattern**: Ported Python's `_parse_code()` logic to Rust, maintaining cross-runtime parity
2. **Key Functions Added**:
   - `parse_prolog_code()` - Splits code into directives, rules, and queries
   - `strip_inline_comment()` - Removes `%` comments while preserving `%` in strings
   - `extract_rule_head()` - Extracts predicate head with wildcards for cleanup (e.g., `add_ten(X, Y)` → `add_ten(_, _)`)
   - `count_arguments()` - Counts arguments respecting nesting for wildcard generation
3. **Test Coverage**: Added 20 unit tests covering parsing, extraction, and execution with inline rules
4. **Validation**:
   - `simple-prolog-agent.yaml` outputs `{"value": 21, "doubled": 42, "result": 52, "category": "medium"}`
   - `reasoning-chain.yaml` outputs correct multi-step reasoning with flu diagnosis
   - All 244 tests pass with no regression

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-23 | 0.1 | Initial story draft | Sarah (PO) |
| 2025-12-23 | 0.2 | Added QA test design (22 scenarios) | Quinn (QA) |
| 2025-12-23 | 0.3 | Simplified approach based on Python implementation, revised tests (19 scenarios) | Sarah (PO) |
| 2025-12-23 | 1.0 | Implementation complete, all tests pass | James (Dev) |
| 2025-12-23 | 1.1 | QA Review complete - PASS, recommended for Done | Quinn (QA) |
