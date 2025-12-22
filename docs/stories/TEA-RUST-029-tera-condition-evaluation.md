# Story TEA-RUST-029: Tera-Based Condition Evaluation

## Status

**Done**

**SM Validation:** 2025-12-21 - Story validation PASSED (9/10 readiness score)

**QA Review:** 2025-12-21 - Gate: CONCERNS (Quality Score: 90/100) - AC-18 benchmark deferred as follow-up

## Story

**As a** YAML agent developer migrating from Python,
**I want** conditional routing expressions to use Tera (Jinja2-compatible) syntax,
**So that** I can use the same condition expressions in both Python and Rust implementations without rewriting.

## Context & Motivation

### Current State

The Rust implementation uses **Lua expressions** for conditional routing:

```rust
// executor.rs:521-522
let result = if let Some(expr) = condition {
    self.lua.eval_condition(expr, state)?  // Lua evaluation
}
```

Example Lua condition:
```lua
state.value > 5 and "high" or "low"
```

### Python State (TEA-YAML-001)

The Python implementation uses **Jinja2** for conditions:

```python
# yaml_engine.py:1463
def _evaluate_condition(self, expr: str, state: Dict[str, Any]) -> bool:
    """TEA-YAML-001: Unified condition evaluation using Jinja2 templates."""
```

Example Jinja2 conditions:
```jinja2
{{ state.x > 5 }}
{{ 'urgent' in state.tags }}
state.x > 5
!escalate
```

### Problem Statement

| Aspect | Python (Jinja2) | Rust (Lua) | Issue |
|--------|-----------------|------------|-------|
| Syntax | `{{ state.x > 5 }}` | `state.x > 5` | Different template syntax |
| Operators | Python-style | Lua-style (`and`/`or`) | Incompatible operators |
| Negation | `!variable` | `not variable` | Different negation |
| Return type | `bool` | `String` | Different semantics |
| Portability | Familiar to most devs | Less familiar | Learning curve |

### Why Align to Tera/Jinja2?

1. **Parity with Python**: Same YAML agents work in both runtimes
2. **Consistency**: Template variables (`{{ state.key }}`) already use Tera
3. **Developer familiarity**: Jinja2 syntax is widely known (Flask, Ansible, dbt)
4. **Single template engine**: One engine (Tera) for all template processing
5. **Reduced complexity**: Remove Lua dependency for conditions (keep for `run:` blocks)

## Acceptance Criteria

### Condition Syntax Support

- [x] **AC-1**: Jinja2-style conditions work: `{{ state.x > 5 }}` returns boolean
- [x] **AC-2**: Expression-only conditions work: `state.x > 5` (auto-wrapped in `{{ }}`)
- [x] **AC-3**: Simple variable reference works: `has_results` → `state.get('has_results', false)`
- [x] **AC-4**: Negation syntax works: `!escalate` → `not state.get('escalate', false)`

### Operators

- [x] **AC-5**: Comparison operators work: `>`, `<`, `>=`, `<=`, `==`, `!=`
- [x] **AC-6**: Logical operators work: `and`, `or`, `not`
- [x] **AC-7**: Containment check works: `'urgent' in state.tags` (Tera syntax)
- [x] **AC-8**: Tera tests work: `state.name is starting_with("test")` (Note: Tera uses `is` tests, not Python method calls)

### Return Value Semantics

- [x] **AC-9**: Condition evaluation returns `bool`, not `String`
- [x] **AC-10**: Truthy/falsy values handled correctly (0, "", [], {} → false)

### Edge Routing

- [x] **AC-11**: `when: "{{ state.ready }}"` routes to edge when condition is true
- [x] **AC-12**: Multiple conditional edges: first matching edge taken
- [x] **AC-13**: No matching condition returns error with helpful message

### Backward Compatibility

- [x] **AC-14**: Existing Tera template syntax unchanged (`{{ state.key }}`)
- [x] **AC-15**: `run:` blocks continue to use Lua for execution
- [x] **AC-16**: Programmatic `conditional_fn` still works (Rust function callbacks)

### Performance

- [x] **AC-17**: Condition expression compilation cached for repeated evaluation (via existing template_cache)
- [ ] **AC-18**: Performance within 20% of Lua conditions (benchmark: 1000 condition evaluations) - Benchmark not yet implemented

## Technical Design

### Current Architecture

```
                    Rust TEA Engine
┌─────────────────────────────────────────────────┐
│                                                 │
│  YAML Engine (Tera)    │    Lua Runtime        │
│  ────────────────────  │    ─────────────────  │
│  • {{ state.key }}     │    • run: blocks      │
│  • Template params     │    • eval_condition() │ ← CHANGE THIS
│  • Variable subst      │    • Custom actions   │
│                        │                       │
└─────────────────────────────────────────────────┘
```

### Target Architecture

```
                    Rust TEA Engine
┌─────────────────────────────────────────────────┐
│                                                 │
│  YAML Engine (Tera) - UNIFIED                   │
│  ───────────────────────────────────────────   │
│  • {{ state.key }}     - Variable substitution  │
│  • {{ state.x > 5 }}   - Condition evaluation   │ ← NEW
│  • Template params     - Action parameters      │
│                                                 │
│  Lua Runtime - CODE EXECUTION ONLY              │
│  ─────────────────────────────────────────────  │
│  • run: blocks         - Inline Lua code        │
│  • Custom actions      - Lua module loading     │
│                                                 │
└─────────────────────────────────────────────────┘
```

### Implementation Strategy

#### 1. Add `eval_condition()` to YamlEngine

```rust
impl YamlEngine {
    /// Evaluate a condition expression using Tera
    ///
    /// Supports (per Python parity):
    /// - Jinja2 template: "{{ state.x > 5 }}"
    /// - Expression only: "state.x > 5" (auto-wrapped)
    /// - Simple variable: "has_results" -> state.get('has_results', false)
    /// - Negation: "!escalate" -> not state.get('escalate', false)
    pub fn eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool> {
        let expr = expr.trim();

        // Handle simple negation: "!variable"
        if expr.starts_with('!') && !expr.starts_with("{{") {
            let var_name = &expr[1..].trim();
            if is_identifier(var_name) {
                return self.get_state_bool(state, var_name, false).map(|v| !v);
            }
        }

        // Handle simple variable reference: "variable_name"
        if is_identifier(expr) {
            return self.get_state_bool(state, expr, false);
        }

        // If already a Jinja2 template, process it
        let template_expr = if expr.contains("{{") || expr.contains("{%") {
            expr.to_string()
        } else {
            // Wrap as Jinja2 expression
            format!("{{{{ {} }}}}", expr)
        };

        // Render and parse as boolean
        let result = self.render_template(&template_expr, state, &HashMap::new())?;
        self.parse_bool_result(&result)
    }
}
```

#### 2. Update Executor to Use Tera Conditions

```rust
// executor.rs - change from:
let result = if let Some(expr) = condition {
    self.lua.eval_condition(expr, state)?
}

// to:
let result = if let Some(expr) = condition {
    self.yaml_engine.eval_condition(expr, state)?
}
```

#### 3. Update Edge Matching Logic

```rust
// Current: expects String result matching target name
// New: expects bool result

match &edge.edge_type {
    EdgeType::Conditional { condition, target, when_value } => {
        let result = self.yaml_engine.eval_condition(condition, state)?;
        let expected = when_value.unwrap_or(true);
        if result == expected {
            return Ok(NextNode::Single(target.to_string()));
        }
    }
}
```

### YAML Syntax Changes

#### Current Rust Syntax (Lua)

```yaml
edges:
  - from: router
    condition:
      type: expression
      value: "state.count > 0 and 'process' or 'skip'"
    targets:
      process: process_node
      skip: skip_node
```

#### New Rust Syntax (Tera - Python parity)

```yaml
edges:
  # Method 1: Simple when clause
  - from: check
    to: proceed
    when: "{{ state.count > 0 }}"

  - from: check
    to: skip
    when: "{{ state.count <= 0 }}"

  # Method 2: Expression without braces
  - from: check
    to: proceed
    when: "state.count > 0"

  # Method 3: Simple variable reference
  - from: check
    to: proceed
    when: "has_results"

  # Method 4: Negation shorthand
  - from: check
    to: skip
    when: "!should_process"
```

## Dependencies

**Blocked By:**
- TEA-RUST-003 (YAML parser and Tera template engine) - ✅ Done

**Blocks:**
- None (can be done independently)

**Related Stories:**
- TEA-YAML-001 (Python Jinja2 implementation) - Reference for parity
- TEA-RUST-005 (Conditional routing with Lua) - Supersedes Lua conditions

**Crate Dependencies:**
- `tera` - Already in use for templates

## Tasks / Subtasks

### Task 1: Add `eval_condition()` to YamlEngine (AC: 1-4, 9-10)
**File:** `rust/src/engine/yaml.rs`
- [x] Implement `eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool>`
- [x] Add helper `fn is_identifier(s: &str) -> bool`
- [x] Add helper `fn get_state_bool(&self, state: &JsonValue, key: &str) -> TeaResult<bool>`
- [x] Add helper `fn parse_bool_result(result: &str) -> bool`
- [x] Handle simple negation syntax: `!variable`
- [x] Handle simple variable reference: `variable_name`
- [x] Handle Jinja2 template: `{{ expr }}`
- [x] Handle bare expression: `expr` → auto-wrap to `{{ expr }}`
- [x] Implement truthy/falsy logic for Tera results (0, "", "false", "[]", "{}" → false)

### Task 2: Add Condition Operators Support (AC: 5-8)
**File:** `rust/src/engine/yaml.rs` (tests in `rust/src/engine/yaml.rs::tests`)
- [x] Verify Tera supports comparison operators: `>`, `<`, `>=`, `<=`, `==`, `!=`
- [x] Verify Tera supports logical operators: `and`, `or`, `not`
- [x] Verify containment works: `'x' in state.list` (Tera syntax)
- [x] Add tests for Tera tests: `state.name is starting_with("test")`
- [x] Document Tera vs Jinja2 syntax differences (see Dev Notes)

### Task 3: Update Executor Edge Matching (AC: 11-13)
**File:** `rust/src/engine/executor.rs` (function: `get_next_node()`)
- [x] Access `yaml_engine` via `self`
- [x] Modify `get_next_node()` to use Tera-based condition evaluation
- [x] Support both boolean mode (`{{ expr }}`) and string-match mode (`{% if %}`)
- [x] Implement first-matching-edge logic (iterate edges, return first match)
- [x] Add helpful error message: `TeaError::NoMatchingEdge("No condition matched for edges from '{node}'. Evaluated: ['{expr1}' → '{result}', ...]")`

### Task 4: Maintain Backward Compatibility (AC: 14-16)
**Files:** `rust/src/engine/yaml.rs`, `rust/src/engine/executor.rs`, `rust/src/engine/graph.rs`
- [x] Ensure `render_template()` unchanged for `{{ state.key }}` variable substitution
- [x] Verify Lua runtime still used for `run:` blocks (`rust/src/engine/lua_runtime.rs`)
- [x] Keep `conditional_fn` (Rust callback) support in executor edge matching

### Task 5: Implement Caching (AC: 17)
**File:** `rust/src/engine/yaml.rs`
- [x] Condition expressions use existing `template_cache` via `render_template()`
- [x] Caching is automatic for all Tera template rendering

### Task 6: Write Tests (AC: 1-17)
**File:** `rust/src/engine/yaml.rs::tests` (added to existing test module)
- [x] Test Jinja2 template conditions: `{{ state.x > 5 }}`
- [x] Test bare expressions: `state.x > 5`
- [x] Test variable references: `has_results`
- [x] Test negation: `!escalate`
- [x] Test comparison operators: `>`, `<`, `>=`, `<=`, `==`, `!=`
- [x] Test logical operators: `and`, `or`, `not`
- [x] Test truthy/falsy values: 0, "", [], {}
- [x] Updated existing edge routing tests to use Tera syntax
- [ ] Performance benchmark: `cargo bench` (not implemented)

### Task 7: Update Documentation
**Files:** `docs/stories/TEA-RUST-029-tera-condition-evaluation.md`
- [x] Updated code comments in `executor.rs` and `yaml.rs`
- [x] Added doc comments with examples to `eval_condition()`
- [ ] Create `rust/examples/conditional-routing.yaml` (future work)

## Test Scenarios

| ID | Scenario | Input | Expected |
|----|----------|-------|----------|
| COND-001 | Jinja2 comparison | `{{ state.x > 5 }}` with `{x: 10}` | `true` |
| COND-002 | Bare expression | `state.x > 5` with `{x: 3}` | `false` |
| COND-003 | Variable reference | `is_ready` with `{is_ready: true}` | `true` |
| COND-004 | Negation | `!skip` with `{skip: false}` | `true` |
| COND-005 | Containment | `'a' in state.tags` with `{tags: ["a","b"]}` | `true` |
| COND-006 | Logical and | `state.x > 0 and state.y > 0` | `true` if both |
| COND-007 | Missing variable | `missing_var` | `false` (default) |
| COND-008 | Falsy values | `{{ state.count }}` with `{count: 0}` | `false` |
| COND-009 | Edge routing | First matching `when:` edge taken | Correct target |
| COND-010 | No match error | All conditions false, no default | `TeaError::NoMatchingEdge` |
| COND-011 | Tera test syntax | `state.name is starting_with("test")` with `{name: "test_foo"}` | `true` |
| COND-012 | Empty string falsy | `{{ state.msg }}` with `{msg: ""}` | `false` |
| COND-013 | Empty array falsy | `{{ state.items }}` with `{items: []}` | `false` |

## Definition of Done

- [ ] All acceptance criteria met
- [ ] All tests passing
- [ ] Existing conditional edge tests updated/passing
- [ ] Documentation updated
- [ ] No performance regression
- [ ] Code reviewed

## Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Tera expression limitations vs Python | Low | Medium | Test all Python patterns, document differences |
| Breaking existing YAML agents | Low | High | Keep Lua fallback option initially |
| Performance regression | Low | Low | Cache compiled expressions |

## Future Considerations

After this story:
- **TEA-RUST-005 status**: Mark as "Superseded by TEA-RUST-029"
- Consider deprecating Lua for conditions entirely
- DSL-only mode (disable Lua `run:` for sandboxed execution)

---

## Dev Notes

### Source Tree (Relevant Files)

```
rust/src/engine/
├── yaml.rs           # YamlEngine - ADD eval_condition() here
├── executor.rs       # Executor::get_next_node() - MODIFY to use Tera conditions
├── graph.rs          # Edge types, EdgeType::Conditional - may need updates
├── lua_runtime.rs    # Current eval_condition() - KEEP for run: blocks only
└── mod.rs

rust/tests/
├── test_stategraph.rs    # Existing integration tests
├── test_conditions.rs    # NEW - condition evaluation tests
└── test_checkpoint.rs

rust/benches/
└── conditions.rs         # NEW - performance benchmarks
```

### Key Code Locations

| Component | File | Line | Description |
|-----------|------|------|-------------|
| Current `eval_condition()` | `lua_runtime.rs` | 327 | Lua-based, returns `Option<String>` |
| Edge matching | `executor.rs` | 509-560 | `get_next_node()` function |
| EdgeType::Conditional | `graph.rs` | 200, 233, 247 | Edge type definition |
| Template cache | `yaml.rs` | 198 | `template_cache: RwLock<HashMap>` |
| `render_template()` | `yaml.rs` | 425-492 | Existing Tera rendering |

### Tera vs Jinja2 Syntax Differences

| Feature | Jinja2 (Python) | Tera (Rust) | Notes |
|---------|-----------------|-------------|-------|
| String methods | `s.startswith('x')` | `s is starting_with("x")` | Tera uses "tests" |
| String methods | `s.endswith('x')` | `s is ending_with("x")` | Tera uses "tests" |
| String methods | `s.upper()` | `s \| upper` | Use filters |
| Contains | `'x' in list` | `'x' in list` | Same |
| Comparison | `x > 5` | `x > 5` | Same |
| Logical | `and`, `or`, `not` | `and`, `or`, `not` | Same |
| Negation | `!var` (custom) | `!var` (custom) | We implement this |

### Tera Built-in Tests (for AC-8)

Available tests in Tera that can be used with `is`:
- `starting_with(pattern)` - string starts with pattern
- `ending_with(pattern)` - string ends with pattern
- `containing(pattern)` - string contains pattern
- `matching(regex)` - string matches regex
- `defined` - variable is defined
- `undefined` - variable is undefined
- `odd` / `even` - number tests
- `divisibleby(n)` - divisibility test

### Error Message Format

```rust
// AC-13: Helpful error message format
TeaError::NoMatchingEdge(format!(
    "No condition matched for edges from '{}'. Evaluated: [{}]",
    node_name,
    conditions.iter()
        .map(|(expr, result)| format!("'{}' → {}", expr, result))
        .collect::<Vec<_>>()
        .join(", ")
))
```

### Testing Standards

| Aspect | Standard |
|--------|----------|
| Test location | `rust/tests/test_conditions.rs` |
| Test framework | Built-in Rust `#[test]` |
| Naming convention | `test_<feature>_<scenario>` |
| Run tests | `cargo test` |
| Run specific | `cargo test test_condition` |
| Benchmarks | `cargo bench` (requires `criterion` crate) |

### Migration Example

```yaml
# BEFORE (Lua conditions - TEA-RUST-005)
edges:
  - from: router
    condition:
      type: expression
      value: "state.count > 0 and 'yes' or 'no'"
    targets:
      yes: process_node
      no: skip_node

# AFTER (Tera conditions - TEA-RUST-029)
edges:
  - from: router
    to: process_node
    when: "{{ state.count > 0 }}"

  - from: router
    to: skip_node
    when: "{{ state.count <= 0 }}"
```

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-21 | 1.0 | Initial draft - Align Rust conditions with Tera/Jinja2 for Python parity | Sarah (PO Agent) |
| 2025-12-21 | 1.1 | Validation fixes: Added Dev Notes with source tree, fixed AC-8 for Tera syntax, added file paths to tasks, defined AC-18 threshold | Sarah (PO Agent) |
| 2025-12-21 | 1.2 | Status → Approved after validation pass (9/10 readiness) | Sarah (PO Agent) |

---

## Dev Agent Record

*This section is populated by the development agent during implementation.*

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

- Initial implementation failed tests due to Lua-style ternary expressions not working in Tera
- Fixed by updating tests to use Tera's `{% if %}...{% endif %}` syntax for string-match conditions
- Edge matching logic updated to detect template type and use appropriate evaluation mode

### Completion Notes

**Implementation Summary:**

1. **eval_condition() in YamlEngine** (yaml.rs:415-491):
   - Added `eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool>`
   - Added helper functions: `is_identifier()`, `is_truthy()`, `parse_bool_result()`, `get_state_bool()`
   - Supports all required syntax: Jinja2 templates, bare expressions, variable references, negation

2. **Executor Edge Matching** (executor.rs:531-572):
   - Modified `get_next_node()` to use Tera-based condition evaluation
   - Two evaluation modes based on template syntax:
     - Block templates (`{% if %}`) → string-match mode (for `targets:` syntax)
     - Expression templates (`{{ }}`) → boolean mode (for `when:` syntax)
   - Added detailed error messages showing evaluated conditions

3. **Test Updates** (tests/test_stategraph.rs):
   - Updated 10 tests to use Tera syntax instead of Lua-style conditions
   - All conditional routing tests now use `{% if %}...{% endif %}` for string-match or `{{ expr }}` for boolean

4. **New Tests** (src/engine/yaml.rs:824-943):
   - Added 9 new tests for `eval_condition()` covering all acceptance criteria
   - Tests for: Jinja2 templates, bare expressions, variable references, negation, comparison operators, logical operators, truthy/falsy values

**Limitations:**
- AC-18 (performance benchmark) not implemented - requires separate benchmark infrastructure
- Example YAML file not created - can be added as follow-up work

### Files Changed

| File | Change Type | Description |
|------|-------------|-------------|
| `rust/src/engine/yaml.rs` | Modified | Added `eval_condition()`, helper functions, and 9 new tests |
| `rust/src/engine/executor.rs` | Modified | Updated `get_next_node()` to use Tera conditions |
| `rust/tests/test_stategraph.rs` | Modified | Updated 10 tests to use Tera syntax |
| `docs/stories/TEA-RUST-029-tera-condition-evaluation.md` | Modified | Updated status and completion notes |

---

## QA Results

### Review Date: 2025-12-21

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall Assessment: EXCELLENT**

The implementation of TEA-RUST-029 demonstrates high-quality Rust code with clear separation of concerns, proper error handling, and comprehensive test coverage. The migration from Lua-based condition evaluation to Tera-based evaluation is well-executed with minimal disruption to the existing codebase.

**Key Strengths:**
1. **Clean API Design**: The `eval_condition()` function in `yaml.rs:448-480` provides a clear, well-documented public API with doc tests
2. **Robust Helper Functions**: Utility functions (`is_identifier`, `is_truthy`, `parse_bool_result`) are modular and testable
3. **Dual Mode Support**: The executor correctly handles both boolean mode (`{{ expr }}`) and string-match mode (`{% if %}`) for backward compatibility
4. **Error Handling**: Proper use of `TeaResult<bool>` with meaningful error messages, including the AC-13 error format with evaluated conditions

**Minor Observations:**
- Warning: Unused variable `engine` in `yaml.rs:740` (test code) - cosmetic only
- AC-18 (performance benchmark) not implemented - documented as "not yet implemented" in story

### Refactoring Performed

No refactoring was required. The code quality meets or exceeds project standards.

### Compliance Check

- Coding Standards: ✓ Follows Rust idioms, proper error handling with `thiserror`, clear documentation
- Project Structure: ✓ Implementation in correct modules (`yaml.rs`, `executor.rs`), tests in appropriate locations
- Testing Strategy: ✓ Unit tests for `eval_condition()`, integration tests updated for Tera syntax
- All ACs Met: ✓ (15 of 16 verified, AC-18 benchmark deferred)

### Improvements Checklist

- [x] All 16 testable acceptance criteria implemented
- [x] Tests cover all condition syntax variants (Jinja2, bare expression, variable, negation)
- [x] Truthy/falsy semantics correctly implemented
- [x] Error messages include evaluated conditions for debugging
- [x] Backward compatibility maintained for `conditional_fn` callbacks
- [x] Template caching leveraged through existing `render_template()` infrastructure
- [ ] AC-18: Performance benchmark not yet implemented (requires benchmark infrastructure)
- [ ] Example YAML file (`rust/examples/conditional-routing.yaml`) not created (future work)

### Security Review

**Status: PASS**

- Template rendering uses Tera's sandboxed environment
- No `exec()` or arbitrary code execution in condition evaluation
- Lua runtime remains isolated for `run:` blocks only
- No new attack vectors introduced by this change

### Performance Considerations

**Status: CONCERNS (Minor)**

- AC-18 specifies "Performance within 20% of Lua conditions (benchmark: 1000 condition evaluations)"
- Benchmark not implemented, so performance parity is unverified
- Mitigation: Tera template compilation is cached via existing `template_cache`, which should provide comparable performance
- Recommendation: Implement `cargo bench` benchmark before marking story fully complete

### Files Modified During Review

None - no modifications required.

### Gate Status

Gate: **CONCERNS** → `docs/qa/gates/TEA-RUST-029-tera-condition-evaluation.yml`

### Recommended Status

✓ Ready for Done (Conditionally)

The implementation is functionally complete and well-tested. The only outstanding item is AC-18 (performance benchmark), which is documented in the story as "not yet implemented."

**Recommendation**:
1. Mark story as Done for functional completeness
2. Create follow-up ticket for AC-18 benchmark implementation
3. Optionally create follow-up for example YAML file
