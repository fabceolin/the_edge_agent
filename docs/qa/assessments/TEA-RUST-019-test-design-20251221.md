# Test Design: Story TEA-RUST-019

**Date:** 2025-12-21
**Designer:** Quinn (Test Architect)
**Story:** Wire Checkpoint Path Updates in Executor

## Test Strategy Overview

- **Total test scenarios:** 12
- **Unit tests:** 2 (17%)
- **Integration tests:** 10 (83%)
- **E2E tests:** 0 (0%)
- **Priority distribution:** P0: 4, P1: 6, P2: 2

### Strategy Rationale

This story focuses on wiring existing components (Executor, YamlEngine, Checkpointer). The functionality is primarily about component interaction rather than algorithmic logic, which favors **integration testing** over unit tests. The `Checkpointer::save()` already returns `TeaResult<String>` (the path), so the main work is calling `set_last_checkpoint()` after saves.

Key testing considerations:
1. **Executor is the integration point** - Tests must verify the full flow: save checkpoint → update yaml_engine → template renders correctly
2. **Multiple save locations** - The story identifies 4 checkpoint save locations (2 in invoke(), 2 in StreamExecutor)
3. **Mutability challenge** - The story notes `yaml_engine` may need `RefCell` for interior mutability; tests should validate this works correctly

## Test Scenarios by Acceptance Criteria

### AC-1: Checkpoint save updates yaml_engine.last_checkpoint()

> GIVEN workflow execution with a checkpointer, WHEN a checkpoint is saved, THEN `yaml_engine.last_checkpoint()` returns the saved checkpoint path

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-019-INT-001 | Integration | P0 | Verify `last_checkpoint()` returns saved path after invoke() checkpoint save | Critical path: validates core wiring works in standard execution flow |
| TEA-RUST-019-INT-002 | Integration | P0 | Verify `last_checkpoint()` returns saved path after StreamExecutor checkpoint save | Critical path: streaming execution is separate code path that must also be wired |
| TEA-RUST-019-INT-003 | Integration | P1 | Verify checkpoint path includes correct directory and filename format | Validates the path returned is meaningful and correctly formatted |

### AC-2: Template {{ checkpoint.last }} renders the checkpoint path

> GIVEN a template using `{{ checkpoint.last }}`, WHEN rendered after a checkpoint save, THEN it contains the path of the most recent checkpoint

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-019-INT-004 | Integration | P0 | Render `{{ checkpoint.last }}` after checkpoint save returns correct path | Critical path: end-to-end validation of the user-facing feature |
| TEA-RUST-019-INT-005 | Integration | P1 | Workflow with node that uses `{{ checkpoint.last }}` in run block | Real-world usage pattern: workflow step accessing last checkpoint path |
| TEA-RUST-019-UNIT-001 | Unit | P1 | Verify checkpoint context structure in template rendering | Validates `checkpoint.dir` and `checkpoint.last` available together |

### AC-3: Multiple checkpoint saves update correctly

> GIVEN multiple checkpoint saves during execution, WHEN `{{ checkpoint.last }}` is rendered, THEN it always reflects the most recent save (not stale)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-019-INT-006 | Integration | P0 | After 3 sequential checkpoint saves, `{{ checkpoint.last }}` shows only the last path | Critical: ensures no stale data from previous saves |
| TEA-RUST-019-INT-007 | Integration | P1 | Multi-node workflow with interrupts at each node shows correct path per step | Real-world pattern: workflow with multiple interrupt points |
| TEA-RUST-019-UNIT-002 | Unit | P2 | Verify `set_last_checkpoint()` overwrites previous value | Basic setter behavior validation |

### AC-4: No checkpointer configured - graceful handling

> GIVEN no checkpointer configured, WHEN executing a workflow, THEN `{{ checkpoint.last }}` remains empty (no errors)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-019-INT-008 | Integration | P1 | Execute workflow without checkpointer - `{{ checkpoint.last }}` is empty string | Validates graceful degradation |
| TEA-RUST-019-INT-009 | Integration | P1 | Execute workflow without checkpointer - no errors in execution | Ensures missing checkpointer doesn't cause failures |
| TEA-RUST-019-INT-010 | Integration | P2 | Template conditionally checks `checkpoint.last` before use | Real-world pattern: `{% if checkpoint.last %}...{% endif %}` |

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| RefCell/interior mutability incorrectly implemented | INT-001, INT-002, INT-006 (require mutable access during execution) |
| StreamExecutor code path not wired | INT-002 specifically targets streaming execution |
| Path format incorrect (missing directory, wrong separator) | INT-003, INT-004 |
| Stale checkpoint path from previous execution | INT-006, INT-007 |
| Breaking change to Checkpointer API | All tests using Checkpointer will fail if signature changes |

## Existing Test Coverage Analysis

**Current tests in `rust/tests/test_checkpoint.rs`:**
- Cover Checkpoint creation, serialization, persistence
- Cover MemoryCheckpointer and FileCheckpointer operations
- Cover edge cases (empty state, unicode, corruption)
- **Do NOT cover:** Executor integration with checkpoint path updates

**Current tests in `rust/src/engine/yaml.rs`:**
- `test_render_template_with_last_checkpoint()` - Validates template rendering with manually-set path
- `test_last_checkpoint_updates_dynamically()` - Validates setter/getter behavior
- **Gap:** No tests validate that Executor calls `set_last_checkpoint()` after saves

This story fills the gap between checkpoint saving and template accessibility.

## Implementation Notes for Developers

### Test Location
Add new integration tests to: `rust/tests/test_checkpoint.rs`

### Sample Test Pattern

```rust
#[test]
fn test_invoke_updates_last_checkpoint_after_save() {
    let temp_dir = tempdir().unwrap();
    let checkpointer = Arc::new(FileCheckpointer::json(temp_dir.path()).unwrap());

    // Create a simple workflow with an interrupt point
    let yaml = r#"
    name: checkpoint-test
    state_schema:
      value: int
    nodes:
      - name: step1
        run: |
          return {"value": state["value"] + 1}
    edges:
      - from: __start__
        to: step1
      - from: step1
        to: __end__
    interrupt_after:
      - step1
    "#;

    let mut engine = YamlEngine::new();
    let workflow = engine.load_from_yaml(yaml).unwrap();

    let executor = Executor::new(workflow, engine);
    let options = ExecuteOptions {
        checkpointer: Some(checkpointer),
        ..Default::default()
    };

    let events = executor.invoke(json!({"value": 0}), options).unwrap();

    // Verify last_checkpoint was updated
    assert!(executor.yaml_engine().last_checkpoint().is_some());

    // Verify template renders correctly
    let rendered = executor.yaml_engine()
        .render_template("{{ checkpoint.last }}", &json!({}), &HashMap::new())
        .unwrap();
    assert!(rendered.contains(temp_dir.path().to_str().unwrap()));
}
```

### Mutability Pattern

The story notes `yaml_engine` may need `RefCell<YamlEngine>` for interior mutability. Tests should account for this:

```rust
// If RefCell is used:
let engine_ref = executor.yaml_engine().borrow();
assert!(engine_ref.last_checkpoint().is_some());

// Or if mutable reference is exposed:
assert!(executor.yaml_engine_mut().last_checkpoint().is_some());
```

## Recommended Execution Order

1. **P0 Unit tests** - Fast validation of core behavior
2. **P0 Integration tests** (INT-001, INT-002, INT-004, INT-006) - Critical wiring validation
3. **P1 Integration tests** - Extended coverage of real-world patterns
4. **P2 tests** - Edge cases and defensive scenarios

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (integration for component wiring)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 12
  by_level:
    unit: 2
    integration: 10
    e2e: 0
  by_priority:
    p0: 4
    p1: 6
    p2: 2
  coverage_gaps: []
  notes:
    - Integration-heavy by design; story is about component wiring not logic
    - All 4 checkpoint save locations in executor.rs should be tested
    - RefCell/interior mutability pattern needs validation
```

## Trace References

Test design matrix: `docs/qa/assessments/TEA-RUST-019-test-design-20251221.md`
P0 tests identified: 4
