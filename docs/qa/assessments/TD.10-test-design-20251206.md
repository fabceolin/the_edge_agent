# Test Design: Story TD.10

Date: 2025-12-06
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 27
- Unit tests: 13 (48%)
- Integration tests: 11 (41%)
- E2E tests: 3 (11%)
- Priority distribution: P0: 10, P1: 12, P2: 5

## Test Scenarios by Acceptance Criteria

### AC1: save_checkpoint method (AC 1, 15, 16)

**Description:** `save_checkpoint(file_path, state, node, config)` method saves execution context to pickle file

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-UNIT-001 | Unit | P0 | Verify save_checkpoint creates pickle file at specified path | Pure file I/O, core functionality |
| TD.10-UNIT-002 | Unit | P0 | Verify checkpoint contains correct state dict | Data integrity validation |
| TD.10-UNIT-003 | Unit | P0 | Verify checkpoint contains correct node name | Data integrity validation |
| TD.10-UNIT-004 | Unit | P0 | Verify checkpoint contains correct config dict | Data integrity validation |
| TD.10-UNIT-005 | Unit | P1 | Verify checkpoint contains timestamp field | Metadata completeness |
| TD.10-UNIT-006 | Unit | P1 | Verify checkpoint contains version field "1.0" | Future compatibility |
| TD.10-UNIT-007 | Unit | P1 | Verify error handling for invalid file path (permission denied) | Error path validation |
| TD.10-UNIT-008 | Unit | P1 | Verify error handling for non-existent directory | Error path validation |

---

### AC2: load_checkpoint class method (AC 2, 15, 16)

**Description:** `load_checkpoint(file_path)` class method loads checkpoint and returns dict with state, node, config

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-UNIT-009 | Unit | P0 | Verify load_checkpoint returns dict with state, node, config keys | Core return structure |
| TD.10-UNIT-010 | Unit | P0 | Verify loaded state matches saved state | Data integrity round-trip |
| TD.10-UNIT-011 | Unit | P1 | Verify FileNotFoundError with clear message for missing file | Error message clarity |
| TD.10-UNIT-012 | Unit | P1 | Verify error handling for corrupt pickle file | Error path validation |
| TD.10-UNIT-013 | Unit | P2 | Verify error handling for incompatible checkpoint version | Future compatibility |

---

### AC3: resume_from_checkpoint method (AC 3, 12)

**Description:** `resume_from_checkpoint(file_path)` method resumes execution from a saved checkpoint

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-INT-001 | Integration | P0 | Verify resume_from_checkpoint loads checkpoint and continues execution | Multi-component flow |
| TD.10-INT-002 | Integration | P0 | Verify execution starts by RE-EXECUTING the saved node | Critical behavior per dev notes |
| TD.10-INT-003 | Integration | P1 | Verify config override (merge saved config with provided config) | Config handling logic |
| TD.10-INT-004 | Integration | P1 | Verify events yielded from resume point only | Event streaming correctness |

---

### AC4: invoke(checkpoint=path) parameter (AC 4, 12)

**Description:** `invoke(checkpoint=path)` parameter allows resuming directly when starting execution

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-INT-005 | Integration | P0 | Verify invoke with checkpoint parameter loads and resumes | API integration |
| TD.10-INT-006 | Integration | P1 | Verify invoke checkpoint resumes at correct node position | Execution correctness |
| TD.10-INT-007 | Integration | P2 | Verify invoke checkpoint parameter is optional (None by default) | API contract |

---

### AC5: stream(checkpoint=path) parameter (AC 5, 12)

**Description:** `stream(checkpoint=path)` parameter allows resuming directly when starting execution

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-INT-008 | Integration | P0 | Verify stream with checkpoint parameter loads and resumes | API integration |
| TD.10-INT-009 | Integration | P1 | Verify stream checkpoint yields intermediate states from resume point | Streaming correctness |

---

### AC6: Checkpoint data structure (AC 6)

**Description:** Checkpoint contains: state dict, current node name, config dict

*Covered by AC1/AC2 tests (TD.10-UNIT-002, 003, 004, 009, 010)*

---

### AC7-10: Auto-save at interrupts (AC 7, 8, 9, 10)

**Description:** `compile(checkpoint_dir=None)` enables auto-save at interrupt points

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-INT-010 | Integration | P0 | Verify compile accepts checkpoint_dir parameter | API contract |
| TD.10-INT-011 | Integration | P0 | Verify auto-save creates checkpoint before interrupt_before yields | Critical feature |
| TD.10-INT-012 | Integration | P0 | Verify auto-save creates checkpoint before interrupt_after yields | Critical feature |
| TD.10-INT-013 | Integration | P1 | Verify auto-save filename format: {node}_{timestamp}.pkl | Naming convention |
| TD.10-INT-014 | Integration | P2 | Verify checkpoint_dir=None disables auto-save (default behavior) | Backward compatibility |

---

### AC11: Backward compatibility (AC 11)

**Description:** Existing interrupt functionality continues to work unchanged when checkpoint_dir is None

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-E2E-001 | E2E | P0 | Verify all existing interrupt tests pass unchanged | Regression prevention |

---

### AC12: Resume node positioning (AC 12)

*Covered by AC3/AC4/AC5 tests (TD.10-INT-002, 006)*

---

### AC13: Parallel flow checkpoints (AC 13)

**Description:** Checkpoint captures main thread state (parallel results if at fan-in)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-INT-015 | Integration | P1 | Verify checkpoint at fan-in node includes parallel_results in state | Parallel flow handling |
| TD.10-INT-016 | Integration | P2 | Verify warning log if checkpoint taken during active parallel flows | Observability |

---

### AC14: All existing tests pass (AC 14)

**Description:** All existing tests pass without modification

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-E2E-002 | E2E | P0 | Run full test suite and verify no regressions | Regression prevention |

---

### AC17: New functionality covered by unit tests (AC 17)

*Covered by all unit and integration tests above*

---

### AC18: Docstrings (AC 18)

**Description:** Docstrings document checkpoint parameters and methods

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.10-E2E-003 | E2E | P2 | Verify docstrings exist for save_checkpoint, load_checkpoint, resume_from_checkpoint | Documentation validation |

---

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| Resume from wrong node position could cause unexpected behavior | TD.10-INT-002, TD.10-INT-006 |
| Corrupt checkpoint files cause crashes | TD.10-UNIT-012 |
| Backward compatibility broken | TD.10-E2E-001, TD.10-INT-014 |
| Parallel flow state lost at checkpoint | TD.10-INT-015, TD.10-INT-016 |
| Auto-save performance impact | Not directly tested (monitoring recommended) |

---

## Recommended Execution Order

1. **P0 Unit tests (fail fast)**
   - TD.10-UNIT-001 through TD.10-UNIT-004, TD.10-UNIT-009, TD.10-UNIT-010

2. **P0 Integration tests**
   - TD.10-INT-001, TD.10-INT-002, TD.10-INT-005, TD.10-INT-008, TD.10-INT-010, TD.10-INT-011, TD.10-INT-012

3. **P0 E2E tests**
   - TD.10-E2E-001, TD.10-E2E-002

4. **P1 tests in order**
   - TD.10-UNIT-005 through TD.10-UNIT-008, TD.10-UNIT-011, TD.10-UNIT-012
   - TD.10-INT-003, TD.10-INT-004, TD.10-INT-006, TD.10-INT-009, TD.10-INT-013, TD.10-INT-015

5. **P2+ as time permits**
   - TD.10-UNIT-013, TD.10-INT-007, TD.10-INT-014, TD.10-INT-016, TD.10-E2E-003

---

## Test Implementation Notes

### Test Fixtures Required

```python
@pytest.fixture
def simple_graph():
    """Create a simple 3-node graph for checkpoint testing."""
    graph = StateGraph({"value": int, "step": str})
    graph.add_node("step1", run=lambda state: {"step": "1", "value": state["value"] + 1})
    graph.add_node("step2", run=lambda state: {"step": "2", "value": state["value"] + 1})
    graph.add_node("step3", run=lambda state: {"step": "3", "value": state["value"] + 1})
    graph.set_entry_point("step1")
    graph.add_edge("step1", "step2")
    graph.add_edge("step2", "step3")
    graph.set_finish_point("step3")
    return graph

@pytest.fixture
def temp_checkpoint_dir(tmp_path):
    """Provide a temporary directory for checkpoint files."""
    return tmp_path / "checkpoints"
```

### Mocking Requirements

- Mock `time.time()` for deterministic filename tests (TD.10-INT-013)
- Use `tempfile.TemporaryDirectory()` for file I/O tests
- Mock `pickle.dump/load` for corrupt file tests (TD.10-UNIT-012)

### Key Test Assertions

1. **Checkpoint structure validation:**
   ```python
   checkpoint = StateGraph.load_checkpoint(filepath)
   assert "state" in checkpoint
   assert "node" in checkpoint
   assert "config" in checkpoint
   assert "timestamp" in checkpoint
   assert checkpoint["version"] == "1.0"
   ```

2. **Resume node position validation:**
   ```python
   # Save at step2, resume should re-execute step2
   events = list(graph.resume_from_checkpoint(checkpoint_path))
   node_sequence = [e["node"] for e in events if e.get("type") == "state"]
   assert node_sequence[0] == "step2"  # Re-executes saved node
   ```

3. **Auto-save at interrupt validation:**
   ```python
   compiled = graph.compile(interrupt_before=["step2"], checkpoint_dir=str(tmp_path))
   events = list(compiled.invoke({"value": 0}))
   checkpoint_files = list(tmp_path.glob("step2_*.pkl"))
   assert len(checkpoint_files) == 1
   ```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
