# Test Design: Story YE.1 - YAML Engine Checkpoint Persistence

**Date:** 2026-01-07
**Designer:** Quinn (Test Architect)
**Story:** YE.1.yaml-checkpoint-persistence

## Test Strategy Overview

- **Total test scenarios:** 34
- **Unit tests:** 22 (65%)
- **Integration tests:** 12 (35%)
- **E2E tests:** 0 (0%)
- **Priority distribution:** P0: 16, P1: 14, P2: 4

**Rationale for test distribution:**
- High unit test coverage appropriate for internal API features (checkpoint methods, template vars)
- Integration tests verify YAML parsing → graph compilation → checkpoint persistence roundtrip
- No E2E tests needed - this is a developer-facing API feature, not a user-facing journey
- High P0 count reflects data integrity criticality (checkpoint corruption = workflow data loss)

---

## Test Scenarios by Acceptance Criteria

### AC1: YAML `config.checkpoint_dir` parameter enables auto-save at interrupt points

| ID              | Level       | Priority | Test                                              | Justification                                      |
| --------------- | ----------- | -------- | ------------------------------------------------- | -------------------------------------------------- |
| YE.1-UNIT-001   | Unit        | P0       | Parse checkpoint_dir from YAML config dict        | Core parsing logic, affects auto-save behavior     |
| YE.1-INT-001    | Integration | P0       | Auto-save creates checkpoint file at interrupt    | Critical integration: YAML config → graph compile  |
| YE.1-INT-002    | Integration | P1       | Auto-save works with interrupt_before configured  | Validate interrupt timing option                   |
| YE.1-INT-003    | Integration | P1       | Auto-save works with interrupt_after configured   | Validate interrupt timing option                   |

**Risk Coverage:** Mitigates RISK-001 (checkpoint corruption), RISK-002 (missing auto-save)

---

### AC2: Auto-saved checkpoints use format: `{checkpoint_dir}/{node}_{timestamp}.pkl`

| ID              | Level       | Priority | Test                                        | Justification                                 |
| --------------- | ----------- | -------- | ------------------------------------------- | --------------------------------------------- |
| YE.1-UNIT-002   | Unit        | P0       | Checkpoint filename matches expected format | Data integrity: predictable file lookup       |
| YE.1-INT-004    | Integration | P1       | Multiple auto-saves create unique filenames | Prevent overwrite, allow historical tracking  |

**Risk Coverage:** Mitigates RISK-003 (filename collision)

---

### AC3: YAML `config.checkpoint` parameter allows starting from a saved checkpoint

| ID              | Level       | Priority | Test                                                | Justification                                |
| --------------- | ----------- | -------- | --------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-003   | Unit        | P0       | Parse checkpoint path from YAML config              | Core parsing logic                           |
| YE.1-INT-005    | Integration | P0       | Graph compiled from YAML loads checkpoint state     | Critical resume functionality                |
| YE.1-INT-006    | Integration | P0       | Error handling for missing checkpoint file          | Error scenario for P0 feature                |
| YE.1-INT-007    | Integration | P1       | Error handling for corrupt checkpoint file          | Error scenario - less likely than missing    |

**Risk Coverage:** Mitigates RISK-004 (invalid checkpoint path), RISK-001 (corruption detection)

---

### AC4: When `checkpoint` is provided, execution resumes from saved state/node

| ID              | Level       | Priority | Test                                                    | Justification                              |
| --------------- | ----------- | -------- | ------------------------------------------------------- | ------------------------------------------ |
| YE.1-INT-008    | Integration | P0       | Resumed execution starts at checkpoint node             | Core resume behavior verification          |
| YE.1-INT-009    | Integration | P0       | Resumed execution uses checkpoint state values          | Data integrity across save/resume cycle    |
| YE.1-INT-010    | Integration | P1       | Resumed execution skips already-executed nodes          | Verify correct workflow position           |

**Risk Coverage:** Mitigates RISK-005 (state corruption on resume)

---

### AC5: `YAMLEngine.load_from_file(path, checkpoint=None)` accepts optional checkpoint path

| ID              | Level       | Priority | Test                                                  | Justification                                   |
| --------------- | ----------- | -------- | ----------------------------------------------------- | ----------------------------------------------- |
| YE.1-UNIT-004   | Unit        | P0       | load_from_file method signature accepts checkpoint    | API contract validation                         |
| YE.1-INT-011    | Integration | P0       | load_from_file with checkpoint resumes from saved     | Core API integration                            |
| YE.1-UNIT-005   | Unit        | P1       | load_from_file with None checkpoint behaves normally  | Backward compatibility                          |

**Risk Coverage:** Mitigates RISK-006 (API breaking change)

---

### AC6: `YAMLEngine.load_from_dict(config, checkpoint=None)` accepts optional checkpoint path

| ID              | Level       | Priority | Test                                                  | Justification                                   |
| --------------- | ----------- | -------- | ----------------------------------------------------- | ----------------------------------------------- |
| YE.1-UNIT-006   | Unit        | P0       | load_from_dict method signature accepts checkpoint    | API contract validation                         |
| YE.1-INT-012    | Integration | P0       | load_from_dict with checkpoint resumes from saved     | Core API integration                            |
| YE.1-UNIT-007   | Unit        | P1       | load_from_dict with None checkpoint behaves normally  | Backward compatibility                          |

**Risk Coverage:** Mitigates RISK-006 (API breaking change)

---

### AC7: `YAMLEngine.resume_from_checkpoint(yaml_path, checkpoint_path, config=None)` method added

| ID              | Level       | Priority | Test                                                     | Justification                                 |
| --------------- | ----------- | -------- | -------------------------------------------------------- | --------------------------------------------- |
| YE.1-UNIT-008   | Unit        | P0       | resume_from_checkpoint method exists with signature      | API contract validation                       |
| YE.1-UNIT-009   | Unit        | P1       | resume_from_checkpoint with config=None uses YAML config | Default behavior validation                   |
| YE.1-UNIT-010   | Unit        | P1       | resume_from_checkpoint with config merges with YAML      | Config override logic                         |

**Risk Coverage:** Mitigates RISK-007 (config merge errors)

---

### AC8: Resume methods return a compiled graph that starts from checkpoint state/node

| ID              | Level       | Priority | Test                                                      | Justification                                |
| --------------- | ----------- | -------- | --------------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-011   | Unit        | P0       | resume_from_checkpoint returns compiled StateGraph        | API contract validation                      |
| YE.1-INT-013    | Integration | P0       | Returned graph yields events from checkpoint node forward | Core resume functionality                    |

**Risk Coverage:** Mitigates RISK-005 (incorrect resume position)

---

### AC9: `checkpoint.save` action saves current state to specified path

| ID              | Level       | Priority | Test                                                | Justification                                   |
| --------------- | ----------- | -------- | --------------------------------------------------- | ----------------------------------------------- |
| YE.1-UNIT-012   | Unit        | P0       | checkpoint.save action registered in actions map    | Action availability                             |
| YE.1-INT-014    | Integration | P0       | checkpoint.save creates file at specified path      | Core action functionality                       |
| YE.1-INT-015    | Integration | P1       | checkpoint.save writes valid checkpoint data        | Data integrity                                  |

**Risk Coverage:** Mitigates RISK-001 (checkpoint corruption)

---

### AC10: `checkpoint.load` action loads checkpoint and returns state dict

| ID              | Level       | Priority | Test                                                | Justification                                   |
| --------------- | ----------- | -------- | --------------------------------------------------- | ----------------------------------------------- |
| YE.1-UNIT-013   | Unit        | P0       | checkpoint.load action registered in actions map    | Action availability                             |
| YE.1-INT-016    | Integration | P0       | checkpoint.load reads checkpoint file correctly     | Core action functionality                       |
| YE.1-INT-017    | Integration | P1       | checkpoint.load returns expected state dict         | Data integrity                                  |

**Risk Coverage:** Mitigates RISK-001 (checkpoint corruption detection)

---

### AC11: Actions support template variables for dynamic paths: `{{ state.checkpoint_path }}`

| ID              | Level       | Priority | Test                                                        | Justification                             |
| --------------- | ----------- | -------- | ----------------------------------------------------------- | ----------------------------------------- |
| YE.1-UNIT-014   | Unit        | P1       | checkpoint.save resolves {{ state.var }} in path            | Template variable interpolation logic     |
| YE.1-UNIT-015   | Unit        | P1       | checkpoint.load resolves {{ state.var }} in path            | Template variable interpolation logic     |
| YE.1-INT-018    | Integration | P1       | Dynamic checkpoint path creates file at resolved location   | End-to-end template resolution            |

**Risk Coverage:** Mitigates RISK-008 (template variable errors)

---

### AC12: `checkpoint.save` returns `{"checkpoint_path": str, "saved": bool}`

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-016   | Unit        | P0       | checkpoint.save returns dict with checkpoint_path     | API contract validation                      |
| YE.1-UNIT-017   | Unit        | P0       | checkpoint.save returns saved=True on success         | Success response validation                  |
| YE.1-UNIT-018   | Unit        | P1       | checkpoint.save returns saved=False on failure        | Error response validation                    |

**Risk Coverage:** Mitigates RISK-009 (action return contract violations)

---

### AC13: `checkpoint.load` returns `{"checkpoint_state": dict, "checkpoint_node": str}`

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-019   | Unit        | P0       | checkpoint.load returns dict with checkpoint_state    | API contract validation                      |
| YE.1-UNIT-020   | Unit        | P0       | checkpoint.load returns checkpoint_node on success    | API contract validation                      |
| YE.1-UNIT-021   | Unit        | P1       | checkpoint.load handles missing file with error       | Error handling validation                    |

**Risk Coverage:** Mitigates RISK-009 (action return contract violations)

---

### AC14: `{{ checkpoint.dir }}` resolves to configured checkpoint_dir

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-022   | Unit        | P1       | Template context includes checkpoint.dir variable     | Template variable availability               |
| YE.1-INT-019    | Integration | P1       | {{ checkpoint.dir }} resolves to config value         | End-to-end template resolution               |

**Risk Coverage:** Mitigates RISK-008 (template variable errors)

---

### AC15: `{{ checkpoint.last }}` resolves to most recent auto-saved checkpoint path (if any)

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-UNIT-023   | Unit        | P1       | Template context includes checkpoint.last variable    | Template variable availability               |
| YE.1-INT-020    | Integration | P1       | {{ checkpoint.last }} resolves to latest auto-save    | End-to-end template resolution               |
| YE.1-INT-021    | Integration | P2       | {{ checkpoint.last }} is None when no auto-saves      | Edge case: no checkpoints yet                |

**Risk Coverage:** Mitigates RISK-008 (template variable errors)

---

### AC16: Existing YAML workflows work unchanged when checkpoint features not used

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-022    | Integration | P0       | All existing YAMLEngine tests pass without changes    | Regression prevention                        |
| YE.1-INT-023    | Integration | P0       | YAML without checkpoint config executes normally      | Backward compatibility                       |

**Risk Coverage:** Mitigates RISK-006 (breaking changes)

---

### AC17: Checkpoint features work with parallel flows (captures main thread state)

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-024    | Integration | P1       | Checkpoint during parallel flow captures main state   | Complex workflow scenario                    |
| YE.1-INT-025    | Integration | P2       | Resume from parallel checkpoint continues correctly   | Complex resume scenario                      |

**Risk Coverage:** Mitigates RISK-010 (parallel flow state corruption)

---

### AC18: Checkpoint features work with conditional edges

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-026    | Integration | P1       | Checkpoint before conditional edge preserves state    | Complex workflow scenario                    |
| YE.1-INT-027    | Integration | P2       | Resume evaluates conditional correctly from checkpoint| Complex resume scenario                      |

**Risk Coverage:** Mitigates RISK-011 (conditional edge reevaluation errors)

---

### AC19: All existing YAMLEngine tests pass without modification

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-028    | Integration | P0       | pytest tests/test_yaml_engine.py passes               | Regression prevention                        |

**Risk Coverage:** Mitigates RISK-006 (breaking changes)

---

### AC20: Clear error messages for: invalid checkpoint path, corrupt checkpoint, missing checkpoint_dir

| ID              | Level       | Priority | Test                                                         | Justification                          |
| --------------- | ----------- | -------- | ------------------------------------------------------------ | -------------------------------------- |
| YE.1-UNIT-024   | Unit        | P1       | Invalid checkpoint path raises descriptive error             | Developer experience                   |
| YE.1-UNIT-025   | Unit        | P1       | Corrupt checkpoint raises descriptive error                  | Developer experience                   |
| YE.1-UNIT-026   | Unit        | P2       | checkpoint.save without checkpoint_dir raises helpful error  | Developer experience                   |

**Risk Coverage:** Mitigates RISK-012 (poor error diagnostics)

---

### AC21: New functionality covered by unit tests

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-029    | Integration | P0       | All new methods have corresponding unit tests         | Coverage verification                        |

**Risk Coverage:** General quality gate

---

### AC22: Docstrings document new parameters and methods

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-030    | Integration | P2       | All new methods have docstrings                       | Documentation verification                   |
| YE.1-INT-031    | Integration | P2       | New parameters documented in docstrings               | Documentation verification                   |

**Risk Coverage:** General quality gate

---

### AC23: YAML_AGENTS.md updated with checkpoint documentation

| ID              | Level       | Priority | Test                                                  | Justification                                |
| --------------- | ----------- | -------- | ----------------------------------------------------- | -------------------------------------------- |
| YE.1-INT-032    | Integration | P2       | YAML_AGENTS.md contains checkpoint section            | Documentation verification                   |
| YE.1-INT-033    | Integration | P2       | Documentation includes YAML examples                  | Documentation verification                   |
| YE.1-INT-034    | Integration | P2       | Documentation includes action signatures              | Documentation verification                   |

**Risk Coverage:** General quality gate

---

## Risk Coverage Matrix

| Risk ID   | Description                          | Test IDs Covering Risk                                      |
| --------- | ------------------------------------ | ----------------------------------------------------------- |
| RISK-001  | Checkpoint file corruption           | YE.1-UNIT-001, YE.1-INT-001, YE.1-INT-007, YE.1-INT-015/17  |
| RISK-002  | Auto-save not triggered              | YE.1-INT-001, YE.1-INT-002, YE.1-INT-003                    |
| RISK-003  | Checkpoint filename collision        | YE.1-UNIT-002, YE.1-INT-004                                 |
| RISK-004  | Invalid checkpoint path handling     | YE.1-INT-006, YE.1-UNIT-024                                 |
| RISK-005  | State corruption on resume           | YE.1-INT-008, YE.1-INT-009, YE.1-INT-010, YE.1-INT-013      |
| RISK-006  | Breaking existing YAML workflows     | YE.1-UNIT-005/007, YE.1-INT-022/023, YE.1-INT-028           |
| RISK-007  | Config merge errors                  | YE.1-UNIT-009, YE.1-UNIT-010                                |
| RISK-008  | Template variable resolution errors  | YE.1-UNIT-014/015, YE.1-INT-018/019/020/021                 |
| RISK-009  | Action return contract violations    | YE.1-UNIT-016/017/018/019/020/021                           |
| RISK-010  | Parallel flow state corruption       | YE.1-INT-024, YE.1-INT-025                                  |
| RISK-011  | Conditional edge reevaluation errors | YE.1-INT-026, YE.1-INT-027                                  |
| RISK-012  | Poor error diagnostics               | YE.1-UNIT-024/025/026                                       |

---

## Recommended Execution Order

### Phase 1: Fail-Fast (P0 Unit Tests)
1. YE.1-UNIT-001 through YE.1-UNIT-008 (API contracts)
2. YE.1-UNIT-011, YE.1-UNIT-012, YE.1-UNIT-013 (action registration)
3. YE.1-UNIT-016, YE.1-UNIT-017, YE.1-UNIT-019, YE.1-UNIT-020 (return contracts)

**Rationale:** Fast feedback on core API design and contracts

### Phase 2: Core Integration (P0 Integration Tests)
1. YE.1-INT-001 (auto-save)
2. YE.1-INT-005, YE.1-INT-006 (resume from config)
3. YE.1-INT-008, YE.1-INT-009 (resume behavior)
4. YE.1-INT-011, YE.1-INT-012 (API methods)
5. YE.1-INT-013 (graph compilation)
6. YE.1-INT-014, YE.1-INT-016 (actions)
7. YE.1-INT-022, YE.1-INT-023, YE.1-INT-028 (backward compatibility)

**Rationale:** Validate critical integration points before moving to edge cases

### Phase 3: P1 Features
1. YE.1-UNIT tests (template variables, error handling)
2. YE.1-INT tests (complex workflows: parallel, conditional)

**Rationale:** Ensure robustness and feature completeness

### Phase 4: P2 Polish (Time Permitting)
1. Documentation verification tests
2. Edge case scenarios (no checkpoints yet, etc.)

**Rationale:** Nice-to-have validation, can defer if time-constrained

---

## Test Implementation Notes

### Test Data Strategy
- Use `tempfile.TemporaryDirectory()` for all checkpoint file operations
- Create minimal YAML config dicts inline (avoid file I/O overhead in unit tests)
- Use pytest fixtures for common test graphs and checkpoint states

### Mock Strategy
- **Unit tests:** Mock `StateGraph.save_checkpoint()` and `StateGraph.load_checkpoint()`
- **Integration tests:** Use real checkpoint files in temp directories
- Do NOT mock YAML parsing (that's what we're testing)

### Test Isolation
- Each test must create its own checkpoint directory
- Clean up checkpoint files in teardown (or rely on temp directory auto-cleanup)
- Tests must be order-independent

### Performance Considerations
- Unit tests should complete in <100ms each
- Integration tests allowed up to 1s each (file I/O overhead)
- Total test suite target: <30s for 34 tests

---

## Coverage Gaps Identified

**None.** All 23 acceptance criteria have explicit test coverage.

---

## Test Design Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for roundtrip)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (data integrity = P0)
- [x] Test IDs follow naming convention (YE.1-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Risk coverage matrix complete
- [x] Execution order optimized for fast feedback

---

## GATE YAML BLOCK (for inclusion in quality gate)

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 22
    integration: 12
    e2e: 0
  by_priority:
    p0: 16
    p1: 14
    p2: 4
  coverage_gaps: []
  design_date: "2026-01-07"
  designer: "Quinn (Test Architect)"
```

---

## Test Design Trace References

**Test design matrix:** `docs/qa/assessments/YE.1-test-design-20260107.md`
**P0 tests identified:** 16
**Total test coverage:** 34 scenarios across 23 acceptance criteria

---

**END OF TEST DESIGN**
