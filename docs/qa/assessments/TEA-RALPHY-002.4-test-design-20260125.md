# Test Design: Story TEA-RALPHY-002.4

**Date:** 2026-01-25
**Designer:** Quinn (Test Architect)
**Story:** BMad Workflow Status Detection Fix

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 14 (50%)
- **Integration tests:** 10 (36%)
- **E2E tests:** 4 (14%)
- **Priority distribution:** P0: 6, P1: 12, P2: 8, P3: 2

### Test Level Rationale

This story is primarily about internal workflow tooling (helper functions and detection logic), favoring unit tests for:
1. Pure regex-based status extraction
2. File existence checks
3. Section parsing

Integration tests validate:
1. End-to-end detection flows (marker + artifact fallback)
2. Actual file system interactions
3. Workflow summary output

E2E tests cover:
1. Full pipeline execution verification
2. Backward compatibility validation

---

## Test Scenarios by Acceptance Criteria

### AC1: Dev phase shows "Completed" when story file status contains "Ready for Review" or "Review"

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-001 | Unit | P0 | `check_story_status` returns True for "Ready for Review" | Core regex logic validation |
| 002.4-UNIT-002 | Unit | P0 | `check_story_status` returns True for "Review" | Partial match requirement |
| 002.4-UNIT-003 | Unit | P1 | `check_story_status` returns False for "Draft" | Negative case |
| 002.4-UNIT-004 | Unit | P1 | `check_story_status` is case-insensitive | Risk mitigation (TECH-001) |
| 002.4-UNIT-005 | Unit | P2 | `check_story_status` handles missing ## Status section | Edge case |
| 002.4-INT-001 | Integration | P1 | Dev detection reads actual story file and extracts status | File I/O integration |

### AC2: QA phase shows "Completed" when gate file exists in `docs/qa/gates/{story-id}*.yml`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-006 | Unit | P0 | `check_artifact_file` returns True for existing gate file | Core glob logic |
| 002.4-UNIT-007 | Unit | P1 | `check_artifact_file` returns False when no match | Negative case |
| 002.4-UNIT-008 | Unit | P2 | `check_artifact_file` handles multiple matching files | Edge case (first match wins) |
| 002.4-INT-002 | Integration | P0 | QA detection finds gate file with various naming patterns | Pattern coverage (TECH-003 mitigation) |

### AC3: SM phase shows "Completed" when story file status is updated to final state

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-009 | Unit | P0 | `check_story_status` returns True for "Done" | Final state detection |
| 002.4-UNIT-010 | Unit | P1 | `check_story_status` returns True for "Changes Required" | Final state detection |
| 002.4-UNIT-011 | Unit | P1 | `check_story_status` returns True for "Done (Waived)" | Final state with qualifier |
| 002.4-INT-003 | Integration | P1 | SM detection reads story file and identifies final status | File I/O integration |

### AC4: Gate status correctly extracted from gate file or story QA Results section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-004 | Integration | P0 | Gate status extraction parses YAML gate file correctly | Critical data extraction |
| 002.4-INT-005 | Integration | P1 | Gate status fallback to story QA Results section | Fallback mechanism |

### AC5: QA Risk Profile shows "Completed" (story section OR assessment file)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-012 | Unit | P1 | `check_story_section` finds "## QA Notes - Risk Profile" | Section detection |
| 002.4-INT-006 | Integration | P1 | Risk Profile detection checks story section first, then assessment file | Dual-source fallback |

### AC6: QA NFR Assessment shows "Completed" (story section OR assessment file)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-013 | Unit | P1 | `check_story_section` finds "## QA Notes - NFR Assessment" | Section detection |
| 002.4-INT-007 | Integration | P1 | NFR Assessment detection checks story section first, then assessment file | Dual-source fallback |

### AC7: QA Test Design shows "Completed" (story section OR assessment file)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-UNIT-014 | Unit | P1 | `check_story_section` finds "## QA Notes - Test Design" | Section detection |
| 002.4-INT-008 | Integration | P1 | Test Design detection checks story section first, then assessment file | Dual-source fallback |

### AC8: SM Story Checklist shows "Completed" (section AND status)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-009 | Integration | P1 | SM Checklist requires both "## SM Validation" section AND correct status | Compound condition |

### AC9: Marker-based detection remains as first-priority check (backward compatible)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-E2E-001 | E2E | P0 | Markers detected when present, skipping artifact detection | Backward compatibility |
| 002.4-E2E-002 | E2E | P1 | Artifact detection triggers only when markers absent | Fallback behavior |

### AC10: Artifact-based detection serves as fallback when markers not found

(Covered by AC9 tests)

### AC11: No changes to agent definitions or slash commands required

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-INT-010 | Integration | P2 | Agent definitions unchanged after implementation | No modification verification |

### AC12: Summary output clearly indicates detection method used (marker vs artifact)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-E2E-003 | E2E | P2 | Summary shows "(via marker)" when marker detected | Output format |
| 002.4-E2E-004 | E2E | P2 | Summary shows "(via artifact)" when artifact detected | Output format |

### AC13: Existing `check_qa_gate` logic preserved in development workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-E2E-005 | E2E | P3 | Development workflow `check_qa_gate` logic unchanged | Regression prevention |

### AC14: No regression in pipeline execution flow for either workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.4-E2E-006 | E2E | P3 | Both workflows execute without errors on existing stories | Full regression |

---

## Risk Coverage

| Risk ID | Description | Test IDs Mitigating |
|---------|-------------|---------------------|
| TECH-001 | Regex pattern mismatch in status extraction | 002.4-UNIT-001 through 004 |
| OPS-002 | Backward compatibility - marker detection altered | 002.4-E2E-001, 002.4-E2E-002 |
| DATA-001 | Story file parsing fails on malformed markdown | 002.4-UNIT-005 |
| TECH-002 | File path resolution fails for edge cases | 002.4-INT-001, 002.4-INT-006-008 |
| TECH-003 | Glob pattern doesn't match all gate file variants | 002.4-INT-002 |
| OPS-001 | Summary output format breaks terminal rendering | 002.4-E2E-003, 002.4-E2E-004 |

---

## Test Data Requirements

### Story Files for Testing

1. **`test_story_draft.md`** - Story with `## Status\n\nDraft`
2. **`test_story_ready_review.md`** - Story with `## Status\n\nReady for Review`
3. **`test_story_done.md`** - Story with `## Status\n\nDone`
4. **`test_story_changes_required.md`** - Story with `## Status\n\nChanges Required`
5. **`test_story_with_qa_notes.md`** - Story with all QA Notes sections populated
6. **`test_story_malformed.md`** - Story with no `## Status` section
7. **`test_story_mixed_case.md`** - Story with `## Status\n\nready FOR review`

### Gate Files for Testing

1. **`docs/qa/gates/TEST-001.1-test-story.yml`** - Valid gate file with PASS status
2. **`docs/qa/gates/TEST-001.2-test-story.yml`** - Valid gate file with CONCERNS status
3. **`docs/qa/gates/TEST-001.3-test-story.yml`** - Valid gate file with FAIL status

### Assessment Files for Testing

1. **`docs/qa/assessments/TEST-001.1-risk-20260125.md`** - Risk assessment file
2. **`docs/qa/assessments/TEST-001.1-nfr-20260125.md`** - NFR assessment file
3. **`docs/qa/assessments/TEST-001.1-test-design-20260125.md`** - Test design file

---

## Test Environment Requirements

### Prerequisites

- Python 3.10+ with TEA installed (`pip install -e .[dev]`)
- Access to `examples/workflows/` directory
- Write access to `docs/qa/` for test fixtures
- Pytest with file fixtures support

### Environment Variables

None required (local file system operations only).

### Test Isolation

- Create temporary test directories for each test run
- Use `pytest` fixtures to setup/teardown test story files
- Avoid modifying production story files

---

## Recommended Execution Order

1. **P0 Unit tests** (002.4-UNIT-001, 002, 006, 009) - Fail fast on core logic
2. **P0 Integration tests** (002.4-INT-002, 004) - Validate file operations
3. **P0 E2E test** (002.4-E2E-001) - Backward compatibility
4. **P1 Unit tests** (003-005, 007, 010-014) - Secondary logic coverage
5. **P1 Integration tests** (001, 003, 005-009) - Secondary integrations
6. **P1 E2E test** (002.4-E2E-002) - Fallback behavior
7. **P2+ tests** as time permits

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 14
    integration: 10
    e2e: 4
  by_priority:
    p0: 6
    p1: 12
    p2: 8
    p3: 2
  coverage_gaps: []
  risk_coverage:
    tech_001: ["UNIT-001", "UNIT-002", "UNIT-003", "UNIT-004"]
    ops_002: ["E2E-001", "E2E-002"]
    data_001: ["UNIT-005"]
    tech_002: ["INT-001", "INT-006", "INT-007", "INT-008"]
    tech_003: ["INT-002"]
    ops_001: ["E2E-003", "E2E-004"]
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
