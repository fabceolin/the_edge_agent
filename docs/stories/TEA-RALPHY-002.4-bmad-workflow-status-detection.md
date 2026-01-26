# Story TEA-RALPHY-002.4: BMad Workflow Status Detection Fix

## Status

Done

## Story

**As a** developer using BMad workflow pipelines,
**I want** both workflow files to correctly detect agent completion status,
**so that** the summary accurately reflects whether each phase succeeded or failed.

## Story Context

**Existing System Integration:**

- Integrates with:
  - `examples/workflows/bmad-story-development.yaml` (Dev → QA → SM pipeline)
  - `examples/workflows/bmad-story-validation.yaml` (Risk → NFR → Test Design → SM pipeline)
- Technology: TEA YAMLEngine, shell provider (Claude Code), BMad slash commands
- Follows pattern: Existing `check_qa_gate` artifact-based detection (lines 130-214)
- Touch points: Summary nodes in both workflow files

**Problem Statement:**

Both workflows rely on magic string markers that agents don't consistently output:

| Workflow | Markers Expected |
|----------|------------------|
| Development | `DEV_STORY_COMPLETED`, `QA_REVIEW_COMPLETED`, `SM_STATUS_UPDATED` |
| Validation | `RISK_PROFILE_COMPLETED`, `NFR_ASSESS_COMPLETED`, `TEST_DESIGN_COMPLETED`, `SM_CHECKLIST_COMPLETED` |

**Why markers fail:**

1. Claude Code agents execute work via tool calls (Edit, Write, Bash) not stdout
2. Agent persona definitions don't include these markers
3. Shell provider captures stdout which may be empty when work completes via tools

**Evidence from investigation:**

- Dev agent definition (`dev.md` line 68): completion is "set story status: 'Ready for Review'" - no marker
- QA agent (`review-story.md`): creates gate files, updates story sections - no marker defined
- Shell provider (`llm_actions.py` line 439): returns `stdout.strip()` - empty if no direct output

## Acceptance Criteria

### bmad-story-development.yaml

**Functional Requirements:**

1. Dev phase shows "Completed" when story file status contains "Ready for Review" or "Review"
2. QA phase shows "Completed" when gate file exists in `docs/qa/gates/{story-id}*.yml`
3. SM phase shows "Completed" when story file status is updated to final state (Done, Changes Required, etc.)
4. Gate status correctly extracted from gate file or story QA Results section

### bmad-story-validation.yaml

**Functional Requirements:**

5. QA Risk Profile shows "Completed" when story contains "## QA Notes - Risk Profile" section OR `docs/qa/assessments/{story-id}-risk-*.md` exists
6. QA NFR Assessment shows "Completed" when story contains "## QA Notes - NFR Assessment" section OR `docs/qa/assessments/{story-id}-nfr-*.md` exists
7. QA Test Design shows "Completed" when story contains "## QA Notes - Test Design" section OR `docs/qa/assessments/{story-id}-test-design-*.md` exists
8. SM Story Checklist shows "Completed" when story contains "## SM Validation" section AND status is "Ready for Development" or "Needs Revision"

### Integration Requirements (Both Workflows)

9. Existing marker-based detection remains as first-priority check (backward compatible)
10. Artifact-based detection serves as fallback when markers not found
11. No changes to agent definitions or slash commands required

### Quality Requirements

12. Summary output clearly indicates detection method used (marker vs artifact)
13. Existing `check_qa_gate` logic preserved in development workflow
14. No regression in pipeline execution flow for either workflow

## Tasks / Subtasks

### bmad-story-development.yaml

- [x] Task 1: Implement artifact-based Dev completion detection (AC: 1, 9)
  - [x] Add helper function to read story file and extract status
  - [x] Check for "Ready for Review" or "Review" status
  - [x] Fallback to marker check if status extraction fails

- [x] Task 2: Implement artifact-based QA completion detection (AC: 2, 4, 9)
  - [x] Add helper function to check gate file existence
  - [x] Extract gate status from gate file if exists
  - [x] Fallback to marker check if gate file not found

- [x] Task 3: Implement artifact-based SM completion detection (AC: 3, 9)
  - [x] Add helper function to detect story status changes post-QA
  - [x] Check for final statuses: "Done", "Changes Required", "Done (Waived)"
  - [x] Fallback to marker check if status detection fails

- [x] Task 4: Update development workflow summary node output format (AC: 12)
  - [x] Add detection method indicator to status output
  - [x] Show "(via artifact)" or "(via marker)" in status line
  - [x] Maintain existing box-drawing format

### bmad-story-validation.yaml

- [x] Task 5: Implement artifact-based Risk Profile completion detection (AC: 5, 9)
  - [x] Check story file for "## QA Notes - Risk Profile" section
  - [x] Check for `docs/qa/assessments/{story-id}-risk-*.md` file existence
  - [x] Fallback to marker check if neither found

- [x] Task 6: Implement artifact-based NFR Assessment completion detection (AC: 6, 9)
  - [x] Check story file for "## QA Notes - NFR Assessment" section
  - [x] Check for `docs/qa/assessments/{story-id}-nfr-*.md` file existence
  - [x] Fallback to marker check if neither found

- [x] Task 7: Implement artifact-based Test Design completion detection (AC: 7, 9)
  - [x] Check story file for "## QA Notes - Test Design" section
  - [x] Check for `docs/qa/assessments/{story-id}-test-design-*.md` file existence
  - [x] Fallback to marker check if neither found

- [x] Task 8: Implement artifact-based SM Checklist completion detection (AC: 8, 9)
  - [x] Check story file for "## SM Validation" section
  - [x] Verify status is "Ready for Development" or "Needs Revision"
  - [x] Fallback to marker check if section not found

- [x] Task 9: Update validation workflow summary node output format (AC: 12)
  - [x] Add detection method indicator to status output
  - [x] Maintain existing box-drawing format

### Testing

- [x] Task 10: Testing and validation (AC: 13, 14)
  - [x] Run development pipeline against existing story to verify detection works
  - [x] Run validation pipeline against existing story to verify detection works
  - [x] Verify no regression in pipeline flow for both workflows
  - [x] Confirm gate status extraction matches `check_qa_gate` logic

## Dev Notes

### File Locations

| File | Purpose |
|------|---------|
| `examples/workflows/bmad-story-development.yaml` | Development pipeline workflow to modify |
| `examples/workflows/bmad-story-validation.yaml` | Validation pipeline workflow to modify |
| `python/src/the_edge_agent/actions/llm_actions.py` | Reference for shell provider behavior (read-only) |
| `.bmad-core/agents/dev.md` | Dev agent definition (read-only, for reference) |
| `.bmad-core/tasks/review-story.md` | QA task definition (read-only, for reference) |

### Key Implementation Details

#### bmad-story-development.yaml

**Summary node location:** Lines 286-326

**Current detection logic (to preserve as primary):**
```python
dev_done = "DEV_STORY_COMPLETED" in dev_output or "DEV_REVIEW_QA_COMPLETED" in dev_output
qa_done = "QA_REVIEW_COMPLETED" in qa_output
sm_done = "SM_STATUS_UPDATED" in sm_output
```

**Artifact-based detection (to add as fallback):**

1. **Dev completion:** Read `state.story_path`, extract `## Status` section, check for "Review" or "Ready for Review"
2. **QA completion:** Check for file existence: `docs/qa/gates/{story_id}*.yml`
3. **SM completion:** Read story file, check status is final state (Done, Changes Required, Done (Waived))

**Pattern to follow:** The existing `check_qa_gate` node (lines 130-214) already implements artifact-based detection - follow this pattern.

#### bmad-story-validation.yaml

**Summary node location:** Lines 178-219

**Current detection logic (to preserve as primary):**
```python
risk_done = "RISK_PROFILE_COMPLETED" in risk_output
nfr_done = "NFR_ASSESS_COMPLETED" in nfr_output
test_done = "TEST_DESIGN_COMPLETED" in test_output
sm_done = "SM_CHECKLIST_COMPLETED" in sm_output
```

**Artifact-based detection (to add as fallback):**

1. **Risk Profile:** Check story for "## QA Notes - Risk Profile" OR `docs/qa/assessments/{story_id}-risk-*.md`
2. **NFR Assessment:** Check story for "## QA Notes - NFR Assessment" OR `docs/qa/assessments/{story_id}-nfr-*.md`
3. **Test Design:** Check story for "## QA Notes - Test Design" OR `docs/qa/assessments/{story_id}-test-design-*.md`
4. **SM Checklist:** Check story for "## SM Validation" AND status is "Ready for Development" or "Needs Revision"

### Helper Function Pattern

Create reusable helper functions in the summary node's `run:` block:

```python
def check_story_section(story_path, section_name):
    """Check if a section exists in the story file."""
    try:
        with open(story_path, 'r') as f:
            return section_name in f.read()
    except Exception:
        return False

def check_story_status(story_path, expected_statuses):
    """Check if story status matches expected values."""
    import re
    try:
        with open(story_path, 'r') as f:
            content = f.read()
        match = re.search(r'## Status\s*\n+\s*(\w+[\w\s]*)', content)
        if match:
            status = match.group(1).strip()
            return any(exp.lower() in status.lower() for exp in expected_statuses)
    except Exception:
        pass
    return False

def check_artifact_file(pattern):
    """Check if artifact file(s) matching pattern exist."""
    import glob
    return len(glob.glob(pattern)) > 0
```

### Testing

- Development workflow: `tea run examples/workflows/bmad-story-development.yaml --arg="TEA-RALPHY-002.4"`
- Validation workflow: `tea run examples/workflows/bmad-story-validation.yaml --arg="TEA-RALPHY-002.4"`
- Verify summary output shows correct completion status with detection method indicator
- Test with stories that have been previously processed through each pipeline

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-25 | 1.0 | Initial story creation | Sarah (PO) |
| 2025-01-25 | 1.1 | Expanded scope to include bmad-story-validation.yaml | Sarah (PO) |
| 2026-01-25 | 1.2 | Implementation complete: artifact-based detection for both workflows, 43 tests | James (Dev) |

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

- All 43 unit tests pass in `python/tests/test_workflow_status_detection.py`
- Pre-existing test failure in `test_llm_actions.py::TestShellProviderConfig::test_opencode_provider_config` (unrelated to this story)

### Completion Notes List

1. **bmad-story-development.yaml**: Updated summary node (lines 286-400) with artifact-based detection:
   - `check_story_status()`: Case-insensitive status extraction using regex
   - `check_artifact_file()`: Glob pattern matching for gate files
   - `get_story_id()`: Story ID extraction from file paths
   - Dev: Checks for "Ready for Review"/"Review" status
   - QA: Checks for gate file in `docs/qa/gates/{story_id}*.yml`
   - SM: Checks for final statuses (Done, Changes Required, Done (Waived))
   - Detection method indicator added to summary output

2. **bmad-story-validation.yaml**: Updated summary node (lines 178-300) with artifact-based detection:
   - Same helper functions as development workflow
   - Risk Profile: Checks section OR assessment file
   - NFR Assessment: Checks section OR assessment file
   - Test Design: Checks section OR assessment file
   - SM Checklist: Checks section AND status (Ready for Development/Needs Revision)
   - Detection method indicator added to summary output

3. **Test coverage**: 43 tests covering:
   - P0: `check_story_status` with various status values
   - P0: `check_artifact_file` with gate/assessment files
   - P1: Case-insensitive matching, edge cases
   - Integration: Marker-first, artifact-fallback logic
   - Backward compatibility: All marker detection preserved

### File List

| File | Action | Description |
|------|--------|-------------|
| `examples/workflows/bmad-story-development.yaml` | Modified | Added artifact-based detection to summary node |
| `examples/workflows/bmad-story-validation.yaml` | Modified | Added artifact-based detection to summary node |
| `python/tests/test_workflow_status_detection.py` | Created | 43 unit tests for helper functions and detection logic |

## SM Validation

**Validation Date:** 2026-01-25
**Reviewer:** Bob (Scrum Master)

### Validation Checklist Results

| Category | Status | Issues |
|----------|--------|--------|
| 1. Goal & Context Clarity | PASS | None |
| 2. Technical Implementation Guidance | PASS | None |
| 3. Reference Effectiveness | PASS | None |
| 4. Self-Containment Assessment | PASS | None |
| 5. Testing Guidance | PASS | None |
| 6. QA Notes Sections | PASS | All three present (Risk, NFR, Test Design) |

### Definition of Ready Checklist

- [x] Story has clear title and description
- [x] Acceptance criteria are defined and testable (14 ACs)
- [x] Dependencies are identified (workflow files, shell provider)
- [x] Technical approach is documented (helper function patterns, file locations)
- [x] Story is properly sized (10 tasks with subtasks)
- [x] QA notes sections are present (Risk Profile, NFR Assessment, Test Design)
- [x] No blocking issues or unknowns

### Quality Observations

**Strengths:**
- Excellent problem analysis with evidence from investigation
- Complete helper function patterns provided for developer reference
- Comprehensive test design with 28 scenarios across unit/integration/E2E
- Clear backward compatibility requirements (marker-first, artifact fallback)

**Minor Notes:**
- NFR Assessment flagged maintainability concerns (helper duplication) - non-blocking
- Consider adding suggested ACs 15-17 for additional maintainability coverage

### Final Assessment

**READY FOR DEVELOPMENT** - All Definition of Ready criteria passed. Story provides exceptional context for implementation with detailed technical guidance, complete QA documentation, and clear acceptance criteria.

## QA Results

### Review Date: 2026-01-25

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT**

The implementation demonstrates high-quality software engineering practices:

1. **Helper Functions**: Well-designed, reusable helper functions (`check_story_status`, `check_story_section`, `check_artifact_file`, `get_story_id`) with consistent patterns across both workflow files.

2. **Regex Design**: The status extraction regex `r'##\s*Status\s*\n+\s*(\S+[\S\s]*?)(?=\n##|\n\n##|\Z)'` is robust, handling:
   - Variable whitespace after section header
   - Multi-word status values
   - Status sections followed by other sections or EOF

3. **Error Handling**: All file operations wrapped in try/except with graceful fallback to `False`, ensuring workflow reliability.

4. **Detection Logic**: Clean marker-first, artifact-fallback pattern with explicit method indicators in output.

5. **Test Coverage**: Comprehensive 43-test suite covering:
   - P0 critical scenarios (status extraction, artifact detection)
   - P1 edge cases (case-insensitivity, partial matches)
   - Integration tests (marker+artifact interaction)
   - Backward compatibility verification

### Refactoring Performed

None required. The implementation is clean and follows established patterns.

### Compliance Check

- Coding Standards: ✓ Follows Python best practices, consistent naming
- Project Structure: ✓ Tests in correct location, workflow files properly organized
- Testing Strategy: ✓ Unit + integration tests with proper isolation (temp directories)
- All ACs Met: ✓ All 14 acceptance criteria verified

### Improvements Checklist

- [x] Helper functions implemented with error handling
- [x] Case-insensitive matching implemented
- [x] Backward compatibility preserved (marker-first)
- [x] Detection method indicators in output
- [x] Comprehensive test coverage (43 tests, all passing)
- [ ] Consider extracting helpers to shared module (non-blocking, for future maintainability)
- [ ] Add inline comments documenting marker-first, artifact-fallback rationale (minor)

### Security Review

**No security concerns.** This is internal workflow tooling that:
- Only reads local files within the project
- Does not handle external input or authentication
- Uses standard library functions (glob, re, os) safely

### Performance Considerations

**No performance concerns.** The implementation:
- Uses scoped glob patterns (not recursive wildcards)
- Reads files only when needed (lazy evaluation)
- No network calls or expensive operations
- File reads are minimal (single story file, single gate file per detection)

### Files Modified During Review

None. No refactoring was necessary.

### Gate Status

Gate: PASS → docs/qa/gates/TEA-RALPHY-002.4-bmad-workflow-status-detection.yml
Risk profile: docs/qa/assessments/TEA-RALPHY-002.4-risk-20260125.md
NFR assessment: docs/qa/assessments/TEA-RALPHY-002.4-nfr-20260125.md

### Recommended Status

✓ Ready for Done

**Rationale:**
1. All 14 acceptance criteria implemented and verified
2. All 43 tests passing
3. No security or performance concerns
4. Code quality is excellent
5. Risk profile shows LOW risk (88/100)
6. NFR assessment shows PASS with minor maintainability CONCERNS (non-blocking)

## QA Notes - Risk Profile

**Assessment Date:** 2026-01-25
**Reviewer:** Quinn (Test Architect)
**Full Report:** `docs/qa/assessments/TEA-RALPHY-002.4-risk-20260125.md`

### Risk Level: LOW (Score: 88/100)

### Risk Summary

| Risk ID  | Description                                      | Score | Priority |
|----------|--------------------------------------------------|-------|----------|
| TECH-001 | Regex pattern mismatch in status extraction      | 4     | Medium   |
| OPS-002  | Backward compatibility - marker detection altered| 3     | Low      |
| DATA-001 | Story file parsing fails on malformed markdown   | 2     | Low      |
| TECH-002 | File path resolution fails for edge cases        | 2     | Low      |
| TECH-003 | Glob pattern doesn't match all gate file variants| 2     | Low      |
| OPS-001  | Summary output format breaks terminal rendering  | 1     | Minimal  |

**Totals:** 0 Critical | 0 High | 1 Medium | 5 Low

### Key Mitigations

1. **TECH-001 (Highest risk):** Use case-insensitive regex matching; add unit tests for status extraction helper function covering all documented status values.

2. **OPS-002 (Backward compatibility):** Preserve marker detection as primary check (per AC 9); artifact detection only as fallback.

3. **DATA-001 & TECH-002:** Wrap file reads in try/except; return False on parse failure.

### Testing Priorities

1. **Priority 1:** Unit test `check_story_status()` regex against all valid status strings
2. **Priority 2:** Integration test backward compatibility - verify markers still detected
3. **Priority 3:** Edge case tests for malformed/missing files and path resolution
4. **Priority 4:** Visual inspection of summary output

### Gate Recommendation

**PASS** - No critical or high risks identified. Medium risk (TECH-001) mitigated through testing requirements already specified in AC 13-14.

## QA Notes - NFR Assessment

**Assessment Date:** 2026-01-25
**Reviewer:** Quinn (Test Architect)
**Full Report:** `docs/qa/assessments/TEA-RALPHY-002.4-nfr-20260125.md`

### Quality Score: 90/100

### NFR Summary

| NFR | Status | Notes |
|-----|--------|-------|
| Security | PASS | Internal tooling, no external data handling or authentication |
| Performance | PASS | Minimal file I/O, scoped globs, no network calls |
| Reliability | PASS | Marker-first with artifact fallback, graceful degradation |
| Maintainability | CONCERNS | Helper function duplication, missing unit test requirements |

### NFR Coverage Assessment

**Covered by Story:**
- ✓ Error handling specified (try/except for file reads)
- ✓ Backward compatibility preserved (marker detection as primary)
- ✓ Graceful degradation (fallback design)
- ✓ Integration testing specified (Task 10)

**Missing Considerations:**
1. **Unit tests for helper functions** - No AC requires testing `check_story_status`, `check_story_section`, `check_artifact_file` individually
2. **Code duplication** - Same helpers duplicated in both workflow files without shared module
3. **Documentation** - No requirement to document dual detection mechanism

### Test Recommendations

| Priority | Test | Rationale |
|----------|------|-----------|
| P1 | Unit test `check_story_status` regex | Status extraction regex must match all valid status strings |
| P1 | Integration test marker+artifact detection | Verify both detection paths work end-to-end |
| P2 | Unit test file read edge cases | Ensure graceful handling of missing/malformed files |
| P2 | Unit test glob patterns | Verify gate file matching works across naming variants |

### Suggested Additional Acceptance Criteria

Consider adding to strengthen maintainability:

- **AC-15:** Unit tests for helper functions covering: "Draft", "Ready for Review", "Done", "Changes Required", "Done (Waived)"
- **AC-16:** Inline comments in workflow files explaining marker-first, artifact-fallback design rationale
- **AC-17:** Test coverage ≥80% for new helper function code

### Gate YAML Block

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'Internal workflow tooling, no external data handling or auth'
  performance:
    status: PASS
    notes: 'Minimal file I/O, scoped globs, no network calls'
  reliability:
    status: PASS
    notes: 'Marker-first with artifact fallback, graceful degradation'
  maintainability:
    status: CONCERNS
    notes: 'Helper function duplication, missing unit test requirements'
```

### Overall Assessment

**PASS with CONCERNS** - NFR posture is adequate for development. Maintainability concerns are non-blocking but should be addressed before production use. Recommend adding unit test ACs to strengthen test coverage.

## QA Notes - Test Design

**Assessment Date:** 2026-01-25
**Reviewer:** Quinn (Test Architect)
**Full Report:** `docs/qa/assessments/TEA-RALPHY-002.4-test-design-20260125.md`

### Test Coverage Matrix

| AC | Description | Unit | Integration | E2E | Total |
|----|-------------|------|-------------|-----|-------|
| AC1 | Dev phase - "Ready for Review" status | 5 | 1 | - | 6 |
| AC2 | QA phase - gate file exists | 3 | 1 | - | 4 |
| AC3 | SM phase - final status | 3 | 1 | - | 4 |
| AC4 | Gate status extraction | - | 2 | - | 2 |
| AC5 | Risk Profile completion | 1 | 1 | - | 2 |
| AC6 | NFR Assessment completion | 1 | 1 | - | 2 |
| AC7 | Test Design completion | 1 | 1 | - | 2 |
| AC8 | SM Checklist completion | - | 1 | - | 1 |
| AC9 | Marker-first detection | - | - | 2 | 2 |
| AC11 | No agent changes | - | 1 | - | 1 |
| AC12 | Detection method indicator | - | - | 2 | 2 |
| AC13-14 | Regression/compatibility | - | - | 2 | 2 |
| **Total** | | **14** | **10** | **4** | **28** |

### Priority Distribution

| Priority | Count | Percentage |
|----------|-------|------------|
| P0 (Critical) | 6 | 21% |
| P1 (High) | 12 | 43% |
| P2 (Medium) | 8 | 29% |
| P3 (Low) | 2 | 7% |

### Key Test Scenarios

#### P0 Scenarios (Must Test)

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| 002.4-UNIT-001 | `check_story_status` with "Ready for Review" | Returns True |
| 002.4-UNIT-002 | `check_story_status` with "Review" | Returns True (partial match) |
| 002.4-UNIT-006 | `check_artifact_file` with existing gate | Returns True |
| 002.4-UNIT-009 | `check_story_status` with "Done" | Returns True |
| 002.4-INT-002 | QA detection with various gate naming patterns | Finds all variants |
| 002.4-E2E-001 | Marker present in output | Skips artifact detection, shows "(via marker)" |

#### P1 Scenarios (Should Test)

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| 002.4-UNIT-003 | `check_story_status` with "Draft" | Returns False |
| 002.4-UNIT-004 | Case-insensitive status matching | "READY FOR REVIEW" matches |
| 002.4-UNIT-010 | `check_story_status` with "Changes Required" | Returns True |
| 002.4-UNIT-011 | `check_story_status` with "Done (Waived)" | Returns True |
| 002.4-INT-001 | Dev detection reads actual story file | Extracts status correctly |
| 002.4-INT-004 | Gate status extraction from YAML | Parses PASS/CONCERNS/FAIL |
| 002.4-E2E-002 | Marker absent, artifact present | Uses artifact, shows "(via artifact)" |

### Test Data Requirements

**Story Files:**
- `test_story_draft.md` - Status: "Draft"
- `test_story_ready_review.md` - Status: "Ready for Review"
- `test_story_done.md` - Status: "Done"
- `test_story_changes_required.md` - Status: "Changes Required"
- `test_story_with_qa_notes.md` - All QA Notes sections populated
- `test_story_malformed.md` - Missing `## Status` section
- `test_story_mixed_case.md` - Status: "ready FOR review"

**Gate Files:**
- `docs/qa/gates/TEST-001.1-test-story.yml` - PASS status
- `docs/qa/gates/TEST-001.2-test-story.yml` - CONCERNS status
- `docs/qa/gates/TEST-001.3-test-story.yml` - FAIL status

**Assessment Files:**
- `docs/qa/assessments/TEST-001.1-risk-20260125.md`
- `docs/qa/assessments/TEST-001.1-nfr-20260125.md`
- `docs/qa/assessments/TEST-001.1-test-design-20260125.md`

### Test Environment Requirements

| Requirement | Details |
|-------------|---------|
| Python Version | 3.10+ |
| TEA Installation | `pip install -e .[dev]` |
| Pytest | With file fixtures support |
| Write Access | `docs/qa/` directory for test fixtures |
| Test Isolation | Temporary directories per test run |

### Execution Order

1. **P0 Unit tests** - Fail fast on core regex/file logic
2. **P0 Integration tests** - Validate file operations work
3. **P0 E2E test** - Backward compatibility check
4. **P1 Unit tests** - Secondary logic coverage
5. **P1 Integration tests** - Secondary integrations
6. **P1 E2E test** - Fallback behavior validation
7. **P2+ tests** - As time permits

### Gate YAML Block

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
```

### Test Design Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels appropriate (unit-heavy for helper functions)
- [x] No duplicate coverage across levels
- [x] Priorities align with risk profile (TECH-001 mitigated by P0 unit tests)
- [x] Test IDs follow `{epic}.{story}-{LEVEL}-{SEQ}` convention
- [x] Scenarios are atomic and independent
- [x] All identified risks have mitigation tests
