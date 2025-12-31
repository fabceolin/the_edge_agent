# Test Design: Story TEA-KIROKU-004

Date: 2024-12-27
Designer: Quinn (Test Architect)
Story: Interview Mode Configuration

## Test Strategy Overview

- Total test scenarios: 22
- Unit tests: 6 (27%)
- Integration tests: 10 (45%)
- E2E tests: 6 (27%)
- Priority distribution: P0: 6, P1: 10, P2: 6

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Terminal encoding issues | Low | Medium | UTF-8 validation |
| Long output truncation | Medium | Low | Configurable limits |
| Command parsing errors | Medium | Medium | Strict command validation |
| Checkpoint corruption on Ctrl+C | Medium | High | Signal handler tests |

## Test Scenarios by Acceptance Criteria

### AC1: tea run --interactive Starts Interactive Session

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-INT-001 | Integration | P0 | `tea run --interactive kiroku.yaml` starts session | CLI integration (depends on TEA-CLI-005) |
| KIROKU-004-INT-002 | Integration | P0 | Session loads YAML and displays initial prompt | Session initialization |
| KIROKU-004-UNIT-001 | Unit | P1 | Interview mode detected from CLI flags | Flag parsing |

### AC2: Contextual Messages at Each Interrupt Point

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-INT-003 | Integration | P0 | suggest_title_review shows title review prompt | Message display |
| KIROKU-004-INT-004 | Integration | P1 | topic_sentence_review shows outline review prompt | Message display |
| KIROKU-004-INT-005 | Integration | P1 | writer_review shows draft review prompt | Message display |
| KIROKU-004-INT-006 | Integration | P1 | reflection_review shows critique prompt | Message display |
| KIROKU-004-INT-007 | Integration | P2 | generate_citations shows references prompt | Message display |

### AC3: Empty Input Advances to Next State

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-UNIT-002 | Unit | P0 | Empty string input treated as "accept" | Input handling |
| KIROKU-004-UNIT-003 | Unit | P1 | Whitespace-only input treated as empty | Edge case |
| KIROKU-004-INT-008 | Integration | P1 | Press Enter advances from title_review to next | Flow validation |

### AC4: Draft Displayed After Generation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-INT-009 | Integration | P1 | After paper_writer, draft is displayed | Output display |
| KIROKU-004-UNIT-004 | Unit | P2 | Long draft truncated to 50 lines | Truncation logic |
| KIROKU-004-E2E-001 | E2E | P2 | Markdown formatting preserved in display | Visual integrity |

### AC5: /save Command Exports Draft

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-UNIT-005 | Unit | P0 | /save command recognized | Command parsing |
| KIROKU-004-INT-010 | Integration | P1 | /save creates file with current draft | File creation |
| KIROKU-004-E2E-002 | E2E | P1 | Saved file contains complete Markdown | Export validation |

### AC6: /status Command Shows Workflow Status

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-UNIT-006 | Unit | P1 | /status command recognized | Command parsing |
| KIROKU-004-E2E-003 | E2E | P1 | /status shows current node and progress | Status display |

### AC7: /references Command Shows References

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-E2E-004 | E2E | P2 | /references shows list of collected references | References display |

### AC8: Ctrl+C Interrupt and Resume Works

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-004-E2E-005 | E2E | P0 | Ctrl+C saves checkpoint and exits gracefully | Signal handling |
| KIROKU-004-E2E-006 | E2E | P0 | Resume with same checkpoint ID restores state | Checkpoint integrity |

## Test Data Requirements

### Test Fixtures Needed

1. **Mock Terminal** - Simulated stdin/stdout for testing
2. **Sample Inputs** - Various user inputs for each interrupt
3. **Checkpoint Files** - Saved states for resume testing
4. **Long Draft** - 100+ lines for truncation testing

### Sample Interview Session

```
$ tea run --interactive kiroku-document-writer.yaml

Starting Kiroku Document Writer...
Loaded: paper-spec.yaml

[1/13] Generating title suggestions...

=== Title Suggestions ===
Original: My Paper
Suggested: "Optimized Neural Networks for Edge Computing"

Type feedback or press Enter to accept:
> [Enter]

[2/13] Searching for relevant content...
Found 15 relevant sources.

[3/13] Writing topic sentences...
```

### Command Test Cases

| Command | Expected Behavior |
|---------|-------------------|
| `/save` | Save to default filename |
| `/save myfile.md` | Save to specified filename |
| `/status` | Show current node, progress % |
| `/references` | List all collected references |
| `/help` | Show available commands |
| `/quit` or Ctrl+C | Save checkpoint, exit |

## Recommended Execution Order

1. P0 Integration tests - Session starts and runs
2. P0 Unit tests - Empty input and command parsing
3. P0 E2E tests - Checkpoint save/resume
4. P1 Integration tests - All interrupt messages
5. P1 E2E tests - /save and /status commands
6. P2 tests - Display formatting

## Quality Checklist

- [x] Every AC has at least one test
- [x] No duplicate coverage across levels
- [x] Critical paths (session start, checkpoint) have P0 tests
- [x] All special commands tested
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-004
  scenarios_total: 22
  by_level:
    unit: 6
    integration: 10
    e2e: 6
  by_priority:
    p0: 6
    p1: 10
    p2: 6
  coverage_gaps: []
  ac_coverage:
    ac1: 3
    ac2: 5
    ac3: 3
    ac4: 3
    ac5: 3
    ac6: 2
    ac7: 1
    ac8: 2
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-KIROKU-004-test-design-20251227.md
P0 tests identified: 6
