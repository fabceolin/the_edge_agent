# Test Design: Story TEA-RELEASE-005.5

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Story Reference

- **ID**: TEA-RELEASE-005.5
- **Title**: Documentation & Migration Guide
- **Type**: Documentation Story
- **Priority**: Medium
- **Status**: Draft

## Test Strategy Overview

- Total test scenarios: 20
- Unit tests: 0 (0%) - Documentation story, no code to unit test
- Integration tests: 0 (0%) - Documentation story, no code to integration test
- E2E tests: 0 (0%) - Documentation story, no automated E2E tests applicable
- **Manual validation tests**: 20 (100%) - Documentation requires human review
- Priority distribution: P0: 4, P1: 8, P2: 6, P3: 2

### Test Strategy Rationale

This is a **documentation-focused story** with no code deliverables. The acceptance criteria focus on:
1. Documentation file creation and updates
2. Content accuracy (commands, syntax, compatibility info)
3. User experience (clarity, completeness, discoverability)

**Testing approach**: Manual validation with structured checklists. Each test verifies:
- File existence and location
- Content completeness per AC requirements
- Technical accuracy (executable commands, correct syntax)
- Cross-platform coverage (Windows/Linux/macOS)

## Test Scenarios by Acceptance Criteria

### AC-1: `docs/installation.md` updated with APE download instructions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-001 | Manual | P0 | Verify APE section exists in installation.md | Critical - primary install path for new users |
| 005.5-MAN-002 | Manual | P1 | Verify curl command downloads valid tea.com binary | Command accuracy critical for user success |
| 005.5-MAN-003 | Manual | P1 | Verify PowerShell command downloads valid tea.exe | Windows users first-class citizens |

**Test Details:**

```yaml
005.5-MAN-001:
  requirement: AC-1
  priority: P0
  level: manual
  description: Verify APE section exists with download instructions
  steps:
    - Open docs/installation.md
    - Verify "Universal Binary (APE)" section exists
    - Verify section appears before other install methods (recommended)
    - Verify download URL points to valid GitHub release
  expected: APE section present with correct GitHub release URL
  mitigates_risks: [New user onboarding failure]

005.5-MAN-002:
  requirement: AC-1
  priority: P1
  level: manual
  description: Execute Linux/macOS curl command and verify binary
  steps:
    - Run documented curl command on Linux/macOS
    - Verify file downloads successfully
    - Run `chmod +x tea` and verify executable
    - Run `tea --version` and verify output
  expected: Binary executes and shows version
  mitigates_risks: [Broken install instructions]

005.5-MAN-003:
  requirement: AC-1
  priority: P1
  level: manual
  description: Execute Windows PowerShell command and verify binary
  steps:
    - Run documented Invoke-WebRequest on Windows
    - Verify file downloads as tea.exe
    - Run `.\tea.exe --version`
  expected: Binary executes and shows version
  mitigates_risks: [Windows user abandonment]
```

---

### AC-2: APE section covers Windows, Linux, macOS installation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-004 | Manual | P0 | Verify Windows installation steps documented | Cross-platform is key APE value prop |
| 005.5-MAN-005 | Manual | P0 | Verify Linux installation steps documented | Primary platform for many users |
| 005.5-MAN-006 | Manual | P1 | Verify macOS installation steps with xattr workaround | macOS Gatekeeper is common blocker |

**Test Details:**

```yaml
005.5-MAN-004:
  requirement: AC-2
  priority: P0
  level: manual
  description: Windows install section completeness
  steps:
    - Verify PowerShell download command present
    - Verify PATH addition guidance provided
    - Verify Windows-specific notes (e.g., .exe extension)
  expected: Complete Windows installation path documented
  mitigates_risks: [Windows platform gap]

005.5-MAN-005:
  requirement: AC-2
  priority: P0
  level: manual
  description: Linux install section completeness
  steps:
    - Verify curl/wget download command
    - Verify chmod +x instruction
    - Verify /usr/local/bin or PATH guidance
  expected: Complete Linux installation path documented
  mitigates_risks: [Primary platform coverage gap]

005.5-MAN-006:
  requirement: AC-2
  priority: P1
  level: manual
  description: macOS install section with Gatekeeper handling
  steps:
    - Verify download command present
    - Verify xattr -d com.apple.quarantine workaround documented
    - Verify System Preferences alternative mentioned
  expected: macOS users can bypass Gatekeeper
  mitigates_risks: [macOS user abandonment]
```

---

### AC-3: `docs/shared/scryer-migration.md` created with SWI → Scryer guide

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-007 | Manual | P1 | Verify migration guide file exists at correct path | File existence is foundational |
| 005.5-MAN-008 | Manual | P1 | Verify guide includes overview comparing SWI vs Scryer | Context needed for migration decisions |
| 005.5-MAN-009 | Manual | P2 | Verify guide includes step-by-step migration examples | Practical examples reduce migration friction |

**Test Details:**

```yaml
005.5-MAN-007:
  requirement: AC-3
  priority: P1
  level: manual
  description: Migration guide file exists
  steps:
    - Navigate to docs/shared/
    - Verify scryer-migration.md exists
    - Verify file has proper H1 title
  expected: File exists at docs/shared/scryer-migration.md
  mitigates_risks: [Migration guide undiscoverable]

005.5-MAN-008:
  requirement: AC-3
  priority: P1
  level: manual
  description: Overview section comparing dialects
  steps:
    - Open scryer-migration.md
    - Verify comparison table (SWI vs Scryer characteristics)
    - Verify key differences highlighted (ISO vs extensions)
  expected: Clear dialect comparison present
  mitigates_risks: [User confusion about dialect differences]

005.5-MAN-009:
  requirement: AC-3
  priority: P2
  level: manual
  description: Practical migration examples
  steps:
    - Verify before/after code examples
    - Verify examples cover common patterns (state, arithmetic)
    - Verify examples are syntactically correct
  expected: At least 2 migration examples with before/after
  mitigates_risks: [Abstract guide without practical help]
```

---

### AC-4: Common predicate compatibility table included

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-010 | Manual | P1 | Verify compatibility table exists with correct categories | Key reference for migrating users |
| 005.5-MAN-011 | Manual | P2 | Verify "Fully Compatible" predicates listed accurately | Prevents unnecessary rewrites |
| 005.5-MAN-012 | Manual | P2 | Verify "Not Available" predicates listed with alternatives | Prevents runtime surprises |

**Test Details:**

```yaml
005.5-MAN-010:
  requirement: AC-4
  priority: P1
  level: manual
  description: Compatibility table structure
  steps:
    - Locate predicate compatibility table in migration guide
    - Verify three categories: Fully Compatible, Requires Changes, Not Available
    - Verify table format is readable (markdown table)
  expected: Three-category compatibility table present
  mitigates_risks: [Incomplete migration guidance]

005.5-MAN-011:
  requirement: AC-4
  priority: P2
  level: manual
  description: Fully compatible predicates accuracy
  steps:
    - Review "Fully Compatible" predicates listed
    - Verify common predicates (member/2, append/3, length/2) included
    - Spot-check 2-3 predicates in Scryer docs
  expected: Listed predicates actually work in Scryer
  mitigates_risks: [False compatibility claims]

005.5-MAN-012:
  requirement: AC-4
  priority: P2
  level: manual
  description: Not available predicates with alternatives
  steps:
    - Review "Not Available" section
    - Verify nb_setval/nb_getval listed (common SWI extension)
    - Verify alternatives provided (e.g., "use TEA state")
  expected: Unavailable predicates have workarounds documented
  mitigates_risks: [User stuck with no path forward]
```

---

### AC-5: Decision flowchart: when to use APE vs AppImage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-013 | Manual | P1 | Verify decision flowchart exists in Mermaid format | Visual aid for distribution choice |
| 005.5-MAN-014 | Manual | P2 | Verify flowchart renders correctly on GitHub | Users view docs on GitHub |

**Test Details:**

```yaml
005.5-MAN-013:
  requirement: AC-5
  priority: P1
  level: manual
  description: Decision flowchart presence
  steps:
    - Locate flowchart in installation.md or dedicated section
    - Verify Mermaid syntax (```mermaid block)
    - Verify decision nodes cover: platform, existing code, size
  expected: Mermaid flowchart with distribution decision logic
  mitigates_risks: [Users choose wrong distribution]

005.5-MAN-014:
  requirement: AC-5
  priority: P2
  level: manual
  description: Flowchart GitHub rendering
  steps:
    - View installation.md on GitHub web UI
    - Verify Mermaid diagram renders as visual flowchart
    - Verify text is readable, colors distinguish paths
  expected: Flowchart renders visually on GitHub
  mitigates_risks: [Broken diagram reduces usability]
```

---

### AC-6: `README.md` updated with universal binary section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-015 | Manual | P0 | Verify README.md includes Universal Binary section | README is primary entry point |
| 005.5-MAN-016 | Manual | P1 | Verify README links to detailed installation docs | Users need path to full instructions |

**Test Details:**

```yaml
005.5-MAN-015:
  requirement: AC-6
  priority: P0
  level: manual
  description: README universal binary section
  steps:
    - Open README.md
    - Verify "Universal Binary" or "APE" section exists
    - Verify section appears prominently (not buried)
    - Verify quick-start commands included
  expected: APE prominently featured in README
  mitigates_risks: [New users miss cross-platform option]

005.5-MAN-016:
  requirement: AC-6
  priority: P1
  level: manual
  description: README links to installation docs
  steps:
    - Verify link to docs/installation.md
    - Click link and verify it resolves
    - Verify link text is descriptive
  expected: Working link to detailed install docs
  mitigates_risks: [Users can't find detailed instructions]
```

---

### AC-7: Troubleshooting section for Windows Defender/Gatekeeper

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-017 | Manual | P1 | Verify Windows Defender workaround documented | Common blocker for APE on Windows |
| 005.5-MAN-018 | Manual | P1 | Verify macOS Gatekeeper workaround documented | Common blocker for APE on macOS |

**Test Details:**

```yaml
005.5-MAN-017:
  requirement: AC-7
  priority: P1
  level: manual
  description: Windows Defender troubleshooting
  steps:
    - Locate troubleshooting section
    - Verify Windows Defender/SmartScreen guidance
    - Verify steps to add exclusion or allow execution
  expected: Clear Windows Defender bypass instructions
  mitigates_risks: [Windows users blocked by security software]

005.5-MAN-018:
  requirement: AC-7
  priority: P1
  level: manual
  description: macOS Gatekeeper troubleshooting
  steps:
    - Locate troubleshooting section
    - Verify xattr -d com.apple.quarantine command
    - Verify System Preferences > Security alternative
  expected: Clear Gatekeeper bypass instructions
  mitigates_risks: [macOS users blocked by Gatekeeper]
```

---

### AC-8: Examples updated with Scryer-compatible syntax (where needed)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-MAN-019 | Manual | P2 | Verify existing Prolog examples reviewed for compatibility | Ensures examples work with APE |
| 005.5-MAN-020 | Manual | P3 | Verify examples include backend: scryer setting where needed | Explicit config reduces confusion |

**Test Details:**

```yaml
005.5-MAN-019:
  requirement: AC-8
  priority: P2
  level: manual
  description: Example files reviewed for Scryer compatibility
  steps:
    - Review examples/prolog/simple-prolog-agent.yaml
    - Review examples/prolog/factorial-agent.yaml
    - Check for SWI-specific predicates (nb_setval, etc.)
    - Document any incompatibilities found
  expected: Examples compatible or documented as SWI-only
  mitigates_risks: [Examples fail with APE distribution]

005.5-MAN-020:
  requirement: AC-8
  priority: P3
  level: manual
  description: Backend setting in examples
  steps:
    - Check if examples specify prolog.backend setting
    - Verify Scryer-compatible examples have backend: scryer
    - Verify SWI-only examples document the requirement
  expected: Examples clear about which backend they require
  mitigates_risks: [User confusion about backend requirements]
```

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| New users can't install APE | 005.5-MAN-001, 002, 003, 004, 005, 006 | Comprehensive install instructions |
| Windows users blocked by Defender | 005.5-MAN-017 | Explicit troubleshooting steps |
| macOS users blocked by Gatekeeper | 005.5-MAN-006, 018 | xattr workaround documented |
| Users choose wrong distribution | 005.5-MAN-013, 014 | Decision flowchart |
| Migration guide unusable | 005.5-MAN-007, 008, 009, 010 | Structured content review |
| Examples fail with APE | 005.5-MAN-019, 020 | Compatibility review |

## Recommended Execution Order

1. **P0 tests first** (005.5-MAN-001, 004, 005, 015) - Core documentation exists
2. **P1 command verification** (005.5-MAN-002, 003, 006) - Commands actually work
3. **P1 content verification** (005.5-MAN-007-008, 010, 013, 016-018) - Content complete
4. **P2 depth verification** (005.5-MAN-009, 011-012, 014, 019) - Quality polish
5. **P3 if time permits** (005.5-MAN-020) - Nice-to-have

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (manual for documentation)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for install path)
- [x] Test IDs follow naming convention (005.5-MAN-NNN)
- [x] Scenarios are atomic and independent

## Test Automation Considerations

While this story produces documentation (no automatable code), future automation could include:

1. **Link checker**: Automated verification of all documentation links
2. **Command extraction**: Parse documented commands and execute in CI
3. **Mermaid validation**: CI check that Mermaid diagrams parse correctly
4. **File existence**: CI check for required documentation files

These would be appropriate for a dedicated "Documentation CI" story.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 20
  by_level:
    unit: 0
    integration: 0
    e2e: 0
    manual: 20
  by_priority:
    p0: 4
    p1: 8
    p2: 6
    p3: 2
  coverage_gaps: []
  notes:
    - Documentation story - all tests are manual validation
    - P0 tests focus on critical install path documentation
    - Consider future link-checker automation
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RELEASE-005.5-test-design-20260112.md
P0 tests identified: 4
P1 tests identified: 8
Total manual validation scenarios: 20
```
