# Risk Profile: Story TEA-RALPHY-002.4

Date: 2026-01-25
Reviewer: Quinn (Test Architect)

## Executive Summary

- Total Risks Identified: 6
- Critical Risks: 0
- High Risks: 0
- Medium Risks: 1
- Low Risks: 5
- Risk Score: 88/100 (Low Risk)

## Risk Matrix

| Risk ID  | Description                                      | Probability | Impact     | Score | Priority |
|----------|--------------------------------------------------|-------------|------------|-------|----------|
| TECH-001 | Regex pattern mismatch in status extraction      | Medium (2)  | Medium (2) | 4     | Medium   |
| OPS-002  | Backward compatibility - marker detection altered| Low (1)     | High (3)   | 3     | Low      |
| DATA-001 | Story file parsing fails on malformed markdown   | Low (1)     | Medium (2) | 2     | Low      |
| TECH-002 | File path resolution fails for edge cases        | Low (1)     | Medium (2) | 2     | Low      |
| TECH-003 | Glob pattern doesn't match all gate file variants| Medium (2)  | Low (1)    | 2     | Low      |
| OPS-001  | Summary output format breaks terminal rendering  | Low (1)     | Low (1)    | 1     | Minimal  |

## Detailed Risk Register

### TECH-001: Regex Pattern Mismatch in Status Extraction

**Score: 4 (Medium)**
**Probability:** Medium - Story status formats vary (e.g., "Ready for Review", "Review", "Done", "Changes Required")
**Impact:** Medium - False negatives would cause workflow to report phase as "Failed" when actually completed

**Mitigation:**
- Use case-insensitive matching
- Test regex against all documented status values from story template
- Add unit tests for status extraction helper function

**Testing Focus:**
- Test with various status formats: "Ready for Review", "Review", "Done", "Changes Required", "Done (Waived)"
- Test with extra whitespace, markdown formatting

---

### OPS-002: Backward Compatibility - Marker Detection Altered

**Score: 3 (Low)**
**Probability:** Low - Story explicitly requires marker detection as first-priority check
**Impact:** High - If markers stop working, existing workflows that rely on them would break

**Mitigation:**
- Preserve exact marker detection logic as primary check (AC 9)
- Add artifact detection as fallback only
- Regression test with stories that output markers correctly

**Testing Focus:**
- Verify marker-based detection still works when markers are present
- Verify fallback only triggers when markers absent

---

### DATA-001: Story File Parsing Fails on Malformed Markdown

**Score: 2 (Low)**
**Probability:** Low - Story files follow standard template
**Impact:** Medium - Would cause phase to report as incomplete

**Mitigation:**
- Wrap file reads in try/except blocks (already shown in helper pattern)
- Return False/empty on parse failure, let fallback handle

**Testing Focus:**
- Test with missing sections
- Test with empty story file
- Test with non-existent file path

---

### TECH-002: File Path Resolution Fails for Edge Cases

**Score: 2 (Low)**
**Probability:** Low - The resolve_story_path node handles multiple patterns
**Impact:** Medium - Wrong file read would cause incorrect detection

**Mitigation:**
- Helper functions should validate file exists before reading
- Log which file was actually used for detection

**Testing Focus:**
- Test with full path input
- Test with short label input (e.g., "TEA-RALPHY-002.4")
- Test with non-existent story ID

---

### TECH-003: Glob Pattern Doesn't Match All Gate File Variants

**Score: 2 (Low)**
**Probability:** Medium - Gate files may use .yml or .yaml extensions
**Impact:** Low - Story also specifies checking story QA sections as alternate detection

**Mitigation:**
- Use pattern that matches both extensions: `{story_id}*.{yml,yaml}`
- Check both story sections AND external files

**Testing Focus:**
- Test with .yml gate file
- Test with .yaml gate file
- Test with no gate file but story section present

---

### OPS-001: Summary Output Format Breaks Terminal Rendering

**Score: 1 (Minimal)**
**Probability:** Low - Box-drawing characters are standard Unicode
**Impact:** Low - Cosmetic issue only, functional output still readable

**Mitigation:**
- Test in various terminals (bash, zsh, Windows Terminal)
- Keep existing format, only add detection method indicator

**Testing Focus:**
- Visual inspection of summary output

---

## Risk Distribution

### By Category

- Technical (TECH): 3 risks (0 critical, 0 high, 1 medium)
- Operational (OPS): 2 risks (0 critical, 0 high)
- Data (DATA): 1 risk (0 critical, 0 high)
- Security (SEC): 0 risks
- Performance (PERF): 0 risks
- Business (BUS): 0 risks

### By Component

- Workflow YAML files: 6 risks
- Python helper functions: 3 risks
- File system operations: 2 risks

## Risk-Based Testing Strategy

### Priority 1: Medium Risk Tests

1. **TECH-001 - Status Regex Testing**
   - Unit test `check_story_status()` with all valid status strings
   - Test case-insensitivity
   - Test whitespace handling

### Priority 2: Low Risk Tests

2. **OPS-002 - Backward Compatibility**
   - Integration test: run pipeline with story that outputs markers
   - Verify marker detection takes precedence

3. **DATA-001 & TECH-002 - File Handling**
   - Edge case tests for malformed/missing files
   - Path resolution tests

4. **TECH-003 - Glob Pattern**
   - Test both .yml and .yaml gate files

### Priority 3: Minimal Risk Tests

5. **OPS-001 - Summary Output**
   - Visual regression testing

## Risk Acceptance Criteria

### Must Fix Before Production

- None (no critical/high risks)

### Can Deploy with Mitigation

- TECH-001: Add comprehensive status regex tests
- OPS-002: Verify marker detection preserved

### Accepted Risks

- OPS-001: Terminal rendering cosmetic, acceptable

## Monitoring Requirements

Post-deployment monitoring for:
- Pipeline completion rates (detect regression)
- Summary output accuracy (via logs)

## Risk Review Triggers

Review and update risk profile when:
- New status values added to story template
- New gate file formats introduced
- Agent persona definitions change marker output

---

## Gate YAML Block (for paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 0
    medium: 1
    low: 5
  highest:
    id: TECH-001
    score: 4
    title: 'Regex pattern mismatch in status extraction'
  recommendations:
    must_fix: []
    monitor:
      - 'Pipeline completion rates after deployment'
```

---

Risk profile: docs/qa/assessments/TEA-RALPHY-002.4-risk-20260125.md
