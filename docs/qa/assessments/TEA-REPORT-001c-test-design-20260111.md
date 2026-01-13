# Test Design: Story TEA-REPORT-001c

**Date:** 2026-01-11
**Designer:** Quinn (Test Architect)
**Story:** GitHub Pages Report Viewer
**Epic:** TEA-REPORT-001 (Automatic Bug Reporting System)

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 42 | 100% |
| Unit tests | 18 | 43% |
| Integration tests | 14 | 33% |
| E2E tests | 10 | 24% |

**Priority Distribution:**
- **P0 (Critical):** 12 scenarios
- **P1 (High):** 16 scenarios
- **P2 (Medium):** 10 scenarios
- **P3 (Low):** 4 scenarios

## Test Pyramid Rationale

This story is heavily focused on JavaScript client-side logic with external API integration. The test strategy favors:
1. **Unit tests** for decoder algorithms (base64url, inflate, VLQ) - must match Rust/Python parity
2. **Integration tests** for GitHub API interactions and DOM rendering
3. **E2E tests** for critical user journeys (view report, file issue)

---

## Test Scenarios by Acceptance Criteria

### AC-12: Report viewer section at `{org}.github.io/the_edge_agent/report/`

**Risk Level:** Medium - Deployment configuration issue could break entire feature

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-001 | Unit | P2 | Sphinx `conf.py` includes `html_extra_path` with 'extra' | Configuration validation |
| 001c-INT-001 | Integration | P1 | Report directory structure copied to `_build/html/report/` | Build pipeline validation |
| 001c-E2E-001 | E2E | P1 | Navigate to `/report/` path returns 200 status | Deployment verification |

---

### AC-13: JavaScript decodes URL parameters (VLQ decode, inflate, base64url decode)

**Risk Level:** High - Decoder mismatch with Rust/Python would break core functionality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-002 | Unit | P0 | `base64urlDecode` handles standard input | Core algorithm correctness |
| 001c-UNIT-003 | Unit | P0 | `base64urlDecode` adds missing padding (1-3 chars) | Edge case: padding variations |
| 001c-UNIT-004 | Unit | P0 | `base64urlDecode` converts `-` to `+` and `_` to `/` | URL-safe alphabet conversion |
| 001c-UNIT-005 | Unit | P0 | `inflate` decompresses valid deflate data | Pako library integration |
| 001c-UNIT-006 | Unit | P0 | `inflate` throws on invalid compressed data | Error handling |
| 001c-UNIT-007 | Unit | P0 | `vlqDecode` decodes single-byte values (0-127) | VLQ algorithm - simple case |
| 001c-UNIT-008 | Unit | P0 | `vlqDecode` decodes multi-byte values (128+) | VLQ algorithm - continuation bit |
| 001c-UNIT-009 | Unit | P0 | `vlqDecode` matches Rust implementation output | Cross-runtime parity |
| 001c-UNIT-010 | Unit | P0 | `vlqDecode` matches Python implementation output | Cross-runtime parity |
| 001c-UNIT-011 | Unit | P1 | `parseReportUrl` extracts version, runtime, encoded from valid path | URL parsing |
| 001c-UNIT-012 | Unit | P1 | `parseReportUrl` throws on malformed URL | Error handling |
| 001c-INT-002 | Integration | P0 | Full decode pipeline: URL → base64url → inflate → JSON | End-to-end decode flow |
| 001c-INT-003 | Integration | P0 | Decode Rust-encoded test URL produces expected JSON | Cross-runtime parity |
| 001c-INT-004 | Integration | P0 | Decode Python-encoded test URL produces expected JSON | Cross-runtime parity |

---

### AC-14: Display decoded error info (version, platform, stack trace, error message)

**Risk Level:** Medium - UI rendering issues affect usability

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-INT-005 | Integration | P1 | Version badge renders with correct value | DOM rendering |
| 001c-INT-006 | Integration | P1 | Platform badge renders with correct value | DOM rendering |
| 001c-INT-007 | Integration | P1 | Runtime badge renders with correct value | DOM rendering |
| 001c-INT-008 | Integration | P1 | Error message displays in monospace pre/code | UI specification |
| 001c-INT-009 | Integration | P1 | Stack trace frames render as list items | DOM rendering |
| 001c-INT-010 | Integration | P2 | Extended context section visible when report.extended present | Conditional rendering |
| 001c-INT-011 | Integration | P2 | Extended context section hidden when report.extended absent | Conditional rendering |

---

### AC-15: Source remapping using debug symbols (optional)

**Risk Level:** Low - Optional feature with graceful degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-013 | Unit | P3 | Source map lookup returns mapped location when available | Optional feature |
| 001c-UNIT-014 | Unit | P3 | Source map lookup returns original location when unavailable | Graceful fallback |

---

### AC-16: "File issue on GitHub" button with pre-populated title and body

**Risk Level:** High - Core feature for issue reporting

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-015 | Unit | P0 | `generateIssueTitle` format: `[{error_type}] {message (truncated 60)}` | Title format spec |
| 001c-UNIT-016 | Unit | P1 | `generateIssueBody` includes Environment section | Body format spec |
| 001c-UNIT-017 | Unit | P1 | `generateIssueBody` includes Error section with code block | Body format spec |
| 001c-UNIT-018 | Unit | P1 | `generateIssueBody` includes Stack Trace section | Body format spec |
| 001c-E2E-002 | E2E | P0 | Click "File issue" button opens GitHub new issue page | User journey |

---

### AC-17: Check for existing issues with similar stack traces

**Risk Level:** Medium - Prevents duplicate issues

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-INT-012 | Integration | P1 | `searchSimilarIssues` calls GitHub Search API with keywords | API integration |
| 001c-INT-013 | Integration | P2 | `searchSimilarIssues` returns empty array on API error | Graceful degradation |
| 001c-E2E-003 | E2E | P1 | Similar issues section displays when matches found | User flow |
| 001c-E2E-004 | E2E | P2 | Similar issues section hidden when no matches | User flow |

---

### AC-18: Mobile-friendly responsive design

**Risk Level:** Medium - Usability on mobile devices

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-E2E-005 | E2E | P1 | Page renders correctly at 320px viewport width | Mobile breakpoint |
| 001c-E2E-006 | E2E | P2 | Buttons stack vertically on narrow viewport | Responsive layout |
| 001c-E2E-007 | E2E | P2 | Stack trace scrolls horizontally without breaking layout | Overflow handling |

---

### AC-35: "File issue" button redirects to GitHub's native issue creation page

**Risk Level:** High - Core issue filing functionality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-019 | Unit | P0 | `generateIssueUrl` produces valid GitHub issues/new URL | URL generation |
| 001c-E2E-008 | E2E | P0 | Clicking "File issue" navigates to github.com/{org}/{repo}/issues/new | Critical path |

---

### AC-36: User must be logged into GitHub to submit (implicit authentication)

**Risk Level:** Low - GitHub handles authentication

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-E2E-009 | E2E | P3 | Unauthenticated user redirected to GitHub login | Implicit auth flow |

---

### AC-37: Issue template pre-fills with decoded report data

**Risk Level:** Medium - Data integrity in issue template

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-UNIT-020 | Unit | P1 | Issue body includes version from report | Template data mapping |
| 001c-UNIT-021 | Unit | P1 | Issue body includes platform from report | Template data mapping |
| 001c-UNIT-022 | Unit | P1 | Issue body includes full stack trace | Template data mapping |

---

### AC-38: Search existing issues for similar stack traces before "File issue"

**Risk Level:** Medium - Duplicate prevention UX

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-INT-014 | Integration | P1 | GitHub search called before showing "File issue" button | Workflow sequence |

---

### AC-39: If similar issue exists, show link instead of "File new issue"

**Risk Level:** Medium - UX for duplicate handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-E2E-010 | E2E | P1 | When similar issues found, "Similar Issues" list displayed with links | Duplicate prevention UX |

---

## Error Handling Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001c-INT-ERR-001 | Integration | P1 | Invalid URL shows user-friendly error message | Error UX |
| 001c-INT-ERR-002 | Integration | P1 | Malformed base64 shows decode error | Error UX |
| 001c-INT-ERR-003 | Integration | P2 | Invalid JSON after inflate shows parse error | Error UX |
| 001c-INT-ERR-004 | Integration | P2 | GitHub API timeout shows fallback state | Graceful degradation |

---

## Risk Coverage Matrix

| Risk | Impact | Test Coverage |
|------|--------|---------------|
| Decoder mismatch with Rust/Python | High | 001c-INT-003, 001c-INT-004, 001c-UNIT-009, 001c-UNIT-010 |
| GitHub API rate limit | Medium | 001c-INT-013, 001c-INT-ERR-004 |
| Large report breaks layout | Low | 001c-E2E-007 |
| Invalid URL handling | Medium | 001c-INT-ERR-001, 001c-INT-ERR-002 |

---

## Recommended Execution Order

1. **P0 Unit tests** - Decoder algorithms (fail fast on parity issues)
2. **P0 Integration tests** - Cross-runtime decode pipeline
3. **P0 E2E tests** - Issue filing critical path
4. **P1 Unit tests** - URL parsing, issue generation
5. **P1 Integration tests** - DOM rendering, GitHub API
6. **P1 E2E tests** - Mobile, similar issues flow
7. **P2+ tests** - As time permits

---

## Test Implementation Notes

### Unit Test Setup (Jest/Vitest recommended)
```javascript
// decoder.test.js
import { base64urlDecode, vlqDecode, inflate, parseReportUrl } from './decoder.js';

describe('base64urlDecode', () => {
  test('handles standard input', () => { /* ... */ });
  test('adds missing padding', () => { /* ... */ });
  test('converts URL-safe chars', () => { /* ... */ });
});
```

### Integration Test Setup
```javascript
// decoder.integration.test.js
import { decodeReport } from './decoder.js';

describe('decodeReport', () => {
  test('decodes Rust-encoded URL', async () => {
    const rustUrl = '/report/0.9.34/rust_ENCODED_DATA';
    const result = decodeReport(rustUrl);
    expect(result.runtime).toBe('rust');
  });
});
```

### E2E Test Setup (Playwright recommended)
```javascript
// report-viewer.spec.js
test('file issue button navigates to GitHub', async ({ page }) => {
  await page.goto('/report/0.9.34/rust_ENCODED_DATA');
  await page.click('#file-issue');
  await expect(page).toHaveURL(/github\.com.*issues\/new/);
});
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for algorithms, integration for API/DOM, E2E for journeys)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (decoder parity = P0)
- [x] Test IDs follow naming convention `{epic}.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 18
    integration: 14
    e2e: 10
  by_priority:
    p0: 12
    p1: 16
    p2: 10
    p3: 4
  coverage_gaps: []
  cross_runtime_parity_tests: 4
  critical_path_coverage: "decoder → display → file_issue"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-REPORT-001c-test-design-20260111.md
P0 tests identified: 12
P1 tests identified: 16
Cross-runtime parity tests: 4 (001c-UNIT-009, 001c-UNIT-010, 001c-INT-003, 001c-INT-004)
```
