# Test Design: Story TEA-RALPHY-001.0

**Date:** 2026-01-19
**Designer:** Quinn (Test Architect)
**Story:** md-parser Shared Crate

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 28 (60%)
- **Integration tests:** 12 (25%)
- **E2E tests:** 7 (15%)
- **Priority distribution:** P0: 28, P1: 14, P2: 5

### Strategy Rationale

This is a **library crate** with pure Rust logic and multiple compilation targets (Rust, PyO3, WASM). The testing strategy emphasizes:

1. **Heavy unit testing** - Parser logic is pure and deterministic, ideal for fast, isolated tests
2. **Integration tests for bindings** - PyO3/Python interop requires cross-language validation
3. **E2E tests for CI/CD workflows** - GitHub Actions must produce working artifacts

---

## Test Scenarios by Acceptance Criteria

### AC 1-5: Core Parsing (P0)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-UNIT-001 | Unit | P0 | Parse H1-H6 headings into `SectionType::Heading(level)` | Pure parsing logic, no side effects |
| TEA-RALPHY-001.0-UNIT-002 | Unit | P0 | Parse paragraphs into `SectionType::Paragraph` | Core markdown type |
| TEA-RALPHY-001.0-UNIT-003 | Unit | P0 | Parse lists into `SectionType::List` | Core markdown type |
| TEA-RALPHY-001.0-UNIT-004 | Unit | P0 | Parse code blocks into `SectionType::Code` with language | Core markdown type, language extraction |
| TEA-RALPHY-001.0-UNIT-005 | Unit | P0 | Parse blockquotes into `SectionType::Blockquote` | Core markdown type |
| TEA-RALPHY-001.0-UNIT-006 | Unit | P0 | Parse horizontal rules into `SectionType::Hr` | Core markdown type |
| TEA-RALPHY-001.0-UNIT-007 | Unit | P0 | Extract document title from first H1 | Derives from parsing, specific extraction logic |
| TEA-RALPHY-001.0-UNIT-008 | Unit | P0 | Handle document without H1 (title = None) | Edge case - graceful degradation |
| TEA-RALPHY-001.0-UNIT-009 | Unit | P0 | Generate unique section IDs (UUID or sequential) | ID generation algorithm |
| TEA-RALPHY-001.0-UNIT-010 | Unit | P0 | Track section order index (0-based sequential) | Index tracking logic |
| TEA-RALPHY-001.0-UNIT-011 | Unit | P0 | Create "follows" edges between sequential sections | Edge generation from section list |
| TEA-RALPHY-001.0-UNIT-012 | Unit | P0 | Empty document produces empty sections, no edges | Edge case - empty input |

### AC 6-9: Checklist Extraction (P0)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-UNIT-013 | Unit | P0 | Extract `- [ ]` as unchecked item | Core checklist parsing |
| TEA-RALPHY-001.0-UNIT-014 | Unit | P0 | Extract `- [x]` as checked item | Core checklist parsing |
| TEA-RALPHY-001.0-UNIT-015 | Unit | P0 | Extract `- [X]` (uppercase) as checked item | Case-insensitivity |
| TEA-RALPHY-001.0-UNIT-016 | Unit | P0 | Track indent level (2 spaces = 1 level) | Nesting algorithm |
| TEA-RALPHY-001.0-UNIT-017 | Unit | P0 | Track indent level (4 spaces = 2 levels) | Nesting algorithm |
| TEA-RALPHY-001.0-UNIT-018 | Unit | P0 | Track indent with tabs (1 tab = 1 level) | Tab handling |
| TEA-RALPHY-001.0-UNIT-019 | Unit | P0 | Extract AC references `(AC: 1)` single | Regex extraction |
| TEA-RALPHY-001.0-UNIT-020 | Unit | P0 | Extract AC references `(AC: 1, 2, 3)` multiple | Regex extraction, list parsing |
| TEA-RALPHY-001.0-UNIT-021 | Unit | P0 | No AC refs when pattern absent | Edge case - no match |
| TEA-RALPHY-001.0-UNIT-022 | Unit | P0 | `ChecklistSummary` with 0/5 = 0% completion | Percentage calculation |
| TEA-RALPHY-001.0-UNIT-023 | Unit | P0 | `ChecklistSummary` with 3/5 = 60% completion | Percentage calculation |
| TEA-RALPHY-001.0-UNIT-024 | Unit | P0 | `ChecklistSummary` with 5/5 = 100% completion | Edge case - full completion |
| TEA-RALPHY-001.0-UNIT-025 | Unit | P0 | `ChecklistSummary` with 0 items (handle division by zero) | Edge case - empty list |

### AC 10-11: Variable Detection (P0)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-UNIT-026 | Unit | P0 | Detect `{{variable_name}}` single variable | Regex extraction |
| TEA-RALPHY-001.0-UNIT-027 | Unit | P0 | Detect multiple variables in single section | List aggregation |
| TEA-RALPHY-001.0-UNIT-028 | Unit | P0 | Collect unique variables document-wide (dedup) | Set-based collection |
| TEA-RALPHY-001.0-UNIT-029 | Unit | P0 | Handle `{{var_with_underscores}}` | Variable naming patterns |
| TEA-RALPHY-001.0-UNIT-030 | Unit | P0 | Handle `{{CamelCaseVar}}` | Variable naming patterns |
| TEA-RALPHY-001.0-UNIT-031 | Unit | P0 | No false positives for `{single_brace}` | Regex specificity |
| TEA-RALPHY-001.0-UNIT-032 | Unit | P0 | No false positives for `{{ spaced }}` (with spaces) | Regex pattern matching |

### AC 12-13: Frontmatter (P1 - Feature-gated)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-UNIT-033 | Unit | P1 | Parse YAML frontmatter (`---` delimited) | Frontmatter extraction |
| TEA-RALPHY-001.0-UNIT-034 | Unit | P1 | Return frontmatter as `HashMap<String, Value>` | Data structure conversion |
| TEA-RALPHY-001.0-UNIT-035 | Unit | P1 | Handle nested YAML values | Complex YAML structures |
| TEA-RALPHY-001.0-UNIT-036 | Unit | P1 | Handle document without frontmatter | Edge case - None result |
| TEA-RALPHY-001.0-UNIT-037 | Unit | P1 | Handle malformed frontmatter gracefully | Error handling |
| TEA-RALPHY-001.0-INT-001 | Integration | P1 | Feature-gate compilation: `frontmatter` feature enables module | Cargo feature system |

### AC 14-16: PyO3 Python Bindings (P1)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-INT-002 | Integration | P1 | `MarkdownParser` class importable in Python | Cross-language binding |
| TEA-RALPHY-001.0-INT-003 | Integration | P1 | `ParsedDocument` accessible as Python object | Struct-to-class binding |
| TEA-RALPHY-001.0-INT-004 | Integration | P1 | `ParsedSection` accessible as Python object | Struct-to-class binding |
| TEA-RALPHY-001.0-INT-005 | Integration | P1 | `ChecklistItem` accessible as Python object | Struct-to-class binding |
| TEA-RALPHY-001.0-INT-006 | Integration | P1 | Parser returns correct document from Python | End-to-end parsing via binding |
| TEA-RALPHY-001.0-INT-007 | Integration | P1 | Python can serialize result to JSON | Serde integration via Python |
| TEA-RALPHY-001.0-E2E-001 | E2E | P1 | Pre-built wheel installable via `pip install <url>` | GitHub Release artifact usability |

### AC 17-20: CI/CD (P0)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-E2E-002 | E2E | P0 | Rust CI workflow runs `cargo test --all-features` | CI correctness |
| TEA-RALPHY-001.0-E2E-003 | E2E | P0 | Rust CI workflow runs `cargo clippy` without warnings | Code quality gate |
| TEA-RALPHY-001.0-E2E-004 | E2E | P0 | Rust CI workflow runs `cargo fmt --check` | Code style gate |
| TEA-RALPHY-001.0-E2E-005 | E2E | P0 | Python wheels build for Linux/macOS/Windows | Multi-platform artifacts |
| TEA-RALPHY-001.0-E2E-006 | E2E | P0 | WASM build succeeds for wasm32-unknown-unknown | Target compilation |
| TEA-RALPHY-001.0-E2E-007 | E2E | P0 | GitHub Release contains wheels + WASM on tag push | Release automation |

### AC 21-24: Quality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-INT-008 | Integration | P0 | Serde serialization to JSON roundtrip | Feature-gated serialization |
| TEA-RALPHY-001.0-INT-009 | Integration | P0 | Serde deserialization from JSON roundtrip | Feature-gated deserialization |
| TEA-RALPHY-001.0-INT-010 | Integration | P2 | Code coverage report shows >90% | Coverage tooling verification |
| TEA-RALPHY-001.0-INT-011 | Integration | P0 | No `unsafe` blocks in codebase | Safety audit (grep/analysis) |
| TEA-RALPHY-001.0-INT-012 | Integration | P0 | WASM build excludes pyo3 feature and compiles | Feature exclusion and target compatibility |

### Error Handling & Edge Cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RALPHY-001.0-UNIT-038 | Unit | P0 | Malformed markdown (unclosed code block) handled gracefully | Robustness |
| TEA-RALPHY-001.0-UNIT-039 | Unit | P1 | Unicode content in sections preserved | Internationalization |
| TEA-RALPHY-001.0-UNIT-040 | Unit | P1 | Very large document (10MB) parses without OOM | Performance/limits |
| TEA-RALPHY-001.0-UNIT-041 | Unit | P2 | Deeply nested list (100 levels) handled | Edge case limits |
| TEA-RALPHY-001.0-UNIT-042 | Unit | P2 | Empty sections (heading with no content) handled | Structural edge case |

---

## Risk Coverage

| Risk | Mitigated By Tests |
|------|-------------------|
| Parser crashes on malformed input | TEA-RALPHY-001.0-UNIT-038 |
| Checklist percentage division by zero | TEA-RALPHY-001.0-UNIT-025 |
| Python binding segfaults | TEA-RALPHY-001.0-INT-002 through INT-007 |
| WASM includes incompatible dependencies | TEA-RALPHY-001.0-INT-012 |
| CI fails to produce artifacts | TEA-RALPHY-001.0-E2E-002 through E2E-007 |
| Serde feature breaks compilation | TEA-RALPHY-001.0-INT-008, INT-009 |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (Unit Tests - P0)
Execute first to catch logic bugs immediately:
1. Core parsing tests (UNIT-001 through UNIT-012)
2. Checklist extraction tests (UNIT-013 through UNIT-025)
3. Variable detection tests (UNIT-026 through UNIT-032)
4. Error handling tests (UNIT-038)

### Phase 2: Feature Validation (Unit Tests - P1)
5. Frontmatter parsing tests (UNIT-033 through UNIT-037)
6. Edge case tests (UNIT-039 through UNIT-042)

### Phase 3: Integration Validation
7. Serde serialization tests (INT-008, INT-009)
8. Feature gate tests (INT-001)
9. Safety audit (INT-011)
10. WASM compatibility (INT-012)
11. PyO3 binding tests (INT-002 through INT-007)

### Phase 4: Release Validation
12. CI workflow tests (E2E-002 through E2E-006)
13. Release automation test (E2E-007)
14. Wheel installation test (E2E-001)

---

## Test Implementation Notes

### Rust Unit Tests Location
```rust
// tests/unit/parser_tests.rs - Core parsing
// tests/unit/checklist_tests.rs - Checklist extraction
// tests/unit/variables_tests.rs - Variable detection
// tests/unit/frontmatter_tests.rs - Frontmatter (feature-gated)
```

### Python Integration Tests Location
```python
# tests/python_tests.py - PyO3 binding validation
```

### Test Fixtures Required
```
tests/fixtures/
├── simple.md           # Basic markdown (AC 1-5)
├── with_checklist.md   # Checklists with nesting (AC 6-9)
├── with_variables.md   # Template variables (AC 10-11)
├── with_frontmatter.md # YAML header (AC 12-13)
├── bmad_story.md       # Real-world BMad story file
├── malformed.md        # Invalid markdown for error tests
└── large_10mb.md       # Performance test file
```

---

## Coverage Gaps

None identified. All 24 acceptance criteria have corresponding test scenarios.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
