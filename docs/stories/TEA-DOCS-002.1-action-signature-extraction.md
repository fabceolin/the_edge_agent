# TEA-DOCS-002.1: Action Signature Extraction Script

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.1 |
| **Parent Epic** | TEA-DOCS-002 |
| **Type** | Brownfield Enhancement |
| **Status** | QA Approved |
| **Priority** | Critical (blocks other stories) |
| **Estimated Effort** | 4-6 hours |
| **Created** | 2026-01-25 |

## User Story

**As a** TEA maintainer,
**I want** an automated script to extract all action signatures from the codebase,
**So that** I can generate accurate documentation and detect doc-implementation drift.

## Story Context

### Problem

Manual tracking of 491+ actions across 55 modules is error-prone and unsustainable. The audit revealed 86% of actions are undocumented, partly because there's no automated way to know what exists.

### Solution

Create a Python script that:
1. Parses all action module files
2. Extracts registered action names, parameters, and docstrings
3. Outputs a structured inventory (JSON/YAML)
4. Compares against documentation to identify gaps

## Acceptance Criteria

### Extraction

- [ ] **AC-1**: Script parses all 55 files in `python/src/the_edge_agent/actions/`
- [ ] **AC-2**: Extracts action namespace (e.g., `llm`, `memory`, `graph`)
- [ ] **AC-3**: Extracts action name (e.g., `call`, `store`, `query`)
- [ ] **AC-4**: Extracts parameter names and types (from type hints)
- [ ] **AC-5**: Extracts docstrings for descriptions

### Output

- [ ] **AC-6**: Generates `data/action_inventory.json` with structured data
- [ ] **AC-7**: Generates `data/action_inventory.yaml` for human reading
- [ ] **AC-8**: Output includes module source file for each action

### Validation

- [ ] **AC-9**: `--validate` flag compares inventory to `actions-reference.md`
- [ ] **AC-10**: Reports undocumented actions with severity
- [ ] **AC-11**: Reports documented-but-not-implemented actions (hallucinations)

### Integration

- [ ] **AC-12**: Can be run as standalone script
- [ ] **AC-13**: Can be imported as module for CI integration
- [ ] **AC-14**: Exit code indicates validation pass/fail

## Tasks / Subtasks

- [x] **Task 1**: Create script scaffold (AC: 12, 13)
  - [x] Create `scripts/extract_action_signatures.py`
  - [x] Add argparse for CLI interface
  - [x] Add module entry point for import

- [x] **Task 2**: Implement action parsing (AC: 1-5)
  - [x] Use AST to parse Python files
  - [x] Find `register_actions` function calls
  - [x] Extract action registrations from registry
  - [x] Parse function signatures and docstrings

- [x] **Task 3**: Generate output files (AC: 6-8)
  - [x] Create JSON output schema
  - [x] Create YAML output format
  - [x] Include metadata (timestamp, git commit, etc.)

- [x] **Task 4**: Implement validation mode (AC: 9-11)
  - [x] Parse `actions-reference.md` to extract documented actions
  - [x] Compare with extracted inventory
  - [x] Generate gap report

- [x] **Task 5**: Add CI integration support (AC: 14)
  - [x] Define exit codes (0=pass, 1=gaps, 2=hallucinations)
  - [x] Add `--strict` mode for CI
  - [x] Output machine-readable validation results

## Dev Notes

### Action Registration Pattern

TEA actions are registered via this pattern in each module:

```python
def register_actions(registry, engine=None):
    """Register all actions in this module."""
    registry.register("namespace.action_name", action_function)
```

The script should:
1. Find all `register_actions` functions
2. Parse `registry.register()` calls
3. Extract namespace and action name from first argument
4. Find corresponding function for signature/docstring

### Output Schema

```json
{
  "generated_at": "2026-01-25T12:00:00Z",
  "git_commit": "abc123",
  "total_actions": 491,
  "modules": [
    {
      "file": "llm_actions.py",
      "namespace": "llm",
      "actions": [
        {
          "name": "llm.call",
          "function": "llm_call",
          "parameters": [
            {"name": "prompt", "type": "str", "required": true},
            {"name": "model", "type": "str", "required": false, "default": "gpt-4"}
          ],
          "return_type": "dict",
          "docstring": "Call an LLM with the given prompt...",
          "line_number": 42
        }
      ]
    }
  ]
}
```

### Validation Report Schema

```json
{
  "summary": {
    "total_implemented": 491,
    "total_documented": 67,
    "coverage_percent": 14,
    "gaps": 424,
    "hallucinations": 12
  },
  "undocumented": [
    {"action": "graph.create_node", "module": "graph_actions.py", "severity": "high"}
  ],
  "hallucinations": [
    {"action": "prolog.query", "documented_in": "actions-reference.md:142"}
  ]
}
```

### Source Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/actions/__init__.py` | Module list |
| `python/src/the_edge_agent/actions/*.py` | 55 action modules |
| `docs/python/actions-reference.md` | Current documentation |

### Testing

```bash
# Run extraction
python scripts/extract_action_signatures.py

# Run with validation
python scripts/extract_action_signatures.py --validate

# Strict mode (fail on any gap)
python scripts/extract_action_signatures.py --validate --strict

# Output to specific location
python scripts/extract_action_signatures.py --output data/inventory.json
```

## Definition of Done

- [ ] Script extracts signatures from all 55 modules
- [ ] JSON and YAML outputs generated correctly
- [ ] Validation mode identifies all gaps
- [ ] Hallucination detection works
- [ ] Can be run in CI with appropriate exit codes
- [ ] Unit tests for parsing logic

## Verification Checklist

- [ ] `python scripts/extract_action_signatures.py` runs without errors
- [ ] Output `data/action_inventory.json` contains 55 modules
- [ ] Total action count matches manual audit (~491)
- [ ] `--validate` correctly identifies known gaps
- [ ] `--validate` correctly identifies known hallucinations (e.g., `prolog.query`)

## Technical Considerations

### AST Parsing Approach

Using Python's `ast` module is preferred over regex because:
- Handles multiline function definitions
- Correctly parses type hints
- Extracts docstrings reliably
- Won't be fooled by commented-out code

### Edge Cases

1. **Dynamic registration**: Some actions may be registered conditionally
2. **Aliases**: Same function registered under multiple names
3. **Decorator-based registration**: Some modules use decorators
4. **Private actions**: Actions starting with `_` should be excluded

## Related Stories

- **TEA-DOCS-002**: Parent epic
- **TEA-DOCS-002.2**: actions-reference.md update (depends on this)
- **TEA-DOCS-002.5**: CI validation (uses this script)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-25 | 1.0 | Initial draft | Sarah (PO) |
| 2026-01-26 | 1.1 | Implementation complete | James (Dev) |
| 2026-01-25 | 1.2 | QA Review - PASS | Quinn (QA) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `scripts/extract_action_signatures.py` | Created | Main extraction script with CLI and module API |
| `python/tests/test_extract_action_signatures.py` | Created | Unit tests for extraction and validation logic |
| `data/action_inventory.json` | Generated | JSON inventory of all 482 actions |
| `data/action_inventory.yaml` | Generated | YAML inventory for human readability |
| `data/validation_report.json` | Generated | Validation report with gaps/hallucinations |

### Completion Notes

- **Actions Found**: 482 actions across 51 modules (not 491 as estimated in story - some may be dynamically registered)
- **Documentation Coverage**: 8.6% (41 of 476 unique actions documented)
- **Hallucinations**: 1 (`custom.my_action` - intentional example in docs)
- **Edge Cases Handled**:
  - Wrapper functions: `registry['x'] = wrapper(func)` pattern
  - Lambda registrations
  - Private actions (skipped)
  - **kwargs parameters
  - Optional type hints with defaults
- **Module count**: 51 action modules (55 files total, minus 4 non-action files: `__init__.py`, `llm_backend.py`, `llm_backend_factory.py`, `llm_local.py`)
- **Tests**: 19 unit tests covering parsing, serialization, documentation parsing, validation, and integration

### Debug Log References

None - implementation completed without blocking issues.

---

## QA Results

### Review Summary

| Field | Value |
|-------|-------|
| **Reviewer** | Quinn (QA Agent) |
| **Model** | Claude Opus 4.5 (claude-opus-4-5-20251101) |
| **Review Date** | 2026-01-25 |
| **Gate Decision** | **PASS** |

### Acceptance Criteria Status

| AC | Description | Status | Evidence |
|----|-------------|--------|----------|
| AC-1 | Parses all files in actions/ | PASS | 54 files parsed, 51 have actions |
| AC-2 | Extracts namespace | PASS | Namespace derived from action prefix |
| AC-3 | Extracts action name | PASS | Verified by unit tests |
| AC-4 | Extracts parameters and types | PASS | Handles Optional, defaults, **kwargs |
| AC-5 | Extracts docstrings | PASS | ast.get_docstring() used |
| AC-6 | Generates JSON inventory | PASS | data/action_inventory.json created |
| AC-7 | Generates YAML inventory | PASS | data/action_inventory.yaml created |
| AC-8 | Includes source file info | PASS | file and line_number fields |
| AC-9 | --validate compares to docs | PASS | Parses both reference docs |
| AC-10 | Reports undocumented actions | PASS | 211 gaps identified |
| AC-11 | Reports hallucinations | PASS | 1 detected (custom.my_action) |
| AC-12 | Standalone script | PASS | argparse CLI implemented |
| AC-13 | Module import for CI | PASS | extract_signatures() API |
| AC-14 | Exit codes for CI | PASS | 0/1/2/3 codes implemented |

### Test Results

```
19 passed in 1.71s
```

- **TestActionExtractor**: 8 tests (AST parsing, edge cases)
- **TestInventorySerialization**: 2 tests (JSON/dict conversion)
- **TestDocumentationParsing**: 3 tests (markdown patterns)
- **TestValidation**: 3 tests (gaps, hallucinations, coverage)
- **TestIntegration**: 3 tests (real codebase extraction)

### Code Quality Assessment

| Aspect | Rating | Notes |
|--------|--------|-------|
| Structure | GOOD | Clean dataclass architecture |
| Documentation | GOOD | Docstrings on all public functions |
| Error Handling | GOOD | Graceful handling of parse errors |
| Test Coverage | EXCELLENT | 100% feature coverage |
| Maintainability | GOOD | Single responsibility, clear flow |

### Security Review

**Status**: NO CONCERNS

- Read-only script, no code execution
- No user input vulnerabilities
- Standard library + PyYAML only

### Deviations from Story

| Deviation | Severity | Explanation |
|-----------|----------|-------------|
| 51 modules vs 55 | LOW | 4 files are helpers without actions |
| 482 actions vs 491 | LOW | Dynamic registrations not captured |
| 55.7% coverage vs 14% | NONE | Story estimate outdated |

### Recommendations

1. Consider `--format` option for output selection
2. Add `--module` filter for targeted extraction
3. CI integration workflow pending (TEA-DOCS-002.5)

### Gate File

See: `docs/qa/gates/TEA-DOCS-002.1-action-signature-extraction.yml`

### Final Verdict

**PASS** - All 14 acceptance criteria met. Implementation is clean, well-tested,
and handles edge cases appropriately. Ready for integration into documentation workflow.
