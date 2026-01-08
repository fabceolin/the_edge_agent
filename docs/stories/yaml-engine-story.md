# User Story: YAML-Based Agent Configuration Engine

## Status

Done


## Metadata

| Field | Value |
|-------|-------|
| **Story ID** | YAML-001 |
| **Title** | YAML-Based Agent Configuration Engine |
| **Status** | Ready for Review |
| **Priority** | High |
| **Story Points** | 8 |
| **Labels** | `feature`, `yaml-engine`, `developer-experience` |
| **Branch** | `yaml-engine` |

---

## Story

**As a** workflow developer
**I want to** define agent workflows using declarative YAML configuration files
**So that** I can create, modify, and share agent workflows without writing Python code

---

## Description

Implement a YAML engine that allows declarative definition of StateGraph workflows, inspired by GitHub Actions and GitLab CI/CD pipelines. This enables non-programmers to create agents and makes workflow configuration portable, version-controllable, and inspectable.

### Benefits
- **Declarative**: Define what you want, not how to build it
- **Portable**: Configuration can be version-controlled and shared
- **Accessible**: Non-programmers can create and modify agents
- **Inspectable**: Easy to understand workflow at a glance

---

## Acceptance Criteria

### Core YAML Engine
- [x] Create `YAMLEngine` class in `src/the_edge_agent/yaml_engine.py`
- [x] Support loading configuration from YAML files via `load_from_file()`
- [x] Support loading configuration from dictionaries via `load_from_dict()`
- [x] Parse and apply global `variables` section
- [x] Parse `state_schema` section for graph state definition
- [x] Parse `config` section for compilation options (raise_exceptions, interrupt_before, interrupt_after)

### Node Definition Support
- [x] Support inline Python code via `run:` string
- [x] Support inline Python code via `script:` (GitLab CI style)
- [x] Support built-in actions via `uses:` with `with:` parameters
- [x] Support multi-step nodes via `steps:` array (GitHub Actions style)
- [x] Support expression evaluation via `run.type: expression`
- [x] Support fan-in nodes via `fan_in: true` flag
- [x] Support `output:` key for action result mapping

### Edge Definition Support
- [x] Support simple edges: `from:` → `to:`
- [x] Support entry point: `from: __start__`
- [x] Support finish point: `to: __end__`
- [x] Support parallel edges: `type: parallel` with `fan_in:` target
- [x] Support conditional edges with `condition.type: expression`
- [x] Support simple `when:` clause (syntactic sugar for conditions)
- [x] Support negation in when clauses (`!variable`)

### Template Variable Processing
- [x] Process `{{ state.key }}` for state access
- [x] Process `{{ variables.key }}` for global variables
- [x] Process `${{ secrets.key }}` for secrets
- [x] Process `${ }` style (GitLab CI)
- [x] Support filters: `| json`, `| upper`, `| lower`

### Built-in Actions Registry
- [x] Implement `llm.call` action for LLM integration
- [x] Implement `http.get` action for GET requests
- [x] Implement `http.post` action for POST requests
- [x] Implement `file.write` action for file output
- [x] Implement `file.read` action for file input
- [x] Implement `actions.notify` action for notifications
- [x] Support custom action registration via `actions_registry` parameter

### Package Integration
- [x] Export `YAMLEngine` from `src/the_edge_agent/__init__.py`
- [x] Add to `__all__` exports

---

## Tasks

> **Note:** Implementation files already exist. Tasks focus on validation, fixing, and testing.

### Task 1: Validate and Fix Existing Implementation
- [x] Verify `yaml_engine.py` implements all acceptance criteria
- [x] Fix YAML syntax errors in example files (f-string/YAML conflicts)
- [x] Ensure `pyyaml` is in `setup.py` dependencies
- [x] Test import: `from the_edge_agent import YAMLEngine`

### Task 2: Create Test Suite
- [x] Create `tests/test_yaml_engine.py`
- [x] Implement P0 tests from QA test design (12 scenarios)
- [x] Implement P1 tests from QA test design (22 scenarios)
- [x] Implement security tests SEC-001 through SEC-004
- [x] Implement E2E tests for all 3 example YAML files

### Task 3: Verify Examples Execute Successfully
- [x] Run `yaml_agent_example.yaml` via `run_yaml_agent.py`
- [x] Run `yaml_customer_support_example.yaml` via `run_yaml_agent.py`
- [x] Run `yaml_perplexity_example.yaml` via `run_yaml_agent.py` (mocked)
- [x] Verify all conditional routing paths work correctly

### Task 4: Documentation Updates
- [x] Verify `docs/YAML_AGENTS.md` is complete and accurate
- [x] Update `CLAUDE.md` with YAML engine section

### Task 5: Final Validation
- [x] Run full test suite: `pytest tests/test_yaml_engine.py -v`
- [x] Verify no breaking changes to existing StateGraph API
- [x] All acceptance criteria checkboxes marked complete

---

## Technical Notes

### Dependencies
- Requires `pyyaml` package
- Uses existing `StateGraph`, `START`, `END` from `stategraph.py`

### Key Implementation Details
- Inline code execution uses `exec()` with prepared globals
- Auto-imports common modules (requests, datetime, OpenAI) when referenced
- Template processing uses regex substitution
- Condition functions use closures with captured expressions
- Actions receive `state` as first parameter plus `**kwargs`

### File Structure
```
src/the_edge_agent/
├── __init__.py          # Add YAMLEngine export
├── stategraph.py        # Existing
└── yaml_engine.py       # NEW - ~533 lines
```

---

## Examples to Include

### 1. Simple Research Agent (`examples/yaml_agent_example.yaml`)
- Demonstrates: search → validate → summarize → format → save flow
- Features: inline code, expression nodes, multi-step nodes, built-in actions, conditional edges

### 2. Customer Support Agent (`examples/yaml_customer_support_example.yaml`)
- Demonstrates: intent classification → routing → specialized handlers → response/escalation
- Features: multi-path conditional routing, interrupt_after for debugging

### 3. Perplexity Research Agent (`examples/yaml_perplexity_example.yaml`)
- Demonstrates: query refinement → API search → report formatting → display → save
- Features: external API integration, multi-step formatting

### 4. Runner Script (`examples/run_yaml_agent.py`)
- CLI for running YAML agents
- Support for custom YAML files with JSON state input

---

## Documentation Deliverables

- [ ] Create `docs/YAML_AGENTS.md` comprehensive documentation
  - Overview and benefits
  - Basic structure reference
  - Node types with examples
  - Edge types with examples
  - Template variables syntax
  - Built-in actions reference
  - Complete workflow examples
  - Troubleshooting guide
  - Comparison with GitHub Actions

- [ ] Update `CLAUDE.md` with YAML engine context

---

## Definition of Done

1. All acceptance criteria checked
2. `YAMLEngine` successfully parses all example YAML files
3. Example agents execute correctly via `run_yaml_agent.py`
4. Documentation is comprehensive and accurate
5. Code integrates cleanly with existing StateGraph architecture
6. No breaking changes to existing Python API

---

## Usage Examples

### Python API
```python
from the_edge_agent import YAMLEngine

# Load and run agent
engine = YAMLEngine()
graph = engine.load_from_file("my_agent.yaml")

# Execute
for event in graph.stream({"input": "hello"}):
    if event["type"] == "final":
        print(event["state"])
```

### Command Line
```bash
python -m the_edge_agent.yaml_engine my_agent.yaml '{"input": "hello"}'
```

---

## Related Files

| File | Purpose |
|------|---------|
| `src/the_edge_agent/yaml_engine.py` | Core YAML engine implementation |
| `src/the_edge_agent/__init__.py` | Package exports |
| `docs/YAML_AGENTS.md` | Comprehensive documentation |
| `CLAUDE.md` | Project context for Claude Code |
| `examples/yaml_agent_example.yaml` | Simple research agent example |
| `examples/yaml_customer_support_example.yaml` | Multi-path routing example |
| `examples/yaml_perplexity_example.yaml` | External API integration example |
| `examples/run_yaml_agent.py` | CLI runner script |

---

## Test Scenarios

1. **Load simple YAML** - Verify basic node/edge parsing
2. **Template substitution** - Verify `{{ state.x }}` replacement works
3. **Conditional routing** - Verify expression-based edge routing
4. **Parallel execution** - Verify fan-out/fan-in with YAML config
5. **Built-in actions** - Verify file.write, http.get work correctly
6. **Multi-step nodes** - Verify steps execute sequentially
7. **Custom actions** - Verify custom action registration works

---

## QA Results

### Test Design Review - 2025-12-05

**Reviewer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 47 |
| Unit Tests | 28 (60%) |
| Integration Tests | 14 (30%) |
| E2E Tests | 5 (10%) |

#### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 12 | Security-critical, data integrity, core parsing |
| P1 | 22 | Core functionality, main user flows |
| P2 | 10 | Secondary features, edge cases |
| P3 | 3 | Nice-to-have scenarios |

#### Security Findings

**CONCERN - Code Execution Risk:**

The implementation uses `exec()` and `eval()` for:
- Inline Python code execution (`run:`, `script:`)
- Expression evaluation (`run.type: expression`)
- Template processing
- Conditional edge evaluation

**Risk Level:** HIGH - Current design trusts YAML authors completely. No sandboxing in place.

**Recommendation:** Document that YAML files should only be loaded from trusted sources. Consider adding optional sandboxing for untrusted YAML in future iterations.

4 security-specific test scenarios added (SEC-001 through SEC-004) to document attack surface.

#### Coverage Assessment

- All 16 acceptance criteria groups have mapped test scenarios
- No coverage gaps identified
- All example YAML files have E2E test coverage

#### Test Design Artifact

```
docs/qa/assessments/YAML-001-test-design-20251205.md
```

#### Gate Status

| Check | Status |
|-------|--------|
| All ACs have test coverage | PASS |
| Test levels appropriate | PASS |
| Priority alignment | PASS |
| Security review | PASS |

**Overall:** PASS - Functional test design complete. Security implications documented in `docs/YAML_AGENTS.md` (Security Notice section).

#### Security Documentation Added (2025-12-05)

Security notice added to documentation covering:
- Trust model (YAML = executable code)
- Attack surface (exec/eval, file system, network)
- Comparison with GitHub Actions isolation model
- Safe usage guidelines

---

### Review Date: 2025-12-05

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: PASS** - The YAML Engine implementation is well-structured, comprehensive, and follows good Python practices. The codebase demonstrates clean separation of concerns, proper error handling patterns, and thorough test coverage.

**Strengths:**
- Clean, modular architecture with `YAMLEngine` class properly encapsulating all functionality
- DotDict helper class enables intuitive template variable access (`state.key` vs `state["key"]`)
- Comprehensive test suite with 57 tests covering P0, P1, security, and E2E scenarios
- Good use of closures for creating node functions dynamically
- Built-in actions provide sensible defaults while allowing extensibility
- Documentation is thorough and includes GitHub Actions comparison table

**Code Highlights:**
- `yaml_engine.py:172-218` - Inline function creation with proper return statement handling
- `yaml_engine.py:380-434` - Template processing with filter support
- `yaml_engine.py:294-378` - Edge configuration handling with multiple edge types

### Refactoring Performed

No refactoring was performed during this review. The implementation is clean and follows project conventions.

### Compliance Check

- Coding Standards: ✓ Code follows Python conventions, uses type hints, has docstrings
- Project Structure: ✓ Files in correct locations, exports properly configured
- Testing Strategy: ✓ 57 tests with proper P0/P1/E2E coverage and security tests
- All ACs Met: ✓ All 16 acceptance criteria groups verified complete

### Improvements Checklist

[All items are recommendations for future consideration, not blockers]

- [ ] Consider adding `__slots__` to `DotDict` for memory efficiency in high-volume scenarios
- [ ] Add optional timeout parameter to `http.get`/`http.post` actions
- [ ] Consider adding retry logic for `llm.call` action (with exponential backoff)
- [ ] Add type stubs (.pyi) for better IDE support
- [ ] Consider adding schema validation for YAML files (using jsonschema or similar)

### Security Review

**Status: CONCERNS (Documented)**

The implementation correctly documents the security model in `docs/YAML_AGENTS.md`:
- Uses `exec()` and `eval()` for code execution (inherent to the design)
- Template processing uses `eval()` without sandboxing
- YAML files have full access to Python runtime, file system, and network

**Mitigations in place:**
- Clear security notice in documentation
- Comparison with GitHub Actions to set expectations
- Safe usage guidelines provided

**Risk Level:** MEDIUM - Acceptable for trusted YAML sources; documented appropriately.

Tests `SEC-001` through `SEC-003` document the attack surface explicitly, which is the correct approach for this design.

### Performance Considerations

No performance issues identified. The implementation:
- Uses lazy imports for `requests`, `datetime`, `OpenAI` (only imported when referenced)
- Compiles graphs once, then reuses for multiple executions
- Template processing uses regex, which is efficient for typical use cases

### Files Modified During Review

None - no modifications made.

### Gate Status

Gate: PASS → docs/qa/gates/YAML-001-yaml-engine.yml

### Recommended Status

✓ Ready for Done

All acceptance criteria are met, tests pass (57 YAML engine + 43 StateGraph = 100 total), documentation is comprehensive, and security implications are properly documented. The implementation is ready for production use with the documented trust model.

---

## Dev Agent Record

> **Instructions:** This section is maintained by the dev agent during implementation.

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- None

### Completion Notes
- Task 1: Fixed pyyaml dependency, inline code execution with return statements, template variable dot notation, YAML syntax errors in examples
- Task 2: Created comprehensive test suite with 57 tests (P0, P1, security, E2E)
- Task 3: Verified all 3 example YAML files execute with all routing paths working
- Task 4: Verified documentation complete, added YAML Engine section to CLAUDE.md
- Task 5: Full regression passed (100 tests), no breaking changes to StateGraph API

### File List
| File | Status | Notes |
|------|--------|-------|
| `src/the_edge_agent/yaml_engine.py` | Modified | Added DotDict class, fixed inline function execution |
| `src/the_edge_agent/__init__.py` | Existing | Exports YAMLEngine (verified) |
| `setup.py` | Modified | Added pyyaml>=6.0 dependency |
| `examples/yaml_agent_example.yaml` | Modified | Fixed multi-line f-string YAML syntax |
| `examples/yaml_customer_support_example.yaml` | Existing | Verified working |
| `examples/yaml_perplexity_example.yaml` | Modified | Fixed multi-line f-string YAML syntax |
| `examples/run_yaml_agent.py` | Existing | CLI runner (verified) |
| `docs/YAML_AGENTS.md` | Existing | Documentation (verified complete) |
| `CLAUDE.md` | Modified | Added YAML Engine section |
| `tests/test_yaml_engine.py` | New | 57 test scenarios |

### Change Log
| Date | Change | Files |
|------|--------|-------|
| 2025-12-05 | Added pyyaml dependency | setup.py |
| 2025-12-05 | Added DotDict for template dot notation access | yaml_engine.py |
| 2025-12-05 | Fixed inline code execution with return statements | yaml_engine.py |
| 2025-12-05 | Fixed multi-line f-string YAML syntax | yaml_agent_example.yaml, yaml_perplexity_example.yaml |
| 2025-12-05 | Created comprehensive test suite | tests/test_yaml_engine.py |
| 2025-12-05 | Added YAML Engine section to project docs | CLAUDE.md |
