# Test Design: Story TEA-RELEASE-004.6

Date: 2026-01-08
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 6 (21%)
- **Integration tests:** 12 (43%)
- **E2E tests:** 10 (36%)
- **Priority distribution:** P0: 4, P1: 14, P2: 8, P3: 2

## Story Analysis

This is a **documentation-only story** for LLM-bundled distributions. The acceptance criteria focus on:

1. Documentation accuracy and completeness
2. Example YAML workflow correctness
3. Cross-platform configuration coverage
4. User journey clarity (decision flowcharts)

**Key Testing Challenge:** Documentation cannot be unit tested directly. Tests must verify:
- Documentation exists and is properly structured
- Example YAML files are syntactically valid and executable
- Links and references are not broken
- Configuration examples match implementation

## Test Scenarios by Acceptance Criteria

### AC-1: Installation docs updated with LLM AppImage instructions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-001 | Integration | P1 | Verify `docs/installation.md` contains "LLM-Bundled Distributions" section | Validates documentation structure exists |
| 004.6-INT-002 | Integration | P1 | Verify download links follow correct naming pattern (`tea-{variant}-llm-{version}-{arch}.AppImage`) | Ensures users can find correct assets |
| 004.6-UNIT-001 | Unit | P2 | Parse installation.md and extract all external URLs for validation | Link rot detection |

### AC-2: Decision flowchart helps users choose distribution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-003 | Integration | P1 | Verify Mermaid flowchart syntax in installation.md is valid | Ensures flowchart renders correctly |
| 004.6-E2E-001 | E2E | P2 | Render installation.md in markdown viewer and verify flowchart displays | Visual validation of decision tree |

### AC-3: Example YAML workflows in `examples/llm/`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-UNIT-002 | Unit | P0 | Validate `examples/llm/local-chat.yaml` is syntactically valid YAML | Ensures examples are parseable |
| 004.6-UNIT-003 | Unit | P0 | Validate `examples/llm/local-embed.yaml` is syntactically valid YAML | Ensures examples are parseable |
| 004.6-UNIT-004 | Unit | P0 | Validate `examples/llm/local-rag.yaml` is syntactically valid YAML | Ensures examples are parseable |
| 004.6-INT-004 | Integration | P1 | Load `local-chat.yaml` with YAMLEngine and verify graph compiles | Validates workflow structure |
| 004.6-INT-005 | Integration | P1 | Load `local-embed.yaml` with YAMLEngine and verify graph compiles | Validates workflow structure |
| 004.6-INT-006 | Integration | P1 | Load `local-rag.yaml` with YAMLEngine and verify graph compiles | Validates workflow structure |

### AC-4: WASM deployment guide with COOP/COEP requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-007 | Integration | P1 | Verify `docs/wasm/llm-deployment.md` exists and contains COOP/COEP section | Critical security headers documented |
| 004.6-INT-008 | Integration | P2 | Verify nginx configuration example is syntactically valid | Server config correctness |
| 004.6-INT-009 | Integration | P2 | Verify Apache configuration example is syntactically valid | Server config correctness |

### AC-5: Model path configuration documented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-010 | Integration | P1 | Verify model path configuration options documented for Linux, macOS, Windows | Cross-platform coverage |
| 004.6-UNIT-005 | Unit | P2 | Validate environment variable syntax examples (`TEA_MODEL_PATH`, etc.) | Correct env var usage |

### AC-6, AC-7, AC-8: Example YAML workflows (detailed execution)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-E2E-002 | E2E | P0 | Execute `local-chat.yaml` with mock LLM backend and verify output schema | Validates example works end-to-end |
| 004.6-E2E-003 | E2E | P1 | Execute `local-embed.yaml` with mock embedding backend | Validates embedding workflow |
| 004.6-E2E-004 | E2E | P1 | Execute `local-rag.yaml` with mock backends | Validates RAG workflow pipeline |

### AC-9: Examples work with bundled AppImage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-E2E-005 | E2E | P1 | Run examples against LLM AppImage (requires CI with AppImage artifact) | Validates documentation matches reality |
| 004.6-E2E-006 | E2E | P2 | Verify examples auto-detect model path in AppImage environment | Model path resolution |

### AC-10, AC-11, AC-12: Actions reference documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-011 | Integration | P1 | Verify `docs/python/actions-reference.md` contains `llm.call` with local backend params | Python docs complete |
| 004.6-INT-012 | Integration | P1 | Verify `docs/rust/actions-reference.md` contains `llm.call` with local backend params | Rust docs complete |
| 004.6-UNIT-006 | Unit | P2 | Validate parameter tables in actions reference have consistent columns | Documentation formatting |

### AC-13: YAML settings reference for `llm` section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-E2E-007 | E2E | P1 | Parse YAML reference and verify `llm` settings section matches implementation schema | Settings docs accurate |
| 004.6-E2E-008 | E2E | P2 | Verify documented default values match actual implementation defaults | Prevents doc/code drift |

## Additional Test Scenarios (Cross-Cutting)

### Documentation Consistency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-E2E-009 | E2E | P1 | Verify all internal links in LLM docs resolve to existing files | Broken link detection |
| 004.6-E2E-010 | E2E | P3 | Spell check all new documentation files | Quality polish |

### README Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-013 | Integration | P3 | Verify README.md contains "Offline LLM" section with link to installation docs | Entry point coverage |

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Documentation out of sync with implementation | 004.6-E2E-002, 004.6-E2E-005, 004.6-E2E-007, 004.6-E2E-008 | E2E tests validate docs match code |
| Example YAML files invalid | 004.6-UNIT-002, 004.6-UNIT-003, 004.6-UNIT-004, 004.6-INT-004, 004.6-INT-005, 004.6-INT-006 | Unit + integration validation |
| Broken links | 004.6-E2E-009, 004.6-UNIT-001 | Automated link checking |
| Missing cross-platform coverage | 004.6-INT-010 | Explicit platform checks |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on YAML syntax):
   - 004.6-UNIT-002, 004.6-UNIT-003, 004.6-UNIT-004

2. **P0 E2E test** (validate core example works):
   - 004.6-E2E-002

3. **P1 Integration tests** (structure validation):
   - 004.6-INT-001 through 004.6-INT-012

4. **P1 E2E tests** (workflow execution):
   - 004.6-E2E-003, 004.6-E2E-004, 004.6-E2E-005, 004.6-E2E-007, 004.6-E2E-009

5. **P2 tests as time permits**

6. **P3 tests in full regression only**

## Test Implementation Recommendations

### Unit Tests (Python)

```python
# tests/test_docs_llm_examples.py
import pytest
import yaml
from pathlib import Path

EXAMPLES_DIR = Path("examples/llm")

@pytest.mark.parametrize("yaml_file", [
    "local-chat.yaml",
    "local-embed.yaml",
    "local-rag.yaml",
])
def test_yaml_syntax_valid(yaml_file):
    """004.6-UNIT-002/003/004: Validate YAML syntax"""
    path = EXAMPLES_DIR / yaml_file
    with open(path) as f:
        data = yaml.safe_load(f)
    assert "name" in data
    assert "nodes" in data
    assert "edges" in data
```

### Integration Tests (Python)

```python
# tests/test_docs_llm_integration.py
import pytest
from the_edge_agent import YAMLEngine
from pathlib import Path

def test_local_chat_compiles():
    """004.6-INT-004: Verify local-chat.yaml compiles"""
    engine = YAMLEngine.from_file("examples/llm/local-chat.yaml")
    graph = engine.compile()
    assert graph is not None
    assert "generate_response" in graph.nodes
```

### E2E Tests (with mock LLM)

```python
# tests/e2e/test_docs_llm_e2e.py
import pytest
from the_edge_agent import YAMLEngine
from unittest.mock import patch

def test_local_chat_executes():
    """004.6-E2E-002: Execute local-chat.yaml with mock"""
    engine = YAMLEngine.from_file("examples/llm/local-chat.yaml")

    with patch("the_edge_agent.actions.llm.call") as mock_call:
        mock_call.return_value = {"answer": "Test response"}
        result = engine.invoke({"question": "What is 2+2?"})

    assert "answer" in result
```

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 6
    integration: 12
    e2e: 10
  by_priority:
    p0: 4
    p1: 14
    p2: 8
    p3: 2
  coverage_gaps: []
  special_considerations:
    - Documentation-only story - tests focus on validation not functional behavior
    - AppImage integration tests require CI artifact availability
    - Mock LLM backend needed for E2E tests without real model
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.6-test-design-20260108.md
P0 tests identified: 4
P1 tests identified: 14
```
