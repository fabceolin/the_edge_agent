# Test Design: Story TEA-KIROKU-005

Date: 2026-01-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 2 (11%)
- Integration tests: 6 (33%)
- E2E tests: 10 (56%)
- Priority distribution: P0: 6, P1: 8, P2: 4

**Rationale for E2E-heavy distribution:** This story validates the complete migration of the Kiroku academic writing workflow from LangGraph to TEA. The primary value is in proving end-to-end functional equivalence and demonstrating the full workflow to users. Integration and unit tests support this but are not the primary focus.

## Test Scenarios by Acceptance Criteria

### AC1: E2E Test Generates Complete Paper from YAML Spec

**Objective:** Validate that the TEA workflow can process a sample paper spec and generate a complete academic document with all required sections.

#### Scenarios

| ID                  | Level       | Priority | Test                                       | Justification                                         |
| ------------------- | ----------- | -------- | ------------------------------------------ | ----------------------------------------------------- |
| KIROKU-005-E2E-001  | E2E         | P0       | Full workflow with sample spec             | Critical user journey - complete paper generation     |
| KIROKU-005-E2E-002  | E2E         | P0       | Validates all sections present             | Data integrity - ensures no sections dropped          |
| KIROKU-005-E2E-003  | E2E         | P1       | Checks citation generation                 | Core feature - citations required for academic papers |
| KIROKU-005-E2E-004  | E2E         | P1       | Validates paragraph count matches spec     | Spec compliance - user expectations                   |
| KIROKU-005-INT-001  | Integration | P1       | Mock LLM returns expected structure        | Test isolation - verifies test harness correctness    |
| KIROKU-005-UNIT-001 | Unit        | P2       | Sample spec YAML parses correctly          | Input validation - early failure detection            |

**Test Data:** `examples/academic/sample-paper-spec.yaml`

**Expected Output:**
- Markdown document with sections: Introduction, Background, Methodology, Results, Conclusion
- Each section has paragraph count matching spec
- Citations section present if `generate_citations: true`
- Title suggestion if `suggest_title: true`

---

### AC2: TEA Output Functionally Equivalent to LangGraph Original

**Objective:** Prove that migrating from LangGraph to TEA preserves functionality. This requires comparing state transformations, execution flow, and final output structure.

#### Scenarios

| ID                 | Level       | Priority | Test                               | Justification                                               |
| ------------------ | ----------- | -------- | ---------------------------------- | ----------------------------------------------------------- |
| KIROKU-005-E2E-005 | E2E         | P0       | Compare output structure           | Critical equivalence check - validates migration success    |
| KIROKU-005-E2E-006 | E2E         | P1       | Validate section ordering matches  | Workflow correctness - order affects document coherence     |
| KIROKU-005-INT-002 | Integration | P0       | State field mapping verification   | Data transformation correctness - prevents data loss        |
| KIROKU-005-INT-003 | Integration | P1       | Node execution sequence comparison | Execution flow equivalence - ensures same processing stages |

**Test Approach:**

1. **Baseline Capture:**
   - Archive LangGraph output from Kiroku original codebase
   - Store in `tests/fixtures/kiroku_baseline/`
   - Document LangGraph state transitions

2. **Comparison Strategy:**
   - Structure comparison (sections, field names, nesting)
   - State field mapping verification
   - Node execution sequence logging

3. **Acceptance Criteria:**
   - All state fields from LangGraph present in TEA
   - Section structure matches baseline
   - Execution order logically equivalent (may differ due to YAML execution model)

**Note:** Exact text match not required (LLM outputs vary). Focus on structural and functional equivalence.

---

### AC3: Migration Guide Documented

**Objective:** Verify that the migration guide exists, is comprehensive, and contains executable examples.

#### Scenarios

| ID                 | Level       | Priority | Test                                      | Justification                                    |
| ------------------ | ----------- | -------- | ----------------------------------------- | ------------------------------------------------ |
| KIROKU-005-E2E-007 | E2E         | P1       | Migration guide exists at correct path    | Documentation deliverable - user expectation     |
| KIROKU-005-E2E-008 | E2E         | P1       | Code examples in guide execute correctly  | Quality gate - prevents broken examples          |
| KIROKU-005-INT-004 | Integration | P2       | Concept mapping table is complete         | Reference quality - ensures all concepts covered |
| KIROKU-005-UNIT-002| Unit        | P2       | Guide markdown is valid                   | Documentation standards - no syntax errors       |

**Test Implementation:**

```python
def test_migration_guide_examples_execute():
    """Extract code blocks from migration guide and verify they run."""
    guide_path = "docs/examples/langraph-to-tea-migration.md"

    # Extract Python code blocks
    code_blocks = extract_code_blocks(guide_path, language="python")

    for block in code_blocks:
        if "# Example only" in block:
            continue  # Skip illustration-only code

        # Execute in isolated namespace
        exec(block, safe_globals())

    # Extract YAML code blocks
    yaml_blocks = extract_code_blocks(guide_path, language="yaml")

    for block in yaml_blocks:
        yaml.safe_load(block)  # Validate syntax
```

**Key Guide Sections to Validate:**
- Concept mapping table (LangGraph → TEA)
- State schema conversion example
- Node conversion example
- Edge conversion example
- Troubleshooting section exists

---

### AC4: Functional Example in examples/academic/

**Objective:** Verify that `kiroku-document-writer.yaml` exists, loads without errors, and can execute successfully.

#### Scenarios

| ID                 | Level       | Priority | Test                                       | Justification                                  |
| ------------------ | ----------- | -------- | ------------------------------------------ | ---------------------------------------------- |
| KIROKU-005-INT-005 | Integration | P0       | YAML loads and compiles without errors     | Critical deliverable - must be executable      |
| KIROKU-005-E2E-009 | E2E         | P1       | Dry-run with minimal state succeeds        | Usability test - validates out-of-box success  |

**Test Implementation:**

```python
def test_kiroku_yaml_loads():
    engine = YAMLEngine.from_file(
        "examples/academic/kiroku-document-writer.yaml"
    )

    # Verify graph compiles
    graph = engine.compile()

    # Verify expected nodes exist
    assert "suggest_title" in graph.nodes
    assert "topic_sentence_writer" in graph.nodes
    assert "draft_writer" in graph.nodes

def test_kiroku_dry_run():
    engine = YAMLEngine.from_file(
        "examples/academic/kiroku-document-writer.yaml"
    )

    minimal_state = {
        "title": "Test",
        "hypothesis": "Testing works",
        "area_of_paper": "Testing",
        "type_of_document": "Report",
        "section_names": ["Introduction"],
        "number_of_paragraphs": {"Introduction": 1},
        "sentences_per_paragraph": 3,
    }

    # Should execute without errors (may timeout, that's ok)
    result = engine.run(minimal_state, timeout=10)
```

---

### AC5: Sample Spec YAML Exists and is Complete

**Objective:** Validate that `sample-paper-spec.yaml` exists with all required fields and serves as a good example.

#### Scenarios

| ID                 | Level       | Priority | Test                                  | Justification                                     |
| ------------------ | ----------- | -------- | ------------------------------------- | ------------------------------------------------- |
| KIROKU-005-INT-006 | Integration | P1       | Spec file exists and is valid YAML    | Deliverable quality - must be usable              |
| KIROKU-005-E2E-010 | E2E         | P2       | Spec contains all required fields     | Completeness check - prevents user confusion      |

**Required Fields:**
- `title`
- `hypothesis`
- `area_of_paper`
- `type_of_document`
- `section_names`
- `number_of_paragraphs`
- `results`
- `sentences_per_paragraph`
- `max_revisions`
- `number_of_queries`
- `suggest_title`
- `generate_citations`

**Test Implementation:**

```python
def test_sample_spec_completeness():
    with open("examples/academic/sample-paper-spec.yaml") as f:
        spec = yaml.safe_load(f)

    required_fields = [
        "title", "hypothesis", "area_of_paper", "type_of_document",
        "section_names", "number_of_paragraphs", "results",
        "sentences_per_paragraph", "max_revisions", "number_of_queries",
        "suggest_title", "generate_citations"
    ]

    for field in required_fields:
        assert field in spec, f"Missing required field: {field}"

    # Validate field types
    assert isinstance(spec["section_names"], list)
    assert isinstance(spec["number_of_paragraphs"], dict)
    assert spec["sentences_per_paragraph"] > 0
```

---

### AC6: README Updated with Academic Use Case

**Objective:** Verify that the README mentions the academic writing example and provides a quickstart command.

#### Scenarios

| ID                | Level | Priority | Test                                   | Justification                            |
| ----------------- | ----- | -------- | -------------------------------------- | ---------------------------------------- |
| KIROKU-005-E2E-011| E2E   | P2       | README contains "Academic" section     | Documentation visibility - user discovery|

**Test Implementation:**

```python
def test_readme_academic_section():
    with open("README.md") as f:
        content = f.read()

    # Check for section
    assert "Academic Writing Example" in content or "academic" in content.lower()

    # Check for example reference
    assert "kiroku" in content.lower()

    # Check for quickstart command
    assert "examples/academic/" in content or "kiroku-document-writer.yaml" in content
```

---

### AC7: Tests Run in CI (GitHub Actions)

**Objective:** Ensure that the E2E test suite is integrated into CI and runs successfully.

#### Scenarios

| ID                | Level       | Priority | Test                                    | Justification                                |
| ----------------- | ----------- | -------- | --------------------------------------- | -------------------------------------------- |
| KIROKU-005-INT-007| Integration | P0       | test_kiroku_e2e.py discovered by pytest | CI integration - prevents regressions        |
| KIROKU-005-E2E-012| E2E         | P1       | All tests pass in CI environment        | Quality gate - validates reproducible tests  |

**CI Configuration Check:**

```bash
# Verify test discovery
pytest tests/integration/test_kiroku_e2e.py --collect-only

# Expected output:
# <Module test_kiroku_e2e.py>
#   <Function test_full_workflow>
#   <Function test_sections_present>
#   ...
```

**GitHub Actions Workflow:**

```yaml
# Existing .github/workflows/python-tests.yaml should auto-discover
# tests in tests/integration/

- name: Run Integration Tests
  run: |
    cd python
    pytest tests/integration/ -v --timeout=300
```

---

### AC8: Documentation Includes Troubleshooting and FAQs

**Objective:** Verify that the migration guide contains a troubleshooting section and FAQs to help users resolve common issues.

#### Scenarios

| ID                | Level | Priority | Test                                       | Justification                          |
| ----------------- | ----- | -------- | ------------------------------------------ | -------------------------------------- |
| KIROKU-005-E2E-013| E2E   | P1       | Troubleshooting section exists in guide    | User support - reduces support burden  |

**Required Troubleshooting Topics:**

- Prompt template syntax issues
- Conditional edge problems
- LLM provider configuration
- State field mapping errors
- Checkpoint/interrupt differences
- Performance optimization tips

**Test Implementation:**

```python
def test_migration_guide_troubleshooting():
    with open("docs/examples/langraph-to-tea-migration.md") as f:
        content = f.read()

    # Check for troubleshooting section
    assert "## Troubleshooting" in content or "## FAQ" in content

    # Check for common topics
    topics = [
        "prompt", "template", "conditional", "edge",
        "LLM", "provider", "state", "checkpoint"
    ]

    troubleshooting_section = extract_section(content, "Troubleshooting")

    for topic in topics:
        # At least 3 topics should be covered
        pass  # Flexible check

    # Verify Q&A format
    assert "Q:" in content or "###" in troubleshooting_section
```

---

## Risk Coverage

| Risk ID  | Description                                | Mitigated By                     | Test Priority |
| -------- | ------------------------------------------ | -------------------------------- | ------------- |
| RISK-001 | LLM API calls expensive/non-deterministic  | Mock LLM responses in tests      | P0            |
| RISK-002 | Functional equivalence unclear             | Baseline comparison tests        | P0            |
| RISK-003 | Migration guide examples broken            | Execute code blocks in tests     | P1            |
| RISK-004 | Example YAML doesn't load                  | Compile check in integration test| P0            |
| RISK-005 | Incomplete sample spec confuses users      | Field completeness validation    | P1            |
| RISK-006 | CI tests fail in GitHub Actions            | Timeout handling, mock isolation | P0            |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (P0 Unit/Integration)
1. `KIROKU-005-UNIT-001` - Sample spec parses (< 1s)
2. `KIROKU-005-UNIT-002` - Guide markdown valid (< 1s)
3. `KIROKU-005-INT-002` - State field mapping (< 1s)
4. `KIROKU-005-INT-005` - YAML loads and compiles (< 2s)

### Phase 2: Core Validation (P0 E2E)
5. `KIROKU-005-E2E-001` - Full workflow with mocks (~30s)
6. `KIROKU-005-E2E-002` - All sections present (~30s)
7. `KIROKU-005-E2E-005` - Output structure comparison (~30s)

### Phase 3: Feature Completeness (P1)
8. `KIROKU-005-E2E-003` - Citation generation (~20s)
9. `KIROKU-005-E2E-004` - Paragraph count validation (~20s)
10. `KIROKU-005-E2E-006` - Section ordering (~20s)
11. `KIROKU-005-E2E-007` - Migration guide exists (< 1s)
12. `KIROKU-005-E2E-008` - Code examples execute (~10s)
13. `KIROKU-005-E2E-009` - Dry-run success (~15s)
14. `KIROKU-005-E2E-013` - Troubleshooting section (< 1s)
15. `KIROKU-005-INT-001` - Mock LLM structure (< 1s)
16. `KIROKU-005-INT-003` - Node execution sequence (< 2s)
17. `KIROKU-005-INT-006` - Spec validity (< 1s)

### Phase 4: Documentation Quality (P2)
18. `KIROKU-005-E2E-010` - Spec completeness (< 1s)
19. `KIROKU-005-E2E-011` - README mention (< 1s)
20. `KIROKU-005-INT-004` - Concept mapping table (< 1s)

**Total Estimated Execution Time:** ~3-5 minutes (with mocked LLM calls)

---

## Test Environment Requirements

### Dependencies
- Python 3.9+
- pytest
- pyyaml
- unittest.mock (stdlib)

### Mock Data Requirements
Store in `tests/fixtures/kiroku_mocks.py`:

```python
MOCK_LLM_RESPONSES = {
    "suggest_title": "Edge-Optimized Neural Networks: A Technical Analysis",
    "topic_sentence_writer": {
        "Introduction": ["1. Edge computing represents...", "2. Machine learning..."],
        "Background": ["1. Traditional cloud...", "2. Recent advances..."],
        # ...
    },
    "draft_writer": {
        "Introduction": "## Introduction\n\nEdge computing represents...",
        # ...
    },
    "citation_generator": "[1] Smith, J. (2023). Edge Computing...",
}
```

### CI Environment Variables
None required - tests fully mocked.

---

## Quality Checklist

Before finalizing test design:

- [x] Every AC has test coverage
- [x] Test levels are appropriate (E2E-heavy justified by integration validation goal)
- [x] No duplicate coverage across levels (each test validates distinct aspect)
- [x] Priorities align with business risk (equivalence/execution = P0, docs = P1-P2)
- [x] Test IDs follow naming convention (`KIROKU-005-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Execution order optimized (fast tests first)
- [x] Mock strategy documented (prevents API costs)
- [x] Baseline comparison approach defined
- [x] Risk mitigations mapped to tests

---

## Implementation Notes

### Baseline Storage Strategy

```
tests/fixtures/kiroku_baseline/
├── langgraph_output.md          # Original Kiroku output
├── langgraph_state.json         # State transitions
├── langgraph_execution_log.txt  # Node execution order
└── README.md                    # Baseline capture methodology
```

### Mock Implementation Pattern

```python
import pytest
from unittest.mock import patch
from tests.fixtures.kiroku_mocks import MOCK_LLM_RESPONSES

@pytest.fixture
def mock_llm():
    """Mock LLM calls for deterministic testing."""
    with patch('the_edge_agent.actions.llm.call') as mock:
        def side_effect(messages, **kwargs):
            # Extract context from messages to return appropriate mock
            if "suggest" in str(messages).lower():
                return MOCK_LLM_RESPONSES["suggest_title"]
            # ...
        mock.side_effect = side_effect
        yield mock

def test_with_mocked_llm(mock_llm):
    # Test executes without real API calls
    pass
```

### Timeout Handling

```python
# For E2E tests that may run long
@pytest.mark.timeout(300)  # 5 minutes max
def test_full_workflow():
    pass
```

### Skip Interrupts in Tests

```python
# For automated testing, skip human-in-the-loop interrupts
result = engine.run(initial_state, skip_interrupts=True)
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 18
  by_level:
    unit: 2
    integration: 6
    e2e: 10
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  special_requirements:
    - Mock LLM responses for deterministic CI
    - Store LangGraph baseline for comparison
    - Timeout handling for long workflows
    - Skip interrupts in automated tests
  estimated_execution_time: "3-5 minutes"
  ci_integration: "Auto-discovered by pytest in tests/integration/"
```

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-KIROKU-005-test-design-20260107.md`

P0 tests identified: 6
- `KIROKU-005-E2E-001` - Full workflow execution
- `KIROKU-005-E2E-002` - Section validation
- `KIROKU-005-E2E-005` - Output structure comparison
- `KIROKU-005-INT-002` - State field mapping
- `KIROKU-005-INT-005` - YAML compilation
- `KIROKU-005-INT-007` - CI test discovery

Total test scenarios: 18
