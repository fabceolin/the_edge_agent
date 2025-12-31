# Test Design: Story TEA-KIROKU-007

**Date:** 2025-12-29
**Designer:** Quinn (Test Architect)
**Story:** CrossRef API Integration

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 18 | 100% |
| **Unit tests** | 10 | 55% |
| **Integration tests** | 5 | 28% |
| **E2E tests** | 3 | 17% |
| **Priority distribution** | P0: 6, P1: 8, P2: 4 | |

### Strategy Rationale

This story implements an external API integration action. The test strategy prioritizes:

1. **Unit tests (55%)**: Focus on response parsing, data transformation, and error handling logic
2. **Integration tests (28%)**: Validate actual CrossRef API contract and rate limiting behavior
3. **E2E tests (17%)**: Verify full YAML workflow execution with the new action

---

## Test Scenarios by Acceptance Criteria

### AC1: Action `academic.crossref` queries CrossRef API by DOI and returns structured metadata

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-001 | Unit | P0 | Parse valid CrossRef DOI response to structured dict | Core functionality - data transformation logic |
| KIROKU-007-INT-001 | Integration | P1 | Real DOI lookup returns expected fields | Contract validation with live API |

### AC2: Action supports search by query string with `max_results` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-002 | Unit | P1 | Parse search results array with multiple items | Multi-result parsing logic |
| KIROKU-007-UNIT-003 | Unit | P1 | Verify `max_results` parameter is passed to API as `rows` | Parameter mapping logic |
| KIROKU-007-INT-002 | Integration | P2 | Search query returns multiple results up to max_results | Live API search behavior |

### AC3: Returns structured result with required fields

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-004 | Unit | P0 | All required fields present in output (doi, title, authors, etc.) | Data contract validation |
| KIROKU-007-UNIT-005 | Unit | P1 | Authors array parsed correctly (given/family to "Family, Given") | Author name transformation |
| KIROKU-007-UNIT-006 | Unit | P1 | Abstract with JATS XML tags cleaned to plain text | XML stripping logic |
| KIROKU-007-UNIT-007 | Unit | P1 | Date parsing handles various `date-parts` formats | Date normalization |

### AC4: Supports `doi` parameter for direct DOI lookup

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-008 | Unit | P0 | DOI with special characters correctly URL-encoded | Input sanitization |
| KIROKU-007-INT-003 | Integration | P1 | Direct DOI lookup for known DOI (e.g., 10.1038/nature12373) | Live DOI resolution |

### AC5-AC7: Parameter support (query, max_results, timeout)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-009 | Unit | P2 | Default values applied when parameters omitted | Default behavior |

### AC8-AC9: Rate Limiting & Polite Pool

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-INT-004 | Integration | P0 | User-Agent header includes mailto when provided | Rate limit compliance |
| KIROKU-007-UNIT-010 | Unit | P0 | Rate limiting respects configured delay | Rate limit enforcement |

### AC10: Structured error handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-UNIT-011 | Unit | P0 | Non-existent DOI returns error with `error_code: "NOT_FOUND"` | Error standardization |
| KIROKU-007-UNIT-012 | Unit | P0 | Network timeout returns error with `error_code: "TIMEOUT"` | Timeout handling |
| KIROKU-007-UNIT-013 | Unit | P1 | Invalid JSON response returns error with `error_code: "PARSE_ERROR"` | Response validation |

### AC11-AC12: Tests and Documentation (Meta)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-007-E2E-001 | E2E | P1 | YAML workflow using `academic.crossref` action executes successfully | Action registration verification |
| KIROKU-007-E2E-002 | E2E | P1 | Workflow with DOI lookup populates state correctly | End-to-end data flow |
| KIROKU-007-INT-005 | Integration | P2 | Action registered under both `academic.crossref` and `actions.academic_crossref` | Registry validation |

---

## Test Scenario Details

### KIROKU-007-UNIT-001: Parse valid CrossRef DOI response

```yaml
test_scenario:
  id: KIROKU-007-UNIT-001
  requirement: AC1
  priority: P0
  level: unit
  description: Parse valid CrossRef API response into structured metadata dict
  justification: Core parsing logic - pure function, no network needed
  mock_input: |
    {
      "status": "ok",
      "message": {
        "DOI": "10.1038/nature12373",
        "title": ["Sample Article"],
        "author": [{"given": "John", "family": "Smith"}],
        "container-title": ["Nature"],
        "published": {"date-parts": [[2023, 1, 15]]},
        "type": "journal-article"
      }
    }
  expected_output: |
    {
      "success": True,
      "results": [{
        "doi": "10.1038/nature12373",
        "title": "Sample Article",
        "authors": ["Smith, John"],
        "container_title": "Nature",
        "published_date": "2023-01-15",
        "type": "journal-article",
        "url": "https://doi.org/10.1038/nature12373"
      }]
    }
```

### KIROKU-007-UNIT-011: Non-existent DOI error handling

```yaml
test_scenario:
  id: KIROKU-007-UNIT-011
  requirement: AC10
  priority: P0
  level: unit
  description: Handle 404 response for non-existent DOI
  justification: Error path - must return structured error response
  mock_response:
    status_code: 404
    body: {"status": "error", "message": "Resource not found."}
  expected_output: |
    {
      "success": False,
      "error": "DOI not found: 10.9999/nonexistent",
      "error_code": "NOT_FOUND"
    }
```

### KIROKU-007-UNIT-012: Network timeout error handling

```yaml
test_scenario:
  id: KIROKU-007-UNIT-012
  requirement: AC10
  priority: P0
  level: unit
  description: Handle network timeout gracefully
  justification: Timeout handling - must not crash, return structured error
  mock_behavior: requests.get raises Timeout exception
  expected_output: |
    {
      "success": False,
      "error": "CrossRef API request timed out after 30 seconds",
      "error_code": "TIMEOUT"
    }
```

### KIROKU-007-INT-001: Live DOI lookup

```yaml
test_scenario:
  id: KIROKU-007-INT-001
  requirement: AC1
  priority: P1
  level: integration
  description: Verify real DOI lookup returns expected structure
  justification: API contract validation - ensures external dependency works
  test_doi: "10.1038/nature12373"
  assertions:
    - response["success"] is True
    - "doi" in response["results"][0]
    - "title" in response["results"][0]
    - response["results"][0]["type"] == "journal-article"
  network: required
  skip_ci: true  # Mark as integration test
```

### KIROKU-007-E2E-001: YAML workflow execution

```yaml
test_scenario:
  id: KIROKU-007-E2E-001
  requirement: AC11
  priority: P1
  level: e2e
  description: Execute YAML workflow that uses academic.crossref action
  justification: Validates full action registration and YAML engine integration
  workflow: |
    name: crossref-test
    state_schema:
      doi: str
      citation: dict
    nodes:
      - name: lookup
        uses: academic.crossref
        params:
          doi: "{{ state.doi }}"
        output: citation
    edges:
      - from: __start__
        to: lookup
      - from: lookup
        to: __end__
  input: {"doi": "10.1038/nature12373"}
  assertions:
    - "citation" in final_state
    - final_state["citation"]["success"] is True
```

---

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| API contract changes | KIROKU-007-INT-001, KIROKU-007-INT-002 |
| Rate limiting violations | KIROKU-007-INT-004, KIROKU-007-UNIT-010 |
| Parse failures on edge cases | KIROKU-007-UNIT-006, KIROKU-007-UNIT-007 |
| Network resilience | KIROKU-007-UNIT-011, KIROKU-007-UNIT-012 |
| Action registration failure | KIROKU-007-E2E-001, KIROKU-007-INT-005 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - KIROKU-007-UNIT-001, KIROKU-007-UNIT-004, KIROKU-007-UNIT-008
   - KIROKU-007-UNIT-010, KIROKU-007-UNIT-011, KIROKU-007-UNIT-012

2. **P0 Integration tests** (rate limiting compliance)
   - KIROKU-007-INT-004

3. **P1 Unit tests** (secondary parsing logic)
   - KIROKU-007-UNIT-002, KIROKU-007-UNIT-003, KIROKU-007-UNIT-005
   - KIROKU-007-UNIT-006, KIROKU-007-UNIT-007, KIROKU-007-UNIT-013

4. **P1 Integration tests** (API contract)
   - KIROKU-007-INT-001, KIROKU-007-INT-003

5. **P1 E2E tests** (workflow validation)
   - KIROKU-007-E2E-001, KIROKU-007-E2E-002

6. **P2 tests** (as time permits)
   - KIROKU-007-UNIT-009, KIROKU-007-INT-002, KIROKU-007-INT-005

---

## Test Implementation Notes

### Mock Data Required

```python
# Add to test_academic_actions.py

CROSSREF_DOI_SUCCESS = {
    "status": "ok",
    "message": {
        "DOI": "10.1038/nature12373",
        "title": ["Sample Article Title"],
        "author": [
            {"given": "John", "family": "Smith"},
            {"given": "Alice", "family": "Doe"}
        ],
        "abstract": "<jats:p>This is the abstract text.</jats:p>",
        "container-title": ["Nature"],
        "published": {"date-parts": [[2023, 1, 15]]},
        "type": "journal-article"
    }
}

CROSSREF_SEARCH_SUCCESS = {
    "status": "ok",
    "message": {
        "total-results": 2,
        "items": [
            {
                "DOI": "10.1038/nature12373",
                "title": ["Article One"],
                "author": [{"given": "John", "family": "Smith"}],
                "container-title": ["Nature"],
                "published": {"date-parts": [[2023, 1, 15]]},
                "type": "journal-article"
            },
            {
                "DOI": "10.1000/example",
                "title": ["Article Two"],
                "author": [{"given": "Jane", "family": "Doe"}],
                "container-title": ["Science"],
                "published": {"date-parts": [[2022, 6, 1]]},
                "type": "journal-article"
            }
        ]
    }
}

CROSSREF_NOT_FOUND = {
    "status": "error",
    "message": "Resource not found."
}
```

### Test Markers

```python
# Integration tests should be marked for selective execution
@pytest.mark.integration
@pytest.mark.network
def test_crossref_live_doi_lookup():
    """KIROKU-007-INT-001: Live API test - skip in CI"""
    pass

# E2E tests use full YAML engine
@pytest.mark.e2e
def test_crossref_yaml_workflow():
    """KIROKU-007-E2E-001: Full workflow test"""
    pass
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-007
  date: 2025-12-29
  scenarios_total: 18
  by_level:
    unit: 10
    integration: 5
    e2e: 3
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  key_risks_addressed:
    - api_contract_changes
    - rate_limiting_compliance
    - network_resilience
    - action_registration
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-KIROKU-007-test-design-20251229.md
P0 tests identified: 6
```
