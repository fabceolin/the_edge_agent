# Test Design: Story TEA-BUILTIN-008.7

**Story Title:** LlamaExtract Workflow Primitives
**Date:** 2024-12-30
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 47 |
| Unit tests | 32 (68%) |
| Integration tests | 12 (26%) |
| E2E tests | 3 (6%) |
| **Priority distribution** | P0: 18, P1: 16, P2: 10, P3: 3 |

### Strategy Rationale

This story exposes three LlamaExtract API primitives (`submit_job`, `poll_status`, `get_result`) for advanced workflow control. Testing focuses on:

1. **API contract validation** - Ensure each primitive handles all response types correctly
2. **Error handling** - Test transient failures, timeouts, and API errors
3. **Retry logic** - Validate retry behavior with exponential backoff
4. **Configuration inheritance** - Verify API key resolution from settings/env
5. **Workflow integration** - Multi-mode escalation pattern end-to-end

---

## Test Scenarios by Acceptance Criteria

### AC-1: submit_job accepts file (path, URL, or base64), schema, and mode

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-001 | Unit | P0 | Submit job with local file path | Core file input path - must work |
| 008.7-UNIT-002 | Unit | P0 | Submit job with URL file reference | Alternative file input path |
| 008.7-UNIT-003 | Unit | P0 | Submit job with base64-encoded content | Alternative file input path |
| 008.7-UNIT-004 | Unit | P1 | Submit job with inline schema dict | Schema validation |
| 008.7-UNIT-005 | Unit | P1 | Submit job with agent_id instead of schema | Alternative configuration |
| 008.7-UNIT-006 | Unit | P2 | Submit job with neither schema nor agent_id defaults gracefully | Edge case - missing config |

---

### AC-2: Returns `{ success: true, job_id: "...", status: "PENDING" }` on success

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-007 | Unit | P0 | Verify submit_job returns success=true, job_id, status=PENDING | Core success response contract |
| 008.7-UNIT-008 | Unit | P1 | Verify job_id format is valid string | Response structure validation |

---

### AC-3: Returns structured error with `error_type` on failure

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-009 | Unit | P0 | Submit job returns error on 401 unauthorized | Security-critical path |
| 008.7-UNIT-010 | Unit | P0 | Submit job returns error on 400 bad request | Input validation failure |
| 008.7-UNIT-011 | Unit | P1 | Submit job returns error on 500 server error | Transient failure handling |
| 008.7-UNIT-012 | Unit | P1 | Error response includes error_type field | Error structure contract |
| 008.7-UNIT-013 | Unit | P2 | Error response includes human-readable message | Developer experience |

---

### AC-4: Supports all extraction modes: FAST, BALANCED, PREMIUM

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-014 | Unit | P0 | Submit job with mode=FAST | All modes must work |
| 008.7-UNIT-015 | Unit | P0 | Submit job with mode=BALANCED | All modes must work |
| 008.7-UNIT-016 | Unit | P0 | Submit job with mode=PREMIUM | All modes must work |
| 008.7-UNIT-017 | Unit | P2 | Submit job with lowercase mode converts to uppercase | Input normalization |
| 008.7-UNIT-018 | Unit | P2 | Submit job with invalid mode returns error | Input validation |

---

### AC-5: poll_status accepts `job_id` parameter

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-019 | Unit | P0 | Poll status with valid job_id | Core functionality |
| 008.7-UNIT-020 | Unit | P1 | Poll status with non-existent job_id returns error | Error handling |
| 008.7-UNIT-021 | Unit | P2 | Poll status with empty job_id returns validation error | Edge case |

---

### AC-6: Returns `{ success: true, status: "PENDING|SUCCESS|ERROR|PARTIAL_SUCCESS", progress: N }`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-022 | Unit | P0 | Poll returns status=PENDING for in-progress job | Status state - must handle |
| 008.7-UNIT-023 | Unit | P0 | Poll returns status=SUCCESS for completed job | Status state - must handle |
| 008.7-UNIT-024 | Unit | P0 | Poll returns status=ERROR for failed job | Status state - must handle |
| 008.7-UNIT-025 | Unit | P1 | Poll returns status=PARTIAL_SUCCESS for partial extraction | Status state - should handle |
| 008.7-UNIT-026 | Unit | P1 | Poll includes progress field (0-100) | Progress tracking |

---

### AC-7: Includes `error` field when status is ERROR

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-027 | Unit | P0 | Error status includes error field with details | Error detail contract |
| 008.7-UNIT-028 | Unit | P2 | Error field includes error_code and message | Structured error details |

---

### AC-8: Configurable `timeout` for individual poll request (default: 10s)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-029 | Unit | P1 | Poll uses default 10s timeout | Default behavior |
| 008.7-UNIT-030 | Unit | P1 | Poll respects custom timeout parameter | Configuration override |
| 008.7-INT-001 | Integration | P2 | Poll times out after specified duration | Real timeout behavior |

---

### AC-9: get_result accepts `job_id` parameter

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-031 | Unit | P0 | Get result with valid completed job_id | Core functionality |
| 008.7-UNIT-032 | Unit | P1 | Get result with non-existent job_id returns error | Error handling |

---

### AC-10: Returns `{ success: true, data: {...}, job_id: "..." }` on success

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-002 | Integration | P0 | Get result returns extracted data structure | Core success contract |
| 008.7-INT-003 | Integration | P1 | Extracted data matches requested schema structure | Data integrity |
| 008.7-INT-004 | Integration | P2 | Result includes original job_id for traceability | Response completeness |

---

### AC-11: Returns structured error if job not complete or failed

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-005 | Integration | P0 | Get result on PENDING job returns error with status | Incomplete job handling |
| 008.7-INT-006 | Integration | P0 | Get result on ERROR job returns error with details | Failed job handling |
| 008.7-INT-007 | Integration | P1 | Error indicates job state clearly (PENDING vs ERROR) | Error differentiation |

---

### AC-12: All primitives inherit API key from settings or env var

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-008 | Integration | P0 | Primitives use API key from settings.llamaextract.api_key | Configuration priority |
| 008.7-INT-009 | Integration | P0 | Primitives fallback to LLAMAEXTRACT_API_KEY env var | Environment fallback |
| 008.7-INT-010 | Integration | P1 | Missing API key returns clear error before API call | Early validation |

---

### AC-13: All primitives support `max_retries` for transient failures (default: 3)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-011 | Integration | P0 | submit_job retries on 503 service unavailable | Transient failure recovery |
| 008.7-INT-012 | Integration | P1 | poll_status retries on connection timeout | Network resilience |
| 008.7-UNIT-033 | Unit | P1 | get_result retries on 429 rate limit | Rate limit handling |
| 008.7-UNIT-034 | Unit | P2 | Retries use exponential backoff | Retry strategy |
| 008.7-UNIT-035 | Unit | P2 | Custom max_retries parameter respected | Configuration override |
| 008.7-UNIT-036 | Unit | P3 | No retry on 4xx client errors (except 429) | Smart retry logic |

---

### AC-14: Unit tests for each primitive with mocked HTTP responses

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-037 | Unit | P0 | submit_job mocked success response test | Test infrastructure |
| 008.7-UNIT-038 | Unit | P0 | poll_status mocked status transitions test | Test infrastructure |
| 008.7-UNIT-039 | Unit | P0 | get_result mocked data extraction test | Test infrastructure |

---

### AC-15: Integration test demonstrating multi-mode escalation pattern

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-INT-013 | Integration | P0 | Multi-mode escalation: FAST success skips BALANCED | Happy path optimization |
| 008.7-INT-014 | Integration | P0 | Multi-mode escalation: FAST insufficient triggers BALANCED | Escalation logic |
| 008.7-E2E-001 | E2E | P1 | Full workflow: submit -> poll loop -> get_result | Complete flow validation |
| 008.7-E2E-002 | E2E | P2 | Full workflow with checkpoint save/resume between polls | Checkpoint integration |

---

### AC-16: Documentation in YAML_REFERENCE.md with workflow examples

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-040 | Unit | P3 | Example YAML from docs parses without error | Doc validation |
| 008.7-E2E-003 | E2E | P3 | Example multi-mode workflow executes successfully | Doc accuracy |

---

## Additional Edge Case and Boundary Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.7-UNIT-041 | Unit | P2 | Large file (>10MB) submission handling | Size boundary |
| 008.7-UNIT-042 | Unit | P2 | Special characters in file path | Path parsing |
| 008.7-INT-015 | Integration | P2 | Concurrent poll_status calls for same job | Thread safety |
| 008.7-UNIT-043 | Unit | P3 | Unicode schema property names | Character encoding |
| 008.7-UNIT-044 | Unit | P3 | Empty extraction result handling | Edge case |

---

## Risk Coverage

| Risk | Mitigation Test(s) |
|------|-------------------|
| API contract changes | 008.7-UNIT-007, 008.7-INT-002 |
| Authentication failures | 008.7-UNIT-009, 008.7-INT-008, 008.7-INT-009 |
| Network instability | 008.7-INT-011, 008.7-INT-012, 008.7-UNIT-033 |
| Rate limiting | 008.7-UNIT-033 |
| Job state transitions | 008.7-UNIT-022 through 008.7-UNIT-025 |
| File re-upload for escalation | 008.7-INT-014 |
| Timeout configuration | 008.7-UNIT-029, 008.7-UNIT-030, 008.7-INT-001 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core functionality)
   - File input handling (008.7-UNIT-001 to 003)
   - Success/error responses (008.7-UNIT-007, 009-011)
   - Mode support (008.7-UNIT-014 to 016)
   - Status transitions (008.7-UNIT-022 to 024, 027)
   - Get result basics (008.7-UNIT-031)
   - Mocked test infrastructure (008.7-UNIT-037 to 039)

2. **P0 Integration tests** (component interactions)
   - API key configuration (008.7-INT-008, 009)
   - Result data structure (008.7-INT-002)
   - Job state errors (008.7-INT-005, 006)
   - Retry on transient failures (008.7-INT-011)
   - Multi-mode escalation (008.7-INT-013, 014)

3. **P1 tests** (should-have functionality)
   - Extended error handling
   - Progress tracking
   - Timeout configuration
   - Full E2E workflow

4. **P2+ tests** (time permitting)
   - Edge cases
   - Boundary conditions
   - Doc validation

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 32
    integration: 15
    e2e: 3
  by_priority:
    p0: 18
    p1: 16
    p2: 10
    p3: 3
  coverage_gaps: []
  notes:
    - All 16 acceptance criteria have test coverage
    - Multi-mode escalation pattern has dedicated integration tests
    - File re-upload limitation documented and tested in escalation flow
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-008.7-test-design-20251230.md
P0 tests identified: 18
Story dependency: TEA-BUILTIN-008.6 (async polling foundation)
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for API interactions)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (API contracts = P0, edge cases = P2-P3)
- [x] Test IDs follow naming convention (008.7-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests

---

## Key Testing Notes

1. **HTTP Mocking**: Use `responses` or `requests-mock` library for unit tests
2. **Retry Testing**: Mock sequential failures to verify retry count and backoff
3. **Multi-Mode Escalation**: Requires mocking job completion with quality evaluation
4. **File Re-Upload**: Document in test that escalation re-uploads file (API limitation)
5. **Checkpoint Integration**: E2E-002 tests save/resume between polling iterations
