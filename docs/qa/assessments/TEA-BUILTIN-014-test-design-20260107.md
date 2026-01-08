# Test Design: Story TEA-BUILTIN-014

Date: 2026-01-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 35
- Unit tests: 24 (69%)
- Integration tests: 9 (26%)
- E2E tests: 2 (5%)
- Priority distribution: P0: 20, P1: 10, P2: 5

## Rationale

This story implements a CLI wrapper for semantic search functionality. The architecture follows the existing shell provider pattern, making it predominantly unit-testable. Integration tests verify the CLI interaction and JSON parsing, while minimal E2E tests validate real-world usage scenarios.

**Key test strategy decisions:**
- Heavy unit test coverage due to pure logic in parameter handling, error detection, and output structuring
- Integration tests for subprocess execution and CLI output parsing (real semtools required)
- Minimal E2E tests for YAML workflow integration (validates action registration and state management)
- Security focus: No dangerous operations but subprocess execution requires validation testing
- Performance: CLI timeout scenarios must be tested

## Test Scenarios by Acceptance Criteria

### AC-1: semtools.search action
**Requirement:** Executes SemTools `search` command and returns structured results

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-001 | Unit | P0 | Action function exists with correct signature | Pure function signature validation |
| TEA-BUILTIN-014-INT-001 | Integration | P0 | Executes semtools CLI with correct parameters | CLI interaction requires subprocess |
| TEA-BUILTIN-014-INT-002 | Integration | P0 | Parses and structures CLI JSON output | Real CLI output format validation |

### AC-2: Query Parameter
**Requirement:** Accepts `query` string for semantic search

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-002 | Unit | P0 | Query string passed to CLI command | String handling logic |
| TEA-BUILTIN-014-UNIT-003 | Unit | P1 | Query with special characters escaped | Edge case validation |
| TEA-BUILTIN-014-UNIT-004 | Unit | P1 | Empty query string returns error | Input validation |

### AC-3: Files Parameter
**Requirement:** Accepts `files` as string (glob) or list of file paths

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-005 | Unit | P0 | String files parameter converted to list | Type handling logic |
| TEA-BUILTIN-014-UNIT-006 | Unit | P0 | List files parameter passed through | Type handling logic |
| TEA-BUILTIN-014-INT-003 | Integration | P0 | Glob pattern expands to file list | Filesystem interaction |
| TEA-BUILTIN-014-UNIT-007 | Unit | P1 | Empty files list returns error | Input validation |
| TEA-BUILTIN-014-INT-004 | Integration | P1 | Non-existent glob pattern handled gracefully | Filesystem interaction |

### AC-4: Max Distance
**Requirement:** Supports `max_distance` (0.0-1.0) to filter by similarity threshold

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-008 | Unit | P0 | Max distance default value applied | Default parameter logic |
| TEA-BUILTIN-014-UNIT-009 | Unit | P0 | Max distance passed to CLI command | Parameter transformation |
| TEA-BUILTIN-014-UNIT-010 | Unit | P1 | Max distance out of range (>1.0) returns error | Input validation |
| TEA-BUILTIN-014-UNIT-011 | Unit | P1 | Max distance negative returns error | Input validation |

### AC-5: N Results
**Requirement:** Supports `n_results` to limit number of matches returned

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-012 | Unit | P0 | N results limits output array | Array slicing logic |
| TEA-BUILTIN-014-UNIT-013 | Unit | P1 | N results default value applied | Default parameter logic |
| TEA-BUILTIN-014-UNIT-014 | Unit | P2 | N results zero returns empty array | Edge case handling |

### AC-6: N Lines Context
**Requirement:** Supports `n_lines` to include surrounding context lines

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-015 | Unit | P1 | N lines passed to CLI command when >0 | Conditional parameter logic |
| TEA-BUILTIN-014-UNIT-016 | Unit | P1 | N lines omitted from CLI when 0 (default) | Conditional parameter logic |

### AC-7: Structured Output
**Requirement:** Returns `{success, matches: [{file, line, score, text, context}], query, total_matches}`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-017 | Unit | P0 | Output includes success boolean | Output structure validation |
| TEA-BUILTIN-014-UNIT-018 | Unit | P0 | Output includes matches array | Output structure validation |
| TEA-BUILTIN-014-UNIT-019 | Unit | P0 | Output includes query string | Output structure validation |
| TEA-BUILTIN-014-UNIT-020 | Unit | P0 | Output includes total_matches count | Output structure validation |
| TEA-BUILTIN-014-INT-005 | Integration | P0 | Match objects have correct structure | Real CLI output parsing |

### AC-8: Similarity Score
**Requirement:** Each match includes `score` (cosine similarity, higher = more similar)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-INT-006 | Integration | P0 | Match score field present and numeric | Real CLI output parsing |
| TEA-BUILTIN-014-UNIT-021 | Unit | P1 | Score used for best_match calculation | Sort/max logic validation |

### AC-9: Best Match Helper
**Requirement:** Returns `best_match` field with highest-scoring result

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-022 | Unit | P0 | Best match is highest score | Max/sort algorithm |
| TEA-BUILTIN-014-UNIT-023 | Unit | P1 | Best match is None when no matches | Edge case handling |

### AC-10: Contains Match Helper
**Requirement:** Returns `has_matches` boolean for easy conditional routing

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-024 | Unit | P0 | has_matches true when matches exist | Boolean logic |
| TEA-BUILTIN-014-UNIT-025 | Unit | P0 | has_matches false when no matches | Boolean logic |

### AC-11: Prerequisite Check
**Requirement:** Validates `semtools` CLI is installed before execution

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-026 | Unit | P0 | shutil.which("semtools") called | Prerequisite check logic |
| TEA-BUILTIN-014-INT-007 | Integration | P0 | Returns error when semtools not in PATH | Real environment validation |

### AC-12: Helpful Error
**Requirement:** If not installed, returns error with installation instructions

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-027 | Unit | P0 | Error includes install_hint field | Error structure validation |
| TEA-BUILTIN-014-UNIT-028 | Unit | P0 | Error includes npm install command | Error message content |

### AC-13: Version Check
**Requirement:** Optionally validates minimum version (v1.5.1+)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-029 | Unit | P2 | Version check implementation (future) | Optional feature - deferred to P2 |

### AC-14: File Not Found
**Requirement:** Gracefully handles missing files with clear error message

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-INT-008 | Integration | P1 | Non-existent file returns error | Real filesystem interaction |
| TEA-BUILTIN-014-UNIT-030 | Unit | P1 | Error message identifies missing file | Error message validation |

### AC-15: Empty Results
**Requirement:** Returns empty matches array (not error) when no semantic matches found

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-031 | Unit | P0 | Empty JSON output returns empty matches | Edge case handling |
| TEA-BUILTIN-014-UNIT-032 | Unit | P0 | success=True when no matches | Correct success semantics |

### AC-16: CLI Error Passthrough
**Requirement:** Captures and returns stderr from semtools CLI

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-INT-009 | Integration | P0 | stderr included in error response | Real subprocess error handling |
| TEA-BUILTIN-014-UNIT-033 | Unit | P1 | Non-zero return code sets success=False | Error detection logic |

### AC-17: Dual Namespace
**Requirement:** Actions accessible as `semtools.search` and `actions.semtools_search`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-E2E-001 | E2E | P0 | YAML uses semtools.search syntax | Action registration validation |
| TEA-BUILTIN-014-E2E-002 | E2E | P1 | actions.semtools_search also works | Namespace validation |

### AC-18: Shell Execution
**Requirement:** Uses subprocess for CLI execution (similar to shell provider pattern)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-BUILTIN-014-UNIT-034 | Unit | P0 | subprocess.run called with correct args | Subprocess invocation validation |
| TEA-BUILTIN-014-UNIT-035 | Unit | P1 | Timeout parameter set to 60s | Timeout configuration |

### AC-19: Documentation
**Requirement:** Updated YAML_REFERENCE.md with semtools action examples

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| (Manual Review) | Documentation | P1 | YAML_REFERENCE.md includes semtools.search | Documentation completeness check |

## Risk Coverage

**Primary Risks Identified:**

1. **RISK-001: CLI Not Installed**
   - Covered by: TEA-BUILTIN-014-UNIT-026, TEA-BUILTIN-014-INT-007, TEA-BUILTIN-014-UNIT-027, TEA-BUILTIN-014-UNIT-028
   - Mitigation: Clear error with installation instructions

2. **RISK-002: CLI Output Format Changes**
   - Covered by: TEA-BUILTIN-014-INT-002, TEA-BUILTIN-014-INT-005, TEA-BUILTIN-014-INT-006
   - Mitigation: Integration tests with real CLI, version pinning documentation

3. **RISK-003: Subprocess Timeout**
   - Covered by: TEA-BUILTIN-014-UNIT-035
   - Mitigation: 60s timeout with configurable parameter

4. **RISK-004: Malformed Input Parameters**
   - Covered by: TEA-BUILTIN-014-UNIT-003, TEA-BUILTIN-014-UNIT-004, TEA-BUILTIN-014-UNIT-007, TEA-BUILTIN-014-UNIT-010, TEA-BUILTIN-014-UNIT-011
   - Mitigation: Comprehensive input validation tests

## Recommended Execution Order

1. **P0 Unit tests (fail fast on logic errors)**
   - Parameter validation (UNIT-002, 005, 006, 008, 009)
   - Output structure (UNIT-017, 018, 019, 020)
   - Error handling (UNIT-026, 027, 028, 031, 032)
   - Helper functions (UNIT-022, 024, 025)
   - Subprocess invocation (UNIT-034)

2. **P0 Integration tests (CLI interaction)**
   - CLI execution (INT-001)
   - JSON parsing (INT-002, INT-005, INT-006)
   - Prerequisite check (INT-007)
   - Error passthrough (INT-009)

3. **P0 E2E tests (YAML integration)**
   - Action registration (E2E-001)

4. **P1 tests**
   - Edge cases (UNIT-003, 004, 007, 010, 011, 013, 015, 016, 021, 023, 030, 033, 035)
   - Integration edge cases (INT-004, INT-008)
   - Dual namespace (E2E-002)

5. **P2 tests (if time permits)**
   - Optional features (UNIT-014, UNIT-029)

## Test Implementation Notes

### Unit Test Setup

```python
# Mock dependencies
@pytest.fixture
def mock_subprocess():
    with patch('subprocess.run') as mock:
        yield mock

@pytest.fixture
def mock_which():
    with patch('shutil.which') as mock:
        mock.return_value = '/usr/local/bin/semtools'
        yield mock

@pytest.fixture
def mock_glob():
    with patch('glob.glob') as mock:
        yield mock
```

### Integration Test Requirements

```python
# Mark integration tests
@pytest.mark.integration
@pytest.mark.skipif(not shutil.which('semtools'),
                   reason='semtools CLI not installed')
def test_real_cli_execution():
    # Requires actual semtools installation
    pass
```

### E2E Test Requirements

```python
# Test YAML workflow with semtools action
@pytest.mark.e2e
def test_yaml_workflow():
    yaml_content = """
    name: test_semtools
    nodes:
      - name: search
        uses: semtools.search
        with:
          query: "test"
          files: "*.md"
    """
    # Load and execute YAML agent
```

## Coverage Gaps Analysis

**No significant gaps identified.** All 19 acceptance criteria have dedicated test coverage.

**Additional coverage recommendations:**
1. Performance testing for large file sets (future story)
2. Concurrent execution safety (if parallel workflows use semtools)
3. Memory usage profiling for large result sets

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Test Maintenance Considerations

**Low maintenance burden:**
- Unit tests stable (pure logic)
- Integration tests depend on CLI output format (document expected format)
- E2E tests minimal (only action registration)

**Change indicators:**
- If semtools CLI output format changes: Update INT-002, INT-005, INT-006
- If action registration pattern changes: Update E2E-001, E2E-002
- If new parameters added: Add corresponding unit/integration tests

## Estimated Test Execution Time

- Unit tests: ~5 seconds (all 24 tests)
- Integration tests: ~15 seconds (requires CLI execution)
- E2E tests: ~10 seconds (YAML loading)
- **Total: ~30 seconds** (excellent for CI/CD)

## Success Metrics

- [ ] All P0 tests passing (100% required)
- [ ] All P1 tests passing (95% required)
- [ ] P2 tests best effort
- [ ] Integration tests stable with real semtools CLI
- [ ] E2E tests validate YAML workflow execution
- [ ] No flaky tests (0 tolerance)
- [ ] Test suite completes in <60 seconds
