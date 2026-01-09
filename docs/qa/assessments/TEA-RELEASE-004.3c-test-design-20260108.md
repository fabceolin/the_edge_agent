# Test Design: Story TEA-RELEASE-004.3c

Date: 2026-01-08
Designer: Quinn (Test Architect)
Story: WASM LLM Release and Testing

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 6 (21%)
- **Integration tests:** 12 (43%)
- **E2E tests:** 10 (36%)
- **Priority distribution:** P0: 8, P1: 12, P2: 6, P3: 2

## Risk Assessment Summary

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| GitHub Release upload fails (>2GB) | High | Medium | Single 1.9GB file fits limit |
| WASM build failure | High | Low | wasm-pack well-tested |
| Browser compatibility (SharedArrayBuffer) | High | Medium | COOP/COEP headers required |
| Model initialization timeout | Medium | Medium | Use small test model in CI |
| IndexedDB quota exceeded | Medium | Low | Document 3GB requirement |
| CI timeout during model download | High | High | Cache model, use small test model |

## Test Scenarios by Acceptance Criteria

---

### AC-1: GitHub Actions workflow builds WASM package on release tag

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-UNIT-001 | Unit | P1 | Validate release tag format detection | Pure logic: regex matching `refs/tags/*` |
| 4.3c-INT-001 | Integration | P0 | Workflow triggers on release tag push | CI/CD critical path |
| 4.3c-INT-002 | Integration | P0 | wasm-pack build succeeds with `--target web --release` | Build correctness verification |
| 4.3c-INT-003 | Integration | P1 | TypeScript compilation produces valid output | Multi-step build integration |

**Given-When-Then:**

```gherkin
Given a tagged commit matching pattern v*.*.*
When the release workflow is triggered
Then the build-wasm-llm job should execute
And wasm-pack build should complete without errors
And pkg/ directory should contain .wasm and .js files
```

---

### AC-2: Workflow uploads `tea-wasm-llm-{version}.tar.gz` to GitHub Releases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-INT-004 | Integration | P0 | Tarball created with correct structure | Release artifact integrity |
| 4.3c-INT-005 | Integration | P0 | gh release upload succeeds | Release distribution |
| 4.3c-UNIT-002 | Unit | P2 | Version extracted from tag matches tarball name | Pure string manipulation |

**Given-When-Then:**

```gherkin
Given a successful WASM build
When the tarball creation step runs
Then tea-wasm-llm-{version}.tar.gz should be created
And should contain pkg/ and js/ directories
And gh release upload should succeed
```

---

### AC-3: Workflow uploads model file (~1.9GB) to GitHub Releases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-INT-006 | Integration | P0 | Model file download completes | Critical release asset |
| 4.3c-INT-007 | Integration | P1 | Model upload under 2GB limit succeeds | GitHub size constraint |
| 4.3c-UNIT-003 | Unit | P2 | Model file naming follows convention | Naming validation |

**Given-When-Then:**

```gherkin
Given the model download script runs
When the model file is verified
Then microsoft_Phi-4-mini-instruct-Q3_K_S.gguf should exist
And file size should be ~1.9GB (under 2GB limit)
And gh release upload should succeed
```

---

### AC-4: Workflow uploads `model-manifest.json` to GitHub Releases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-UNIT-004 | Unit | P1 | Manifest JSON schema validation | Data format correctness |
| 4.3c-INT-008 | Integration | P1 | Manifest matches actual model file | Integrity verification |

**Given-When-Then:**

```gherkin
Given the model file is present
When manifest.json is generated
Then it should contain correct model filename
And should contain correct sha256 hash
And should upload successfully
```

---

### AC-5: SHA256SUMS generated for all release assets

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-INT-009 | Integration | P0 | SHA256SUMS contains all assets | Integrity verification critical |
| 4.3c-UNIT-005 | Unit | P1 | SHA256 format validation | Checksum format correctness |

**Given-When-Then:**

```gherkin
Given all release assets are present
When sha256sum command runs
Then SHA256SUMS.wasm should be created
And should contain entry for tarball
And should contain entry for model file
And should contain entry for manifest.json
```

---

### AC-6: Playwright test loads package in headless browser

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-001 | E2E | P0 | Test server starts with COOP/COEP headers | Browser security requirements |
| 4.3c-E2E-002 | E2E | P0 | WASM package loads without errors | Basic functionality gate |
| 4.3c-INT-010 | Integration | P1 | Playwright config correctly targets test server | Test infrastructure |

**Given-When-Then:**

```gherkin
Given the test server is running on port 8080
When Playwright navigates to test page
Then Cross-Origin-Opener-Policy header should be 'same-origin'
And Cross-Origin-Embedder-Policy header should be 'require-corp'
And window.teaLlmLoaded should become true
```

---

### AC-7: Playwright test verifies model initialization

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-003 | E2E | P0 | initTeaLlm() completes with test model | Core initialization path |
| 4.3c-E2E-004 | E2E | P1 | Progress callback reports loading status | User feedback mechanism |
| 4.3c-E2E-005 | E2E | P2 | Initialization fails gracefully without model | Error handling |

**Given-When-Then:**

```gherkin
Given the WASM package is loaded
When initTeaLlm() is called with test model path
Then initialization should return 'initialized'
And onProgress callback should receive updates
And no console errors should appear
```

---

### AC-8: Playwright test executes simple LLM workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-006 | E2E | P0 | executeLlmYaml returns valid result | Core workflow execution |
| 4.3c-E2E-007 | E2E | P1 | Workflow error returns meaningful message | Error reporting |

**Given-When-Then:**

```gherkin
Given initTeaLlm() has completed
When executeLlmYaml() is called with valid YAML
Then result should have 'gen' property
And result should contain LLM output
```

---

### AC-9: Playwright test verifies IndexedDB caching

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-008 | E2E | P1 | Model stored in IndexedDB after init | Caching mechanism |
| 4.3c-E2E-009 | E2E | P1 | Second load uses cached model | Performance optimization |
| 4.3c-E2E-010 | E2E | P2 | Cache clear triggers re-download | Cache management |

**Given-When-Then:**

```gherkin
Given model initialization completes
When IndexedDB 'tea-llm-cache' is queried
Then 'models' store should have count > 0
And second page load should not trigger download
```

---

### AC-10: Package README with usage examples

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-UNIT-006 | Unit | P2 | README exists and contains required sections | Documentation completeness |
| 4.3c-INT-011 | Integration | P2 | Code examples in README are syntactically valid | Example correctness |

**Given-When-Then:**

```gherkin
Given rust/tea-wasm-llm/README.md exists
When content is analyzed
Then should contain Installation section
And should contain Usage section with code examples
And code examples should be syntactically valid JavaScript
```

---

### AC-11: COOP/COEP header requirements documented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-INT-012 | Integration | P1 | Nginx config example is valid | Server config correctness |

**Given-When-Then:**

```gherkin
Given README.md Server Configuration section exists
When nginx/Apache examples are extracted
Then nginx config should be syntactically valid
And Apache config should be syntactically valid
```

---

### AC-12: Package size breakdown documented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-MANUAL-001 | Manual | P3 | Size table matches actual release | Documentation accuracy |

**Given-When-Then:**

```gherkin
Given release assets are published
When sizes are measured
Then documented sizes should be within 10% of actual
```

---

### AC-13: Troubleshooting guide for common issues

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 4.3c-E2E-MANUAL-002 | Manual | P3 | Troubleshooting steps resolve documented issues | Support documentation |

**Given-When-Then:**

```gherkin
Given troubleshooting section exists
When SharedArrayBuffer error is encountered
Then following documented steps should resolve it
```

---

## Risk Coverage Matrix

| Risk | Test IDs | Mitigation Level |
|------|----------|------------------|
| GitHub Release upload fails | 4.3c-INT-005, 4.3c-INT-007 | Full |
| WASM build failure | 4.3c-INT-002 | Full |
| Browser compatibility | 4.3c-E2E-001, 4.3c-E2E-002 | Full |
| Model initialization timeout | 4.3c-E2E-003, 4.3c-E2E-004 | Full |
| IndexedDB quota exceeded | 4.3c-E2E-008 | Partial (documented) |
| CI timeout during model download | 4.3c-INT-006 | Full (cached model strategy) |

## Recommended Execution Order

### Phase 1: P0 Unit Tests (fail fast)
1. None at P0 level - logic is simple

### Phase 2: P0 Integration Tests
1. 4.3c-INT-001 - Workflow triggers on release tag
2. 4.3c-INT-002 - wasm-pack build succeeds
3. 4.3c-INT-004 - Tarball created correctly
4. 4.3c-INT-005 - Upload succeeds
5. 4.3c-INT-006 - Model download completes
6. 4.3c-INT-009 - SHA256SUMS generated

### Phase 3: P0 E2E Tests
1. 4.3c-E2E-001 - COOP/COEP headers set
2. 4.3c-E2E-002 - Package loads in browser
3. 4.3c-E2E-003 - Model initialization
4. 4.3c-E2E-006 - Workflow execution

### Phase 4: P1 Tests
1. All P1 integration tests
2. All P1 E2E tests

### Phase 5: P2+ Tests (as time permits)
1. P2 unit and integration tests
2. P3 manual verification

## Test Environment Requirements

### CI Environment (GitHub Actions)
- Ubuntu latest runner
- Rust toolchain with wasm32-unknown-unknown target
- Node.js 20+
- wasm-pack installed
- gh CLI available

### Browser Test Environment
- Node.js HTTP server with COOP/COEP headers
- Playwright with Chromium
- Small test model (~100MB or less) for fast CI
- IndexedDB support (all modern browsers)

### Test Fixtures Required
- `tests/e2e/fixtures/test-model.gguf` - Small quantized model for testing
- `tests/e2e/test.html` - Test harness HTML page
- `tests/server.js` - HTTP server with security headers

## Test Data Requirements

### YAML Workflow for Testing
```yaml
name: test-workflow
nodes:
  - name: gen
    action: llm.call
    params:
      prompt: "Say hello"
      max_tokens: 5
edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
```

## Automation Notes

### CI Integration
- Tests should run on PR and release branches
- Model download should be cached between runs
- Use small test model (~100MB) for faster CI execution
- Full model testing only on release tags

### Parallel Execution
- Unit tests: Full parallel
- Integration tests: Can parallelize except workflow trigger tests
- E2E tests: Serial (share browser state)

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
  story_id: "TEA-RELEASE-004.3c"
  scenarios_total: 28
  by_level:
    unit: 6
    integration: 12
    e2e: 10
  by_priority:
    p0: 8
    p1: 12
    p2: 6
    p3: 2
  coverage_gaps: []
  test_infrastructure_required:
    - playwright
    - test_http_server
    - small_test_model
  estimated_ci_time: "10-15 minutes"
```

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-RELEASE-004.3c-test-design-20260108.md`
P0 tests identified: 8
Total ACs covered: 13/13 (100%)
