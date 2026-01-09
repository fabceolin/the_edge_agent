# Test Design: Story TEA-RELEASE-004.3b

**Story:** WASM Model Loading and Caching (Phi-4-mini)
**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 10 (42%) |
| **Integration tests** | 9 (37%) |
| **E2E tests** | 5 (21%) |
| **Priority distribution** | P0: 8, P1: 10, P2: 6 |

### Strategy Rationale

This story focuses on browser-based model loading and IndexedDB caching. The test strategy emphasizes:

1. **Unit tests** for pure functions (checksum verification, manifest parsing, progress calculation)
2. **Integration tests** for IndexedDB operations and model loader interactions
3. **E2E tests** for critical user journeys (cache hit/miss flows, corrupted cache recovery)

**Key Risk:** IndexedDB has browser-specific storage limits (Safari ~1GB). The 1.9GB Phi-4-mini model may fail on Safari. Tests must verify graceful degradation.

---

## Test Scenarios by Acceptance Criteria

### AC-2: Model loads directly from single GGUF file (no reassembly)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-UNIT-001 | Unit | P0 | Validate `loadModel()` returns Uint8Array from fetch response | Core loading logic - pure function with mocked fetch |
| 004.3b-UNIT-002 | Unit | P1 | Verify correct buffer size matches manifest.totalSize | Data integrity validation - pure calculation |
| 004.3b-INT-001 | Integration | P0 | Load actual GGUF file from local HTTP server | Validates complete loading flow with real fetch |

**Given-When-Then:**
```gherkin
Given a valid model manifest with file "model.gguf"
When loadModel() is called with the manifest
Then a Uint8Array of size manifest.totalSize is returned
And no chunk reassembly operations occur
```

---

### AC-3: Model loads from bundled/local assets without external CDN requests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-INT-002 | Integration | P0 | Verify all fetch URLs are relative paths (no https://) | Security & offline capability - requires network inspection |
| 004.3b-UNIT-003 | Unit | P1 | Validate basePath is correctly prepended to file path | URL construction logic |

**Given-When-Then:**
```gherkin
Given modelBasePath is "./models"
When loadModel() is invoked
Then all network requests target "./models/{filename}"
And no requests are made to external domains
```

---

### AC-4: IndexedDB caching stores model after first load

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-INT-003 | Integration | P0 | After loadBundledModel(), verify IndexedDB contains model data | Core caching functionality - requires real IndexedDB |
| 004.3b-INT-004 | Integration | P1 | Verify stored data matches original Uint8Array | Data integrity in storage |
| 004.3b-UNIT-004 | Unit | P2 | Validate cacheModel() creates correct CachedModel structure | Object structure validation |

**Given-When-Then:**
```gherkin
Given an empty IndexedDB cache
And useCache is true (default)
When loadBundledModel() completes successfully
Then the model is stored in IndexedDB with version key
And timestamp is recorded
```

---

### AC-5: Subsequent page loads use cached model (skip download)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-001 | E2E | P0 | Load model twice, verify second load skips network fetch | Critical user experience - full browser behavior |
| 004.3b-INT-005 | Integration | P1 | getCachedModel() returns cached data for matching version | Cache retrieval logic |
| 004.3b-UNIT-005 | Unit | P1 | Verify loadBundledModel checks cache before fetching | Control flow logic |

**Given-When-Then:**
```gherkin
Given a model is already cached with version "v1"
When loadBundledModel() is called
Then getCachedModel("v1") returns the cached model
And no network requests are made for the model file
And console logs "Model loaded from cache"
```

---

### AC-6: Cache invalidation works when model version changes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-002 | E2E | P0 | Change manifest version, verify re-download occurs | Critical for model updates - full flow |
| 004.3b-INT-006 | Integration | P1 | getCachedModel() returns null for non-matching version | Version mismatch handling |
| 004.3b-UNIT-006 | Unit | P2 | Version comparison logic handles edge cases (v1 vs v1.0) | Edge case validation |

**Given-When-Then:**
```gherkin
Given a model is cached with version "v1"
And the manifest now specifies version "v2"
When loadBundledModel() is called
Then getCachedModel("v2") returns null
And the model is re-downloaded from source
And the new model is cached with version "v2"
```

---

### AC-7: model-manifest.json describes single file and SHA256 checksum

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-UNIT-007 | Unit | P1 | Validate manifest JSON schema (model, version, totalSize, file, sha256) | Schema validation - pure parsing |
| 004.3b-INT-007 | Integration | P1 | Parse and load manifest from HTTP endpoint | Real manifest fetch |

**Given-When-Then:**
```gherkin
Given a manifest.json file exists at "{basePath}/manifest.json"
When the manifest is fetched and parsed
Then it contains fields: model, version, totalSize, file, sha256
And sha256 is a valid 64-character hex string
And file ends with ".gguf"
```

---

### AC-8: Build script downloads model (no splitting needed)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-INT-008 | Integration | P1 | Execute download-model.sh, verify .gguf file created | Build tooling verification |
| 004.3b-UNIT-008 | Unit | P2 | Validate SHA256 calculation matches expected format | Checksum logic |

**Given-When-Then:**
```gherkin
Given the download-model.sh script exists
When the script is executed
Then "models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf" is created
And "models/manifest.json" is generated
And the manifest sha256 matches the actual file checksum
```

---

### AC-9: Total bundled size documented (~1.9GB model + ~50MB WASM = ~2GB total)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-UNIT-009 | Unit | P2 | Verify manifest.totalSize is within expected range (1.8GB - 2.0GB) | Size bounds validation |

**Given-When-Then:**
```gherkin
Given the model manifest is loaded
When manifest.totalSize is read
Then the value is between 1,800,000,000 and 2,000,000,000 bytes
```

---

### AC-10: Cache hit/miss behavior tested

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-003 | E2E | P0 | Measure and verify cache hit is >10x faster than cache miss | Performance validation - real timing |
| 004.3b-INT-009 | Integration | P1 | Log output distinguishes "loaded from cache" vs "loading from chunks" | Observability verification |

**Given-When-Then:**
```gherkin
Given timing instrumentation is enabled
When loadBundledModel() is called on cache miss
Then console logs "Loading model from chunks..."
And load time is recorded

When loadBundledModel() is called on cache hit
Then console logs "Model loaded from cache"
And load time is at least 10x faster than cache miss
```

---

### AC-11: Corrupted cache recovery works (re-download)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-004 | E2E | P0 | Corrupt cached data, verify automatic recovery | Critical resilience - full browser behavior |
| 004.3b-UNIT-010 | Unit | P1 | loadBundledModelSafe catches error and clears cache | Error handling logic |

**Given-When-Then:**
```gherkin
Given a corrupted model is stored in IndexedDB cache
When loadBundledModelSafe() is called
Then the initial load fails with an error
And clearCache() is invoked
And the model is re-downloaded successfully
And the fresh model is cached
```

---

### AC-12: Progress callback during model loading

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-005 | E2E | P1 | UI progress bar updates during model load | User experience - real rendering |

**Given-When-Then:**
```gherkin
Given an onProgress callback is provided to loadModel()
When the model download is in progress
Then onProgress(loaded, total) is called multiple times
And loaded increases monotonically
And the final call has loaded === total
```

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Test IDs Mitigating |
|------|-------------|--------|---------------------|
| IndexedDB storage limit exceeded (Safari) | Medium | High | 004.3b-E2E-001, 004.3b-INT-003 |
| Corrupted cache causes permanent failure | Low | Critical | 004.3b-E2E-004, 004.3b-UNIT-010 |
| Version mismatch leads to stale model | Medium | High | 004.3b-E2E-002, 004.3b-INT-006 |
| Network timeout during large file download | Medium | Medium | 004.3b-E2E-003, 004.3b-UNIT-001 |
| Checksum mismatch undetected | Low | Critical | 004.3b-UNIT-008, 004.3b-INT-007 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Critical)
1. 004.3b-UNIT-001 - Core loading function
2. 004.3b-INT-001 - Real file loading
3. 004.3b-INT-002 - No external CDN requests
4. 004.3b-INT-003 - IndexedDB storage works
5. 004.3b-E2E-001 - Cache hit skips download
6. 004.3b-E2E-002 - Version invalidation
7. 004.3b-E2E-003 - Performance verification
8. 004.3b-E2E-004 - Corrupted cache recovery

### Phase 2: Core Functionality (P1)
9. 004.3b-UNIT-002 - Buffer size validation
10. 004.3b-UNIT-003 - URL construction
11. 004.3b-INT-004 - Data integrity in storage
12. 004.3b-INT-005 - Cache retrieval
13. 004.3b-UNIT-005 - Cache-first flow
14. 004.3b-INT-006 - Version mismatch
15. 004.3b-UNIT-007 - Manifest schema
16. 004.3b-INT-007 - Manifest fetch
17. 004.3b-INT-008 - Build script
18. 004.3b-E2E-005 - Progress callback

### Phase 3: Edge Cases (P2)
19. 004.3b-UNIT-004 - CachedModel structure
20. 004.3b-UNIT-006 - Version comparison edge cases
21. 004.3b-UNIT-008 - SHA256 format
22. 004.3b-UNIT-009 - Size bounds
23. 004.3b-INT-009 - Log output verification
24. 004.3b-UNIT-010 - Safe loader error handling

---

## Test Environment Requirements

### Unit Tests
- **Framework:** Jest or Vitest
- **Mocking:** fetch API, IndexedDB (fake-indexeddb)
- **Execution:** Node.js or jsdom

### Integration Tests
- **Framework:** Jest with jsdom or Playwright component tests
- **Dependencies:** Real IndexedDB (browser or polyfill)
- **Test data:** Small (~1MB) mock GGUF file for fast tests

### E2E Tests
- **Framework:** Playwright or Cypress
- **Browsers:** Chrome, Firefox (Safari optional due to storage limits)
- **Test server:** Local HTTP server serving model assets
- **Large file handling:** Use ~10MB test model for CI, full 1.9GB for nightly

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for IndexedDB, E2E for user flows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (cache hit/miss is P0 for user experience)
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 10
    integration: 9
    e2e: 5
  by_priority:
    p0: 8
    p1: 10
    p2: 6
  coverage_gaps: []
  key_risks:
    - id: RISK-SAFARI-LIMIT
      description: Safari IndexedDB 1GB limit may fail for 1.9GB model
      tests: ["004.3b-E2E-001", "004.3b-INT-003"]
    - id: RISK-CORRUPT-CACHE
      description: Corrupted cache must trigger automatic recovery
      tests: ["004.3b-E2E-004", "004.3b-UNIT-010"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.3b-test-design-20260108.md
P0 tests identified: 8
Total scenarios: 24
Coverage: 100% of acceptance criteria
```
