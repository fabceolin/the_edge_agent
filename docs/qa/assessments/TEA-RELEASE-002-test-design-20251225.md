# Test Design: Story TEA-RELEASE-002

Date: 2025-12-25
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 26
- Unit tests: 0 (0%) - No pure logic to test; this is a CI/CD and packaging story
- Integration tests: 12 (46%) - Build verification, artifact validation
- E2E tests: 14 (54%) - Full workflow validation, cross-platform compatibility
- Priority distribution: P0: 8, P1: 12, P2: 6

## Story Summary

TEA-RELEASE-002 focuses on:
1. Adding Rust Prolog binary variants to release workflow
2. Creating self-contained AppImage distributions for Linux
3. Ensuring proper artifact naming and smoke testing
4. Documentation updates

## Test Scenarios by Acceptance Criteria

### AC-1: Release includes `tea-rust-{platform}` binaries WITHOUT Prolog

**Test Focus**: Ensure base binaries (current behavior) continue to work

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-001 | Integration | P0 | Verify `tea-rust-linux-x86_64` artifact exists after build | Core artifact existence check |
| TEA-RELEASE-002-INT-002 | Integration | P0 | Verify `tea-rust-linux-aarch64` artifact exists after build | ARM64 artifact existence check |
| TEA-RELEASE-002-E2E-001 | E2E | P1 | Non-prolog binary runs `--version` successfully on clean Ubuntu | Basic functionality validation |
| TEA-RELEASE-002-E2E-002 | E2E | P1 | Non-prolog binary runs `--impl` and shows "prolog: false" | Feature flag verification |

### AC-2 & AC-3: Release includes `tea-rust-{platform}-prolog` binaries WITH Prolog feature

**Test Focus**: Ensure Prolog binaries are built with correct feature flag

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-003 | Integration | P0 | Verify `tea-rust-linux-x86_64-prolog` artifact exists after prolog build | Core artifact existence check |
| TEA-RELEASE-002-INT-004 | Integration | P0 | Verify `tea-rust-linux-aarch64-prolog` artifact exists after prolog build | ARM64 artifact existence check |
| TEA-RELEASE-002-INT-005 | Integration | P1 | Build log contains `--features prolog` flag | Build configuration verification |
| TEA-RELEASE-002-E2E-003 | E2E | P1 | Prolog binary runs `--impl` and shows "prolog: true" | Feature flag verification |

### AC-4: Non-prolog binaries remain statically linked (musl)

**Test Focus**: Verify static linking for non-prolog binaries

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-006 | Integration | P1 | `file tea-rust-linux-x86_64` shows "statically linked" | Binary linkage verification |
| TEA-RELEASE-002-INT-007 | Integration | P1 | `ldd tea-rust-linux-x86_64` returns "not a dynamic executable" | No dynamic dependencies |

### AC-5: Prolog binaries use glibc dynamic linking

**Test Focus**: Verify dynamic linking with glibc for Prolog binaries

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-008 | Integration | P1 | `file tea-rust-linux-x86_64-prolog` shows "dynamically linked" | Binary linkage verification |
| TEA-RELEASE-002-INT-009 | Integration | P1 | `ldd tea-rust-linux-x86_64-prolog` shows libswipl.so dependency | Prolog library linkage |

### AC-6 & AC-7: AppImage bundles tea binary with libswipl.so and is self-contained

**Test Focus**: AppImage completeness and portability

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-E2E-004 | E2E | P0 | AppImage runs on clean Ubuntu container (no SWI-Prolog installed) | Self-contained validation |
| TEA-RELEASE-002-E2E-005 | E2E | P0 | AppImage executes `examples/prolog/simple-prolog-agent.yaml` successfully | Functional Prolog test |
| TEA-RELEASE-002-E2E-006 | E2E | P1 | AppImage contains libswipl.so in usr/lib | Library bundling verification |
| TEA-RELEASE-002-E2E-007 | E2E | P1 | AppImage contains swipl runtime (usr/lib/swipl/boot, library) | Runtime bundling verification |

### AC-8: AppImage follows naming convention

**Test Focus**: Artifact naming compliance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-010 | Integration | P2 | AppImage artifact matches pattern `tea-{version}-x86_64.AppImage` | Naming convention |
| TEA-RELEASE-002-INT-011 | Integration | P2 | AppImage artifact matches pattern `tea-{version}-aarch64.AppImage` | Naming convention |

### AC-9: AppImage includes desktop entry

**Test Focus**: Desktop integration files present

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-E2E-008 | E2E | P2 | AppImage contains valid .desktop file | Desktop integration |
| TEA-RELEASE-002-E2E-009 | E2E | P2 | AppRun script sets SWI_HOME_DIR correctly | Custom AppRun verification |

### AC-10: Both x86_64 and aarch64 AppImages are built natively

**Test Focus**: Multi-architecture build verification

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-E2E-010 | E2E | P1 | x86_64 AppImage runs on x86_64 runner | Native execution |
| TEA-RELEASE-002-E2E-011 | E2E | P1 | aarch64 AppImage runs on ARM64 runner | Native execution |

### AC-11: GitHub Actions builds both variants for Linux x86_64 and ARM64

**Test Focus**: Build matrix completeness

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-INT-012 | Integration | P1 | Release workflow produces 6 Linux artifacts (2 base + 2 prolog + 2 AppImage) | Matrix completeness |

### AC-14: Each binary passes smoke test

**Test Focus**: Binary functionality validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-E2E-012 | E2E | P0 | All binaries pass `--version` smoke test | Basic functionality |
| TEA-RELEASE-002-E2E-013 | E2E | P0 | All binaries pass `--impl` smoke test | Feature detection |
| TEA-RELEASE-002-E2E-014 | E2E | P1 | Prolog binary executes simple-prolog-agent.yaml successfully | Functional Prolog validation |

### AC-15 & AC-16: Documentation requirements

**Test Focus**: Documentation completeness (manual review)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-002-E2E-015 | E2E | P2 | README documents binary variants table | Documentation check (manual) |
| TEA-RELEASE-002-E2E-016 | E2E | P2 | README includes AppImage installation instructions | Documentation check (manual) |

## Risk Coverage

Based on story analysis, key risks and their test coverage:

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| AppImage missing swipl runtime | Medium | High | E2E-004, E2E-005, E2E-007 |
| Prolog binary fails without libswipl | Low | Medium | E2E-003 (expected behavior) |
| musl/glibc confusion | Medium | High | INT-006 through INT-009 |
| ARM64 AppImage build failure | Medium | Medium | E2E-011 |
| Naming convention mismatch | Low | Low | INT-010, INT-011 |

## Test Environment Requirements

### Integration Tests (CI Workflow)
- GitHub Actions Ubuntu runners (x86_64 and ARM64)
- SWI-Prolog packages: `swi-prolog-nox`, `libswipl-dev`
- linuxdeploy tool for AppImage creation

### E2E Tests
- Clean Ubuntu container (Docker) without SWI-Prolog
- Native execution on target architectures
- Example YAML files from `examples/prolog/`

## Recommended Execution Order

1. **P0 Integration tests** (fail fast)
   - INT-001, INT-002: Base artifact existence
   - INT-003, INT-004: Prolog artifact existence

2. **P0 E2E tests** (critical functionality)
   - E2E-004, E2E-005: AppImage self-contained
   - E2E-012, E2E-013: Smoke tests

3. **P1 Integration tests**
   - INT-005 through INT-009: Linkage verification
   - INT-012: Matrix completeness

4. **P1 E2E tests**
   - E2E-001 through E2E-003: Binary feature verification
   - E2E-006, E2E-007: AppImage structure
   - E2E-010, E2E-011: Multi-arch validation
   - E2E-014: Functional Prolog test

5. **P2 tests**
   - INT-010, INT-011: Naming conventions
   - E2E-008, E2E-009: Desktop integration
   - E2E-015, E2E-016: Documentation (manual)

## Implementation Notes

### Smoke Test Implementation (in workflow)

```yaml
- name: Smoke test binaries
  run: |
    # Base binary
    ./tea-rust-linux-x86_64 --version
    ./tea-rust-linux-x86_64 --impl | grep 'prolog: false'

    # Prolog binary (requires libswipl in LD_LIBRARY_PATH)
    LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux \
      ./tea-rust-linux-x86_64-prolog --version
    LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux \
      ./tea-rust-linux-x86_64-prolog --impl | grep 'prolog: true'
```

### AppImage Test on Clean Container

```yaml
- name: Test AppImage on clean container
  run: |
    docker run --rm -v $(pwd):/work ubuntu:22.04 bash -c '
      cd /work
      chmod +x tea-*.AppImage
      ./tea-*-x86_64.AppImage --version
      ./tea-*-x86_64.AppImage run examples/prolog/simple-prolog-agent.yaml
    '
```

### Linkage Verification

```bash
# Static (non-prolog)
file tea-rust-linux-x86_64 | grep -q "statically linked"
ldd tea-rust-linux-x86_64 2>&1 | grep -q "not a dynamic executable"

# Dynamic (prolog)
file tea-rust-linux-x86_64-prolog | grep -q "dynamically linked"
ldd tea-rust-linux-x86_64-prolog | grep -q "libswipl.so"
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (integration for build verification, E2E for runtime validation)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (AppImage self-containment = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 26
  by_level:
    unit: 0
    integration: 12
    e2e: 14
  by_priority:
    p0: 8
    p1: 12
    p2: 6
  coverage_gaps: []
  key_risks_mitigated:
    - AppImage self-containment
    - Prolog runtime bundling
    - musl/glibc linkage confusion
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-002-test-design-20251225.md
P0 tests identified: 8
Critical paths covered: AppImage portability, smoke tests, artifact existence
```
