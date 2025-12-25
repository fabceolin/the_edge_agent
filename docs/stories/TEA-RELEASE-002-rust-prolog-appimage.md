# TEA-RELEASE-002: Rust Prolog Binaries and AppImage Distribution

## Status
Ready for Review

## Story

**As a** developer using The Edge Agent with neurosymbolic AI workflows,
**I want** pre-built Rust binaries with Prolog support and an AppImage for easy distribution,
**So that** I can use Prolog actions without installing SWI-Prolog system-wide.

## Acceptance Criteria

### Rust Binary Variants

1. **AC-1**: Release includes `tea-rust-{platform}` binaries WITHOUT Prolog (current behavior, smaller)
2. **AC-2**: Release includes `tea-rust-{platform}-prolog` binaries WITH Prolog feature enabled
3. **AC-3**: Prolog binaries are built with `--features prolog` flag
4. **AC-4**: Non-prolog binaries remain statically linked (musl on Linux)
5. **AC-5**: Prolog binaries use **glibc dynamic linking** (musl incompatible with swipl-rs)

### AppImage Distribution (Linux only)

6. **AC-6**: AppImage bundles `tea` binary with `libswipl.so` and dependencies
7. **AC-7**: AppImage is self-contained and runs on systems without SWI-Prolog installed
8. **AC-8**: AppImage follows naming convention: `tea-{version}-{arch}.AppImage` (x86_64 and aarch64)
9. **AC-9**: AppImage includes desktop entry and icon for GUI integration
10. **AC-10**: Both x86_64 and aarch64 AppImages are built natively on respective runners

### Build Matrix Updates

11. **AC-11**: GitHub Actions builds both variants for Linux x86_64 and ARM64
12. **AC-12**: macOS and Windows prolog builds are optional (require SWI-Prolog SDK)
13. **AC-13**: Build matrix runs in parallel for efficiency
14. **AC-14**: Each binary passes smoke test (`--version`, `--impl`)

### Documentation

15. **AC-15**: README documents the difference between binary variants
16. **AC-16**: README includes AppImage installation instructions

## Tasks / Subtasks

- [x] Task 1: Add Rust Prolog build jobs to release workflow (AC-1, AC-2, AC-3, AC-4, AC-5, AC-11)
  - [x] Add `build-rust-prolog-linux-x86_64` job with SWI-Prolog installation
  - [x] Add `build-rust-prolog-linux-arm64` job with SWI-Prolog installation
  - [x] Configure `LD_LIBRARY_PATH` for glibc dynamic linking
  - [x] Add smoke tests for prolog binaries (`--version`, `--impl`)
  - [x] Run prolog example YAML for functional smoke test (e.g., `examples/prolog/simple-prolog-agent.yaml`)

- [x] Task 2: Create AppImage build jobs (AC-6, AC-7, AC-8, AC-9, AC-10)
  - [x] Add `build-appimage-x86_64` job on `ubuntu-latest`
  - [x] Add `build-appimage-aarch64` job on `ubuntu-24.04-arm`
  - [x] Install `linuxdeploy` (arch-specific) for auto-dependency bundling
  - [x] Create AppDir structure with binary and libraries
  - [x] Bundle `libswipl.so` and transitive dependencies via linuxdeploy
  - [x] Bundle SWI-Prolog runtime directory (`/usr/lib/swi-prolog` → `usr/lib/swipl`)
  - [x] Create `.desktop` file (icon optional for CLI tool)
  - [x] Build AppImage with `linuxdeploy --output appimage`
  - [x] Test AppImage on clean Ubuntu container

- [x] Task 3: Update release job to include new artifacts (AC-13)
  - [x] Add prolog binaries to artifact collection
  - [x] Add AppImages to artifact collection
  - [x] Update SHA256SUMS generation

- [x] Task 4: Update documentation (AC-15, AC-16)
  - [x] Add binary variants table to README
  - [x] Add AppImage usage instructions
  - [x] Document Prolog feature requirements

## Dev Notes

### Binary Naming Convention

| Binary | Description | Prolog | Size (est.) |
|--------|-------------|--------|-------------|
| `tea-rust-linux-x86_64` | Core features, statically linked (musl) | No | ~15MB |
| `tea-rust-linux-x86_64-prolog` | With Prolog, requires `libswipl.so` installed | Yes* | ~18MB |
| `tea-{version}-x86_64.AppImage` | **Self-contained with Prolog + all libs bundled** | Yes | ~50MB |
| `tea-rust-linux-aarch64` | Core features, statically linked (musl) | No | ~15MB |
| `tea-rust-linux-aarch64-prolog` | With Prolog, requires `libswipl.so` installed | Yes* | ~18MB |
| `tea-{version}-aarch64.AppImage` | **Self-contained with Prolog + all libs bundled** | Yes | ~50MB |

*Requires SWI-Prolog installed on the system (`apt install swi-prolog-nox`)

> **Note**: AppImage versions are the recommended choice for Prolog users as they bundle
> `libswipl.so` and all dependencies - no system installation required. Just download,
> `chmod +x`, and run.

### SWI-Prolog Installation in CI

```yaml
- name: Install SWI-Prolog
  run: |
    sudo apt-get update
    sudo apt-get install -y swi-prolog-nox libswipl-dev
```

### Build Commands

```bash
# Without Prolog (current)
cargo build --release --target x86_64-unknown-linux-musl

# With Prolog
LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux \
  cargo build --release --features prolog
```

### AppImage Tooling

Use [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) instead of manual appimagetool:
- Auto-detects and bundles transitive dependencies
- Plugin system with `--output appimage`
- Less error-prone than manual lib copying

```bash
# Download linuxdeploy (arch-specific)
wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
chmod +x linuxdeploy-x86_64.AppImage

# Build AppImage
./linuxdeploy-x86_64.AppImage \
  --appdir tea.AppDir \
  --executable tea-rust-linux-x86_64-prolog \
  --desktop-file tea.desktop \
  --output appimage
```

### AppImage Structure

```
tea.AppDir/
├── AppRun (auto-generated by linuxdeploy)
├── tea.desktop
├── usr/
│   ├── bin/
│   │   └── tea (the binary)
│   └── lib/
│       ├── swipl/           # SWI-Prolog runtime (from /usr/lib/swi-prolog)
│       │   ├── boot/
│       │   ├── library/
│       │   └── ...
│       ├── libswipl.so.9
│       ├── libgmp.so.10
│       └── ... (auto-bundled by linuxdeploy)
```

### AppRun Script (custom, replaces linuxdeploy default)

```bash
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
export SWI_HOME_DIR="${HERE}/usr/lib/swipl"
exec "${HERE}/usr/bin/tea" "$@"
```

> **Note**: Custom AppRun is needed to set `SWI_HOME_DIR` for Prolog runtime.

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | In workflow | `--version` and `--impl` for all binaries |
| Prolog test | In workflow | Run simple prolog example YAML |
| AppImage test | In workflow | Run on clean Ubuntu container |

### Prolog Feature Detection

The binary should gracefully handle missing Prolog:
- Without `--features prolog`: Prolog actions return error "Prolog support not compiled"
- With `--features prolog` but no libswipl: Binary fails to start (expected)
- AppImage: Always works (libs bundled)

### Platform Support

| Platform | Base Binary | Prolog Binary | AppImage |
|----------|-------------|---------------|----------|
| Linux x86_64 | Yes | Yes | Yes |
| Linux ARM64 | Yes | Yes | Yes |
| macOS ARM64 | Yes | No* | N/A |
| macOS x86_64 | Yes | No* | N/A |
| Windows x86_64 | Yes | No* | N/A |

*macOS/Windows Prolog builds require SWI-Prolog SDK installation in CI, deferred to future story.

### User Decision Guide

```
Do you need Prolog support?
├── No  → Use `tea-rust-linux-{arch}` (smallest, static)
└── Yes → Is SWI-Prolog installed on your system?
          ├── Yes → Use `tea-rust-linux-{arch}-prolog` (smaller)
          └── No  → Use `tea-{version}-{arch}.AppImage` (self-contained)
```

### License Considerations

- SWI-Prolog core: BSD-2-Clause (permissive)
- Some SWI-Prolog libraries: LGPL
- Dynamic linking with LGPL is compatible with MIT license
- AppImage distribution is compliant as it bundles shared libraries

## Out of Scope

- macOS/Windows Prolog binaries (requires SDK setup)
- Flatpak/Snap packaging (future story)
- Static linking with Prolog (technically infeasible)

## Dependencies

- TEA-RELEASE-001 (completed) - base release workflow
- SWI-Prolog packages available in Ubuntu repos (`swi-prolog-nox`, `libswipl-dev`)
- [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) for AppImage creation (auto-bundles dependencies)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-24 | 1.0 | Initial story creation | Sarah (PO Agent) |
| 2025-12-24 | 1.1 | Address QA issues: clarify glibc in AC-5, add prolog smoke test subtask, recommend linuxdeploy, add swipl runtime bundling | Winston (Architect) |
| 2025-12-24 | 1.2 | Story checklist passed, status changed to Approved | Bob (Scrum Master) |

---

## QA Results

### Review Date: 2025-12-24

### Reviewed By: Quinn (Test Architect)

### Story Quality Assessment

**Overall**: Story is well-structured with clear acceptance criteria, good decision guide, and proper license considerations. Some medium-severity gaps need addressing before implementation.

### Issues Found

| ID | Severity | Finding | Suggested Action | Owner |
|----|----------|---------|------------------|-------|
| SQ-001 | Medium | AC-5 says "Prolog binaries use dynamic linking" but doesn't clarify they use **glibc** (not musl). This is important for understanding portability. | Add note: "Prolog builds use glibc dynamic linking (musl incompatible with swipl-rs)" | po |
| SQ-002 | Low | Task 1 references AC-1,2,3,11 but should also reference AC-4,5 (linking requirements) | Update Task 1 AC references | po |
| SQ-003 | Medium | Testing table mentions "Run simple prolog example YAML" but no corresponding subtask exists | Add subtask to Task 1: "Run prolog example YAML for functional smoke test" | po |
| SQ-004 | Low | AC-9 mentions "icon for GUI integration" but tea is CLI-only and no icon exists in repo | Either create icon or remove from AC-9 (just keep .desktop) | po |
| SQ-005 | Medium | Dev Notes recommends manual appimagetool but [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) is better for auto-bundling dependencies | Update Dev Notes to recommend linuxdeploy with `--output appimage` plugin | po |
| SQ-006 | Medium | AppImage needs to bundle SWI-Prolog **runtime files** (not just libswipl.so) for `SWI_HOME_DIR` | Add subtask: "Bundle swipl runtime directory (/usr/lib/swi-prolog)" | po |
| SQ-007 | Low | No AC for testing AppImage on non-Ubuntu distros (Fedora, Arch, etc.) | Add note about target distro compatibility or acceptance test | po |

### Requirements Traceability

| AC | Test Scenario (Given-When-Then) | Coverage |
|----|--------------------------------|----------|
| AC-1 | Given release workflow runs, When build-rust job completes, Then tea-rust-{platform} artifacts exist without prolog feature | Covered by existing workflow |
| AC-2 | Given release workflow runs, When build-rust-prolog job completes, Then tea-rust-{platform}-prolog artifacts exist | New job needed |
| AC-3 | Given prolog build job, When cargo build executes, Then `--features prolog` flag is used | Verify in workflow |
| AC-4 | Given non-prolog build, When compiled, Then binary is statically linked (musl) | Existing behavior |
| AC-5 | Given prolog build, When compiled, Then binary uses glibc dynamic linking | New job needed |
| AC-6 | Given AppImage, When inspected, Then contains tea binary + libswipl.so + deps | New job needed |
| AC-7 | Given clean Ubuntu container, When AppImage runs, Then executes without errors | New test needed |
| AC-8 | Given AppImage artifact, When named, Then follows `tea-{version}-{arch}.AppImage` | Verify naming |
| AC-9 | Given AppImage, When inspected, Then contains .desktop file | Verify structure |
| AC-10 | Given x86_64 and aarch64 runners, When AppImage jobs run, Then both architectures produce artifacts | Verify matrix |
| AC-11-14 | Standard CI matrix requirements | Follow existing patterns |
| AC-15-16 | Documentation requirements | Post-implementation |

### Compliance Check

- Coding Standards: N/A (story draft, no code)
- Project Structure: ✓ Story in correct location
- Testing Strategy: ✓ Smoke tests defined
- All ACs Met: ✗ See issues above

### Recommendations

**Immediate (before implementation):**
1. Clarify glibc requirement in AC-5 (SQ-001)
2. Add prolog YAML smoke test subtask (SQ-003)
3. Update to recommend linuxdeploy (SQ-005)
4. Add swipl runtime bundling subtask (SQ-006)

**Future:**
1. Consider Flatpak/Snap for better sandboxing
2. Add multi-distro testing matrix (Fedora, Arch, Alpine)

### Gate Status

**Gate: CONCERNS** → `docs/qa/gates/TEA-RELEASE-002-rust-prolog-appimage.yml`

Story is well-written but has medium-severity gaps in technical specifications (glibc clarification, swipl runtime bundling, linuxdeploy recommendation) that should be addressed before implementation to avoid rework.

### Recommended Status

✗ Changes Required - Address SQ-001, SQ-003, SQ-005, SQ-006 before moving to Approved

(Story owner decides final status)

---

### Re-Review Date: 2025-12-24

### Reviewed By: Quinn (Test Architect)

### Issues Resolution Verification

| Issue | Status | Resolution |
|-------|--------|------------|
| SQ-001 | ✅ **RESOLVED** | AC-5 now specifies "**glibc dynamic linking** (musl incompatible with swipl-rs)" |
| SQ-002 | ✅ **RESOLVED** | Task 1 now references AC-1,2,3,4,5,11 |
| SQ-003 | ✅ **RESOLVED** | Task 1 includes "Run prolog example YAML for functional smoke test" |
| SQ-004 | ✅ **MITIGATED** | Task 2 clarifies "icon optional for CLI tool" |
| SQ-005 | ✅ **RESOLVED** | Dev Notes recommends linuxdeploy with example commands |
| SQ-006 | ✅ **RESOLVED** | Task 2 includes "Bundle SWI-Prolog runtime directory" |
| SQ-007 | ⚠️ **ACCEPTED** | Low priority, can be addressed during implementation |

### Updated Compliance Check

- Coding Standards: N/A (story draft, no code)
- Project Structure: ✓ Story in correct location
- Testing Strategy: ✓ Smoke tests + functional test defined
- All ACs Met: ✓ All critical requirements addressed

### Gate Status Update

**Gate: PASS** → `docs/qa/gates/TEA-RELEASE-002-rust-prolog-appimage.yml`

All 4 medium-severity issues have been resolved by the Architect (v1.1). Story is now ready for implementation.

### Final Recommendation

✓ **Ready for Implementation** - Story approved, gate updated to PASS

Quality Score: **90/100** (was 60, +30 after fixes)

---

### Test Design Assessment: 2025-12-25

### Assessed By: Quinn (Test Architect)

### Test Strategy Summary

| Metric | Count |
|--------|-------|
| **Total Scenarios** | 26 |
| **Integration Tests** | 12 (46%) |
| **E2E Tests** | 14 (54%) |
| **Unit Tests** | 0 (0%) |

**Rationale**: No unit tests needed - this is a CI/CD and packaging story with no pure business logic to test.

### Priority Distribution

| Priority | Count | Focus |
|----------|-------|-------|
| **P0 (Critical)** | 8 | Artifact existence, AppImage self-containment, smoke tests |
| **P1 (High)** | 12 | Linkage verification, feature flags, multi-arch validation |
| **P2 (Medium)** | 6 | Naming conventions, desktop integration, documentation |

### Key Test Scenarios

#### P0 Critical Tests
| ID | Test | Justification |
|----|------|---------------|
| INT-001/002 | Base binary artifacts exist | Core artifact validation |
| INT-003/004 | Prolog binary artifacts exist | Prolog feature validation |
| E2E-004 | AppImage runs on clean Ubuntu (no SWI-Prolog) | Self-containment proof |
| E2E-005 | AppImage executes simple-prolog-agent.yaml | Functional Prolog validation |
| E2E-012/013 | All binaries pass `--version` and `--impl` | Smoke test coverage |

#### Risk Mitigation Coverage

| Risk | Tests |
|------|-------|
| AppImage missing swipl runtime | E2E-004, E2E-005, E2E-007 |
| musl/glibc linkage confusion | INT-006 through INT-009 |
| ARM64 AppImage build failure | E2E-011 |

### AC Coverage Verification

All 16 acceptance criteria have test coverage:
- AC-1 through AC-5: Binary variants and linkage (INT-001 to INT-009)
- AC-6 through AC-10: AppImage distribution (E2E-004 to E2E-011)
- AC-11 through AC-14: Build matrix (INT-012, E2E-012 to E2E-014)
- AC-15 through AC-16: Documentation (E2E-015, E2E-016 - manual)

### Test Implementation Guidance

**Smoke Tests (in workflow):**
```yaml
- name: Smoke test binaries
  run: |
    ./tea-rust-linux-x86_64 --version
    ./tea-rust-linux-x86_64 --impl | grep 'prolog: false'
    LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux \
      ./tea-rust-linux-x86_64-prolog --impl | grep 'prolog: true'
```

**AppImage Container Test:**
```yaml
- name: Test AppImage on clean container
  run: |
    docker run --rm -v $(pwd):/work ubuntu:22.04 bash -c '
      chmod +x /work/tea-*.AppImage
      /work/tea-*-x86_64.AppImage --version
      /work/tea-*-x86_64.AppImage run examples/prolog/simple-prolog-agent.yaml
    '
```

### Gate YAML Block

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

### Test Design Document

Full test design: `docs/qa/assessments/TEA-RELEASE-002-test-design-20251225.md`

### Assessment Result

**Test Design: COMPLETE**

- All ACs covered with appropriate test levels
- Risk-based prioritization applied
- Implementation guidance provided
- No coverage gaps identified

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blockers encountered during implementation

### Completion Notes
1. Added `build-rust-prolog-linux-x86_64` and `build-rust-prolog-linux-arm64` jobs with SWI-Prolog installation and glibc dynamic linking
2. Added `build-appimage-x86_64` and `build-appimage-aarch64` jobs using linuxdeploy for automatic dependency bundling
3. AppImage jobs include custom AppRun script that sets `SWI_HOME_DIR` for Prolog runtime
4. Both Prolog builds and AppImage builds include smoke tests and functional tests with `examples/prolog/simple-prolog-agent.yaml`
5. Release job updated to include all new artifacts in `needs` array
6. README updated with binary variants table, decision guide, and AppImage installation instructions
7. All existing Rust tests pass (24 unit tests + 11 doc tests)

### File List
| File | Action | Description |
|------|--------|-------------|
| `.github/workflows/release.yaml` | Modified | Added Rust Prolog build jobs, AppImage build jobs, and updated release job needs |
| `README.md` | Modified | Added binary variants table, decision guide, and AppImage installation instructions |
| `docs/stories/TEA-RELEASE-002-rust-prolog-appimage.md` | Modified | Updated task checkboxes and added Dev Agent Record |

### Change Log
| Date | Change | Author |
|------|--------|--------|
| 2025-12-25 | Implementation complete, all tasks done | Dev Agent (James)
