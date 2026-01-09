# Story TEA-RELEASE-005: Release Workflow Modularization

## Status

Approved

## Story

**As a** maintainer of the TEA project,
**I want** the monolithic release.yaml workflow split into modular reusable workflows,
**so that** the CI/CD pipeline is easier to maintain, test, and extend.

## Story Context

**Existing System Integration:**

- Integrates with: `.github/workflows/release.yaml` (current 3800-line monolithic workflow)
- Technology: GitHub Actions, YAML, reusable workflows (`workflow_call`)
- Follows pattern: GitHub Actions reusable workflow pattern with `workflow_call` trigger
- Touch points: Tag-triggered releases, artifact upload/download, GitHub Releases API

**Current State:**

The existing `release.yaml` contains ~30 jobs in a single file:
- 8 base Python builds (linux, linux-full, arm64, arm64-full, windows, windows-full, macos, macos-full)
- 4 Prolog-enabled Python builds
- 2 Python AppImage builds
- 4 Python LLM AppImage builds
- 5 Rust builds (linux-x86_64, linux-arm64, windows, macos-x86_64, macos-arm64)
- 2 Rust AppImage builds
- 3 Rust LLM AppImage builds
- 1 WASM build
- 1 Python wheel build
- 1 Release aggregation job

## Acceptance Criteria

### Functional Requirements

1. Split `release.yaml` into 10 reusable workflow modules:
   - `build-python-base.yaml` - 8 base Python binary builds
   - `build-python-prolog.yaml` - 4 Prolog-enabled Python builds
   - `build-python-appimage.yaml` - 2 Python AppImage builds
   - `build-python-llm.yaml` - 4 Python LLM AppImage builds
   - `build-rust-base.yaml` - 5 base Rust binary builds
   - `build-rust-appimage.yaml` - 2 Rust AppImage builds
   - `build-rust-llm.yaml` - 3 Rust LLM AppImage builds
   - `build-wasm.yaml` - 1 WASM LLM build
   - `build-wheel.yaml` - 1 Python wheel build
   - `release.yaml` - Orchestrator calling all modules + release job

2. Each reusable workflow must:
   - Use `on: workflow_call` trigger
   - Accept necessary inputs (e.g., version tag)
   - Upload artifacts with consistent naming
   - Be independently testable via `workflow_dispatch`

3. Main `release.yaml` orchestrator must:
   - Trigger on tag push (`v*`)
   - Call all reusable workflows
   - Aggregate all artifacts in final release job
   - Produce identical release artifacts as current implementation

### Integration Requirements

4. Existing release process continues to work unchanged
5. All 30+ artifacts are still produced with same names
6. SHA256 checksums are generated identically
7. GitHub Release creation works as before

### Quality Requirements

8. Each module is documented with header comments explaining its purpose
9. Workflow syntax is validated (`actionlint` or similar)
10. No regression in build times (parallel execution preserved)

## Tasks / Subtasks

- [ ] **Task 1: Create reusable workflow structure** (AC: 1, 2)
  - [ ] Create `build-python-base.yaml` with workflow_call trigger
  - [ ] Create `build-python-prolog.yaml` with workflow_call trigger
  - [ ] Create `build-python-appimage.yaml` with workflow_call trigger
  - [ ] Create `build-python-llm.yaml` with workflow_call trigger
  - [ ] Create `build-rust-base.yaml` with workflow_call trigger
  - [ ] Create `build-rust-appimage.yaml` with workflow_call trigger
  - [ ] Create `build-rust-llm.yaml` with workflow_call trigger
  - [ ] Create `build-wasm.yaml` with workflow_call trigger
  - [ ] Create `build-wheel.yaml` with workflow_call trigger

- [ ] **Task 2: Extract Python base builds** (AC: 1, 4)
  - [ ] Move `build-python-linux` job
  - [ ] Move `build-python-linux-full` job
  - [ ] Move `build-python-linux-arm64` job
  - [ ] Move `build-python-linux-arm64-full` job
  - [ ] Move `build-python-windows` job
  - [ ] Move `build-python-windows-full` job
  - [ ] Move `build-python-macos` job
  - [ ] Move `build-python-macos-full` job

- [ ] **Task 3: Extract Python Prolog builds** (AC: 1, 4)
  - [ ] Move `build-python-linux-prolog-x86_64` job
  - [ ] Move `build-python-linux-prolog-arm64` job
  - [ ] Move `build-python-linux-full-prolog-x86_64` job
  - [ ] Move `build-python-linux-full-prolog-arm64` job

- [ ] **Task 4: Extract Python AppImage builds** (AC: 1, 4)
  - [ ] Move `build-python-appimage-x86_64` job
  - [ ] Move `build-python-appimage-aarch64` job
  - [ ] Update `needs:` to reference reusable workflow outputs

- [ ] **Task 5: Extract Python LLM builds** (AC: 1, 4)
  - [ ] Move `build-python-llm-gemma-x86_64` job
  - [ ] Move `build-python-llm-gemma-aarch64` job
  - [ ] Move `build-python-llm-phi4-x86_64` job
  - [ ] Move `build-python-llm-phi4-aarch64` job

- [ ] **Task 6: Extract Rust base builds** (AC: 1, 4)
  - [ ] Move `build-rust` job (matrix: 5 platforms)
  - [ ] Move `build-rust-prolog-linux-x86_64` job
  - [ ] Move `build-rust-prolog-linux-arm64` job

- [ ] **Task 7: Extract Rust AppImage builds** (AC: 1, 4)
  - [ ] Move `build-appimage-x86_64` job
  - [ ] Move `build-appimage-aarch64` job

- [ ] **Task 8: Extract Rust LLM builds** (AC: 1, 4)
  - [ ] Move `build-rust-llm-linux-x86_64` job
  - [ ] Move `build-rust-llm-linux-arm64` job
  - [ ] Move `build-rust-llm-gemma-appimage-x86_64` job
  - [ ] Move `build-rust-llm-phi4-appimage-x86_64` job
  - [ ] Move `build-rust-llm-phi4-appimage-aarch64` job

- [ ] **Task 9: Extract WASM and Wheel builds** (AC: 1, 4)
  - [ ] Move `build-wasm-llm` job to `build-wasm.yaml`
  - [ ] Move `build-wheel` job to `build-wheel.yaml`

- [ ] **Task 10: Create orchestrator release.yaml** (AC: 3, 5, 6, 7)
  - [ ] Define workflow_call invocations for all modules
  - [ ] Configure artifact download in release job
  - [ ] Verify SHA256SUMS generation
  - [ ] Verify GitHub Release creation

- [ ] **Task 11: Add workflow_dispatch for testing** (AC: 2)
  - [ ] Add `workflow_dispatch` trigger to each reusable workflow
  - [ ] Add input parameters for manual testing

- [ ] **Task 12: Documentation and validation** (AC: 8, 9, 10)
  - [ ] Add header comments to each workflow file
  - [ ] Run `actionlint` validation
  - [ ] Verify parallel execution is preserved

## Dev Notes

### Reusable Workflow Pattern

GitHub Actions reusable workflows use `workflow_call` trigger:

```yaml
# .github/workflows/build-python-base.yaml
name: Build Python Base

on:
  workflow_call:
    inputs:
      ref:
        description: 'Git ref to build'
        type: string
        required: false
        default: ${{ github.ref }}
  workflow_dispatch:  # For manual testing

jobs:
  build-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          ref: ${{ inputs.ref }}
      # ... build steps ...
      - uses: actions/upload-artifact@v4
        with:
          name: python-linux-x86_64
          path: python/dist/tea-python-linux-x86_64
```

### Orchestrator Pattern

```yaml
# .github/workflows/release.yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  python-base:
    uses: ./.github/workflows/build-python-base.yaml
    with:
      ref: ${{ github.ref }}

  python-prolog:
    uses: ./.github/workflows/build-python-prolog.yaml
    with:
      ref: ${{ github.ref }}

  # ... other workflow calls ...

  release:
    needs: [python-base, python-prolog, rust-base, ...]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          path: dist
      # ... release steps ...
```

### Key Constraints

1. **Artifact sharing**: Artifacts uploaded in called workflows are available to the caller's `download-artifact`
2. **Job dependencies**: Cross-workflow `needs` requires the called workflow job names to be referenced as `<workflow-job>.<internal-job>`
3. **Secrets**: Reusable workflows inherit secrets from caller by default (or use `secrets: inherit`)

### File Locations

| File | Purpose |
|------|---------|
| `.github/workflows/build-python-base.yaml` | 8 base Python builds |
| `.github/workflows/build-python-prolog.yaml` | 4 Prolog Python builds |
| `.github/workflows/build-python-appimage.yaml` | 2 Python AppImage builds |
| `.github/workflows/build-python-llm.yaml` | 4 Python LLM AppImage builds |
| `.github/workflows/build-rust-base.yaml` | 5 base Rust builds |
| `.github/workflows/build-rust-appimage.yaml` | 2 Rust AppImage builds |
| `.github/workflows/build-rust-llm.yaml` | 3 Rust LLM AppImage builds |
| `.github/workflows/build-wasm.yaml` | 1 WASM build |
| `.github/workflows/build-wheel.yaml` | 1 Python wheel build |
| `.github/workflows/release.yaml` | Orchestrator + release job |

## Testing

### Testing Standards

- **Workflow validation**: Use `actionlint` to validate YAML syntax
- **Manual testing**: Each workflow supports `workflow_dispatch` for isolated testing
- **Integration testing**: Full release tested via tag push to test branch

### Test Matrix

| Scenario | Validation |
|----------|------------|
| Individual workflow runs | `workflow_dispatch` trigger works |
| Artifact upload/download | Artifacts visible in Actions UI |
| Full release pipeline | Tag push produces identical artifacts |
| SHA256 checksums | Checksums match expected format |

## Risk and Compatibility Check

### Minimal Risk Assessment

- **Primary Risk**: Artifact naming mismatch between modules and release job
- **Mitigation**: Use explicit artifact names matching current implementation exactly
- **Rollback**: Revert to original `release.yaml` (keep backup)

### Compatibility Verification

- [x] No breaking changes to existing release artifacts
- [x] Tag-based triggering preserved
- [x] Artifact names unchanged
- [x] Release notes generation unchanged

## Definition of Done

- [ ] All 10 workflow files created and functional
- [ ] Main `release.yaml` reduced to ~150 lines (orchestration only)
- [ ] All 30+ artifacts produced identically to current workflow
- [ ] `actionlint` passes on all workflow files
- [ ] Each workflow can be triggered independently via `workflow_dispatch`
- [ ] Documentation updated (this story serves as primary doc)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 1.0 | Initial story creation | Sarah (PO) |
