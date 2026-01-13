# TEA-RELEASE-005.2: Cosmopolitan Build Pipeline

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.2 |
| **Type** | Story |
| **Priority** | High |
| **Estimated Effort** | 8 points |
| **Status** | Ready for Development (Blocked on TEA-RELEASE-005.1) |
| **Parent Epic** | TEA-RELEASE-005 |
| **Depends On** | TEA-RELEASE-005.1 (Scryer spike must pass) |
| **Files to Create** | `.github/workflows/build-ape.yaml`, `rust/cosmo-config.toml` |
| **Files to Modify** | `.github/workflows/release.yaml`, `rust/Cargo.toml` |

## Story

**As a** developer,
**I want** a GitHub Actions workflow that builds TEA as an Actually Portable Executable,
**So that** we produce a universal binary for all desktop platforms.

## Background

### What is Cosmopolitan?

Cosmopolitan Libc creates **Actually Portable Executables (APE)** - polyglot binaries that run natively on multiple operating systems:

- Windows (x86_64, ARM64)
- Linux (x86_64, ARM64)
- macOS (x86_64, ARM64)
- FreeBSD, OpenBSD, NetBSD

A single `.com` file works everywhere without recompilation.

### How APE Works

```
┌─────────────────────────────────────┐
│  MZ header (Windows PE)             │ ← Windows reads this
│  #!/bin/sh (Unix shebang)           │ ← Unix shells see script
│  ELF magic + Mach-O magic           │ ← Linux/macOS loaders
│  ... actual code ...                │
│  ... embedded data (zip) ...        │
└─────────────────────────────────────┘
```

## Acceptance Criteria

- [ ] **AC-1**: GitHub Actions workflow `build-ape.yaml` created
- [ ] **AC-2**: Cosmopolitan toolchain (`cosmocc`) installed in CI
- [ ] **AC-3**: TEA Rust compiles with Cosmopolitan target
- [ ] **AC-4**: `tea.com` includes Scryer Prolog support
- [ ] **AC-5**: `tea.com` includes mlua (Lua) support
- [ ] **AC-6**: Binary runs on Ubuntu 22.04 without dependencies
- [ ] **AC-7**: Binary runs on Windows 11 without dependencies
- [ ] **AC-8**: Binary runs on macOS 13+ without dependencies
- [ ] **AC-9**: `tea.com --version` and `tea.com --impl` work on all platforms
- [ ] **AC-10**: Binary size <25MB (without model)
- [ ] **AC-11**: Release workflow updated to include APE artifact

## Technical Design

### GitHub Actions Workflow

```yaml
# .github/workflows/build-ape.yaml
name: Build APE

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:

env:
  COSMO_VERSION: '3.3.2'

jobs:
  build-ape:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install Cosmopolitan
        run: |
          mkdir -p ~/cosmo
          cd ~/cosmo
          wget https://github.com/jart/cosmopolitan/releases/download/${{ env.COSMO_VERSION }}/cosmocc-${{ env.COSMO_VERSION }}.zip
          unzip cosmocc-${{ env.COSMO_VERSION }}.zip
          echo "$HOME/cosmo/bin" >> $GITHUB_PATH

      - name: Setup Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Build TEA APE
        run: |
          cd rust
          # Configure for Cosmopolitan
          export CC="cosmocc"
          export CXX="cosmoc++"
          export AR="cosmoar"

          # Build with Scryer Prolog
          cargo build --release --features scryer

          # Convert to APE format
          objcopy -O binary target/release/tea tea.com

      - name: Smoke test (Linux)
        run: |
          chmod +x tea.com
          ./tea.com --version
          ./tea.com --impl

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: tea-ape
          path: tea.com

  test-windows:
    needs: build-ape
    runs-on: windows-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: tea-ape

      - name: Test on Windows
        run: |
          .\tea.com --version
          .\tea.com --impl

  test-macos:
    needs: build-ape
    runs-on: macos-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: tea-ape

      - name: Test on macOS
        run: |
          chmod +x tea.com
          ./tea.com --version
          ./tea.com --impl

  release:
    needs: [build-ape, test-windows, test-macos]
    runs-on: ubuntu-latest
    if: startsWith(github.ref, 'refs/tags/')

    steps:
      - uses: actions/download-artifact@v4
        with:
          name: tea-ape

      - name: Rename for release
        run: |
          VERSION=${GITHUB_REF#refs/tags/}
          mv tea.com tea-${VERSION}.com

      - name: Upload to release
        uses: softprops/action-gh-release@v1
        with:
          files: tea-*.com
```

### Rust Configuration

```toml
# rust/cosmo-config.toml
# Cosmopolitan-specific build configuration

[build]
target = "x86_64-unknown-linux-cosmo"

[target.x86_64-unknown-linux-cosmo]
linker = "cosmocc"
ar = "cosmoar"
rustflags = ["-C", "target-feature=+crt-static"]
```

### Build Script

```bash
#!/bin/bash
# scripts/build-ape.sh

set -e

# Ensure Cosmopolitan is available
if ! command -v cosmocc &> /dev/null; then
    echo "Error: cosmocc not found. Install Cosmopolitan first."
    exit 1
fi

cd rust

# Set environment for Cosmopolitan
export CC="cosmocc"
export CXX="cosmoc++"
export AR="cosmoar"

# Build with Scryer Prolog and Lua
cargo build --release --features "scryer"

# The output is already in APE format when linked with cosmocc
cp target/release/tea ../tea.com

echo "Built tea.com successfully"
ls -lh ../tea.com
```

## Tasks / Subtasks

- [ ] **Task 1: Create APE workflow** (AC: 1, 2)
  - [ ] Create `.github/workflows/build-ape.yaml`
  - [ ] Add Cosmopolitan installation step
  - [ ] Configure build environment

- [ ] **Task 2: Configure Rust for Cosmopolitan** (AC: 3)
  - [ ] Create `rust/cosmo-config.toml`
  - [ ] Add custom target configuration
  - [ ] Test local build with cosmocc

- [ ] **Task 3: Integrate Scryer Prolog** (AC: 4)
  - [ ] Ensure Scryer compiles with Cosmopolitan
  - [ ] Test Prolog queries in APE binary

- [ ] **Task 4: Integrate Lua** (AC: 5)
  - [ ] Ensure mlua compiles with Cosmopolitan
  - [ ] Test Lua scripts in APE binary

- [ ] **Task 5: Cross-platform smoke tests** (AC: 6, 7, 8, 9)
  - [ ] Add Windows test job
  - [ ] Add macOS test job
  - [ ] Verify `--version` and `--impl` on all platforms

- [ ] **Task 6: Optimize binary size** (AC: 10)
  - [ ] Enable LTO (Link-Time Optimization)
  - [ ] Strip debug symbols
  - [ ] Verify size <25MB

- [ ] **Task 7: Update release workflow** (AC: 11)
  - [ ] Add APE build to release.yaml
  - [ ] Include tea-{version}.com in release artifacts

## Dev Notes

### Cosmopolitan Resources

- Repository: https://github.com/jart/cosmopolitan
- Documentation: https://justine.lol/cosmopolitan/
- APE explanation: https://justine.lol/ape.html

### Known Challenges

| Challenge | Mitigation |
|-----------|------------|
| Rust Cosmopolitan target experimental | Use C linker wrapper approach |
| Some crates may not compile | Identify and patch or replace |
| Binary size may be large | Enable LTO, strip symbols |

### Local Testing

```bash
# Install Cosmopolitan locally
mkdir -p ~/cosmo && cd ~/cosmo
wget https://github.com/jart/cosmopolitan/releases/latest/download/cosmocc.zip
unzip cosmocc.zip
export PATH="$HOME/cosmo/bin:$PATH"

# Build TEA APE
cd the_edge_agent
./scripts/build-ape.sh

# Test on Linux
./tea.com --version

# Test on Windows (WSL or actual Windows)
# Copy tea.com to Windows and run
```

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke | In workflow | `--version` and `--impl` checks |
| Integration | Manual | Run example YAML agents |
| Cross-platform | CI matrix | Ubuntu, Windows, macOS |

## QA Notes

**Assessment Date:** 2026-01-12
**Assessor:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 28 |
| **Unit tests** | 4 (14%) |
| **Integration tests** | 10 (36%) |
| **E2E tests** | 14 (50%) |
| **Priority distribution** | P0: 8, P1: 12, P2: 6, P3: 2 |

All 11 acceptance criteria have test coverage. The heavy E2E distribution (50%) is appropriate given the story's focus on cross-platform binary distribution.

### Risk Areas Identified

| Risk ID | Description | Probability | Impact | Mitigation |
|---------|-------------|-------------|--------|------------|
| RISK-001 | Cosmopolitan toolchain unavailable/broken | Low | Critical | Pin version, cache in CI |
| RISK-002 | Rust crates incompatible with cosmocc | Medium | High | TEA-RELEASE-005.1 spike validates |
| RISK-003 | Binary fails on specific platform | Medium | High | Multi-runner CI matrix |
| RISK-004 | Scryer/Lua runtime broken in APE | Medium | High | Cross-platform E2E tests |
| RISK-005 | Binary size exceeds 25MB limit | Low | Medium | LTO + strip, size assertion |
| RISK-006 | Release workflow missing APE artifact | Low | Medium | E2E-015 validates |

### Recommended Test Scenarios (P0 Critical Path)

1. **TEA-RELEASE-005.2-INT-003**: Cosmocc downloads and extracts successfully
2. **TEA-RELEASE-005.2-INT-006**: `cargo build --release` with cosmocc succeeds
3. **TEA-RELEASE-005.2-INT-008**: `--features scryer` compiles with cosmocc
4. **TEA-RELEASE-005.2-E2E-007**: Run `tea.com` on clean Ubuntu 22.04 container
5. **TEA-RELEASE-005.2-E2E-009**: Run `tea.com` on clean Windows 11 runner
6. **TEA-RELEASE-005.2-E2E-011**: Run `tea.com` on macOS 13 runner
7. **TEA-RELEASE-005.2-E2E-001/002/003**: Execute Prolog query on all platforms

### Concerns and Blockers

| Type | Description | Severity |
|------|-------------|----------|
| **Dependency** | Story blocked until TEA-RELEASE-005.1 (Scryer spike) passes | Blocker |
| **Technical Risk** | Cosmopolitan Rust target is experimental; C linker wrapper approach may have edge cases | Medium |
| **CI Complexity** | Multi-platform matrix requires Ubuntu, Windows, macOS runners | Low |
| **Testing Gap** | ARM64 testing limited to macOS; no Linux ARM64 runner in standard GHA | Advisory |

### Special Considerations

- Cross-platform testing requires multi-runner CI matrix
- Scryer Prolog tests require valid Prolog query syntax
- Binary size monitoring should be automated in CI
- Clean container tests validate no hidden system dependencies
- Consider caching Cosmopolitan toolchain between runs for performance

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-RELEASE-005.2-test-design-20260112.md`

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-12 | 1.1 | Added QA Notes section | Quinn (QA Agent) |
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

