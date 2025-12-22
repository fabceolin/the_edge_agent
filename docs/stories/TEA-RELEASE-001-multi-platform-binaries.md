# TEA-RELEASE-001: Multi-Platform Binary Release Pipeline

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-001 |
| **Type** | Story |
| **Priority** | High |
| **Estimated Effort** | 8 points |
| **Status** | Ready for Review |
| **Parent Epic** | TEA-MONO-001 |
| **Depends On** | TEA-CLI-004 (CLI Alignment - recommended), TEA-RUST-034 (External Imports - optional) |
| **Files to Create** | `.github/workflows/release.yaml` |
| **Files to Modify** | `python/pyproject.toml`, `rust/Cargo.toml` |

## Description

**As a** user of The Edge Agent,
**I want** pre-built binaries for Linux, Windows, and macOS available in GitHub Releases,
**So that** I can run `tea` without installing Python or Rust toolchains.

## Background

### Current State

- Python: Only `.whl` (wheel) files are published, requiring Python runtime
- Rust: No pre-built binaries; users must compile from source with `cargo build`

### Problem

Users who want to use TEA for workflow automation face barriers:
1. Python users need Python 3.10+ installed
2. Rust users need the Rust toolchain installed
3. CI/CD pipelines require additional setup steps
4. Edge computing environments may not have development tools

### Solution

Create a GitHub Actions workflow that:
1. Builds standalone Python binaries using PyInstaller (no Python runtime needed)
2. Builds Rust binaries for each target platform
3. Publishes all binaries to GitHub Releases on tag push

## Target Platforms

| Platform | Python Binary | Rust Binary | Runner |
|----------|--------------|-------------|--------|
| Linux x86_64 | `tea-python-linux-x86_64` | `tea-rust-linux-x86_64` | `ubuntu-latest` |
| Linux ARM64 | `tea-python-linux-arm64` | `tea-rust-linux-arm64` | `ubuntu-24.04-arm` |
| Windows x86_64 | `tea-python-windows-x86_64.exe` | `tea-rust-windows-x86_64.exe` | `windows-latest` |
| macOS x86_64 | `tea-python-darwin-x86_64` | `tea-rust-darwin-x86_64` | `macos-13` |
| macOS ARM64 | `tea-python-darwin-arm64` | `tea-rust-darwin-arm64` | `macos-latest` (M1) |

## Acceptance Criteria

### Build Infrastructure

- [ ] **AC-1**: GitHub Actions workflow triggers on `v*` tag push
- [ ] **AC-2**: Build matrix covers all 5 target platforms
- [ ] **AC-3**: Builds run in parallel for each platform
- [ ] **AC-4**: Build artifacts are cached to speed up subsequent runs

### Python Standalone Binaries

- [ ] **AC-5**: Python binary is created using PyInstaller with `--onefile` flag
- [ ] **AC-6**: Binary includes all dependencies (no external Python needed)
- [ ] **AC-7**: Binary size is under 50MB per platform
- [ ] **AC-8**: Binary executes `tea-agent --version` successfully on clean system
- [ ] **AC-9**: Binary name follows convention: `tea-agent-{os}-{arch}[.exe]`

### Rust Binaries

- [ ] **AC-10**: Rust binary is built with `--release` flag for optimizations
- [ ] **AC-11**: Binary is statically linked where possible (musl on Linux)
- [ ] **AC-12**: Binary size is under 20MB per platform
- [ ] **AC-13**: Binary executes `tea --version` successfully on clean system
- [ ] **AC-14**: Binary name follows convention: `tea-{os}-{arch}[.exe]`

### GitHub Release

- [ ] **AC-15**: Release is created automatically on tag push
- [ ] **AC-16**: Release includes all 10 binaries (5 Python + 5 Rust)
- [ ] **AC-17**: Release includes checksums file (SHA256SUMS.txt)
- [ ] **AC-18**: Release notes are auto-generated from commits since last tag
- [ ] **AC-19**: Release includes existing Python wheel (.whl) files

### Verification

- [ ] **AC-20**: Smoke tests run on each binary before upload
- [ ] **AC-21**: Version in binary matches git tag version
- [ ] **AC-22**: CI fails if any binary fails smoke test

## Technical Design

### GitHub Actions Workflow

```yaml
# .github/workflows/release.yaml
name: Release

on:
  push:
    tags:
      - 'v*'

permissions:
  contents: write

env:
  PYTHON_VERSION: '3.11'
  RUST_VERSION: 'stable'

jobs:
  # ============================================================
  # Python Standalone Binaries (PyInstaller)
  # ============================================================
  build-python:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: linux-x86_64
            ext: ''
          - os: ubuntu-24.04-arm
            target: linux-arm64
            ext: ''
          - os: windows-latest
            target: windows-x86_64
            ext: '.exe'
          - os: macos-13
            target: darwin-x86_64
            ext: ''
          - os: macos-latest
            target: darwin-arm64
            ext: ''

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: ${{ env.PYTHON_VERSION }}

      - name: Install dependencies
        run: |
          cd python
          pip install -e .[dev]
          pip install pyinstaller

      - name: Build standalone binary
        run: |
          cd python
          pyinstaller --onefile --name tea-agent-${{ matrix.target }} \
            --hidden-import=the_edge_agent \
            --collect-all the_edge_agent \
            src/the_edge_agent/cli.py

      - name: Smoke test
        run: |
          ./python/dist/tea-agent-${{ matrix.target }}${{ matrix.ext }} --version

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: python-${{ matrix.target }}
          path: python/dist/tea-agent-${{ matrix.target }}${{ matrix.ext }}

  # ============================================================
  # Rust Binaries
  # ============================================================
  build-rust:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            target: x86_64-unknown-linux-musl
            artifact: tea-rust-linux-x86_64
            ext: ''
          - os: ubuntu-24.04-arm
            target: aarch64-unknown-linux-musl
            artifact: tea-rust-linux-arm64
            ext: ''
          - os: windows-latest
            target: x86_64-pc-windows-msvc
            artifact: tea-rust-windows-x86_64
            ext: '.exe'
          - os: macos-13
            target: x86_64-apple-darwin
            artifact: tea-rust-darwin-x86_64
            ext: ''
          - os: macos-latest
            target: aarch64-apple-darwin
            artifact: tea-rust-darwin-arm64
            ext: ''

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Setup Rust
        uses: dtolnay/rust-action@stable
        with:
          targets: ${{ matrix.target }}

      - name: Install musl tools (Linux only)
        if: contains(matrix.target, 'musl')
        run: sudo apt-get update && sudo apt-get install -y musl-tools

      - name: Build release binary
        run: |
          cd rust
          cargo build --release --target ${{ matrix.target }} --bin tea
          cp target/${{ matrix.target }}/release/tea${{ matrix.ext }} ../tea-${{ matrix.artifact }}${{ matrix.ext }}

      - name: Smoke test
        run: ./tea-${{ matrix.artifact }}${{ matrix.ext }} --version

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: rust-${{ matrix.artifact }}
          path: tea-${{ matrix.artifact }}${{ matrix.ext }}

  # ============================================================
  # Python Wheel (existing)
  # ============================================================
  build-wheel:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: ${{ env.PYTHON_VERSION }}

      - name: Build wheel
        run: |
          cd python
          pip install build
          python -m build

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: python-wheel
          path: python/dist/*.whl

  # ============================================================
  # Create Release
  # ============================================================
  release:
    needs: [build-python, build-rust, build-wheel]
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Download all artifacts
        uses: actions/download-artifact@v4
        with:
          path: dist

      - name: Flatten artifacts
        run: |
          mkdir -p release
          find dist -type f -exec mv {} release/ \;

      - name: Generate checksums
        run: |
          cd release
          sha256sum * > SHA256SUMS.txt

      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          files: release/*
          generate_release_notes: true
          draft: false
          prerelease: ${{ contains(github.ref, 'alpha') || contains(github.ref, 'beta') || contains(github.ref, 'rc') }}
```

### PyInstaller Configuration

Create `python/tea-agent.spec` for reproducible builds:

```python
# python/tea-agent.spec
# -*- mode: python ; coding: utf-8 -*-

block_cipher = None

a = Analysis(
    ['src/the_edge_agent/cli.py'],
    pathex=[],
    binaries=[],
    datas=[],
    hiddenimports=[
        'the_edge_agent',
        'the_edge_agent.yaml_engine',
        'the_edge_agent.stategraph',
        'yaml',
        'json',
        'jinja2',
    ],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[
        'tkinter',
        'matplotlib',
        'numpy',
        'pandas',
    ],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False,
)

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

exe = EXE(
    pyz,
    a.scripts,
    a.binaries,
    a.zipfiles,
    a.datas,
    [],
    name='tea-agent',
    debug=False,
    bootloader_ignore_signals=False,
    strip=True,
    upx=True,
    upx_exclude=[],
    runtime_tmpdir=None,
    console=True,
    disable_windowed_traceback=False,
    argv_emulation=False,
    target_arch=None,
    codesign_identity=None,
    entitlements_file=None,
)
```

### Rust Static Linking (Linux)

Update `rust/.cargo/config.toml`:

```toml
[target.x86_64-unknown-linux-musl]
linker = "x86_64-linux-musl-gcc"

[target.aarch64-unknown-linux-musl]
linker = "aarch64-linux-musl-gcc"
```

## Alternative Approaches Considered

### Python Bundling

| Tool | Pros | Cons |
|------|------|------|
| **PyInstaller** (chosen) | Mature, well-documented, --onefile | Larger binaries |
| PyOxidizer | Smaller binaries, Rust-based | More complex setup |
| Nuitka | AOT compilation, faster execution | Long compile times |
| cx_Freeze | Simple API | Less portable |

**Decision**: PyInstaller for maturity and simplicity.

### Cross-Compilation

| Approach | Pros | Cons |
|----------|------|------|
| **Native runners** (chosen) | Reliable, no SDK issues | Requires paid ARM runners |
| Cross-compile from Linux | Single runner | macOS requires SDK, Windows needs mingw |
| Docker multi-arch | Consistent environment | Slow, macOS not supported |

**Decision**: Native GitHub runners for reliability.

## Out of Scope

- Code signing (macOS notarization, Windows Authenticode) - future story
- Homebrew/apt/chocolatey package managers - future story
- Automatic version bumping - manual tag for now
- ARM Windows binaries - low demand

## Dependencies

- GitHub Actions with ARM runner access (ubuntu-24.04-arm)
- PyInstaller v6.x
- Rust stable toolchain

## Related Stories

- **TEA-CLI-004**: CLI alignment should be done first for consistent UX across binaries

## Tasks / Subtasks

- [x] **Task 1**: Create `.github/workflows/release.yaml` (AC-1, AC-2, AC-3)
  - [x] Add Python build matrix
  - [x] Add Rust build matrix
  - [x] Add wheel build job
  - [x] Add release job with artifact collection

- [x] **Task 2**: Configure PyInstaller (AC-5, AC-6, AC-7)
  - [x] Create `python/tea-agent.spec`
  - [x] Test onefile build locally
  - [x] Verify hidden imports are included

- [x] **Task 3**: Configure Rust static linking (AC-10, AC-11)
  - [x] Add musl targets to workflow
  - [x] Update `.cargo/config.toml` for cross-linking

- [x] **Task 4**: Add smoke tests (AC-8, AC-13, AC-20, AC-21)
  - [x] Verify `--version` output
  - [x] Verify version matches tag

- [x] **Task 5**: Configure release publishing (AC-15, AC-16, AC-17, AC-18)
  - [x] Generate SHA256SUMS.txt
  - [x] Enable auto-generated release notes
  - [ ] Test with `v0.0.0-test` tag

- [x] **Task 6**: Document release process in README
  - [x] Add download links section
  - [x] Add verification instructions

## Dev Notes

### Testing the Workflow Locally

Use [act](https://github.com/nektos/act) to test locally:

```bash
# Install act
brew install act  # or appropriate for your OS

# Test the workflow (dry run)
act push --dryrun -e .github/workflows/test-release-event.json

# Note: ARM builds cannot be tested locally with act
```

### Binary Naming Convention

```
tea-python-{os}-{arch}[.exe]   # Python standalone
tea-rust-{os}-{arch}[.exe]     # Rust native
```

Where:
- `{os}`: `linux`, `darwin`, `windows`
- `{arch}`: `x86_64`, `arm64`

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | In workflow | `--version` verification |
| Integration | Manual | Test with sample YAML after release |

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Initial story creation | Sarah (PO Agent) |

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered during implementation.

### Completion Notes
1. Created comprehensive `.github/workflows/release.yaml` with 5-platform build matrix for both Python (PyInstaller) and Rust binaries
2. Created `python/tea-agent.spec` for reproducible PyInstaller builds with all required hidden imports
3. Created `rust/.cargo/config.toml` for musl static linking on Linux and optimized release builds
4. Added smoke tests in workflow: `--version` and `--impl` checks, version tag matching, binary size verification
5. Configured SHA256SUMS.txt generation and auto-release notes in release job
6. Updated README.md with download section including binary table, quick install, and verification instructions
7. Fixed `python/tests/test_cli.py` - rewrote for new Typer-based CLI (47 tests, all passing)
8. Note: Task 5 subtask "Test with v0.0.0-test tag" left unchecked - requires actual tag push to test

### Pre-existing Issue Noted
- `actions::memory::tests::test_memory_namespace_isolation_via_prefix` test failure in Rust is pre-existing and unrelated to this story

### File List

| Action | File |
|--------|------|
| Modified | `.github/workflows/release.yaml` |
| Created | `python/tea-agent.spec` |
| Created | `rust/.cargo/config.toml` |
| Modified | `README.md` |
| Modified | `python/tests/test_cli.py` |
| Modified | `docs/stories/TEA-RELEASE-001-multi-platform-binaries.md` |
