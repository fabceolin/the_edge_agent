# TEA-RELEASE-003: Python Prolog Binaries and AppImage Distribution

## Status
Ready for Done

## Story

**As a** developer using The Edge Agent with neurosymbolic AI workflows,
**I want** pre-built Python binaries with Prolog support and an AppImage for easy distribution,
**So that** I can use Prolog actions in Python TEA without installing SWI-Prolog or janus-swi manually.

## Acceptance Criteria

### Python Binary Variants

1. **AC-1**: Release includes `tea-python-{platform}-prolog` binaries (core deps + janus-swi)
2. **AC-2**: Release includes `tea-python-{platform}-full-prolog` binaries (all deps + janus-swi)
3. **AC-3**: Prolog binaries include janus-swi and all pre-loaded Prolog modules (lists, clpfd, apply, aggregate)
4. **AC-4**: Prolog binaries are built with PyInstaller using `--hidden-import=janus_swi`
5. **AC-5**: Prolog binaries use **glibc dynamic linking** (required for libswipl.so compatibility)
6. **AC-6**: Non-prolog Python binaries remain unchanged (existing core/full variants)

### AppImage Distribution (Linux only)

7. **AC-7**: AppImage bundles `tea-python-full-prolog` binary with ALL optional dependencies + `libswipl.so` + janus-swi
8. **AC-8**: AppImage is self-contained and runs on systems without SWI-Prolog, Python, or any dependencies installed
9. **AC-9**: AppImage follows naming convention: `tea-python-{version}-{arch}.AppImage` (x86_64 and aarch64)
10. **AC-10**: AppImage includes desktop entry for GUI integration
11. **AC-11**: Both x86_64 and aarch64 AppImages are built natively on respective runners

### Build Matrix Updates

12. **AC-12**: GitHub Actions builds Prolog variants (both core-prolog and full-prolog) for Linux x86_64 and ARM64
13. **AC-13**: Build jobs install SWI-Prolog 9.1+ from official PPA and janus-swi from PyPI
14. **AC-14**: Build matrix runs in parallel for efficiency
15. **AC-15**: Each binary passes smoke test (`--version`, `--impl`, and Prolog example YAML)

### Documentation

16. **AC-16**: README documents the difference between Python binary variants (core, full, prolog, full-prolog)
17. **AC-17**: README includes Python+Prolog AppImage installation instructions

## Tasks / Subtasks

- [x] Task 1: Add Python Prolog build jobs to release workflow (AC-1, AC-2, AC-3, AC-4, AC-5, AC-12, AC-13)
  - [x] Add `build-python-linux-prolog-x86_64` job (core deps + janus-swi)
  - [x] Add `build-python-linux-prolog-arm64` job (core deps + janus-swi)
  - [x] Add `build-python-linux-full-prolog-x86_64` job (all deps + janus-swi)
  - [x] Add `build-python-linux-full-prolog-arm64` job (all deps + janus-swi)
  - [x] Install SWI-Prolog 9.1+ from official PPA and janus-swi from PyPI
  - [x] Configure PyInstaller with `--hidden-import=janus_swi` and `--collect-all janus_swi`
  - [x] Add smoke tests for all prolog binaries (`--version`, `--impl`)
  - [x] Run prolog example YAML for functional smoke test (e.g., `examples/prolog/simple-prolog-agent.yaml`)
  - [x] Bundle SWI-Prolog runtime directory for AppImage job

- [x] Task 2: Create Python Prolog AppImage build jobs (AC-7, AC-8, AC-9, AC-10, AC-11)
  - [x] Add `build-python-appimage-x86_64` job on `ubuntu-latest`
  - [x] Add `build-python-appimage-aarch64` job on `ubuntu-24.04-arm`
  - [x] Use `tea-python-linux-{arch}-full-prolog` as base binary
  - [x] Install `linuxdeploy` (arch-specific) for auto-dependency bundling
  - [x] Create AppDir structure with Python binary and libraries
  - [x] Bundle `libswipl.so` and transitive dependencies via linuxdeploy
  - [x] Bundle SWI-Prolog runtime directory (`/usr/lib/swi-prolog` → `usr/lib/swipl`)
  - [x] Create `.desktop` file (icon optional for CLI tool)
  - [x] Build AppImage with custom AppRun setting `SWI_HOME_DIR` and `LD_LIBRARY_PATH`
  - [x] Test AppImage on clean Ubuntu container

- [x] Task 3: Update release job to include new artifacts (AC-14)
  - [x] Add all prolog Python binaries (4 variants) to artifact collection
  - [x] Add Python AppImages (2 variants) to artifact collection
  - [x] Update SHA256SUMS generation

- [x] Task 4: Update documentation (AC-16, AC-17)
  - [x] Add Python binary variants table to README (core, full, prolog, full-prolog, AppImage)
  - [x] Add Python AppImage usage instructions
  - [x] Document janus-swi and SWI-Prolog 9.1+ requirements

## Dev Notes

### Binary Naming Convention

| Binary | Description | Prolog | Size (est.) |
|--------|-------------|--------|-------------|
| `tea-python-linux-x86_64` | Core features only | No | ~15MB |
| `tea-python-linux-x86_64-full` | All optional deps (LLM, RAG, etc.) | No | ~80MB |
| `tea-python-linux-x86_64-prolog` | Core + Prolog (janus-swi bundled) | Yes* | ~25MB |
| `tea-python-linux-x86_64-full-prolog` | **All deps + Prolog** | Yes* | ~90MB |
| `tea-python-{version}-x86_64.AppImage` | **Full + Prolog + SWI-Prolog runtime (self-contained)** | Yes | ~150MB |
| `tea-python-linux-arm64` | Core features only | No | ~15MB |
| `tea-python-linux-arm64-full` | All optional deps | No | ~80MB |
| `tea-python-linux-arm64-prolog` | Core + Prolog (janus-swi bundled) | Yes* | ~25MB |
| `tea-python-linux-arm64-full-prolog` | **All deps + Prolog** | Yes* | ~90MB |
| `tea-python-{version}-aarch64.AppImage` | **Full + Prolog + SWI-Prolog runtime (self-contained)** | Yes | ~150MB |

*Requires SWI-Prolog 9.1+ installed on the system (`apt install swi-prolog`)

> **Note**: AppImage is the **batteries-included** choice for neurosymbolic AI - bundles ALL optional dependencies
> (openai, numpy, chromadb, pandas, etc.) PLUS janus-swi + libswipl.so + SWI-Prolog runtime. Zero system deps required.

### Python-Prolog Architecture

```
┌─────────────────────────────────────┐
│  Python TEA (PyInstaller binary)    │
│  ┌─────────────────────────────────┐│
│  │ the_edge_agent + dependencies   ││
│  │ janus-swi (Python bindings)     ││
│  └─────────────────────────────────┘│
│            ↓ FFI                    │
│  ┌─────────────────────────────────┐│
│  │ libswipl.so (SWI-Prolog core)   ││
│  │ SWI-Prolog runtime (boot.prc,  ││
│  │   library/, lib/*.so)           ││
│  └─────────────────────────────────┘│
└─────────────────────────────────────┘
```

### Key Differences from Rust Prolog (TEA-RELEASE-002)

| Aspect | Rust (swipl-rs) | Python (janus-swi) |
|--------|-----------------|-------------------|
| **Binding** | Rust FFI crate | Python C extension |
| **SWI-Prolog Version** | 8.x or 9.x | **9.1+ required** |
| **Module Loading** | `use_module/1` via query | `consult_string()` native |
| **Packaging Tool** | Cargo + linuxdeploy | PyInstaller + linuxdeploy |
| **Binary Type** | Native ELF | Python embedded + libs |

### SWI-Prolog Installation in CI

```yaml
- name: Install SWI-Prolog 9.1+
  run: |
    # Official PPA required for 9.1+
    sudo apt-add-repository -y ppa:swi-prolog/stable
    sudo apt-get update
    sudo apt-get install -y swi-prolog-nox pkg-config

    # Verify version (must be 9.1+)
    swipl --version

- name: Install janus-swi
  run: pip install janus-swi
```

### PyInstaller Build Commands

```bash
# With Prolog support
pip install janus-swi
pyinstaller --onefile --name tea-python-linux-x86_64-prolog \
  --hidden-import=the_edge_agent \
  --hidden-import=the_edge_agent.yaml_engine \
  --hidden-import=the_edge_agent.stategraph \
  --hidden-import=janus_swi \
  --collect-all the_edge_agent \
  --collect-all janus_swi \
  --exclude-module tkinter \
  --exclude-module matplotlib \
  --strip \
  src/the_edge_agent/cli.py
```

### AppImage Tooling

Use [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) for automatic dependency bundling:

```bash
# Download linuxdeploy (arch-specific)
wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
chmod +x linuxdeploy-x86_64.AppImage

# Build AppImage
./linuxdeploy-x86_64.AppImage \
  --appdir tea-python.AppDir \
  --executable tea-python-linux-x86_64-prolog \
  --desktop-file tea-python.desktop \
  --output appimage
```

### AppImage Structure

```
tea-python.AppDir/
├── AppRun (custom script)
├── tea-python.desktop
├── usr/
│   ├── bin/
│   │   └── tea (the Python binary)
│   └── lib/
│       ├── swipl/           # SWI-Prolog runtime (from /usr/lib/swi-prolog)
│       │   ├── boot.prc     # CRITICAL: VM initialization
│       │   ├── library/     # Standard library
│       │   └── lib/         # Architecture-specific extensions
│       ├── libswipl.so.9
│       ├── libgmp.so.10
│       ├── libpython3.11.so
│       └── ... (auto-bundled by linuxdeploy)
```

### AppRun Script (custom)

```bash
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
export SWI_HOME_DIR="${HERE}/usr/lib/swipl"
# Python may need PYTHONHOME/PYTHONPATH adjustments
export PYTHONDONTWRITEBYTECODE=1
exec "${HERE}/usr/bin/tea" "$@"
```

> **Note**: Custom AppRun is needed to set `SWI_HOME_DIR` for Prolog runtime.

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | In workflow | `--version` and `--impl` for all binaries |
| Prolog import test | In workflow | Verify janus-swi loads correctly |
| Functional test | In workflow | Run `examples/prolog/simple-prolog-agent.yaml` |
| AppImage test | In workflow | Run on clean Ubuntu container |

### Prolog Feature Detection

The binary should gracefully handle missing Prolog:
- Without janus-swi: Prolog actions return error "Prolog support not available (install janus-swi)"
- With janus-swi but no SWI-Prolog: Import error at startup
- AppImage: Always works (everything bundled)

### Platform Support

| Platform | Core | Full | Prolog | Full-Prolog | AppImage |
|----------|------|------|--------|-------------|----------|
| Linux x86_64 | Yes | Yes | Yes | Yes | Yes |
| Linux ARM64 | Yes | Yes | Yes | Yes | Yes |
| macOS ARM64 | Yes | Yes | No* | No* | N/A |
| macOS x86_64 | No** | No** | No* | No* | N/A |
| Windows x86_64 | Yes | Yes | No* | No* | N/A |

*macOS/Windows Prolog builds require SWI-Prolog SDK installation in CI, deferred to future story.
**PyInstaller cannot cross-compile Python from ARM64 to x86_64.

### User Decision Guide

```
Do you need Prolog support in Python TEA?
├── No  → Use `tea-python-linux-{arch}` (smallest, core deps)
│         or `tea-python-linux-{arch}-full` (all optional deps)
│
└── Yes → Is SWI-Prolog 9.1+ installed on your system?
          │
          ├── Yes → Do you need full deps (openai, numpy, chromadb, etc.)?
          │         ├── No  → Use `tea-python-linux-{arch}-prolog` (core + Prolog)
          │         └── Yes → Use `tea-python-linux-{arch}-full-prolog` (all deps + Prolog)
          │
          └── No  → Use `tea-python-{version}-{arch}.AppImage`
                    (RECOMMENDED: batteries-included, zero system deps)
```

### janus-swi Version Requirements

- **janus-swi**: Latest from PyPI (tested with 1.3+)
- **SWI-Prolog**: 9.1+ (official PPA provides 9.2.x)
- **Python**: 3.9+ (same as TEA core requirement)

### License Considerations

- janus-swi: BSD-2-Clause (permissive)
- SWI-Prolog core: BSD-2-Clause (permissive)
- Some SWI-Prolog libraries: LGPL
- Dynamic linking with LGPL is compatible with MIT license
- AppImage distribution is compliant as it bundles shared libraries

## Out of Scope

- macOS/Windows Prolog binaries (requires SDK setup in CI)
- Flatpak/Snap packaging (future story)
- Prolog binaries for platforms without native runners (cross-compilation not feasible)

## Dependencies

- TEA-RELEASE-001 (completed) - base release workflow with Python binaries
- TEA-RELEASE-002 (completed) - Rust Prolog AppImage (reference implementation)
- TEA-PY-005 (completed) - janus-swi migration for Python Prolog support
- SWI-Prolog 9.1+ packages available via official PPA
- [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) for AppImage creation

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Status | Description |
|------|--------|-------------|
| `.github/workflows/release.yaml` | Modified | Added 4 Python Prolog build jobs, 2 Python AppImage jobs, updated release job dependencies |
| `docs/installation.md` | Modified | Added Python binary variants table, Mermaid decision flowcharts, Python AppImage documentation |

### Completion Notes

- All 4 Python Prolog build jobs added (x86_64 and ARM64 for both core and full variants)
- 2 Python AppImage build jobs added (x86_64 and aarch64)
- Native runners used for glibc dynamic linking compatibility (required for libswipl.so)
- SWI-Prolog 9.1+ installed from official PPA, janus-swi from PyPI
- PyInstaller configured with `--hidden-import=janus_swi` and `--collect-all janus_swi`
- Smoke tests added for all binaries (`--version`, `--impl`)
- Functional tests run Prolog example YAML
- SWI-Prolog runtime bundled for AppImage jobs
- AppImages use custom AppRun with `SWI_HOME_DIR` and `PYTHONDONTWRITEBYTECODE` settings
- Documentation updated with Mermaid decision flowcharts

### Debug Log References
N/A - No blocking issues encountered

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-03 | 1.0 | Initial story creation | Sarah (PO Agent) |
| 2026-01-03 | 1.1 | Story checklist passed, status changed to Approved | Bob (Scrum Master) |
| 2026-01-03 | 1.2 | Implementation complete, all tasks done, status changed to Ready for Review | James (Dev Agent) |
| 2026-01-03 | 1.3 | QA fixes applied: IMPL-001 (AppImage full-prolog deps), TEST-001 (x86_64 Prolog test) | James (Dev Agent) |
| 2026-01-03 | 1.4 | QA gate PASS, status changed to Ready for Merge | James (Dev Agent) |

## QA Results

### Review Date: 2026-01-03

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The implementation is well-structured and follows existing patterns from the Rust Prolog AppImage build (TEA-RELEASE-002). All 17 acceptance criteria have corresponding implementations. The workflow YAML is clean with good comments and consistent job structure.

### Refactoring Performed

None - code quality is acceptable.

### Compliance Check

- Coding Standards: ✓ Follows existing workflow patterns
- Project Structure: ✓ Files in correct locations
- Testing Strategy: ✓ Smoke tests + functional tests included
- All ACs Met: ✓ 17/17 ACs have implementations

### Improvements Checklist

- [x] **CONCERN**: AppImage jobs use core-prolog binary instead of full-prolog (AC-7 specifies "ALL optional dependencies")
  - File: `.github/workflows/release.yaml:1829,2005`
  - Change `needs: [build-python-linux-prolog-x86_64]` to `needs: [build-python-linux-full-prolog-x86_64]`
  - Change `needs: [build-python-linux-prolog-arm64]` to `needs: [build-python-linux-full-prolog-arm64]`
  - **FIXED**: 2026-01-03 - Updated needs dependencies and artifact paths to use full-prolog binaries
- [x] **MINOR**: x86_64 AppImage Docker test should run Prolog example like aarch64 does
  - File: `.github/workflows/release.yaml:1982-1988`
  - Add: `./work/tea-python-*-x86_64.AppImage --appimage-extract-and-run run examples/prolog/simple-prolog-agent.yaml --input '{"value": 21}'`
  - **FIXED**: 2026-01-03 - Added Prolog functional test to x86_64 Docker test

### Security Review

No security concerns. Build process uses official PPAs and trusted artifact sources.

### Performance Considerations

Jobs run in parallel where possible. Native runners used for proper architecture support.

### Files Modified During Review

None - no refactoring performed.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RELEASE-003-python-prolog-appimage.yml

### Recommended Status

**✓ APPROVED for Merge** - All concerns addressed. QA gate PASS with quality score 100.

