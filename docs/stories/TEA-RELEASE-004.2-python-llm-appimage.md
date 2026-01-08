# Story TEA-RELEASE-004.2: Python LLM AppImage with Bundled Gemma 3n E4B

## Status

Draft

## Story

**As a** developer using The Edge Agent Python,
**I want** a Python AppImage with bundled Gemma 3n E4B model,
**So that** I can run LLM-powered workflows offline without installing llama-cpp-python or downloading models.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-RELEASE-003 Python AppImage build infrastructure
- Technology: Python + llama-cpp-python + PyInstaller + linuxdeploy + GGUF
- Follows pattern: Existing Python AppImage build jobs in `.github/workflows/release.yaml`
- Touch points: `python/pyproject.toml`, `python/src/the_edge_agent/actions/`, `.github/workflows/release.yaml`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: Release includes `tea-python-llm-{version}-x86_64.AppImage` with bundled Gemma 3n E4B Q4_K_M model
2. **AC-2**: Release includes `tea-python-llm-{version}-aarch64.AppImage` with bundled Gemma 3n E4B Q4_K_M model
3. **AC-3**: `llm.call` action routes to local llama-cpp-python backend when model is present
4. **AC-4**: AppImage runs on systems without any LLM/Python dependencies installed (fully self-contained)
5. **AC-5**: Model path auto-detected from AppImage extraction directory or `TEA_MODEL_PATH` env var

### Build Requirements

6. **AC-6**: Add `llama-cpp-python` to PyInstaller build
7. **AC-7**: GitHub Actions job `build-python-llm-appimage-x86_64` builds on `ubuntu-latest`
8. **AC-8**: GitHub Actions job `build-python-llm-appimage-aarch64` builds on `ubuntu-24.04-arm`
9. **AC-9**: Model file downloaded from HuggingFace during build
10. **AC-10**: AppImage bundled with linuxdeploy including model file

### Quality Requirements

11. **AC-11**: Smoke test: `--version` and `--impl` pass
12. **AC-12**: Functional test: Run `examples/llm/local-chat.yaml` with local model
13. **AC-13**: Python build without llama-cpp-python continues to work (graceful import handling)
14. **AC-14**: Existing Python Prolog AppImage (TEA-RELEASE-003) unchanged

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-python dependency to Python package (AC: 6, 13)
  - [ ] Add `llama-cpp-python` to pyproject.toml as optional dependency
  - [ ] Create `[llm-local]` extras group
  - [ ] Implement graceful import with fallback message if not installed
  - [ ] Run `pytest` to confirm no regressions

- [ ] Task 2: Implement local LLM backend in Python (AC: 3, 5)
  - [ ] Create `python/src/the_edge_agent/actions/llm_local.py` module
  - [ ] Implement `LlmLocalBackend` class with llama-cpp-python
  - [ ] Support `llm.call` action with local model
  - [ ] Auto-detect model path from `$APPDIR/usr/share/models/`
  - [ ] Fallback to `TEA_MODEL_PATH` environment variable

- [ ] Task 3: Add x86_64 LLM AppImage build job (AC: 1, 7, 9, 10)
  - [ ] Create `build-python-llm-appimage-x86_64` job in release.yaml
  - [ ] Install `llama-cpp-python` in build environment
  - [ ] Configure PyInstaller with `--collect-all llama_cpp`
  - [ ] Download `gemma-3n-E4B-it-Q4_K_M.gguf` from HuggingFace
  - [ ] Bundle model into AppDir at `usr/share/models/`
  - [ ] Use linuxdeploy to create AppImage

- [ ] Task 4: Add ARM64 LLM AppImage build job (AC: 2, 8, 9, 10)
  - [ ] Create `build-python-llm-appimage-aarch64` job in release.yaml
  - [ ] Use native ARM64 runner for glibc compatibility
  - [ ] Same model bundling approach as x86_64

- [ ] Task 5: Add smoke and functional tests (AC: 11, 12)
  - [ ] Add `--version` and `--impl` smoke tests in workflow
  - [ ] Use `examples/llm/local-chat.yaml` example (shared with Rust)
  - [ ] Run example in workflow using bundled model
  - [ ] Test AppImage on clean Ubuntu container

- [ ] Task 6: Update release job artifacts (AC: 14)
  - [ ] Add Python LLM AppImages to artifact collection
  - [ ] Update SHA256SUMS generation
  - [ ] Ensure existing Python Prolog AppImages still build

## Dev Notes

### pyproject.toml Configuration

```toml
[project.optional-dependencies]
llm-local = [
    "llama-cpp-python>=0.2.0",
]

# Full bundle includes everything
llm-full = [
    "the_edge_agent[full]",
    "the_edge_agent[llm-local]",
]
```

### Graceful Import Pattern

```python
# python/src/the_edge_agent/actions/llm_local.py
try:
    from llama_cpp import Llama
    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False
    Llama = None

def check_llm_local_available():
    if not LLAMA_CPP_AVAILABLE:
        raise ImportError(
            "llama-cpp-python not installed. "
            "Install with: pip install the_edge_agent[llm-local]"
        )
```

### Model Path Resolution

```python
import os

def get_model_path() -> str:
    # 1. Explicit environment variable
    if path := os.environ.get("TEA_MODEL_PATH"):
        return path

    # 2. AppImage extraction directory
    if appdir := os.environ.get("APPDIR"):
        candidate = os.path.join(appdir, "usr/share/models/gemma-3n-E4B-it-Q4_K_M.gguf")
        if os.path.exists(candidate):
            return candidate

    # 3. Default cache location
    cache_dir = os.path.expanduser("~/.cache/tea/models")
    return os.path.join(cache_dir, "gemma-3n-E4B-it-Q4_K_M.gguf")
```

### PyInstaller Configuration

```yaml
- name: Build Python LLM binary
  run: |
    pip install llama-cpp-python
    pip install pyinstaller
    pyinstaller --onefile --name tea-python-llm-linux-x86_64 \
      --hidden-import=the_edge_agent \
      --hidden-import=llama_cpp \
      --collect-all the_edge_agent \
      --collect-all llama_cpp \
      --exclude-module tkinter \
      --strip \
      python/src/the_edge_agent/cli.py
```

### AppDir Structure

```
tea-python-llm.AppDir/
├── AppRun (custom script)
├── tea-python.desktop
├── usr/
│   ├── bin/
│   │   └── tea (PyInstaller binary with llama-cpp-python)
│   ├── lib/
│   │   ├── libpython3.11.so
│   │   ├── libswipl.so.9 (if Prolog also included)
│   │   └── ... (dependencies)
│   └── share/
│       └── models/
│           └── gemma-3n-E4B-it-Q4_K_M.gguf (~4.5GB)
```

### Custom AppRun Script

```bash
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
export APPDIR="${HERE}"
export TEA_MODEL_PATH="${HERE}/usr/share/models/gemma-3n-E4B-it-Q4_K_M.gguf"
export PYTHONDONTWRITEBYTECODE=1
export SWI_HOME_DIR="${HERE}/usr/lib/swipl"  # If Prolog included
exec "${HERE}/usr/bin/tea" "$@"
```

### Relevant Source Files

```
python/
├── pyproject.toml          # Add llama-cpp-python optional dep
├── src/the_edge_agent/
│   ├── actions/
│   │   ├── __init__.py     # Register llm_local action
│   │   └── llm_local.py    # NEW: Local LLM backend
│   └── yaml_engine.py      # Action routing logic
```

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | In workflow | `--version` and `--impl` |
| Functional test | In workflow | Run local-chat.yaml with bundled model |
| Container test | In workflow | Run on clean Ubuntu container |
| Unit tests | python/tests/ | Test llm_local module with mocked Llama |

### llama-cpp-python Installation Notes

For CI builds, install with:
```bash
# CPU only (default)
pip install llama-cpp-python

# With CUDA (if GPU available in CI)
CMAKE_ARGS="-DLLAMA_CUBLAS=on" pip install llama-cpp-python
```

## Definition of Done

- [ ] Python package has optional `llm-local` dependency
- [ ] `llm.call` action works with local llama-cpp-python backend
- [ ] x86_64 LLM AppImage builds successfully
- [ ] aarch64 LLM AppImage builds successfully
- [ ] Smoke tests pass for both architectures
- [ ] Functional test runs local-chat.yaml successfully
- [ ] Existing Python Prolog AppImages unchanged
- [ ] Model auto-detected from AppImage bundle

## Risk and Compatibility Check

**Primary Risk:** llama-cpp-python compilation on ARM64 may require specific CMAKE flags

**Mitigation:** Use native ARM64 runner, pre-test compilation in separate workflow

**Rollback:** Optional dependency, can be excluded from release

## Compatibility Verification

- [x] No breaking changes to existing APIs (additive feature)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: Large file download in CI, acceptable

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
