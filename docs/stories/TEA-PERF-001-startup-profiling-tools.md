# Story TEA-PERF-001: Startup Profiling Tools

## Status

Approved

## Story

**As a** developer/maintainer,
**I want** tools to profile and analyze the startup time of the tea CLI (both Python dev and PyInstaller binary),
**so that** I can identify import bottlenecks, slow module loads, and optimization opportunities for cold-start performance.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/cli.py` (Typer-based CLI entry point)
- Technology: Python 3.x, PyInstaller (binaries), pytest (testing)
- Follows pattern: Existing CLI module structure (`python/src/the_edge_agent/`)
- Touch points: CLI entry point, `__init__.py` imports, action modules, runtimes (Lua, Prolog)

**Current Import Chain (to be profiled):**

```
cli.py
 └─> the_edge_agent/__init__.py
      └─> stategraph.py
      └─> yaml_engine.py
           └─> actions/__init__.py (40+ action modules)
           └─> lua_runtime.py (lupa)
           └─> prolog_runtime.py (janus-swi)
           └─> jinja2, yaml, json, etc.
```

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1**: Create a `scripts/profile_startup.py` script that measures import time for the tea CLI
2. **AC-2**: Script outputs timing breakdown by module (top-level and nested imports)
3. **AC-3**: Script can profile both `python -m the_edge_agent.cli` and PyInstaller binary
4. **AC-4**: Support for `--baseline` mode to establish reference timings
5. **AC-5**: Support for `--compare` mode to compare current vs baseline
6. **AC-6**: Output formats: text (human-readable), JSON (for CI), CSV (for analysis)

**Integration Requirements:**

7. Existing CLI functionality continues to work unchanged
8. Profiling tools are development-only (not shipped in production binary)
9. Script works on Linux, macOS, and Windows

**Quality Requirements:**

10. Script is self-documenting with `--help`
11. Tests validate script execution (smoke tests)
12. Documentation for interpreting profiling results

## Technical Notes

### Profiling Approaches

**Python Import Profiling:**
```python
# Option A: importlib hooks
import sys
import importlib.abc
import importlib.machinery

class ImportTimer(importlib.abc.MetaPathFinder):
    def find_module(self, fullname, path=None):
        # Record timing for each import
        pass

# Option B: python -X importtime
# python -X importtime -c "from the_edge_agent.cli import main"

# Option C: Custom sys.settrace for fine-grained analysis
```

**Binary Profiling:**
```bash
# Wall-clock time for PyInstaller binary
time ./dist/tea-python-linux-x86_64 --version

# Detailed startup profiling with strace (Linux)
strace -tt -T ./dist/tea-python-linux-x86_64 --version 2>&1 | grep -E "open|read"

# macOS equivalent with dtruss
sudo dtruss ./dist/tea-python-darwin-arm64 --version 2>&1
```

**Expected Bottleneck Areas:**
- Heavy imports: `numpy`, `pandas` (if loaded eagerly)
- Runtime initialization: `lupa` (Lua), `janus-swi` (Prolog)
- Action modules: 40+ modules in `actions/__init__.py`
- Jinja2 template engine initialization

### Key Constraints

- Profile must not affect production runtime (development-only)
- Script should be runnable from project root
- Output should identify "quick wins" (modules that can be lazy-loaded)

### Implementation Guidance

```python
# scripts/profile_startup.py structure
import argparse
import time
import subprocess
import sys
import json
from pathlib import Path

def profile_python_import():
    """Profile import time for Python development mode."""
    pass

def profile_binary_startup(binary_path: str):
    """Profile cold-start time for PyInstaller binary."""
    pass

def generate_report(timings: dict, format: str):
    """Generate human-readable or machine-parseable report."""
    pass

def main():
    parser = argparse.ArgumentParser(
        description="Profile tea CLI startup time"
    )
    parser.add_argument("--mode", choices=["python", "binary"], default="python")
    parser.add_argument("--binary", help="Path to PyInstaller binary")
    parser.add_argument("--format", choices=["text", "json", "csv"], default="text")
    parser.add_argument("--baseline", help="Save results as baseline")
    parser.add_argument("--compare", help="Compare against baseline")
    parser.add_argument("--runs", type=int, default=5, help="Number of runs for averaging")
    args = parser.parse_args()
    # ...
```

### Output Example

```
=== Tea CLI Startup Profile ===
Mode: python
Runs: 5 (averaged)

Total startup time: 1.234s

Top 10 Slowest Imports:
  1. lupa                          0.312s (25.3%)
  2. janus_swi                     0.198s (16.0%)
  3. numpy                         0.156s (12.6%)
  4. jinja2                        0.089s (7.2%)
  5. yaml                          0.067s (5.4%)
  6. actions/__init__              0.054s (4.4%)
  7. typer                         0.043s (3.5%)
  8. chromadb                      0.041s (3.3%)
  9. networkx                      0.032s (2.6%)
 10. requests                      0.028s (2.3%)

Recommendations:
  - Consider lazy-loading 'lupa' (only needed for Lua nodes)
  - Consider lazy-loading 'janus_swi' (only needed for Prolog nodes)
  - Consider lazy-loading 'numpy' (only needed for specific actions)
```

## Tasks / Subtasks

- [ ] **Task 1: Create profiling script skeleton** (AC: 1, 10)
  - [ ] Create `scripts/profile_startup.py` with argparse CLI
  - [ ] Add `--help` documentation
  - [ ] Add mode selection (python vs binary)

- [ ] **Task 2: Implement Python import profiling** (AC: 2, 3)
  - [ ] Use `-X importtime` parsing or custom meta path finder
  - [ ] Capture nested import hierarchy
  - [ ] Calculate cumulative vs self time

- [ ] **Task 3: Implement binary profiling** (AC: 3)
  - [ ] Wall-clock timing with subprocess
  - [ ] Multiple runs for statistical averaging
  - [ ] Cross-platform support (Linux, macOS, Windows)

- [ ] **Task 4: Implement output formats** (AC: 6)
  - [ ] Text format with colored output (optional)
  - [ ] JSON format for CI integration
  - [ ] CSV format for spreadsheet analysis

- [ ] **Task 5: Implement baseline comparison** (AC: 4, 5)
  - [ ] Save baseline to `.tea-startup-baseline.json`
  - [ ] Compare current run against baseline
  - [ ] Show diff with percentage change

- [ ] **Task 6: Add smoke tests** (AC: 11)
  - [ ] Test script execution in CI
  - [ ] Test all output formats

- [ ] **Task 7: Documentation** (AC: 12)
  - [ ] Add usage section to `docs/python/development-guide.md`
  - [ ] Document how to interpret results

## Dev Notes

### Source Tree Reference

```
python/
├── src/the_edge_agent/
│   ├── __init__.py          # Main package exports
│   ├── cli.py               # CLI entry point (Typer)
│   ├── yaml_engine.py       # YAML configuration loader
│   ├── stategraph.py        # Core state graph
│   ├── lua_runtime.py       # Lua scripting (lupa)
│   ├── prolog_runtime.py    # Prolog scripting (janus-swi)
│   └── actions/
│       ├── __init__.py      # Action registry (40+ modules)
│       ├── llm_actions.py
│       ├── web_actions.py
│       └── ...
└── scripts/                  # NEW: Development scripts
    └── profile_startup.py    # Startup profiler
```

### Known Heavy Dependencies

| Module | Purpose | Lazy-Load Candidate |
|--------|---------|---------------------|
| `lupa` | Lua runtime | Yes (only for Lua nodes) |
| `janus-swi` | Prolog runtime | Yes (only for Prolog nodes) |
| `numpy` | Numeric operations | Yes (only for some actions) |
| `chromadb` | Vector store | Yes (only for RAG actions) |
| `pandas` | Data frames | Yes (only for data actions) |

### Testing

- **Test location**: `python/tests/test_profile_startup.py` (smoke tests)
- **Test framework**: pytest
- **Coverage requirement**: Script execution should not fail

## Definition of Done

- [ ] Functional requirements met (AC 1-6)
- [ ] Integration requirements verified (existing CLI unaffected)
- [ ] Script works cross-platform
- [ ] Tests pass (smoke tests for script execution)
- [ ] Documentation updated

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk**: Script may have platform-specific behavior (strace vs dtruss)
- **Mitigation**: Use subprocess-based wall-clock timing as fallback
- **Rollback**: Script is development-only, can be removed without impact

**Compatibility Verification:**

- [ ] No breaking changes to existing CLI
- [ ] No new production dependencies
- [ ] Script is optional (not required for normal development)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-28 | 0.1 | Initial draft | PO Agent (Sarah) |
