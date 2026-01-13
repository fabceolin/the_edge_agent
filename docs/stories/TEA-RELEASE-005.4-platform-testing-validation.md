# TEA-RELEASE-005.4: Platform Testing & Validation

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.4 |
| **Type** | Story |
| **Priority** | High |
| **Estimated Effort** | 5 points |
| **Status** | Ready for Development |
| **Parent Epic** | TEA-RELEASE-005 |
| **Depends On** | TEA-RELEASE-005.2, TEA-RELEASE-005.3 |
| **Files to Create** | `tests/ape/`, `.github/workflows/test-ape-matrix.yaml` |
| **Files to Modify** | `.github/workflows/build-ape.yaml` |

## Story

**As a** release engineer,
**I want** comprehensive cross-platform testing for APE binaries,
**So that** we ensure quality across Windows, Linux, and macOS.

## Background

### Platform Matrix

Actually Portable Executables should run on:

| OS | Versions | Architecture |
|----|----------|--------------|
| **Linux** | Ubuntu 20.04, 22.04, 24.04 | x86_64 |
| **Linux** | Fedora latest, Arch latest | x86_64 |
| **Windows** | 10, 11 | x86_64 |
| **macOS** | 12 (Monterey), 13 (Ventura), 14 (Sonoma) | x86_64, ARM64 |

### Known Platform Quirks

| Platform | Potential Issue | Mitigation |
|----------|-----------------|------------|
| Windows | Defender false positive | Document exception process |
| macOS | Gatekeeper quarantine | `xattr -d com.apple.quarantine` |
| Linux (Alpine) | musl libc differences | Test specifically |
| WSL | May behave differently | Test WSL1 and WSL2 |

## Acceptance Criteria

- [ ] **AC-1**: Test matrix covers Ubuntu 20.04, 22.04, 24.04
- [ ] **AC-2**: Test matrix covers Windows 10, Windows 11
- [ ] **AC-3**: Test matrix covers macOS 12, 13, 14 (Intel and ARM)
- [ ] **AC-4**: Prolog queries produce identical results across platforms
- [ ] **AC-5**: Lua scripts produce identical results across platforms
- [ ] **AC-6**: LLM inference produces consistent results (deterministic seed)
- [ ] **AC-7**: No Windows Defender blocking (or documented workaround)
- [ ] **AC-8**: No macOS Gatekeeper blocking (or documented workaround)
- [ ] **AC-9**: Platform-specific quirks documented
- [ ] **AC-10**: CI workflow runs full test matrix on release

## Technical Design

### Test Matrix Workflow

```yaml
# .github/workflows/test-ape-matrix.yaml
name: Test APE Matrix

on:
  workflow_call:
    inputs:
      artifact-name:
        required: true
        type: string

jobs:
  test-linux:
    strategy:
      matrix:
        os: [ubuntu-20.04, ubuntu-22.04, ubuntu-24.04]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: ${{ inputs.artifact-name }}

      - name: Run tests
        run: |
          chmod +x tea.com
          ./tea.com --version
          ./tea.com --impl
          ./tea.com run tests/ape/prolog-test.yaml
          ./tea.com run tests/ape/lua-test.yaml

  test-windows:
    strategy:
      matrix:
        os: [windows-2019, windows-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: ${{ inputs.artifact-name }}

      - name: Run tests
        shell: pwsh
        run: |
          .\tea.com --version
          .\tea.com --impl
          .\tea.com run tests/ape/prolog-test.yaml
          .\tea.com run tests/ape/lua-test.yaml

  test-macos:
    strategy:
      matrix:
        os: [macos-12, macos-13, macos-14]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: ${{ inputs.artifact-name }}

      - name: Remove quarantine (if present)
        run: xattr -d com.apple.quarantine tea.com 2>/dev/null || true

      - name: Run tests
        run: |
          chmod +x tea.com
          ./tea.com --version
          ./tea.com --impl
          ./tea.com run tests/ape/prolog-test.yaml
          ./tea.com run tests/ape/lua-test.yaml

  test-linux-llm:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: tea-ape-llm

      - name: Run LLM test
        run: |
          chmod +x tea-llm.com
          ./tea-llm.com run tests/ape/llm-test.yaml --input '{"prompt": "Say hello"}'
```

### Test YAML Agents

```yaml
# tests/ape/prolog-test.yaml
name: prolog-platform-test
state_schema:
  input: int
  result: str
  platform: str

settings:
  prolog:
    backend: scryer

nodes:
  - name: detect-platform
    run: |
      import platform
      return {"platform": platform.system()}

  - name: prolog-test
    language: prolog
    run: |
      factorial(0, 1) :- !.
      factorial(N, F) :-
        N > 0,
        N1 is N - 1,
        factorial(N1, F1),
        F is N * F1.

      test_factorial(Result) :-
        factorial(5, Result).
    query: test_factorial(Result)
    output_var: result

  - name: verify
    run: |
      expected = 120
      actual = int(state["result"])
      if actual != expected:
        raise ValueError(f"Expected {expected}, got {actual}")
      return {"result": f"PASS: factorial(5) = {actual} on {state['platform']}"}

edges:
  - from: __start__
    to: detect-platform
  - from: detect-platform
    to: prolog-test
  - from: prolog-test
    to: verify
  - from: verify
    to: __end__
```

```yaml
# tests/ape/lua-test.yaml
name: lua-platform-test
state_schema:
  input: int
  result: str

nodes:
  - name: lua-test
    language: lua
    run: |
      function fibonacci(n)
        if n <= 1 then return n end
        return fibonacci(n-1) + fibonacci(n-2)
      end

      local result = fibonacci(10)
      return {result = tostring(result)}

  - name: verify
    run: |
      expected = "55"
      actual = state["result"]
      if actual != expected:
        raise ValueError(f"Expected {expected}, got {actual}")
      return {"result": f"PASS: fibonacci(10) = {actual}"}

edges:
  - from: __start__
    to: lua-test
  - from: lua-test
    to: verify
  - from: verify
    to: __end__
```

```yaml
# tests/ape/llm-test.yaml
name: llm-platform-test
state_schema:
  prompt: str
  response: str

settings:
  llm:
    backend: local
    temperature: 0  # Deterministic
    seed: 42

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.prompt }}"
      max_tokens: 50
    output_var: response

  - name: verify
    run: |
      if not state["response"]:
        raise ValueError("LLM returned empty response")
      return {"response": state["response"][:100]}

edges:
  - from: __start__
    to: generate
  - from: generate
    to: verify
  - from: verify
    to: __end__
```

## Tasks / Subtasks

- [ ] **Task 1: Create test YAML agents** (AC: 4, 5, 6)
  - [ ] Create `tests/ape/prolog-test.yaml`
  - [ ] Create `tests/ape/lua-test.yaml`
  - [ ] Create `tests/ape/llm-test.yaml`
  - [ ] Verify deterministic outputs

- [ ] **Task 2: Create test matrix workflow** (AC: 1, 2, 3, 10)
  - [ ] Create `.github/workflows/test-ape-matrix.yaml`
  - [ ] Add Linux matrix (Ubuntu versions)
  - [ ] Add Windows matrix
  - [ ] Add macOS matrix

- [ ] **Task 3: Windows Defender testing** (AC: 7)
  - [ ] Test on fresh Windows VM
  - [ ] Document if Defender flags the binary
  - [ ] Create workaround documentation

- [ ] **Task 4: macOS Gatekeeper testing** (AC: 8)
  - [ ] Test on fresh macOS VM
  - [ ] Document Gatekeeper behavior
  - [ ] Add `xattr` workaround to workflow

- [ ] **Task 5: Document platform quirks** (AC: 9)
  - [ ] Create `docs/shared/ape-platform-notes.md`
  - [ ] Document Windows-specific issues
  - [ ] Document macOS-specific issues
  - [ ] Document Linux distro variations

## Dev Notes

### Deterministic LLM Testing

For consistent cross-platform LLM results:
- Set `temperature: 0`
- Set `seed: 42` (or any fixed value)
- Use short prompts to minimize variation

### Security Software Considerations

| Software | Expected Behavior | Workaround |
|----------|-------------------|------------|
| Windows Defender | May flag as "unrecognized" | Add exception |
| Windows SmartScreen | May block first run | "Run anyway" |
| macOS Gatekeeper | Quarantine attribute | `xattr -d` |
| Linux AppArmor | Usually no issue | - |

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke | CI workflow | Basic `--version` checks |
| Functional | `tests/ape/*.yaml` | Prolog, Lua, LLM tests |
| Integration | Manual | Full agent workflows |

## QA Notes

**Test Design Reference:** `docs/qa/assessments/TEA-RELEASE-005.4-test-design-20260112.md`
**Assessment Date:** 2026-01-12
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Scenarios** | 34 |
| **Unit Tests** | 6 (18%) |
| **Integration Tests** | 16 (47%) |
| **E2E Tests** | 12 (35%) |
| **P0 (Critical)** | 14 |
| **P1 (High)** | 12 |
| **P2 (Medium)** | 6 |
| **P3 (Low)** | 2 |
| **AC Coverage** | 100% (all 10 ACs have tests) |

### Risk Areas Identified

| Risk | Probability | Impact | Status |
|------|-------------|--------|--------|
| Windows Defender blocks execution | **High** | **High** | Tests 005.4-E2E-005, 006 cover this with workaround documentation |
| macOS Gatekeeper quarantine | **High** | **High** | Tests 005.4-E2E-007, 008 cover with `xattr -d` workaround |
| Binary incompatible with older Ubuntu | Medium | High | Platform matrix tests 005.4-INT-001 through 003 validate |
| CI workflow misconfigured | Medium | High | Tests 005.4-INT-016, E2E-013, E2E-014 validate workflow |
| Prolog/Lua output varies by platform | Low | **Critical** | Hash verification across platforms (E2E-001, E2E-002) |
| LLM non-deterministic despite seed | Medium | Medium | Determinism tests with fixed seed verify consistency |

### Recommended Test Scenarios (Priority Order)

**P0 - Must Pass for Release:**
1. TEA binary executes on Ubuntu 20.04, 22.04, 24.04 (INT-001, 002, 003)
2. TEA binary executes on Windows 10, 11 (INT-005, 006)
3. TEA binary executes on macOS 12, 13, 14 (INT-008, 009, 010)
4. Prolog factorial(5)=120 on all platforms (UNIT-001, 002, 003)
5. Lua fibonacci(10)=55 on all platforms (UNIT-004, 005, 006)
6. Cross-platform output hash match (E2E-001, E2E-002)
7. Windows execution without manual intervention (E2E-005)
8. macOS execution after quarantine removal (E2E-007)
9. Workflow file valid and triggers correctly (INT-016, E2E-013)

**P1 - Should Pass:**
- Multi-version Prolog tests, PowerShell execution, LLM determinism
- Documentation accessibility, workflow automation

### Concerns / Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **High Risk** | Security software (Defender/Gatekeeper) may block on fresh VMs | Ensure workaround documentation is created before release |
| **Testing Gap** | ARM64 macOS (M1/M2) not explicitly differentiated from Intel | macOS 14 runner uses ARM64, but cross-architecture verification should be explicit |
| **Dependency** | Story depends on TEA-RELEASE-005.2 and 005.3 completion | Validate dependencies are complete before running matrix tests |
| **Flakiness Risk** | LLM determinism tests may have variance | Use short prompts, verify seed works as expected |

### Quality Gate Recommendation

**CONCERNS** - Story is well-designed but has high-probability security software risks that require documented workarounds. Proceed with development, ensure platform documentation (AC-9) is complete before final validation.

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-12 | 1.2 | Status updated to Ready for Development after QA checklist validation | SM Agent |
| 2026-01-12 | 1.1 | Added QA Notes section with test design assessment | Quinn (Test Architect) |
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

