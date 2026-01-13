# TEA-RELEASE-005: Actually Portable Executable (APE) Distribution

## Status

Ready for Development

**Created:** 2026-01-11
**Author:** Sarah (PO Agent)
**Notes:** Epic covers universal cross-platform binary using Cosmopolitan Libc with Scryer Prolog (Rust-native) replacing SWI-Prolog for the APE variant.
**Status Updated:** 2026-01-12 - QA validation passed. All 47 test scenarios defined, all acceptance criteria covered, all risks mitigated.

## Epic Goal

Provide a **single universal executable** (`tea.com`) that runs natively on Windows, Linux, and macOS without recompilation, using Cosmopolitan Libc to create Actually Portable Executables with Scryer Prolog, Lua, and bundled LLM models via ZIP append.

## Epic Description

### Existing System Context

- **Current relevant functionality:**
  - TEA-RELEASE-001: Multi-platform binaries (separate builds per OS)
  - TEA-RELEASE-002/003: Linux AppImages with SWI-Prolog bundling
  - TEA-RELEASE-004: LLM-bundled AppImages (Linux-only)
- **Technology stack:** Rust (primary), llama.cpp via `llama-cpp-2` crate, GitHub Actions CI/CD
- **Integration points:** Existing release workflow, `tea` CLI binary, model bundling infrastructure

### Problem Statement

Currently, TEA requires **6 separate binaries** for full platform coverage:
- Linux x86_64, Linux ARM64
- macOS x86_64, macOS ARM64
- Windows x86_64 (no ARM64)

Users must download the correct binary for their platform. Distribution channels (GitHub, websites) must host multiple files.

### Solution

Cosmopolitan Libc enables building **Actually Portable Executables (APE)** - polyglot binaries that run natively on all platforms from a single file. Combined with ZIP append for model bundling, this provides:

1. **One file** → runs on Windows, Linux, macOS (x86_64 + ARM64)
2. **No extraction** → models read in-place via virtual `/zip/` filesystem
3. **Instant startup** → no temp folder creation or extraction delay
4. **Simplified distribution** → one download link for all users

### Enhancement Details

- **What's being added:**
  1. Cosmopolitan-based build pipeline for Rust TEA
  2. **Scryer Prolog** integration (replaces SWI-Prolog for APE variant)
  3. Universal `tea.com` binary with Prolog + Lua + LLM support
  4. LLM-bundled variant `tea-llm.com` (~2GB with Phi-4-mini via ZIP append)

- **Runtime Support Matrix:**

| Runtime | APE (`tea.com`) | Existing AppImage | Notes |
|---------|-----------------|-------------------|-------|
| **Prolog** | Scryer Prolog | SWI-Prolog | Different dialects |
| **Lua** | mlua | mlua | Same |
| **LLM** | llama.cpp | llama.cpp | Same |

- **How it integrates:**
  - New GitHub Actions workflow alongside existing platform-specific builds
  - Existing builds remain unchanged (APE is additive)
  - Same CLI interface, same YAML engine

- **Success criteria:**
  - Single `tea.com` runs on Windows 10+, Ubuntu 20.04+, macOS 12+
  - LLM inference works via bundled model on all platforms
  - Startup time <2 seconds (no extraction delay)
  - File size within 10% of native Rust binary

### Compatibility Note

Scryer Prolog uses ISO Prolog syntax. Some SWI-Prolog-specific predicates may need adaptation. Existing SWI-Prolog AppImages remain available for users requiring SWI compatibility.

### Potential Unified Architecture (Future)

If Story 005.6 (WASM Scryer spike) succeeds, future work could unify Prolog across APE and WASM:

```
┌─────────────────────────────────────────────────────────────┐
│                    TEA Runtime Matrix                        │
├─────────────────┬─────────────────┬─────────────────────────┤
│   APE (.com)    │  WASM (browser) │  AppImage (Linux)       │
├─────────────────┼─────────────────┼─────────────────────────┤
│ Scryer Prolog   │ Scryer Prolog   │ SWI-Prolog (legacy)     │
│ mlua (Lua)      │ Lua via WASM    │ mlua (Lua)              │
│ llama.cpp       │ wllama          │ llama.cpp               │
├─────────────────┴─────────────────┴─────────────────────────┤
│              Same Prolog dialect across APE + WASM!          │
└─────────────────────────────────────────────────────────────┘
```

## Stories

### Story 1: TEA-RELEASE-005.1 - Scryer Prolog Integration Spike

**As a** developer,
**I want** to validate Scryer Prolog integration with TEA Rust,
**So that** we confirm feasibility before committing to full APE implementation.

**Key deliverables:**
- Add `scryer-prolog` crate as optional dependency (`--features scryer`)
- Implement `PrologBackend` trait for Scryer
- Port `examples/prolog/simple-prolog-agent.yaml` to Scryer syntax
- Document syntax differences from SWI-Prolog
- Benchmark: startup time, query performance vs SWI-Prolog

**Story file:** [TEA-RELEASE-005.1-scryer-prolog-spike.md](TEA-RELEASE-005.1-scryer-prolog-spike.md)

---

### Story 2: TEA-RELEASE-005.2 - Cosmopolitan Build Pipeline

**As a** developer,
**I want** a GitHub Actions workflow that builds TEA as an Actually Portable Executable,
**So that** we produce a universal binary for all desktop platforms.

**Key deliverables:**
- Install Cosmopolitan toolchain (`cosmocc`) in CI
- Configure Rust to target `x86_64-unknown-cosmo` (experimental)
- Build `tea.com` with Scryer Prolog + Lua + core features
- Smoke test on Linux, Windows, macOS runners
- Add to release workflow as additional artifact

**Story file:** [TEA-RELEASE-005.2-cosmopolitan-build-pipeline.md](TEA-RELEASE-005.2-cosmopolitan-build-pipeline.md)

---

### Story 3: TEA-RELEASE-005.3 - LLM Model Bundling via ZIP Append

**As a** user,
**I want** a single `tea-llm.com` file with bundled LLM model,
**So that** I can run offline AI inference on any platform without downloading models separately.

**Key deliverables:**
- Implement `/zip/` virtual filesystem access for model loading
- Create build script to append Phi-4-mini Q3_K_S (~1.9GB) via ZIP
- Modify `llm.call` action to detect and use bundled model
- Produce `tea-llm.com` artifact (~2GB)

**Story file:** [TEA-RELEASE-005.3-llm-model-bundling-zip.md](TEA-RELEASE-005.3-llm-model-bundling-zip.md)

---

### Story 4: TEA-RELEASE-005.4 - Platform Testing & Validation

**As a** release engineer,
**I want** comprehensive cross-platform testing for APE binaries,
**So that** we ensure quality across Windows, Linux, and macOS.

**Key deliverables:**
- Matrix test on: Ubuntu 20.04/22.04/24.04, Windows 10/11, macOS 12/13/14
- Test Prolog, Lua, and LLM actions on each platform
- Document any platform-specific quirks or limitations
- Add smoke tests to CI for each platform

**Story file:** [TEA-RELEASE-005.4-platform-testing-validation.md](TEA-RELEASE-005.4-platform-testing-validation.md)

---

### Story 5: TEA-RELEASE-005.5 - Documentation & Migration Guide

**As a** user migrating from SWI-Prolog AppImage,
**I want** clear documentation on APE distribution and Scryer Prolog syntax,
**So that** I can adopt the universal binary with minimal friction.

**Key deliverables:**
- Update `docs/installation.md` with APE download instructions
- Create `docs/shared/scryer-migration.md` (SWI → Scryer syntax guide)
- Add decision flowchart: when to use APE vs AppImage
- Update README with universal binary section

**Story file:** [TEA-RELEASE-005.5-documentation-migration-guide.md](TEA-RELEASE-005.5-documentation-migration-guide.md)

---

### Story 6: TEA-RELEASE-005.6 - Scryer Prolog WASM Spike

**As a** developer,
**I want** to validate Scryer Prolog compilation to WebAssembly,
**So that** browser-based TEA can use the same Prolog dialect as APE.

**Key deliverables:**
- Attempt `cargo build --target wasm32-unknown-unknown` for Scryer
- Identify blocking issues (threading, I/O, etc.)
- Create minimal WASM Prolog demo if feasible
- Document path to production WASM Prolog support

**Story file:** [TEA-RELEASE-005.6-scryer-wasm-spike.md](TEA-RELEASE-005.6-scryer-wasm-spike.md)

---

## Artifact Naming Convention

| Artifact | Size (est.) | Contents | Platforms |
|----------|-------------|----------|-----------|
| `tea-{version}.com` | ~20MB | Core TEA + Scryer Prolog + Lua | Windows, Linux, macOS |
| `tea-llm-{version}.com` | ~2GB | Core + Phi-4-mini Q3_K_S model | Windows, Linux, macOS |

**Platform compatibility:** Windows 10+, Linux (glibc 2.17+), macOS 12+

## Dependencies

| Dependency | Type | Status | Notes |
|------------|------|--------|-------|
| TEA-RELEASE-001 | Internal | Done | Multi-platform binary patterns |
| TEA-RELEASE-002 | Internal | Done | Rust AppImage build patterns |
| TEA-RELEASE-004.4 | Internal | Optional | LLM actions integration (can reuse) |
| [scryer-prolog](https://crates.io/crates/scryer-prolog) | External | - | Rust Prolog crate |
| [Cosmopolitan](https://github.com/jart/cosmopolitan) | External | - | APE toolchain |
| llama.cpp Cosmo support | External | Available | `make LLAMA_COSMO=1` |

## Compatibility Requirements

- [x] Existing APIs remain unchanged (new `scryer` feature flag)
- [x] No database schema changes
- [x] Existing AppImages unaffected (APE is additive artifact)
- [x] CLI interface identical across APE and native builds

## Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scryer Prolog incompatible with some SWI syntax | High | Medium | Migration guide + keep AppImage for SWI users |
| Cosmopolitan Rust target experimental | Medium | High | Story 2 validates early; fallback to native builds |
| Windows Defender false positive | Medium | Medium | Document workaround; consider code signing |
| macOS Gatekeeper blocking | Medium | Medium | Document `xattr -d` workaround |
| WASM Scryer spike fails | Medium | Low | Informational only; doesn't block APE delivery |

**Rollback Plan:** Remove APE artifacts from release; existing platform-specific binaries remain available.

## Definition of Done

- [ ] All 6 stories completed with acceptance criteria met
- [ ] `tea-{version}.com` runs on Windows, Linux, macOS
- [ ] `tea-llm-{version}.com` includes bundled Phi-4-mini model
- [ ] Scryer Prolog executes correctly on all platforms
- [ ] Lua scripts execute correctly on all platforms
- [ ] LLM inference works with bundled model
- [ ] Documentation updated with APE instructions
- [ ] SWI→Scryer migration guide published
- [ ] WASM Scryer feasibility documented (spike results)
- [ ] No regression in existing AppImage builds

## Out of Scope

- macOS code signing / notarization (future story)
- Windows Authenticode signing (future story)
- ARM64-specific APE optimizations (Cosmopolitan handles via fat binary)
- Full WASM Prolog implementation (spike only in this epic)
- SWI-Prolog compatibility layer for Scryer

## References

- [Cosmopolitan Libc](https://github.com/jart/cosmopolitan) - APE toolchain
- [Scryer Prolog](https://github.com/mthom/scryer-prolog) - Rust-native ISO Prolog
- [llama.cpp Cosmopolitan support](https://github.com/ggerganov/llama.cpp) - `make LLAMA_COSMO=1`
- [Actually Portable Executable](https://justine.lol/ape.html) - Justine Tunney's blog

## QA Results

**Reviewed:** 2026-01-12
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 47 |
| **Unit tests** | 14 (30%) |
| **Integration tests** | 18 (38%) |
| **E2E tests** | 15 (32%) |
| **P0 (Critical)** | 18 |
| **P1 (High)** | 16 |
| **P2 (Medium)** | 10 |
| **P3 (Low)** | 3 |

All 6 stories have test coverage. Every acceptance criterion traced to at least one test scenario.

### Risk Areas Identified

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| **RISK-001:** Scryer Prolog incompatible with SWI syntax | High | Medium | INT-003, INT-004, E2E-015 |
| **RISK-002:** Cosmopolitan Rust target experimental | Medium | High | INT-007, INT-008, E2E-002 |
| **RISK-003:** Windows Defender false positive | Medium | Medium | E2E-011 |
| **RISK-004:** macOS Gatekeeper blocking | Medium | Medium | E2E-013 |
| **RISK-005:** WASM Scryer spike fails | Medium | Low | INT-017, INT-018 (informational) |

**Highest concern:** RISK-002 (Cosmopolitan Rust experimental target) has Medium probability but High impact. Story 005.2 serves as early validation; fallback to native builds available.

### Recommended Test Scenarios

**Phase 1 (Fail-Fast P0):**
1. Scryer Prolog crate compilation and trait implementation
2. `/zip/` virtual filesystem and bundled model detection
3. Cosmopolitan build pipeline validation
4. LLM model bundling via ZIP append

**Phase 2 (Cross-Platform E2E P0):**
1. `tea.com --version` smoke tests on Linux/Windows/macOS
2. Prolog/Lua execution on all platforms
3. LLM inference with bundled model on Linux

**Phase 3 (Comprehensive P1):**
1. Platform matrix expansion (Ubuntu 22.04/24.04, Windows 11, macOS 13/14)
2. Windows LLM inference validation
3. User journey: agent porting from SWI to Scryer

**Phase 4 (Nice-to-Have P2+):**
1. Documentation validation
2. Performance benchmarks (Scryer vs SWI)
3. WASM spike validation

### Concerns and Blockers

1. **No blocking issues identified** - Epic is well-structured with clear rollback plan.

2. **Mitigation concerns:**
   - RISK-002 (experimental Cosmo target) should be validated early in Story 005.2 before dependent work begins
   - RISK-003/RISK-004 (antivirus/Gatekeeper) require documented workarounds in Story 005.5

3. **Testing infrastructure:**
   - GitHub Actions matrix must include all supported OS versions
   - Local development testing recommended for platform quirks discovery
   - ~2GB LLM-bundled binary may require extended CI timeouts

4. **Scope boundaries respected:**
   - WASM spike (005.6) is P2/P3 priority; failure does not block APE delivery
   - Code signing explicitly out of scope (future story)

**Assessment:** Test design is comprehensive with appropriate shift-left strategy. Risk coverage is adequate. Ready for implementation with fail-fast validation of experimental Cosmopolitan target.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-11 | 0.1 | Initial epic draft | Sarah (PO Agent) |
