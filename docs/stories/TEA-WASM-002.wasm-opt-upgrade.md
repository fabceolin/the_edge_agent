# Story TEA-WASM-002: Upgrade wasm-opt to v119+

## Status: Draft

## Story

**As a** developer building the tea-wasm-llm package,
**I want** to upgrade wasm-opt to v119+ and re-enable WASM optimization,
**So that** the WASM binary size is reduced by ~22% without table growth runtime errors.

## Story Context

### Background

wasm-opt versions 108-118 have a regression ([binaryen#4711](https://github.com/WebAssembly/binaryen/issues/4711)) that incorrectly seals WebAssembly table size to `max_size == initial_size`. This blocks wasm-bindgen's runtime `table.grow()` calls, causing:

```
Error: WebAssembly.Table.grow(): failed to grow table by 4
```

### Current Workaround

`Cargo.toml` disables wasm-opt entirely:

```toml
[package.metadata.wasm-pack.profile.release]
wasm-opt = false
```

### Size Impact

| Build | Size |
|-------|------|
| With optimization (target) | ~4.4 MB |
| Without optimization (current) | ~5.4 MB (+22%) |

## Acceptance Criteria

### Functional Requirements

1. wasm-opt v119+ is installed and available in the build environment
2. `wasm-opt = false` is removed from Cargo.toml
3. `wasm-pack build --target web --release` completes successfully with optimization
4. WASM demo loads without `WebAssembly.Table.grow()` errors

### Integration Requirements

5. All existing WASM tests pass (`npm test` - 54 tests)
6. All 4 demo examples work correctly in browser
7. WASM binary size is ≤4.5 MB (restored optimization benefit)

### Quality Requirements

8. CI/CD pipeline uses correct wasm-opt version
9. Build documentation updated with version requirements

## Tasks / Subtasks

- [ ] Verify current wasm-opt version (`wasm-opt --version`)
- [ ] Research upgrade options:
  - [ ] Check if system package manager has v119+
  - [ ] Check wasm-pack bundled version
  - [ ] Manual binaryen install from releases
- [ ] Upgrade wasm-opt to v119+
- [ ] Remove `wasm-opt = false` from Cargo.toml
- [ ] Build and test locally:
  - [ ] Run `wasm-pack build --target web --release`
  - [ ] Verify WASM size is ~4.4 MB
  - [ ] Run `npm test` (54 tests)
  - [ ] Test demo in browser (all 4 examples)
- [ ] Update CI/CD if needed:
  - [ ] Check `.github/workflows/` for wasm-opt version
  - [ ] Update Docker images if they install wasm-opt
- [ ] Update documentation

## Technical Notes

### Upgrade Options

1. **System package upgrade** (if available for your OS)
2. **Manual binaryen install** from [releases](https://github.com/WebAssembly/binaryen/releases)
3. **wasm-pack bundled** - Check if newer wasm-pack bundles fixed version

### Verification Commands

```bash
# Check current version
wasm-opt --version
# Should report v119 or higher

# Build with optimization
cd rust/tea-wasm-llm
wasm-pack build --target web --release

# Check WASM size
ls -lh pkg/tea_wasm_llm_bg.wasm

# Run tests
npm test
```

### Files to Modify

- `rust/tea-wasm-llm/Cargo.toml` - Remove or modify wasm-opt config
- `.github/workflows/` - Ensure CI uses correct wasm-opt version
- `docker/Dockerfile.rust-*` - Update if wasm-opt is installed there

## Risk Assessment

| Risk | Mitigation | Rollback |
|------|------------|----------|
| wasm-opt v119+ not available | Document manual install steps | Re-add `wasm-opt = false` |
| CI environment has old version | Pin binaryen version in CI | Use existing workaround |
| New wasm-opt introduces other issues | Run full test suite | Revert Cargo.toml change |

## Definition of Done

- [ ] wasm-opt v119+ confirmed in build environment
- [ ] Cargo.toml wasm-opt workaround removed
- [ ] Build succeeds with optimization enabled
- [ ] Browser demo works without table growth errors
- [ ] All 54 Node.js tests pass
- [ ] WASM size reduced to ~4.4 MB
- [ ] CI pipeline updated if needed
- [ ] Documentation updated

## Dev Notes

### Root Cause Analysis

The WebAssembly Function Table is used for indirect function calls (trait objects, closures, `dyn Trait`). When wasm-bindgen generates JavaScript glue code, it may need to add entries to this table at runtime via `WebAssembly.Table.prototype.grow()`.

wasm-opt v108-118 has a regression where it incorrectly sets `max_size = initial_size` for the table, preventing any runtime growth. This was fixed in v119.

### References

- [Binaryen Issue #4711](https://github.com/WebAssembly/binaryen/issues/4711)
- [wasm-bindgen Table Usage](https://rustwasm.github.io/docs/wasm-bindgen/)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 1.0 | Initial story creation | Sarah (PO) |
