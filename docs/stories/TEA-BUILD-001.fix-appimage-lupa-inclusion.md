# STORY: Fix AppImage Python Build - Include lupa/LuaJIT

| Field | Value |
|-------|-------|
| **Story ID** | TEA-BUILD-001 |
| **Title** | Fix AppImage Python Build - Include lupa/LuaJIT Libraries |
| **Status** | Ready |
| **Priority** | High |
| **Estimate** | 2-4 hours |

---

## Story

**As a** TEA user running YAML agents with Lua code,
**I want** the tea-python AppImage to include the lupa/LuaJIT libraries,
**so that** Lua scripting works in the packaged binary without installation errors.

---

## Story Context

### Existing System Integration

- **Integrates with:** PyInstaller build system (`python/tea-python-linux-x86_64-full.spec`)
- **Technology:** PyInstaller, lupa (LuaJIT bindings), AppImage
- **Follows pattern:** Other packages use `collect_all()` (numpy, pandas, the_edge_agent)
- **Touch points:**
  - `python/tea-python-linux-x86_64-full.spec`
  - `python/src/the_edge_agent/lua_runtime.py`

### Problem Description

The current tea-python AppImage build includes `lupa` in `hiddenimports` but does NOT use `collect_all('lupa')` to collect the native compiled libraries (LuaJIT `.so` files).

**Error observed:**
```
FileNotFoundError: [Errno 2] No such file or directory: '/tmp/_MEI.../lupa'
```

The lupa package contains:
- Python wrapper code
- Compiled LuaJIT shared libraries (`.so` on Linux, `.dylib` on macOS)
- These native libraries are NOT collected by `hiddenimports` alone

---

## Acceptance Criteria

### Functional Requirements

1. AppImage includes all lupa package files including native LuaJIT libraries
2. Lua code execution works in YAML agents when using the AppImage
3. `language: lua` nodes execute successfully without import errors

### Integration Requirements

4. Existing build process continues to work for other variants
5. AppImage size increase is reasonable (< 50MB additional)
6. No regression in existing functionality

### Quality Requirements

7. Test Lua execution in built AppImage before release
8. Document any build changes in release notes

---

## Tasks / Subtasks

- [ ] **Task 1: Update PyInstaller spec to collect lupa** (AC: 1)
  - [ ] Add `collect_all('lupa')` to spec file
  - [ ] Verify lupa data files and binaries are included

- [ ] **Task 2: Test AppImage build locally** (AC: 2, 3)
  - [ ] Build AppImage with updated spec
  - [ ] Run test: `./tea-python run agents/test-lua.yaml --input '{}'`
  - [ ] Verify Lua node execution succeeds

- [ ] **Task 3: Verify no regression** (AC: 4, 6)
  - [ ] Run existing test suite
  - [ ] Compare AppImage sizes before/after
  - [ ] Test non-Lua agents still work

- [ ] **Task 4: Update documentation** (AC: 8)
  - [ ] Add entry to CHANGELOG or release notes about lupa inclusion fix
  - [ ] Check if other spec variants (macOS, minimal) need similar fix

---

## Dev Notes

### Technical Implementation

**File to modify:** `python/tea-python-linux-x86_64-full.spec`

**Change:**
```python
# Add after existing collect_all calls:
tmp_ret = collect_all('lupa')
datas += tmp_ret[0]; binaries += tmp_ret[1]; hiddenimports += tmp_ret[2]
```

### Key Files

| File | Purpose |
|------|---------|
| `python/tea-python-linux-x86_64-full.spec` | PyInstaller spec - **modify this** |
| `python/src/the_edge_agent/lua_runtime.py` | Lua runtime using lupa |
| `python/tests/test_lua_runtime.py` | Tests for Lua runtime |

### Testing

**Test file location:** `python/tests/test_lua_runtime.py`

**Manual test after build:**
```bash
# Build the AppImage
cd python && pyinstaller tea-python-linux-x86_64-full.spec

# Test Lua execution
./dist/tea-python-linux-x86_64-full run test_workflow.yaml --input '{"x": 1}'
```

**Test workflow (create if needed):**
```yaml
name: test-lua
nodes:
  - name: lua_test
    language: lua
    run: |
      return { result = state.x * 2 }
```

---

## Definition of Done

- [ ] `collect_all('lupa')` added to PyInstaller spec
- [ ] AppImage builds successfully with lupa included
- [ ] Lua execution verified in built AppImage
- [ ] No regression in existing tests
- [ ] PR reviewed and merged

---

## Risk and Compatibility Check

**Primary Risk:** AppImage size may increase significantly due to LuaJIT libraries

**Mitigation:**
- LuaJIT is typically ~2-5MB, acceptable for full variant
- If size is concern, consider lupa as optional in minimal variants

**Rollback:** Revert spec file changes

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 1.0 | Story created | Sarah (PO) |

---

## Dev Agent Record

*(To be filled during implementation)*

### Agent Model Used
*(Record during implementation)*

### Debug Log References
*(Add during implementation)*

### Completion Notes List
*(Add during implementation)*

### File List
*(Add during implementation)*

---

## QA Results

*(To be filled by QA Agent)*
