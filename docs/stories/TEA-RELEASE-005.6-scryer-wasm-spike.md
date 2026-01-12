# TEA-RELEASE-005.6: Scryer Prolog WASM Spike

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.6 |
| **Type** | Spike |
| **Priority** | Low |
| **Estimated Effort** | 3 points |
| **Status** | Draft |
| **Parent Epic** | TEA-RELEASE-005 |
| **Depends On** | TEA-RELEASE-005.1 (Scryer integration proven) |
| **Files to Create** | `docs/research/scryer-wasm-feasibility.md` |
| **Files to Modify** | None (research only) |

## Story

**As a** developer,
**I want** to validate Scryer Prolog compilation to WebAssembly,
**So that** browser-based TEA can use the same Prolog dialect as APE.

## Background

### Why WASM Prolog Matters

If Scryer Prolog compiles to WASM, we achieve **dialect consistency**:

```
┌─────────────────────────────────────────────────────────────┐
│                    Unified Prolog Dialect                    │
├─────────────────┬─────────────────┬─────────────────────────┤
│   APE (.com)    │  WASM (browser) │  AppImage (Linux)       │
├─────────────────┼─────────────────┼─────────────────────────┤
│ Scryer Prolog   │ Scryer Prolog   │ SWI-Prolog (legacy)     │
└─────────────────┴─────────────────┴─────────────────────────┘
```

Benefits:
- Write Prolog once, runs in CLI and browser
- Shared test suites
- Single migration path (SWI → Scryer)
- Simplified documentation

### Current WASM Situation

TEA-RELEASE-004 planned WASM with:
- LLM via wllama (works)
- No Prolog (SWI-Prolog WASM too heavy)

This spike explores whether Scryer can fill that gap.

## Acceptance Criteria

- [ ] **AC-1**: Attempt `cargo build --target wasm32-unknown-unknown` for Scryer crate
- [ ] **AC-2**: Document all compilation errors/blockers
- [ ] **AC-3**: If compilable, create minimal browser demo
- [ ] **AC-4**: Measure WASM binary size
- [ ] **AC-5**: Test basic query execution in browser (if compilable)
- [ ] **AC-6**: Document threading limitations (WASM is single-threaded)
- [ ] **AC-7**: Document I/O limitations (no filesystem in browser)
- [ ] **AC-8**: Provide effort estimate for production WASM Prolog support
- [ ] **AC-9**: Feasibility assessment written to `docs/research/scryer-wasm-feasibility.md`

## Technical Exploration

### Step 1: Compilation Attempt

```bash
# Clone Scryer Prolog
git clone https://github.com/mthom/scryer-prolog
cd scryer-prolog

# Attempt WASM build
cargo build --target wasm32-unknown-unknown --lib

# Document any errors
```

### Step 2: Identify Blockers

Common WASM compilation blockers:

| Issue | Description | Potential Fix |
|-------|-------------|---------------|
| `std::fs` | File system access | Mock or remove |
| `std::thread` | Threading | Remove/stub |
| `std::net` | Networking | Remove/stub |
| Native deps | C libraries | Find pure Rust alternatives |

### Step 3: Minimal Demo (if compilable)

```javascript
// demo.js - Browser Prolog demo
import init, { ScrbyerMachine } from './scryer_wasm.js';

async function main() {
    await init();

    const machine = new ScryerMachine();

    // Load a simple program
    machine.consult(`
        factorial(0, 1) :- !.
        factorial(N, F) :-
            N > 0,
            N1 is N - 1,
            factorial(N1, F1),
            F is N * F1.
    `);

    // Query
    const result = machine.query("factorial(5, X).");
    console.log("factorial(5) =", result);
}

main();
```

### Step 4: Integration Assessment

If Scryer compiles to WASM:

```
TEA WASM Architecture (Future)
┌────────────────────────────────────────┐
│  Browser                               │
├────────────────────────────────────────┤
│  tea-wasm.js                           │
│  ├── YAML Engine (Rust→WASM)           │
│  ├── Scryer Prolog (Rust→WASM)         │
│  ├── Lua (C→WASM via Emscripten)       │
│  └── wllama (llama.cpp→WASM)           │
├────────────────────────────────────────┤
│  IndexedDB (model cache)               │
└────────────────────────────────────────┘
```

## Expected Outcomes

### Optimistic Scenario

Scryer compiles to WASM with minor patches:
- WASM size: ~5-10MB
- Basic queries work
- Effort: 1-2 stories to productionize

### Realistic Scenario

Scryer has some blockers:
- Threading code needs stubbing
- File I/O needs virtual filesystem
- Effort: 3-5 stories, may need upstream patches

### Pessimistic Scenario

Scryer has fundamental blockers:
- Deep threading dependencies
- Unportable code patterns
- Effort: Not feasible without major rewrite

## Tasks / Subtasks

- [ ] **Task 1: Environment setup**
  - [ ] Install wasm-pack and wasm32 target
  - [ ] Clone Scryer Prolog repository
  - [ ] Review existing WASM discussions/issues

- [ ] **Task 2: Compilation attempt** (AC: 1, 2)
  - [ ] Run `cargo build --target wasm32-unknown-unknown`
  - [ ] Document all errors
  - [ ] Categorize blockers (threading, I/O, deps)

- [ ] **Task 3: Blocker analysis** (AC: 6, 7)
  - [ ] Assess each blocker's severity
  - [ ] Research potential fixes
  - [ ] Check for existing patches/forks

- [ ] **Task 4: Demo (if possible)** (AC: 3, 4, 5)
  - [ ] Create minimal wasm-pack project
  - [ ] Build WASM binary
  - [ ] Create HTML test page
  - [ ] Test basic queries

- [ ] **Task 5: Write feasibility report** (AC: 8, 9)
  - [ ] Create `docs/research/scryer-wasm-feasibility.md`
  - [ ] Include compilation results
  - [ ] Provide effort estimate
  - [ ] Recommend next steps

## Dev Notes

### Resources

- Scryer Prolog: https://github.com/mthom/scryer-prolog
- wasm-pack: https://rustwasm.github.io/wasm-pack/
- wasm-bindgen: https://rustwasm.github.io/wasm-bindgen/

### Known Scryer WASM Discussions

Check these before starting:
- Scryer GitHub issues for "wasm" or "webassembly"
- Rust Prolog community discussions
- Any existing forks with WASM support

### Spike Report Template

```markdown
# Scryer Prolog WASM Feasibility Report

## Summary
[One paragraph summary]

## Compilation Results
- Target: wasm32-unknown-unknown
- Result: [Success/Partial/Failed]
- Errors: [List]

## Blockers Identified
| Blocker | Severity | Fix Effort |
|---------|----------|------------|
| ... | ... | ... |

## Demo Results (if applicable)
- WASM size: X MB
- Query performance: X ms
- Browser compatibility: [List]

## Effort Estimate
- Minimal viable: X stories
- Production ready: X stories

## Recommendation
[Proceed/Defer/Abandon] with rationale
```

## Spike Success Criteria

This spike is successful if:

1. **Clear feasibility assessment**: We know if Scryer→WASM is viable
2. **Documented blockers**: All issues catalogued with severity
3. **Effort estimate**: Realistic story count for production support
4. **Recommendation**: Clear path forward

## Spike Failure Criteria

This spike fails if:

1. Unable to determine feasibility (inconclusive)
2. No actionable next steps identified

**Note:** "Scryer doesn't compile to WASM" is a valid, useful outcome - it informs future architecture decisions.

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

