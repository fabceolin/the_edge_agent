# NFR Assessment: TEA-DX-001.1

Date: 2026-05-01
Reviewer: Quinn
Mode: YOLO (non-interactive, core-four default)
Story: [`docs/stories/TEA-DX-001.1-settings-block-env-template-expansion.md`](../../stories/TEA-DX-001.1-settings-block-env-template-expansion.md)
Companion risk profile: [`TEA-DX-001.1-risk-20260501.md`](TEA-DX-001.1-risk-20260501.md)

## Scope

Core four NFRs assessed: **Security, Performance, Reliability, Maintainability**.
Out of scope: usability, compatibility, portability, functional suitability (no triggering signals in the story).

## Summary

| NFR             | Status   | Headline                                                                                       |
| --------------- | -------- | ---------------------------------------------------------------------------------------------- |
| Security        | CONCERNS | Env-driven file-write path needs explicit operator-trust note; no path validation in scope.   |
| Performance     | PASS     | One regex pass over ≤3 string values at config-load time; no hot-path effect.                 |
| Reliability     | CONCERNS | AC-4 contradicts `expand_env_vars` semantics; empty post-expansion `trace_file` is silent.    |
| Maintainability | PASS     | Mirrors three established call sites; AC-8 covers unit tests; DoD requires a doc note.        |

**Quality score:** `100 − (0 × 20) − (2 × 10) = 80`

**Deterministic gate (NFR-only):** **CONCERNS** (driven by Security + Reliability).

## Detailed Findings

### Security — CONCERNS

**What we have:**
- `expand_env_vars` is the same helper already trusted on `ltm_settings`, `secrets_settings`, `firestore_settings` (`yaml_engine.py:1015,1052,1104`). No new attack surface introduced by the helper itself.
- AC-7 keeps expansion narrow to known trace keys, avoiding accidental expansion of prompt templates or other user-content fields.
- DoD requires a `YAML_REFERENCE.md` note; risk profile recommends a one-line operator-responsibility statement covering SEC-001 / DATA-001.

**Gaps:**
- A trace path resolved from `${TEA_TRACE_FILE}` is a file-write sink. If the env is attacker-controlled, the resolved path can redirect writes (overwrite `~/.bashrc`, write into shared NFS, etc.) — see SEC-001 / DATA-001 in the risk profile.
- No path normalization, allowlist, or "must be under workdir" check is in scope (and that's the right call for a narrow story), but the operator needs to be told this in the docs.
- Trace payloads can contain LLM I/O / state — sensitive by default. Routing them through a parameterised path is a confidentiality consideration even when the env is trusted.

**Why CONCERNS not PASS:** the documentation note that translates these caveats into operator guidance is in DoD but not yet written. Once the YAML_REFERENCE.md note lands with an explicit "treat env vars feeding `trace_file` as trusted; the resolved path is the final filesystem destination" line, this NFR moves to PASS.

**Why not FAIL:** the change does not introduce a vulnerability; it surfaces a configuration capability. The threat model is "operator misuse / hostile env," which is consistent with how every other env-driven setting in the codebase is treated.

### Performance — PASS

**What we have:**
- Cost is one `re.sub` call per expanded string, done once at `_configure_from_settings` time — i.e., agent compile / runner-init, not per node execution.
- At most three keys (`trace_file`, `trace_exporter`, `trace_format`) are expanded.
- No I/O, no allocations of any consequence, no impact on the runtime hot path.

**Targets:** none stated in the story; none warranted at this granularity.

**Verdict:** trivial overhead; nothing to monitor.

### Reliability — CONCERNS

**What we have:**
- Pattern is well-trodden — three prior call sites have not regressed.
- AC-3 explicitly calls out "literal values pass through unchanged."
- Test plan covers literal, expanded, defaulted, and missing-required-var paths.

**Gaps (both already in the risk profile must-fix list):**
1. **AC-4 is wrong.** It says "missing env vars without a default raise the same error the existing `expand_env_vars` raises elsewhere." But `expand_env_vars` at `python/src/the_edge_agent/memory/base.py:528-537` does **not** raise — it returns `""` for an unset var with no default. Tests written against AC-4 as drafted will assert wrong behavior (and either fail or, worse, encode the wrong contract). Must be rewritten before code is written.
2. **Silent loss of tracing.** With AC-4 corrected, `trace_exporter=file` + unset `${TEA_TRACE_FILE}` (no default) yields `trace_file=""`, which the existing `elif trace_exporter == "file" and trace_file:` guard at `yaml_engine.py:991` quietly skips. The operator turned tracing on, the runner reports nothing, the operator notices much later. Risk profile mandates a `WARNING + skip` here; this is the highest-likelihood real-world failure mode of the feature.

**Why CONCERNS not FAIL:** these are addressable inside the same PR; the risk-profile must-fix list catches them. They're not blockers, just unmet preconditions for PASS.

### Maintainability — PASS

**What we have:**
- Implementation is one narrow line that mirrors `ltm_settings = expand_env_vars(ltm_settings)` at `yaml_engine.py:1015` and the two sibling sites at `1052` / `1104`. A reader who has seen any of those three already understands this one.
- AC-8 mandates the four obvious test cases; AC-9 forbids new dependencies; DoD forbids touching `expand_env_vars` itself. Blast radius is contained.
- DoD requires a `YAML_REFERENCE.md` `settings.trace_*` doc update.
- Risk profile recommends a single inline comment at the call site listing the three keys, so future schema additions (e.g., a `trace_compression`) surface in code review (TECH-001). This is a small but real maintainability win.
- Rollback is a single-PR revert (story §"Risk and Compatibility").

**Verdict:** this is the easy quadrant. PASS.

## Critical Issues

1. **AC-4 contradicts implementation reality** (Reliability, blocks PASS)
   - Risk: tests encode wrong contract; story author or reviewer chases a non-bug.
   - Fix: rewrite AC-4 to read "missing env vars without a default expand to the empty string, matching `expand_env_vars` semantics. Strict-required behavior is out of scope."
   - Effort: ~5 min, story edit only.

2. **Silent skip when post-expansion `trace_file` is empty** (Reliability, blocks PASS)
   - Risk: operators turn on tracing, get no trace, blame the runner.
   - Fix: at `yaml_engine.py:991`, when `trace_exporter == "file"` and `trace_file` is falsy *after expansion*, emit a `logger.warning(...)` naming the unset env var (or "trace_file empty after expansion") and skip the exporter. Add a unit test for the warning + no-exporter outcome.
   - Effort: ~15 min code + ~10 min test.

3. **YAML_REFERENCE.md `settings.trace_*` doc note** (Security + Maintainability, blocks Security PASS)
   - Risk: operators copy-paste env-var pattern without the trust caveat; ops/security review flags it later.
   - Fix: add a short subsection covering: (a) `${VAR}` and `${VAR:-default}` are honored; (b) missing-no-default → empty string → tracing skipped with warning; (c) the resolved path is written to the filesystem as-is — env source must be trusted.
   - Effort: ~15 min.

## Quick Wins

- Rewrite AC-4: ~5 min — unblocks Reliability PASS once item 2 also lands.
- Add empty-`trace_file` warning + skip + test: ~25 min — eliminates the highest-likelihood real-world failure mode.
- Add YAML_REFERENCE.md note (DoD-required anyway): ~15 min — moves Security to PASS.
- Inline comment at the narrow-expansion site listing the three keys: ~2 min — guards against TECH-001 drift.

Total to gate-PASS on NFR axis: well under an hour.

## Test Recommendations

Aligned with the risk profile's testing priorities; flagged here only where the NFR lens adds something:

- **Reliability — empty-after-expansion path** *(must add; not yet in AC-8):*
  Set `trace_exporter: file`, `trace_file: ${UNSET_VAR}` (or `${UNSET_VAR:-}` ), no env. Assert: exactly one warning logged identifying the empty trace path, `_trace_context.exporters` length unchanged, no exception raised.
- **Reliability — defaulted path:**
  `trace_file: ${UNSET_VAR:-/tmp/x.jsonl}` → `FileExporter` constructed with `/tmp/x.jsonl`. Already in risk profile P1.
- **Security — narrowness check:**
  `settings.variables.prompt_template: "Hello ${USER}"` (with `USER` set) → value remains literal `"Hello ${USER}"` after `_configure_from_settings`. Confirms AC-7 narrowness; doubles as a regression guard.
- **Maintainability — schema-drift sentinel:**
  When the inline comment lists the expanded keys, a `# noqa`-style or simple `assert set(EXPANDED_TRACE_KEYS) == {"trace_file","trace_exporter","trace_format"}` in the test would catch silent additions. Optional; skip if it feels like over-engineering.
- **Performance:** none warranted.

## Acceptance Criteria — NFR Lens

The functional ACs are unchanged from the story; below is what NFR PASS requires *in addition*:

- **NFR-AC-1 (Reliability):** AC-4 is rewritten to match `expand_env_vars` semantics (empty string on unset, no exception).
- **NFR-AC-2 (Reliability):** When `trace_exporter == "file"` and post-expansion `trace_file` is empty/falsy, the runner logs a `WARNING` (naming the offending key) and does not append a `FileExporter`. Covered by a unit test.
- **NFR-AC-3 (Security + Maintainability):** `docs/shared/YAML_REFERENCE.md` `settings.trace_*` section documents env-expansion behavior, default-fallback syntax, the empty-string-on-missing semantics, and a one-line operator-trust statement.
- **NFR-AC-4 (Maintainability):** A single inline comment at the narrow-expansion call site enumerates the expanded keys.

When NFR-AC-1..3 are satisfied, NFR gate moves to **PASS** (quality score → 100). NFR-AC-4 is nice-to-have and does not gate.

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'YAML_REFERENCE.md operator-trust note for env-driven trace paths is in DoD but not yet written; no path validation in scope (intentional). SEC-001 / DATA-001 in risk profile.'
  performance:
    status: PASS
    notes: 'One regex pass over ≤3 string values at config-load time. No hot-path impact.'
  reliability:
    status: CONCERNS
    notes: 'AC-4 contradicts expand_env_vars actual semantics (returns "" on missing, never raises) — TECH-002. Empty post-expansion trace_file silently skips FileExporter — TECH-005. Both must-fix in risk profile.'
  maintainability:
    status: PASS
    notes: 'Mirrors yaml_engine.py:1015/1052/1104. AC-8 covers literal/expanded/defaulted/missing tests. Recommend inline comment listing the three keys to guard TECH-001 drift.'
quality_score: 80
```

## Story Update Line

```
NFR assessment: docs/qa/assessments/TEA-DX-001.1-nfr-20260501.md
```

## Gate Integration Line

```
Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml under nfr_validation
```
