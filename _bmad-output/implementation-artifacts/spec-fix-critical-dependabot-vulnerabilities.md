---
title: 'Remediate critical Dependabot vulnerabilities'
type: 'chore'
created: '2026-08-09'
status: 'done'
baseline_commit: '14b64873fdebd1b7fea6b3c2ef185a89cbaa0963'
context: []
---

<frozen-after-approval reason="human-owned intent — do not modify unless human renegotiates">

## Intent

**Problem:** The default branch has four open critical Dependabot alerts: three against the directly pinned development dependency `litellm==1.82.6`, and one against `chromadb==1.1.1`. These dependencies are also distributed through optional extras and, for ChromaDB, bundled build workflows.

**Approach:** Upgrade LiteLLM to the minimum version that closes all its current alerts (`1.84.0`) and raise OpenAI to its minimum compatible version (`2.20.0`). Keep ChromaDB at `1.1.1` until upstream publishes a patched release, preserving compatibility with `crewai==1.6.1` and documenting the remaining critical alert.

## Boundaries & Constraints

**Always:** Keep repeated LiteLLM and OpenAI pins consistent across packaging and bundled-build workflows; preserve all optional extras and their compatible combinations; validate real imports plus the LiteLLM integration paths; keep tests deterministic and offline. Treat GitHub advisory ranges as authoritative: LiteLLM must be `>=1.84.0`, while the unpatched ChromaDB alert must remain explicitly documented.

**Ask First:** Stop before changing the ChromaDB or CrewAI pins, removing Chroma support, or changing public behavior. Stop before expanding this change to unrelated high, medium, or low alerts.

**Never:** Dismiss or suppress Dependabot alerts; weaken security scanning; remove an optional extra to make the dependency disappear; alter application APIs; make real provider or model network calls during verification.

## I/O & Edge-Case Matrix

| Scenario | Input / State | Expected Output / Behavior | Error Handling |
|----------|--------------|---------------------------|----------------|
| LiteLLM remediation | Install `dev`, `litellm`, or `all` extra | Resolver selects `litellm==1.84.0`; mocked call, stream, tool, retry, and cost paths pass | Stop on resolver conflict or provider regression |
| Chroma upstream block | Install `rag-chroma` together with `tools-crewai` | Resolver retains the compatible `chromadb==1.1.1`; the unresolved critical alert is reported | Revisit only when upstream publishes a patched compatible release |
| Pin consistency | Inspect every active package/build declaration | No active `litellm==1.82.6` or `openai==2.9.0` pin remains | Fail verification on any stale upgraded pin |

</frozen-after-approval>

## Code Map

- `python/setup.py` -- canonical Python runtime, development, and optional-extra pins for LiteLLM and OpenAI.
- `.github/workflows/build-python-base.yaml` -- cross-platform full Python bundles that install OpenAI explicitly.
- `.github/workflows/build-python-prolog.yaml` -- Prolog-enabled Python bundles that install OpenAI explicitly.
- `.github/workflows/python-tests.yaml` -- CI path filters that ensure bundle pin changes run the regression suite.
- `python/src/the_edge_agent/actions/llm_actions.py` -- LiteLLM integration exercised without code changes.
- `python/tests/test_llm_litellm_provider.py` -- deterministic LiteLLM provider coverage.
- `python/tests/test_dependency_security_pins.py` -- semantic pin consistency and imported-version regression coverage.

## Tasks & Acceptance

**Execution:**
- [x] `python/setup.py` -- replace every LiteLLM pin with `1.84.0` and every OpenAI pin with `2.20.0` so the upgraded extras resolve consistently.
- [x] `.github/workflows/build-python-base.yaml` -- align all bundled OpenAI installations with the LiteLLM-compatible pin.
- [x] `.github/workflows/build-python-prolog.yaml` -- align all bundled OpenAI installations with the LiteLLM-compatible pin.
- [x] `python/tests/test_dependency_security_pins.py` -- assert every active packaging/workflow declaration and imported development dependency uses the approved versions.
- [x] Focused Python dependency/integration checks -- resolve the changed extras, verify the Chroma/CrewAI combination remains installable, and run the LiteLLM suite.

**Acceptance Criteria:**
- Given the current 57 open alerts, when the updated default branch is rescanned, then all 13 LiteLLM alerts, including three critical alerts, no longer match the declared dependency version, while the unpatched ChromaDB critical alert remains open.
- Given any supported extra or bundled build containing LiteLLM or OpenAI, when dependencies are resolved, then all declarations select compatible aligned versions.
- Given the hardened packages are installed, when the focused integration tests run, then existing LiteLLM behavior passes without external service calls.
- Given the change diff, when reviewed, then it contains dependency remediation and necessary compatibility adjustments only.

## Spec Change Log

- 2026-08-09: Human approved preserving ChromaDB `1.1.1` after resolver evidence showed `chromadb==0.6.3` conflicts with `crewai==1.6.1`; scope now resolves all LiteLLM alerts and leaves the upstream-blocked ChromaDB critical alert open.
- 2026-08-09: Adversarial review found resolver coverage and durable evidence too narrow. Amended tasks and verification with a semantic pin regression test, isolated `--ignore-installed` matrices for `litellm`, `dev`, `all`, Chroma/CrewAI, and bundled NumPy constraints, plus recorded observed results. Known-bad state avoided: environment-satisfied packages masking conflicts. KEEP: LiteLLM `1.84.0`, OpenAI `2.20.0`, ChromaDB `1.1.1`, exact-pin convention, preserved extras, and the 24/24 focused-test behavior.

## Design Notes

ChromaDB has no published patched release. The investigated downgrade to `0.6.3` is outside the advisory range but conflicts with `crewai==1.6.1`, which requires `chromadb~=1.1.0`. The human chose to preserve supported extra compatibility and leave this single critical alert open pending an upstream fix.

## Verification

**Commands:**
- `python3 -m pip install --dry-run --ignore-installed "./python[litellm]"` -- expected: resolves LiteLLM `1.84.0` and OpenAI `2.20.0`.
- `python3 -m pip install --dry-run --ignore-installed "./python[dev]"` -- expected: development extra resolves with the approved pins.
- `python3 -m pip install --dry-run --ignore-installed "./python[all]"` -- expected: aggregate extra resolves with the approved pins.
- `python3 -m pip install --dry-run --ignore-installed "./python[rag-chroma,tools-crewai]"` -- expected: ChromaDB `1.1.1` remains compatible with CrewAI `1.6.1`.
- `python3 -m pip install --dry-run --ignore-installed --python-version 3.11 --only-binary=:all: "openai==2.20.0" "numpy<2" "chromadb==1.1.1"` -- expected: the Python 3.11 bundled-build dependency subset resolves.
- `cd python && pytest tests/test_dependency_security_pins.py tests/test_llm_litellm_provider.py -v` -- expected: semantic pins, imported versions, and all focused LiteLLM behaviors pass.
- `cd python && pytest` -- expected: no failures in the changed dependency/LLM surface; unrelated environment or baseline failures are recorded explicitly with counts.


## Verification Results

- Isolated resolver reports selected LiteLLM `1.84.0` and OpenAI `2.20.0` for the `litellm`, `dev`, and `all` extras.
- The isolated `rag-chroma,tools-crewai` resolver selected ChromaDB `1.1.1`, CrewAI `1.6.1`, CrewAI Tools `1.6.1`, and OpenAI `2.20.0`.
- The Python 3.11 bundled-build subset selected OpenAI `2.20.0`, ChromaDB `1.1.1`, and NumPy `1.26.4`.
- Focused semantic pin and LiteLLM behavior tests after review patches: `31 passed, 1 warning in 6.06s`.
- Clean pre-change baseline: `5013 passed, 67 skipped, 15 failed, 41 warnings in 394.16s`.
- Full post-change Python suite: `5018 passed, 67 skipped, 15 failed, 41 warnings in 304.52s`. The same 15 tests fail in the same categories: ten Neo4j/GDS tests for pre-existing missing behavior, four tests requiring a working external Ollama endpoint, and one Firestore initialization-state test. No failure is in the changed dependency or LiteLLM surface.


## Suggested Review Order

**Dependency policy**

- Start with the canonical pin alignment and preserved Chroma compatibility boundary.
  [`setup.py:61`](../../python/setup.py#L61)

- Explicit OpenAI coupling keeps the LiteLLM extra independently reproducible.
  [`setup.py:83`](../../python/setup.py#L83)

**Bundle alignment**

- Linux bundle pins mirror the canonical OpenAI version without altering Chroma.
  [`build-python-base.yaml:133`](../../.github/workflows/build-python-base.yaml#L133)

- Prolog bundle pins receive the same compatibility adjustment.
  [`build-python-prolog.yaml:395`](../../.github/workflows/build-python-prolog.yaml#L395)

**CI regression guard**

- Bundle-only changes now trigger the Python security regression suite.
  [`python-tests.yaml:10`](../../.github/workflows/python-tests.yaml#L10)

- Approved versions and exact declaration counts prevent silent pin drift.
  [`test_dependency_security_pins.py:16`](../../python/tests/test_dependency_security_pins.py#L16)

- Variant-aware parsing catches case, extras, and whitespace spellings.
  [`test_dependency_security_pins.py:25`](../../python/tests/test_dependency_security_pins.py#L25)
