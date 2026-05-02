# `tea validate` — Workflow Validation Command

`tea validate` performs **structural validation** of a YAML workflow without
executing any nodes, instantiating LLM clients, opening LTM connections, or
making network calls. Use it to catch YAML errors *before* a workflow burns
LLM API calls and fails halfway through.

## Synopsis

```bash
tea validate <workflow.yaml>
tea validate <workflow.yaml> --strict
tea validate <workflow.yaml> --detailed
```

## Exit Codes

| Code | Meaning |
| ---- | ------- |
| 0    | All structural checks passed |
| 1    | Validation failed (one or more errors) |

In `--strict` mode, exit code 1 is also returned when at least one warning
is emitted.

## Output

### Success

```
OK: <workflow.yaml> (N nodes, M edges) [structural checks only — run blocks not executed]
```

The bracketed qualifier is intentional: it reminds users that semantic
correctness of `run:` block contents is **not** validated.

### Failure

Each error appears as its own block on stderr, followed by a summary line:

```
<workflow.yaml>:12:4: error [EDGE_TO_UNDEFINED]: Edge 'to' references undefined node 'foobar'
    node: step2

FAIL: <workflow.yaml> (1 error(s))
```

## Checks Performed (v1)

| Check                                      | Code                              |
| ------------------------------------------ | --------------------------------- |
| YAML parse errors                          | `YAML_PARSE`                      |
| Required top-level field `nodes`           | `MISSING_FIELD`                   |
| Edge `from`/`to` reference declared nodes  | `EDGE_FROM_UNDEFINED` / `EDGE_TO_UNDEFINED` |
| Node-name uniqueness                       | `DUPLICATE_NODE`                  |
| `dynamic_parallel`: exactly one of action/steps/subgraph | `DYN_PARALLEL_MODE` |
| `dynamic_parallel`: `fan_in` target valid  | `DYN_PARALLEL_MISSING_FAN_IN` / `DYN_PARALLEL_FAN_IN_UNDEFINED` |
| `dynamic_parallel`: `items` expression set | `DYN_PARALLEL_MISSING_ITEMS`     |
| Parallel edges have a `fan_in`             | `PARALLEL_FAN_IN_UNDEFINED`       |
| `condition:` expressions parse as Jinja2   | `INVALID_CONDITION`               |
| `goto` targets reference declared nodes    | `GOTO_UNDEFINED`                  |

## Not Checked (v1)

`tea validate` is a **structural** check. The following are intentionally
out of scope for v1:

- Semantic correctness of `run:` block contents (Python code is not executed)
- Runtime template undefined errors (e.g. `{{ state.unknown }}`) — these
  surface only when a workflow runs with real data
- LLM API availability or credentials
- LTM / storage backend reachability
- Whether action `with:` parameters match the action's actual signature

## `--strict` Mode

`--strict` adds opt-in soft warnings as additional findings:

- **Unreferenced nodes** — node declared but never connected by an edge or
  `goto` (`UNREFERENCED_NODE`)
- **Dead state-key references** — a `condition:` reads a state key that no
  node appears to set and that is not declared in `state_schema`
  (`DEAD_STATE_KEY`)
- **Missing `name:`** — top-level workflow `name:` is recommended

Strict checks are best-effort static analysis with known false-positive risk.
They are intended for CI guard-rails on curated, well-known-good fixtures —
not for blanket adoption against arbitrary user workflows.

## CI Integration

Add `tea validate` as a fast pre-flight check before running expensive
workflow tests in CI:

```yaml
# .github/workflows/validate-workflows.yml
- name: Validate all workflow YAMLs
  run: |
    for f in workflows/**/*.yaml; do
      tea validate "$f" || exit 1
    done
```

For one-shot single-file validation:

```yaml
- name: Validate workflow
  run: tea validate workflow.yaml
```

## Programmatic Use

The validator is also exposed as a public Python API. Useful for embedding
in custom test suites:

```python
from the_edge_agent.yaml_validation import validate_workflow

errors = validate_workflow("workflow.yaml", strict=False)
for err in errors:
    print(err.code, err.message, err.line)
if any(e.is_error() for e in errors):
    raise SystemExit(1)
```

The validator is **pure-Python** and side-effect-free: no sockets, no
file writes, no API-key reads. A unit test (`1.6-UNIT-002`) enforces this
import allow-list.

## Security Properties

The validator module's import surface is constrained to:
`yaml`, `jinja2`, the standard library, and the `yaml_config` dataclasses.
It must not transitively import executor or backend modules. This is
mechanized by:

- **Import allow-list test** (1.6-UNIT-002): inspects `sys.modules` after a
  clean-subprocess import of `yaml_validation` and asserts no executor
  modules are loaded.
- **Sentinel-file test** (1.6-INT-006): a fixture `run:` block that would
  write a uniquely-named sentinel file is validated; the sentinel file
  must not exist afterward.

Both tests run as part of every PR; either failing is treated as a
security-critical regression.
