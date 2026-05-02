# CLI Reference

Both Python and Rust implementations share the same CLI interface with identical subcommands and flags.

## Basic Commands

```bash
# Run a workflow
tea run workflow.yaml --input '{"query": "hello"}'
tea run workflow.yaml --input @state.json

# Run with secrets
tea run workflow.yaml --secrets '{"api_key": "sk-123"}'
tea run workflow.yaml --secrets @secrets.json
tea run workflow.yaml --secrets-env TEA_SECRET_

# Streaming NDJSON output (for pipelines)
tea run workflow.yaml --stream
```

## Interrupt Control

```bash
# CLI interrupt control
tea run workflow.yaml --interrupt-before node1,node2
tea run workflow.yaml --interrupt-after classify
tea run workflow.yaml --auto-continue
```

See [Human-in-the-Loop Guide](../guides/human-in-the-loop.md) for detailed interrupt workflow documentation.

## Custom Actions (Python Only)

```bash
# Load custom actions
tea run workflow.yaml --actions-module my_package.actions
tea run workflow.yaml --actions-file ./custom_actions.py
```

See [Custom Actions Guide](../python/custom-actions.md) for creating custom action modules.

## Resume from Checkpoint

```bash
# Resume from checkpoint
tea resume checkpoint.pkl --workflow workflow.yaml
tea resume checkpoint.pkl --workflow workflow.yaml --input '{"update": "value"}'
```

## Validation and Inspection

```bash
# Validate workflow (without execution)
tea validate workflow.yaml
tea validate workflow.yaml --detailed

# Inspect workflow structure
tea inspect workflow.yaml
tea inspect workflow.yaml --format json
tea inspect workflow.yaml --format dot    # Graphviz output
```

## Verbosity Control

```bash
tea run workflow.yaml -v      # info
tea run workflow.yaml -vv     # debug
tea run workflow.yaml -vvv    # trace
tea run workflow.yaml -q      # quiet (errors only)
```

## Progress Heartbeat (TEA-DX-001.7)

```bash
# Emit one progress line on stderr per node completion (default off)
tea run workflow.yaml --heartbeat
tea run workflow.yaml --quiet --heartbeat        # silent stdout, heartbeat-only stderr
```

The `--heartbeat` flag is the recommended middle ground between `--quiet`
(no output) and `--stream` (full NDJSON). It prints exactly one line to
**stderr** per node completion:

- Success: `[<node_name> done in <duration>]`
- Failure: `[<node_name> FAILED in <duration>]`
- Parallel branches: `[parallel:<branch_name> done in <duration>]`

Notes:

- Default off; combine with any other run flag.
- Output goes to **stderr** so stdout pipelines and `--output` redirects
  remain byte-identical to a non-heartbeat run.
- Durations are wall-clock deltas between consecutive engine events
  (`50ms` / `1.2s` / `2m 18s` / `1h 1m 40s`).
- Pair with `--quiet` for long workflows where you want progress signal
  without the verbosity of `--stream`. `--heartbeat` together with
  `--stream` works but is redundant — prefer one or the other.

## Intermediate State Dumps for Debug (TEA-DX-001.3)

```bash
# Write per-node JSON state snapshots to <dir> as the workflow runs (default off)
tea run workflow.yaml --debug-state ./dumps/
```

The `--debug-state <dir>` flag captures the state after every node so you
can inspect the failure point post-mortem when a multi-node workflow
raises before reaching `__end__` and the `--output` file would otherwise
be empty.

File naming:

- Success: `<NN>-after-<node_name>.json` (e.g. `01-after-classify.json`)
- Failure: `<NN>-FAILED-<node_name>.json` — payload includes a
  `traceback` field
- `<NN>` is a zero-padded sequence counter for the run

Notes:

- The dump dir is created if missing; pre-existing files in the dir are
  preserved (you can `rm -rf` between runs).
- A `WARNING:` line is emitted to stderr at startup whenever
  `--debug-state` is set, surviving `--quiet`. The flag is intended for
  development; do not enable it in production / CI.
- Node-name path components are sanitized: characters outside
  `[A-Za-z0-9_-]` are replaced with `_` so a YAML node name like
  `../../etc/passwd` cannot escape the dump directory.
- Compatible with `--quiet`, `--stream`, `--show-graph`, and
  `--checkpoint` — combine freely.
- For parallel / `dynamic_parallel` blocks, only the parent fan-out and
  fan-in nodes are dumped; per-branch events (`parallel_state`,
  `parallel_error`, `branch_complete`) do not produce extra files.
- **Redaction scope (Option B):** `--debug-state` does **not** redact
  state fields. Whatever lives in state is written to disk as-is, and
  exception messages appear verbatim in the FAILED dump's `traceback`
  field. Keep secrets in the secrets backend
  (`tea run --secrets ...` / `secrets.get`), not in state. The startup
  `WARNING` exists precisely to make this scope explicit at every
  invocation.
- Dump-write failures (e.g. read-only / full-disk dump dir) are
  swallowed and logged at DEBUG level — they never replace the user's
  actual node exception.
- No size cap in v1; on very long workflows with large states this can
  fill disk fast — use a dedicated directory and clean up afterwards.

### Troubleshooting: empty `--output` after a mid-run failure

If a node raises before reaching `__end__`, the `--output` file is
empty/missing because the engine never reaches the terminal `final`
event. Re-run with `--debug-state ./dumps/` to capture per-node JSON
snapshots; the last `<NN>-after-...` file is the state right before the
failure, and `<NN>-FAILED-...` contains both the pre-node state and the
exception traceback.

## Trace File Override (TEA-DX-001.2)

```bash
# Override settings.trace_file for a single run
tea run workflow.yaml --trace-file ./traces/run-1.jsonl
tea run workflow.yaml --trace-file '${TRACE_DIR}/run.jsonl'   # ${ENV_VAR} expansion
```

The `--trace-file` flag wires a JSON-lines `FileExporter` for the run
without requiring YAML edits or temp YAML rendering. Useful for external
runners that re-execute the same YAML many times with per-run trace paths.

Precedence and implicit behavior:

- **CLI `--trace-file` overrides `settings.trace_file`.** When set, the
  YAML's trace path is ignored.
- **Implicitly enables `auto_trace=true`** even if YAML has
  `settings.auto_trace: false` — the user's intent is clear when they ask
  for a trace path on the command line.
- **Implicitly switches `trace_exporter` to `"file"`** if it is unset or
  `"console"`. If the YAML already specifies `trace_exporter: file`, only
  the path is overridden (exporter type unchanged).
- The path supports `${ENV_VAR}` and `${ENV_VAR:-default}` expansion via
  the same helper used by `settings.trace_file` (parity with
  TEA-DX-001.1).
- A bad path (e.g. parent-not-a-directory) exits with a
  `typer.BadParameter` error referencing `--trace-file`, not a raw Python
  traceback.

## Exit Condition Control (TEA-CLI-008)

```bash
# Exit with code 1 if final state matches condition
tea run workflow.yaml --fail-on-state 'final_status=failed'
tea run workflow.yaml --fail-on-state 'final_status=failed' --fail-on-state 'error=true'

# Useful for CI/CD pipelines to detect workflow failures
tea run workflow.yaml --fail-on-state 'validation_status=incomplete' && echo "Success" || echo "Failed"
```

The `--fail-on-state` option checks the final workflow state against key=value conditions:
- Specify multiple conditions (any match triggers exit 1)
- Comparison is string-based (numeric values are compared as strings)
- Works with `--stream`, `--show-graph`, and default output modes

## DOT File Execution (Python Only, TEA-GAME-001)

```bash
# Execute DOT files with tmux (phases respect dependencies)
tea run --from-dot workflow.dot
tea run --from-dot workflow.dot --dot-session my-session
tea run --from-dot workflow.dot --dot-max-parallel 4
tea run --from-dot workflow.dot --dot-dry-run

# Workflow mode: run a YAML for each DOT node
tea run --from-dot stories.dot --dot-workflow bmad-validation.yaml
tea run --from-dot stories.dot --dot-workflow dev.yaml --dot-input '{"mode": "quick"}'

# Stop-on-failure control (TEA-CLI-008)
tea run --from-dot stories.dot --dot-stop-on-failure      # Default: stop after phase if any node fails
tea run --from-dot stories.dot --no-dot-stop-on-failure   # Continue all phases regardless of failures

# Resume from specific wave/step (TEA-CLI-009)
tea run --from-dot workflow.dot --dot-start-wave 3        # Skip waves 1-2, start from wave 3
tea run --from-dot workflow.dot --dot-start-step 2        # Skip step 1 in wave 1
tea run --from-dot workflow.dot --dot-start-wave 2 --dot-start-step 3  # Skip wave 1, skip steps 1-2 in wave 2
tea run --from-dot workflow.dot --dot-start-from "Build"  # Start from node "Build" (finds wave/step)
tea run --from-dot workflow.dot --dot-start-wave 2 --dot-dry-run  # Preview skipped/executed nodes
```

Exit code behavior for `--from-dot`:
- Exit code 0: All nodes completed successfully
- Exit code 1: One or more nodes failed (actual exit codes captured)

Wave/step selection options (TEA-CLI-009):
- `--dot-start-wave N`: Skip waves 1 to N-1, start from wave N (1-based)
- `--dot-start-step M`: Skip steps 1 to M-1 in the starting wave (1-based)
- `--dot-start-from <label>`: Start from node with this label (mutually exclusive with wave/step options)

## Version and Implementation Info

```bash
tea --version                 # tea 0.1.0
tea --impl                    # python or rust
tea --version --impl          # tea 0.1.0 (python)

# Show help
tea --help
tea run --help
```

## Example Output

```
================================================================================
Running agent from: examples/yaml_agent_example.yaml
================================================================================

Initial state: {
  "query": "artificial intelligence"
}

[check] search
[check] validate_results
[check] summarize
[check] format_output
[check] save_report

================================================================================
[check] Completed
================================================================================
Final state: {...}
```

## Actions Loading Priority

When multiple action sources are specified, they are loaded in this order (later sources override earlier):

1. Built-in actions (lowest priority)
2. CLI `--actions-module` flags (in order specified)
3. CLI `--actions-file` flags (in order specified)
4. YAML `imports:` section (highest priority - overrides CLI actions)

## Security Warning

The `--actions-module` and `--actions-file` flags execute Python code from the specified modules. Only load actions from trusted sources. For production use, prefer installed packages over local files.

## See Also

- [Installation Guide](../installation.md)
- [YAML Reference](YAML_REFERENCE.md)
- [Human-in-the-Loop Guide](../guides/human-in-the-loop.md)
- [Custom Actions Guide](../python/custom-actions.md)
