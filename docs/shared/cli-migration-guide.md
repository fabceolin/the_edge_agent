# CLI Migration Guide

This guide helps you migrate from the legacy `tea-agent` CLI to the new unified `tea` CLI.

## Overview

The Edge Agent CLI has been unified across Python and Rust implementations. Both now use:
- The same binary name: `tea`
- The same subcommand structure: `run`, `resume`, `validate`, `inspect`
- The same flag names and behaviors

## Quick Migration Reference

| Old Command | New Command |
|-------------|-------------|
| `tea-agent workflow.yaml` | `tea run workflow.yaml` |
| `tea-agent workflow.yaml --state '{"key": "value"}'` | `tea run workflow.yaml --input '{"key": "value"}'` |
| `tea-agent workflow.yaml --state-file state.json` | `tea run workflow.yaml --input @state.json` |
| `tea-agent workflow.yaml --resume checkpoint.pkl` | `tea resume checkpoint.pkl --workflow workflow.yaml` |
| `tea-agent --version` | `tea --version` |

## Detailed Changes

### 1. Binary Name Change

**Before:**
```bash
tea-agent workflow.yaml
```

**After:**
```bash
tea run workflow.yaml
```

The old `tea-agent` command still works but shows a deprecation warning. It will be removed in v1.0.

### 2. Subcommand Structure

The CLI now uses explicit subcommands:

| Subcommand | Purpose |
|------------|---------|
| `tea run` | Execute a workflow |
| `tea resume` | Resume from checkpoint |
| `tea validate` | Validate without execution |
| `tea inspect` | Inspect workflow structure |

### 3. State Input Flag

**Before:**
```bash
tea-agent workflow.yaml --state '{"key": "value"}'
tea-agent workflow.yaml --state-file state.json
```

**After:**
```bash
tea run workflow.yaml --input '{"key": "value"}'
tea run workflow.yaml --input @state.json
```

Note the `@` prefix for file paths - this is consistent with the Rust CLI and common CLI conventions.

### 4. Resume Command

**Before:**
```bash
tea-agent workflow.yaml --resume ./checkpoints/node_123.pkl
```

**After:**
```bash
tea resume ./checkpoints/node_123.pkl --workflow workflow.yaml
```

The checkpoint is now a positional argument, and the workflow file is specified with `--workflow`.

### 5. New Features

The unified CLI adds several new features:

#### Secrets Support
```bash
tea run workflow.yaml --secrets '{"api_key": "sk-123"}'
tea run workflow.yaml --secrets @secrets.json
tea run workflow.yaml --secrets-env TEA_SECRET_
```

#### NDJSON Streaming
```bash
tea run workflow.yaml --stream
# Output: {"type":"start",...}{"type":"node_complete",...}{"type":"complete",...}
```

#### CLI Interrupt Control
```bash
tea run workflow.yaml --interrupt-before node1,node2
tea run workflow.yaml --interrupt-after classify
```

#### Verbosity Control
```bash
tea run workflow.yaml -v      # info level
tea run workflow.yaml -vv     # debug level
tea run workflow.yaml -vvv    # trace level
tea run workflow.yaml -q      # quiet (errors only)
```

#### Workflow Validation
```bash
tea validate workflow.yaml
tea validate workflow.yaml --detailed
```

#### Workflow Inspection
```bash
tea inspect workflow.yaml              # text format
tea inspect workflow.yaml --format json
tea inspect workflow.yaml --format dot  # Graphviz
```

#### Implementation Identifier
```bash
tea --impl          # Output: python or rust
tea --version --impl  # Output: tea 0.1.0 (python)
```

## Script Migration Examples

### Bash Script

**Before:**
```bash
#!/bin/bash
tea-agent agent.yaml --state '{"input": "hello"}' > output.json
```

**After:**
```bash
#!/bin/bash
tea run agent.yaml --input '{"input": "hello"}' -q > output.json
```

### CI/CD Pipeline

**Before:**
```yaml
- run: tea-agent agent.yaml --state-file input.json --auto-continue
```

**After:**
```yaml
- run: tea run agent.yaml --input @input.json --auto-continue
```

### Python Subprocess

**Before:**
```python
import subprocess
result = subprocess.run(["tea-agent", "workflow.yaml", "--state", '{"key": "value"}'])
```

**After:**
```python
import subprocess
result = subprocess.run(["tea", "run", "workflow.yaml", "--input", '{"key": "value"}'])
```

## Backwards Compatibility

The following still work with deprecation warnings:

1. **`tea-agent` binary** - Still available, shows warning
2. **Direct file argument** - `tea workflow.yaml` redirects to `tea run workflow.yaml`
3. **`--state` flag** - Aliased to `--input`
4. **`--state-file` flag** - Aliased to `--input @file`

These compatibility features will be removed in v1.0. Update your scripts before then.

## Feature Parity Table

| Feature | Python | Rust |
|---------|--------|------|
| `tea run` | ✅ | ✅ |
| `tea resume` | ✅ | ✅ |
| `tea validate` | ✅ | ✅ |
| `tea inspect` | ✅ | ✅ |
| `--input` / `--input @file` | ✅ | ✅ |
| `--secrets` / `--secrets @file` | ✅ | ✅ |
| `--secrets-env` | ✅ | ✅ |
| `--stream` (NDJSON) | ✅ | ✅ |
| `--interrupt-before/after` | ✅ | ✅ |
| `--auto-continue` | ✅ | ✅ |
| `-v/-vv/-vvv/-q` | ✅ | ✅ |
| `--impl` | ✅ | ✅ |
| `--actions-module` | ✅ | ❌ (Python only) |
| `--actions-file` | ✅ | ❌ (Python only) |

Note: Rust will show "not implemented (Python only)" if action flags are used.

## Getting Help

```bash
tea --help
tea run --help
tea resume --help
tea validate --help
tea inspect --help
```
