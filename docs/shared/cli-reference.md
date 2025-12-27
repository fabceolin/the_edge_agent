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
