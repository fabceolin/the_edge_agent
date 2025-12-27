# Human-in-the-Loop Workflows

The Edge Agent supports human-in-the-loop workflows via interactive interrupts. When a YAML agent defines `interrupt_before` or `interrupt_after`, execution pauses at those points, allowing you to review state and make decisions before continuing.

## Basic Usage

```bash
# Run agent with interrupts (interactive mode)
tea run examples/customer_support.yaml --input '{"message": "My bill is wrong"}'

# Resume from a saved checkpoint
tea resume ./checkpoints/classify_intent_1734567890.pkl --workflow examples/customer_support.yaml

# Auto-continue mode (skip interactive prompts for CI/CD)
tea run examples/customer_support.yaml --auto-continue
```

## Interactive Prompt Example

When execution reaches an interrupt point:

```
[check] classify_intent

[pause]  Interrupt at: classify_intent
   State: {
     "customer_message": "My bill is wrong",
     "intent": "billing",
     "confidence": 0.95
   }

Checkpoint saved: ./checkpoints/classify_intent_1734567890123.pkl

Review the state above. Options:
  [c] Continue with current state
  [u] Update state before continuing
  [a] Abort execution

Choice: u

Enter state updates as JSON (or press Enter to skip):
{"escalate": true, "priority": "high"}

State updated. Resuming execution...

[check] handle_billing
[check] escalate_to_human

================================================================================
[check] Completed
================================================================================
```

## Configuring Interrupts in YAML

Define interrupts in your YAML agent configuration:

```yaml
name: customer_support_agent

nodes:
  - name: classify_intent
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "Classify this customer message: {{ state.message }}"

  - name: handle_billing
    run: |
      return {"handled": True, "response": "Billing issue processed"}

edges:
  - from: __start__
    to: classify_intent
  - from: classify_intent
    to: handle_billing
  - from: handle_billing
    to: __end__

config:
  checkpoint_dir: ./checkpoints
  interrupt_after: [classify_intent]  # Pause after intent classification
  raise_exceptions: true
```

## Resume with State Updates

You can resume from a checkpoint and merge in new state:

```bash
# Resume with additional state via --input flag
tea resume ./checkpoints/classify_intent_123.pkl \
  --workflow agent.yaml \
  --input '{"approved": true, "notes": "Verified with supervisor"}'
```

### State Merge Precedence

When resuming, state is merged in this order (highest to lowest priority):

1. User input from interactive prompt
2. `--input` flag value
3. Checkpoint state
4. YAML initial state defaults

## Non-Interactive Mode (CI/CD)

For automated environments where interactive prompts would block execution:

```bash
# Auto-continue mode: execution continues at interrupts without pausing
tea run agent.yaml --auto-continue

# This also works in Docker, systemd services, and CI pipelines
# where stdin is not a TTY
```

The CLI automatically detects non-TTY environments (Docker, CI/CD) and auto-continues to prevent hanging.

## CLI Interrupt Flags

You can override YAML interrupt configuration via CLI flags:

```bash
# Interrupt before specific nodes
tea run workflow.yaml --interrupt-before node1,node2

# Interrupt after specific nodes
tea run workflow.yaml --interrupt-after classify,validate

# Combine both
tea run workflow.yaml --interrupt-before review --interrupt-after approve
```

## Use Cases

### 1. Approval Workflows

Pause before executing critical actions that require human approval:

```yaml
config:
  interrupt_before: [execute_payment, delete_account]
```

### 2. Review and Correction

Pause after LLM processing to review and correct outputs:

```yaml
config:
  interrupt_after: [generate_response, extract_entities]
```

### 3. Debugging

Step through workflow execution to understand agent behavior:

```bash
tea run agent.yaml --interrupt-after all_nodes
```

## Security Warning

**Checkpoint files use Python pickle format and should only be loaded from trusted sources.** Do not load checkpoints from untrusted origins as they can execute arbitrary code during unpickling.

## See Also

- [CLI Reference](../shared/cli-reference.md)
- [Checkpoint Guide](../shared/architecture/checkpoint-guide.md)
- [YAML Reference](../shared/YAML_REFERENCE.md)
