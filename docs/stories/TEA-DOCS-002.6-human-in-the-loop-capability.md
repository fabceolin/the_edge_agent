# TEA-DOCS-002.6: Human-in-the-Loop Capability Landing Page

## Status

Ready for Development


## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.6 |
| **Epic** | TEA-DOCS-002 |
| **Type** | Documentation |
| **Status** | Done |
| **Priority** | P1 - Should Have |
| **Effort** | 45 minutes |

## User Story

**As a** developer building agents that require human oversight,
**I want** a landing page explaining TEA's interrupt and checkpoint capabilities,
**So that** I can understand human-in-the-loop patterns and find resources.

## Acceptance Criteria

- [ ] Create `docs/capabilities/human-in-the-loop.md`
- [ ] Value proposition: interrupts, checkpoints, resume, approval workflows
- [ ] YAML example showing interrupt workflow (10-15 lines)
- [ ] Feature table (interrupt_before, interrupt_after, checkpoint, resume)
- [ ] CLI usage for interactive mode
- [ ] Links to checkpoint guide and examples

## Existing Resources to Link

| Resource | Path |
|----------|------|
| Checkpoint guide | `docs/shared/architecture/checkpoint-guide.md` |
| Interactive support (CLI) | `docs/stories/TEA-CLI-003.interactive-interrupt-support.md` |
| Interactive HITL mode | `docs/stories/TEA-CLI-005-interactive-hitl-mode.md` |
| Rust interactive | `docs/stories/TEA-CLI-005a-interactive-rust-core.md` |

## CLI Commands to Document

From TEA-CLI-005, include these commands:
- `tea run workflow.yaml --interactive` (basic interactive mode)
- `--question-key`, `--response-key`, `--complete-key` (configurable Q&A)
- `--auto-continue` (for CI/CD usage)

## YAML Example Source

Use checkpoint-guide.md (lines 224-232) as reference:
```yaml
config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [review_node]
  interrupt_after: [validation_node]
```

## Verification Checklist

- [ ] All internal links resolve correctly
- [ ] YAML example is valid
- [ ] CLI commands are accurate
- [ ] Preview in GitHub markdown

## Definition of Done

- [ ] Landing page created at `docs/capabilities/human-in-the-loop.md`
- [ ] Follows epic template (TEA-DOCS-002 lines 57-99)
- [ ] All links valid (verified)
- [ ] Renders correctly (previewed)
