# TEA-DOCS-002.1: Neurosymbolic AI Capability Landing Page

## Status

Done


## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.1 |
| **Epic** | TEA-DOCS-002 |
| **Type** | Documentation |
| **Status** | Done |
| **Priority** | P0 - Must Have |
| **Effort** | 1 hour |

## User Story

**As a** developer interested in neurosymbolic AI,
**I want** a landing page explaining TEA's Prolog + LLM integration,
**So that** I can quickly understand the capability and find relevant resources.

## Acceptance Criteria

- [ ] Create `docs/capabilities/neurosymbolic.md`
- [ ] Value proposition in first 2 sentences
- [ ] YAML example showing Prolog + LLM workflow (10-15 lines)
- [ ] Feature table (knowledge graphs, constraint solving, inference chains)
- [ ] Actions table linking to `prolog.query`, `prolog.assert`, etc.
- [ ] Links to examples in `examples/prolog/`
- [ ] Links to TEA-PROLOG stories for technical depth

## Existing Resources to Link

| Resource | Path |
|----------|------|
| Prolog examples | `examples/prolog/` |
| Neurosymbolic examples | `examples/prolog/neurosymbolic/` |
| Prolog integration epic | `docs/stories/TEA-PROLOG-001-prolog-integration-epic.md` |
| Python Prolog support | `docs/stories/TEA-PY-004-prolog-scripting-support.md` |
| Rust Prolog support | `docs/stories/TEA-RUST-035-prolog-scripting-support.md` |

## Content Structure

Follow this structure (from TEA-DOCS-002 epic template):

```markdown
# Neurosymbolic AI (Prolog + LLM)

> One-sentence value proposition about symbolic reasoning + LLM hybrid

## Why This Matters
2-3 sentences on hallucination prevention, logical correctness

## Quick Example
```yaml
# 10-15 line YAML from examples/prolog/neurosymbolic/
```

## Key Features
| Feature | Description |
|---------|-------------|
| Knowledge Graphs | ... |
| Constraint Solving | ... |
| Inference Chains | ... |

## Available Actions
| Action | Description |
|--------|-------------|
| `prolog.query` | Execute Prolog query |
| `prolog.assert` | Add facts to knowledge base |
| `prolog.retract` | Remove facts |

## Examples
- [Family Reasoning](../../examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml)
- [Simple Agent](../../examples/prolog/simple-prolog-agent.yaml)

## Learn More
- [Prolog Integration Epic](TEA-PROLOG-001-prolog-integration-epic.md)
- [Python Prolog Support](TEA-PY-004-prolog-scripting-support.md)
```

## Verification Checklist

- [ ] All internal links resolve correctly
- [ ] YAML example is valid and runnable
- [ ] Preview in GitHub markdown
- [ ] Actions table matches actual registry

## Definition of Done

- [ ] Landing page created at `docs/capabilities/neurosymbolic.md`
- [ ] Follows epic template (structure above)
- [ ] All links valid (verified)
- [ ] Renders correctly (previewed)
