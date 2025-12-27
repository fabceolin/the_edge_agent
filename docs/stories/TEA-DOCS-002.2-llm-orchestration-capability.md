# TEA-DOCS-002.2: LLM Orchestration Capability Landing Page

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.2 |
| **Epic** | TEA-DOCS-002 |
| **Type** | Documentation |
| **Status** | Done |
| **Priority** | P0 - Must Have |
| **Effort** | 1 hour |

## User Story

**As a** developer building LLM-powered agents,
**I want** a landing page explaining TEA's LLM orchestration capabilities,
**So that** I can quickly understand provider support and find relevant resources.

## Acceptance Criteria

- [ ] Create `docs/capabilities/llm-orchestration.md`
- [ ] Value proposition: 100+ providers, streaming, tool use
- [ ] YAML example showing LLM call with tools (10-15 lines)
- [ ] Provider table (OpenAI, Ollama, Azure, LiteLLM)
- [ ] Actions table (`llm.call`, `llm.stream`, `llm.extract`)
- [ ] Links to LLM examples
- [ ] Links to actions reference

## Existing Resources to Link

| Resource | Path |
|----------|------|
| Actions reference (Python) | `docs/python/actions-reference.md` |
| Actions reference (Rust) | `docs/rust/actions-reference.md` |
| LiteLLM integration | `docs/stories/TEA-LLM-003-litellm-provider-integration.md` |
| Ollama support | `docs/stories/TEA-BUILTIN-009-ollama-provider-support.md` |
| YAML Reference (LLM section) | `docs/shared/YAML_REFERENCE.md` |

**Note**: `examples/llm/` directory does not exist. Use LLM examples from other locations (e.g., examples within actions-reference.md or main examples/).

## Content Structure

Follow TEA-DOCS-002 epic template (lines 57-99). Key sections:
- Value proposition: "100+ providers via LiteLLM, streaming, tool use"
- Provider table: OpenAI, Ollama, Azure, Anthropic, local models
- Actions: `llm.call`, `llm.stream`, `llm.extract`

## Verification Checklist

- [ ] All internal links resolve correctly
- [ ] YAML example is valid
- [ ] Preview in GitHub markdown
- [ ] Provider claims are accurate

## Definition of Done

- [ ] Landing page created at `docs/capabilities/llm-orchestration.md`
- [ ] Follows epic template
- [ ] All links valid (verified)
- [ ] Renders correctly (previewed)
