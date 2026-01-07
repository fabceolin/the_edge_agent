# TEA-DOCS-002.3: RAG & Memory Capability Landing Page

## Status

Ready for Development


## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.3 |
| **Epic** | TEA-DOCS-002 |
| **Type** | Documentation |
| **Status** | Done |
| **Priority** | P1 - Should Have |
| **Effort** | 1 hour |

## User Story

**As a** developer building agents that need memory and retrieval,
**I want** a landing page explaining TEA's RAG and memory capabilities,
**So that** I can quickly understand the options and find relevant resources.

## Acceptance Criteria

- [ ] Create `docs/capabilities/rag-memory.md`
- [ ] Value proposition: embeddings, vector search, short/long-term memory
- [ ] YAML example showing RAG workflow (10-15 lines)
- [ ] Memory types table (short-term, long-term, cloud-synced)
- [ ] LTM backends table (SQLite, DuckDB, Litestream)
- [ ] Actions table (`memory.*`, `rag.*` actions)
- [ ] Links to LTM documentation and examples

## Existing Resources to Link

| Resource | Path |
|----------|------|
| LTM backend guide | `CLAUDE.md` section on LTM |
| Memory actions story | `docs/stories/TEA-BUILTIN-001.1.memory-actions.md` |
| Long-term memory | `docs/stories/TEA-BUILTIN-001.4.long-term-memory.md` |
| DuckDB LTM | `docs/stories/TEA-BUILTIN-001.6.duckdb-ltm-backend.md` |
| RAG actions | `docs/stories/TEA-BUILTIN-002.2.rag-actions.md` |

## Verification Checklist

- [ ] All internal links resolve correctly
- [ ] YAML example is valid
- [ ] Preview in GitHub markdown
- [ ] LTM backend table matches CLAUDE.md lines 154-199

## Definition of Done

- [ ] Landing page created at `docs/capabilities/rag-memory.md`
- [ ] Follows epic template (TEA-DOCS-002 lines 57-99)
- [ ] All links valid (verified)
- [ ] Renders correctly (previewed)
