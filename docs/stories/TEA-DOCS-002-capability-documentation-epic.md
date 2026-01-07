# TEA-DOCS-002: Capability Documentation Epic

## Status

Ready for Development


## Epic Metadata

| Field | Value |
|-------|-------|
| **Epic ID** | TEA-DOCS-002 |
| **Type** | Documentation Epic |
| **Status** | Done |
| **Priority** | High |
| **Depends On** | TEA-DOCS-001 (README restructure) |
| **Created** | 2025-12-26 |

## Epic Summary

Create a `/docs/capabilities/` documentation structure with landing pages for each TEA capability. Each landing page provides a quick overview and links to existing detailed documentation.

## Business Value

- **Discoverability**: Users can quickly find the capability they need
- **Completeness**: Shows TEA's full power beyond neurosymbolic AI
- **Navigation**: Gateway section in README (TEA-DOCS-001) links here
- **SEO**: Additional indexed pages for capability-specific searches

## Epic Scope

### In Scope

- Create `/docs/capabilities/` folder structure
- One landing page per capability (~1 page each)
- Links to existing documentation, examples, and actions reference
- Consistent template across all capability pages

### Out of Scope

- Deep rewriting of existing documentation
- New tutorials or guides (separate stories)
- API reference generation (already exists)

## Stories in This Epic

| Story ID | Capability | Priority | Effort | Status |
|----------|------------|----------|--------|--------|
| TEA-DOCS-002.1 | Neurosymbolic AI (Prolog) | P0 | 1 hr | Draft |
| TEA-DOCS-002.2 | LLM Orchestration | P0 | 1 hr | Draft |
| TEA-DOCS-002.3 | RAG & Memory | P1 | 1 hr | Draft |
| TEA-DOCS-002.4 | Web Automation | P1 | 1 hr | Draft |
| TEA-DOCS-002.5 | Parallel Workflows | P1 | 45 min | Draft |
| TEA-DOCS-002.6 | Human-in-the-Loop | P1 | 45 min | Draft |
| TEA-DOCS-002.7 | Edge Deployment | P2 | 45 min | Draft |
| TEA-DOCS-002.8 | Observability | P2 | 45 min | Draft |

**Total Estimated Effort**: 6-7 hours (parallelizable)

## Landing Page Template

Each capability landing page follows this structure:

```markdown
# [Capability Name]

> One-sentence value proposition

## Why This Matters

2-3 sentences on the problem this solves.

## Quick Example

```yaml
# 10-15 line YAML showing capability in action
```

## Key Features

| Feature | Description |
|---------|-------------|
| Feature 1 | Brief description |
| Feature 2 | Brief description |

## Available Actions

| Action | Description |
|--------|-------------|
| `action.name` | What it does |

[Full Actions Reference →](link)

## Examples

- [Example 1](link) - Description
- [Example 2](link) - Description

## Learn More

- [Detailed Guide](link)
- [API Reference](link)
- [Related Story](link)
```

## Folder Structure

```
docs/
└── capabilities/
    ├── README.md              # Index page
    ├── neurosymbolic.md       # TEA-DOCS-002.1
    ├── llm-orchestration.md   # TEA-DOCS-002.2
    ├── rag-memory.md          # TEA-DOCS-002.3
    ├── web-automation.md      # TEA-DOCS-002.4
    ├── parallel-workflows.md  # TEA-DOCS-002.5
    ├── human-in-the-loop.md   # TEA-DOCS-002.6
    ├── edge-deployment.md     # TEA-DOCS-002.7
    └── observability.md       # TEA-DOCS-002.8
```

## Existing Documentation Map

| Capability | Existing Docs | Examples | Actions |
|------------|---------------|----------|---------|
| Neurosymbolic | `docs/stories/TEA-PROLOG-*` | `examples/prolog/` | `prolog.*` |
| LLM Orchestration | `docs/python/actions-reference.md` | `examples/` (various) | `llm.*` |
| RAG & Memory | `docs/stories/TEA-BUILTIN-001.*` | - | `memory.*`, `rag.*` |
| Web Automation | `docs/stories/TEA-BUILTIN-002.*`, `008.*` | `examples/web/` | `web.*`, `scrape.*` |
| Parallel Workflows | `docs/shared/architecture/concepts.md` | `examples/prolog/parity/` | - |
| Human-in-the-Loop | `docs/shared/architecture/checkpoint-guide.md` | - | `checkpoint.*` |
| Edge Deployment | `docs/stories/TEA-RELEASE-*` | - | - |
| Observability | `docs/stories/TEA-OBS-*`, `TEA-BUILTIN-005.*` | - | `trace.*`, `obs.*` |

## Verification Checklist (applies to all stories)

- [ ] Run link validation: `find docs/capabilities -name "*.md" -exec grep -l '\[.*\](.*\.md)' {} \; | xargs -I {} sh -c 'grep -oE "\[.*\]\(.*\.md\)" {} | while read link; do file=$(echo "$link" | sed "s/.*(\(.*\))/\1/"); test -f "docs/capabilities/$file" || test -f "$file" || echo "Broken: $link in {}"; done'`
- [ ] Preview each page in GitHub markdown preview
- [ ] Verify YAML examples are syntactically valid

## Definition of Done (Epic Level)

- [ ] `/docs/capabilities/` folder created
- [ ] Index page (README.md) links to all capability pages
- [ ] All 8 capability landing pages created
- [ ] Each page links to relevant existing docs and examples
- [ ] README.md gateway section (TEA-DOCS-001) links to capabilities index
- [ ] No broken links (verified via checklist)

## Dependencies

```
TEA-DOCS-001 (README Restructure)
       │
       ▼
TEA-DOCS-002 (This Epic)
       │
       ├── TEA-DOCS-002.1 (Neurosymbolic) ─┐
       ├── TEA-DOCS-002.2 (LLM)            │
       ├── TEA-DOCS-002.3 (RAG)            ├── Can run in parallel
       ├── TEA-DOCS-002.4 (Web)            │
       ├── TEA-DOCS-002.5 (Parallel)       │
       ├── TEA-DOCS-002.6 (HITL)           │
       ├── TEA-DOCS-002.7 (Edge)           │
       └── TEA-DOCS-002.8 (Observability) ─┘
```

## Notes

- Stories can be implemented in parallel
- P0 stories should complete before README restructure goes live
- Landing pages point to existing docs; no need to duplicate content

---

## QA Results

### Review Date: 2025-12-27

### Reviewed By: Quinn (Test Architect)

### Epic Quality Assessment

All 8 capability landing pages created successfully following the epic template. Index page at `docs/capabilities/README.md` links all capabilities with consistent structure. Each page includes: value proposition, quick example, feature table, actions table, and links to examples and documentation.

### Substory Status

| Story | Capability | Gate |
|-------|------------|------|
| TEA-DOCS-002.1 | Neurosymbolic | PASS |
| TEA-DOCS-002.2 | LLM Orchestration | PASS |
| TEA-DOCS-002.3 | RAG & Memory | PASS |
| TEA-DOCS-002.4 | Web Automation | PASS |
| TEA-DOCS-002.5 | Parallel Workflows | PASS |
| TEA-DOCS-002.6 | Human-in-the-Loop | PASS |
| TEA-DOCS-002.7 | Edge Deployment | PASS |
| TEA-DOCS-002.8 | Observability | PASS |

### Compliance Check

- Folder Structure: ✓ `docs/capabilities/` created with all 9 files
- Template Adherence: ✓ All pages follow epic template
- Link Validation: ✓ All internal links verified valid
- All ACs Met: ✓ Epic definition of done satisfied

### Gate Status

Gate: PASS → docs/qa/gates/TEA-DOCS-002-capability-documentation-epic.yml

### Recommended Status

✓ Ready for Done (Epic owner decides final status)
