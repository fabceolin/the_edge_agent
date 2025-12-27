# TEA-DOCS-001: README Neurosymbolic Focus Restructure

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-001 |
| **Type** | Brownfield Enhancement |
| **Status** | Done |
| **Priority** | High |
| **Estimated Effort** | 2-4 hours |
| **Created** | 2025-12-26 |

## User Story

**As a** potential TEA user visiting the GitHub repository,
**I want** to immediately understand TEA's unique neurosymbolic value proposition,
**So that** I can decide within 30 seconds if TEA solves my problem (and star the repo).

## Story Context

### Existing System Integration

| Aspect | Details |
|--------|---------|
| **Integrates with** | Existing `docs/` structure, `examples/` directory |
| **Technology** | Markdown, GitHub-flavored markdown |
| **Follows pattern** | Current documentation style in `docs/shared/` |
| **Touch points** | README.md only (links to existing docs) |

### Business Context

- **Goal**: Increase GitHub stars through clearer value proposition
- **Current State**: README.md is 547 lines with diluted messaging
- **Target State**: Focused README (~150 lines) with neurosymbolic AI as the hook

## Acceptance Criteria

### Functional Requirements

- [ ] **AC-1**: README.md is reduced from ~547 lines to ~150 lines
- [ ] **AC-2**: Neurosymbolic AI (Prolog + LLM) is positioned in first 20 lines
- [ ] **AC-3**: A 10-15 line YAML "hero example" showing Prolog + LLM appears before line 50
- [ ] **AC-4**: "Why TEA?" table is preserved and refined
- [ ] **AC-5**: Installation reduced to 3-5 lines with link to full docs

### Discoverability Requirements

- [ ] **AC-6**: "TEA Does More" gateway section links to existing capability docs
- [ ] **AC-7**: All removed content has a clear home in existing `/docs/` (no content loss)
- [ ] **AC-8**: Binary download matrix moved to `docs/installation.md` (create if needed)

### Star-Optimization Requirements

- [ ] **AC-9**: GitHub star badge added to header
- [ ] **AC-10**: Clear call-to-action for starring
- [ ] **AC-11**: "vs Alternatives" comparison table added (TEA vs LangGraph/AutoGen)

### Quality Requirements

- [ ] **AC-12**: All existing links remain valid or are updated
- [ ] **AC-13**: No broken internal references
- [ ] **AC-14**: README renders correctly on GitHub

## Technical Notes

| Aspect | Details |
|--------|---------|
| **Integration Approach** | Extract content to existing docs, keep README as "landing page" |
| **Existing Pattern Reference** | `docs/shared/YAML_REFERENCE.md` for doc style |
| **Key Constraints** | Must not lose any existing documentation content |

## Content Migration Map

| Current README Section | Lines (approx) | Destination |
|------------------------|----------------|-------------|
| Binary download tables | 44-100 | `docs/installation.md` (new) |
| CLI Usage details | 193-246 | `docs/shared/cli-reference.md` |
| Interactive Interrupt Workflow | 306-426 | `docs/guides/human-in-the-loop.md` (new) |
| Python API Usage | 451-530 | `docs/python/api-usage.md` |
| Custom Actions Modules | 249-304 | `docs/python/custom-actions.md` (new) |

## Proposed README Structure

```markdown
# The Edge Agent (TEA)

![Stars](badge) ![License](badge)

> **Small LLMs hallucinate. TEA fixes that with Prolog.**

One binary. No cloud. Neurosymbolic AI that actually reasons.

## 30-Second Example
[10-15 line YAML showing Prolog + LLM]

## Why TEA?
[Existing table, refined]

## Quick Install
[3 lines + link to docs/installation.md]

## vs Alternatives
[TEA vs LangGraph vs AutoGen comparison]

## TEA Does More
[Gateway table linking to all capabilities]

## Contributing / License
[Brief, link to full docs]
```

## Risk Assessment

| Risk Type | Assessment |
|-----------|------------|
| **Primary Risk** | SEO impact from reduced README content |
| **Mitigation** | Ensure key terms remain in README, proper linking |
| **Rollback** | Git revert to previous README.md |

### Compatibility Verification

- [x] No breaking changes to code
- [x] No API changes
- [x] Documentation only
- [x] Easily reversible

## Hero Example Source

Use `examples/prolog/neurosymbolic/llm-prolog-family-reasoning-interview.yaml` as the basis for the hero example. Extract a minimal 10-15 line snippet showing:
- LLM extracting entities from natural language
- Prolog knowledge base with facts/rules
- Query that demonstrates reasoning the LLM couldn't do alone

## Key Terms (for implementer context)

| Term | Definition |
|------|------------|
| **Neurosymbolic AI** | Combining neural networks (LLMs) with symbolic reasoning (Prolog) to get both flexibility and guaranteed logical correctness |
| **Grounding** | Using Prolog facts/rules to constrain LLM outputs to provable truths |
| **Prolog** | Logic programming language for declarative reasoning, constraint solving |

## Verification Checklist

- [ ] Run `wc -l README.md` - must return <= 150
- [ ] Verify all links: `grep -oE '\[.*\]\(.*\)' README.md` and check each
- [ ] Push to feature branch and preview in GitHub
- [ ] Verify hero example runs: `tea run examples/prolog/neurosymbolic/[hero-example].yaml`
- [ ] PR reviewed by at least one team member

## Definition of Done

- [ ] README.md <= 150 lines
- [ ] Neurosymbolic value prop in first 20 lines
- [ ] Hero YAML example before line 50
- [ ] Star badge + CTA present
- [ ] "vs Alternatives" table present
- [ ] Gateway section links to capabilities
- [ ] All migrated content preserved in `/docs/`
- [ ] No broken links (verified via checklist)
- [ ] Renders correctly on GitHub (verified via preview)

## Related Stories

- Future: TEA-DOCS-002 - Capability Documentation (LLM, RAG, Web, etc.)
- Future: TEA-DOCS-003 - Installation Guide Consolidation

## Notes

- This story focuses on README only; capability documentation is a separate effort
- If scope grows during implementation, consider splitting into epic
- Priority is discoverability and star conversion, not comprehensive documentation

---

## QA Results

### Review Date: 2025-12-27

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Excellent documentation restructure. README reduced from 547 to 144 lines while maintaining all essential information. Hero example is tested and working on both Python and Rust runtimes. All migrated content preserved in appropriate `/docs/` locations.

### Refactoring Performed

None required - documentation only story.

### Compliance Check

- Coding Standards: N/A (documentation)
- Project Structure: ✓ Follows existing `/docs/` patterns
- Testing Strategy: ✓ Hero example verified runnable
- All ACs Met: ✓ 13/14 verified, 1 pending manual preview

### Improvements Checklist

- [x] README restructured to 144 lines
- [x] Neurosymbolic value prop in first 10 lines
- [x] Hero example before line 50
- [x] All 5 migration docs created
- [x] All links verified valid
- [x] Hero example tested on both runtimes
- [ ] GitHub preview verification (manual step)

### Security Review

No security concerns - documentation only.

### Performance Considerations

N/A - documentation only.

### Files Modified During Review

None.

### Gate Status

Gate: PASS → docs/qa/gates/TEA-DOCS-001-readme-neurosymbolic-focus.yml

### Recommended Status

✓ Ready for Done (Story owner decides final status)
