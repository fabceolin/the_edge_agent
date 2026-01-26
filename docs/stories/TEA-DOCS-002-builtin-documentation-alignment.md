# TEA-DOCS-002: Built-in Actions Documentation Alignment Epic

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002 |
| **Type** | Brownfield Enhancement (Epic) |
| **Status** | In Progress |
| **Priority** | High |
| **Estimated Effort** | 3-5 days |
| **Created** | 2026-01-25 |

## User Story

**As a** TEA developer or YAML agent author,
**I want** comprehensive documentation for all ~491 implemented built-in actions,
**So that** I can discover and correctly use TEA's full capabilities without reading source code.

## Story Context

### Problem Statement

An audit of TEA's built-in actions revealed a significant documentation gap:

| Category | Count | Status |
|----------|-------|--------|
| **Implemented actions** | ~491 | Across 55 action modules |
| **Documented actions** | ~67 | In actions-reference.md |
| **Documentation gap** | ~424 | **86% undocumented** |

### Root Cause Analysis

1. **Rapid development**: New actions added without corresponding documentation
2. **Distributed modules**: 55 separate Python files make manual tracking difficult
3. **No doc validation**: No CI check to ensure new actions have documentation

### Business Impact

- **Developer friction**: Users must read source code to discover capabilities
- **Adoption barrier**: Potential users can't evaluate TEA's full feature set
- **Support burden**: Questions about undocumented features increase support load
- **Hallucination risk**: AI assistants "invent" non-existent actions due to incomplete docs

## Scope

### In Scope

1. Document all implemented built-in actions in `actions-reference.md`
2. Create module-specific documentation for major action categories
3. Establish automated doc-implementation alignment validation
4. Update YAML_REFERENCE.md with accurate action inventory

### Out of Scope

- Implementing new actions
- Changing action behavior
- Rust implementation documentation (separate story)

## Acceptance Criteria

### Documentation Completeness

- [ ] **AC-1**: All 55 action modules have corresponding documentation
- [ ] **AC-2**: Each documented action includes: signature, parameters, return value, example
- [ ] **AC-3**: `actions-reference.md` serves as master index with links to module docs

### Module Documentation

- [ ] **AC-4**: Create `docs/python/actions/` directory for module-specific docs
- [ ] **AC-5**: High-priority modules documented first (see priority table below)
- [ ] **AC-6**: Each module doc follows consistent template

### Validation & Automation

- [ ] **AC-7**: Script to extract action signatures from implementation
- [ ] **AC-8**: CI check that fails if new actions lack documentation
- [ ] **AC-9**: Generated action inventory matches implementation

### Quality Requirements

- [ ] **AC-10**: All code examples are tested and runnable
- [ ] **AC-11**: Cross-references between related actions are accurate
- [ ] **AC-12**: No broken internal links

## Technical Notes

### Action Module Priority

| Priority | Module | Actions | Current Doc Status |
|----------|--------|---------|-------------------|
| P0 | llm_actions.py | 15+ | Partial |
| P0 | memory_actions.py | 10+ | Minimal |
| P0 | data_actions.py | 20+ | Partial |
| P1 | graph_actions.py | 50+ | None |
| P1 | agent_actions.py | 10+ | None |
| P1 | neo4j_actions.py | 36+ | None |
| P2 | validation_actions.py | 14+ | None |
| P2 | reason_actions.py | 14+ | None |
| P2 | plan_actions.py | 8+ | None |
| P3 | All others | 200+ | None |

### Key Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/actions/__init__.py` | Action registration master |
| `python/src/the_edge_agent/actions/*.py` | 55 action module files |
| `docs/python/actions-reference.md` | Current documentation (incomplete) |
| `docs/shared/YAML_REFERENCE.md` | YAML syntax reference |

### Documentation Template

```markdown
## module_name.action_name

**Signature**: `module_name.action_name(param1, param2, ...)`

**Description**: Brief description of what the action does.

**Parameters**:
| Name | Type | Required | Description |
|------|------|----------|-------------|
| param1 | str | Yes | Description |
| param2 | int | No | Description (default: value) |

**Returns**: Description of return value and type.

**Example**:
```yaml
run: |
  result = module_name.action_name(
    param1="value",
    param2=42
  )
  return {"result": result}
```

**Related Actions**: `module_name.related_action`
```

## Implementation Approach

### Phase 1: Inventory & Automation (AC-7, AC-9)

1. Create `scripts/extract_action_signatures.py` to parse all action modules
2. Generate machine-readable action inventory (JSON/YAML)
3. Compare against current documentation to identify gaps

### Phase 2: High-Priority Documentation (AC-1, AC-2, AC-5)

1. Document P0 modules (llm, memory, data)
2. Document P1 modules (graph, agent, neo4j)
3. Update `actions-reference.md` as master index

### Phase 3: Complete Documentation (AC-3, AC-4, AC-6)

1. Document P2 and P3 modules
2. Create module-specific docs in `docs/python/actions/`
3. Add cross-references and examples

### Phase 4: CI Integration (AC-8)

1. Add GitHub Action to validate doc-implementation alignment
2. Fail builds when new actions lack documentation

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Scope creep | High | Medium | Strict prioritization, phase delivery |
| Stale docs | Medium | High | Automated validation in CI |
| Incomplete examples | Medium | Medium | Test all examples in CI |

## Verification Checklist

- [ ] Run `python scripts/extract_action_signatures.py` - inventory generated
- [ ] Compare inventory to docs - gap reduced to <10%
- [ ] Run all documentation examples - all pass
- [ ] CI validation enabled and passing
- [ ] No broken links in documentation

## Definition of Done

- [ ] All P0 and P1 modules fully documented
- [ ] `actions-reference.md` serves as complete index
- [ ] Automated signature extraction script working
- [ ] CI check for doc-implementation alignment
- [ ] All code examples tested
- [ ] No broken links

## Child Stories

| Story ID | Title | Status |
|----------|-------|--------|
| TEA-DOCS-002.1 | Action Signature Extraction Script | QA Approved |
| TEA-DOCS-002.2 | Update actions-reference.md Master Index | Draft |
| TEA-DOCS-002.3 | P0 Module Documentation (llm, memory, data) | Draft |
| TEA-DOCS-002.4 | P1 Module Documentation (graph, agent, neo4j) | Draft |
| TEA-DOCS-002.5 | CI Documentation Validation | Approved |

## Related Stories

- TEA-DOCS-001: README Neurosymbolic Focus (completed)
- Future: TEA-DOCS-003: Rust Actions Documentation

## Appendix: Verified Action Inventory

### Confirmed Implemented (from audit)

**LLM Actions**: `llm.call`, `llm.stream`, `llm.tools`, `llm.retry`
**Memory Actions**: `memory.store`, `memory.retrieve`, `memory.summarize`
**LTM Actions**: `ltm.store`, `ltm.retrieve`, `ltm.search`, `ltm.delete`
**Data Actions**: `json.parse`, `json.stringify`, `json.transform`, `csv.parse`, `csv.stringify`, `data.filter`, `data.merge`, `data.validate`
**A2A Actions**: `a2a.send`, `a2a.receive`, `a2a.broadcast`, `a2a.delegate`, `a2a.discover`, `a2a.state.get`, `a2a.state.set`
**Auth Actions**: `auth.verify`, `auth.get_user`
**Cache Actions**: `cache.wrap`, `cache.get`, `cache.invalidate`
**File Actions**: `file.read`, `file.write`
**HTTP Actions**: `http.get`, `http.post`
**Checkpoint Actions**: `checkpoint.save`, `checkpoint.load`

### Confirmed NOT Implemented (hallucinations)

- `prolog.*` (no YAML actions, only runtime support)
- `mcp.*` (only `tools.mcp` bridge, not direct actions)
- `schema.validate`, `schema.generate` (only `schema.merge`)
- `auth.check_permission`, `auth.refresh_token`
- `cache.set` (use `cache.wrap` instead)
- `memory.search`, `memory.delete`
- `file.append`, `file.delete`, `file.exists`
- `http.put`, `http.patch`, `http.delete`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-25 | 1.0 | Initial draft | Sarah (PO) |
| 2026-01-25 | 1.1 | TEA-DOCS-002.1 completed (QA Approved), TEA-DOCS-002.5 drafted and approved | Bob (SM) |
