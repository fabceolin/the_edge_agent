# TEA-DOCS-002.4: Web Automation Capability Landing Page

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-DOCS-002.4 |
| **Epic** | TEA-DOCS-002 |
| **Type** | Documentation |
| **Status** | Done |
| **Priority** | P1 - Should Have |
| **Effort** | 1 hour |

## User Story

**As a** developer building agents that interact with the web,
**I want** a landing page explaining TEA's web automation capabilities,
**So that** I can quickly understand scraping/crawling options and find resources.

## Acceptance Criteria

- [ ] Create `docs/capabilities/web-automation.md`
- [ ] Value proposition: scraping, crawling, AI-powered extraction
- [ ] YAML example showing web scrape + extract (10-15 lines)
- [ ] Tools table (Firecrawl, ScrapeGraphAI, LlamaExtract)
- [ ] Actions table (`web.*`, `scrape.*` actions)
- [ ] Links to web examples

## Existing Resources to Link

| Resource | Path |
|----------|------|
| Web actions | `docs/stories/TEA-BUILTIN-002.1.web-actions.md` |
| ScrapeGraphAI | `docs/stories/TEA-BUILTIN-008.4-scrapegraphai-integration.md` |
| LlamaExtract | `docs/stories/TEA-BUILTIN-008.1-llamaextract-actions.md` |
| Web examples | `examples/web/deep-research-crawler.yaml`, `examples/web/scrapegraph-simple-test.yaml` |

## Verification Checklist

- [ ] All internal links resolve correctly
- [ ] YAML example is valid
- [ ] Preview in GitHub markdown
- [ ] Tools table includes Firecrawl, ScrapeGraphAI, LlamaExtract

## Definition of Done

- [ ] Landing page created at `docs/capabilities/web-automation.md`
- [ ] Follows epic template (TEA-DOCS-002 lines 57-99)
- [ ] All links valid (verified)
- [ ] Renders correctly (previewed)
