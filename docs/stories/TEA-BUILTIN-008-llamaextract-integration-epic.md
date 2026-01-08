# TEA-BUILTIN-008: LlamaExtract Integration Epic

## Status

In Progress (6/10 stories complete, 2 ready for development, 1 deferred)



## Epic Overview

Integrate LlamaCloud LlamaExtract document extraction capabilities into The Edge Agent as a first-class YAML built-in action module. This epic enables TEA agents to extract structured data from documents using LlamaExtract's AI-powered extraction with support for reusable JSON Schemas loaded from Git repositories.

## Business Value

- **Document Intelligence**: Enable TEA agents to extract structured data from PDFs, images, and documents
- **Schema Reusability**: Share and version extraction schemas across projects via Git
- **Enterprise Ready**: Support private repositories and schema composition for complex extraction needs
- **Cross-Runtime**: Full parity between Python and Rust implementations

## Dependencies

### Python Dependencies
```
llama-cloud==0.1.45          # SDK with ExtractMode.PREMIUM support
llama-cloud-services==0.6.88  # Latest API compatibility
```

### Environment Variables
```
LLAMAEXTRACT_API_KEY    # or LLAMAPARSE_API_KEY (fallback)
GIT_SSH_KEY             # Private key for Git auth (optional)
GIT_SSH_KEY_PATH        # Path to SSH key file (alternative)
```

## Epic Scope

### In Scope
1. `llamaextract.*` YAML built-in actions (extract, upload_agent, list_agents) - **Python only**
2. Schema loading via `uses:` syntax with Git repository references - **Python + Rust**
3. Deep merge algorithm for composing multiple schemas (kubectl-style) - **Python + Rust**
4. CLI tool for schema merging and validation - **Python + Rust**
5. Rankellix schema conversion script (custom format → JSON Schema) - **Python only**
6. LlamaExtract agent upload script - **Python only**

### Implementation Notes
- **LlamaExtract actions are Python-only** due to no official Rust SDK
- **Schema loading and merging** are implemented in both runtimes for use with other actions

### Out of Scope
- LlamaParse integration (separate epic)
- Custom extraction model training
- Schema editor UI

## Stories

| Story | Title | Status | Priority |
|-------|-------|--------|----------|
| [TEA-BUILTIN-008.1](TEA-BUILTIN-008.1-llamaextract-actions.md) | Core LlamaExtract Actions | ✅ Complete | P0 |
| [TEA-BUILTIN-008.2](TEA-BUILTIN-008.2-schema-git-loading.md) | Schema Loading with Git Refs & Remote Storage | ✅ Complete | P0 |
| [TEA-BUILTIN-008.3](TEA-BUILTIN-008.3-schema-deep-merge.md) | Schema Deep Merge CLI & Algorithm | ✅ Complete | P1 |
| [TEA-BUILTIN-008.4](TEA-BUILTIN-008.4-scrapegraphai-integration.md) | ScrapeGraphAI Integration | ✅ Complete | P0 |
| [TEA-BUILTIN-008.5](TEA-BUILTIN-008.5-llamaextract-direct-rest-api.md) | LlamaExtract Direct REST API | ✅ Complete | P0 |
| [TEA-BUILTIN-008.6](TEA-BUILTIN-008.6-llamaextract-async-polling.md) | Async Polling Configuration | 🟡 Ready for Dev | P1 |
| [TEA-BUILTIN-008.7](TEA-BUILTIN-008.7-llamaextract-primitives.md) | LlamaExtract Workflow Primitives | 🟡 Ready for Dev | P1 |
| [TEA-BUILTIN-008.7](TEA-BUILTIN-008.7-scrapegraphai-cache.md) | ScrapeGraphAI Result Caching | ✅ Complete | P2 |
| [TEA-BUILTIN-008.9](TEA-BUILTIN-008.9-scrapegraphai-opensource-backend.md) | ScrapeGraphAI Open-Source Backend | ⏸️ Deferred | P3 |

> **Note**: Rankellix Schema Conversion (formerly TEA-BUILTIN-008.4) moved to `spa-base` as [RANKELLIX-001](../../spa-base/docs/stories/RANKELLIX-001-schema-conversion-upload.md) - project-specific tooling.

## Architecture Overview

### Action Module Structure

```
llamaextract.extract      # Extract data from document using schema
llamaextract.upload_agent # Upload/update extraction agent
llamaextract.list_agents  # List available agents
llamaextract.get_agent    # Get agent details
llamaextract.delete_agent # Delete an agent
```

### Schema Loading Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                     YAML Agent Definition                        │
├─────────────────────────────────────────────────────────────────┤
│  nodes:                                                          │
│    - name: extract-invoice                                       │
│      action: llamaextract.extract                                │
│      with:                                                       │
│        file: "{{ state.document_url }}"                          │
│        schema:                                                   │
│          uses:                                                   │
│            - company/schemas@v1.0.0#base/document.json  # Git    │
│            - s3://bucket/schemas/invoice.json           # S3     │
│            - gs://bucket/schemas/overlay.json           # GCS    │
│            - git+ssh://git@github.com/private@main#x.json # SSH  │
│        mode: PREMIUM                                             │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Schema Resolution Pipeline                    │
├─────────────────────────────────────────────────────────────────┤
│  1. Parse uses: references (detect Git vs fsspec URIs)           │
│  2. Route to appropriate fetcher:                                │
│     - Git refs → GitSchemaFetcher (SSH/HTTPS auth)               │
│     - fsspec URIs → FsspecSchemaFetcher (cloud credentials)      │
│  3. Deep merge schemas (last wins, kubectl-style)                │
│  4. Validate merged schema (JSON Schema Draft 2020-12)           │
│  5. Cache resolved schema                                        │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                    LlamaExtract API Call                         │
├─────────────────────────────────────────────────────────────────┤
│  POST /api/v1/extraction/extract                                 │
│  {                                                               │
│    "file": <file_content>,                                       │
│    "data_schema": <merged_json_schema>,                          │
│    "config": { "extraction_mode": "PREMIUM" }                    │
│  }                                                               │
└─────────────────────────────────────────────────────────────────┘
```

### Schema Reference Syntax

Three formats supported (GitHub Actions-inspired + fsspec URIs):

```yaml
# Format 1: Short form Git (GitHub repos)
uses: owner/repo@ref#path/to/schema.json

# Format 2: Full URL Git (any Git host, including private)
uses: git+https://github.com/owner/repo.git@ref#path/to/schema.json
uses: git+ssh://git@github.com/owner/repo.git@ref#path/to/schema.json

# Format 3: fsspec URIs (cloud storage, HTTP, local)
uses: s3://bucket/path/to/schema.json           # AWS S3
uses: gs://bucket/path/to/schema.json           # Google Cloud Storage
uses: az://container/path/to/schema.json        # Azure Blob Storage
uses: https://example.com/schemas/public.json   # HTTP/HTTPS
uses: file:///absolute/path/schema.json         # Local filesystem

# Multiple schemas with deep merge (mixed sources)
uses:
  - base/schemas@v1.0.0#common/base.json        # Git (lowest priority)
  - s3://company-bucket/invoice/fields.json     # S3
  - gs://shared-schemas/overlay.json            # GCS
  - company/private@main#overrides.json         # Git (highest priority)
```

### Deep Merge Algorithm (kubectl-style)

```python
# Last wins for scalar values
# Arrays are replaced (not concatenated)
# Objects are recursively merged

base = {"a": 1, "b": {"x": 10, "y": 20}, "items": [1, 2]}
overlay = {"b": {"y": 30, "z": 40}, "items": [3, 4], "c": 3}

result = deep_merge(base, overlay)
# Result: {"a": 1, "b": {"x": 10, "y": 30, "z": 40}, "items": [3, 4], "c": 3}
```

## Schema Versioning

All schemas must include version metadata:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/schemas/invoice/v1.0.0",
  "title": "Invoice Extraction Schema",
  "version": "1.0.0",
  "properties": {
    ...
  }
}
```

Version follows semantic versioning (MAJOR.MINOR.PATCH):
- MAJOR: Breaking changes to schema structure
- MINOR: New optional fields added
- PATCH: Documentation or description updates

## Acceptance Criteria (Epic Level)

1. **Core Actions**: All `llamaextract.*` actions work in both Python and Rust
2. **Git Loading**: Schemas can be loaded from public and private Git repos
3. **Deep Merge**: Multiple schemas merge correctly with last-wins semantics
4. **CLI Tool**: `tea schema merge` command available for schema composition
5. **Conversion**: Rankellix schemas convert to valid JSON Schema
6. **Upload**: Script successfully uploads agents to LlamaExtract
7. **Documentation**: YAML_REFERENCE.md updated with new actions
8. **Tests**: Unit and integration tests for all components
9. **Parity**: Python and Rust implementations produce identical results

## Technical Risks

| Risk | Mitigation |
|------|------------|
| LlamaExtract API rate limits | Implement retry with exponential backoff |
| Git auth complexity | Support multiple auth methods (SSH key, token) |
| Large schema files | Implement caching with TTL |
| Schema validation errors | Provide clear error messages with line numbers |

## Definition of Done (Epic)

- [x] Core stories (008.1-008.5) completed and merged (6/9 stories complete)
- [x] Documentation updated (YAML_REFERENCE.md, actions-reference.md)
- [x] CI/CD passing for Python (Rust deferred for LlamaExtract - Python-only SDK)
- [x] Example agents demonstrating the feature
- [x] Schema loading and merging with cross-runtime parity (Python + Rust)
- [ ] Advanced async features (008.6-008.7) - Ready for Development
- [x] ScrapeGraphAI integration and caching complete

**Epic Status**: 6/9 stories complete (67%), core functionality delivered and production-ready

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial epic creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Added fsspec URI support to Story 008.2 | Sarah (PO) |
| 2024-12-22 | 0.3.0 | Test designs created (178 total scenarios), stories Ready for Dev | Quinn (QA) |
| 2024-12-22 | 0.4.0 | Story checklist validation PASSED on all 4 stories | Bob (SM) |
| 2026-01-07 | 0.5.0 | Status update: 6/9 stories complete, core functionality production-ready | Sarah (PO) |
