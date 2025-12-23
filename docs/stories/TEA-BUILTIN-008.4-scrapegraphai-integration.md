# Story TEA-BUILTIN-008.4: ScrapeGraphAI Integration (Python Only)

## Status: Ready for Development

## Story

**As a** TEA agent developer,
**I want** a built-in action `web.ai_scrape` powered by ScrapeGraphAI,
**so that** I can extract structured data from websites using Pydantic schemas with AI-powered extraction.

## Story Context

**Existing System Integration:**
- Integrates with: YAMLEngine action registry (`python/src/the_edge_agent/actions/`)
- Technology: Python + ScrapeGraphAI SDK + Pydantic
- Follows pattern: `web_actions.py` (existing `web.search`, `web.scrape`, `web.crawl`)
- Touch points: `web_actions.py`, `yaml_engine.py`

**Dependencies:**
- **Story 008.2**: Schema loading with Git refs (`schema.uses` syntax)
- **Story 008.3**: Deep merge CLI & algorithm (`schema.merge` action)

**Schema Loading (from 008.2):**
```yaml
# Schema can be loaded from Git repositories
uses: company/schemas@v1.0.0#extraction/law-firm.json
```

**Schema Merging (from 008.3):**
```yaml
# Multiple schemas merged with kubectl-style semantics
schema:
  uses:
    - base/schemas@v1.0.0#common/base.json
    - company/private@main#law-firm/fields.json
```

## Acceptance Criteria

### Functional Requirements

1. **New built-in action**: `web.ai_scrape` using ScrapeGraphAI API
   ```yaml
   - name: extract-data
     uses: web.ai_scrape
     with:
       url: "https://example.com"
       prompt: "Extract company information"
       output_schema: "{{ state.schema }}"
   ```

2. **Pydantic schema support**: Accept JSON Schema or inline dict for structured extraction
   ```yaml
   - name: extract-products
     uses: web.ai_scrape
     with:
       url: "{{ state.target_url }}"
       prompt: "Extract all products with prices"
       output_schema:
         type: object
         properties:
           products:
             type: array
             items:
               type: object
               properties:
                 name: { type: string, description: "Product name" }
                 price: { type: string, description: "Product price" }
                 description: { type: string, description: "Product description" }
   ```

3. **Git schema loading**: Support `schema.uses` syntax from Story 008.2
   ```yaml
   - name: extract-invoice
     uses: web.ai_scrape
     with:
       url: "{{ state.invoice_url }}"
       prompt: "Extract invoice data"
       schema:
         uses: company/schemas@v1.0.0#invoice/schema.json
   ```

4. **Schema merging**: Support multiple `uses:` entries with deep merge from Story 008.3
   ```yaml
   - name: extract-complex
     uses: web.ai_scrape
     with:
       url: "{{ state.url }}"
       prompt: "Extract all data"
       schema:
         uses:
           - base/schemas@v1#common.json
           - company/schemas@v1#custom.json
   ```

5. **API key configuration**: Use `SCRAPEGRAPH_API_KEY` environment variable

6. **Error handling**: Return structured error response on failure
   ```python
   {
       "success": False,
       "error": "API key not configured",
       "error_type": "configuration"
   }
   ```

### Integration Requirements

7. Registered in action registry as `web.ai_scrape` and `actions.web_ai_scrape`
8. Works with YAMLEngine template variables (`{{ state.* }}`)
9. Compatible with existing `web.search`, `web.scrape`, `web.crawl` patterns
10. Schema loading integrates with 008.2 `git_loader` module
11. Schema merging integrates with 008.3 `deep_merge` module

### Quality Requirements

12. Unit tests for action with mocked API responses
13. Integration tests with real ScrapeGraphAI API (optional, requires API key)
14. Documentation with examples
15. Error handling tests (missing API key, invalid schema, network errors)

## Tasks / Subtasks

- [ ] **Task 1: Add ScrapeGraphAI Dependency** (AC: 5)
  - [ ] Add `scrapegraph-py` to `requirements.txt`
  - [ ] Add `SCRAPEGRAPH_API_KEY` to environment documentation

- [ ] **Task 2: Implement web.ai_scrape Action** (AC: 1, 2, 6-9)
  - [ ] Create `web_ai_scrape()` function in `web_actions.py`
  - [ ] Support `url`, `prompt`, `output_schema` parameters
  - [ ] Convert dict/inline schema to Pydantic model dynamically
  - [ ] Handle API key configuration
  - [ ] Return structured response (success/error)
  - [ ] Register action as `web.ai_scrape` and `actions.web_ai_scrape`

- [ ] **Task 3: Schema Loading Integration** (AC: 3, 10)
  - [ ] Import `git_loader` from Story 008.2
  - [ ] Support `schema.uses` for single Git reference
  - [ ] Resolve schema before calling ScrapeGraphAI

- [ ] **Task 4: Schema Merging Integration** (AC: 4, 11)
  - [ ] Import `deep_merge` from Story 008.3
  - [ ] Support multiple `uses:` entries in schema
  - [ ] Merge schemas in order (first = lowest priority)

- [ ] **Task 5: Testing** (AC: 12, 13, 15)
  - [ ] Unit tests with mocked ScrapeGraphAI responses
  - [ ] Test schema conversion (dict to Pydantic)
  - [ ] Test error handling (missing API key, invalid schema)
  - [ ] Integration test (optional, requires `SCRAPEGRAPH_API_KEY`)

- [ ] **Task 6: Documentation** (AC: 14)
  - [ ] Add `web.ai_scrape` section to `docs/python/actions-reference.md`
  - [ ] Add examples to `docs/shared/YAML_REFERENCE.md`
  - [ ] Document environment variable configuration

## Dev Notes

### ScrapeGraphAI API Overview

ScrapeGraphAI provides AI-powered web scraping via a simple API:

```python
from scrapegraph_py import Client
from pydantic import BaseModel, Field
from typing import List

class ProductInfo(BaseModel):
    name: str = Field(description="Product name")
    price: str = Field(description="Product price")
    features: List[str] = Field(description="List of key features")

client = Client(api_key="your-api-key")

response = client.smartscraper(
    website_url="https://example.com/products",
    user_prompt="Extract product information",
    output_schema=ProductInfo
)
```

**Key Features:**
- Uses LLM to extract structured data from any website
- Supports Pydantic schemas for type-safe extraction
- Handles JavaScript-rendered content
- Returns data matching the provided schema

### Python Implementation

```python
def web_ai_scrape(
    state,
    url: str,
    prompt: str,
    output_schema: Optional[Dict[str, Any]] = None,
    schema: Optional[Dict[str, Any]] = None,
    timeout: int = 60,
    **kwargs
) -> Dict[str, Any]:
    """
    Extract structured data from a URL using ScrapeGraphAI.

    Args:
        state: Current workflow state
        url: URL to scrape
        prompt: Natural language prompt describing what to extract
        output_schema: JSON Schema dict for structured output (inline)
        schema: Schema configuration with optional `uses` for Git refs
        timeout: Request timeout in seconds. Default: 60

    Returns:
        On success:
        {
            "success": True,
            "data": {...},           # Extracted data matching schema
            "url": str,
            "schema_used": {...}     # Final merged schema
        }

        On failure:
        {
            "success": False,
            "error": str,
            "error_type": str  # configuration, api_error, schema_error, timeout
        }
    """
    import os
    from typing import Any, Dict, Optional, Type

    # Check for API key
    api_key = os.environ.get('SCRAPEGRAPH_API_KEY')
    if not api_key:
        return {
            "success": False,
            "error": "SCRAPEGRAPH_API_KEY environment variable not set. "
                    "Get your API key from https://scrapegraphai.com",
            "error_type": "configuration"
        }

    try:
        from scrapegraph_py import Client
        from pydantic import BaseModel, Field, create_model
    except ImportError:
        return {
            "success": False,
            "error": "scrapegraph-py package not installed. "
                    "Install with: pip install scrapegraph-py",
            "error_type": "configuration"
        }

    # Resolve schema (handles Git refs and merging)
    final_schema = _resolve_schema(output_schema, schema, engine)

    if final_schema is None:
        return {
            "success": False,
            "error": "No schema provided. Use output_schema or schema.uses",
            "error_type": "schema_error"
        }

    # Convert JSON Schema to Pydantic model
    try:
        pydantic_model = _json_schema_to_pydantic(final_schema)
    except Exception as e:
        return {
            "success": False,
            "error": f"Invalid schema: {str(e)}",
            "error_type": "schema_error"
        }

    # Call ScrapeGraphAI API
    try:
        client = Client(api_key=api_key)

        response = client.smartscraper(
            website_url=url,
            user_prompt=prompt,
            output_schema=pydantic_model
        )

        # Response is already structured according to schema
        return {
            "success": True,
            "data": response if isinstance(response, dict) else response.model_dump(),
            "url": url,
            "schema_used": final_schema
        }

    except Exception as e:
        error_msg = str(e)
        error_type = "api_error"

        if "timeout" in error_msg.lower():
            error_type = "timeout"
        elif "rate limit" in error_msg.lower():
            error_type = "rate_limit"
        elif "api key" in error_msg.lower():
            error_type = "authentication"

        return {
            "success": False,
            "error": f"ScrapeGraphAI error: {error_msg}",
            "error_type": error_type
        }


def _resolve_schema(
    output_schema: Optional[Dict],
    schema_config: Optional[Dict],
    engine: Any
) -> Optional[Dict]:
    """
    Resolve schema from inline dict or Git references.

    Priority:
    1. output_schema (inline dict) - used directly
    2. schema.uses (Git ref or list of refs) - loaded and merged
    """
    # Direct inline schema
    if output_schema:
        return output_schema

    # Schema config with uses
    if schema_config:
        uses = schema_config.get('uses')
        if uses:
            from the_edge_agent.schema.git_loader import fetch_schema_from_git, parse_git_reference
            from the_edge_agent.schema.deep_merge import merge_all

            # Normalize to list
            refs = uses if isinstance(uses, list) else [uses]

            # Load all schemas
            schemas = []
            for ref in refs:
                parsed = parse_git_reference(ref)
                schema = fetch_schema_from_git(parsed)
                schemas.append(schema)

            # Merge schemas (first = lowest priority)
            return merge_all(schemas)

        # Inline schema within config
        if 'inline' in schema_config:
            return schema_config['inline']

    return None


def _json_schema_to_pydantic(schema: Dict[str, Any]) -> Type[BaseModel]:
    """
    Dynamically create Pydantic model from JSON Schema.

    Handles common JSON Schema types:
    - string -> str
    - integer -> int
    - number -> float
    - boolean -> bool
    - array -> List[...]
    - object -> nested model
    """
    from pydantic import BaseModel, Field, create_model
    from typing import List, Optional, Any

    def get_python_type(prop: Dict) -> Any:
        """Convert JSON Schema type to Python type."""
        schema_type = prop.get('type', 'string')

        if schema_type == 'string':
            return str
        elif schema_type == 'integer':
            return int
        elif schema_type == 'number':
            return float
        elif schema_type == 'boolean':
            return bool
        elif schema_type == 'array':
            items = prop.get('items', {})
            item_type = get_python_type(items)
            return List[item_type]
        elif schema_type == 'object':
            # Recursive: create nested model
            nested_props = prop.get('properties', {})
            return _create_model_from_properties(nested_props)
        else:
            return Any

    def _create_model_from_properties(properties: Dict) -> Type[BaseModel]:
        """Create Pydantic model from properties dict."""
        fields = {}
        for name, prop in properties.items():
            python_type = get_python_type(prop)
            description = prop.get('description', '')
            default = prop.get('default', ...)
            fields[name] = (python_type, Field(default=default, description=description))

        return create_model('DynamicSchema', **fields)

    # Root must be object type
    if schema.get('type') != 'object':
        schema = {'type': 'object', 'properties': schema}

    properties = schema.get('properties', {})
    return _create_model_from_properties(properties)
```

### YAML Usage Examples

**Basic usage with inline schema:**
```yaml
name: product-scraper
description: Extract product information from e-commerce sites

state_schema:
  target_url: str
  products: list

nodes:
  - name: scrape-products
    uses: web.ai_scrape
    with:
      url: "{{ state.target_url }}"
      prompt: "Extract all products with their names, prices, and descriptions"
      output_schema:
        type: object
        properties:
          products:
            type: array
            items:
              type: object
              properties:
                name: { type: string, description: "Product name" }
                price: { type: string, description: "Product price" }
                description: { type: string, description: "Product description" }
                in_stock: { type: boolean, description: "Whether product is in stock" }
    output: products

edges:
  - from: __start__
    to: scrape-products
  - from: scrape-products
    to: __end__
```

**Usage with Git schema reference (Story 008.2):**
```yaml
name: law-firm-scraper
description: Extract law firm information using versioned schema

state_schema:
  firm_url: str
  firm_data: dict

nodes:
  - name: extract-firm-info
    uses: web.ai_scrape
    with:
      url: "{{ state.firm_url }}"
      prompt: "Extract law firm information including partners, offices, and practice areas"
      schema:
        uses: acme/legal-schemas@v1.0.0#firm/basic-info.json
    output: firm_data

edges:
  - from: __start__
    to: extract-firm-info
  - from: extract-firm-info
    to: __end__
```

**Usage with schema merging (Story 008.3):**
```yaml
name: comprehensive-extractor
description: Extract data using merged schemas

state_schema:
  url: str
  data: dict

nodes:
  - name: extract-all
    uses: web.ai_scrape
    with:
      url: "{{ state.url }}"
      prompt: "Extract all available information"
      schema:
        uses:
          - base/schemas@v1.0.0#common/base.json       # Base fields
          - base/schemas@v1.0.0#legal/firm.json        # Legal firm fields
          - acme/private@main#custom-fields.json       # Custom overrides
    output: data

edges:
  - from: __start__
    to: extract-all
  - from: extract-all
    to: __end__
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── web_actions.py          # MODIFIED: Add web.ai_scrape
│   └── ...
├── schema/
│   ├── git_loader.py           # From Story 008.2
│   └── deep_merge.py           # From Story 008.3
└── yaml_engine.py

python/tests/
├── test_web_ai_scrape.py       # NEW: Unit tests for web.ai_scrape
└── ...
```

### Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `SCRAPEGRAPH_API_KEY` | Yes | API key from https://scrapegraphai.com |

### Comparison with Existing Actions

| Feature | `web.scrape` | `web.search` | `web.ai_scrape` |
|---------|--------------|--------------|-----------------|
| Provider | Firecrawl | Perplexity | ScrapeGraphAI |
| Extraction Type | DOM-based | Search results | AI-powered schema |
| Schema Support | No | No | Yes (Pydantic) |
| Git Schema Loading | No | No | Yes (Story 008.2) |
| Schema Merging | No | No | Yes (Story 008.3) |
| Best For | Raw content | Information lookup | Structured extraction |

## Testing

### Test File Location
- Python: `python/tests/test_web_ai_scrape.py`

### Test Standards
- Mock ScrapeGraphAI client for unit tests
- Test schema resolution (inline, Git ref, merged)
- Test Pydantic model generation from JSON Schema
- Test error handling (missing API key, invalid schema, network errors)
- Optional integration test with real API key

### Test Cases

```python
import pytest
from unittest.mock import Mock, patch

class TestWebAiScrape:
    """Tests for web.ai_scrape action."""

    def test_missing_api_key_returns_error(self):
        """Should return configuration error when API key missing."""
        with patch.dict('os.environ', {}, clear=True):
            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract data",
                output_schema={"type": "object", "properties": {"name": {"type": "string"}}}
            )
            assert result["success"] is False
            assert result["error_type"] == "configuration"

    def test_inline_schema_extraction(self):
        """Should extract data using inline schema."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('scrapegraph_py.Client') as mock_client:
                mock_client.return_value.smartscraper.return_value = {
                    "name": "Test Product",
                    "price": "$99.99"
                }

                result = web_ai_scrape(
                    state={},
                    url="https://example.com/product",
                    prompt="Extract product info",
                    output_schema={
                        "type": "object",
                        "properties": {
                            "name": {"type": "string"},
                            "price": {"type": "string"}
                        }
                    }
                )

                assert result["success"] is True
                assert result["data"]["name"] == "Test Product"
                assert result["data"]["price"] == "$99.99"

    def test_git_schema_loading(self):
        """Should load schema from Git reference."""
        # Test with mocked git_loader
        pass

    def test_schema_merging(self):
        """Should merge multiple schemas from Git refs."""
        # Test with mocked deep_merge
        pass

    def test_invalid_schema_returns_error(self):
        """Should return schema_error for invalid schema."""
        pass

    def test_api_timeout_handling(self):
        """Should handle API timeout gracefully."""
        pass
```

## Risk Assessment

- **Primary Risk:** ScrapeGraphAI API rate limits or downtime
- **Mitigation:** Implement retry logic with exponential backoff
- **Rollback:** Agent can fall back to `web.scrape` + `llm.generate` pattern

## Definition of Done

- [ ] `scrapegraph-py` added to requirements.txt (AC 5)
- [ ] `web.ai_scrape` action implemented in `web_actions.py` (AC 1, 2, 6-9)
- [ ] Inline schema support working (AC 2)
- [ ] Git schema loading working (AC 3, 10)
- [ ] Schema merging working (AC 4, 11)
- [ ] Error handling complete (AC 6, 15)
- [ ] Unit tests passing (AC 12)
- [ ] Documentation complete (AC 14)

## QA Results

**Review Date:** 2024-12-22
**Reviewer:** Quinn (Test Architect)

### Test Design Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 24 |
| Unit Tests | 14 (58%) |
| Integration Tests | 7 (29%) |
| E2E Tests | 3 (13%) |
| P0 (Critical) | 8 |
| P1 (High) | 10 |
| P2 (Medium) | 6 |

### Coverage by Acceptance Criteria

| AC | Description | Test Count | Status |
|----|-------------|------------|--------|
| AC1 | New `web.ai_scrape` action | 4 | Covered |
| AC2 | Pydantic schema support | 6 | Covered |
| AC3 | Git schema loading | 3 | Covered |
| AC4 | Schema merging | 3 | Covered |
| AC5 | API key configuration | 2 | Covered |
| AC6 | Error handling | 6 | Covered |
| AC7-9 | Integration requirements | 3 | Covered |

### Risk Coverage

| Risk | Priority | Mitigating Tests |
|------|----------|------------------|
| API key not configured | High | 008.4-UNIT-012, 008.4-UNIT-014 |
| Invalid schema crashes | High | 008.4-UNIT-016, 008.4-UNIT-017 |
| Schema conversion fails | Medium | 008.4-UNIT-004 through 008.4-UNIT-009 |
| API timeout/unavailable | Medium | 008.4-INT-006, 008.4-INT-007 |

### Test Design Document

Full test design: `docs/qa/assessments/TEA-BUILTIN-008.4-test-design-20251222.md`

### QA Notes

- **Shift-left strategy applied**: 58% unit tests for fast feedback
- **Mocking strategy defined**: ScrapeGraphAI API, git_loader, deep_merge
- **P0 focus on security**: API key validation is critical path
- **Dependencies**: Requires Stories 008.2 and 008.3 to be implemented first
- **Optional integration tests**: Real API tests require `SCRAPEGRAPH_API_KEY`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Story validated and approved | Bob (SM) |
| 2024-12-22 | 0.3.0 | Test design completed | Quinn (QA) |
| 2024-12-22 | 0.4.0 | Status: Ready for Development | Bob (SM) |

---

## Related Stories

- **TEA-BUILTIN-008**: LlamaExtract Integration Epic (parent)
- **TEA-BUILTIN-008.2**: Schema Loading with Git Refs
- **TEA-BUILTIN-008.3**: Schema Deep Merge CLI & Algorithm
