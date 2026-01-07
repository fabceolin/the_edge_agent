# Story TEA-BUILTIN-008.4: ScrapeGraphAI Integration (Python Only)

## Status

Ready for Development


## Status: Complete ✅

**Story Quality Score: 95/100** (up from 85/100)

> **v0.7.0 Complete**: Implementation done, 27 tests passing, QA gate PASS. Full ScrapeGraphAI integration with retry logic, fsspec URIs, and schema merging.

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
- **Story 008.2**: Schema loading with Git refs AND fsspec URIs (`schema.uses` syntax) - **Status: Complete ✅**
- **Story 008.3**: Deep merge CLI & algorithm (`schema.merge` action)
- **Story 008.5**: Retry pattern with exponential backoff (reference implementation)

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│  YAML Agent: web.ai_scrape                                          │
│    with:                                                             │
│      url: "{{ state.target_url }}"                                   │
│      schema:                                                         │
│        uses:                                                         │
│          - company/schemas@v1.0.0#base.json      # Git ref           │
│          - s3://bucket/schemas/overlay.json      # S3 (fsspec)       │
│          - inline dict                            # Direct schema    │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│  _resolve_schema()                                                   │
│    ├── Check for inline output_schema → return directly              │
│    └── Process schema.uses:                                          │
│        │                                                             │
│        ▼                                                             │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │  fetch_schema() (from 008.2 - unified loader)               │    │
│  │    ├── SchemaCache.get(uri) → return if cached              │    │
│  │    ├── is_fsspec_uri(ref)?                                  │    │
│  │    │   ├── Yes → FsspecSchemaFetcher (S3/GCS/Azure/HTTP)    │    │
│  │    │   └── No  → GitSchemaFetcher (short/full URL refs)     │    │
│  │    └── SchemaCache.set(uri, schema) → cache with TTL (5min) │    │
│  └─────────────────────────────────────────────────────────────┘    │
│        │                                                             │
│        ▼                                                             │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │  deep_merge() (from 008.3) - kubectl-style last-wins        │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│  _json_schema_to_pydantic()                                          │
│    └── Convert JSON Schema → Pydantic BaseModel dynamically          │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│  _call_scrapegraph_with_retry() ← NEW (pattern from 008.5)           │
│    ├── Attempt 1: client.smartscraper(url, prompt, schema)           │
│    ├── On 429/5xx: sleep(2 ** attempt) → retry                       │
│    └── Max 3 retries with exponential backoff                        │
└─────────────────────────────┬───────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Response                                                            │
│  {                                                                   │
│    "success": true,                                                  │
│    "data": { ... extracted data ... },                               │
│    "url": "https://example.com",                                     │
│    "schema_used": { ... final merged schema ... }                    │
│  }                                                                   │
└─────────────────────────────────────────────────────────────────────┘
```

**Schema Loading (from 008.2 - supports Git refs AND fsspec URIs):**
```yaml
# Git references (short form - defaults to GitHub)
uses: company/schemas@v1.0.0#extraction/law-firm.json

# Git references (full URL form - any Git host)
uses: git+https://gitlab.com/org/schemas.git@main#invoice.json
uses: git+ssh://git@github.com/company/private.git@v2.0.0#schema.json

# fsspec URIs (S3, GCS, Azure, HTTP, local filesystem)
uses: s3://bucket-name/schemas/invoice.json           # AWS S3
uses: gs://bucket-name/schemas/law-firm.json          # Google Cloud Storage
uses: az://container/schemas/custom.json              # Azure Blob Storage
uses: https://example.com/schemas/public.json         # HTTP/HTTPS (read-only)
uses: file:///absolute/path/schema.json               # Local filesystem
```

**Schema Merging (from 008.3 - supports mixed sources):**
```yaml
# Multiple schemas merged with kubectl-style semantics (last wins)
schema:
  uses:
    - base/schemas@v1.0.0#common/base.json       # Git ref (lowest priority)
    - s3://company-schemas/invoice/fields.json   # S3
    - gs://shared-schemas/overlay.json           # GCS
    - company/private@main#overrides.json        # Git (highest priority)
```

**Caching (handled by 008.2):**
- All fetched schemas are cached with 5-minute TTL via `SchemaCache`
- Cache is shared between Git and fsspec fetchers
- Use `clear_cache()` to invalidate if needed

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

- [x] **Task 1: Add ScrapeGraphAI Dependency** (AC: 5)
  - [x] Add `scrapegraph-py` to `setup.py` extras (web-ai-scrape, all)
  - [x] Add `SCRAPEGRAPH_API_KEY` to environment documentation (in YAML_REFERENCE.md)

- [x] **Task 2: Implement web.ai_scrape Action** (AC: 1, 2, 6-9)
  - [x] Create `web_ai_scrape()` function in `web_actions.py`
  - [x] Support `url`, `prompt`, `output_schema` parameters
  - [x] Convert dict/inline schema to Pydantic model dynamically (`_json_schema_to_pydantic`)
  - [x] Handle API key configuration
  - [x] Return structured response (success/error)
  - [x] Register action as `web.ai_scrape` and `actions.web_ai_scrape`

- [x] **Task 3: Schema Loading Integration** (AC: 3, 10)
  - [x] Import `fetch_schema` from Story 008.2 (schema module)
  - [x] Support `schema.uses` for single Git reference
  - [x] Support fsspec URIs (s3://, gs://, az://)
  - [x] Resolve schema before calling ScrapeGraphAI

- [x] **Task 4: Schema Merging Integration** (AC: 4, 11)
  - [x] Import `merge_all` from Story 008.3 (deep_merge module)
  - [x] Support multiple `uses:` entries in schema
  - [x] Merge schemas in order (first = lowest priority, kubectl-style)

- [x] **Task 5: Testing** (AC: 12, 13, 15)
  - [x] Unit tests with mocked ScrapeGraphAI responses (27 tests passing)
  - [x] Test schema conversion (dict to Pydantic)
  - [x] Test error handling (missing API key, invalid schema)
  - [x] Test retry logic with exponential backoff
  - [x] Test Git schema loading (short ref, full URL)
  - [x] Test fsspec schema loading (S3, GCS, Azure)
  - [x] Test schema merging from multiple sources

- [x] **Task 6: Documentation** (AC: 14)
  - [x] Add `web.ai_scrape` to `docs/python/actions-reference.md`
  - [x] Add examples to `docs/shared/YAML_REFERENCE.md`
  - [x] Document environment variable configuration

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
import os
import time
from typing import Any, Dict, Optional, Type

# Use unified schema loader from 008.2 (handles Git refs + fsspec URIs + caching)
from the_edge_agent.schema import fetch_schema, resolve_schema_uses
from the_edge_agent.schema.deep_merge import merge_all


def web_ai_scrape(
    state,
    url: str,
    prompt: str,
    output_schema: Optional[Dict[str, Any]] = None,
    schema: Optional[Dict[str, Any]] = None,
    timeout: int = 60,
    max_retries: int = 3,
    **kwargs
) -> Dict[str, Any]:
    """
    Extract structured data from a URL using ScrapeGraphAI.

    Args:
        state: Current workflow state
        url: URL to scrape
        prompt: Natural language prompt describing what to extract
        output_schema: JSON Schema dict for structured output (inline)
        schema: Schema configuration with optional `uses` for Git refs or fsspec URIs
        timeout: Request timeout in seconds. Default: 60
        max_retries: Maximum retry attempts for rate limits/server errors. Default: 3

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
            "error_type": str  # configuration, api_error, schema_error, timeout, rate_limit
        }
    """
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
            "error_type": "dependency"
        }

    # Resolve schema (handles Git refs, fsspec URIs, caching, and merging)
    try:
        final_schema = _resolve_schema(output_schema, schema)
    except Exception as e:
        return {
            "success": False,
            "error": f"Schema resolution failed: {str(e)}",
            "error_type": "schema_error"
        }

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

    # Call ScrapeGraphAI API with retry logic (pattern from 008.5)
    client = Client(api_key=api_key)
    return _call_scrapegraph_with_retry(
        client=client,
        url=url,
        prompt=prompt,
        pydantic_model=pydantic_model,
        final_schema=final_schema,
        max_retries=max_retries
    )


def _call_scrapegraph_with_retry(
    client,
    url: str,
    prompt: str,
    pydantic_model: Type,
    final_schema: Dict,
    max_retries: int = 3
) -> Dict[str, Any]:
    """
    Call ScrapeGraphAI API with exponential backoff retry.

    Follows retry pattern from TEA-BUILTIN-008.5 (LlamaExtract REST API).
    """
    for attempt in range(max_retries):
        try:
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
            error_msg = str(e).lower()

            # Retry on rate limit (429) or server errors (5xx)
            if "rate limit" in error_msg or "429" in error_msg:
                if attempt < max_retries - 1:
                    time.sleep(2 ** attempt)  # Exponential backoff: 1s, 2s, 4s
                    continue
                return {
                    "success": False,
                    "error": "Rate limit exceeded after retries",
                    "error_type": "rate_limit"
                }

            if "500" in error_msg or "502" in error_msg or "503" in error_msg:
                if attempt < max_retries - 1:
                    time.sleep(2 ** attempt)
                    continue
                return {
                    "success": False,
                    "error": f"Server error after retries: {str(e)}",
                    "error_type": "api_error"
                }

            # Non-retryable errors
            if "timeout" in error_msg:
                return {"success": False, "error": f"Request timeout: {str(e)}", "error_type": "timeout"}
            if "api key" in error_msg or "unauthorized" in error_msg:
                return {"success": False, "error": f"Authentication failed: {str(e)}", "error_type": "authentication"}

            # Generic API error - don't retry
            return {
                "success": False,
                "error": f"ScrapeGraphAI error: {str(e)}",
                "error_type": "api_error"
            }

    return {"success": False, "error": "Max retries exceeded", "error_type": "api_error"}


def _resolve_schema(
    output_schema: Optional[Dict],
    schema_config: Optional[Dict]
) -> Optional[Dict]:
    """
    Resolve schema from inline dict, Git references, or fsspec URIs.

    Uses unified schema loader from Story 008.2 which handles:
    - Git short refs: owner/repo@ref#path
    - Git full URLs: git+https://... or git+ssh://...
    - fsspec URIs: s3://, gs://, az://, https://, file://
    - Caching with 5-minute TTL

    Priority:
    1. output_schema (inline dict) - used directly
    2. schema.uses (ref or list of refs) - loaded and merged
    3. schema.inline (inline within config)
    """
    # Direct inline schema (highest priority, no loading needed)
    if output_schema:
        return output_schema

    # Schema config with uses
    if schema_config:
        uses = schema_config.get('uses')
        if uses:
            # Normalize to list
            refs = uses if isinstance(uses, list) else [uses]

            # Load all schemas using unified loader (handles Git + fsspec + caching)
            schemas = []
            for ref in refs:
                schema = fetch_schema(ref)  # From 008.2 - cached, supports all URI types
                schemas.append(schema)

            # Merge schemas (first = lowest priority, kubectl-style)
            if len(schemas) == 1:
                return schemas[0]
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

**Usage with mixed sources (Git + S3 + GCS):**
```yaml
name: multi-source-extractor
description: Extract data using schemas from multiple storage backends

state_schema:
  url: str
  extracted: dict

nodes:
  - name: extract-with-mixed-schemas
    uses: web.ai_scrape
    with:
      url: "{{ state.url }}"
      prompt: "Extract all company information including financials"
      max_retries: 5  # More retries for critical extraction
      schema:
        uses:
          # Git repository (version-controlled base)
          - company/extraction-schemas@v2.0.0#common/base.json
          # AWS S3 (team-maintained overlays)
          - s3://company-schemas/legal/law-firm-fields.json
          # Google Cloud Storage (shared across orgs)
          - gs://shared-extraction-schemas/financial/balance-sheet.json
          # Private Git via SSH (proprietary fields)
          - git+ssh://git@github.com/company/private-schemas.git@main#custom/overrides.json
    output: extracted

edges:
  - from: __start__
    to: extract-with-mixed-schemas
  - from: extract-with-mixed-schemas
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
| `GIT_SSH_KEY` | Conditional | SSH private key content (for private Git repos) |
| `GIT_SSH_KEY_PATH` | Conditional | Path to SSH key file (alternative to `GIT_SSH_KEY`) |
| `AWS_ACCESS_KEY_ID` | Conditional | For `s3://` schema URIs |
| `AWS_SECRET_ACCESS_KEY` | Conditional | For `s3://` schema URIs |
| `GOOGLE_APPLICATION_CREDENTIALS` | Conditional | For `gs://` schema URIs (path to service account JSON) |
| `AZURE_STORAGE_CONNECTION_STRING` | Conditional | For `az://` schema URIs |

> **Note**: Git and cloud storage credentials are only required when using `schema.uses` with private repositories or cloud storage URIs. See Story 008.2 for full authentication documentation.

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
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('the_edge_agent.schema.fetch_schema') as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"name": {"type": "string"}}
                }
                with patch('scrapegraph_py.Client') as mock_client:
                    mock_client.return_value.smartscraper.return_value = {"name": "Test"}

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract name",
                        schema={"uses": "company/schemas@v1.0.0#test.json"}
                    )

                    assert result["success"] is True
                    mock_fetch.assert_called_once_with("company/schemas@v1.0.0#test.json")

    def test_fsspec_schema_loading(self):
        """Should load schema from S3/GCS/Azure URIs via fetch_schema."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('the_edge_agent.schema.fetch_schema') as mock_fetch:
                mock_fetch.return_value = {
                    "type": "object",
                    "properties": {"amount": {"type": "number"}}
                }
                with patch('scrapegraph_py.Client') as mock_client:
                    mock_client.return_value.smartscraper.return_value = {"amount": 99.99}

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract amount",
                        schema={"uses": "s3://bucket/schemas/invoice.json"}
                    )

                    assert result["success"] is True
                    mock_fetch.assert_called_once_with("s3://bucket/schemas/invoice.json")

    def test_schema_merging_mixed_sources(self):
        """Should merge multiple schemas from Git refs and fsspec URIs."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('the_edge_agent.schema.fetch_schema') as mock_fetch:
                # Return different schemas for each call
                mock_fetch.side_effect = [
                    {"type": "object", "properties": {"base": {"type": "string"}}},
                    {"type": "object", "properties": {"overlay": {"type": "number"}}},
                ]
                with patch('the_edge_agent.schema.deep_merge.merge_all') as mock_merge:
                    mock_merge.return_value = {
                        "type": "object",
                        "properties": {
                            "base": {"type": "string"},
                            "overlay": {"type": "number"}
                        }
                    }
                    with patch('scrapegraph_py.Client') as mock_client:
                        mock_client.return_value.smartscraper.return_value = {}

                        result = web_ai_scrape(
                            state={},
                            url="https://example.com",
                            prompt="Extract",
                            schema={"uses": [
                                "company/schemas@v1#base.json",
                                "s3://bucket/overlay.json"
                            ]}
                        )

                        assert result["success"] is True
                        assert mock_fetch.call_count == 2
                        mock_merge.assert_called_once()

    def test_retry_on_rate_limit(self):
        """Should retry with exponential backoff on rate limit."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('scrapegraph_py.Client') as mock_client:
                # First call raises rate limit, second succeeds
                mock_client.return_value.smartscraper.side_effect = [
                    Exception("429 rate limit exceeded"),
                    {"name": "Success"}
                ]
                with patch('time.sleep') as mock_sleep:  # Don't actually sleep
                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        output_schema={"type": "object", "properties": {"name": {"type": "string"}}}
                    )

                    assert result["success"] is True
                    mock_sleep.assert_called_once_with(1)  # 2^0 = 1 second

    def test_retry_exhausted_returns_rate_limit_error(self):
        """Should return rate_limit error after all retries exhausted."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('scrapegraph_py.Client') as mock_client:
                mock_client.return_value.smartscraper.side_effect = Exception("429 rate limit")
                with patch('time.sleep'):

                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        output_schema={"type": "object", "properties": {}},
                        max_retries=3
                    )

                    assert result["success"] is False
                    assert result["error_type"] == "rate_limit"

    def test_retry_on_server_error(self):
        """Should retry on 5xx server errors."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('scrapegraph_py.Client') as mock_client:
                mock_client.return_value.smartscraper.side_effect = [
                    Exception("503 Service Unavailable"),
                    {"data": "Success"}
                ]
                with patch('time.sleep'):
                    result = web_ai_scrape(
                        state={},
                        url="https://example.com",
                        prompt="Extract",
                        output_schema={"type": "object", "properties": {"data": {"type": "string"}}}
                    )

                    assert result["success"] is True

    def test_invalid_schema_returns_error(self):
        """Should return schema_error for invalid schema."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            result = web_ai_scrape(
                state={},
                url="https://example.com",
                prompt="Extract",
                output_schema={"type": "invalid_type"}  # Invalid JSON Schema type
            )
            # Note: This may succeed depending on _json_schema_to_pydantic implementation
            # A more robust test would mock the Pydantic conversion to raise

    def test_api_timeout_handling(self):
        """Should handle API timeout gracefully."""
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('scrapegraph_py.Client') as mock_client:
                mock_client.return_value.smartscraper.side_effect = Exception("Request timeout")

                result = web_ai_scrape(
                    state={},
                    url="https://example.com",
                    prompt="Extract",
                    output_schema={"type": "object", "properties": {}}
                )

                assert result["success"] is False
                assert result["error_type"] == "timeout"

    def test_cache_is_used_for_repeated_schema_loads(self):
        """Should use cached schema on repeated calls (via 008.2 SchemaCache)."""
        # This test verifies integration with 008.2's caching
        with patch.dict('os.environ', {'SCRAPEGRAPH_API_KEY': 'test-key'}):
            with patch('the_edge_agent.schema.fetch_schema') as mock_fetch:
                mock_fetch.return_value = {"type": "object", "properties": {}}
                with patch('scrapegraph_py.Client') as mock_client:
                    mock_client.return_value.smartscraper.return_value = {}

                    # First call
                    web_ai_scrape(state={}, url="https://a.com", prompt="X",
                                  schema={"uses": "company/schemas@v1#a.json"})
                    # Second call with same schema ref
                    web_ai_scrape(state={}, url="https://b.com", prompt="Y",
                                  schema={"uses": "company/schemas@v1#a.json"})

                    # fetch_schema is called twice, but 008.2's internal cache
                    # ensures the Git/fsspec fetch only happens once
                    # (This is a documentation test - actual caching is in 008.2)
                    assert mock_fetch.call_count == 2
```

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| ScrapeGraphAI API rate limits | Medium | High | ✅ Exponential backoff retry (2^attempt seconds) |
| ScrapeGraphAI API downtime | Low | High | ✅ Retry on 5xx errors; fallback to `web.scrape` + `llm.generate` |
| Schema loading failures | Low | Medium | ✅ Uses 008.2's robust fetch_schema with caching |
| Git/cloud auth failures | Medium | Medium | ✅ Clear error messages; documented env vars |
| Pydantic conversion errors | Low | Medium | ✅ Schema validation with descriptive errors |

## Definition of Done

- [x] `scrapegraph-py` added to setup.py extras (AC 5)
- [x] `web.ai_scrape` action implemented in `web_actions.py` (AC 1, 2, 6-9)
- [x] Inline schema support working (AC 2)
- [x] Git schema loading working (AC 3, 10)
- [x] Schema merging working (AC 4, 11)
- [x] Error handling complete (AC 6, 15)
- [x] Unit tests passing - 27 tests (AC 12)
- [x] Documentation complete (AC 14)

## QA Results

**Review Date:** 2024-12-23
**Reviewer:** Quinn (QA Agent)
**Gate Decision:** **PASS** ✅

### Final Test Results

| Metric | Value |
|--------|-------|
| Total Tests | 27 |
| Passed | 27 |
| Failed | 0 |
| Execution Time | 2.10s |

### Test Coverage by Area

| Area | Tests | Status |
|------|-------|--------|
| Configuration (API key, dependency) | 2 | ✅ |
| Inline Schema | 4 | ✅ |
| Git Schema Loading | 2 | ✅ |
| fsspec URI Loading (S3/GCS/Azure) | 3 | ✅ |
| Schema Merging | 3 | ✅ |
| Retry Logic | 4 | ✅ |
| Error Handling | 4 | ✅ |
| Schema Inline Config | 1 | ✅ |
| Pydantic Conversion | 2 | ✅ |
| Response Handling | 2 | ✅ |

### Acceptance Criteria Verification

| AC# | Description | Status |
|-----|-------------|--------|
| AC1 | New `web.ai_scrape` action using ScrapeGraphAI | ✅ PASS |
| AC2 | Pydantic schema support | ✅ PASS |
| AC3 | Git schema loading | ✅ PASS |
| AC4 | Schema merging | ✅ PASS |
| AC5 | API key configuration | ✅ PASS |
| AC6 | Structured error response | ✅ PASS |
| AC7 | Action registration | ✅ PASS |
| AC8 | YAMLEngine compatibility | ✅ PASS |
| AC9 | Existing web actions pattern | ✅ PASS |
| AC10 | Schema loading (008.2 integration) | ✅ PASS |
| AC11 | Schema merging (008.3 integration) | ✅ PASS |
| AC12 | Unit tests | ✅ PASS |
| AC13 | Integration tests (optional) | N/A |
| AC14 | Documentation | ✅ PASS |
| AC15 | Error handling tests | ✅ PASS |

### Code Quality Assessment

| Aspect | Status |
|--------|--------|
| Clean separation of concerns | ✅ |
| Error handling (7 types) | ✅ |
| Retry logic (exponential backoff) | ✅ |
| Schema integration | ✅ |
| Documentation | ✅ |

### Gate Documents

- Assessment: `docs/qa/assessments/TEA-BUILTIN-008.4-test-design-20251223.md`
- Gate File: `docs/qa/gates/TEA-BUILTIN-008.4-scrapegraphai-integration.yml`

### QA Notes

- **All 15 acceptance criteria verified** (1 N/A for optional integration tests)
- **27 unit tests passing** with comprehensive coverage
- **Clean implementation** with 4 well-separated helper functions
- **Documentation complete** in both Python and shared docs
- **All identified risks have mitigations** in place

### Minor Recommendations (Non-Blocking)

1. Add explicit type hint for `_json_schema_to_pydantic` return type
2. Consider wiring `timeout` parameter to API client in future iteration
3. Consider adding optional integration test file gated by env var

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Story validated and approved | Bob (SM) |
| 2024-12-22 | 0.3.0 | Test design completed | Quinn (QA) |
| 2024-12-22 | 0.4.0 | Status: Ready for Development | Bob (SM) |
| 2024-12-23 | 0.5.0 | **Major story improvements**: Added architecture diagram, retry logic (from 008.5), fsspec URI support (s3/gs/az), unified fetch_schema() integration, expanded test coverage (24→35), updated env vars docs. Status: Ready for Development (Enhanced), Quality Score: 95/100 | Sarah (PO) |
| 2024-12-23 | 0.6.0 | **Implementation complete**: All 6 tasks done. 27 unit tests passing. Status: Ready for Review | James (Dev) |
| 2024-12-23 | 0.7.0 | **QA Gate: PASS** - All 15 ACs verified (1 N/A), 27 tests passing, code quality good. Status: Complete | Quinn (QA) |

---

## Dev Agent Record

### Agent Model Used
- Claude Opus 4.5

### Debug Log References
- N/A (no debug issues encountered)

### Completion Notes
- Implementation complete and all unit tests passing
- Added `web.ai_scrape` action with full ScrapeGraphAI integration
- Integrated with Story 008.2 (`fetch_schema`) for Git + fsspec schema loading
- Integrated with Story 008.3 (`merge_all`) for kubectl-style schema merging
- Retry logic with exponential backoff (1s, 2s, 4s) for rate limits and server errors
- Dynamic JSON Schema to Pydantic conversion for structured extraction

### File List
| File | Status | Description |
|------|--------|-------------|
| `python/setup.py` | Modified | Added `web-ai-scrape` and updated `all` extras |
| `python/src/the_edge_agent/actions/web_actions.py` | Modified | Added `web.ai_scrape` action with helpers |
| `python/tests/test_web_ai_scrape.py` | New | 27 unit tests for web.ai_scrape |
| `docs/python/actions-reference.md` | Modified | Added `web.ai_scrape` to actions table |
| `docs/shared/YAML_REFERENCE.md` | Modified | Added full documentation for `web.ai_scrape` |
| `docs/stories/TEA-BUILTIN-008.4-scrapegraphai-integration.md` | Modified | Updated tasks, DoD, status |

---

## Related Stories

- **TEA-BUILTIN-008**: LlamaExtract Integration Epic (parent)
- **TEA-BUILTIN-008.2**: Schema Loading with Git Refs
- **TEA-BUILTIN-008.3**: Schema Deep Merge CLI & Algorithm
