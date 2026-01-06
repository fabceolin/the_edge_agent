# Web Automation

> Extract structured data from websites using AI-powered scraping, multi-page crawling, and intelligent web search.

## Why This Matters

Modern web data extraction requires more than simple HTML parsing. JavaScript-rendered content, anti-bot measures, and unstructured layouts make traditional scrapers ineffective. TEA's web automation capabilities delegate to specialized AI-powered services that handle the complexity for you, returning clean, structured data ready for your agent workflows.

Whether you need to monitor competitor pricing, aggregate research content, or build data pipelines from web sources, TEA provides the actions to do it declaratively in YAML.

## Quick Example

```yaml
name: product-monitor
description: Extract product data from any e-commerce site

state_schema:
  target_url: str
  products: list

nodes:
  - name: scrape-products
    uses: web.ai_scrape
    with:
      url: "{{ state.target_url }}"
      prompt: "Extract all products with names, prices, and availability"
      output_schema:
        type: object
        properties:
          products:
            type: array
            items:
              type: object
              properties:
                name: { type: string }
                price: { type: string }
                in_stock: { type: boolean }
    output: products

edges:
  - from: __start__
    to: scrape-products
  - from: scrape-products
    to: __end__
```

## Key Features

| Feature | Description |
|---------|-------------|
| AI-Powered Extraction | LLMs understand page structure, not just DOM selectors |
| JavaScript Rendering | Handle dynamic SPAs and client-side rendered content |
| Schema-Driven Output | Define exactly what data you need with JSON Schema |
| Multi-Page Crawling | Recursively crawl sites with path filters and depth limits |
| Web Search | Find relevant pages before scraping with AI-powered search |
| No Browser Dependencies | Cloud APIs mean no Playwright/Selenium to configure |

## Tools

TEA integrates with multiple web automation services:

| Tool | Actions | Best For | API Key Required |
|------|---------|----------|------------------|
| **Firecrawl** | `web.scrape`, `web.crawl` | LLM-ready markdown, multi-page crawling | `FIRECRAWL_API_KEY` |
| **ScrapeGraphAI** | `web.ai_scrape` | AI-powered structured extraction with Pydantic schemas | `SCRAPEGRAPH_API_KEY` |
| **Perplexity** | `web.search` | AI-powered web search with citations | `PERPLEXITY_API_KEY` |
| **LlamaExtract** | `llamaextract.extract` | Document extraction (PDFs, images, invoices) | `LLAMAEXTRACT_API_KEY` |

## Available Actions

### Web Scraping (`web.*`)

| Action | Description |
|--------|-------------|
| `web.scrape` | Extract LLM-ready markdown from a URL via Firecrawl |
| `web.crawl` | Recursively crawl websites with path filters and depth limits |
| `web.search` | Perform AI-powered web search via Perplexity |
| `web.ai_scrape` | Extract structured data using ScrapeGraphAI with Pydantic schemas |

### Structured Extraction (`llamaextract.*`)

| Action | Description |
|--------|-------------|
| `llamaextract.extract` | Extract structured data from documents (PDF, images) |
| `llamaextract.upload_agent` | Create/update extraction agent with schema |
| `llamaextract.list_agents` | List available extraction agents |
| `llamaextract.get_agent` | Get agent details and schema |
| `llamaextract.delete_agent` | Delete an extraction agent |

[Full Actions Reference](../python/actions-reference.md)

## Examples

- [Deep Research Crawler](https://github.com/fabceolin/the_edge_agent/blob/main/examples/web/deep-research-crawler.yaml) - Multi-stage law firm data extraction with sitemap analysis
- [ScrapeGraph Simple Test](../../examples/web/scrapegraph-simple-test.yaml) - Quick test for AI-powered extraction
- [ScrapeGraph Production Test](../../examples/web/test-scrapegraph-production.yaml) - Production validation example

## Use Cases

### Research Automation

```yaml
# Search for relevant pages, then scrape them
- name: find-sources
  uses: web.search
  with:
    query: "{{ state.topic }} latest research 2025"
    num_results: 10
  output: sources

- name: scrape-articles
  uses: web.scrape
  with:
    url: "{{ state.sources.results[0].url }}"
    formats: ["markdown", "links"]
  output: article_content
```

### Structured Data Extraction

```yaml
# Extract product data with schema validation
- name: extract-products
  uses: web.ai_scrape
  with:
    url: "{{ state.product_page }}"
    prompt: "Extract all product information"
    schema:
      uses: company/schemas@v1.0.0#products.json  # Git ref to schema
  output: products
```

### Multi-Page Crawling

```yaml
# Crawl documentation site
- name: crawl-docs
  uses: web.crawl
  with:
    url: "https://docs.example.com"
    max_depth: 3
    limit: 50
    include_paths: ["/api/*", "/guides/*"]
    exclude_paths: ["/blog/*", "/changelog/*"]
  output: documentation
```

## Configuration

### Environment Variables

```bash
# Firecrawl (scraping and crawling)
export FIRECRAWL_API_KEY="fc-your-key"

# ScrapeGraphAI (AI extraction)
export SCRAPEGRAPH_API_KEY="sgai-your-key"

# Perplexity (web search)
export PERPLEXITY_API_KEY="pplx-your-key"

# LlamaExtract (document extraction)
export LLAMAEXTRACT_API_KEY="llx-your-key"
```

### Schema Loading (Advanced)

Web actions support loading extraction schemas from multiple sources:

```yaml
# Git repository reference
schema:
  uses: company/schemas@v1.0.0#extraction/product.json

# Cloud storage (S3, GCS, Azure)
schema:
  uses: s3://bucket-name/schemas/invoice.json

# Multiple schemas merged (kubectl-style, last wins)
schema:
  uses:
    - base/schemas@v1#common/base.json
    - s3://company-schemas/overlay.json
    - company/private@main#overrides.json
```

## Error Handling

All web actions return consistent error responses:

```python
{
    "success": False,
    "error": "Rate limit exceeded",
    "error_type": "rate_limit"  # configuration, rate_limit, timeout, api_error
}
```

Actions automatically retry on:
- Rate limits (HTTP 429) with exponential backoff
- Server errors (HTTP 5xx) with retry

## Learn More

- [Web Actions Story](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-002.1.web-actions.md) - Technical implementation details
- [ScrapeGraphAI Integration](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-008.4-scrapegraphai-integration.md) - AI-powered extraction
- [LlamaExtract Actions](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-BUILTIN-008.1-llamaextract-actions.md) - Document extraction
- [YAML Reference](../shared/YAML_REFERENCE.md) - Full action documentation

---

*Part of the [TEA Capabilities](./README.md) documentation.*
