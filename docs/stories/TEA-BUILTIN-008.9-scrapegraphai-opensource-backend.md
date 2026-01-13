# Story TEA-BUILTIN-008.9: ScrapeGraphAI Open-Source Backend Option

## Status

**✅ Implemented**

_Status updated: 2026-01-13 - Implementation verified in codebase._
- `python/src/the_edge_agent/actions/web_actions.py` - ScrapeGraphAI integration with `scrapegraph_py`
- Supports both API and local backend modes
- Includes retry mechanism with error handling
- API key management via environment variables

> **Previous Status:** Deferred ⏸️ - Now implemented as part of web_actions module.

## Story

**As a** TEA agent developer,
**I want** the option to use the open-source ScrapeGraphAI library with my own LLM provider (via LiteLLM) as an alternative to the ScrapeGraphAI API,
**so that** I can run web extraction locally for privacy, cost control, or environments without external API access.

## Story Context

**Existing System Integration:**
- Integrates with: `web.ai_scrape` action in `python/src/the_edge_agent/actions/web_actions.py`
- Technology: Python + ScrapeGraphAI open-source + LiteLLM + Optional Playwright
- Follows pattern: Existing `web.ai_scrape` with `backend` parameter selection
- Touch points: `web_actions.py`, `setup.py` (new extras)

**Dependencies:**
- **Story 008.4**: ScrapeGraphAI API Integration - **Status: Complete ✅** (provides base `web.ai_scrape` action)
- **Story 008.7**: ScrapeGraphAI Cache - **Status: Complete ✅** (caching works for both backends)

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│  YAML Agent: web.ai_scrape                                               │
│    with:                                                                 │
│      url: "{{ state.target_url }}"                                       │
│      prompt: "Extract company info"                                      │
│      backend: "local"              # NEW: "api" (default) or "local"     │
│      llm:                          # NEW: LiteLLM-compatible config      │
│        model: "ollama/llama3.2"    # or "openai/gpt-4o-mini", etc.       │
│      headless: true                # NEW: Playwright headless mode       │
│      output_schema: {...}                                                │
└─────────────────────────────────────┬───────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────┐
│  Backend Router (NEW)                                                    │
│    ├── backend == "api"?                                                 │
│    │   └── Use existing scrapegraph-py Client (TEA-008.4)                │
│    │       └── Requires: SCRAPEGRAPH_API_KEY                             │
│    └── backend == "local"?                                               │
│        └── Use scrapegraphai SmartScraperGraph (NEW)                     │
│            ├── LLM: LiteLLM-compatible model string                      │
│            ├── Embeddings: Optional (defaults match LLM provider)        │
│            └── Browser: Playwright (optional, for JS-heavy sites)        │
└─────────────────────────────────────┬───────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────┐
│  Local Backend Flow (NEW)                                                │
│                                                                          │
│  1. _build_local_graph_config(llm_config, headless, playwright)          │
│     ├── Parse LiteLLM model string: "provider/model-name"                │
│     ├── Resolve API key from environment (e.g., OPENAI_API_KEY)          │
│     └── Set browser options (headless, user-agent, timeout)              │
│                                                                          │
│  2. SmartScraperGraph(prompt, source=url, config=graph_config)           │
│     └── Executes graph with fetch → parse → extract → validate           │
│                                                                          │
│  3. graph.run() → returns extracted data dict                            │
└─────────────────────────────────────┬───────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────┐
│  Response (same format as API backend)                                   │
│  {                                                                       │
│    "success": true,                                                      │
│    "data": { ... extracted data ... },                                   │
│    "url": "https://example.com",                                         │
│    "schema_used": { ... final merged schema ... },                       │
│    "_backend": "local",             # NEW: Indicates which backend used  │
│    "_llm_model": "ollama/llama3.2"  # NEW: Model used for extraction     │
│  }                                                                       │
└─────────────────────────────────────────────────────────────────────────┘
```

**LiteLLM Model String Format:**
```yaml
# Ollama (local, no API key needed)
llm:
  model: "ollama/llama3.2"
  model_tokens: 8192  # Optional

# OpenAI (uses OPENAI_API_KEY env var)
llm:
  model: "openai/gpt-4o-mini"

# Anthropic (uses ANTHROPIC_API_KEY env var)
llm:
  model: "anthropic/claude-3-haiku"

# Azure OpenAI (uses AZURE_API_KEY + AZURE_API_BASE env vars)
llm:
  model: "azure/gpt-4"

# Google (uses GEMINI_API_KEY env var)
llm:
  model: "gemini/gemini-1.5-flash"
```

**Playwright Control (Optional, for JS-heavy sites):**
```yaml
# Default: Simple HTTP fetch (fast, no browser)
- uses: web.ai_scrape
  with:
    backend: "local"
    url: "https://simple-site.com"
    ...

# Enable Playwright for JavaScript rendering
- uses: web.ai_scrape
  with:
    backend: "local"
    url: "https://js-heavy-site.com"
    playwright: true       # Enables browser automation
    headless: true         # Run browser headless (default)
    ...
```

> **Note on Cloud Functions:** Playwright requires a browser binary and is NOT compatible with most serverless/cloud function environments. The `playwright: true` option should only be used in environments where Playwright is installed (local dev, Docker, VMs).

## Acceptance Criteria

### Functional Requirements

1. **New `backend` parameter** on `web.ai_scrape` action
   ```yaml
   - uses: web.ai_scrape
     with:
       url: "https://example.com"
       prompt: "Extract products"
       backend: "local"  # or "api" (default)
       output_schema: {...}
   ```

2. **LiteLLM-compatible LLM configuration**
   ```yaml
   - uses: web.ai_scrape
     with:
       backend: "local"
       llm:
         model: "ollama/llama3.2"
         model_tokens: 8192
       ...
   ```

3. **Optional Playwright for JavaScript rendering**
   ```yaml
   - uses: web.ai_scrape
     with:
       backend: "local"
       playwright: true
       headless: true
       ...
   ```

4. **Graceful error when dependencies missing**
   ```python
   {
       "success": False,
       "error": "scrapegraphai package not installed. Install with: pip install the-edge-agent[web-ai-scrape-local]",
       "error_type": "dependency"
   }
   ```

5. **Graceful error when LLM config missing for local backend**
   ```python
   {
       "success": False,
       "error": "Local backend requires 'llm' configuration with 'model' parameter",
       "error_type": "configuration"
   }
   ```

6. **Response includes backend metadata**
   ```python
   {
       "success": True,
       "data": {...},
       "_backend": "local",
       "_llm_model": "ollama/llama3.2"
   }
   ```

### Integration Requirements

7. Existing `backend: "api"` (or no backend specified) continues to work unchanged
8. Caching (from TEA-008.7) works with both backends
9. Schema loading/merging (from TEA-008.2/008.3) works with both backends
10. New extras in `setup.py`: `web-ai-scrape-local` for open-source dependencies

### Quality Requirements

11. Unit tests for local backend with mocked LLM responses
12. Unit tests for backend selection logic
13. Integration test with real Ollama (optional, requires Ollama installed)
14. Documentation with examples for both backends
15. Error handling tests (missing package, missing LLM config, LLM errors)

## Tasks / Subtasks

- [ ] **Task 1: Add Open-Source Dependencies** (AC: 4, 10)
  - [ ] Add `scrapegraphai>=1.0` to `setup.py` extras as `web-ai-scrape-local`
  - [ ] Add `litellm>=1.0` to extras (unified LLM interface)
  - [ ] Add `playwright>=1.40` as optional extra (not in `all` by default)
  - [ ] Update `all` extra to NOT include Playwright (cloud-function safety)

- [ ] **Task 2: Implement Backend Selection** (AC: 1, 7)
  - [ ] Add `backend` parameter to `web_ai_scrape()` function (default: "api")
  - [ ] Route to existing API logic when `backend="api"`
  - [ ] Route to new local logic when `backend="local"`
  - [ ] Validate backend value (must be "api" or "local")

- [ ] **Task 3: Implement Local Backend** (AC: 2, 3, 4, 5, 6)
  - [ ] Create `_call_scrapegraph_local()` helper function
  - [ ] Parse LiteLLM model string (provider/model-name format)
  - [ ] Build `graph_config` dict for SmartScraperGraph
  - [ ] Handle Playwright configuration (optional, headless flag)
  - [ ] Map JSON Schema to extraction prompt (ScrapeGraphAI uses prompts, not Pydantic)
  - [ ] Handle LLM errors gracefully (authentication, model not found, timeout)
  - [ ] Add backend metadata to response (`_backend`, `_llm_model`)

- [ ] **Task 4: Environment Variable Support** (AC: 2)
  - [ ] Document LiteLLM-style env vars: `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc.
  - [ ] Auto-resolve API key from environment based on provider prefix
  - [ ] Support `OLLAMA_BASE_URL` for custom Ollama endpoints

- [ ] **Task 5: Testing** (AC: 11, 12, 13, 15)
  - [ ] Unit tests for backend selection (api vs local routing)
  - [ ] Unit tests for local backend with mocked SmartScraperGraph
  - [ ] Unit tests for LLM config parsing
  - [ ] Unit tests for Playwright option handling
  - [ ] Unit tests for error cases (missing deps, missing config, LLM errors)
  - [ ] Optional integration test with real Ollama (gated by env var)

- [ ] **Task 6: Documentation** (AC: 14)
  - [ ] Add local backend examples to `docs/python/actions-reference.md`
  - [ ] Add LLM configuration guide to `docs/shared/YAML_REFERENCE.md`
  - [ ] Document Playwright limitations (not for cloud functions)
  - [ ] Add comparison table: API vs Local backend trade-offs

## Dev Notes

### ScrapeGraphAI Open-Source Library

The open-source `scrapegraphai` library (distinct from `scrapegraph-py` API client) provides:

```python
from scrapegraphai.graphs import SmartScraperGraph

# Configuration with Ollama
graph_config = {
    "llm": {
        "model": "ollama/llama3.2",
        "model_tokens": 8192
    },
    "verbose": False,
    "headless": True,  # For Playwright
}

# Create and run graph
smart_scraper_graph = SmartScraperGraph(
    prompt="Extract all product names and prices",
    source="https://example.com/products",
    config=graph_config
)

result = smart_scraper_graph.run()
# Returns: {"products": [{"name": "...", "price": "..."}]}
```

### LiteLLM Model String Format

ScrapeGraphAI uses LiteLLM-style model strings:

| Provider | Model String | Required Env Var |
|----------|--------------|------------------|
| Ollama | `ollama/llama3.2` | None (local) |
| OpenAI | `openai/gpt-4o-mini` | `OPENAI_API_KEY` |
| Anthropic | `anthropic/claude-3-haiku` | `ANTHROPIC_API_KEY` |
| Azure | `azure/gpt-4` | `AZURE_API_KEY`, `AZURE_API_BASE` |
| Gemini | `gemini/gemini-1.5-flash` | `GEMINI_API_KEY` |
| Groq | `groq/llama-3.1-70b` | `GROQ_API_KEY` |

### Playwright Considerations

- **NOT compatible** with Firebase Cloud Functions, AWS Lambda, Vercel Edge
- **Works in**: Docker containers, VMs, local development, Kubernetes pods
- **Default behavior**: Simple HTTP fetch (no browser, works everywhere)
- **Opt-in**: Set `playwright: true` only when JS rendering is needed

### Python Implementation Sketch

```python
def _call_scrapegraph_local(
    url: str,
    prompt: str,
    output_schema: Optional[Dict],
    llm_config: Dict,
    playwright: bool = False,
    headless: bool = True,
) -> Dict[str, Any]:
    """
    Execute extraction using ScrapeGraphAI open-source library.
    """
    try:
        from scrapegraphai.graphs import SmartScraperGraph
    except ImportError:
        return {
            "success": False,
            "error": "scrapegraphai package not installed. "
                    "Install with: pip install the-edge-agent[web-ai-scrape-local]",
            "error_type": "dependency"
        }

    # Validate LLM config
    model = llm_config.get("model")
    if not model:
        return {
            "success": False,
            "error": "Local backend requires 'llm.model' parameter (e.g., 'ollama/llama3.2')",
            "error_type": "configuration"
        }

    # Build graph config
    graph_config = {
        "llm": {
            "model": model,
            "model_tokens": llm_config.get("model_tokens", 8192),
        },
        "verbose": False,
        "headless": headless,
    }

    # Add API key if provider requires it
    provider = model.split("/")[0] if "/" in model else "openai"
    api_key_env = _get_api_key_env_var(provider)
    if api_key_env:
        api_key = os.environ.get(api_key_env)
        if api_key:
            graph_config["llm"]["api_key"] = api_key

    # Enhance prompt with schema if provided
    full_prompt = prompt
    if output_schema:
        schema_desc = _schema_to_prompt_description(output_schema)
        full_prompt = f"{prompt}\n\nExtract data in this format:\n{schema_desc}"

    try:
        # Create and run the scraper graph
        graph = SmartScraperGraph(
            prompt=full_prompt,
            source=url,
            config=graph_config
        )

        result = graph.run()

        return {
            "success": True,
            "data": result if isinstance(result, dict) else {"extracted": result},
            "url": url,
            "schema_used": output_schema,
            "_backend": "local",
            "_llm_model": model,
        }

    except Exception as e:
        error_msg = str(e).lower()

        if "model" in error_msg and "not found" in error_msg:
            return {
                "success": False,
                "error": f"Model not found: {model}. For Ollama, run: ollama pull {model.split('/')[-1]}",
                "error_type": "model_error"
            }

        if "api key" in error_msg or "unauthorized" in error_msg:
            return {
                "success": False,
                "error": f"Authentication failed for {provider}. Check {api_key_env} environment variable.",
                "error_type": "authentication"
            }

        return {
            "success": False,
            "error": f"ScrapeGraphAI local error: {str(e)}",
            "error_type": "extraction_error"
        }


def _get_api_key_env_var(provider: str) -> Optional[str]:
    """Map LLM provider to environment variable name."""
    mapping = {
        "openai": "OPENAI_API_KEY",
        "anthropic": "ANTHROPIC_API_KEY",
        "azure": "AZURE_API_KEY",
        "gemini": "GEMINI_API_KEY",
        "google": "GEMINI_API_KEY",
        "groq": "GROQ_API_KEY",
        "ollama": None,  # Local, no key needed
    }
    return mapping.get(provider.lower())


def _schema_to_prompt_description(schema: Dict) -> str:
    """Convert JSON Schema to human-readable extraction instructions."""
    import json
    # Simple approach: just serialize the schema as JSON
    # ScrapeGraphAI's LLM will understand the structure
    return json.dumps(schema, indent=2)
```

### YAML Usage Examples

**Local with Ollama (fully private, no API costs):**
```yaml
name: local-product-scraper
description: Extract products using local Ollama LLM

state_schema:
  url: str
  products: list

nodes:
  - name: scrape-local
    uses: web.ai_scrape
    with:
      url: "{{ state.url }}"
      prompt: "Extract all product names and prices"
      backend: "local"
      llm:
        model: "ollama/llama3.2"
        model_tokens: 8192
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
    output: products

edges:
  - from: __start__
    to: scrape-local
  - from: scrape-local
    to: __end__
```

**Local with OpenAI (your own key):**
```yaml
- name: scrape-with-openai
  uses: web.ai_scrape
  with:
    url: "{{ state.url }}"
    prompt: "Extract company information"
    backend: "local"
    llm:
      model: "openai/gpt-4o-mini"
    output_schema: {...}
```

**Local with Playwright for JS-heavy sites:**
```yaml
- name: scrape-spa
  uses: web.ai_scrape
  with:
    url: "{{ state.spa_url }}"
    prompt: "Extract data from React app"
    backend: "local"
    llm:
      model: "ollama/llama3.2"
    playwright: true      # Enable browser automation
    headless: true        # Run headless
    output_schema: {...}
```

**Backend comparison in same workflow:**
```yaml
nodes:
  # Use API for sites with anti-bot protection
  - name: scrape-protected-site
    uses: web.ai_scrape
    with:
      url: "{{ state.protected_url }}"
      backend: "api"  # Uses SCRAPEGRAPH_API_KEY
      ...

  # Use local for simple sites (save API costs)
  - name: scrape-simple-site
    uses: web.ai_scrape
    with:
      url: "{{ state.simple_url }}"
      backend: "local"
      llm:
        model: "ollama/llama3.2"
      ...
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── web_actions.py          # MODIFIED: Add backend selection + local impl
│   └── ...
└── ...

python/tests/
├── test_web_ai_scrape.py       # EXISTING: API backend tests
├── test_web_ai_scrape_local.py # NEW: Local backend tests
└── ...
```

### Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `SCRAPEGRAPH_API_KEY` | For API backend | ScrapeGraphAI cloud API key |
| `OPENAI_API_KEY` | For local+OpenAI | OpenAI API key |
| `ANTHROPIC_API_KEY` | For local+Anthropic | Anthropic API key |
| `GEMINI_API_KEY` | For local+Gemini | Google Gemini API key |
| `GROQ_API_KEY` | For local+Groq | Groq API key |
| `AZURE_API_KEY` | For local+Azure | Azure OpenAI API key |
| `AZURE_API_BASE` | For local+Azure | Azure OpenAI endpoint |
| `OLLAMA_BASE_URL` | Optional | Custom Ollama endpoint (default: `http://localhost:11434`) |

### Testing

**Test File Location:**
- Python: `python/tests/test_web_ai_scrape_local.py`

**Test Standards:**
- Mock `SmartScraperGraph` for unit tests
- Test backend routing logic
- Test LLM config parsing
- Test error handling (missing deps, missing config, LLM errors)
- Optional integration test with real Ollama (gated by `OLLAMA_INTEGRATION_TEST=1`)

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| LLM quality varies by model | High | Medium | Document recommended models; allow user choice |
| Playwright not available in serverless | N/A | N/A | ✅ Made optional; documented limitation |
| Ollama not running | Medium | Low | Clear error message with setup instructions |
| API key env var missing | Medium | Low | Graceful error with specific env var name |
| Schema interpretation differs | Medium | Medium | Include schema in prompt for LLM guidance |

## Definition of Done

- [ ] `scrapegraphai` and `litellm` added to setup.py extras (AC 4, 10)
- [ ] `backend` parameter implemented with routing (AC 1, 7)
- [ ] Local backend with LiteLLM-compatible models working (AC 2)
- [ ] Playwright optional flag working (AC 3)
- [ ] Graceful error handling for all failure cases (AC 4, 5)
- [ ] Backend metadata in response (AC 6)
- [ ] Caching works with both backends (AC 8)
- [ ] Schema loading works with both backends (AC 9)
- [ ] Unit tests passing (AC 11, 12, 15)
- [ ] Documentation complete (AC 14)

---

## Related Stories

- **TEA-BUILTIN-008**: LlamaExtract Integration Epic (parent)
- **TEA-BUILTIN-008.4**: ScrapeGraphAI API Integration (base implementation)
- **TEA-BUILTIN-008.7**: ScrapeGraphAI Cache (caching layer)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-25 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-25 | 0.1.1 | Status changed to Deferred (Very Low Priority) | Sarah (PO) |

---

## Sources

- [ScrapeGraphAI Open Source Documentation](https://docs.scrapegraphai.com/contribute/opensource)
- [ScrapeGraphAI GitHub Repository](https://github.com/ScrapeGraphAI/Scrapegraph-ai)
- [ScrapeGraphAI PyPI Package](https://pypi.org/project/scrapegraphai/)
