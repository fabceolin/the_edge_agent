# Python Actions Reference

This document lists all built-in actions available in the Python implementation of The Edge Agent.

For complete action documentation including parameters and examples, see the [YAML Reference](../shared/YAML_REFERENCE.md#built-in-actions).

## Action Modules

The Python implementation provides 20+ action modules organized by domain:

### Core Actions

| Action | Module | Description |
|--------|--------|-------------|
| `llm.call` | `llm_actions.py` | Call OpenAI-compatible LLM |
| `llm.stream` | `llm_actions.py` | Stream LLM response |
| `llm.tools` | `llm_actions.py` | LLM with tool calling |

#### LLM Provider Configuration

The LLM actions support multiple providers: **OpenAI**, **Azure OpenAI**, **Ollama**, and **LiteLLM**.

**Provider Detection Priority:**
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection:
   - `OLLAMA_API_BASE` → Ollama
   - `AZURE_OPENAI_API_KEY` + `AZURE_OPENAI_ENDPOINT` → Azure OpenAI
3. Default → OpenAI

**Ollama Example:**

```yaml
# Explicit provider parameter
- name: ask_local_llm
  uses: llm.call
  with:
    provider: ollama
    model: llama3.2
    api_base: http://localhost:11434/v1  # optional, this is the default
    messages:
      - role: user
        content: "{{ state.question }}"

# Environment variable fallback (set OLLAMA_API_BASE)
- name: ask_llm
  uses: llm.call
  with:
    model: llama3.2
    messages:
      - role: user
        content: "{{ state.question }}"
```

**Environment Variables:**

| Variable | Provider | Description |
|----------|----------|-------------|
| `OPENAI_API_KEY` | OpenAI | OpenAI API key |
| `AZURE_OPENAI_API_KEY` | Azure | Azure OpenAI API key |
| `AZURE_OPENAI_ENDPOINT` | Azure | Azure endpoint URL |
| `AZURE_OPENAI_DEPLOYMENT` | Azure | Deployment name (optional) |
| `OLLAMA_API_BASE` | Ollama | Ollama API URL (default: `http://localhost:11434/v1`) |

**Ollama Tool Calling:**

Tool calling with Ollama requires models that support it:
- `llama3.1+`
- `mistral-nemo`
- `qwen2.5`

```yaml
- name: llm_with_tools
  uses: llm.tools
  with:
    provider: ollama
    model: mistral-nemo
    messages:
      - role: user
        content: "What's the weather in London?"
    tools:
      - name: get_weather
        description: Get weather for a location
        parameters:
          location:
            type: string
            required: true
```

#### LiteLLM Provider (TEA-LLM-003)

LiteLLM provides access to 100+ LLM providers through a unified interface. Install with:

```bash
pip install the_edge_agent[litellm]
```

**LiteLLM Example:**

```yaml
# Use Anthropic Claude via LiteLLM
- name: ask_claude
  uses: llm.call
  with:
    provider: litellm
    model: anthropic/claude-3-opus-20240229
    messages:
      - role: user
        content: "{{ state.question }}"

# Use Google Gemini via LiteLLM
- name: ask_gemini
  uses: llm.call
  with:
    provider: litellm
    model: gemini/gemini-pro
    messages:
      - role: user
        content: "{{ state.question }}"
```

**LiteLLM Model Format:** `provider/model-name`

| Provider | Model Example |
|----------|---------------|
| Anthropic | `anthropic/claude-3-opus-20240229` |
| AWS Bedrock | `bedrock/anthropic.claude-v2` |
| Google Gemini | `gemini/gemini-pro` |
| Cohere | `cohere/command-r-plus` |
| Mistral | `mistral/mistral-large-latest` |

**LiteLLM Features:**
- Built-in cost tracking (`cost_usd` in response)
- Opik observability integration (`opik_trace=True`)
- Streaming support (`llm.stream`)
- Tool calling support (`llm.tools`)

See [LiteLLM Providers](https://docs.litellm.ai/docs/providers) for complete list.

### HTTP Actions

| Action | Module | Description |
|--------|--------|-------------|
| `http.get` | `http_actions.py` | HTTP GET request |
| `http.post` | `http_actions.py` | HTTP POST request |

### File Actions

| Action | Module | Description |
|--------|--------|-------------|
| `file.read` | `file_actions.py` | Read local/remote file |
| `file.write` | `file_actions.py` | Write local/remote file |

### Storage Actions

| Action | Module | Description |
|--------|--------|-------------|
| `storage.list` | `storage_actions.py` | List files in directory |
| `storage.copy` | `storage_actions.py` | Copy files |
| `storage.delete` | `storage_actions.py` | Delete files |

### Data Actions

| Action | Module | Description |
|--------|--------|-------------|
| `json.parse` | `data_actions.py` | Parse JSON string |
| `json.transform` | `data_actions.py` | Transform JSON with JMESPath |
| `csv.parse` | `data_actions.py` | Parse CSV data |
| `data.validate` | `data_actions.py` | Validate against JSON Schema |

### Code Execution

| Action | Module | Description |
|--------|--------|-------------|
| `code.execute` | `code_actions.py` | Execute Python code (sandboxed) |
| `code.sandbox` | `code_actions.py` | Run in RestrictedPython sandbox |

### Memory Actions

| Action | Module | Description |
|--------|--------|-------------|
| `memory.store` | `memory_actions.py` | Store in session memory |
| `memory.retrieve` | `memory_actions.py` | Retrieve from session memory |
| `ltm.store` | `ltm_actions.py` | Store in long-term memory |
| `ltm.retrieve` | `ltm_actions.py` | Retrieve from long-term memory |

### Vector/RAG Actions

| Action | Module | Description |
|--------|--------|-------------|
| `embedding.create` | `vector_actions.py` | Create text embeddings |
| `vector.store` | `vector_actions.py` | Store in vector database |
| `vector.query` | `vector_actions.py` | Query vector database |

### Web Actions

| Action | Module | Description |
|--------|--------|-------------|
| `web.scrape` | `web_actions.py` | Scrape web page (Firecrawl) |
| `web.crawl` | `web_actions.py` | Crawl multiple pages (Firecrawl) |
| `web.search` | `web_actions.py` | Web search (Perplexity) |
| `web.ai_scrape` | `web_actions.py` | AI-powered structured extraction (ScrapeGraphAI) |

### Academic Actions

| Action | Module | Description |
|--------|--------|-------------|
| `academic.pubmed` | `academic_actions.py` | Search PubMed via NCBI E-utilities |
| `academic.arxiv` | `academic_actions.py` | Search ArXiv preprint server |

#### academic.pubmed

Search the PubMed database for scientific articles using NCBI E-utilities API.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `query` | string | required | Search query (PubMed query syntax) |
| `max_results` | int | 5 | Maximum results to return |
| `sort_by` | string | "relevance" | Sort order: "relevance" or "date" |
| `timeout` | int | 30 | Request timeout in seconds |

**Returns:**

```python
{
    "success": True,
    "results": [
        {
            "pmid": "12345678",
            "title": "Article Title",
            "authors": ["Smith John", "Doe A"],
            "abstract": "Full abstract text...",
            "journal": "Nature Medicine",
            "pub_date": "2024-01-15",
            "doi": "10.1038/xxxxx",
            "url": "https://pubmed.ncbi.nlm.nih.gov/12345678/"
        }
    ],
    "query": "machine learning cancer",
    "total_results": 150,
    "returned_results": 5
}
```

**YAML Example:**

```yaml
name: search-research
nodes:
  - name: find_papers
    uses: academic.pubmed
    with:
      query: "{{ state.research_topic }}"
      max_results: 10
      sort_by: date
    output:
      articles: "{{ result.results }}"

  - name: summarize
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: |
            Summarize these research findings:
            {% for article in state.articles %}
            - {{ article.title }} ({{ article.journal }}, {{ article.pub_date }})
            {% endfor %}
```

**Rate Limiting:** 3 requests/second (10 req/s with `NCBI_API_KEY`)

**Environment Variables:**

| Variable | Required | Description |
|----------|----------|-------------|
| `NCBI_API_KEY` | No | Increases rate limit from 3 to 10 req/s |

#### academic.arxiv

Search the ArXiv preprint server for research papers.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `query` | string | optional | Search query string |
| `arxiv_id` | string | optional | Direct paper lookup by ID |
| `max_results` | int | 5 | Maximum results to return |
| `sort_by` | string | "relevance" | Sort order: "relevance" or "date" |
| `timeout` | int | 30 | Request timeout in seconds |

*Note: Either `query` or `arxiv_id` must be provided.*

**Returns:**

```python
{
    "success": True,
    "results": [
        {
            "arxiv_id": "2301.00001v2",
            "title": "Paper Title",
            "authors": ["Alice Smith", "Bob Jones"],
            "abstract": "Full abstract...",
            "categories": ["cs.CL", "cs.AI"],
            "published": "2023-01-01T00:00:00Z",
            "updated": "2023-01-15T12:00:00Z",
            "pdf_url": "https://arxiv.org/pdf/2301.00001"
        }
    ],
    "query": "transformer neural networks",
    "total_results": 250
}
```

**YAML Examples:**

```yaml
# Search by query
- name: search_arxiv
  uses: academic.arxiv
  with:
    query: "large language models"
    max_results: 5
    sort_by: date
  output:
    papers: "{{ result.results }}"

# Direct lookup by ID
- name: get_paper
  uses: academic.arxiv
  with:
    arxiv_id: "2301.00001"
  output:
    paper: "{{ result.results[0] }}"
```

**Rate Limiting:** 1 request per 3 seconds (per ArXiv terms of service)

**Error Handling:**

Both actions return structured errors:

```python
{
    "success": False,
    "error": "Rate limit exceeded. Please wait and try again.",
    "error_code": "rate_limit"  # empty_query, network, timeout, api_error
}
```

### Text Actions

| Action | Module | Description |
|--------|--------|-------------|
| `text.insert_citations` | `text_actions.py` | Insert citation markers using semantic embedding matching |

#### text.insert_citations

Insert citation markers into text using semantic embedding matching. Uses OpenAI embeddings to compute similarity between sentences and references, placing citations at the most semantically relevant positions.

**Algorithm** (from original Kiroku project):
1. Tokenize text into sentences using NLTK
2. Compute embeddings for sentences and references using OpenAI
3. Calculate similarity matrix via dot product
4. Insert citations at sentences with highest similarity to each reference
5. Reorder references by first occurrence in text

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `text` | string | required | Markdown text to process |
| `references` | list[str] | required | List of reference strings |
| `model` | string | "text-embedding-3-large" | OpenAI embedding model |
| `api_key` | string | None | OpenAI API key (uses OPENAI_API_KEY env var if not provided) |
| `base_url` | string | None | Optional API base URL for compatible endpoints |

**Returns:**

```python
{
    "cited_text": "Text with [1] citation markers inserted.",
    "references_section": "## References\n\n1. Author. Title. 2020.",
    "citation_map": {1: "Author. Title. 2020."},
    "text": "Full text with citations and References section"
}
```

**Features:**
- Semantic matching via embeddings (not just keyword matching)
- Citations placed at most relevant sentences
- Conclusions section excluded from citation (academic convention)
- Abstract section excluded from citation
- References reordered by first occurrence
- Markdown formatting preserved

**YAML Examples:**

```yaml
# Basic citation insertion with semantic matching
name: add-citations
nodes:
  - name: cite_draft
    uses: text.insert_citations
    with:
      text: "{{ state.draft }}"
      references: "{{ state.references }}"
      model: text-embedding-3-large  # or text-embedding-ada-002 for lower cost
    output:
      final_document: "{{ result.text }}"

# Complete academic workflow
name: academic-writer
state_schema:
  topic: str
  draft: str
  references: list
  final_document: str

nodes:
  - name: research
    uses: academic.arxiv
    with:
      query: "{{ state.topic }}"
      max_results: 5
    output:
      papers: "{{ result.results }}"

  - name: format_refs
    run: |
      refs = []
      for p in state['papers']:
        ref = f"{', '.join(p['authors'][:3])}. {p['title']}. ArXiv {p['arxiv_id']}. {p['pdf_url']}"
        refs.append(ref)
      return {"references": refs}

  - name: write_draft
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: |
            Write a research summary on {{ state.topic }}.
            Reference these papers: {{ state.references | tojson }}

  - name: add_citations
    uses: text.insert_citations
    with:
      text: "{{ state.draft }}"
      references: "{{ state.references }}"
    output:
      final_document: "{{ result.text }}"

edges:
  - from: __start__
    to: research
  - from: research
    to: format_refs
  - from: format_refs
    to: write_draft
  - from: write_draft
    to: add_citations
  - from: add_citations
    to: __end__
```

**Input/Output Example:**

```python
# Input
text = """
Machine learning has revolutionized many fields.
The transformer architecture changed NLP forever.
"""

references = [
    "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762"
]

# Output
{
    "cited_text": """
Machine learning has revolutionized many fields.
The transformer architecture changed NLP forever [1].
""",
    "references_section": """## References

1. Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762""",
    "citation_map": {
        1: "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762"
    },
    "text": "... (full combined document)"
}
```

### Graph Actions

| Action | Module | Description |
|--------|--------|-------------|
| `graph.store_entity` | `graph_actions.py` | Store entity in graph DB |
| `graph.query` | `graph_actions.py` | Query graph database |

### Observability

| Action | Module | Description |
|--------|--------|-------------|
| `trace.start` | `observability_actions.py` | Start trace span |
| `trace.log` | `observability_actions.py` | Log trace event |
| `trace.end` | `observability_actions.py` | End trace span |

## Source Location

All action modules are in:

```
python/src/the_edge_agent/actions/
├── __init__.py
├── llm_actions.py
├── http_actions.py
├── file_actions.py
├── storage_actions.py
├── data_actions.py
├── code_actions.py
├── memory_actions.py
├── ltm_actions.py
├── cloud_memory_actions.py
├── vector_actions.py
├── graph_actions.py
├── web_actions.py
├── academic_actions.py
├── text_actions.py
├── tools_actions.py
├── observability_actions.py
└── context_actions.py
```

## Custom Actions

Register custom actions via the `imports:` section in YAML:

```yaml
imports:
  - path: ./my_actions.py
    actions:
      - my_custom_action
```

Your module must implement `register_actions()`:

```python
def register_actions(registry, engine):
    registry['my_custom_action'] = my_action_function
```
