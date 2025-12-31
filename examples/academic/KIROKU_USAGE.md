# Kiroku Document Writer - Usage Guide

Kiroku is an AI-powered academic paper generation workflow with human-in-the-loop review.

## Quick Start

```bash
# From the python directory
cd python
source .venv/bin/activate

# Run with interactive mode (pauses at interrupt points for review)
python -m the_edge_agent.cli run \
  ../examples/academic/kiroku-document-writer.yaml \
  --input @../examples/academic/sample-paper-spec.yaml

# Run without interrupts (for testing - runs straight through)
python -m the_edge_agent.cli run \
  ../examples/academic/kiroku-document-writer.yaml \
  --input @../examples/academic/sample-paper-spec.yaml \
  --auto-continue
```

## Required Environment Variables

### LLM Provider (choose one)

**Azure OpenAI:**
```bash
export AZURE_OPENAI_API_KEY="your-key"
export AZURE_OPENAI_ENDPOINT="https://your-endpoint.openai.azure.com"
export AZURE_OPENAI_DEPLOYMENT="gpt-4"
```

**OpenAI:**
```bash
export OPENAI_API_KEY="sk-..."
```

### Web Search

**Perplexity:**
```bash
export PERPLEXITY_API_KEY="pplx-..."
```

## Input Options

The `--input` flag accepts:
- `@file.yaml` - Load initial state from YAML file
- `@file.json` - Load initial state from JSON file
- `'{"key": "value"}'` - Inline JSON string

## Sample Specs

| File | Description |
|------|-------------|
| `examples/academic/sample-paper-spec.yaml` | Edge Computing for ML (with detailed comments) |
| `examples/academic/test-paper-spec.yaml` | AI-Assisted Academic Writing (minimal) |

## Workflow Features

- AI-assisted title suggestion
- Web research for content
- Topic sentence planning with manual review
- Draft generation with revision cycles
- Reflection and critique
- Abstract and citation generation
- Figure caption formatting

## Interrupt Points

The workflow pauses at 5 points for human review:
1. `suggest_title_review` - Review suggested title
2. `topic_sentence_manual_review` - Review topic sentences
3. `writer_manual_reviewer` - Review draft
4. `reflection_manual_review` - Review critique
5. `generate_citations` - Review citations

Use `--auto-continue` to bypass these for automated testing.

## CLI Options

```
--input, -i          Initial state as JSON string or @file.yaml/@file.json
--auto-continue      Skip interactive prompts at interrupts
--verbose, -v        Increase verbosity (-v, -vv, -vvv)
--stream, -s         Output events as NDJSON stream
--checkpoint-dir     Directory for checkpoint files (for resume)
--interactive, -I    Enable interactive human-in-the-loop mode
```

## Creating Custom Paper Specs

See `examples/academic/sample-paper-spec.yaml` for a fully commented template.

Required fields:
- `title` - Working title
- `hypothesis` - Main claim or research question
- `area_of_paper` - Academic field/domain
- `section_names` - List of section titles
- `number_of_paragraphs` - Paragraph count per section

## Documentation

- [Migration Guide](docs/examples/langraph-to-tea-migration.md) - Migrating from LangGraph
- [YAML Reference](docs/shared/YAML_REFERENCE.md) - Full YAML syntax
- [Actions Reference](docs/python/actions-reference.md) - Built-in actions
