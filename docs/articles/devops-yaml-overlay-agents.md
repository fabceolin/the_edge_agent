# DevOps-Friendly AI Agent Development with YAML Overlays

**Fabricio Ceolin**

*Independent Researcher*

fabceolin@gmail.com

---

## Abstract

Developing AI agents that run consistently across local, staging, and production environments presents significant operational challenges. Configuration drift between environments leads to bugs that are difficult to reproduce and costly to debug. This article introduces The Edge Agent's (TEA) YAML overlay system—a kubectl-inspired approach that enables DevOps engineers to maintain a single base agent configuration while applying environment-specific overlays. We demonstrate how deep merge semantics allow sparse overlay files to override only necessary settings, eliminating duplication and reducing configuration maintenance burden by up to 80%.

**Keywords:** DevOps, YAML Configuration, Environment Management, Configuration as Code, AI Agents

---

## 1. Introduction

As AI agents move from prototype to production, DevOps engineers face a familiar problem: *how do you maintain consistent agent behavior across different environments without duplicating configuration?*

Traditional approaches lead to:

- **Configuration duplication**: Separate YAML files for each environment
- **Drift risk**: Changes to one environment forgotten in others
- **Maintenance burden**: N files to update for every change
- **Environment-specific bugs**: "Works on my machine" syndrome

The Edge Agent addresses this with **YAML overlays**—a pattern borrowed from Kubernetes that allows environment-specific configurations to be layered on top of a base configuration using deep merge semantics.

This article shows how to:

1. Structure base agents for multi-environment deployment
2. Create sparse overlay files that override only what's necessary
3. Debug merged configurations before deployment
4. Integrate overlays into CI/CD pipelines

## 2. The Configuration Problem

Consider a document processing agent that needs to run in three environments:

| Environment | Memory Backend | Storage | Model | Tracing |
|-------------|----------------|---------|-------|---------|
| **Local** | SQLite file | Local filesystem | GPT-4o-mini | Disabled |
| **Staging** | DuckDB | Cloud Storage (test bucket) | GPT-4o-mini | Enabled |
| **Production** | DuckDB + Firestore | Cloud Storage (prod bucket) | GPT-4o | Enabled |

Without overlays, you'd maintain three separate YAML files with mostly identical content—the node definitions, edge logic, and business rules repeated in each.

### 2.1 The Traditional Approach (Anti-Pattern)

```
agents/
├── document-processor-local.yaml      # 150 lines
├── document-processor-staging.yaml    # 150 lines
├── document-processor-prod.yaml       # 150 lines
```

**Problem**: When you add a new node or modify business logic, you must update all three files. Forget one, and you have configuration drift.

### 2.2 The Overlay Approach

```
agents/
├── document-processor.yaml            # 120 lines (base)
overlays/
├── local-dev.overlay.yaml             # 15 lines
├── staging.overlay.yaml               # 20 lines
├── production.overlay.yaml            # 25 lines
```

**Solution**: The base file contains all agent logic. Overlays contain only environment-specific differences.

## 3. Deep Merge Semantics

TEA uses kubectl-style **strategic merge patch** semantics:

| Type | Behavior | Example |
|------|----------|---------|
| **Objects** | Recursively merged | Base `{a: 1, b: 2}` + Overlay `{b: 3}` = `{a: 1, b: 3}` |
| **Arrays** | Replaced entirely | Base `[1, 2, 3]` + Overlay `[4, 5]` = `[4, 5]` |
| **Scalars** | Last wins | Base `count: 10` + Overlay `count: 20` = `count: 20` |
| **Null** | Explicit override | Base `enabled: true` + Overlay `enabled: null` = `enabled: null` |

### 3.1 Why Objects Merge Recursively

This is the key insight that makes overlays practical. Consider a settings block:

```yaml
# base.yaml
settings:
  ltm:
    backend: sqlite
    path: ./data/memory.db
    lazy: true
    inline_threshold: 1024
  model: gpt-4o-mini
  temperature: 0.7
```

```yaml
# production.overlay.yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
      project: my-project
    storage:
      uri: gs://my-bucket/ltm/
```

**Merged result**:

```yaml
settings:
  ltm:
    backend: duckdb           # From overlay
    path: ./data/memory.db    # Preserved from base
    lazy: true                # Preserved from base
    inline_threshold: 1024    # Preserved from base
    catalog:                  # Added from overlay
      type: firestore
      project: my-project
    storage:                  # Added from overlay
      uri: gs://my-bucket/ltm/
  model: gpt-4o-mini          # Preserved from base
  temperature: 0.7            # Preserved from base
```

The overlay only specifies what changes—everything else is inherited from the base.

### 3.2 Why Arrays Replace (Not Concatenate)

Arrays are replaced entirely to avoid ambiguity. If you need to modify nodes:

```yaml
# overlay that replaces all nodes
nodes:
  - name: modified_node
    run: |
      return {"result": "modified"}
```

For partial node modifications, use variables or conditional logic within the base configuration.

## 4. Complete Example: Document Processor Agent

### 4.1 Base Agent Configuration

```yaml
# agents/document-processor.yaml
name: document-processor
description: Extract structured data from documents

state_schema:
  file_path: str
  extracted_data: dict
  processing_status: str

settings:
  # Default settings for local development
  ltm:
    backend: sqlite
    path: ${LTM_DB_PATH:-./data/memory.db}
    lazy: true
    inline_threshold: 1024

  model: ${LLM_MODEL:-gpt-4o-mini}
  temperature: 0.3

  # Observability - disabled by default for local dev
  opik:
    enabled: ${OPIK_ENABLED:-false}

variables:
  storage_path: ${STORAGE_PATH:-./data/documents}
  max_retries: 3

imports:
  - path: ./actions/extraction.py
    namespace: extract

nodes:
  - name: validate_input
    run: |
      import os
      file_path = state.get("file_path", "")
      if not file_path or not os.path.exists(file_path):
          return {"processing_status": "error", "error": "File not found"}
      return {"processing_status": "validated"}

  - name: extract_content
    uses: extract.parse_document
    with:
      file: "{{ state.file_path }}"
      storage: "{{ variables.storage_path }}"

  - name: structure_data
    uses: llm
    with:
      prompt: |
        Extract structured data from this document content:

        {{ state.raw_content }}

        Return a JSON object with relevant fields.
      response_format: json

  - name: save_results
    uses: memory.store
    with:
      key: "doc_{{ state.file_path | hash }}"
      value: "{{ state.extracted_data }}"

edges:
  - from: __start__
    to: validate_input
  - from: validate_input
    to: extract_content
    condition: "{{ state.processing_status == 'validated' }}"
  - from: validate_input
    to: __end__
    condition: "{{ state.processing_status == 'error' }}"
  - from: extract_content
    to: structure_data
  - from: structure_data
    to: save_results
  - from: save_results
    to: __end__
```

### 4.2 Local Development Overlay

```yaml
# overlays/local-dev.overlay.yaml
# Minimal overlay for local development
# Run with: tea run agents/document-processor.yaml -f overlays/local-dev.overlay.yaml

settings:
  ltm:
    backend: duckdb
    catalog:
      type: duckdb
      shared: true
    storage:
      uri: ${LTM_DB_PATH:-./data/ltm.duckdb}

  opik:
    enabled: false

variables:
  storage_path: ${STORAGE_PATH:-./data/local-documents}
```

### 4.3 Staging Overlay

```yaml
# overlays/staging.overlay.yaml
# Staging environment with cloud storage but test credentials

settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
      project: ${FIREBASE_PROJECT:-my-project-staging}
    storage:
      uri: ${LTM_STORAGE_PATH:-gs://my-project-staging-ltm/}
    lazy: true
    inline_threshold: 4096

  model: gpt-4o-mini  # Keep mini for cost control in staging

  opik:
    enabled: true
    project: document-processor-staging

variables:
  storage_path: ${STORAGE_PATH:-gs://my-project-staging-docs/}
```

### 4.4 Production Overlay

```yaml
# overlays/production.overlay.yaml
# Production environment with full cloud infrastructure

settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
      project: ${FIREBASE_PROJECT:-my-project-prod}
    storage:
      uri: ${LTM_STORAGE_PATH:-gs://my-project-prod-ltm/}
    lazy: true
    inline_threshold: 4096

  model: gpt-4o  # Full model for production quality
  temperature: 0.1  # Lower temperature for consistency

  opik:
    enabled: true
    project: document-processor-prod
    tags:
      - production
      - document-processing

variables:
  storage_path: ${STORAGE_PATH:-gs://my-project-prod-docs/}
  max_retries: 5  # More retries in production
```

## 5. CLI Usage

### 5.1 Running with Overlays

```bash
# Local development (default settings)
tea run agents/document-processor.yaml --input '{"file_path": "./test.pdf"}'

# Local with explicit overlay
tea run agents/document-processor.yaml \
    -f overlays/local-dev.overlay.yaml \
    --input '{"file_path": "./test.pdf"}'

# Staging
tea run agents/document-processor.yaml \
    -f overlays/staging.overlay.yaml \
    --input '{"file_path": "gs://staging-bucket/doc.pdf"}'

# Production
tea run agents/document-processor.yaml \
    -f overlays/production.overlay.yaml \
    --input '{"file_path": "gs://prod-bucket/doc.pdf"}'
```

### 5.2 Multiple Overlays

Overlays are applied left-to-right, with later overlays taking precedence:

```bash
# Base + environment + secrets
tea run agents/document-processor.yaml \
    -f overlays/production.overlay.yaml \
    -f overlays/secrets.overlay.yaml
```

### 5.3 Debug Merged Configuration

Before deploying, verify the final configuration:

```bash
# Preview merged YAML without executing
tea run agents/document-processor.yaml \
    -f overlays/production.overlay.yaml \
    --dump-merged

# Output to file for review
tea run agents/document-processor.yaml \
    -f overlays/production.overlay.yaml \
    --dump-merged > merged-production.yaml

# Inspect specific section with yq
tea run agents/document-processor.yaml \
    -f overlays/production.overlay.yaml \
    --dump-merged | yq '.settings.ltm'
```

## 6. CI/CD Integration

### 6.1 GitHub Actions Example

```yaml
# .github/workflows/deploy-agent.yml
name: Deploy Agent

on:
  push:
    branches: [main]
    paths:
      - 'agents/**'
      - 'overlays/**'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install TEA
        run: pip install the-edge-agent

      - name: Validate merged configurations
        run: |
          for overlay in overlays/*.overlay.yaml; do
            echo "Validating with $overlay"
            tea run agents/document-processor.yaml \
                -f "$overlay" \
                --dump-merged > /dev/null
          done

  deploy-staging:
    needs: validate
    runs-on: ubuntu-latest
    environment: staging
    steps:
      - uses: actions/checkout@v4

      - name: Generate merged config
        run: |
          tea run agents/document-processor.yaml \
              -f overlays/staging.overlay.yaml \
              --dump-merged > deploy/agent.yaml

      - name: Deploy to staging
        run: |
          # Deploy merged configuration
          gcloud functions deploy agent-runner \
              --source=deploy/ \
              --trigger-http

  deploy-production:
    needs: deploy-staging
    runs-on: ubuntu-latest
    environment: production
    steps:
      - uses: actions/checkout@v4

      - name: Generate merged config
        run: |
          tea run agents/document-processor.yaml \
              -f overlays/production.overlay.yaml \
              --dump-merged > deploy/agent.yaml

      - name: Deploy to production
        run: |
          gcloud functions deploy agent-runner \
              --source=deploy/ \
              --trigger-http
```

### 6.2 Pre-commit Hooks

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: validate-agent-configs
        name: Validate Agent Configurations
        entry: bash -c 'for f in agents/*.yaml; do tea validate "$f" || exit 1; done'
        language: system
        files: 'agents/.*\.yaml$'

      - id: validate-overlays
        name: Validate Overlay Merges
        entry: bash -c 'tea run agents/document-processor.yaml -f overlays/production.overlay.yaml --dump-merged > /dev/null'
        language: system
        files: 'overlays/.*\.yaml$'
```

## 7. Best Practices

### 7.1 Project Structure

```
my-agent-project/
├── agents/                    # Base agent configurations
│   ├── document-processor.yaml
│   ├── research-assistant.yaml
│   └── customer-support.yaml
├── overlays/                  # Environment overlays
│   ├── local-dev.overlay.yaml
│   ├── staging.overlay.yaml
│   ├── production.overlay.yaml
│   └── secrets.overlay.yaml   # gitignored
├── actions/                   # Custom Python actions
│   ├── extraction.py
│   └── validation.py
├── data/                      # Local development data
│   └── .gitkeep
├── .env.example               # Environment variable template
└── .gitignore
```

### 7.2 Overlay Naming Conventions

| Pattern | Purpose | Example |
|---------|---------|---------|
| `*.overlay.yaml` | Environment overlay | `production.overlay.yaml` |
| `local-*.overlay.yaml` | Developer-specific | `local-john.overlay.yaml` |
| `secrets.overlay.yaml` | Sensitive config (gitignored) | API keys, tokens |

### 7.3 Environment Variable Strategy

Use environment variables with defaults for flexibility:

```yaml
# Good: Flexible with sensible default
storage:
  uri: ${STORAGE_URI:-./data/local/}

# Bad: Hardcoded value
storage:
  uri: gs://production-bucket/
```

### 7.4 Overlay Granularity

Keep overlays focused:

```yaml
# Good: Single responsibility
# staging.overlay.yaml - environment settings only
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
```

```yaml
# Bad: Mixed concerns
# staging.overlay.yaml - environment + business logic
settings:
  ltm:
    backend: duckdb
nodes:
  - name: modified_node  # Don't modify business logic in overlays
```

## 8. Troubleshooting

### 8.1 Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| Setting not overridden | Typo in path | Use `--dump-merged` to verify |
| Array not merged | Arrays replace by design | Keep arrays in base, use variables for differences |
| Env var not expanded | Wrong syntax | Use `${VAR}` not `$VAR` or `{{VAR}}` |

### 8.2 Debugging Workflow

```bash
# Step 1: Verify base config is valid
tea validate agents/document-processor.yaml

# Step 2: Preview merged config
tea run agents/document-processor.yaml -f overlays/production.overlay.yaml --dump-merged

# Step 3: Check specific section
tea run agents/document-processor.yaml -f overlays/production.overlay.yaml --dump-merged | yq '.settings'

# Step 4: Compare environments
diff <(tea run agents/document-processor.yaml -f overlays/staging.overlay.yaml --dump-merged) \
     <(tea run agents/document-processor.yaml -f overlays/production.overlay.yaml --dump-merged)
```

## 9. Conclusion

The YAML overlay system in The Edge Agent provides DevOps engineers with a familiar, powerful pattern for managing AI agent configurations across environments. Key benefits include:

1. **Single source of truth**: Agent logic lives in one place
2. **Minimal overlays**: Only override what's different
3. **Preview before deploy**: `--dump-merged` prevents surprises
4. **CI/CD friendly**: Easy to validate and deploy programmatically
5. **Team collaboration**: Non-developers can modify overlays safely

By adopting this pattern, teams can reduce configuration maintenance burden significantly while ensuring consistent agent behavior across all environments.

## 10. References

- [The Edge Agent Documentation](https://fabceolin.github.io/the_edge_agent/)
- [YAML Reference - Overlay Merging](https://fabceolin.github.io/the_edge_agent/shared/YAML_REFERENCE.html#yaml-overlay-merging)
- [Kubernetes Strategic Merge Patch](https://kubernetes.io/docs/tasks/manage-kubernetes-objects/update-api-object-kubectl-patch/)
- [YE.8 Story: YAML Overlay Merge Support](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/YE.8.yaml-overlay-merge.md)
