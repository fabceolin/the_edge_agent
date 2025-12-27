# TEA Capabilities

The Edge Agent (TEA) is more than just a YAML agent framework. Here's everything TEA can do:

## Core Capabilities

| Capability | Description | Status |
|------------|-------------|--------|
| [Neurosymbolic AI](neurosymbolic.md) | Combine LLMs with Prolog for logical reasoning | Production |
| [LLM Orchestration](llm-orchestration.md) | 100+ providers via LiteLLM, streaming, tool use | Production |
| [RAG & Memory](rag-memory.md) | Vector search, embeddings, persistent memory | Production |
| [Web Automation](web-automation.md) | Scraping, crawling, AI-powered extraction | Production |
| [Parallel Workflows](parallel-workflows.md) | Fan-out/fan-in, concurrent execution | Production |
| [Human-in-the-Loop](human-in-the-loop.md) | Interrupts, checkpoints, approval workflows | Production |
| [Edge Deployment](edge-deployment.md) | Single binary, offline-first, serverless | Production |
| [Observability](observability.md) | Distributed tracing, Opik integration | Production |

## Quick Links

- [Getting Started (Python)](../python/getting-started.md)
- [Getting Started (Rust)](../rust/getting-started.md)
- [YAML Reference](../shared/YAML_REFERENCE.md)
- [Actions Reference (Python)](../python/actions-reference.md)
- [Actions Reference (Rust)](../rust/actions-reference.md)

## What Makes TEA Different?

| Feature | TEA | Traditional Frameworks |
|---------|-----|------------------------|
| **Symbolic Reasoning** | Prolog integration | None |
| **Deployment** | Single binary | Python + dependencies |
| **Offline Support** | Full offline capability | Requires cloud |
| **Runtime** | Python + Rust | Python only |
