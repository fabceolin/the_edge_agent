# Comprehensive Comparison of LLM Agent Frameworks (2025)

**Research Date:** December 2025
**Scope:** LangGraph/LangChain, AutoGen, CrewAI, Semantic Kernel, Haystack, DSPy

---

## Executive Summary

This research compares six major LLM agent frameworks across twelve critical dimensions. Key findings:

- **LangGraph** leads in state management, human-in-the-loop workflows, and observability
- **CrewAI** excels in multi-agent collaboration and ease of use with 700+ tool integrations
- **Haystack** dominates RAG and document-heavy applications with built-in evaluation
- **AutoGen** (via Microsoft Agent Framework) offers cross-language support and enterprise-grade reliability
- **Semantic Kernel** focuses on composable plugins and enterprise .NET/Python integration
- **DSPy** differentiates on modular AI programming and systematic optimization

**For The Edge Agent (tea):**
- Strengthen YAML DSL with tool definitions similar to CrewAI/Semantic Kernel
- Implement memory management patterns from CrewAI (short/long-term, entity-based)
- Add streaming capabilities and observability hooks like LangGraph
- Consider Model Context Protocol (MCP) for extensible tools

---

## Detailed Capability Comparison Matrix

### 1. Built-in LLM Capabilities

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Streaming** | Full (messages, updates, values, custom) | Partial | Basic | Partial | Via Adapters | Via Adapters |
| **Retry Logic** | Per-node retry policies w/ exponential backoff | Built-in | Basic | Basic | Via retries module | Via adapters |
| **Multi-Model Support** | Any LangChain provider | Any provider via config | 10+ model types (HF, OpenAI, Cohere, Mistral, Ollama) | Any OpenAI-compatible | 7+ provider support | LLM-agnostic |
| **Rate Limiting** | Via LangSmith | Via config | Via integration | Built-in handling | Built-in handling | Via adapter |
| **Token Counting** | Built-in for major models | Via config | Included | Built-in | Per-model support | Per-model support |
| **Context Window Optimization** | Built-in compression | Managed per agent | Document chunking | Dynamic management | Via plugins | Via signature design |

**Key Differentiator:** LangGraph provides the most granular streaming control; DSPy excels at systematic prompt optimization.

---

### 2. Tool/Function Calling Support

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Built-in Tools** | Via LangChain ecosystem | 100+ tools (search, files, API, code, analysis) | Component-based (Loader, Retriever, Embedder, Generator, Evaluator) | Extensions framework | Plugin ecosystem | Retriever abstraction |
| **Tool Categories** | ✓ Web search (SerpAPI, DuckDuckGo) | ✓ Web search (SerperDev, Exa, YouTube, GitHub, Firecrawl) | ✓ Web scrapers, PDF processing | ✓ Function calling via OpenAI API paradigm | ✓ Native plugins + OpenAPI specs | ✓ Dspy.Retrieve for RAG |
| **Tool Categories** | ✓ File operations | ✓ File ops (DirectoryRead, FileRead, PDF, DOCX, JSON, CSV, XML) | ✓ Vector DB connectors | ✓ Custom tool integration | ✓ HTTP + MCP servers | ✓ Tool composition |
| **Tool Categories** | ✓ API integration | ✓ Data analysis, vision (DALL-E, CodeInterpreter) | ✓ Code execution | ✓ Tool registries | ✓ Memory interfaces | ✓ Composable modules |
| **Tool Categories** | ✓ Code execution | ✓ Integrations (Composio, LlamaIndex, Apify, Browserbase) | | | |
| **Dynamic Tool Registration** | Yes | Yes (Agent.tools list) | Yes (component composition) | Yes (dynamic extensions) | Yes (kernel.add_plugin) | Limited |
| **Tool Sharing Mechanism** | LangChain tools + custom | CrewAI toolkit + custom | Components reusable across pipelines | Extensions API + community registry | OpenAPI/MCP/native code | Manual integration |
| **Enterprise Tool Packages** | Via LangSmith Hub | CrewAI AOP Tools Repository (700+) | Modular components | Built-in extensions | Via SDK partners | N/A |

**Key Differentiator:** CrewAI offers the most extensive built-in tool library (700+ integrations); Semantic Kernel emphasizes standards-based plugins (OpenAPI, MCP).

---

### 3. Memory & Conversation Management

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Short-Term Memory** | Via state dict | ChromaDB with RAG | Implicit in state | Message history | Memory interfaces | Context management |
| **Long-Term Memory** | Via checkpointers (file, DB) | SQLite3 per-session storage | Document retrieval | Persistent storage | Pluggable storage | Manual state tracking |
| **Entity Memory** | Custom via state | Entity tracking with RAG | N/A | N/A | Custom via plugins | N/A |
| **Contextual Memory** | Full state propagation | Combined ST+LT+Entity memory | Query-document context | Conversation context | Custom composable | Signature-based context |
| **Memory Persistence** | Checkpoint files (.pkl) | Automatic SQLite3 + ChromaDB | Vector store backed | Message persistence | Configurable backends | Implicit in data structures |
| **Session Management** | Resume from checkpoint | Persistent across runs | Document-centric | In-memory + persistence | Custom via memory interface | Manual state tracking |
| **Embedding Providers** | Via LangChain | 8+ (OpenAI, Ollama, Google, Azure, Cohere, VoyageAI) | 10+ (HF, OpenAI, Cohere, etc.) | Configurable | Azure integration built-in | Custom integration |

**Key Differentiator:** CrewAI has the most mature multi-tier memory system with entity tracking; LangGraph emphasizes state-centric persistence.

---

### 4. RAG/Vector Store Integration

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Vector Stores** | 20+ via LangChain (Chroma, Pinecone, Weaviate, Qdrant, FAISS, Milvus) | ChromaDB (built-in) | 10+ (FAISS, Elasticsearch, Weaviate, Qdrant, Milvus) | Configurable | Azure AI Search, custom | External (Chroma, FAISS) |
| **Retrieval Methods** | Similarity, MMR, reranking | RAG-integrated via memory | Hybrid (keyword + semantic), ranking algorithms | Custom | Similarity + custom | Top-K similarity |
| **Document Processing** | Via LangChain loaders | Via File tools | Native loaders (PDF, Web, Markdown) | Via tools | Via plugins | Manual chunking |
| **Evaluation Framework** | Via LangSmith | Via task execution traces | RAGAS integration built-in | N/A | N/A | N/A |
| **Production Scaling** | Via LangSmith cloud | Via file/DB backends | Kubernetes-ready, cloud-agnostic | Local + distributed | Azure deployment | Local only |

**Key Differentiator:** Haystack leads in RAG production maturity with built-in evaluation (RAGAS), hybrid retrieval, and enterprise scaling; DSPy enables systematic RAG optimization.

---

### 5. Web/Search Capabilities

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Web Search Tools** | SerpAPI, DuckDuckGo (via LangChain) | SerperDev, Exa, DuckDuckGo, Google Search | Firecrawl web scraper, custom retrievers | Custom via function calling | Via OpenAPI specs | N/A |
| **Web Scraping** | Via LangChain tools | Firecrawl, direct browser tools | Built-in document converters | Via extensions | Via OpenAPI | N/A |
| **Search Result Processing** | Basic | Links, summaries, structured data | Full document extraction, chunking | Structured via tools | Via plugins | N/A |
| **YouTube Integration** | N/A | YoutubeChannelSearchTool, YoutubeVideoSearchTool | N/A | N/A | Via OpenAPI | N/A |
| **GitHub Integration** | N/A | GithubSearchTool | N/A | N/A | Via OpenAPI | N/A |
| **Browser Automation** | N/A | Browserbase integration | N/A | Magentic-One web browsing | N/A | N/A |

**Key Differentiator:** CrewAI has the broadest web integration toolkit; AutoGen's Magentic-One provides sophisticated web browsing for complex tasks.

---

### 6. Code Execution Capabilities

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Code Generation** | Via custom nodes | CodeInterpreterTool (Python execution) | N/A | Function calling for code | Via plugins | ProgramOfThought module |
| **Sandboxing** | Not built-in (external services) | Limited (local execution) | Not built-in | Not built-in | Not built-in | Not built-in |
| **Supported Languages** | Via LLM | Python (primary) | N/A | Python + others via extensions | Via plugins | Python via Pyodide |
| **Execution Environment** | User-dependent | Local interpreter | N/A | Local + distributed | Local + plugins | Sandboxed (DSPy 3.0 via MCP) |
| **Security Consideration** | Custom handling | Manual isolation needed | N/A | Enterprise isolation via extensions | Delegation to plugins | MCP sandbox (Pydantic run-python) |

**Key Differentiator:** CrewAI provides native Python code execution; DSPy 3.0 adds sandboxed execution via MCP; all frameworks need explicit security measures for untrusted code.

---

### 7. Human-in-the-Loop Patterns

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Interrupt Points** | Built-in (interrupt_before, interrupt_after) | Via memory events | N/A | Via message passing | Manual via plugins | Manual checkpoints |
| **State Resume** | With state merge (LangGraph 1.0) | Via memory + continued execution | N/A | Via conversation replay | Via custom flow | Manual restoration |
| **Approval Workflows** | Native with checkpoints | Via agent delegation | N/A | Via human-in-the-loop agents | Via custom agents | Manual control flow |
| **Feedback Integration** | Via state updates | Memory feedback loops | N/A | Via message modification | Via state changes | Manual feedback |
| **Multi-Turn Interaction** | Native via streaming | Native multi-agent dialog | Document iteration | Native multi-agent chat | Via custom orchestration | Manual conversation loops |

**Key Differentiator:** LangGraph leads with first-class interrupt/resume support; CrewAI provides team coordination; others require manual orchestration.

---

### 8. Observability & Tracing

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Built-in Tracing** | Via LangSmith callbacks | Event system | Built-in logging | Async event-driven | Via diagnostic hooks | N/A |
| **Third-Party Integration** | LangSmith (primary), Langfuse | Langfuse, custom events | Ragas integration | Telemetry via extensions | Application Insights (.NET) | Custom logging |
| **Metrics Captured** | Traces, latency, tokens, costs | Memory operations, task events | Document retrieval, latency, throughput | Message flow, extension calls | Task execution | N/A |
| **Visualization** | LangSmith Studio (flow, state, timeline) | Real-time observability console (AOP) | Pipeline diagrams, eval dashboards | Agent Studio / AutoGen Bench | Custom dashboards | N/A |
| **Production Monitoring** | Via LangSmith Deployments | Via AOP Suite | Retrieval/generation anomalies, feedback loops | Experiment tracking | Via Azure AI | Custom implementation |
| **Debugging Features** | Time-travel debugging, state snapshots | Task replay, memory inspection | Eval metrics (correctness, hallucination) | Conversation replay | Diagnostic events | Manual tracing |

**Key Differentiator:** LangGraph+LangSmith provide the most comprehensive observability with time-travel debugging; Haystack excels at RAG-specific evaluation metrics.

---

### 9. Multi-Agent Patterns & Coordination

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Agent Types** | Generic nodes + conditional routing | Role-based agents (specialized workers) | Components with orchestration | Modular agents with different capabilities | Agent framework (custom coordination) | Module-based (no agent semantics) |
| **Collaboration Patterns** | Via edges and state passing | Hierarchical teams, delegation, chat | Sequential workflows | Two-agent chat, group chat, sequential | Custom via Core/AgentChat APIs | N/A |
| **Coordination Method** | Graph topology | Manager agent + team reporting structure | Pipeline composition | Asynchronous messaging, RoundRobin/Selector | Message passing, async patterns | Composition of modules |
| **Dynamic Interaction** | Via conditional edges | Via task delegation and feedback loops | Via dynamic routing (Routers) | Dynamic conversation spawning | Via message-driven control | N/A |
| **Team Communication** | Implicit via state | Direct agent chat with memory | Component data flow | LLM-mediated + API calls | Message passing | Parameter passing |
| **Parallel Execution** | Via parallel edges + ThreadPoolExecutor | Via team parallel tasks | Via pipeline parallelization | Via concurrent agents | Via async orchestration | N/A |
| **Handoff Patterns** | Via graph edges | Clear agent-to-agent handoffs | Via router components | Handoff via conversation routing | Custom orchestration | N/A |

**Key Differentiator:** CrewAI's role-based agents with hierarchical teams offer the most structure for multi-agent collaboration; LangGraph provides the most flexible graph-based orchestration.

---

### 10. Extensibility & Custom Integration

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Custom Nodes** | Yes (add_node) | Yes (Tool base class) | Yes (Component base class) | Yes (Agent subclasses) | Yes (KernelFunction) | Yes (dspy.Module) |
| **Plugin System** | Via LangChain tools | CrewAI tools + integrations | Component library (100+) | Extensions API with registry | Plugin ecosystem + OpenAPI/MCP | Module composition |
| **Standard Integrations** | 50+ LangChain integrations | 700+ via CrewAI tools + Composio | 10+ model providers, vector stores | Community extensions | 7+ LLM providers, Azure ecosystem | Pluggable retrieval/models |
| **Standards Support** | LangChain ecosystem | Custom + OpenAI functions | Custom components | OpenAPI via extensions | **OpenAPI, MCP (Model Context Protocol), native code** | Custom |
| **Community Extensions** | Active LangChain community | CrewAI toolkit ecosystem | Community components | AutoGen marketplace | Microsoft + partners | Community modules |
| **Version Stability** | Rapid iteration (v1.0 Oct 2025) | Stable releases | Stable + quarterly updates | Ramp up in 2025 | Stable enterprise release cycle | Monthly minor releases |

**Key Differentiator:** Semantic Kernel emphasizes standards (OpenAPI, MCP); CrewAI has the largest ecosystem (700+ tools); LangGraph in rapid development with breaking changes planned.

---

### 11. Production Readiness & Enterprise Features

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Error Recovery** | Checkpoint-based recovery | Memory-based recovery | Pipeline state recovery | Via retry logic | Manual error handling | Manual restoration |
| **Scalability** | Via LangSmith cloud | File/DB backend scaling | Kubernetes-ready, cloud-agnostic | Local + Azure deployment | Azure ecosystem | Local only |
| **Performance** | Optimized streaming | Reports 5.76x faster than LangGraph on some tasks | Production-tested pipelines | Not emphasized | Enterprise-grade performance | Systematic optimization |
| **Security** | Via LangSmith (OpenAI trusted) | External security via integrations | Enterprise-grade logging/audit | RBAC, compliance via Azure | Microsoft enterprise standards | N/A |
| **Monitoring** | LangSmith Deployments | AOP Suite (24/7 support) | Built-in eval + monitoring | Via Azure AI | Application Insights, custom | Manual logging |
| **Support** | LangChain support | 24/7 enterprise support (AOP) | Community + enterprise support | Microsoft support (Agent Framework) | Microsoft support | Stanford + community |
| **Compliance** | Via Azure/LangSmith | Compliance ready | Data residency options | Azure compliance (HIPAA, SOC2) | Azure compliance | N/A |

**Key Differentiator:** Semantic Kernel and AutoGen backed by Microsoft enterprise infrastructure; CrewAI and Haystack offer community-first with enterprise options; LangGraph emerging as enterprise option via LangSmith.

---

### 12. Language & Platform Support

| Capability | LangGraph | CrewAI | Haystack | AutoGen | Semantic Kernel | DSPy |
|------------|-----------|--------|----------|---------|-----------------|------|
| **Primary Languages** | Python, JavaScript | Python (primary) | Python | Python, .NET (Agent Framework) | C#, Python, Java | Python |
| **Cross-Language Support** | LangChain ecosystem | Python-only | Python + integrations | **Python + .NET + more planned** | **C# (native), Python, Java** | Python |
| **Deployment Targets** | Node.js, Python environments | Python environments | Kubernetes, cloud-agnostic | Azure AI Foundry, local | Azure, on-prem, cloud | Local Python |
| **Framework Dependencies** | LangChain core | Pydantic, SQLite, ChromaDB | Networkx, Sentence-transformers | Minimal (pluggable) | Pydantic (core) | Minimal |
| **Integration with IDE/Editor** | LangSmith IDE | Custom IDE | Custom runners | AutoGen Studio (no-code) | Visual Studio ecosystem | Jupyter notebooks |

**Key Differentiator:** Semantic Kernel excels in multi-language (.NET/Python first-class) and enterprise IDE integration; AutoGen Studio offers no-code GUI.

---

## Summary Comparison Table: Quick Reference

| Framework | Strengths | Weaknesses | Best For |
|-----------|-----------|-----------|----------|
| **LangGraph** | State management, human-in-the-loop, streaming, debugging, time-travel | Rapid API changes, requires LangSmith for full features, learning curve | Complex stateful workflows, financial/healthcare agents, sophisticated state handling |
| **CrewAI** | Ease of use, 700+ tools, multi-agent teams, fast execution (5.76x vs LangGraph), memory system | Less flexible graph control, Python-only, smaller ecosystem | Quick MVP, collaborative multi-agent systems, teams of specialized agents |
| **Haystack** | RAG maturity, evaluation framework (RAGAS), production scaling, component modularity, Kubernetes-ready | Less suitable for general agentic workflows, steep learning curve for complex orchestration | Document-heavy applications, RAG pipelines, regulated industries (finance, healthcare), semantic search |
| **AutoGen** | Cross-language support (Python/.NET), enterprise backing (Microsoft), multi-agent patterns, Magentic-One | Newer ecosystem, less mature than alternatives, learning curve | Enterprise multi-agent systems, cross-platform deployment, team coordination |
| **Semantic Kernel** | Standards-based (OpenAPI, MCP), enterprise .NET support, Microsoft backing, clean plugin model | Fewer built-in tools, smaller ecosystem than CrewAI, requires more custom code | Enterprise .NET shops, standards-based integrations, Microsoft ecosystem (Azure, M365) |
| **DSPy** | Systematic prompt optimization, modular composition, RAG quality improvements (42%→61%), LLM-agnostic | Less mature for agents, limited production examples, steep learning curve for optimization | Research, systematic optimization of prompts/weights, RAG quality improvement, modular AI systems |

---

## Key Differentiators: What Each Framework Does Uniquely

### LangGraph
- **First-class human-in-the-loop:** Native interrupt/resume with state merging
- **Time-travel debugging:** Inspect and replay execution from any checkpoint
- **Granular streaming:** Messages, updates, values, or custom event types
- **Planned improvements:** v1.0 release (Oct 2025) with enhanced state management

### CrewAI
- **700+ integrated tools:** Largest pre-built tool ecosystem
- **Role-based agent architecture:** Agents with consistent personas and responsibilities
- **Hierarchical team coordination:** Manager agents with team hierarchies
- **Performance:** Reports 5.76x faster execution on certain tasks vs LangGraph
- **Memory sophistication:** Multi-tier (short-term, long-term, entity-based)

### Haystack
- **RAG production maturity:** Built-in RAGAS evaluation, hybrid retrieval, component reusability
- **Enterprise scaling:** Kubernetes-ready, cloud-agnostic, strong logging/monitoring
- **Explainability:** Confidence scoring, document ranking transparency
- **Regulated industry focus:** Audit trails, compliance-ready logging
- **Component composability:** Swap models/vector stores without rewriting

### AutoGen
- **Cross-language orchestration:** Python, .NET, more languages planned
- **Magentic-One:** State-of-the-art multi-agent team for web browsing, code, file handling
- **Microsoft integration:** Azure AI Foundry, enterprise compliance (HIPAA, SOC2)
- **Studio GUI:** No-code interface for building multi-agent systems
- **Async event-driven:** Scalable asynchronous communication between agents

### Semantic Kernel
- **Standards compliance:** OpenAPI, MCP (Model Context Protocol) support
- **Enterprise .NET:** First-class C# support with clean DI integration
- **Plugin composability:** Clear semantic (prompt) vs native (code) function boundaries
- **Microsoft backing:** Deep integration with Azure, Copilot ecosystem
- **Built-in planners:** Orchestrate multi-step tool calling automatically

### DSPy
- **Systematic optimization:** Teleprompters compile programs into optimized prompts
- **RAG improvement:** Documented 19pt F1 improvement (42%→61%)
- **Modular programming:** Composition of predictors and reasoning chains
- **LLM-agnostic:** Works with any provider/model
- **Latest capabilities:** Native image/audio support in DSPy 3.0 (Aug 2025)

---

## Recommendations for The Edge Agent (tea)

### 1. **Adopt Memory Patterns from CrewAI**
- Implement multi-tier memory: short-term (current context), long-term (persistent), entity-based
- Consider ChromaDB or similar for embedding-based memory retrieval
- Add memory events for observability

```yaml
# Example tea YAML enhancement
agents:
  analyst:
    memory:
      short_term: true  # ChromaDB RAG for context
      long_term: true   # SQLite for task results
      entity_tracking: true
```

### 2. **Enhance YAML DSL with Tool Definitions**
- Add `tools` section in YAML similar to CrewAI/Semantic Kernel
- Support tool metadata (description, parameters, required auth)
- Enable dynamic tool registration at runtime

```yaml
# Example enhancement
tools:
  - name: web_search
    provider: serper_dev
    description: "Search the web for current information"
    required_fields: [query]
    auth_key: ${secrets.SERPER_KEY}

  - name: code_interpreter
    type: python_sandbox
    timeout: 30s
    max_output: 10000
```

### 3. **Implement Streaming & Observability Hooks**
- Add streaming modes similar to LangGraph (state deltas, custom events)
- Implement observability instrumentation (hooks for LangSmith/Langfuse integration)
- Support structured tracing of node execution

```python
# Example API enhancement
for event in graph.stream(
    {"input": "..."},
    streaming_mode="updates"  # Full state, deltas, or custom
):
    if event["type"] == "node_execute":
        print(f"Node {event['node']} started")
```

### 4. **Add MCP (Model Context Protocol) Support**
- Implement MCP client to connect to external tools/services
- Enables tool sharing across frameworks and standardization
- Pydantic's mcp-run-python provides secure Python execution

```yaml
# Example tea YAML
mcp_servers:
  - name: python_sandbox
    url: "stdio://mcp-run-python"
    description: "Secure Python execution"
```

### 5. **Strengthen Error Recovery & Retry**
- Implement per-node retry policies with exponential backoff (like LangGraph)
- Add circuit breaker patterns for cascading failures
- Support graceful degradation and error state routing

```yaml
# Example enhancement
nodes:
  api_call:
    action: http.get
    retries: 3
    backoff: exponential
    on_error: fallback_node
```

### 6. **Consider Tool Ecosystem**
- **Short-term:** Document integration with popular LangChain tools
- **Medium-term:** Build bridge to CrewAI tool ecosystem (700+ integrations)
- **Long-term:** Develop native tool library for edge-specific use cases (IoT, telemetry, local file systems)

### 7. **Add Human-in-the-Loop Mechanisms**
- Implement interrupt points before/after critical nodes
- Support state merging on resume (like LangGraph 1.0)
- Enable approval workflows for sensitive operations

```yaml
# Example enhancement
nodes:
  sensitive_action:
    action: api.call
    interrupt_before: true
    approval_required: true
```

### 8. **Establish Observability & Monitoring**
- Add structured logging for all node transitions
- Support callback hooks for metrics collection
- Enable third-party observability integration (Langfuse, custom)

```python
# Example usage
graph = StateGraph({...}, observe=True)
graph.add_observability_callback("langfuse", endpoint=os.getenv("LANGFUSE_HOST"))
```

---

## Competitive Positioning of tea

### Where tea Excels (Current)
1. **Lightweight for edge environments** - Minimal dependencies, no heavy ML infrastructure
2. **LangGraph-compatible API** - Easier adoption for LangGraph users
3. **Interrupt/resume support** - Matches LangGraph capabilities for human-in-the-loop
4. **YAML configuration** - Unique declarative approach vs graph-based DSLs

### Where tea Should Strengthen
1. **Tool ecosystem** - Currently limited vs 700+ in CrewAI
2. **Memory management** - Add CrewAI-style multi-tier memory
3. **Observability** - Add hooks for Langfuse/LangSmith integration
4. **Performance optimization** - Systematic approach like DSPy or CrewAI's reported 5.76x speedup
5. **Web capabilities** - Limited to basic web search vs CrewAI's full toolkit
6. **Code execution** - No sandboxing vs frameworks with E2B/Pydantic integration

### Unique Positioning Opportunities
1. **Edge-first design** - Only framework optimized for edge devices (IoT, local deployment)
2. **YAML as primary DSL** - Declarative approach more accessible than graph/module code
3. **Python-agnostic tools** - Not locked into LangChain ecosystem
4. **Checkpoint portability** - Pickle-based checkpoints can be migrated across platforms
5. **Minimal dependencies** - Appeal to teams avoiding heavy MLOps stacks

---

## Technology Trends to Monitor

### 1. **Model Context Protocol (MCP) Adoption**
- Semantic Kernel, AutoGen, and now DSPy supporting MCP
- Emerging standard for tool/service integration across frameworks
- **Action:** Add MCP client to tea for standardized tool discovery

### 2. **Agentic AI in Production (2025)**
- Shift from research to commercial deployment
- Focus on reliability, observability, and compliance
- **Action:** Strengthen monitoring, error recovery, and audit trails in tea

### 3. **Multi-Agent Orchestration Maturing**
- CrewAI and AutoGen leading with clear patterns
- Convergence on hierarchical and handoff-based coordination
- **Action:** Consider formalizing multi-agent patterns in tea YAML

### 4. **RAG Evaluation & Optimization**
- RAGAS and DSPy proving RAG quality can be systematically improved
- Shift from simple retrieval to agentic reasoning over retrieved content
- **Action:** Integrate RAG evaluation metrics into tea examples

### 5. **Enterprise Convergence**
- Microsoft Agent Framework merging AutoGen + Semantic Kernel
- Focus on compliance, auditing, and cross-language support
- **Action:** Monitor for opportunity to position tea as lightweight alternative

---

## Detailed Sources & Links

### LangGraph/LangChain
- [LangGraph Official](https://www.langchain.com/langgraph)
- [LangGraph vs LangChain Comparison (Medium, Aug 2025)](https://medium.com/@vinodkrane/langchain-vs-langgraph-choosing-the-right-framework-for-your-ai-workflows-in-2025-5aeab94833ce)
- [LangChain Tools Documentation](https://python.langchain.com/docs/concepts/tools/)
- [LangSmith Observability](https://docs.langchain.com/oss/python/langgraph/observability)
- [LangGraph Persistence](https://www.baihezi.com/mirrors/langgraph/how-tos/persistence/index.html)

### CrewAI
- [CrewAI Framework 2025 Review](https://latenode.com/blog/crewai-framework-2025-complete-review-of-the-open-source-multi-agent-ai-platform)
- [CrewAI Tools Documentation](https://docs.crewai.com/en/concepts/tools)
- [CrewAI Memory System](https://docs.crewai.com/en/concepts/memory)
- [CrewAI GitHub](https://github.com/crewAIInc/crewAI)
- [Observability for CrewAI (Langfuse)](https://langfuse.com/integrations/frameworks/crewai)

### Haystack
- [Haystack Official Documentation](https://haystack.deepset.ai/)
- [How to Build Agentic QA RAG System (AnalyticsVidhya)](https://www.analyticsvidhya.com/blog/2025/02/qa-rag-using-haystack/)
- [RAG Frameworks Comparison (Firecrawl)](https://www.firecrawl.dev/blog/best-open-source-rag-frameworks)
- [RAG in Production with Haystack (O'Reilly Guide)](https://4561480.fs1.hubspotusercontent-na1.net/hubfs/4561480/Ebooks%20whitepapers%20and%20reports/O'Reilly%20Guide%20-%20RAG%20in%20Production%20with%20Haystack/OReilly%20Guide%20-%20RAG_in_production_with_Haystack-FINAL.pdf)

### AutoGen / Microsoft Agent Framework
- [Microsoft Agent Framework](https://learn.microsoft.com/en-us/agent-framework/overview/agent-framework-overview)
- [AutoGen GitHub](https://github.com/microsoft/autogen)
- [Deep Dive into AutoGen (SparkCo)](https://sparkco.ai/blog/deep-dive-into-autogen-microsoft-agent-framework)
- [Microsoft Agent Framework Deep Dive (SparkCo 2025)](https://sparkco.ai/blog/deep-dive-into-autogen-multi-agent-patterns-2025)

### Semantic Kernel
- [Semantic Kernel Official](https://learn.microsoft.com/en-us/semantic-kernel/overview/)
- [Plugins in Semantic Kernel](https://learn.microsoft.com/en-us/semantic-kernel/concepts/plugins/)
- [Building Smarter AI Agents (Neel Bhatt)](https://neelbhatt.com/2025/06/08/building-smarter-ai-agents-with-semantic-kernel/)

### DSPy
- [DSPy Official](https://dspy.ai/)
- [DSPy on GitHub](https://github.com/stanfordnlp/dspy)
- [DSPy 3.0: Evolution to Programming (Medium, Sep 2025)](https://devoscientist.medium.com/dspy-3-0-the-evolution-from-prompt-engineering-to-programming-b8df5ead5c60)
- [Retrieval-Augmented Generation (RAG) - DSPy](https://dspy.ai/tutorials/rag/)

### Comparative Analysis
- [Best Agentic AI Frameworks 2025 (TrixlyAI)](https://www.trixlyai.com/blog/technical-14/best-agentic-ai-frameworks-in-2025-comparing-langchain-langgraph-crewai-and-haystack-74)
- [AI Agent Frameworks: Top 14 Comparison (Softcery)](https://softcery.com/lab/top-14-ai-agent-frameworks-of-2025-a-founders-guide-to-building-smarter-systems)
- [LangChain Alternatives (Akka)](https://akka.io/blog/langchain-alternatives)
- [Top 5 Ranked AI Agent Frameworks (AlphaCorp, Nov 2025)](https://alphacorp.ai/top-5-ai-agent-frameworks-november-2025/)

### Security & Code Execution
- [Pydantic Sandboxed Python via MCP (WinBuzzer)](https://winbuzzer.com/2025/04/20/pydantic-releases-sandboxed-python-execution-server-for-ai-agents-via-model-context-protocol-xcxwbn/)
- [E2B Enterprise AI Agent Cloud](https://e2b.dev/)
- [Code Sandboxes for LLMs (Amir's Blog)](https://amirmalik.net/2025/03/07/code-sandboxes-for-llm-ai-agents)
- [Secure Code Execution (Medium)](https://saurabh-shukla.medium.com/secure-code-execution-in-ai-agents-d2ad84cbec97)

---

## Appendix: Tool Integration Examples

### CrewAI Tool Categories (Most Comprehensive)
```
File & Document Tools: DirectoryReadTool, FileReadTool, PDFSearchTool,
                       DOCXSearchTool, MDXSearchTool, XMLSearchTool,
                       TXTSearchTool, JSONSearchTool, CSVSearchTool
Web & Search Tools: SerperDevTool, WebsiteSearchTool, EXASearchTool,
                   GithubSearchTool, YoutubeChannelSearchTool,
                   YoutubeVideoSearchTool, FirecrawlScrapeWebsiteTool
AI & Data Tools: DALL-E Tool, Vision Tool, CodeInterpreterTool,
                RagTool, PGSearchTool, MySQLTool
Integration Tools: ComposioTool, LlamaIndexTool, ApifyActorsTool,
                  BrowserbaseLoadTool
```

### Semantic Kernel Plugin Types
```
Native Code: C# classes, Python functions with @KernelFunction decorators
OpenAPI Specs: Import via OpenAPI definitions for external APIs
MCP Servers: Model Context Protocol for standardized tool discovery
Memory Plugins: Short/long-term storage interfaces
Planning Plugins: Orchestrate multi-step tool calling
```

### AutoGen Extensions
```
Model Clients: OpenAI, Azure OpenAI, Anthropic, etc.
Agents: Base agents, code-executing agents, custom agents
Multi-Agent Teams: Group chat, hierarchical patterns
Tools: Built-in + community extensions via registry
Runtime: Local and distributed execution environments
```

---

**Document Version:** 1.0
**Last Updated:** December 2025
**Research Scope:** 6 major frameworks, 12 capability dimensions
**Recommendation Status:** Ready for architecture decisions
