# LLM Agent Frameworks - Quick Reference (2025)

## At-a-Glance Comparison

```
FRAMEWORK      | Maturity | Ease of Use | Tool Library | Enterprise | Edge-Ready
───────────────┼──────────┼─────────────┼──────────────┼────────────┼───────────
LangGraph      |   ✓✓✓    |     ✓✓      |     ✓✓       |    ✓✓✓     |    ✗
CrewAI         |   ✓✓✓    |     ✓✓✓     |     ✓✓✓      |    ✓✓      |    ✗
Haystack       |   ✓✓✓    |     ✓       |     ✓✓       |    ✓✓✓     |    ✗
AutoGen        |   ✓✓     |     ✓✓      |     ✓✓       |    ✓✓✓     |    ✗
Semantic K.    |   ✓✓     |     ✓       |     ✓        |    ✓✓✓     |    ✗
DSPy           |   ✓✓     |     ✗       |     ✗        |    ✗       |    ✗
───────────────┼──────────┼─────────────┼──────────────┼────────────┼───────────
tea (today)    |   ✓✓     |     ✓✓✓     |     ✗        |    ✓✓      |    ✓✓✓
tea (roadmap)  |   ✓✓✓    |     ✓✓✓     |     ✓✓       |    ✓✓✓     |    ✓✓✓
```

---

## Framework Selection Guide

### Choose LangGraph if:
- ✓ State machines are your primary need
- ✓ Human-in-the-loop workflows are critical
- ✓ You need advanced debugging (time-travel)
- ✓ You're in finance/healthcare (complex workflows)
- ✓ You have budget for LangSmith observability
- ✗ Working on edge devices
- ✗ Want configuration-based approach

### Choose CrewAI if:
- ✓ You want agents to work as a team
- ✓ You need quick MVP (lots of tools available)
- ✓ You like high-level role-based abstraction
- ✓ You want 700+ pre-built integrations
- ✓ Python-only deployment is fine
- ✗ You need enterprise .NET support
- ✗ You're building RAG-specific system

### Choose Haystack if:
- ✓ RAG is your primary use case
- ✓ You need production-grade evaluation (RAGAS)
- ✓ You value component reusability
- ✓ You need Kubernetes scaling
- ✓ You're in regulated industry (audit trails)
- ✗ You want quick agent prototyping
- ✗ You need 700+ tools

### Choose AutoGen if:
- ✓ You need cross-language support (Python/.NET)
- ✓ Microsoft ecosystem integration matters
- ✓ You like no-code GUI (AutoGen Studio)
- ✓ You want Magentic-One for complex browsing
- ✓ Enterprise compliance is required
- ✗ You prefer simplicity over features
- ✗ You're not in Microsoft ecosystem

### Choose Semantic Kernel if:
- ✓ You're building in .NET enterprise environment
- ✓ Microsoft Azure integration is critical
- ✓ Standards-based tools matter (OpenAPI, MCP)
- ✓ Plugin architecture appeals to you
- ✓ You want clean DI integration
- ✗ You need 700+ pre-built tools
- ✗ You're Python-first shop

### Choose DSPy if:
- ✓ You're researching prompt/weight optimization
- ✓ RAG quality improvement matters
- ✓ You want modular composition
- ✓ You're building from research papers
- ✓ Performance systematic optimization is important
- ✗ You need production-ready framework
- ✗ You want built-in tools

### Choose tea if:
- ✓ You're deploying to edge/IoT devices
- ✓ You prefer YAML configuration over code
- ✓ Minimal dependencies are critical
- ✓ You need LangGraph-like state management
- ✓ You want checkpoint portability
- ✓ You like human-in-the-loop workflows
- ? You'll bridge to CrewAI/LangChain tools
- ✗ You need 700+ tools today (pre-roadmap)
- ✗ You need .NET support

---

## Key Statistics (2025)

| Metric | LangGraph | CrewAI | Haystack | AutoGen | Semantic K. | DSPy |
|--------|-----------|--------|----------|---------|-------------|------|
| **GitHub Stars** | 4.5K+ | 30.5K+ | 8K+ | 28K+ | 8K+ | 9K+ |
| **NPM Monthly Downloads** | 5M+ | 1M+ | 500K | 100K+ | 200K | 50K |
| **Supported LLMs** | 20+ | Any | 10+ | Any | 7+ | Any |
| **Built-in Tools** | Via chain | 100+ | Components | Extensions | Plugins | N/A |
| **Vector Stores** | 20+ | 1 (Chroma) | 10+ | Custom | Custom | Custom |
| **Checkpointing** | ✓ (.pkl) | ✓ (SQLite) | ✓ (state) | ✗ | ✓ (custom) | Manual |
| **Streaming** | Full | Partial | Basic | Partial | Via adapters | Via adapters |
| **Enterprise Support** | LangChain | CrewAI AOP | Community | Microsoft | Microsoft | Stanford |
| **Performance** | Baseline | 5.76x faster* | Optimized | Unknown | Enterprise-grade | Optimized |
| **License** | MIT | Apache 2.0 | Apache 2.0 | MIT | MIT | Apache 2.0 |

*CrewAI claims 5.76x faster on certain QA tasks vs LangGraph

---

## Capability Deep Dive: Tools

### Tool Availability
```
CrewAI:       ████████████████████ 700+
LangChain:    ████████████ 50+
Haystack:     ████████ 10+
Semantic K.:  ███ 5-10 (mostly custom)
AutoGen:      ███ 5-10 (mostly custom)
DSPy:         ░ 0 (composable only)
tea:          ░ 0 (pre-roadmap)
```

### Easiest Tool Integration
1. **CrewAI**: Drop-in tools, 700+ pre-built
2. **LangGraph**: Via LangChain tools ecosystem
3. **Haystack**: Component-based composition
4. **Semantic Kernel**: OpenAPI + plugins
5. **AutoGen**: Function calling + extensions
6. **DSPy**: Manual composition
7. **tea**: YAML tool definitions (post-roadmap)

### Web Search Capabilities
```
CrewAI:       SerperDev, Exa, YouTube, GitHub, Firecrawl, Browserbase
LangGraph:    SerpAPI, DuckDuckGo (via LangChain)
Haystack:     Firecrawl, custom scrapers
AutoGen:      Magentic-One (sophisticated browsing)
Semantic K.:  Via OpenAPI integration
DSPy:         Custom implementation
tea:          (roadmap: SerperDev, basic HTTP)
```

---

## Capability Deep Dive: Memory

### Memory Systems Ranking
```
1. CrewAI:        Short-term (ChromaDB) + Long-term (SQLite) + Entity tracking
2. LangGraph:     State dict + Checkpointers (file/DB)
3. Haystack:      Document-centric (implicit)
4. Semantic K.:   Pluggable interfaces
5. AutoGen:       Message history + persistence
6. DSPy:          Manual state tracking
7. tea:           State dict only (pre-roadmap)
```

### Memory Persistence
```
CrewAI:        ✓ Automatic (SQLite + ChromaDB)
LangGraph:     ✓ Via checkpointers (.pkl files)
Haystack:      ✓ Vector store backed
AutoGen:       ✓ Custom backends
Semantic K.:   ✓ Pluggable storage
DSPy:          ✗ Manual only
tea:           ✓ Pickle checkpoints (today)
               ✓ Multi-tier (roadmap)
```

---

## Capability Deep Dive: Observability

### Observability Maturity
```
LangSmith:         ████████████████ Professional (native)
CrewAI AOP:        █████████████ Enterprise (24/7 support)
Langfuse:          ███████████ Open-source + commercial
Haystack:          ████████ Built-in + RAGAS
AutoGen:           ██████ Emerging
Semantic K.:       ██████ Application Insights (.NET)
DSPy:              ░ Manual only
tea:               ░░ Pre-roadmap (Langfuse integration planned)
```

### Production Monitoring
```
Best-in-class:     Haystack (retrieval anomalies, feedback loops)
Very Good:         LangGraph (time-travel, state snapshots)
Good:              CrewAI (event system, memory inspection)
Acceptable:        AutoGen, Semantic K.
Limited:           DSPy
```

---

## Capability Deep Dive: Code Execution

### Code Execution Support
```
CrewAI:            ✓ Native Python (local interpreter)
AutoGen:           ✓ Function calling (code generation)
DSPy:              ✓ ProgramOfThought (code generation)
LangGraph:         ✓ Via LangChain tools
Semantic K.:       ✓ Via plugins
Haystack:          ✗ Not primary feature
tea:               ✗ Pre-roadmap (planned with sandboxing)
```

### Sandboxing Options
```
Enterprise:        E2B (LLM-agnostic), gVisor + Jupyter
Framework-native:  None (all have gaps)
Emerging standard: Pydantic mcp-run-python (Deno/WebAssembly)
```

---

## Capability Deep Dive: RAG

### RAG Specialization Ranking
```
1. Haystack:       Built-in evaluation, hybrid retrieval, component reuse
2. DSPy:           Systematic optimization (42%→61% F1 improvement)
3. LangGraph:      Via LangChain retrievers
4. CrewAI:         Via RAG tool
5. Semantic K.:    Via plugins
6. AutoGen:        Via extensions
7. tea:            (roadmap: retriever abstraction)
```

### Vector Store Support
```
LangGraph:    ████████████████ 20+ (via LangChain)
Haystack:     ███████████ 10+ (FAISS, Elasticsearch, Weaviate, Qdrant, Milvus)
CrewAI:       ██ 1 (ChromaDB)
DSPy:         ████████ 8+ (Milvus, Chroma, FAISS, custom)
AutoGen:      ███ 3-5 (custom)
Semantic K.:  ██ Azure AI Search + custom
tea:          ░░ (roadmap: Chroma, FAISS, custom)
```

---

## Enterprise Feature Matrix

| Feature | LangGraph | CrewAI | Haystack | AutoGen | Semantic K. |
|---------|-----------|--------|----------|---------|-------------|
| **Multi-tenancy** | Via LangSmith | AOP suite | Custom | Custom | Via Azure |
| **Audit Logging** | ✓ (LangSmith) | ✓ (AOP) | ✓ Built-in | Custom | ✓ (Azure) |
| **RBAC** | Via LangSmith | Custom | Custom | Azure AD | Azure AD |
| **Data Residency** | US/EU (LangSmith) | Custom | Custom | Azure regions | Azure regions |
| **Compliance** | SOC2 (LangSmith) | Custom | Custom | HIPAA/SOC2 | HIPAA/SOC2 |
| **SLA** | 99.9% (LangSmith) | 99.5% (AOP) | Custom | Azure SLA | Azure SLA |
| **24/7 Support** | Via LangChain | ✓ (AOP) | Community | Microsoft | Microsoft |

---

## Performance Benchmarks (When Available)

### Task Execution Speed
```
CrewAI:        5.76x faster than LangGraph (claimed, certain QA tasks)
LangGraph:     Baseline (complex state handling overhead)
Haystack:      Unknown (optimized for RAG, not speed)
AutoGen:       Unknown (asynchronous, likely fast)
DSPy:          Unknown (focus on quality, not speed)
tea:           Unknown (to be benchmarked)
```

### Memory Footprint
```
tea:           <50MB (lightweight)
LangGraph:     100-300MB (due to LangChain dependencies)
CrewAI:        150-400MB (ChromaDB, SQLite, tools)
AutoGen:       80-200MB (event system overhead)
Semantic K.:   100-250MB (depends on .NET runtime)
Haystack:      200-500MB (component heavy)
DSPy:          50-150MB (modular, variable)
```

### Context Window Efficiency
```
DSPy:          Optimized (systematic optimization of prompts)
Haystack:      Good (intelligent chunking, retrieval)
LangGraph:     Good (state compression available)
CrewAI:        Acceptable (entity tracking helps)
AutoGen:       Acceptable (message history management)
Semantic K.:   Acceptable (configurable)
tea:           Unknown (to be optimized)
```

---

## Deployment Scenarios

### Scenario: Customer Support Chatbot (Simple)
**Best choice:** CrewAI
- Reasons: Easiest setup, team coordination, web search
- Effort: 2-4 days
- Cost: Low (open-source + LLM API)

### Scenario: Financial Document Analysis with Approval
**Best choice:** LangGraph
- Reasons: State management, human-in-the-loop, debugging
- Effort: 2-3 weeks
- Cost: Medium (LangSmith for observability)

### Scenario: Enterprise RAG System (Compliance-Heavy)
**Best choice:** Haystack
- Reasons: Evaluation, audit trails, component stability
- Effort: 3-4 weeks
- Cost: Medium (evaluation setup)

### Scenario: IoT Agent Network (Edge Deployment)
**Best choice:** tea (today or post-roadmap)
- Reasons: Lightweight, edge-optimized, YAML config
- Effort: 2-3 weeks
- Cost: Low (minimal infrastructure)

### Scenario: .NET Enterprise System
**Best choice:** Semantic Kernel / AutoGen (Agent Framework)
- Reasons: Language parity, Azure integration, compliance
- Effort: 2-3 weeks
- Cost: Medium (Azure infrastructure)

### Scenario: Research on Prompt Optimization
**Best choice:** DSPy
- Reasons: Systematic optimization, modular composition
- Effort: Variable (research-dependent)
- Cost: Low (compute-dependent)

---

## Technology Trends to Watch (2025+)

1. **Model Context Protocol (MCP) Adoption**
   - Emerging standard for tool integration
   - Semantic Kernel, AutoGen, DSPy all supporting
   - Impact: Frameworks becoming interoperable

2. **Agentic AI in Production**
   - Shift from research to commercial deployment
   - Focus on reliability, compliance, observability
   - Impact: Haystack/LangGraph lead on enterprise

3. **Edge AI Growth**
   - IoT, robotics, autonomous systems emerging
   - **Opportunity:** tea's unique positioning

4. **Multi-Agent Orchestration Standardization**
   - CrewAI's team patterns spreading to others
   - AutoGen's conversation patterns maturing
   - Impact: Convergence on hierarchical + handoff patterns

5. **RAG Evaluation Maturity**
   - RAGAS integration becoming standard
   - Systematic quality improvements (DSPy model)
   - Impact: RAG moving beyond retrieval → reasoning

---

## Estimated Learning Curves

### To "Hello World" (Minimal Viable Agent)
```
CrewAI:        ████ 2-4 hours (most accessible)
tea:           ████ 2-4 hours (YAML-based)
LangGraph:     █████ 4-6 hours (state concept)
AutoGen:       █████ 4-6 hours (conversation patterns)
Semantic K.:   ██████ 6-8 hours (plugin model)
Haystack:      ██████ 6-8 hours (component pipeline)
DSPy:          ███████ 8-12 hours (modular paradigm)
```

### To Production-Grade System
```
CrewAI:        ██████ 2-3 weeks (tools, memory)
tea:           ██████ 2-3 weeks (checkpoints, observability)
LangGraph:     ███████ 3-4 weeks (state machine patterns)
Semantic K.:   ███████ 3-4 weeks (plugin composition)
AutoGen:       ████████ 4-6 weeks (multi-agent coordination)
Haystack:      ████████ 4-6 weeks (component tuning)
DSPy:          █████████ 6-8 weeks (systematic optimization)
```

---

## Cost Comparison (Monthly Estimate for Small App)

### Baseline System: Customer Support Agent (10K interactions/month)

| Framework | Open-source | LLM API | Observability | Infrastructure | Total |
|-----------|------------|---------|---------------|-----------------|-------|
| **CrewAI** | Free | $50-200 | Free/paid | $20-100 | $70-300 |
| **tea** | Free | $50-200 | Free/paid | $10-50 | $60-250 |
| **LangGraph** | Free | $50-200 | $50-500 | $20-100 | $120-800 |
| **Haystack** | Free | $50-200 | Free/paid | $20-100 | $70-300 |
| **AutoGen** | Free | $50-200 | Azure | $50-500 | $100-700 |
| **Semantic K.** | Free | $50-200 | Azure | $50-500 | $100-700 |
| **DSPy** | Free | $50-200 | Custom | $10-50 | $60-250 |

*Highly variable based on scale, LLM choice, and tool usage*

---

## Community & Ecosystem Size

| Framework | GitHub Stars | Contributors | Discord/Community | Package Ecosystem |
|-----------|-------------|---------------|------------------|-------------------|
| **CrewAI** | 30.5K | 50+ | 5K+ active | 700+ tools |
| **LangGraph** | 4.5K | 30+ | 10K+ LangChain community | 50+ integrations |
| **Haystack** | 8K+ | 40+ | 2K+ | Component library |
| **AutoGen** | 28K+ | 40+ | 3K+ | Extensions registry |
| **Semantic Kernel** | 8K+ | 30+ | Microsoft community | .NET ecosystem |
| **DSPy** | 9K+ | 250+ | Active community | Research-focused |
| **tea** | <500 | <10 | Small but growing | Growing |

---

## Recommended Reading

### For Framework Comparison
- [Best Agentic AI Frameworks 2025 (TrixlyAI)](https://www.trixlyai.com/blog/technical-14/best-agentic-ai-frameworks-in-2025-comparing-langchain-langgraph-crewai-and-haystack-74)
- [LangChain vs LangGraph vs CrewAI (Medium)](https://medium.com/@vinodkrane/langchain-vs-langgraph-choosing-the-right-framework-for-your-ai-workflows-in-2025-5aeab94833ce)
- [Top 5 RAG Frameworks (AnalyticsVidhya)](https://www.analyticsvidhya.com/blog/2025/03/top-rag-frameworks-for-ai-applications/)

### For Specific Frameworks
- [LangGraph Official Docs](https://www.langchain.com/langgraph)
- [CrewAI Official Docs](https://docs.crewai.com/)
- [Haystack O'Reilly Guide](https://4561480.fs1.hubspotusercontent-na1.net/hubfs/4561480/Ebooks%20whitepapers%20and%20reports/O'Reilly%20Guide%20-%20RAG%20in%20Production%20with%20Haystack/OReilly%20Guide%20-%20RAG_in_production_with_Haystack-FINAL.pdf)
- [Microsoft Agent Framework](https://learn.microsoft.com/en-us/agent-framework/overview/agent-framework-overview)
- [DSPy Official](https://dspy.ai/)

---

**Quick Reference Version:** 1.0
**Last Updated:** December 2025
**Perfect for:** Framework selection, team presentations, quick lookups
