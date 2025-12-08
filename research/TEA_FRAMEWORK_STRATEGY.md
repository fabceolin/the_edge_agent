# The Edge Agent (tea) - Framework Strategy & Roadmap

**Date:** December 2025
**Objective:** Define tea's competitive positioning and feature roadmap based on 2025 framework landscape analysis

---

## I. Executive Overview

The Edge Agent has a unique niche in the 2025 LLM agent framework landscape: **lightweight, edge-optimized orchestration with declarative YAML configuration**. While LangGraph excels at state management and CrewAI dominates tool ecosystems, no framework prioritizes edge deployment, minimal dependencies, and configuration-as-code accessibility.

### Current Market State
- **LangGraph**: Production-grade state machine orchestration, but requires LangSmith for full features
- **CrewAI**: Easiest to learn with 700+ tools, but Python-specific and resource-hungry
- **Haystack**: RAG maturity and evaluation, but complex for simple agents
- **AutoGen**: Enterprise Microsoft backing, but still maturing
- **Semantic Kernel**: Standards-based (.NET/Python), but smaller ecosystem
- **DSPy**: Systematic optimization, but research-focused

### tea's Advantages (Today)
✓ YAML-first declarative approach (unique vs code-based DSLs)
✓ LangGraph-compatible interrupt/resume (human-in-the-loop)
✓ Lightweight architecture (perfect for edge/IoT)
✓ Checkpoint persistence (enable/disable at deployment time)
✓ Agnostic tool integration (not locked into LangChain)

### tea's Gaps (Today)
✗ Limited built-in tool library (vs CrewAI's 700+)
✗ No multi-tier memory management (vs CrewAI's entity tracking)
✗ Minimal observability hooks (vs LangSmith/Langfuse)
✗ No web/search integration (vs CrewAI's comprehensive toolkit)
✗ No code sandbox (vs frameworks with E2B/Pydantic integration)

---

## II. Market Positioning Matrix

```
         Feature Completeness
              High
                ↑
     LangGraph   │ AutoGen    Semantic Kernel
                 │  CrewAI ×
                 │   ↗
      Haystack × │  ↑
                 │ tea × DSPy
      Low        ├─────────────────────→ Edge Suitability
                 ├─────────────────────→
                Low            High
```

**tea's Position:** Lightweight + Edge-optimized (upper-right quadrant)
**Competitors in this space:** None directly (niche opportunity)

---

## III. Recommended Feature Roadmap (Phased)

### Phase 1: Core Enhancement (Months 1-2)
**Focus:** Establish feature parity with market leaders in tea's strengths

#### 1.1 Memory Management System
**Rationale:** CrewAI's multi-tier memory is a key differentiator
**Implementation:**
```python
# Add to StateGraph
memory_config = {
    "short_term": {"backend": "chroma", "k": 10},
    "long_term": {"backend": "sqlite", "table": "memories"},
    "entities": {"enabled": True, "types": ["person", "place", "concept"]}
}

graph = StateGraph({...}, memory_config=memory_config)
```

**YAML Enhancement:**
```yaml
memory:
  short_term:
    backend: chroma
    embedding_provider: openai
    max_entries: 100
  long_term:
    backend: sqlite
    retention_days: 90
  entities:
    enabled: true
    tracking: [person, place, concept, product]
```

**Effort:** 2-3 weeks
**Impact:** Enables persistent context across sessions, entity tracking

#### 1.2 Tool Registry & Definitions
**Rationale:** Standardize tool integration similar to Semantic Kernel/CrewAI
**Implementation:**
```yaml
tools:
  - name: web_search
    provider: serper_dev
    description: "Search the web for current information"
    parameters:
      query:
        type: string
        required: true
      max_results:
        type: integer
        default: 5
    auth: ${secrets.SERPER_KEY}
    timeout: 10s

  - name: local_file_read
    provider: builtin
    description: "Read local files safely"
    sandboxed: true
    allowed_paths:
      - /data/**
      - /tmp/**
```

**Effort:** 2 weeks
**Impact:** Declarative tool management, security constraints

#### 1.3 Observability Hooks
**Rationale:** Enable integration with Langfuse/LangSmith
**Implementation:**
```python
from the_edge_agent import StateGraph
from the_edge_agent.observability import LangfuseCallback

graph = StateGraph({...})
graph.add_callback(
    LangfuseCallback(
        public_key=os.getenv("LANGFUSE_PUBLIC_KEY"),
        secret_key=os.getenv("LANGFUSE_SECRET_KEY"),
        host=os.getenv("LANGFUSE_HOST")
    )
)
```

**YAML Enhancement:**
```yaml
observability:
  provider: langfuse
  config:
    public_key: ${secrets.LANGFUSE_PUBLIC_KEY}
    secret_key: ${secrets.LANGFUSE_SECRET_KEY}
    host: ${variables.LANGFUSE_HOST}
    trace_level: full  # or 'basic'
```

**Effort:** 1-2 weeks
**Impact:** Production visibility, debugging, cost tracking

---

### Phase 2: Tool Ecosystem (Months 3-4)
**Focus:** Build initial tool library for common use cases

#### 2.1 Built-in Tools
**Start with highest-impact subset:**
- Web search (SerperDev, DuckDuckGo, basic HTTP)
- File operations (read, write, directory listing - with path safety)
- HTTP client (GET, POST, with auth)
- Basic Python evaluation (limited, non-sandboxed initially)
- Local database queries (SQLite, basic)

**Example YAML:**
```yaml
actions:
  search_web:
    action: tools.web.search
    parameters:
      query: ${state.query}
      provider: serper_dev

  read_file:
    action: tools.file.read
    parameters:
      path: ${state.file_path}
      mode: safe  # Check against allowed_paths

  http_post:
    action: tools.http.post
    parameters:
      url: ${state.api_url}
      payload: ${state.data}
      headers: ${variables.headers}
```

**Effort:** 3-4 weeks
**Impact:** Self-contained workflows without external tool dependencies

#### 2.2 Community Tool Bridge
**Rationale:** Leverage existing ecosystems without duplicating work
**Implementation:**
- CrewAI tools adapter (import CrewAI tools into tea)
- LangChain tools bridge (already possible, document it)
- OpenAPI tool importer (auto-generate tools from OpenAPI specs)

```yaml
tools:
  - name: crewai_web_search
    type: external
    source: crewai
    tool_name: SerperDevTool
    config:
      api_key: ${secrets.SERPER_KEY}
```

**Effort:** 1-2 weeks
**Impact:** Instant access to 700+ CrewAI tools

---

### Phase 3: Advanced Capabilities (Months 5-6)
**Focus:** Competitive differentiation

#### 3.1 Streaming & Real-time Updates
**Rationale:** Match LangGraph's streaming modes for responsive UIs
**Implementation:**
```python
for event in graph.stream(
    state,
    streaming_mode="updates",  # deltas only
    chunk_size=1024  # For token streaming
):
    if event["type"] == "node_start":
        emit({"node": event["node"], "status": "running"})
    elif event["type"] == "node_complete":
        emit({"node": event["node"], "output": event["output"]})
    elif event["type"] == "token":
        emit({"token": event["token"]})  # LLM streaming
```

**Effort:** 2-3 weeks
**Impact:** Real-time agent interactions (web, mobile, streaming responses)

#### 3.2 Error Recovery & Retry Policies
**Rationale:** Production reliability (match LangGraph)
**Implementation:**
```yaml
nodes:
  api_call:
    action: tools.http.get
    retry_policy:
      max_attempts: 3
      initial_delay: 1s
      max_delay: 10s
      backoff: exponential
      multiplier: 2.0
      jitter: true
    fallback: fallback_node
    on_error_state: error_node
```

**Effort:** 1-2 weeks
**Impact:** Resilient production agents

#### 3.3 MCP (Model Context Protocol) Support
**Rationale:** Emerging standard for tool discovery and sharing
**Implementation:**
```yaml
mcp_servers:
  - name: python_sandbox
    type: stdio
    command: mcp-run-python
    description: "Secure Python code execution"

tools:
  - name: execute_python
    provider: mcp
    server: python_sandbox
    description: "Execute Python code in sandbox"
```

**Effort:** 2-3 weeks
**Impact:** Standards compliance, access to Pydantic/Cursor/Claude Desktop tools

---

### Phase 4: Production Hardening (Months 7-8)
**Focus:** Enterprise readiness

#### 4.1 Security & Audit
- Input validation/sanitization for YAML
- Tool permission management (Principle of Least Privilege)
- Audit logging for all operations
- Secrets management best practices (env vars, vault integration)

#### 4.2 Performance Optimization
- Async tool execution (non-blocking I/O)
- Caching for expensive operations (embeddings, retrievals)
- Parallel node execution (similar to tea's existing parallel edges)
- Resource limits per node (timeouts, memory)

#### 4.3 Documentation & Examples
- Tutorial: "Build a Customer Support Agent in 10 Minutes"
- Tutorial: "Deploy tea to IoT Device"
- Tutorial: "Integrate with Existing APIs"
- Comparison guide: "tea vs LangGraph vs CrewAI"

**Effort:** 4-5 weeks
**Impact:** Production-ready framework

---

## IV. Competitive Feature Matrix: After Roadmap

| Feature | tea (Today) | tea (Post-Roadmap) | LangGraph | CrewAI | Haystack |
|---------|------------|-------------------|-----------|--------|----------|
| **State Management** | ✓✓ | ✓✓ | ✓✓✓ | ✓ | - |
| **Human-in-the-Loop** | ✓✓ | ✓✓ | ✓✓✓ | ✓ | - |
| **Tool Library** | ✗ | ✓✓ (via bridge) | ✓ | ✓✓✓ | ✓✓ |
| **Memory Management** | ✗ | ✓✓ | ✓ | ✓✓✓ | ✓ |
| **Observability Hooks** | ✗ | ✓✓ | ✓✓✓ | ✓✓ | ✓✓ |
| **Streaming** | ✗ | ✓✓ | ✓✓✓ | ✓ | ✓ |
| **Error Recovery** | ✓ | ✓✓ | ✓✓✓ | ✓ | ✓ |
| **Edge Suitability** | ✓✓✓ | ✓✓✓ | ✗ | ✗ | ✗ |
| **YAML Configuration** | ✓✓✓ | ✓✓✓ | ✗ | ✗ | ✗ |
| **Web Integration** | ✗ | ✓✓ | ✓ | ✓✓✓ | ✓✓ |
| **Code Execution** | ✗ | ✓ (limited) | ✓ | ✓✓ | - |
| **Production Ready** | ✓✓ | ✓✓✓ | ✓✓✓ | ✓✓ | ✓✓✓ |

---

## V. Marketing & Positioning Messages

### Primary Message
> **tea: The Lightweight Agent Framework for Edge Deployment**
> Configure complex AI workflows with simple YAML. Deploy anywhere—from data centers to IoT devices.

### Secondary Messages

1. **For Data Scientists & DevOps**
   > No LangChain lock-in. No vendor infrastructure required. Your tools, your control.

2. **For IoT & Edge Teams**
   > Built for constrained environments. Minimal dependencies. Ship agents to millions of devices.

3. **For API Teams**
   > Orchestrate microservices with AI reasoning. YAML-based orchestration replaces complex code.

4. **For Compliance & Security Teams**
   > Full audit trails. No cloud requirement. Checkpoint portability enables disaster recovery.

### Comparison Taglines
- **vs LangGraph:** "Same power, simpler YAML, lighter weight"
- **vs CrewAI:** "Like CrewAI, but for edge environments"
- **vs Haystack:** "RAG optional. Agents first."

---

## VI. Technical Debt & Risks

### Current Risks
1. **Tool ecosystem imbalance:** CrewAI's 700 tools vs tea's 0
   - Mitigation: Tool bridge strategy (Phase 2)
   - Timeline: 4-6 weeks to MVP

2. **Observability gap:** No native tracing vs LangSmith/Langfuse
   - Mitigation: Callback infrastructure (Phase 1)
   - Timeline: 2 weeks to basic integration

3. **Memory limitations:** No entity tracking or RAG-integrated memory
   - Mitigation: ChromaDB + SQLite integration (Phase 1)
   - Timeline: 3 weeks to MVP

4. **Performance unknown:** No benchmarks vs CrewAI (5.76x claim) or LangGraph
   - Mitigation: Establish baseline metrics, optimize critical paths
   - Timeline: Ongoing (start Month 3)

### Opportunities
1. **First mover in edge AI:** No competitor in lightweight orchestration space
2. **YAML DSL adoption:** DevOps teams already familiar with declarative configuration
3. **Checkpoint portability:** Unique ability to migrate between environments
4. **Tool ecosystem partnership:** Partner with CrewAI, LangChain for mutual benefit

---

## VII. Success Metrics & KPIs

### Phase 1 Completion (Month 2)
- [ ] Memory system MVP (short + long term)
- [ ] 10+ built-in tools functional
- [ ] Langfuse integration working
- [ ] **KPI:** 80% feature parity with LangGraph core

### Phase 2 Completion (Month 4)
- [ ] CrewAI tool bridge functional
- [ ] 50+ tools accessible (via bridge + built-in)
- [ ] 3 end-to-end example applications
- [ ] **KPI:** 400+ tools available (100+ built-in candidates + CrewAI bridge)

### Phase 3 Completion (Month 6)
- [ ] Streaming API launched
- [ ] MCP client support
- [ ] Performance benchmarks published
- [ ] **KPI:** 50% of LangGraph's feature set with 10% of the complexity

### Phase 4 Completion (Month 8)
- [ ] Security audit completed
- [ ] 5 production case studies
- [ ] 1.0 release candidate
- [ ] **KPI:** Production-ready, enterprise-tested

---

## VIII. Team & Resource Requirements

### Core Team
- **1x Framework Architect** (full-time): Overall design, API consistency
- **2x Backend Engineers** (full-time): Tool system, memory, observability
- **1x DevOps/Cloud** (part-time): Deployment, benchmarking, security
- **1x Developer Advocate** (part-time): Examples, documentation, community

### Part-time Support
- **Security consultant:** 2 weeks (Phase 4)
- **Performance engineer:** 2-3 weeks (Phase 3-4)
- **Technical writer:** 4 weeks (Phase 4)

**Total Effort:** ~8-10 person-months over 8 months

---

## IX. Go-to-Market Strategy

### Phase 1: Build Community (Months 1-4)
- Launch memory/tool features incrementally
- Weekly blog posts: "Building X with tea"
- Reddit/HackerNews launch (Month 2-3)
- Engage with edge AI communities (IoT, robotics, embedded systems)

### Phase 2: Establish Credibility (Months 5-6)
- Publish benchmarks: tea vs LangGraph vs CrewAI
- 3 production case studies
- Conference talks (LLM/AI conferences)
- Tool ecosystem partnership announcements

### Phase 3: Scale (Months 7+)
- GitHub trending (target 1K+ stars Month 6)
- Enterprise pilot programs
- Managed hosting option (cloud-optional)
- Certified partner ecosystem

---

## X. Decision Checkpoints

### Month 1 Checkpoint
**Question:** Is memory system architecture aligned with market needs?
**Metric:** Community feedback on Phase 1 memory RFC
**Go/No-Go:** Proceed with tool system if positive; adjust memory design if concerns

### Month 3 Checkpoint
**Question:** Does CrewAI tool bridge work as expected?
**Metric:** 100+ tools functional via bridge
**Go/No-Go:** Proceed to streaming if bridge stable; invest in built-in tools if bridge unstable

### Month 5 Checkpoint
**Question:** Are benchmarks showing competitive performance?
**Metric:** Within 2x of LangGraph on state machine tasks
**Go/No-Go:** Proceed to Phase 4 if performance acceptable; delay if optimization needed

---

## XI. Dependencies & Assumptions

### External Dependencies
- CrewAI API stability (for tool bridge)
- Langfuse API (for observability)
- Pydantic MCP server availability (for code execution)

### Market Assumptions
- Edge AI/IoT market growing (✓ validated by searches)
- YAML-first approach appealing to DevOps teams (? unvalidated)
- Users value simplicity over feature completeness (? unvalidated)

### Technical Assumptions
- ThreadPoolExecutor sufficient for parallel edges (✓ current implementation)
- Checkpoint portability valuable for migrations (? unvalidated)
- CrewAI tools compatible with tea's tool interface (? unvalidated)

---

## XII. Conclusion: tea's Strategic Window

**Now (December 2025)** is the optimal moment for this strategy because:

1. **Frameworks converging on common patterns** (state, tools, memory)
   - tea can standardize quickly

2. **Edge AI emerging as serious workload** (IoT, local deployment)
   - No competitor optimized for this yet

3. **Tool ecosystems maturing** (CrewAI 700+, Semantic Kernel standards)
   - Can leverage via bridges rather than build-from-scratch

4. **Developer fatigue with complexity** (LangGraph API changes, configuration overhead)
   - YAML simplicity is differentiated value

5. **Observability standards consolidating** (Langfuse, LangSmith, MCP)
   - Can build plug-in architecture rather than proprietary

**Risk:** Waiting 12 months risks 2-3 competitors launching edge-optimized variants.

**Opportunity:** Establish market leadership in edge+lightweight+YAML by Q2 2026.

---

**Document Version:** 1.0
**Author:** Framework Analysis (December 2025)
**Status:** Ready for Architecture Review & Planning
