# Declarative Agent Frameworks: A Comparative Analysis of Docker cagent and The Edge Agent

**Fabricio Ceolin**

*Independent Researcher*

https://www.linkedin.com/in/fabceolin/

---

## Abstract

The emergence of agentic AI represents a fundamental shift in software development, moving from simple language models to autonomous systems capable of planning, reasoning, and executing complex tasks. This article presents a comparative analysis of two declarative YAML-based agent frameworks: Docker cagent and The Edge Agent (TEA). We examine their architectural approaches, orchestration models, security mechanisms, and deployment strategies. Our analysis reveals that while cagent excels in containerized security isolation and IDE integration, TEA offers superior control over workflow orchestration, multi-agent coordination primitives, and edge computing deployment options. This comparison provides guidance for practitioners selecting the appropriate framework for their specific use cases.

**Keywords:** AI Agents, Declarative Configuration, YAML, Orchestration, LangGraph, Docker, Edge Computing

---

## 1. Introduction

The transition from monolithic language model interactions to orchestrated agent systems has created demand for frameworks that simplify agent development while maintaining flexibility and security. Two notable approaches have emerged in this space: Docker cagent, which extends the container paradigm to "containerize intelligence," and The Edge Agent (TEA), which applies state graph principles inspired by LangGraph to edge computing environments.

Both frameworks adopt declarative YAML configuration as their primary interface, eliminating the need for imperative orchestration code. However, their underlying philosophies, architectural decisions, and target use cases differ substantially.

This article provides a systematic comparison across ten dimensions: configuration structure, orchestration models, multi-agent patterns, tool integration, memory systems, model providers, security, persistence, distribution, and IDE integration. We conclude with recommendations for framework selection based on specific requirements.

## 2. Configuration Paradigms

### 2.1 Docker cagent

Docker cagent prioritizes complete abstraction. A minimal agent requires only a model specification and instructions:

```yaml
name: simple-assistant
model: gpt-4o
instructions: |
  You are a helpful coding assistant.
  Always explain your reasoning before providing code.
```

The configuration schema comprises five primary sections:

| Component | Purpose |
|-----------|---------|
| `agents` | Personas, instructions, sub-agent definitions |
| `models` | LLM providers and inference parameters |
| `toolsets` | Native tools and MCP integrations |
| `rag` | Knowledge sources and retrieval strategies |
| `metadata` | Authorship, versioning, licensing |

### 2.2 The Edge Agent

TEA requires explicit state schema definition and workflow nodes:

```yaml
name: simple-assistant
state_schema:
  input: str
  output: str

settings:
  llm:
    model: gpt-4o

nodes:
  - name: process
    uses: llm.call
    with:
      messages:
        - role: user
          content: "{{ state.input }}"
    output: output

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

TEA's configuration is more verbose but provides explicit control over:

- State shape and typing via `state_schema`
- Execution flow via `nodes` and `edges`
- Template interpolation via Jinja2 (`{{ state.key }}`)
- Inline code execution in `run:` blocks

### 2.3 Comparison

| Aspect | Docker cagent | TEA |
|--------|---------------|-----|
| Minimum config | 2 fields | Schema + 1 node |
| Code execution | Not allowed inline | Python/Lua/Prolog inline |
| Template engine | Simple variables | Full Jinja2 |
| Learning curve | Lower | Higher |
| Granular control | Limited | Extensive |

## 3. Orchestration Models

### 3.1 Agent Hierarchies vs State Graphs

Docker cagent models orchestration as **agent hierarchies**. A root agent analyzes requests and delegates to specialized sub-agents:

```yaml
name: coordinator
sub_agents:
  - researcher
  - writer
  - reviewer
instructions: |
  Analyze the user request and delegate to appropriate specialists.
  Use transfer_task to assign work to sub-agents.
```

TEA models orchestration as **directed state graphs**, inspired by LangGraph:

```yaml
nodes:
  - name: research
    run: |
      return {"findings": perform_research(state["topic"])}

  - name: write
    run: |
      return {"draft": write_article(state["findings"])}

  - name: review
    uses: llm.call
    with:
      messages:
        - role: user
          content: "Review this draft: {{ state.draft }}"

edges:
  - from: __start__
    to: research
  - from: research
    to: write
  - from: write
    to: review
  - from: review
    to: __end__
    condition: "{{ state.approved }}"
  - from: review
    to: write
    condition: "{{ not state.approved }}"
```

### 3.2 Parallel Execution

Docker cagent handles parallelism implicitly through sub-agent delegation.

TEA provides explicit fan-out/fan-in patterns with configurable strategies:

```yaml
settings:
  parallel:
    strategy: thread  # thread | process | remote
    max_workers: 4

edges:
  - from: start
    to: [worker_a, worker_b, worker_c]  # Fan-out
  - from: [worker_a, worker_b, worker_c]
    to: aggregator  # Fan-in
```

Fan-in nodes receive a `parallel_results` parameter containing states from all parallel branches.

## 4. Multi-Agent Coordination

### 4.1 Docker cagent Mechanisms

Docker cagent provides two collaboration patterns:

**Hierarchical Delegation**: Parent agent uses `transfer_task` to assign work to children. Children execute in isolated contexts and report results back.

**Conversation Handoff**: Complete control transfer to another agent. Original agent remains inactive until control is explicitly returned.

### 4.2 TEA Agent-to-Agent Protocol

TEA implements a more sophisticated A2A (Agent-to-Agent) protocol:

```yaml
settings:
  a2a:
    agent_id: worker-1
    namespace: production

nodes:
  - name: send_result
    uses: a2a.send
    with:
      to: coordinator
      message:
        type: task_complete
        payload: "{{ state.result }}"

  - name: await_instruction
    uses: a2a.receive
    with:
      from: coordinator
      timeout: 30s
```

TEA's multi-agent primitives include:

| Action | Description |
|--------|-------------|
| `agent.dispatch` | Single agent task delegation |
| `agent.parallel` | Parallel dispatch with voting/merge |
| `agent.sequential` | Chain agents with output threading |
| `agent.coordinate` | Leader-worker pattern with synthesis |
| `a2a.discover` | Find agents in namespace |
| `a2a.broadcast` | Message all agents |
| `a2a.state.get/set` | Shared state with optimistic locking |

## 5. Tool Integration

### 5.1 Docker cagent Toolsets

Docker cagent provides six native toolsets:

| Toolset | Capabilities |
|---------|-------------|
| Filesystem | Read, write, edit, search files |
| Shell | Execute system commands |
| Think | Internal reasoning scratchpad |
| Todo | Task list management |
| Memory | SQLite-based persistence |
| Fetch | HTTP/HTTPS requests |

External tools integrate via Model Context Protocol (MCP):

```yaml
toolsets:
  - name: github
    type: mcp
    transport: docker
    image: docker.io/mcp/github
```

### 5.2 TEA Actions Registry

TEA provides 276 built-in actions across 51 modules:

| Category | Count | Examples |
|----------|-------|----------|
| LLM | 4 | `llm.call`, `llm.stream`, `llm.tools` |
| Memory | 7 | `memory.store`, `ltm.store`, `cache.wrap` |
| Graph DB | 25 | `graph.store_entity`, `graph.query` |
| Multi-Agent | 15 | `agent.dispatch`, `a2a.send` |
| RAG | 4 | `rag.search`, `rag.embed` |
| Web | 4 | `web.scrape`, `web.crawl` |
| Planning | 4 | `plan.decompose`, `plan.execute` |
| Reasoning | 7 | `reason.cot`, `reason.react` |

Custom actions register via Python:

```python
def register_actions(registry, engine):
    def my_action(state, param1, **kwargs):
        return {"result": process(param1)}
    registry['my_action'] = my_action
```

TEA also provides native bridges to popular frameworks:

- CrewAI: `agent.crewai_delegate`
- LlamaIndex: `llamaindex.query`, `llamaindex.router`
- DSPy: `dspy.cot`, `dspy.compile`
- Mem0: `mem0.add`, `mem0.search`

## 6. Memory and RAG Systems

### 6.1 Docker cagent RAG

Docker cagent implements sophisticated RAG with multiple strategies:

| Strategy | Description |
|----------|-------------|
| Chunked Embeddings | Semantic search via vector similarity |
| BM25 | Keyword-based term frequency retrieval |
| Hybrid Search | Combined vectors + BM25 with RRF fusion |
| Reranking | LLM-based result re-scoring |

Notable feature: **code-aware chunking** that respects programming language structure during indexing.

### 6.2 TEA Long-Term Memory

TEA provides eight LTM backend implementations:

| Backend | Use Case | Scale Target |
|---------|----------|--------------|
| sqlite | Local development | Single-node |
| duckdb | Analytics-heavy workloads | 10GB+ |
| sqlalchemy | Database-agnostic | Varies |
| hierarchical | Multi-tenant production | 10GB-100GB+ |
| turso | Edge-native HTTP | Distributed |
| firestore | Serverless | Firebase scale |

The **Entity Hierarchy** system (`TEA-LTM-013`) enables O(1) hierarchical queries using closure tables:

```python
hierarchy = EntityHierarchy(
    levels=["org", "project", "user", "session"],
    url="postgresql://..."
)

# O(1) query for all entries under an organization
result = hierarchy.get_entries_for_entity("org", "acme")
```

## 7. Security Models

### 7.1 Docker cagent: microVM Isolation

Docker cagent provides **hardware-level isolation** via microVMs:

- Each sandbox runs in a dedicated microVM with isolated kernel
- Private Docker daemon enables autonomous container builds
- Only project directory is mounted
- Disposable environments enable "YOLO mode" (unsupervised execution)

This architecture offers the strongest security boundary, suitable for running untrusted agent code.

### 7.2 TEA: Process-Level Sandboxing

TEA operates within the Python runtime with optional sandboxing:

| Execution Mode | Isolation Level |
|----------------|-----------------|
| Python `exec()` | None (full runtime access) |
| Lua sandbox | Dangerous globals removed |
| Prolog sandbox | Restricted predicates |
| RestrictedPython | Sandboxed code actions |

TEA assumes **trust in the YAML author**. The security model focuses on:

- Jinja2 sandboxed templates (`__import__` blocked)
- Authentication providers (Firebase, JWT, API Key)
- Secrets management (AWS, Azure, GCP, env)

```yaml
settings:
  auth:
    provider: firebase
    required: true
  secrets:
    backend: aws
    region: us-east-1
```

### 7.3 Security Comparison

| Aspect | Docker cagent | TEA |
|--------|---------------|-----|
| Isolation boundary | Hardware (hypervisor) | Process |
| Untrusted code | Safe | Not recommended |
| YOLO mode | Supported | Not applicable |
| Auth providers | Not built-in | Firebase, JWT, API Key |
| Secrets management | 1Password (roadmap) | AWS, Azure, GCP, env |

## 8. Persistence and Human-in-the-Loop

### 8.1 Docker cagent: Cassette Testing

Docker cagent introduces **cassettes** for deterministic testing:

```bash
# Record mode: capture all API calls
cagent run --record cassette.yaml agent.yaml

# Replay mode: deterministic execution
cagent test --cassette cassette.yaml agent.yaml
```

This enables CI/CD integration without API costs or LLM variability.

### 8.2 TEA: Checkpoint System

TEA provides checkpoints for human-in-the-loop workflows:

```yaml
config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [review_node]
  interrupt_after: [validation_node]
```

Execution pauses at interrupt points, allowing human review:

```python
# First execution - pauses at review_node
events = list(graph.invoke({"document": doc}))
checkpoint = events[-1]["checkpoint_path"]

# Resume after human approval
events = list(graph.invoke(
    {"approved": True},
    checkpoint=checkpoint
))
```

## 9. Distribution and Deployment

### 9.1 Docker cagent: OCI Artifacts

Docker cagent treats agents as OCI artifacts:

```bash
# Publish to Docker Hub
cagent push docker.io/myuser/my-agent:v1.0

# Run from registry
cagent run docker.io/myuser/my-agent:v1.0
```

This leverages existing Docker infrastructure for versioning, distribution, and access control.

### 9.2 TEA: Multi-Format Distribution

TEA supports multiple distribution formats:

| Format | Size | Use Case |
|--------|------|----------|
| PyPI package | ~5MB | Standard Python deployment |
| AppImage | ~50MB | Linux self-contained |
| AppImage + LLM | ~2GB | Offline inference |
| WASM | ~10MB | Browser execution |

The LLM-bundled AppImage enables fully offline operation:

```bash
# Download and run offline
./tea-python-llm-gemma3-1b.AppImage run agent.yaml \
  --input '{"question": "What is TEA?"}'
```

WASM distribution enables browser-based agents:

```javascript
import { initTeaLlm, executeLlmYaml } from 'tea-wasm-llm';

await initTeaLlm();
const result = await executeLlmYaml(workflow, { input: "..." });
```

## 10. IDE Integration

### 10.1 Docker cagent: Agent Client Protocol

Docker cagent implements ACP (Agent Client Protocol), co-developed with JetBrains and Zed:

- Synchronized project view with editor
- Cursor context awareness
- Direct buffer editing
- Native support in Zed, adapters for Neovim/IntelliJ

### 10.2 TEA: CLI-First Approach

TEA focuses on CLI and API interfaces without dedicated IDE protocol support. Integration occurs through:

- CLI invocation from editor terminals
- REST API for programmatic access
- Opik observability dashboard

## 11. Decision Framework

Based on our analysis, we recommend framework selection according to the following criteria:

### Choose Docker cagent when:

- **Security is paramount**: microVM isolation for untrusted agents
- **IDE integration matters**: ACP provides superior editor experience
- **DevOps familiarity**: Team already uses Docker infrastructure
- **RAG is primary use case**: Superior code-aware chunking
- **Deterministic testing needed**: Cassette mechanism enables CI/CD
- **Minimal code desired**: Pure declarative configuration

### Choose The Edge Agent when:

- **Edge computing target**: AppImage/WASM for offline deployment
- **Complex workflows**: StateGraph provides precise orchestration control
- **Multi-agent coordination**: A2A protocol with discovery and shared state
- **Multi-tenant hierarchies**: Entity Hierarchy with O(1) queries
- **Neurosymbolic AI**: Prolog/Lua integration for symbolic reasoning
- **Human-in-the-loop critical**: Checkpoint system with interrupt points
- **Framework bridges needed**: Native CrewAI, LlamaIndex, DSPy integration

## 12. Conclusion

Docker cagent and The Edge Agent represent two distinct philosophies for declarative agent development. Docker cagent extends the container paradigm to AI, prioritizing security isolation, distribution standardization, and zero-code configuration. TEA adapts state graph principles from LangGraph, prioritizing orchestration control, multi-agent coordination primitives, and edge deployment flexibility.

Neither framework is universally superior. Docker cagent excels in enterprise environments requiring strong security boundaries and familiar DevOps workflows. TEA excels in scenarios demanding fine-grained workflow control, offline capability, or integration with existing AI frameworks.

As agentic AI matures, we anticipate convergence in capabilities. MCP adoption may standardize tool integration across frameworks. ACP may become the universal IDE protocol. The choice today should consider not only current requirements but also the trajectory of each ecosystem.

## 13. References

- [Docker cagent Documentation](https://docs.docker.com/ai/cagent/) - Official Docker cagent reference
- [Docker cagent GitHub](https://github.com/docker/cagent) - Source repository
- [The Edge Agent Documentation](https://fabceolin.github.io/the_edge_agent/) - Official TEA documentation
- [The Edge Agent GitHub](https://github.com/fabceolin/the_edge_agent) - Source repository
- [Model Context Protocol](https://modelcontextprotocol.io/) - MCP specification
- [Agent Client Protocol](https://docs.docker.com/ai/cagent/integrations/acp/) - ACP specification
- [LangGraph](https://langchain-ai.github.io/langgraph/) - State graph inspiration for TEA
- [Docker Sandboxes Architecture](https://docs.docker.com/ai/sandboxes/architecture/) - microVM isolation details
