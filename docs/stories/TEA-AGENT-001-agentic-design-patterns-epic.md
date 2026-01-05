# Epic TEA-AGENT-001: Agentic Design Patterns - Built-in Primitives

## Status

**Ready for Development**

_Status updated: 2026-01-05 - QA Gate passed with 186 test scenarios, no blockers identified._

## Executive Summary

This epic implements 5 high-priority agentic design patterns as built-in primitives in The Edge Agent (both Python and Rust), based on the [Agentic Design Patterns](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns) framework. These patterns enable sophisticated multi-agent workflows without requiring users to manually wire complex graph topologies.

| Metric | Value |
|--------|-------|
| **Total Stories** | 5 |
| **Target Patterns** | Multi-Agent, Reflection, Planning, Reasoning, A2A |
| **Priority** | P0 (Critical for agentic workflows) |
| **Runtime Targets** | Python + Rust (feature parity when possible) |
| **Integration** | MCP, CrewAI, LangChain bridges (when possible) |

---

## Epic Goal

Provide built-in agentic design pattern primitives that enable YAML agent developers to build production-grade multi-agent systems with reflection loops, planning capabilities, structured reasoning, and inter-agent communication without writing complex graph wiring code.

---

## Epic Description

### Business Context

The Edge Agent currently provides excellent low-level graph primitives (nodes, edges, parallel fan-out/fan-in, checkpoints) but lacks higher-level abstractions for common agentic patterns. Users must manually implement:

- Multi-agent coordination with specialized agents
- Self-correction loops with evaluation and retry
- Task decomposition and planning
- Structured reasoning (Chain-of-Thought, ReAct)
- Agent-to-agent messaging and delegation

This epic elevates these patterns to first-class built-in features, reducing YAML complexity and enabling best practices by default.

### Gap Analysis: Current State vs. Agentic Design Patterns Book

| Chapter | Pattern | Current State | Target State |
|---------|---------|---------------|--------------|
| **7** | Multi-Agent Collaboration | Manual parallel edges + custom fan-in | Built-in `agent.dispatch`, `agent.coordinate`, `agent.parallel` |
| **4** | Reflection/Self-Correction | Manual evaluate→correct edge loops | Built-in `reflection.loop` action with configurable evaluator |
| **6** | Planning | User-designed LLM prompts + parsing | Built-in `plan.decompose`, `plan.execute`, `plan.replan` |
| **17** | Reasoning Techniques | No structured reasoning support | Built-in `reason.cot`, `reason.react`, `reason.self_correct` |
| **15** | Inter-Agent Communication | No agent messaging | Built-in `a2a.send`, `a2a.receive`, `a2a.broadcast` |

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        AGENTIC PATTERN PRIMITIVES                            │
│                     (New Built-in Actions & Node Types)                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐              │
│  │  Multi-Agent    │  │   Reflection    │  │    Planning     │              │
│  │  Coordinator    │  │     Loop        │  │   Primitive     │              │
│  │                 │  │                 │  │                 │              │
│  │ agent.dispatch  │  │ reflection.loop │  │ plan.decompose  │              │
│  │ agent.coordinate│  │ reflection.eval │  │ plan.execute    │              │
│  │ agent.parallel  │  │ reflection.fix  │  │ plan.replan     │              │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘              │
│           │                    │                    │                        │
│           └────────────────────┴────────────────────┘                        │
│                               │                                              │
│                    ┌──────────┴──────────┐                                   │
│                    │  StateGraph Engine  │                                   │
│                    │  (Existing Core)    │                                   │
│                    └──────────┬──────────┘                                   │
│                               │                                              │
│  ┌─────────────────┐  ┌──────┴──────────┐  ┌─────────────────┐              │
│  │    Reasoning    │  │ Existing Actions│  │    A2A Comm     │              │
│  │   Techniques    │  │                 │  │                 │              │
│  │                 │  │ llm.call        │  │ a2a.send        │              │
│  │ reason.cot      │  │ llm.tools       │  │ a2a.receive     │              │
│  │ reason.react    │  │ memory.*        │  │ a2a.broadcast   │              │
│  │ reason.correct  │  │ web.*           │  │ a2a.delegate    │              │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘              │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘

                                    │
                                    ▼
                    ┌─────────────────────────────┐
                    │  External Tool Bridges       │
                    │  (Integration Layer)         │
                    │                              │
                    │  tools.mcp   tools.crewai   │
                    │  tools.langchain            │
                    └─────────────────────────────┘
```

### Existing System Context

**Current Relevant Functionality:**
- `StateGraph` class - core graph execution engine
- `add_parallel_edge()` - parallel fan-out/fan-in
- `llm.call`, `llm.tools` - LLM integration
- `tools.mcp`, `tools.crewai`, `tools.langchain` - external tool bridges
- `retry.loop` action - basic retry with circuit breaker
- `validate.schema` - JSON schema validation

**Technology Stack:**
- Python 3.11+ with existing action framework
- Rust with existing YAML engine
- Jinja2/Tera template processing
- MCP, CrewAI, LangChain bridges (existing)

**Integration Points:**
- `actions/` directory - new action modules
- `yaml_engine.py` / `yaml.rs` - new node type handling
- `stategraph.py` / `stategraph.rs` - coordination primitives

---

## Stories

### Story 1: TEA-AGENT-001.1 - Multi-Agent Collaboration Primitives

**Status:** Draft

**Description:** Implement built-in primitives for multi-agent collaboration including coordinator patterns, agent dispatch, and parallel agent execution.

**Scope:**
- New actions: `agent.dispatch`, `agent.coordinate`, `agent.parallel`, `agent.sequential`
- Agent definition in YAML with specialized roles
- Coordinator pattern with consensus/voting aggregation
- Integration with existing parallel edge infrastructure

**YAML Syntax Preview:**

```yaml
name: multi-agent-research
settings:
  agents:
    researcher:
      system_prompt: "You are a research specialist..."
      model: gpt-4
    critic:
      system_prompt: "You are a critical reviewer..."
      model: gpt-4
    synthesizer:
      system_prompt: "You synthesize multiple perspectives..."
      model: gpt-4

nodes:
  - name: parallel_research
    action: agent.parallel
    with:
      agents: [researcher, critic]
      task: "{{ state.research_question }}"
      aggregation: collect  # or: vote, consensus, first

  - name: synthesize
    action: agent.dispatch
    with:
      agent: synthesizer
      task: "Synthesize these perspectives: {{ state.parallel_results | tojson }}"
```

**Acceptance Criteria:**
1. `agent.dispatch` executes single agent with task and returns result
2. `agent.parallel` executes multiple agents in parallel with configurable aggregation
3. `agent.sequential` chains agents with state threading
4. `agent.coordinate` implements coordinator pattern with retry on disagreement
5. Agent definitions support model, system_prompt, tools, and temperature
6. Integration with `tools.mcp`, `tools.crewai`, `tools.langchain` bridges
7. Python implementation complete with tests
8. Rust implementation complete with feature parity (when possible)

**Link:** `docs/stories/TEA-AGENT-001.1-multi-agent-collaboration.md`

---

### Story 2: TEA-AGENT-001.2 - Reflection Loop Primitive

**Status:** Draft

**Description:** Implement built-in reflection loop primitive that enables iterative self-correction with configurable evaluators.

**Scope:**
- New actions: `reflection.loop`, `reflection.evaluate`, `reflection.correct`
- Configurable evaluation strategies (LLM-based, schema-based, custom)
- Maximum iteration limits with circuit breaker
- Integration with existing `validate.schema` action

**YAML Syntax Preview:**

```yaml
nodes:
  - name: generate_with_reflection
    action: reflection.loop
    with:
      generator:
        action: llm.call
        prompt: "Generate a JSON response for: {{ state.request }}"
      evaluator:
        type: schema  # or: llm, custom
        schema:
          type: object
          required: [name, email]
      corrector:
        action: llm.call
        prompt: "Fix this JSON based on errors: {{ state.errors }}"
      max_iterations: 3
      on_failure: return_best  # or: raise, return_last
```

**Acceptance Criteria:**
1. `reflection.loop` executes generate→evaluate→correct cycle
2. Configurable evaluator types: `schema`, `llm`, `custom`
3. Maximum iteration limit with circuit breaker
4. State tracks iteration count, errors, and improvement history
5. On-failure strategies: `return_best`, `raise`, `return_last`
6. Python implementation complete with tests
7. Rust implementation complete with feature parity

**Link:** `docs/stories/TEA-AGENT-001.2-reflection-loop.md`

---

### Story 3: TEA-AGENT-001.3 - Planning & Decomposition Primitive

**Status:** Draft

**Description:** Implement built-in planning primitive that decomposes complex tasks into subtasks and executes them.

**Scope:**
- New actions: `plan.decompose`, `plan.execute`, `plan.replan`, `plan.status`
- Task dependency graph generation
- Parallel execution of independent subtasks
- Re-planning on subtask failure

**YAML Syntax Preview:**

```yaml
nodes:
  - name: plan_and_execute
    action: plan.decompose
    with:
      goal: "{{ state.user_goal }}"
      planner:
        model: gpt-4
        strategy: hierarchical  # or: flat, iterative
      executor:
        parallel: true  # Execute independent tasks in parallel
        max_concurrent: 3
      on_subtask_failure: replan  # or: skip, abort, retry

  - name: check_progress
    action: plan.status
    with:
      include_completed: false
```

**Acceptance Criteria:**
1. `plan.decompose` generates subtask list with dependencies
2. `plan.execute` runs subtasks respecting dependency order
3. Independent subtasks execute in parallel when `parallel: true`
4. `plan.replan` triggers re-planning when subtask fails
5. `plan.status` returns current progress and remaining tasks
6. Task state persists across checkpoints for resume
7. Python implementation complete with tests
8. Rust implementation complete with feature parity

**Link:** `docs/stories/TEA-AGENT-001.3-planning-primitive.md`

---

### Story 4: TEA-AGENT-001.4 - Reasoning Techniques Primitives

**Status:** Draft

**Description:** Implement built-in reasoning technique primitives including Chain-of-Thought, ReAct, and self-correction patterns.

**Scope:**
- New actions: `reason.cot`, `reason.react`, `reason.self_correct`, `reason.decompose`
- Structured output with reasoning traces
- ReAct pattern with thought→action→observation loop
- Integration with tool calling via `llm.tools`

**YAML Syntax Preview:**

```yaml
nodes:
  - name: solve_problem
    action: reason.cot
    with:
      problem: "{{ state.problem }}"
      model: gpt-4
      output_format:
        thinking: str  # Chain of thought
        answer: str    # Final answer
      few_shot_examples:
        - problem: "What is 2+2?"
          thinking: "I need to add 2 and 2. 2+2=4."
          answer: "4"

  - name: research_and_act
    action: reason.react
    with:
      goal: "{{ state.research_goal }}"
      tools:
        - web.search
        - web.scrape
      max_steps: 5
      output_format:
        thought: str
        action: str
        observation: str
        final_answer: str
```

**Acceptance Criteria:**
1. `reason.cot` implements Chain-of-Thought with structured output
2. `reason.react` implements ReAct loop with tool integration
3. `reason.self_correct` adds self-evaluation step after reasoning
4. `reason.decompose` breaks complex problems into sub-problems
5. Structured output preserves full reasoning trace
6. Integration with existing `llm.tools` action
7. Python implementation complete with tests
8. Rust implementation complete with feature parity

**Link:** `docs/stories/TEA-AGENT-001.4-reasoning-techniques.md`

---

### Story 5: TEA-AGENT-001.5 - Inter-Agent Communication (A2A)

**Status:** Draft

**Description:** Implement built-in inter-agent communication primitives for message passing, delegation, and shared memory.

**Scope:**
- New actions: `a2a.send`, `a2a.receive`, `a2a.broadcast`, `a2a.delegate`
- Message queue abstraction (in-memory default, pluggable backends)
- Shared memory space for agent coordination
- Agent discovery and capability advertisement

**YAML Syntax Preview:**

```yaml
name: coordinator-agent
settings:
  a2a:
    enabled: true
    namespace: research-team
    discovery: broadcast  # or: registry, static

nodes:
  - name: delegate_task
    action: a2a.delegate
    with:
      to: specialist-agent
      task: "{{ state.subtask }}"
      timeout: 60s
      on_timeout: fallback_local

  - name: gather_results
    action: a2a.receive
    with:
      from: [specialist-agent, reviewer-agent]
      timeout: 30s
      require_all: true

  - name: announce_completion
    action: a2a.broadcast
    with:
      namespace: research-team
      message:
        type: completion
        result: "{{ state.final_result }}"
```

**Acceptance Criteria:**
1. `a2a.send` sends message to specific agent
2. `a2a.receive` waits for messages with timeout
3. `a2a.broadcast` sends to all agents in namespace
4. `a2a.delegate` sends task and waits for result (request/response)
5. In-memory message queue for single-process execution
6. Shared state namespace for coordination
7. Python implementation complete with tests
8. Rust implementation: design document only (complex FFI)

**Link:** `docs/stories/TEA-AGENT-001.5-inter-agent-communication.md`

---

## Dependency Graph

```mermaid
flowchart TD
    subgraph EXISTING["Existing Infrastructure"]
        SG[StateGraph Engine]
        PE[Parallel Edges]
        LLM[llm.call / llm.tools]
        TB[Tools Bridge]
        VAL[validate.schema]
    end

    subgraph P0["Phase 0: Foundation"]
        S2["TEA-AGENT-001.2<br/>Reflection Loop<br/><span style='color:green'>Uses: validate.schema</span>"]
        S4["TEA-AGENT-001.4<br/>Reasoning Techniques<br/><span style='color:green'>Uses: llm.tools</span>"]
    end

    subgraph P1["Phase 1: Coordination"]
        S1["TEA-AGENT-001.1<br/>Multi-Agent Collaboration<br/><span style='color:blue'>Uses: Parallel Edges</span>"]
        S3["TEA-AGENT-001.3<br/>Planning Primitive<br/><span style='color:blue'>Uses: Multi-Agent</span>"]
    end

    subgraph P2["Phase 2: Communication"]
        S5["TEA-AGENT-001.5<br/>Inter-Agent Communication<br/><span style='color:orange'>Complex</span>"]
    end

    VAL --> S2
    LLM --> S4
    TB --> S4

    PE --> S1
    S1 --> S3
    S2 --> S3

    S1 --> S5
    S3 --> S5

    classDef existing fill:#E6E6FA,stroke:#4B0082
    classDef p0 fill:#90EE90,stroke:#228B22
    classDef p1 fill:#87CEEB,stroke:#4682B4
    classDef p2 fill:#FFB366,stroke:#FF8C00

    class SG,PE,LLM,TB,VAL existing
    class S2,S4 p0
    class S1,S3 p1
    class S5 p2
```

### Dependency Matrix

| Story | Blocked By | Blocks | Can Parallelize With |
|-------|------------|--------|----------------------|
| **001.2 Reflection** | validate.schema | 001.3 Planning | 001.4 Reasoning |
| **001.4 Reasoning** | llm.tools | 001.3 Planning | 001.2 Reflection |
| **001.1 Multi-Agent** | Parallel Edges | 001.3, 001.5 | None |
| **001.3 Planning** | 001.1, 001.2 | 001.5 | None |
| **001.5 A2A** | 001.1, 001.3 | None | None |

---

## Recommended Execution Order

### Sprint 1: Foundation (Week 1-2)

**Goal:** Establish reasoning and reflection primitives

| Order | Story | Rationale |
|-------|-------|-----------|
| 1 | **001.2 Reflection Loop** | Builds on existing validate.schema, enables self-correction |
| 2 | **001.4 Reasoning Techniques** | Builds on existing llm.tools, enables structured reasoning |

**Deliverable:** YAML agents can self-correct and use CoT/ReAct patterns.

### Sprint 2: Coordination (Week 3-4)

**Goal:** Multi-agent and planning capabilities

| Order | Story | Rationale |
|-------|-------|-----------|
| 3 | **001.1 Multi-Agent** | Foundation for complex agent workflows |
| 4 | **001.3 Planning** | Combines multi-agent with reflection |

**Deliverable:** YAML agents can coordinate multiple specialists and decompose tasks.

### Sprint 3: Communication (Week 5-6)

**Goal:** Full agent-to-agent communication

| Order | Story | Rationale |
|-------|-------|-----------|
| 5 | **001.5 A2A Communication** | Enables distributed agent systems |

**Deliverable:** YAML agents can communicate and delegate across processes.

---

## Compatibility Requirements

- [x] Existing YAML agents continue to work unchanged
- [x] Existing `llm.call`, `llm.tools` actions unchanged
- [x] Existing parallel edge behavior preserved
- [x] Existing checkpoint/interrupt behavior preserved
- [x] No breaking changes to StateGraph API
- [x] Optional feature flags for each primitive
- [x] Graceful degradation when dependencies unavailable

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Reflection infinite loops | Medium | High | Max iterations + circuit breaker |
| Multi-agent state conflicts | Medium | Medium | Deep copy per agent + merge strategy |
| Planning over-decomposition | Low | Medium | Max depth + token budget |
| A2A message loss | Medium | High | Acknowledgment + retry |
| Rust parity complexity | High | Medium | Design doc first, implement critical paths |
| Tool bridge compatibility | Medium | Medium | Abstract tool interface |

---

## Definition of Done

- [ ] TEA-AGENT-001.1 Multi-Agent completed with acceptance criteria
- [ ] TEA-AGENT-001.2 Reflection Loop completed with acceptance criteria
- [ ] TEA-AGENT-001.3 Planning completed with acceptance criteria
- [ ] TEA-AGENT-001.4 Reasoning completed with acceptance criteria
- [ ] TEA-AGENT-001.5 A2A Communication completed with acceptance criteria
- [ ] Python implementations complete with >90% test coverage
- [ ] Rust implementations complete where feasible
- [ ] Integration tests with MCP/CrewAI/LangChain bridges
- [ ] Documentation updated (YAML_REFERENCE.md, getting-started)
- [ ] Example agents demonstrating each pattern
- [ ] No regressions in existing functionality

---

## References

### Agentic Design Patterns Book
- [GitHub Repository](https://github.com/sarwarbeing-ai/Agentic_Design_Patterns)
- Chapter 4: Reflection
- Chapter 6: Planning
- Chapter 7: Multi-Agent Collaboration
- Chapter 15: Inter-Agent Communication
- Chapter 17: Reasoning Techniques

### Related TEA Stories
- TEA-BUILTIN-001.2: LLM Enhanced Actions (llm.tools foundation)
- TEA-BUILTIN-002.3: Tools Bridge Actions (MCP/CrewAI/LangChain)
- TD.13: Parallel Reliability Enhancement (parallel edge foundation)

---

## QA Results

**Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)
**Assessment Document:** `docs/qa/assessments/TEA-AGENT-001-test-design-20260105.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 186 |
| **Unit Tests** | 98 (53%) |
| **Integration Tests** | 62 (33%) |
| **E2E Tests** | 26 (14%) |
| **P0 Critical Tests** | 82 |
| **P1 High Tests** | 68 |
| **P2/P3 Tests** | 36 |

**Coverage by Story:**
- 001.1 Multi-Agent: 50 scenarios
- 001.2 Reflection: 40 scenarios
- 001.3 Planning: 42 scenarios
- 001.4 Reasoning: 36 scenarios
- 001.5 A2A Communication: 18 scenarios

### Risk Areas Identified

| Risk | Likelihood | Impact | Mitigation Tests |
|------|------------|--------|------------------|
| **Reflection infinite loops** | Medium | High | 001.2-UNIT-005, 001.2-UNIT-025 (circuit breaker & raise strategy) |
| **Multi-agent state conflicts** | Medium | Medium | 001.1-UNIT-021 (deep copy), 001.1-INT-005 (ThreadPoolExecutor) |
| **Planning over-decomposition** | Low | Medium | 001.3-UNIT-005 (max_depth), 001.3-UNIT-022 (max_replans) |
| **A2A message loss** | Medium | High | 001.5-INT-002 (timeout), 001.5-INT-007 (fallback_local) |
| **Rust parity complexity** | High | Medium | E2E parity tests across all stories |
| **Tool bridge compatibility** | Medium | Medium | 001.1-INT-012/13/14/15 (MCP/CrewAI/LangChain) |

### Recommended Test Scenarios

**P0 Critical Path (Must Execute First):**
1. **001.1-E2E-001**: Multi-agent research workflow
2. **001.2-E2E-001**: JSON generation with schema reflection
3. **001.3-E2E-001**: Research planning workflow
4. **001.4-E2E-001**: CoT problem-solving workflow
5. **001.5-E2E-001**: Coordinator-worker workflow

**P0 Unit Tests (Fail Fast):**
- Agent registry validation (001.1-UNIT-001 to 003)
- Core action logic for dispatch, parallel, reflection loop
- Circuit breaker and max iteration limits
- State isolation verification (deep copy per agent)

**Integration Tests (Service Boundaries):**
- LLM service integration with correct parameters
- Tool bridge availability and error handling
- Checkpoint persistence across all patterns
- Action registry verification

### Concerns and Blockers

**CONCERNS:**
1. **Rust Parity Risk**: Story 001.5 (A2A) explicitly limits Rust to "design document only" due to complex FFI. Cross-runtime tests should validate Python-Rust interop carefully.
2. **LLM-as-Judge Reliability**: 001.2 LLM evaluator tests (001.2-INT-004/05) depend on LLM consistency. Consider mock-based deterministic tests for CI.
3. **Parallel Execution Race Conditions**: Multi-agent parallel tests (001.1-INT-005) should include stress tests under high concurrency.

**NO BLOCKERS IDENTIFIED**

### QA Gate Recommendation

**READY FOR DEVELOPMENT** - Test design is comprehensive with 186 scenarios covering all acceptance criteria. No coverage gaps identified. All epic-level risks have corresponding test mitigations.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.2 | Added QA Results section with test design analysis | Quinn (QA) |
| 2026-01-04 | 0.1 | Initial epic draft from gap analysis | Sarah (PO) |
