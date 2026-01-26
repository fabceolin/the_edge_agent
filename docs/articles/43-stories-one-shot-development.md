# 43 User Stories Developed in One-Shot: From Validation to Implementation

**Fabricio Ceolin**

*Principal Engineer, The Edge Agent Project*

https://www.linkedin.com/in/fabceolin/

---

## Abstract

This article documents the execution of 12 epics with 43 user stories using The Edge Agent (TEA) DOT orchestration in two phases: validation and development. Using the methodology described in [Writing TEA with TEA](./writing-tea-with-tea.md), Phase 1 validated all stories with QA workflows, and Phase 2 implemented 43 stories with full development cycles. The development phase completed in approximately 6 hours with zero human intervention.

**Keywords:** TEA, BMad, DOT Workflows, Parallel Execution, Story Development, One-Shot Development

---

## 1. Introduction

A few weeks ago, I documented [how I am using TEA with TEA development](./writing-tea-with-tea.md) — the methodology of using TEA to orchestrate its own development.

With the same approach, I executed 12 epics simultaneously in two phases:

1. **Validation Phase** — All stories validated with QA workflows
2. **Development Phase** — 43 stories across 12 epics implemented autonomously in ~6 hours

## 2. The Approach: Stories as an Executable Graph

Stories are modeled as a **DAG (Directed Acyclic Graph)**:

```mermaid
flowchart TB
    subgraph A["Epic A - Foundation"]
        A.1
        A.2
    end

    subgraph B["Epic B - Core Module"]
        B.1 --> B.2 --> B.3 --> B.4 --> B.5 --> B.6 --> B.7 --> B.8
    end

    subgraph C["Epic C - Integration"]
        C.0 --> C.0a
        C.0 --> C.2 --> C.4
        C.0 --> C.3
        C.4 --> C.6
        C.3 --> C.6
        C.5 --> C.6
    end

    subgraph D["Epic D - Import"]
        D.0 --> D.0a --> D.0c
        D.0 --> D.0b --> D.0c
        D.0c --> D.1
        D.0c --> D.2 --> D.3 --> D.4
    end

    subgraph E["Epic E - Entity 1"]
        E.1 --> E.2 --> E.3
    end

    subgraph F["Epic F - Entity 2"]
        F.1 --> F.2 --> F.3
    end

    subgraph G["Epic G - Entity 3"]
        G.1 --> G.2
    end

    subgraph H["Epic H - Entity 4"]
        H.0 --> H.1 --> H.2 --> H.3
    end

    subgraph I["Epic I - Entity 5"]
        I.1 --> I.2
    end

    subgraph J["Epic J - Config"]
        J.1 --> J.2
    end

    subgraph K["Epic K - Feature 1"]
        K.1
    end

    subgraph L["Epic L - Feature 2"]
        L.1
    end

    %% Cross-epic dependencies
    A.1 --> B.6
    A.1 --> C.3
    A.1 --> C.5
    B.1 --> C.5
```

Each node represents a story. Each edge represents a dependency. TEA respects the dependency order while maximizing parallelism.

### Development Waves

TEA automatically computes the optimal execution waves based on the dependency graph:

```mermaid
flowchart LR
    subgraph W1["Wave 1 (13 stories)"]
        direction TB
        W1_1["A.1, A.2"]
        W1_2["B.1, C.0, D.0"]
        W1_3["E.1, F.1, G.1"]
        W1_4["H.0, I.1, J.1"]
        W1_5["K.1, L.1"]
    end

    subgraph W2["Wave 2 (12 stories)"]
        direction TB
        W2_1["B.2, C.0a, C.2"]
        W2_2["C.3, C.5"]
        W2_3["D.0a, D.0b"]
        W2_4["E.2, F.2, G.2"]
        W2_5["H.1, I.2, J.2"]
    end

    subgraph W3["Wave 3 (6 stories)"]
        direction TB
        W3_1["B.3, C.4"]
        W3_2["D.0c, E.3"]
        W3_3["F.3, H.2"]
    end

    subgraph W4["Wave 4 (4 stories)"]
        direction TB
        W4_1["B.4, C.6"]
        W4_2["D.1, D.2, H.3"]
    end

    subgraph W5["Wave 5 (2 stories)"]
        W5_1["B.5, D.3"]
    end

    subgraph W6["Wave 6 (2 stories)"]
        W6_1["B.6, D.4"]
    end

    subgraph W7["Wave 7 (1 story)"]
        W7_1["B.7"]
    end

    subgraph W8["Wave 8 (1 story)"]
        W8_1["B.8"]
    end

    W1 --> W2 --> W3 --> W4 --> W5 --> W6 --> W7 --> W8
```

With `--parallel-max 3`, each wave executes up to 3 stories concurrently until all stories in that wave complete, then proceeds to the next wave.

## 3. TEA Features That Enabled This

### 3.1 DOT Graph Executor

The DOT graph becomes an executable workflow. Each node is a story, each edge is a dependency. TEA parses the graph, determines execution order, and runs nodes in parallel where dependencies allow.

### 3.2 One Command to Run Everything

```bash
TEA_SHELL_VERBOSE=1 tea run --from-dot development-all-43.dot --verbose --parallel-max 3
```

The `TEA_SHELL_VERBOSE=1` environment variable streams the LLM output to the terminal in real-time.

| Parameter | Purpose |
|-----------|---------|
| `--from-dot` | Execute DOT file as workflow |
| `--verbose` | Show detailed execution progress |
| `--parallel-max 3` | Limit concurrent processes |

### 3.3 Parallel Fan-out/Fan-in

Up to 3 stories run concurrently (limited by local machine resources, not TEA). Stories without dependencies execute in parallel, maximizing throughput while respecting the dependency graph.

### 3.4 Declarative YAML Workflows

The `bmad-story-development.yaml` workflow orchestrates each story execution:

1. Read story specification
2. Analyze existing code
3. Implement changes
4. Run tests
5. Commit code

## 4. The Execution

### 4.1 Story Distribution

```
12 Epics | 43 Stories | ~6 hours

A: 2    B: 8    C: 7    D: 8
E: 3    F: 3    G: 2    H: 4
I: 2    J: 2    K: 1    L: 1
```

### 4.2 Epic Breakdown

| Epic | Domain | Stories |
|------|--------|---------|
| A | Foundation | 2 |
| B | Core Module | 8 |
| C | Integration | 7 |
| D | Import | 8 |
| E | Entity 1 | 3 |
| F | Entity 2 | 3 |
| G | Entity 3 | 2 |
| H | Entity 4 | 4 |
| I | Entity 5 | 2 |
| J | Config | 2 |
| K | Feature 1 | 1 |
| L | Feature 2 | 1 |

## 5. The Two-Phase Execution

I executed the epic in two distinct phases:

### Phase 1: Validation

```bash
TEA_SHELL_VERBOSE=1 tea run --from-dot validation-all-46.dot --verbose --parallel-max 3
```

Using `bmad-story-validation.yaml`, each story goes through:
- **Risk Profile** — Identify technical and business risks
- **NFR Assessment** — Non-functional requirements analysis
- **Test Design** — QA defines test cases before implementation
- **SM Checklist** — Scrum Master validates Definition of Ready

**Result:** 46 stories across 12 epics validated, ready for development.

### Phase 2: Development

```bash
TEA_SHELL_VERBOSE=1 tea run --from-dot development-all-43.dot --verbose --parallel-max 3
```

Using `bmad-story-development.yaml`, each story goes through:
- **Dev Implementation** — Code changes based on story spec
- **Test Execution** — Run designed tests
- **QA Review** — Validate acceptance criteria
- **Commit** — Incremental commits per story

**Result:** 43 stories implemented in ~6 hours.

### Summary

| Phase | Workflow | Stories | Purpose |
|-------|----------|---------|---------|
| 1 | `bmad-story-validation.yaml` | 46 | QA: risk-profile, NFR, test design |
| 2 | `bmad-story-development.yaml` | 43 | Dev: implement, test, commit |

## 6. Results

- **43 stories developed**
- **Zero human intervention** during execution
- **Code committed incrementally** as each story completed
- **~6 hours** total execution time

## 7. Key Takeaway

Backlogs aren't lists — they're **dependency graphs**. When you model this explicitly:

- Execution becomes **deterministic**
- Parallelization becomes **safe**
- Progress becomes **measurable**

The developer's role shifts: from executor to **work graph architect**.

## 8. References

- [Writing TEA with TEA: A Meta-Development Approach](./writing-tea-with-tea.md) - Phase 1: Validation
- [DOT Workflow Orchestration](./dot-workflow-orchestration.md) - Complete DOT orchestration guide
- [BMad Method v4](https://github.com/bmad-code-org/BMAD-METHOD) - Story creation methodology
- [TEA Documentation](https://fabceolin.github.io/the_edge_agent/) - The Edge Agent official docs
