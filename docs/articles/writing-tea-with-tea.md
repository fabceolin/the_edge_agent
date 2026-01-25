# Writing TEA Features with TEA: A Meta-Development Approach

**Fabricio Ceolin**

*Principal Engineer, The Edge Agent Project*

fabricio@rankellix.com

---

## Abstract

This article presents a meta-development methodology where The Edge Agent (TEA) orchestrates its own feature development. By combining BMad structured workflows for story creation with TEA-powered parallel execution, we achieve higher code quality and faster iteration cycles. The key insight is separating concerns: BMad agents excel at human-guided elicitation and documentation, while TEA excels at clean-context parallel execution. We demonstrate this approach using a real example—implementing an 8-story game feature epic—showing how dependency analysis enables 25% reduction in execution time through intelligent parallelization.

**Keywords:** TEA, BMad, Meta-Development, Parallel Execution, Story Validation, DOT Workflows

---

## 1. Introduction

When developing complex features for The Edge Agent, we face a paradox: the very tool we're building could help us build it better. This article explores how we leverage TEA to accelerate TEA development, creating a virtuous cycle of meta-improvement.

The traditional AI-assisted development workflow looks like this:

```
Human → AI Chat → Code → Review → Iterate
```

Each iteration carries context from previous attempts, accumulating cognitive debt. The AI's context window fills with failed approaches, abandoned directions, and incremental fixes.

What if we could guarantee clean context for each execution?

```
BMad: Create perfect story specifications
TEA:  Execute each story with fresh context
DOT:  Orchestrate parallel execution
```

This separation of concerns gives us the best of both worlds: BMad's human-in-the-loop elicitation produces high-quality specifications, while TEA's workflow engine guarantees isolated, reproducible execution.

### 1.1 The Problem with Context Accumulation

Consider developing a feature over multiple chat sessions:

| Session | Context State | Quality Impact |
|---------|---------------|----------------|
| 1 | Fresh context | High quality decisions |
| 2 | Previous mistakes loaded | May repeat patterns that failed |
| 3 | Accumulated corrections | Defensive coding, over-engineering |
| 4 | Context window pressure | Forgotten requirements, inconsistencies |

By Session 4, the AI is juggling too much history. It may remember that "we tried X and it failed" but forget *why* it failed, leading to suboptimal alternatives.

## 2. The BMad Method: Getting Stories Right

[BMad (Breakthrough Method for Agile AI-Driven Development)](https://github.com/bmad-code-org/BMAD-METHOD) provides the structured foundation for high-quality AI-assisted development.

### 2.1 What Makes BMad Different

Unlike traditional AI coding assistants that "do the thinking for you, producing average results," BMad positions AI agents as **expert guides** through structured workflows:

```mermaid
flowchart LR
    A[Human Intent] --> B[BMad Agent]
    B --> C{Elicitation}
    C --> D[Structured Artifact]
    D --> E[Validation]
    E --> F[Ready for Implementation]
```

### 2.2 BMad Agent Roles

BMad deploys specialized agents for different concerns:

| Agent | Role | Key Contribution |
|-------|------|------------------|
| **PO (Sarah)** | Product Owner | Story refinement, acceptance criteria |
| **Architect** | System Design | Technical decisions, dependency analysis |
| **QA** | Quality Assurance | Test design, risk identification |
| **SM** | Scrum Master | Story validation, Definition of Ready |
| **Dev** | Developer | Implementation, code quality |

### 2.3 The One-Shot Advantage

BMad's elicitation process ensures stories are **complete before implementation begins**. This is critical because:

1. **No mid-implementation pivots** - Requirements are validated upfront
2. **Clear acceptance criteria** - The AI knows exactly what "done" looks like
3. **Dependency mapping** - Stories can be parallelized safely
4. **Test design included** - QA validates before code is written

Here's an example of BMad-produced story structure:

```yaml
# Story created by BMad PO Agent
name: TEA-GAME-001.1
title: Rust Game Engine Core
status: Draft

acceptance_criteria:
  - AC-1: GameSession struct with required fields
  - AC-2: GameRound struct with phrase and choices
  - AC-3: generate_username() returns random pattern
  - AC-4: calculate_score() implements weighted formula
  - AC-5: adjust_difficulty() uses rolling window
  - AC-6: Difficulty bounded to [0.1, 0.95]
  - AC-7: Unit tests for all calculations

tasks:
  - Create rust/src/games/mod.rs module
  - Implement GameSession and GameRound structs
  - Implement generate_username() with word lists
  # ... detailed subtasks
```

## 3. The Context Problem: Why TEA Instead of BMad Workflows

BMad agents run within a conversational context (Claude Code, Cursor, etc.). While this enables rich human interaction, it creates a problem for **autonomous parallel execution**.

### 3.1 BMad's Conversational Context

```
┌─────────────────────────────────────────────────────────┐
│                  CLAUDE CODE SESSION                     │
├─────────────────────────────────────────────────────────┤
│  Turn 1: /po *create-story TEA-GAME-001.1               │
│  Turn 2: [PO creates story, asks clarifying questions]  │
│  Turn 3: User provides answers                          │
│  Turn 4: [PO refines story]                             │
│  ...                                                     │
│  Turn 50: Context window filling up                     │
│  Turn 100: Earlier decisions forgotten                  │
└─────────────────────────────────────────────────────────┘
```

When running 8 stories sequentially in one session, context from Story 1 affects Story 8. This is sometimes desirable (learning from patterns) but often problematic (accumulated noise).

### 3.2 TEA's Clean Context Guarantee

TEA workflows execute in **isolated contexts**:

```
┌─────────────────────────────────────────────────────────┐
│                    TEA WORKFLOW                          │
├─────────────────────────────────────────────────────────┤
│  Node 1: Load story file (fresh context)                │
│  Node 2: Execute validation (isolated)                  │
│  Node 3: Write results (clean output)                   │
│  [Process terminates, context released]                 │
└─────────────────────────────────────────────────────────┘
```

Each story validation runs in a completely fresh subprocess:

```yaml
# bmad-story-validation.yaml
nodes:
  - name: run_qa_test_design
    uses: llm.call
    with:
      provider: shell
      shell_provider: claude
      model: claude
      messages:
        - role: user
          content: |
            CRITICAL: ACTIVATE PERSONA MODE.
            {{ state.agent_persona }}

            TASK: Perform *test-design for story {{ state.arg }}.
            {{ state.task_definition }}

            MODE: YOLO - Do NOT ask permission. Execute commands.
```

The `shell_provider: claude` spawns a fresh Claude Code instance for each node execution. No context bleeds between stories.

### 3.3 Comparison Table

| Aspect | BMad (Conversational) | TEA (Workflow) |
|--------|----------------------|----------------|
| **Context** | Accumulated across session | Fresh per execution |
| **Parallelization** | Sequential only | Parallel via DOT |
| **Human Interaction** | Rich, real-time | Pre-configured |
| **Best For** | Elicitation, refinement | Execution, validation |
| **Reproducibility** | Varies by session | Deterministic |

## 4. Parallelizing Story Validation with DOT

With BMad stories ready and TEA workflows defined, we can orchestrate parallel execution using DOT (Graphviz) graphs.

> **For comprehensive DOT orchestration documentation**—including CLI reference, execution modes, verbose output, and best practices—see the [DOT Workflow Orchestration](./dot-workflow-orchestration.md) article.

### 4.1 Dependency Analysis

Given an epic with 8 stories, the first step is mapping dependencies:

| Story | Depends On | Can Parallelize With |
|-------|------------|---------------------|
| 1. Rust Core | None | - |
| 2. DuckDB Schema | Story 1 | Stories 4, 8 |
| 3. Embeddings | Story 2 | - |
| 4. LLM Phrase Gen | Story 1 | Stories 2, 8 |
| 5. Game Engine | Stories 3, 4 | - |
| 6. WASM Port | Story 5 | - |
| 7. Browser UI | Story 6 | - |
| 8. Opik Integration | Story 1 | Stories 2, 4 |

### 4.2 The DOT Workflow

```dot
digraph tea_game_001_validation {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    // Phase 1: Foundation
    subgraph cluster_phase1 {
        label="1. Foundation";
        story_1 [label="TEA-GAME-001.1",
                 command="tea-python run examples/workflows/bmad-story-validation.yaml
                          --input-timeout 54000
                          --input '{\"arg\": \"docs/stories/TEA-GAME-001.1-rust-game-engine-core.md\"}'"];
    }

    // Phase 2: Parallel Track - Stories 2, 4, 8 run simultaneously
    subgraph cluster_phase2 {
        label="2. Parallel Track A";
        story_2 [label="TEA-GAME-001.2", command="..."];
        story_4 [label="TEA-GAME-001.4", command="..."];
        story_8 [label="TEA-GAME-001.8", command="..."];
    }

    // Edges define execution order
    Start -> story_1;
    story_1 -> story_2;
    story_1 -> story_4;
    story_1 -> story_8;

    // Story 8 completes independently
    story_8 -> End;

    // Continue dependency chain...
    story_2 -> story_3;
    story_3 -> story_5;
    story_4 -> story_5;
    story_5 -> story_6;
    story_6 -> story_7;
    story_7 -> End;
}
```

### 4.3 Visual Dependency Graph

```
                    ┌─────────────────┐
                    │     Start       │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │   Story 1       │  Phase 1
                    │ (Rust Core)     │
                    └────────┬────────┘
                             │
           ┌─────────────────┼─────────────────┐
           │                 │                 │
    ┌──────▼──────┐   ┌──────▼──────┐   ┌──────▼──────┐
    │  Story 2    │   │  Story 4    │   │  Story 8    │  Phase 2
    │  (DuckDB)   │   │  (LLM Gen)  │   │  (Opik)     │  PARALLEL
    └──────┬──────┘   └──────┬──────┘   └──────┬──────┘
           │                 │                 │
    ┌──────▼──────┐          │                 │
    │  Story 3    │          │                 │  Phase 3
    │ (Embeddings)│          │                 │
    └──────┬──────┘          │                 │
           │                 │                 │
           └────────┬────────┘                 │
                    │                          │
             ┌──────▼──────┐                   │
             │  Story 5    │                   │  Phase 4
             │ (GameEngine)│                   │
             └──────┬──────┘                   │
                    │                          │
             ┌──────▼──────┐                   │
             │  Story 6    │                   │  Phase 5
             │ (WASM Port) │                   │
             └──────┬──────┘                   │
                    │                          │
             ┌──────▼──────┐                   │
             │  Story 7    │                   │  Phase 6
             │ (Browser UI)│                   │
             └──────┬──────┘                   │
                    │                          │
                    └────────────┬─────────────┘
                                 │
                    ┌────────────▼────────────┐
                    │          End            │
                    └─────────────────────────┘
```

### 4.4 Efficiency Gain

| Metric | Sequential | Parallelized | Improvement |
|--------|------------|--------------|-------------|
| Total Phases | 8 | 6 | 25% reduction |
| Max Parallel | 1 | 3 | 3x throughput |
| Story 8 (Opik) | Waits for 1-7 | Runs with 2,4 | ~70% faster |

## 5. Setup: Configuring Claude Code as Backend

Before running TEA workflows that use Claude Code as the LLM backend, you need to configure your environment.

### 5.1 Prerequisites

1. **TEA Python** installed:
   ```bash
   cd python
   pip install -e .[dev]
   ```

2. **Claude Code CLI** installed and authenticated:
   ```bash
   # Install Claude Code (if not already installed)
   npm install -g @anthropic-ai/claude-code

   # Authenticate with your Anthropic API key
   claude auth
   ```

3. **BMad v4 (Required)** - The Ralphy workflows require BMad v4 story format:
   ```
   .bmad-core/
   ├── agents/
   │   ├── qa.md          # QA agent persona
   │   ├── sm.md          # Scrum Master agent persona
   │   ├── po.md          # Product Owner agent persona
   │   └── dev.md         # Developer agent persona
   ├── tasks/
   │   ├── test-design.md        # Test design task
   │   ├── validate-next-story.md # Story validation task
   │   └── qa-gate.md            # QA gate task
   ├── checklists/
   │   ├── story-draft-checklist.md
   │   └── po-master-checklist.md
   └── templates/
       └── story-tmpl.yaml       # Story template
   ```

   To verify BMad v4 is installed:
   ```bash
   # Check for .bmad-core directory
   ls -la .bmad-core/

   # Check for required task files
   ls .bmad-core/tasks/validate-next-story.md
   ```

   If you don't have BMad v4, see the [BMad Setup Guide](https://github.com/bmad-code-org/BMAD-METHOD) to initialize it.

### 5.2 The Shell Provider Configuration

TEA's `llm.call` action supports a `shell_provider` that spawns external CLI tools. For Claude Code:

```yaml
nodes:
  - name: run_validation
    uses: llm.call
    with:
      provider: shell           # Use shell-based LLM provider
      shell_provider: claude    # Spawn Claude Code CLI
      model: claude             # Model selection handled by Claude Code
      timeout: 600              # 10 minutes per node
      messages:
        - role: user
          content: |
            Your prompt here...
```

**How it works:**

1. TEA invokes `claude` CLI as a subprocess
2. The prompt is passed via stdin
3. Claude Code executes with fresh context (no history)
4. Output is captured and returned to TEA state

### 5.3 Environment Variables

Set these environment variables for optimal execution:

```bash
# Required: Anthropic API key (Claude Code will use this)
export ANTHROPIC_API_KEY="sk-ant-..."

# Optional: Control Claude Code behavior
export CLAUDE_CODE_AUTO_ACCEPT=1    # Auto-accept tool calls
export CLAUDE_CODE_NO_CONFIRM=1     # Skip confirmation prompts
```

### 5.4 Verify Setup

Test that Claude Code works as a shell provider:

```bash
# Simple test
echo "Say hello" | claude --print

# Test with TEA
tea-python run examples/workflows/bmad-story-validation.yaml \
    --input '{"arg": "docs/stories/TEA-GAME-001.1-rust-game-engine-core.md"}' \
    --dry-run
```

---

## 6. Execution: From DOT to Running Workflow

### 6.1 Execute DOT Directly (Recommended)

The simplest approach is to execute the DOT file directly with tmux-based output.

**Two execution modes:**

#### Mode 1: Command Mode (nodes have `command` attribute)

```bash
# Execute DOT directly with tmux output (respects dependency order)
tea run --from-dot examples/dot/tea-game-001-validation.dot

# Or use the dedicated command with more options
tea run-from-dot examples/dot/tea-game-001-validation.dot \
    --session tea-game \
    --max-parallel 3

# Preview execution plan without running
tea run --from-dot examples/dot/tea-game-001-validation.dot --dot-dry-run
```

#### Mode 2: Workflow Mode (run a workflow for each node)

When nodes represent stories/tasks, use `--dot-workflow` to run a workflow for each:

```bash
# Run bmad-story-development.yaml for each node
tea run --from-dot stories.dot \
    --dot-workflow examples/workflows/bmad-story-development.yaml

# With additional input parameters
tea run-from-dot stories.dot \
    -w examples/workflows/bmad-story-development.yaml \
    -i '{"mode": "sequential"}'

# With custom executable (--dot-exec or --exec)
tea run --from-dot stories.dot \
    --dot-workflow examples/workflows/bmad-story-validation.yaml \
    --dot-exec "python -m the_edge_agent"

# Or using run-from-dot with --exec/-e
tea-python run-from-dot stories.dot \
    -w examples/workflows/bmad-story-validation.yaml \
    -e "/path/to/custom/tea-python"
```

The node label is passed as `{"arg": "<node_label>"}` to the workflow.

Monitor progress with: `tmux attach -t tea-dot`

#### Parameter Reference

| `run-from-dot` | `run --from-dot` | Purpose |
|----------------|------------------|---------|
| `--workflow`/`-w` | `--dot-workflow` | Workflow YAML file to run for each node |
| `--max-parallel`/`-m` | `--dot-max-parallel` | Maximum parallel processes |
| `--exec`/`-e` | `--dot-exec` | Custom executable (default: `tea-python`) |
| `--session`/`-s` | `--dot-session` | Tmux session name |

### 6.2 Alternative: Execute via YAML

If you need YAML-specific features (checkpoints, interrupts), use the `dot_to_yaml` API:

```python
from the_edge_agent import dot_to_yaml

# Generate YAML from DOT
yaml_content = dot_to_yaml("workflow.dot", use_node_commands=True)
```

Then run:

```bash
# Run with extended timeout (15 hours for large epics)
tea run examples/dot/tea-game-001-validation.yaml \
    --input-timeout 54000
```

### 6.3 What Happens During Execution

```
┌──────────────────────────────────────────────────────────────┐
│  tea-python run tea-game-001-validation.yaml                 │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  [Phase 1]  Story 1 ─────────────────────────► Complete      │
│                                                              │
│  [Phase 2]  Story 2 ─────────────────────────►┐              │
│             Story 4 ─────────────────────────►├─► All Done   │
│             Story 8 ─────────────────────────►┘              │
│                                                              │
│  [Phase 3]  Story 3 ─────────────────────────► Complete      │
│                                                              │
│  [Phase 4]  Story 5 ─────────────────────────► Complete      │
│                                                              │
│  [Phase 5]  Story 6 ─────────────────────────► Complete      │
│                                                              │
│  [Phase 6]  Story 7 ─────────────────────────► Complete      │
│                                                              │
│  ═══════════════════════════════════════════════════════════ │
│  WORKFLOW COMPLETE: 8 stories validated, 6 phases            │
└──────────────────────────────────────────────────────────────┘
```

## 7. Real-World Example: Matter Import Resolution Epic

This section demonstrates a production execution of 46 stories across 12 epics using TEA DOT orchestration.

### 7.1 Workflow Selection: Validation vs Development

TEA provides two complementary BMad workflows:

| Workflow | Purpose | When to Use |
|----------|---------|-------------|
| `bmad-story-validation.yaml` | QA-only: risk-profile, NFR, test-design, SM checklist | Before development starts |
| `bmad-story-development.yaml` | Full cycle: Dev → QA → SM with code implementation | After stories pass validation |

For this epic, we use **bmad-story-validation** to validate all 46 stories before any implementation begins.

### 7.2 The Execution Command

```bash
# Actual command running in tmux session 'tea-dot'
tea-python run-from-dot \
    examples/dot/matter-import-resolution-workflow-short.dot \
    --workflow examples/workflows/bmad-story-validation.yaml \
    --max-parallel 3 \
    --session tea-dot
```

**Parameters explained:**

| Parameter | Value | Purpose |
|-----------|-------|---------|
| `run-from-dot` | - | Command to execute DOT workflow |
| DOT file | `matter-import-resolution-workflow-short.dot` | 46-node dependency graph |
| `--workflow`/`-w` | `bmad-story-validation.yaml` | QA + SM validation workflow |
| `--max-parallel`/`-m` | `3` | Limit concurrent processes |
| `--session`/`-s` | `tea-dot` | Tmux session name |

### 7.3 DOT File Structure (Short Labels)

For tmux compatibility, node labels must be short (tmux truncates window names to ~30 chars):

```dot
digraph matter_import_resolution_workflow {
    rankdir=TB;
    node [shape=box];

    // Use short labels like "PIR.1.person-table-row-level-accept-reject"
    // NOT full paths like "/home/user/docs/stories/PIR.1.person-table..."

    PIR_1 [label="PIR.1.person-table-row-level-accept-reject"];
    PIR_2 [label="PIR.2.person-import-accept-all-enhancement"];

    Start -> PIR_1;
    Start -> PIR_2;
}
```

The workflow's `resolve_story_path` node converts short labels to full paths at runtime.

### 7.4 Execution Progress

```
Graph loaded: 46 nodes in 6 phases

=== Execution Plan ===
Phase 1 (parallel): 13 nodes
Phase 2 (parallel): 11 nodes
Phase 3 (parallel): 9 nodes
Phase 4 (parallel): 7 nodes
Phase 5 (parallel): 4 nodes
Phase 6 (parallel): 2 nodes

>>> Phase 1/6: 13 node(s)...
  Starting: MPR.1.managing-partners-pattern-b-accept-ui
  Starting: RIR.0.referee-import-pending-state
  Starting: MIR.1.matter-deduplication-service-enhancement
  Waiting for 3 window(s)...
  ✓ Completed: RIR.0.referee-import-pending-state (388.3s)
  ✓ Completed: MIR.1.matter-deduplication-service-enhancement (433.1s)
  ✓ Completed: MPR.1.managing-partners-pattern-b-accept-ui (453.8s)
  ...
```

### 7.5 Monitoring the Execution

```bash
# Attach to the tmux session
tmux attach -t tea-dot

# View output file (if running in background)
tail -f /tmp/claude/-home-fabricio-src-spa-base/tasks/<task-id>.output

# List all windows in session
tmux list-windows -t tea-dot
```

### 7.6 Using Custom Executable

If you need to use a different TEA installation or virtual environment:

```bash
# Use a specific virtualenv
tea-python run-from-dot workflow.dot \
    -w bmad-story-validation.yaml \
    --exec "/home/user/.venv/bin/tea-python"

# Use Python module directly
tea-python run-from-dot workflow.dot \
    -w bmad-story-validation.yaml \
    -e "python -m the_edge_agent"

# Via run --from-dot syntax
tea-python run agent.yaml --from-dot workflow.dot \
    --dot-workflow bmad-story-validation.yaml \
    --dot-exec "python -m the_edge_agent" \
    --dot-max-parallel 3
```

---

## 8. Best Practices

### 8.1 When to Use Each Approach

| Scenario | Recommended Approach |
|----------|---------------------|
| Initial story creation | BMad PO agent (human elicitation) |
| Story refinement | BMad PO/Architect (interactive) |
| Story validation | `bmad-story-validation` workflow |
| Full development cycle | `bmad-story-development` workflow |
| Complex epic execution | DOT workflow + validation/development |
| Debugging failures | BMad Dev agent (conversational) |
| QA review | BMad QA agent or `bmad-review` workflow |

### 8.2 DOT Workflow Guidelines

1. **Simple Labels** - Use story IDs, not descriptions
2. **Explicit Dependencies** - Every edge represents a real constraint
3. **Timeout Planning** - Use `--input-timeout 54000` (15 hours) for complex stories
4. **Cluster Phases** - Group parallel stories in `subgraph cluster_*` blocks

### 8.3 Context Isolation Principles

1. **Each story = one execution** - Never batch stories in one context
2. **State via files** - Stories communicate through file system, not memory
3. **Idempotent operations** - Re-running a story should produce same result
4. **Explicit inputs** - All dependencies declared in DOT edges

## 9. Conclusion

The meta-development approach—using TEA to build TEA—provides several key advantages:

1. **Quality through separation** - BMad ensures story quality; TEA ensures execution quality
2. **Parallelization through analysis** - Dependency graphs enable safe concurrent execution
3. **Clean context guarantee** - Each story executes in isolation
4. **Reproducibility** - DOT workflows are version-controlled and repeatable

The Matter Import Resolution epic demonstrates this in practice: 46 stories, 6 phases, parallel validation with `--max-parallel 3`, zero context contamination.

**Available BMad workflows:**

- `bmad-story-validation.yaml` - QA validation: risk-profile, NFR, test-design, SM checklist
- `bmad-story-development.yaml` - Full cycle: Dev → QA → SM with code implementation

As TEA continues to evolve, this meta-development cycle accelerates: better TEA enables better TEA development, which produces better TEA.

## 10. References

- [BMad Method v4](https://github.com/bmad-code-org/BMAD-METHOD) - Breakthrough Method for Agile AI-Driven Development (Required)
- [TEA Documentation](https://fabceolin.github.io/the_edge_agent/) - The Edge Agent official docs
- [DOT Workflow Orchestration](./dot-workflow-orchestration.md) - Complete guide to DOT orchestration, CLI reference, and best practices
- [Graphviz DOT Language](https://graphviz.org/doc/info/lang.html) - DOT syntax reference
