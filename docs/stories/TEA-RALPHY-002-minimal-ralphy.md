# Epic TEA-RALPHY-002: Minimal Ralphy - LLM-Driven DOT Orchestration

## Status
Done

## Vision

A minimal, composable approach to autonomous task execution using two simple commands:

```bash
# Step 1: Agent analyzes files and generates DOT
tea run dependency-analyzer.yaml --input '{"source": "docs/stories/*.md"}' > workflow.dot

# Step 2: Execute DOT with tmux visualization
tea from dot workflow.dot --use-node-commands --tmux
```

Or as a pipeline:

```bash
tea run dependency-analyzer.yaml --input '{"source": "docs/stories/*.md"}' \
  | tea from dot - --use-node-commands --tmux
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         MINIMAL RALPHY                                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  INPUT                     AGENT ANALYSIS                EXECUTION          │
│  ┌─────────────────┐      ┌─────────────────┐          ┌─────────────────┐ │
│  │ Files (glob)    │      │ dependency-     │          │ tea from dot    │ │
│  │                 │─────▶│ analyzer.yaml   │─────────▶│ --use-node-cmds │ │
│  │ story1.md       │      │                 │          │ --tmux          │ │
│  │ story2.md       │      │ Reads files,    │          │                 │ │
│  │ story3.md       │      │ analyzes deps,  │          │ Creates tmux    │ │
│  └─────────────────┘      │ outputs DOT     │          │ panes per task  │ │
│                           └─────────────────┘          └─────────────────┘ │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Design Principles

1. **Composable Commands** - Two independent commands piped together
2. **Reuse Existing Infrastructure** - `tea run` + `tea from dot` already exist
3. **LLM Does the Analysis** - Agent uses Claude to understand dependencies
4. **DOT is the Interface** - Standard format, human-readable, debuggable
5. **Tmux for Visibility** - Real-time task progress in terminal

## Stories

| Story | Name | Status | Description |
|-------|------|--------|-------------|
| [TEA-RALPHY-002.1](./TEA-RALPHY-002.1.dependency-analyzer-agent.md) | Dependency Analyzer Agent | Draft | YAML agent that analyzes files and generates DOT |
| [TEA-RALPHY-002.2](./TEA-RALPHY-002.2.dot-stdin-support.md) | DOT Stdin Support | Done | Add stdin (`-`) support to `tea from dot` |
| [TEA-RALPHY-002.3](./TEA-RALPHY-002.3.dot-exec-parameter.md) | Custom Executable Parameter | Done | `--exec`/`--dot-exec` for custom workflow runners |

## Deferred from TEA-RALPHY-001

The following features from the original RALPHY epic are intentionally deferred:

| Feature | Original Story | Reason |
|---------|----------------|--------|
| Git Worktrees | 001.6 | Not needed for MVP - parallel tasks run in same directory |
| GitHub Integration | 001.3 | Can be added later as alternative input source |
| Markdown Parsing | 001.1, 001.2 | LLM can read raw markdown directly |
| BMad Story Parsing | 001.4 | LLM can extract tasks from any format |
| Token Tracking | 001.7 | Covered by Opik integration |
| Desktop Notifications | 001.8 | Tmux provides sufficient visibility |

## Usage Examples

### Basic: Analyze and Execute Stories

```bash
# Generate DOT from story files
tea run examples/agents/dependency-analyzer.yaml \
  --input '{"source": "docs/stories/TEA-FEATURE-*.md"}' \
  > workflow.dot

# Review the generated DOT (optional)
cat workflow.dot

# Execute with tmux visualization
tea from dot workflow.dot --use-node-commands --tmux
```

### Pipeline Mode

```bash
tea run examples/agents/dependency-analyzer.yaml \
  --input '{"source": "docs/stories/TEA-FEATURE-*.md"}' \
  | tea from dot - --use-node-commands --tmux -s my-session
```

### Custom Engine

```bash
# Use Codex instead of Claude for execution
tea run examples/agents/dependency-analyzer.yaml \
  --input '{"source": "docs/stories/*.md", "engine": "codex"}' \
  | tea from dot - --use-node-commands --tmux
```

### Dry Run (Generate DOT Only)

```bash
tea run examples/agents/dependency-analyzer.yaml \
  --input '{"source": "docs/stories/*.md"}' \
  > workflow.dot

# Inspect the dependency graph
dot -Tpng workflow.dot -o workflow.png
open workflow.png
```

## Success Criteria

1. User can analyze any set of files for dependencies using the agent
2. Agent outputs valid DOT with command attributes
3. `tea from dot -` accepts DOT from stdin
4. Tmux shows one pane per executing task
5. Parallel phases execute concurrently
6. Sequential phases wait for dependencies

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-22 | 0.1 | Created minimal epic from architectural review | Winston (Architect) |
