# DOT Workflow Orchestration Guide for LLMs

> **TEA Version**: 0.9.82+ | **Primary CLI**: `tea-python run --from-dot`

This document provides instructions for generating DOT (Graphviz) files that orchestrate sequential and parallel execution of stories, tasks, or any workflow items using TEA agents.

## Version Requirements

- **Minimum version**: 0.9.82
- **Recommended**: Latest version for bug fixes and performance improvements
- The `run --from-dot` option and Workflow Mode are only available in the Python implementation (`tea-python`)

## Use Case

When you need to:
- Implement multiple stories in a specific order
- Run development workflows with dependencies
- Orchestrate parallel tasks within sequential phases
- Generate executable YAML agents from visual diagrams

## File Location

**Standard location for DOT files:** `examples/dot/` (relative to TEA repository root)

All DOT workflow files should be saved in the `examples/dot/` directory within the TEA repository. This keeps workflow orchestration files organized and separate from documentation and source code.

### Repository Structure (Relative Paths)

```
<tea-repo-root>/
├── examples/
│   ├── dot/                          # DOT workflow files (save here)
│   │   ├── linear.dot
│   │   ├── parallel-phases.dot
│   │   └── ...
│   └── workflows/                    # YAML workflow agents
│       ├── bmad-story-development.yaml
│       ├── bmad-story-validation.yaml
│       └── ...
└── docs/
    └── stories/                      # Story files
        └── ...
```

### Key File Paths (Relative to TEA Root)

| File | Relative Path |
|------|---------------|
| BMAD Story Development | `examples/workflows/bmad-story-development.yaml` |
| BMAD Story Validation | `examples/workflows/bmad-story-validation.yaml` |
| DOT Files Directory | `examples/dot/` |
| Stories Directory | `docs/stories/` |

### Runtime Path Discovery (IMPORTANT)

**Before generating a DOT file, the LLM MUST use tools to discover actual paths:**

The project root (`PROJECT_ROOT`) can be:
- The `the_edge_agent` repository (when working on TEA itself)
- Any other project that uses TEA workflows (e.g., `spa-base`, custom projects)

1. **Find BMAD workflow files:**
   ```bash
   # Glob pattern to find BMAD workflows in any project
   **/bmad-story-development.yaml
   **/bmad-story-validation.yaml
   ```

2. **Extract PROJECT_ROOT from discovered path:**
   ```bash
   # If found: /home/user/myproject/the_edge_agent/examples/workflows/bmad-story-development.yaml
   # PROJECT_ROOT could be: /home/user/myproject/the_edge_agent
   # OR if the project has its own workflows:
   # /home/user/myproject/workflows/bmad-story-development.yaml
   # PROJECT_ROOT = /home/user/myproject
   ```

3. **Find story files in the project:**
   ```bash
   # Glob pattern to find stories (may be in different locations)
   **/docs/stories/*.md
   **/stories/*.md
   ```

4. **Use discovered absolute paths in DOT commands**

**Note:** Different projects may have different directory structures. Always discover paths dynamically.

**Naming convention:**
- Use lowercase with hyphens: `epic-name-workflow.dot`
- Include purpose in name: `tea-release-004-validation.dot`, `feature-x-development.dot`
- Generated YAML goes alongside: `examples/dot/my-workflow.yaml`

---

## Quick Start

### Step 1: Generate DOT File

```dot
digraph workflow_name {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    subgraph cluster_phase1 {
        label="1. Phase Name";
        task_1 [label="Task 1", command="tea-python run workflow.yaml --input '{\"arg\": \"path/to/input\"}'"];
    }

    Start -> task_1;
    task_1 -> End;
}
```

### Step 2: Execute DOT Directly (Recommended)

**Two execution modes:**

#### Mode 1: Command Mode (nodes have `command` attribute)

Each DOT node must have a `command` attribute that specifies what to execute:

```bash
tea-python run --from-dot workflow.dot
tea-python run --from-dot workflow.dot --dot-session my-session --dot-max-parallel 4
tea-python run --from-dot workflow.dot --dot-dry-run
```

#### Mode 2: Workflow Mode (RECOMMENDED - run a workflow for each node)

When nodes don't have commands, use `--dot-workflow` to run a TEA workflow for each node.
The node label is passed as `{"arg": "<node_label>"}` to the workflow.

```bash
tea-python run --from-dot stories.dot --dot-workflow bmad-story-validation.yaml
tea-python run --from-dot stories.dot --dot-workflow dev.yaml --dot-max-parallel 4
tea-python run --from-dot stories.dot --dot-workflow dev.yaml --dot-max-parallel 4 --dot-input '{"mode": "sequential"}'
```

**Important:** In Workflow Mode, the node label becomes `state.arg` in the workflow. Design your workflow to handle this input appropriately.

#### Verbose Mode (see LLM output in real-time)

When workflows call `llm.call` with shell providers (e.g., Claude Code), use `TEA_SHELL_VERBOSE=1` to see the output in real-time:

```bash
# See Claude Code output while running
TEA_SHELL_VERBOSE=1 tea-python run --from-dot stories.dot --dot-workflow dev.yaml

# Or export for all commands in the session
export TEA_SHELL_VERBOSE=1
tea-python run --from-dot stories.dot --dot-workflow bmad-story-validation.yaml --dot-max-parallel 3
```

#### Command Reference

**`run --from-dot` options:**

| Option | Default | Description |
|--------|---------|-------------|
| `--dot-workflow` | None | Workflow YAML to run for each node |
| `--dot-max-parallel` | 3 | Maximum parallel tmux windows |
| `--dot-session` | `tea-dot` | Tmux session name |
| `--dot-input` | None | Additional JSON input to merge |
| `--dot-exec` | `tea-python` | Executable to use for running workflows |
| `--dot-timeout` | 54000 | Command timeout in seconds (15h) |
| `--dot-poll-interval` | 5 | Seconds between completion checks |
| `--dot-dry-run` | false | Show plan without executing |
| `--verbose` / `-v` | 0 | Verbosity level (-v, -vv, -vvv) |

**Custom executable examples:**

```bash
# Use a specific Python path
tea-python run --from-dot stories.dot --dot-workflow workflow.yaml --dot-exec "/usr/bin/python3 -m the_edge_agent"

# Use a custom TEA installation
tea-python run --from-dot stories.dot --dot-workflow workflow.yaml --dot-exec "/opt/tea/bin/tea-python"

# Use Claude Code directly (experimental)
tea-python run --from-dot stories.dot --dot-workflow workflow.yaml --dot-exec "claude"
```

Monitor progress: `tmux attach -t tea-dot`

### Step 2b: Alternative - Generate YAML First

If you need YAML-specific features (checkpoints, interrupts):

```python
from the_edge_agent import dot_to_yaml
yaml_content = dot_to_yaml("workflow.dot", use_node_commands=True, output_path="workflow.yaml")
```

Then run:

```bash
tea run workflow.yaml
```

---

## DOT File Structure

### Required Elements

| Element | Purpose | Example |
|---------|---------|---------|
| `digraph name` | Graph container | `digraph story_implementation {}` |
| `Start` node | Entry point (ellipse shape) | `Start [label="Start", shape=ellipse];` |
| `End` node | Exit point (ellipse shape) | `End [label="End", shape=ellipse];` |
| `subgraph cluster_*` | Phase grouping | `subgraph cluster_build {}` |
| `command` attribute | Per-node command | `command="tea-python run ..."` |
| Edges | Execution order | `task_a -> task_b;` |

### Node Attributes

```dot
node_id [
    label="Display Name",           // Required: shown in output
    command="shell command here"    // Required: executed for this node
];
```

### Command Format for TEA Workflows

```dot
command="tea-python run <workflow.yaml> --input '{\"arg\": \"<value>\"}'"
```

**Escaping rules:**
- Use `\"` for quotes inside the command string
- The command is wrapped in double quotes in DOT format

---

## Patterns

### Pattern 1: Sequential Story Implementation

All stories execute one after another.

```dot
digraph sequential_stories {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    subgraph cluster_phase1 {
        label="1. Foundation";
        story_1 [label="<STORY-ID-1>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-1-FILE>.md\"}'"];
    }

    subgraph cluster_phase2 {
        label="2. Core Features";
        story_2 [label="<STORY-ID-2>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-2-FILE>.md\"}'"];
    }

    subgraph cluster_phase3 {
        label="3. Integration";
        story_3 [label="<STORY-ID-3>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-3-FILE>.md\"}'"];
    }

    Start -> story_1;
    story_1 -> story_2;
    story_2 -> story_3;
    story_3 -> End;
}
```

**Note:** Replace `<WORKFLOW_PATH>` and `<STORIES_PATH>` with actual paths discovered at runtime using Glob tools.

### Pattern 2: Parallel Stories Within Phases

Stories within the same phase run in parallel; phases run sequentially.

```dot
digraph parallel_phases {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    subgraph cluster_phase1 {
        label="1. Phase 1 Stories";
        story_a [label="<STORY-A>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-A-FILE>.md\"}'"];
        story_b [label="<STORY-B>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-B-FILE>.md\"}'"];
    }

    subgraph cluster_phase2 {
        label="2. Phase 2 Stories";
        story_c [label="<STORY-C>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-C-FILE>.md\"}'"];
        story_d [label="<STORY-D>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-D-FILE>.md\"}'"];
    }

    // Phase 1: story_a and story_b run in parallel
    Start -> story_a;
    Start -> story_b;

    // Phase 2: story_c and story_d run in parallel (after phase 1)
    story_a -> story_c;
    story_b -> story_d;

    // End after phase 2
    story_c -> End;
    story_d -> End;
}
```

### Pattern 3: Mixed Dependencies

Some stories depend on specific predecessors.

```dot
digraph mixed_deps {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    subgraph cluster_foundation {
        label="1. Foundation";
        core [label="<CORE-STORY>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<CORE-STORY-FILE>.md\"}'"];
    }

    subgraph cluster_features {
        label="2. Features";
        feature_a [label="<FEATURE-A>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<FEATURE-A-FILE>.md\"}'"];
        feature_b [label="<FEATURE-B>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<FEATURE-B-FILE>.md\"}'"];
        feature_c [label="<FEATURE-C>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<FEATURE-C-FILE>.md\"}'"];
    }

    subgraph cluster_integration {
        label="3. Integration";
        integration [label="<INTEGRATION>", command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<INTEGRATION-FILE>.md\"}'"];
    }

    // Core is prerequisite for all features
    Start -> core;
    core -> feature_a;
    core -> feature_b;
    core -> feature_c;

    // All features must complete before integration
    feature_a -> integration;
    feature_b -> integration;
    feature_c -> integration;
    integration -> End;
}
```

---

## Command Templates

### Story Development Workflow

Use JSON input format with `arg` key:

```dot
command="tea-python run <WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-FILE>.md\"}'"
```

### Story Validation Workflow

```dot
command="tea-python run <VALIDATION_WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-FILE>.md\"}'"
```

### With Extended Timeout

For long-running workflows, add `--timeout` (in seconds). Default is 300s (5 minutes):

```dot
command="tea-python run <VALIDATION_WORKFLOW_PATH>  --input '{\"arg\": \"<STORIES_PATH>/<STORY-FILE>.md\"}'"
```

**Placeholder Reference:**
- `<WORKFLOW_PATH>` - Absolute path to bmad-story-development.yaml (discovered at runtime)
- `<VALIDATION_WORKFLOW_PATH>` - Absolute path to bmad-story-validation.yaml (discovered at runtime)
- `<STORIES_PATH>` - Absolute path to stories directory (discovered at runtime)
- `<STORY-FILE>` - Story filename without extension (e.g., `TEA-RELEASE-004.1-rust-llm-appimage`)

**Common timeout values:**
- `--timeout 600` - 10 minutes
- `--timeout 1800` - 30 minutes
- `--timeout 3600` - 1 hour
- `` - 15 hours (900 minutes) - recommended for complex workflows

### Custom Workflow with JSON Inputs

For JSON inputs, use single quotes around the JSON (but avoid nested quotes when possible):

```dot
command="echo simple_value | tea-python run my-workflow.yaml --input -"
```

Or use environment variables:

```dot
command="STORY_ID=STORY-001 tea-python run my-workflow.yaml"
```

### Shell Commands (Non-TEA)

```dot
command="npm run build && npm test"
command="cargo build --release"
command="pytest tests/ -v"
```

---

## Execution Commands

### Direct Execution via run --from-dot (Recommended)

```bash
# Command Mode: Execute DOT with node commands
tea-python run --from-dot workflow.dot

# Workflow Mode: Run a workflow for each node (RECOMMENDED)
tea-python run --from-dot stories.dot --dot-workflow bmad-story-validation.yaml

# With parallelism control (default is 3)
tea-python run --from-dot stories.dot --dot-workflow bmad-story-validation.yaml --dot-max-parallel 4

# With custom session name
tea-python run --from-dot workflow.dot --dot-session my-session --dot-max-parallel 3

# Preview execution plan (dry run)
tea-python run --from-dot workflow.dot --dot-dry-run

# With verbose output for LLM shell calls
TEA_SHELL_VERBOSE=1 tea-python run --from-dot stories.dot --dot-workflow dev.yaml -v
```

Monitor with: `tmux attach -t tea-dot`

### Programmatic YAML Generation (Alternative)

If you need YAML for checkpoints, interrupts, or other advanced features:

```python
from the_edge_agent import dot_to_yaml

# Generate with custom options
yaml_content = dot_to_yaml(
    "workflow.dot",
    use_node_commands=True,
    max_concurrency=5,
    workflow_name="my-pipeline",
    validate=True,
    output_path="workflow.yaml"
)
```

### Execute Generated Workflow

```bash
# Run the generated workflow
tea-python run workflow.yaml

# With extended timeout (900 minutes = 54000 seconds)
tea-python run workflow.yaml 

# With visual graph progress (shows ASCII workflow diagram with running/completed states)
tea-python run workflow.yaml  --show-graph

# Graph visualization only (no other output)
tea-python run workflow.yaml  --show-graph --quiet

# With streaming output (NDJSON mode - mutually exclusive with --show-graph)
tea-python run workflow.yaml  --stream

# With verbose logging
tea-python run workflow.yaml  -vv
```

**Note:** The `--show-graph` flag displays an ASCII visualization of the workflow structure, highlighting the currently executing node and marking completed nodes with ✓. This is especially useful for complex parallel workflows where you want to see progress at a glance. **Always combine with `` for long-running story workflows.**

### Alternative Tea Implementations

When you have multiple tea implementations (Python, Rust, or different versions), use the appropriate executable directly:

```bash
# Python implementation v0.9.82+ (required for run --from-dot)
tea-python run workflow.yaml

# Rust implementation
tea-rust run workflow.yaml
```

---

## LLM Generation Instructions

When asked to create a workflow orchestration DOT file:

### 0. Discover Paths (CRITICAL - Do This First!)

**Before generating any DOT file, use tools to discover actual paths:**

```bash
# Step 1: Find BMAD workflow files in the project
glob_pattern: "**/bmad-story-development.yaml"

# Step 2: Extract PROJECT_ROOT and WORKFLOW_PATH from the found path
# Example A (TEA repo): /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml
#   WORKFLOW_PATH = /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml
#
# Example B (Other project): /home/user/myapp/workflows/bmad-story-development.yaml
#   WORKFLOW_PATH = /home/user/myapp/workflows/bmad-story-development.yaml

# Step 3: Find story files in the project
glob_pattern: "**/docs/stories/*.md"
# OR
glob_pattern: "**/stories/*.md"

# Step 4: Store discovered paths for use in DOT commands
# WORKFLOW_PATH = <discovered workflow path>
# STORIES_PATH = <discovered stories directory>
```

**Store discovered WORKFLOW_PATH and STORIES_PATH for use in all commands.**

### 1. Identify Stories/Tasks

Extract the list of items to orchestrate:
- Story IDs (e.g., `TEA-PARALLEL-001.1`)
- File paths discovered in Step 0 (e.g., `<STORIES_PATH>/TEA-PARALLEL-001.1-executor-abstraction.md`)
- Dependencies between items

### 2. Determine Phase Structure

Group items into phases based on:
- Dependencies (items with no deps go in early phases)
- Logical grouping (related items in same phase)
- Parallelization opportunity (independent items in same phase run in parallel)

### 3. Generate DOT File

```dot
digraph <workflow_name> {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    // For each phase:
    subgraph cluster_<phase_name> {
        label="<N>. <Phase Label>";
        // For each item in phase:
        <item_id> [label="<Display Name>", command="tea-python run <workflow>  --input '{\"arg\": \"<path>\"}'"];
    }

    // Define edges based on dependencies
    Start -> <first_items>;
    <item_a> -> <item_b>;
    <last_items> -> End;
}
```

### 4. Save DOT File

Save the DOT file to the standard location:

```bash
# Standard location
examples/dot/<workflow-name>.dot
```

### 5. Output Commands (REQUIRED)

**After generating the DOT file, ALWAYS output these commands with full discovered paths:**

#### Workflow Mode (RECOMMENDED for story orchestration):
```bash
# Run a workflow for each DOT node (node label becomes state.arg)
tea-python run --from-dot <DOT_OUTPUT>/<filename>.dot \
    --dot-workflow <WORKFLOW_PATH>/bmad-story-validation.yaml \
    --dot-max-parallel 4

# With verbose LLM output
TEA_SHELL_VERBOSE=1 tea-python run --from-dot <DOT_OUTPUT>/<filename>.dot \
    --dot-workflow <WORKFLOW_PATH>/bmad-story-validation.yaml --dot-max-parallel 3
```

#### Command Mode (for DOT files with embedded commands):
```bash
tea-python run --from-dot <DOT_OUTPUT>/<filename>.dot --dot-max-parallel 4
```

Monitor: `tmux attach -t tea-dot`

#### Alternative - Execute via YAML (if YAML generation is needed):
```bash
tea-python run <DOT_OUTPUT>/<filename>.yaml --show-graph
```

**Example with discovered paths:**
```bash
# Workflow Mode (RECOMMENDED)
tea-python run --from-dot /home/user/project/examples/dot/epic-stories.dot \
    --dot-workflow /home/user/project/examples/workflows/bmad-story-validation.yaml \
    --dot-max-parallel 4

# Command Mode
tea-python run --from-dot /home/user/project/examples/dot/my-workflow.dot \
    --dot-session my-workflow --dot-max-parallel 3

# Monitor progress
tmux attach -t tea-dot
```

---

## Complete Example: Epic Implementation

Given an epic with 5 stories and this dependency structure:
- 001.1 has no dependencies
- 001.2 depends on 001.1
- 001.3 depends on 001.2
- 001.4 depends on 001.3
- 001.5 depends on 001.4

### Step 1: Discover Paths

```bash
# LLM discovers workflow paths using Glob tool
Glob pattern: "**/bmad-story-development.yaml"
Result: /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml

# Store discovered paths
WORKFLOW_PATH = /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml

# Find stories directory
Glob pattern: "**/docs/stories/*.md"
Result: /home/user/project/the_edge_agent/docs/stories/...

# Store stories path
STORIES_PATH = /home/user/project/the_edge_agent/docs/stories

# DOT output directory (sibling to workflows or in examples/dot)
DOT_OUTPUT = /home/user/project/the_edge_agent/examples/dot
```

### Step 2: Generate DOT File

```dot
digraph epic_implementation {
    rankdir=TB;
    node [shape=box];

    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    subgraph cluster_foundation {
        label="1. Foundation";
        story_1_1 [label="EPIC-001.1", command="tea-python run /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/home/user/project/the_edge_agent/docs/stories/EPIC-001.1-core-setup.md\"}'"];
    }

    subgraph cluster_core {
        label="2. Core Implementation";
        story_1_2 [label="EPIC-001.2", command="tea-python run /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/home/user/project/the_edge_agent/docs/stories/EPIC-001.2-main-feature.md\"}'"];
    }

    subgraph cluster_extensions {
        label="3. Extensions";
        story_1_3 [label="EPIC-001.3", command="tea-python run /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/home/user/project/the_edge_agent/docs/stories/EPIC-001.3-extension-a.md\"}'"];
        story_1_4 [label="EPIC-001.4", command="tea-python run /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/home/user/project/the_edge_agent/docs/stories/EPIC-001.4-extension-b.md\"}'"];
    }

    subgraph cluster_docs {
        label="4. Documentation";
        story_1_5 [label="EPIC-001.5", command="tea-python run /home/user/project/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/home/user/project/the_edge_agent/docs/stories/EPIC-001.5-documentation.md\"}'"];
    }

    Start -> story_1_1;
    story_1_1 -> story_1_2;
    story_1_2 -> story_1_3;
    story_1_2 -> story_1_4;
    story_1_3 -> story_1_5;
    story_1_4 -> story_1_5;
    story_1_5 -> End;
}
```

**Note:** The paths `/home/user/project/the_edge_agent/` are examples. The LLM MUST discover the actual path at runtime using tools.

### Step 3: Save DOT File

```bash
# Save DOT to discovered output directory (using discovered DOT_OUTPUT path)
Write to: /home/user/project/the_edge_agent/examples/dot/epic-implementation.dot
```

### Step 4: Output Commands (with discovered paths)

**After saving the DOT file, output these executable commands:**

```bash
# Command Mode: Execute DOT with embedded commands
tea-python run --from-dot /home/user/project/the_edge_agent/examples/dot/epic-implementation.dot \
    --dot-session epic-impl \
    --dot-max-parallel 3

# Workflow Mode (RECOMMENDED): Run workflow for each node
tea-python run --from-dot /home/user/project/the_edge_agent/examples/dot/epic-stories.dot \
    --dot-workflow /home/user/project/the_edge_agent/examples/workflows/bmad-story-validation.yaml \
    --dot-max-parallel 4

# With verbose LLM output
TEA_SHELL_VERBOSE=1 tea-python run --from-dot stories.dot --dot-workflow bmad-story-validation.yaml --dot-max-parallel 3

# Monitor execution in tmux
tmux attach -t tea-dot
```

**Note:** All paths in the output commands MUST be the actual discovered absolute paths, not placeholders.

---

## Best Practices

### Path Discovery (CRITICAL)
- **ALWAYS discover paths at runtime** using Glob or Bash tools
- **NEVER hardcode paths** like `/home/fabricio/...` in templates
- **Verify files exist** before including them in the DOT file
- **Store PROJECT_ROOT** (derived from discovered workflow path) and reuse it for all command paths

### Labels (CRITICAL for Workflow Mode)
- **Keep labels SHORT (max 30 chars recommended)**: Use IDs like `PIR.1.person-table-accept` instead of full paths
- **Avoid full file paths as labels**: tmux window names are truncated to 30 chars, causing collisions
- **Avoid special characters**: No newlines (`\n`), quotes, or special chars in labels
- Labels are used as dict keys, tmux window names, AND passed as `state.arg` in Workflow Mode

**Example - Good vs Bad Labels:**

```dot
// GOOD - Short, unique labels that work well with tmux
PIR_1 [label="PIR.1.person-table-accept"];
MIR_2 [label="MIR.2.matter-table-expand"];

// BAD - Full paths get truncated and cause collisions
// Both would become "_home_fabricio_src_spa-base_do" in tmux!
PIR_1 [label="/home/fabricio/src/spa-base/docs/stories/PIR.1.person-table-row-level-accept-reject.md"];
MIR_2 [label="/home/fabricio/src/spa-base/docs/stories/MIR.2.matter-table-expandable-parent-child-rows.md"];
```

**Workflow Design for Short Labels:**

When using Workflow Mode with short labels, design your workflow to resolve the full path:

```yaml
# In your workflow YAML, add a node to resolve the story path
nodes:
  - name: resolve_story_path
    run: |
      import os, glob
      arg = state.get("arg", "")
      # If already a full path, use it
      if os.path.isfile(arg):
          return {"story_path": arg}
      # Otherwise, search for matching story
      pattern = f"docs/stories/{arg}*.md"
      matches = glob.glob(pattern)
      if matches:
          return {"story_path": matches[0]}
      return {"story_path": arg}
```

### Commands
- **Use absolute paths**: Discovered at runtime via `<WORKFLOW_PATH>` and `<STORIES_PATH>`
- **Use JSON input format**: `--input '{\"arg\": \"<path>\"}'` with proper escaping
- **Escape double quotes**: Use `\"` for quotes inside the command string
- **Test commands independently**: Run the command in a shell before embedding in DOT

### Example: Good vs Bad

```dot
// GOOD - absolute path discovered at runtime, simple label, with timeout
story_1 [label="STORY-001", command="tea-python run /discovered/path/the_edge_agent/examples/workflows/bmad-story-development.yaml  --input '{\"arg\": \"/discovered/path/the_edge_agent/docs/stories/STORY-001.md\"}'"];

// BAD - relative path (may not resolve correctly), multi-line label, no timeout
story_1 [label="STORY-001\nDescription", command="tea-python run examples/workflows/bmad-story-development.yaml --input '{\"arg\": \"docs/stories/STORY-001.md\"}'"];
```

---

## Validation Checklist

Before generating:

- [ ] All task nodes have `command` attribute
- [ ] Start/End nodes use `shape=ellipse` (no command needed)
- [ ] Each phase is wrapped in `subgraph cluster_*` block
- [ ] Edges define correct execution order
- [ ] No circular dependencies
- [ ] Labels contain only alphanumeric, `-`, `_`, `.` characters
- [ ] Commands use JSON input format with proper escaping: `'{\"arg\": \"<path>\"}'`
- [ ] File paths are correct and exist

---

## Error Handling

| Error | Cause | Solution |
|-------|-------|----------|
| `Missing command attribute` | Node without `command` | Add `command="..."` to node |
| `Circular dependency` | A -> B -> A | Redesign flow to remove cycle |
| `Invalid JSON in input` | Bad escaping | Use `'{\"key\": \"value\"}'` format |
| `File not found` | Wrong path | Verify story file paths exist |

---

## Related Documentation

- [DOT-to-YAML LLM Reference](./DOT_TO_YAML_LLM_REFERENCE.md) - General DOT conversion guide
- [YAML Reference](./YAML_REFERENCE.md) - TEA YAML workflow syntax
