# DOT-to-YAML Generation Guide for LLMs

This document provides instructions for generating DOT (Graphviz) files that can be converted to TEA YAML workflows using the `tea from dot` command.

## Two Execution Modes

The TEA DOT-to-YAML converter supports **two mutually exclusive modes**:

| Mode | Flag | Use Case |
|------|------|----------|
| **Template Mode** | `--command "cmd {{ item }}"` | All nodes run same command with different items |
| **Per-Node Mode** | `--use-node-commands` | Each node has its own specific command |

**Important**: These modes cannot be combined. Choose one based on your workflow needs.

## Quick Reference

### Basic DOT Structure

```dot
digraph workflow_name {
    // Graph settings
    rankdir=TB;
    node [shape=box];

    // Cluster = Parallel execution phase
    subgraph cluster_phase_name {
        label="Phase Label";

        // Nodes with per-node commands
        node_id [label="Display Name", command="shell command to execute"];
    }

    // Edges define execution order
    Start -> node1;
    node1 -> End;
}
```

### Key Attributes

| Attribute | Location | Purpose |
|-----------|----------|---------|
| `label` | Node | Display name for the task |
| `command` | Node | **TEA-TOOLS-002**: Shell command to execute for this specific node |
| `shape` | Node | Visual shape (box, ellipse, circle) |
| `label` | Cluster | Phase/group display name |

## Generating Parallel Workflows

### Pattern 1: Homogeneous Parallel (Same Command Template)

Use when all tasks in a phase run the same command with different arguments.

```dot
digraph build_all {
    subgraph cluster_build {
        label="Build Services";
        service_a [label="Service A"];
        service_b [label="Service B"];
        service_c [label="Service C"];
    }
}
```

**CLI Usage:**
```bash
tea from dot workflow.dot -c "make build-{{ item }}"
```

### Pattern 2: Heterogeneous Parallel (Per-Node Commands)

Use when each task requires a different command. **ALL nodes must have a `command` attribute.**

```dot
digraph multi_lang_build {
    subgraph cluster_build {
        label="Build Phase";
        frontend [label="Frontend", command="npm run build"];
        backend [label="Backend", command="cargo build --release"];
        docs [label="Documentation", command="mkdocs build"];
    }

    // Start/End markers (ellipse shape) are exempt from command requirement
    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    Start -> frontend;
    Start -> backend;
    Start -> docs;
    frontend -> End;
    backend -> End;
    docs -> End;
}
```

**CLI Usage:**
```bash
tea from dot workflow.dot --use-node-commands
```

**Validation Rules for Per-Node Mode:**
- ALL nodes must have `command` attribute
- Exception: Nodes with shapes `ellipse`, `circle`, `point`, `doublecircle` (Start/End markers)
- Clear error message lists any nodes missing commands

## Multi-Phase Workflows

Create sequential phases that each execute in parallel internally:

```dot
digraph ci_pipeline {
    // Phase 1: Build
    subgraph cluster_build {
        label="1. Build";
        build_frontend [label="Frontend", command="npm run build"];
        build_backend [label="Backend", command="cargo build"];
    }

    // Phase 2: Test
    subgraph cluster_test {
        label="2. Test";
        unit_tests [label="Unit", command="pytest tests/unit/"];
        integration [label="Integration", command="pytest tests/integration/"];
    }

    // Phase 3: Deploy
    subgraph cluster_deploy {
        label="3. Deploy";
        deploy_staging [label="Staging", command="kubectl apply -f k8s/staging/"];
        deploy_prod [label="Production", command="kubectl apply -f k8s/prod/"];
    }

    // Phase ordering (implicit from cluster order)
    // TEA generator handles phase sequencing automatically
}
```

## Command Attribute Best Practices

### 1. Use Full Commands
```dot
// Good: Complete command
node_a [label="Build", command="cd frontend && npm run build"];

// Avoid: Incomplete commands
node_a [label="Build", command="npm"];
```

### 2. Handle Paths and Variables
```dot
// Environment variables work
node_a [label="Deploy", command="kubectl apply -f $KUBE_DIR/"];

// Relative paths from working directory
node_b [label="Test", command="pytest tests/ -v"];
```

### 3. Multi-Step Commands
```dot
// Chain with && for sequential steps
node_a [label="Build & Test", command="npm run build && npm test"];

// Use semicolons to continue on failure
node_b [label="Cleanup", command="rm -rf dist; mkdir dist"];
```

### 4. Quotes in Commands
```dot
// Single quotes inside double quotes
node_a [label="Echo", command="echo 'Hello World'"];

// Escape double quotes
node_b [label="JSON", command="echo '{\"key\": \"value\"}'"];
```

## Generated YAML Structure

When converted with `--use-node-commands`, the DOT file produces this YAML structure:

```yaml
name: workflow-name
description: Generated from DOT diagram

nodes:
  - name: setup
    run: |
      # Initialize phase items and per-node command mapping
      state["phase1_items"] = ["Frontend", "Backend"]
      state["_phase1_commands"] = {
          "Frontend": "npm run build",
          "Backend": "cargo build"
      }
      return state

  - name: phase1_parallel
    type: dynamic_parallel
    items: "{{ state.phase1_items }}"
    item_var: item
    max_concurrency: 3
    fan_in: phase1_collect
    steps:
      - name: execute
        run: |
          import subprocess

          item = state.get("item", "")
          commands = state.get("_phase1_commands", {})
          cmd = commands.get(item)

          if not cmd:
              return {"item": item, "success": False, "error": "No command defined"}

          result = subprocess.run(
              cmd, shell=True, capture_output=True, text=True,
              executable='/bin/bash', timeout=300
          )
          return {
              "item": item,
              "success": result.returncode == 0,
              "stdout": result.stdout,
              "stderr": result.stderr
          }
    output: phase1_results

  - name: phase1_collect
    fan_in: true
    run: |
      # Collect results from parallel execution
      results = state.get("phase1_results", [])
      success_count = sum(1 for r in results if r.get("success", False))
      state["phase1_complete"] = True
      state["phase1_success_count"] = success_count
      return state

edges:
  - from: __start__
    to: setup
  - from: setup
    to: phase1_parallel
  - from: phase1_collect
    to: __end__
```

## CLI Reference

```bash
# Mode 1: Template mode - all nodes use same command template
tea from dot workflow.dot -c "make build-{{ item }}" -o output.yaml

# Mode 2: Per-node mode - each node has command attribute (TEA-TOOLS-002)
tea from dot workflow.dot --use-node-commands -o output.yaml

# ERROR: Cannot combine modes (mutually exclusive)
# tea from dot workflow.dot --use-node-commands -c "fallback"  # INVALID

# With tmux execution mode
tea from dot workflow.dot --use-node-commands --tmux -s my-session

# Validate output before writing
tea from dot workflow.dot --use-node-commands --validate

# Custom concurrency (default: 3)
tea from dot workflow.dot --use-node-commands -m 5

# Custom workflow name
tea from dot workflow.dot --use-node-commands -n "my-pipeline"
```

## Common Generation Patterns for LLMs

When generating DOT files programmatically, use these patterns:

### Build System Pattern
```dot
digraph build_system {
    subgraph cluster_build {
        label="Build";
        ${for each service in services}
        ${service.id} [label="${service.name}", command="${service.build_cmd}"];
        ${end for}
    }
}
```

### CI/CD Pipeline Pattern
```dot
digraph cicd {
    subgraph cluster_lint {
        label="Lint";
        eslint [label="ESLint", command="eslint ."];
        prettier [label="Prettier", command="prettier --check ."];
    }

    subgraph cluster_test {
        label="Test";
        unit [label="Unit Tests", command="npm test"];
        e2e [label="E2E Tests", command="playwright test"];
    }

    subgraph cluster_deploy {
        label="Deploy";
        staging [label="Staging", command="deploy.sh staging"];
        prod [label="Production", command="deploy.sh prod"];
    }
}
```

### Data Processing Pattern
```dot
digraph etl {
    subgraph cluster_extract {
        label="Extract";
        db_export [label="Database", command="pg_dump -f data.sql"];
        api_fetch [label="API Data", command="curl -o api.json $API_URL"];
    }

    subgraph cluster_transform {
        label="Transform";
        normalize [label="Normalize", command="python transform.py"];
        validate [label="Validate", command="python validate.py"];
    }

    subgraph cluster_load {
        label="Load";
        warehouse [label="Data Warehouse", command="python load_dw.py"];
        cache [label="Redis Cache", command="python load_cache.py"];
    }
}
```

## Validation Checklist

Before submitting a DOT file for conversion:

### For Template Mode (`--command`)
1. [ ] All nodes have `label` attribute
2. [ ] Clusters have `label` attribute for phase naming
3. [ ] No circular dependencies exist
4. [ ] Each phase is wrapped in `subgraph cluster_*` block

### For Per-Node Mode (`--use-node-commands`)
1. [ ] All nodes have `label` attribute
2. [ ] **ALL nodes have `command` attribute** (except Start/End markers)
3. [ ] Start/End markers use `shape=ellipse` or `shape=circle`
4. [ ] Clusters have `label` attribute for phase naming
5. [ ] No circular dependencies exist
6. [ ] Commands don't contain unescaped special characters
7. [ ] Each phase is wrapped in `subgraph cluster_*` block

## Error Handling

| Error | Cause | Solution |
|-------|-------|----------|
| `DotParseError` | Invalid DOT syntax | Check DOT syntax, balanced brackets |
| `CircularDependencyError` | A -> B -> C -> A cycle | Remove cycles or redesign flow |
| `--use-node-commands and --command are mutually exclusive` | Both flags specified | Choose one mode only |
| `--command is required (or use --use-node-commands)` | Neither flag specified | Add `--command` or `--use-node-commands` |
| `ALL nodes to have command attribute. Missing: X, Y` | Missing command attrs | Add `command` attribute to listed nodes, or use `shape=ellipse` for markers |

## Complete Example: Multi-Phase CI/CD Pipeline

Here's a complete, valid DOT file for per-node commands mode:

```dot
// CI/CD Pipeline with per-node commands
// Usage: tea from dot cicd.dot --use-node-commands -o cicd.yaml

digraph cicd_pipeline {
    rankdir=TB;
    node [shape=box];

    // Start/End markers (ellipse shape = no command required)
    Start [label="Start", shape=ellipse];
    End [label="End", shape=ellipse];

    // Phase 1: Linting and formatting checks
    subgraph cluster_lint {
        label="1. Code Quality";
        eslint [label="ESLint", command="npm run lint"];
        prettier [label="Prettier", command="prettier --check ."];
        typecheck [label="TypeScript", command="tsc --noEmit"];
    }

    // Phase 2: Testing
    subgraph cluster_test {
        label="2. Tests";
        unit [label="Unit Tests", command="npm run test:unit"];
        integration [label="Integration", command="npm run test:integration"];
        e2e [label="E2E Tests", command="npx playwright test"];
    }

    // Phase 3: Build
    subgraph cluster_build {
        label="3. Build";
        frontend [label="Frontend", command="npm run build:frontend"];
        backend [label="Backend", command="npm run build:backend"];
        docker [label="Docker Image", command="docker build -t app:latest ."];
    }

    // Edges: Start -> Phase 1
    Start -> eslint;
    Start -> prettier;
    Start -> typecheck;

    // Phase 1 -> Phase 2 (fan-in/fan-out handled automatically)
    eslint -> unit;
    prettier -> integration;
    typecheck -> e2e;

    // Phase 2 -> Phase 3
    unit -> frontend;
    integration -> backend;
    e2e -> docker;

    // Phase 3 -> End
    frontend -> End;
    backend -> End;
    docker -> End;
}
```

**Convert to YAML:**
```bash
tea from dot cicd.dot --use-node-commands -o cicd.yaml --validate
```

**Key Points:**
1. `Start` and `End` use `shape=ellipse` so they don't require `command`
2. All other nodes have `command` attribute with their specific shell command
3. Phases are defined with `subgraph cluster_*` blocks
4. Edges define the execution flow between phases
