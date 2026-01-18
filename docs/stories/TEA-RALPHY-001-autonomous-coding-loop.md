# Epic TEA-RALPHY-001: Autonomous AI Coding Loop

## Status
Draft

## Epic Goal

Implement a Ralphy-compatible autonomous AI coding loop as TEA YAML workflows, enabling parallel task execution with multiple AI engines, automatic testing, git branch management, and PR creation - all orchestrated through declarative YAML agents.

## Background Context

[Ralphy](https://github.com/michaelshimeles/ralphy) is a bash automation script (~1500 lines) that orchestrates AI assistants to autonomously complete development tasks. TEA can provide the same functionality with:

- **Declarative YAML** instead of imperative bash
- **Checkpoint/resume** for interrupted runs
- **Composability** with testable, isolated nodes
- **Cross-runtime** support (Python and Rust)

### Existing TEA Infrastructure

| Component | Status | Notes |
|-----------|--------|-------|
| `llm.call` shell providers | ✅ Built | claude, codex, gemini, qwen |
| Parallel fan-out/fan-in | ✅ Built | TEA-WASM-001.5 |
| Git operations | ✅ Available | via `run:` subprocess |
| Secrets management | ✅ Built | TEA-BUILTIN-012 |
| Retry patterns | ✅ Built | TEA-YAML-005 |

### Gap Analysis

| Ralphy Feature | TEA Status | Required Work |
|----------------|------------|---------------|
| Parse Markdown PRD | ❌ Missing | `markdown.parse` action |
| Parse YAML tasks | ✅ Native | YAML parsing built-in |
| Parse GitHub Issues | ❌ Missing | `github.list_issues` action |
| Parse BMad stories | ❌ Missing | `markdown.parse` + BMad schema |
| OpenCode provider | ❌ Missing | Add to shell providers |
| Cursor provider | ❌ Missing | Add to shell providers |
| Git worktree isolation | ❌ Missing | Workflow pattern |
| Token tracking | ⚠️ Partial | Aggregation needed |
| Cost estimation | ❌ Missing | Pricing lookup |
| Progress reporting | ❌ Missing | Callback hooks |
| Desktop notifications | ❌ Missing | `notify.send` action |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        TEA-RALPHY Architecture                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐                  │
│  │ Task Sources │    │ AI Engines   │    │ Git Workflow │                  │
│  ├──────────────┤    ├──────────────┤    ├──────────────┤                  │
│  │ • Markdown   │    │ • Claude     │    │ • Branch     │                  │
│  │ • YAML       │───▶│ • Codex      │───▶│ • Commit     │                  │
│  │ • GitHub     │    │ • Gemini     │    │ • PR Create  │                  │
│  │ • BMad Story │    │ • Cursor     │    │ • Worktree   │                  │
│  └──────────────┘    └──────────────┘    └──────────────┘                  │
│         │                   │                   │                           │
│         ▼                   ▼                   ▼                           │
│  ┌──────────────────────────────────────────────────────────────┐          │
│  │                    Orchestration Layer                        │          │
│  ├──────────────────────────────────────────────────────────────┤          │
│  │ • Sequential/Parallel execution    • Retry with backoff      │          │
│  │ • Checkpoint/resume                • Token aggregation       │          │
│  │ • Test/lint validation             • Progress callbacks      │          │
│  └──────────────────────────────────────────────────────────────┘          │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Story List

| Story | Title | Priority | Dependencies |
|-------|-------|----------|--------------|
| TEA-RALPHY-001.0 | [md-graph-parser Shared Crate](./TEA-RALPHY-001.0.md-graph-parser-crate.md) | P0 | None (external repo) |
| TEA-RALPHY-001.1 | [Python `markdown.parse` Action](./TEA-RALPHY-001.1.python-markdown-parse.md) | P0 | 001.0 |
| TEA-RALPHY-001.2 | [Rust `markdown.parse` Action](./TEA-RALPHY-001.2.rust-markdown-parse.md) | P0 | 001.0 |
| TEA-RALPHY-001.3 | [GitHub Issues Integration](./TEA-RALPHY-001.3.github-issues-integration.md) | P1 | None |
| TEA-RALPHY-001.4 | [BMad Story Task Extraction](./TEA-RALPHY-001.4.bmad-story-task-extraction.md) | P1 | 001.1, 001.2 |
| TEA-RALPHY-001.5 | [Additional Shell Providers](./TEA-RALPHY-001.5.additional-shell-providers.md) | P1 | None |
| TEA-RALPHY-001.6 | [Execution Modes (Sequential, Parallel, Graph)](./TEA-RALPHY-001.6.execution-modes.md) | P1 | 001.10 (for graph mode) |
| TEA-RALPHY-001.7 | [Token Tracking & Cost Estimation](./TEA-RALPHY-001.7.token-tracking-cost-estimation.md) | P2 | None |
| TEA-RALPHY-001.8 | [Progress Reporting & Notifications](./TEA-RALPHY-001.8.progress-reporting-notifications.md) | Backlog | None |
| TEA-RALPHY-001.9 | [Integration Workflow Agent](./TEA-RALPHY-001.9.integration-workflow-agent.md) | P0 | 001.1-001.6 |
| TEA-RALPHY-001.10 | [Dynamic Dependency Analysis & DOT Generation](./TEA-RALPHY-001.10.dynamic-dependency-analysis.md) | P1 | 001.1, 001.4 |

### Shared Library Note

> **TEA-RALPHY-001.0** is a separate Rust crate (`md-graph-parser`) hosted at https://github.com/fabceolin/md-graph-parser.
> It provides structured Markdown parsing shared between TEA and agentfs, ensuring cross-runtime schema parity.

---

## Story TEA-RALPHY-001.10: Dynamic Dependency Analysis & DOT Generation

### Story

**As a** workflow developer,
**I want** an LLM to analyze story dependencies and generate an optimal DOT orchestration graph,
**So that** I can automatically maximize parallel execution across multiple stories.

### Acceptance Criteria

1. Accept multiple files via glob pattern or explicit list
2. Use LLM to analyze each file and extract dependencies
3. Generate DOT file with clusters for parallel phases
4. Maximize parallelization based on dependency graph
5. Output DOT file compatible with `tea from dot --use-node-commands`
6. Support the DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md format

### Tasks / Subtasks

- [ ] Create `ralphy.analyze_dependencies` action (AC: 2)
  - [ ] Read each story file
  - [ ] Extract `## Dependencies` section
  - [ ] Use LLM to infer implicit dependencies from content
  - [ ] Return dependency graph as adjacency list
- [ ] Create `ralphy.generate_dot` action (AC: 3, 4, 5)
  - [ ] Topological sort of dependency graph
  - [ ] Group independent stories into parallel phases
  - [ ] Generate DOT with `subgraph cluster_*` structure
  - [ ] Include `command` attributes per node
- [ ] Create meta-workflow YAML (AC: 1, 6)
  - [ ] Accept `files` list or `glob_pattern`
  - [ ] Chain: glob → analyze → generate DOT → `tea from dot` → execute
- [ ] Integration with existing DOT guide

### Dev Notes

#### Input Formats

```yaml
# Option 1: Explicit file list
initial_state:
  files:
    - docs/stories/TEA-RALPHY-001.1.md
    - docs/stories/TEA-RALPHY-001.2.md

# Option 2: Glob pattern (resolved at runtime)
initial_state:
  glob_pattern: "docs/stories/TEA-RALPHY-*.md"

# Option 3: GitHub Issues
initial_state:
  source: "github:owner/repo"
  labels: ["ready-for-dev"]
```

#### Dependency Analysis Prompt

```yaml
- name: analyze_dependencies
  uses: llm.call
  with:
    provider: shell
    shell_provider: claude
    messages:
      - role: user
        content: |
          Analyze these story files and identify dependencies between them.

          Files:
          {% for file in state.files %}
          === {{ file.path }} ===
          {{ file.content }}
          {% endfor %}

          Output a JSON dependency graph:
          {
            "nodes": ["STORY-1", "STORY-2", ...],
            "edges": [
              {"from": "STORY-1", "to": "STORY-2", "reason": "STORY-2 uses API from STORY-1"}
            ]
          }

          Rules:
          - A story depends on another if it uses code/APIs defined there
          - Foundation/setup stories should have no dependencies
          - Identify implicit dependencies from content, not just explicit ## Dependencies
  output: dependency_graph
```

#### DOT Generation Template

```yaml
- name: generate_dot
  run: |
    from the_edge_agent.actions.dot_generator import generate_parallel_dot

    dot_content = generate_parallel_dot(
        nodes=state["dependency_graph"]["nodes"],
        edges=state["dependency_graph"]["edges"],
        workflow_path=state["workflow_path"],
        stories_path=state["stories_path"],
    )
    return {"dot_content": dot_content, "dot_path": "generated-workflow.dot"}
```

#### Full Meta-Workflow

```yaml
name: ralphy-meta-orchestrator
description: Analyze dependencies and generate optimal parallel workflow

nodes:
  - name: resolve_files
    run: |
      import glob
      if "glob_pattern" in state:
          files = glob.glob(state["glob_pattern"])
      else:
          files = state.get("files", [])
      return {"files": files}

  - name: read_files
    run: |
      contents = []
      for f in state["files"]:
          with open(f) as fp:
              contents.append({"path": f, "content": fp.read()})
      return {"file_contents": contents}

  - name: analyze_dependencies
    uses: llm.call
    with:
      provider: shell
      shell_provider: claude
      messages:
        - role: user
          content: |
            {{ include('prompts/analyze-dependencies.md') }}

            Files: {{ state.file_contents | tojson }}
    output: dependency_graph

  - name: generate_dot
    uses: ralphy.generate_dot
    with:
      graph: "{{ state.dependency_graph }}"
      workflow_template: "bmad-story-development.yaml"
    output: dot_file

  - name: convert_to_yaml
    run: |
      import subprocess
      result = subprocess.run([
          "tea", "from", "dot", state["dot_file"],
          "--use-node-commands", "-o", "generated-workflow.yaml"
      ], capture_output=True, text=True)
      return {"yaml_path": "generated-workflow.yaml"}

  - name: execute_workflow
    run: |
      import subprocess
      subprocess.run(["tea", "run", state["yaml_path"]], check=True)
      return {"status": "completed"}

edges:
  - from: __start__
    to: resolve_files
  - from: resolve_files
    to: read_files
  - from: read_files
    to: analyze_dependencies
  - from: analyze_dependencies
    to: generate_dot
  - from: generate_dot
    to: convert_to_yaml
  - from: convert_to_yaml
    to: execute_workflow
  - from: execute_workflow
    to: __end__
```

### Testing

```bash
# Test with explicit files
tea run examples/workflows/ralphy-meta-orchestrator.yaml \
  --input '{"files": ["docs/stories/TEA-RALPHY-001.1.md", "docs/stories/TEA-RALPHY-001.2.md"]}'

# Test with glob
tea run examples/workflows/ralphy-meta-orchestrator.yaml \
  --input '{"glob_pattern": "docs/stories/TEA-RALPHY-*.md"}'
```

---

## Story TEA-RALPHY-001.1: Python `markdown.parse` Action

### Story

**As a** workflow developer,
**I want** a `markdown.parse` action in the Python runtime,
**So that** I can extract tasks, sections, and variables from Markdown documents in YAML agents.

### Acceptance Criteria

1. Parse Markdown into structured sections (heading, paragraph, list, code, checklist)
2. Extract `- [ ]` and `- [x]` checklist items with completion status
3. Detect `{{variable}}` template variables
4. Parse YAML frontmatter if present
5. Support nested list items with indent tracking
6. Return standardized schema compatible with Rust implementation
7. Handle malformed Markdown gracefully with partial results

### Tasks / Subtasks

- [ ] Define output schema (AC: 6)
  - [ ] `ParsedDocument` dataclass with title, sections, variables, frontmatter
  - [ ] `ParsedSection` with type, level, content, items
  - [ ] `ChecklistItem` with text, checked, indent, ac_ref
- [ ] Implement parser using `mistune` or `markdown-it-py` (AC: 1, 2)
  - [ ] Section extraction (headers, paragraphs, code blocks)
  - [ ] Checklist extraction with checkbox state
  - [ ] Nested indent detection
- [ ] Add variable extraction (AC: 3)
  - [ ] Regex for `{{variable_name}}`
  - [ ] Collect unique variable names
- [ ] Add frontmatter parsing (AC: 4)
  - [ ] Detect `---` delimited YAML block
  - [ ] Parse into dict
- [ ] Register as built-in action (AC: 1)
  - [ ] Add to `actions/markdown_actions.py`
  - [ ] Export in action registry
- [ ] Add unit tests (AC: 7)
  - [ ] Test each section type
  - [ ] Test malformed input handling
  - [ ] Test BMad story format

### Dev Notes

#### Recommended Library: `mistune`

```python
import mistune
from dataclasses import dataclass
from typing import List, Optional, Dict, Any

@dataclass
class ChecklistItem:
    text: str
    checked: bool
    indent: int
    ac_ref: Optional[str] = None  # Extracted from "(AC: X)"

@dataclass
class ParsedSection:
    type: str  # heading, paragraph, list, code, checklist, blockquote
    level: Optional[int] = None
    content: str = ""
    items: List[ChecklistItem] = None
    language: Optional[str] = None  # For code blocks

@dataclass
class ParsedDocument:
    title: Optional[str]
    sections: List[ParsedSection]
    variables: List[str]
    frontmatter: Optional[Dict[str, Any]]
    tasks: List[ChecklistItem]  # Convenience: all checklist items
```

#### Action Signature

```python
def markdown_parse(
    content: str,
    extract: List[str] = None,  # ["tasks", "sections", "variables", "frontmatter"]
    **kwargs
) -> Dict[str, Any]:
    """
    Parse Markdown content into structured document.

    Args:
        content: Raw Markdown string
        extract: Optional list of components to extract (default: all)

    Returns:
        {
            "title": "Document Title",
            "sections": [...],
            "variables": ["var1", "var2"],
            "frontmatter": {...},
            "tasks": [...]
        }
    """
```

#### Reference Implementation

See GraphDocs parser: `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs`

### Testing

**Test Location:** `python/tests/test_markdown_actions.py`

```python
def test_parse_checklist():
    result = markdown_parse("""
## Tasks
- [ ] Task 1 (AC: 1)
  - [x] Subtask 1.1
- [x] Task 2
""")
    assert len(result["tasks"]) == 3
    assert result["tasks"][0]["checked"] == False
    assert result["tasks"][1]["checked"] == True
    assert result["tasks"][1]["indent"] == 1
```

---

## Story TEA-RALPHY-001.2: Rust `markdown.parse` Action

### Story

**As a** workflow developer,
**I want** a `markdown.parse` action in the Rust runtime,
**So that** YAML agents work identically in both Python and Rust.

### Acceptance Criteria

1. Same output schema as Python implementation
2. Use `pulldown-cmark` for parsing
3. Extract checklists with GitHub Flavored Markdown extension
4. Parse YAML frontmatter using `serde_yaml`
5. Variable extraction via regex
6. Expose as WASM-compatible action for tea-wasm-llm

### Tasks / Subtasks

- [ ] Define Rust structs matching Python schema (AC: 1)
  - [ ] `ParsedDocument`, `ParsedSection`, `ChecklistItem`
  - [ ] Serde serialization for JSON output
- [ ] Implement parser with `pulldown-cmark` (AC: 2, 3)
  - [ ] Enable `ENABLE_TASKLISTS` extension
  - [ ] Section extraction
  - [ ] Checklist state tracking
- [ ] Add frontmatter parsing (AC: 4)
  - [ ] Detect and strip YAML header
  - [ ] Parse with `serde_yaml`
- [ ] Add variable extraction (AC: 5)
  - [ ] Regex crate for `{{variable}}`
- [ ] Register as built-in action (AC: 1)
  - [ ] Add to action registry
- [ ] WASM bindings (AC: 6)
  - [ ] Export via wasm-bindgen
  - [ ] JSON serialization for JS interop

### Dev Notes

#### Dependencies

```toml
[dependencies]
pulldown-cmark = { version = "0.9", features = ["simd"] }
regex = "1"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
serde_yaml = "0.9"
```

#### Reference Implementation

Port from GraphDocs: `/home/fabricio/src/agentfs/sdk/rust/src/graphdocs/parser.rs`

---

## Story TEA-RALPHY-001.3: GitHub Issues Integration

### Story

**As a** workflow developer,
**I want** a `github.list_issues` action,
**So that** I can use GitHub Issues as a task source in YAML agents.

### Acceptance Criteria

1. List issues from a repository with optional label filter
2. Extract issue title, body, labels, assignees
3. Parse issue body for task checkboxes
4. Support pagination for large issue lists
5. Authenticate via `GITHUB_TOKEN` environment variable
6. Handle rate limiting gracefully

### Tasks / Subtasks

- [ ] Implement `github.list_issues` action (AC: 1, 2)
  - [ ] Use `gh api` CLI or `requests` library
  - [ ] Filter by labels, state, milestone
- [ ] Add checkbox extraction from issue body (AC: 3)
  - [ ] Reuse `markdown.parse` for body parsing
- [ ] Add pagination support (AC: 4)
  - [ ] `per_page` and `page` parameters
  - [ ] Auto-pagination option
- [ ] Authentication handling (AC: 5)
  - [ ] Read from env or secrets
- [ ] Rate limit handling (AC: 6)
  - [ ] Retry with backoff on 429

### Dev Notes

#### Action Signature

```python
def github_list_issues(
    repo: str,  # "owner/repo"
    labels: List[str] = None,
    state: str = "open",  # open, closed, all
    parse_body: bool = True,
    **kwargs
) -> Dict[str, Any]:
    """
    Returns:
        {
            "issues": [
                {
                    "number": 123,
                    "title": "Issue title",
                    "body": "...",
                    "labels": ["bug", "priority:high"],
                    "tasks": [...]  # If parse_body=True
                }
            ],
            "total_count": 45
        }
    """
```

---

## Story TEA-RALPHY-001.4: BMad Story Task Extraction

### Story

**As a** BMad workflow user,
**I want** to extract tasks from BMad story files,
**So that** I can use existing stories as task sources for autonomous execution.

### Acceptance Criteria

1. Parse BMad story markdown format (see `docs/stories/*.md`)
2. Extract `## Tasks / Subtasks` section
3. Extract `## Acceptance Criteria` section
4. Link tasks to AC references (e.g., "(AC: 1)")
5. Detect story status from `## Status` section
6. Support BMad template validation

### Tasks / Subtasks

- [ ] Create `bmad.parse_story` action (AC: 1-5)
  - [ ] Use `markdown.parse` as foundation
  - [ ] Extract BMad-specific sections
  - [ ] Map AC references
- [ ] Add template conformance check (AC: 6)
  - [ ] Load template from `.bmad-core/templates/story-tmpl.yaml`
  - [ ] Validate required sections present

### Dev Notes

#### BMad Story Structure

```markdown
# Story TEA-XXX-001.1: Title

## Status
Draft | Approved | InProgress | Review | Done

## Acceptance Criteria
1. First criterion
2. Second criterion

## Tasks / Subtasks
- [ ] Task 1 (AC: 1)
  - [ ] Subtask 1.1
  - [x] Subtask 1.2 ← Completed
- [x] Task 2 (AC: 2)
```

#### Output Schema

```python
{
    "story_id": "TEA-XXX-001.1",
    "title": "Title",
    "status": "Draft",
    "acceptance_criteria": [
        {"number": 1, "text": "First criterion"},
        {"number": 2, "text": "Second criterion"}
    ],
    "tasks": [
        {
            "text": "Task 1",
            "checked": False,
            "ac_refs": [1],
            "subtasks": [
                {"text": "Subtask 1.1", "checked": False},
                {"text": "Subtask 1.2", "checked": True}
            ]
        }
    ],
    "completion": {
        "total": 4,
        "completed": 2,
        "percentage": 50.0
    }
}
```

---

## Story TEA-RALPHY-001.5: Additional Shell Providers

### Story

**As a** workflow developer,
**I want** OpenCode and Cursor shell providers,
**So that** I can use all Ralphy-supported AI engines.

### Acceptance Criteria

1. Add `opencode` shell provider with correct CLI invocation
2. Add `cursor` shell provider with correct CLI invocation
3. Document CLI installation requirements
4. Test with mock CLI responses

### Tasks / Subtasks

- [x] Research OpenCode CLI interface ✅ COMPLETED
  - [x] Command: `opencode -p "prompt" -q`
  - [x] Quiet mode (`-q`) suppresses spinner for scripting
  - [x] JSON output available with `-f json`
- [x] Research Cursor CLI interface ✅ COMPLETED
  - [x] Command: `agent -p "prompt" --output-format text`
  - [x] Note: Cursor CLI is named `agent`, not `cursor`
- [ ] Add to `_get_default_shell_providers()` (AC: 1, 2)
  - [ ] Modify `python/src/the_edge_agent/actions/llm_actions.py`
  - [ ] Add `opencode` provider entry
  - [ ] Add `cursor` provider entry (using `agent` command)
- [ ] Add documentation (AC: 3)
  - [ ] Update `docs/python/actions-reference.md`
  - [ ] Document installation commands for each provider
- [ ] Add unit tests (AC: 4)
  - [ ] Add tests to `python/tests/test_llm_actions.py`
  - [ ] Mock subprocess calls
  - [ ] Test provider selection

### Dev Notes

#### Research Results (Verified)

| Provider | Command | Non-Interactive | Quiet Mode | Source |
|----------|---------|-----------------|------------|--------|
| **OpenCode** | `opencode` | `-p "prompt"` | `-q` | [Docs](https://opencode.ai/docs/cli/) |
| **Cursor** | `agent` | `-p "prompt"` | `--output-format text` | [Docs](https://cursor.com/docs/cli/overview) |

#### Implementation

```python
# python/src/the_edge_agent/actions/llm_actions.py

def _get_default_shell_providers() -> Dict[str, Dict[str, Any]]:
    return {
        "claude": {
            "command": "claude",
            "args": ["-p", "{prompt}", "--dangerously-skip-permissions"],
            "timeout": 108000,
        },
        "codex": {
            "command": "codex",
            "args": ["exec", "-"],
            "stdin_mode": "pipe",
            "timeout": 108000,
        },
        "gemini": {
            "command": "gemini",
            "args": ["-p", "{prompt}"],
            "timeout": 108000,
        },
        "qwen": {
            "command": "qwen",
            "args": ["-p", "{prompt}"],
            "timeout": 108000,
        },
        # NEW: OpenCode provider
        "opencode": {
            "command": "opencode",
            "args": ["-p", "{prompt}", "-q"],  # -q suppresses spinner
            "timeout": 108000,
        },
        # NEW: Cursor provider (CLI command is 'agent')
        "cursor": {
            "command": "agent",  # Cursor CLI is named 'agent'
            "args": ["-p", "{prompt}", "--output-format", "text"],
            "timeout": 108000,
        },
    }
```

#### Installation Requirements

```bash
# OpenCode (Go-based)
curl -fsSL https://raw.githubusercontent.com/opencode-ai/opencode/refs/heads/main/install | bash
# OR
brew install opencode-ai/tap/opencode
# OR
go install github.com/opencode-ai/opencode@latest

# Cursor CLI (requires Cursor subscription)
# Installed via Cursor app: Cursor > Install CLI Command
# Or download from: https://cursor.com/download
```

#### YAML Usage

```yaml
nodes:
  - name: process_with_opencode
    uses: llm.call
    with:
      provider: shell
      shell_provider: opencode
      messages:
        - role: user
          content: "Implement feature X"
    output: result

  - name: process_with_cursor
    uses: llm.call
    with:
      provider: shell
      shell_provider: cursor
      messages:
        - role: user
          content: "Review this code for security issues"
    output: review
```

### Testing

**Test Location:** `python/tests/test_llm_actions.py`

```python
import pytest
from unittest.mock import patch, MagicMock
from the_edge_agent.actions.llm_actions import _get_default_shell_providers

class TestShellProviders:
    def test_opencode_provider_config(self):
        """Test OpenCode provider configuration."""
        providers = _get_default_shell_providers()

        assert "opencode" in providers
        assert providers["opencode"]["command"] == "opencode"
        assert "-p" in providers["opencode"]["args"]
        assert "-q" in providers["opencode"]["args"]  # Quiet mode

    def test_cursor_provider_config(self):
        """Test Cursor provider configuration."""
        providers = _get_default_shell_providers()

        assert "cursor" in providers
        assert providers["cursor"]["command"] == "agent"  # CLI is 'agent'
        assert "-p" in providers["cursor"]["args"]
        assert "--output-format" in providers["cursor"]["args"]

    @patch("subprocess.run")
    def test_opencode_invocation(self, mock_run):
        """Test OpenCode CLI invocation."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="Implementation complete",
            stderr=""
        )

        # Invoke via llm.call with shell provider
        # ... test implementation
```

---

## Story TEA-RALPHY-001.6: Execution Modes (Sequential, Parallel, Graph)

### Story

**As a** workflow developer,
**I want** configurable execution modes (sequential, parallel, graph) for multi-task workflows,
**So that** I can choose the optimal strategy for task isolation, parallelization, and dependency management.

### Acceptance Criteria

#### Core Worktree Operations (Parallel Mode)
1. Create isolated worktree per parallel task
2. Automatic branch creation from base branch
3. Execute task within worktree context
4. Merge worktree changes back to main
5. Cleanup worktree after merge
6. Handle merge conflicts by delegating to AI

#### Execution Mode Selection
7. Support three execution modes via `settings.execution.mode`: `sequential`, `parallel`, `graph`
8. Default mode is `sequential` when not specified
9. Settings can be overridden at runtime via `--input` JSON/YAML or `--config` file

#### Sequential Mode
10. Tasks execute one at a time in order specified
11. All tasks share single working directory (no worktrees)
12. Single branch used for all changes

#### Parallel Mode
13. Each task gets isolated worktree with dedicated branch
14. Tasks execute concurrently (limited by `max_parallel`)
15. Results merged back in dependency order
16. Merge conflicts halt execution and return conflict info

#### Graph Mode
17. LLM analyzes task dependencies and generates DOT file
18. DOT file follows `DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` format
19. Execute via `tea from dot --use-node-commands` for maximum parallelization
20. **Git worktree/branching is DISABLED** (DOT handles orchestration)
21. All changes made directly to current working directory
22. DOT file saved to configurable location for debugging/reuse

### Tasks / Subtasks

- [ ] Create `ExecutionMode` enum and configuration
- [ ] Implement `SequentialExecutor` (AC: 10-12)
- [ ] Implement `ParallelExecutor` with git worktree (AC: 1-6, 13-16)
- [ ] Implement `GraphExecutor` with DOT generation (AC: 17-22)
- [ ] Support `--input` and `--config` overrides (AC: 9)
- [ ] Add integration tests for all modes

### Dev Notes

See full implementation details in the extracted story file: `TEA-RALPHY-001.6.execution-modes.md`

#### Mode Comparison

| Mode | Git Worktrees | Parallelization | Best For |
|------|---------------|-----------------|----------|
| **sequential** | ❌ No | ❌ None | Simple tasks |
| **parallel** | ✅ Yes | ✅ Concurrent | Isolated changes |
| **graph** | ❌ Disabled | ✅ DOT-optimized | Complex dependencies |

---

## Story TEA-RALPHY-001.7: Token Tracking & Cost Estimation

### Story

**As a** workflow operator,
**I want** token usage tracking and cost estimation,
**So that** I can monitor AI spending across workflow runs.

### Acceptance Criteria

1. Aggregate `usage` from all `llm.call` responses
2. Track input_tokens and output_tokens separately
3. Apply model-specific pricing (configurable)
4. Report totals at workflow completion
5. Store in workflow state for downstream nodes
6. Optional: persist to LTM for historical tracking

### Tasks / Subtasks

- [ ] Create `TokenTracker` class (AC: 1, 2, 5)
  - [ ] Create `python/src/the_edge_agent/tracking/token_tracker.py`
  - [ ] Implement `TokenTracker` dataclass with `input_tokens`, `output_tokens`, `model`, `timestamp`
  - [ ] Implement `TokenAggregator` class to accumulate across nodes
  - [ ] Add thread-safe accumulation for parallel execution
  - [ ] Store aggregated totals in `state["_token_usage"]`
- [ ] Create pricing configuration schema (AC: 3)
  - [ ] Define `CostConfig` in `python/src/the_edge_agent/tracking/cost_config.py`
  - [ ] Support YAML `settings.cost_tracking.models` configuration
  - [ ] Load default pricing from bundled `data/model_pricing.yaml`
  - [ ] Allow override via environment variable `TEA_COST_CONFIG_PATH`
- [ ] Integrate with `llm.call` action (AC: 1, 2)
  - [ ] Modify `python/src/the_edge_agent/actions/llm_actions.py`
  - [ ] Extract `usage` from API response (OpenAI format)
  - [ ] Extract `usage` from shell provider output (parse JSON if available)
  - [ ] Call `TokenAggregator.add()` after each LLM call
- [ ] Implement cost calculation (AC: 3, 4)
  - [ ] Create `calculate_cost(usage, model, config)` function
  - [ ] Formula: `(input_tokens / 1_000_000) * input_price + (output_tokens / 1_000_000) * output_price`
  - [ ] Handle unknown models with warning and zero cost
- [ ] Add summary reporting (AC: 4, 5)
  - [ ] Create `ralphy.token_summary` action
  - [ ] Output formatted table: model, input_tokens, output_tokens, cost
  - [ ] Store summary in `state["_cost_summary"]`
- [ ] Add LTM persistence (AC: 6)
  - [ ] Create `tracking.persist_to_ltm` action
  - [ ] Schema: `workflow_id`, `run_id`, `timestamp`, `total_cost`, `breakdown`
  - [ ] Support query by date range
- [ ] Add unit tests
  - [ ] Create `python/tests/test_token_tracker.py`
  - [ ] Test aggregation across multiple calls
  - [ ] Test cost calculation accuracy
  - [ ] Test thread-safety in parallel execution

### Dev Notes

#### Source Tree

```
python/src/the_edge_agent/
├── tracking/                    # NEW: Token tracking module
│   ├── __init__.py
│   ├── token_tracker.py         # TokenTracker, TokenAggregator
│   └── cost_config.py           # CostConfig, load_pricing()
├── actions/
│   └── llm_actions.py           # MODIFY: Add tracking integration
└── data/
    └── model_pricing.yaml       # NEW: Default pricing table
```

#### Token Tracker Implementation

```python
# python/src/the_edge_agent/tracking/token_tracker.py
from dataclasses import dataclass, field
from typing import Dict, List, Optional
from threading import Lock
from datetime import datetime

@dataclass
class TokenUsage:
    model: str
    input_tokens: int
    output_tokens: int
    timestamp: datetime = field(default_factory=datetime.utcnow)
    node_name: Optional[str] = None

@dataclass
class TokenAggregator:
    usages: List[TokenUsage] = field(default_factory=list)
    _lock: Lock = field(default_factory=Lock, repr=False)

    def add(self, usage: TokenUsage) -> None:
        with self._lock:
            self.usages.append(usage)

    def total_by_model(self) -> Dict[str, Dict[str, int]]:
        totals = {}
        for u in self.usages:
            if u.model not in totals:
                totals[u.model] = {"input": 0, "output": 0}
            totals[u.model]["input"] += u.input_tokens
            totals[u.model]["output"] += u.output_tokens
        return totals

    def to_state(self) -> Dict:
        return {
            "usages": [vars(u) for u in self.usages],
            "totals": self.total_by_model(),
        }
```

#### Pricing Configuration

```yaml
# python/src/the_edge_agent/data/model_pricing.yaml
models:
  # Anthropic
  claude-sonnet-4-20250514:
    input_per_1m: 3.00
    output_per_1m: 15.00
  claude-opus-4-20250514:
    input_per_1m: 15.00
    output_per_1m: 75.00
  claude-3-5-haiku-20241022:
    input_per_1m: 0.80
    output_per_1m: 4.00

  # OpenAI
  gpt-4o:
    input_per_1m: 2.50
    output_per_1m: 10.00
  gpt-4o-mini:
    input_per_1m: 0.15
    output_per_1m: 0.60

  # Google
  gemini-1.5-pro:
    input_per_1m: 1.25
    output_per_1m: 5.00
  gemini-1.5-flash:
    input_per_1m: 0.075
    output_per_1m: 0.30

# Default for unknown models
default:
  input_per_1m: 1.00
  output_per_1m: 3.00
```

#### YAML Integration

```yaml
# Workflow with cost tracking enabled
settings:
  cost_tracking:
    enabled: true
    persist_to_ltm: true
    pricing_override:
      my-custom-model:
        input_per_1m: 2.00
        output_per_1m: 8.00

nodes:
  - name: process_with_llm
    uses: llm.call
    with:
      model: claude-sonnet-4-20250514
      messages:
        - role: user
          content: "Process this: {{ state.input }}"
    output: result

  - name: report_costs
    uses: ralphy.token_summary
    output: cost_report

edges:
  - from: __start__
    to: process_with_llm
  - from: process_with_llm
    to: report_costs
  - from: report_costs
    to: __end__
```

#### Output Format

```python
# state["_cost_summary"] after ralphy.token_summary
{
    "total_cost_usd": 0.0234,
    "total_input_tokens": 5420,
    "total_output_tokens": 1230,
    "breakdown": [
        {
            "model": "claude-sonnet-4-20250514",
            "input_tokens": 5420,
            "output_tokens": 1230,
            "cost_usd": 0.0234
        }
    ],
    "formatted": """
┌─────────────────────────┬──────────┬───────────┬──────────┐
│ Model                   │ Input    │ Output    │ Cost     │
├─────────────────────────┼──────────┼───────────┼──────────┤
│ claude-sonnet-4-20250514│ 5,420    │ 1,230     │ $0.0234  │
├─────────────────────────┼──────────┼───────────┼──────────┤
│ TOTAL                   │ 5,420    │ 1,230     │ $0.0234  │
└─────────────────────────┴──────────┴───────────┴──────────┘
"""
}
```

### Testing

**Test Location:** `python/tests/test_token_tracker.py`

```python
import pytest
from the_edge_agent.tracking.token_tracker import TokenUsage, TokenAggregator
from the_edge_agent.tracking.cost_config import calculate_cost, load_pricing

def test_aggregator_thread_safety():
    """Test that aggregator handles concurrent additions."""
    from concurrent.futures import ThreadPoolExecutor

    agg = TokenAggregator()

    def add_usage(i):
        agg.add(TokenUsage(model="test", input_tokens=100, output_tokens=50))

    with ThreadPoolExecutor(max_workers=10) as ex:
        list(ex.map(add_usage, range(100)))

    assert len(agg.usages) == 100
    totals = agg.total_by_model()
    assert totals["test"]["input"] == 10000
    assert totals["test"]["output"] == 5000

def test_cost_calculation():
    """Test cost calculation accuracy."""
    config = load_pricing()

    cost = calculate_cost(
        input_tokens=1_000_000,
        output_tokens=500_000,
        model="claude-sonnet-4-20250514",
        config=config
    )

    # $3.00 for 1M input + $7.50 for 500K output = $10.50
    assert cost == pytest.approx(10.50, rel=0.01)

def test_unknown_model_uses_default():
    """Test that unknown models use default pricing with warning."""
    config = load_pricing()

    cost = calculate_cost(
        input_tokens=1_000_000,
        output_tokens=1_000_000,
        model="unknown-model-xyz",
        config=config
    )

    # Default: $1.00 input + $3.00 output = $4.00
    assert cost == pytest.approx(4.00, rel=0.01)
```

---

## Story TEA-RALPHY-001.8: Progress Reporting & Notifications

### Story

**As a** workflow operator,
**I want** real-time progress reporting and completion notifications,
**So that** I know when tasks complete without watching the terminal.

### Acceptance Criteria

1. Progress callback hook for node completion
2. Percentage tracking based on completed tasks
3. Desktop notification on workflow completion
4. Support macOS, Linux, Windows notification APIs
5. Optional webhook for external integrations

### Tasks / Subtasks

- [ ] Create `ProgressTracker` class (AC: 1, 2)
  - [ ] Create `python/src/the_edge_agent/tracking/progress_tracker.py`
  - [ ] Implement `ProgressTracker` with `total_nodes`, `completed_nodes`, `current_node`
  - [ ] Calculate percentage: `(completed / total) * 100`
  - [ ] Emit progress events to registered callbacks
  - [ ] Store progress in `state["_progress"]`
- [ ] Integrate with workflow execution (AC: 1)
  - [ ] Modify `python/src/the_edge_agent/stategraph.py`
  - [ ] Call `progress_tracker.on_node_start(node_name)` before node execution
  - [ ] Call `progress_tracker.on_node_complete(node_name)` after node execution
  - [ ] Handle parallel node tracking (count as single unit or individual)
- [ ] Implement `notify.send` action (AC: 3, 4)
  - [ ] Create `python/src/the_edge_agent/actions/notify_actions.py`
  - [ ] Detect platform via `sys.platform`
  - [ ] macOS implementation:
    - [ ] Use `osascript -e 'display notification "message" with title "TEA"'`
    - [ ] Fallback to `terminal-notifier` if available
  - [ ] Linux implementation:
    - [ ] Use `notify-send "TEA" "message"`
    - [ ] Check for `notify-send` availability
  - [ ] Windows implementation:
    - [ ] Use PowerShell `New-BurntToastNotification` or `[Windows.UI.Notifications]`
    - [ ] Fallback to `msg` command
  - [ ] Add `sound` parameter (boolean, default: True)
  - [ ] Add `urgency` parameter (low, normal, critical)
- [ ] Implement `notify.webhook` action (AC: 5)
  - [ ] Create webhook sender in `notify_actions.py`
  - [ ] Support POST with JSON payload
  - [ ] Support custom headers (for auth tokens)
  - [ ] Retry on failure with exponential backoff
  - [ ] Support Slack, Discord, Teams webhook formats
- [ ] Add progress callback registration (AC: 1)
  - [ ] Create `settings.progress.callbacks` configuration
  - [ ] Support multiple callback types: `webhook`, `file`, `state`
  - [ ] File callback: append progress to log file
- [ ] Add unit tests
  - [ ] Create `python/tests/test_notify_actions.py`
  - [ ] Mock subprocess calls for platform notifications
  - [ ] Test webhook with mock server
  - [ ] Test progress percentage calculation

### Dev Notes

#### Source Tree

```
python/src/the_edge_agent/
├── tracking/
│   ├── __init__.py
│   ├── token_tracker.py         # From 001.7
│   ├── cost_config.py           # From 001.7
│   └── progress_tracker.py      # NEW: Progress tracking
├── actions/
│   ├── llm_actions.py
│   └── notify_actions.py        # NEW: Notification actions
└── stategraph.py                # MODIFY: Progress hooks
```

#### Progress Tracker Implementation

```python
# python/src/the_edge_agent/tracking/progress_tracker.py
from dataclasses import dataclass, field
from typing import Callable, List, Optional, Dict, Any
from datetime import datetime
from enum import Enum

class NodeStatus(Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"

@dataclass
class NodeProgress:
    name: str
    status: NodeStatus = NodeStatus.PENDING
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    error: Optional[str] = None

@dataclass
class ProgressTracker:
    total_nodes: int
    nodes: Dict[str, NodeProgress] = field(default_factory=dict)
    callbacks: List[Callable[["ProgressTracker"], None]] = field(default_factory=list)

    @property
    def completed_count(self) -> int:
        return sum(1 for n in self.nodes.values() if n.status == NodeStatus.COMPLETED)

    @property
    def percentage(self) -> float:
        if self.total_nodes == 0:
            return 100.0
        return (self.completed_count / self.total_nodes) * 100

    @property
    def current_node(self) -> Optional[str]:
        for name, node in self.nodes.items():
            if node.status == NodeStatus.RUNNING:
                return name
        return None

    def on_node_start(self, name: str) -> None:
        self.nodes[name] = NodeProgress(
            name=name,
            status=NodeStatus.RUNNING,
            started_at=datetime.utcnow()
        )
        self._notify_callbacks()

    def on_node_complete(self, name: str, error: Optional[str] = None) -> None:
        if name in self.nodes:
            self.nodes[name].status = NodeStatus.FAILED if error else NodeStatus.COMPLETED
            self.nodes[name].completed_at = datetime.utcnow()
            self.nodes[name].error = error
        self._notify_callbacks()

    def _notify_callbacks(self) -> None:
        for callback in self.callbacks:
            try:
                callback(self)
            except Exception:
                pass  # Don't let callback errors break execution

    def to_state(self) -> Dict[str, Any]:
        return {
            "total": self.total_nodes,
            "completed": self.completed_count,
            "percentage": round(self.percentage, 1),
            "current_node": self.current_node,
            "nodes": {
                name: {
                    "status": node.status.value,
                    "started_at": node.started_at.isoformat() if node.started_at else None,
                    "completed_at": node.completed_at.isoformat() if node.completed_at else None,
                }
                for name, node in self.nodes.items()
            }
        }
```

#### Notify Action Implementation

```python
# python/src/the_edge_agent/actions/notify_actions.py
import subprocess
import sys
import json
from typing import Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)

def notify_send(
    message: str,
    title: str = "TEA Workflow",
    sound: bool = True,
    urgency: str = "normal",  # low, normal, critical
    **kwargs
) -> Dict[str, Any]:
    """
    Send desktop notification cross-platform.

    Args:
        message: Notification body text
        title: Notification title
        sound: Play notification sound
        urgency: Notification priority level

    Returns:
        {"success": bool, "platform": str, "error": str|None}
    """
    platform = sys.platform
    result = {"success": False, "platform": platform, "error": None}

    try:
        if platform == "darwin":  # macOS
            # Try osascript first (built-in)
            sound_cmd = 'with sound name "default"' if sound else ""
            script = f'display notification "{message}" with title "{title}" {sound_cmd}'
            subprocess.run(
                ["osascript", "-e", script],
                check=True,
                capture_output=True
            )
            result["success"] = True

        elif platform == "linux":
            # Use notify-send (libnotify)
            urgency_map = {"low": "low", "normal": "normal", "critical": "critical"}
            cmd = ["notify-send", "-u", urgency_map.get(urgency, "normal"), title, message]
            subprocess.run(cmd, check=True, capture_output=True)
            result["success"] = True

        elif platform == "win32":  # Windows
            # Use PowerShell toast notification
            ps_script = f'''
            [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
            $template = [Windows.UI.Notifications.ToastTemplateType]::ToastText02
            $xml = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent($template)
            $text = $xml.GetElementsByTagName("text")
            $text[0].AppendChild($xml.CreateTextNode("{title}")) | Out-Null
            $text[1].AppendChild($xml.CreateTextNode("{message}")) | Out-Null
            $toast = [Windows.UI.Notifications.ToastNotification]::new($xml)
            [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier("TEA").Show($toast)
            '''
            subprocess.run(
                ["powershell", "-Command", ps_script],
                check=True,
                capture_output=True
            )
            result["success"] = True

        else:
            result["error"] = f"Unsupported platform: {platform}"

    except subprocess.CalledProcessError as e:
        result["error"] = f"Command failed: {e.stderr.decode() if e.stderr else str(e)}"
    except FileNotFoundError as e:
        result["error"] = f"Notification tool not found: {e}"
    except Exception as e:
        result["error"] = str(e)

    return result


def notify_webhook(
    url: str,
    payload: Dict[str, Any],
    headers: Optional[Dict[str, str]] = None,
    format: str = "json",  # json, slack, discord
    timeout: int = 30,
    **kwargs
) -> Dict[str, Any]:
    """
    Send webhook notification.

    Args:
        url: Webhook URL
        payload: Data to send
        headers: Optional HTTP headers
        format: Payload format (json, slack, discord)
        timeout: Request timeout in seconds

    Returns:
        {"success": bool, "status_code": int, "error": str|None}
    """
    import requests

    result = {"success": False, "status_code": None, "error": None}

    # Format payload for specific services
    if format == "slack":
        payload = {"text": payload.get("message", str(payload))}
    elif format == "discord":
        payload = {"content": payload.get("message", str(payload))}

    try:
        response = requests.post(
            url,
            json=payload,
            headers=headers or {"Content-Type": "application/json"},
            timeout=timeout
        )
        result["status_code"] = response.status_code
        result["success"] = response.status_code < 400

        if not result["success"]:
            result["error"] = f"HTTP {response.status_code}: {response.text[:200]}"

    except requests.RequestException as e:
        result["error"] = str(e)

    return result


# Action registry
def register_actions(registry: dict, engine=None):
    registry["notify.send"] = notify_send
    registry["notify.webhook"] = notify_webhook
```

#### YAML Integration

```yaml
# Workflow with progress and notifications
settings:
  progress:
    track: true
    callbacks:
      - type: file
        path: "./progress.log"

nodes:
  - name: task_1
    run: |
      import time
      time.sleep(1)
      return {"step": 1}

  - name: task_2
    run: |
      import time
      time.sleep(1)
      return {"step": 2}

  - name: notify_complete
    uses: notify.send
    with:
      title: "Workflow Complete"
      message: "All tasks finished successfully!"
      sound: true

  - name: webhook_notify
    uses: notify.webhook
    with:
      url: "{{ secrets.SLACK_WEBHOOK_URL }}"
      format: slack
      payload:
        message: "TEA workflow completed: {{ state._progress.percentage }}%"

edges:
  - from: __start__
    to: task_1
  - from: task_1
    to: task_2
  - from: task_2
    to: notify_complete
  - from: notify_complete
    to: webhook_notify
  - from: webhook_notify
    to: __end__
```

### Testing

**Test Location:** `python/tests/test_notify_actions.py`

```python
import pytest
from unittest.mock import patch, MagicMock
from the_edge_agent.actions.notify_actions import notify_send, notify_webhook
from the_edge_agent.tracking.progress_tracker import ProgressTracker, NodeStatus

class TestNotifySend:
    @patch("subprocess.run")
    def test_macos_notification(self, mock_run):
        with patch("sys.platform", "darwin"):
            result = notify_send("Test message", title="Test")
            assert result["success"] == True
            assert result["platform"] == "darwin"
            mock_run.assert_called_once()

    @patch("subprocess.run")
    def test_linux_notification(self, mock_run):
        with patch("sys.platform", "linux"):
            result = notify_send("Test message", urgency="critical")
            assert result["success"] == True
            mock_run.assert_called_once()
            # Verify urgency flag
            call_args = mock_run.call_args[0][0]
            assert "-u" in call_args
            assert "critical" in call_args

    def test_unsupported_platform(self):
        with patch("sys.platform", "unknown"):
            result = notify_send("Test")
            assert result["success"] == False
            assert "Unsupported" in result["error"]


class TestNotifyWebhook:
    @patch("requests.post")
    def test_json_webhook(self, mock_post):
        mock_post.return_value = MagicMock(status_code=200)

        result = notify_webhook(
            url="https://example.com/webhook",
            payload={"message": "Test"}
        )

        assert result["success"] == True
        assert result["status_code"] == 200

    @patch("requests.post")
    def test_slack_format(self, mock_post):
        mock_post.return_value = MagicMock(status_code=200)

        notify_webhook(
            url="https://hooks.slack.com/...",
            payload={"message": "Test"},
            format="slack"
        )

        # Verify Slack format
        call_kwargs = mock_post.call_args[1]
        assert call_kwargs["json"] == {"text": "Test"}


class TestProgressTracker:
    def test_percentage_calculation(self):
        tracker = ProgressTracker(total_nodes=4)

        assert tracker.percentage == 0.0

        tracker.on_node_start("node1")
        tracker.on_node_complete("node1")
        assert tracker.percentage == 25.0

        tracker.on_node_start("node2")
        tracker.on_node_complete("node2")
        assert tracker.percentage == 50.0

    def test_callback_invocation(self):
        callback_calls = []
        tracker = ProgressTracker(
            total_nodes=2,
            callbacks=[lambda t: callback_calls.append(t.percentage)]
        )

        tracker.on_node_start("node1")
        tracker.on_node_complete("node1")

        # Callback called on start and complete
        assert len(callback_calls) == 2
```

---

## Story TEA-RALPHY-001.9: Integration Workflow Agent

### Story

**As a** developer,
**I want** a complete Ralphy-compatible workflow agent,
**So that** I can run autonomous coding loops with a single YAML file.

### Acceptance Criteria

1. Single YAML agent that orchestrates full loop
2. Support all task sources (Markdown, YAML, GitHub, BMad)
3. Configurable AI engine selection
4. Parallel or sequential execution mode
5. Automatic test and lint execution
6. Git branch and PR workflow
7. Token tracking and cost reporting
8. Checkpoint/resume support

### Tasks / Subtasks

- [ ] Create main workflow file (AC: 1)
  - [ ] Create `examples/workflows/ralphy-loop.yaml`
  - [ ] Define `state_schema` for all inputs
  - [ ] Define `initial_state` with sensible defaults
  - [ ] Configure `settings.cost_tracking.enabled: true`
  - [ ] Configure `settings.progress.track: true`
- [ ] Implement task source routing node (AC: 2)
  - [ ] Create `detect_source_type` node
  - [ ] Detect source type from input: `markdown`, `yaml`, `github`, `bmad`, `glob`
  - [ ] Route to appropriate parser based on type
  - [ ] Create `parse_markdown_source` node using `markdown.parse`
  - [ ] Create `parse_yaml_source` node for direct YAML task files
  - [ ] Create `parse_github_source` node using `github.list_issues`
  - [ ] Create `parse_bmad_source` node using `bmad.parse_story`
  - [ ] Unify output to `state.tasks[]` array
- [ ] Implement engine selection node (AC: 3)
  - [ ] Create `configure_engine` node
  - [ ] Support engines: `claude`, `codex`, `gemini`, `opencode`, `cursor`
  - [ ] Validate engine is available (check CLI exists)
  - [ ] Store in `state.engine_config`
- [ ] Implement execution mode selection (AC: 4)
  - [ ] Create `select_execution_mode` node
  - [ ] Support modes: `sequential`, `parallel`
  - [ ] For parallel: create `parallel_for` over `state.tasks`
  - [ ] For sequential: create linear edge chain
  - [ ] Respect `max_concurrency` setting (default: 4)
- [ ] Implement single task execution node (AC: 3, 5, 6)
  - [ ] Create `execute_task` node
  - [ ] Create git branch: `git checkout -b task/{task_id}`
  - [ ] Call AI engine via `llm.call` with shell provider
  - [ ] Run tests: detect test command from project (`npm test`, `pytest`, `cargo test`)
  - [ ] Run linter: detect linter from project
  - [ ] Commit changes: `git add -A && git commit -m "..."`
  - [ ] Handle test failures: retry with AI or mark failed
- [ ] Implement test detection (AC: 5)
  - [ ] Create `detect_test_command` node
  - [ ] Check for `package.json` → `npm test`
  - [ ] Check for `pyproject.toml` / `setup.py` → `pytest`
  - [ ] Check for `Cargo.toml` → `cargo test`
  - [ ] Check for `Makefile` → `make test`
  - [ ] Store in `state.test_command`
- [ ] Implement PR creation flow (AC: 6)
  - [ ] Create `create_pull_request` node
  - [ ] Use `gh pr create` via subprocess
  - [ ] Generate PR title from task description
  - [ ] Generate PR body with task details and test results
  - [ ] Support draft PRs via `--draft` flag
  - [ ] Store PR URL in `state.pr_url`
- [ ] Implement cost reporting (AC: 7)
  - [ ] Create `report_costs` node using `ralphy.token_summary`
  - [ ] Output formatted cost table
  - [ ] Send notification if total cost exceeds threshold
- [ ] Implement checkpoint configuration (AC: 8)
  - [ ] Add `interrupt_before` for key decision points
  - [ ] Configure checkpoint directory
  - [ ] Document resume command in output
- [ ] Add documentation
  - [ ] Create `examples/workflows/README-ralphy.md`
  - [ ] Document all input parameters
  - [ ] Document output state
  - [ ] Add troubleshooting section
- [ ] Add unit and integration tests
  - [ ] Create `python/tests/test_ralphy_workflow.py`
  - [ ] Test source detection
  - [ ] Test execution mode selection
  - [ ] Test with mock AI responses

### Dev Notes

#### Source Tree

```
examples/workflows/
├── ralphy-loop.yaml              # NEW: Main integration workflow
├── ralphy-task-executor.yaml     # NEW: Single task execution sub-workflow
├── ralphy-pr-creator.yaml        # NEW: PR creation sub-workflow
└── README-ralphy.md              # NEW: Documentation

python/tests/
└── test_ralphy_workflow.py       # NEW: Integration tests
```

#### State Schema

```yaml
state_schema:
  # Input parameters
  source: str                    # Task source (file path, glob, github:owner/repo)
  engine: str                    # AI engine name (claude, codex, gemini)
  mode: str                      # Execution mode (sequential, parallel)
  max_concurrency: int           # Max parallel tasks (default: 4)
  base_branch: str               # Git base branch (default: main)
  create_prs: bool               # Create PRs for each task (default: true)
  draft_prs: bool                # Create as draft PRs (default: false)
  run_tests: bool                # Run tests after each task (default: true)
  test_command: str              # Override test command
  lint_command: str              # Override lint command

  # Internal state
  source_type: str               # Detected source type
  tasks: list                    # Parsed tasks
  engine_config: dict            # Engine configuration
  test_results: list             # Test results per task
  completed_tasks: list          # Successfully completed tasks
  failed_tasks: list             # Failed tasks
  pr_urls: list                  # Created PR URLs

  # Tracking
  _token_usage: dict             # From 001.7
  _cost_summary: dict            # From 001.7
  _progress: dict                # From 001.8
```

#### Main Workflow Structure

```yaml
# examples/workflows/ralphy-loop.yaml
name: ralphy-loop
description: Autonomous AI Coding Loop - Ralphy-compatible workflow

initial_state:
  engine: claude
  mode: sequential
  max_concurrency: 4
  base_branch: main
  create_prs: true
  draft_prs: false
  run_tests: true

settings:
  cost_tracking:
    enabled: true
  progress:
    track: true

nodes:
  # ============================================
  # Phase 1: Source Detection & Parsing
  # ============================================
  - name: detect_source_type
    description: Detect the type of task source
    run: |
      source = state.get("source", "")

      if source.startswith("github:"):
          source_type = "github"
      elif source.endswith(".yaml") or source.endswith(".yml"):
          source_type = "yaml"
      elif source.endswith(".md"):
          source_type = "markdown"
      elif "*" in source:
          source_type = "glob"
      else:
          source_type = "bmad"  # Default to BMad story

      return {"source_type": source_type}

  - name: parse_source
    description: Parse tasks from source
    run: |
      source_type = state["source_type"]
      source = state["source"]
      tasks = []

      if source_type == "glob":
          import glob
          files = glob.glob(source)
          for f in files:
              # Each file becomes a task
              tasks.append({
                  "id": os.path.basename(f).replace(".md", ""),
                  "source_file": f,
                  "type": "bmad"
              })
      elif source_type == "github":
          # Will be handled by github.list_issues action
          pass
      elif source_type == "markdown":
          # Will be handled by markdown.parse action
          pass

      return {"tasks": tasks, "total_tasks": len(tasks)}

  # ============================================
  # Phase 2: Engine Configuration
  # ============================================
  - name: configure_engine
    description: Configure the AI engine
    run: |
      import shutil
      engine = state.get("engine", "claude")

      # Verify engine CLI exists
      engine_cmd = {
          "claude": "claude",
          "codex": "codex",
          "gemini": "gemini",
          "opencode": "opencode",
          "cursor": "cursor"
      }.get(engine, engine)

      if not shutil.which(engine_cmd):
          raise RuntimeError(f"Engine CLI not found: {engine_cmd}")

      return {
          "engine_config": {
              "name": engine,
              "command": engine_cmd,
              "available": True
          }
      }

  # ============================================
  # Phase 3: Test Command Detection
  # ============================================
  - name: detect_test_command
    description: Auto-detect test command for the project
    run: |
      import os

      test_cmd = state.get("test_command")
      lint_cmd = state.get("lint_command")

      if not test_cmd:
          if os.path.exists("package.json"):
              test_cmd = "npm test"
          elif os.path.exists("pyproject.toml") or os.path.exists("setup.py"):
              test_cmd = "pytest"
          elif os.path.exists("Cargo.toml"):
              test_cmd = "cargo test"
          elif os.path.exists("Makefile"):
              test_cmd = "make test"
          else:
              test_cmd = "echo 'No test command detected'"

      if not lint_cmd:
          if os.path.exists("package.json"):
              lint_cmd = "npm run lint"
          elif os.path.exists("pyproject.toml"):
              lint_cmd = "ruff check ."
          elif os.path.exists("Cargo.toml"):
              lint_cmd = "cargo clippy"
          else:
              lint_cmd = "echo 'No lint command detected'"

      return {"test_command": test_cmd, "lint_command": lint_cmd}

  # ============================================
  # Phase 4: Task Execution
  # ============================================
  - name: execute_tasks_sequential
    description: Execute tasks sequentially
    condition: "{{ state.mode == 'sequential' }}"
    run: |
      # Sequential execution handled by loop
      return {"execution_mode": "sequential"}

  - name: execute_tasks_parallel
    description: Execute tasks in parallel
    condition: "{{ state.mode == 'parallel' }}"
    parallel_for: "{{ state.tasks[:state.max_concurrency] }}"
    uses: workflow.invoke
    with:
      workflow: examples/workflows/ralphy-task-executor.yaml
      input:
        task: "{{ item }}"
        engine_config: "{{ state.engine_config }}"
        test_command: "{{ state.test_command }}"
        base_branch: "{{ state.base_branch }}"
    output: task_results

  # ============================================
  # Phase 5: Results Collection
  # ============================================
  - name: collect_results
    description: Collect results from all tasks
    run: |
      results = state.get("task_results", [])
      completed = [r for r in results if r.get("success")]
      failed = [r for r in results if not r.get("success")]

      return {
          "completed_tasks": completed,
          "failed_tasks": failed,
          "success_rate": len(completed) / max(len(results), 1) * 100
      }

  # ============================================
  # Phase 6: Cost Reporting
  # ============================================
  - name: report_costs
    description: Generate cost summary
    uses: ralphy.token_summary
    output: cost_report

  - name: notify_completion
    description: Send completion notification
    uses: notify.send
    with:
      title: "Ralphy Loop Complete"
      message: |
        Tasks: {{ state.completed_tasks | length }}/{{ state.total_tasks }} completed
        Cost: ${{ state._cost_summary.total_cost_usd | round(4) }}

edges:
  - from: __start__
    to: detect_source_type
  - from: detect_source_type
    to: parse_source
  - from: parse_source
    to: configure_engine
  - from: configure_engine
    to: detect_test_command
  - from: detect_test_command
    to: execute_tasks_sequential
    condition: "{{ state.mode == 'sequential' }}"
  - from: detect_test_command
    to: execute_tasks_parallel
    condition: "{{ state.mode == 'parallel' }}"
  - from: execute_tasks_sequential
    to: collect_results
  - from: execute_tasks_parallel
    to: collect_results
  - from: collect_results
    to: report_costs
  - from: report_costs
    to: notify_completion
  - from: notify_completion
    to: __end__
```

#### Task Executor Sub-Workflow

```yaml
# examples/workflows/ralphy-task-executor.yaml
name: ralphy-task-executor
description: Execute a single task with AI engine

state_schema:
  task: dict
  engine_config: dict
  test_command: str
  base_branch: str

nodes:
  - name: create_branch
    run: |
      import subprocess
      task_id = state["task"]["id"]
      branch = f"task/{task_id}"

      subprocess.run(["git", "checkout", state["base_branch"]], check=True)
      subprocess.run(["git", "checkout", "-b", branch], check=True)

      return {"branch": branch}

  - name: execute_with_ai
    uses: llm.call
    with:
      provider: shell
      shell_provider: "{{ state.engine_config.name }}"
      timeout: 3600
      messages:
        - role: user
          content: |
            Implement the following task:

            Task ID: {{ state.task.id }}
            Source: {{ state.task.source_file }}

            Instructions:
            1. Read the source file for requirements
            2. Implement all acceptance criteria
            3. Write tests for your implementation
            4. Do not ask for confirmation, just implement

            When done, output: TASK_COMPLETED
    output: ai_output

  - name: run_tests
    run: |
      import subprocess

      result = subprocess.run(
          state["test_command"],
          shell=True,
          capture_output=True,
          text=True
      )

      return {
          "test_passed": result.returncode == 0,
          "test_output": result.stdout + result.stderr
      }

  - name: commit_changes
    run: |
      import subprocess

      task_id = state["task"]["id"]
      subprocess.run(["git", "add", "-A"], check=True)
      subprocess.run([
          "git", "commit", "-m",
          f"feat: implement {task_id}\n\nAutonomous implementation by Ralphy"
      ], check=True)

      return {"committed": True}

  - name: create_pr
    run: |
      import subprocess

      task_id = state["task"]["id"]
      result = subprocess.run([
          "gh", "pr", "create",
          "--title", f"feat: {task_id}",
          "--body", f"Autonomous implementation of {task_id}\n\nTest results: {'✅ Passed' if state['test_passed'] else '❌ Failed'}",
          "--draft" if state.get("draft_prs") else ""
      ], capture_output=True, text=True)

      pr_url = result.stdout.strip()
      return {"pr_url": pr_url, "success": True}

edges:
  - from: __start__
    to: create_branch
  - from: create_branch
    to: execute_with_ai
  - from: execute_with_ai
    to: run_tests
  - from: run_tests
    to: commit_changes
  - from: commit_changes
    to: create_pr
  - from: create_pr
    to: __end__
```

### Target Usage

```bash
# Run from Markdown PRD
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/prd.md", "engine": "claude", "parallel": 4}'

# Run from GitHub Issues
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "github:owner/repo", "labels": ["ready"], "engine": "claude"}'

# Run from BMad stories (using input file)
echo '{"source": "./docs/stories/TEA-*.md", "engine": "claude"}' > vars.json
tea run examples/workflows/ralphy-loop.yaml --input @vars.json

# Sequential mode (default)
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/stories/TEA-001.md", "mode": "sequential"}'

# With visual progress
tea run examples/workflows/ralphy-loop.yaml \
  --input '{"source": "./docs/stories/*.md"}' --show-graph
```

### Testing

**Test Location:** `python/tests/test_ralphy_workflow.py`

```python
import pytest
import tempfile
import os
from the_edge_agent import YAMLEngine

class TestRalphyWorkflow:
    @pytest.fixture
    def engine(self):
        return YAMLEngine()

    def test_source_type_detection(self, engine):
        """Test that source types are correctly detected."""
        graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
        compiled = graph.compile()

        # Test GitHub source
        result = list(compiled.invoke({"source": "github:owner/repo"}))
        final_state = result[-1]["state"]
        assert final_state["source_type"] == "github"

        # Test glob source
        result = list(compiled.invoke({"source": "docs/*.md"}))
        final_state = result[-1]["state"]
        assert final_state["source_type"] == "glob"

    def test_test_command_detection(self, engine):
        """Test auto-detection of test commands."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create package.json
            with open(os.path.join(tmpdir, "package.json"), "w") as f:
                f.write("{}")

            os.chdir(tmpdir)
            graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
            # ... test that npm test is detected

    def test_engine_validation(self, engine):
        """Test that invalid engines raise errors."""
        graph = engine.load_from_file("examples/workflows/ralphy-loop.yaml")
        compiled = graph.compile()

        with pytest.raises(RuntimeError, match="Engine CLI not found"):
            list(compiled.invoke({
                "source": "test.md",
                "engine": "nonexistent-engine"
            }))
```

---

## Implementation Order

```
Phase 0: Shared Library (P0) - External Repo
└── TEA-RALPHY-001.0 (md-graph-parser crate)
    │
    ▼
Phase 1: Core Parsing (P0)
├── TEA-RALPHY-001.1 (Python markdown.parse) ─┐
└── TEA-RALPHY-001.2 (Rust markdown.parse) ───┴─ parallel, both depend on 001.0
    │
    ▼
Phase 2: Task Sources (P1)
├── TEA-RALPHY-001.3 (GitHub Issues)
├── TEA-RALPHY-001.4 (BMad Stories) ← depends on 001.1, 001.2
├── TEA-RALPHY-001.5 (Shell Providers)
└── TEA-RALPHY-001.6 (Execution Modes)
    │ can parallelize ↑
    ▼
Phase 3: Observability (P2)
└── TEA-RALPHY-001.7 (Token Tracking)
    │
    ▼
Phase 4: Integration (P0)
├── TEA-RALPHY-001.9 (Full Workflow Agent)
└── TEA-RALPHY-001.10 (Meta-Orchestrator: LLM → DOT → Parallel Execution)
```

## Architecture: Meta-Orchestration Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     TEA-RALPHY Meta-Orchestration                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  INPUT                    ANALYSIS                    GENERATION            │
│  ┌─────────────────┐     ┌─────────────────┐        ┌─────────────────┐    │
│  │ glob_pattern:   │     │ LLM analyzes    │        │ DOT file with   │    │
│  │ docs/stories/   │────▶│ dependencies    │───────▶│ parallel phases │    │
│  │ TEA-RALPHY-*.md │     │ between stories │        │ & commands      │    │
│  └─────────────────┘     └─────────────────┘        └─────────────────┘    │
│                                                              │              │
│  CONVERSION                              EXECUTION           ▼              │
│  ┌─────────────────┐                    ┌─────────────────────────────────┐│
│  │ tea from dot    │                    │  Parallel execution via         ││
│  │ --use-node-cmds │───────────────────▶│  dynamic_parallel + fan_in      ││
│  └─────────────────┘                    │  with shell providers (claude)  ││
│                                         └─────────────────────────────────┘│
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘

Usage:
  tea run ralphy-meta-orchestrator.yaml --input '{"glob_pattern": "docs/stories/TEA-RALPHY-*.md"}'
```

---

## Success Metrics

| Metric | Target |
|--------|--------|
| Parse 100 BMad stories | < 5 seconds |
| Parallel execution (4 tasks) | Linear speedup |
| Checkpoint resume | < 1 second overhead |
| Cross-runtime parity | 100% schema match |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Initial epic creation | Sarah (PO) |
| 2025-01-17 | 0.2 | Added detailed tasks for 001.7, 001.8, 001.9 | Sarah (PO) |
| 2025-01-17 | 0.3 | Added story 001.10 (Meta-Orchestrator) | Sarah (PO) |
| 2025-01-17 | 0.4 | Completed research for 001.5 (OpenCode/Cursor CLI) | Sarah (PO) |
| 2025-01-18 | 0.5 | Added story 001.0 (md-graph-parser shared crate), updated dependencies for 001.1/001.2 | Sarah (PO) |
| 2025-01-18 | 0.6 | Moved 001.8 (Notifications) to Backlog | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
