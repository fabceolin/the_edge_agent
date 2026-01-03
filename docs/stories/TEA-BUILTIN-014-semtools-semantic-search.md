# Story TEA-BUILTIN-014: SemTools Semantic Search Action

## Status

**Draft**

## Story

**As a** YAML agent developer,
**I want** a `semtools.search` action that performs local semantic search on text files using CLI-based embeddings,
**so that** I can find semantically similar content across documents without requiring API calls or vector database setup.

## Story Context

**Existing System Integration:**
- Integrates with: `file.read` action for file content, conditional edges for routing based on search results
- Technology: [SemTools CLI](https://github.com/run-llama/semtools) - Rust-based semantic search toolkit
- Pattern: Shell provider pattern (similar to `llm.call` with `provider: shell`)
- Touch points: `yaml_engine.py`, `actions/__init__.py`, shell command execution

**Key Design Decision:** Wrap the SemTools CLI `search` command as a YAML action, enabling semantic grep-like functionality:

```yaml
# Semantic search for QA gate concerns in story file
- name: check_concerns
  uses: semtools.search
  with:
    query: "quality gate concerns issues problems"
    files: "docs/stories/{{ state.story_id }}.md"
    max_distance: 0.3
    n_results: 5
  output: search_results
```

## Background: SemTools CLI

SemTools is a high-performance CLI toolkit for semantic search built in Rust:

- **Installation**: `npm i -g @llamaindex/semtools` or `cargo install semtools`
- **Embeddings**: Uses model2vec (minishlab/potion-multilingual-128M) - runs locally, no API needed
- **Search Command**: `search "query" files... --max-distance 0.3 --n-lines 5`
- **Output**: JSON with matches, similarity scores, and context lines

**CLI Usage Examples:**
```bash
# Basic semantic search
semtools search "API authentication" docs/*.md

# With distance threshold (0.0 = exact, 1.0 = unrelated)
semtools search "error handling" src/*.py --max-distance 0.3

# With context lines
semtools search "database connection" *.py --n-lines 3
```

## Acceptance Criteria

### Core Semantic Search Action

1. **AC-1: semtools.search action**: Executes SemTools `search` command and returns structured results
2. **AC-2: Query Parameter**: Accepts `query` string for semantic search
3. **AC-3: Files Parameter**: Accepts `files` as string (glob) or list of file paths
4. **AC-4: Max Distance**: Supports `max_distance` (0.0-1.0) to filter by similarity threshold
5. **AC-5: N Results**: Supports `n_results` to limit number of matches returned
6. **AC-6: N Lines Context**: Supports `n_lines` to include surrounding context lines

### Output Format

7. **AC-7: Structured Output**: Returns `{success, matches: [{file, line, score, text, context}], query, total_matches}`
8. **AC-8: Similarity Score**: Each match includes `score` (cosine similarity, higher = more similar)
9. **AC-9: Best Match Helper**: Returns `best_match` field with highest-scoring result
10. **AC-10: Contains Match Helper**: Returns `has_matches` boolean for easy conditional routing

### Installation & Prerequisites

11. **AC-11: Prerequisite Check**: Validates `semtools` CLI is installed before execution
12. **AC-12: Helpful Error**: If not installed, returns error with installation instructions
13. **AC-13: Version Check**: Optionally validates minimum version (v1.5.1+)

### Error Handling

14. **AC-14: File Not Found**: Gracefully handles missing files with clear error message
15. **AC-15: Empty Results**: Returns empty matches array (not error) when no semantic matches found
16. **AC-16: CLI Error Passthrough**: Captures and returns stderr from semtools CLI

### Integration

17. **AC-17: Dual Namespace**: Actions accessible as `semtools.search` and `actions.semtools_search`
18. **AC-18: Shell Execution**: Uses subprocess for CLI execution (similar to shell provider pattern)
19. **AC-19: Documentation**: Updated YAML_REFERENCE.md with semtools action examples

## Dependencies

**External Dependency:**
- SemTools CLI v1.5.1+ (https://github.com/run-llama/semtools)
- Install: `npm i -g @llamaindex/semtools` or `cargo install semtools`

**Blocked By:**
- None (uses subprocess for CLI execution)

**Reuses From:**
- Shell command execution pattern from `llm_actions.py` (shell provider)
- Action registration pattern from existing actions

**Enables:**
- Semantic similarity routing in YAML workflows
- Content-aware conditional edges (e.g., route based on semantic match to "concerns")
- Document similarity analysis without API costs

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    SEMTOOLS SEARCH ARCHITECTURE                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  YAML Agent                         SemTools Integration                     │
│  ──────────                         ────────────────────                     │
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  nodes:                                                              │   │
│  │    - name: semantic_search                                           │   │
│  │      uses: semtools.search                                           │   │
│  │      with:                                                           │   │
│  │        query: "Gate: CONCERNS"                                       │   │
│  │        files: "docs/stories/{{ state.story_id }}.md"                │   │
│  │        max_distance: 0.3                                             │   │
│  │        n_results: 5                                                  │   │
│  │      output: search_results                                          │   │
│  │                                                                      │   │
│  │    - name: route_on_concerns                                         │   │
│  │      # Conditional edge based on semantic match                      │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                            │                                                 │
│                            ▼                                                 │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                     semtools_actions.py                                │ │
│  │                                                                        │ │
│  │   def semtools_search(state, query, files, max_distance, ...):        │ │
│  │       # 1. Validate semtools CLI installed                            │ │
│  │       # 2. Build command: semtools search "query" files...            │ │
│  │       # 3. Execute via subprocess                                     │ │
│  │       # 4. Parse JSON output                                          │ │
│  │       # 5. Return structured results                                  │ │
│  │                                                                        │ │
│  │   subprocess.run([                                                     │ │
│  │       "semtools", "search", query,                                    │ │
│  │       *files,                                                          │ │
│  │       "--max-distance", "0.3",                                        │ │
│  │       "--output", "json"                                              │ │
│  │   ])                                                                   │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                            │                                                 │
│                            ▼                                                 │
│  ┌───────────────────────────────────────────────────────────────────────┐ │
│  │                     SemTools CLI (External)                            │ │
│  │                                                                        │ │
│  │   - Rust binary with model2vec embeddings                             │ │
│  │   - Local execution (no API calls)                                    │ │
│  │   - Returns JSON: [{file, line, score, text}, ...]                   │ │
│  └───────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Use Case: QA Gate Semantic Detection

```yaml
# Replace regex-based gate detection with semantic search
- name: check_qa_gate
  uses: semtools.search
  with:
    query: "quality gate concerns issues problems blockers"
    files: "docs/stories/{{ state.arg }}.md"
    max_distance: 0.25
    n_results: 1
  output: gate_search

- name: determine_gate_status
  run: |
    search_results = state.get("gate_search", {})

    if search_results.get("has_matches"):
      best = search_results.get("best_match", {})
      text = best.get("text", "").upper()

      # Semantic search found relevant content - extract status
      if "CONCERNS" in text:
        return {"qa_gate_status": "CONCERNS"}
      elif "FAIL" in text:
        return {"qa_gate_status": "FAIL"}
      elif "PASS" in text:
        return {"qa_gate_status": "PASS"}

    return {"qa_gate_status": "UNKNOWN"}

edges:
  - from: check_qa_gate
    to: determine_gate_status
  - from: determine_gate_status
    to: run_dev_review_qa
    when: "state.get('qa_gate_status') == 'CONCERNS'"
```

## Tasks / Subtasks

- [ ] **Task 1**: Create `semtools_actions.py` module (AC: 1, 17)
  - [ ] Create new file in `actions/` directory
  - [ ] Implement base action function signature
  - [ ] Register with dual namespace

- [ ] **Task 2**: Implement prerequisite checking (AC: 11, 12, 13)
  - [ ] Check if `semtools` command exists in PATH
  - [ ] Return helpful error with installation instructions if missing
  - [ ] Optional: Check version with `semtools --version`

- [ ] **Task 3**: Implement core search execution (AC: 2, 3, 4, 5, 6)
  - [ ] Build command array from parameters
  - [ ] Handle `files` as string (glob expansion) or list
  - [ ] Add `--max-distance`, `--output json` flags
  - [ ] Execute via `subprocess.run()`

- [ ] **Task 4**: Implement output parsing (AC: 7, 8, 9, 10)
  - [ ] Parse JSON output from semtools
  - [ ] Structure matches as `{file, line, score, text, context}`
  - [ ] Calculate `best_match` (highest score)
  - [ ] Add `has_matches` boolean helper

- [ ] **Task 5**: Implement error handling (AC: 14, 15, 16)
  - [ ] Handle FileNotFoundError gracefully
  - [ ] Return empty matches (not error) for no results
  - [ ] Capture and include stderr in error responses

- [ ] **Task 6**: Documentation (AC: 19)
  - [ ] Add `semtools.search` to YAML_REFERENCE.md
  - [ ] Include installation instructions
  - [ ] Add usage examples for semantic routing

- [ ] **Task 7**: Testing
  - [ ] Unit tests with mocked subprocess
  - [ ] Integration tests (requires semtools installed)
  - [ ] Test error handling paths
  - [ ] Test output parsing

## Dev Notes

### CLI Command Reference

```bash
# Basic search
semtools search "query text" file1.md file2.md

# With options
semtools search "query" *.md \
  --max-distance 0.3 \     # 0.0 = exact, 1.0 = unrelated
  --n-lines 5 \            # Context lines around match
  --output json            # JSON output format

# Expected JSON output format (verify with actual CLI)
[
  {
    "file": "docs/story.md",
    "line": 42,
    "score": 0.87,
    "text": "Gate: CONCERNS - Several issues found",
    "context": ["Previous line", "Gate: CONCERNS...", "Next line"]
  }
]
```

### Implementation Reference

```python
# semtools_actions.py

import subprocess
import shutil
import json
from typing import Any, Dict, List, Optional, Union

def semtools_search(
    state: Dict[str, Any],
    query: str,
    files: Union[str, List[str]],
    max_distance: float = 0.5,
    n_results: int = 10,
    n_lines: int = 0,
    **kwargs
) -> Dict[str, Any]:
    """
    Semantic search using SemTools CLI.

    Args:
        state: Current workflow state
        query: Semantic search query
        files: File path(s) or glob pattern
        max_distance: Maximum cosine distance (0.0-1.0, lower = more similar)
        n_results: Maximum number of results
        n_lines: Context lines around each match

    Returns:
        {success, matches, best_match, has_matches, query, error?}
    """
    # Check if semtools is installed
    if not shutil.which("semtools"):
        return {
            "success": False,
            "error": "semtools CLI not found",
            "error_type": "prerequisite_missing",
            "install_hint": "Install with: npm i -g @llamaindex/semtools"
        }

    # Build command
    cmd = ["semtools", "search", query]

    # Handle files parameter
    if isinstance(files, str):
        import glob as glob_module
        file_list = glob_module.glob(files) or [files]
    else:
        file_list = files

    cmd.extend(file_list)
    cmd.extend(["--max-distance", str(max_distance)])
    cmd.extend(["--output", "json"])

    if n_lines > 0:
        cmd.extend(["--n-lines", str(n_lines)])

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=60
        )

        if result.returncode != 0:
            return {
                "success": False,
                "error": result.stderr or "semtools search failed",
                "error_type": "cli_error",
                "matches": [],
                "has_matches": False
            }

        # Parse JSON output
        matches = json.loads(result.stdout) if result.stdout.strip() else []

        # Limit results
        matches = matches[:n_results]

        # Find best match
        best_match = max(matches, key=lambda m: m.get("score", 0)) if matches else None

        return {
            "success": True,
            "matches": matches,
            "best_match": best_match,
            "has_matches": len(matches) > 0,
            "total_matches": len(matches),
            "query": query
        }

    except subprocess.TimeoutExpired:
        return {
            "success": False,
            "error": "semtools search timed out",
            "error_type": "timeout",
            "matches": [],
            "has_matches": False
        }
    except json.JSONDecodeError as e:
        return {
            "success": False,
            "error": f"Failed to parse semtools output: {e}",
            "error_type": "parse_error",
            "matches": [],
            "has_matches": False
        }
```

### Source Tree Reference

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py              # Register semtools actions
│   ├── semtools_actions.py      # NEW: semtools.search implementation
│   └── llm_actions.py           # Reference: shell provider pattern
└── yaml_engine.py               # Action registration
```

## Testing

### Test File Location
`python/tests/test_semtools_actions.py`

### Testing Standards
- Mock `subprocess.run` for unit tests
- Integration tests marked with `@pytest.mark.integration`
- Skip integration tests if semtools not installed

### Key Test Scenarios

| ID | Priority | Scenario | AC |
|----|----------|----------|-----|
| T1 | P0 | Basic search returns matches | AC-1, AC-2 |
| T2 | P0 | Files parameter accepts glob | AC-3 |
| T3 | P0 | Files parameter accepts list | AC-3 |
| T4 | P0 | Max distance filters results | AC-4 |
| T5 | P0 | N results limits output | AC-5 |
| T6 | P1 | N lines includes context | AC-6 |
| T7 | P0 | Output includes score | AC-8 |
| T8 | P0 | Best match helper works | AC-9 |
| T9 | P0 | Has matches boolean correct | AC-10 |
| T10 | P0 | Missing CLI returns helpful error | AC-11, AC-12 |
| T11 | P1 | File not found handled | AC-14 |
| T12 | P0 | Empty results not an error | AC-15 |
| T13 | P1 | Dual namespace registration | AC-17 |

## Risk and Compatibility

### Minimal Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| SemTools not installed | Medium | High | Clear error message with install instructions |
| CLI output format changes | High | Low | Pin to v1.5.1+, version check |
| Subprocess timeout | Low | Low | 60s default timeout, configurable |
| Large file performance | Medium | Medium | Leverage semtools' Rust performance |

### Compatibility

- No breaking changes to existing actions
- SemTools is optional dependency (graceful degradation)
- Works on Linux, macOS, Windows (anywhere Rust runs)

## Example Usage

### Basic Semantic Search

```yaml
name: semantic_grep_agent

nodes:
  - name: find_related_code
    uses: semtools.search
    with:
      query: "error handling exception catch"
      files: "src/**/*.py"
      max_distance: 0.3
      n_results: 10
    output: related_code
```

### Semantic Routing for QA Gate

```yaml
name: smart_qa_routing

nodes:
  - name: semantic_gate_check
    uses: semtools.search
    with:
      query: "quality concerns issues blockers problems"
      files: "docs/stories/{{ state.story_id }}.md"
      max_distance: 0.25
      n_results: 3

  - name: extract_gate_status
    run: |
      results = state.get("semantic_gate_check", {})
      if results.get("has_matches"):
        # Found semantically similar content to "concerns"
        return {"qa_gate_status": "CONCERNS"}
      return {"qa_gate_status": "PASS"}

edges:
  - from: semantic_gate_check
    to: extract_gate_status
  - from: extract_gate_status
    to: fix_concerns
    when: "state.get('qa_gate_status') == 'CONCERNS'"
  - from: extract_gate_status
    to: complete
    when: "state.get('qa_gate_status') != 'CONCERNS'"
```

### Document Similarity Analysis

```yaml
name: find_similar_docs

nodes:
  - name: search_similar
    uses: semtools.search
    with:
      query: "{{ state.reference_text }}"
      files: "docs/**/*.md"
      max_distance: 0.2
      n_results: 5
      n_lines: 3
    output: similar_docs

  - name: report_findings
    run: |
      results = state.get("similar_docs", {})
      matches = results.get("matches", [])

      report = f"Found {len(matches)} similar documents:\n"
      for m in matches:
        report += f"- {m['file']}:{m['line']} (score: {m['score']:.2f})\n"
        report += f"  {m['text']}\n"

      return {"similarity_report": report}
```

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Unit tests pass with mocked subprocess
- [ ] Integration tests pass (with semtools installed)
- [ ] Documentation updated
- [ ] Code follows existing TEA patterns

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1.0 | Initial story creation | Claude Opus 4.5 |

---

## Dev Agent Record

### Agent Model Used
(To be filled during implementation)

### Completion Notes
(To be filled during implementation)

### File List

**New Files:**
- `python/src/the_edge_agent/actions/semtools_actions.py` - SemTools action implementation
- `python/tests/test_semtools_actions.py` - Unit and integration tests

**Modified Files:**
- `python/src/the_edge_agent/actions/__init__.py` - Register semtools actions
- `docs/shared/YAML_REFERENCE.md` - Add semtools.search documentation

---

## QA Results

(To be filled during QA review)
