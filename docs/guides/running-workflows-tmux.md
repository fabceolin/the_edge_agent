# Running Workflows in Tmux

This guide explains how to run TEA workflows in tmux terminals for parallel execution and session management.

## Prerequisites

- tmux installed (`sudo apt install tmux` or `brew install tmux`)
- TEA Python environment activated
- Workflow YAML files in `examples/workflows/`

## Quick Start

### Single Workflow

```bash
# Activate environment and run
source python/.venv/bin/activate
tea run examples/workflows/bmad-story-validation.yaml --input '{"arg": "STORY-ID"}'
```

### Run in Background with Tmux

```bash
# Create detached tmux session with workflow
tmux new-session -d -s my-workflow \
  "source python/.venv/bin/activate && \
   tea run examples/workflows/bmad-story-validation.yaml --input '{\"arg\": \"STORY-ID\"}'; \
   read -p 'Press Enter to close...'"

# Attach to see output
tmux attach -t my-workflow
```

## Running Multiple Workflows in Parallel

### Method 1: Multiple Windows in One Session

```bash
# Create session with first workflow
tmux new-session -d -s validation -n "story-1" \
  "source python/.venv/bin/activate && \
   tea run examples/workflows/bmad-story-validation.yaml --input '{\"arg\": \"STORY-001\"}'; \
   read -p 'Press Enter...'"

# Add more windows
tmux new-window -t validation -n "story-2" \
  "source python/.venv/bin/activate && \
   tea run examples/workflows/bmad-story-validation.yaml --input '{\"arg\": \"STORY-002\"}'; \
   read -p 'Press Enter...'"

tmux new-window -t validation -n "story-3" \
  "source python/.venv/bin/activate && \
   tea run examples/workflows/bmad-story-validation.yaml --input '{\"arg\": \"STORY-003\"}'; \
   read -p 'Press Enter...'"

# Attach to session
tmux attach -t validation
```

### Method 2: Batch Script

Create a script `scripts/run-validations.sh`:

```bash
#!/bin/bash
# Run multiple story validations in tmux

SESSION_NAME="${1:-validation}"
WORKFLOW="examples/workflows/bmad-story-validation.yaml"
VENV_ACTIVATE="source python/.venv/bin/activate"

# Story IDs to validate
STORIES=(
    "TEA-AGENT-001.1"
    "TEA-AGENT-001.2"
    "TEA-AGENT-001.3"
    "TEA-AGENT-001.4"
    "TEA-AGENT-001.5"
)

# Create session with first story
tmux new-session -d -s "$SESSION_NAME" -n "${STORIES[0]}" \
    "$VENV_ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"${STORIES[0]}\"}'; read -p 'Done. Press Enter...'"

# Add windows for remaining stories
for story in "${STORIES[@]:1}"; do
    tmux new-window -t "$SESSION_NAME" -n "$story" \
        "$VENV_ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"$story\"}'; read -p 'Done. Press Enter...'"
done

echo "Created tmux session: $SESSION_NAME"
echo "Windows: ${STORIES[*]}"
echo ""
echo "Attach with: tmux attach -t $SESSION_NAME"
```

Make executable and run:
```bash
chmod +x scripts/run-validations.sh
./scripts/run-validations.sh rust-stories
```

### Method 3: From a File List

```bash
#!/bin/bash
# Run validations from a file containing story IDs (one per line)

SESSION_NAME="batch-validation"
WORKFLOW="examples/workflows/bmad-story-validation.yaml"
STORY_FILE="stories-to-validate.txt"

first=true
while IFS= read -r story; do
    [[ -z "$story" || "$story" == \#* ]] && continue  # Skip empty/comments

    if $first; then
        tmux new-session -d -s "$SESSION_NAME" -n "$story" \
            "source python/.venv/bin/activate && tea run $WORKFLOW --input '{\"arg\": \"$story\"}'; read"
        first=false
    else
        tmux new-window -t "$SESSION_NAME" -n "$story" \
            "source python/.venv/bin/activate && tea run $WORKFLOW --input '{\"arg\": \"$story\"}'; read"
    fi
done < "$STORY_FILE"

tmux attach -t "$SESSION_NAME"
```

## Tmux Navigation

| Command | Action |
|---------|--------|
| `tmux attach -t SESSION` | Attach to session |
| `Ctrl-b d` | Detach from session |
| `Ctrl-b n` | Next window |
| `Ctrl-b p` | Previous window |
| `Ctrl-b NUMBER` | Go to window NUMBER |
| `Ctrl-b w` | List all windows |
| `Ctrl-b &` | Kill current window |
| `tmux kill-session -t SESSION` | Kill entire session |
| `tmux list-sessions` | List all sessions |
| `tmux list-windows -t SESSION` | List windows in session |

## Available Workflows

| Workflow | Purpose | Input |
|----------|---------|-------|
| `bmad-story-validation.yaml` | Validate story documents with QA agent | `{"arg": "STORY-ID"}` |

## Example: Rust Story Validation Sprint

Run all Rust agentic pattern stories:

```bash
#!/bin/bash
# Validate Rust agentic pattern stories

SESSION="rust-stories"
WORKFLOW="examples/workflows/bmad-story-validation.yaml"
ACTIVATE="source python/.venv/bin/activate"

declare -A STORIES=(
    ["TEA-RUST-041"]="TEA-AGENT-001.1-rust"
    ["TEA-RUST-042"]="TEA-AGENT-001.2-rust"
    ["TEA-RUST-043"]="TEA-AGENT-001.3-rust"
    ["TEA-RUST-044"]="TEA-AGENT-001.4-rust"
    ["TEA-RUST-045"]="TEA-AGENT-001.5-rust"
)

first=true
for window_name in "${!STORIES[@]}"; do
    story_id="${STORIES[$window_name]}"
    if $first; then
        tmux new-session -d -s "$SESSION" -n "$window_name" \
            "$ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"$story_id\"}'; read -p 'Press Enter...'"
        first=false
    else
        tmux new-window -t "$SESSION" -n "$window_name" \
            "$ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"$story_id\"}'; read -p 'Press Enter...'"
    fi
done

echo "Session '$SESSION' created with windows:"
tmux list-windows -t "$SESSION"
echo ""
echo "Attach: tmux attach -t $SESSION"
```

## Tips

### Keep Output After Completion
The `read -p 'Press Enter...'` at the end prevents the window from closing, allowing you to review output.

### Check Running Workflows
```bash
# List all tmux sessions
tmux list-sessions

# List windows in a session
tmux list-windows -t validation
```

### Kill All Validation Sessions
```bash
tmux kill-session -t validation
```

### View Output Without Attaching
```bash
# Capture current pane content
tmux capture-pane -t validation:0 -p
```

### Run with Timeout
```bash
tmux new-window -t session -n "story" \
    "timeout 300 tea run workflow.yaml --input '{}' || echo 'Timed out'; read"
```

## Workflow Input Reference

### bmad-story-validation.yaml

| Parameter | Type | Description |
|-----------|------|-------------|
| `arg` | string | Story ID to validate (e.g., "TEA-AGENT-001.1") |

The workflow:
1. Detects BMad directory structure
2. Loads QA agent persona and task definition
3. Runs QA test design validation
4. Generates QA notes
5. Runs SM (Scrum Master) review
6. Updates story status

## Creating New Workflows

When creating new workflows for batch execution:

1. Use consistent input schema (prefer `arg` for simple cases)
2. Document required inputs in workflow description
3. Add workflow to the table in this guide
4. Create example batch scripts if needed

## Troubleshooting

### "Session not found"
```bash
# Check if session exists
tmux list-sessions

# Create new session
tmux new-session -s my-session
```

### "Window closed immediately"
Add `read` or `read -p 'Press Enter...'` at the end of the command to keep window open.

### "Python environment not found"
Ensure the virtual environment path is correct:
```bash
# Check venv exists
ls -la python/.venv/bin/activate

# Or use full path
source /home/user/project/python/.venv/bin/activate
```
