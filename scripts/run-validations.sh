#!/bin/bash
# Run multiple story validations in tmux
# Usage: ./scripts/run-validations.sh [session-name] [story1] [story2] ...
#
# Examples:
#   ./scripts/run-validations.sh                    # Use defaults
#   ./scripts/run-validations.sh rust-sprint TEA-AGENT-001.1-rust TEA-AGENT-001.2-rust
#   ./scripts/run-validations.sh python-sprint TEA-AGENT-001.{1..5}

set -e

# Configuration
WORKFLOW="examples/workflows/bmad-story-validation.yaml"
VENV_ACTIVATE="source python/.venv/bin/activate"
DEFAULT_SESSION="story-validation"

# Parse arguments
SESSION_NAME="${1:-$DEFAULT_SESSION}"
shift 2>/dev/null || true

# If no stories provided, show usage
if [ $# -eq 0 ]; then
    echo "Usage: $0 [session-name] story1 [story2] [story3] ..."
    echo ""
    echo "Examples:"
    echo "  $0 rust-stories TEA-AGENT-001.1-rust TEA-AGENT-001.2-rust"
    echo "  $0 python-stories TEA-AGENT-001.1 TEA-AGENT-001.2 TEA-AGENT-001.3"
    echo ""
    echo "Predefined story sets:"
    echo "  $0 --rust-agentic    # Rust agentic pattern stories (TEA-RUST-041-045)"
    echo "  $0 --python-agentic  # Python agentic pattern stories"
    exit 1
fi

# Handle predefined sets
case "$SESSION_NAME" in
    --rust-agentic)
        SESSION_NAME="rust-agentic"
        set -- "TEA-AGENT-001.1-rust" "TEA-AGENT-001.2-rust" "TEA-AGENT-001.3-rust" "TEA-AGENT-001.4-rust" "TEA-AGENT-001.5-rust"
        ;;
    --python-agentic)
        SESSION_NAME="python-agentic"
        set -- "TEA-AGENT-001.1" "TEA-AGENT-001.2" "TEA-AGENT-001.3" "TEA-AGENT-001.4" "TEA-AGENT-001.5"
        ;;
esac

STORIES=("$@")

# Check if session already exists
if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    echo "Session '$SESSION_NAME' already exists."
    echo "Kill it with: tmux kill-session -t $SESSION_NAME"
    echo "Or attach with: tmux attach -t $SESSION_NAME"
    exit 1
fi

# Create session with first story
first_story="${STORIES[0]}"
echo "Creating session '$SESSION_NAME' with ${#STORIES[@]} workflows..."

tmux new-session -d -s "$SESSION_NAME" -n "$first_story" \
    "$VENV_ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"$first_story\"}'; echo ''; echo '=== Completed: $first_story ==='; read -p 'Press Enter to close...'"

# Add windows for remaining stories
for story in "${STORIES[@]:1}"; do
    tmux new-window -t "$SESSION_NAME" -n "$story" \
        "$VENV_ACTIVATE && tea run $WORKFLOW --input '{\"arg\": \"$story\"}'; echo ''; echo '=== Completed: $story ==='; read -p 'Press Enter to close...'"
done

echo ""
echo "Created tmux session: $SESSION_NAME"
echo "Windows:"
tmux list-windows -t "$SESSION_NAME" -F "  - #W"
echo ""
echo "Attach with:"
echo "  tmux attach -t $SESSION_NAME"
echo ""
echo "Navigation:"
echo "  Ctrl-b n  - Next window"
echo "  Ctrl-b p  - Previous window"
echo "  Ctrl-b d  - Detach"
