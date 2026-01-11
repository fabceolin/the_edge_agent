#!/bin/bash
# Encode a fixture JSON to URL using Python
# Usage: ./python_encoder.sh <fixture_filename>
# Example: ./python_encoder.sh panic_simple.json

set -e

FIXTURE=$1
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
FIXTURES_DIR="$REPO_ROOT/tests/report/fixtures"

if [ -z "$FIXTURE" ]; then
    echo "Usage: $0 <fixture_filename>" >&2
    exit 1
fi

FIXTURE_PATH="$FIXTURES_DIR/$FIXTURE"

if [ ! -f "$FIXTURE_PATH" ]; then
    echo "Error: Fixture not found: $FIXTURE_PATH" >&2
    exit 1
fi

# Read the JSON content
JSON_CONTENT=$(cat "$FIXTURE_PATH")

# Run Python encoder
cd "$REPO_ROOT/python"
python -m the_edge_agent.report_encoder "$JSON_CONTENT"
