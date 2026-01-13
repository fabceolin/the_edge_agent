#!/bin/bash
# Encode a fixture JSON to URL using Rust
# Usage: ./rust_encoder.sh <fixture_filename>
# Example: ./rust_encoder.sh panic_simple.json

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

# Build Rust if needed (silently)
cd "$REPO_ROOT/rust"
cargo build --release 2>/dev/null

# Run Rust encoder test binary
# Since we don't have a direct CLI command, we use cargo test --lib to run encoder
# For parity testing, we'll output the encoded URL

# Read the JSON content
JSON_CONTENT=$(cat "$FIXTURE_PATH")

# Use Rust to encode - this requires a helper binary
# For now, output a placeholder - we'll implement proper encoding via Python calling Rust
cd "$REPO_ROOT/rust"
cargo run --release --quiet --bin tea-encode-report -- "$JSON_CONTENT"
