#!/bin/bash
# Copy wllama WASM assets from node_modules to assets directory
#
# This script copies the wllama WASM files (single-thread and multi-thread)
# from the @anthropic-wllama/wllama package to our assets directory for bundling.
#
# Usage:
#   ./scripts/copy-wllama-assets.sh
#
set -e

cd "$(dirname "$0")/.."

WLLAMA_DIR="node_modules/@anthropic-wllama/wllama"
ASSETS_DIR="assets"

# Check if wllama is installed
if [[ ! -d "$WLLAMA_DIR" ]]; then
    echo "Error: wllama not found in node_modules"
    echo "Run 'npm install' first"
    exit 1
fi

# Create assets directories
mkdir -p "$ASSETS_DIR/single-thread"
mkdir -p "$ASSETS_DIR/multi-thread"

echo "Copying wllama WASM assets..."

# Find and copy single-thread wasm
if [[ -f "$WLLAMA_DIR/dist/single-thread/wllama.wasm" ]]; then
    cp "$WLLAMA_DIR/dist/single-thread/wllama.wasm" "$ASSETS_DIR/single-thread/"
    echo "  Copied single-thread/wllama.wasm"
elif [[ -f "$WLLAMA_DIR/esm/single-thread/wllama.wasm" ]]; then
    cp "$WLLAMA_DIR/esm/single-thread/wllama.wasm" "$ASSETS_DIR/single-thread/"
    echo "  Copied single-thread/wllama.wasm (from esm)"
else
    echo "Warning: Could not find single-thread wllama.wasm"
    # Try to find it anywhere in the package
    FOUND=$(find "$WLLAMA_DIR" -name "wllama.wasm" -path "*/single-thread/*" 2>/dev/null | head -1)
    if [[ -n "$FOUND" ]]; then
        cp "$FOUND" "$ASSETS_DIR/single-thread/"
        echo "  Copied single-thread/wllama.wasm (from $FOUND)"
    fi
fi

# Find and copy multi-thread wasm
if [[ -f "$WLLAMA_DIR/dist/multi-thread/wllama.wasm" ]]; then
    cp "$WLLAMA_DIR/dist/multi-thread/wllama.wasm" "$ASSETS_DIR/multi-thread/"
    echo "  Copied multi-thread/wllama.wasm"
elif [[ -f "$WLLAMA_DIR/esm/multi-thread/wllama.wasm" ]]; then
    cp "$WLLAMA_DIR/esm/multi-thread/wllama.wasm" "$ASSETS_DIR/multi-thread/"
    echo "  Copied multi-thread/wllama.wasm (from esm)"
else
    echo "Warning: Could not find multi-thread wllama.wasm"
    # Try to find it anywhere in the package
    FOUND=$(find "$WLLAMA_DIR" -name "wllama.wasm" -path "*/multi-thread/*" 2>/dev/null | head -1)
    if [[ -n "$FOUND" ]]; then
        cp "$FOUND" "$ASSETS_DIR/multi-thread/"
        echo "  Copied multi-thread/wllama.wasm (from $FOUND)"
    fi
fi

# Show asset sizes
echo ""
echo "Asset sizes:"
ls -lh "$ASSETS_DIR/single-thread/"*.wasm 2>/dev/null || echo "  (no single-thread wasm found)"
ls -lh "$ASSETS_DIR/multi-thread/"*.wasm 2>/dev/null || echo "  (no multi-thread wasm found)"

echo ""
echo "Done!"
