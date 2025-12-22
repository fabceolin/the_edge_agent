#!/bin/bash
# Usage: ./scripts/release.sh 0.7.0 "Release message"
# Creates a git tag and updates version in all files:
#   - python/setup.py
#   - rust/Cargo.toml
#   - docs/shared/YAML_REFERENCE.md

set -e

# Get script directory (works even when called from different locations)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

VERSION="${1:-}"
MESSAGE="${2:-Release v$VERSION}"

if [ -z "$VERSION" ]; then
    echo "Usage: $0 <version> [message]"
    echo "Example: $0 0.7.0 'Add new features'"
    exit 1
fi

# Validate version format (semver-like)
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Error: Version must be in format X.Y.Z (e.g., 0.7.0)"
    exit 1
fi

echo "Updating version to $VERSION in all files..."
echo ""

# Update python/setup.py
SETUP_PY="$PROJECT_ROOT/python/setup.py"
if [ -f "$SETUP_PY" ]; then
    CURRENT_PY_VERSION=$(grep -oP 'version="\K[^"]+' "$SETUP_PY")
    echo "Python setup.py: $CURRENT_PY_VERSION -> $VERSION"
    sed -i "s/version=\"$CURRENT_PY_VERSION\"/version=\"$VERSION\"/" "$SETUP_PY"
else
    echo "Warning: $SETUP_PY not found"
fi

# Update rust/Cargo.toml
CARGO_TOML="$PROJECT_ROOT/rust/Cargo.toml"
if [ -f "$CARGO_TOML" ]; then
    CURRENT_RUST_VERSION=$(grep -oP '^version = "\K[^"]+' "$CARGO_TOML")
    echo "Rust Cargo.toml: $CURRENT_RUST_VERSION -> $VERSION"
    sed -i "s/^version = \"$CURRENT_RUST_VERSION\"/version = \"$VERSION\"/" "$CARGO_TOML"
else
    echo "Warning: $CARGO_TOML not found"
fi

# Update docs/shared/YAML_REFERENCE.md
YAML_REF="$PROJECT_ROOT/docs/shared/YAML_REFERENCE.md"
if [ -f "$YAML_REF" ]; then
    CURRENT_DOC_VERSION=$(grep -oP '^Version: \K[0-9]+\.[0-9]+\.[0-9]+' "$YAML_REF")
    echo "YAML_REFERENCE.md: $CURRENT_DOC_VERSION -> $VERSION"
    sed -i "s/^Version: $CURRENT_DOC_VERSION/Version: $VERSION/" "$YAML_REF"
else
    echo "Warning: $YAML_REF not found"
fi

echo ""

# Commit and tag
git add "$SETUP_PY" "$CARGO_TOML" "$YAML_REF"
git commit -m "chore: bump version to $VERSION"
git tag -a "v$VERSION" -m "$MESSAGE"

echo "Done! Created tag v$VERSION"
echo "To push: git push origin main --tags"
