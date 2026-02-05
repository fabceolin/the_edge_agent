#!/bin/bash
# Usage: ./scripts/release.sh <version|increment> [message]
#
# Version can be:
#   - Explicit: 0.7.0, 1.2.3
#   - Increment: major, minor, patch (or X.0.0, 0.X.0, 0.0.X)
#
# Examples:
#   ./scripts/release.sh 0.7.0 "Release message"
#   ./scripts/release.sh patch              # 0.9.33 -> 0.9.34
#   ./scripts/release.sh minor              # 0.9.33 -> 0.10.0
#   ./scripts/release.sh major              # 0.9.33 -> 1.0.0
#
# Creates a git tag and updates version in all files:
#   - python/setup.py
#   - rust/Cargo.toml
#   - docs/shared/YAML_REFERENCE.md
#   - python/src/the_edge_agent/__init__.py

set -e

# Get script directory (works even when called from different locations)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

VERSION_ARG="${1:-}"
MESSAGE="${2:-}"

if [ -z "$VERSION_ARG" ]; then
    echo "Usage: $0 <version|increment> [message]"
    echo ""
    echo "Version can be:"
    echo "  Explicit:  0.7.0, 1.2.3"
    echo "  Increment: major (X.0.0), minor (0.X.0), patch (0.0.X)"
    echo ""
    echo "Examples:"
    echo "  $0 0.7.0 'Add new features'"
    echo "  $0 patch                      # Increment patch version"
    echo "  $0 minor 'New feature release'"
    exit 1
fi

# Get current version from __init__.py
INIT_PY="$PROJECT_ROOT/python/src/the_edge_agent/__init__.py"
if [ -f "$INIT_PY" ]; then
    CURRENT_VERSION=$(grep -oP '__version__ = "\K[^"]+' "$INIT_PY")
else
    echo "Error: Cannot find $INIT_PY to read current version"
    exit 1
fi

# Parse current version into components
IFS='.' read -r CURRENT_MAJOR CURRENT_MINOR CURRENT_PATCH <<< "$CURRENT_VERSION"

# Handle semver increment arguments
case "$VERSION_ARG" in
    major|X.0.0)
        VERSION="$((CURRENT_MAJOR + 1)).0.0"
        echo "Incrementing major: $CURRENT_VERSION -> $VERSION"
        ;;
    minor|0.X.0)
        VERSION="$CURRENT_MAJOR.$((CURRENT_MINOR + 1)).0"
        echo "Incrementing minor: $CURRENT_VERSION -> $VERSION"
        ;;
    patch|0.0.X)
        VERSION="$CURRENT_MAJOR.$CURRENT_MINOR.$((CURRENT_PATCH + 1))"
        echo "Incrementing patch: $CURRENT_VERSION -> $VERSION"
        ;;
    *)
        # Assume it's an explicit version
        VERSION="$VERSION_ARG"
        # Validate version format (semver-like)
        if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
            echo "Error: Version must be in format X.Y.Z (e.g., 0.7.0)"
            echo "       Or use: major, minor, patch (or X.0.0, 0.X.0, 0.0.X)"
            exit 1
        fi
        ;;
esac

# Set default message if not provided
MESSAGE="${MESSAGE:-Release v$VERSION}"

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

# Update rust/Cargo.toml (only the [package] version, not dependencies)
CARGO_TOML="$PROJECT_ROOT/rust/Cargo.toml"
if [ -f "$CARGO_TOML" ]; then
    CURRENT_RUST_VERSION=$(grep -oP '^version = "\K[^"]+' "$CARGO_TOML" | head -1)
    echo "Rust Cargo.toml: $CURRENT_RUST_VERSION -> $VERSION"
    sed -i "0,/^version = \"$CURRENT_RUST_VERSION\"/s//version = \"$VERSION\"/" "$CARGO_TOML"
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

# Update python/src/the_edge_agent/__init__.py (__version__)
INIT_PY="$PROJECT_ROOT/python/src/the_edge_agent/__init__.py"
if [ -f "$INIT_PY" ]; then
    CURRENT_INIT_VERSION=$(grep -oP '__version__ = "\K[^"]+' "$INIT_PY")
    echo "__init__.py: $CURRENT_INIT_VERSION -> $VERSION"
    sed -i "s/__version__ = \"$CURRENT_INIT_VERSION\"/__version__ = \"$VERSION\"/" "$INIT_PY"
else
    echo "Warning: $INIT_PY not found"
fi

# Update docs/conf.py (Sphinx documentation copyright year)
CONF_PY="$PROJECT_ROOT/docs/conf.py"
CURRENT_YEAR=$(date +%Y)
if [ -f "$CONF_PY" ]; then
    CURRENT_DOC_YEAR=$(grep -oP 'copyright = "\K[0-9]+' "$CONF_PY")
    if [ "$CURRENT_DOC_YEAR" != "$CURRENT_YEAR" ]; then
        echo "docs/conf.py copyright: $CURRENT_DOC_YEAR -> $CURRENT_YEAR"
        sed -i "s/copyright = \"$CURRENT_DOC_YEAR,/copyright = \"$CURRENT_YEAR,/" "$CONF_PY"
    else
        echo "docs/conf.py copyright: already $CURRENT_YEAR (no change)"
    fi
else
    echo "Warning: $CONF_PY not found"
fi

echo ""

# Commit and tag
# Update Cargo.lock after Cargo.toml version change
cd "$PROJECT_ROOT/rust" && cargo update --workspace && cd "$PROJECT_ROOT"
CARGO_LOCK="$PROJECT_ROOT/rust/Cargo.lock"

git add "$SETUP_PY" "$CARGO_TOML" "$CARGO_LOCK" "$YAML_REF" "$INIT_PY"
git commit -m "chore(release): bump version to $VERSION"
git tag -a "v$VERSION" -m "$MESSAGE"

echo "Done! Created tag v$VERSION"
echo "To push: git push origin main --tags"
