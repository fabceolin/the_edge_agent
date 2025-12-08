#!/bin/bash
# Usage: ./scripts/release.sh 0.7.0 "Release message"
# Creates a git tag and updates setup.py version

set -e

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

# Update setup.py
CURRENT_VERSION=$(grep -oP 'version="\K[^"]+' setup.py)
echo "Updating version: $CURRENT_VERSION -> $VERSION"
sed -i "s/version=\"$CURRENT_VERSION\"/version=\"$VERSION\"/" setup.py

# Commit and tag
git add setup.py
git commit -m "chore: bump version to $VERSION"
git tag -a "v$VERSION" -m "$MESSAGE"

echo ""
echo "Done! Created tag v$VERSION"
echo "To push: git push origin main --tags"
