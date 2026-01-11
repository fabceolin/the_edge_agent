#!/usr/bin/env bash
# Update tea-python and tea-rust AppImages locally
# Usage: ./update-tea-local.sh [VERSION]
# Example: ./update-tea-local.sh 0.9.33

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() { echo -e "${GREEN}[INFO]${NC} $*"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# Check for gh CLI
if ! command -v gh &> /dev/null; then
    error "GitHub CLI (gh) is required. Install with: sudo apt install gh"
    exit 1
fi

# Get version (from arg or latest release)
if [[ -n "${1:-}" ]]; then
    VERSION="$1"
    info "Using specified version: v${VERSION}"
else
    VERSION=$(gh release list --repo fabceolin/the_edge_agent --limit 1 --json tagName -q '.[0].tagName' | sed 's/^v//')
    info "Latest version: v${VERSION}"
fi

# Detect architecture
ARCH=$(uname -m)
[[ "$ARCH" == "aarch64" ]] && ARCH="aarch64" || ARCH="x86_64"
info "Architecture: ${ARCH}"

# Determine install directory
if command -v tea-python &> /dev/null; then
    INSTALL_DIR=$(dirname "$(command -v tea-python)")
else
    INSTALL_DIR="${HOME}/.local/bin"
    mkdir -p "$INSTALL_DIR"
    if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
        warn "$INSTALL_DIR is not in PATH. Add to ~/.bashrc:"
        warn "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    fi
fi
info "Install directory: ${INSTALL_DIR}"

# Download AppImages
info "Downloading tea-python and tea-rust v${VERSION}..."
gh release download "v${VERSION}" \
    --repo fabceolin/the_edge_agent \
    --pattern "tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage" \
    --pattern "tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage" \
    --dir /tmp \
    --clobber

# Install
info "Installing to ${INSTALL_DIR}..."
install -m 755 "/tmp/tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage" "${INSTALL_DIR}/tea-python"
install -m 755 "/tmp/tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage" "${INSTALL_DIR}/tea-rust"

# Cleanup
rm -f "/tmp/tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage"
rm -f "/tmp/tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage"

# Verify
info "Verifying installation..."
echo -e "  tea-python: $(tea-python --version 2>/dev/null || echo 'not found')"
echo -e "  tea-rust:   $(tea-rust --version 2>/dev/null || echo 'not found')"

info "Done!"
