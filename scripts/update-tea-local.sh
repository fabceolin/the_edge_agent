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

# Use a temp directory that works with snap-installed gh
# Snap apps can't write to /tmp, so use ~/.cache instead
DOWNLOAD_DIR="${HOME}/.cache/tea-update"
mkdir -p "$DOWNLOAD_DIR"

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

# Define file names
PYTHON_APPIMAGE="tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage"
RUST_APPIMAGE="tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage"

# Download AppImages
info "Downloading tea-python and tea-rust v${VERSION}..."
gh release download "v${VERSION}" \
    --repo fabceolin/the_edge_agent \
    --pattern "$PYTHON_APPIMAGE" \
    --pattern "$RUST_APPIMAGE" \
    --dir "$DOWNLOAD_DIR" \
    --clobber

# Verify downloads succeeded (gh can silently fail with snap confinement issues)
if [[ ! -f "${DOWNLOAD_DIR}/${PYTHON_APPIMAGE}" ]]; then
    error "Failed to download ${PYTHON_APPIMAGE}"
    error "This may be due to snap confinement. Try: sudo snap connect gh:removable-media"
    exit 1
fi
if [[ ! -f "${DOWNLOAD_DIR}/${RUST_APPIMAGE}" ]]; then
    error "Failed to download ${RUST_APPIMAGE}"
    error "This may be due to snap confinement. Try: sudo snap connect gh:removable-media"
    exit 1
fi

# Install
info "Installing to ${INSTALL_DIR}..."
install -m 755 "${DOWNLOAD_DIR}/${PYTHON_APPIMAGE}" "${INSTALL_DIR}/tea-python"
install -m 755 "${DOWNLOAD_DIR}/${RUST_APPIMAGE}" "${INSTALL_DIR}/tea-rust"

# Cleanup
rm -f "${DOWNLOAD_DIR}/${PYTHON_APPIMAGE}"
rm -f "${DOWNLOAD_DIR}/${RUST_APPIMAGE}"

# Verify
info "Verifying installation..."
echo -e "  tea-python: $(tea-python --version 2>/dev/null || echo 'not found')"
echo -e "  tea-rust:   $(tea-rust --version 2>/dev/null || echo 'not found')"

info "Done!"
