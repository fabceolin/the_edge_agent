#!/bin/bash
# TEA-DIST-001: Install script for Linux and macOS
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.sh | bash
#
# Or specify a version:
#   curl -fsSL https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.sh | bash -s -- v0.9.42

set -e

# Configuration
REPO="fabceolin/the_edge_agent"
BINARY_NAME="tea.com"
INSTALL_DIR="${HOME}/.local/bin"
VERSION="${1:-latest}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check for required tools
check_requirements() {
    if ! command -v curl &> /dev/null; then
        error "curl is required but not installed. Please install curl and try again."
    fi

    if ! command -v docker &> /dev/null; then
        warn "Docker is not installed. TEA requires Docker to run."
        warn "Install Docker from: https://docs.docker.com/get-docker/"
    fi
}

# Get the latest release tag
get_latest_version() {
    curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" | \
        grep '"tag_name":' | \
        sed -E 's/.*"([^"]+)".*/\1/'
}

# Download and install
install_tea() {
    local version="$1"
    local download_url

    # Resolve latest version if needed
    if [ "$version" = "latest" ]; then
        info "Fetching latest release..."
        version=$(get_latest_version)
        if [ -z "$version" ]; then
            error "Failed to get latest version"
        fi
    fi

    info "Installing TEA ${version}..."

    # Create install directory
    mkdir -p "$INSTALL_DIR"

    # Download binary
    download_url="https://github.com/${REPO}/releases/download/${version}/${BINARY_NAME}"
    info "Downloading from ${download_url}..."

    if ! curl -fsSL -o "${INSTALL_DIR}/tea" "$download_url"; then
        error "Failed to download TEA. Check if version ${version} exists."
    fi

    # Make executable
    chmod +x "${INSTALL_DIR}/tea"

    info "TEA installed to ${INSTALL_DIR}/tea"
}

# Check if install dir is in PATH
check_path() {
    if [[ ":$PATH:" != *":${INSTALL_DIR}:"* ]]; then
        echo ""
        warn "The installation directory is not in your PATH."
        echo ""
        echo "Add the following to your shell configuration file:"
        echo ""
        echo "  For bash (~/.bashrc):"
        echo "    export PATH=\"\$HOME/.local/bin:\$PATH\""
        echo ""
        echo "  For zsh (~/.zshrc):"
        echo "    export PATH=\"\$HOME/.local/bin:\$PATH\""
        echo ""
        echo "  For fish (~/.config/fish/config.fish):"
        echo "    set -gx PATH \$HOME/.local/bin \$PATH"
        echo ""
        echo "Then restart your shell or run: source ~/.bashrc"
    fi
}

# Verify installation
verify() {
    if [ -x "${INSTALL_DIR}/tea" ]; then
        echo ""
        info "Installation successful!"
        echo ""
        "${INSTALL_DIR}/tea" --wrapper-version
        echo ""
        info "Run 'tea --help' to get started."
    else
        error "Installation verification failed"
    fi
}

# Pull Docker image
pull_image() {
    if command -v docker &> /dev/null; then
        echo ""
        info "Pulling TEA Docker image (this may take a minute)..."
        docker pull ghcr.io/fabceolin/tea:latest || warn "Failed to pull Docker image. You can pull it later with: tea --docker-pull"
    fi
}

# Main
main() {
    echo ""
    echo "================================================"
    echo "  TEA - The Edge Agent Installer"
    echo "================================================"
    echo ""

    check_requirements
    install_tea "$VERSION"
    check_path
    verify
    pull_image

    echo ""
    echo "================================================"
    echo "  Installation Complete!"
    echo "================================================"
    echo ""
}

main
