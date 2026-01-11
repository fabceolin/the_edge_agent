# Updating TEA AppImages Locally

This guide explains how to update `tea-python` and `tea-rust` AppImages with embedded LLM models.

## Overview

The Edge Agent provides AppImage bundles that include embedded LLM models:
- **tea-python**: Python implementation with Gemma3 1B model
- **tea-rust**: Rust implementation with Phi-4 model

## LLM Instructions

When asked to update or install tea-python and tea-rust, follow these steps:

### 1. Determine Install Location

Check where existing binaries are installed:
```bash
which tea-python tea-rust 2>/dev/null
```

If not found, use `~/.local/bin` (XDG standard, no sudo required):
```bash
mkdir -p ~/.local/bin
```

Ensure the directory is in PATH. If not, inform the user to add it:
```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="$HOME/.local/bin:$PATH"
```

### 2. Download and Install

```bash
# Set version (update this to the requested version)
VERSION="0.9.32"

# Detect architecture
ARCH=$(uname -m)
[[ "$ARCH" == "aarch64" ]] && ARCH="aarch64" || ARCH="x86_64"

# Determine install directory (use existing location or default)
INSTALL_DIR=$(dirname "$(which tea-python 2>/dev/null)" || echo "$HOME/.local/bin")
mkdir -p "$INSTALL_DIR"

# Download using gh CLI
gh release download "v${VERSION}" \
  --repo fabceolin/the_edge_agent \
  --pattern "tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage" \
  --pattern "tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage" \
  --dir /tmp

# Install
install -m 755 "/tmp/tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage" "$INSTALL_DIR/tea-python"
install -m 755 "/tmp/tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage" "$INSTALL_DIR/tea-rust"

# Cleanup
rm -f /tmp/tea-python-llm-gemma3-1b-${VERSION}-${ARCH}.AppImage
rm -f /tmp/tea-rust-llm-phi4-${VERSION}-${ARCH}.AppImage
```

### 3. Verify Installation

```bash
tea-python --version
tea-rust --version
```

## Install Locations

| Location | Scope | Requires sudo | Recommended |
|----------|-------|---------------|-------------|
| `~/.local/bin` | User only (XDG standard) | No | Yes (default) |
| `~/bin` | User only | No | Yes |
| `/usr/local/bin` | System-wide | Yes | For shared systems |

## Available AppImage Variants

| AppImage | LLM Model | Use Case |
|----------|-----------|----------|
| `tea-python-llm-gemma3-1b-{VERSION}-{ARCH}.AppImage` | Gemma3 1B | Python workflows with fast inference |
| `tea-rust-llm-phi4-{VERSION}-{ARCH}.AppImage` | Phi-4 | Rust workflows with reasoning tasks |
| `tea-rust-llm-gemma3-1b-{VERSION}-{ARCH}.AppImage` | Gemma3 1B | Rust workflows with fast inference |
| `tea-{VERSION}-{ARCH}.AppImage` | None | Rust CLI without embedded LLM |
| `tea-python-{VERSION}-{ARCH}.AppImage` | None | Python CLI without embedded LLM |

## Troubleshooting

### Command Not Found After Install
```bash
# Reload shell or run:
hash -r
# Or add to PATH:
export PATH="$HOME/.local/bin:$PATH"
```

### FUSE Error
```bash
# Extract and run directly
tea-python --appimage-extract-and-run --version
```

### Wrong Architecture
```bash
uname -m  # Should show x86_64 or aarch64
```

## Release Notes

https://github.com/fabceolin/the_edge_agent/releases
