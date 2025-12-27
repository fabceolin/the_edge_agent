# Installation Guide

This guide covers all installation methods for The Edge Agent (tea).

## Quick Install

```bash
# Linux/macOS - Download and install Rust binary
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-rust-linux-x86_64 -o tea
chmod +x tea
sudo mv tea /usr/local/bin/

# Verify installation
tea --version
```

## Pre-built Binaries

Pre-built binaries are available for all major platforms. No Python or Rust installation required!

**Latest Release:** [GitHub Releases](https://github.com/fabceolin/the_edge_agent/releases/latest)

### Platform Matrix

| Platform | Python CLI | Rust CLI |
|----------|-----------|----------|
| Linux x86_64 | `tea-python-linux-x86_64` | `tea-rust-linux-x86_64` |
| Linux ARM64 | `tea-python-linux-arm64` | `tea-rust-linux-arm64` |
| macOS Intel | `tea-python-darwin-x86_64` | `tea-rust-darwin-x86_64` |
| macOS Apple Silicon | `tea-python-darwin-arm64` | `tea-rust-darwin-arm64` |
| Windows | `tea-python-windows-x86_64.exe` | `tea-rust-windows-x86_64.exe` |

### Rust Binary Variants (Prolog Support)

For **neurosymbolic AI** with Prolog inference, additional binary variants are available:

| Binary | Prolog | Size (est.) | Description |
|--------|--------|-------------|-------------|
| `tea-rust-linux-x86_64` | No | ~15MB | Core features, statically linked (musl) |
| `tea-rust-linux-x86_64-prolog` | Yes* | ~18MB | With Prolog support, requires `libswipl.so` installed |
| `tea-{version}-x86_64.AppImage` | Yes | ~50MB | **Self-contained** with all libs bundled |
| `tea-rust-linux-arm64` | No | ~15MB | Core features, statically linked (musl) |
| `tea-rust-linux-arm64-prolog` | Yes* | ~18MB | With Prolog support, requires `libswipl.so` installed |
| `tea-{version}-aarch64.AppImage` | Yes | ~50MB | **Self-contained** with all libs bundled |

*Requires SWI-Prolog installed on the system (`apt install swi-prolog-nox`)

### Which Binary Should I Use?

```
Do you need Prolog support?
+-- No  -> Use `tea-rust-linux-{arch}` (smallest, static)
+-- Yes -> Is SWI-Prolog installed on your system?
           +-- Yes -> Use `tea-rust-linux-{arch}-prolog` (smaller)
           +-- No  -> Use `tea-{version}-{arch}.AppImage` (self-contained)
```

## AppImage Installation

AppImages are **self-contained** executables that bundle the tea binary, SWI-Prolog runtime, and all dependencies. No installation required!

```bash
# Download the AppImage
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-0.8.1-x86_64.AppImage -o tea.AppImage

# Make executable and run
chmod +x tea.AppImage
./tea.AppImage --version

# Run a Prolog-enabled agent
./tea.AppImage run examples/prolog/simple-prolog-agent.yaml
```

AppImages work on any Linux distribution (Ubuntu, Fedora, Arch, Alpine, etc.) without installing SWI-Prolog system-wide.

## Python Installation

### From Source

```bash
cd python && pip install -e .
python -c "import the_edge_agent as tea; print(tea.__version__)"
```

### From Git

```bash
pip install git+https://github.com/fabceolin/the_edge_agent.git
```

After installation, the `tea` command will be available globally.

## Rust Installation

### From Source

```bash
cd rust && cargo build --release
./target/release/tea --help
```

## Verify Downloads

Each release includes `SHA256SUMS.txt` for verification:

```bash
# Download checksum file and binary
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/SHA256SUMS.txt -o SHA256SUMS.txt
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-rust-linux-x86_64 -o tea-rust-linux-x86_64

# Verify (Linux)
sha256sum -c SHA256SUMS.txt --ignore-missing

# Verify (macOS)
shasum -a 256 -c SHA256SUMS.txt --ignore-missing
```

## Implementations

This is a **polyglot monorepo** with two implementations:

| Implementation | Status | Best For |
|----------------|--------|----------|
| **[Python](python/getting-started.md)** | Production-ready | Online edge computing, full feature set, 20+ built-in actions |
| **[Rust](rust/getting-started.md)** | Active development | Embedded offline systems, resource-constrained environments |

The **Python implementation** is optimized for online edge computing scenarios where network connectivity enables access to external APIs, LLM services, and cloud resources. The **Rust implementation** is designed for embedded offline systems where minimal footprint, deterministic execution, and operation without network dependencies are critical.

Both implementations share the same YAML agent syntax and can run the same agent configurations from the `examples/` directory.

## Repository Structure

```
the_edge_agent/
+-- python/          # Python implementation (full features)
+-- rust/            # Rust implementation (performance)
+-- examples/        # Shared YAML agents (works with both)
+-- docs/
    +-- shared/      # Language-agnostic docs (YAML reference)
    +-- python/      # Python-specific guides
    +-- rust/        # Rust-specific guides
```

## Next Steps

- [CLI Reference](shared/cli-reference.md) - Command-line usage
- [YAML Reference](shared/YAML_REFERENCE.md) - Agent configuration syntax
- [Python Getting Started](python/getting-started.md)
- [Rust Getting Started](rust/getting-started.md)
