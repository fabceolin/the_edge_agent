# Edge Deployment

> Deploy AI agents anywhere: single binary, offline-first, serverless-ready.

## Why This Matters

Traditional AI frameworks require Python runtimes, package managers, and complex dependency chains. TEA eliminates deployment friction with statically-linked binaries that run anywhere - from Lambda functions to Raspberry Pis. No Docker containers needed. No runtime installations. Just copy the binary and execute.

## Value Proposition

| Benefit | Description |
|---------|-------------|
| **Single Binary** | One file contains everything - no Python/Rust installation required |
| **Offline-First** | Agents run without internet once deployed (unless they call external APIs) |
| **Serverless-Ready** | Cold start in milliseconds, perfect for Lambda/Cloudflare Workers |
| **Edge Computing** | Runs on resource-constrained devices (ARM64, IoT, embedded) |
| **Zero Dependencies** | Static linking (musl) means no shared library conflicts |

## Binary Matrix

TEA provides pre-built binaries for all major platforms:

| Platform | Standard Binary | With Prolog | AppImage | Size |
|----------|----------------|-------------|----------|------|
| Linux x86_64 | `tea-rust-linux-x86_64` | `tea-rust-linux-x86_64-prolog` | `tea-{version}-x86_64.AppImage` | 15-50MB |
| Linux ARM64 | `tea-rust-linux-arm64` | `tea-rust-linux-arm64-prolog` | `tea-{version}-aarch64.AppImage` | 15-50MB |
| macOS Intel | `tea-rust-darwin-x86_64` | - | N/A | ~15MB |
| macOS Apple Silicon | `tea-rust-darwin-arm64` | - | N/A | ~15MB |
| Windows x86_64 | `tea-rust-windows-x86_64.exe` | - | N/A | ~15MB |

### Binary Variants Explained

| Variant | Prolog Support | Dependencies | Use Case |
|---------|---------------|--------------|----------|
| **Standard** (`tea-rust-*`) | No | None (static musl) | Minimal footprint, no neurosymbolic AI |
| **Prolog** (`*-prolog`) | Yes | `libswipl.so` required | Neurosymbolic AI when SWI-Prolog is installed |
| **AppImage** (`*.AppImage`) | Yes | None (self-contained) | Neurosymbolic AI without system dependencies |

### Which Binary Should I Use?

```
Do you need Prolog (neurosymbolic AI) support?
|
+-- No  --> Use `tea-rust-{platform}` (smallest, ~15MB)
|
+-- Yes --> Is SWI-Prolog installed on your system?
            |
            +-- Yes --> Use `tea-rust-{platform}-prolog` (~18MB)
            |
            +-- No  --> Use `tea-{version}-{arch}.AppImage` (~50MB, self-contained)
```

## Deployment Targets

### AWS Lambda

```bash
# Package for Lambda
zip function.zip tea-rust-linux-x86_64 agent.yaml
aws lambda create-function \
  --function-name tea-agent \
  --runtime provided.al2 \
  --handler bootstrap \
  --zip-file fileb://function.zip
```

TEA's static binary eliminates Lambda layer complexity. Cold start is under 100ms.

### Cloudflare Workers

TEA's Rust implementation compiles to WebAssembly (future roadmap). For now, use HTTP-triggered workflows:

```yaml
name: cloudflare-edge-agent
nodes:
  - name: process
    run: |
      # Process edge request
      return {"response": state["request"]["body"].upper()}
```

### Raspberry Pi / ARM64

```bash
# Download ARM64 binary
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-rust-linux-arm64 -o tea
chmod +x tea

# Run on Pi
./tea run my-iot-agent.yaml
```

Tested on Raspberry Pi 4 (ARM Cortex-A72). Memory usage under 50MB for typical agents.

### Docker (Optional)

While TEA doesn't require Docker, you can containerize for orchestration:

```dockerfile
FROM scratch
COPY tea-rust-linux-x86_64 /tea
COPY agent.yaml /agent.yaml
ENTRYPOINT ["/tea", "run", "/agent.yaml"]
```

Results in a minimal ~20MB container image.

## AppImage: Self-Contained Prolog

AppImage is a portable Linux application format that bundles:

- TEA binary with Prolog feature enabled
- SWI-Prolog runtime (`libswipl.so`)
- All transitive dependencies (glibc-compatible)
- Prolog library files (`/usr/lib/swi-prolog`)

### Why AppImage?

The Prolog feature requires SWI-Prolog's shared library. Instead of asking users to install `swi-prolog-nox` system-wide, the AppImage bundles everything:

```bash
# Download AppImage (no installation needed)
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-0.8.13-x86_64.AppImage -o tea.AppImage
chmod +x tea.AppImage

# Run Prolog-enabled agent (works on any Linux distro)
./tea.AppImage run examples/prolog/simple-prolog-agent.yaml
```

### AppImage Portability

| Distribution | Standard Binary | AppImage |
|--------------|----------------|----------|
| Ubuntu 22.04+ | Works | Works |
| Fedora 38+ | Works | Works |
| Arch Linux | Works | Works |
| Alpine Linux | Works (musl) | Not tested |
| RHEL 8+ | Works | Works |

The AppImage uses FUSE for mounting. On minimal systems without FUSE:

```bash
# Extract and run directly
./tea.AppImage --appimage-extract
./squashfs-root/AppRun run agent.yaml
```

## Quick Start

### Install Standard Binary

```bash
# Linux x86_64
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-rust-linux-x86_64 -o tea
chmod +x tea
sudo mv tea /usr/local/bin/

# Verify
tea --version
tea --impl  # Shows: prolog: false
```

### Install AppImage (with Prolog)

```bash
# Download
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea-0.8.13-x86_64.AppImage -o tea.AppImage
chmod +x tea.AppImage

# Verify
./tea.AppImage --version
./tea.AppImage --impl  # Shows: prolog: true
```

### Verify Downloads

Each release includes checksums for security:

```bash
curl -L https://github.com/fabceolin/the_edge_agent/releases/latest/download/SHA256SUMS.txt -o SHA256SUMS.txt
sha256sum -c SHA256SUMS.txt --ignore-missing
```

## Key Features

| Feature | Description |
|---------|-------------|
| **Static Linking** | musl libc on Linux eliminates glibc version conflicts |
| **Cross-Platform** | Single codebase compiles for Linux, macOS, Windows |
| **Feature Flags** | Compile with `--features prolog` for neurosymbolic AI |
| **UPX Compression** | Python binaries compressed for smaller downloads |
| **Checksum Verification** | SHA256 sums for all release artifacts |

## Examples

- [Simple Prolog Agent](https://github.com/fabceolin/the_edge_agent/blob/main/examples/prolog/simple-prolog-agent.yaml) - Basic neurosymbolic inference
- [Parallel Workflows](https://github.com/fabceolin/the_edge_agent/tree/main/examples/prolog/parity) - Fan-out/fan-in patterns
- [Web Automation](https://github.com/fabceolin/the_edge_agent/tree/main/examples/web) - Headless browser agents

## Learn More

- [GitHub Releases](https://github.com/fabceolin/the_edge_agent/releases) - Download binaries
- [Multi-Platform Binaries Story](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-RELEASE-001-multi-platform-binaries.md) - Implementation details
- [Prolog AppImage Story](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-RELEASE-002-rust-prolog-appimage.md) - AppImage build process
- [README Download Section](https://github.com/fabceolin/the_edge_agent#download-pre-built-binaries) - Quick install commands
- [Neurosymbolic Capability](./neurosymbolic.md) - Prolog integration details

## Technical Details

### Build Process

Binaries are built via GitHub Actions on tag push:

1. **Python binaries**: PyInstaller with `--onefile` flag
2. **Rust binaries**: `cargo build --release` with musl target
3. **Prolog binaries**: `cargo build --features prolog` with glibc (dynamic)
4. **AppImages**: `linuxdeploy` auto-bundles dependencies

### Why musl for Standard, glibc for Prolog?

- **musl**: Fully static, maximum portability, but incompatible with SWI-Prolog's FFI
- **glibc**: Required for `swipl-rs` Prolog bindings, hence Prolog builds are dynamically linked

The AppImage solves this by bundling glibc libraries, making Prolog portable without system dependencies.

### Release Artifacts

Each GitHub release includes:

| Artifact | Description |
|----------|-------------|
| `tea-rust-{platform}` | Standard Rust binary (5 platforms) |
| `tea-rust-{platform}-prolog` | Prolog-enabled binary (Linux only) |
| `tea-{version}-{arch}.AppImage` | Self-contained AppImage (Linux only) |
| `tea-python-{platform}` | Python standalone binary (5 platforms) |
| `*.whl` | Python wheel for pip installation |
| `SHA256SUMS.txt` | Checksums for all artifacts |
