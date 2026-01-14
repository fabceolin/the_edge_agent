# TEA Release Variants: Choosing the Right Distribution

**Fabricio Ceolin**

*Creator, The Edge Agent*

fabceolin@gmail.com

---

## Abstract

The Edge Agent (TEA) provides multiple distribution formats to accommodate diverse deployment scenarios, from minimal edge devices to GPU-accelerated AI workloads. This article explains the rationale behind each release variant, their capabilities, trade-offs, and optimal use cases. Understanding these options enables developers to select the most appropriate distribution for their specific requirements, whether prioritizing minimal footprint, self-contained deployment, or neurosymbolic AI capabilities with embedded language models.

**Keywords:** Distribution, Docker, AppImage, GPU, Neurosymbolic AI, Edge Computing

---

## 1. Introduction

Modern AI deployment faces a fundamental tension: the need for powerful capabilities versus the constraints of target environments. A cloud server with abundant resources has different requirements than an embedded device with limited storage. TEA addresses this through a tiered distribution strategy that offers:

- **Minimal footprint** for resource-constrained environments
- **Full-featured packages** for development and production servers
- **Self-contained bundles** for simplified deployment
- **AI-ready distributions** with embedded language models

This article examines each variant, explaining when and why to use them.

## 2. Distribution Overview

TEA releases include the following variant categories:

| Category | Formats | Size Range | Use Case |
|----------|---------|------------|----------|
| Base/Minimal | Binary, Docker | ~15-400MB | Edge devices, minimal dependencies |
| Full | Binary, Docker | ~80-772MB | Development, full features |
| AppImage | Self-contained | ~150-200MB | Linux desktop, no installation |
| LLM Embedded | AppImage, Docker | ~1-5GB | Offline AI, air-gapped environments |
| Docker Wrapper | APE binary | ~1.1MB | Cross-platform, GPU passthrough |

## 3. Base/Minimal Variants

### 3.1 Purpose

The base variants provide the smallest possible TEA installation. They include only the core workflow engine without optional features like Prolog reasoning, Lua scripting, or experiment tracking.

### 3.2 Specifications

**Python Base Binary:**
- Size: ~15-25MB (PyInstaller compiled)
- Dependencies: None (statically linked)
- Features: Core workflow engine, YAML parsing, basic actions

**Docker Base Image (`tea:base`):**
- Size: ~400MB
- Base: Ubuntu 24.04 + Python 3.12
- Features: Core TEA functionality only

### 3.3 When to Use

Choose base variants when:

1. **Storage is limited** - IoT devices, embedded systems
2. **Prolog/Lua not needed** - Pure Python workflows
3. **Container size matters** - Kubernetes with tight quotas
4. **Fast cold starts required** - Serverless functions

### 3.4 Build Command

```bash
# Docker base image
docker build -t tea:base -f docker/Dockerfile.base .
```

## 4. Full Variants

### 4.1 Purpose

Full variants include all TEA features: SWI-Prolog for logical reasoning, Lua scripting via lupa, Opik experiment tracking, and extended action libraries.

### 4.2 Specifications

**Python Full Binary:**
- Size: ~80-100MB
- Includes: Prolog bindings, Lua runtime, full action set

**Docker Full Image (`tea:full`):**
- Size: ~772MB
- Includes: Python 3.12, SWI-Prolog 9.3+, Lua, Opik

### 4.3 When to Use

Choose full variants when:

1. **Neurosymbolic AI** - Combining LLMs with logical reasoning
2. **Scripting flexibility** - Lua-based custom logic
3. **Experiment tracking** - Opik integration for ML workflows
4. **Development environment** - Access to all features

### 4.4 Build Command

```bash
# Docker full image
docker build -t tea:full -f docker/Dockerfile.full .
```

## 5. AppImage Variants

### 5.1 Purpose

AppImage bundles TEA with all dependencies (including SWI-Prolog and Lua runtimes) into a single executable file. No installation required - download, make executable, and run.

### 5.2 Specifications

| Variant | Architecture | Size | Includes |
|---------|--------------|------|----------|
| Python AppImage | x86_64 | ~150MB | Python + Prolog + Lua |
| Python AppImage | aarch64 | ~150MB | Python + Prolog + Lua |
| Rust AppImage | x86_64 | ~80MB | Rust + Prolog |
| Rust AppImage | aarch64 | ~80MB | Rust + Prolog |

### 5.3 Build Environment Limitation

AppImages are built on **Ubuntu 24.04**. This means:

- **Compatible with**: Ubuntu 22.04+, Debian 12+, Fedora 38+, and other modern distributions
- **May not work on**: Older distributions (CentOS 7, Ubuntu 20.04) due to glibc requirements
- **Workaround**: Use Docker variants for older systems

### 5.4 When to Use

Choose AppImage when:

1. **No root access** - User-level installation
2. **Portable deployment** - Copy single file to target
3. **Air-gapped Linux systems** - No network for Docker pulls
4. **Prolog required** - Self-contained Prolog runtime included

### 5.5 Usage

```bash
# Download and run
chmod +x tea-python-1.0.0-x86_64.AppImage
./tea-python-1.0.0-x86_64.AppImage --version

# Run Prolog-based agent
./tea-python-1.0.0-x86_64.AppImage run neurosymbolic-agent.yaml
```

## 6. LLM Embedded Variants

### 6.1 Purpose

LLM variants bundle a complete language model inside the distribution. This enables AI inference without external API calls or model downloads - critical for air-gapped environments and offline deployments.

### 6.2 Available Models

| Variant | Model | Size | Parameters | Use Case |
|---------|-------|------|------------|----------|
| `tea:gemma3-1b` | Google Gemma 3 1B | ~2GB | 1B | Fast inference, edge |
| `tea:gemma3-4b` | Google Gemma 3 4B | ~5GB | 4B | Better quality, balanced |
| `tea:phi4-mini` | Microsoft Phi-4 Mini | ~5GB | 3.8B | Reasoning tasks |
| `tea:gemma3n-e4b` | Google Gemma 3N E4B | ~5GB | 4B | Efficient variant |

### 6.3 Neurosymbolic AI Use Case

These variants are specifically designed for **neurosymbolic AI** in isolated environments:

```yaml
# Example: Offline neurosymbolic agent
name: offline-reasoning
settings:
  llm:
    provider: local
    model_path: ${TEA_MODEL_PATH}  # Set by container

nodes:
  - name: analyze
    action: llm.generate
    prompt: "Analyze: {{ state.input }}"

  - name: verify
    action: prolog.query
    query: "verify_analysis({{ state.analysis }})"
```

The combination of embedded LLM + Prolog enables:
- **Offline operation** - No internet required
- **Verifiable AI** - Prolog validates LLM outputs
- **Deterministic reasoning** - Symbolic logic constraints

### 6.4 When to Use

Choose LLM variants when:

1. **Air-gapped environments** - Secure facilities, offline systems
2. **Neurosymbolic AI** - LLM + Prolog hybrid reasoning
3. **Consistent behavior** - Same model across deployments
4. **No API costs** - Local inference only

### 6.5 Build Command

```bash
# Docker with Gemma 3 1B
docker build -t tea:gemma3-1b -f docker/Dockerfile.gemma3-1b .

# Docker with Phi-4 Mini
docker build -t tea:phi4-mini -f docker/Dockerfile.phi4-mini .
```

## 7. Docker Wrapper (tea.com)

### 7.1 Purpose

The Docker wrapper is an **Actually Portable Executable (APE)** built with Cosmopolitan Libc. A single ~1.1MB binary runs on Linux, macOS, Windows, and BSD systems, internally invoking TEA Docker containers.

### 7.2 How It Works

```
┌─────────────────────────────────────────────────────────┐
│  User runs: ./tea.com run agent.yaml                    │
│                         │                               │
│                         ▼                               │
│  ┌─────────────────────────────────────────────────┐   │
│  │  tea.com (Cosmopolitan APE)                     │   │
│  │  - Detects platform (Linux/macOS/Windows)       │   │
│  │  - Detects GPU vendor (NVIDIA/AMD/Intel)        │   │
│  │  - Builds docker run command                    │   │
│  │  - Mounts volumes, passes environment          │   │
│  └─────────────────────────────────────────────────┘   │
│                         │                               │
│                         ▼                               │
│  ┌─────────────────────────────────────────────────┐   │
│  │  Docker Container (tea:full or tea:gemma3-1b)   │   │
│  │  - Executes TEA with full capabilities          │   │
│  │  - GPU available via --gpus or --device         │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

### 7.3 GPU Passthrough

The wrapper automatically detects and configures GPU access:

| Vendor | Detection | Docker Flags |
|--------|-----------|--------------|
| NVIDIA | `nvidia-smi` | `--gpus all` |
| AMD | `/dev/kfd` or `rocm-smi` | `--device=/dev/kfd --device=/dev/dri --group-add video` |
| Intel/Vulkan | `/dev/dri` | `--device=/dev/dri` |

**Configuration via TEA_GPU:**

```bash
# Auto-detect (default)
./tea.com run llm-agent.yaml

# Force NVIDIA
TEA_GPU=nvidia ./tea.com run llm-agent.yaml

# Force AMD ROCm
TEA_GPU=amd ./tea.com run llm-agent.yaml

# Force Intel/Vulkan
TEA_GPU=vulkan ./tea.com run llm-agent.yaml

# Disable GPU
TEA_GPU=none ./tea.com run agent.yaml

# Specific GPU count (NVIDIA)
TEA_GPU=2 ./tea.com run llm-agent.yaml
```

### 7.4 Platform Compatibility

| Platform | Status | Notes |
|----------|--------|-------|
| Linux x86_64 | Full support | All GPU vendors |
| Linux aarch64 | Full support | All GPU vendors |
| macOS x86_64 | Full support | Docker Desktop required |
| macOS arm64 | Full support | Docker Desktop required |
| Windows x86_64 | Full support | NVIDIA via WSL2 only |
| FreeBSD/NetBSD | Full support | - |

### 7.5 When to Use

Choose the Docker wrapper when:

1. **Cross-platform teams** - Same binary for all developers
2. **GPU acceleration needed** - Automatic GPU detection and passthrough
3. **Container isolation preferred** - Security through containerization
4. **Multiple image variants** - Easy switching via `TEA_VERSION`

### 7.6 Usage

```bash
# Basic usage
./tea.com run agent.yaml

# Use specific image version
TEA_VERSION=v1.0.0 ./tea.com run agent.yaml

# Use LLM variant
TEA_VERSION=gemma3-1b ./tea.com run llm-agent.yaml

# Pull latest image
./tea.com --docker-pull

# Windows: rename to .exe
ren tea.com tea.exe
tea.exe --version
```

## 8. Decision Matrix

Use this matrix to select the appropriate variant:

| Requirement | Recommended Variant |
|-------------|---------------------|
| Minimal size, no Prolog | Base binary or `tea:base` |
| Full features, Docker available | `tea:full` |
| Linux, no Docker, Prolog needed | AppImage |
| Offline AI, air-gapped | LLM Docker (`tea:gemma3-1b`) |
| Cross-platform with GPU | Docker wrapper (`tea.com`) |
| Serverless/Lambda | Base binary |
| Kubernetes | `tea:base` or `tea:full` |
| Edge device (ARM) | AppImage aarch64 or Rust binary |

## 9. Size Comparison

```
┌──────────────────────────────────────────────────────────────┐
│ Distribution Size Comparison                                 │
├──────────────────────────────────────────────────────────────┤
│ tea.com (wrapper)     █ 1.1MB                               │
│ Rust base binary      ████ 15MB                              │
│ Python base binary    █████ 25MB                             │
│ Rust AppImage         █████████ 80MB                         │
│ Python full binary    ███████████ 100MB                      │
│ Python AppImage       ██████████████ 150MB                   │
│ tea:base Docker       ████████████████████████████ 400MB     │
│ tea:full Docker       ██████████████████████████████████ 772MB│
│ tea:gemma3-1b Docker  █████████████████████████████████████████████ 2GB │
│ tea:phi4-mini Docker  ██████████████████████████████████████████████████████████████ 5GB │
└──────────────────────────────────────────────────────────────┘
```

## 10. Conclusion

TEA's distribution strategy balances capability with resource constraints:

1. **Base variants** - Minimal footprint for constrained environments
2. **Full variants** - Complete feature set for development and production
3. **AppImages** - Self-contained deployment without installation (Ubuntu 24.04+ compatible)
4. **LLM variants** - Offline neurosymbolic AI for air-gapped environments
5. **Docker wrapper** - Universal binary with automatic GPU passthrough

The key insight is that **neurosymbolic AI** - combining neural (LLM) and symbolic (Prolog) approaches - becomes practical for isolated deployments through the LLM embedded variants. Meanwhile, the Cosmopolitan-based Docker wrapper democratizes GPU access across platforms with a single binary.

Select based on your constraints: storage, network availability, GPU requirements, and target operating system. When in doubt, the Docker wrapper (`tea.com`) with `tea:full` provides the most flexible starting point.

## 11. References

- [TEA Documentation](https://fabceolin.github.io/the_edge_agent/)
- [Cosmopolitan Libc](https://github.com/jart/cosmopolitan) - Actually Portable Executables
- [AppImage](https://appimage.org/) - Linux application distribution format
- [NVIDIA Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/)
- [AMD ROCm](https://rocm.docs.amd.com/) - AMD GPU compute platform
- [Google Gemma](https://ai.google.dev/gemma) - Open language models
- [Microsoft Phi-4](https://azure.microsoft.com/en-us/products/phi-4) - Efficient language models
