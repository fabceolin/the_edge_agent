# Docker Installation Guide

TEA can be run via Docker with a cross-platform wrapper binary that provides a native CLI experience on Linux, macOS, and Windows.

## Docker Image Variants

TEA provides multiple Docker image variants for different use cases:

| Variant | Tag | Size | Description |
|---------|-----|------|-------------|
| **Base** | `tea:base` | ~400MB | Minimal core - Python 3.12 only |
| **Full** | `tea:full`, `tea:latest` | ~772MB | All features - Prolog, Lua, Opik |
| **Gemma 3 1B** | `tea:gemma3-1b` | ~2GB | Full + bundled Gemma 3 1B Q8_0 model |
| **Gemma 3 4B** | `tea:gemma3-4b` | ~5GB | Full + bundled Gemma 3 4B Q8_0 model |
| **Phi-4 Mini** | `tea:phi4-mini` | ~5GB | Full + bundled Phi-4 mini Q8_0 model |

### Choosing a Variant

- **Base**: Use when you only need core TEA functionality without Prolog, Lua, or local LLMs
- **Full** (default): Use when you need Prolog reasoning or Lua scripting
- **LLM variants**: Use when you want self-contained offline LLM inference without external model downloads

### Using LLM Variants

The LLM variants come with bundled GGUF models and set `TEA_MODEL_PATH` automatically:

```bash
# Run with bundled Gemma 3 1B model
docker run --rm -v $(pwd):/work ghcr.io/fabceolin/tea:gemma3-1b run llm-agent.yaml

# Run with bundled Phi-4 mini model
docker run --rm -v $(pwd):/work ghcr.io/fabceolin/tea:phi4-mini run llm-agent.yaml
```

## Quick Install

### Linux / macOS

```bash
curl -fsSL https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.sh | bash
```

### Windows (PowerShell)

```powershell
irm https://raw.githubusercontent.com/fabceolin/the_edge_agent/main/scripts/install.ps1 | iex
```

### Manual Download

Download `tea.com` from the [latest release](https://github.com/fabceolin/the_edge_agent/releases/latest):

```bash
# Linux/macOS
curl -fsSL -o tea https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea.com
chmod +x tea
./tea --version
```

```powershell
# Windows
Invoke-WebRequest -Uri "https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea.com" -OutFile "tea.exe"
.\tea.exe --version
```

## Requirements

- **Docker**: Docker Desktop (macOS/Windows) or Docker Engine (Linux)
- **Architecture**: x86_64 or arm64

### Installing Docker

| Platform | Installation |
|----------|--------------|
| macOS | [Docker Desktop for Mac](https://docs.docker.com/desktop/install/mac-install/) |
| Windows | [Docker Desktop for Windows](https://docs.docker.com/desktop/install/windows-install/) |
| Linux | [Docker Engine](https://docs.docker.com/engine/install/) |

## Usage

The `tea` command works identically on all platforms:

```bash
# Run an agent
tea run agent.yaml --input '{"query": "Hello"}'

# Validate a workflow
tea validate workflow.yaml

# Check TEA version
tea --version

# Show help
tea --help
```

## Wrapper Commands

The wrapper provides additional commands not passed to the container:

| Command | Description |
|---------|-------------|
| `--wrapper-version` | Show wrapper binary version |
| `--docker-pull` | Pull/update the Docker image |
| `--docker-image` | Show current Docker image name |
| `--wrapper-help` | Show wrapper-specific help |

## Environment Variables

### Wrapper Configuration

| Variable | Description | Default |
|----------|-------------|---------|
| `TEA_IMAGE` | Docker image name | `ghcr.io/fabceolin/tea` |
| `TEA_VERSION` | Docker image tag | `latest` |

### API Keys (Passed to Container)

The following environment variables are automatically passed to the container:

- `OPENAI_API_KEY`
- `ANTHROPIC_API_KEY`
- `GOOGLE_API_KEY` / `GEMINI_API_KEY`
- `GROQ_API_KEY`
- `OLLAMA_HOST`
- `NEO4J_URI` / `NEO4J_USER` / `NEO4J_PASSWORD`
- `FIREBASE_PROJECT_ID`
- `GOOGLE_APPLICATION_CREDENTIALS`
- `OPIK_API_KEY` / `OPIK_PROJECT_NAME`
- Any variable starting with `TEA_*`

## Volume Mounts

The wrapper automatically mounts:

| Host Path | Container Path | Purpose |
|-----------|----------------|---------|
| Current directory | `/work` | Working directory for agent files |
| `~/.tea` | `/home/tea/.tea` | Persistent configuration |

## Technical Details

### Binary Specifications

| Metric | Value |
|--------|-------|
| Binary format | Actually Portable Executable (APE) |
| Binary size | ~1.1 MB |
| Supported platforms | Linux, macOS, Windows, FreeBSD, NetBSD, OpenBSD |
| Architectures | x86_64 (native), arm64 (via Docker) |

### Docker Image Specifications

| Metric | Base | Full | LLM Variants |
|--------|------|------|--------------|
| Base image | Ubuntu 24.04 | Ubuntu 24.04 | Ubuntu 24.04 |
| Image size | ~400 MB | ~772 MB | ~2-5 GB |
| Python version | 3.12 | 3.12 | 3.12 |
| SWI-Prolog | No | 9.3+ | 9.3+ |
| Lua runtime | No | LuaJIT | LuaJIT |
| llama-cpp | No | No | Yes |
| Bundled model | No | No | Yes |
| Architectures | amd64, arm64 | amd64, arm64 | amd64, arm64 |
| Startup time | <2 seconds | <3 seconds | <3 seconds |

### Image Tags

| Tag | Description |
|-----|-------------|
| `latest` | Latest stable release (full variant) |
| `base` | Minimal variant |
| `full` | Full features variant |
| `gemma3-1b` | Full + Gemma 3 1B model |
| `gemma3-4b` | Full + Gemma 3 4B model |
| `phi4-mini` | Full + Phi-4 mini model |
| `X.Y.Z` | Specific version (full variant) |
| `X.Y.Z-base` | Specific version (base variant) |
| `X.Y.Z-gemma3-1b` | Specific version with Gemma 3 1B |
| `sha-XXXXXX` | Specific commit |

## Troubleshooting

### Docker not found

```
Error: Docker command failed. Please ensure:
  1. Docker is installed
  2. Docker daemon is running
  3. You have permission to run Docker
```

**Solutions:**
- Install Docker from https://docs.docker.com/get-docker/
- Start Docker Desktop (macOS/Windows) or the Docker service (Linux)
- On Linux, add your user to the docker group: `sudo usermod -aG docker $USER`

### Permission denied

```
Got permission denied while trying to connect to the Docker daemon socket
```

**Solution:** Add your user to the docker group and log out/in:
```bash
sudo usermod -aG docker $USER
# Log out and back in
```

### Slow first run

The first run may be slow as it pulls the Docker image (~772 MB). Subsequent runs use the cached image.

```bash
# Pre-pull the image
tea --docker-pull
```

### Wrong architecture

If you get architecture-related errors, ensure Docker is configured for the correct platform:

```bash
# Check Docker platform
docker info | grep Architecture

# On Apple Silicon, ensure Rosetta is enabled for x86_64 images
```

## Direct Docker Usage

You can also run TEA directly with Docker without the wrapper:

```bash
# Using full variant (default)
docker run --rm \
    -v "$(pwd):/work" \
    -v "$HOME/.tea:/home/tea/.tea" \
    -w /work \
    -e OPENAI_API_KEY \
    ghcr.io/fabceolin/tea:latest \
    run agent.yaml --input '{"query": "test"}'

# Using base variant (smaller, no Prolog/Lua)
docker run --rm \
    -v "$(pwd):/work" \
    -e OPENAI_API_KEY \
    ghcr.io/fabceolin/tea:base \
    run simple-agent.yaml

# Using LLM variant (bundled model, no API keys needed)
docker run --rm \
    -v "$(pwd):/work" \
    ghcr.io/fabceolin/tea:gemma3-1b \
    run local-llm-agent.yaml
```

## Uninstallation

### Linux/macOS

```bash
rm ~/.local/bin/tea
rm -rf ~/.tea  # Optional: remove config
```

### Windows

```powershell
Remove-Item "$env:LOCALAPPDATA\tea\tea.exe"
Remove-Item -Recurse "$env:USERPROFILE\.tea"  # Optional: remove config
```

### Remove Docker Image

```bash
docker rmi ghcr.io/fabceolin/tea:latest
```
