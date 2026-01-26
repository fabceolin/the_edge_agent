# Docker-Based Installation and Model Selection for The Edge Agent

**Fabricio Ceolin**

*The Edge Agent Project*

https://www.linkedin.com/in/fabceolin/

---

## Abstract

The Edge Agent (TEA) provides Docker-based distribution for consistent deployment across platforms. This article explains how to install TEA using Docker, select from multiple bundled LLM model variants, and use the cross-platform `tea.com` wrapper for a native CLI experience. We cover available image variants, environment variable configuration, and practical usage patterns for both development and production environments.

**Keywords:** Docker, LLM, Model Selection, Installation, Cross-Platform

---

## 1. Introduction

Running AI agents requires consistent environments and often large language model dependencies. TEA addresses this through Docker-based distribution, providing:

- **Consistent environments** - Same behavior across Linux, macOS, and Windows
- **Bundled models** - Self-contained images with pre-downloaded LLM weights
- **Zero configuration** - Works out of the box with sensible defaults
- **Cross-platform wrapper** - Native CLI experience via `tea.com`

This guide covers the complete workflow from installation to running agents with your preferred model.

## 2. Available Docker Images

TEA provides multiple Docker image variants optimized for different use cases:

| Image Tag | Size | Description | Use Case |
|-----------|------|-------------|----------|
| `latest` / `full` | ~772MB | All features (Prolog, Lua, Opik) | General development |
| `base` | ~400MB | Minimal core dependencies | Lightweight deployments |
| `gemma3-1b` | ~1.5GB | Full + Gemma 3 1B model | Edge devices, fast inference |
| `gemma3-4b` | ~4GB | Full + Gemma 3 4B model | Balanced performance |
| `gemma3n-e4b` | ~8GB | Full + Gemma 3n E4B model | Higher quality responses |
| `phi4-mini` | ~3GB | Full + Phi-4 mini model | Microsoft Phi-4 capabilities |

All LLM images include the complete TEA runtime plus a pre-downloaded GGUF model from the Ollama registry, enabling fully offline operation.

## 3. Installation Methods

### 3.1 Method 1: Cross-Platform Wrapper (Recommended)

The `tea.com` wrapper provides a native CLI experience while internally using Docker.

**Download:**

```bash
# Linux/macOS
curl -L -o tea.com https://github.com/fabceolin/the_edge_agent/releases/latest/download/tea.com
chmod +x tea.com

# Optional: Move to PATH
sudo mv tea.com /usr/local/bin/tea
```

**Windows:**

Download `tea.com` from the releases page and rename to `tea.exe`.

**Verify installation:**

```bash
./tea.com --version
```

### 3.2 Method 2: Direct Docker

Pull the image directly:

```bash
# Default (full) image
docker pull ghcr.io/fabceolin/tea:latest

# Specific model variant
docker pull ghcr.io/fabceolin/tea:gemma3-4b
```

## 4. Model Selection

### 4.1 Using the Wrapper

The wrapper uses two environment variables for image selection:

| Variable | Description | Default |
|----------|-------------|---------|
| `TEA_IMAGE` | Docker image name | `ghcr.io/fabceolin/tea` |
| `TEA_VERSION` | Image tag | `latest` |

**Examples:**

```bash
# Use default image
tea.com run agent.yaml

# Use Gemma 3 1B (fast, lightweight)
TEA_VERSION=gemma3-1b tea.com run agent.yaml

# Use Gemma 3 4B (balanced)
TEA_VERSION=gemma3-4b tea.com run agent.yaml

# Use Gemma 3n E4B (higher quality)
TEA_VERSION=gemma3n-e4b tea.com run agent.yaml

# Use Phi-4 mini
TEA_VERSION=phi4-mini tea.com run agent.yaml

# Use minimal base image (no LLM)
TEA_VERSION=base tea.com run agent.yaml
```

**Persistent configuration (bash/zsh):**

```bash
# Add to ~/.bashrc or ~/.zshrc
export TEA_VERSION=gemma3-4b
```

### 4.2 Using Direct Docker

```bash
# Run with specific model
docker run --rm -it \
  -v "$(pwd):/work" \
  -w /work \
  ghcr.io/fabceolin/tea:gemma3-4b \
  run agent.yaml

# With API keys for external LLMs
docker run --rm -it \
  -v "$(pwd):/work" \
  -w /work \
  -e OPENAI_API_KEY \
  -e ANTHROPIC_API_KEY \
  ghcr.io/fabceolin/tea:latest \
  run agent.yaml
```

## 5. Using Bundled Models in Agents

When using an LLM image, the model path is pre-configured via the `TEA_MODEL_PATH` environment variable. Reference it in your agent YAML:

```yaml
name: local-llm-agent
state_schema:
  query: str
  response: str

settings:
  llm:
    provider: llamacpp
    model_path: ${TEA_MODEL_PATH}
    temperature: 0.7

nodes:
  - name: generate
    run: |
      from the_edge_agent.actions import llm_complete
      response = llm_complete(state["query"])
      return {"response": response}

edges:
  - from: __start__
    to: generate
  - from: generate
    to: __end__
```

Run with a bundled model:

```bash
TEA_VERSION=gemma3-4b tea.com run agent.yaml --input '{"query": "Explain quantum computing"}'
```

## 6. Wrapper Commands

The `tea.com` wrapper provides several utility commands:

```bash
# Show wrapper version
tea.com --wrapper-version

# Show current Docker image
tea.com --docker-image

# Pull latest image for current TEA_VERSION
tea.com --docker-pull

# Show wrapper help
tea.com --wrapper-help
```

**Pull a specific model:**

```bash
TEA_VERSION=gemma3n-e4b tea.com --docker-pull
```

## 7. Environment Variables

The wrapper automatically passes these environment variables to the container:

| Variable | Purpose |
|----------|---------|
| `OPENAI_API_KEY` | OpenAI API access |
| `ANTHROPIC_API_KEY` | Anthropic Claude access |
| `GOOGLE_API_KEY` | Google AI access |
| `GEMINI_API_KEY` | Google Gemini access |
| `GROQ_API_KEY` | Groq inference access |
| `OLLAMA_HOST` | Custom Ollama server |
| `OPIK_API_KEY` | Comet Opik tracing |
| `TEA_*` | All TEA-prefixed variables |

## 8. Platform-Specific Notes

### 8.1 Windows

```powershell
# PowerShell
$env:TEA_VERSION = "gemma3-4b"
.\tea.exe run agent.yaml

# Command Prompt
set TEA_VERSION=gemma3-4b
tea.exe run agent.yaml
```

### 8.2 macOS with Apple Silicon

All images support ARM64 architecture natively:

```bash
TEA_VERSION=gemma3-1b tea.com run agent.yaml
```

### 8.3 Linux with GPU

For GPU acceleration with bundled models, mount the NVIDIA runtime:

```bash
docker run --rm -it --gpus all \
  -v "$(pwd):/work" \
  -w /work \
  ghcr.io/fabceolin/tea:gemma3-4b \
  run agent.yaml
```

## 9. Choosing the Right Model

| Model | Best For | Memory | Speed |
|-------|----------|--------|-------|
| `gemma3-1b` | Edge devices, rapid prototyping | ~2GB | Fast |
| `gemma3-4b` | General use, good balance | ~5GB | Medium |
| `gemma3n-e4b` | Complex reasoning, quality focus | ~10GB | Slower |
| `phi4-mini` | Code generation, technical tasks | ~4GB | Medium |

:::{tip}
Start with `gemma3-1b` for development iteration, then switch to larger models for production quality.
:::

## 10. Troubleshooting

### Docker Not Found

```
Error: Docker command failed. Please ensure:
  1. Docker is installed
  2. Docker daemon is running
  3. You have permission to run Docker
```

**Solution (Linux):**

```bash
sudo usermod -aG docker $USER
# Log out and back in
```

### Image Not Found

```bash
# Pull the image first
TEA_VERSION=gemma3-4b tea.com --docker-pull
```

### Out of Memory

Switch to a smaller model:

```bash
TEA_VERSION=gemma3-1b tea.com run agent.yaml
```

## 11. Conclusion

TEA's Docker-based distribution simplifies deployment across platforms while providing flexibility in model selection. Key takeaways:

1. Use `TEA_VERSION` to select model variants
2. The `tea.com` wrapper provides native CLI experience
3. Bundled models enable fully offline operation
4. Start small (`gemma3-1b`) and scale up as needed

For production deployments, consider the trade-offs between model size, inference speed, and response quality when selecting your image variant.

## 12. References

- [The Edge Agent GitHub](https://github.com/fabceolin/the_edge_agent)
- [Docker Installation Guide](https://docs.docker.com/get-docker/)
- [Ollama Model Library](https://ollama.com/library)
- [Cosmopolitan Libc](https://github.com/jart/cosmopolitan) - Powers the cross-platform wrapper
