# Docker Build Instructions

## Image Variants

TEA provides multiple Docker image variants:

| Variant | Dockerfile | Size | Description |
|---------|------------|------|-------------|
| `base` | `Dockerfile.base` | ~400MB | Minimal - Python 3.12 only |
| `full` | `Dockerfile.full` | ~772MB | All features - Prolog, Lua, Opik |
| `gemma3-1b` | `Dockerfile.gemma3-1b` | ~2GB | Full + Gemma 3 1B Q8_0 model |
| `gemma3-4b` | `Dockerfile.gemma3-4b` | ~5GB | Full + Gemma 3 4B Q8_0 model |
| `phi4-mini` | `Dockerfile.phi4-mini` | ~5GB | Full + Phi-4 mini Q8_0 model |

## Local Build (Single Architecture)

```bash
# Build base variant (minimal)
docker build -t tea:base -f docker/Dockerfile.base .

# Build full variant (all features)
docker build -t tea:full -f docker/Dockerfile.full .

# Build LLM variant (with bundled model - takes longer)
docker build -t tea:gemma3-1b -f docker/Dockerfile.gemma3-1b .

# Test the builds
docker run --rm tea:base --version
docker run --rm tea:full --version
docker run --rm tea:gemma3-1b --version
```

## Multi-Architecture Build (CI)

Multi-architecture builds require `docker buildx` with QEMU emulation:

```bash
# Create builder with multi-arch support
docker buildx create --name tea-builder --use --bootstrap

# Build and push full variant
docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t ghcr.io/fabceolin/tea:latest \
    -t ghcr.io/fabceolin/tea:full \
    -f docker/Dockerfile.full \
    --push \
    .

# Build and push base variant
docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t ghcr.io/fabceolin/tea:base \
    -f docker/Dockerfile.base \
    --push \
    .

# Build and push LLM variant (requires ~10GB disk space)
docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t ghcr.io/fabceolin/tea:gemma3-1b \
    -f docker/Dockerfile.gemma3-1b \
    --push \
    .
```

## Supported Platforms

| Platform | Status | Notes |
|----------|--------|-------|
| `linux/amd64` | Supported | Primary platform |
| `linux/arm64` | Supported | Tested in CI |

## Build Requirements

- Docker 20.10+ with BuildKit enabled
- For multi-arch: `docker-buildx-plugin` and QEMU user-static
- For LLM variants: ~10GB free disk space

## Image Verification

After building, verify the runtimes:

```bash
# Check TEA version (all variants)
docker run --rm tea:base --version
docker run --rm tea:full --version

# Verify Prolog runtime (full/LLM variants only)
docker run --rm --entrypoint python tea:full \
    -c "import janus_swi; print('Prolog: OK')"

# Verify Lua runtime (full/LLM variants only)
docker run --rm --entrypoint python tea:full \
    -c "import lupa; print('Lua: OK')"

# Verify bundled model (LLM variants only)
docker run --rm tea:gemma3-1b \
    python -c "import os; print(f'Model: {os.environ.get(\"TEA_MODEL_PATH\")}')"
```

## Size Information

| Variant | Target | Current |
|---------|--------|---------|
| base | <500MB | ~400MB |
| full | <1GB | ~772MB |
| gemma3-1b | <2.5GB | ~2GB |
| gemma3-4b | <6GB | ~5GB |
| phi4-mini | <6GB | ~5GB |

### Full Variant Components
- Base Ubuntu 24.04: ~80MB
- Python 3.12 + SWI-Prolog: ~150MB
- TEA venv with all extras: ~540MB

### LLM Variant Additional Components
- llama-cpp-python: ~50MB
- Bundled GGUF model: 1-4GB (varies by model)
