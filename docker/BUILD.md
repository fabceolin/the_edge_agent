# Docker Build Instructions

TEA provides Docker images for both Python and Rust implementations.

## Python Image Variants

| Variant | Dockerfile | Size | Description |
|---------|------------|------|-------------|
| `base` | `Dockerfile.base` | ~400MB | Minimal - Python 3.12 only |
| `full` | `Dockerfile.full` | ~772MB | All features - Prolog, Lua, Opik |
| `gemma3-1b` | `Dockerfile.gemma3-1b` | ~2GB | Full + Gemma 3 1B Q8_0 model |
| `gemma3-4b` | `Dockerfile.gemma3-4b` | ~5GB | Full + Gemma 3 4B Q8_0 model |
| `gemma3n-e4b` | `Dockerfile.gemma3n-e4b` | ~8.5GB | Full + Gemma 3N E4B model |
| `phi4-mini` | `Dockerfile.phi4-mini` | ~3.5GB | Full + Phi-4 mini Q8_0 model |
| `qwen3-8b` | `Dockerfile.qwen3-8b` | ~6GB | Full + Qwen 3 8B Q4_K_M model |

## Rust Image Variants

| Variant | Dockerfile | Size | Description |
|---------|------------|------|-------------|
| `rust-base` | `Dockerfile.rust-base` | ~50MB | Minimal - default features only |
| `rust-full` | `Dockerfile.rust-full` | ~100MB | All features - Scryer Prolog, DuckDB |
| `rust-gemma3-1b` | `Dockerfile.rust-gemma3-1b` | ~1.6GB | Full + Gemma 3 1B model |
| `rust-gemma3-4b` | `Dockerfile.rust-gemma3-4b` | ~4.5GB | Full + Gemma 3 4B model |
| `rust-gemma3n-e4b` | `Dockerfile.rust-gemma3n-e4b` | ~8.5GB | Full + Gemma 3N E4B model |
| `rust-phi4-mini` | `Dockerfile.rust-phi4-mini` | ~3.5GB | Full + Phi-4 mini model |
| `rust-qwen3-8b` | `Dockerfile.rust-qwen3-8b` | ~5.5GB | Full + Qwen 3 8B model |

## Size Comparison: Python vs Rust

| Variant Type | Python | Rust | Savings |
|--------------|--------|------|---------|
| Base | ~400MB | ~50MB | 87% |
| Full | ~772MB | ~100MB | 87% |
| Gemma 3 1B | ~2GB | ~1.6GB | 20% |

## Local Build

### Python Variants

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
```

### Rust Variants

```bash
# Build Rust base variant (~2-3 minutes)
docker build -t tea:rust-base -f docker/Dockerfile.rust-base .

# Build Rust full variant (~5 minutes)
docker build -t tea:rust-full -f docker/Dockerfile.rust-full .

# Build Rust LLM variant (requires model download)
docker build -t tea:rust-gemma3-1b -f docker/Dockerfile.rust-gemma3-1b .

# Test the builds
docker run --rm tea:rust-base --version
docker run --rm tea:rust-full --version
```

## Multi-Architecture Build (CI)

Multi-architecture builds require `docker buildx` with QEMU emulation:

```bash
# Create builder with multi-arch support
docker buildx create --name tea-builder --use --bootstrap

# Build and push Python full variant
docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t ghcr.io/fabceolin/tea:latest \
    -t ghcr.io/fabceolin/tea:full \
    -f docker/Dockerfile.full \
    --push \
    .

# Build and push Rust full variant
docker buildx build \
    --platform linux/amd64,linux/arm64 \
    -t ghcr.io/fabceolin/tea:rust-latest \
    -t ghcr.io/fabceolin/tea:rust-full \
    -f docker/Dockerfile.rust-full \
    --push \
    .
```

## Supported Platforms

| Platform | Python | Rust | Notes |
|----------|--------|------|-------|
| `linux/amd64` | ✅ | ✅ | Primary platform |
| `linux/arm64` | ⚠️ | ✅ | Python full requires SWI-Prolog PPA arm64 support |

## Build Requirements

- Docker 20.10+ with BuildKit enabled
- For multi-arch: `docker-buildx-plugin` and QEMU user-static
- For LLM variants: ~10GB free disk space
- For Rust: ~5 minutes compile time (cached after first build)

## Image Verification

### Python Images

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

### Rust Images

```bash
# Check TEA version (all variants)
docker run --rm tea:rust-base --version
docker run --rm tea:rust-full --version

# Verify Lua scripting works (create test agent)
cat > /tmp/lua-test.yaml << 'EOF'
name: lua-test
state_schema:
  input: str
  output: str
nodes:
  - name: process
    run: |
      return {output = "Hello from Lua: " .. state.input}
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
EOF

docker run --rm -v /tmp:/work tea:rust-full run /work/lua-test.yaml --initial '{"input": "test"}'

# Verify bundled model (LLM variants only)
docker run --rm tea:rust-gemma3-1b env | grep TEA_MODEL_PATH
docker run --rm --entrypoint sh tea:rust-gemma3-1b -c 'ls -lh $TEA_MODEL_PATH'
```

## Registry Tags

### Python Tags

| Tag | Description |
|-----|-------------|
| `tea:latest` | Python full variant (default) |
| `tea:base` | Python base variant |
| `tea:full` | Python full variant |
| `tea:gemma3-1b` | Python + Gemma 3 1B model |
| `tea:1.2.3` | Versioned Python full |
| `tea:1.2.3-base` | Versioned Python base |

### Rust Tags

| Tag | Description |
|-----|-------------|
| `tea:rust-latest` | Rust full variant |
| `tea:rust-base` | Rust base variant |
| `tea:rust-full` | Rust full variant |
| `tea:rust-gemma3-1b` | Rust + Gemma 3 1B model |
| `tea:1.2.3-rust` | Versioned Rust full |
| `tea:1.2.3-rust-base` | Versioned Rust base |

## Rust Features Included

### rust-base (Default Features)
- memory, trace, data, llm, agent, reflection, reasoning, planning

### rust-full (All Features except graph)
- All default features plus: web, rag, code, a2a, ltm-duckdb, scryer (pure Rust Prolog)
- Note: `graph` (Cozo) excluded due to build compatibility issues

### rust-*-llm (LLM Variants)
- All rust-full features plus: llm-local (llama.cpp bindings)
- Bundled GGUF model with `TEA_MODEL_PATH` and `TEA_MODEL_NAME` set
