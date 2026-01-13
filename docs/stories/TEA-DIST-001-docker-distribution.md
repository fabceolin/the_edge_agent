# TEA-DIST-001: Docker Distribution with CLI Wrapper

## Status

**Draft**

---

## Story

**As a** TEA user,
**I want** a Docker image with a local wrapper script that behaves like a native CLI,
**so that** I can run TEA agents with Prolog and Lua runtimes without managing dependencies.

---

## Story Context

**Existing System Integration:**

- Integrates with: Python TEA CLI (`python/src/the_edge_agent/cli.py`)
- Technology: Docker for containerization, shell wrapper for UX
- Follows pattern: Containerized CLI tools (e.g., `act`, `dive`, `hadolint`)
- Touch points: CI/CD workflow (GitHub Actions), GitHub Container Registry

**Architecture:**

```
User runs:  tea run agent.yaml --input '{"query": "test"}'
                    │
                    ▼
            ┌─────────────────┐
            │   tea (wrapper) │  ← Shell script
            └────────┬────────┘
                     │
                     ▼
     docker run -v $(pwd):/work ghcr.io/[org]/tea:latest \
         run agent.yaml --input '{"query": "test"}'
                     │
                     ▼
            ┌─────────────────┐
            │  Docker Image   │  ← Ubuntu 24.04 + Python 3.12
            │  SWI-Prolog 9.3 │     + SWI-Prolog + Lua
            │  Full TEA       │
            └─────────────────┘
```

---

## Acceptance Criteria

### Docker Image Requirements

1. **Base Image**: Ubuntu 24.04 LTS as base
2. **Python Runtime**: Python 3.12 with full TEA installation
3. **Prolog Runtime**: SWI-Prolog 9.3+ with janus-swi bindings functional
4. **Lua Runtime**: LuaJIT via lupa functional
5. **Full Actions**: All TEA actions included (LLM, Neo4j, data, etc.)
6. **Multi-arch**: Support for `linux/amd64` and `linux/arm64`

### Wrapper Script Requirements

7. **Native CLI Feel**: `tea <command> <args>` works like a local binary
8. **Working Directory**: Current directory mounted and accessible
9. **Config Persistence**: `~/.tea` or `~/.config/tea` persisted between runs
10. **Environment Passthrough**: `TEA_*`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, etc. passed to container
11. **Interactive Mode**: TTY allocation for interactive HITL workflows
12. **Exit Codes**: Container exit codes propagated to wrapper

### CI/CD Requirements

13. **GitHub Actions Workflow**: Automated build on push to `main` and version tags
14. **Container Registry**: Push to GitHub Container Registry (`ghcr.io`)
15. **Version Tags**: `:latest`, `:vX.Y.Z`, and `:sha-XXXXXX` tags
16. **Multi-arch Build**: Use `docker buildx` for arm64 support

### Quality Requirements

17. **Image Size**: Target <1GB (document actual size)
18. **Startup Time**: Container ready within 3 seconds
19. **Smoke Tests**: Basic agent execution passes in CI
20. **Security**: Non-root user inside container, no unnecessary capabilities

---

## Tasks / Subtasks

### Phase 1: Dockerfile

- [ ] **Task 1**: Create multi-stage Dockerfile (AC: 1, 2, 3, 4, 5)
  - [ ] Create `docker/Dockerfile` with Ubuntu 24.04 base
  - [ ] Install Python 3.12 and pip
  - [ ] Install SWI-Prolog 9.3+ from PPA or build
  - [ ] Install TEA with all extras: `pip install .[all,prolog,lua]`
  - [ ] Verify janus-swi links correctly to installed SWI-Prolog
  - [ ] Create non-root user for runtime (AC: 20)
  - [ ] Set entrypoint to `tea` CLI

- [ ] **Task 2**: Optimize image size (AC: 17)
  - [ ] Use multi-stage build (builder + runtime)
  - [ ] Remove build dependencies in final stage
  - [ ] Clean apt cache and pip cache
  - [ ] Consider slim variants if available

### Phase 2: Wrapper Script

- [ ] **Task 3**: Create wrapper script (AC: 7, 8, 9, 10, 11, 12)
  - [ ] Create `docker/tea-wrapper.sh`
  - [ ] Mount current directory as `/work`
  - [ ] Mount config directory (`~/.tea` or `~/.config/tea`)
  - [ ] Pass through environment variables (allowlist approach)
  - [ ] Handle TTY detection for interactive mode
  - [ ] Propagate exit codes correctly
  - [ ] Support `TEA_IMAGE` and `TEA_VERSION` overrides
  - [ ] Add `--docker-pull` flag to force image update

- [ ] **Task 4**: Installation script (AC: 7)
  - [ ] Create `install.sh` that downloads wrapper to `/usr/local/bin/tea`
  - [ ] Support `TEA_INSTALL_DIR` override
  - [ ] Verify Docker is available
  - [ ] Pull image on first install

### Phase 3: CI/CD Integration

- [ ] **Task 5**: GitHub Actions workflow (AC: 13, 14, 15, 16)
  - [ ] Create `.github/workflows/docker-build.yaml`
  - [ ] Set up QEMU and buildx for multi-arch
  - [ ] Build and push on `main` branch (`:latest`, `:sha-xxx`)
  - [ ] Build and push on version tags (`:vX.Y.Z`)
  - [ ] Cache Docker layers for faster builds
  - [ ] Login to ghcr.io with `GITHUB_TOKEN`

- [ ] **Task 6**: Smoke tests in CI (AC: 19)
  - [ ] Run container and execute simple agent
  - [ ] Test Prolog node execution
  - [ ] Test Lua node execution
  - [ ] Verify volume mounts work correctly
  - [ ] Test on both amd64 and arm64 runners (if available)

### Phase 4: Documentation

- [ ] **Task 7**: Documentation (AC: 17, 18)
  - [ ] Add "Docker Installation" section to README
  - [ ] Document wrapper script usage
  - [ ] Document environment variable passthrough
  - [ ] Document volume mount behavior
  - [ ] Record image size and startup time
  - [ ] Add troubleshooting section

---

## Dev Notes

### Dockerfile Structure

```dockerfile
# docker/Dockerfile
FROM ubuntu:24.04 AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    python3.12 python3.12-venv python3-pip \
    software-properties-common \
    && rm -rf /var/lib/apt/lists/*

# Install SWI-Prolog 9.3+ from PPA
RUN add-apt-repository ppa:swi-prolog/stable \
    && apt-get update \
    && apt-get install -y swi-prolog \
    && rm -rf /var/lib/apt/lists/*

# Create venv and install TEA
RUN python3.12 -m venv /opt/tea-venv
ENV PATH="/opt/tea-venv/bin:$PATH"
COPY python/ /src/python/
RUN pip install --no-cache-dir /src/python[all,prolog,lua]

# Runtime stage
FROM ubuntu:24.04

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    python3.12 \
    swi-prolog \
    && rm -rf /var/lib/apt/lists/*

# Copy venv from builder
COPY --from=builder /opt/tea-venv /opt/tea-venv
ENV PATH="/opt/tea-venv/bin:$PATH"

# Create non-root user
RUN useradd -m -s /bin/bash tea
USER tea
WORKDIR /work

ENTRYPOINT ["tea"]
CMD ["--help"]
```

### Wrapper Script Structure

```bash
#!/usr/bin/env bash
# tea - Docker wrapper for The Edge Agent
set -euo pipefail

TEA_IMAGE="${TEA_IMAGE:-ghcr.io/[org]/tea}"
TEA_VERSION="${TEA_VERSION:-latest}"
TEA_CONFIG_DIR="${TEA_CONFIG_DIR:-${HOME}/.tea}"

# Create config dir if not exists
mkdir -p "${TEA_CONFIG_DIR}"

# Build docker run command
DOCKER_ARGS=(
    --rm
    -v "$(pwd):/work"
    -v "${TEA_CONFIG_DIR}:/home/tea/.tea"
    -w /work
)

# TTY handling
if [ -t 0 ] && [ -t 1 ]; then
    DOCKER_ARGS+=(-it)
fi

# Environment passthrough (allowlist)
ENV_VARS=(
    OPENAI_API_KEY
    ANTHROPIC_API_KEY
    GOOGLE_API_KEY
    GEMINI_API_KEY
    GROQ_API_KEY
    OLLAMA_HOST
    NEO4J_URI
    NEO4J_USER
    NEO4J_PASSWORD
    FIREBASE_PROJECT_ID
    GOOGLE_APPLICATION_CREDENTIALS
)

for var in "${ENV_VARS[@]}"; do
    if [ -n "${!var:-}" ]; then
        DOCKER_ARGS+=(-e "${var}")
    fi
done

# Pass through all TEA_* variables
for var in $(compgen -e | grep '^TEA_'); do
    DOCKER_ARGS+=(-e "${var}")
done

# Handle special commands
case "${1:-}" in
    --docker-pull)
        docker pull "${TEA_IMAGE}:${TEA_VERSION}"
        exit 0
        ;;
    --docker-version)
        echo "TEA Docker wrapper"
        echo "Image: ${TEA_IMAGE}:${TEA_VERSION}"
        docker run --rm "${TEA_IMAGE}:${TEA_VERSION}" --version
        exit 0
        ;;
esac

# Run container
exec docker run "${DOCKER_ARGS[@]}" "${TEA_IMAGE}:${TEA_VERSION}" "$@"
```

### GitHub Actions Workflow

```yaml
# .github/workflows/docker-build.yaml
name: Docker Build

on:
  push:
    branches: [main]
    tags: ['v*']
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}/tea

jobs:
  build:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
      - uses: actions/checkout@v4

      - name: Set up QEMU
        uses: docker/setup-qemu-action@v3

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to GHCR
        if: github.event_name != 'pull_request'
        uses: docker/login-action@v3
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=semver,pattern={{version}}
            type=semver,pattern={{major}}.{{minor}}
            type=sha

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          file: docker/Dockerfile
          platforms: linux/amd64,linux/arm64
          push: ${{ github.event_name != 'pull_request' }}
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  smoke-test:
    needs: build
    runs-on: ubuntu-latest
    if: github.event_name != 'pull_request'

    steps:
      - uses: actions/checkout@v4

      - name: Run smoke tests
        run: |
          docker run --rm ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:sha-${{ github.sha }} --version
          # Add more smoke tests here
```

### Volume Mount Considerations

| Host Path | Container Path | Purpose |
|-----------|---------------|---------|
| `$(pwd)` | `/work` | Agent YAML files, outputs |
| `~/.tea` | `/home/tea/.tea` | Config, checkpoints, cache |
| `~/.config/gcloud` | `/home/tea/.config/gcloud` | GCP credentials (optional) |

### Environment Variables

**Automatically passed through:**
- `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY`
- `GROQ_API_KEY`, `OLLAMA_HOST`
- `NEO4J_URI`, `NEO4J_USER`, `NEO4J_PASSWORD`
- `FIREBASE_PROJECT_ID`, `GOOGLE_APPLICATION_CREDENTIALS`
- All `TEA_*` variables

---

## Testing

### Test File Location

- `docker/test/smoke_test.sh` (new)
- CI workflow tests in `.github/workflows/docker-build.yaml`

### Test Cases

1. **Version check**: `tea --version` returns correctly
2. **Simple agent**: Execute basic YAML agent
3. **Prolog test**: Run agent with Prolog node
4. **Lua test**: Run agent with Lua node
5. **File output**: Verify files written to mounted volume
6. **Interactive mode**: Test TTY allocation (manual)

---

## Risk Assessment

**Primary Risk:** Volume permissions (host user vs container user)

**Mitigation:**
- Use `--user $(id -u):$(id -g)` in wrapper if needed
- Document permission handling

**Rollback:**
- Docker approach is additive - doesn't affect pip installation
- Users can always use `pip install the-edge-agent`

---

## Definition of Done

- [ ] Dockerfile builds successfully
- [ ] Image pushed to ghcr.io on main/tags
- [ ] Multi-arch (amd64 + arm64) builds work
- [ ] Wrapper script tested on Linux and macOS
- [ ] Prolog and Lua runtimes functional in container
- [ ] Smoke tests pass in CI
- [ ] Documentation updated with installation instructions
- [ ] Image size and startup time documented

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-12 | 0.1 | Initial story creation (AppImage) | Sarah (PO) |
| 2025-01-12 | 0.2 | Rewrite for Docker + wrapper approach, Ubuntu 24.04 | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

*To be filled during implementation*

### Debug Log References

*To be filled during implementation*

### Completion Notes List

*To be filled during implementation*

### File List

*To be filled during implementation*

---

## QA Results

*To be filled after QA review*
