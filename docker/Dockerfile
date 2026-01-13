# TEA-DIST-001: Docker Full Image - All Dependencies (Default)
#
# This is the default Dockerfile that builds the full variant.
# For other variants, see:
#   - Dockerfile.base (minimal, ~400MB)
#   - Dockerfile.full (same as this file)
#   - Dockerfile.gemma3-1b (full + Gemma 3 1B model, ~2GB)
#   - Dockerfile.gemma3-4b (full + Gemma 3 4B model, ~5GB)
#   - Dockerfile.phi4-mini (full + Phi-4 mini model, ~5GB)
#
# Build: docker build -t tea:latest -f docker/Dockerfile .
# Run: docker run --rm -v $(pwd):/work tea:latest run agent.yaml
# Size: ~772MB

# =============================================================================
# Stage 1: Builder - Install build dependencies and compile packages
# =============================================================================
FROM ubuntu:24.04 AS builder

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3.12 \
    python3.12-venv \
    python3.12-dev \
    python3-pip \
    software-properties-common \
    build-essential \
    git \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install SWI-Prolog 9.3+ from PPA
# Required for janus-swi bindings
RUN add-apt-repository ppa:swi-prolog/stable -y \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
    swi-prolog \
    swi-prolog-nox \
    && rm -rf /var/lib/apt/lists/*

# Create virtual environment
RUN python3.12 -m venv /opt/tea-venv
ENV PATH="/opt/tea-venv/bin:$PATH"

# Upgrade pip
RUN pip install --no-cache-dir --upgrade pip wheel setuptools

# Copy Python source for installation
COPY python/ /src/python/
WORKDIR /src/python

# Install TEA with all extras including prolog and lua
# Note: janus-swi requires SWI-Prolog to be installed first
RUN pip install --no-cache-dir ".[all,prolog,lua,opik]"

# Clean up the venv to reduce size
RUN find /opt/tea-venv -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true \
    && find /opt/tea-venv -type f -name "*.pyc" -delete 2>/dev/null || true \
    && find /opt/tea-venv -type f -name "*.pyo" -delete 2>/dev/null || true \
    && rm -rf /opt/tea-venv/share/doc 2>/dev/null || true \
    && rm -rf /opt/tea-venv/share/man 2>/dev/null || true

# Verify installations
RUN python -c "import the_edge_agent; print(f'TEA version: {the_edge_agent.__version__}')"
RUN python -c "import janus_swi; print('janus-swi: OK')" || echo "janus-swi verification will be done at runtime"
RUN python -c "import lupa; print('lupa (Lua): OK')"

# =============================================================================
# Stage 2: Runtime - Minimal image for execution
# =============================================================================
FROM ubuntu:24.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies only (minimized package set)
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3.12 \
    software-properties-common \
    ca-certificates \
    && add-apt-repository ppa:swi-prolog/stable -y \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
    swi-prolog-nox \
    && apt-get purge -y software-properties-common \
    && apt-get autoremove -y \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /var/cache/apt/archives/* \
    && rm -rf /usr/share/doc/* \
    && rm -rf /usr/share/man/*

# Copy virtual environment from builder
COPY --from=builder /opt/tea-venv /opt/tea-venv
ENV PATH="/opt/tea-venv/bin:$PATH"

# Set Python environment
ENV PYTHONUNBUFFERED=1
ENV PYTHONDONTWRITEBYTECODE=1

# Create non-root user for security (AC: 26)
# Use a different UID to avoid conflicts with existing users
RUN useradd -m -s /bin/bash tea \
    && mkdir -p /home/tea/.tea \
    && chown -R tea:tea /home/tea

# Switch to non-root user
USER tea

# Set working directory for mounted volumes
WORKDIR /work

# Labels for container registry
LABEL org.opencontainers.image.source="https://github.com/fabceolin/the_edge_agent"
LABEL org.opencontainers.image.description="TEA Full - All features (Prolog, Lua, Opik)"
LABEL org.opencontainers.image.licenses="MIT"
LABEL tea.variant="full"

# Entrypoint is the tea CLI
ENTRYPOINT ["tea"]

# Default command shows help
CMD ["--help"]
