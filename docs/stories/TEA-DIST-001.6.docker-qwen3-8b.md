# TEA-DIST-001.6: Docker LLM Image - Qwen 3 8B (Python + Rust)

## Status

**Ready for Review**

---

## Story

**As a** TEA user running offline LLM workflows,
**I want** Docker images (Python and Rust) with the bundled Qwen 3 8B model,
**so that** I can run self-contained LLM inference in either runtime without external model downloads.

---

## Story Context

**Existing System Integration:**

- Integrates with: TEA Docker distribution system (`docker/Dockerfile.*`)
- Technology: Docker multi-stage builds, Ollama Registry blob downloads, llama-cpp-python (Python), llm-local feature (Rust)
- Follows pattern: Existing LLM Dockerfiles (`Dockerfile.phi4-mini`, `Dockerfile.rust-phi4-mini`)
- Touch points: `docker/Dockerfile.qwen3-8b`, `docker/Dockerfile.rust-qwen3-8b`, `docker/BUILD.md`

**Model Details:**

| Property | Value |
|----------|-------|
| Model | Qwen 3 8B (qwen3:8b) |
| Source | Ollama Registry |
| Parameters | 8.19B |
| Quantization | Q4_K_M |
| Size | ~5.2GB |
| Blob SHA256 | `a3de86cd1c132c822487ededd47a324c50491393e6565cd14bafa40d0b8e686f` |
| Reference | https://ollama.com/library/qwen3:8b |

**Deliverables:**

| Variant | Dockerfile | Image Tag | Base |
|---------|------------|-----------|------|
| Python | `docker/Dockerfile.qwen3-8b` | `tea:qwen3-8b` | Ubuntu 24.04 + Python 3.12 |
| Rust | `docker/Dockerfile.rust-qwen3-8b` | `tea:rust-qwen3-8b` | Debian Trixie-slim + Rust binary |

---

## Acceptance Criteria

### Python Variant (Dockerfile.qwen3-8b)

1. Dockerfile `docker/Dockerfile.qwen3-8b` exists and follows `Dockerfile.phi4-mini` pattern
2. Image builds successfully with `docker build -t tea:qwen3-8b -f docker/Dockerfile.qwen3-8b .`
3. Qwen 3 8B model is bundled at `/opt/tea-models/qwen3-8b.gguf`
4. Environment variables set: `TEA_MODEL_PATH`, `TEA_MODEL_NAME`
5. Labels follow convention: `tea.variant=qwen3-8b`, `tea.model=qwen3-8b`, `tea.model.size=5.2GB`

### Rust Variant (Dockerfile.rust-qwen3-8b)

6. Dockerfile `docker/Dockerfile.rust-qwen3-8b` exists and follows `Dockerfile.rust-phi4-mini` pattern
7. Image builds successfully with `docker build -t tea:rust-qwen3-8b -f docker/Dockerfile.rust-qwen3-8b .`
8. Rust binary compiled with `llm-local` feature for embedded inference
9. Qwen 3 8B model is bundled at `/opt/tea-models/qwen3-8b.gguf`
10. Environment variables set: `TEA_MODEL_PATH`, `TEA_MODEL_NAME`
11. Labels include: `tea.variant=rust-qwen3-8b`, `tea.runtime=rust`, `tea.model=qwen3-8b`

### Integration Requirements

12. Existing Docker images (gemma3-4b, phi4-mini, rust-phi4-mini, etc.) continue to work unchanged
13. Both Dockerfiles follow existing multi-stage build patterns exactly
14. Model downloads from correct Ollama Registry blob URL

### Quality Requirements

15. Python image size ~6GB (772MB base + 5.2GB model)
16. Rust image size ~5.5GB (smaller base + 5.2GB model)
17. Non-root user (`tea`) for security in both variants

---

## Technical Notes

- **Blob Download URL:** `https://registry.ollama.ai/v2/library/qwen3/blobs/sha256:a3de86cd1c132c822487ededd47a324c50491393e6565cd14bafa40d0b8e686f`
- **Python Base Pattern:** Copy from `Dockerfile.phi4-mini`
- **Rust Base Pattern:** Copy from `Dockerfile.rust-phi4-mini`
- **Rust Features:** `memory,trace,data,web,rag,llm,code,agent,reflection,reasoning,planning,a2a,ltm-duckdb,scryer,llm-local`

---

## Tasks / Subtasks

### Python Variant

- [x] Create `docker/Dockerfile.qwen3-8b` (AC: 1, 2, 3, 4, 5, 14, 15, 17)
  - [x] Copy `Dockerfile.phi4-mini` as base template
  - [x] Update header comments with qwen3-8b info and reference URL
  - [x] Update model download URL to qwen3 blob SHA
  - [x] Update model filename to `qwen3-8b.gguf`
  - [x] Update `TEA_MODEL_PATH` to `/opt/tea-models/qwen3-8b.gguf`
  - [x] Update `TEA_MODEL_NAME` to `qwen3-8b`
  - [x] Update Docker labels (variant, model, size=5.2GB)
  - [x] Update image description label

### Rust Variant

- [x] Create `docker/Dockerfile.rust-qwen3-8b` (AC: 6, 7, 8, 9, 10, 11, 14, 16, 17)
  - [x] Copy `Dockerfile.rust-phi4-mini` as base template
  - [x] Update header comments with qwen3-8b info and reference URL
  - [x] Verify cargo build includes `llm-local` feature
  - [x] Update model download URL to qwen3 blob SHA
  - [x] Update model filename to `qwen3-8b.gguf`
  - [x] Update `TEA_MODEL_PATH` to `/opt/tea-models/qwen3-8b.gguf`
  - [x] Update `TEA_MODEL_NAME` to `qwen3-8b`
  - [x] Update Docker labels (variant=rust-qwen3-8b, runtime=rust, model, size=5.2GB)
  - [x] Update image description label

### Documentation

- [x] Update `docker/BUILD.md` variant list (AC: 12)
  - [x] Add `tea:qwen3-8b` to Python variant table
  - [x] Add `tea:rust-qwen3-8b` to Rust variant table

### Testing

- [x] Test Python build locally (AC: 2, 15)
  - [x] Run `docker build -t tea:qwen3-8b -f docker/Dockerfile.qwen3-8b .`
  - [x] Verify image size is approximately 6GB (actual: 6.03GB)
  - [x] Verify model file exists at `/opt/tea-models/qwen3-8b.gguf` (4.9G)

- [x] Test Rust build locally (AC: 7, 16)
  - [x] Run `docker build -t tea:rust-qwen3-8b -f docker/Dockerfile.rust-qwen3-8b .`
  - [x] Verify image size is approximately 5.5GB (actual: 5.33GB)
  - [x] Verify model file exists at `/opt/tea-models/qwen3-8b.gguf` (4.9G)
  - [x] Verify `tea --version` works in container (tea 0.9.72)

---

## Dev Notes

### Python Dockerfile Key Changes (from phi4-mini)

```dockerfile
# Header comment
# TEA-DIST-001: Docker LLM Image - Qwen 3 8B
# Model: Alibaba/Qwen3-8B
# Source: Ollama Registry (https://ollama.com/library/qwen3:8b)

# Model download (line ~107-111)
RUN mkdir -p /opt/tea-models && \
    curl -L -o /opt/tea-models/qwen3-8b.gguf \
    "https://registry.ollama.ai/v2/library/qwen3/blobs/sha256:a3de86cd1c132c822487ededd47a324c50491393e6565cd14bafa40d0b8e686f" && \
    ls -lh /opt/tea-models/ && \
    test -f /opt/tea-models/qwen3-8b.gguf

# Environment (line ~114-117)
ENV TEA_MODEL_PATH="/opt/tea-models/qwen3-8b.gguf"
ENV TEA_MODEL_NAME="qwen3-8b"

# Labels (line ~131-137)
LABEL org.opencontainers.image.description="TEA Qwen3-8B - Full features with bundled Qwen 3 8B model"
LABEL tea.variant="qwen3-8b"
LABEL tea.model="qwen3-8b"
LABEL tea.model.size="5.2GB"
```

### Rust Dockerfile Key Changes (from rust-phi4-mini)

```dockerfile
# Header comment
# TEA-DIST-002: Docker Rust LLM Image - Qwen 3 8B
# Model: Alibaba/Qwen3-8B
# Source: Ollama Registry (https://ollama.com/library/qwen3:8b)

# Model download (line ~58-62)
RUN mkdir -p /opt/tea-models && \
    curl -L -o /opt/tea-models/qwen3-8b.gguf \
    "https://registry.ollama.ai/v2/library/qwen3/blobs/sha256:a3de86cd1c132c822487ededd47a324c50491393e6565cd14bafa40d0b8e686f" && \
    ls -lh /opt/tea-models/ && \
    test -f /opt/tea-models/qwen3-8b.gguf

# Environment (line ~68-69)
ENV TEA_MODEL_PATH="/opt/tea-models/qwen3-8b.gguf"
ENV TEA_MODEL_NAME="qwen3-8b"

# Labels (line ~80-87)
LABEL org.opencontainers.image.description="TEA Rust Qwen3-8B - Full features with bundled Qwen 3 8B model"
LABEL tea.variant="rust-qwen3-8b"
LABEL tea.runtime="rust"
LABEL tea.model="qwen3-8b"
LABEL tea.model.size="5.2GB"
```

### Testing Commands

```bash
# Python variant
docker build -t tea:qwen3-8b -f docker/Dockerfile.qwen3-8b .
docker run --rm tea:qwen3-8b bash -c "ls -lh /opt/tea-models/"
docker run --rm -v $(pwd):/work tea:qwen3-8b --version

# Rust variant
docker build -t tea:rust-qwen3-8b -f docker/Dockerfile.rust-qwen3-8b .
docker run --rm tea:rust-qwen3-8b bash -c "ls -lh /opt/tea-models/"
docker run --rm tea:rust-qwen3-8b --version
```

---

## Risk Assessment

**Primary Risk:** Model download URL changes if Ollama updates the blob
- **Mitigation:** Blob SHA is stable for specific model version; document version in Dockerfile

**Secondary Risk:** Rust build with llm-local feature may have compatibility issues
- **Mitigation:** Feature already proven with phi4-mini variant

**Rollback:** Delete both new Dockerfiles - no impact on existing images

---

## Definition of Done

- [x] `docker/Dockerfile.qwen3-8b` created following Python pattern
- [x] `docker/Dockerfile.rust-qwen3-8b` created following Rust pattern
- [x] Both Docker builds succeed locally
- [x] Model file exists at correct path in both containers
- [x] `docker/BUILD.md` updated with both new variants
- [x] No changes to existing Dockerfiles

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-18 | 0.1 | Initial story creation (Python only) | Sarah (PO) |
| 2026-01-18 | 0.2 | Added Rust variant requirement | Sarah (PO) |
| 2026-01-18 | 1.0 | Implementation complete - both variants built and tested | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Completion Notes List

- Created Python Dockerfile following `Dockerfile.phi4-mini` pattern exactly
- Created Rust Dockerfile following `Dockerfile.rust-phi4-mini` pattern exactly
- Both images build successfully with full model download (~5.2GB Qwen 3 8B Q4_K_M)
- Python image: 6.03GB (within expected ~6GB target)
- Rust image: 5.33GB (within expected ~5.5GB target)
- Environment variables correctly set in both images
- Model files verified at `/opt/tea-models/qwen3-8b.gguf` (4.9G)
- `tea --version` works in both containers (v0.9.72)
- No modifications to existing Dockerfiles
- BUILD.md updated with both new variants

### File List

**New Files:**
- `docker/Dockerfile.qwen3-8b` - Python variant Dockerfile
- `docker/Dockerfile.rust-qwen3-8b` - Rust variant Dockerfile

**Modified Files:**
- `docker/BUILD.md` - Added qwen3-8b and rust-qwen3-8b to variant tables

---

## QA Results

*To be filled after QA review*
