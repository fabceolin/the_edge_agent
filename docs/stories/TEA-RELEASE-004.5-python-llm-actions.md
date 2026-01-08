# Story TEA-RELEASE-004.5: Python Local LLM Actions Integration

## Status

Draft

## Story

**As a** developer using Python TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

## Story Context

**Existing System Integration:**

- Integrates with: Python actions system (`python/src/the_edge_agent/actions/`)
- Technology: Python + llama-cpp-python
- Follows pattern: Existing action modules (http_actions, file_actions)
- Touch points: `python/pyproject.toml`, `python/src/the_edge_agent/actions/`, `python/src/the_edge_agent/yaml_engine.py`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: `llm.call` action generates text using local llama-cpp-python backend
2. **AC-2**: `llm.embed` action generates embeddings using local model
3. **AC-3**: `llm.stream` action supports streaming generation (callback-based)
4. **AC-4**: Model path configurable via YAML settings or environment variable
5. **AC-5**: Graceful fallback to API-based LLM if local model not found

### Configuration Requirements

6. **AC-6**: Optional dependency `[llm-local]` enables local LLM support
7. **AC-7**: YAML settings support `llm.backend: local` configuration
8. **AC-8**: `llm.model_path` setting specifies GGUF file location
9. **AC-9**: Default model search path: `~/.cache/tea/models/`

### Quality Requirements

10. **AC-10**: Unit tests for LlmBackend class
11. **AC-11**: Integration test with small test model (TinyLlama ~500MB)
12. **AC-12**: Build without `llm-local` extras excludes llama-cpp-python dependency
13. **AC-13**: No regressions in existing Python tests

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-python as optional dependency (AC: 6, 12)
  - [ ] Add `llama-cpp-python` to pyproject.toml as optional
  - [ ] Create `[llm-local]` extras group
  - [ ] Implement graceful import with ImportError handling
  - [ ] Verify install with and without extras

- [ ] Task 2: Define LlmBackend abstract class (AC: 1, 2, 3, 5)
  - [ ] Create `python/src/the_edge_agent/actions/llm_backend.py`
  - [ ] Define `LlmBackend` ABC with `call`, `embed`, `stream` methods
  - [ ] Implement `ApiLlmBackend` for existing HTTP-based LLM
  - [ ] Implement `LocalLlmBackend` for llama-cpp-python

- [ ] Task 3: Implement LocalLlmBackend (AC: 1, 2, 3, 4)
  - [ ] Create `python/src/the_edge_agent/actions/llm_local.py`
  - [ ] Initialize llama-cpp-python Llama model
  - [ ] Implement `call()` for text generation
  - [ ] Implement `embed()` for embeddings
  - [ ] Implement `stream()` with callback

- [ ] Task 4: Add YAML configuration support (AC: 7, 8, 9)
  - [ ] Add `llm` section to settings schema
  - [ ] Support `backend: local | api` selection
  - [ ] Support `model_path` configuration
  - [ ] Implement model path resolution (env > yaml > default)

- [ ] Task 5: Integrate with yaml_engine action dispatcher (AC: 5)
  - [ ] Update `yaml_engine.py` action routing
  - [ ] Select backend based on settings
  - [ ] Implement fallback from local to API if model missing
  - [ ] Log backend selection for debugging

- [ ] Task 6: Add tests (AC: 10, 11, 13)
  - [ ] Add unit tests for LlmBackend class
  - [ ] Add integration test with mock or test model
  - [ ] Run full test suite to verify no regressions

## Dev Notes

### pyproject.toml Configuration

```toml
[project.optional-dependencies]
llm-local = [
    "llama-cpp-python>=0.2.0",
]

# Full bundle includes everything
llm-full = [
    "the_edge_agent[full]",
    "the_edge_agent[llm-local]",
]
```

### LlmBackend Abstract Class

```python
# python/src/the_edge_agent/actions/llm_backend.py
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, List, Callable

@dataclass
class LlmCallParams:
    prompt: str
    max_tokens: int = 100
    temperature: float = 0.7
    stop: Optional[List[str]] = None

@dataclass
class LlmCallResult:
    content: str
    model: str
    tokens_used: Optional[int] = None

class LlmBackend(ABC):
    @abstractmethod
    def call(self, params: LlmCallParams) -> LlmCallResult:
        """Generate text completion."""
        pass

    @abstractmethod
    def embed(self, text: str) -> List[float]:
        """Generate embeddings for text."""
        pass

    @abstractmethod
    def stream(
        self,
        params: LlmCallParams,
        callback: Callable[[str], None]
    ) -> LlmCallResult:
        """Stream text generation with callback."""
        pass
```

### LocalLlmBackend Implementation

```python
# python/src/the_edge_agent/actions/llm_local.py
import os
from pathlib import Path
from typing import Optional

try:
    from llama_cpp import Llama
    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False
    Llama = None

from .llm_backend import LlmBackend, LlmCallParams, LlmCallResult


class LocalLlmBackend(LlmBackend):
    def __init__(self, model_path: str, n_ctx: int = 2048, n_threads: int = 4):
        if not LLAMA_CPP_AVAILABLE:
            raise ImportError(
                "llama-cpp-python not installed. "
                "Install with: pip install the_edge_agent[llm-local]"
            )

        self.model_path = Path(model_path)
        if not self.model_path.exists():
            raise FileNotFoundError(f"Model not found: {model_path}")

        self.llm = Llama(
            model_path=str(self.model_path),
            n_ctx=n_ctx,
            n_threads=n_threads,
            verbose=False,
        )

    def call(self, params: LlmCallParams) -> LlmCallResult:
        output = self.llm(
            params.prompt,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
        )

        return LlmCallResult(
            content=output["choices"][0]["text"],
            model=self.model_path.name,
            tokens_used=output.get("usage", {}).get("total_tokens"),
        )

    def embed(self, text: str) -> list[float]:
        embedding = self.llm.embed(text)
        return embedding

    def stream(self, params: LlmCallParams, callback) -> LlmCallResult:
        full_content = ""
        for output in self.llm(
            params.prompt,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
            stream=True,
        ):
            chunk = output["choices"][0]["text"]
            full_content += chunk
            callback(chunk)

        return LlmCallResult(
            content=full_content,
            model=self.model_path.name,
        )
```

### Model Path Resolution

```python
def resolve_model_path(settings: dict) -> Optional[str]:
    """Resolve model path from environment, settings, or default."""
    # 1. Explicit environment variable
    if env_path := os.environ.get("TEA_MODEL_PATH"):
        return env_path

    # 2. YAML settings
    llm_settings = settings.get("llm", {})
    if yaml_path := llm_settings.get("model_path"):
        # Expand environment variables in path
        return os.path.expandvars(yaml_path)

    # 3. APPDIR for AppImage
    if appdir := os.environ.get("APPDIR"):
        candidate = os.path.join(appdir, "usr/share/models/gemma-3n-E4B-it-Q4_K_M.gguf")
        if os.path.exists(candidate):
            return candidate

    # 4. Default cache location
    default_path = os.path.expanduser("~/.cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf")
    if os.path.exists(default_path):
        return default_path

    return None
```

### YAML Settings Example

```yaml
# Example workflow with local LLM settings
settings:
  llm:
    backend: local           # 'local' or 'api'
    model_path: ~/.cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf
    n_ctx: 4096             # Context window
    n_threads: 4            # CPU threads

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.question }}"
      max_tokens: 100
```

### Backend Factory

```python
def create_llm_backend(settings: dict) -> LlmBackend:
    """Create appropriate LLM backend based on settings."""
    llm_settings = settings.get("llm", {})
    backend_type = llm_settings.get("backend", "api")

    if backend_type == "local":
        model_path = resolve_model_path(settings)
        if model_path:
            try:
                return LocalLlmBackend(
                    model_path=model_path,
                    n_ctx=llm_settings.get("n_ctx", 2048),
                    n_threads=llm_settings.get("n_threads", 4),
                )
            except (ImportError, FileNotFoundError) as e:
                logger.warning(f"Failed to load local LLM: {e}, falling back to API")

    # Default to API backend
    return ApiLlmBackend(settings)
```

### Relevant Source Files

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py         # Register llm actions
│   ├── llm_backend.py      # NEW: LlmBackend ABC
│   ├── llm_local.py        # NEW: LocalLlmBackend
│   └── llm_api.py          # Existing API backend (refactor)
└── yaml_engine.py          # Action routing, settings parsing
```

### Testing Strategy

| Test Type | File | Description |
|-----------|------|-------------|
| Unit | `tests/test_llm_backend.py` | ABC implementation tests |
| Unit | `tests/test_llm_local.py` | LocalLlmBackend tests (mocked) |
| Integration | `tests/test_llm_local_integration.py` | With TinyLlama model |
| Regression | `pytest` | Full test suite |

### Mocking for Tests

```python
# tests/test_llm_local.py
from unittest.mock import Mock, patch

@patch("the_edge_agent.actions.llm_local.Llama")
def test_local_llm_call(mock_llama):
    mock_instance = Mock()
    mock_instance.return_value = {
        "choices": [{"text": "Hello, world!"}],
        "usage": {"total_tokens": 10},
    }
    mock_llama.return_value = mock_instance

    backend = LocalLlmBackend("/fake/model.gguf")
    result = backend.call(LlmCallParams(prompt="Hello"))

    assert result.content == "Hello, world!"
```

## Definition of Done

- [ ] `LlmBackend` ABC defined with `call`, `embed`, `stream` methods
- [ ] `LocalLlmBackend` implements ABC using llama-cpp-python
- [ ] Optional dependency `[llm-local]` in pyproject.toml
- [ ] YAML settings support `llm.backend` and `llm.model_path`
- [ ] Fallback to API if local model unavailable
- [ ] Unit tests pass (with mocks)
- [ ] Integration test with test model passes
- [ ] No regressions in existing tests

## Risk and Compatibility Check

**Primary Risk:** llama-cpp-python API changes between versions

**Mitigation:** Pin minimum version, add version check

**Rollback:** Optional dependency, can be excluded

## Compatibility Verification

- [x] No breaking changes to existing APIs (additive)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: None without optional dependency

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
