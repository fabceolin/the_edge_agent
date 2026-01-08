# Story TEA-RELEASE-004.5: Python Local LLM Actions Integration

## Status

Draft

## Story

**As a** developer using Python TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

## LLM Backend Comparison

Understanding how Python's local LLM differs from WASM's wllama approach:

```
┌─────────────────────────────────────────────────────────────────────┐
│                         llama.cpp (C++)                             │
│              Core inference engine for GGUF models                  │
└─────────────────────────────────────────────────────────────────────┘
        │                        │                        │
        ▼                        ▼                        ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  llama-cpp-2    │    │ llama-cpp-python│    │     wllama      │
│  (Rust crate)   │    │  (Python pkg)   │    │   (JS/WASM)     │
│                 │    │                 │    │                 │
│ Direct bindings │    │ Direct bindings │    │ Callback bridge │
│ via bindgen     │    │ via ctypes/cffi │    │ Rust→JS→WASM    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                        │                        │
        ▼                        ▼                        ▼
   TEA Rust CLI            TEA Python CLI           TEA Browser
   (TEA-RELEASE-004.4)     (THIS STORY)            (TEA-RELEASE-004.3)
```

**Key difference:** Python uses direct bindings (simpler), WASM uses callback bridge (necessary for JS interop).

## Story Context

**Existing System Integration:**

- Integrates with: Python actions system (`python/src/the_edge_agent/actions/`)
- Technology: Python + llama-cpp-python
- Follows pattern: Existing action modules (http_actions, file_actions)
- Touch points: `python/pyproject.toml`, `python/src/the_edge_agent/actions/`, `python/src/the_edge_agent/yaml_engine.py`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: `llm.call` action generates text using local llama-cpp-python backend (raw prompt)
2. **AC-2**: `llm.chat` action generates text using OpenAI-compatible chat format (recommended for Phi-4-mini)
3. **AC-3**: `llm.embed` action generates embeddings using local model
4. **AC-4**: `llm.stream` action supports streaming generation (callback-based)
5. **AC-5**: Model path configurable via YAML settings or environment variable
6. **AC-6**: Graceful fallback to API-based LLM if local model not found
7. **AC-7**: Auto-detect model configuration (context size, chat format) from filename

### Configuration Requirements

8. **AC-8**: Optional dependency `[llm-local]` enables local LLM support
9. **AC-9**: YAML settings support `llm.backend: local` configuration
10. **AC-10**: `llm.model_path` setting specifies GGUF file location
11. **AC-11**: Default model search paths: `$APPDIR`, `~/.cache/tea/models/`
12. **AC-12**: Support both Phi-4-mini (128K ctx) and Gemma (32K ctx) models

### Quality Requirements

13. **AC-13**: Unit tests for LlmBackend class
14. **AC-14**: Integration test with small test model (TinyLlama ~500MB)
15. **AC-15**: Build without `llm-local` extras excludes llama-cpp-python dependency
16. **AC-16**: No regressions in existing Python tests

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-python as optional dependency (AC: 8, 15)
  - [ ] Add `llama-cpp-python` to pyproject.toml as optional
  - [ ] Create `[llm-local]` extras group
  - [ ] Implement graceful import with ImportError handling
  - [ ] Verify install with and without extras

- [ ] Task 2: Define LlmBackend abstract class (AC: 1, 2, 3, 4, 6)
  - [ ] Create `python/src/the_edge_agent/actions/llm_backend.py`
  - [ ] Define `LlmBackend` ABC with `call`, `chat`, `embed`, `stream` methods
  - [ ] Define `LlmCallParams` and `LlmCallResult` dataclasses
  - [ ] Implement `ApiLlmBackend` for existing HTTP-based LLM

- [ ] Task 3: Implement LocalLlmBackend (AC: 1, 2, 3, 4, 7, 12)
  - [ ] Create `python/src/the_edge_agent/actions/llm_local.py`
  - [ ] Initialize llama-cpp-python Llama model with auto-config
  - [ ] Implement `call()` for raw prompt completion
  - [ ] Implement `chat()` for OpenAI-compatible chat completion
  - [ ] Implement `embed()` for embeddings
  - [ ] Implement `stream()` and `stream_chat()` with callbacks
  - [ ] Add model auto-detection (Phi-4-mini 128K, Gemma 32K)

- [ ] Task 4: Add model path resolution (AC: 5, 10, 11)
  - [ ] Implement `resolve_model_path()` with priority order
  - [ ] Implement `get_model_info()` for auto-configuration
  - [ ] Support TEA_MODEL_PATH environment variable
  - [ ] Support APPDIR for AppImage detection
  - [ ] Support default cache path `~/.cache/tea/models/`

- [ ] Task 5: Add YAML configuration support (AC: 9, 10)
  - [ ] Add `llm` section to settings schema
  - [ ] Support `backend: local | api` selection
  - [ ] Support `model_path`, `n_ctx`, `n_threads`, `n_gpu_layers` options
  - [ ] Document settings in YAML_REFERENCE.md

- [ ] Task 6: Integrate with yaml_engine action dispatcher (AC: 1, 2, 3, 4, 6)
  - [ ] Update `yaml_engine.py` with lazy LLM backend initialization
  - [ ] Register `llm.call`, `llm.chat`, `llm.stream`, `llm.embed` actions
  - [ ] Implement backend factory with fallback logic
  - [ ] Log backend selection for debugging

- [ ] Task 7: Add tests (AC: 13, 14, 16)
  - [ ] Add unit tests for LlmBackend ABC
  - [ ] Add unit tests for LocalLlmBackend (mocked)
  - [ ] Add unit tests for model path resolution
  - [ ] Add integration test with TinyLlama test model
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
import logging
from pathlib import Path
from typing import Optional, List, Callable

try:
    from llama_cpp import Llama
    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False
    Llama = None

from .llm_backend import LlmBackend, LlmCallParams, LlmCallResult

logger = logging.getLogger(__name__)


# Default models supported (aligned with TEA-RELEASE-004 epic)
SUPPORTED_MODELS = {
    "phi4-mini": {
        "file": "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",
        "n_ctx": 128000,  # 128K context - Phi-4-mini's strength
        "chat_format": "chatml",
    },
    "gemma": {
        "file": "gemma-3n-E4B-it-Q4_K_M.gguf",
        "n_ctx": 32768,   # 32K context
        "chat_format": "gemma",
    },
}


class LocalLlmBackend(LlmBackend):
    """
    Local LLM backend using llama-cpp-python.

    This is the Python equivalent of:
    - Rust's llama-cpp-2 crate (TEA-RELEASE-004.4)
    - WASM's wllama callback bridge (TEA-RELEASE-004.3)

    All three use the same underlying llama.cpp engine and GGUF model format.
    """

    def __init__(
        self,
        model_path: str,
        n_ctx: int = 4096,
        n_threads: Optional[int] = None,
        n_gpu_layers: int = 0,
        chat_format: Optional[str] = None,
        embedding: bool = False,
    ):
        if not LLAMA_CPP_AVAILABLE:
            raise ImportError(
                "llama-cpp-python not installed. "
                "Install with: pip install the_edge_agent[llm-local]"
            )

        self.model_path = Path(model_path)
        if not self.model_path.exists():
            raise FileNotFoundError(f"Model not found: {model_path}")

        # Auto-detect model settings from filename
        model_name = self.model_path.name.lower()
        if "phi" in model_name and n_ctx == 4096:
            n_ctx = 128000  # Phi-4-mini supports 128K
            logger.info(f"Auto-detected Phi model, using 128K context")

        # Use all CPU cores if not specified
        if n_threads is None:
            import multiprocessing
            n_threads = multiprocessing.cpu_count()

        logger.info(f"Loading model: {model_path} (n_ctx={n_ctx}, n_threads={n_threads})")

        self.llm = Llama(
            model_path=str(self.model_path),
            n_ctx=n_ctx,
            n_threads=n_threads,
            n_gpu_layers=n_gpu_layers,
            chat_format=chat_format,
            embedding=embedding,
            verbose=False,
        )

        self._model_name = self.model_path.stem

    def call(self, params: LlmCallParams) -> LlmCallResult:
        """Generate text completion (raw prompt mode)."""
        output = self.llm(
            params.prompt,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
        )

        return LlmCallResult(
            content=output["choices"][0]["text"],
            model=self._model_name,
            tokens_used=output.get("usage", {}).get("total_tokens"),
        )

    def chat(self, messages: List[dict], **kwargs) -> LlmCallResult:
        """
        Chat completion using OpenAI-compatible format.

        This is the recommended method for instruction models like Phi-4-mini.

        Example:
            messages = [
                {"role": "system", "content": "You are a helpful assistant."},
                {"role": "user", "content": "What is 2+2?"},
            ]
            result = backend.chat(messages, max_tokens=100)
        """
        output = self.llm.create_chat_completion(
            messages=messages,
            max_tokens=kwargs.get("max_tokens", 100),
            temperature=kwargs.get("temperature", 0.7),
            stop=kwargs.get("stop"),
        )

        return LlmCallResult(
            content=output["choices"][0]["message"]["content"],
            model=self._model_name,
            tokens_used=output.get("usage", {}).get("total_tokens"),
        )

    def embed(self, text: str) -> List[float]:
        """Generate embeddings for text."""
        # Note: Model must be loaded with embedding=True for this to work
        embedding = self.llm.embed(text)
        return list(embedding)

    def stream(
        self,
        params: LlmCallParams,
        callback: Callable[[str], None]
    ) -> LlmCallResult:
        """Stream text generation with callback for each token."""
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
            model=self._model_name,
        )

    def stream_chat(
        self,
        messages: List[dict],
        callback: Callable[[str], None],
        **kwargs
    ) -> LlmCallResult:
        """Stream chat completion with callback for each token."""
        full_content = ""
        for output in self.llm.create_chat_completion(
            messages=messages,
            max_tokens=kwargs.get("max_tokens", 100),
            temperature=kwargs.get("temperature", 0.7),
            stop=kwargs.get("stop"),
            stream=True,
        ):
            delta = output["choices"][0].get("delta", {})
            if chunk := delta.get("content", ""):
                full_content += chunk
                callback(chunk)

        return LlmCallResult(
            content=full_content,
            model=self._model_name,
        )
```

### Model Path Resolution

```python
import os
from pathlib import Path
from typing import Optional
import logging

logger = logging.getLogger(__name__)

# Supported model filenames (in order of preference)
DEFAULT_MODELS = [
    "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",  # Phi-4-mini (smaller, 128K ctx)
    "gemma-3n-E4B-it-Q4_K_M.gguf",                 # Gemma (larger, higher quality)
]


def resolve_model_path(settings: dict) -> Optional[str]:
    """
    Resolve model path using priority order:
    1. TEA_MODEL_PATH environment variable (explicit override)
    2. YAML settings llm.model_path
    3. AppImage bundled model ($APPDIR/usr/share/models/)
    4. Default cache location (~/.cache/tea/models/)
    """
    # 1. Explicit environment variable
    if env_path := os.environ.get("TEA_MODEL_PATH"):
        if os.path.exists(env_path):
            logger.info(f"Using model from TEA_MODEL_PATH: {env_path}")
            return env_path
        logger.warning(f"TEA_MODEL_PATH set but file not found: {env_path}")

    # 2. YAML settings
    llm_settings = settings.get("llm", {})
    if yaml_path := llm_settings.get("model_path"):
        expanded = os.path.expandvars(os.path.expanduser(yaml_path))
        if os.path.exists(expanded):
            logger.info(f"Using model from settings: {expanded}")
            return expanded
        logger.warning(f"Settings model_path not found: {expanded}")

    # 3. APPDIR for AppImage (search for any supported model)
    if appdir := os.environ.get("APPDIR"):
        models_dir = Path(appdir) / "usr/share/models"
        for model_file in DEFAULT_MODELS:
            candidate = models_dir / model_file
            if candidate.exists():
                logger.info(f"Using AppImage bundled model: {candidate}")
                return str(candidate)

    # 4. Default cache location (search for any supported model)
    cache_dir = Path.home() / ".cache/tea/models"
    for model_file in DEFAULT_MODELS:
        candidate = cache_dir / model_file
        if candidate.exists():
            logger.info(f"Using cached model: {candidate}")
            return str(candidate)

    logger.warning("No local model found. Set TEA_MODEL_PATH or download a model.")
    return None


def get_model_info(model_path: str) -> dict:
    """Get model-specific configuration based on filename."""
    filename = Path(model_path).name.lower()

    if "phi" in filename:
        return {
            "n_ctx": 128000,      # Phi-4-mini's 128K context
            "chat_format": "chatml",
            "family": "phi",
        }
    elif "gemma" in filename:
        return {
            "n_ctx": 32768,       # Gemma's 32K context
            "chat_format": "gemma",
            "family": "gemma",
        }
    else:
        return {
            "n_ctx": 4096,        # Safe default
            "chat_format": None,
            "family": "unknown",
        }
```

### YAML Settings Example

```yaml
# Example workflow with local LLM settings (Phi-4-mini)
settings:
  llm:
    backend: local           # 'local' or 'api'
    model_path: ~/.cache/tea/models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf
    # Auto-detected from model, but can override:
    # n_ctx: 128000          # Phi-4-mini supports 128K context
    # n_threads: 8           # CPU threads (default: all cores)
    # n_gpu_layers: 0        # GPU offload layers (0 = CPU only)

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.question }}"
      max_tokens: 100
```

### YAML Action Usage Examples

**1. Raw prompt completion (`llm.call`):**
```yaml
nodes:
  - name: complete
    action: llm.call
    params:
      prompt: "Complete this sentence: The capital of France is"
      max_tokens: 50
      temperature: 0.7
```

**2. Chat completion (`llm.chat`) - Recommended for instruction models:**
```yaml
nodes:
  - name: chat_response
    action: llm.chat
    params:
      messages:
        - role: system
          content: "You are a helpful coding assistant."
        - role: user
          content: "{{ state.question }}"
      max_tokens: 500
      temperature: 0.3
```

**3. Streaming output (`llm.stream`):**
```yaml
nodes:
  - name: stream_response
    action: llm.stream
    params:
      prompt: "Write a short poem about programming"
      max_tokens: 200
    # Output streams to state.stream_response as tokens arrive
```

**4. Embeddings (`llm.embed`):**
```yaml
nodes:
  - name: embed_text
    action: llm.embed
    params:
      text: "{{ state.document }}"
    # Output: state.embed_text = [0.123, -0.456, ...]
```

**5. Multi-turn conversation:**
```yaml
state_schema:
  messages: list
  response: str

nodes:
  - name: add_user_message
    run: |
      state["messages"].append({"role": "user", "content": state["input"]})
      return state

  - name: get_response
    action: llm.chat
    params:
      messages: "{{ state.messages }}"
      max_tokens: 500

  - name: add_assistant_message
    run: |
      state["messages"].append({"role": "assistant", "content": state["get_response"]})
      state["response"] = state["get_response"]
      return state
```

### Backend Factory

```python
import logging
from typing import Optional
from .llm_backend import LlmBackend
from .llm_local import LocalLlmBackend, LLAMA_CPP_AVAILABLE
from .llm_api import ApiLlmBackend

logger = logging.getLogger(__name__)


def create_llm_backend(settings: dict) -> LlmBackend:
    """
    Create appropriate LLM backend based on settings.

    Priority:
    1. If backend='local' and model available, use LocalLlmBackend
    2. If local fails, fall back to ApiLlmBackend
    3. If backend='api' or unset, use ApiLlmBackend
    """
    llm_settings = settings.get("llm", {})
    backend_type = llm_settings.get("backend", "api")

    if backend_type == "local":
        model_path = resolve_model_path(settings)

        if model_path:
            try:
                # Get model-specific configuration
                model_info = get_model_info(model_path)

                return LocalLlmBackend(
                    model_path=model_path,
                    n_ctx=llm_settings.get("n_ctx", model_info["n_ctx"]),
                    n_threads=llm_settings.get("n_threads"),  # None = auto
                    n_gpu_layers=llm_settings.get("n_gpu_layers", 0),
                    chat_format=llm_settings.get("chat_format", model_info["chat_format"]),
                    embedding=llm_settings.get("embedding", False),
                )
            except ImportError as e:
                logger.warning(f"llama-cpp-python not installed: {e}")
                logger.warning("Install with: pip install the_edge_agent[llm-local]")
            except FileNotFoundError as e:
                logger.warning(f"Model file not found: {e}")
            except Exception as e:
                logger.warning(f"Failed to load local LLM: {e}")

            logger.info("Falling back to API backend")

        else:
            logger.warning("No local model found, falling back to API backend")

    # Default to API backend
    return ApiLlmBackend(settings)
```

### Action Registration in yaml_engine

```python
# python/src/the_edge_agent/yaml_engine.py (excerpt)

from .actions.llm_backend import LlmBackend, LlmCallParams
from .actions.llm_local import create_llm_backend

class YamlEngine:
    def __init__(self, yaml_content: str):
        # ... existing init ...
        self._llm_backend: Optional[LlmBackend] = None

    def _get_llm_backend(self) -> LlmBackend:
        """Lazy initialization of LLM backend."""
        if self._llm_backend is None:
            self._llm_backend = create_llm_backend(self.settings)
        return self._llm_backend

    def _execute_action(self, action: str, params: dict, state: dict) -> Any:
        """Execute a named action."""
        if action == "llm.call":
            return self._action_llm_call(params, state)
        elif action == "llm.chat":
            return self._action_llm_chat(params, state)
        elif action == "llm.stream":
            return self._action_llm_stream(params, state)
        elif action == "llm.embed":
            return self._action_llm_embed(params, state)
        # ... other actions ...

    def _action_llm_call(self, params: dict, state: dict) -> str:
        """Execute llm.call action (raw prompt completion)."""
        backend = self._get_llm_backend()
        result = backend.call(LlmCallParams(
            prompt=params["prompt"],
            max_tokens=params.get("max_tokens", 100),
            temperature=params.get("temperature", 0.7),
            stop=params.get("stop"),
        ))
        return result.content

    def _action_llm_chat(self, params: dict, state: dict) -> str:
        """Execute llm.chat action (chat completion)."""
        backend = self._get_llm_backend()
        result = backend.chat(
            messages=params["messages"],
            max_tokens=params.get("max_tokens", 100),
            temperature=params.get("temperature", 0.7),
            stop=params.get("stop"),
        )
        return result.content

    def _action_llm_stream(self, params: dict, state: dict) -> str:
        """Execute llm.stream action with streaming output."""
        backend = self._get_llm_backend()

        def stream_callback(chunk: str):
            # Could emit to event system, write to stdout, etc.
            print(chunk, end="", flush=True)

        result = backend.stream(
            LlmCallParams(
                prompt=params["prompt"],
                max_tokens=params.get("max_tokens", 100),
                temperature=params.get("temperature", 0.7),
                stop=params.get("stop"),
            ),
            callback=stream_callback,
        )
        return result.content

    def _action_llm_embed(self, params: dict, state: dict) -> list[float]:
        """Execute llm.embed action."""
        backend = self._get_llm_backend()
        return backend.embed(params["text"])
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

- [ ] `LlmBackend` ABC defined with `call`, `chat`, `embed`, `stream` methods
- [ ] `LocalLlmBackend` implements ABC using llama-cpp-python
- [ ] `llm.chat` action supports OpenAI-compatible chat completion format
- [ ] Auto-detection of model config (Phi-4-mini 128K, Gemma 32K)
- [ ] Optional dependency `[llm-local]` in pyproject.toml
- [ ] YAML settings support `llm.backend`, `llm.model_path`, `llm.n_ctx`
- [ ] Model path resolution: TEA_MODEL_PATH > YAML > APPDIR > cache
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
| 2026-01-08 | 0.2 | Expanded with detailed implementation: llm.chat action, OpenAI-compatible format, Phi-4-mini/Gemma auto-config, model path resolution, yaml_engine integration, comparison to wllama | Sarah (PO Agent) |
