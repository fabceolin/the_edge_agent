"""
Local LLM Backend using llama-cpp-python (TEA-RELEASE-004.5).

This module provides LocalLlmBackend for running GGUF models locally using
llama-cpp-python. It's the Python equivalent of:
- Rust's llama-cpp-2 crate (TEA-RELEASE-004.4)
- WASM's wllama callback bridge (TEA-RELEASE-004.3)

All three implementations use the same underlying llama.cpp engine and GGUF model format.

Features:
- Raw prompt completion (call)
- OpenAI-compatible chat completion (chat)
- Streaming with callbacks (stream, stream_chat)
- Text embeddings (embed)
- Auto-detection of model configuration (Phi-4-mini 128K, Gemma 32K)
- Model path resolution from multiple sources

Example:
    >>> from the_edge_agent.actions.llm_local import LocalLlmBackend
    >>> backend = LocalLlmBackend("./models/phi4-mini.gguf")
    >>> result = backend.chat([
    ...     {"role": "user", "content": "What is 2+2?"}
    ... ])
    >>> print(result.content)

Requirements:
    pip install 'the_edge_agent[llm-local]'
"""

import logging
import multiprocessing
import os
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

from .llm_backend import LlmBackend, LlmCallParams, LlmCallResult

logger = logging.getLogger(__name__)

# Try to import llama-cpp-python (optional dependency)
try:
    from llama_cpp import Llama

    LLAMA_CPP_AVAILABLE = True
except ImportError:
    LLAMA_CPP_AVAILABLE = False
    Llama = None  # type: ignore


# Supported models configuration (aligned with TEA-RELEASE-004 epic)
SUPPORTED_MODELS = {
    "phi4-mini": {
        "file": "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",
        "n_ctx": 128000,  # 128K context - Phi-4-mini's strength
        "chat_format": "chatml",
    },
    "gemma": {
        "file": "gemma-3n-E4B-it-Q4_K_M.gguf",
        "n_ctx": 32768,  # 32K context
        "chat_format": "gemma",
    },
}

# Default model filenames in order of preference
DEFAULT_MODELS = [
    "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",  # Phi-4-mini (smaller, 128K ctx)
    "gemma-3n-E4B-it-Q4_K_M.gguf",  # Gemma (larger, higher quality)
]


def get_model_info(model_path: str) -> Dict[str, Any]:
    """
    Get model-specific configuration based on filename.

    Auto-detects model family and returns optimal configuration
    for context size and chat format.

    Args:
        model_path: Path to the GGUF model file.

    Returns:
        Dictionary with keys:
        - n_ctx: Recommended context window size
        - chat_format: Chat template format (e.g., "chatml", "gemma")
        - family: Model family name ("phi", "gemma", "unknown")

    Example:
        >>> info = get_model_info("./models/Phi-4-mini.gguf")
        >>> print(info["n_ctx"])  # 128000
        >>> print(info["chat_format"])  # "chatml"
    """
    filename = Path(model_path).name.lower()

    if "phi" in filename:
        return {
            "n_ctx": 128000,  # Phi-4-mini's 128K context
            "chat_format": "chatml",
            "family": "phi",
        }
    elif "gemma" in filename:
        return {
            "n_ctx": 32768,  # Gemma's 32K context
            "chat_format": "gemma",
            "family": "gemma",
        }
    else:
        return {
            "n_ctx": 4096,  # Safe default
            "chat_format": None,
            "family": "unknown",
        }


def resolve_model_path(settings: Dict[str, Any]) -> Optional[str]:
    """
    Resolve model path using priority order.

    Search order:
    1. TEA_MODEL_PATH environment variable (explicit override)
    2. YAML settings llm.model_path
    3. AppImage bundled model ($APPDIR/usr/share/models/)
    4. Default cache location (~/.cache/tea/models/)

    Args:
        settings: YAML settings dictionary with optional 'llm' section.

    Returns:
        Path to found model file, or None if no model found.

    Example:
        >>> path = resolve_model_path({"llm": {"model_path": "./my-model.gguf"}})
        >>> print(path)  # "./my-model.gguf"
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


class LocalLlmBackend(LlmBackend):
    """
    Local LLM backend using llama-cpp-python.

    This is the Python equivalent of:
    - Rust's llama-cpp-2 crate (TEA-RELEASE-004.4)
    - WASM's wllama callback bridge (TEA-RELEASE-004.3)

    All three use the same underlying llama.cpp engine and GGUF model format.

    Features:
    - call(): Raw prompt completion
    - chat(): OpenAI-compatible chat completion (recommended for instruction models)
    - stream()/stream_chat(): Streaming generation with callbacks
    - embed(): Text embedding generation
    - Auto-detection of model config (Phi-4-mini 128K, Gemma 32K)

    Example:
        >>> backend = LocalLlmBackend("./models/phi4-mini.gguf")
        >>> result = backend.chat([
        ...     {"role": "system", "content": "You are helpful."},
        ...     {"role": "user", "content": "What is Python?"}
        ... ], max_tokens=200)
        >>> print(result.content)

    Args:
        model_path: Path to GGUF model file.
        n_ctx: Context window size (auto-detected if not specified).
        n_threads: CPU threads to use (default: all available).
        n_gpu_layers: GPU layers for acceleration (-1 = auto/all, 0 = CPU only).
        chat_format: Chat template format (auto-detected if not specified).
        embedding: Enable embedding mode for embed() method.

    Raises:
        ImportError: If llama-cpp-python is not installed.
        FileNotFoundError: If model file doesn't exist.
    """

    def __init__(
        self,
        model_path: str,
        n_ctx: int = 4096,
        n_threads: Optional[int] = None,
        n_gpu_layers: int = -1,  # -1 = auto-offload all layers to GPU (matches Rust behavior)
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
        model_info = get_model_info(model_path)

        # Use auto-detected settings unless explicitly overridden
        if n_ctx == 4096 and model_info["n_ctx"] != 4096:
            n_ctx = model_info["n_ctx"]
            logger.info(
                f"Auto-detected {model_info['family']} model, "
                f"using {n_ctx // 1000}K context"
            )

        if chat_format is None and model_info["chat_format"]:
            chat_format = model_info["chat_format"]
            logger.info(f"Auto-detected chat format: {chat_format}")

        # Use all CPU cores if not specified
        if n_threads is None:
            n_threads = multiprocessing.cpu_count()

        logger.info(
            f"Loading model: {model_path} "
            f"(n_ctx={n_ctx}, n_threads={n_threads}, n_gpu_layers={n_gpu_layers})"
        )

        self._llm = Llama(
            model_path=str(self.model_path),
            n_ctx=n_ctx,
            n_threads=n_threads,
            n_gpu_layers=n_gpu_layers,
            chat_format=chat_format,
            embedding=embedding,
            verbose=False,
        )

        self._model_name = self.model_path.stem
        self._embedding_mode = embedding
        self._model_info = model_info

    def call(self, params: LlmCallParams) -> LlmCallResult:
        """
        Generate text completion from a raw prompt.

        Uses llama.cpp's raw completion API. For instruction models
        like Phi-4-mini, prefer chat() which handles proper formatting.

        Args:
            params: LlmCallParams with prompt and generation settings.

        Returns:
            LlmCallResult with generated content and token usage.

        Example:
            >>> result = backend.call(LlmCallParams(
            ...     prompt="The capital of France is",
            ...     max_tokens=50,
            ...     temperature=0.7
            ... ))
            >>> print(result.content)
        """
        output = self._llm(
            params.prompt,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
        )

        return LlmCallResult(
            content=output["choices"][0]["text"],
            model=self._model_name,
            tokens_used=output.get("usage", {}).get("total_tokens"),
            finish_reason=output["choices"][0].get("finish_reason"),
        )

    def chat(
        self,
        messages: List[Dict[str, str]],
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs: Any,
    ) -> LlmCallResult:
        """
        Chat completion using OpenAI-compatible format.

        This is the recommended method for instruction models like Phi-4-mini.
        The chat format (ChatML, Gemma, etc.) is auto-detected from the model.

        Args:
            messages: List of message dicts with 'role' and 'content'.
                     Roles: "system", "user", "assistant".
            max_tokens: Maximum tokens to generate (default: 100).
            temperature: Sampling temperature (default: 0.7).
            stop: Optional stop sequences.
            **kwargs: Additional parameters passed to llama-cpp-python.

        Returns:
            LlmCallResult with assistant's response.

        Example:
            >>> result = backend.chat([
            ...     {"role": "system", "content": "You are a coding assistant."},
            ...     {"role": "user", "content": "Write a Python hello world."}
            ... ], max_tokens=200)
            >>> print(result.content)
        """
        # Filter kwargs to only include parameters accepted by create_chat_completion
        # This prevents errors from extra params passed by the YAML engine
        valid_params = {
            "top_p",
            "top_k",
            "repeat_penalty",
            "presence_penalty",
            "frequency_penalty",
            "seed",
            "stream",
            "logprobs",
            "top_logprobs",
        }
        filtered_kwargs = {k: v for k, v in kwargs.items() if k in valid_params}

        output = self._llm.create_chat_completion(
            messages=messages,
            max_tokens=max_tokens,
            temperature=temperature,
            stop=stop,
            **filtered_kwargs,
        )

        return LlmCallResult(
            content=output["choices"][0]["message"]["content"],
            model=self._model_name,
            tokens_used=output.get("usage", {}).get("total_tokens"),
            finish_reason=output["choices"][0].get("finish_reason"),
        )

    def embed(self, text: str) -> List[float]:
        """
        Generate embeddings for text.

        Note: Model must be loaded with embedding=True for this to work.

        Args:
            text: Input text to embed.

        Returns:
            List of floats representing the embedding vector.

        Raises:
            RuntimeError: If model not loaded with embedding=True.

        Example:
            >>> backend = LocalLlmBackend("model.gguf", embedding=True)
            >>> vec = backend.embed("Hello world")
            >>> print(len(vec))  # e.g., 4096
        """
        if not self._embedding_mode:
            raise RuntimeError(
                "Model not loaded with embedding=True. "
                "Create backend with: LocalLlmBackend(path, embedding=True)"
            )

        embedding = self._llm.embed(text)
        return list(embedding)

    def stream(
        self,
        params: LlmCallParams,
        callback: Callable[[str], None],
    ) -> LlmCallResult:
        """
        Stream text generation with callback for each token.

        Args:
            params: LlmCallParams with prompt and generation settings.
            callback: Function called with each generated text chunk.

        Returns:
            LlmCallResult with complete generated content.

        Example:
            >>> def print_chunk(chunk):
            ...     print(chunk, end="", flush=True)
            >>> result = backend.stream(
            ...     LlmCallParams(prompt="Write a poem about AI"),
            ...     callback=print_chunk
            ... )
        """
        full_content = ""
        for output in self._llm(
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
        messages: List[Dict[str, str]],
        callback: Callable[[str], None],
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs: Any,
    ) -> LlmCallResult:
        """
        Stream chat completion with callback for each token.

        Args:
            messages: List of message dicts with 'role' and 'content'.
            callback: Function called with each generated text chunk.
            max_tokens: Maximum tokens to generate.
            temperature: Sampling temperature.
            stop: Optional stop sequences.
            **kwargs: Additional parameters.

        Returns:
            LlmCallResult with complete assistant response.

        Example:
            >>> def print_chunk(chunk):
            ...     print(chunk, end="", flush=True)
            >>> result = backend.stream_chat(
            ...     [{"role": "user", "content": "Tell me a story"}],
            ...     callback=print_chunk,
            ...     max_tokens=500
            ... )
        """
        full_content = ""
        for output in self._llm.create_chat_completion(
            messages=messages,
            max_tokens=max_tokens,
            temperature=temperature,
            stop=stop,
            stream=True,
            **kwargs,
        ):
            delta = output["choices"][0].get("delta", {})
            if chunk := delta.get("content", ""):
                full_content += chunk
                callback(chunk)

        return LlmCallResult(
            content=full_content,
            model=self._model_name,
        )

    def is_available(self) -> bool:
        """Check if the backend is ready."""
        return self._llm is not None

    def close(self) -> None:
        """Release model memory."""
        if self._llm is not None:
            # llama-cpp-python handles cleanup on del
            self._llm = None  # type: ignore

    @property
    def model_info(self) -> Dict[str, Any]:
        """Get auto-detected model information."""
        return self._model_info.copy()


# Export public API
__all__ = [
    "LocalLlmBackend",
    "LLAMA_CPP_AVAILABLE",
    "SUPPORTED_MODELS",
    "DEFAULT_MODELS",
    "get_model_info",
    "resolve_model_path",
]
