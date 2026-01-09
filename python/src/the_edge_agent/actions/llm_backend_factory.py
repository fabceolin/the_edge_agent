"""
LLM Backend Factory for The Edge Agent (TEA-RELEASE-004.5).

This module provides factory functions to create the appropriate LLM backend
based on configuration, with automatic fallback from local to API backends.

Factory Logic:
1. If backend='local' and model available, use LocalLlmBackend
2. If local fails, fall back to ApiLlmBackend with warning
3. If backend='api' or unset, use ApiLlmBackend directly

Example:
    >>> from the_edge_agent.actions.llm_backend_factory import create_llm_backend
    >>> backend = create_llm_backend({"llm": {"backend": "local"}})
    >>> result = backend.chat([{"role": "user", "content": "Hello"}])
"""

import logging
from typing import Any, Dict, Optional

from .llm_backend import LlmBackend, LlmCallParams, LlmCallResult

logger = logging.getLogger(__name__)


class ApiLlmBackend(LlmBackend):
    """
    API-based LLM backend using existing llm_actions infrastructure.

    This backend wraps the existing llm.call action to provide a consistent
    LlmBackend interface for API-based models (OpenAI, Azure, Ollama, LiteLLM).

    Note: This is a wrapper that delegates to the existing llm_actions.py
    implementation to avoid code duplication.

    Args:
        settings: Settings dictionary with LLM configuration.
        engine: Optional YAMLEngine instance for accessing actions registry.

    Example:
        >>> backend = ApiLlmBackend({"llm": {"model": "gpt-4o-mini"}})
        >>> result = backend.chat([{"role": "user", "content": "Hi"}])
    """

    def __init__(
        self,
        settings: Dict[str, Any],
        engine: Optional[Any] = None,
    ):
        self.settings = settings
        self._engine = engine
        self._llm_settings = settings.get("llm", {})
        self._model = self._llm_settings.get("model", "gpt-4o-mini")
        self._provider = self._llm_settings.get("provider", "auto")
        self._api_base = self._llm_settings.get("api_base")
        self._temperature = self._llm_settings.get("temperature", 0.7)
        self._timeout = self._llm_settings.get("timeout", 300)

    def call(self, params: LlmCallParams) -> LlmCallResult:
        """
        Generate text completion from a raw prompt.

        Note: For API backends, this converts the raw prompt to a single
        user message and uses chat completion internally.

        Args:
            params: LlmCallParams with prompt and generation settings.

        Returns:
            LlmCallResult with generated content.
        """
        # Convert raw prompt to chat message
        messages = [{"role": "user", "content": params.prompt}]
        return self.chat(
            messages=messages,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
        )

    def chat(
        self,
        messages: list,
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[list] = None,
        **kwargs: Any,
    ) -> LlmCallResult:
        """
        Chat completion using API backend.

        Uses the existing llm.call action infrastructure for API requests.

        Args:
            messages: List of message dicts with 'role' and 'content'.
            max_tokens: Maximum tokens to generate.
            temperature: Sampling temperature.
            stop: Optional stop sequences.
            **kwargs: Additional parameters.

        Returns:
            LlmCallResult with assistant's response.
        """
        # Use OpenAI SDK directly (consistent with llm_actions.py)
        try:
            from openai import OpenAI, AzureOpenAI
        except ImportError:
            raise ImportError(
                "OpenAI library not installed. Install with: pip install openai"
            )

        import os

        # Determine provider (same logic as llm_actions.py)
        provider = self._provider.lower()
        if provider == "auto":
            if os.getenv("OLLAMA_API_BASE"):
                provider = "ollama"
            elif os.getenv("AZURE_OPENAI_API_KEY") and os.getenv(
                "AZURE_OPENAI_ENDPOINT"
            ):
                provider = "azure"
            else:
                provider = "openai"

        # Initialize client
        if provider == "ollama":
            ollama_base = self._api_base or os.getenv(
                "OLLAMA_API_BASE", "http://localhost:11434/v1"
            )
            client = OpenAI(
                base_url=ollama_base, api_key="ollama", timeout=self._timeout
            )
            model = self._model
        elif provider == "azure":
            azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
            azure_endpoint = self._api_base or os.getenv("AZURE_OPENAI_ENDPOINT")
            client = AzureOpenAI(
                api_key=azure_api_key,
                azure_endpoint=azure_endpoint,
                api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview"),
                timeout=self._timeout,
            )
            model = os.getenv("AZURE_OPENAI_DEPLOYMENT", self._model)
        else:
            if self._api_base:
                client = OpenAI(base_url=self._api_base, timeout=self._timeout)
            else:
                client = OpenAI(timeout=self._timeout)
            model = self._model

        # Filter out internal parameters that OpenAI doesn't accept
        internal_params = {
            "state",
            "config",
            "node",
            "graph",
            "parallel_results",
            "max_retries",
            "base_delay",
            "max_delay",
            "opik_trace",
            "provider",
            "api_base",
        }
        filtered_kwargs = {k: v for k, v in kwargs.items() if k not in internal_params}

        # Make API call
        response = client.chat.completions.create(
            model=model,
            messages=messages,
            max_tokens=max_tokens,
            temperature=temperature,
            stop=stop,
            **filtered_kwargs,
        )

        usage = (
            response.usage.model_dump() if hasattr(response.usage, "model_dump") else {}
        )

        return LlmCallResult(
            content=response.choices[0].message.content or "",
            model=model,
            tokens_used=usage.get("total_tokens"),
            finish_reason=response.choices[0].finish_reason,
        )

    def embed(self, text: str) -> list:
        """
        Generate embeddings using API backend.

        Args:
            text: Input text to embed.

        Returns:
            List of floats representing the embedding.
        """
        try:
            from openai import OpenAI
        except ImportError:
            raise ImportError(
                "OpenAI library not installed. Install with: pip install openai"
            )

        import os

        if self._api_base:
            client = OpenAI(base_url=self._api_base, timeout=self._timeout)
        else:
            client = OpenAI(timeout=self._timeout)

        # Use embedding model (default to text-embedding-ada-002 for OpenAI)
        embed_model = self._llm_settings.get(
            "embedding_model", "text-embedding-ada-002"
        )

        response = client.embeddings.create(model=embed_model, input=text)

        return response.data[0].embedding

    def stream(
        self,
        params: LlmCallParams,
        callback: Any,
    ) -> LlmCallResult:
        """
        Stream text generation with callback.

        Args:
            params: LlmCallParams with prompt and generation settings.
            callback: Function called with each chunk.

        Returns:
            LlmCallResult with complete content.
        """
        messages = [{"role": "user", "content": params.prompt}]
        return self.stream_chat(
            messages=messages,
            callback=callback,
            max_tokens=params.max_tokens,
            temperature=params.temperature,
            stop=params.stop,
        )

    def stream_chat(
        self,
        messages: list,
        callback: Any,
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[list] = None,
        **kwargs: Any,
    ) -> LlmCallResult:
        """
        Stream chat completion with callback.

        Args:
            messages: List of message dicts.
            callback: Function called with each chunk.
            max_tokens: Maximum tokens.
            temperature: Sampling temperature.
            stop: Optional stop sequences.
            **kwargs: Additional parameters.

        Returns:
            LlmCallResult with complete content.
        """
        try:
            from openai import OpenAI, AzureOpenAI
        except ImportError:
            raise ImportError(
                "OpenAI library not installed. Install with: pip install openai"
            )

        import os

        # Determine provider
        provider = self._provider.lower()
        if provider == "auto":
            if os.getenv("OLLAMA_API_BASE"):
                provider = "ollama"
            elif os.getenv("AZURE_OPENAI_API_KEY") and os.getenv(
                "AZURE_OPENAI_ENDPOINT"
            ):
                provider = "azure"
            else:
                provider = "openai"

        # Initialize client
        if provider == "ollama":
            ollama_base = self._api_base or os.getenv(
                "OLLAMA_API_BASE", "http://localhost:11434/v1"
            )
            client = OpenAI(
                base_url=ollama_base, api_key="ollama", timeout=self._timeout
            )
            model = self._model
        elif provider == "azure":
            azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
            azure_endpoint = self._api_base or os.getenv("AZURE_OPENAI_ENDPOINT")
            client = AzureOpenAI(
                api_key=azure_api_key,
                azure_endpoint=azure_endpoint,
                api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview"),
                timeout=self._timeout,
            )
            model = os.getenv("AZURE_OPENAI_DEPLOYMENT", self._model)
        else:
            if self._api_base:
                client = OpenAI(base_url=self._api_base, timeout=self._timeout)
            else:
                client = OpenAI(timeout=self._timeout)
            model = self._model

        # Filter out internal parameters that OpenAI doesn't accept
        internal_params = {
            "state",
            "config",
            "node",
            "graph",
            "parallel_results",
            "max_retries",
            "base_delay",
            "max_delay",
            "opik_trace",
            "provider",
            "api_base",
        }
        filtered_kwargs = {k: v for k, v in kwargs.items() if k not in internal_params}

        # Stream
        stream = client.chat.completions.create(
            model=model,
            messages=messages,
            max_tokens=max_tokens,
            temperature=temperature,
            stop=stop,
            stream=True,
            **filtered_kwargs,
        )

        full_content = []
        for chunk in stream:
            if chunk.choices and chunk.choices[0].delta.content:
                content = chunk.choices[0].delta.content
                full_content.append(content)
                callback(content)

        return LlmCallResult(
            content="".join(full_content),
            model=model,
        )


def create_llm_backend(
    settings: Dict[str, Any],
    engine: Optional[Any] = None,
) -> LlmBackend:
    """
    Create appropriate LLM backend based on settings.

    Factory Priority:
    1. If backend='local' and model available, use LocalLlmBackend
    2. If local fails (import error, model not found), fall back to ApiLlmBackend
    3. If backend='api' or unset, use ApiLlmBackend

    Args:
        settings: Settings dictionary with 'llm' section.
        engine: Optional YAMLEngine instance.

    Returns:
        LlmBackend instance (LocalLlmBackend or ApiLlmBackend).

    Example:
        >>> # Local backend with fallback
        >>> backend = create_llm_backend({"llm": {"backend": "local"}})
        >>>
        >>> # Explicit API backend
        >>> backend = create_llm_backend({"llm": {"backend": "api", "model": "gpt-4"}})
    """
    llm_settings = settings.get("llm", {})
    backend_type = llm_settings.get("backend", "api")

    if backend_type == "local":
        # Try to create local backend
        try:
            from .llm_local import (
                LocalLlmBackend,
                LLAMA_CPP_AVAILABLE,
                resolve_model_path,
                get_model_info,
            )

            if not LLAMA_CPP_AVAILABLE:
                logger.warning(
                    "llama-cpp-python not installed. "
                    "Install with: pip install the_edge_agent[llm-local]. "
                    "Falling back to API backend."
                )
                return ApiLlmBackend(settings, engine)

            model_path = resolve_model_path(settings)

            if not model_path:
                logger.warning(
                    "No local model found. Set TEA_MODEL_PATH or configure llm.model_path. "
                    "Falling back to API backend."
                )
                return ApiLlmBackend(settings, engine)

            # Get model-specific configuration
            model_info = get_model_info(model_path)

            # Create local backend with settings
            return LocalLlmBackend(
                model_path=model_path,
                n_ctx=llm_settings.get("n_ctx", model_info["n_ctx"]),
                n_threads=llm_settings.get("n_threads"),  # None = auto
                n_gpu_layers=llm_settings.get("n_gpu_layers", 0),
                chat_format=llm_settings.get("chat_format", model_info["chat_format"]),
                embedding=llm_settings.get("embedding", False),
            )

        except ImportError as e:
            logger.warning(f"llama-cpp-python not available: {e}")
            logger.warning(
                "Install with: pip install the_edge_agent[llm-local]. "
                "Falling back to API backend."
            )
            return ApiLlmBackend(settings, engine)

        except FileNotFoundError as e:
            logger.warning(f"Model file not found: {e}")
            logger.info("Falling back to API backend.")
            return ApiLlmBackend(settings, engine)

        except Exception as e:
            logger.warning(f"Failed to load local LLM: {e}")
            logger.info("Falling back to API backend.")
            return ApiLlmBackend(settings, engine)

    # Default to API backend
    logger.debug(f"Using API backend (backend={backend_type})")
    return ApiLlmBackend(settings, engine)


# Export public API
__all__ = [
    "LlmBackend",
    "ApiLlmBackend",
    "create_llm_backend",
]
