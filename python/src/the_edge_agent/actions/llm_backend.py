"""
LLM Backend abstraction for The Edge Agent (TEA-RELEASE-004.5).

This module defines the abstract interface for LLM backends, enabling
pluggable implementations for local llama-cpp-python, API-based services,
and other LLM providers.

The LlmBackend ABC provides a consistent interface across:
- LocalLlmBackend (llama-cpp-python, this story)
- ApiLlmBackend (existing OpenAI-compatible APIs)
- Future backends (Ollama server, vLLM, etc.)

Example:
    >>> from the_edge_agent.actions.llm_backend import LlmBackend, LlmCallParams
    >>>
    >>> # Use appropriate backend
    >>> backend = create_llm_backend(settings)
    >>> result = backend.call(LlmCallParams(prompt="Hello"))
    >>> print(result.content)
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Optional


@dataclass
class LlmCallParams:
    """
    Parameters for raw LLM completion call.

    Attributes:
        prompt: The input prompt text for completion.
        max_tokens: Maximum tokens to generate (default: 100).
        temperature: Sampling temperature, higher = more random (default: 0.7).
        stop: Optional list of stop sequences to halt generation.
    """

    prompt: str
    max_tokens: int = 100
    temperature: float = 0.7
    stop: Optional[List[str]] = None


@dataclass
class LlmChatMessage:
    """
    A single message in a chat conversation.

    Follows OpenAI chat format for compatibility.

    Attributes:
        role: Message role - "system", "user", or "assistant".
        content: The message content text.
    """

    role: str
    content: str


@dataclass
class LlmCallResult:
    """
    Result from LLM generation.

    Attributes:
        content: The generated text content.
        model: The model identifier used.
        tokens_used: Optional total token count for the call.
        finish_reason: Optional reason for generation stop (e.g., "stop", "length").
    """

    content: str
    model: str
    tokens_used: Optional[int] = None
    finish_reason: Optional[str] = None


class LlmBackend(ABC):
    """
    Abstract base class for LLM backends.

    Provides a consistent interface for different LLM implementations:
    - Local models via llama-cpp-python (LocalLlmBackend)
    - API-based models (ApiLlmBackend)
    - Other backends (Ollama, vLLM, etc.)

    All backends must implement:
    - call(): Raw prompt completion
    - chat(): OpenAI-compatible chat completion
    - embed(): Text embedding generation
    - stream(): Streaming text generation with callback
    - stream_chat(): Streaming chat completion with callback

    Example:
        >>> class MyBackend(LlmBackend):
        ...     def call(self, params: LlmCallParams) -> LlmCallResult:
        ...         # Implementation
        ...         pass
        ...     # ... other methods
    """

    @abstractmethod
    def call(self, params: LlmCallParams) -> LlmCallResult:
        """
        Generate text completion from a raw prompt.

        This is the basic completion mode where you provide a prompt
        and the model continues it. For instruction-following models,
        prefer chat() which handles proper formatting.

        Args:
            params: LlmCallParams with prompt and generation settings.

        Returns:
            LlmCallResult with generated content and metadata.

        Example:
            >>> result = backend.call(LlmCallParams(
            ...     prompt="The capital of France is",
            ...     max_tokens=50
            ... ))
            >>> print(result.content)  # "Paris..."
        """
        pass

    @abstractmethod
    def chat(
        self,
        messages: List[Dict[str, str]],
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs: Any,
    ) -> LlmCallResult:
        """
        Generate chat completion using OpenAI-compatible message format.

        This is the recommended method for instruction-following models
        like Phi-4-mini and Gemma. The backend handles proper chat
        template formatting (ChatML, Gemma format, etc.).

        Args:
            messages: List of message dicts with 'role' and 'content' keys.
                     Roles: "system", "user", "assistant".
            max_tokens: Maximum tokens to generate.
            temperature: Sampling temperature.
            stop: Optional stop sequences.
            **kwargs: Additional backend-specific parameters.

        Returns:
            LlmCallResult with assistant's response.

        Example:
            >>> result = backend.chat([
            ...     {"role": "system", "content": "You are helpful."},
            ...     {"role": "user", "content": "What is 2+2?"}
            ... ])
            >>> print(result.content)  # "4"
        """
        pass

    @abstractmethod
    def embed(self, text: str) -> List[float]:
        """
        Generate embeddings for text.

        Converts input text to a dense vector representation suitable
        for similarity search, clustering, etc.

        Note: Not all models support embeddings. For local llama-cpp-python,
        the model must be loaded with embedding=True.

        Args:
            text: Input text to embed.

        Returns:
            List of floats representing the text embedding vector.

        Raises:
            NotImplementedError: If the backend doesn't support embeddings.

        Example:
            >>> embedding = backend.embed("Hello world")
            >>> print(len(embedding))  # e.g., 4096
        """
        pass

    @abstractmethod
    def stream(
        self,
        params: LlmCallParams,
        callback: Callable[[str], None],
    ) -> LlmCallResult:
        """
        Stream text generation with callback for each token/chunk.

        Similar to call() but invokes the callback for each generated
        chunk as it becomes available, enabling real-time output.

        Args:
            params: LlmCallParams with prompt and generation settings.
            callback: Function called with each generated text chunk.

        Returns:
            LlmCallResult with complete generated content.

        Example:
            >>> def on_token(chunk: str):
            ...     print(chunk, end="", flush=True)
            >>> result = backend.stream(
            ...     LlmCallParams(prompt="Write a poem"),
            ...     callback=on_token
            ... )
        """
        pass

    @abstractmethod
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
        Stream chat completion with callback for each token/chunk.

        Similar to chat() but invokes the callback for each generated
        chunk as it becomes available.

        Args:
            messages: List of message dicts with 'role' and 'content'.
            callback: Function called with each generated text chunk.
            max_tokens: Maximum tokens to generate.
            temperature: Sampling temperature.
            stop: Optional stop sequences.
            **kwargs: Additional backend-specific parameters.

        Returns:
            LlmCallResult with complete assistant response.
        """
        pass

    def is_available(self) -> bool:
        """
        Check if the backend is available and ready for use.

        Subclasses can override to perform availability checks
        (e.g., model file exists, API key configured).

        Returns:
            True if backend is ready, False otherwise.
        """
        return True

    def close(self) -> None:
        """
        Release resources held by the backend.

        Subclasses should override to clean up model memory,
        close connections, etc.
        """
        pass


# Export public API
__all__ = [
    "LlmBackend",
    "LlmCallParams",
    "LlmCallResult",
    "LlmChatMessage",
]
