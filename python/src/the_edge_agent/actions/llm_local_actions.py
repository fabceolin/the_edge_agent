"""
Local LLM Actions for YAMLEngine (TEA-RELEASE-004.5).

This module registers local LLM actions using the LlmBackend abstraction.
It integrates with yaml_engine to provide llm.call, llm.chat, llm.stream,
and llm.embed actions that use local llama-cpp-python when configured.

The actions use lazy backend initialization to avoid loading the model
until the first LLM action is executed.

Backend Selection Logic:
1. If settings.llm.backend == "local", try LocalLlmBackend
2. If local fails (import, model not found), fall back to ApiLlmBackend
3. If settings.llm.backend == "api" or unset, use existing llm.call action

Example YAML:
    settings:
      llm:
        backend: local
        model_path: ~/.cache/tea/models/phi4-mini.gguf
        n_ctx: 128000

    nodes:
      - name: generate
        action: llm.chat
        params:
          messages:
            - role: user
              content: "{{ state.question }}"
          max_tokens: 500
"""

import logging
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register local LLM actions into the provided registry.

    This replaces the standard llm.call, llm.chat, llm.stream actions with
    versions that use the LlmBackend abstraction, enabling local model support.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing settings and shared resources
    """

    # Lazy backend initialization
    _llm_backend: Optional[Any] = None

    def _get_llm_backend():
        """Get or create the LLM backend based on engine settings."""
        nonlocal _llm_backend

        if _llm_backend is not None:
            return _llm_backend

        # Build settings from engine's llm_settings
        settings = {"llm": getattr(engine, "llm_settings", {}).copy()}

        # Check for explicit backend setting
        llm_config = settings.get("llm", {})

        # If backend is explicitly "local", use backend factory
        backend_type = llm_config.get("backend", "api")

        if backend_type == "local":
            try:
                from .llm_backend_factory import create_llm_backend

                _llm_backend = create_llm_backend(settings, engine)
                logger.info(f"Using LLM backend: {type(_llm_backend).__name__}")
                return _llm_backend
            except Exception as e:
                logger.warning(f"Failed to create local LLM backend: {e}")
                logger.info("Falling back to API backend via existing llm.call")
                return None

        # For API backend, return None to signal use of existing llm.call
        return None

    def llm_call_local(
        state: Dict[str, Any],
        prompt: Optional[str] = None,
        messages: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        LLM completion using local or API backend.

        Uses the configured backend (local or API) based on settings.llm.backend.
        If local backend is configured but unavailable, falls back to API.

        Args:
            state: Current state dictionary
            prompt: Raw prompt text (for completion mode)
            messages: Chat messages (for chat mode, takes precedence)
            max_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            stop: Optional stop sequences
            **kwargs: Additional parameters passed to backend

        Returns:
            {"content": str, "model": str, "tokens_used": int|None}

        Example YAML:
            - name: generate
              action: llm.call
              params:
                prompt: "Complete this: Hello"
                max_tokens: 50
        """
        backend = _get_llm_backend()

        if backend is None:
            # Use existing llm.call action (API-based)
            if "llm.call" in registry:
                return registry["llm.call"](
                    state=state,
                    prompt=prompt,
                    messages=messages or [{"role": "user", "content": prompt}],
                    max_tokens=max_tokens,
                    temperature=temperature,
                    stop=stop,
                    **kwargs,
                )
            else:
                return {"error": "No LLM backend available", "success": False}

        # Use backend
        try:
            if messages:
                # Chat mode
                result = backend.chat(
                    messages=messages,
                    max_tokens=max_tokens,
                    temperature=temperature,
                    stop=stop,
                    **kwargs,
                )
            elif prompt:
                # Raw completion mode
                from .llm_backend import LlmCallParams

                result = backend.call(
                    LlmCallParams(
                        prompt=prompt,
                        max_tokens=max_tokens,
                        temperature=temperature,
                        stop=stop,
                    )
                )
            else:
                return {
                    "error": "Either 'prompt' or 'messages' is required",
                    "success": False,
                }

            return {
                "content": result.content,
                "model": result.model,
                "tokens_used": result.tokens_used,
            }

        except Exception as e:
            logger.error(f"LLM call failed: {e}")
            return {"error": str(e), "success": False}

    def llm_chat_local(
        state: Dict[str, Any],
        messages: List[Dict[str, str]],
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Chat completion using OpenAI-compatible format.

        This is the recommended method for instruction-following models.
        The backend handles proper chat template formatting.

        Args:
            state: Current state dictionary
            messages: List of message dicts with 'role' and 'content'
            max_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            stop: Optional stop sequences
            **kwargs: Additional backend parameters

        Returns:
            {"content": str, "model": str, "tokens_used": int|None}

        Example YAML:
            - name: chat
              action: llm.chat
              params:
                messages:
                  - role: system
                    content: "You are a helpful assistant."
                  - role: user
                    content: "{{ state.question }}"
                max_tokens: 500
        """
        backend = _get_llm_backend()

        if backend is None:
            # Use existing llm.call action with messages
            if "llm.call" in registry:
                # Get model from engine settings
                model = getattr(engine, "llm_settings", {}).get("model", "gpt-4o-mini")
                return registry["llm.call"](
                    state=state,
                    model=model,
                    messages=messages,
                    max_tokens=max_tokens,
                    temperature=temperature,
                    stop=stop,
                    **kwargs,
                )
            else:
                return {"error": "No LLM backend available", "success": False}

        try:
            result = backend.chat(
                messages=messages,
                max_tokens=max_tokens,
                temperature=temperature,
                stop=stop,
                **kwargs,
            )

            return {
                "content": result.content,
                "model": result.model,
                "tokens_used": result.tokens_used,
            }

        except Exception as e:
            logger.error(f"LLM chat failed: {e}")
            return {"error": str(e), "success": False}

    def llm_stream_local(
        state: Dict[str, Any],
        prompt: Optional[str] = None,
        messages: Optional[List[Dict[str, str]]] = None,
        max_tokens: int = 100,
        temperature: float = 0.7,
        stop: Optional[List[str]] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Streaming LLM generation with token-by-token output.

        Streams output to stdout and returns the complete response.
        Uses local backend if configured, otherwise falls back to API.

        Args:
            state: Current state dictionary
            prompt: Raw prompt text (for completion mode)
            messages: Chat messages (for chat mode, takes precedence)
            max_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            stop: Optional stop sequences
            **kwargs: Additional parameters

        Returns:
            {"content": str, "model": str, "streamed": True, "chunk_count": int}

        Example YAML:
            - name: stream_response
              action: llm.stream
              params:
                prompt: "Write a haiku about coding"
                max_tokens: 100
        """
        backend = _get_llm_backend()

        if backend is None:
            # Use existing llm.stream action
            if "llm.stream" in registry:
                model = getattr(engine, "llm_settings", {}).get("model", "gpt-4o-mini")
                return registry["llm.stream"](
                    state=state,
                    model=model,
                    messages=messages or [{"role": "user", "content": prompt}],
                    max_tokens=max_tokens,
                    temperature=temperature,
                    stop=stop,
                    **kwargs,
                )
            else:
                return {"error": "No LLM backend available", "success": False}

        try:
            chunk_count = 0

            def stream_callback(chunk: str):
                nonlocal chunk_count
                print(chunk, end="", flush=True)
                chunk_count += 1

            if messages:
                result = backend.stream_chat(
                    messages=messages,
                    callback=stream_callback,
                    max_tokens=max_tokens,
                    temperature=temperature,
                    stop=stop,
                    **kwargs,
                )
            elif prompt:
                from .llm_backend import LlmCallParams

                result = backend.stream(
                    LlmCallParams(
                        prompt=prompt,
                        max_tokens=max_tokens,
                        temperature=temperature,
                        stop=stop,
                    ),
                    callback=stream_callback,
                )
            else:
                return {
                    "error": "Either 'prompt' or 'messages' is required",
                    "success": False,
                }

            print()  # Newline after streaming

            return {
                "content": result.content,
                "model": result.model,
                "streamed": True,
                "chunk_count": chunk_count,
            }

        except Exception as e:
            logger.error(f"LLM stream failed: {e}")
            return {"error": str(e), "success": False}

    def llm_embed_local(
        state: Dict[str, Any],  # noqa: ARG001
        text: str,
        **kwargs,  # noqa: ARG001
    ) -> Dict[str, Any]:
        """
        Generate text embeddings using local or API backend.

        Note: For local backend, the model must be loaded with embedding=True.

        Args:
            state: Current state dictionary
            text: Input text to embed

        Returns:
            {"embedding": List[float], "dimensions": int}

        Example YAML:
            - name: embed_document
              action: llm.embed
              params:
                text: "{{ state.document }}"
        """
        backend = _get_llm_backend()

        if backend is None:
            # Embeddings require local backend or explicit API backend
            # For API, would need to implement via OpenAI embeddings
            return {
                "error": "Embeddings require local backend with embedding=True or explicit API configuration",
                "success": False,
            }

        try:
            embedding = backend.embed(text)

            return {
                "embedding": embedding,
                "dimensions": len(embedding),
            }

        except Exception as e:
            logger.error(f"LLM embed failed: {e}")
            return {"error": str(e), "success": False}

    # Register actions with local suffix (to not override existing llm.call)
    # These are available when backend is explicitly set to "local"
    registry["llm.local.call"] = llm_call_local
    registry["llm.local.chat"] = llm_chat_local
    registry["llm.local.stream"] = llm_stream_local
    registry["llm.local.embed"] = llm_embed_local

    # Also register as llm.chat (new action not in existing registry)
    # This allows: action: llm.chat to work with both local and API backends
    registry["llm.chat"] = llm_chat_local
    registry["llm.embed"] = llm_embed_local

    logger.debug("Registered local LLM actions: llm.chat, llm.embed, llm.local.*")
