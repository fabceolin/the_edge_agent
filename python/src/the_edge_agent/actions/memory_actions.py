"""
Memory Actions for YAMLEngine (TEA-BUILTIN-001.1).

This module provides key-value storage actions with TTL support for
YAMLEngine workflows. Actions integrate with the engine's memory backend
for persistent agent memory across workflow execution.

Actions:
    - memory.store: Store a key-value pair with optional TTL
    - memory.retrieve: Retrieve a value by key
    - memory.summarize: Summarize conversation history using LLM

Example:
    >>> # Store with TTL
    >>> result = registry['memory.store'](
    ...     state={},
    ...     key="user_preference",
    ...     value={"theme": "dark"},
    ...     ttl=3600  # 1 hour
    ... )
    >>> print(result['stored'])  # True

    >>> # Retrieve value
    >>> result = registry['memory.retrieve'](
    ...     state={},
    ...     key="user_preference",
    ...     default={"theme": "light"}
    ... )
    >>> print(result['value'])  # {'theme': 'dark'}

    >>> # Namespace isolation
    >>> registry['memory.store'](
    ...     state={},
    ...     key="count",
    ...     value=10,
    ...     namespace="user_123"
    ... )
"""

from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register memory actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing memory backend
    """

    def memory_store(state, key, value, ttl=None, namespace="default", **kwargs):
        """
        Store a key-value pair in memory with optional TTL.

        Args:
            state: Current state dictionary
            key: The key to store the value under
            value: The value to store (must be pickle-serializable)
            ttl: Time-to-live in seconds (None for no expiration)
            namespace: Namespace for key isolation (default: "default")

        Returns:
            {"stored": True, "key": str, "namespace": str} on success
            {"stored": False, "key": str, "error": str} on failure
        """
        if key is None:
            return {
                "stored": False,
                "key": None,
                "error": "Key is required"
            }

        try:
            success = engine._memory_backend.store(
                key=str(key),
                value=value,
                ttl=ttl,
                namespace=str(namespace)
            )

            return {
                "stored": success,
                "key": str(key),
                "namespace": str(namespace)
            }
        except Exception as e:
            return {
                "stored": False,
                "key": str(key),
                "error": str(e)
            }

    registry['memory.store'] = memory_store
    registry['actions.memory_store'] = memory_store

    def memory_retrieve(state, key, default=None, namespace="default", **kwargs):
        """
        Retrieve a value from memory by key.

        Args:
            state: Current state dictionary
            key: The key to retrieve
            default: Default value to return if key not found or expired
            namespace: Namespace to look in (default: "default")

        Returns:
            {"value": any, "found": True, "key": str} if found
            {"value": default, "found": False, "key": str} if not found/expired
        """
        if key is None:
            return {
                "value": default,
                "found": False,
                "key": None,
                "error": "Key is required"
            }

        try:
            value = engine._memory_backend.retrieve(
                key=str(key),
                namespace=str(namespace)
            )

            if value is None:
                return {
                    "value": default,
                    "found": False,
                    "key": str(key)
                }

            return {
                "value": value,
                "found": True,
                "key": str(key)
            }
        except Exception as e:
            return {
                "value": default,
                "found": False,
                "key": str(key),
                "error": str(e)
            }

    registry['memory.retrieve'] = memory_retrieve
    registry['actions.memory_retrieve'] = memory_retrieve

    def memory_summarize(state, messages_key, max_tokens=1000, model=None, **kwargs):
        """
        Summarize conversation history using LLM to fit token windows.

        Uses the internal llm.call action for summarization. Implements
        a sliding window + summarization strategy for long conversations.

        Args:
            state: Current state dictionary
            messages_key: Key in state containing messages list
            max_tokens: Maximum tokens for the summary (default: 1000)
            model: LLM model to use (default: gpt-3.5-turbo)

        Returns:
            {
                "summary": str,
                "original_count": int,
                "token_estimate": int,
                "success": True
            }
            Or {"error": str, "success": False} on failure
        """
        # Get messages from state
        messages = state.get(messages_key) if isinstance(state, dict) else None

        if messages is None:
            return {
                "error": f"Messages key '{messages_key}' not found in state",
                "success": False
            }

        if not isinstance(messages, list):
            return {
                "error": f"Messages at '{messages_key}' must be a list",
                "success": False
            }

        if not messages:
            return {
                "summary": "",
                "original_count": 0,
                "token_estimate": 0,
                "success": True
            }

        # Get llm.call action from engine's registry
        llm_call = engine.actions_registry.get('llm.call')
        if llm_call is None:
            return {
                "error": "llm.call action not available. Ensure OpenAI is configured.",
                "success": False
            }

        # Format messages for summarization
        message_texts = []
        for msg in messages:
            if isinstance(msg, dict):
                role = msg.get('role', 'user')
                content = msg.get('content', str(msg))
                message_texts.append(f"{role}: {content}")
            else:
                message_texts.append(str(msg))

        conversation_text = "\n".join(message_texts)
        original_count = len(messages)

        # Estimate tokens (rough: ~4 chars per token)
        original_token_estimate = len(conversation_text) // 4

        # If already within limit, return as-is
        if original_token_estimate <= max_tokens:
            return {
                "summary": conversation_text,
                "original_count": original_count,
                "token_estimate": original_token_estimate,
                "success": True
            }

        # Build summarization prompt
        summarization_messages = [
            {
                "role": "system",
                "content": f"Summarize the following conversation in approximately {max_tokens} tokens or less. "
                          "Preserve key information, decisions, and context needed for continuation."
            },
            {
                "role": "user",
                "content": conversation_text
            }
        ]

        try:
            # Call LLM for summarization
            result = llm_call(
                state=state,
                model=model or "gpt-3.5-turbo",
                messages=summarization_messages,
                temperature=0.3  # Lower temperature for factual summarization
            )

            summary = result.get('content', '')
            token_estimate = len(summary) // 4

            return {
                "summary": summary,
                "original_count": original_count,
                "token_estimate": token_estimate,
                "success": True
            }
        except ImportError:
            return {
                "error": "OpenAI library not installed. Install with: pip install openai",
                "success": False
            }
        except Exception as e:
            return {
                "error": f"LLM summarization failed: {str(e)}",
                "success": False
            }

    registry['memory.summarize'] = memory_summarize
    registry['actions.memory_summarize'] = memory_summarize
