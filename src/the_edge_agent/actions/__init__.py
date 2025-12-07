"""
Built-in Actions for YAMLEngine.

This package provides the built-in action registry for YAMLEngine workflows.
Actions are organized by domain:

- llm_actions: LLM calls, streaming, retry, and tool calling (TEA-BUILTIN-001.2)
- core_actions: HTTP, file operations, notifications, checkpoints
- data_actions: JSON/CSV parsing, transformation, validation (TEA-BUILTIN-003.2)
- observability_actions: Tracing spans and event logging (TEA-BUILTIN-001.3)
- memory_actions: Key-value storage with TTL (TEA-BUILTIN-001.1)
- ltm_actions: Long-term persistent key-value storage with FTS5 (TEA-BUILTIN-001.4)
- graph_actions: Graph database with Datalog and HNSW vectors (TEA-BUILTIN-001.4)
- web_actions: Web scraping, crawling, and search via Firecrawl/Perplexity (TEA-BUILTIN-002.1)
- rag_actions: Embedding creation, vector storage, and semantic search (TEA-BUILTIN-002.2)
- tools_actions: Bridges to CrewAI, MCP, and LangChain tool ecosystems (TEA-BUILTIN-002.3)
- code_actions: Sandboxed Python code execution (TEA-BUILTIN-003.1)

Usage:
    >>> from the_edge_agent.actions import build_actions_registry
    >>>
    >>> # Build registry for an engine instance
    >>> registry = build_actions_registry(engine)
    >>>
    >>> # Actions available as 'domain.action' and 'actions.domain_action'
    >>> registry['llm.call'](state, model="gpt-4", messages=[...])
    >>> registry['json.parse'](state, text='{"key": "value"}')

Custom Action Registration:
    Each module provides a `register_actions(registry, engine)` function
    that adds actions to the registry. The engine parameter provides access
    to shared resources like memory backends and trace contexts.
"""

from typing import Any, Callable, Dict

from .llm_actions import register_actions as register_llm
from .core_actions import register_actions as register_core
from .data_actions import register_actions as register_data
from .observability_actions import register_actions as register_observability
from .memory_actions import register_actions as register_memory
from .ltm_actions import register_actions as register_ltm
from .graph_actions import register_actions as register_graph
from .web_actions import register_actions as register_web
from .rag_actions import register_actions as register_rag
from .tools_actions import register_actions as register_tools
from .code_actions import register_actions as register_code


def build_actions_registry(engine: Any) -> Dict[str, Callable]:
    """
    Build the complete actions registry for a YAMLEngine instance.

    This function creates a new registry and populates it with all built-in
    actions. Each action module's register_actions function is called with
    the registry and engine reference, allowing actions to access engine
    internals like memory backends and trace contexts.

    Args:
        engine: YAMLEngine instance that actions may need to access for
                memory backends, trace contexts, and other shared resources.

    Returns:
        Dictionary mapping action names to callable functions.
        Actions are registered under both 'domain.action' and 'actions.domain_action'
        naming conventions for backward compatibility.

    Example:
        >>> from the_edge_agent import YAMLEngine
        >>> engine = YAMLEngine()
        >>> registry = build_actions_registry(engine)
        >>> print(list(registry.keys())[:5])
        ['llm.call', 'actions.llm_call', 'llm.stream', ...]
    """
    registry: Dict[str, Callable] = {}

    # Register all action modules
    register_llm(registry, engine)
    register_core(registry, engine)
    register_data(registry, engine)
    register_observability(registry, engine)
    register_memory(registry, engine)
    register_ltm(registry, engine)
    register_graph(registry, engine)
    register_web(registry, engine)
    register_rag(registry, engine)
    register_tools(registry, engine)
    register_code(registry, engine)

    return registry


__all__ = ['build_actions_registry']
