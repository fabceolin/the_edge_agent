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
- storage_actions: Cloud storage operations via fsspec (S3, GCS, Azure, etc.) (TEA-BUILTIN-004.1)
- web_actions: Web scraping, crawling, and search via Firecrawl/Perplexity (TEA-BUILTIN-002.1)
- rag_actions: Embedding creation, vector storage, and semantic search (TEA-BUILTIN-002.2)
- tools_actions: Bridges to CrewAI, MCP, and LangChain tool ecosystems (TEA-BUILTIN-002.3)
- code_actions: Sandboxed Python code execution (TEA-BUILTIN-003.1)
- schema_actions: Schema merge and manipulation (TEA-BUILTIN-008.3)
- llamaextract_actions: Document extraction via LlamaExtract (TEA-BUILTIN-008.1)
- cache_actions: Cache and memoization with LTM backend (TEA-BUILTIN-010)
- validation_actions: Generic extraction validation with Prolog/probes (TEA-YAML-004)
- retry_actions: General-purpose retry loop with correction (TEA-YAML-005)
- academic_actions: Academic research via PubMed and ArXiv APIs (TEA-KIROKU-001)
- text_actions: Text processing including citation insertion (TEA-KIROKU-002)
- neo4j_trigger_actions: Neo4j APOC trigger management (TEA-BUILTIN-001.7.5)
- neo4j_gds_actions: Neo4j GDS graph analytics algorithms (TEA-BUILTIN-001.7.4)

Firebase Agent Memory Infrastructure (TEA-BUILTIN-006):
- catalog_actions: Data catalog for tables, files, and snapshots
- cloud_memory_actions: Cloud storage with metadata management
- search_actions: SQL and full-text search via QueryEngine
- vector_actions: Vector similarity search via VectorIndex
- session_actions: Session lifecycle with archive-based expiration
- context_actions: Context assembly with relevance ranking

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
from .storage_actions import register_actions as register_storage
from .web_actions import register_actions as register_web
from .rag_actions import register_actions as register_rag
from .tools_actions import register_actions as register_tools
from .code_actions import register_actions as register_code
from .schema_actions import register_actions as register_schema
from .llamaextract_actions import register_actions as register_llamaextract
from .cache_actions import register_actions as register_cache
from .cache_actions import register_jinja_filters as register_cache_jinja_filters

# TEA-YAML-004: Generic Extraction Validation
from .validation_actions import register_actions as register_validation

# TEA-YAML-005: General-Purpose Retry Loop Action
from .retry_actions import register_actions as register_retry

# TEA-KIROKU-001: Academic Research Actions
from .academic_actions import register_actions as register_academic

# TEA-KIROKU-002: Text Processing Actions (Citation Insertion)
from .text_actions import register_actions as register_text

# TEA-BUILTIN-001.7.5: Neo4j APOC Trigger Actions
from .neo4j_trigger_actions import register_actions as register_neo4j_triggers

# TEA-BUILTIN-001.7.4: Neo4j GDS Actions
from .neo4j_gds_actions import register_actions as register_neo4j_gds

# TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
from .catalog_actions import register_actions as register_catalog
from .cloud_memory_actions import register_actions as register_cloud_memory
from .search_actions import register_actions as register_search
from .vector_actions import register_actions as register_vector
from .session_actions import register_actions as register_session
from .context_actions import register_actions as register_context
from .data_tabular_actions import register_actions as register_data_tabular


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
    register_storage(registry, engine)
    register_web(registry, engine)
    register_rag(registry, engine)
    register_tools(registry, engine)
    register_code(registry, engine)
    register_schema(registry, engine)
    register_llamaextract(registry, engine)
    register_cache(registry, engine)

    # TEA-YAML-004: Generic Extraction Validation
    register_validation(registry, engine)

    # TEA-YAML-005: General-Purpose Retry Loop Action
    register_retry(registry, engine)

    # TEA-KIROKU-001: Academic Research Actions
    register_academic(registry, engine)

    # TEA-KIROKU-002: Text Processing Actions
    register_text(registry, engine)

    # TEA-BUILTIN-001.7.5: Neo4j APOC Trigger Actions
    register_neo4j_triggers(registry, engine)

    # TEA-BUILTIN-001.7.4: Neo4j GDS Actions
    register_neo4j_gds(registry, engine)

    # TEA-BUILTIN-006: Firebase Agent Memory Infrastructure
    register_catalog(registry, engine)
    register_cloud_memory(registry, engine)
    register_search(registry, engine)
    register_vector(registry, engine)
    register_session(registry, engine)
    register_context(registry, engine)
    register_data_tabular(registry, engine)

    return registry


__all__ = ["build_actions_registry", "register_cache_jinja_filters"]
