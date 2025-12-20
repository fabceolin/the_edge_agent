"""
Context Assembly Actions for TEA YAMLEngine.

Migrated from: firebase/functions-agents/actions/context.py
Story: TEA-BUILTIN-006 (Firebase Agent Memory Layer)

Provides generic context assembly that retrieves memory from configurable layers,
enriches with tabular data, and respects token limits using relevance ranking.

Dependencies (via state or kwargs):
- cloud_list_fn: memory.cloud_list action
- cloud_retrieve_fn: memory.cloud_retrieve action
- data_query_fn: data.query action (optional)
- vector_search_fn: memory.vector_search action (optional)
- config_get_scopes_fn: config.get_scopes action (optional)

Architecture:
    ┌─────────────────────────────────────────────────────────────────────┐
    │                    CONTEXT ASSEMBLY PIPELINE                         │
    │                    context.assemble(config, query, variables)        │
    ├─────────────────────────────────────────────────────────────────────┤
    │                                                                      │
    │  Step 1: Load scope config                                          │
    │  Step 2: Resolve paths for each layer                               │
    │  Step 3: Retrieve memory files from each resolved path               │
    │  Step 4: Retrieve tabular data via SQL                               │
    │  Step 5: Rank by relevance (if query provided)                       │
    │  Step 6: Token counting and truncation                               │
    │  Step 7: Format and return assembled context                         │
    │                                                                      │
    └─────────────────────────────────────────────────────────────────────┘

Actions:
- context.assemble: Main context assembly action
"""

import logging
import re
from typing import Any, Callable, Dict, List, Optional, Tuple

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONSTANTS
# =============================================================================

# Default token encoding for OpenAI models
ENCODING_NAME = "cl100k_base"

# Scoring weights (RX.12 AC5)
RELEVANCE_WEIGHT = 0.7
PRIORITY_WEIGHT = 0.3

# Default configuration
DEFAULT_MAX_TOKENS = 8000
DEFAULT_RELEVANCE_SCORE = 0.5  # Used when VSS doesn't return a match

# SQL variable pattern for substitution
SQL_VAR_PATTERN = re.compile(r'\{\{(\w+)\}\}')


# =============================================================================
# LAZY ENCODING INITIALIZATION
# =============================================================================

_encoding = None


def _get_encoding():
    """
    Lazily initialize tiktoken encoding.

    This avoids the startup cost if context assembly is not used.
    Falls back to a simple word-based estimation if tiktoken is not available.
    """
    global _encoding
    if _encoding is None:
        try:
            import tiktoken
            _encoding = tiktoken.get_encoding(ENCODING_NAME)
        except ImportError:
            logger.warning("tiktoken not available, using word-based token estimation")
            _encoding = "word_estimation"
    return _encoding


# =============================================================================
# DEPENDENCY INJECTION HELPERS
# =============================================================================

def _get_action_function(
    state: Dict[str, Any],
    kwargs: Dict[str, Any],
    key: str,
    action_name: str
) -> Optional[Callable]:
    """
    Get an action function from state or kwargs.

    Resolution order:
    1. kwargs[key] (explicit parameter)
    2. state['_action_registry'][action_name]
    3. state['action_registry'][action_name]
    4. state[key]

    Args:
        state: Current agent state
        kwargs: Action keyword arguments
        key: Key name (e.g., 'cloud_list_fn')
        action_name: Action name in registry (e.g., 'memory.cloud_list')

    Returns:
        Action function or None if not available
    """
    # Check kwargs first
    fn = kwargs.get(key)
    if fn is not None:
        return fn

    # Check action registry (private)
    registry = state.get('_action_registry')
    if registry and action_name in registry:
        return registry[action_name]

    # Check action registry (public)
    registry = state.get('action_registry')
    if registry and action_name in registry:
        return registry[action_name]

    # Check direct state
    fn = state.get(key)
    if fn is not None:
        return fn

    return None


# =============================================================================
# TOKEN MANAGEMENT (AC6)
# =============================================================================

def _count_tokens(text: str) -> int:
    """
    Count tokens using tiktoken cl100k_base encoding.

    Falls back to word-based estimation if tiktoken is not available.

    Args:
        text: Text to count tokens for

    Returns:
        Token count
    """
    if not text:
        return 0

    encoding = _get_encoding()

    if encoding == "word_estimation":
        # Simple word-based estimation: ~1.3 tokens per word
        words = len(text.split())
        return int(words * 1.3)

    return len(encoding.encode(text))


def _truncate_to_limit(
    items: List[Dict],
    max_tokens: int
) -> Tuple[List[Dict], bool]:
    """
    Truncate items to fit within token limit.

    Items should already be sorted by final_score (descending).

    Args:
        items: List of memory items with 'content' field
        max_tokens: Maximum total tokens allowed

    Returns:
        (truncated_items, was_truncated)
    """
    result = []
    total_tokens = 0
    truncated = False

    for item in items:
        content = item.get("content", "")
        item_tokens = _count_tokens(content)
        item["tokens"] = item_tokens

        if total_tokens + item_tokens <= max_tokens:
            result.append(item)
            total_tokens += item_tokens
        else:
            truncated = True
            # Don't add more items once we exceed limit

    return result, truncated


# =============================================================================
# VARIABLE SUBSTITUTION
# =============================================================================

def _substitute_variables(template: str, variables: Dict[str, str]) -> str:
    """
    Substitute {{variable}} placeholders in a template string.

    Args:
        template: String with {{var}} placeholders
        variables: Dict of variable values

    Returns:
        String with substituted values
    """
    if not variables:
        return template

    def replace_match(match):
        var_name = match.group(1)
        return str(variables.get(var_name, match.group(0)))

    return SQL_VAR_PATTERN.sub(replace_match, template)


# =============================================================================
# LAYER RESOLUTION (AC2)
# =============================================================================

def _resolve_layers(
    state: Dict[str, Any],
    layers_config: List[Dict],
    variables: Dict[str, str],
    scope_defs: Dict[str, Dict]
) -> List[Dict]:
    """
    Resolve layer configurations into concrete paths.

    Args:
        state: Agent state
        layers_config: List of layer config dicts with 'scope' and optional vars
        variables: Variable values for substitution
        scope_defs: Scope definitions from config.get_scopes

    Returns:
        List of resolved layer dicts with 'scope', 'path', 'priority'
    """
    resolved = []

    for layer in layers_config:
        scope_id = layer.get("scope")
        if not scope_id:
            logger.warning("Layer missing 'scope' field, skipping")
            continue

        scope_def = scope_defs.get(scope_id)
        if not scope_def:
            logger.warning(f"Scope '{scope_id}' not found in config, skipping")
            continue

        # Merge layer-specific variables with global variables
        layer_vars = {**variables}
        for key in ["entity_id", "directory", "year", "month"]:
            if key in layer:
                layer_vars[key] = layer[key]

        # Get path pattern from scope definition
        path_pattern = scope_def.get("path_pattern", "")
        if not path_pattern:
            logger.warning(f"Scope '{scope_id}' has no path_pattern, skipping")
            continue

        # Substitute variables in path pattern
        # Convert {{var}} to {var} format for config.resolve_path compatibility
        resolved_path = path_pattern
        for var_name, var_value in layer_vars.items():
            resolved_path = resolved_path.replace(f"{{{var_name}}}", str(var_value))

        # Check for unresolved variables
        if "{" in resolved_path and "}" in resolved_path:
            # There are still unresolved variables
            unresolved = re.findall(r'\{(\w+)\}', resolved_path)
            logger.warning(
                f"Layer '{scope_id}' has unresolved variables: {unresolved}"
            )
            continue

        resolved.append({
            "scope": scope_id,
            "path": resolved_path,
            "priority": scope_def.get("priority", 0.5)
        })

    return resolved


# =============================================================================
# MEMORY RETRIEVAL (AC3)
# =============================================================================

def _retrieve_layer_memory(
    state: Dict[str, Any],
    scope: str,
    path: str,
    cloud_list_fn: Callable,
    cloud_retrieve_fn: Callable
) -> List[Dict]:
    """
    Retrieve memory files for a resolved layer path.

    Args:
        state: Agent state
        scope: Scope identifier
        path: Resolved path pattern (may contain * wildcard)
        cloud_list_fn: memory.cloud_list function
        cloud_retrieve_fn: memory.cloud_retrieve function

    Returns:
        List of memory items with 'path', 'content', 'scope' fields
    """
    items = []

    # Convert path to prefix (remove trailing /*)
    prefix = path.rstrip("/*").rstrip("*")

    try:
        # List files matching the path pattern
        list_result = cloud_list_fn(
            state,
            prefix=prefix,
            status="active",  # Exclude archived/invalid
            limit=100
        )

        if not list_result.get("success"):
            logger.warning(
                f"Failed to list memory for scope '{scope}': "
                f"{list_result.get('error')}"
            )
            return items

        files = list_result.get("files", [])

        # Retrieve content for each file
        for file_info in files:
            file_path = file_info.get("path")
            if not file_path:
                continue

            try:
                retrieve_result = cloud_retrieve_fn(state, path=file_path)

                if not retrieve_result.get("success"):
                    logger.warning(
                        f"Failed to retrieve '{file_path}': "
                        f"{retrieve_result.get('error')}"
                    )
                    continue

                items.append({
                    "path": file_path,
                    "content": retrieve_result.get("content", ""),
                    "scope": scope,
                    "metadata": retrieve_result.get("metadata", {})
                })

            except Exception as e:
                logger.warning(f"Error retrieving '{file_path}': {e}")
                continue

    except Exception as e:
        logger.error(f"Error listing memory for scope '{scope}': {e}")

    return items


# =============================================================================
# TABULAR ENRICHMENT (AC4)
# =============================================================================

def _retrieve_tabular(
    state: Dict[str, Any],
    tabular_config: List[Dict],
    variables: Dict[str, str],
    data_query_fn: Optional[Callable]
) -> List[Dict]:
    """
    Retrieve tabular data via SQL queries.

    Args:
        state: Agent state
        tabular_config: List of {table, sql} dicts
        variables: Variable values for SQL substitution
        data_query_fn: data.query function (or None to skip)

    Returns:
        List of tabular results with 'table', 'rows', 'data' fields
    """
    if not data_query_fn:
        logger.debug("No data_query_fn provided, skipping tabular retrieval")
        return []

    results = []

    for tab in tabular_config:
        table_name = tab.get("table")
        sql_template = tab.get("sql")

        if not table_name or not sql_template:
            logger.warning("Tabular config missing 'table' or 'sql', skipping")
            continue

        # Substitute variables in SQL
        sql = _substitute_variables(sql_template, variables)

        try:
            query_result = data_query_fn(state, table=table_name, sql=sql)

            if not query_result.get("success"):
                logger.warning(
                    f"Tabular query failed for '{table_name}': "
                    f"{query_result.get('error')}"
                )
                continue

            rows = query_result.get("rows", [])
            results.append({
                "table": table_name,
                "rows": len(rows),
                "data": rows
            })

        except Exception as e:
            logger.warning(f"Error querying table '{table_name}': {e}")
            continue

    return results


# =============================================================================
# RELEVANCE RANKING (AC5, AC8)
# =============================================================================

def _rank_by_relevance(
    state: Dict[str, Any],
    items: List[Dict],
    query: str,
    vector_search_fn: Optional[Callable]
) -> List[Dict]:
    """
    Rank items by combined relevance and priority score.

    Final Score = (Relevance * 0.7) + (Priority * 0.3)

    Args:
        state: Agent state
        items: Memory items with 'content', 'scope', 'priority' fields
        query: Search query for relevance scoring
        vector_search_fn: memory.vector_search function (or None for fallback)

    Returns:
        Items sorted by final_score descending
    """
    relevance_map = {}

    # Try to get relevance scores from VSS
    if vector_search_fn and query:
        try:
            vss_result = vector_search_fn(state, query=query, top_k=100)

            if vss_result.get("success"):
                for result in vss_result.get("results", []):
                    file_path = result.get("file_path")
                    score = result.get("similarity_score", 0.0)
                    if file_path:
                        relevance_map[file_path] = score
            else:
                logger.warning(
                    f"VSS returned error, falling back to priority-only: "
                    f"{vss_result.get('error')}"
                )
        except Exception as e:
            logger.warning(f"VSS failed, falling back to priority-only: {e}")

    # Calculate final scores for each item
    for item in items:
        path = item.get("path", "")
        priority = item.get("priority", 0.5)

        # Get relevance score or use default
        relevance = relevance_map.get(path, DEFAULT_RELEVANCE_SCORE)

        item["relevance_score"] = relevance
        item["final_score"] = (relevance * RELEVANCE_WEIGHT) + (priority * PRIORITY_WEIGHT)

    # Sort by final score descending
    items.sort(key=lambda x: x.get("final_score", 0), reverse=True)

    return items


# =============================================================================
# CONTEXT FORMATTING (AC7)
# =============================================================================

def _format_context(memory_items: List[Dict], tabular_results: List[Dict]) -> str:
    """
    Format assembled context with clear sections.

    Output format:
    # Memory: {scope}
    ## {filename}
    Path: {path}

    {content}

    # Tabular Data: {table}
    | col1 | col2 | ...
    | --- | --- | ...
    | val1 | val2 | ...

    Args:
        memory_items: List of memory items with 'path', 'content', 'scope'
        tabular_results: List of tabular results with 'table', 'data'

    Returns:
        Formatted context string
    """
    sections = []

    # Group memory by scope
    by_scope: Dict[str, List[Dict]] = {}
    for item in memory_items:
        scope = item.get("scope", "unknown")
        if scope not in by_scope:
            by_scope[scope] = []
        by_scope[scope].append(item)

    # Format memory sections
    for scope, items in by_scope.items():
        section = f"# Memory: {scope}\n\n"
        for item in items:
            filename = item.get("path", "").split("/")[-1]
            section += f"## {filename}\n"
            section += f"Path: {item.get('path', '')}\n\n"
            section += f"{item.get('content', '')}\n\n"
        sections.append(section)

    # Format tabular sections
    for tab in tabular_results:
        data = tab.get("data", [])
        if not data:
            continue

        section = f"# Tabular Data: {tab.get('table', 'unknown')}\n\n"

        # Build markdown table
        headers = list(data[0].keys())
        section += "| " + " | ".join(str(h) for h in headers) + " |\n"
        section += "| " + " | ".join(["---"] * len(headers)) + " |\n"

        for row in data:
            values = [str(row.get(h, "")) for h in headers]
            section += "| " + " | ".join(values) + " |\n"

        sections.append(section)

    return "\n".join(sections)


# =============================================================================
# TEA CUSTOM ACTION (AC1)
# =============================================================================

def context_assemble(
    state: Dict[str, Any],
    config: Dict[str, Any],
    query: Optional[str] = None,
    variables: Optional[Dict[str, str]] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Assemble context from configured layers with relevance ranking.

    TEA Custom Action: context.assemble

    Args:
        state: Current agent state (must contain project_id)
        config: Layer configuration:
            - max_tokens: int (default 8000)
            - layers: [{scope, entity_id?, directory?, year?, month?}, ...]
            - tabular: [{table, sql}, ...] (optional)
        query: Search query for relevance ranking (optional)
        variables: Variable substitution dict
        **kwargs: Additional arguments
            cloud_list_fn: memory.cloud_list function (optional)
            cloud_retrieve_fn: memory.cloud_retrieve function (optional)
            data_query_fn: data.query function (optional)
            vector_search_fn: memory.vector_search function (optional)
            config_get_scopes_fn: config.get_scopes function (optional)

    Returns:
        On success:
        {
            "success": True,
            "context": str,  # Assembled context string
            "tokens": int,   # Total token count
            "sources": [{path, scope, tokens, score}, ...],
            "tabular": [{table, rows}, ...],
            "truncated": bool
        }

        On error:
        {
            "success": False,
            "error": str
        }

    Example:
        context.assemble(
            config={
                "max_tokens": 8000,
                "layers": [
                    {"scope": "app"},
                    {"scope": "firm", "entity_id": "firm123"},
                    {"scope": "directory", "directory": "chambers", "year": "2024", "month": "12"}
                ],
                "tabular": [
                    {"table": "firm_scores", "sql": "SELECT * FROM data WHERE firm_id = '{{firm_id}}'"}
                ]
            },
            query="What are the key matters for this firm?",
            variables={"firm_id": "firm123", "year": "2024", "month": "12"}
        )
    """
    logger.info("context.assemble starting")

    variables = variables or {}
    config = config or {}
    max_tokens = config.get("max_tokens", DEFAULT_MAX_TOKENS)
    layers_config = config.get("layers", [])
    tabular_config = config.get("tabular", [])

    # Get required action functions via dependency injection
    cloud_list_fn = _get_action_function(
        state, kwargs, 'cloud_list_fn', 'memory.cloud_list'
    )
    cloud_retrieve_fn = _get_action_function(
        state, kwargs, 'cloud_retrieve_fn', 'memory.cloud_retrieve'
    )

    if not cloud_list_fn or not cloud_retrieve_fn:
        return {
            "success": False,
            "error": "cloud_list_fn and cloud_retrieve_fn are required. "
                    "Provide via kwargs or register memory.cloud_list/memory.cloud_retrieve actions.",
            "error_type": "configuration_error"
        }

    # Get optional action functions
    data_query_fn = _get_action_function(
        state, kwargs, 'data_query_fn', 'data.query'
    )
    vector_search_fn = _get_action_function(
        state, kwargs, 'vector_search_fn', 'memory.vector_search'
    )
    config_get_scopes_fn = _get_action_function(
        state, kwargs, 'config_get_scopes_fn', 'config.get_scopes'
    )

    if not vector_search_fn:
        logger.debug("Vector search not available, using priority-only ranking")

    # Step 1: Load scope definitions from config
    scope_defs: Dict[str, Dict] = {}
    if config_get_scopes_fn:
        try:
            scopes_result = config_get_scopes_fn(state)
            if scopes_result.get("success"):
                scope_defs = {s["id"]: s for s in scopes_result.get("scopes", [])}
            else:
                logger.warning(
                    f"Failed to load scope config: {scopes_result.get('error')}. "
                    "Using empty scope definitions."
                )
        except Exception as e:
            logger.warning(f"Error loading scope config: {e}")
    else:
        # Fall back to scope definitions provided in config
        for layer in layers_config:
            scope_id = layer.get("scope")
            if scope_id and scope_id not in scope_defs:
                scope_defs[scope_id] = {
                    "id": scope_id,
                    "path_pattern": layer.get("path_pattern", f"{scope_id}/*"),
                    "priority": layer.get("priority", 0.5)
                }

    # Step 2: Resolve layer paths
    resolved_layers = _resolve_layers(state, layers_config, variables, scope_defs)

    if not resolved_layers and not tabular_config:
        logger.warning("No layers resolved and no tabular config")
        return {
            "success": True,
            "context": "",
            "tokens": 0,
            "sources": [],
            "tabular": [],
            "truncated": False
        }

    # Step 3: Retrieve memory from each layer
    memory_items = []
    for layer in resolved_layers:
        layer_items = _retrieve_layer_memory(
            state=state,
            scope=layer["scope"],
            path=layer["path"],
            cloud_list_fn=cloud_list_fn,
            cloud_retrieve_fn=cloud_retrieve_fn
        )
        # Add priority from layer definition
        for item in layer_items:
            item["priority"] = layer["priority"]
        memory_items.extend(layer_items)

    # Step 4: Retrieve tabular data
    tabular_results = _retrieve_tabular(
        state=state,
        tabular_config=tabular_config,
        variables=variables,
        data_query_fn=data_query_fn
    )

    # Step 5: Rank by relevance (if query provided)
    if query and memory_items:
        memory_items = _rank_by_relevance(
            state=state,
            items=memory_items,
            query=query,
            vector_search_fn=vector_search_fn
        )
    else:
        # Priority-only ordering
        for item in memory_items:
            item["relevance_score"] = DEFAULT_RELEVANCE_SCORE
            item["final_score"] = item.get("priority", 0.5)
        memory_items.sort(key=lambda x: x.get("final_score", 0), reverse=True)

    # Step 6: Token counting and truncation
    memory_items, truncated = _truncate_to_limit(memory_items, max_tokens)

    # Step 7: Format context
    context = _format_context(memory_items, tabular_results)
    total_tokens = _count_tokens(context)

    # Build sources list
    sources = [{
        "path": item.get("path", ""),
        "scope": item.get("scope", ""),
        "tokens": item.get("tokens", 0),
        "score": item.get("final_score", item.get("priority", 0.5))
    } for item in memory_items]

    # Build tabular summary
    tabular_summary = [
        {"table": t["table"], "rows": t["rows"]}
        for t in tabular_results
    ]

    logger.info(
        f"context.assemble complete: {len(sources)} sources, "
        f"{total_tokens} tokens, truncated={truncated}"
    )

    return {
        "success": True,
        "context": context,
        "tokens": total_tokens,
        "sources": sources,
        "tabular": tabular_summary,
        "truncated": truncated
    }


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register context assembly actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Wrapper to inject action functions from engine's registry
    def wrap_with_actions(fn: Callable) -> Callable:
        """Wrap action to inject action functions from engine's registry."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject action functions from engine's action registry
            if hasattr(engine, 'actions_registry') and engine.actions_registry:
                ar = engine.actions_registry
                if 'cloud_list_fn' not in kwargs and 'memory.cloud_list' in ar:
                    kwargs['cloud_list_fn'] = ar['memory.cloud_list']
                if 'cloud_retrieve_fn' not in kwargs and 'memory.cloud_retrieve' in ar:
                    kwargs['cloud_retrieve_fn'] = ar['memory.cloud_retrieve']
                if 'data_query_fn' not in kwargs and 'data.query' in ar:
                    kwargs['data_query_fn'] = ar['data.query']
                if 'vector_search_fn' not in kwargs and 'memory.vector_search' in ar:
                    kwargs['vector_search_fn'] = ar['memory.vector_search']
                if 'config_get_scopes_fn' not in kwargs and 'config.get_scopes' in ar:
                    kwargs['config_get_scopes_fn'] = ar['config.get_scopes']
            return fn(state, *args, **kwargs)
        # Preserve function metadata for introspection
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    registry["context.assemble"] = wrap_with_actions(context_assemble)

    logger.info("Context actions registered: context.assemble")


# Module metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Context assembly action with dependency injection",
    "actions": [
        "context.assemble"
    ],
    "story": "TEA-BUILTIN-006"
}
