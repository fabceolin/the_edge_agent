"""
DuckDB Search Actions for TEA YAMLEngine (TEA-BUILTIN-006).

Provides custom actions for deterministic SQL-based searches across agent memory
artifacts, implementing the "Grep Engine" layer of the Agentic Data Lake architecture.

Migrated from: firebase/functions-agents/actions/duckdb_search.py
Uses QueryEngine ABC for provider portability.

Risk Mitigations Implemented:
- SEC-001: Multi-layer SQL injection prevention
- TECH-001: Resilient connection via QueryEngine with circuit breaker
- TECH-003: Cold start optimization with lazy loading

Actions:
- memory.grep: LIKE/regex search across memory files
- memory.sql_query: Full SQL access with safety controls
- memory.search_content: Structured field search via JSON path
"""

import logging
import re
from typing import Any, Callable, Dict, List, Optional

from ..memory.query import QueryEngine, QueryConfig

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONSTANTS
# =============================================================================

# Parquet file location
PARQUET_PATH = "parquet/agent_memory.parquet"

# Query limits
DEFAULT_ROW_LIMIT = 1000
DEFAULT_QUERY_TIMEOUT = 30  # seconds
MAX_CONTEXT_LINES = 10

# Supported grep modes
GREP_MODES = {"like", "regex", "exact"}

# Supported search operators
SEARCH_OPERATORS = {"=", "!=", "LIKE", "IN", "IS NULL", "IS NOT NULL", ">", "<", ">=", "<="}

# File type mapping
CONTENT_TYPES = {"yaml", "yml", "json", "md"}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _get_query_engine(state: Dict[str, Any], kwargs: Dict[str, Any]) -> QueryEngine:
    """Get QueryEngine from state or kwargs."""
    engine = kwargs.get('query_engine')
    if engine is not None:
        return engine

    engine = state.get('_query_engine')
    if engine is not None:
        return engine

    engine = state.get('query_engine')
    if engine is not None:
        return engine

    raise ValueError(
        "No QueryEngine available. Pass query_engine in kwargs or state."
    )


def _get_parquet_url(state: Dict[str, Any], kwargs: Dict[str, Any]) -> str:
    """Get Parquet URL from config or defaults."""
    parquet_url = kwargs.get('parquet_url')
    if parquet_url:
        return parquet_url

    parquet_url = state.get('_parquet_url')
    if parquet_url:
        return parquet_url

    # Build from bucket
    bucket = kwargs.get('bucket') or state.get('_bucket', 'rankellix-law.firebasestorage.app')
    return f"gs://{bucket}/{PARQUET_PATH}"


def _extract_context(content: str, line_index: int, context_lines: int) -> Dict[str, Any]:
    """Extract context lines around a match."""
    lines = content.split('\n')
    total_lines = len(lines)

    context_lines = min(context_lines, MAX_CONTEXT_LINES)

    start = max(0, line_index - context_lines)
    end = min(total_lines, line_index + context_lines + 1)

    return {
        "before": lines[start:line_index] if start < line_index else [],
        "match": lines[line_index] if line_index < total_lines else "",
        "after": lines[line_index + 1:end] if line_index + 1 < end else [],
        "start_line": start + 1,
        "end_line": end
    }


def _build_file_type_filter(file_types: List[str]) -> str:
    """Build SQL WHERE clause fragment for file type filtering."""
    if not file_types:
        return ""

    normalized = []
    for ft in file_types:
        ft_lower = ft.lower()
        if ft_lower in ("yaml", "yml"):
            normalized.append("yaml")
        elif ft_lower in CONTENT_TYPES:
            normalized.append(ft_lower)

    if not normalized:
        return ""

    normalized = list(set(normalized))
    types_str = ", ".join(f"'{t}'" for t in normalized)
    return f" AND content_type IN ({types_str})"


def _build_path_filter(paths: List[str]) -> str:
    """Build SQL WHERE clause fragment for path filtering."""
    if not paths:
        return ""

    conditions = []
    for path in paths:
        safe_path = path.replace("'", "''")
        conditions.append(f"file_path LIKE '%{safe_path}%'")

    return f" AND ({' OR '.join(conditions)})"


def _find_matches_in_content(
    content: str,
    pattern: str,
    mode: str,
    case_sensitive: bool,
    context_lines: int
) -> List[Dict[str, Any]]:
    """Find all matches of a pattern in content."""
    matches = []
    lines = content.split('\n')

    if mode == "regex":
        try:
            flags = 0 if case_sensitive else re.IGNORECASE
            compiled_pattern = re.compile(pattern, flags)
        except re.error as e:
            logger.warning(f"Invalid regex pattern: {e}")
            return []

    for i, line in enumerate(lines):
        is_match = False

        if mode == "regex":
            is_match = bool(compiled_pattern.search(line))
        elif mode == "like" or mode == "exact":
            search_line = line if case_sensitive else line.lower()
            search_pattern = pattern if case_sensitive else pattern.lower()
            is_match = search_pattern in search_line

        if is_match:
            match_entry = {
                "line_number": i + 1,
                "match_text": line.strip(),
            }

            if context_lines > 0:
                match_entry["context"] = _extract_context(content, i, context_lines)

            matches.append(match_entry)

    return matches


# =============================================================================
# TEA CUSTOM ACTIONS
# =============================================================================

def memory_grep(
    state: Dict[str, Any],
    pattern: str,
    mode: str = "like",
    file_types: List[str] = None,
    paths: List[str] = None,
    case_sensitive: bool = True,
    context_lines: int = 0,
    limit: int = 100,
    **kwargs
) -> Dict[str, Any]:
    """
    Execute grep-like search across agent memory.

    TEA Custom Action: memory.grep

    Args:
        state: Current agent state (must contain project_id)
        pattern: Search pattern
        mode: 'like' (SQL LIKE), 'regex' (regexp_matches), 'exact' (contains)
        file_types: Filter by content_type ['yaml', 'json', 'md']
        paths: Filter by path prefixes
        case_sensitive: Whether search is case-sensitive (default True)
        context_lines: Number of lines before/after match to include
        limit: Maximum number of files to return (default 100)
        **kwargs: May contain query_engine for dependency injection

    Returns:
        {success: bool, matches: [{file_path, line_number, match_text, context}]}
    """
    logger.info(f"memory.grep: pattern={pattern!r}, mode={mode}")

    # Validate mode
    if mode not in GREP_MODES:
        return {
            "success": False,
            "error": f"Invalid mode: {mode}. Must be one of: {', '.join(GREP_MODES)}",
            "error_type": "validation_error"
        }

    # Validate pattern
    if not pattern or not pattern.strip():
        return {
            "success": False,
            "error": "Pattern cannot be empty",
            "error_type": "validation_error"
        }

    project_id = state.get("project_id", "rankellix")

    try:
        engine = _get_query_engine(state, kwargs)
        parquet_url = _get_parquet_url(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    try:
        # Build search column based on case sensitivity
        search_column = "content" if case_sensitive else "LOWER(content)"
        search_pattern = pattern if case_sensitive else pattern.lower()

        # Build WHERE clause based on mode
        if mode == "like":
            escaped = search_pattern.replace("'", "''").replace("%", "\\%").replace("_", "\\_")
            where_clause = f"{search_column} LIKE '%{escaped}%' ESCAPE '\\'"
        elif mode == "regex":
            escaped = search_pattern.replace("'", "''")
            where_clause = f"regexp_matches(content, '{escaped}')"
        elif mode == "exact":
            escaped = search_pattern.replace("'", "''")
            where_clause = f"{search_column} LIKE '%{escaped}%'"

        # Add project filter
        where_clause += f" AND project_id = '{project_id}'"

        # Add file type filter
        where_clause += _build_file_type_filter(file_types or [])

        # Add path filter
        where_clause += _build_path_filter(paths or [])

        # Build SQL query
        sql = f"""
        SELECT file_path, content, content_type
        FROM read_parquet('{parquet_url}')
        WHERE {where_clause}
        LIMIT {min(limit, DEFAULT_ROW_LIMIT)}
        """

        logger.debug(f"memory.grep SQL: {sql}")

        # Execute query
        config = QueryConfig(
            timeout_sec=DEFAULT_QUERY_TIMEOUT,
            max_rows=min(limit, DEFAULT_ROW_LIMIT)
        )
        result = engine.execute(sql, config=config)

        if not result.get("success"):
            return result

        # Process results to extract line-level matches
        all_matches = []
        rows = result.get("rows", [])

        for row in rows:
            file_path, content, content_type = row[:3]

            # Find matches within this file
            file_matches = _find_matches_in_content(
                content,
                pattern,
                mode,
                case_sensitive,
                context_lines
            )

            for match in file_matches:
                all_matches.append({
                    "file_path": file_path,
                    "content_type": content_type,
                    **match
                })

        return {
            "success": True,
            "matches": all_matches,
            "file_count": len(rows),
            "match_count": len(all_matches),
            "pattern": pattern,
            "mode": mode,
            "case_sensitive": case_sensitive
        }

    except Exception as e:
        logger.error(f"memory.grep failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "search_failed"
        }


def memory_sql_query(
    state: Dict[str, Any],
    query: str,
    row_limit: int = DEFAULT_ROW_LIMIT,
    timeout_sec: int = DEFAULT_QUERY_TIMEOUT,
    **kwargs
) -> Dict[str, Any]:
    """
    Execute SQL query against agent_memory table with safety controls.

    TEA Custom Action: memory.sql_query

    Args:
        state: Current agent state (must contain project_id)
        query: SQL SELECT query
        row_limit: Maximum rows to return (default 1000)
        timeout_sec: Query timeout in seconds (default 30)
        **kwargs: May contain query_engine and query_sandbox

    Returns:
        {success: bool, columns: [], rows: [[]], row_count: int}
    """
    logger.info(f"memory.sql_query: query length={len(query)}")

    # Validate query
    if not query or not query.strip():
        return {
            "success": False,
            "error": "Query cannot be empty",
            "error_type": "validation_error"
        }

    project_id = state.get("project_id", "rankellix")

    try:
        engine = _get_query_engine(state, kwargs)
        parquet_url = _get_parquet_url(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    try:
        # Basic SQL injection prevention
        query_lower = query.lower().strip()

        # Only allow SELECT queries
        if not query_lower.startswith("select"):
            return {
                "success": False,
                "error": "Only SELECT queries are allowed",
                "error_type": "validation_error"
            }

        # Block dangerous keywords
        dangerous_keywords = [
            "insert", "update", "delete", "drop", "create", "alter",
            "truncate", "exec", "execute", "--", ";--", "/*", "*/"
        ]
        for keyword in dangerous_keywords:
            if keyword in query_lower:
                return {
                    "success": False,
                    "error": f"Query contains forbidden keyword: {keyword}",
                    "error_type": "validation_error"
                }

        # Inject Parquet URL if query references agent_memory table
        if "agent_memory" in query_lower or "from " in query_lower:
            # Replace table reference with Parquet read
            safe_query = re.sub(
                r'\bfrom\s+agent_memory\b',
                f"FROM read_parquet('{parquet_url}')",
                query,
                flags=re.IGNORECASE
            )
        else:
            safe_query = query

        # Add project filter if not present
        if "project_id" not in query_lower:
            if "where" in safe_query.lower():
                safe_query = re.sub(
                    r'\bwhere\b',
                    f"WHERE project_id = '{project_id}' AND ",
                    safe_query,
                    count=1,
                    flags=re.IGNORECASE
                )
            else:
                # No WHERE clause, add one before ORDER BY/LIMIT/GROUP BY
                order_match = re.search(
                    r'\b(order\s+by|limit|group\s+by)\b',
                    safe_query,
                    flags=re.IGNORECASE
                )
                if order_match:
                    insert_pos = order_match.start()
                    safe_query = (
                        safe_query[:insert_pos] +
                        f" WHERE project_id = '{project_id}' " +
                        safe_query[insert_pos:]
                    )
                else:
                    safe_query += f" WHERE project_id = '{project_id}'"

        # Add row limit if not present
        if "limit" not in safe_query.lower():
            safe_query += f" LIMIT {min(row_limit, DEFAULT_ROW_LIMIT)}"

        logger.debug(f"memory.sql_query wrapped: {safe_query}")

        # Execute query
        config = QueryConfig(
            timeout_sec=min(timeout_sec, 60),
            max_rows=min(row_limit, DEFAULT_ROW_LIMIT)
        )
        result = engine.execute(safe_query, config=config)

        return result

    except Exception as e:
        logger.error(f"memory.sql_query failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "query_error"
        }


def memory_search_content(
    state: Dict[str, Any],
    field_path: str,
    operator: str,
    value: Any = None,
    file_types: List[str] = None,
    limit: int = 100,
    **kwargs
) -> Dict[str, Any]:
    """
    Search for files by structured content field values.

    TEA Custom Action: memory.search_content

    Args:
        state: Current agent state (must contain project_id)
        field_path: JSON path to field (e.g., '$.status', '$.metadata.author')
        operator: Comparison operator ('=', '!=', 'LIKE', 'IN', 'IS NULL', 'IS NOT NULL')
        value: Value to compare against (not needed for IS NULL/IS NOT NULL)
        file_types: Filter by content_type ['yaml', 'json', 'md']
        limit: Maximum results (default 100)
        **kwargs: May contain query_engine

    Returns:
        {success: bool, files: [{file_path, field_value, content_type}]}
    """
    logger.info(f"memory.search_content: field_path={field_path}, operator={operator}")

    # Validate operator
    if operator.upper() not in SEARCH_OPERATORS:
        return {
            "success": False,
            "error": f"Invalid operator: {operator}. Must be one of: {', '.join(SEARCH_OPERATORS)}",
            "error_type": "validation_error"
        }

    # Validate field_path
    if not field_path or not field_path.strip():
        return {
            "success": False,
            "error": "field_path cannot be empty",
            "error_type": "validation_error"
        }

    # Validate value for operators that need it
    op_upper = operator.upper()
    if op_upper not in ("IS NULL", "IS NOT NULL") and value is None:
        return {
            "success": False,
            "error": f"Value required for operator: {operator}",
            "error_type": "validation_error"
        }

    project_id = state.get("project_id", "rankellix")

    try:
        engine = _get_query_engine(state, kwargs)
        parquet_url = _get_parquet_url(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    try:
        # Build WHERE clause using DuckDB's json_extract
        safe_field_path = field_path.replace("'", "''")

        # Build comparison
        if op_upper == "IS NULL":
            condition = f"json_extract(content_parsed, '{safe_field_path}') IS NULL"
        elif op_upper == "IS NOT NULL":
            condition = f"json_extract(content_parsed, '{safe_field_path}') IS NOT NULL"
        elif op_upper == "IN":
            if not isinstance(value, (list, tuple)):
                return {
                    "success": False,
                    "error": "Value must be a list for IN operator",
                    "error_type": "validation_error"
                }
            values_str = ", ".join(f"'{str(v).replace(chr(39), chr(39)+chr(39))}'" for v in value)
            condition = f"CAST(json_extract(content_parsed, '{safe_field_path}') AS VARCHAR) IN ({values_str})"
        elif op_upper == "LIKE":
            safe_value = str(value).replace("'", "''")
            condition = f"CAST(json_extract(content_parsed, '{safe_field_path}') AS VARCHAR) LIKE '{safe_value}'"
        else:
            safe_value = str(value).replace("'", "''")
            condition = f"CAST(json_extract(content_parsed, '{safe_field_path}') AS VARCHAR) {operator} '{safe_value}'"

        # Add project filter
        condition += f" AND project_id = '{project_id}'"

        # Only search files with parsed content
        condition += " AND content_parsed IS NOT NULL"

        # Add file type filter
        condition += _build_file_type_filter(file_types or [])

        # Build SQL query
        sql = f"""
        SELECT
            file_path,
            content_type,
            json_extract(content_parsed, '{safe_field_path}') as field_value,
            anchors,
            status,
            summary
        FROM read_parquet('{parquet_url}')
        WHERE {condition}
        LIMIT {min(limit, DEFAULT_ROW_LIMIT)}
        """

        logger.debug(f"memory.search_content SQL: {sql}")

        # Execute query
        config = QueryConfig(
            timeout_sec=DEFAULT_QUERY_TIMEOUT,
            max_rows=min(limit, DEFAULT_ROW_LIMIT)
        )
        result = engine.execute(sql, config=config)

        if not result.get("success"):
            return result

        # Format results
        files = []
        for row in result.get("rows", []):
            file_path, content_type, field_value, anchors, status, summary = row[:6]
            files.append({
                "file_path": file_path,
                "content_type": content_type,
                "field_value": field_value,
                "anchors": anchors or [],
                "status": status,
                "summary": summary or ""
            })

        return {
            "success": True,
            "files": files,
            "count": len(files),
            "field_path": field_path,
            "operator": operator,
            "value": value
        }

    except Exception as e:
        logger.error(f"memory.search_content failed: {e}")
        return {
            "success": False,
            "error": str(e),
            "error_type": "search_failed"
        }


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register DuckDB search actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Wrapper to inject engine backends into kwargs
    def wrap_with_backends(fn: Callable) -> Callable:
        """Wrap action to inject engine's query_engine if not provided."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject query_engine from engine if not already provided
            if 'query_engine' not in kwargs:
                if hasattr(engine, '_query_engine') and engine._query_engine is not None:
                    kwargs['query_engine'] = engine._query_engine
            return fn(state, *args, **kwargs)
        # Preserve function metadata for introspection
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    registry["memory.grep"] = wrap_with_backends(memory_grep)
    registry["memory.sql_query"] = wrap_with_backends(memory_sql_query)
    registry["memory.search_content"] = wrap_with_backends(memory_search_content)

    logger.info(
        "DuckDB search actions registered: "
        "memory.grep, memory.sql_query, memory.search_content"
    )
