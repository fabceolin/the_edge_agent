"""SemTools semantic search actions.

This module provides a YAML action for semantic search using the SemTools CLI,
enabling grep-like semantic similarity search across text files without requiring
API calls or vector database setup.

Story: TEA-BUILTIN-014

Actions:
    - semtools.search: Execute semantic search using SemTools CLI
"""

import subprocess
import shutil
import json
import logging
from typing import Any, Callable, Dict, List, Optional, Union
import glob as glob_module

logger = logging.getLogger(__name__)

# Minimum required version of semtools (optional check)
MIN_SEMTOOLS_VERSION = "1.5.1"


def _check_semtools_installed() -> tuple[bool, Optional[str]]:
    """Check if semtools CLI is installed and accessible.

    Returns:
        Tuple of (is_installed, path_or_error_message)
    """
    semtools_path = shutil.which("semtools")
    if semtools_path:
        return True, semtools_path
    return False, None


def _get_semtools_version() -> Optional[str]:
    """Get the installed semtools version.

    Returns:
        Version string or None if version cannot be determined.
    """
    try:
        result = subprocess.run(
            ["semtools", "--version"], capture_output=True, text=True, timeout=10
        )
        if result.returncode == 0:
            # Parse version from output (e.g., "semtools 1.5.1")
            version_str = result.stdout.strip()
            parts = version_str.split()
            if len(parts) >= 2:
                return parts[1]
            return version_str
        return None
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return None


def _expand_file_patterns(files: Union[str, List[str]]) -> List[str]:
    """Expand file patterns (globs) to actual file paths.

    Args:
        files: A string (glob pattern) or list of file paths/patterns

    Returns:
        List of expanded file paths
    """
    if isinstance(files, str):
        # Single pattern - expand glob
        expanded = glob_module.glob(files, recursive=True)
        if not expanded:
            # Return as-is if no matches (let semtools handle the error)
            return [files]
        return expanded
    else:
        # List of patterns/files - expand each
        result = []
        for pattern in files:
            expanded = glob_module.glob(pattern, recursive=True)
            if expanded:
                result.extend(expanded)
            else:
                result.append(pattern)
        return result


def semtools_search(
    state: Dict[str, Any],  # noqa: ARG001 - Required by action signature
    query: str,
    files: Union[str, List[str]],
    max_distance: float = 0.5,
    n_results: int = 10,
    n_lines: int = 0,
    timeout: int = 60,
    check_version: bool = False,
    **kwargs: Any,  # noqa: ARG001 - Accept additional kwargs for forward compatibility
) -> Dict[str, Any]:
    """
    Semantic search using SemTools CLI.

    Performs local semantic search on text files using model2vec embeddings.
    No API calls required - runs entirely locally.

    Args:
        state: Current workflow state
        query: Semantic search query string
        files: File path(s) or glob pattern to search in
        max_distance: Maximum cosine distance (0.0-1.0, lower = more similar).
                     Default: 0.5. Use 0.3 for stricter matching.
        n_results: Maximum number of results to return. Default: 10
        n_lines: Number of context lines around each match. Default: 0
        timeout: Command timeout in seconds. Default: 60
        check_version: Whether to verify minimum semtools version. Default: False

    Returns:
        Dictionary with:
        - success: bool - Whether search completed successfully
        - matches: List[dict] - List of match objects with file, line, score, text
        - best_match: dict|None - Highest-scoring match
        - has_matches: bool - Whether any matches were found
        - total_matches: int - Number of matches returned
        - query: str - The search query used
        - error: str (optional) - Error message if failed
        - error_type: str (optional) - Error category
        - install_hint: str (optional) - Installation instructions if CLI missing

    Example:
        >>> result = semtools_search(
        ...     state={},
        ...     query="error handling exception",
        ...     files="src/**/*.py",
        ...     max_distance=0.3,
        ...     n_results=5
        ... )
        >>> if result["has_matches"]:
        ...     print(result["best_match"]["text"])
    """
    # Validate parameters
    if not query or not isinstance(query, str):
        return {
            "success": False,
            "error": "Query parameter is required and must be a non-empty string",
            "error_type": "validation_error",
            "matches": [],
            "has_matches": False,
        }

    if not files:
        return {
            "success": False,
            "error": "Files parameter is required",
            "error_type": "validation_error",
            "matches": [],
            "has_matches": False,
        }

    if not 0.0 <= max_distance <= 1.0:
        return {
            "success": False,
            "error": f"max_distance must be between 0.0 and 1.0, got {max_distance}",
            "error_type": "validation_error",
            "matches": [],
            "has_matches": False,
        }

    if n_results < 1:
        return {
            "success": False,
            "error": f"n_results must be at least 1, got {n_results}",
            "error_type": "validation_error",
            "matches": [],
            "has_matches": False,
        }

    # Check if semtools is installed
    is_installed, _ = _check_semtools_installed()
    if not is_installed:
        logger.warning("semtools CLI not found in PATH")
        return {
            "success": False,
            "error": "semtools CLI not found. Please install it first.",
            "error_type": "prerequisite_missing",
            "install_hint": "Install with: npm i -g @llamaindex/semtools (or: cargo install semtools)",
            "matches": [],
            "has_matches": False,
        }

    # Optional version check
    if check_version:
        version = _get_semtools_version()
        if version:
            logger.debug(f"semtools version: {version}")
        else:
            logger.warning("Could not determine semtools version")

    # Expand file patterns
    file_list = _expand_file_patterns(files)
    logger.debug(f"Searching {len(file_list)} files for query: {query[:50]}...")

    # Build command
    cmd = ["semtools", "search", query]
    cmd.extend(file_list)
    cmd.extend(["--max-distance", str(max_distance)])
    cmd.extend(["--output", "json"])

    if n_lines > 0:
        cmd.extend(["--n-lines", str(n_lines)])

    try:
        logger.debug(f"Executing: {' '.join(cmd[:5])}... ({len(cmd)} args)")
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)

        if result.returncode != 0:
            error_msg = (
                result.stderr.strip() if result.stderr else "semtools search failed"
            )
            logger.warning(f"semtools returned non-zero: {error_msg}")
            return {
                "success": False,
                "error": error_msg,
                "error_type": "cli_error",
                "matches": [],
                "has_matches": False,
                "query": query,
            }

        # Parse JSON output
        stdout = result.stdout.strip()
        if not stdout:
            # Empty output = no matches (not an error)
            logger.debug("semtools returned empty output (no matches)")
            return {
                "success": True,
                "matches": [],
                "best_match": None,
                "has_matches": False,
                "total_matches": 0,
                "query": query,
            }

        try:
            matches = json.loads(stdout)
        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse semtools JSON output: {e}")
            return {
                "success": False,
                "error": f"Failed to parse semtools output: {e}",
                "error_type": "parse_error",
                "raw_output": stdout[:500] if stdout else None,
                "matches": [],
                "has_matches": False,
                "query": query,
            }

        # Validate and normalize matches
        if not isinstance(matches, list):
            matches = [matches] if matches else []

        # Limit results
        matches = matches[:n_results]

        # Find best match (highest score)
        best_match = None
        if matches:
            best_match = max(
                matches, key=lambda m: m.get("score", 0) if isinstance(m, dict) else 0
            )

        logger.debug(f"semtools found {len(matches)} matches")

        return {
            "success": True,
            "matches": matches,
            "best_match": best_match,
            "has_matches": len(matches) > 0,
            "total_matches": len(matches),
            "query": query,
        }

    except subprocess.TimeoutExpired:
        logger.error(f"semtools search timed out after {timeout}s")
        return {
            "success": False,
            "error": f"semtools search timed out after {timeout} seconds",
            "error_type": "timeout",
            "matches": [],
            "has_matches": False,
            "query": query,
        }
    except FileNotFoundError:
        # This shouldn't happen since we checked with shutil.which, but handle it anyway
        logger.error("semtools command not found during execution")
        return {
            "success": False,
            "error": "semtools command not found",
            "error_type": "prerequisite_missing",
            "install_hint": "Install with: npm i -g @llamaindex/semtools",
            "matches": [],
            "has_matches": False,
            "query": query,
        }
    except Exception as e:
        logger.exception(f"Unexpected error during semtools search: {e}")
        return {
            "success": False,
            "error": f"Unexpected error: {str(e)}",
            "error_type": "unexpected_error",
            "matches": [],
            "has_matches": False,
            "query": query,
        }


def register_actions(
    registry: Dict[str, Callable], engine: Any
) -> None:  # noqa: ARG001
    """Register SemTools actions in the action registry.

    Actions are registered under multiple namespaces:
    - semtools.search (primary)
    - actions.semtools_search (alternative)

    Args:
        registry: The action registry dictionary
        engine: The YAMLEngine instance (unused, kept for API consistency)
    """
    _ = engine  # Unused but kept for API consistency
    # Primary namespace
    registry["semtools.search"] = semtools_search

    # Alternative namespaces for compatibility
    registry["actions.semtools_search"] = semtools_search

    logger.debug(
        "Registered SemTools actions: semtools.search, actions.semtools_search"
    )
