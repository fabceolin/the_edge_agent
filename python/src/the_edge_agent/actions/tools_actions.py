"""
Tools Bridge Actions for YAMLEngine (TEA-BUILTIN-002.3).

This module provides bridge actions to external tool ecosystems:
- CrewAI: Access to 700+ pre-built tools
- MCP: Model Context Protocol server connections
- LangChain: LangChain tool ecosystem integration

All bridges are optional - they gracefully degrade if dependencies are not installed.
Tools follow tea's action pattern: state in, dict out.

Actions:
    - tools.crewai: Execute CrewAI tools
    - tools.mcp: Execute MCP server tools
    - tools.langchain: Execute LangChain tools
    - tools.discover: Discover available tools from all sources

Example:
    >>> # Use a CrewAI tool
    >>> result = registry['tools.crewai'](
    ...     state={},
    ...     tool="SerperDevTool",
    ...     query="latest AI news"
    ... )
    >>> print(result['result'])

    >>> # Discover available tools
    >>> result = registry['tools.discover'](state={}, source="all")
    >>> for tool in result['tools']:
    ...     print(f"{tool['name']}: {tool['description']}")
"""

import asyncio
import functools
import time
from typing import Any, Callable, Dict, List, Optional, Protocol, runtime_checkable


# Check which dependencies are available
def _check_crewai() -> bool:
    """Check if CrewAI is installed."""
    try:
        import crewai_tools
        return True
    except ImportError:
        return False


def _check_mcp() -> bool:
    """Check if MCP is installed."""
    try:
        import mcp
        return True
    except ImportError:
        return False


def _check_langchain() -> bool:
    """Check if LangChain is installed."""
    try:
        import langchain
        return True
    except ImportError:
        return False


HAS_CREWAI = _check_crewai()
HAS_MCP = _check_mcp()
HAS_LANGCHAIN = _check_langchain()


# ============================================================================
# Tool Bridge Protocol (Task 1: Design tool bridge abstraction)
# ============================================================================

@runtime_checkable
class ToolBridge(Protocol):
    """
    Protocol defining the interface for tool bridges.

    All tool bridges must implement these methods to provide
    consistent behavior across different tool ecosystems.
    """

    def discover_tools(self, filter: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Discover available tools from this bridge.

        Args:
            filter: Optional filter string to narrow results

        Returns:
            List of tool schemas in tea format:
            [
                {
                    "name": str,
                    "description": str,
                    "parameters": {
                        "param_name": {"type": str, "description": str, "required": bool}
                    },
                    "source": str  # "crewai", "mcp", "langchain"
                }
            ]
        """
        ...

    def execute_tool(self, tool_name: str, **params) -> Dict[str, Any]:
        """
        Execute a tool by name.

        Args:
            tool_name: Name or identifier of the tool
            **params: Parameters to pass to the tool

        Returns:
            {"result": any, "tool": str, "success": bool}
            or {"error": str, "error_type": str, "tool": str, "success": False}
        """
        ...

    def translate_schema(self, external_schema: Any) -> Dict[str, Any]:
        """
        Translate external tool schema to tea format.

        Args:
            external_schema: Tool schema in external format

        Returns:
            Tool schema in tea format
        """
        ...


# Common tool schema format
def create_tool_schema(
    name: str,
    description: str,
    parameters: Dict[str, Dict[str, Any]],
    source: str
) -> Dict[str, Any]:
    """
    Create a standardized tool schema.

    Args:
        name: Tool name
        description: Tool description
        parameters: Parameter definitions
        source: Source bridge ("crewai", "mcp", "langchain")

    Returns:
        Standardized tool schema dict
    """
    return {
        "name": name,
        "description": description,
        "parameters": parameters,
        "source": source
    }


def normalize_result(result: Any, tool_name: str) -> Dict[str, Any]:
    """
    Normalize tool result to tea action format.

    Args:
        result: Raw result from tool execution
        tool_name: Name of the tool

    Returns:
        Normalized dict with result, tool, and success keys
    """
    if isinstance(result, dict):
        # Already a dict, add required fields if missing
        normalized = dict(result)
        if "tool" not in normalized:
            normalized["tool"] = tool_name
        if "success" not in normalized:
            normalized["success"] = True
        return normalized
    else:
        # Wrap non-dict results
        return {
            "result": result,
            "tool": tool_name,
            "success": True
        }


def create_error_result(
    error: str,
    error_type: str,
    tool_name: str
) -> Dict[str, Any]:
    """
    Create a standardized error result.

    Args:
        error: Error message
        error_type: Type of error ("import", "execution", "timeout", "schema")
        tool_name: Name of the tool

    Returns:
        Error result dict
    """
    return {
        "success": False,
        "error": error,
        "error_type": error_type,
        "tool": tool_name
    }


def run_async_in_sync(coro):
    """
    Run an async coroutine in a sync context.

    Handles the case where we're in an existing event loop
    or need to create a new one.
    """
    try:
        loop = asyncio.get_running_loop()
        # We're in an async context, need to use a new thread
        import concurrent.futures
        with concurrent.futures.ThreadPoolExecutor() as pool:
            future = pool.submit(asyncio.run, coro)
            return future.result()
    except RuntimeError:
        # No running loop, safe to use asyncio.run
        return asyncio.run(coro)


# ============================================================================
# Discovery Cache
# ============================================================================

class DiscoveryCache:
    """Simple cache for tool discovery results."""

    def __init__(self, ttl_seconds: float = 300.0):
        self._cache: Dict[str, tuple] = {}  # source -> (timestamp, tools)
        self._ttl = ttl_seconds

    def get(self, source: str) -> Optional[List[Dict[str, Any]]]:
        """Get cached tools for source if not expired."""
        if source in self._cache:
            timestamp, tools = self._cache[source]
            if time.time() - timestamp < self._ttl:
                return tools
            else:
                del self._cache[source]
        return None

    def set(self, source: str, tools: List[Dict[str, Any]]) -> None:
        """Cache tools for source."""
        self._cache[source] = (time.time(), tools)

    def clear(self, source: Optional[str] = None) -> None:
        """Clear cache, optionally for specific source."""
        if source:
            self._cache.pop(source, None)
        else:
            self._cache.clear()


# Global discovery cache
_discovery_cache = DiscoveryCache()


# ============================================================================
# CrewAI Bridge Implementation (Task 2)
# ============================================================================

# Known CrewAI tools for discovery (subset of most common)
KNOWN_CREWAI_TOOLS = [
    "SerperDevTool",
    "ScrapeWebsiteTool",
    "WebsiteSearchTool",
    "FileReadTool",
    "DirectoryReadTool",
    "CodeDocsSearchTool",
    "YoutubeVideoSearchTool",
    "GithubSearchTool",
    "PDFSearchTool",
    "TXTSearchTool",
    "CSVSearchTool",
    "JSONSearchTool",
    "MDXSearchTool",
    "DOCXSearchTool",
    "XMLSearchTool",
    "BrowserbaseTool",
    "EXASearchTool",
    "FirecrawlSearchTool",
    "FirecrawlCrawlWebsiteTool",
    "FirecrawlScrapeWebsiteTool",
]


def _get_crewai_tool(tool_name: str) -> Any:
    """
    Get a CrewAI tool instance by name.

    Args:
        tool_name: Name of the tool class (e.g., "SerperDevTool")

    Returns:
        Tool instance

    Raises:
        ImportError: If tool not found
    """
    try:
        import crewai_tools

        # Try to get tool from crewai_tools module
        if hasattr(crewai_tools, tool_name):
            tool_class = getattr(crewai_tools, tool_name)
            return tool_class()

        # Try dynamic import for tools in submodules
        try:
            from importlib import import_module
            # Common patterns for CrewAI tool locations
            for module_path in [
                f"crewai_tools.tools.{tool_name.lower()}",
                f"crewai_tools.{tool_name.lower()}",
            ]:
                try:
                    module = import_module(module_path)
                    if hasattr(module, tool_name):
                        tool_class = getattr(module, tool_name)
                        return tool_class()
                except ImportError:
                    continue
        except Exception:
            pass

        raise ImportError(f"CrewAI tool '{tool_name}' not found")

    except ImportError as e:
        raise ImportError(f"CrewAI tool '{tool_name}' not found: {e}")


def _translate_crewai_schema(tool: Any) -> Dict[str, Any]:
    """
    Translate CrewAI tool schema to tea format.

    Args:
        tool: CrewAI tool instance

    Returns:
        Tool schema in tea format
    """
    name = tool.__class__.__name__
    description = getattr(tool, 'description', '') or getattr(tool, '__doc__', '') or ''

    # CrewAI tools use pydantic for args schema
    parameters = {}
    if hasattr(tool, 'args_schema') and tool.args_schema:
        schema = tool.args_schema
        if hasattr(schema, 'model_fields'):
            # Pydantic v2
            for field_name, field_info in schema.model_fields.items():
                param_type = "string"
                if hasattr(field_info, 'annotation'):
                    ann = field_info.annotation
                    if ann == int:
                        param_type = "integer"
                    elif ann == float:
                        param_type = "number"
                    elif ann == bool:
                        param_type = "boolean"

                parameters[field_name] = {
                    "type": param_type,
                    "description": getattr(field_info, 'description', '') or '',
                    "required": getattr(field_info, 'is_required', lambda: True)()
                    if callable(getattr(field_info, 'is_required', None))
                    else field_info.default is None
                }
        elif hasattr(schema, '__fields__'):
            # Pydantic v1
            for field_name, field in schema.__fields__.items():
                param_type = "string"
                if field.type_ == int:
                    param_type = "integer"
                elif field.type_ == float:
                    param_type = "number"
                elif field.type_ == bool:
                    param_type = "boolean"

                parameters[field_name] = {
                    "type": param_type,
                    "description": field.field_info.description or '',
                    "required": field.required
                }

    return create_tool_schema(name, description, parameters, "crewai")


def _discover_crewai_tools(filter: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Discover available CrewAI tools.

    Args:
        filter: Optional filter string

    Returns:
        List of tool schemas
    """
    if not HAS_CREWAI:
        return []

    tools = []
    for tool_name in KNOWN_CREWAI_TOOLS:
        if filter and filter.lower() not in tool_name.lower():
            continue
        try:
            tool = _get_crewai_tool(tool_name)
            schema = _translate_crewai_schema(tool)
            tools.append(schema)
        except Exception:
            # Tool not available, skip
            continue

    return tools


# ============================================================================
# MCP Bridge Implementation (Task 3)
# ============================================================================

def _translate_mcp_schema(tool_info: Dict[str, Any]) -> Dict[str, Any]:
    """
    Translate MCP tool schema to tea format.

    Args:
        tool_info: MCP tool info dict

    Returns:
        Tool schema in tea format
    """
    name = tool_info.get("name", "unknown")
    description = tool_info.get("description", "")

    parameters = {}
    input_schema = tool_info.get("inputSchema", {})
    if isinstance(input_schema, dict):
        properties = input_schema.get("properties", {})
        required_fields = input_schema.get("required", [])

        for param_name, param_info in properties.items():
            parameters[param_name] = {
                "type": param_info.get("type", "string"),
                "description": param_info.get("description", ""),
                "required": param_name in required_fields
            }

    return create_tool_schema(name, description, parameters, "mcp")


async def _mcp_connect_and_execute(
    server_config: Dict[str, Any],
    tool_name: str,
    params: Dict[str, Any],
    timeout: float = 30.0
) -> Dict[str, Any]:
    """
    Connect to MCP server and execute a tool.

    Args:
        server_config: MCP server configuration
        tool_name: Name of the tool to execute
        params: Tool parameters
        timeout: Execution timeout in seconds

    Returns:
        Execution result
    """
    try:
        from mcp import ClientSession
        from mcp.client.stdio import stdio_client, StdioServerParameters
    except ImportError:
        return create_error_result(
            "MCP library not installed. Install with: pip install mcp",
            "import",
            tool_name
        )

    try:
        # Build server parameters
        command = server_config.get("command", "")
        args = server_config.get("args", [])
        env = server_config.get("env")

        server_params = StdioServerParameters(
            command=command,
            args=args,
            env=env
        )

        async with asyncio.timeout(timeout):
            async with stdio_client(server_params) as (read, write):
                async with ClientSession(read, write) as session:
                    # Initialize session
                    await session.initialize()

                    # Call the tool
                    result = await session.call_tool(tool_name, arguments=params)

                    # Extract content from result
                    if hasattr(result, 'content') and result.content:
                        content = result.content[0]
                        if hasattr(content, 'text'):
                            return normalize_result(content.text, tool_name)
                        return normalize_result(str(content), tool_name)

                    return normalize_result(str(result), tool_name)

    except asyncio.TimeoutError:
        return create_error_result(
            f"MCP tool execution timed out after {timeout}s",
            "timeout",
            tool_name
        )
    except Exception as e:
        return create_error_result(str(e), "execution", tool_name)


async def _mcp_discover_tools(
    server_config: Dict[str, Any],
    timeout: float = 10.0
) -> List[Dict[str, Any]]:
    """
    Discover tools from an MCP server.

    Args:
        server_config: MCP server configuration
        timeout: Discovery timeout in seconds

    Returns:
        List of tool schemas
    """
    try:
        from mcp import ClientSession
        from mcp.client.stdio import stdio_client, StdioServerParameters
    except ImportError:
        return []

    try:
        command = server_config.get("command", "")
        args = server_config.get("args", [])
        env = server_config.get("env")

        server_params = StdioServerParameters(
            command=command,
            args=args,
            env=env
        )

        async with asyncio.timeout(timeout):
            async with stdio_client(server_params) as (read, write):
                async with ClientSession(read, write) as session:
                    await session.initialize()

                    result = await session.list_tools()
                    tools = []

                    for tool in result.tools:
                        tool_dict = {
                            "name": tool.name,
                            "description": tool.description or "",
                            "inputSchema": tool.inputSchema if hasattr(tool, 'inputSchema') else {}
                        }
                        schema = _translate_mcp_schema(tool_dict)
                        # Add server info
                        schema["server"] = server_config.get("name", command)
                        tools.append(schema)

                    return tools

    except Exception:
        return []


# ============================================================================
# LangChain Bridge Implementation (Task 4)
# ============================================================================

# Known LangChain tools for discovery
KNOWN_LANGCHAIN_TOOLS = [
    ("langchain_community.tools", "DuckDuckGoSearchRun"),
    ("langchain_community.tools", "WikipediaQueryRun"),
    ("langchain_community.tools.arxiv.tool", "ArxivQueryRun"),
    ("langchain_community.tools.pubmed.tool", "PubmedQueryRun"),
    ("langchain_community.tools.tavily_search", "TavilySearchResults"),
    ("langchain_community.tools.google_search", "GoogleSearchRun"),
    ("langchain_community.tools.bing_search", "BingSearchRun"),
    ("langchain_community.utilities.wolfram_alpha", "WolframAlphaAPIWrapper"),
    ("langchain_community.tools.shell.tool", "ShellTool"),
    ("langchain_community.tools.file_management", "ReadFileTool"),
    ("langchain_community.tools.file_management", "WriteFileTool"),
    ("langchain_community.tools.file_management", "CopyFileTool"),
    ("langchain_community.tools.file_management", "DeleteFileTool"),
    ("langchain_community.tools.file_management", "MoveFileTool"),
    ("langchain_community.tools.file_management", "ListDirectoryTool"),
]


def _get_langchain_tool(tool_name: str) -> Any:
    """
    Get a LangChain tool instance by name.

    Args:
        tool_name: Name of the tool class

    Returns:
        Tool instance

    Raises:
        ImportError: If tool not found
    """
    try:
        # Try known locations first
        for module_path, class_name in KNOWN_LANGCHAIN_TOOLS:
            if class_name == tool_name:
                from importlib import import_module
                module = import_module(module_path)
                tool_class = getattr(module, class_name)
                return tool_class()

        # Try common patterns
        from importlib import import_module
        patterns = [
            f"langchain_community.tools.{tool_name.lower()}",
            f"langchain.tools.{tool_name.lower()}",
            f"langchain_community.tools",
            f"langchain.tools",
        ]

        for pattern in patterns:
            try:
                module = import_module(pattern)
                if hasattr(module, tool_name):
                    tool_class = getattr(module, tool_name)
                    return tool_class()
            except ImportError:
                continue

        raise ImportError(f"LangChain tool '{tool_name}' not found")

    except ImportError as e:
        raise ImportError(f"LangChain tool '{tool_name}' not found: {e}")


def _translate_langchain_schema(tool: Any) -> Dict[str, Any]:
    """
    Translate LangChain tool schema to tea format.

    Args:
        tool: LangChain tool instance

    Returns:
        Tool schema in tea format
    """
    name = getattr(tool, 'name', tool.__class__.__name__)
    description = getattr(tool, 'description', '') or getattr(tool, '__doc__', '') or ''

    parameters = {}

    # LangChain tools use args_schema (pydantic model)
    if hasattr(tool, 'args_schema') and tool.args_schema:
        schema = tool.args_schema
        if hasattr(schema, 'model_fields'):
            # Pydantic v2
            for field_name, field_info in schema.model_fields.items():
                param_type = "string"
                if hasattr(field_info, 'annotation'):
                    ann = field_info.annotation
                    if ann == int:
                        param_type = "integer"
                    elif ann == float:
                        param_type = "number"
                    elif ann == bool:
                        param_type = "boolean"

                parameters[field_name] = {
                    "type": param_type,
                    "description": getattr(field_info, 'description', '') or '',
                    "required": field_info.default is None
                }
        elif hasattr(schema, '__fields__'):
            # Pydantic v1
            for field_name, field in schema.__fields__.items():
                param_type = "string"
                if field.type_ == int:
                    param_type = "integer"
                elif field.type_ == float:
                    param_type = "number"
                elif field.type_ == bool:
                    param_type = "boolean"

                parameters[field_name] = {
                    "type": param_type,
                    "description": field.field_info.description or '',
                    "required": field.required
                }
    else:
        # Single string input (common for simple tools)
        parameters["query"] = {
            "type": "string",
            "description": "Input query for the tool",
            "required": True
        }

    return create_tool_schema(name, description, parameters, "langchain")


def _discover_langchain_tools(filter: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Discover available LangChain tools.

    Args:
        filter: Optional filter string

    Returns:
        List of tool schemas
    """
    if not HAS_LANGCHAIN:
        return []

    tools = []
    for module_path, tool_name in KNOWN_LANGCHAIN_TOOLS:
        if filter and filter.lower() not in tool_name.lower():
            continue
        try:
            tool = _get_langchain_tool(tool_name)
            schema = _translate_langchain_schema(tool)
            tools.append(schema)
        except Exception:
            # Tool not available, skip
            continue

    return tools


# ============================================================================
# Action Registration (Task 5, 6, 7, 8)
# ============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register tools bridge actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # ========================================================================
    # tools.crewai - Execute CrewAI tools
    # ========================================================================

    def tools_crewai(state, tool, timeout=30.0, **params):
        """
        Execute a CrewAI tool.

        Bridges to CrewAI's extensive tool library. The tool parameter can be
        a class name (e.g., "SerperDevTool") or import path.

        Args:
            state: Current state dictionary
            tool: Tool name or import path
            timeout: Execution timeout in seconds (default: 30.0)
            **params: Parameters to pass to the tool

        Returns:
            {"result": any, "tool": str, "success": True} on success
            {"error": str, "error_type": str, "tool": str, "success": False} on failure

        Example:
            result = tools_crewai(state, "SerperDevTool", query="AI news")
        """
        if not HAS_CREWAI:
            return create_error_result(
                "CrewAI not installed. Install with: pip install crewai crewai-tools",
                "import",
                tool
            )

        try:
            tool_instance = _get_crewai_tool(tool)

            # Execute with timeout (CrewAI tools are sync)
            import signal

            def timeout_handler(signum, frame):
                raise TimeoutError(f"Tool execution timed out after {timeout}s")

            # Set timeout (Unix only)
            try:
                old_handler = signal.signal(signal.SIGALRM, timeout_handler)
                signal.alarm(int(timeout))
            except (AttributeError, ValueError):
                # Windows or invalid timeout
                old_handler = None

            try:
                # CrewAI tools use _run() or run() method
                if hasattr(tool_instance, '_run'):
                    result = tool_instance._run(**params)
                elif hasattr(tool_instance, 'run'):
                    result = tool_instance.run(**params)
                else:
                    # Try calling directly
                    result = tool_instance(**params)

                return normalize_result(result, tool)

            finally:
                if old_handler is not None:
                    signal.alarm(0)
                    signal.signal(signal.SIGALRM, old_handler)

        except TimeoutError as e:
            return create_error_result(str(e), "timeout", tool)
        except ImportError as e:
            return create_error_result(str(e), "import", tool)
        except Exception as e:
            return create_error_result(str(e), "execution", tool)

    registry['tools.crewai'] = tools_crewai
    registry['actions.tools_crewai'] = tools_crewai

    # ========================================================================
    # tools.mcp - Execute MCP server tools
    # ========================================================================

    def tools_mcp(state, server, tool, timeout=30.0, **params):
        """
        Execute a tool from an MCP server.

        Connects to a Model Context Protocol server and executes the specified tool.

        Args:
            state: Current state dictionary
            server: Server configuration dict with 'command', 'args', optionally 'env'
                   Or server name if configured in YAML settings
            tool: Name of the tool to execute
            timeout: Execution timeout in seconds (default: 30.0)
            **params: Parameters to pass to the tool

        Returns:
            {"result": any, "tool": str, "server": str, "success": True} on success
            {"error": str, "error_type": str, "tool": str, "success": False} on failure

        Example:
            result = tools_mcp(
                state,
                server={"command": "npx", "args": ["-y", "@anthropic/mcp-server-filesystem"]},
                tool="read_file",
                path="/tmp/test.txt"
            )
        """
        if not HAS_MCP:
            return create_error_result(
                "MCP not installed. Install with: pip install mcp",
                "import",
                tool
            )

        # Resolve server configuration
        if isinstance(server, str):
            # Look up in engine settings
            settings = getattr(engine, 'variables', {}).get('settings', {})
            mcp_config = settings.get('tools', {}).get('mcp', {})
            servers = mcp_config.get('servers', [])

            server_config = None
            for s in servers:
                if s.get('name') == server:
                    server_config = s
                    break

            if not server_config:
                return create_error_result(
                    f"MCP server '{server}' not found in configuration",
                    "configuration",
                    tool
                )
        else:
            server_config = server

        # Execute via async bridge
        result = run_async_in_sync(
            _mcp_connect_and_execute(server_config, tool, params, timeout)
        )

        # Add server info
        if result.get("success"):
            result["server"] = server_config.get("name", server_config.get("command", "unknown"))

        return result

    registry['tools.mcp'] = tools_mcp
    registry['actions.tools_mcp'] = tools_mcp

    # ========================================================================
    # tools.langchain - Execute LangChain tools
    # ========================================================================

    def tools_langchain(state, tool, timeout=30.0, **params):
        """
        Execute a LangChain tool.

        Bridges to LangChain's tool ecosystem. The tool parameter can be
        a class name (e.g., "DuckDuckGoSearchRun") or tool instance.

        Args:
            state: Current state dictionary
            tool: Tool name or class name
            timeout: Execution timeout in seconds (default: 30.0)
            **params: Parameters to pass to the tool

        Returns:
            {"result": any, "tool": str, "success": True} on success
            {"error": str, "error_type": str, "tool": str, "success": False} on failure

        Example:
            result = tools_langchain(state, "DuckDuckGoSearchRun", query="AI news")
        """
        if not HAS_LANGCHAIN:
            return create_error_result(
                "LangChain not installed. Install with: pip install langchain langchain-community",
                "import",
                tool
            )

        try:
            tool_instance = _get_langchain_tool(tool)

            # Execute with timeout
            import signal

            def timeout_handler(signum, frame):
                raise TimeoutError(f"Tool execution timed out after {timeout}s")

            try:
                old_handler = signal.signal(signal.SIGALRM, timeout_handler)
                signal.alarm(int(timeout))
            except (AttributeError, ValueError):
                old_handler = None

            try:
                # LangChain tools use invoke() or run()
                if hasattr(tool_instance, 'invoke'):
                    # Invoke expects a single input or dict
                    if len(params) == 1:
                        result = tool_instance.invoke(list(params.values())[0])
                    else:
                        result = tool_instance.invoke(params)
                elif hasattr(tool_instance, 'run'):
                    if len(params) == 1:
                        result = tool_instance.run(list(params.values())[0])
                    else:
                        result = tool_instance.run(**params)
                else:
                    result = tool_instance(**params)

                return normalize_result(result, tool)

            finally:
                if old_handler is not None:
                    signal.alarm(0)
                    signal.signal(signal.SIGALRM, old_handler)

        except TimeoutError as e:
            return create_error_result(str(e), "timeout", tool)
        except ImportError as e:
            return create_error_result(str(e), "import", tool)
        except Exception as e:
            return create_error_result(str(e), "execution", tool)

    registry['tools.langchain'] = tools_langchain
    registry['actions.tools_langchain'] = tools_langchain

    # ========================================================================
    # tools.discover - Discover available tools
    # ========================================================================

    def tools_discover(state, source="all", filter=None, use_cache=True, **kwargs):
        """
        Discover available tools from specified sources.

        Args:
            state: Current state dictionary
            source: Source to discover from: "crewai", "mcp", "langchain", "all"
            filter: Optional filter string to narrow results
            use_cache: Use cached discovery results (default: True)
            **kwargs: Additional arguments (e.g., mcp_servers for MCP discovery)

        Returns:
            {
                "tools": List[Dict],  # List of tool schemas
                "sources": List[str],  # Sources checked
                "count": int,  # Total tools found
                "success": True
            }

        Example:
            result = tools_discover(state, source="all")
            for tool in result['tools']:
                print(f"{tool['source']}: {tool['name']}")
        """
        sources_to_check = []
        if source == "all":
            sources_to_check = ["crewai", "langchain", "mcp"]
        else:
            sources_to_check = [source]

        all_tools = []
        checked_sources = []

        for src in sources_to_check:
            # Check cache first
            if use_cache:
                cached = _discovery_cache.get(src)
                if cached is not None:
                    # Apply filter to cached results
                    if filter:
                        cached = [t for t in cached if filter.lower() in t.get("name", "").lower()]
                    all_tools.extend(cached)
                    checked_sources.append(src)
                    continue

            tools = []

            if src == "crewai":
                tools = _discover_crewai_tools(filter)
            elif src == "langchain":
                tools = _discover_langchain_tools(filter)
            elif src == "mcp":
                # MCP requires server configs
                mcp_servers = kwargs.get("mcp_servers", [])

                # Also check engine settings
                settings = getattr(engine, 'variables', {}).get('settings', {})
                mcp_config = settings.get('tools', {}).get('mcp', {})
                servers = mcp_config.get('servers', [])
                mcp_servers = mcp_servers + servers

                for server_config in mcp_servers:
                    server_tools = run_async_in_sync(
                        _mcp_discover_tools(server_config)
                    )
                    if filter:
                        server_tools = [t for t in server_tools
                                       if filter.lower() in t.get("name", "").lower()]
                    tools.extend(server_tools)

            # Cache results (without filter applied)
            if use_cache and not filter:
                _discovery_cache.set(src, tools)

            all_tools.extend(tools)
            checked_sources.append(src)

        return {
            "tools": all_tools,
            "sources": checked_sources,
            "count": len(all_tools),
            "success": True
        }

    registry['tools.discover'] = tools_discover
    registry['actions.tools_discover'] = tools_discover

    # ========================================================================
    # tools.clear_cache - Clear discovery cache
    # ========================================================================

    def tools_clear_cache(state, source=None, **kwargs):
        """
        Clear the tool discovery cache.

        Args:
            state: Current state dictionary
            source: Optional source to clear ("crewai", "mcp", "langchain")
                   If None, clears all caches

        Returns:
            {"cleared": True, "source": str or None}
        """
        _discovery_cache.clear(source)
        return {"cleared": True, "source": source, "success": True}

    registry['tools.clear_cache'] = tools_clear_cache
    registry['actions.tools_clear_cache'] = tools_clear_cache
