# Story TEA-BUILTIN-002.3: Tools Bridge Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in tools bridge actions (CrewAI, MCP, LangChain),
**so that** I can leverage existing tool ecosystems without rewriting them or writing Python code.

## Acceptance Criteria

1. `tools.crewai` action bridges to CrewAI's 700+ tool library
2. `tools.mcp` action connects to Model Context Protocol servers
3. `tools.langchain` action wraps LangChain tools for use in tea
4. Bridges handle tool discovery and schema translation automatically
5. Tool execution follows tea's action pattern (state in, dict out)
6. Error handling wraps external library errors gracefully
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `tools.*` and `actions.tools_*` namespaces
9. Comprehensive unit tests cover all bridge operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**:
- TEA-BUILTIN-001.2 (LLM Enhanced) - `llm.tools` establishes action dispatch pattern

**Blocks**: None

**Internal Dependencies**:
- Uses action dispatch pattern from `llm.tools`
- All bridge dependencies are optional

## User Prerequisites

- [ ] **Optional**: `pip install crewai crewai-tools` (for CrewAI bridge)
- [ ] **Optional**: `pip install mcp` (for MCP bridge)
- [ ] **Optional**: `pip install langchain langchain-community` (for LangChain bridge)
- [ ] **Note**: At least one bridge dependency required for any bridge to work

## Tasks / Subtasks

- [ ] Task 1: Design tool bridge abstraction (AC: 4, 5)
  - [ ] Create `ToolBridge` protocol/interface
  - [ ] Define common tool schema:
    ```python
    {
        "name": str,
        "description": str,
        "parameters": {
            "param_name": {"type": str, "description": str, "required": bool}
        }
    }
    ```
  - [ ] Implement schema translation from external formats
  - [ ] Design result normalization to dict output

- [ ] Task 2: Implement `tools.crewai` action (AC: 1, 4, 5, 6, 7, 8)
  - [ ] Define function signature: `tools_crewai(state, tool, action=None, **params)`
  - [ ] Support tool specification by name or import path
  - [ ] Auto-discover available CrewAI tools
  - [ ] Translate CrewAI tool schema to tea format
  - [ ] Execute tool and normalize result
  - [ ] Handle CrewAI exceptions gracefully
  - [ ] Return `{"result": any, "tool": str, "success": bool}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 3: Implement `tools.mcp` action (AC: 2, 4, 5, 6, 7, 8)
  - [ ] Define function signature: `tools_mcp(state, server, tool, **params)`
  - [ ] Connect to MCP server via stdio or HTTP
  - [ ] Discover available tools from server
  - [ ] Translate MCP tool schema to tea format
  - [ ] Execute tool call via MCP protocol
  - [ ] Handle MCP protocol errors gracefully
  - [ ] Return `{"result": any, "tool": str, "server": str, "success": bool}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 4: Implement `tools.langchain` action (AC: 3, 4, 5, 6, 7, 8)
  - [ ] Define function signature: `tools_langchain(state, tool, **params)`
  - [ ] Support tool specification by name or class
  - [ ] Translate LangChain tool schema to tea format
  - [ ] Execute tool and normalize result
  - [ ] Handle LangChain exceptions gracefully
  - [ ] Return `{"result": any, "tool": str, "success": bool}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 5: Implement tool discovery action (AC: 4)
  - [ ] Create `tools.discover` action to list available tools
  - [ ] Define function signature: `tools_discover(state, source, filter=None, **kwargs)`
  - [ ] Support sources: "crewai", "mcp", "langchain", "all"
  - [ ] Return list of tool schemas
  - [ ] Cache discovery results for performance

- [ ] Task 6: YAML tool configuration (AC: 4)
  - [ ] Design YAML schema for bridge configuration:
    ```yaml
    settings:
      tools:
        crewai:
          enabled: true
          tools: [SerperDevTool, ScrapeWebsiteTool]
        mcp:
          servers:
            - name: filesystem
              command: npx
              args: ["-y", "@anthropic/mcp-server-filesystem"]
        langchain:
          enabled: true
          tools: [DuckDuckGoSearchRun, WikipediaQueryRun]
    ```
  - [ ] Validate configuration at load time
  - [ ] Initialize bridges based on config

- [ ] Task 7: Write tests (AC: 9)
  - [ ] Test tools.crewai with mock CrewAI tool
  - [ ] Test tools.mcp with mock MCP server
  - [ ] Test tools.langchain with mock LangChain tool
  - [ ] Test schema translation for each bridge
  - [ ] Test error handling for each bridge
  - [ ] Test tools.discover with multiple sources
  - [ ] Skip tests if dependencies not installed

- [ ] Task 8: Update documentation (AC: 10)
  - [ ] Add tools bridge actions to CLAUDE.md
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Document each bridge's configuration
  - [ ] Create example YAML showing multi-tool agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **All Optional** - bridges only work if dependencies installed
- **CrewAI**: `crewai`, `crewai-tools`
- **MCP**: `mcp` (Model Context Protocol client)
- **LangChain**: `langchain`, `langchain-community`

### CrewAI Integration Pattern
```python
def tools_crewai(state, tool, **params):
    try:
        from crewai_tools import SerperDevTool  # Example
        tool_instance = get_crewai_tool(tool)
        result = tool_instance.run(**params)
        return {"result": result, "tool": tool, "success": True}
    except ImportError:
        return {"error": "CrewAI not installed", "success": False}
```

### MCP Integration Pattern
```python
def tools_mcp(state, server, tool, **params):
    from mcp import Client
    async with Client(server) as client:
        tools = await client.list_tools()
        result = await client.call_tool(tool, params)
        return {"result": result, "tool": tool, "server": server, "success": True}
```

### LangChain Integration Pattern
```python
def tools_langchain(state, tool, **params):
    from langchain.tools import get_tool
    tool_instance = get_tool(tool)
    result = tool_instance.invoke(params)
    return {"result": result, "tool": tool, "success": True}
```

### Key Constraints
- All bridges are optional - graceful degradation
- Bridges should handle async tools in sync context
- Schema translation must preserve required/optional semantics
- Tool execution should respect timeouts

### Error Handling
```python
{
    "success": False,
    "error": str,
    "error_type": "import" | "execution" | "timeout" | "schema",
    "tool": str
}
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Testing Standards**:
- Mock external libraries for unit tests
- Skip tests if dependencies not installed
- Test both success and error paths

**Unit Test Cases**:
```python
class TestToolsBridgeActions(unittest.TestCase):
    @unittest.skipUnless(has_crewai, "CrewAI not installed")
    def test_tools_crewai_basic(self): ...
    def test_tools_crewai_not_installed(self): ...
    def test_tools_crewai_schema_translation(self): ...

    @unittest.skipUnless(has_mcp, "MCP not installed")
    def test_tools_mcp_basic(self): ...
    def test_tools_mcp_not_installed(self): ...
    def test_tools_mcp_schema_translation(self): ...

    @unittest.skipUnless(has_langchain, "LangChain not installed")
    def test_tools_langchain_basic(self): ...
    def test_tools_langchain_not_installed(self): ...
    def test_tools_langchain_schema_translation(self): ...

    def test_tools_discover_all(self): ...
    def test_tools_discover_filtered(self): ...
```

**Integration Test Cases**:
```python
class TestToolsBridgeActionsIntegration(unittest.TestCase):
    def test_tools_bridge_in_yaml_workflow(self): ...
    def test_tools_with_llm_tools_action(self): ...
    def test_tools_error_handling_in_workflow(self): ...
```

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Graceful degradation when dependencies missing
- [ ] Code follows existing patterns in yaml_engine.py

## Rollback Procedure

If tools bridge actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['tools.crewai'] = tools_crewai
   # actions['tools.mcp'] = tools_mcp
   # actions['tools.langchain'] = tools_langchain
   # actions['tools.discover'] = tools_discover
   ```

2. **State Cleanup**:
   - No persistent state; safe to remove
   - External tool libraries remain installed but unused

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify `llm.tools` still works independently

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_tools_bridge=False)`
   - Enable one bridge at a time (crewai → langchain → mcp)
   - Each bridge is independent; can enable/disable individually

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
