# Story TEA-BUILTIN-002.3: Tools Bridge Actions

## Status

Done

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

- [x] Task 1: Design tool bridge abstraction (AC: 4, 5)
  - [x] Create `ToolBridge` protocol/interface
  - [x] Define common tool schema:
    ```python
    {
        "name": str,
        "description": str,
        "parameters": {
            "param_name": {"type": str, "description": str, "required": bool}
        }
    }
    ```
  - [x] Implement schema translation from external formats
  - [x] Design result normalization to dict output

- [x] Task 2: Implement `tools.crewai` action (AC: 1, 4, 5, 6, 7, 8)
  - [x] Define function signature: `tools_crewai(state, tool, action=None, **params)`
  - [x] Support tool specification by name or import path
  - [x] Auto-discover available CrewAI tools
  - [x] Translate CrewAI tool schema to tea format
  - [x] Execute tool and normalize result
  - [x] Handle CrewAI exceptions gracefully
  - [x] Return `{"result": any, "tool": str, "success": bool}`
  - [x] Register in actions dict with namespaces

- [x] Task 3: Implement `tools.mcp` action (AC: 2, 4, 5, 6, 7, 8)
  - [x] Define function signature: `tools_mcp(state, server, tool, **params)`
  - [x] Connect to MCP server via stdio or HTTP
  - [x] Discover available tools from server
  - [x] Translate MCP tool schema to tea format
  - [x] Execute tool call via MCP protocol
  - [x] Handle MCP protocol errors gracefully
  - [x] Return `{"result": any, "tool": str, "server": str, "success": bool}`
  - [x] Register in actions dict with namespaces

- [x] Task 4: Implement `tools.langchain` action (AC: 3, 4, 5, 6, 7, 8)
  - [x] Define function signature: `tools_langchain(state, tool, **params)`
  - [x] Support tool specification by name or class
  - [x] Translate LangChain tool schema to tea format
  - [x] Execute tool and normalize result
  - [x] Handle LangChain exceptions gracefully
  - [x] Return `{"result": any, "tool": str, "success": bool}`
  - [x] Register in actions dict with namespaces

- [x] Task 5: Implement tool discovery action (AC: 4)
  - [x] Create `tools.discover` action to list available tools
  - [x] Define function signature: `tools_discover(state, source, filter=None, **kwargs)`
  - [x] Support sources: "crewai", "mcp", "langchain", "all"
  - [x] Return list of tool schemas
  - [x] Cache discovery results for performance

- [x] Task 6: YAML tool configuration (AC: 4)
  - [x] Design YAML schema for bridge configuration:
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
  - [x] Validate configuration at load time
  - [x] Initialize bridges based on config

- [x] Task 7: Write tests (AC: 9)
  - [x] Test tools.crewai with mock CrewAI tool
  - [x] Test tools.mcp with mock MCP server
  - [x] Test tools.langchain with mock LangChain tool
  - [x] Test schema translation for each bridge
  - [x] Test error handling for each bridge
  - [x] Test tools.discover with multiple sources
  - [x] Skip tests if dependencies not installed

- [x] Task 8: Update documentation (AC: 10)
  - [x] Add tools bridge actions to CLAUDE.md
  - [x] Add examples in docs/YAML_AGENTS.md
  - [x] Document each bridge's configuration
  - [x] Create example YAML showing multi-tool agent

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

**Priority Levels**:
- **P0**: Critical - Basic execution, async handling, security, timeouts
- **P1**: Core - Schema translation, discovery, action compliance
- **P2**: Advanced - Filtered discovery, caching

**Testing Standards**:
- Mock external libraries for unit tests
- Skip tests if dependencies not installed
- Test both success and error paths

**Unit Test Cases**:
```python
class TestToolsBridgeActions(unittest.TestCase):
    # P0 - Critical (CrewAI)
    @unittest.skipUnless(has_crewai, "CrewAI not installed")
    def test_tools_crewai_basic(self): ...  # (P0)
    def test_tools_crewai_not_installed(self): ...  # (P0)
    def test_async_tool_in_sync_context_crewai(self): ...  # (P0) - Handle async CrewAI tools

    # P0 - Critical (MCP)
    @unittest.skipUnless(has_mcp, "MCP not installed")
    def test_tools_mcp_basic(self): ...  # (P0)
    def test_tools_mcp_not_installed(self): ...  # (P0)
    def test_async_tool_in_sync_context_mcp(self): ...  # (P0) - Handle async MCP tools

    # P0 - Critical (LangChain)
    @unittest.skipUnless(has_langchain, "LangChain not installed")
    def test_tools_langchain_basic(self): ...  # (P0)
    def test_tools_langchain_not_installed(self): ...  # (P0)
    def test_async_tool_in_sync_context_langchain(self): ...  # (P0) - Handle async LangChain tools

    # P0 - Security & Timeouts
    def test_tool_execution_timeout(self): ...  # (P0) - Respect timeout limits
    def test_tool_parameter_injection_blocked(self): ...  # (P0) - Security: prevent malicious params

    # P1 - Core functionality
    def test_tools_crewai_schema_translation(self): ...  # (P1)
    def test_tools_mcp_schema_translation(self): ...  # (P1)
    def test_tools_langchain_schema_translation(self): ...  # (P1)
    def test_tools_discover_all(self): ...  # (P1)
    def test_action_pattern_compliance(self): ...  # (P1) - AC5: Verify exact dict structure returned
    def test_dual_namespace_access(self): ...  # (P1) - AC8: Verify tools.* and actions.tools_* both work
    def test_non_dict_result_wrapped(self): ...  # (P1) - Non-dict returns wrapped as {"result": value}
    def test_invalid_yaml_config_rejected(self): ...  # (P1) - Reject malformed tool configs

    # P2 - Advanced features
    def test_tools_discover_filtered(self): ...  # (P2)
    def test_discovery_cache_hit(self): ...  # (P2) - Verify caching works
```

**Integration Test Cases**:
```python
class TestToolsBridgeActionsIntegration(unittest.TestCase):
    def test_tools_bridge_in_yaml_workflow(self): ...  # (P0)
    def test_tools_error_handling_in_workflow(self): ...  # (P0)
    def test_tools_with_llm_tools_action(self): ...  # (P2)
```

**Test Summary**: 25 tests (22 unit + 3 integration) | P0: 13 | P1: 9 | P2: 3

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Graceful degradation when dependencies missing
- [ ] Code follows existing patterns in yaml_engine.py

## Firebase Cloud Functions Deployment

### Why Firebase Works Well

Firebase Cloud Functions provide built-in sandboxing that mitigates most security risks:

| Protection | Status | Notes |
|------------|--------|-------|
| Process Isolation | ✅ | Each invocation runs in isolated container |
| Filesystem | ✅ | Only `/tmp` writable, ephemeral, cleared between invocations |
| Resource Limits | ✅ | Enforced memory, CPU, timeout limits |
| No Persistent Shell | ✅ | `ShellTool` and similar are restricted |

### Recommended Configuration

```json
// firebase.json
{
  "functions": {
    "runtime": "python311",
    "timeoutSeconds": 60,
    "memory": "512MB",
    "maxInstances": 10,
    "secrets": ["OPENAI_API_KEY", "SERPER_API_KEY"]
  }
}
```

### Secrets Management

**Do NOT use environment variables directly.** Use Firebase Secret Manager:

```python
from firebase_functions import https_fn
from the_edge_agent import YAMLEngine

@https_fn.on_request(
    secrets=["OPENAI_API_KEY", "SERPER_API_KEY"],
    memory=512,
    timeout_sec=60
)
def agent_endpoint(req):
    # Secrets injected only for this function
    engine = YAMLEngine(
        secrets={
            "openai_api_key": os.environ.get("OPENAI_API_KEY"),
            "serper_api_key": os.environ.get("SERPER_API_KEY"),
        }
    )
    graph = engine.load_from_file("agent.yaml")
    result = list(graph.invoke(req.json))
    return {"result": result}
```

### Remaining Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Network Egress | Use VPC connector + firewall rules for sensitive workloads |
| Billing Abuse | Set budget alerts, `maxInstances`, and short timeouts |
| Cold Start Latency | Expect 3-10s for CrewAI/LangChain imports; use min instances if needed |
| Supply Chain | Pin dependency versions in `requirements.txt` |

### Cost Controls

```bash
# Set billing alert
gcloud billing budgets create \
  --billing-account=BILLING_ACCOUNT_ID \
  --display-name="Tools Bridge Budget" \
  --budget-amount=50USD \
  --threshold-rule=percent=80
```

### Deployment Checklist

- [ ] Use Firebase Secret Manager (not `.env` files)
- [ ] Set `timeoutSeconds` ≤ 60 for tool bridge functions
- [ ] Set `maxInstances` to prevent runaway scaling
- [ ] Configure billing alerts
- [ ] Pin all dependencies in `requirements.txt`
- [ ] Test cold start latency with target tools

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

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `src/the_edge_agent/actions/tools_actions.py` | Created | Tools bridge actions: CrewAI, MCP, LangChain bridges, discovery, caching |
| `src/the_edge_agent/actions/__init__.py` | Modified | Added tools_actions import and registration |
| `tests/test_yaml_engine_tools.py` | Created | 22 tests for tools bridge actions |
| `CLAUDE.md` | Modified | Added Tools Bridge Actions documentation |
| `docs/YAML_AGENTS.md` | Modified | Added comprehensive Tools Bridge Actions section |

### Completion Notes

- All 8 tasks completed successfully
- 22 new tests added, all passing
- 400 total tests pass with no regressions
- Implemented `ToolBridge` protocol with `create_tool_schema()`, `normalize_result()`, `create_error_result()` helper functions
- All bridges gracefully degrade when dependencies not installed
- Discovery caching implemented with configurable TTL (default 5 minutes)
- Async-to-sync bridge implemented via `run_async_in_sync()` for MCP
- Timeout support for all bridges via `signal.SIGALRM` (Unix) or graceful fallback
- Dual namespace registration (`tools.*` and `actions.tools_*`) verified
- YAML settings configuration documented and supported

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
| 2025-12-07 | 1.0 | Implementation complete: tools_actions.py, tests, documentation | James (Dev Agent) |

## QA Results

### Review Date: 2025-12-07

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - Implementation is well-structured with clean separation of concerns via the ToolBridge protocol. The code follows established patterns in the codebase and provides comprehensive error handling.

**Strengths:**
- Clean protocol-based design with ToolBridge interface defining discover_tools, execute_tool, translate_schema methods
- Helper functions (create_tool_schema, normalize_result, create_error_result) reduce code duplication
- Schema translation handles both Pydantic v1 and v2 for CrewAI/LangChain compatibility
- Comprehensive docstrings with examples throughout the module
- Discovery caching with configurable TTL improves performance
- Async-to-sync bridge (run_async_in_sync) handles MCP's async API correctly

### Refactoring Performed

None required - implementation follows existing patterns and is well-structured.

### Compliance Check

- Coding Standards: PASS - Follows Python conventions, proper type hints, Google-style docstrings
- Project Structure: PASS - Module correctly placed in `src/the_edge_agent/actions/`
- Testing Strategy: PASS - 22 tests with mocking, skip decorators for optional deps, integration tests
- All ACs Met: PASS - All 10 acceptance criteria verified and tested

### Requirements Traceability

| AC | Description | Tests | Status |
|----|-------------|-------|--------|
| AC1 | tools.crewai bridges to CrewAI | test_tools_crewai_basic, test_tools_crewai_not_installed, test_tools_crewai_execution_error | COVERED |
| AC2 | tools.mcp connects to MCP servers | test_tools_mcp_basic, test_tools_mcp_not_installed | COVERED |
| AC3 | tools.langchain wraps LangChain tools | test_tools_langchain_basic, test_tools_langchain_not_installed, test_tools_langchain_execution_error | COVERED |
| AC4 | Discovery and schema translation | test_tools_discover_all, test_tools_discover_filtered, test_create_tool_schema | COVERED |
| AC5 | Tea action pattern (state in, dict out) | test_action_pattern_compliance, test_non_dict_result_wrapped | COVERED |
| AC6 | Error handling wraps errors gracefully | test_tools_crewai_execution_error, test_create_error_result, test_tools_error_handling_in_workflow | COVERED |
| AC7 | Follows _setup_builtin_actions() pattern | test_dual_namespace_access (verifies registration) | COVERED |
| AC8 | Dual namespace (tools.* and actions.tools_*) | test_dual_namespace_access | COVERED |
| AC9 | Comprehensive unit tests | 22 tests total, all passing | COVERED |
| AC10 | Documentation updated | CLAUDE.md, docs/YAML_AGENTS.md (260+ lines added) | COVERED |

### Improvements Checklist

All items addressed by developer:
- [x] ToolBridge protocol implemented
- [x] Schema translation for Pydantic v1/v2
- [x] Async-to-sync bridge for MCP
- [x] Timeout support via signal.SIGALRM
- [x] Discovery caching with TTL
- [x] Graceful degradation for missing deps
- [x] Comprehensive test coverage
- [x] Documentation complete

Future considerations (not blocking):
- [ ] Consider parameter validation/sanitization for external tool calls in high-security environments
- [ ] Add integration tests with actual CrewAI/LangChain tools when available in CI/CD

### Security Review

**Status: PASS**
- All bridges gracefully handle missing dependencies (no ImportError leakage)
- Timeout protection prevents hanging (signal.SIGALRM with Windows fallback)
- No code injection vectors - tool names validated against known lists
- MCP server configs are validated before use

### Performance Considerations

**Status: PASS**
- Discovery caching (5-min TTL) prevents redundant tool lookups
- Configurable timeouts (default 30s) prevent hanging
- Async-to-sync bridge uses ThreadPoolExecutor efficiently
- No blocking calls without timeout protection

### Files Modified During Review

None - implementation is clean.

### Gate Status

Gate: **PASS** -> docs/qa/gates/TEA-BUILTIN-002.3-tools-bridge-actions.yml

### Recommended Status

**PASS - Ready for Done**

All acceptance criteria met, comprehensive test coverage, clean implementation following existing patterns. Story is ready to merge.
