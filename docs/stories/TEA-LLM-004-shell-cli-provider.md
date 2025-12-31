# Status

Ready for Review

# Story

**As a** TEA workflow developer,
**I want** to use `provider: shell` in `llm.call` and `llm.stream` actions with configurable CLI commands,
**so that** I can leverage local LLM CLIs (claude, gemini, qwen) that I already have installed, avoiding API costs while using familiar command-line tools.

# Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/actions/llm_actions.py` (~1420 lines)
- Technology: Python, subprocess, YAML settings
- Follows pattern: Existing provider detection (`openai`, `azure`, `ollama`, `litellm`)
- Touch points: `llm.call`, `llm.stream` actions, `settings.llm.shell_providers` configuration

**Current Provider Architecture:**

The existing implementation uses a priority-based provider detection system:
1. Explicit `provider` parameter (highest priority)
2. Environment variable detection (OLLAMA_API_BASE, AZURE_OPENAI_*)
3. Default to OpenAI

This story adds `shell` as a new explicit provider that executes local CLI commands via subprocess, with flexible configuration for different CLI tools.

**Key Design Decisions:**

1. **stdin for input**: Use pipes (stdin) to send prompts/context to avoid shell argument length limits
2. **Configurable templates**: Allow different CLI syntaxes via settings YAML
3. **File fallback**: Support temporary files for very large contexts when needed
4. **Streaming via line buffering**: Read stdout incrementally for `llm.stream`

# Acceptance Criteria

## Functional Requirements

1. **AC1**: `llm.call` accepts `provider: shell` and executes a configured CLI command
2. **AC2**: `llm.stream` accepts `provider: shell` and streams output line-by-line from CLI stdout
3. **AC3**: Shell providers are configurable via `settings.llm.shell_providers` in YAML
4. **AC4**: Messages are sent to CLI via stdin (pipe) to avoid shell argument length limits
5. **AC5**: Existing providers (`openai`, `azure`, `ollama`, `litellm`) continue to work unchanged

## Configuration Requirements

6. **AC6**: Each shell provider supports: `command`, `args`, `prompt_flag`, `stdin_mode`
7. **AC7**: Default providers pre-configured: `claude`, `gemini`, `qwen`
8. **AC8**: Environment variables can be used in command configuration (`${VAR}` syntax)
9. **AC9**: Timeout configuration supported per provider (default: 300s)

## Quality Requirements

10. **AC10**: Unit tests cover shell provider with mocked subprocess
11. **AC11**: Documentation updated in `docs/shared/YAML_REFERENCE.md` and `docs/python/actions-reference.md`
12. **AC12**: No regression in existing LLM action behavior (all existing tests pass)
13. **AC13**: Graceful error handling for CLI not found, timeout, non-zero exit codes

# Tasks / Subtasks

- [x] **Task 1: Design shell provider configuration schema** (AC: 6, 7)
  - [x] Define `settings.llm.shell_providers` schema in YAML
  - [x] Create default configurations for claude, gemini, qwen
  - [x] Support `stdin_mode: pipe` (default) and `stdin_mode: file` (for large contexts)

- [x] **Task 2: Implement shell provider in `llm.call`** (AC: 1, 4, 6, 8, 9, 13)
  - [x] Add `shell` case to provider resolution in `llm_call()` function
  - [x] Load shell provider config from engine settings
  - [x] Build command with arguments and environment variable expansion
  - [x] Execute via `subprocess.Popen` with stdin pipe
  - [x] Format messages into prompt text for stdin
  - [x] Handle stdout/stderr capture
  - [x] Implement timeout handling
  - [x] Parse CLI output (assume plain text response)

- [x] **Task 3: Implement shell provider in `llm.stream`** (AC: 2, 4)
  - [x] Add `shell` case to `llm_stream()` function
  - [x] Use `subprocess.Popen` with stdout line buffering
  - [x] Yield lines as they arrive (or aggregate like current pattern)
  - [x] Return consistent response format with chunk_count

- [x] **Task 4: Add settings schema support** (AC: 3, 7, 8)
  - [x] Extend YAMLEngine settings parsing for `llm.shell_providers`
  - [x] Add default shell provider configs (claude, gemini, qwen)
  - [x] Implement environment variable expansion in config values

- [x] **Task 5: Write unit tests** (AC: 10, 12, 13)
  - [x] Create `tests/test_llm_shell_provider.py`
  - [x] Mock `subprocess.Popen` for deterministic tests
  - [x] Test `llm.call` with `provider: shell`
  - [x] Test `llm.stream` with `provider: shell`
  - [x] Test error handling (command not found, timeout, non-zero exit)
  - [x] Test large input via stdin
  - [x] Verify existing tests still pass (no regression)

- [x] **Task 6: Update documentation** (AC: 11)
  - [x] Add Shell provider section to `docs/shared/YAML_REFERENCE.md`
  - [x] Add Shell examples to `docs/python/actions-reference.md`
  - [x] Document settings schema for `llm.shell_providers`
  - [x] Include examples for claude, gemini, qwen

# Dev Notes

## Shell Provider Configuration Schema

**Settings YAML schema:**
```yaml
settings:
  llm:
    shell_providers:
      claude:
        command: claude
        args: ["-p"]                    # Arguments before prompt
        stdin_mode: pipe                # pipe (default) or file
        timeout: 300                    # seconds
        env:                            # Optional extra env vars
          ANTHROPIC_API_KEY: "${ANTHROPIC_API_KEY}"

      gemini:
        command: gemini
        args: ["prompt"]
        stdin_mode: pipe
        timeout: 300

      qwen:
        command: qwen
        args: []
        stdin_mode: pipe
        timeout: 300

      # Custom provider example
      my_local_llm:
        command: /usr/local/bin/my-llm
        args: ["--model", "mistral-7b", "--input", "-"]
        stdin_mode: pipe
        timeout: 600
```

**Usage in workflow:**
```yaml
- name: ask_claude
  uses: llm.call
  with:
    provider: shell
    shell_provider: claude            # Which shell provider config to use
    messages:
      - role: user
        content: "{{ state.question }}"
  output: response
```

## Implementation Pattern

**File**: `python/src/the_edge_agent/actions/llm_actions.py`

```python
def llm_call(..., provider="auto", shell_provider=None, ...):
    # ... existing provider detection ...

    if resolved_provider == "shell":
        return _execute_shell_provider(
            engine=engine,
            shell_provider=shell_provider,
            messages=messages,
            state=state,
            **kwargs
        )

def _execute_shell_provider(engine, shell_provider, messages, state, **kwargs):
    """Execute LLM call via shell CLI command."""
    import subprocess
    import shlex

    # Get shell provider config from settings
    settings = engine.settings or {}
    shell_providers = settings.get("llm", {}).get("shell_providers", {})

    if shell_provider not in shell_providers:
        # Try default configs
        shell_providers = _get_default_shell_providers()

    config = shell_providers.get(shell_provider)
    if not config:
        raise ValueError(f"Shell provider '{shell_provider}' not configured")

    # Build command
    command = config.get("command", shell_provider)
    args = config.get("args", [])
    timeout = config.get("timeout", 300)
    stdin_mode = config.get("stdin_mode", "pipe")

    # Format messages into prompt text
    prompt_text = _format_messages_for_cli(messages)

    # Execute command
    full_command = [command] + args

    try:
        if stdin_mode == "pipe":
            proc = subprocess.Popen(
                full_command,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            stdout, stderr = proc.communicate(input=prompt_text, timeout=timeout)
        elif stdin_mode == "file":
            # Write to temp file for very large contexts
            import tempfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
                f.write(prompt_text)
                temp_path = f.name
            try:
                # Replace {input_file} placeholder in args
                file_args = [a.replace('{input_file}', temp_path) for a in args]
                proc = subprocess.Popen(
                    [command] + file_args,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                )
                stdout, stderr = proc.communicate(timeout=timeout)
            finally:
                os.unlink(temp_path)

        if proc.returncode != 0:
            return {
                "error": f"Shell command failed (exit {proc.returncode}): {stderr}",
                "success": False,
            }

        return {
            "content": stdout.strip(),
            "usage": {},  # CLI doesn't provide token counts
            "provider": "shell",
            "shell_provider": shell_provider,
        }

    except subprocess.TimeoutExpired:
        proc.kill()
        return {
            "error": f"Shell command timed out after {timeout}s",
            "success": False,
        }
    except FileNotFoundError:
        return {
            "error": f"Shell command not found: {command}",
            "success": False,
        }

def _format_messages_for_cli(messages):
    """Format chat messages into plain text for CLI stdin."""
    parts = []
    for msg in messages:
        role = msg.get("role", "user")
        content = msg.get("content", "")
        if role == "system":
            parts.append(f"System: {content}")
        elif role == "assistant":
            parts.append(f"Assistant: {content}")
        else:
            parts.append(content)
    return "\n\n".join(parts)

def _get_default_shell_providers():
    """Return default shell provider configurations."""
    return {
        "claude": {
            "command": "claude",
            "args": ["-p"],
            "stdin_mode": "pipe",
            "timeout": 300,
        },
        "gemini": {
            "command": "gemini",
            "args": ["prompt"],
            "stdin_mode": "pipe",
            "timeout": 300,
        },
        "qwen": {
            "command": "qwen",
            "args": [],
            "stdin_mode": "pipe",
            "timeout": 300,
        },
    }
```

## Message Formatting

Based on the user's example usage:
```bash
(git diff; git diff --cached) | claude -p 'prompt text here'
```

The CLI expects the prompt via stdin with the `-p` flag specifying additional instructions. Format:
- System message: Prepended as context
- User message: Main content (can include piped input)
- Multi-turn: Concatenate with clear separators

## Testing

**Test file location**: `python/tests/test_llm_shell_provider.py`

**Testing pattern:**
```python
from unittest.mock import Mock, patch, MagicMock

def test_llm_call_shell_provider():
    """Test llm.call with provider=shell."""
    mock_proc = MagicMock()
    mock_proc.returncode = 0
    mock_proc.communicate.return_value = ("Hello from Claude CLI", "")

    with patch('subprocess.Popen', return_value=mock_proc) as mock_popen:
        result = registry['llm.call'](
            state={},
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude"
        )

        assert result['content'] == "Hello from Claude CLI"
        mock_popen.assert_called_once()
        # Verify stdin was used
        call_args = mock_popen.call_args
        assert call_args[1]['stdin'] == subprocess.PIPE
```

**Test commands:**
```bash
# Run new tests
cd python && pytest tests/test_llm_shell_provider.py -v

# Run all LLM tests to verify no regression
cd python && pytest tests/test_llm_actions.py tests/test_llm_litellm_provider.py -v

# Run full test suite
cd python && pytest
```

# Testing

**Test file location**: `python/tests/test_llm_shell_provider.py`

**Testing standards**:
- Use `pytest` framework
- Mock `subprocess.Popen` for deterministic tests
- Follow existing patterns in `test_llm_actions.py`
- Test both success and error paths
- Test stdin pipe mode
- Test timeout handling
- Test command not found error

# Definition of Done

- [x] Functional requirements met (AC1-AC5)
- [x] Configuration requirements verified (AC6-AC9)
- [x] Existing functionality regression tested (AC12)
- [x] Code follows existing patterns and standards
- [x] Tests pass (existing and new)
- [x] Documentation updated (AC11)

# Risk and Compatibility Check

**Primary Risk**: Shell command injection if user input not properly escaped

**Mitigation**:
- Use `subprocess.Popen` with list arguments (no shell=True)
- Send user content only via stdin (never in command arguments)
- Validate shell_provider name against configured providers only

**Secondary Risk**: Large context may exceed CLI's stdin buffer

**Mitigation**:
- Support `stdin_mode: file` for temporary file fallback
- Document size limitations per CLI tool

**Rollback**: Remove `shell` case from provider switch; no impact on existing providers

**Compatibility Verification**:
- [x] No breaking changes to existing APIs (`provider: openai/azure/ollama/litellm` unchanged)
- [x] No database changes
- [x] UI changes: N/A (Python-only, CLI/API)
- [x] Performance impact: Negligible (only affects calls with `provider: shell`)

# Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-28 | 1.0 | Initial story draft | Sarah (PO Agent) |
| 2024-12-28 | 1.1 | Story approved | Fabricio (User) |

---

# Dev Agent Record

## Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

## File List

**Modified:**
- `python/src/the_edge_agent/actions/llm_actions.py` - Added shell provider implementation, `_execute_shell_provider()`, `_stream_shell_provider()`, `_get_default_shell_providers()`, `_expand_env_vars()`, `_format_messages_for_cli()` helper functions
- `python/src/the_edge_agent/yaml_engine.py` - Added `shell_providers` attribute and parsing from `settings.llm.shell_providers`
- `docs/shared/YAML_REFERENCE.md` - Added Shell Provider documentation section with configuration examples
- `docs/python/actions-reference.md` - Added Shell CLI Provider section

**Created:**
- `python/tests/test_llm_shell_provider.py` - 20 unit tests for shell provider functionality

## Completion Notes

1. **Implementation Complete**: All 6 tasks completed successfully
   - Shell provider supports `llm.call` and `llm.stream` actions
   - Three built-in providers: `claude`, `gemini`, `qwen`
   - Custom providers configurable via `settings.llm.shell_providers`
   - Environment variable expansion with `${VAR}` syntax
   - Both `pipe` and `file` stdin modes supported

2. **Test Results**: 59 tests passed, 3 skipped (integration tests requiring real Ollama)
   - 20 new shell provider tests
   - All existing LLM provider tests pass (no regression)

3. **Key Design Decisions**:
   - Messages sent via stdin to avoid shell argument length limits
   - subprocess.Popen with list arguments (no shell=True) for security
   - Error responses follow existing LLM action patterns
   - Default configs merged with user configs (user takes precedence)

## Debug Log References

No debug logs required - all tests passed on first run after minor mock adjustments.

---

## QA Results

### QA Review - 2024-12-29

**Reviewer**: Quinn (QA Agent)
**Model**: Claude Opus 4.5

---

#### Real Integration Tests (Without Mocks)

| Test | Result | Details |
|------|--------|---------|
| `llm.call` with `claude` CLI | ✅ PASS | Returns `{"content": ..., "provider": "shell", "shell_provider": "claude"}` |
| `llm.stream` with `claude` CLI | ✅ PASS | Returns `{"streamed": True, "chunk_count": 1, ...}` |
| `stdin_mode: file` | ✅ PASS | Temp file created, `{input_file}` placeholder replaced, file read correctly |
| Temp file cleanup | ✅ PASS | No temp file leak after 3 consecutive calls |
| YAML `shell_providers` parsing | ✅ PASS | Custom config with command, args, timeout, env parsed correctly |
| Error: Missing `shell_provider` | ✅ PASS | Returns descriptive error message |
| Error: Command not found | ✅ PASS | Returns `Shell command not found: ...` |
| Error: Unconfigured provider | ✅ PASS | Lists available providers in error |
| Error: Non-zero exit code | ✅ PASS | Returns exit code in error message |

**Note on `stdin_mode: file`**: This mode is for CLIs that accept a file path argument (e.g., `my-llm --input {input_file}`). The `{input_file}` placeholder gets replaced with the temp file path. The `claude -p` flag expects stdin, not a file path, so it uses `stdin_mode: pipe` (the default).

#### Code Quality Assessment

| Aspect | Rating | Notes |
|--------|--------|-------|
| Security | ✅ Good | Uses `subprocess.Popen` with list args (no `shell=True`), stdin for user content |
| Error Handling | ✅ Good | Covers timeout, command not found, non-zero exit, unknown stdin_mode |
| Code Organization | ✅ Good | Helper functions well-named and single-purpose |
| Documentation | ✅ Good | Docstrings complete with Args/Returns |
| Test Coverage | ✅ Good | 20 unit tests covering success/error paths |

#### Acceptance Criteria Verification

| AC | Status | Verification |
|----|--------|--------------|
| AC1: `llm.call` provider=shell | ✅ Met | Real integration test passed |
| AC2: `llm.stream` provider=shell | ✅ Met | Real integration test passed |
| AC3: YAML settings.llm.shell_providers | ✅ Met | Parsing test passed |
| AC4: stdin pipe mode | ✅ Met | Messages sent via stdin |
| AC5: Existing providers unchanged | ✅ Met | 39 existing LLM tests pass |
| AC6: Provider config schema | ✅ Met | command, args, stdin_mode, timeout, env |
| AC7: Default providers | ✅ Met | claude, gemini, qwen pre-configured |
| AC8: Env var expansion | ✅ Met | `${VAR}` syntax implemented |
| AC9: Timeout config | ✅ Met | Per-provider timeout with override |
| AC10: Unit tests | ✅ Met | 20 tests in test_llm_shell_provider.py |
| AC11: Documentation | ✅ Met | YAML_REFERENCE.md and actions-reference.md updated |
| AC12: No regression | ✅ Met | 59 passed, 3 skipped |
| AC13: Error handling | ✅ Met | All error paths tested |

#### Out of Scope (Noted)

- `llm.tools` does NOT support shell provider (intentionally - not in story scope)
- A future story could add tool calling support if CLI tools support structured output

#### Risk Assessment

| Risk | Level | Mitigation |
|------|-------|------------|
| Shell injection | Low | List args, stdin for user content, no `shell=True` |
| Large context buffer | Low | `stdin_mode: file` option available |
| CLI availability | Low | Clear error message if command not found |

---

### Gate Decision

**PASS** ✅

All acceptance criteria met. Real integration tests confirm the implementation works with the `claude` CLI. Error handling is comprehensive and follows existing patterns.

**Recommendation**: Ready for merge.
