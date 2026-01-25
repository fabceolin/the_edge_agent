# Story TEA-RALPHY-001.5: Additional Shell Providers

## Status
Done

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** workflow developer,
**I want** OpenCode and Cursor shell providers,
**So that** I can use all Ralphy-supported AI engines.

## Acceptance Criteria

1. Add `opencode` shell provider with correct CLI invocation
2. Add `cursor` shell provider with correct CLI invocation
3. Document CLI installation requirements
4. Test with mock CLI responses

## Tasks / Subtasks

- [x] Research OpenCode CLI interface (COMPLETED)
  - [x] Command: `opencode -p "prompt" -q`
  - [x] Quiet mode (`-q`) suppresses spinner for scripting
  - [x] JSON output available with `-f json`
- [x] Research Cursor CLI interface (COMPLETED)
  - [x] Command: `agent -p "prompt" --output-format text`
  - [x] Note: Cursor CLI is named `agent`, not `cursor`
- [x] Add to `_get_default_shell_providers()` (AC: 1, 2)
  - [x] Modify `python/src/the_edge_agent/actions/llm_actions.py`
  - [x] Add `opencode` provider entry
  - [x] Add `cursor` provider entry (using `agent` command)
- [x] Add documentation (AC: 3)
  - [x] Update `docs/python/actions-reference.md`
  - [x] Document installation commands for each provider
- [x] Add unit tests (AC: 4)
  - [x] Add tests to `python/tests/test_llm_actions.py`
  - [x] Mock subprocess calls
  - [x] Test provider selection

## Dev Notes

### Research Results (Verified)

| Provider | Command | Non-Interactive | Quiet Mode | Source |
|----------|---------|-----------------|------------|--------|
| **OpenCode** | `opencode` | `-p "prompt"` | `-q` | [Docs](https://opencode.ai/docs/cli/) |
| **Cursor** | `agent` | `-p "prompt"` | `--output-format text` | [Docs](https://cursor.com/docs/cli/overview) |

### Implementation

```python
# python/src/the_edge_agent/actions/llm_actions.py

def _get_default_shell_providers() -> Dict[str, Dict[str, Any]]:
    return {
        "claude": {
            "command": "claude",
            "args": ["-p", "{prompt}", "--dangerously-skip-permissions"],
            "timeout": 108000,
        },
        "codex": {
            "command": "codex",
            "args": ["exec", "-"],
            "stdin_mode": "pipe",
            "timeout": 108000,
        },
        "gemini": {
            "command": "gemini",
            "args": ["-p", "{prompt}"],
            "timeout": 108000,
        },
        "qwen": {
            "command": "qwen",
            "args": ["-p", "{prompt}"],
            "timeout": 108000,
        },
        # NEW: OpenCode provider
        "opencode": {
            "command": "opencode",
            "args": ["-p", "{prompt}", "-q"],  # -q suppresses spinner
            "timeout": 108000,
        },
        # NEW: Cursor provider (CLI command is 'agent')
        "cursor": {
            "command": "agent",  # Cursor CLI is named 'agent'
            "args": ["-p", "{prompt}", "--output-format", "text"],
            "timeout": 108000,
        },
    }
```

### Installation Requirements

```bash
# OpenCode (Go-based)
curl -fsSL https://raw.githubusercontent.com/opencode-ai/opencode/refs/heads/main/install | bash
# OR
brew install opencode-ai/tap/opencode
# OR
go install github.com/opencode-ai/opencode@latest

# Cursor CLI (requires Cursor subscription)
# Installed via Cursor app: Cursor > Install CLI Command
# Or download from: https://cursor.com/download
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   └── llm_actions.py       # MODIFY: Add opencode, cursor providers
└── ...

docs/python/
└── actions-reference.md     # MODIFY: Document new providers
```

### YAML Usage

```yaml
nodes:
  - name: process_with_opencode
    uses: llm.call
    with:
      provider: shell
      shell_provider: opencode
      messages:
        - role: user
          content: "Implement feature X"
    output: result

  - name: process_with_cursor
    uses: llm.call
    with:
      provider: shell
      shell_provider: cursor
      messages:
        - role: user
          content: "Review this code for security issues"
    output: review
```

## Testing

**Test Location:** `python/tests/test_llm_actions.py`

```python
import pytest
from unittest.mock import patch, MagicMock
from the_edge_agent.actions.llm_actions import _get_default_shell_providers

class TestShellProviders:
    def test_opencode_provider_config(self):
        """Test OpenCode provider configuration."""
        providers = _get_default_shell_providers()

        assert "opencode" in providers
        assert providers["opencode"]["command"] == "opencode"
        assert "-p" in providers["opencode"]["args"]
        assert "-q" in providers["opencode"]["args"]  # Quiet mode

    def test_cursor_provider_config(self):
        """Test Cursor provider configuration."""
        providers = _get_default_shell_providers()

        assert "cursor" in providers
        assert providers["cursor"]["command"] == "agent"  # CLI is 'agent'
        assert "-p" in providers["cursor"]["args"]
        assert "--output-format" in providers["cursor"]["args"]

    @patch("subprocess.run")
    def test_opencode_invocation(self, mock_run):
        """Test OpenCode CLI invocation."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="Implementation complete",
            stderr=""
        )

        # Invoke via llm.call with shell provider
        # ... test implementation
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_opencode_provider_config | OpenCode config is correct | 1 |
| test_cursor_provider_config | Cursor config uses `agent` command | 2 |
| test_opencode_invocation | Mock CLI call works | 1, 4 |
| test_cursor_invocation | Mock CLI call works | 2, 4 |
| test_all_providers_registered | All 6 providers available | 1, 2 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-17 | 0.2 | Research completed for OpenCode/Cursor CLI | Sarah (PO) |
| 2026-01-19 | 0.3 | Implementation complete: providers, docs, and tests | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

None - implementation completed without debug issues.

### Completion Notes List

1. Added `opencode` provider with `-p "{prompt}" -q` args (quiet mode for scripting)
2. Added `cursor` provider using `agent` command (Cursor CLI is named `agent`, not `cursor`)
3. Updated `docs/python/actions-reference.md` with all 6 built-in providers and installation instructions
4. Added 6 unit tests with mocked subprocess calls covering:
   - Provider configuration verification
   - CLI invocation with correct args
   - All providers registered check
   - Shell provider parameter validation
5. All 21 tests pass (3 skipped - Ollama integration tests requiring local server)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/llm_actions.py` | Modified | Added opencode and cursor to `_get_default_shell_providers()` |
| `docs/python/actions-reference.md` | Modified | Added installation requirements and updated provider list |
| `python/tests/test_llm_actions.py` | Modified | Added TestShellProviderConfig and TestShellProviderInvocation test classes |

---

## QA Results

### Review Date: 2026-01-19

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Implementation is clean, focused, and follows existing patterns. The new shell providers (`opencode` and `cursor`) are added to `_get_default_shell_providers()` with correct CLI arguments matching the researched specifications. Code changes are minimal (~10 lines) and low-risk.

**Strengths:**
- Follows established provider configuration pattern exactly
- Correct CLI flags: `opencode -p "{prompt}" -q` and `agent -p "{prompt}" --output-format text`
- Cursor provider correctly mapped to `agent` command (not `cursor`)
- Good inline comments explaining non-obvious details

### Refactoring Performed

None required - implementation is appropriately simple and follows existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing codebase patterns
- Project Structure: ✓ Files in correct locations
- Testing Strategy: ✓ Mocked tests avoiding external dependencies
- All ACs Met: ✓ All 4 acceptance criteria satisfied

### Requirements Traceability

| AC | Test Coverage | Status |
|----|---------------|--------|
| 1. Add `opencode` shell provider | `test_opencode_provider_config`, `test_opencode_invocation_mock` | ✓ |
| 2. Add `cursor` shell provider | `test_cursor_provider_config`, `test_cursor_invocation_mock` | ✓ |
| 3. Document CLI installation | `docs/python/actions-reference.md` lines 158-172 | ✓ |
| 4. Test with mock CLI | 6 tests with mocked subprocess | ✓ |

### Improvements Checklist

- [x] Provider configurations match researched CLI specs
- [x] Tests properly mock subprocess to avoid hanging
- [x] Documentation includes all 6 providers
- [x] Installation instructions provided for opencode and cursor

### Security Review

No security concerns. Shell providers execute user-specified CLI tools which is the intended behavior for this feature. No new attack surface introduced.

### Performance Considerations

No performance concerns. Providers share the same 108000s timeout as existing providers.

### Files Modified During Review

None - no refactoring required.

### Test Quality Assessment

**Tests reviewed:** 6 new tests (TestShellProviderConfig: 3, TestShellProviderInvocation: 3)

**Test design quality:**
- ✓ Tests verify CLI command and argument correctness
- ✓ Tests mock subprocess.Popen correctly
- ✓ Tests verify response handling
- ✓ Tests verify error handling (missing shell_provider param)
- ✓ Tests verify all 6 providers registered

**Coverage gaps:** None identified - all acceptance criteria have corresponding test coverage.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-RALPHY-001.5-additional-shell-providers.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests pass, implementation follows patterns.
