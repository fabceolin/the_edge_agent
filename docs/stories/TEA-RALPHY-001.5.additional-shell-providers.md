# Story TEA-RALPHY-001.5: Additional Shell Providers

## Status
Draft

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
- [ ] Add to `_get_default_shell_providers()` (AC: 1, 2)
  - [ ] Modify `python/src/the_edge_agent/actions/llm_actions.py`
  - [ ] Add `opencode` provider entry
  - [ ] Add `cursor` provider entry (using `agent` command)
- [ ] Add documentation (AC: 3)
  - [ ] Update `docs/python/actions-reference.md`
  - [ ] Document installation commands for each provider
- [ ] Add unit tests (AC: 4)
  - [ ] Add tests to `python/tests/test_llm_actions.py`
  - [ ] Mock subprocess calls
  - [ ] Test provider selection

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

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
