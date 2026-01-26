# TEA-LLM-005: Add `prompt` Parameter Support to Python LLM Actions

## Status

Ready for Review

## Story

**As a** TEA developer using Python,
**I want** `llm.call`, `llm.stream`, and `llm.tools` to accept a `prompt` parameter (like the Rust implementation),
**so that** I have API parity between Python and Rust, and can use simpler single-prompt calls without constructing a messages array.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/actions/llm_actions.py`
- Technology: Python, OpenAI SDK, LiteLLM
- Follows pattern: Rust LLM actions implementation in `rust/src/actions/llm.rs`
- Touch points: `llm_call()`, `llm_stream()`, `llm_tools()` functions

**Current Behavior:**

- Python `llm.call`, `llm.stream`, `llm.tools` only accept `messages` parameter
- Rust equivalents accept both `messages` AND `prompt` (string)
- When Rust receives `prompt`, it converts to: `[{role: "user", content: prompt}]`
- Rust also supports optional `system` parameter with `prompt`

**Rust Implementation Locations:**

| Function | Lines | Pattern |
|----------|-------|---------|
| `llm.call` | 244-278 | `messages` > `prompt` > error |
| `llm.stream` | 440-478 | Same pattern |
| `llm.tools` | 733-748 | Same pattern |

## Acceptance Criteria

### Functional Requirements

1. Python `llm.call`, `llm.stream`, and `llm.tools` accept `prompt` parameter as alternative to `messages`
2. When `prompt` is provided, convert to `[{"role": "user", "content": prompt}]`
3. When both `prompt` and `system` are provided, prepend system message: `[{"role": "system", "content": system}, {"role": "user", "content": prompt}]`
4. Error if neither `prompt` nor `messages` is provided
5. If both `prompt` and `messages` are provided, prefer `messages` (match Rust behavior)

### Integration Requirements

6. Existing `messages`-based calls continue to work unchanged
7. Works with all providers: OpenAI, Azure, Ollama, LiteLLM, Shell, Local
8. All three actions (`llm.call`, `llm.stream`, `llm.tools`) have identical `prompt`/`system` behavior

### Quality Requirements

9. Unit tests cover prompt-to-messages conversion for all three actions
10. Tests verify system message prepending
11. No regression in existing functionality

## Tasks / Subtasks

- [x] Task 1: Create shared helper function `_convert_prompt_to_messages()` (AC: 2, 3, 4, 5)
  - [x] Create helper that handles prompt/system → messages conversion
  - [x] Return `None` if neither prompt nor messages provided (let caller handle error)
  - [x] Handle precedence: return original `messages` if provided, else convert `prompt`

- [x] Task 2: Add `prompt` parameter to `llm_call()` (AC: 1, 6, 7)
  - [x] Add `prompt=None` and `system=None` parameters to function signature
  - [x] Call `_convert_prompt_to_messages()` at start of function
  - [x] Return clear error if neither provided

- [x] Task 3: Add `prompt` parameter to `llm_stream()` (AC: 1, 8)
  - [x] Add `prompt=None` and `system=None` parameters to function signature
  - [x] Call `_convert_prompt_to_messages()` at start of function
  - [x] Return clear error if neither provided

- [x] Task 4: Add `prompt` parameter to `llm_tools()` (AC: 1, 8)
  - [x] Add `prompt=None` and `system=None` parameters to function signature
  - [x] Call `_convert_prompt_to_messages()` at start of function
  - [x] Return clear error if neither provided

- [x] Task 5: Add unit tests (AC: 9, 10, 11)
  - [x] Test helper function directly
  - [x] Test `llm.call` with `prompt` only
  - [x] Test `llm.call` with `prompt` + `system`
  - [x] Test `llm.stream` with `prompt`
  - [x] Test `llm.tools` with `prompt`
  - [x] Test: `messages` takes precedence over `prompt`
  - [x] Test: Error when neither provided
  - [x] Test: Existing `messages` calls unchanged

- [x] Task 6: Update documentation
  - [x] Update docstrings for all three functions
  - [x] Add examples in docstrings

## Dev Notes

### Reference Implementation (Rust)

The same pattern is used in all three Rust functions (`llm.call`, `llm.stream`, `llm.tools`):

```rust
// From rust/src/actions/llm.rs (pattern repeated in lines 244-278, 440-478, 733-748)
let messages: Vec<Message> =
    if let Some(msgs) = params.get("messages").and_then(|v| v.as_array()) {
        // Use messages array directly (takes precedence)
        msgs.clone()
    } else if let Some(prompt) = params.get("prompt").and_then(|v| v.as_str()) {
        // Convert prompt string to messages
        let mut msgs = vec![];
        if let Some(system) = params.get("system").and_then(|v| v.as_str()) {
            msgs.push(json!({"role": "system", "content": system}));
        }
        msgs.push(json!({"role": "user", "content": prompt}));
        msgs
    } else {
        return Err(TeaError::InvalidInput {
            action: "llm.call".to_string(),
            message: "Missing required parameter: prompt or messages".to_string(),
        });
    };
```

### Implementation Location

File: `python/src/the_edge_agent/actions/llm_actions.py`

**Function locations:**
- `llm_call()` - line 623
- `llm_stream()` - line 1259
- `llm_tools()` - line 1636

### Recommended Implementation

Create a shared helper function inside `register_actions()` to avoid code duplication:

```python
def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    # ... existing code ...

    def _convert_prompt_to_messages(
        messages: Optional[list],
        prompt: Optional[str],
        system: Optional[str],
    ) -> Optional[list]:
        """
        Convert prompt/system to messages format (TEA-LLM-005 - Rust parity).

        Args:
            messages: Existing messages array (takes precedence if provided)
            prompt: Simple prompt string
            system: Optional system message

        Returns:
            messages list, or None if neither messages nor prompt provided
        """
        if messages is not None:
            return messages
        if prompt is not None:
            result = []
            if system is not None:
                result.append({"role": "system", "content": system})
            result.append({"role": "user", "content": prompt})
            return result
        return None

    def llm_call(
        state,
        model=None,
        messages=None,
        prompt=None,      # NEW - TEA-LLM-005
        system=None,      # NEW - TEA-LLM-005
        temperature=0.7,
        # ... rest of existing params
    ):
        # Convert prompt to messages if provided (TEA-LLM-005)
        messages = _convert_prompt_to_messages(messages, prompt, system)
        if messages is None:
            return {
                "error": "Missing required parameter: prompt or messages",
                "success": False,
            }

        # ... rest of existing function unchanged
```

Apply the same pattern to `llm_stream()` and `llm_tools()`.

### Testing

- **Test file:** `python/tests/test_llm_prompt_parameter.py` (new file)
- Use mocking to avoid actual API calls
- Test the helper function directly for unit coverage
- Integration tests for each action with `prompt` parameter

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** None - additive change, existing API unchanged
- **Mitigation:** `messages` takes precedence, so existing code unaffected
- **Rollback:** Remove `prompt`/`system` parameters and conversion logic

**Compatibility Verification:**

- [x] No breaking changes to existing APIs
- [x] No database changes
- [x] UI changes: N/A
- [x] Performance impact is negligible

## Definition of Done

- [x] Functional requirements met
- [x] Integration requirements verified
- [x] Existing functionality regression tested
- [x] Code follows existing patterns and standards
- [x] Tests pass (existing and new)
- [x] Documentation updated

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-25 | 0.1 | Initial draft | Sarah (PO) |
| 2026-01-25 | 0.2 | Expanded scope to include `llm.stream` and `llm.tools`; added shared helper function approach | Sarah (PO) |
| 2026-01-25 | 1.0 | Implementation complete - all tasks done | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/llm_actions.py` | Modified | Added `_convert_prompt_to_messages()` helper; added `prompt`/`system` params to `llm_call`, `llm_stream`, `llm_tools` |
| `python/tests/test_llm_prompt_parameter.py` | Created | 12 unit tests for prompt parameter parity feature |

### Completion Notes

1. Implemented `_convert_prompt_to_messages()` helper function inside `register_actions()` to match the Rust implementation pattern
2. Added `prompt=None` and `system=None` parameters to all three LLM actions (`llm.call`, `llm.stream`, `llm.tools`)
3. Conversion logic placed early in each function, after provider resolution but before any provider-specific handling
4. Error handling returns `{"error": "Missing required parameter: prompt or messages", "success": False}` matching Rust behavior
5. All 12 new tests pass, plus 4488 existing tests pass (full regression)
6. Updated docstrings with comprehensive Args documentation and usage examples

### Debug Log References

None - implementation was straightforward with no debugging required.
