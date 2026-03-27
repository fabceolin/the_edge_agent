# BUG.004: ratelimit.wrap Silently Drops Extra kwargs

**Status:** ready-for-dev
**Priority:** Medium
**Reported:** 2026-03-20
**Reporter:** Dev Agent (llm_prompt POC)
**Affected Version:** TEA current

---

## Story

**As a** TEA YAML agent developer,
**I want** `ratelimit.wrap` to forward all parameters from `args` to the inner action (filtering None values),
**so that** I don't need duplicate action nodes just to handle optional parameters like `response_format`.

---

## Problem

In `ratelimit_actions.py`, the `ratelimit_wrap` function signature accepts `**kwargs`:

```python
def ratelimit_wrap(
    state, action, args, limiter, rpm=None, rps=None, timeout=None, **kwargs
) -> Dict[str, Any]:
```

These `**kwargs` are **captured but never forwarded** to the inner action. Only the `args` dict is spread:

```python
result = registry[action](state=state, **args)  # line 339
```

This means if you pass parameters at the top level of `with:` (outside `args`), they silently disappear:

```yaml
# response_format is silently dropped!
- uses: ratelimit.wrap
  with:
    action: llm.call
    args:
      model: gpt-4
      messages: "{{ state.messages }}"
    response_format: "{{ state.response_format }}"  # LOST — captured in **kwargs but never used
```

### Secondary issue: None values in args

When `args` contains a key with a `None` value (e.g., from a Jinja2 template that resolved to None), that `None` is passed through to the inner action. For `llm.call`, passing `response_format=None` to the OpenAI SDK can cause errors.

### Current workaround

Use two separate action nodes (one with the optional parameter, one without) and route between them with conditional edges.

---

## Acceptance Criteria

- [ ] **AC-1:** `args` dict values that are `None` are filtered out before forwarding to the inner action
- [ ] **AC-2:** Existing agents that pass explicit non-None values in `args` are unaffected
- [ ] **AC-3:** Unit test: `args: {response_format: null}` does not pass `response_format` to inner action
- [ ] **AC-4:** Unit test: `args: {response_format: {type: json_object}}` correctly passes to inner action
- [ ] **AC-5:** (Optional) Top-level kwargs beyond the known parameters are merged into `args` before forwarding

---

## Tasks / Subtasks

### Task 1: Filter None values from args (AC-1, AC-2)

- [ ] 1.1 In `ratelimit_actions.py` `ratelimit_wrap`, add None filtering before the inner action call:
  ```python
  filtered_args = {k: v for k, v in args.items() if v is not None}
  result = registry[action](state=state, **filtered_args)
  ```
- [ ] 1.2 Apply same filtering on the error recovery path (line 302)

### Task 2: (Optional) Forward extra kwargs (AC-5)

- [ ] 2.1 Merge `**kwargs` into `args` before forwarding:
  ```python
  merged_args = {**kwargs, **args}  # args takes precedence
  filtered_args = {k: v for k, v in merged_args.items() if v is not None}
  ```
- [ ] 2.2 Only merge kwargs that aren't internal ratelimit params

### Task 3: Tests (AC-3, AC-4)

- [ ] 3.1 Test: None value in args is stripped before forwarding
- [ ] 3.2 Test: Non-None values pass through unchanged
- [ ] 3.3 Test: Empty args dict works correctly

---

## Dev Notes

### Root cause location

`python/src/the_edge_agent/actions/ratelimit_actions.py`, `ratelimit_wrap` function, lines 302 and 339.

### Impact on llm_prompt agent

With this fix, the `llm_prompt.yaml` agent could use a single LLM call node with conditional `response_format` in args instead of the current two-node pattern:

```yaml
# After fix: single node, response_format=None gets filtered out
- name: invoke_llm
  uses: ratelimit.wrap
  with:
    action: llm.call
    args:
      model: "{{ settings.llm.model }}"
      messages: "{{ state.llm_messages }}"
      response_format: "{{ state.response_format }}"  # None when not structured — gets filtered
  output: llm_result
```

### References

- [Source: python/src/the_edge_agent/actions/ratelimit_actions.py#ratelimit_wrap]
- Discovered during: `visionQuest/ai-workflow/agents/llm_prompt.yaml` POC
