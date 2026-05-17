"""Regression test for uPilot Story 3.5 AC1.

When ``auto_trace_llm_payloads: true`` is set on a workflow that uses
``dynamic_parallel``, every ``llm.call`` invocation fanned out by the
parallel node must produce a span in the sibling ``*.llm.jsonl`` file —
not just the parent ``dynamic_parallel`` span.

Background: the parent ``dynamic_parallel`` node opens a span on the
*main* thread. Each branch is executed in a worker thread via
``ThreadPoolExecutor``. Because ``TraceContext`` uses
``threading.local()`` for the span stack, worker threads start with an
empty stack — so ``_capture_llm_payload_to_span`` finds
``current_span() is None`` and silently drops the payload. The fix is
to open a per-branch span on the worker thread before invoking the
branch function.

Without the fix, only the top-level (non-parallel) ``llm.call`` spans
land in ``*.llm.jsonl``; the fan-out LLM calls — which are usually the
bulk of token spend — are invisible. See
``upilot/_bmad-output/implementation-artifacts/3-5-extracao-ia-hardening.md``
AC1 for context.
"""

from __future__ import annotations

import json
import threading
from pathlib import Path
from tempfile import TemporaryDirectory

import pytest

from the_edge_agent import LLM_PAYLOAD_KEY, YAMLEngine


def _fake_llm_call_action(state, **kwargs):
    """Stand-in for the real ``llm.call`` action.

    Mirrors the public contract: returns ``{content, usage, cost_usd}``
    *and* attaches ``llm_payload`` to the current trace span via the
    same code path the real action uses (``_capture_llm_payload_to_span``).

    The action runs on the worker thread (because the branch executor
    invokes it there). It looks up the engine's trace context, fetches
    ``current_span()`` and stores the payload — which is exactly what
    the real action does in its closure.
    """
    engine = kwargs.get("_engine")
    item = state.get("item")

    response_payload = {
        "messages_input": [{"role": "user", "content": f"item={item}"}],
        "response_content": f"ok-{item}",
        "tokens_input": 10 + (item or 0),
        "tokens_output": 5,
        "model": "test-model",
        "stop_reason": "end_turn",
        "cost_usd": 0.0001,
    }

    if engine is not None and getattr(engine, "_llm_payload_capture", False):
        ctx = getattr(engine, "_trace_context", None)
        if ctx is not None:
            span = ctx.current_span()
            if span is not None:
                metadata = span.setdefault("metadata", {})
                metadata[LLM_PAYLOAD_KEY] = response_payload

    return {
        "content": response_payload["response_content"],
        "usage": {"prompt_tokens": 10, "completion_tokens": 5},
        "cost_usd": 0.0001,
    }


def _build_workflow(tmpdir: Path) -> tuple[YAMLEngine, object, Path]:
    trace_file = tmpdir / "run.jsonl"
    yaml_text = f"""
name: test-dynamic-parallel-trace
description: Minimal repro for AC1.

state_schema:
  batches: object
  results: object

settings:
  auto_trace: true
  auto_trace_llm_payloads: true
  trace_exporter: file
  trace_file: "{trace_file}"

nodes:
  - name: extract_batches
    type: dynamic_parallel
    items: "{{{{ state.batches }}}}"
    item_var: item
    max_concurrency: 4
    fail_fast: false
    fan_in: merge
    action:
      uses: fake_llm.call

  - name: merge
    fan_in: true
    language: python
    run: |
      return {{"results": parallel_results}}

edges:
  - from: __start__
    to: extract_batches
  - from: merge
    to: __end__
"""
    yaml_path = tmpdir / "workflow.yaml"
    yaml_path.write_text(yaml_text, encoding="utf-8")

    # Wrap the fake action so the engine reference is injected through kwargs.
    engine_holder: dict[str, YAMLEngine] = {}

    def _action(state, **kwargs):
        return _fake_llm_call_action(state, _engine=engine_holder.get("engine"), **kwargs)

    engine = YAMLEngine(actions_registry={"fake_llm.call": _action})
    engine_holder["engine"] = engine
    graph = engine.load_from_file(str(yaml_path))
    return engine, graph, trace_file


def test_dynamic_parallel_writes_one_llm_payload_per_branch(tmp_path: Path) -> None:
    engine, graph, trace_file = _build_workflow(tmp_path)
    batches = [1, 2, 3, 4]

    final_state = None
    for event in graph.invoke({"batches": batches}):
        if event.get("type") == "final":
            final_state = event.get("state")
    assert final_state is not None, "workflow did not finish"

    # Sibling *.llm.jsonl file
    payload_file = trace_file.with_suffix(".llm.jsonl")
    assert payload_file.exists(), (
        f"expected payload file at {payload_file}, but it was not created. "
        "This indicates AC1 is regressing — dynamic_parallel branches "
        "are not opening spans that the LlmPayloadFileExporter can pick up."
    )

    lines = [line for line in payload_file.read_text().splitlines() if line and not line.startswith("#")]
    payloads = [json.loads(line) for line in lines]
    assert len(payloads) == len(batches), (
        f"expected one *.llm.jsonl entry per fan-out branch, got {len(payloads)} "
        f"for {len(batches)} batches. Lines: {lines}"
    )

    seen_items = set()
    for record in payloads:
        meta = record.get("metadata", {})
        llm_payload = meta.get(LLM_PAYLOAD_KEY) or {}
        assert llm_payload.get("tokens_input") is not None
        assert llm_payload.get("tokens_output") is not None
        seen_items.add(llm_payload.get("messages_input", [{}])[0].get("content"))

    # Each branch saw its own item.
    assert seen_items == {f"item={i}" for i in batches}


def test_dynamic_parallel_branch_span_isolated_per_thread(tmp_path: Path) -> None:
    """Sanity: each worker thread sees its own current_span (not the parent's).

    Guards against a regression where someone "fixes" thread-locality by
    sharing the main thread's span stack — that would make
    ``current_span()`` race-y across branches and the per-branch
    payloads would collide on a single shared span.
    """
    engine, graph, _ = _build_workflow(tmp_path)
    seen_span_ids: list[str] = []
    seen_threads: list[int] = []

    def _action(state, **kwargs):
        ctx = engine._trace_context
        span = ctx.current_span()
        if span is not None:
            seen_span_ids.append(span["span_id"])
        seen_threads.append(threading.get_ident())
        return _fake_llm_call_action(state, _engine=engine)

    engine.actions_registry["fake_llm.call"] = _action

    for event in graph.invoke({"batches": [1, 2, 3, 4]}):
        if event.get("type") == "final":
            break

    # All four branches must have observed a non-None current_span.
    assert len(seen_span_ids) == 4, (
        f"expected 4 branches to see a span, got {len(seen_span_ids)}"
    )
    # And those spans must be distinct — one per branch.
    assert len(set(seen_span_ids)) == 4, (
        f"expected 4 distinct span_ids across branches, got {len(set(seen_span_ids))} "
        "(branches are sharing a span — payload capture will collide)"
    )
