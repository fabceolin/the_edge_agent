# Epic TEA-BUILTIN-005: Comet Opik Integration - Brownfield Enhancement

## Status

In Progress (Story 4 added)

## Epic Goal

Integrate Comet Opik observability platform with The Edge Agent's built-in LLM actions, enabling production-grade tracing, cost monitoring, evaluation metrics, and debugging capabilities without requiring users to write custom instrumentation code.

## Epic Description

### Existing System Context

**Current Relevant Functionality:**
- `TraceContext` class in `src/the_edge_agent/tracing.py` manages hierarchical spans
- Existing exporters: `ConsoleExporter`, `FileExporter`, `CallbackExporter`
- `llm.call`, `llm.stream`, `llm.tools` actions in `src/the_edge_agent/actions/llm_actions.py`
- YAMLEngine supports `trace_exporter` configuration parameter
- Auto-instrumentation via `auto_trace: true` in YAML settings

**Technology Stack:**
- Python >=3.7
- OpenAI SDK for LLM calls (supports OpenAI and Azure OpenAI)
- Threading for parallel execution
- unittest framework for testing

**Integration Points:**
- `YAMLEngine.__init__()` - Add `trace_exporter="opik"` option
- `TraceContext` - Add `OpikExporter` as new exporter backend
- `llm_call()` function - Optional native Opik `track_openai` wrapper
- New environment variables: `OPIK_API_KEY`, `OPIK_WORKSPACE`, `OPIK_PROJECT_NAME`

### Enhancement Details

**What's Being Added:**

1. **OpikExporter Backend**: New exporter that sends spans to Comet Opik platform, compatible with existing `TraceContext` infrastructure

2. **Native Opik LLM Instrumentation**: Optional wrapper around OpenAI client using Opik's `track_openai` for richer LLM-specific telemetry (token usage, cost calculation, model parameters)

3. **Opik Configuration Layer**: Environment variable support, project/workspace management, and optional CLI utilities for health checks

**How It Integrates:**
- Pluggable exporter model - OpikExporter implements existing `TraceExporter` protocol
- Feature flag pattern - `enable_opik=True` parameter for opt-in instrumentation
- Graceful degradation - If `opik` SDK not installed, features are disabled with clear error messages
- Dual-mode operation - Can use internal TraceContext spans OR native Opik spans

**Success Criteria:**
- LLM calls are automatically traced to Opik dashboard with latency, tokens, and cost
- Existing `TraceContext` spans can be exported to Opik
- Zero code changes required for existing YAML agents (config-only enablement)
- No performance impact when Opik is disabled
- Clear error messages when `opik` SDK is missing

## Architectural Decision: Integration Approach

Based on the Opik SDK analysis, there are two integration strategies:

### Option A: TraceExporter Adapter (Recommended for Phase 1)
- Create `OpikExporter` that converts TEA spans to Opik spans
- Preserves existing `TraceContext` infrastructure
- Lower risk, backward compatible
- Works with all existing tracing features

### Option B: Native Opik Instrumentation (Phase 2 Enhancement)
- Use `track_openai()` wrapper directly on OpenAI client
- Richer LLM-specific telemetry (model fingerprint, prompt tokens, etc.)
- Requires changes to `llm_call()` implementation
- Can coexist with Option A

**Decision**: Implement both in sequence. Story 1 delivers Option A (immediate value), Story 2 adds Option B (enhanced telemetry).

## Stories

### Story 1: OpikExporter Backend (TEA-BUILTIN-005.1)

**Title:** Add OpikExporter for TraceContext Integration

**Description:** Create a new `OpikExporter` class that implements the `TraceExporter` protocol and sends completed spans to Comet Opik. This enables existing YAML agents to export traces to Opik without code changes.

**Scope:**
- New file: `src/the_edge_agent/exporters/opik_exporter.py`
- OpikExporter class implementing TraceExporter protocol
- Span format conversion (TEA format -> Opik format)
- Configuration via `YAMLEngine(trace_exporter="opik")`
- Environment variable support for authentication
- Graceful degradation when `opik` SDK not installed

**Acceptance Criteria:**
1. OpikExporter implements TraceExporter protocol
2. Spans are exported to Opik with name, duration, status, metadata
3. `YAMLEngine(trace_exporter="opik")` enables Opik export
4. Clear error message if `opik` package not installed
5. Environment variables respected: `OPIK_API_KEY`, `OPIK_PROJECT_NAME`, `OPIK_WORKSPACE`
6. Hierarchical spans preserve parent-child relationships in Opik
7. Unit tests cover exporter functionality
8. Documentation in CLAUDE.md

**Estimated Complexity:** Medium (1-2 days)

---

### Story 2: Native Opik LLM Instrumentation (TEA-BUILTIN-005.2)

**Title:** Enhanced LLM Tracing with Native Opik track_openai

**Description:** Add optional native Opik instrumentation to `llm.call` action using `track_openai()` wrapper. This provides richer LLM-specific telemetry including token counts, cost estimation, and model parameters directly captured by Opik.

**Scope:**
- Modify `src/the_edge_agent/actions/llm_actions.py`
- Optional `track_openai()` wrapper around OpenAI client
- Configuration via `YAMLEngine(opik_llm_tracing=True)` or YAML settings
- Cost tracking integration with Opik dashboard
- Streaming support with chunk aggregation
- Azure OpenAI compatibility

**Acceptance Criteria:**
1. `llm.call` with `opik_llm_tracing=True` uses `track_openai()` wrapper
2. Token usage (prompt, completion, total) captured in Opik
3. Estimated cost calculated based on model pricing
4. Streaming responses aggregated correctly for complete trace
5. Azure OpenAI endpoint auto-detected and traced
6. Feature disabled by default (opt-in)
7. Works alongside OpikExporter (complementary, not exclusive)
8. Integration tests verify end-to-end tracing

**Estimated Complexity:** Medium (1-2 days)

**Dependencies:** Story 1 (OpikExporter provides foundation)

---

### Story 3: Opik Configuration and Utilities (TEA-BUILTIN-005.3)

**Title:** Opik Configuration Layer and CLI Utilities

**Description:** Add comprehensive configuration support for Opik integration including YAML settings, environment variable hierarchy, project management, and optional CLI healthcheck command.

**Scope:**
- YAML settings section for Opik configuration
- Configuration hierarchy: constructor params > env vars > defaults
- Project and workspace auto-creation
- Optional healthcheck action (`opik.healthcheck`)
- Documentation and examples

**Acceptance Criteria:**
1. YAML `settings.opik` section for declarative configuration
2. Configuration precedence: code params > env vars > YAML settings > defaults
3. `opik.healthcheck` action validates connectivity and authentication
4. Project auto-creation if `OPIK_PROJECT_NAME` doesn't exist
5. Clear configuration documentation with examples
6. Error messages guide users to correct configuration issues

**Estimated Complexity:** Low-Medium (0.5-1 day)

**Dependencies:** Story 1, Story 2

---

### Story 4: Opik Experiment Framework (TEA-BUILTIN-005.4)

**Title:** Generic Experiment Framework for Agent Evaluation

**Description:** Add a reusable experiment infrastructure that wraps Opik's `evaluate()` API, enabling systematic agent evaluation, A/B comparisons, and quality metrics. Consumer projects extend this framework with domain-specific metrics.

**Scope:**
- New module: `src/the_edge_agent/experiments/`
- `run_tea_experiment()` function for YAMLEngine evaluation
- Dataset creation utilities for Opik datasets
- `BaseTeaMetric` base class with scoring helpers
- A/B comparison utilities for strategy testing
- CLI framework (`tea-experiments` command)

**Acceptance Criteria:**
1. `run_tea_experiment(agent_yaml, dataset, metrics)` runs Opik experiment
2. `create_dataset_from_fixtures()` creates Opik datasets from JSON fixtures
3. `BaseTeaMetric` provides extensible base for custom metrics
4. `compare_strategies()` runs A/B comparison on same dataset
5. CLI supports `--agent`, `--dataset`, `--version`, `--dry-run` flags
6. Graceful degradation when `opik` SDK not installed
7. Documentation in `docs/python/experiments-guide.md`

**Estimated Complexity:** Medium (3-5 days)

**Dependencies:** Story 1, Story 2, Story 3

---

### Story 5: Opik Agent Graph Visualization (TEA-BUILTIN-005.5)

**Title:** Agent Graph Visualization in Opik Dashboard

**Description:** Add Mermaid graph export to StateGraph and integrate with experiment runner so that TEA agent graph structure appears in Opik's "Show Agent Graph" UI during experiment traces.

**Scope:**
- Add `to_mermaid()` method to `VisualizationMixin`
- Add `get_mermaid_graph()` to YAMLEngine
- Integrate with experiment runner via `_opik_graph_definition` metadata
- Handle conditional edges, parallel flows, and special nodes

**Acceptance Criteria:**
1. StateGraph provides `to_mermaid()` returning valid Mermaid syntax
2. Conditional and parallel edges rendered with appropriate labels
3. `run_tea_experiment()` includes graph automatically (`include_graph=True` by default)
4. Graph uses Opik's `_opik_graph_definition` metadata format
5. Graceful degradation if graph generation fails
6. YAMLEngine exposes `get_mermaid_graph()` for declarative agents
7. No new dependencies - Mermaid is string output only
8. Unit tests verify Mermaid output validity

**Estimated Complexity:** Low-Medium (1-2 days)

**Dependencies:** Story 4 (experiment framework)

---

## Compatibility Requirements

- [x] Existing `TraceContext` API remains unchanged
- [x] Existing exporters (Console, File, Callback) continue to work
- [x] `llm.call` without Opik configuration works identically to today
- [x] **CRITICAL: `opik` SDK is OPTIONAL** - All features gracefully degrade when not installed
- [x] Performance impact negligible when Opik disabled
- [x] No breaking changes to existing API signatures

## Optional Dependency Model

The `opik` package is an **optional extra dependency**. Users install it explicitly:

```bash
# Install The Edge Agent with Opik support
pip install the-edge-agent[opik]

# Or install opik separately
pip install opik
```

**Behavior when `opik` not installed:**
- `trace_exporter="opik"` raises clear `ImportError` with install instructions
- `opik_trace=True` in llm.call logs warning and continues without tracing
- `opik.healthcheck` action returns `{"success": False, "error": "SDK not installed"}`
- All other functionality works unchanged

## Risk Mitigation

**Primary Risk:** Opik SDK dependency introduces external service coupling

**Mitigation:**
- Opik SDK is optional - import wrapped in try/except
- Graceful degradation with clear error messages
- Feature flags allow disabling without code changes
- No breaking changes to existing functionality

**Rollback Plan:**
1. Set `trace_exporter="console"` or `enable_opik=False`
2. Opik-specific code isolated in separate module
3. Remove `opik` from optional dependencies if needed
4. Existing functionality unaffected

## Definition of Done

- [x] Stories 1-3 completed with acceptance criteria met
- [x] Story 4 (Experiment Framework) completed with acceptance criteria met
- [ ] Story 5 (Agent Graph Visualization) completed with acceptance criteria met
- [ ] Existing tracing functionality verified through regression tests
- [ ] Integration with Opik dashboard demonstrated
- [ ] Documentation updated (CLAUDE.md, YAML_REFERENCE.md)
- [ ] No regressions in existing features (all 258+ tests pass)
- [ ] Optional dependency handling tested (with and without `opik` installed)

## Technical Notes

### Opik SDK Key APIs

```python
# Configuration
import opik
opik.configure(api_key="...", workspace="...", project_name="...")

# OpenAI wrapper
from opik.integrations.openai import track_openai
client = track_openai(OpenAI())

# Manual tracing
from opik import track
@track(name="operation", tags=["tag1"])
def my_function(): ...

# Context manager
with opik.start_as_current_span("operation") as span:
    span.set_attribute("key", "value")
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `OPIK_API_KEY` | Authentication key for Opik Cloud | Required for Cloud |
| `OPIK_WORKSPACE` | Workspace name | User's default workspace |
| `OPIK_PROJECT_NAME` | Project for trace grouping | "default" |
| `OPIK_URL_OVERRIDE` | Self-hosted Opik URL | https://www.comet.com/opik/api |

### Span Format Mapping

| TEA TraceContext | Opik Span |
|------------------|-----------|
| span_id | span_id |
| parent_id | parent_span_id |
| name | name |
| start_time | start_time |
| end_time | end_time |
| duration_ms | (calculated) |
| status | status ("ok" -> "completed") |
| error | error_message |
| metadata | metadata |
| metrics | metrics (usage.total_tokens, etc.) |

## Story Manager Handoff

Please develop detailed user stories for this brownfield epic. Key considerations:

- This is an enhancement to an existing system running Python 3.7+ with OpenAI SDK integration
- Integration points: `TraceContext`, `YAMLEngine`, `llm_actions.py`
- Existing patterns to follow: `ConsoleExporter`, `FileExporter` in `tracing.py`
- Critical compatibility requirements: Existing exporters and `llm.call` must work unchanged
- Each story must include verification that existing functionality remains intact

The epic should maintain system integrity while delivering comprehensive Opik observability integration.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-18 | 0.1 | Initial epic draft | Sarah (PO Agent) |
| 2025-12-18 | 1.0 | All 3 substories (005.1, 005.2, 005.3) completed and marked Done | James (Dev Agent) |
| 2026-01-07 | 1.1 | Status updated from "Draft" to "Complete" - epic implementation verified | Sarah (PO) |
| 2026-01-12 | 1.2 | Added Story 4 (TEA-BUILTIN-005.4) - Experiment Framework for agent evaluation. Status back to "In Progress" | Sarah (PO) |
| 2026-01-12 | 1.3 | Added Story 5 (TEA-BUILTIN-005.5) - Opik Agent Graph Visualization for Mermaid export and dashboard integration | Sarah (PO) |
