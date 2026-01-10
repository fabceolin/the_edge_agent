# Planning Actions

Version: 0.8.22

This document covers planning and decomposition primitives for agentic workflows (TEA-AGENT-001.3).

## Overview

Planning actions implement the planning pattern from Agentic Design Patterns, enabling:

- **Goal Decomposition**: Break complex goals into manageable subtasks
- **Dependency Management**: Express and validate subtask dependencies as DAGs
- **Execution Control**: Sequential or parallel execution respecting dependencies
- **Failure Handling**: Strategies for dealing with subtask failures
- **Re-planning**: Adjust plans based on execution results

## Actions Summary

| Action | Description | Story |
|--------|-------------|-------|
| `plan.decompose` | Decompose goal into subtasks | TEA-AGENT-001.3 |
| `plan.execute` | Execute plan respecting dependencies | TEA-AGENT-001.3 |
| `plan.replan` | Re-plan from current state | TEA-AGENT-001.3 |
| `plan.status` | Get plan execution status | TEA-AGENT-001.3 |

---

## plan.decompose

Decompose a goal into subtasks using LLM. Returns a structured plan with subtask dependencies forming a DAG.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `goal` | string | Yes | - | The goal to decompose |
| `model` | string | No | `gpt-4` | LLM model for planning |
| `strategy` | string | No | `flat` | Planning strategy: `flat`, `hierarchical`, `iterative` |
| `max_depth` | integer | No | `3` | Max depth for hierarchical plans |
| `max_subtasks` | integer | No | `15` | Maximum number of subtasks |
| `prompt_template` | string | No | - | Custom prompt for decomposition |
| `temperature` | float | No | `0.7` | LLM temperature |

### Planning Strategies

| Strategy | Description | Best For |
|----------|-------------|----------|
| `flat` | Simple sequential list of subtasks | Linear tasks |
| `hierarchical` | Tree of subtasks with sub-subtasks | Complex goals |
| `iterative` | Plan one step at a time based on current state | Exploratory tasks |

### Returns

```text
{
  "plan": {
    "id": "plan_abc123",
    "goal": "Research and summarize topic X",
    "strategy": "hierarchical",
    "subtasks": [
      {
        "id": "search",
        "description": "Search for relevant sources",
        "dependencies": [],
        "status": "pending"
      },
      {
        "id": "analyze",
        "description": "Analyze found sources",
        "dependencies": ["search"],
        "status": "pending"
      }
    ],
    "metadata": {
      "created_at": 1704393600.0,
      "replan_count": 0,
      "max_replans": 3
    }
  },
  "planning_trace": [...],
  "success": true,
  "plan_id": "plan_abc123",
  "subtask_count": 2
}
```

### Example

```yaml
nodes:
  - name: create_plan
    action: plan.decompose
    with:
      goal: "{{ state.user_goal }}"
      strategy: hierarchical
      max_depth: 3
      model: gpt-4
      prompt_template: |
        Break down this goal into actionable subtasks.
        Each subtask should be specific and measurable.
        Goal: {{ goal }}
```

---

## plan.execute

Execute plan subtasks respecting dependency order. Supports sequential and parallel execution.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `plan` | object | No | `state.plan` | Plan to execute |
| `parallel` | boolean | No | `false` | Execute independent subtasks in parallel |
| `max_concurrent` | integer | No | `3` | Max concurrent subtasks when parallel |
| `subtask_executor` | object | No | `llm.call` | Action config for executing subtasks |
| `on_subtask_failure` | string | No | `abort` | Failure strategy: `replan`, `retry`, `skip`, `abort` |
| `max_retries` | integer | No | `2` | Max retries per subtask (for retry strategy) |
| `retry_delay` | float | No | `1.0` | Delay between retries in seconds |
| `max_replans` | integer | No | `2` | Max replan attempts (for replan strategy) |

### Failure Strategies

| Strategy | Behavior |
|----------|----------|
| `abort` | Stop execution immediately on failure |
| `skip` | Mark failed subtask as skipped, continue |
| `retry` | Retry with exponential backoff |
| `replan` | Re-plan from current state preserving completed work |

### Subtask Executor

The `subtask_executor` parameter configures how each subtask is executed:

```yaml
subtask_executor:
  action: llm.call
  with:
    model: gpt-4
    temperature: 0.7
```

Or use agent dispatch:

```yaml
subtask_executor:
  action: agent.dispatch
  with:
    agent: worker
```

### Returns

```text
{
  "plan": {...},
  "subtask_results": {
    "search": "Found 5 relevant sources...",
    "analyze": "Key findings: ..."
  },
  "plan_status": {
    "total": 4,
    "completed": 2,
    "pending": 2,
    "failed": 0,
    "skipped": 0
  },
  "replan_count": 0,
  "planning_trace": [...],
  "success": true
}
```

### Example

```yaml
nodes:
  - name: execute_plan
    action: plan.execute
    with:
      plan: "{{ state.plan }}"
      parallel: true
      max_concurrent: 3
      on_subtask_failure: retry
      max_retries: 2
      subtask_executor:
        action: llm.call
        with:
          model: gpt-4
```

---

## plan.replan

Re-plan from current state, preserving completed subtasks. Used when a subtask fails or when the plan needs adjustment based on new information.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `plan` | object | No | `state.plan` | Current plan |
| `model` | string | No | `gpt-4` | LLM model for re-planning |
| `temperature` | float | No | `0.7` | LLM temperature |

### State Variables Used

- `state.plan`: Current plan
- `state.subtask_results`: Results from completed subtasks
- `state.failed_subtask`: ID of the failed subtask
- `state.failure_reason`: Reason for failure

### Returns

```text
{
  "plan": {...},
  "preserved_subtasks": 2,
  "new_subtasks": 3,
  "planning_trace": [...],
  "success": true
}
```

### Example

```yaml
nodes:
  - name: handle_failure
    action: plan.replan
    with:
      model: gpt-4
      temperature: 0.5
```

---

## plan.status

Get current plan execution status with aggregated counts and optional subtask details.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `plan` | object | No | `state.plan` | Plan to get status for |
| `include_completed` | boolean | No | `true` | Include completed subtasks in response |
| `include_details` | boolean | No | `false` | Include full subtask details |

### Returns

```text
{
  "status": {
    "total": 5,
    "pending": 2,
    "in_progress": 1,
    "completed": 2,
    "failed": 0,
    "skipped": 0
  },
  "progress": 0.4,
  "plan_id": "plan_abc123",
  "goal": "Research and summarize topic X",
  "subtasks": [...],  // if include_details=true
  "success": true
}
```

### Example

```yaml
nodes:
  - name: check_progress
    action: plan.status
    with:
      include_details: true
      include_completed: false
```

---

## Plan Structure

Plans follow this structure:

```json
{
  "id": "plan_abc123",
  "goal": "Research and summarize topic X",
  "strategy": "hierarchical",
  "subtasks": [
    {
      "id": "subtask_1",
      "description": "Search for relevant sources",
      "dependencies": [],
      "status": "completed",
      "result": "Found 5 sources..."
    },
    {
      "id": "subtask_2",
      "description": "Analyze sources",
      "dependencies": ["subtask_1"],
      "status": "in_progress",
      "result": null
    }
  ],
  "metadata": {
    "created_at": 1704393600.0,
    "replan_count": 0,
    "max_replans": 3
  }
}
```

### Subtask Status Values

| Status | Description |
|--------|-------------|
| `pending` | Not yet started |
| `in_progress` | Currently executing |
| `completed` | Successfully completed |
| `failed` | Execution failed |
| `skipped` | Skipped due to failure strategy |

---

## State Variables

Planning actions set these state variables:

| Variable | Type | Description |
|----------|------|-------------|
| `plan` | dict | Current plan structure |
| `plan_status` | dict | Aggregated status counts |
| `current_subtask` | dict | Currently executing subtask |
| `subtask_results` | dict | Map of subtask_id → result |
| `replan_count` | int | Number of re-plans triggered |

---

## Complete Workflow Example

```yaml
name: research-planner
state_schema:
  user_goal: str
  plan: dict
  subtask_results: dict
  final_result: str

nodes:
  - name: plan_research
    action: plan.decompose
    with:
      goal: "{{ state.user_goal }}"
      strategy: hierarchical
      max_depth: 2
      model: gpt-4

  - name: execute_research
    action: plan.execute
    with:
      plan: "{{ state.plan }}"
      parallel: true
      max_concurrent: 3
      on_subtask_failure: replan
      max_replans: 2
      subtask_executor:
        action: llm.call
        with:
          model: gpt-4

  - name: check_completion
    action: plan.status
    with:
      include_details: false

  - name: synthesize
    action: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: Synthesize the research results into a comprehensive summary.
        - role: user
          content: |
            Goal: {{ state.user_goal }}
            Results: {{ state.subtask_results | tojson }}

navigation:
  plan_research: execute_research
  execute_research: check_completion
  check_completion: synthesize
  synthesize: __end__
```

---

## Error Handling

Planning actions return structured errors:

```text
{
  "error": "Invalid plan structure: Cycle detected: a -> b -> c -> a",
  "success": false,
  "planning_trace": [...]
}
```

Common error conditions:

| Error | Cause | Solution |
|-------|-------|----------|
| "No plan provided" | Missing plan in state | Call `plan.decompose` first |
| "Cycle detected" | Subtask dependencies form a cycle | Fix dependency structure |
| "Unknown subtask" | Dependency references non-existent subtask | Verify subtask IDs |
| "Max replans exceeded" | Too many replan attempts | Increase `max_replans` or change strategy |

---

## Best Practices

1. **Start with flat strategy** for simple tasks, upgrade to hierarchical for complex goals
2. **Use iterative strategy** when the full scope isn't known upfront
3. **Set appropriate max_subtasks** to prevent overly complex plans
4. **Use parallel execution** for independent subtasks to speed up execution
5. **Prefer retry or skip** over abort for robust workflows
6. **Monitor progress** with `plan.status` for long-running plans
7. **Store plans in checkpoints** for resumable execution

## Related Actions

- [Agent Actions](./agent.md) - Multi-agent collaboration
- [Reasoning Actions](./reasoning.md) - Chain-of-thought, ReAct patterns
- [LLM Actions](./llm.md) - Direct LLM calls
