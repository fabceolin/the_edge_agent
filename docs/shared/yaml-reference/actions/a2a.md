# A2A (Agent-to-Agent) Communication Actions

> **Parent document:** [Actions Index](./README.md)
> **Story:** [TEA-AGENT-001.5 Inter-Agent Communication](../../../stories/TEA-AGENT-001.5-inter-agent-communication.md)

## Overview

A2A actions enable communication between multiple agents running in the same process or namespace. These primitives support message passing, task delegation, shared state coordination, and agent discovery.

**Key Features:**
- Direct messaging and broadcast
- Request/response delegation with timeout
- Shared state with optimistic locking
- Agent discovery by capability
- Namespace isolation

---

## Actions

| Action | Description |
|--------|-------------|
| [`a2a.send`](#a2asend) | Send message to specific agent |
| [`a2a.receive`](#a2areceive) | Receive messages from agents |
| [`a2a.broadcast`](#a2abroadcast) | Broadcast to all agents in namespace |
| [`a2a.delegate`](#a2adelegate) | Request/response task delegation |
| [`a2a.state.get`](#a2astateget) | Get shared state value |
| [`a2a.state.set`](#a2astateset) | Set shared state value |
| [`a2a.discover`](#a2adiscover) | Discover available agents |
| [`a2a.register`](#a2aregister) | Register agent for discovery |
| [`a2a.unregister`](#a2aunregister) | Unregister agent |
| [`a2a.heartbeat`](#a2aheartbeat) | Update agent last_seen timestamp |

---

## Configuration

Configure A2A settings in your YAML agent:

```yaml
settings:
  a2a:
    enabled: true
    agent_id: "coordinator"        # This agent's ID
    namespace: "research-team"     # Namespace for isolation
    discovery: broadcast           # Discovery mode: broadcast, registry, static
```

---

## a2a.send

Send a message to a specific agent.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `to` | string | Yes | - | Recipient agent ID |
| `message` | object | Yes | - | Message with `type` and `payload` |
| `confirm` | bool | No | `false` | Wait for delivery confirmation |
| `correlation_id` | string | No | - | ID for request/response matching |
| `ttl` | int | No | - | Time-to-live in seconds |

### Example

```yaml
- name: send_status
  uses: a2a.send
  with:
    to: coordinator
    message:
      type: status_update
      payload:
        progress: "{{ state.progress }}"
        task_id: "{{ state.task_id }}"
    confirm: true
```

### Return Value

```yaml
{
  "a2a_message_sent": true,
  "a2a_message_id": "msg_abc123"  # Only if confirm=true
}
```

---

## a2a.receive

Receive messages from agents with optional filtering.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `from_agents` | list | No | - | Filter by sender agent IDs |
| `type` | string | No | - | Filter by message type |
| `timeout` | string | No | `"0s"` | Timeout (e.g., "30s", "5m") |
| `require_all` | bool | No | `false` | Wait for messages from ALL agents |

### Example

```yaml
- name: gather_results
  uses: a2a.receive
  with:
    from_agents: [worker-1, worker-2, worker-3]
    type: task_result
    timeout: 60s
    require_all: true
  output: worker_results
```

### Return Value

```yaml
# Success
{
  "a2a_messages": [
    {
      "from_agent": "worker-1",
      "to_agent": "coordinator",
      "namespace": "research-team",
      "type": "task_result",
      "payload": {"result": "..."},
      "timestamp": 1704067200.0,
      "correlation_id": null
    }
  ]
}

# Timeout
{
  "a2a_messages": [],
  "a2a_timeout": true,
  "a2a_error": "Timeout waiting for messages from [worker-3]"
}
```

---

## a2a.broadcast

Broadcast a message to all agents in the namespace.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `message` | object | Yes | - | Message with `type` and `payload` |
| `agent_type_filter` | string | No | - | Filter recipients by agent type |
| `ttl` | int | No | - | Time-to-live in seconds |

### Example

```yaml
- name: announce_completion
  uses: a2a.broadcast
  with:
    message:
      type: announcement
      payload:
        event: task_complete
        result: "{{ state.final_result }}"
    agent_type_filter: worker  # Only send to workers
```

### Return Value

```yaml
{
  "a2a_broadcast_count": 5  # Number of agents that received the message
}
```

---

## a2a.delegate

Delegate a task to another agent and wait for the response (request/response pattern).

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `to` | string | Yes | - | Delegate agent ID |
| `task` | object | Yes | - | Task with `type` and other fields |
| `timeout` | string | No | `"60s"` | Timeout for response |
| `on_timeout` | string | No | `"raise"` | Strategy: raise, fallback_local, retry |
| `fallback` | object | No | - | Fallback action config |

### Example

```yaml
- name: delegate_search
  uses: a2a.delegate
  with:
    to: search-specialist
    task:
      type: web_search
      query: "{{ state.query }}"
      max_results: 10
    timeout: 60s
    on_timeout: fallback_local
    fallback:
      action: web.search
      with:
        query: "{{ state.query }}"
  output: search_results
```

### Return Value

```yaml
# Success
{
  "a2a_delegation_result": {"results": [...]},
  "a2a_delegation_success": true
}

# Timeout with fallback
{
  "a2a_delegation_result": {"results": [...]},  # From fallback action
  "a2a_delegation_success": false,
  "a2a_delegation_fallback": true
}

# Timeout without fallback
{
  "a2a_delegation_result": null,
  "a2a_delegation_success": false,
  "a2a_delegation_timeout": true,
  "a2a_error": "Delegation to search-specialist timed out after 60.0s"
}
```

---

## a2a.state.get

Get a value from shared state (accessible by all agents in namespace).

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `key` | string | Yes | - | State key |
| `default` | any | No | `null` | Default value if key doesn't exist |

### Example

```yaml
- name: get_progress
  uses: a2a.state.get
  with:
    key: team_progress
    default:
      completed: 0
      total: 0
  output: progress
```

### Return Value

```yaml
{
  "a2a_shared_state": {"completed": 5, "total": 10},
  "a2a_state_version": 3  # Version for optimistic locking
}
```

---

## a2a.state.set

Set a value in shared state with optional optimistic locking.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `key` | string | Yes | - | State key |
| `value` | any | Yes | - | Value to store |
| `ttl` | int | No | - | Time-to-live in seconds |
| `expected_version` | int | No | - | Version for optimistic locking |

### Example

```yaml
- name: update_progress
  uses: a2a.state.set
  with:
    key: team_progress
    value:
      completed: "{{ state.completed_tasks }}"
      total: "{{ state.total_tasks }}"
    expected_version: "{{ state.progress.a2a_state_version }}"
    ttl: 3600
  output: update_result
```

### Return Value

```yaml
# Success
{
  "a2a_state_version": 4,
  "a2a_state_updated": true
}

# Conflict (optimistic lock failure)
{
  "a2a_state_updated": false,
  "a2a_state_conflict": true,
  "a2a_error": "Version conflict: expected 3, actual 4",
  "a2a_state_expected_version": 3,
  "a2a_state_actual_version": 4
}
```

---

## a2a.discover

Discover available agents in the namespace.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `capability` | string | No | - | Filter by capability |
| `agent_type` | string | No | - | Filter by agent type |
| `status` | string | No | - | Filter by status (active, idle, etc.) |

### Example

```yaml
- name: find_summarizers
  uses: a2a.discover
  with:
    capability: summarize
    status: active
  output: available_agents
```

### Return Value

```yaml
{
  "a2a_agents": [
    {
      "agent_id": "summarizer-1",
      "namespace": "research-team",
      "capabilities": ["summarize", "translate"],
      "agent_type": "worker",
      "status": "active",
      "last_seen": 1704067200.0,
      "metadata": {"model": "gpt-4"}
    }
  ]
}
```

---

## a2a.register

Register the current agent for discovery and broadcast messages.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `agent_id` | string | No | From config | Agent ID override |
| `capabilities` | list | No | `[]` | List of capabilities |
| `agent_type` | string | No | - | Agent type category |
| `metadata` | object | No | `{}` | Additional metadata |

### Example

```yaml
- name: register_worker
  uses: a2a.register
  with:
    capabilities: [search, summarize, translate]
    agent_type: worker
    metadata:
      model: gpt-4
      version: "1.0"
```

### Return Value

```yaml
{
  "a2a_registered": true,
  "a2a_agent_info": {
    "agent_id": "worker-1",
    "namespace": "research-team",
    "capabilities": ["search", "summarize", "translate"],
    "agent_type": "worker",
    "status": "active",
    "last_seen": 1704067200.0
  },
  "_agent_id": "worker-1",
  "_namespace": "research-team"
}
```

---

## a2a.unregister

Unregister the current agent from discovery and broadcasts.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `agent_id` | string | No | From config | Agent ID override |

### Example

```yaml
- name: cleanup
  uses: a2a.unregister
```

---

## a2a.heartbeat

Send heartbeat to update the agent's last_seen timestamp.

### Example

```yaml
- name: keepalive
  uses: a2a.heartbeat
```

### Return Value

```yaml
{
  "a2a_heartbeat": 1704067200.0,
  "a2a_status": "active"
}
```

---

## Complete Example: Coordinator-Worker Pattern

### Coordinator Agent

```yaml
name: coordinator-agent
description: Coordinates worker agents for parallel research

settings:
  a2a:
    enabled: true
    agent_id: coordinator
    namespace: research-team

nodes:
  - name: register
    uses: a2a.register
    with:
      capabilities: [coordinate, aggregate]
      agent_type: coordinator

  - name: discover_workers
    uses: a2a.discover
    with:
      capability: search
      status: active
    output: workers

  - name: distribute_tasks
    run: |
      workers = state["workers"]["a2a_agents"]
      tasks = state["subtasks"]
      assignments = []
      for i, task in enumerate(tasks):
          worker = workers[i % len(workers)]
          assignments.append({"worker": worker["agent_id"], "task": task})
      return {"assignments": assignments}

  - name: delegate_tasks
    type: dynamic_parallel
    items: "{{ state.assignments }}"
    action: a2a.delegate
    with:
      to: "{{ item.worker }}"
      task:
        type: search
        query: "{{ item.task.query }}"
      timeout: 120s
      on_timeout: fallback_local
      fallback:
        action: web.search
        with:
          query: "{{ item.task.query }}"

  - name: aggregate_results
    uses: a2a.state.set
    with:
      key: research_results
      value: "{{ state.parallel_results }}"

  - name: broadcast_completion
    uses: a2a.broadcast
    with:
      message:
        type: research_complete
        payload:
          task_id: "{{ state.task_id }}"
          result_count: "{{ state.parallel_results | length }}"
```

### Worker Agent

```yaml
name: worker-agent
description: Search worker for research tasks

settings:
  a2a:
    enabled: true
    agent_id: "worker-{{ state.worker_id }}"
    namespace: research-team

nodes:
  - name: register
    uses: a2a.register
    with:
      capabilities: [search]
      agent_type: worker

  - name: wait_for_task
    uses: a2a.receive
    with:
      type: search
      timeout: 300s
    output: task_message

  - name: execute_search
    uses: web.search
    with:
      query: "{{ state.task_message.a2a_messages[0].payload.query }}"
    output: search_results

  - name: send_result
    uses: a2a.send
    with:
      to: coordinator
      message:
        type: search_result
        payload: "{{ state.search_results }}"
      correlation_id: "{{ state.task_message.a2a_messages[0].correlation_id }}"
```

---

## Best Practices

1. **Use namespaces**: Isolate agent groups to prevent message collision
2. **Set appropriate timeouts**: Balance responsiveness with reliability
3. **Implement fallbacks**: Use `on_timeout: fallback_local` for critical paths
4. **Register with capabilities**: Enable capability-based discovery
5. **Use optimistic locking**: Prevent race conditions in shared state
6. **Send heartbeats**: Keep agent discovery accurate with periodic heartbeats

---

## See Also

- [Agent Actions](./agent.md) - Multi-agent coordination primitives
- [Planning Actions](./planning.md) - Task decomposition and parallel execution
- [Reflection Actions](./reflection.md) - Self-correcting patterns
