# A2A (Agent-to-Agent) Communication Examples

This directory contains examples demonstrating inter-agent communication patterns using TEA's A2A actions.

## Examples

### 1. Coordinator-Worker Pattern (`coordinator-worker.yaml`)

Demonstrates a coordinator distributing tasks to worker agents and collecting results:
- Coordinator registers and discovers available workers
- Tasks are assigned round-robin to workers
- Results are collected and aggregated
- Completion is broadcast to all workers

**Use cases:** Parallel research, distributed processing, load balancing

### 2. Delegation with Fallback (`delegation-fallback.yaml`)

Demonstrates resilient task delegation with timeout handling:
- Attempts to delegate to a specialist agent
- Falls back to local execution on timeout
- Results are cached in shared state

**Use cases:** Resilient systems, graceful degradation, hybrid processing

### 3. Broadcast Consensus (`broadcast-consensus.yaml`)

Demonstrates a voting/consensus pattern:
- Proposer broadcasts a proposal to all voters
- Voters respond with their votes
- Votes are tallied and outcome determined
- Results are broadcast back to voters

**Use cases:** Decision making, voting systems, distributed consensus

## Running the Examples

```bash
# Run an example
tea run examples/a2a/coordinator-worker.yaml

# Run with custom input
tea run examples/a2a/delegation-fallback.yaml --input '{"query": "your query"}'
```

## Key Concepts

### Namespace Isolation

Each example uses a `namespace` in `settings.a2a` to isolate agent communication:
```yaml
settings:
  a2a:
    namespace: research-team
```

### Agent Registration

Agents register themselves for discovery and broadcast:
```yaml
- name: register
  action: a2a.register
  with:
    capabilities: [search, summarize]
    agent_type: worker
```

### Message Types

Messages include a `type` for filtering:
- `status_update`: Progress reports
- `task_assignment`: Task delegation
- `task_result`: Completed work
- `proposal`: Consensus proposals
- `vote`: Voting responses

### Timeout Handling

Receive and delegate actions support timeouts:
```yaml
- name: wait
  action: a2a.receive
  with:
    timeout: 30s
    require_all: true
```

### Shared State

Agents can coordinate via shared state with optimistic locking:
```yaml
- name: update_progress
  action: a2a.state.set
  with:
    key: team_progress
    value: "{{ state.progress }}"
    expected_version: "{{ state.a2a_state_version }}"
```

## See Also

- [A2A Actions Reference](../../docs/shared/yaml-reference/actions/a2a.md)
- [Story: TEA-AGENT-001.5](../../docs/stories/TEA-AGENT-001.5-inter-agent-communication.md)
