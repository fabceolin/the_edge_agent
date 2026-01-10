# Multi-Agent Collaboration Actions

Version: 0.8.22 | Feature: TEA-AGENT-001.1

Multi-agent collaboration primitives for orchestrating multiple AI agents within YAML workflows.

## Table of Contents

- [Overview](#overview)
- [Agent Configuration](#agent-configuration)
- [Actions](#actions)
  - [agent.dispatch](#agentdispatch)
  - [agent.parallel](#agentparallel)
  - [agent.sequential](#agentsequential)
  - [agent.coordinate](#agentcoordinate)
  - [agent.crewai_delegate](#agentcrewai_delegate)
- [Aggregation Strategies](#aggregation-strategies)
- [Examples](#examples)

---

## Overview

The multi-agent actions enable sophisticated collaboration patterns without manual graph wiring:

| Action | Description | Use Case |
|--------|-------------|----------|
| `agent.dispatch` | Single agent task dispatch | Delegate specific tasks to specialized agents |
| `agent.parallel` | Parallel dispatch to multiple agents | Consensus, voting, redundancy |
| `agent.sequential` | Chain agents where output feeds next | Multi-step pipelines |
| `agent.coordinate` | Leader-worker coordination | Complex task decomposition |
| `agent.crewai_delegate` | CrewAI integration | Advanced multi-agent workflows |

---

## Agent Configuration

Agents are defined in `settings.agents` and inherit defaults from `settings.llm`:

```yaml
settings:
  llm:
    model: gpt-4
    temperature: 0.7
    timeout: 60

  agents:
    researcher:
      system_prompt: "You are a research assistant..."
      temperature: 0.5
      tools:
        - web.search

    analyst:
      model: gpt-4-turbo
      system_prompt: "You are a data analyst..."
      max_tokens: 2000

    writer:
      model: gpt-3.5-turbo
      system_prompt: "You are a technical writer..."
      retry:
        max_attempts: 3
        backoff: 2.0
```

### Agent Configuration Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `model` | string | (from llm) | LLM model name |
| `system_prompt` | string | "" | System prompt for the agent |
| `temperature` | float | 0.7 | Sampling temperature (0.0-2.0) |
| `max_tokens` | int | null | Maximum response tokens |
| `tools` | list | [] | Tool names from bridges |
| `timeout` | int | 60 | Request timeout in seconds |
| `retry.max_attempts` | int | 3 | Maximum retry attempts |
| `retry.backoff` | float | 2.0 | Exponential backoff multiplier |

---

## Actions

### agent.dispatch

Dispatch a task to a single named agent.

```yaml
- name: research_topic
  action: agent.dispatch
  params:
    agent: researcher
    task: "Research {{ state.topic }} and summarize key findings"
    timeout: 120  # Optional override
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agent` | string | Yes | Agent name from settings.agents |
| `task` | string | Yes | Task description (supports Jinja2) |
| `timeout` | int | No | Override agent timeout |
| `retry` | object | No | Override retry config |

**Returns:**

```json
{
  "response": "Agent's response text",
  "agent": "researcher",
  "usage": {"prompt_tokens": 100, "completion_tokens": 200},
  "success": true
}
```

---

### agent.parallel

Dispatch the same task to multiple agents in parallel with aggregation.

```yaml
- name: classify_sentiment
  action: agent.parallel
  params:
    agents:
      - analyst1
      - analyst2
      - analyst3
    task: "Classify the sentiment of: {{ state.text }}"
    aggregation: vote
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agents` | list | Yes | List of agent names |
| `task` | string | Yes | Task for all agents |
| `aggregation` | string | No | Strategy: collect, vote, first, consensus |
| `max_concurrent` | int | No | Maximum parallel workers |
| `consensus_threshold` | float | No | Agreement threshold (0.0-1.0) |
| `consensus_max_retries` | int | No | Max retries for consensus |

**Returns (varies by aggregation):**

```text
{
  "result": "Yes",
  "votes": {"Yes": 2, "No": 1},
  "unanimous": false,
  "aggregation": "vote",
  "raw_results": [...]
}
```

---

### agent.sequential

Chain multiple agents where each output feeds the next agent's input.

```yaml
- name: content_pipeline
  action: agent.sequential
  params:
    agents:
      - researcher
      - writer
      - editor
    task: "Research {{ state.topic }}"
    transform: "Improve this draft: {{ previous_response }}"
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agents` | list | Yes | Ordered list of agent names |
| `task` | string | Yes | Initial task template |
| `transform` | string | No | Template for subsequent agents |
| `early_exit_on_error` | bool | No | Stop on first failure (default: true) |

**Returns:**

```json
{
  "response": "Final edited content...",
  "chain": [
    {"agent": "researcher", "response": "..."},
    {"agent": "writer", "response": "..."},
    {"agent": "editor", "response": "..."}
  ],
  "success": true
}
```

---

### agent.coordinate

Coordinator pattern with a leader agent dispatching to workers.

```yaml
- name: implement_feature
  action: agent.coordinate
  params:
    leader: architect
    workers:
      - backend_dev
      - frontend_dev
      - qa_engineer
    task: "Implement user authentication feature"
    max_rounds: 3
    validation_prompt: "Verify all components integrate correctly"
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `leader` | string | Yes | Leader agent name |
| `workers` | list | Yes | Worker agent names |
| `task` | string | Yes | Main task description |
| `max_rounds` | int | No | Maximum coordination rounds (default: 3) |
| `validation_prompt` | string | No | Validation prompt for leader |

**Returns:**

```text
{
  "response": "Coordinated implementation complete...",
  "rounds": 2,
  "worker_results": [...],
  "success": true
}
```

---

### agent.crewai_delegate

Delegate to CrewAI for complex multi-agent workflows. Falls back to native TEA implementation when CrewAI is not installed.

```yaml
- name: research_and_write
  action: agent.crewai_delegate
  params:
    agents:
      - role: Researcher
        goal: Find comprehensive information
        backstory: Expert research analyst
      - role: Writer
        goal: Create engaging content
        backstory: Professional technical writer
    task: "Research AI trends and write a report"
    process: sequential
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `agents` | list | Yes | CrewAI agent definitions |
| `task` | string | Yes | Task description |
| `process` | string | No | "sequential" or "hierarchical" |
| `manager_agent` | object | No | Manager for hierarchical process |
| `fallback_to_native` | bool | No | Use TEA if CrewAI unavailable (default: true) |

---

## Aggregation Strategies

Used with `agent.parallel`:

| Strategy | Description | Best For |
|----------|-------------|----------|
| `collect` | Returns all responses as list | Data gathering |
| `vote` | Returns majority response | Classification, decisions |
| `first` | Returns first success | Latency-sensitive tasks |
| `consensus` | Retries until threshold agreement | Critical decisions |

### Consensus Details

Consensus aggregation retries the parallel execution until the specified agreement threshold is reached or max retries is exceeded.

```yaml
aggregation: consensus
consensus_threshold: 0.7  # 70% agreement required
consensus_max_retries: 3  # Try up to 3 times
```

---

## Examples

### Research Pipeline

```yaml
name: research-pipeline
settings:
  llm:
    model: gpt-4
  agents:
    researcher:
      system_prompt: "You gather comprehensive information."
    synthesizer:
      system_prompt: "You create executive summaries."

state_schema:
  topic: str
  research: str
  summary: str

nodes:
  - name: research
    action: agent.dispatch
    params:
      agent: researcher
      task: "Research {{ state.topic }} thoroughly"
    output: research

  - name: summarize
    action: agent.dispatch
    params:
      agent: synthesizer
      task: "Summarize: {{ state.research }}"
    output: summary

edges:
  - from: __start__
    to: research
  - from: research
    to: summarize
  - from: summarize
    to: __end__
```

### Voting Classifier

```yaml
name: sentiment-voting
settings:
  agents:
    analyst1:
      model: gpt-4
      system_prompt: "Classify sentiment as positive, negative, or neutral."
    analyst2:
      model: gpt-4
      system_prompt: "Classify sentiment as positive, negative, or neutral."
    analyst3:
      model: gpt-3.5-turbo
      system_prompt: "Classify sentiment as positive, negative, or neutral."

nodes:
  - name: classify
    action: agent.parallel
    params:
      agents: [analyst1, analyst2, analyst3]
      task: "{{ state.text }}"
      aggregation: vote
    output: classification

edges:
  - from: __start__
    to: classify
  - from: classify
    to: __end__
```

### Coordinated Development

```yaml
name: feature-development
settings:
  agents:
    architect:
      system_prompt: "You design software architecture."
    backend:
      system_prompt: "You implement backend services."
    frontend:
      system_prompt: "You build user interfaces."
    qa:
      system_prompt: "You ensure quality through testing."

nodes:
  - name: implement
    action: agent.coordinate
    params:
      leader: architect
      workers: [backend, frontend, qa]
      task: "{{ state.feature_request }}"
      validation_prompt: "Verify integration and test coverage"
    output: implementation

edges:
  - from: __start__
    to: implement
  - from: implement
    to: __end__
```

---

## Python Implementation

```python
# Register and use directly via Python API
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
result = engine.actions_registry['agent.dispatch'](
    state={'topic': 'quantum computing'},
    agent='researcher',
    task='Research {{ state.topic }}'
)
print(result['response'])
```

---

## Rust Implementation

The Rust implementation provides the same actions with inline agent configuration:

```rust
use the_edge_agent::actions::agent::{AgentConfig, AgentRegistry};

let mut params = HashMap::new();
params.insert("agent".to_string(), json!("researcher"));
params.insert("task".to_string(), json!("Research topic"));
params.insert("_settings".to_string(), json!({
    "agents": {
        "researcher": {"model": "gpt-4"}
    }
}));

let result = agent_dispatch(&state, &params)?;
```

---

## See Also

- [LLM Actions](./llm.md) - Underlying LLM operations
- [Memory Actions](./memory.md) - State persistence
- [Specialized Actions](./specialized.md) - Extraction and validation
