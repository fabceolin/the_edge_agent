# Multi-Agent Collaboration Examples

This directory contains examples demonstrating the multi-agent collaboration primitives implemented in TEA-AGENT-001.1.

## Overview

Multi-agent collaboration allows you to:
- Define specialized agents with unique capabilities
- Execute agents in parallel, sequentially, or with coordination
- Aggregate results using different strategies (collect, vote, consensus)
- Bridge to external tool ecosystems (MCP, CrewAI, LangChain)

## Examples

### 1. Research Team (`research-team.yaml`)

A sequential workflow with three specialized agents:
- **Researcher**: Gathers comprehensive information
- **Analyst**: Analyzes data and identifies patterns
- **Writer**: Synthesizes findings into a report

```bash
tea run examples/multi-agent/research-team.yaml \
  --input '{"topic": "quantum computing trends 2024"}'
```

### 2. Parallel Reviewers (`parallel-reviewers.yaml`)

Multiple agents review code in parallel:
- **Security Reviewer**: Checks for vulnerabilities
- **Performance Reviewer**: Analyzes efficiency
- **Style Reviewer**: Evaluates code quality

Results are collected and synthesized by a lead reviewer.

```bash
tea run examples/multi-agent/parallel-reviewers.yaml \
  --input '{"code": "def calculate_risk(portfolio): return sum(p.risk for p in portfolio)"}'
```

### 3. Coordinator Pattern (`coordinator-pattern.yaml`)

A coordinator agent orchestrates specialized workers:
- **Coordinator**: Plans work and synthesizes results
- **Designer**: Creates UI/UX specifications
- **Copywriter**: Writes marketing content
- **Developer**: Implements the solution

```bash
tea run examples/multi-agent/coordinator-pattern.yaml \
  --input '{"request": "Create a pricing page for a B2B SaaS product"}'
```

### 4. Sequential Pipeline (`sequential-pipeline.yaml`)

A data processing pipeline where each agent builds on the previous:
- **Extractor**: Extracts structured data from raw input
- **Enricher**: Adds context and connections
- **Summarizer**: Creates an executive summary

```bash
tea run examples/multi-agent/sequential-pipeline.yaml \
  --input '{"raw_data": "Customer feedback from Q3 survey..."}'
```

## Agent Configuration

Agents are defined in `settings.agents` with inheritance from `settings.llm`:

```yaml
settings:
  llm:
    model: gpt-4o-mini      # Default model
    temperature: 0.7        # Default temperature

  agents:
    my_agent:
      # Inherits model from settings.llm
      system_prompt: |
        You are a specialized agent...
      temperature: 0.3      # Override default
      max_tokens: 2000
      tools:                # Optional tool list
        - web.search
        - llm.call
```

## Actions Reference

### `agent.dispatch`
Execute a single agent with a task.

```yaml
action: agent.dispatch
params:
  agent: my_agent
  task: "Process this: {{ state.data }}"
output: result
```

### `agent.parallel`
Execute multiple agents concurrently.

```yaml
action: agent.parallel
params:
  agents:
    - agent_a
    - agent_b
    - agent_c
  task: "Analyze: {{ state.input }}"
  aggregation: collect  # collect, vote, consensus, first
output: results
```

### `agent.sequential`
Execute agents in sequence, threading state.

```yaml
action: agent.sequential
params:
  agents:
    - extractor
    - transformer
    - loader
  initial_task: "Process: {{ state.data }}"
  thread_state: true
output:
  step_1: stage_0
  step_2: stage_1
  step_3: stage_2
```

### `agent.coordinate`
Use a coordinator to orchestrate workers.

```yaml
action: agent.coordinate
params:
  coordinator: lead_agent
  workers:
    - worker_a
    - worker_b
  request: "{{ state.task }}"
  coordinator_instructions: "Plan and delegate..."
  synthesis_instructions: "Combine results..."
output:
  worker_outputs: results
  final: synthesis
```

## Aggregation Strategies

- **collect**: Gather all results into a list
- **vote**: Find the most common result
- **consensus**: LLM-based synthesis of results
- **first**: Return the first successful result

## Best Practices

1. **Clear System Prompts**: Give each agent a focused role with specific instructions
2. **Temperature Tuning**: Lower temperature for analytical tasks, higher for creative
3. **State Threading**: Use `thread_state: true` when agents need previous context
4. **Aggregation Choice**: Match aggregation strategy to your use case
5. **Error Handling**: Agents have built-in retry with configurable attempts
