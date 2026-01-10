# Complete Examples

> **Parent document:** [YAML Reference](../YAML_REFERENCE.md)
> **Related:** [Node Specification](./nodes.md) | [Navigation](./navigation.md) | [Actions](./actions/README.md)

## Overview

This document provides complete, runnable example agents demonstrating various YAML agent patterns.

---

## Table of Contents

- [Example 1: Research Agent with Conditional Routing](#example-1-research-agent-with-conditional-routing)
- [Example 2: Parallel Processing Pipeline](#example-2-parallel-processing-pipeline)
- [Example 3: Customer Support Agent](#example-3-customer-support-agent)

---

## Example 1: Research Agent with Conditional Routing

This example demonstrates:
- Conditional edge routing based on state
- File output action
- Variable substitution

```yaml
name: research-agent
description: Search, validate, and summarize research results

variables:
  min_results: 3
  output_dir: ./reports

state_schema:
  query: str
  results: list
  has_enough: bool
  summary: str

nodes:
  - name: search
    run: |
      query = state["query"]
      results = [
        {"title": f"Result {i}", "snippet": f"Info about {query}"}
        for i in range(5)
      ]
      return {"results": results}

  - name: validate
    run:
      type: expression
      value: len(state.get("results", [])) >= {{ variables.min_results }}
      output_key: has_enough

  - name: summarize
    run: |
      results = state["results"]
      summary = f"Found {len(results)} results for '{state['query']}':\n"
      for r in results:
        summary += f"- {r['title']}: {r['snippet']}\n"
      return {"summary": summary}

  - name: save_report
    uses: file.write
    with:
      path: "{{ variables.output_dir }}/{{ state.query }}.md"
      content: "# Research Report\n\n{{ state.summary }}"

  - name: insufficient_results
    run: |
      return {"summary": f"Insufficient results for query: {state['query']}"}

edges:
  - from: __start__
    to: search
  - from: search
    to: validate
  - from: validate
    to: summarize
    when: "state['has_enough']"
  - from: validate
    to: insufficient_results
    when: "!state['has_enough']"
  - from: summarize
    to: save_report
  - from: save_report
    to: __end__
  - from: insufficient_results
    to: __end__

config:
  raise_exceptions: true
```

---

## Example 2: Parallel Processing Pipeline

This example demonstrates:
- Parallel fan-out execution
- Fan-in result collection
- Multiple analysis branches

```yaml
name: parallel-analyzer
description: Analyze data from multiple sources in parallel

state_schema:
  input: str
  combined: dict

nodes:
  - name: prepare
    run: |
      return {"prepared_input": state["input"].strip()}

  - name: analyze_sentiment
    run: |
      return {"analysis_a": {"type": "sentiment", "score": 0.75}}

  - name: analyze_entities
    run: |
      return {"analysis_b": {"type": "entities", "entities": ["AI", "Python"]}}

  - name: analyze_topics
    run: |
      return {"analysis_c": {"type": "topics", "topics": ["technology"]}}

  - name: combine_results
    fan_in: true
    run: |
      combined = {}
      for result in parallel_results:
        for key in ["analysis_a", "analysis_b", "analysis_c"]:
          if key in result:
            combined[key] = result[key]
      return {"combined": combined}

  - name: generate_report
    run: |
      combined = state["combined"]
      return {"final_report": {"analyses": combined, "count": len(combined)}}

edges:
  - from: __start__
    to: prepare
  - from: prepare
    to: analyze_sentiment
    type: parallel
    fan_in: combine_results
  - from: prepare
    to: analyze_entities
    type: parallel
    fan_in: combine_results
  - from: prepare
    to: analyze_topics
    type: parallel
    fan_in: combine_results
  - from: combine_results
    to: generate_report
  - from: generate_report
    to: __end__
```

---

## Example 3: Customer Support Agent

This example demonstrates:
- Intent classification
- Multi-step nodes
- Conditional routing to different handlers

```yaml
name: customer-support-agent
description: Classify and route customer inquiries

variables:
  support_email: support@example.com

state_schema:
  customer_id: str
  message: str
  intent: str
  response: str
  ticket_id: str

nodes:
  - name: classify_intent
    run: |
      message = state["message"].lower()
      if "bill" in message or "payment" in message:
        intent = "billing"
      elif "cancel" in message or "refund" in message:
        intent = "cancellation"
      elif "bug" in message or "error" in message:
        intent = "technical"
      else:
        intent = "general"
      return {"intent": intent}

  - name: handle_billing
    steps:
      - name: lookup_account
        run: |
          return {"account_status": "active", "last_payment": "2025-01-01"}
      - name: generate_response
        run: |
          return {
            "response": f"Your account is {state['account_status']}.",
            "ticket_id": f"BILL-{state['customer_id']}"
          }

  - name: handle_cancellation
    run: |
      return {
        "response": "Let me connect you with our retention team.",
        "ticket_id": f"CANCEL-{state['customer_id']}"
      }

  - name: handle_technical
    run: |
      return {
        "response": "I've created a support ticket for our technical team.",
        "ticket_id": f"TECH-{state['customer_id']}"
      }

  - name: handle_general
    run: |
      return {
        "response": f"Please email {{ variables.support_email }}",
        "ticket_id": f"GEN-{state['customer_id']}"
      }

edges:
  - from: __start__
    to: classify_intent
  - from: classify_intent
    to: handle_billing
    when: "state['intent'] == 'billing'"
  - from: classify_intent
    to: handle_cancellation
    when: "state['intent'] == 'cancellation'"
  - from: classify_intent
    to: handle_technical
    when: "state['intent'] == 'technical'"
  - from: classify_intent
    to: handle_general
    when: "state['intent'] == 'general'"
  - from: handle_billing
    to: __end__
  - from: handle_cancellation
    to: __end__
  - from: handle_technical
    to: __end__
  - from: handle_general
    to: __end__

config:
  raise_exceptions: true
```

---

## See Also

- [Node Specification](./nodes.md) - All execution methods
- [Navigation](./navigation.md) - Flow control patterns
- [Actions Reference](./actions/README.md) - Built-in actions
- `examples/yaml/` - Additional example agents
