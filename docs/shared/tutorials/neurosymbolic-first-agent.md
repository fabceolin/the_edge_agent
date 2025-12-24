# Tutorial: Build Your First Neurosymbolic Agent

This step-by-step tutorial walks you through building a complete neurosymbolic AI agent that combines neural classification with Prolog rule-based reasoning.

## What You'll Build

A **customer ticket router** that:
1. Classifies incoming support tickets using keyword matching (simulating a neural classifier)
2. Applies business rules in Prolog to determine routing and priority
3. Outputs a structured decision with reasoning

```
Input: "URGENT: Our production system is down!"
         ↓
    [Classifier Node]
    category: "critical_issue"
    confidence: 0.95
         ↓
    [Prolog Rule Engine]
    routing: "immediate_escalation"
    reason: "Critical issue with high confidence"
         ↓
Output: Routed to on-call team
```

## Prerequisites

### Python TEA

```bash
# Install TEA with Prolog support
pip install 'the_edge_agent[prolog]'

# Verify SWI-Prolog is installed
swipl --version  # Should show 9.1+
```

### Rust TEA

```bash
# Clone and build with Prolog
cd the_edge_agent/rust
cargo build --features prolog

# Verify SWI-Prolog is installed
swipl --version  # Should show 9.1+
```

## Step 1: Create the YAML Agent File

Create a new file called `ticket-router.yaml`:

```yaml
# ticket-router.yaml
# A neurosymbolic agent that routes support tickets

name: ticket-router-agent

description: |
  Demonstrates neurosymbolic AI: Neural classification
  followed by Prolog rule-based routing decisions.

state_schema:
  # Input
  ticket_text: str

  # Classification output
  category: str
  confidence: float

  # Routing decision
  routing: str
  priority: str
  reason: str

initial_state:
  ticket_text: ""
  category: ""
  confidence: 0.0
  routing: ""
  priority: ""
  reason: ""

nodes:
  # Node 1 will be added in Step 2
  # Node 2 will be added in Step 3

edges:
  - from: __start__
    to: classify_ticket
  - from: classify_ticket
    to: apply_routing_rules
  - from: apply_routing_rules
    to: __end__
```

## Step 2: Add the Classifier Node

The classifier node simulates a neural classifier. In production, you would replace this with an actual ML model or LLM call.

Add this node to the `nodes:` section:

```yaml
nodes:
  # Step 1: Classify the ticket using keyword matching
  # (In production, replace with LLM.call or ML model)
  - name: classify_ticket
    language: lua
    run: |
      local text = string.lower(state.ticket_text)
      local category = "general_inquiry"
      local confidence = 0.7

      -- Check for critical keywords
      if string.find(text, "urgent") or
         string.find(text, "critical") or
         string.find(text, "down") or
         string.find(text, "emergency") then
        category = "critical_issue"
        confidence = 0.95

      -- Check for billing keywords
      elseif string.find(text, "refund") or
             string.find(text, "billing") or
             string.find(text, "payment") or
             string.find(text, "invoice") then
        category = "billing"
        confidence = 0.88

      -- Check for technical support
      elseif string.find(text, "how to") or
             string.find(text, "help") or
             string.find(text, "error") or
             string.find(text, "issue") then
        category = "technical_support"
        confidence = 0.82
      end

      return {
        category = category,
        confidence = confidence
      }
```

**Why Lua?**
We use Lua here because it works in both Python TEA and Rust TEA. If you only target Python TEA, you can use Python instead.

## Step 3: Add the Prolog Rule Engine

Now add the Prolog node that applies business rules:

```yaml
  # Step 2: Apply routing rules with Prolog
  - name: apply_routing_rules
    language: prolog
    run: |
      % Read classification results from state
      state(category, Category),
      state(confidence, Confidence),

      % Determine priority based on category
      (Category = critical_issue ->
        Priority = p1
      ; Category = billing ->
        Priority = p2
      ;
        Priority = p3
      ),

      % Apply routing rules
      (
        % Rule 1: Critical issues with high confidence -> immediate escalation
        (Category = critical_issue, Confidence > 0.9) ->
          (Routing = immediate_escalation,
           Reason = 'Critical issue with high confidence - paging on-call')

        % Rule 2: Critical issues with lower confidence -> senior review
        ; Category = critical_issue ->
          (Routing = senior_review,
           Reason = 'Critical issue needs senior verification')

        % Rule 3: Billing issues -> billing team
        ; Category = billing, Confidence > 0.7 ->
          (Routing = billing_team,
           Reason = 'Billing-related issue routed to billing team')

        % Rule 4: Technical support -> support queue
        ; Category = technical_support ->
          (Routing = support_queue,
           Reason = 'Technical issue routed to support queue')

        % Rule 5: Low confidence -> human review
        ; Confidence < 0.5 ->
          (Routing = human_review,
           Reason = 'Low classification confidence - needs human review')

        % Default: general queue
        ; (Routing = general_queue,
           Reason = 'General inquiry routed to standard queue')
      ),

      % Return the routing decision
      return(routing, Routing),
      return(priority, Priority),
      return(reason, Reason).
```

**Key Prolog Concepts:**

1. **`state/2`**: Reads values from workflow state
   - `state(category, Category)` binds `Category` to the value of `state["category"]`

2. **Pattern Matching**: `Category = critical_issue` checks if Category equals the atom `critical_issue`

3. **Conditionals**: `(Condition -> ThenBranch ; ElseBranch)` is Prolog's if-then-else

4. **`return/2`**: Writes values back to workflow state

## Step 4: Complete YAML File

Here's the complete `ticket-router.yaml`:

```yaml
name: ticket-router-agent

description: |
  Demonstrates neurosymbolic AI: Neural classification
  followed by Prolog rule-based routing decisions.

state_schema:
  ticket_text: str
  category: str
  confidence: float
  routing: str
  priority: str
  reason: str

initial_state:
  ticket_text: ""
  category: ""
  confidence: 0.0
  routing: ""
  priority: ""
  reason: ""

nodes:
  - name: classify_ticket
    language: lua
    run: |
      local text = string.lower(state.ticket_text)
      local category = "general_inquiry"
      local confidence = 0.7

      if string.find(text, "urgent") or
         string.find(text, "critical") or
         string.find(text, "down") or
         string.find(text, "emergency") then
        category = "critical_issue"
        confidence = 0.95
      elseif string.find(text, "refund") or
             string.find(text, "billing") or
             string.find(text, "payment") then
        category = "billing"
        confidence = 0.88
      elseif string.find(text, "how to") or
             string.find(text, "help") or
             string.find(text, "error") then
        category = "technical_support"
        confidence = 0.82
      end

      return {
        category = category,
        confidence = confidence
      }

  - name: apply_routing_rules
    language: prolog
    run: |
      state(category, Category),
      state(confidence, Confidence),

      (Category = critical_issue -> Priority = p1
      ; Category = billing -> Priority = p2
      ; Priority = p3),

      ((Category = critical_issue, Confidence > 0.9) ->
        (Routing = immediate_escalation,
         Reason = 'Critical issue with high confidence - paging on-call')
      ; Category = critical_issue ->
        (Routing = senior_review,
         Reason = 'Critical issue needs senior verification')
      ; Category = billing, Confidence > 0.7 ->
        (Routing = billing_team,
         Reason = 'Billing-related issue routed to billing team')
      ; Category = technical_support ->
        (Routing = support_queue,
         Reason = 'Technical issue routed to support queue')
      ; Confidence < 0.5 ->
        (Routing = human_review,
         Reason = 'Low classification confidence - needs human review')
      ; (Routing = general_queue,
         Reason = 'General inquiry routed to standard queue')
      ),

      return(routing, Routing),
      return(priority, Priority),
      return(reason, Reason).

edges:
  - from: __start__
    to: classify_ticket
  - from: classify_ticket
    to: apply_routing_rules
  - from: apply_routing_rules
    to: __end__
```

## Step 5: Run the Agent

### Python

```python
from the_edge_agent import YAMLEngine
import json

# Create engine with Prolog enabled
engine = YAMLEngine(prolog_enabled=True)

# Load the agent
graph = engine.load_from_file("ticket-router.yaml")
compiled = graph.compile()

# Test with different tickets
test_tickets = [
    "URGENT: Our production system is down!",
    "I need a refund for my last purchase",
    "How do I reset my password?",
    "Just checking on my order status"
]

for ticket in test_tickets:
    print(f"\n{'='*60}")
    print(f"Input: {ticket}")
    print('='*60)

    initial_state = {
        "ticket_text": ticket,
        "category": "",
        "confidence": 0.0,
        "routing": "",
        "priority": "",
        "reason": ""
    }

    for event in compiled.invoke(initial_state):
        result = event

    state = result.get("state", result)
    print(f"Category:   {state['category']}")
    print(f"Confidence: {state['confidence']}")
    print(f"Routing:    {state['routing']}")
    print(f"Priority:   {state['priority']}")
    print(f"Reason:     {state['reason']}")
```

### Rust

```bash
cargo run --features prolog -- run ticket-router.yaml \
  --input '{"ticket_text": "URGENT: Production is down!"}'
```

## Expected Output

```
============================================================
Input: URGENT: Our production system is down!
============================================================
Category:   critical_issue
Confidence: 0.95
Routing:    immediate_escalation
Priority:   p1
Reason:     Critical issue with high confidence - paging on-call

============================================================
Input: I need a refund for my last purchase
============================================================
Category:   billing
Confidence: 0.88
Routing:    billing_team
Priority:   p2
Reason:     Billing-related issue routed to billing team

============================================================
Input: How do I reset my password?
============================================================
Category:   technical_support
Confidence: 0.82
Routing:    support_queue
Priority:   p3
Reason:     Technical issue routed to support queue

============================================================
Input: Just checking on my order status
============================================================
Category:   general_inquiry
Confidence: 0.7
Routing:    general_queue
Priority:   p3
Reason:     General inquiry routed to standard queue
```

## Step 6: Enhance with LLM Classification

Replace the Lua classifier with an actual LLM call:

```yaml
  - name: classify_ticket
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: system
          content: |
            Classify the support ticket into one of these categories:
            - critical_issue: Production down, urgent, emergency
            - billing: Refunds, payments, invoices
            - technical_support: How-to questions, errors, issues
            - general_inquiry: Everything else

            Return JSON: {"category": "...", "confidence": 0.0-1.0}
        - role: user
          content: "{{ state.ticket_text }}"
      response_format: json
    output: classification
```

The Prolog rules remain unchanged - they work with any classification input!

## Key Takeaways

1. **Separation of Concerns**: Neural components handle perception/classification; symbolic components handle logic/rules.

2. **Explainability**: The Prolog rules provide clear reasoning traces.

3. **Maintainability**: Business rules can be updated without retraining ML models.

4. **Portability**: Using Lua + Prolog works in both Python and Rust TEA.

5. **Extensibility**: Add new rules without changing the classifier; add new categories without changing rules.

## Next Steps

- **Add more rules**: SLA-based escalation, customer tier-based routing
- **Add validation**: Use Prolog to validate ticket format
- **Add CLP(FD)**: Schedule tickets based on agent availability
- **Explore examples**: See `examples/prolog/neurosymbolic/` for more patterns

## Related Documentation

- [Neurosymbolic Patterns Guide](../architecture/neurosymbolic-patterns.md)
- [Python Prolog Guide](../../python/prolog-guide.md)
- [Rust Prolog Guide](../../rust/prolog-guide.md)
- [YAML Reference](../YAML_REFERENCE.md)
