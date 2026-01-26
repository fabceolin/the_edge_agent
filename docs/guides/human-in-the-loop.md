# Human-in-the-Loop Workflows

The Edge Agent supports human-in-the-loop workflows via interactive interrupts. When a YAML agent defines `interrupt_before` or `interrupt_after`, execution pauses at those points, allowing you to review state and make decisions before continuing.

## Basic Usage

```bash
# Run agent with interrupts (interactive mode)
tea run examples/customer_support.yaml --input '{"message": "My bill is wrong"}'

# Run with automatic interactive loop (recommended for conversational agents)
tea run examples/interview.yaml --interactive

# Resume from a saved checkpoint
tea resume ./checkpoints/classify_intent_1734567890.pkl --workflow examples/customer_support.yaml

# Auto-continue mode (skip interactive prompts for CI/CD)
tea run examples/customer_support.yaml --auto-continue
```

## Interactive Mode (`--interactive` / `-I`)

The `--interactive` flag enables a seamless conversational loop for agents that require back-and-forth human input. Instead of manually resuming from checkpoints, interactive mode automatically:

1. Extracts questions from agent state
2. Displays them to the user
3. Collects user input
4. Injects the response back into state
5. Resumes execution until completion

### Basic Interactive Example

```bash
tea run interview_agent.yaml --interactive
```

This starts a conversational session:

```
Agent: What is your name?
> John Smith

Agent: Tell me about your experience with sales.
> I worked at TechStore for 3 years where I...

Agent: How do you handle difficult customers?
> I always try to listen first and understand their concerns...

[Interview Complete]
Recommendation: HIRE
```

### Interactive Mode with Custom Input

```bash
# Start interactive session with initial state
tea run retail_sales_interviewer.yaml --interactive \
  --input '{"candidate_name": "John Smith", "candidate_id": "cand_001"}'
```

### Configuring Question/Response Keys

By default, interactive mode looks for common state keys. You can customize this:

```bash
tea run agent.yaml --interactive \
  --question-key "next_question,prompt,ask" \
  --response-key "user_response" \
  --complete-key "interview_complete,done"
```

| Flag | Description | Default |
|------|-------------|---------|
| `--interactive` / `-I` | Enable interactive HITL mode | Off |
| `--question-key` | State key(s) for question extraction (comma-separated) | `question,prompt,message,ask,next_question` |
| `--response-key` | State key for user response injection | `response` |
| `--complete-key` | State key(s) signaling completion (comma-separated) | `complete,done,finished` |
| `--display-key` | Additional state key(s) to display to user | Question only |
| `--display-format` | Output format: `pretty`, `json`, `raw` | `pretty` |
| `--input-timeout` | Timeout in seconds for user input (0=infinite) | `0` |

### Display Formats

```bash
# Pretty output (default) - formatted for readability
tea run agent.yaml --interactive --display-format pretty

# JSON output - machine-readable
tea run agent.yaml --interactive --display-format json

# Raw output - minimal formatting
tea run agent.yaml --interactive --display-format raw
```

### Interactive Mode vs Manual Resume

| Aspect | Without `--interactive` | With `--interactive` |
|--------|-------------------------|----------------------|
| Checkpoint handling | Manual `tea resume` | Automatic |
| User input | Edit checkpoint or `--input` | Direct stdin prompt |
| Session continuity | Multiple commands | Single session |
| Best for | CI/CD, scripted flows | Conversational agents |

### Example: Interview Bot

```yaml
# interview.yaml
name: interview_agent
description: Simple Q&A interview bot

state_schema:
  questions: list
  current_index: int
  next_question: str
  response: str
  answers: list
  interview_complete: bool

nodes:
  - name: ask_question
    run: |
      questions = state.get("questions", ["What is your name?", "What is your experience?"])
      idx = state.get("current_index", 0)
      answers = state.get("answers", [])

      # Store previous answer if any
      if state.get("response"):
        answers = list(answers)
        answers.append(state["response"])

      if idx >= len(questions):
        return {
          "interview_complete": True,
          "answers": answers,
          "next_question": "Thank you for completing the interview!"
        }

      return {
        "next_question": questions[idx],
        "current_index": idx + 1,
        "answers": answers,
        "interview_complete": False
      }

edges:
  - from: __start__
    to: ask_question
  - from: ask_question
    to: __end__

config:
  interrupt_after: [ask_question]
  checkpoint_dir: ./checkpoints
```

Run it:

```bash
tea run interview.yaml --interactive

# Output:
# What is your name?
# > Alice
# What is your experience?
# > 5 years in software development
# Thank you for completing the interview!
```

### Mutual Exclusivity

The `--interactive` and `--stream` flags are mutually exclusive:

```bash
# This will error:
tea run agent.yaml --interactive --stream
# Error: --interactive and --stream are mutually exclusive
```

## Interactive Prompt Example

When execution reaches an interrupt point:

```
[check] classify_intent

[pause]  Interrupt at: classify_intent
   State: {
     "customer_message": "My bill is wrong",
     "intent": "billing",
     "confidence": 0.95
   }

Checkpoint saved: ./checkpoints/classify_intent_1734567890123.pkl

Review the state above. Options:
  [c] Continue with current state
  [u] Update state before continuing
  [a] Abort execution

Choice: u

Enter state updates as JSON (or press Enter to skip):
{"escalate": true, "priority": "high"}

State updated. Resuming execution...

[check] handle_billing
[check] escalate_to_human

================================================================================
[check] Completed
================================================================================
```

## Configuring Interrupts in YAML

Define interrupts in your YAML agent configuration:

```yaml
name: customer_support_agent

nodes:
  - name: classify_intent
    uses: llm.call
    with:
      model: gpt-4
      messages:
        - role: user
          content: "Classify this customer message: {{ state.message }}"

  - name: handle_billing
    run: |
      return {"handled": True, "response": "Billing issue processed"}

edges:
  - from: __start__
    to: classify_intent
  - from: classify_intent
    to: handle_billing
  - from: handle_billing
    to: __end__

config:
  checkpoint_dir: ./checkpoints
  interrupt_after: [classify_intent]  # Pause after intent classification
  raise_exceptions: true
```

## Resume with State Updates

You can resume from a checkpoint and merge in new state:

```bash
# Resume with additional state via --input flag
tea resume ./checkpoints/classify_intent_123.pkl \
  --workflow agent.yaml \
  --input '{"approved": true, "notes": "Verified with supervisor"}'
```

### State Merge Precedence

When resuming, state is merged in this order (highest to lowest priority):

1. User input from interactive prompt
2. `--input` flag value
3. Checkpoint state
4. YAML initial state defaults

## Non-Interactive Mode (CI/CD)

For automated environments where interactive prompts would block execution:

```bash
# Auto-continue mode: execution continues at interrupts without pausing
tea run agent.yaml --auto-continue

# This also works in Docker, systemd services, and CI pipelines
# where stdin is not a TTY
```

The CLI automatically detects non-TTY environments (Docker, CI/CD) and auto-continues to prevent hanging.

## CLI Interrupt Flags

You can override YAML interrupt configuration via CLI flags:

```bash
# Interrupt before specific nodes
tea run workflow.yaml --interrupt-before node1,node2

# Interrupt after specific nodes
tea run workflow.yaml --interrupt-after classify,validate

# Combine both
tea run workflow.yaml --interrupt-before review --interrupt-after approve
```

## Use Cases

### 1. Approval Workflows

Pause before executing critical actions that require human approval:

```yaml
config:
  interrupt_before: [execute_payment, delete_account]
```

### 2. Review and Correction

Pause after LLM processing to review and correct outputs:

```yaml
config:
  interrupt_after: [generate_response, extract_entities]
```

### 3. Conversational Agents (Interactive Mode)

For multi-turn conversations like interviews, surveys, or chatbots:

```bash
tea run interview.yaml --interactive \
  --question-key "next_question" \
  --response-key "user_response" \
  --complete-key "interview_complete"
```

### 4. Debugging

Step through workflow execution to understand agent behavior:

```bash
tea run agent.yaml --interrupt-after all_nodes
```

## Complete Interactive Example: Retail Interview Bot

Here's a real-world example of using `--interactive` for a sales position interview:

```bash
# Start the interview with candidate info
tea run retail_sales_interviewer.yaml --interactive \
  --input '{"candidate_name": "John Smith", "candidate_id": "cand_001"}'
```

Session output:

```
Agent: Hello John Smith! Welcome to the interview for Sales Associate at
       TechStore. To start, tell me about yourself: your experience with
       sales and why you want to work with us?

> I've worked in retail for 3 years at BestBuy. I love helping customers
  find the right products and I'm passionate about technology.

[INFO] Extracted 2/10 competencies with evidence
[INFO] Identified 5 competency gaps to explore

Agent: A customer throws a shirt at you and demands a refund for a 6-month-old
       purchase. Other customers are watching. What do you do in the first
       30 seconds?

> First, I would stay calm and not take it personally. I'd acknowledge their
  frustration, move to a quieter area if possible, and listen to understand
  the real issue before explaining our return policy options.

[INFO] Extracted 4/10 competencies with evidence

Agent: It's 9:50pm on a Sunday, you're exhausted, and a customer walks in
       wanting to see 10 shoe models. How do you serve them?

> I would greet them warmly and help them, even though I'm tired. A good
  customer experience is more important than leaving a few minutes early.
  I'd ask qualifying questions to narrow down their options efficiently.

...

[INFO] Ending interview: all_competencies_assessed
[INFO] Final recommendation: HIRE

================================================================================
Interview Complete!
================================================================================
Recommendation: HIRE
Justification: Candidate demonstrated strong sales technique with specific
examples from their BestBuy experience, excellent conflict management skills,
and genuine customer-first attitude.

Results saved to: /tmp/retail_interviews/result_cand_001.json
```

The key YAML configuration for this flow:

```yaml
config:
  checkpoint_dir: /tmp/retail_checkpoints
  interrupt_after:
    - filter_followup_response  # Pause after each question
```

The agent uses `next_question` for questions, `response` for user input, and `interview_complete` to signal completion—all matching the default interactive mode keys.

## Security Warning

**Checkpoint files use Python pickle format and should only be loaded from trusted sources.** Do not load checkpoints from untrusted origins as they can execute arbitrary code during unpickling.

## See Also

- [CLI Reference](../shared/cli-reference.md)
- [Checkpoint Guide](../shared/architecture/checkpoint-guide.md)
- [YAML Reference](../shared/YAML_REFERENCE.md)
