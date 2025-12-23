# CLP(FD) Constraint Scheduling

This example demonstrates **constraint-based scheduling** using SWI-Prolog's CLP(FD) (Constraint Logic Programming over Finite Domains) library.

> **Note:** This example uses inline rule definitions (TEA-RUST-038) and the `:- use_module(library(clpfd)).` directive, demonstrating full Prolog integration in both Python and Rust runtimes.

## The Problem

Given:
- A set of tasks with durations
- Precedence constraints (task A must complete before task B starts)
- A maximum time window

Find a valid schedule that satisfies all constraints, minimizing total time.

## Why CLP(FD)?

Traditional approaches require explicit search algorithms:

```
// Pseudo-code for brute force scheduling
for each permutation of tasks:
    for each possible start time:
        if satisfies_all_constraints():
            return schedule
```

CLP(FD) declares constraints and lets the solver find solutions:

```prolog
% Declare constraints
Starts ins 0..MaxTime,
StartA + DurA #=< StartB,  % A finishes before B starts

% Solver finds valid assignment
labeling([min(Makespan)], Starts).
```

## The Neurosymbolic Pattern

```
┌─────────────────────┐     ┌─────────────────────┐     ┌─────────────────────┐
│   Task Parameters   │     │   CLP(FD) Solver    │     │   Valid Schedule    │
│   (from workflow)   │ ──► │   (constraint)      │ ──► │   (guaranteed)      │
└─────────────────────┘     └─────────────────────┘     └─────────────────────┘
         │                           │                           │
         ▼                           ▼                           ▼
   tasks: [A, B, C]           Starts ins 0..10           A: 0-2, B: 2-6...
   precedences: [[A,B]]       A + dur_A <= B             feasible: true
   total_time: 10             labeling(min, Starts)      makespan: 9
```

## How It Works

### Step 1: Define Variables

```prolog
% Create N start time variables
length(Starts, N),
Starts ins 0..MaxTime,
```

### Step 2: Add Constraints

```prolog
% Precedence: A must finish before B starts
StartA + DurationA #=< StartB,

% Time limit: all tasks must complete
Start + Duration #=< MaxTime,
```

### Step 3: Solve with Optimization

```prolog
% Find minimum makespan (total completion time)
labeling([min(Makespan)], Starts).
```

## CLP(FD) Constraint Types

| Constraint | Syntax | Purpose |
|------------|--------|---------|
| Domain | `X in 1..10` | Variable range |
| Equality | `X #= Y + 5` | Arithmetic equality |
| Less than | `X #< Y` | Ordering |
| All different | `all_different(Xs)` | Unique values |
| Chain | `chain(Xs, #<)` | Ordered sequence |
| Reification | `X #< Y #<==> B` | Boolean constraint |

## Running the Example

**Python:**
```bash
cd python
python -m the_edge_agent run ../examples/prolog/neurosymbolic/clpfd-scheduling.yaml
```

**Rust:**
```bash
cd rust
LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux \
  cargo run --features prolog -- run ../examples/prolog/neurosymbolic/clpfd-scheduling.yaml
```

**Expected output:**
```json
{
  "schedule": [
    ["design", 0, 2],
    ["implement", 2, 6],
    ["test", 6, 8],
    ["deploy", 8, 9]
  ],
  "feasible": true,
  "makespan": 9,
  "solver_info": "CLP(FD) solved with first-fail heuristic"
}
```

## Use Cases

### 1. Project Planning

```yaml
tasks:
  - {name: requirements, duration: 5}
  - {name: design, duration: 3}
  - {name: implementation, duration: 10}
  - {name: testing, duration: 4}
  - {name: deployment, duration: 1}
must_precede:
  - [requirements, design]
  - [design, implementation]
  - [implementation, testing]
  - [testing, deployment]
```

### 2. Resource Allocation

Add resource constraints:

```prolog
% Each resource can only do one task at a time
no_overlap(Tasks, Starts, Durations).
```

### 3. Meeting Scheduling

```yaml
tasks:
  - {name: standup, duration: 1}
  - {name: planning, duration: 2}
  - {name: review, duration: 1}
constraints:
  - no_overlap  # people can't be in two meetings
  - within_working_hours  # 9am-5pm
```

### 4. Manufacturing

```prolog
% Machine capacity constraints
machine_1_tasks ins 0..8,  % Machine 1 available 8 hours
machine_2_tasks ins 0..6,  % Machine 2 available 6 hours
```

## Extending the Example

### Adding Resource Constraints

```prolog
% Each resource can handle only one task at a time
cumulative(Tasks, Starts, Durations, Resources, MaxCapacity).
```

### Adding Time Windows

```prolog
% Task must start between 9am and 5pm
Start in 9..17.
```

### Finding All Solutions

```prolog
% Get all valid schedules
findall(Schedule, solve_schedule(Schedule), AllSchedules).
```

## Performance Considerations

| Problem Size | Approach |
|--------------|----------|
| < 20 tasks | Direct labeling |
| 20-100 tasks | Heuristic labeling (`ff`, `ffc`) |
| > 100 tasks | Decomposition or hybrid approaches |

### Labeling Strategies

```prolog
% First-fail: choose variable with smallest domain first
labeling([ff], Starts).

% First-fail + constraint count
labeling([ffc], Starts).

% Minimize objective
labeling([min(Makespan)], Starts).
```

## Error Handling

If no valid schedule exists:

```prolog
% Check feasibility
(solve_schedule(Schedule) ->
    return(feasible, true)
;
    return(feasible, false),
    return(solver_info, 'No valid schedule exists')
).
```

## Related Examples

- [Classifier Rules](classifier-rules.md) - Decision making with rules
- [Knowledge Graph](knowledge-graph.md) - Graph-based reasoning
- [Multi-Step Reasoning](reasoning-chain.md) - Iterative inference

## Further Reading

- [SWI-Prolog CLP(FD) Manual](https://www.swi-prolog.org/man/clpfd.html)
- [Constraint Logic Programming](https://en.wikipedia.org/wiki/Constraint_logic_programming)
