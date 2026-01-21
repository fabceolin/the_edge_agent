# Task Dependency Analysis Prompt

Analyze the following tasks and determine their execution dependencies to maximize parallelization while respecting data and control flow requirements.

## Tasks to Analyze

{tasks_json}

## Analysis Guidelines

### Dependency Types to Identify

1. **Data Dependencies**: Task B requires output/results from Task A
   - Example: "Generate report" depends on "Fetch data"

2. **File Dependencies**: Task B reads/modifies files that Task A creates/modifies
   - Example: "Run tests" depends on "Build project"

3. **State Dependencies**: Task B requires system state created by Task A
   - Example: "Deploy" depends on "Configure environment"

4. **Order Dependencies**: Task B must logically follow Task A
   - Example: "Migrate data" depends on "Backup database"

### Rules for Analysis

- Tasks with NO dependencies between them can run in PARALLEL
- If task B uses ANY output from task A, add edge: A → B
- If task B modifies files task A reads, add edge: A → B (to avoid conflicts)
- Avoid circular dependencies (A → B → C → A is invalid)
- When uncertain, prefer sequential order (safer) over parallel (faster)

### Output Format

Return a JSON object with this exact structure:

```json
{{
    "nodes": ["task-id-1", "task-id-2", "task-id-3"],
    "edges": [
        {{"from": "task-id-1", "to": "task-id-2"}},
        {{"from": "task-id-1", "to": "task-id-3"}}
    ],
    "analysis": "Brief explanation of the dependency structure",
    "parallelizable_groups": [
        ["task-id-2", "task-id-3"]
    ]
}}
```

### Field Descriptions

- **nodes**: Array of all task IDs (must match input task IDs exactly)
- **edges**: Array of dependency edges where "from" must complete before "to" starts
- **analysis**: Human-readable explanation of why dependencies exist
- **parallelizable_groups**: Groups of tasks that can safely run in parallel

## Example Analysis

Input tasks:
```json
[
  {{"id": "fetch-users", "description": "Fetch user data from API"}},
  {{"id": "fetch-orders", "description": "Fetch order data from API"}},
  {{"id": "process-data", "description": "Process and merge user and order data"}},
  {{"id": "generate-report", "description": "Generate PDF report from processed data"}}
]
```

Output:
```json
{{
    "nodes": ["fetch-users", "fetch-orders", "process-data", "generate-report"],
    "edges": [
        {{"from": "fetch-users", "to": "process-data"}},
        {{"from": "fetch-orders", "to": "process-data"}},
        {{"from": "process-data", "to": "generate-report"}}
    ],
    "analysis": "fetch-users and fetch-orders are independent API calls that can run in parallel. process-data requires both data sources, so it depends on both. generate-report requires the processed data.",
    "parallelizable_groups": [
        ["fetch-users", "fetch-orders"]
    ]
}}
```

## Your Analysis

Analyze the tasks provided above and return ONLY the JSON object (no markdown code blocks or additional text).
