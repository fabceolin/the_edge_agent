# Custom Actions

You can create reusable action modules that can be loaded via the CLI or YAML configuration.

## Creating an Action Module

```python
# my_custom_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register custom actions into the provided registry."""

    def custom_search(state, query, **kwargs):
        # Your custom search logic
        return {"results": [...], "success": True}

    def custom_transform(state, data, **kwargs):
        # Your custom transformation logic
        return {"transformed": data, "success": True}

    registry['custom_search'] = custom_search
    registry['custom_transform'] = custom_transform

# Optional metadata for module discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My company's custom actions",
    "actions": ["custom_search", "custom_transform"],
}
```

## Using Custom Actions in YAML

Reference your custom actions in agent YAML files:

```yaml
name: my_agent
nodes:
  - name: search
    uses: custom_search
    with:
      query: "{{ state.query }}"

  - name: process
    uses: custom_transform
    with:
      data: "{{ state.search_results }}"

edges:
  - from: __start__
    to: search
  - from: search
    to: process
  - from: process
    to: __end__
```

## Loading Actions via CLI

```bash
# Load from installed package
tea run agent.yaml --actions-module my_custom_actions

# Load from file
tea run agent.yaml --actions-file ./my_custom_actions.py

# Load multiple modules
tea run agent.yaml --actions-module company.actions --actions-file ./local_actions.py
```

## Loading Actions via YAML

You can also specify action imports directly in your YAML:

```yaml
name: my_agent

imports:
  - module: company.actions
  - file: ./local_actions.py

nodes:
  - name: process
    uses: custom_action_from_import
    with:
      data: "{{ state.input }}"
```

## Action Function Signature

Custom actions receive the current state and any parameters specified in `with`:

```python
def my_action(state, param1, param2=None, **kwargs):
    """
    Args:
        state: Current workflow state (dict)
        param1: Required parameter from 'with' section
        param2: Optional parameter with default
        **kwargs: Additional parameters including:
            - secrets: Dict of secrets if provided
            - engine: Reference to YAMLEngine instance

    Returns:
        dict: State updates to merge into current state
    """
    # Access current state
    current_value = state.get("some_key")

    # Access secrets if needed
    api_key = kwargs.get("secrets", {}).get("api_key")

    # Return state updates
    return {
        "result": "processed",
        "success": True
    }
```

## Action Loading Priority

When multiple action sources are specified, they are loaded in this order (later sources override earlier):

1. **Built-in actions** (lowest priority)
2. **CLI `--actions-module` flags** (in order specified)
3. **CLI `--actions-file` flags** (in order specified)
4. **YAML `imports:` section** (highest priority - overrides CLI actions)

## Best Practices

### 1. Namespace Your Actions

Avoid conflicts with built-in actions:

```python
def register_actions(registry, engine):
    registry['mycompany.search'] = my_search
    registry['mycompany.transform'] = my_transform
```

### 2. Validate Inputs

Check required parameters early:

```python
def my_action(state, query, **kwargs):
    if not query:
        return {"error": "Query is required", "success": False}
    # ... rest of logic
```

### 3. Handle Errors Gracefully

Return error information in state rather than raising exceptions:

```python
def my_action(state, **kwargs):
    try:
        result = do_something()
        return {"result": result, "success": True}
    except Exception as e:
        return {"error": str(e), "success": False}
```

### 4. Document Your Actions

Include the metadata block for discoverability:

```python
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Description of this action module",
    "actions": ["action1", "action2"],
    "author": "Your Name",
    "requires": ["requests", "pandas"],  # External dependencies
}
```

## Security Warning

The `--actions-module` and `--actions-file` flags execute Python code from the specified modules. **Only load actions from trusted sources.** For production use, prefer installed packages over local files.

## See Also

- [CLI Reference](../shared/cli-reference.md)
- [Actions Reference](actions-reference.md)
- [YAML Reference](../shared/YAML_REFERENCE.md)
