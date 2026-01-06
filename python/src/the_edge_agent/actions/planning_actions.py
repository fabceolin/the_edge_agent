"""
Planning Actions for YAMLEngine (TEA-AGENT-001.3).

This module provides planning and decomposition primitives for YAMLEngine workflows.
These actions implement the planning pattern from Chapter 6 of Agentic Design Patterns:

- plan.decompose: Decompose a goal into subtasks with dependencies
- plan.execute: Execute subtasks respecting dependency order
- plan.replan: Re-plan from current state preserving completed work
- plan.status: Get current plan execution status

All actions return structured output with planning traces suitable for
debugging and observability (Opik-compatible).

Example:
    >>> # Decompose a goal into subtasks
    >>> result = registry['plan.decompose'](
    ...     state={},
    ...     goal="Research and summarize topic X",
    ...     strategy="hierarchical",
    ...     model="gpt-4"
    ... )
    >>> print(result['plan']['subtasks'])

    >>> # Execute the plan
    >>> result = registry['plan.execute'](
    ...     state={"plan": result['plan']},
    ...     parallel=True,
    ...     max_concurrent=3
    ... )
    >>> print(result['subtask_results'])
"""

import json
import logging
import time
import copy
import uuid
from typing import Any, Callable, Dict, List, Optional, Set
from dataclasses import dataclass, field, asdict
from enum import Enum
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)


class SubtaskStatus(str, Enum):
    """Status of a subtask in the plan."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


class PlanningStrategy(str, Enum):
    """Strategy for plan decomposition."""

    FLAT = "flat"
    HIERARCHICAL = "hierarchical"
    ITERATIVE = "iterative"


class FailureStrategy(str, Enum):
    """Strategy for handling subtask failures."""

    REPLAN = "replan"
    RETRY = "retry"
    SKIP = "skip"
    ABORT = "abort"


@dataclass
class Subtask:
    """Represents a subtask in a plan."""

    id: str
    description: str
    dependencies: List[str] = field(default_factory=list)
    status: SubtaskStatus = SubtaskStatus.PENDING
    result: Optional[Any] = None
    error: Optional[str] = None
    retry_count: int = 0
    started_at: Optional[float] = None
    completed_at: Optional[float] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert subtask to dictionary."""
        return {
            "id": self.id,
            "description": self.description,
            "dependencies": self.dependencies,
            "status": (
                self.status.value
                if isinstance(self.status, SubtaskStatus)
                else self.status
            ),
            "result": self.result,
            "error": self.error,
            "retry_count": self.retry_count,
            "started_at": self.started_at,
            "completed_at": self.completed_at,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Subtask":
        """Create subtask from dictionary."""
        status = data.get("status", "pending")
        if isinstance(status, str):
            status = SubtaskStatus(status)
        return cls(
            id=data["id"],
            description=data["description"],
            dependencies=data.get("dependencies", []),
            status=status,
            result=data.get("result"),
            error=data.get("error"),
            retry_count=data.get("retry_count", 0),
            started_at=data.get("started_at"),
            completed_at=data.get("completed_at"),
        )


@dataclass
class Plan:
    """
    Represents a complete plan with subtasks and dependencies.

    The subtasks form a DAG (Directed Acyclic Graph) where dependencies
    define the execution order.
    """

    id: str
    goal: str
    strategy: PlanningStrategy
    subtasks: List[Subtask] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """Initialize metadata with defaults."""
        if "created_at" not in self.metadata:
            self.metadata["created_at"] = time.time()
        if "replan_count" not in self.metadata:
            self.metadata["replan_count"] = 0
        if "max_replans" not in self.metadata:
            self.metadata["max_replans"] = 3

    def to_dict(self) -> Dict[str, Any]:
        """Convert plan to dictionary for serialization."""
        return {
            "id": self.id,
            "goal": self.goal,
            "strategy": (
                self.strategy.value
                if isinstance(self.strategy, PlanningStrategy)
                else self.strategy
            ),
            "subtasks": [st.to_dict() for st in self.subtasks],
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Plan":
        """Create plan from dictionary."""
        strategy = data.get("strategy", "flat")
        if isinstance(strategy, str):
            strategy = PlanningStrategy(strategy)
        return cls(
            id=data["id"],
            goal=data["goal"],
            strategy=strategy,
            subtasks=[Subtask.from_dict(st) for st in data.get("subtasks", [])],
            metadata=data.get("metadata", {}),
        )

    def validate_dag(self) -> tuple[bool, Optional[str]]:
        """
        Validate that subtasks form a DAG (no cycles).

        Returns:
            (is_valid, error_message): True if DAG is valid, False with error message if not
        """
        # Build adjacency list
        subtask_ids = {st.id for st in self.subtasks}

        # Check all dependencies exist
        for st in self.subtasks:
            for dep_id in st.dependencies:
                if dep_id not in subtask_ids:
                    return (
                        False,
                        f"Subtask '{st.id}' depends on unknown subtask '{dep_id}'",
                    )

        # Detect cycles using DFS with coloring
        # WHITE = 0 (unvisited), GRAY = 1 (in progress), BLACK = 2 (completed)
        color: Dict[str, int] = {st.id: 0 for st in self.subtasks}

        # Build reverse adjacency (subtask -> its dependents)
        adjacency: Dict[str, List[str]] = {st.id: [] for st in self.subtasks}
        for st in self.subtasks:
            for dep_id in st.dependencies:
                adjacency[dep_id].append(st.id)

        def dfs(node_id: str, path: List[str]) -> Optional[str]:
            """DFS to detect cycle, returns cycle path if found."""
            if color[node_id] == 1:  # GRAY - cycle detected
                cycle_start = path.index(node_id)
                cycle_path = " -> ".join(path[cycle_start:] + [node_id])
                return f"Cycle detected: {cycle_path}"

            if color[node_id] == 2:  # BLACK - already processed
                return None

            color[node_id] = 1  # GRAY
            path.append(node_id)

            for neighbor in adjacency[node_id]:
                result = dfs(neighbor, path)
                if result:
                    return result

            path.pop()
            color[node_id] = 2  # BLACK
            return None

        # Run DFS from each unvisited node
        for st in self.subtasks:
            if color[st.id] == 0:
                cycle = dfs(st.id, [])
                if cycle:
                    return False, cycle

        return True, None

    def get_topological_order(self) -> List[str]:
        """
        Get subtask IDs in topological order (respecting dependencies).

        Returns:
            List of subtask IDs in execution order

        Raises:
            ValueError: If DAG is invalid (has cycles)
        """
        is_valid, error = self.validate_dag()
        if not is_valid:
            raise ValueError(f"Cannot compute topological order: {error}")

        # Kahn's algorithm for topological sort
        subtask_map = {st.id: st for st in self.subtasks}
        in_degree: Dict[str, int] = {
            st.id: len(st.dependencies) for st in self.subtasks
        }

        # Build reverse adjacency (subtask -> its dependents)
        dependents: Dict[str, List[str]] = {st.id: [] for st in self.subtasks}
        for st in self.subtasks:
            for dep_id in st.dependencies:
                dependents[dep_id].append(st.id)

        # Start with nodes that have no dependencies
        queue = [st_id for st_id, deg in in_degree.items() if deg == 0]
        result = []

        while queue:
            # Process in stable order for determinism
            queue.sort()
            current = queue.pop(0)
            result.append(current)

            for dependent in dependents[current]:
                in_degree[dependent] -= 1
                if in_degree[dependent] == 0:
                    queue.append(dependent)

        return result

    def get_ready_subtasks(self) -> List[Subtask]:
        """
        Get subtasks that are ready to execute (all dependencies completed).

        Returns:
            List of Subtask objects ready for execution
        """
        completed_ids = {
            st.id
            for st in self.subtasks
            if st.status in (SubtaskStatus.COMPLETED, SubtaskStatus.SKIPPED)
        }

        ready = []
        for st in self.subtasks:
            if st.status != SubtaskStatus.PENDING:
                continue
            # Check all dependencies are completed
            if all(dep_id in completed_ids for dep_id in st.dependencies):
                ready.append(st)

        return ready

    def get_subtask(self, subtask_id: str) -> Optional[Subtask]:
        """Get subtask by ID."""
        for st in self.subtasks:
            if st.id == subtask_id:
                return st
        return None

    def get_status_counts(self) -> Dict[str, int]:
        """Get count of subtasks by status."""
        counts = {
            "pending": 0,
            "in_progress": 0,
            "completed": 0,
            "failed": 0,
            "skipped": 0,
            "total": len(self.subtasks),
        }
        for st in self.subtasks:
            status_value = (
                st.status.value if isinstance(st.status, SubtaskStatus) else st.status
            )
            if status_value in counts:
                counts[status_value] += 1
        return counts


# Default prompts for planning actions
DECOMPOSE_FLAT_PROMPT = """You are a planning assistant. Decompose the given goal into a flat list of sequential subtasks.

Your response MUST be valid JSON in this exact format:
{{
  "subtasks": [
    {{"id": "subtask_1", "description": "First step to accomplish", "dependencies": []}},
    {{"id": "subtask_2", "description": "Second step that depends on first", "dependencies": ["subtask_1"]}},
    ...
  ]
}}

Rules:
1. Each subtask should be atomic and clearly defined
2. Dependencies must reference existing subtask IDs
3. Use descriptive IDs like "search_sources", "analyze_data", etc.
4. Ensure dependencies form a DAG (no cycles)
5. Return 3-10 subtasks for typical goals"""

DECOMPOSE_HIERARCHICAL_PROMPT = """You are a planning assistant. Decompose the given goal into a hierarchical tree of subtasks.

Your response MUST be valid JSON in this exact format:
{{
  "subtasks": [
    {{"id": "phase_1", "description": "First major phase", "dependencies": []}},
    {{"id": "phase_1_1", "description": "Sub-step of phase 1", "dependencies": ["phase_1"]}},
    {{"id": "phase_1_2", "description": "Another sub-step of phase 1", "dependencies": ["phase_1"]}},
    {{"id": "phase_2", "description": "Second phase (after phase 1 subtasks)", "dependencies": ["phase_1_1", "phase_1_2"]}},
    ...
  ]
}}

Rules:
1. Organize subtasks into phases/stages
2. Each phase can have sub-subtasks that depend on it
3. Use hierarchical IDs like "phase_1", "phase_1_1", "phase_2_3", etc.
4. Dependencies must form a tree/DAG structure
5. Max depth: {max_depth}
6. Limit total subtasks to {max_subtasks}"""

DECOMPOSE_ITERATIVE_PROMPT = """You are a planning assistant. Given the current state and goal, determine the next 1-3 subtasks to work on.

Current completed subtasks:
{completed_subtasks}

Your response MUST be valid JSON in this exact format:
{{
  "subtasks": [
    {{"id": "next_1", "description": "Next step based on current progress", "dependencies": []}},
    ...
  ],
  "planning_complete": false
}}

Set "planning_complete" to true when the goal can be achieved with the returned subtasks.

Rules:
1. Return only the immediately actionable next steps (1-3 subtasks)
2. Consider what has already been completed
3. Use descriptive IDs that indicate the subtask purpose"""


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register planning actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def _make_llm_call(
        model: str, messages: List[Dict], temperature: float = 0.7, **kwargs
    ) -> Dict[str, Any]:
        """
        Helper to make LLM call using the engine's llm.call action.
        """
        llm_call = registry.get("llm.call")
        if not llm_call:
            raise RuntimeError("llm.call action not found in registry")

        return llm_call(
            state={}, model=model, messages=messages, temperature=temperature, **kwargs
        )

    def _parse_json_response(content: str) -> Dict[str, Any]:
        """
        Parse JSON from LLM response, handling common issues.
        """
        if not content:
            raise ValueError("Empty response from LLM")

        # Try direct JSON parse first
        try:
            return json.loads(content)
        except json.JSONDecodeError:
            pass

        # Try to extract JSON from markdown code blocks
        import re

        json_match = re.search(r"```(?:json)?\s*([\s\S]*?)```", content)
        if json_match:
            try:
                return json.loads(json_match.group(1).strip())
            except json.JSONDecodeError:
                pass

        # Try to find JSON object in the response
        brace_match = re.search(r"\{[\s\S]*\}", content)
        if brace_match:
            try:
                return json.loads(brace_match.group())
            except json.JSONDecodeError:
                pass

        raise ValueError(f"Could not parse JSON from response: {content[:200]}...")

    def plan_decompose(
        state: Dict[str, Any],
        goal: str,
        model: str = "gpt-4",
        strategy: str = "flat",
        max_depth: int = 3,
        max_subtasks: int = 15,
        prompt_template: Optional[str] = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Decompose a goal into subtasks using LLM.

        Uses LLM to decompose goal into subtasks with dependencies,
        validates the plan structure, and returns a Plan object.

        Args:
            state: Current state dictionary
            goal: The goal to decompose
            model: LLM model to use (default: gpt-4)
            strategy: Decomposition strategy - flat, hierarchical, iterative
            max_depth: Maximum depth for hierarchical plans (default: 3)
            max_subtasks: Maximum number of subtasks (default: 15)
            prompt_template: Optional custom prompt template
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional LLM parameters

        Returns:
            {
                "plan": dict,              # Plan structure with subtasks
                "planning_trace": list,    # Trace for observability
                "success": True
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []

        try:
            # Validate strategy
            try:
                plan_strategy = PlanningStrategy(strategy)
            except ValueError:
                return {
                    "error": f"Unknown strategy: {strategy}. Use 'flat', 'hierarchical', or 'iterative'",
                    "success": False,
                }

            trace.append(
                {
                    "step": "decompose_start",
                    "timestamp": time.time(),
                    "goal": goal,
                    "strategy": strategy,
                    "model": model,
                }
            )

            # Select prompt based on strategy
            if prompt_template:
                system_prompt = prompt_template
            elif plan_strategy == PlanningStrategy.FLAT:
                system_prompt = DECOMPOSE_FLAT_PROMPT
            elif plan_strategy == PlanningStrategy.HIERARCHICAL:
                system_prompt = DECOMPOSE_HIERARCHICAL_PROMPT.format(
                    max_depth=max_depth, max_subtasks=max_subtasks
                )
            else:  # ITERATIVE
                completed = state.get("subtask_results", {})
                completed_list = [
                    f"- {k}: {str(v)[:100]}" for k, v in completed.items()
                ]
                system_prompt = DECOMPOSE_ITERATIVE_PROMPT.format(
                    completed_subtasks=(
                        "\n".join(completed_list) if completed_list else "None"
                    )
                )

            # Build messages
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"Goal: {goal}"},
            ]

            # Make LLM call
            response = _make_llm_call(
                model=model, messages=messages, temperature=temperature, **kwargs
            )

            if response.get("error"):
                return {
                    "error": response.get("error"),
                    "success": False,
                    "planning_trace": trace,
                }

            # Parse response
            content = response.get("content", "")
            try:
                parsed = _parse_json_response(content)
            except ValueError as e:
                return {
                    "error": f"Failed to parse plan: {e}",
                    "success": False,
                    "planning_trace": trace,
                }

            # Build subtasks from response
            subtasks_data = parsed.get("subtasks", [])
            if not subtasks_data:
                return {
                    "error": "No subtasks returned from LLM",
                    "success": False,
                    "planning_trace": trace,
                }

            # Enforce max_subtasks limit
            if len(subtasks_data) > max_subtasks:
                subtasks_data = subtasks_data[:max_subtasks]
                trace.append(
                    {
                        "step": "subtasks_truncated",
                        "timestamp": time.time(),
                        "original_count": len(parsed.get("subtasks", [])),
                        "truncated_to": max_subtasks,
                    }
                )

            # Create Subtask objects
            subtasks = []
            for st_data in subtasks_data:
                subtask = Subtask(
                    id=st_data.get("id", f"subtask_{len(subtasks)+1}"),
                    description=st_data.get("description", ""),
                    dependencies=st_data.get("dependencies", []),
                )
                subtasks.append(subtask)

            # Create Plan
            plan = Plan(
                id=f"plan_{uuid.uuid4().hex[:8]}",
                goal=goal,
                strategy=plan_strategy,
                subtasks=subtasks,
            )

            # Validate DAG
            is_valid, error = plan.validate_dag()
            if not is_valid:
                return {
                    "error": f"Invalid plan structure: {error}",
                    "success": False,
                    "planning_trace": trace,
                }

            # Record completion trace
            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "decompose_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "subtask_count": len(subtasks),
                    "plan_id": plan.id,
                }
            )

            plan_dict = plan.to_dict()

            return {
                "plan": plan_dict,
                "planning_trace": trace,
                "success": True,
                # State variable aliases
                "plan_id": plan.id,
                "subtask_count": len(subtasks),
            }

        except Exception as e:
            logger.error(f"plan.decompose failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Plan decomposition failed: {str(e)}",
                "success": False,
                "planning_trace": trace,
            }

    registry["plan.decompose"] = plan_decompose
    registry["actions.plan_decompose"] = plan_decompose

    def _execute_subtask(
        subtask: Subtask,
        state: Dict[str, Any],
        subtask_executor_config: Dict[str, Any],
        subtask_results: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute a single subtask using the configured executor.

        Args:
            subtask: Subtask to execute
            state: Current state
            subtask_executor_config: Configuration for subtask execution
            subtask_results: Results from previously completed subtasks

        Returns:
            Execution result dict
        """
        executor_action = subtask_executor_config.get("action", "llm.call")
        executor_params = subtask_executor_config.get("with", {})

        # Get the action from registry
        action_func = registry.get(executor_action)
        if not action_func:
            return {
                "error": f"Executor action '{executor_action}' not found",
                "success": False,
            }

        # Build execution context
        exec_state = copy.deepcopy(state)
        exec_state["current_subtask"] = subtask.to_dict()
        exec_state["subtask_results"] = subtask_results

        # Build parameters for the action
        params = copy.deepcopy(executor_params)

        # For LLM calls, build a meaningful prompt
        if executor_action == "llm.call":
            # Build context from dependencies
            dep_context = ""
            for dep_id in subtask.dependencies:
                if dep_id in subtask_results:
                    dep_result = subtask_results[dep_id]
                    dep_context += f"\nResult from '{dep_id}': {str(dep_result)[:500]}"

            messages = params.get("messages", [])
            if not messages:
                messages = [
                    {
                        "role": "system",
                        "content": "You are executing a subtask as part of a larger plan. Complete the subtask and return a useful result.",
                    },
                    {
                        "role": "user",
                        "content": f"Subtask: {subtask.description}{dep_context}",
                    },
                ]
            params["messages"] = messages

        try:
            result = action_func(state=exec_state, **params)
            return result
        except Exception as e:
            return {"error": str(e), "success": False}

    def plan_execute(
        state: Dict[str, Any],
        plan: Optional[Dict[str, Any]] = None,
        parallel: bool = False,
        max_concurrent: int = 3,
        subtask_executor: Optional[Dict[str, Any]] = None,
        on_subtask_failure: str = "abort",
        max_retries: int = 2,
        retry_delay: float = 1.0,
        max_replans: int = 2,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute plan subtasks respecting dependency order.

        Args:
            state: Current state dictionary (should contain 'plan' if not provided)
            plan: Plan dict to execute (optional, uses state['plan'] if not provided)
            parallel: Execute independent subtasks in parallel (default: False)
            max_concurrent: Max concurrent subtasks when parallel=True (default: 3)
            subtask_executor: Action config for executing subtasks
            on_subtask_failure: Failure strategy - replan, retry, skip, abort
            max_retries: Max retries per subtask (for retry strategy)
            retry_delay: Delay between retries in seconds
            max_replans: Max replan attempts (for replan strategy)
            **kwargs: Additional parameters

        Returns:
            {
                "plan": dict,              # Updated plan with results
                "subtask_results": dict,   # Map of subtask_id -> result
                "plan_status": dict,       # Status counts
                "success": True
            }
            Or {"error": str, "success": False} on failure
        """
        start_time = time.time()
        trace = []

        try:
            # Get plan from parameter or state
            plan_data = plan or state.get("plan")
            if not plan_data:
                return {
                    "error": "No plan provided. Use plan.decompose first or provide plan parameter.",
                    "success": False,
                }

            # Parse plan
            if isinstance(plan_data, dict):
                exec_plan = Plan.from_dict(plan_data)
            elif isinstance(plan_data, Plan):
                exec_plan = plan_data
            else:
                return {
                    "error": f"Invalid plan type: {type(plan_data)}",
                    "success": False,
                }

            # Validate failure strategy
            try:
                failure_strat = FailureStrategy(on_subtask_failure)
            except ValueError:
                return {
                    "error": f"Unknown failure strategy: {on_subtask_failure}. Use 'replan', 'retry', 'skip', or 'abort'",
                    "success": False,
                }

            # Default subtask executor
            if subtask_executor is None:
                subtask_executor = {
                    "action": "llm.call",
                    "with": {"model": kwargs.get("model", "gpt-4")},
                }

            trace.append(
                {
                    "step": "execute_start",
                    "timestamp": time.time(),
                    "plan_id": exec_plan.id,
                    "parallel": parallel,
                    "max_concurrent": max_concurrent,
                    "failure_strategy": on_subtask_failure,
                }
            )

            # Initialize subtask results from state or empty
            subtask_results: Dict[str, Any] = state.get("subtask_results", {}).copy()
            replan_count = state.get("replan_count", 0)

            # Get execution order
            try:
                execution_order = exec_plan.get_topological_order()
            except ValueError as e:
                return {"error": str(e), "success": False, "planning_trace": trace}

            # Execute subtasks
            if parallel:
                # Parallel execution
                completed_ids: Set[str] = {
                    st.id
                    for st in exec_plan.subtasks
                    if st.status in (SubtaskStatus.COMPLETED, SubtaskStatus.SKIPPED)
                }

                while True:
                    # Get ready subtasks
                    ready = exec_plan.get_ready_subtasks()
                    if not ready:
                        # Check if we're done or stuck
                        remaining = [
                            st
                            for st in exec_plan.subtasks
                            if st.status == SubtaskStatus.PENDING
                        ]
                        if not remaining:
                            break
                        # Stuck - check for failures
                        failed = [
                            st
                            for st in exec_plan.subtasks
                            if st.status == SubtaskStatus.FAILED
                        ]
                        if failed:
                            break
                        # This shouldn't happen if DAG is valid
                        return {
                            "error": "Execution stuck - no ready subtasks but pending remain",
                            "success": False,
                            "plan": exec_plan.to_dict(),
                            "subtask_results": subtask_results,
                        }

                    # Limit concurrent execution
                    batch = ready[:max_concurrent]

                    # Execute batch in parallel
                    with ThreadPoolExecutor(max_workers=max_concurrent) as executor:
                        futures = {}
                        for subtask in batch:
                            subtask.status = SubtaskStatus.IN_PROGRESS
                            subtask.started_at = time.time()
                            future = executor.submit(
                                _execute_subtask,
                                subtask,
                                state,
                                subtask_executor,
                                subtask_results,
                            )
                            futures[future] = subtask

                        for future in as_completed(futures):
                            subtask = futures[future]
                            try:
                                result = future.result()
                                if result.get("error"):
                                    # Handle failure
                                    handled = _handle_subtask_failure(
                                        subtask,
                                        result,
                                        exec_plan,
                                        state,
                                        failure_strat,
                                        max_retries,
                                        retry_delay,
                                        max_replans,
                                        replan_count,
                                        subtask_executor,
                                        subtask_results,
                                        trace,
                                    )
                                    if handled.get("abort"):
                                        return {
                                            "error": handled.get(
                                                "error", "Subtask failed"
                                            ),
                                            "success": False,
                                            "plan": exec_plan.to_dict(),
                                            "subtask_results": subtask_results,
                                            "planning_trace": trace,
                                        }
                                    if handled.get("replanned"):
                                        exec_plan = handled["new_plan"]
                                        replan_count = handled["replan_count"]
                                else:
                                    # Success
                                    subtask.status = SubtaskStatus.COMPLETED
                                    subtask.completed_at = time.time()
                                    subtask.result = result.get("content", result)
                                    subtask_results[subtask.id] = subtask.result
                                    completed_ids.add(subtask.id)

                                    trace.append(
                                        {
                                            "step": f"subtask_{subtask.id}_complete",
                                            "timestamp": time.time(),
                                            "duration": subtask.completed_at
                                            - subtask.started_at,
                                        }
                                    )
                            except Exception as e:
                                subtask.status = SubtaskStatus.FAILED
                                subtask.error = str(e)
                                subtask.completed_at = time.time()
                                trace.append(
                                    {
                                        "step": f"subtask_{subtask.id}_error",
                                        "timestamp": time.time(),
                                        "error": str(e),
                                    }
                                )
            else:
                # Sequential execution
                for subtask_id in execution_order:
                    subtask = exec_plan.get_subtask(subtask_id)
                    if not subtask:
                        continue

                    # Skip already completed subtasks
                    if subtask.status in (
                        SubtaskStatus.COMPLETED,
                        SubtaskStatus.SKIPPED,
                    ):
                        continue

                    subtask.status = SubtaskStatus.IN_PROGRESS
                    subtask.started_at = time.time()

                    result = _execute_subtask(
                        subtask, state, subtask_executor, subtask_results
                    )

                    if result.get("error"):
                        # Handle failure
                        handled = _handle_subtask_failure(
                            subtask,
                            result,
                            exec_plan,
                            state,
                            failure_strat,
                            max_retries,
                            retry_delay,
                            max_replans,
                            replan_count,
                            subtask_executor,
                            subtask_results,
                            trace,
                        )
                        if handled.get("abort"):
                            return {
                                "error": handled.get("error", "Subtask failed"),
                                "success": False,
                                "plan": exec_plan.to_dict(),
                                "subtask_results": subtask_results,
                                "planning_trace": trace,
                            }
                        if handled.get("replanned"):
                            exec_plan = handled["new_plan"]
                            replan_count = handled["replan_count"]
                            # Restart execution with new plan
                            execution_order = exec_plan.get_topological_order()
                    else:
                        # Success
                        subtask.status = SubtaskStatus.COMPLETED
                        subtask.completed_at = time.time()
                        subtask.result = result.get("content", result)
                        subtask_results[subtask.id] = subtask.result

                        trace.append(
                            {
                                "step": f"subtask_{subtask.id}_complete",
                                "timestamp": time.time(),
                                "duration": subtask.completed_at - subtask.started_at,
                            }
                        )

            # Get final status
            status_counts = exec_plan.get_status_counts()

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "execute_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "status_counts": status_counts,
                }
            )

            # Determine overall success
            success = status_counts["failed"] == 0 and status_counts["pending"] == 0

            return {
                "plan": exec_plan.to_dict(),
                "subtask_results": subtask_results,
                "plan_status": status_counts,
                "replan_count": replan_count,
                "planning_trace": trace,
                "success": success,
            }

        except Exception as e:
            logger.error(f"plan.execute failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Plan execution failed: {str(e)}",
                "success": False,
                "planning_trace": trace,
            }

    def _handle_subtask_failure(
        subtask: Subtask,
        result: Dict[str, Any],
        plan: Plan,
        state: Dict[str, Any],
        strategy: FailureStrategy,
        max_retries: int,
        retry_delay: float,
        max_replans: int,
        replan_count: int,
        subtask_executor: Dict[str, Any],
        subtask_results: Dict[str, Any],
        trace: List[Dict],
    ) -> Dict[str, Any]:
        """Handle subtask failure according to strategy."""
        error_msg = result.get("error", "Unknown error")

        if strategy == FailureStrategy.ABORT:
            subtask.status = SubtaskStatus.FAILED
            subtask.error = error_msg
            subtask.completed_at = time.time()
            trace.append(
                {
                    "step": f"subtask_{subtask.id}_failed_abort",
                    "timestamp": time.time(),
                    "error": error_msg,
                }
            )
            return {"abort": True, "error": error_msg}

        elif strategy == FailureStrategy.SKIP:
            subtask.status = SubtaskStatus.SKIPPED
            subtask.error = error_msg
            subtask.completed_at = time.time()
            trace.append(
                {
                    "step": f"subtask_{subtask.id}_skipped",
                    "timestamp": time.time(),
                    "error": error_msg,
                }
            )
            return {"skipped": True}

        elif strategy == FailureStrategy.RETRY:
            if subtask.retry_count >= max_retries:
                subtask.status = SubtaskStatus.FAILED
                subtask.error = f"Max retries ({max_retries}) exceeded: {error_msg}"
                subtask.completed_at = time.time()
                trace.append(
                    {
                        "step": f"subtask_{subtask.id}_max_retries",
                        "timestamp": time.time(),
                        "error": subtask.error,
                    }
                )
                return {"abort": True, "error": subtask.error}

            # Retry with backoff
            subtask.retry_count += 1
            delay = retry_delay * (
                2 ** (subtask.retry_count - 1)
            )  # Exponential backoff
            time.sleep(delay)

            trace.append(
                {
                    "step": f"subtask_{subtask.id}_retry_{subtask.retry_count}",
                    "timestamp": time.time(),
                    "delay": delay,
                }
            )

            # Execute again
            retry_result = _execute_subtask(
                subtask, state, subtask_executor, subtask_results
            )

            if retry_result.get("error"):
                # Recursively handle failure
                return _handle_subtask_failure(
                    subtask,
                    retry_result,
                    plan,
                    state,
                    strategy,
                    max_retries,
                    retry_delay,
                    max_replans,
                    replan_count,
                    subtask_executor,
                    subtask_results,
                    trace,
                )
            else:
                # Success on retry
                subtask.status = SubtaskStatus.COMPLETED
                subtask.completed_at = time.time()
                subtask.result = retry_result.get("content", retry_result)
                subtask_results[subtask.id] = subtask.result
                return {"success": True}

        elif strategy == FailureStrategy.REPLAN:
            if replan_count >= max_replans:
                subtask.status = SubtaskStatus.FAILED
                subtask.error = f"Max replans ({max_replans}) exceeded: {error_msg}"
                subtask.completed_at = time.time()
                trace.append(
                    {
                        "step": f"subtask_{subtask.id}_max_replans",
                        "timestamp": time.time(),
                        "error": subtask.error,
                    }
                )
                return {"abort": True, "error": subtask.error}

            # Trigger replan
            trace.append(
                {
                    "step": "triggering_replan",
                    "timestamp": time.time(),
                    "failed_subtask": subtask.id,
                    "replan_count": replan_count + 1,
                }
            )

            replan_result = plan_replan(
                state={
                    "plan": plan.to_dict(),
                    "subtask_results": subtask_results,
                    "failed_subtask": subtask.id,
                    "failure_reason": error_msg,
                },
                **state.get("planner_config", {}),
            )

            if replan_result.get("error"):
                subtask.status = SubtaskStatus.FAILED
                subtask.error = f"Replan failed: {replan_result.get('error')}"
                return {"abort": True, "error": subtask.error}

            return {
                "replanned": True,
                "new_plan": Plan.from_dict(replan_result["plan"]),
                "replan_count": replan_count + 1,
            }

        return {"abort": True, "error": "Unknown failure strategy"}

    registry["plan.execute"] = plan_execute
    registry["actions.plan_execute"] = plan_execute

    def plan_replan(
        state: Dict[str, Any],
        plan: Optional[Dict[str, Any]] = None,
        model: str = "gpt-4",
        temperature: float = 0.7,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Re-plan from current state, preserving completed subtasks.

        Triggers re-planning while keeping completed work. Adjusts the
        remaining plan based on current context and any failures.

        Args:
            state: Current state (should contain plan, subtask_results)
            plan: Plan dict to replan (optional, uses state['plan'])
            model: LLM model for planning (default: gpt-4)
            temperature: LLM temperature (default: 0.7)
            **kwargs: Additional parameters

        Returns:
            {
                "plan": dict,              # New plan with adjusted subtasks
                "preserved_subtasks": int, # Count of preserved completed subtasks
                "success": True
            }
            Or {"error": str, "success": False} on failure
        """
        trace = []
        start_time = time.time()

        try:
            plan_data = plan or state.get("plan")
            if not plan_data:
                return {"error": "No plan to replan", "success": False}

            current_plan = Plan.from_dict(plan_data)
            subtask_results = state.get("subtask_results", {})
            failed_subtask = state.get("failed_subtask")
            failure_reason = state.get("failure_reason", "Unknown")

            trace.append(
                {
                    "step": "replan_start",
                    "timestamp": time.time(),
                    "plan_id": current_plan.id,
                    "failed_subtask": failed_subtask,
                    "failure_reason": failure_reason,
                }
            )

            # Identify completed and remaining subtasks
            completed = []
            remaining = []
            for st in current_plan.subtasks:
                if st.status == SubtaskStatus.COMPLETED:
                    completed.append(st)
                else:
                    remaining.append(st)

            # Build replan prompt
            completed_info = "\n".join(
                [
                    f"- {st.id}: {st.description} (Result: {str(st.result)[:100]})"
                    for st in completed
                ]
            )

            remaining_info = "\n".join(
                [f"- {st.id}: {st.description}" for st in remaining]
            )

            replan_prompt = f"""You are re-planning after a subtask failure.

Original goal: {current_plan.goal}

COMPLETED subtasks (keep these):
{completed_info or "None"}

REMAINING subtasks (may need adjustment):
{remaining_info or "None"}

Failed subtask: {failed_subtask}
Failure reason: {failure_reason}

Create a NEW plan for the remaining work. You may:
1. Retry the failed subtask with different approach
2. Add alternative subtasks
3. Remove blocked subtasks
4. Adjust dependencies

Completed subtask IDs: {[st.id for st in completed]}

Your response MUST be valid JSON:
{{
  "subtasks": [
    {{"id": "new_subtask_1", "description": "...", "dependencies": [...]}},
    ...
  ],
  "adjustment_rationale": "Why you made these changes"
}}"""

            messages = [
                {
                    "role": "system",
                    "content": "You are a planning assistant helping to recover from a failed subtask.",
                },
                {"role": "user", "content": replan_prompt},
            ]

            response = _make_llm_call(
                model=model, messages=messages, temperature=temperature, **kwargs
            )

            if response.get("error"):
                return {
                    "error": response.get("error"),
                    "success": False,
                    "planning_trace": trace,
                }

            try:
                parsed = _parse_json_response(response.get("content", ""))
            except ValueError as e:
                return {
                    "error": f"Failed to parse replan: {e}",
                    "success": False,
                    "planning_trace": trace,
                }

            # Build new subtasks
            new_subtasks = []
            for st_data in parsed.get("subtasks", []):
                subtask = Subtask(
                    id=st_data.get("id", f"replan_subtask_{len(new_subtasks)+1}"),
                    description=st_data.get("description", ""),
                    dependencies=st_data.get("dependencies", []),
                )
                new_subtasks.append(subtask)

            # Create new plan preserving completed subtasks
            new_plan = Plan(
                id=f"replan_{uuid.uuid4().hex[:8]}",
                goal=current_plan.goal,
                strategy=current_plan.strategy,
                subtasks=completed + new_subtasks,
                metadata={
                    **current_plan.metadata,
                    "replan_count": current_plan.metadata.get("replan_count", 0) + 1,
                    "replanned_at": time.time(),
                    "replan_reason": failure_reason,
                },
            )

            # Validate new plan
            is_valid, error = new_plan.validate_dag()
            if not is_valid:
                return {
                    "error": f"Replanned structure invalid: {error}",
                    "success": False,
                    "planning_trace": trace,
                }

            elapsed = time.time() - start_time
            trace.append(
                {
                    "step": "replan_complete",
                    "timestamp": time.time(),
                    "elapsed_seconds": elapsed,
                    "preserved_subtasks": len(completed),
                    "new_subtasks": len(new_subtasks),
                    "rationale": parsed.get("adjustment_rationale", ""),
                }
            )

            return {
                "plan": new_plan.to_dict(),
                "preserved_subtasks": len(completed),
                "new_subtasks": len(new_subtasks),
                "planning_trace": trace,
                "success": True,
            }

        except Exception as e:
            logger.error(f"plan.replan failed: {e}")
            trace.append({"step": "error", "timestamp": time.time(), "error": str(e)})
            return {
                "error": f"Replan failed: {str(e)}",
                "success": False,
                "planning_trace": trace,
            }

    registry["plan.replan"] = plan_replan
    registry["actions.plan_replan"] = plan_replan

    def plan_status(
        state: Dict[str, Any],
        plan: Optional[Dict[str, Any]] = None,
        include_completed: bool = True,
        include_details: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Get current plan execution status.

        Returns aggregated status counts and optionally detailed subtask info.

        Args:
            state: Current state (should contain 'plan')
            plan: Plan dict (optional, uses state['plan'])
            include_completed: Include completed subtasks in response
            include_details: Include full subtask details
            **kwargs: Additional parameters

        Returns:
            {
                "status": dict,           # Status counts
                "progress": float,        # 0.0 to 1.0
                "subtasks": list,         # Optional: subtask details
                "success": True
            }
        """
        try:
            plan_data = plan or state.get("plan")
            if not plan_data:
                return {"error": "No plan found", "success": False}

            current_plan = Plan.from_dict(plan_data)
            status_counts = current_plan.get_status_counts()

            # Calculate progress
            total = status_counts["total"]
            completed = status_counts["completed"] + status_counts["skipped"]
            progress = completed / total if total > 0 else 1.0

            result = {
                "status": status_counts,
                "progress": progress,
                "plan_id": current_plan.id,
                "goal": current_plan.goal,
                "success": True,
            }

            if include_details:
                subtasks_info = []
                for st in current_plan.subtasks:
                    if not include_completed and st.status == SubtaskStatus.COMPLETED:
                        continue
                    subtasks_info.append(st.to_dict())
                result["subtasks"] = subtasks_info

            return result

        except Exception as e:
            logger.error(f"plan.status failed: {e}")
            return {"error": f"Status check failed: {str(e)}", "success": False}

    registry["plan.status"] = plan_status
    registry["actions.plan_status"] = plan_status
