"""
Multi-Agent Collaboration Actions for YAMLEngine (TEA-AGENT-001.1).

This module provides high-level multi-agent collaboration primitives:
- Agent registry for managing agent definitions from settings.agents
- agent.dispatch: Dispatch task to a single named agent
- agent.parallel: Parallel dispatch with aggregation strategies
- agent.sequential: Chain agents with state threading
- agent.coordinate: Coordinator pattern with leader agent
- agent.crewai_delegate: Bridge to CrewAI for complex workflows

Actions:
    - agent.dispatch: Dispatch task to a single agent
    - agent.parallel: Execute same task across multiple agents
    - agent.sequential: Chain agents with output->input threading
    - agent.coordinate: Leader-worker coordination pattern
    - agent.crewai_delegate: Delegate to CrewAI hierarchical process

Example:
    >>> # Dispatch to a single agent
    >>> result = registry['agent.dispatch'](
    ...     state={"query": "Explain quantum computing"},
    ...     agent="researcher",
    ...     task="Answer the query: {{ state.query }}"
    ... )
    >>> print(result['response'])

    >>> # Parallel dispatch with voting
    >>> result = registry['agent.parallel'](
    ...     state={"question": "Is AI beneficial?"},
    ...     agents=["optimist", "pessimist", "neutral"],
    ...     task="Answer: {{ state.question }}",
    ...     aggregation="vote"
    ... )
    >>> print(result['result'])  # Majority response
"""

import copy
import hashlib
import logging
import time
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed, Future
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Union

from jinja2 import Environment, BaseLoader, StrictUndefined, TemplateError

logger = logging.getLogger(__name__)


# ============================================================================
# Agent Registry (Task 1: AC1)
# ============================================================================


@dataclass
class AgentConfig:
    """Configuration for a single agent."""

    name: str
    model: str
    system_prompt: Optional[str] = None
    temperature: float = 0.7
    max_tokens: Optional[int] = None
    tools: List[str] = field(default_factory=list)
    timeout: int = 60
    retry_max_attempts: int = 3
    retry_backoff: float = 2.0

    def to_llm_params(self) -> Dict[str, Any]:
        """Convert agent config to llm.call parameters."""
        params: Dict[str, Any] = {"model": self.model}
        if self.system_prompt:
            params["system_prompt"] = self.system_prompt
        if self.temperature is not None:
            params["temperature"] = self.temperature
        if self.max_tokens:
            params["max_tokens"] = self.max_tokens
        if self.timeout:
            params["timeout"] = self.timeout
        return params


class AgentRegistry:
    """
    Registry for managing agent definitions.

    Parses agent configurations from settings.agents YAML section
    and provides agent lookup with inheritance from settings.llm defaults.
    """

    def __init__(
        self,
        agents_config: Optional[Dict[str, Dict[str, Any]]] = None,
        llm_defaults: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize the agent registry.

        Args:
            agents_config: Dictionary of agent configurations from settings.agents
            llm_defaults: Default LLM settings from settings.llm for inheritance
        """
        self._agents: Dict[str, AgentConfig] = {}
        self._llm_defaults = llm_defaults or {}
        if agents_config:
            self._parse_agents(agents_config)

    def _parse_agents(self, agents_config: Dict[str, Dict[str, Any]]) -> None:
        """
        Parse and validate agent configurations.

        Args:
            agents_config: Raw agent configurations from YAML

        Raises:
            ValueError: If required fields are missing or invalid
        """
        for name, config in agents_config.items():
            if not isinstance(config, dict):
                raise ValueError(
                    f"Agent '{name}' configuration must be a dictionary, "
                    f"got {type(config).__name__}"
                )

            # Model is required (unless inheriting from llm defaults)
            model = config.get("model", self._llm_defaults.get("model"))
            if not model:
                raise ValueError(
                    f"Agent '{name}' must specify 'model' or inherit from settings.llm"
                )

            # Parse retry config
            retry_config = config.get("retry", {})

            agent = AgentConfig(
                name=name,
                model=model,
                system_prompt=config.get(
                    "system_prompt", self._llm_defaults.get("system_prompt")
                ),
                temperature=config.get(
                    "temperature", self._llm_defaults.get("temperature", 0.7)
                ),
                max_tokens=config.get(
                    "max_tokens", self._llm_defaults.get("max_tokens")
                ),
                tools=config.get("tools", []),
                timeout=config.get("timeout", self._llm_defaults.get("timeout", 60)),
                retry_max_attempts=retry_config.get("max_attempts", 3),
                retry_backoff=retry_config.get("backoff", 2.0),
            )

            # Validate temperature range
            if not 0.0 <= agent.temperature <= 2.0:
                raise ValueError(
                    f"Agent '{name}' temperature must be between 0.0 and 2.0, "
                    f"got {agent.temperature}"
                )

            self._agents[name] = agent
            logger.debug(f"Registered agent '{name}' with model '{model}'")

    def get(self, name: str) -> Optional[AgentConfig]:
        """Get agent configuration by name."""
        return self._agents.get(name)

    def get_or_raise(self, name: str) -> AgentConfig:
        """
        Get agent configuration by name, raising if not found.

        Args:
            name: Agent name

        Returns:
            AgentConfig for the agent

        Raises:
            KeyError: If agent not found
        """
        agent = self._agents.get(name)
        if agent is None:
            available = ", ".join(self._agents.keys()) or "(none)"
            raise KeyError(f"Agent '{name}' not found. Available agents: {available}")
        return agent

    def list_agents(self) -> List[str]:
        """List all registered agent names."""
        return list(self._agents.keys())

    def __contains__(self, name: str) -> bool:
        return name in self._agents

    def __len__(self) -> int:
        return len(self._agents)


# ============================================================================
# Template Processing Utilities
# ============================================================================


def process_template(template_str: str, state: Dict[str, Any]) -> str:
    """
    Process a Jinja2 template string with state context.

    Args:
        template_str: Template with {{ }} placeholders
        state: State dictionary for variable substitution

    Returns:
        Processed string
    """
    if not isinstance(template_str, str) or "{{" not in template_str:
        return template_str

    try:
        env = Environment(loader=BaseLoader(), undefined=StrictUndefined)
        template = env.from_string(template_str)
        return template.render(state=state, **state)
    except TemplateError as e:
        logger.warning(f"Template processing error: {e}")
        return template_str


# ============================================================================
# Aggregation Strategies (AC3)
# ============================================================================


def aggregate_collect(results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Collect aggregation: Returns list of all responses.

    Args:
        results: List of agent results

    Returns:
        {"responses": [...], "count": int}
    """
    responses = [r.get("content", r.get("response", r)) for r in results]
    return {"responses": responses, "count": len(responses), "aggregation": "collect"}


def aggregate_vote(results: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Vote aggregation: Returns majority response.

    For classification tasks. Uses deterministic tie-breaking (first encountered).

    Args:
        results: List of agent results

    Returns:
        {"result": str, "votes": Counter, "unanimous": bool}
    """
    responses = [
        str(r.get("content", r.get("response", "")))
        for r in results
        if r.get("success", True)
    ]

    if not responses:
        return {"result": None, "votes": {}, "unanimous": False, "aggregation": "vote"}

    counter = Counter(responses)
    # Get the most common response
    most_common = counter.most_common(1)[0]
    result = most_common[0]

    return {
        "result": result,
        "votes": dict(counter),
        "unanimous": len(counter) == 1,
        "total_votes": len(responses),
        "aggregation": "vote",
    }


def aggregate_first(
    results: List[Dict[str, Any]], successful_only: bool = True
) -> Dict[str, Any]:
    """
    First aggregation: Returns first (successful) response.

    Args:
        results: List of agent results
        successful_only: Only consider successful results

    Returns:
        First result or error if none
    """
    for result in results:
        if not successful_only or result.get("success", True):
            return {
                "result": result.get("content", result.get("response", result)),
                "agent": result.get("agent"),
                "aggregation": "first",
            }

    return {
        "result": None,
        "error": "No successful responses",
        "aggregation": "first",
    }


def aggregate_consensus(
    results: List[Dict[str, Any]], threshold: float = 0.5
) -> Dict[str, Any]:
    """
    Consensus aggregation: Returns result if agreement threshold met.

    Args:
        results: List of agent results
        threshold: Minimum agreement ratio (0.0 to 1.0)

    Returns:
        {"result": str, "agreement": float, "consensus_reached": bool}
    """
    responses = [
        str(r.get("content", r.get("response", "")))
        for r in results
        if r.get("success", True)
    ]

    if not responses:
        return {
            "result": None,
            "agreement": 0.0,
            "consensus_reached": False,
            "aggregation": "consensus",
        }

    counter = Counter(responses)
    most_common = counter.most_common(1)[0]
    result = most_common[0]
    count = most_common[1]
    agreement = count / len(responses)

    return {
        "result": result if agreement >= threshold else None,
        "agreement": agreement,
        "consensus_reached": agreement >= threshold,
        "threshold": threshold,
        "aggregation": "consensus",
    }


AGGREGATION_STRATEGIES = {
    "collect": aggregate_collect,
    "vote": aggregate_vote,
    "first": aggregate_first,
    "consensus": aggregate_consensus,
}


# ============================================================================
# Action Registration
# ============================================================================


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register multi-agent collaboration actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # Initialize agent registry from engine settings
    _agent_registry: Optional[AgentRegistry] = None

    def _get_agent_registry() -> AgentRegistry:
        """Lazy initialization of agent registry from engine settings."""
        nonlocal _agent_registry
        if _agent_registry is None:
            agents_config = {}
            llm_defaults = {}

            # Get settings from engine if available
            if hasattr(engine, "_config") and engine._config:
                settings = engine._config.get("settings", {})
                agents_config = settings.get("agents", {})

            if hasattr(engine, "llm_settings"):
                llm_defaults = engine.llm_settings

            _agent_registry = AgentRegistry(agents_config, llm_defaults)
        return _agent_registry

    def _call_llm(
        agent: AgentConfig,
        task: str,
        state: Dict[str, Any],
        extra_params: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Execute LLM call for an agent.

        Args:
            agent: Agent configuration
            task: Task prompt (already templated)
            state: Current state
            extra_params: Additional parameters to pass to llm.call

        Returns:
            LLM response dictionary
        """
        llm_call = registry.get("llm.call")
        if not llm_call:
            return {"error": "llm.call action not available", "success": False}

        params = agent.to_llm_params()
        params["messages"] = [{"role": "user", "content": task}]

        # Add system prompt as first message if defined
        if agent.system_prompt:
            params["messages"].insert(
                0, {"role": "system", "content": agent.system_prompt}
            )

        # Merge extra params
        if extra_params:
            params.update(extra_params)

        try:
            result = llm_call(state, **params)
            result["agent"] = agent.name
            result["success"] = True
            return result
        except Exception as e:
            logger.error(f"Agent '{agent.name}' LLM call failed: {e}")
            return {"error": str(e), "agent": agent.name, "success": False}

    # ========================================================================
    # agent.dispatch (AC2)
    # ========================================================================

    def agent_dispatch(
        state: Dict[str, Any],
        agent: str,
        task: str,
        timeout: Optional[int] = None,
        max_retries: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Dispatch a task to a single named agent.

        Args:
            state: Current workflow state
            agent: Name of agent to dispatch to (from settings.agents)
            task: Task description (supports Jinja2 templating)
            timeout: Override agent timeout (seconds)
            max_retries: Override agent retry count
            **kwargs: Additional parameters for LLM call

        Returns:
            {
                "response": str,
                "content": str,  # Alias for response
                "agent": str,
                "success": bool,
                "attempts": int,
                "elapsed_ms": float
            }
        """
        start_time = time.time()

        try:
            agent_registry = _get_agent_registry()
            agent_config = agent_registry.get_or_raise(agent)
        except KeyError as e:
            return {
                "error": str(e),
                "agent": agent,
                "success": False,
                "elapsed_ms": (time.time() - start_time) * 1000,
            }

        # Process task template
        processed_task = process_template(task, state)

        # Override timeout/retries if specified
        if timeout:
            agent_config = copy.copy(agent_config)
            agent_config.timeout = timeout
        if max_retries is not None:
            agent_config = copy.copy(agent_config)
            agent_config.retry_max_attempts = max_retries

        # Execute with retries
        attempts = 0
        last_error = None
        delay = 1.0

        while attempts < agent_config.retry_max_attempts:
            attempts += 1
            result = _call_llm(agent_config, processed_task, state, kwargs)

            if result.get("success", False):
                result["attempts"] = attempts
                result["elapsed_ms"] = (time.time() - start_time) * 1000
                result["response"] = result.get("content", "")
                return result

            last_error = result.get("error", "Unknown error")
            if attempts < agent_config.retry_max_attempts:
                time.sleep(delay)
                delay *= agent_config.retry_backoff

        return {
            "error": last_error,
            "agent": agent,
            "success": False,
            "attempts": attempts,
            "elapsed_ms": (time.time() - start_time) * 1000,
        }

    # ========================================================================
    # agent.parallel (AC3)
    # ========================================================================

    def agent_parallel(
        state: Dict[str, Any],
        agents: List[str],
        task: str,
        aggregation: str = "collect",
        max_concurrent: Optional[int] = None,
        timeout: Optional[int] = None,
        consensus_threshold: float = 0.5,
        consensus_max_rounds: int = 3,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Dispatch same task to multiple agents in parallel.

        Args:
            state: Current workflow state
            agents: List of agent names to dispatch to
            task: Task description (supports Jinja2 templating)
            aggregation: Aggregation strategy: collect, vote, consensus, first
            max_concurrent: Maximum concurrent agent calls
            timeout: Override agent timeout (seconds)
            consensus_threshold: Agreement threshold for consensus mode (0.0-1.0)
            consensus_max_rounds: Max retry rounds for consensus mode
            **kwargs: Additional parameters for LLM calls

        Returns:
            Aggregated result based on aggregation strategy
        """
        start_time = time.time()
        agent_registry = _get_agent_registry()

        # Validate agents
        valid_agents: List[AgentConfig] = []
        for agent_name in agents:
            try:
                valid_agents.append(agent_registry.get_or_raise(agent_name))
            except KeyError as e:
                logger.warning(str(e))

        if not valid_agents:
            return {
                "error": "No valid agents found",
                "agents_requested": agents,
                "success": False,
            }

        # Process task template
        processed_task = process_template(task, state)

        # Determine max workers
        max_workers = max_concurrent or len(valid_agents)

        def execute_agent(agent_config: AgentConfig) -> Dict[str, Any]:
            """Execute single agent call with state isolation."""
            # Deep copy state to prevent cross-agent mutation
            isolated_state = copy.deepcopy(state)
            config = agent_config
            if timeout:
                config = copy.copy(agent_config)
                config.timeout = timeout
            return _call_llm(config, processed_task, isolated_state, kwargs)

        # Execute in parallel
        results: List[Dict[str, Any]] = []
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures: Dict[Future, str] = {
                executor.submit(execute_agent, agent): agent.name
                for agent in valid_agents
            }

            for future in as_completed(futures):
                agent_name = futures[future]
                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    results.append(
                        {"error": str(e), "agent": agent_name, "success": False}
                    )

        # Handle consensus with retry logic
        if aggregation == "consensus":
            rounds = 1
            agg_result = aggregate_consensus(results, consensus_threshold)

            while not agg_result["consensus_reached"] and rounds < consensus_max_rounds:
                rounds += 1
                logger.info(
                    f"Consensus not reached (agreement={agg_result['agreement']:.2f}), "
                    f"retrying round {rounds}/{consensus_max_rounds}"
                )

                # Re-execute with new state containing previous results
                retry_state = copy.deepcopy(state)
                retry_state["previous_responses"] = results

                results = []
                with ThreadPoolExecutor(max_workers=max_workers) as executor:
                    futures = {
                        executor.submit(execute_agent, agent): agent.name
                        for agent in valid_agents
                    }
                    for future in as_completed(futures):
                        agent_name = futures[future]
                        try:
                            results.append(future.result())
                        except Exception as e:
                            results.append(
                                {"error": str(e), "agent": agent_name, "success": False}
                            )

                agg_result = aggregate_consensus(results, consensus_threshold)

            agg_result["rounds"] = rounds
            agg_result["elapsed_ms"] = (time.time() - start_time) * 1000
            agg_result["agents_called"] = [a.name for a in valid_agents]
            return agg_result

        # Apply aggregation strategy
        strategy = AGGREGATION_STRATEGIES.get(aggregation, aggregate_collect)
        agg_result = strategy(results)
        agg_result["elapsed_ms"] = (time.time() - start_time) * 1000
        agg_result["agents_called"] = [a.name for a in valid_agents]
        agg_result["raw_results"] = results

        return agg_result

    # ========================================================================
    # agent.sequential (AC4)
    # ========================================================================

    def agent_sequential(
        state: Dict[str, Any],
        agents: List[str],
        task: Optional[str] = None,
        tasks: Optional[List[str]] = None,
        transform: Optional[str] = None,
        early_exit_on_failure: bool = True,
        output_key: str = "sequential_result",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Chain multiple agents where output feeds into next agent's input.

        Args:
            state: Current workflow state
            agents: List of agent names to chain
            task: Single task template for all agents (uses {{ previous_response }})
            tasks: Per-agent task templates (must match agents length)
            transform: Optional transformation template applied between agents
            early_exit_on_failure: Stop chain on first failure
            output_key: Key to store final result in state
            **kwargs: Additional parameters for LLM calls

        Returns:
            {
                "result": final_response,
                "chain": [{"agent": str, "response": str}, ...],
                "success": bool
            }
        """
        start_time = time.time()
        agent_registry = _get_agent_registry()

        # Validate agents
        valid_agents: List[AgentConfig] = []
        for agent_name in agents:
            try:
                valid_agents.append(agent_registry.get_or_raise(agent_name))
            except KeyError as e:
                if early_exit_on_failure:
                    return {
                        "error": str(e),
                        "success": False,
                        "chain": [],
                    }
                logger.warning(str(e))

        if not valid_agents:
            return {"error": "No valid agents found", "success": False, "chain": []}

        # Validate tasks
        if tasks and len(tasks) != len(valid_agents):
            return {
                "error": f"Tasks count ({len(tasks)}) must match agents count ({len(valid_agents)})",
                "success": False,
                "chain": [],
            }

        # Execute chain
        chain: List[Dict[str, Any]] = []
        current_state = copy.deepcopy(state)
        previous_response = ""

        for i, agent_config in enumerate(valid_agents):
            # Update state with previous response
            current_state["previous_response"] = previous_response
            current_state["chain_index"] = i
            current_state["chain_length"] = len(valid_agents)

            # Determine task for this agent
            if tasks:
                current_task = tasks[i]
            elif task:
                current_task = task
            else:
                # Default: pass previous response
                current_task = (
                    "{{ previous_response }}" if i > 0 else "{{ state.input }}"
                )

            # Process task template
            processed_task = process_template(current_task, current_state)

            # Execute agent
            result = _call_llm(agent_config, processed_task, current_state, kwargs)

            chain_entry = {
                "agent": agent_config.name,
                "response": result.get("content", ""),
                "success": result.get("success", False),
            }
            chain.append(chain_entry)

            if not result.get("success", False):
                if early_exit_on_failure:
                    return {
                        "error": result.get("error", "Agent failed"),
                        "chain": chain,
                        "success": False,
                        "failed_at": i,
                        "elapsed_ms": (time.time() - start_time) * 1000,
                    }
                continue

            # Apply transformation if specified
            previous_response = result.get("content", "")
            if transform:
                current_state["current_response"] = previous_response
                previous_response = process_template(transform, current_state)

        return {
            "result": previous_response,
            output_key: previous_response,
            "chain": chain,
            "success": True,
            "agents_called": [a.name for a in valid_agents],
            "elapsed_ms": (time.time() - start_time) * 1000,
        }

    # ========================================================================
    # agent.coordinate (AC5)
    # ========================================================================

    def agent_coordinate(
        state: Dict[str, Any],
        leader: str,
        workers: List[str],
        task: str,
        validation_prompt: Optional[str] = None,
        max_rounds: int = 3,
        parallel_workers: bool = True,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Coordinator pattern with leader agent dispatching to workers.

        The leader:
        1. Analyzes the task and generates subtasks for workers
        2. Dispatches subtasks to worker agents
        3. Aggregates and validates worker results
        4. Re-dispatches on validation failure

        Args:
            state: Current workflow state
            leader: Name of leader/coordinator agent
            workers: List of worker agent names
            task: Main task description
            validation_prompt: Template for leader to validate results
            max_rounds: Maximum coordination rounds
            parallel_workers: Execute workers in parallel (default: True)
            **kwargs: Additional parameters for LLM calls

        Returns:
            {
                "result": final_result,
                "rounds": int,
                "worker_results": [...],
                "success": bool
            }
        """
        start_time = time.time()
        agent_registry = _get_agent_registry()

        # Validate agents
        try:
            leader_config = agent_registry.get_or_raise(leader)
        except KeyError as e:
            return {"error": str(e), "success": False}

        worker_configs: List[AgentConfig] = []
        for worker_name in workers:
            try:
                worker_configs.append(agent_registry.get_or_raise(worker_name))
            except KeyError as e:
                logger.warning(str(e))

        if not worker_configs:
            return {"error": "No valid workers found", "success": False}

        # Process task template
        processed_task = process_template(task, state)

        # Coordination loop
        rounds = 0
        final_result = None
        all_worker_results: List[List[Dict[str, Any]]] = []

        while rounds < max_rounds:
            rounds += 1
            logger.info(f"Coordination round {rounds}/{max_rounds}")

            # Step 1: Leader generates subtasks
            leader_prompt = f"""You are a coordinator. Break down this task into subtasks for {len(worker_configs)} workers.

Task: {processed_task}

Respond with a JSON array of subtasks, one for each worker:
["subtask 1", "subtask 2", ...]"""

            leader_result = _call_llm(leader_config, leader_prompt, state, kwargs)
            if not leader_result.get("success", False):
                continue

            # Parse subtasks from leader response
            try:
                import json

                content = leader_result.get("content", "")
                # Extract JSON array from response
                start = content.find("[")
                end = content.rfind("]") + 1
                if start >= 0 and end > start:
                    subtasks = json.loads(content[start:end])
                else:
                    subtasks = [processed_task] * len(worker_configs)
            except (json.JSONDecodeError, ValueError):
                subtasks = [processed_task] * len(worker_configs)

            # Ensure we have enough subtasks
            while len(subtasks) < len(worker_configs):
                subtasks.append(subtasks[-1] if subtasks else processed_task)

            # Step 2: Dispatch to workers
            worker_results: List[Dict[str, Any]] = []

            if parallel_workers:
                with ThreadPoolExecutor(max_workers=len(worker_configs)) as executor:
                    futures = {
                        executor.submit(
                            _call_llm,
                            worker,
                            subtasks[i],
                            copy.deepcopy(state),
                            kwargs,
                        ): worker.name
                        for i, worker in enumerate(worker_configs)
                    }
                    for future in as_completed(futures):
                        try:
                            worker_results.append(future.result())
                        except Exception as e:
                            worker_results.append({"error": str(e), "success": False})
            else:
                for i, worker in enumerate(worker_configs):
                    result = _call_llm(
                        worker, subtasks[i], copy.deepcopy(state), kwargs
                    )
                    worker_results.append(result)

            all_worker_results.append(worker_results)

            # Step 3: Leader aggregates and validates
            worker_responses = [
                r.get("content", "") for r in worker_results if r.get("success", False)
            ]

            if not worker_responses:
                logger.warning("No successful worker responses, retrying...")
                continue

            aggregation_prompt = f"""You are a coordinator. Aggregate these worker responses into a final answer.

Original task: {processed_task}

Worker responses:
{chr(10).join(f'- Worker {i+1}: {r}' for i, r in enumerate(worker_responses))}

Provide the final aggregated result."""

            if validation_prompt:
                aggregation_prompt += (
                    f"\n\n{process_template(validation_prompt, state)}"
                )

            final_response = _call_llm(leader_config, aggregation_prompt, state, kwargs)

            if final_response.get("success", False):
                final_result = final_response.get("content", "")
                break

        return {
            "result": final_result,
            "rounds": rounds,
            "worker_results": all_worker_results,
            "success": final_result is not None,
            "leader": leader,
            "workers": [w.name for w in worker_configs],
            "elapsed_ms": (time.time() - start_time) * 1000,
        }

    # ========================================================================
    # agent.crewai_delegate (AC9)
    # ========================================================================

    def agent_crewai_delegate(
        state: Dict[str, Any],
        agents: List[str],
        task: str,
        process: str = "sequential",
        backend: str = "native",
        fallback_to_native: bool = True,
        verbose: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Delegate to CrewAI for complex multi-agent workflows.

        Args:
            state: Current workflow state
            agents: List of agent names to use
            task: Task description
            process: CrewAI process type: sequential, hierarchical
            backend: Execution backend: crewai, native
            fallback_to_native: Use native TEA if CrewAI unavailable
            verbose: Enable verbose logging
            **kwargs: Additional CrewAI parameters

        Returns:
            {
                "result": str,
                "backend_used": str,
                "success": bool
            }
        """
        start_time = time.time()

        # Try to use CrewAI backend
        if backend == "crewai":
            try:
                from crewai import Agent, Task, Crew, Process

                agent_registry = _get_agent_registry()

                # Build CrewAI agents from our registry
                crewai_agents = []
                for agent_name in agents:
                    try:
                        config = agent_registry.get_or_raise(agent_name)
                        crewai_agent = Agent(
                            role=agent_name,
                            goal=config.system_prompt
                            or f"Complete tasks as {agent_name}",
                            backstory=f"You are {agent_name}",
                            verbose=verbose,
                        )
                        crewai_agents.append(crewai_agent)
                    except KeyError:
                        logger.warning(f"Agent '{agent_name}' not found, skipping")

                if not crewai_agents:
                    raise ValueError("No valid agents for CrewAI")

                # Create task
                processed_task = process_template(task, state)
                crewai_task = Task(
                    description=processed_task,
                    agent=crewai_agents[0],  # Primary agent
                )

                # Select process type
                process_type = (
                    Process.hierarchical
                    if process == "hierarchical"
                    else Process.sequential
                )

                # Run crew
                crew = Crew(
                    agents=crewai_agents,
                    tasks=[crewai_task],
                    process=process_type,
                    verbose=verbose,
                )

                result = crew.kickoff()

                return {
                    "result": str(result),
                    "backend_used": "crewai",
                    "process": process,
                    "success": True,
                    "elapsed_ms": (time.time() - start_time) * 1000,
                }

            except ImportError:
                if not fallback_to_native:
                    return {
                        "error": "CrewAI not installed. Install with: pip install crewai",
                        "backend_used": "none",
                        "success": False,
                    }
                logger.info("CrewAI not available, falling back to native TEA")
            except Exception as e:
                if not fallback_to_native:
                    return {
                        "error": str(e),
                        "backend_used": "crewai",
                        "success": False,
                    }
                logger.warning(f"CrewAI execution failed: {e}, falling back to native")

        # Native TEA execution (fallback or explicit)
        if process == "hierarchical":
            # Use coordinate pattern for hierarchical
            if len(agents) < 2:
                return {
                    "error": "Hierarchical process requires at least 2 agents (leader + workers)",
                    "success": False,
                }
            return agent_coordinate(
                state=state,
                leader=agents[0],
                workers=agents[1:],
                task=task,
                **kwargs,
            )
        else:
            # Use sequential pattern
            return agent_sequential(
                state=state,
                agents=agents,
                task=task,
                **kwargs,
            )

    # ========================================================================
    # Register all actions
    # ========================================================================

    registry["agent.dispatch"] = agent_dispatch
    registry["agent.parallel"] = agent_parallel
    registry["agent.sequential"] = agent_sequential
    registry["agent.coordinate"] = agent_coordinate
    registry["agent.crewai_delegate"] = agent_crewai_delegate

    # Also register under actions.* namespace for compatibility
    registry["actions.agent_dispatch"] = agent_dispatch
    registry["actions.agent_parallel"] = agent_parallel
    registry["actions.agent_sequential"] = agent_sequential
    registry["actions.agent_coordinate"] = agent_coordinate
    registry["actions.agent_crewai_delegate"] = agent_crewai_delegate

    logger.debug("Registered multi-agent collaboration actions")
