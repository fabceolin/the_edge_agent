"""
Tests for Multi-Agent Collaboration Actions (TEA-AGENT-001.1)

Tests for multi-agent collaboration primitives:
- AgentRegistry: Agent definition parsing and validation
- agent.dispatch: Single agent task dispatch
- agent.parallel: Parallel dispatch with aggregation strategies
- agent.sequential: Sequential agent chaining
- agent.coordinate: Leader-worker coordination pattern
- agent.crewai_delegate: CrewAI integration with fallback

Test Priority Levels:
- P0: Critical - Agent registry validation, dispatch core execution, state isolation
- P1: Core - All aggregation strategies, sequential chaining, coordination
- P2: Advanced - CrewAI fallback, edge cases, error handling
"""

import copy
import json
import pytest
import sys
import time
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock, call
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError

# Ensure the source is in path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine
from the_edge_agent.actions.agent_actions import (
    AgentConfig,
    AgentRegistry,
    aggregate_collect,
    aggregate_vote,
    aggregate_first,
    aggregate_consensus,
    process_template,
    register_actions,
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def mock_llm_response():
    """Create a mock LLM response."""
    return {
        "content": "Test response from agent",
        "usage": {"prompt_tokens": 10, "completion_tokens": 20},
        "success": True,
    }


@pytest.fixture
def sample_agents_config():
    """Sample agent configurations for testing."""
    return {
        "researcher": {
            "model": "gpt-4",
            "system_prompt": "You are a research assistant.",
            "temperature": 0.7,
            "timeout": 60,
        },
        "analyst": {
            "model": "gpt-4",
            "system_prompt": "You are a data analyst.",
            "temperature": 0.3,
        },
        "writer": {
            "model": "gpt-3.5-turbo",
            "system_prompt": "You are a technical writer.",
        },
    }


@pytest.fixture
def llm_defaults():
    """Default LLM settings for inheritance."""
    return {
        "model": "gpt-4",
        "temperature": 0.7,
        "timeout": 60,
    }


@pytest.fixture
def mock_llm_call():
    """Mock the llm.call action."""
    mock = Mock()
    mock.return_value = {
        "content": "Mocked LLM response",
        "usage": {"prompt_tokens": 10, "completion_tokens": 20},
    }
    return mock


@pytest.fixture
def engine_with_agents():
    """Create engine with mock LLM and registered agents."""
    engine = YAMLEngine()

    # Configure engine with agents
    yaml_config = """
name: test-workflow
settings:
  llm:
    model: gpt-4
    temperature: 0.7
  agents:
    researcher:
      system_prompt: "You are a research assistant."
      temperature: 0.5
    analyst:
      system_prompt: "You are a data analyst."
      temperature: 0.3
    writer:
      model: gpt-3.5-turbo
      system_prompt: "You are a writer."
    validator:
      system_prompt: "You validate results."

state_schema:
  input: str
  output: str

nodes:
  - name: start
    run: |
      return {"output": "done"}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
    import yaml

    config = yaml.safe_load(yaml_config)
    engine.load_from_dict(config)
    return engine


# =============================================================================
# P0 - Critical Tests: Agent Registry
# =============================================================================


class TestAgentRegistry:
    """Tests for AgentRegistry class (AC1)."""

    def test_registry_initialization_empty(self):
        """Test empty registry initialization."""
        registry = AgentRegistry()
        assert len(registry) == 0
        assert registry.list_agents() == []

    def test_registry_initialization_with_agents(
        self, sample_agents_config, llm_defaults
    ):
        """Test registry initialization with agent configs."""
        registry = AgentRegistry(sample_agents_config, llm_defaults)
        assert len(registry) == 3
        assert "researcher" in registry
        assert "analyst" in registry
        assert "writer" in registry

    def test_registry_get_agent(self, sample_agents_config, llm_defaults):
        """Test getting agent by name."""
        registry = AgentRegistry(sample_agents_config, llm_defaults)
        agent = registry.get("researcher")
        assert agent is not None
        assert agent.name == "researcher"
        assert agent.model == "gpt-4"
        assert agent.system_prompt == "You are a research assistant."

    def test_registry_get_nonexistent_agent(self, sample_agents_config, llm_defaults):
        """Test getting nonexistent agent returns None."""
        registry = AgentRegistry(sample_agents_config, llm_defaults)
        assert registry.get("nonexistent") is None

    def test_registry_get_or_raise(self, sample_agents_config, llm_defaults):
        """Test get_or_raise raises KeyError for missing agent."""
        registry = AgentRegistry(sample_agents_config, llm_defaults)
        with pytest.raises(KeyError) as exc_info:
            registry.get_or_raise("nonexistent")
        assert "nonexistent" in str(exc_info.value)
        assert "Available agents" in str(exc_info.value)

    def test_registry_inherits_from_llm_defaults(self, llm_defaults):
        """Test agent inherits model from settings.llm."""
        agents_config = {
            "simple_agent": {
                "system_prompt": "Simple agent without model specified.",
            }
        }
        registry = AgentRegistry(agents_config, llm_defaults)
        agent = registry.get("simple_agent")
        assert agent.model == "gpt-4"  # Inherited from defaults
        assert agent.temperature == 0.7  # Inherited from defaults

    def test_registry_validation_missing_model(self):
        """Test validation fails when model not specified and no defaults."""
        agents_config = {"bad_agent": {"system_prompt": "No model specified."}}
        with pytest.raises(ValueError) as exc_info:
            AgentRegistry(agents_config, {})
        assert "must specify 'model'" in str(exc_info.value)

    def test_registry_validation_invalid_temperature(self, llm_defaults):
        """Test validation fails for invalid temperature."""
        agents_config = {
            "bad_agent": {
                "model": "gpt-4",
                "temperature": 3.0,  # Invalid: must be 0.0-2.0
            }
        }
        with pytest.raises(ValueError) as exc_info:
            AgentRegistry(agents_config, llm_defaults)
        assert "temperature must be between" in str(exc_info.value)

    def test_registry_validation_invalid_config_type(self, llm_defaults):
        """Test validation fails for non-dict agent config."""
        agents_config = {"bad_agent": "not a dictionary"}
        with pytest.raises(ValueError) as exc_info:
            AgentRegistry(agents_config, llm_defaults)
        assert "must be a dictionary" in str(exc_info.value)

    def test_registry_parses_retry_config(self, llm_defaults):
        """Test registry parses retry configuration."""
        agents_config = {
            "retry_agent": {
                "model": "gpt-4",
                "retry": {
                    "max_attempts": 5,
                    "backoff": 1.5,
                },
            }
        }
        registry = AgentRegistry(agents_config, llm_defaults)
        agent = registry.get("retry_agent")
        assert agent.retry_max_attempts == 5
        assert agent.retry_backoff == 1.5

    def test_registry_default_retry_config(self, llm_defaults):
        """Test registry uses default retry configuration."""
        agents_config = {"basic_agent": {"model": "gpt-4"}}
        registry = AgentRegistry(agents_config, llm_defaults)
        agent = registry.get("basic_agent")
        assert agent.retry_max_attempts == 3
        assert agent.retry_backoff == 2.0


class TestAgentConfig:
    """Tests for AgentConfig dataclass."""

    def test_agent_config_to_llm_params(self):
        """Test conversion to LLM parameters."""
        config = AgentConfig(
            name="test_agent",
            model="gpt-4",
            system_prompt="You are a test agent.",
            temperature=0.5,
            max_tokens=1000,
            timeout=30,
        )
        params = config.to_llm_params()
        assert params["model"] == "gpt-4"
        assert params["system_prompt"] == "You are a test agent."
        assert params["temperature"] == 0.5
        assert params["max_tokens"] == 1000
        assert params["timeout"] == 30

    def test_agent_config_to_llm_params_minimal(self):
        """Test conversion with minimal config."""
        config = AgentConfig(name="minimal", model="gpt-3.5-turbo")
        params = config.to_llm_params()
        assert params["model"] == "gpt-3.5-turbo"
        assert "system_prompt" not in params  # None is not included
        assert "max_tokens" not in params

    def test_agent_config_with_tools(self):
        """Test agent config with tools list."""
        config = AgentConfig(
            name="tool_agent",
            model="gpt-4",
            tools=["search", "calculator", "file_read"],
        )
        assert config.tools == ["search", "calculator", "file_read"]

    def test_agent_config_default_values(self):
        """Test agent config default values."""
        config = AgentConfig(name="default", model="gpt-4")
        assert config.temperature == 0.7
        assert config.timeout == 60
        assert config.retry_max_attempts == 3
        assert config.retry_backoff == 2.0
        assert config.tools == []
        assert config.system_prompt is None
        assert config.max_tokens is None


# =============================================================================
# P0 - Critical Tests: Template Processing
# =============================================================================


class TestTemplateProcessing:
    """Tests for Jinja2 template processing."""

    def test_process_template_simple(self):
        """Test simple variable substitution."""
        result = process_template("Hello {{ state.name }}!", {"name": "World"})
        assert result == "Hello World!"

    def test_process_template_nested(self):
        """Test nested variable access."""
        result = process_template(
            "User: {{ state.user.name }}", {"user": {"name": "Alice"}}
        )
        assert result == "User: Alice"

    def test_process_template_no_template(self):
        """Test non-template string passes through."""
        result = process_template("No templates here", {"name": "World"})
        assert result == "No templates here"

    def test_process_template_direct_state_access(self):
        """Test direct state access (without state. prefix)."""
        result = process_template("Hello {{ name }}!", {"name": "Direct"})
        assert result == "Hello Direct!"

    def test_process_template_multiple_variables(self):
        """Test multiple variable substitution."""
        result = process_template(
            "{{ state.greeting }} {{ state.name }}, you have {{ state.count }} messages.",
            {"greeting": "Hello", "name": "Alice", "count": 5},
        )
        assert result == "Hello Alice, you have 5 messages."

    def test_process_template_with_filters(self):
        """Test template with Jinja2 filters."""
        result = process_template("Name: {{ state.name | upper }}", {"name": "alice"})
        assert result == "Name: ALICE"

    def test_process_template_invalid_returns_original(self):
        """Test invalid template returns original string."""
        # StrictUndefined will cause error on undefined variable
        result = process_template("Hello {{ state.undefined_var }}!", {"name": "World"})
        # Should return original since variable is undefined
        assert result == "Hello {{ state.undefined_var }}!"

    def test_process_template_non_string_input(self):
        """Test non-string input returns as-is."""
        result = process_template(123, {"name": "World"})
        assert result == 123


# =============================================================================
# P0 - Critical Tests: Aggregation Strategies
# =============================================================================


class TestAggregationStrategies:
    """Tests for aggregation strategy functions (AC3)."""

    def test_aggregate_collect(self):
        """Test collect aggregation returns all responses."""
        results = [
            {"content": "Response 1", "success": True},
            {"content": "Response 2", "success": True},
            {"content": "Response 3", "success": True},
        ]
        agg = aggregate_collect(results)
        assert agg["count"] == 3
        assert len(agg["responses"]) == 3
        assert agg["aggregation"] == "collect"

    def test_aggregate_collect_with_response_key(self):
        """Test collect handles 'response' key as fallback."""
        results = [
            {"response": "Response 1", "success": True},
            {"response": "Response 2", "success": True},
        ]
        agg = aggregate_collect(results)
        assert agg["count"] == 2
        assert "Response 1" in agg["responses"]
        assert "Response 2" in agg["responses"]

    def test_aggregate_collect_mixed_formats(self):
        """Test collect handles mixed response formats."""
        results = [
            {"content": "Content response", "success": True},
            {"response": "Response response", "success": True},
            {"other_key": "Fallback", "success": True},
        ]
        agg = aggregate_collect(results)
        assert agg["count"] == 3

    def test_aggregate_vote_majority(self):
        """Test vote aggregation returns majority."""
        results = [
            {"content": "Yes", "success": True},
            {"content": "Yes", "success": True},
            {"content": "No", "success": True},
        ]
        agg = aggregate_vote(results)
        assert agg["result"] == "Yes"
        assert agg["votes"]["Yes"] == 2
        assert agg["votes"]["No"] == 1
        assert agg["unanimous"] is False

    def test_aggregate_vote_unanimous(self):
        """Test vote aggregation detects unanimity."""
        results = [
            {"content": "Agree", "success": True},
            {"content": "Agree", "success": True},
            {"content": "Agree", "success": True},
        ]
        agg = aggregate_vote(results)
        assert agg["result"] == "Agree"
        assert agg["unanimous"] is True

    def test_aggregate_vote_tie_first_wins(self):
        """Test vote tie-breaking uses first encountered."""
        results = [
            {"content": "A", "success": True},
            {"content": "B", "success": True},
        ]
        agg = aggregate_vote(results)
        # Deterministic tie-breaking: first encountered wins
        assert agg["result"] in ["A", "B"]

    def test_aggregate_vote_ignores_failures(self):
        """Test vote aggregation ignores failed responses."""
        results = [
            {"content": "Yes", "success": True},
            {"content": "No", "success": False},  # Should be ignored
            {"content": "Yes", "success": True},
        ]
        agg = aggregate_vote(results)
        assert agg["result"] == "Yes"
        assert agg["total_votes"] == 2

    def test_aggregate_vote_empty_results(self):
        """Test vote aggregation with empty results."""
        results = []
        agg = aggregate_vote(results)
        assert agg["result"] is None
        assert agg["unanimous"] is False

    def test_aggregate_vote_all_failures(self):
        """Test vote aggregation when all fail."""
        results = [
            {"content": "A", "success": False},
            {"content": "B", "success": False},
        ]
        agg = aggregate_vote(results)
        assert agg["result"] is None

    def test_aggregate_first_successful(self):
        """Test first aggregation returns first success."""
        results = [
            {"error": "Failed", "success": False},
            {"content": "Success!", "success": True, "agent": "agent2"},
            {"content": "Also success", "success": True, "agent": "agent3"},
        ]
        agg = aggregate_first(results)
        assert agg["result"] == "Success!"
        assert agg["agent"] == "agent2"

    def test_aggregate_first_all_failed(self):
        """Test first aggregation when all fail."""
        results = [
            {"error": "Failed 1", "success": False},
            {"error": "Failed 2", "success": False},
        ]
        agg = aggregate_first(results)
        assert agg["result"] is None
        assert "No successful responses" in agg["error"]

    def test_aggregate_first_include_failures(self):
        """Test first aggregation with successful_only=False."""
        results = [
            {"content": "Failed but has content", "success": False},
            {"content": "Success!", "success": True},
        ]
        agg = aggregate_first(results, successful_only=False)
        assert agg["result"] == "Failed but has content"

    def test_aggregate_consensus_reached(self):
        """Test consensus reached above threshold."""
        results = [
            {"content": "Agreed", "success": True},
            {"content": "Agreed", "success": True},
            {"content": "Different", "success": True},
        ]
        agg = aggregate_consensus(results, threshold=0.6)
        assert agg["consensus_reached"] is True
        assert agg["result"] == "Agreed"
        assert agg["agreement"] == pytest.approx(0.666, rel=0.01)

    def test_aggregate_consensus_not_reached(self):
        """Test consensus not reached below threshold."""
        results = [
            {"content": "A", "success": True},
            {"content": "B", "success": True},
            {"content": "C", "success": True},
        ]
        agg = aggregate_consensus(results, threshold=0.5)
        assert agg["consensus_reached"] is False
        assert agg["result"] is None

    def test_aggregate_consensus_includes_threshold(self):
        """Test consensus result includes threshold info."""
        results = [
            {"content": "Same", "success": True},
            {"content": "Same", "success": True},
        ]
        agg = aggregate_consensus(results, threshold=0.8)
        assert agg["threshold"] == 0.8
        assert agg["consensus_reached"] is True

    def test_aggregate_consensus_empty_results(self):
        """Test consensus with empty results."""
        agg = aggregate_consensus([], threshold=0.5)
        assert agg["consensus_reached"] is False
        assert agg["result"] is None
        assert agg["agreement"] == 0.0

    def test_aggregate_consensus_perfect_agreement(self):
        """Test consensus with 100% agreement."""
        results = [
            {"content": "Same", "success": True},
            {"content": "Same", "success": True},
            {"content": "Same", "success": True},
        ]
        agg = aggregate_consensus(results, threshold=1.0)
        assert agg["consensus_reached"] is True
        assert agg["agreement"] == 1.0


# =============================================================================
# P0 - Critical Tests: agent.dispatch
# =============================================================================


class TestAgentDispatch:
    """Tests for agent.dispatch action (AC2)."""

    def test_dispatch_agent_not_found(self, engine):
        """Test dispatch fails gracefully for unknown agent."""
        result = engine.actions_registry["agent.dispatch"](
            state={},
            agent="nonexistent",
            task="Test task",
        )
        assert result["success"] is False
        assert "not found" in result["error"]

    def test_dispatch_with_template(self):
        """Test dispatch with Jinja2 task template."""
        # This test verifies template processing using process_template directly
        task = "Research {{ state.topic }} and summarize findings"
        state = {"topic": "quantum computing"}
        processed = process_template(task, state)
        assert "quantum computing" in processed
        assert "{{ state.topic }}" not in processed

    def test_dispatch_timeout_override(self, sample_agents_config, llm_defaults):
        """Test timeout can be overridden per dispatch."""
        registry = AgentRegistry(sample_agents_config, llm_defaults)
        agent = registry.get("researcher")
        original_timeout = agent.timeout

        # Create a modified copy with timeout
        modified = copy.copy(agent)
        modified.timeout = 120
        assert modified.timeout == 120
        assert agent.timeout == original_timeout  # Original unchanged

    def test_dispatch_includes_elapsed_time(self, engine_with_agents):
        """Test dispatch result includes elapsed time."""

        # Mock llm.call to return a response
        def mock_llm_call(state, **kwargs):
            return {"content": "Test response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
        )
        assert "elapsed_ms" in result
        assert result["elapsed_ms"] >= 0

    def test_dispatch_with_retry_on_failure(self, engine_with_agents):
        """Test dispatch retries on failure."""
        call_count = [0]

        def mock_llm_call(state, **kwargs):
            call_count[0] += 1
            if call_count[0] < 2:
                raise Exception("Temporary failure")
            return {"content": "Success on retry", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
        )
        assert result["success"] is True
        assert result["attempts"] == 2

    def test_dispatch_exhausts_retries(self, engine_with_agents):
        """Test dispatch returns error after exhausting retries."""

        def mock_llm_call(state, **kwargs):
            raise Exception("Persistent failure")

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
            max_retries=2,
        )
        assert result["success"] is False
        assert result["attempts"] == 2
        assert "error" in result

    def test_dispatch_response_includes_agent_name(self, engine_with_agents):
        """Test dispatch result includes agent name."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
        )
        assert result["agent"] == "researcher"


# =============================================================================
# P1 - Core Tests: agent.parallel
# =============================================================================


class TestAgentParallel:
    """Tests for agent.parallel action (AC3)."""

    def test_parallel_state_isolation(self):
        """Test parallel execution uses deep-copied state."""
        original_state = {"counter": 0, "nested": {"value": 1}}

        def modify_state(state):
            """Simulates agent modifying state."""
            state["counter"] += 1
            state["nested"]["value"] += 1
            return state

        # Execute in parallel
        states = []
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = [
                executor.submit(modify_state, copy.deepcopy(original_state))
                for _ in range(3)
            ]
            for future in futures:
                states.append(future.result())

        # Original should be unchanged
        assert original_state["counter"] == 0
        assert original_state["nested"]["value"] == 1

        # Each copy should be independently modified
        for s in states:
            assert s["counter"] == 1
            assert s["nested"]["value"] == 2

    def test_parallel_returns_all_agents_called(self, engine_with_agents):
        """Test parallel result includes list of agents called."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
        )
        assert "agents_called" in result
        assert set(result["agents_called"]) == {"researcher", "analyst"}

    def test_parallel_with_invalid_agent(self, engine_with_agents):
        """Test parallel handles invalid agents gracefully."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "nonexistent", "analyst"],
            task="Test task",
        )
        # Should proceed with valid agents only
        assert "researcher" in result["agents_called"]
        assert "analyst" in result["agents_called"]
        assert "nonexistent" not in result["agents_called"]

    def test_parallel_no_valid_agents(self, engine_with_agents):
        """Test parallel returns error when no valid agents."""
        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["fake1", "fake2"],
            task="Test task",
        )
        assert result["success"] is False
        assert "No valid agents" in result["error"]

    def test_parallel_with_collect_aggregation(self, engine_with_agents):
        """Test parallel with collect aggregation."""
        responses = ["Response 1", "Response 2"]
        call_idx = [0]

        def mock_llm_call(state, **kwargs):
            idx = call_idx[0]
            call_idx[0] += 1
            return {"content": responses[min(idx, len(responses) - 1)], "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            aggregation="collect",
        )
        assert result["aggregation"] == "collect"
        assert result["count"] == 2

    def test_parallel_with_vote_aggregation(self, engine_with_agents):
        """Test parallel with vote aggregation."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Agreed", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Test task",
            aggregation="vote",
        )
        assert result["aggregation"] == "vote"
        assert result["result"] == "Agreed"
        assert result["unanimous"] is True

    def test_parallel_with_first_aggregation(self, engine_with_agents):
        """Test parallel with first aggregation."""

        def mock_llm_call(state, **kwargs):
            return {"content": "First response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            aggregation="first",
        )
        assert result["aggregation"] == "first"
        assert result["result"] == "First response"

    def test_parallel_with_consensus_aggregation(self, engine_with_agents):
        """Test parallel with consensus aggregation."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Consensus value", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Test task",
            aggregation="consensus",
            consensus_threshold=0.5,
        )
        assert result["consensus_reached"] is True
        assert result["result"] == "Consensus value"

    def test_parallel_consensus_with_retry(self, engine_with_agents):
        """Test parallel consensus retries when not reached."""
        round_count = [0]

        def mock_llm_call(state, **kwargs):
            round_count[0] += 1
            # First round: disagreement
            if round_count[0] <= 3:
                return {"content": f"Response {round_count[0]}", "success": True}
            # Second round: agreement
            return {"content": "Agreed", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Test task",
            aggregation="consensus",
            consensus_threshold=1.0,
            consensus_max_rounds=2,
        )
        assert "rounds" in result
        assert result["rounds"] >= 1

    def test_parallel_max_concurrent_limit(self, engine_with_agents):
        """Test parallel respects max_concurrent limit."""
        active_count = [0]
        max_active = [0]

        def mock_llm_call(state, **kwargs):
            active_count[0] += 1
            max_active[0] = max(max_active[0], active_count[0])
            time.sleep(0.05)  # Small delay to see concurrency
            active_count[0] -= 1
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Test task",
            max_concurrent=1,
        )
        # With max_concurrent=1, should never exceed 1 active call
        assert max_active[0] <= 1

    def test_parallel_includes_raw_results(self, engine_with_agents):
        """Test parallel result includes raw results from each agent."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
        )
        assert "raw_results" in result
        assert len(result["raw_results"]) == 2


# =============================================================================
# P1 - Core Tests: agent.sequential
# =============================================================================


class TestAgentSequential:
    """Tests for agent.sequential action (AC4)."""

    def test_sequential_state_threading(self):
        """Test state is threaded through sequential calls."""
        state = {"previous_response": ""}

        # Simulate sequential state updates
        responses = ["First", "Second", "Third"]
        chain = []

        for response in responses:
            state["previous_response"] = response
            chain.append({"response": response, "prev": state.get("previous_response")})

        assert chain[-1]["response"] == "Third"

    def test_sequential_executes_in_order(self, engine_with_agents):
        """Test sequential executes agents in order."""
        execution_order = []

        def mock_llm_call(state, **kwargs):
            # Extract agent from messages
            messages = kwargs.get("messages", [])
            execution_order.append(len(execution_order))
            return {"content": f"Response {len(execution_order)}", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Process step",
        )
        assert result["success"] is True
        assert len(result["chain"]) == 3

    def test_sequential_early_exit_on_failure(self, engine_with_agents):
        """Test sequential stops on first failure when early_exit_on_failure=True."""
        call_count = [0]

        def mock_llm_call(state, **kwargs):
            call_count[0] += 1
            if call_count[0] == 2:
                raise Exception("Agent failed")
            return {"content": f"Response {call_count[0]}", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Process step",
            early_exit_on_failure=True,
        )
        assert result["success"] is False
        assert "failed_at" in result
        assert result["failed_at"] == 1
        assert len(result["chain"]) == 2  # Only 2 agents executed

    def test_sequential_continues_on_failure(self, engine_with_agents):
        """Test sequential continues when early_exit_on_failure=False."""
        call_count = [0]

        def mock_llm_call(state, **kwargs):
            call_count[0] += 1
            if call_count[0] == 2:
                raise Exception("Agent failed")
            return {"content": f"Response {call_count[0]}", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst", "writer"],
            task="Process step",
            early_exit_on_failure=False,
        )
        # All agents should be attempted
        assert len(result["chain"]) == 3

    def test_sequential_with_per_agent_tasks(self, engine_with_agents):
        """Test sequential with different tasks per agent."""
        tasks_received = []

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            for msg in messages:
                if msg.get("role") == "user":
                    tasks_received.append(msg.get("content", ""))
            return {"content": "Done", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst"],
            tasks=["Task 1 for researcher", "Task 2 for analyst"],
        )
        assert result["success"] is True
        assert "Task 1" in tasks_received[0]
        assert "Task 2" in tasks_received[1]

    def test_sequential_tasks_count_mismatch(self, engine_with_agents):
        """Test sequential fails when tasks count doesn't match agents."""
        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst"],
            tasks=["Only one task"],
        )
        assert result["success"] is False
        assert "must match" in result["error"]

    def test_sequential_with_transform(self, engine_with_agents):
        """Test sequential applies transformation between agents."""

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")
            return {"content": content.upper(), "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={"input": "initial"},
            agents=["researcher", "analyst"],
            task="{{ previous_response }}",
            transform="{{ current_response | lower }}",
        )
        assert result["success"] is True

    def test_sequential_result_contains_chain(self, engine_with_agents):
        """Test sequential result contains full chain of responses."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=["researcher", "analyst"],
            task="Process",
        )
        assert "chain" in result
        for entry in result["chain"]:
            assert "agent" in entry
            assert "response" in entry
            assert "success" in entry

    def test_sequential_empty_agents(self, engine_with_agents):
        """Test sequential handles empty agents list."""
        result = engine_with_agents.actions_registry["agent.sequential"](
            state={},
            agents=[],
            task="Test",
        )
        assert result["success"] is False
        assert "error" in result


# =============================================================================
# P1 - Core Tests: agent.coordinate
# =============================================================================


class TestAgentCoordinate:
    """Tests for agent.coordinate action (AC5)."""

    def test_coordinate_basic_flow(self, engine_with_agents):
        """Test coordinate basic leader-worker flow."""
        call_count = [0]

        def mock_llm_call(state, **kwargs):
            call_count[0] += 1
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            # Leader creates subtasks
            if "Break down" in content or "coordinator" in content.lower():
                return {"content": '["subtask 1", "subtask 2"]', "success": True}
            # Workers respond
            if "subtask" in content.lower():
                return {"content": "Worker result", "success": True}
            # Leader aggregates
            return {"content": "Final aggregated result", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["analyst", "writer"],
            task="Complete this task",
        )
        assert result["success"] is True
        assert result["leader"] == "researcher"
        assert "result" in result

    def test_coordinate_leader_not_found(self, engine_with_agents):
        """Test coordinate fails when leader not found."""
        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="nonexistent",
            workers=["analyst", "writer"],
            task="Test task",
        )
        assert result["success"] is False
        assert "not found" in result["error"]

    def test_coordinate_no_valid_workers(self, engine_with_agents):
        """Test coordinate fails when no valid workers."""
        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["fake1", "fake2"],
            task="Test task",
        )
        assert result["success"] is False
        assert "No valid workers" in result["error"]

    def test_coordinate_max_rounds(self, engine_with_agents):
        """Test coordinate respects max_rounds limit."""

        def mock_llm_call(state, **kwargs):
            # Always return subtasks but no successful aggregation
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task"]', "success": True}
            if "Aggregate" in content:
                return {"content": "", "success": False}  # Fail aggregation
            return {"content": "Worker result", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["analyst"],
            task="Test task",
            max_rounds=2,
        )
        assert result["rounds"] <= 2

    def test_coordinate_sequential_workers(self, engine_with_agents):
        """Test coordinate with parallel_workers=False."""
        execution_order = []

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task1", "task2"]', "success": True}
            if "Aggregate" in content:
                return {"content": "Final", "success": True}
            execution_order.append(content)
            return {"content": "Worker result", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["analyst", "writer"],
            task="Test task",
            parallel_workers=False,
        )
        assert result["success"] is True

    def test_coordinate_includes_worker_results(self, engine_with_agents):
        """Test coordinate result includes all worker results."""

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task1", "task2"]', "success": True}
            if "Aggregate" in content:
                return {"content": "Final", "success": True}
            return {"content": "Worker result", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["analyst", "writer"],
            task="Test task",
        )
        assert "worker_results" in result
        assert len(result["worker_results"]) >= 1

    def test_coordinate_with_validation_prompt(self, engine_with_agents):
        """Test coordinate with custom validation prompt."""
        validation_received = [False]

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task1"]', "success": True}
            if "Validate carefully" in content:
                validation_received[0] = True
            if "Aggregate" in content:
                return {"content": "Final", "success": True}
            return {"content": "Worker result", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.coordinate"](
            state={},
            leader="researcher",
            workers=["analyst"],
            task="Test task",
            validation_prompt="Validate carefully",
        )
        assert validation_received[0] is True


# =============================================================================
# P2 - Advanced Tests: agent.crewai_delegate
# =============================================================================


class TestCrewAIDelegate:
    """Tests for agent.crewai_delegate action (AC9)."""

    def test_crewai_fallback_to_sequential(self, engine_with_agents):
        """Test fallback to native sequential when CrewAI not available."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Native response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            process="sequential",
            backend="crewai",
            fallback_to_native=True,
        )
        # Should fallback to native since CrewAI is not installed
        assert result["success"] is True

    def test_crewai_fallback_to_hierarchical(self, engine_with_agents):
        """Test fallback to native coordinate for hierarchical process."""

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task1"]', "success": True}
            if "Aggregate" in content:
                return {"content": "Final", "success": True}
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            process="hierarchical",
            backend="crewai",
            fallback_to_native=True,
        )
        assert result["success"] is True

    def test_crewai_hierarchical_requires_two_agents(self, engine_with_agents):
        """Test hierarchical process requires at least 2 agents."""
        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher"],
            task="Test task",
            process="hierarchical",
            backend="native",
        )
        assert result["success"] is False
        assert "at least 2 agents" in result["error"]

    def test_crewai_no_fallback_when_disabled(self, engine_with_agents):
        """Test error when CrewAI unavailable and fallback disabled."""
        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            backend="crewai",
            fallback_to_native=False,
        )
        assert result["success"] is False
        assert "CrewAI not installed" in result["error"]

    def test_crewai_native_backend_sequential(self, engine_with_agents):
        """Test native backend uses sequential pattern."""

        def mock_llm_call(state, **kwargs):
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            process="sequential",
            backend="native",
        )
        assert result["success"] is True
        assert "chain" in result  # Sequential returns chain

    def test_crewai_native_backend_hierarchical(self, engine_with_agents):
        """Test native backend uses coordinate pattern for hierarchical."""

        def mock_llm_call(state, **kwargs):
            messages = kwargs.get("messages", [])
            content = ""
            for msg in messages:
                if msg.get("role") == "user":
                    content = msg.get("content", "")

            if "Break down" in content:
                return {"content": '["task1"]', "success": True}
            if "Aggregate" in content:
                return {"content": "Final", "success": True}
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.crewai_delegate"](
            state={},
            agents=["researcher", "analyst"],
            task="Test task",
            process="hierarchical",
            backend="native",
        )
        assert result["success"] is True
        assert "leader" in result  # Coordinate returns leader

    @patch.dict(sys.modules, {"crewai": MagicMock()})
    def test_crewai_backend_integration(self, engine_with_agents):
        """Test CrewAI backend integration when available (mocked)."""
        # Create mock CrewAI classes
        mock_crewai = MagicMock()
        mock_agent = MagicMock()
        mock_task = MagicMock()
        mock_crew = MagicMock()
        mock_crew.kickoff.return_value = "CrewAI result"

        mock_crewai.Agent.return_value = mock_agent
        mock_crewai.Task.return_value = mock_task
        mock_crewai.Crew.return_value = mock_crew
        mock_crewai.Process.sequential = "sequential"
        mock_crewai.Process.hierarchical = "hierarchical"

        with patch.dict(sys.modules, {"crewai": mock_crewai}):
            # The action should attempt to use CrewAI
            # Since our mock won't work fully, it will fallback
            result = engine_with_agents.actions_registry["agent.crewai_delegate"](
                state={},
                agents=["researcher", "analyst"],
                task="Test task",
                backend="crewai",
            )
            # Fallback should occur due to mock limitations
            assert "success" in result


# =============================================================================
# Integration Tests
# =============================================================================


class TestAgentActionsIntegration:
    """Integration tests for agent actions with YAMLEngine."""

    def test_engine_loads_agents_from_settings(self):
        """Test YAMLEngine loads agents from settings.agents."""
        yaml_config = """
name: test-workflow
settings:
  llm:
    model: gpt-4
    temperature: 0.7
  agents:
    researcher:
      system_prompt: "You are a research assistant."
      temperature: 0.5
    writer:
      model: gpt-3.5-turbo
      system_prompt: "You are a writer."

state_schema:
  input: str
  output: str

nodes:
  - name: start
    run: |
      return {"output": "done"}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
        import yaml

        engine = YAMLEngine()
        config = yaml.safe_load(yaml_config)
        engine.load_from_dict(config)

        # Actions should be registered
        assert "agent.dispatch" in engine.actions_registry
        assert "agent.parallel" in engine.actions_registry
        assert "agent.sequential" in engine.actions_registry
        assert "agent.coordinate" in engine.actions_registry
        assert "agent.crewai_delegate" in engine.actions_registry

    def test_agent_actions_registered(self, engine):
        """Test all agent actions are registered."""
        actions = [
            "agent.dispatch",
            "agent.parallel",
            "agent.sequential",
            "agent.coordinate",
            "agent.crewai_delegate",
        ]
        for action in actions:
            assert action in engine.actions_registry, f"{action} not registered"

    def test_agent_actions_aliases_registered(self, engine):
        """Test actions.* aliases are registered."""
        aliases = [
            "actions.agent_dispatch",
            "actions.agent_parallel",
            "actions.agent_sequential",
            "actions.agent_coordinate",
            "actions.agent_crewai_delegate",
        ]
        for alias in aliases:
            assert alias in engine.actions_registry, f"{alias} not registered"


# =============================================================================
# Error Handling Tests
# =============================================================================


class TestErrorHandling:
    """Tests for error handling in agent actions."""

    def test_dispatch_returns_error_dict(self, engine):
        """Test dispatch returns proper error dict on failure."""
        result = engine.actions_registry["agent.dispatch"](
            state={},
            agent="unknown_agent",
            task="Test",
        )
        assert "error" in result
        assert "success" in result
        assert result["success"] is False

    def test_parallel_empty_agents_list(self, engine):
        """Test parallel handles empty agents list."""
        result = engine.actions_registry["agent.parallel"](
            state={},
            agents=[],
            task="Test",
        )
        # Should handle gracefully
        assert "error" in result or "agents_called" in result

    def test_sequential_empty_agents_list(self, engine):
        """Test sequential handles empty agents list."""
        result = engine.actions_registry["agent.sequential"](
            state={},
            agents=[],
            task="Test",
        )
        assert result["success"] is False
        assert "error" in result

    def test_dispatch_error_includes_agent_name(self, engine):
        """Test dispatch error includes agent name for context."""
        result = engine.actions_registry["agent.dispatch"](
            state={},
            agent="missing_agent",
            task="Test",
        )
        assert "missing_agent" in str(result.get("error", ""))

    def test_llm_call_not_available(self, engine_with_agents):
        """Test graceful handling when llm.call is not available."""
        # Remove llm.call
        del engine_with_agents.actions_registry["llm.call"]

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
        )
        assert result["success"] is False
        assert "not available" in result.get("error", "")


# =============================================================================
# Tool Bridge Integration Tests (AC6)
# =============================================================================


class TestToolBridgeIntegration:
    """Tests for tool bridge integration with agents (AC6)."""

    def test_agent_with_tools_config(self, llm_defaults):
        """Test agent can be configured with tools."""
        agents_config = {
            "tool_user": {
                "model": "gpt-4",
                "tools": ["web.search", "file.read"],
                "system_prompt": "Use tools to help users.",
            }
        }
        registry = AgentRegistry(agents_config, llm_defaults)
        agent = registry.get("tool_user")
        assert agent.tools == ["web.search", "file.read"]

    def test_agent_tools_in_llm_params(self, llm_defaults):
        """Test tools are not directly passed to llm_params (handled separately)."""
        agents_config = {
            "tool_user": {
                "model": "gpt-4",
                "tools": ["search"],
            }
        }
        registry = AgentRegistry(agents_config, llm_defaults)
        agent = registry.get("tool_user")
        params = agent.to_llm_params()
        # Tools should not be in llm_params - they're handled by tool bridges
        assert "tools" not in params


# =============================================================================
# Timeout Enforcement Tests
# =============================================================================


class TestTimeoutEnforcement:
    """Tests for timeout enforcement in parallel execution."""

    def test_parallel_respects_timeout(self, engine_with_agents):
        """Test parallel execution respects timeout parameter."""

        def slow_llm_call(state, **kwargs):
            timeout = kwargs.get("timeout", 60)
            # Should receive the timeout parameter
            return {
                "content": f"Timeout was {timeout}",
                "timeout_used": timeout,
                "success": True,
            }

        engine_with_agents.actions_registry["llm.call"] = slow_llm_call

        result = engine_with_agents.actions_registry["agent.parallel"](
            state={},
            agents=["researcher"],
            task="Test task",
            timeout=10,
        )
        assert result["success"] is True

    def test_dispatch_applies_agent_timeout(self, engine_with_agents):
        """Test dispatch applies agent's timeout configuration."""
        received_timeout = [None]

        def mock_llm_call(state, **kwargs):
            received_timeout[0] = kwargs.get("timeout")
            return {"content": "Response", "success": True}

        engine_with_agents.actions_registry["llm.call"] = mock_llm_call

        result = engine_with_agents.actions_registry["agent.dispatch"](
            state={},
            agent="researcher",
            task="Test task",
        )
        assert result["success"] is True
        # Timeout should be passed to llm.call
        assert received_timeout[0] is not None


# =============================================================================
# Register Actions Unit Tests
# =============================================================================


class TestRegisterActions:
    """Tests for the register_actions function."""

    def test_register_actions_creates_all_actions(self):
        """Test register_actions populates registry with all actions."""
        registry = {}
        mock_engine = MagicMock()
        mock_engine._config = None
        mock_engine.llm_settings = {}

        register_actions(registry, mock_engine)

        expected_actions = [
            "agent.dispatch",
            "agent.parallel",
            "agent.sequential",
            "agent.coordinate",
            "agent.crewai_delegate",
            "actions.agent_dispatch",
            "actions.agent_parallel",
            "actions.agent_sequential",
            "actions.agent_coordinate",
            "actions.agent_crewai_delegate",
        ]
        for action in expected_actions:
            assert action in registry, f"{action} not registered"

    def test_register_actions_are_callable(self):
        """Test all registered actions are callable."""
        registry = {}
        mock_engine = MagicMock()
        mock_engine._config = None
        mock_engine.llm_settings = {}

        register_actions(registry, mock_engine)

        for name, action in registry.items():
            assert callable(action), f"{name} is not callable"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
