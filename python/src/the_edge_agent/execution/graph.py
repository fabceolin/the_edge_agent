"""
Graph Executor for TEA.

This module implements the GraphExecutor that uses LLM to analyze task
dependencies and generates a DOT file for optimal parallelization.
Git worktree/branching is DISABLED in graph mode.

Story: TEA-RALPHY-001.6 (Execution Modes)
Acceptance Criteria: 17, 18, 19, 20, 21, 22

Example:
    >>> from the_edge_agent.execution import GraphExecutor, ExecutionConfig
    >>>
    >>> config = ExecutionConfig(
    ...     mode=ExecutionMode.GRAPH,
    ...     dot_output_path="./workflow.dot",
    ...     shell_provider="claude"
    ... )
    >>> executor = GraphExecutor(config)
    >>> result = await executor.execute(tasks, initial_state)
"""

import json
import logging
import os
import subprocess
from collections import defaultdict
from typing import Any, Dict, List, Optional

from .orchestrator import BaseExecutor
from .mode import ExecutionConfig

logger = logging.getLogger(__name__)

# Default prompt for dependency analysis
DEFAULT_DEPENDENCY_PROMPT = """
Analyze the following tasks and identify their dependencies.

Tasks:
{tasks_json}

For each task, determine:
1. Which other tasks must complete BEFORE this task can start
2. Whether tasks can run in parallel (no dependencies between them)

Return a JSON object with this structure:
{{
    "nodes": ["task-id-1", "task-id-2", ...],
    "edges": [
        {{"from": "task-a", "to": "task-b"}},  // task-b depends on task-a
        ...
    ],
    "analysis": "Brief explanation of the dependency structure"
}}

Rules:
- If task B uses output from task A, add edge from A to B
- If task B modifies files that task A reads, add edge from A to B
- If tasks are independent, no edge is needed (they can run in parallel)
- Avoid circular dependencies

Return ONLY the JSON object, no markdown or explanation.
"""


class GraphExecutor(BaseExecutor):
    """
    Execute tasks via DOT-based orchestration with LLM dependency analysis.

    Characteristics:
    - LLM analyzes task dependencies and generates dependency graph
    - DOT file generated following workflow orchestration format
    - Execution via `tea from dot --use-node-commands`
    - Git worktree/branching is DISABLED
    - All changes made directly to current working directory
    """

    def __init__(self, config: ExecutionConfig):
        """
        Initialize the graph executor.

        Args:
            config: ExecutionConfig with graph mode settings
        """
        super().__init__(config)

    async def execute(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Execute tasks via DOT-based orchestration.

        Steps:
        1. Analyze dependencies with LLM
        2. Generate DOT file
        3. Convert DOT to YAML via `tea from dot`
        4. Execute generated workflow

        Args:
            tasks: List of task dictionaries
            state: Initial state dictionary

        Returns:
            {
                "mode": "graph",
                "status": "success" | "error",
                "dot_path": str,
                "yaml_path": str,
                "dependency_graph": Dict,
                "stdout": str,
                "stderr": str,
                "returncode": int
            }
        """
        working_dir = self.config.working_directory or os.getcwd()

        logger.info(
            f"Starting graph execution of {len(tasks)} tasks "
            f"(DOT output: {self.config.dot_output_path})"
        )

        try:
            # Step 1: Analyze dependencies with LLM
            logger.debug("Analyzing task dependencies with LLM...")
            dependency_graph = await self._analyze_dependencies(tasks, state)

            # Step 2: Generate DOT file
            logger.debug("Generating DOT file...")
            dot_content = self._generate_dot(dependency_graph, tasks)

            dot_path = os.path.join(working_dir, self.config.dot_output_path)
            os.makedirs(os.path.dirname(dot_path) or ".", exist_ok=True)

            with open(dot_path, "w") as f:
                f.write(dot_content)

            logger.info(f"DOT file saved to {dot_path}")

            # Step 3: Convert DOT to YAML via tea from dot
            logger.debug("Converting DOT to YAML...")
            yaml_path = os.path.join(working_dir, self.config.yaml_output_path)

            convert_result = subprocess.run(
                [
                    "tea",
                    "from",
                    "dot",
                    dot_path,
                    "--use-node-commands",
                    "-o",
                    yaml_path,
                ],
                capture_output=True,
                text=True,
                cwd=working_dir,
            )

            if convert_result.returncode != 0:
                logger.error(f"DOT conversion failed: {convert_result.stderr}")
                return {
                    "mode": "graph",
                    "status": "error",
                    "error": f"DOT to YAML conversion failed: {convert_result.stderr}",
                    "dot_path": dot_path,
                    "dependency_graph": dependency_graph,
                }

            logger.info(f"YAML workflow saved to {yaml_path}")

            # Step 4: Execute generated workflow
            logger.debug("Executing generated workflow...")
            exec_result = subprocess.run(
                ["tea", "run", yaml_path],
                capture_output=True,
                text=True,
                cwd=working_dir,
            )

            # Step 5: Cleanup if configured
            if self.config.cleanup_generated_files:
                self._cleanup_files(dot_path, yaml_path)

            status = "success" if exec_result.returncode == 0 else "error"

            logger.info(f"Graph execution complete: status={status}")

            return {
                "mode": "graph",
                "status": status,
                "dot_path": dot_path,
                "yaml_path": yaml_path,
                "dependency_graph": dependency_graph,
                "stdout": exec_result.stdout,
                "stderr": exec_result.stderr,
                "returncode": exec_result.returncode,
            }

        except Exception as e:
            logger.error(f"Graph execution failed: {e}")
            return {
                "mode": "graph",
                "status": "error",
                "error": str(e),
            }

    async def _analyze_dependencies(
        self,
        tasks: List[Dict[str, Any]],
        state: Dict[str, Any],
    ) -> Dict[str, Any]:
        """
        Use LLM to analyze task dependencies.

        Args:
            tasks: List of task dictionaries
            state: Current state

        Returns:
            Dependency graph: {"nodes": [...], "edges": [...]}
        """
        # Try to load custom prompt template
        prompt_template = self._load_prompt_template()
        if prompt_template is None:
            prompt_template = DEFAULT_DEPENDENCY_PROMPT

        # Format tasks for prompt
        tasks_json = json.dumps(tasks, indent=2)
        prompt = prompt_template.format(tasks_json=tasks_json)

        # Call LLM via shell provider
        try:
            response = await self._call_llm(prompt)
            graph = self._parse_dependency_response(response, tasks)
            return graph
        except Exception as e:
            logger.warning(
                f"LLM dependency analysis failed: {e}, using sequential order"
            )
            # Fallback: create sequential dependency chain
            return self._create_sequential_graph(tasks)

    def _load_prompt_template(self) -> Optional[str]:
        """
        Load custom prompt template if specified and exists.

        Returns:
            Template string or None
        """
        template_path = self.config.prompt_template

        if os.path.exists(template_path):
            try:
                with open(template_path, "r") as f:
                    return f.read()
            except Exception as e:
                logger.warning(f"Failed to load prompt template: {e}")

        return None

    async def _call_llm(self, prompt: str) -> str:
        """
        Call LLM using configured shell provider.

        Args:
            prompt: Prompt to send to LLM

        Returns:
            LLM response text
        """
        provider = self.config.shell_provider

        if provider == "claude":
            return await self._call_claude(prompt)
        elif provider == "openai":
            return await self._call_openai(prompt)
        else:
            # Try shell-based call
            return await self._call_shell_llm(prompt, provider)

    async def _call_claude(self, prompt: str) -> str:
        """Call Claude API for dependency analysis."""
        try:
            import anthropic

            client = anthropic.Anthropic()
            message = client.messages.create(
                model="claude-sonnet-4-20250514",
                max_tokens=4096,
                messages=[{"role": "user", "content": prompt}],
            )
            return message.content[0].text
        except ImportError:
            raise RuntimeError(
                "anthropic package not installed. Run: pip install anthropic"
            )

    async def _call_openai(self, prompt: str) -> str:
        """Call OpenAI API for dependency analysis."""
        try:
            import openai

            client = openai.OpenAI()
            response = client.chat.completions.create(
                model="gpt-4",
                messages=[{"role": "user", "content": prompt}],
            )
            return response.choices[0].message.content
        except ImportError:
            raise RuntimeError("openai package not installed. Run: pip install openai")

    async def _call_shell_llm(self, prompt: str, provider: str) -> str:
        """Call LLM via shell command."""
        # Use llm.call action if engine is available
        if self.config.engine is not None:
            try:
                from ..actions.llm_actions import llm_call

                result = llm_call(
                    {},  # state
                    provider="shell",
                    shell_provider=provider,
                    messages=[{"role": "user", "content": prompt}],
                )
                if result.get("success"):
                    return result.get("content", "")
            except Exception as e:
                logger.warning(f"llm.call failed: {e}")

        # Fallback: direct shell call
        result = subprocess.run(
            ["tea", "llm", "--provider", provider, "--prompt", prompt],
            capture_output=True,
            text=True,
        )
        return result.stdout

    def _parse_dependency_response(
        self,
        response: str,
        tasks: List[Dict[str, Any]],
    ) -> Dict[str, Any]:
        """
        Parse LLM response into dependency graph.

        Args:
            response: LLM response text
            tasks: Original task list

        Returns:
            Dependency graph dict
        """
        # Try to extract JSON from response
        try:
            # Find JSON in response
            start = response.find("{")
            end = response.rfind("}") + 1
            if start >= 0 and end > start:
                json_str = response[start:end]
                graph = json.loads(json_str)

                # Validate structure
                if "nodes" not in graph:
                    graph["nodes"] = [
                        t.get("id", f"task-{i}") for i, t in enumerate(tasks)
                    ]
                if "edges" not in graph:
                    graph["edges"] = []

                return graph
        except json.JSONDecodeError:
            pass

        # Fallback: sequential graph
        logger.warning("Could not parse LLM response, using sequential order")
        return self._create_sequential_graph(tasks)

    def _create_sequential_graph(
        self,
        tasks: List[Dict[str, Any]],
    ) -> Dict[str, Any]:
        """
        Create a sequential dependency graph (fallback).

        Args:
            tasks: List of tasks

        Returns:
            Sequential dependency graph
        """
        nodes = [t.get("id", f"task-{i}") for i, t in enumerate(tasks)]
        edges = []

        for i in range(len(nodes) - 1):
            edges.append({"from": nodes[i], "to": nodes[i + 1]})

        return {"nodes": nodes, "edges": edges}

    def _generate_dot(
        self,
        graph: Dict[str, Any],
        tasks: List[Dict[str, Any]],
    ) -> str:
        """
        Generate DOT file following workflow orchestration format.

        Args:
            graph: Dependency graph with nodes and edges
            tasks: Original task list for command info

        Returns:
            DOT file content
        """
        nodes = graph["nodes"]
        edges = graph["edges"]

        # Build task lookup for commands
        task_lookup = {t.get("id", f"task-{i}"): t for i, t in enumerate(tasks)}

        # Build adjacency list for dependency calculation
        deps = defaultdict(set)
        for edge in edges:
            deps[edge["to"]].add(edge["from"])

        # Topological sort into phases
        phases = self._compute_phases(nodes, deps)

        # Generate DOT content
        lines = [
            "digraph workflow {",
            "    rankdir=TB;",
            "    compound=true;",
            "",
        ]

        for i, phase in enumerate(phases, 1):
            parallel_label = " (Parallel)" if len(phase) > 1 else ""
            lines.append(f"    // Phase {i}{parallel_label}")
            lines.append(f"    subgraph cluster_phase_{i} {{")
            lines.append(f'        label="Phase {i}";')
            lines.append("        style=dashed;")
            lines.append("")

            for node in phase:
                task = task_lookup.get(node, {})
                # Get command from task or generate default
                cmd = task.get(
                    "command", f'tea run task.yaml --input \'{{"task": "{node}"}}\''
                )
                # Escape quotes in command for DOT
                cmd_escaped = cmd.replace('"', '\\"')
                lines.append(f'        "{node}" [command="{cmd_escaped}"];')

            lines.append("    }")
            lines.append("")

        # Add edges
        lines.append("    // Edges")
        for edge in edges:
            lines.append(f'    "{edge["from"]}" -> "{edge["to"]}";')

        lines.append("}")
        return "\n".join(lines)

    def _compute_phases(
        self,
        nodes: List[str],
        deps: Dict[str, set],
    ) -> List[List[str]]:
        """
        Compute execution phases via topological sort.

        Args:
            nodes: List of node IDs
            deps: Dependency adjacency list

        Returns:
            List of phases, each phase is a list of parallel nodes

        Raises:
            ValueError: If circular dependency detected
        """
        phases = []
        remaining = set(nodes)
        completed = set()

        while remaining:
            # Find nodes with all dependencies satisfied
            ready = [n for n in remaining if deps[n].issubset(completed)]

            if not ready:
                # Check for circular dependency
                raise ValueError(
                    f"Circular dependency detected. "
                    f"Remaining nodes: {remaining}, "
                    f"Unsatisfied deps: {[(n, deps[n] - completed) for n in remaining]}"
                )

            phases.append(ready)
            completed.update(ready)
            remaining -= set(ready)

        return phases

    def _cleanup_files(self, dot_path: str, yaml_path: str) -> None:
        """
        Remove generated DOT and YAML files.

        Args:
            dot_path: Path to DOT file
            yaml_path: Path to YAML file
        """
        for path in [dot_path, yaml_path]:
            try:
                if os.path.exists(path):
                    os.remove(path)
                    logger.debug(f"Removed generated file: {path}")
            except Exception as e:
                logger.warning(f"Failed to remove {path}: {e}")
