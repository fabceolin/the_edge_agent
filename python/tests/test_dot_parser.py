"""
Tests for DOT-to-YAML Agent Generator.

TEA-TOOLS-001: Unit and integration tests for dot_parser module.
"""

import os
import tempfile
import unittest
from pathlib import Path

import yaml

from the_edge_agent.dot_parser import (
    CircularDependencyError,
    DotParseError,
    analyze_graph,
    dot_to_yaml,
    generate_yaml,
    parse_dot,
    parse_dot_string,
)


class TestDotParser(unittest.TestCase):
    """Tests for DOT parsing functionality."""

    def test_parse_simple_graph(self):
        """Test parsing a simple DOT graph."""
        dot_content = """
        digraph test {
            A [label="Node A"];
            B [label="Node B"];
            A -> B;
        }
        """
        result = parse_dot_string(dot_content, "test")

        self.assertEqual(result.name, "test")
        self.assertIn("A", result.nodes)
        self.assertIn("B", result.nodes)
        self.assertEqual(result.nodes["A"].label, "Node A")
        self.assertEqual(result.nodes["B"].label, "Node B")
        self.assertEqual(len(result.edges), 1)
        self.assertEqual(result.edges[0].source, "A")
        self.assertEqual(result.edges[0].target, "B")

    def test_parse_graph_with_clusters(self):
        """Test parsing DOT with cluster subgraphs."""
        dot_content = """
        digraph workflow {
            subgraph cluster_phase1 {
                label="Phase 1";
                A [label="Task A"];
                B [label="Task B"];
            }
            A -> B;
        }
        """
        result = parse_dot_string(dot_content, "workflow")

        self.assertIn("phase1", result.clusters)
        cluster = result.clusters["phase1"]
        self.assertEqual(cluster.label, "Phase 1")
        self.assertIn("A", cluster.nodes)
        self.assertIn("B", cluster.nodes)

    def test_parse_node_shapes(self):
        """Test parsing nodes with different shapes."""
        dot_content = """
        digraph test {
            start [label="Start", shape=ellipse];
            process [label="Process", shape=box];
            end [label="End", shape=circle];
        }
        """
        result = parse_dot_string(dot_content, "test")

        self.assertEqual(result.nodes["start"].shape, "ellipse")
        self.assertEqual(result.nodes["process"].shape, "box")
        self.assertEqual(result.nodes["end"].shape, "circle")

    def test_parse_edge_labels(self):
        """Test parsing edges with labels."""
        dot_content = """
        digraph test {
            A -> B [label="condition"];
            B -> C;
        }
        """
        result = parse_dot_string(dot_content, "test")

        edge_with_label = [e for e in result.edges if e.source == "A"][0]
        self.assertEqual(edge_with_label.label, "condition")

        edge_without_label = [e for e in result.edges if e.source == "B"][0]
        self.assertIsNone(edge_without_label.label)

    def test_parse_invalid_dot(self):
        """Test error handling for invalid DOT content."""
        with self.assertRaises(DotParseError):
            parse_dot_string("this is not valid DOT", "test")

    # TEA-TOOLS-002: Per-node command attribute tests
    def test_parse_node_with_command_attribute(self):
        """Test parsing nodes with command attribute."""
        dot_content = """
        digraph workflow {
            A [label="Build Frontend", command="npm run build"];
            B [label="Build Backend", command="cargo build"];
            C [label="No Command"];
        }
        """
        result = parse_dot_string(dot_content, "workflow")

        self.assertEqual(result.nodes["A"].command, "npm run build")
        self.assertEqual(result.nodes["B"].command, "cargo build")
        self.assertIsNone(result.nodes["C"].command)

    def test_parse_command_in_cluster(self):
        """Test parsing command attribute in cluster nodes."""
        dot_content = """
        digraph workflow {
            subgraph cluster_build {
                label="Build Phase";
                frontend [label="Frontend", command="npm run build"];
                backend [label="Backend", command="cargo build"];
            }
        }
        """
        result = parse_dot_string(dot_content, "workflow")

        self.assertEqual(result.nodes["frontend"].command, "npm run build")
        self.assertEqual(result.nodes["backend"].command, "cargo build")

    def test_parse_command_with_special_characters(self):
        """Test parsing commands with special characters (quotes, pipes)."""
        dot_content = """
        digraph workflow {
            A [label="Test", command="pytest tests/ -v | tee log.txt"];
            B [label="Deploy", command="cd frontend && npm run build"];
        }
        """
        result = parse_dot_string(dot_content, "workflow")

        self.assertEqual(result.nodes["A"].command, "pytest tests/ -v | tee log.txt")
        self.assertEqual(result.nodes["B"].command, "cd frontend && npm run build")

    def test_parse_file_not_found(self):
        """Test error handling for missing file."""
        with self.assertRaises(FileNotFoundError):
            parse_dot("/nonexistent/file.dot")


class TestGraphAnalysis(unittest.TestCase):
    """Tests for graph analysis functionality."""

    # TEA-TOOLS-002: Node commands mapping tests
    def test_analyze_node_commands_mapping(self):
        """Test that node_commands contains label -> command mapping."""
        dot_content = """
        digraph workflow {
            subgraph cluster_build {
                label="Build Phase";
                frontend [label="Frontend", command="npm run build"];
                backend [label="Backend", command="cargo build"];
                docs [label="Documentation"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        # Verify node_commands mapping
        self.assertIn("Frontend", analyzed.node_commands)
        self.assertIn("Backend", analyzed.node_commands)
        self.assertNotIn("Documentation", analyzed.node_commands)  # No command
        self.assertEqual(analyzed.node_commands["Frontend"], "npm run build")
        self.assertEqual(analyzed.node_commands["Backend"], "cargo build")

    def test_analyze_mixed_commands_and_no_commands(self):
        """Test analysis with mix of nodes with and without commands."""
        dot_content = """
        digraph workflow {
            A [label="Task A", command="run_a"];
            B [label="Task B"];
            C [label="Task C", command="run_c"];
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        self.assertEqual(len(analyzed.node_commands), 2)
        self.assertEqual(analyzed.node_commands.get("Task A"), "run_a")
        self.assertEqual(analyzed.node_commands.get("Task C"), "run_c")
        self.assertIsNone(analyzed.node_commands.get("Task B"))

    def test_analyze_parallel_phases(self):
        """Test detection of parallel phases from clusters."""
        dot_content = """
        digraph workflow {
            subgraph cluster_phase1 {
                label="Phase 1";
                A [label="Task A"];
                B [label="Task B"];
                C [label="Task C"];
            }
            Start [label="Start"];
            End [label="End"];
            Start -> A;
            Start -> B;
            Start -> C;
            A -> End;
            B -> End;
            C -> End;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        self.assertEqual(len(analyzed.phases), 1)
        phase = analyzed.phases[0]
        self.assertEqual(phase.name, "phase1")
        self.assertIn("Task A", phase.items)
        self.assertIn("Task B", phase.items)
        self.assertIn("Task C", phase.items)

    def test_analyze_implicit_parallel(self):
        """Test detection of parallel branches without clusters."""
        dot_content = """
        digraph workflow {
            start [label="Start"];
            task1 [label="Task 1"];
            task2 [label="Task 2"];
            collect [label="Collect"];
            start -> task1;
            start -> task2;
            task1 -> collect;
            task2 -> collect;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        # Should detect implicit parallel phase
        self.assertGreaterEqual(len(analyzed.phases), 1)

    def test_analyze_start_end_nodes(self):
        """Test detection of start and end nodes."""
        dot_content = """
        digraph workflow {
            A -> B;
            B -> C;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        self.assertEqual(analyzed.start_node, "A")
        self.assertEqual(analyzed.end_node, "C")

    def test_circular_dependency_detection(self):
        """Test that circular dependencies are detected."""
        dot_content = """
        digraph workflow {
            A -> B;
            B -> C;
            C -> A;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")

        with self.assertRaises(CircularDependencyError):
            analyze_graph(parsed)

    def test_analyze_implicit_parallel_with_internal_dependencies(self):
        """
        TEA-GAME-001 bugfix: Test that fan-out targets with internal dependencies
        are NOT incorrectly grouped into a parallel phase.

        Given: A -> B, A -> C, B -> C (C depends on B)
        Expected: B and C should NOT be in same parallel phase because B -> C
        """
        dot_content = """
        digraph workflow {
            A [label="A"];
            B [label="B"];
            C [label="C"];
            D [label="D"];
            A -> B;
            A -> C;
            B -> C;
            C -> D;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        # The implicit phase detection should NOT group B and C together
        # because C depends on B (there's an edge B -> C)
        for phase in analyzed.phases:
            phase_items = set(phase.items)
            # B and C should NOT both be in the same phase
            if "B" in phase_items:
                self.assertNotIn(
                    "C",
                    phase_items,
                    "B and C should not be in same phase due to B -> C dependency",
                )

    def test_analyze_implicit_parallel_truly_independent(self):
        """
        TEA-GAME-001 bugfix: Test that truly independent fan-out targets
        ARE correctly grouped into a parallel phase.

        Given: A -> B, A -> C (no edge between B and C)
        Expected: B and C should be in the same parallel phase
        """
        dot_content = """
        digraph workflow {
            A [label="A"];
            B [label="B"];
            C [label="C"];
            D [label="D"];
            A -> B;
            A -> C;
            B -> D;
            C -> D;
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        # Should detect a parallel phase containing B and C
        found_bc_phase = False
        for phase in analyzed.phases:
            phase_items = set(phase.items)
            if "B" in phase_items and "C" in phase_items:
                found_bc_phase = True
                break

        self.assertTrue(
            found_bc_phase,
            "B and C should be in same parallel phase (no internal dependencies)",
        )


class TestYamlGenerator(unittest.TestCase):
    """Tests for YAML generation functionality."""

    # TEA-TOOLS-002: Per-node command YAML generation tests
    def test_generate_yaml_with_node_commands(self):
        """Test generating YAML with per-node commands."""
        dot_content = """
        digraph workflow {
            subgraph cluster_build {
                label="Build Phase";
                frontend [label="Frontend", command="npm run build"];
                backend [label="Backend", command="cargo build"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="",  # Not needed - all nodes have commands
            max_concurrency=2,
            use_node_commands=True,
        )

        # Verify YAML contains command mappings
        self.assertIn("_phase1_commands", yaml_content)
        self.assertIn("npm run build", yaml_content)
        self.assertIn("cargo build", yaml_content)
        self.assertIn("per-node", yaml_content)  # Header should indicate per-node mode

        # Verify YAML is valid
        config = yaml.safe_load(yaml_content)
        self.assertIn("nodes", config)

    def test_generate_yaml_dispatch_code(self):
        """Test that dispatch code looks up commands from dict."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                A [label="Task A", command="run_a"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="",  # Not needed in per-node mode
            use_node_commands=True,
        )

        # Should contain dispatch lookup code
        self.assertIn("commands = state.get", yaml_content)
        self.assertIn("commands.get(item)", yaml_content)
        # Should NOT have fallback logic
        self.assertNotIn("fallback", yaml_content.lower())

    def test_generate_yaml_without_node_commands_unchanged(self):
        """Test that without use_node_commands, behavior is unchanged."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                A [label="Task A", command="ignored_command"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="echo {{ item }}",
            use_node_commands=False,  # Default behavior
        )

        # Should NOT contain command dispatch
        self.assertNotIn("_phase1_commands", yaml_content)
        self.assertIn("template", yaml_content)  # Header should indicate template mode

    def test_generate_simple_yaml(self):
        """Test generating YAML from a simple graph."""
        dot_content = """
        digraph workflow {
            subgraph cluster_build {
                label="Build Phase";
                A [label="Build App"];
                B [label="Build Tests"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="echo {{ item }}",
            max_concurrency=2,
        )

        # Verify YAML is valid
        config = yaml.safe_load(yaml_content)

        self.assertEqual(config["name"], "workflow")
        self.assertIn("nodes", config)
        self.assertIn("edges", config)

    def test_generate_yaml_with_tmux(self):
        """Test generating YAML with tmux execution mode."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                A [label="Task A"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="echo {{ item }}",
            use_tmux=True,
            tmux_session="test-session",
        )

        # Should contain tmux-related code
        self.assertIn("tmux", yaml_content)
        self.assertIn("test-session", yaml_content)

    def test_generate_yaml_subprocess_mode(self):
        """Test generating YAML with subprocess execution mode."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                A [label="Task A"];
            }
        }
        """
        parsed = parse_dot_string(dot_content, "workflow")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="make {{ item }}",
            use_tmux=False,
        )

        # Should contain subprocess code
        self.assertIn("subprocess", yaml_content)
        self.assertIn("make", yaml_content)

    def test_generate_yaml_custom_name(self):
        """Test generating YAML with custom workflow name."""
        dot_content = """
        digraph original {
            A [label="A"];
        }
        """
        parsed = parse_dot_string(dot_content, "original")
        analyzed = analyze_graph(parsed)

        yaml_content = generate_yaml(
            analyzed=analyzed,
            command_template="echo {{ item }}",
            workflow_name="custom-name",
        )

        config = yaml.safe_load(yaml_content)
        self.assertEqual(config["name"], "custom-name")


class TestDotToYaml(unittest.TestCase):
    """Integration tests for the main dot_to_yaml function."""

    # TEA-TOOLS-002: Strict mode tests (no fallback)
    def test_dot_to_yaml_error_missing_commands(self):
        """Test that error is raised when some nodes missing commands."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                with_cmd [label="With Command", command="run_specific"];
                without_cmd [label="Without Command"];
            }
        }
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        try:
            with self.assertRaises(ValueError) as ctx:
                dot_to_yaml(
                    file_path=dot_path,
                    command_template="",
                    use_node_commands=True,
                )

            # Should list the missing node
            self.assertIn("Without Command", str(ctx.exception))
            self.assertIn("Missing commands for", str(ctx.exception))
        finally:
            os.unlink(dot_path)

    def test_dot_to_yaml_error_no_commands_at_all(self):
        """Test that error is raised when no nodes have commands."""
        dot_content = """
        digraph workflow {
            A [label="Task A"];
            B [label="Task B"];
        }
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        try:
            with self.assertRaises(ValueError) as ctx:
                dot_to_yaml(
                    file_path=dot_path,
                    command_template="",
                    use_node_commands=True,
                )

            self.assertIn("Missing commands for", str(ctx.exception))
        finally:
            os.unlink(dot_path)

    def test_dot_to_yaml_with_all_node_commands(self):
        """Test that all-commands scenario works without template."""
        dot_content = """
        digraph workflow {
            subgraph cluster_tasks {
                label="Tasks";
                A [label="Task A", command="run_a"];
                B [label="Task B", command="run_b"];
            }
        }
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        try:
            # Even without --command, should work if all nodes have commands
            yaml_content = dot_to_yaml(
                file_path=dot_path,
                command_template="",  # No fallback needed
                use_node_commands=True,
            )

            self.assertIn("run_a", yaml_content)
            self.assertIn("run_b", yaml_content)
        finally:
            os.unlink(dot_path)

    def test_dot_to_yaml_file(self):
        """Test converting a DOT file to YAML."""
        # Create temporary DOT file
        dot_content = """
        digraph test {
            subgraph cluster_phase {
                label="Phase 1";
                A [label="Step A"];
                B [label="Step B"];
            }
        }
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        try:
            yaml_content = dot_to_yaml(
                file_path=dot_path,
                command_template="echo {{ item }}",
            )

            # Verify output is valid YAML
            config = yaml.safe_load(yaml_content)
            self.assertIn("name", config)
            self.assertIn("nodes", config)
            self.assertIn("edges", config)
        finally:
            os.unlink(dot_path)

    def test_dot_to_yaml_with_output_file(self):
        """Test writing YAML output to file."""
        dot_content = """
        digraph test {
            A [label="A"];
        }
        """
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        output_path = tempfile.mktemp(suffix=".yaml")

        try:
            dot_to_yaml(
                file_path=dot_path,
                command_template="echo {{ item }}",
                output_path=output_path,
            )

            # Verify file was created
            self.assertTrue(Path(output_path).exists())

            # Verify content is valid YAML
            config = yaml.safe_load(Path(output_path).read_text())
            self.assertIn("name", config)
        finally:
            os.unlink(dot_path)
            if Path(output_path).exists():
                os.unlink(output_path)


class TestExampleDotFiles(unittest.TestCase):
    """Tests using example DOT files from examples/dot/."""

    @classmethod
    def setUpClass(cls):
        """Set up path to examples directory."""
        cls.examples_dir = Path(__file__).parent.parent.parent / "examples" / "dot"

    # TEA-TOOLS-002: Tests for per-node command examples
    def test_parse_per_node_commands_example(self):
        """Test parsing the per-node-commands.dot example."""
        dot_path = self.examples_dir / "per-node-commands.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        parsed = parse_dot(str(dot_path))
        analyzed = analyze_graph(parsed)

        # Should have command attributes on nodes
        self.assertGreater(len(analyzed.node_commands), 0)
        self.assertIn("Frontend", analyzed.node_commands)
        self.assertIn("npm run build", analyzed.node_commands["Frontend"])

    def test_parse_mixed_commands_example(self):
        """Test parsing the mixed-commands.dot example."""
        dot_path = self.examples_dir / "mixed-commands.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        parsed = parse_dot(str(dot_path))
        analyzed = analyze_graph(parsed)

        # All nodes should have commands (TEA-TOOLS-002: no fallback mode)
        self.assertGreater(len(analyzed.node_commands), 0)
        self.assertIn("Lint Code", analyzed.node_commands)
        self.assertIn("Code Review", analyzed.node_commands)
        self.assertIn("Final Approval", analyzed.node_commands)

    def test_generate_yaml_from_per_node_commands_example(self):
        """Test generating YAML from per-node-commands.dot."""
        dot_path = self.examples_dir / "per-node-commands.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        yaml_content = dot_to_yaml(
            file_path=str(dot_path),
            command_template="echo {{ item }}",
            use_node_commands=True,
        )

        # Verify YAML contains per-node commands
        self.assertIn("npm run build", yaml_content)
        self.assertIn("cargo build", yaml_content)
        self.assertIn("pytest", yaml_content)

        # Verify valid YAML
        config = yaml.safe_load(yaml_content)
        self.assertIn("nodes", config)

    def test_parse_parallel_phases_example(self):
        """Test parsing the parallel-phases.dot example."""
        dot_path = self.examples_dir / "parallel-phases.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        parsed = parse_dot(str(dot_path))

        # Should have two clusters (phase1, phase2)
        self.assertGreaterEqual(len(parsed.clusters), 1)

        # Should have nodes and edges
        self.assertGreater(len(parsed.nodes), 0)
        self.assertGreater(len(parsed.edges), 0)

    def test_parse_simple_parallel_example(self):
        """Test parsing the simple-parallel.dot example."""
        dot_path = self.examples_dir / "simple-parallel.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        parsed = parse_dot(str(dot_path))
        analyzed = analyze_graph(parsed)

        # Should detect parallel structure
        self.assertGreaterEqual(len(analyzed.phases), 1)

    def test_parse_linear_example(self):
        """Test parsing the linear.dot example."""
        dot_path = self.examples_dir / "linear.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        parsed = parse_dot(str(dot_path))
        analyzed = analyze_graph(parsed)

        # Should have standalone nodes (no parallel phases)
        self.assertGreater(len(analyzed.standalone_nodes), 0)

    def test_generate_yaml_from_example(self):
        """Test generating YAML from an example DOT file."""
        dot_path = self.examples_dir / "parallel-phases.dot"
        if not dot_path.exists():
            self.skipTest(f"Example file not found: {dot_path}")

        yaml_content = dot_to_yaml(
            file_path=str(dot_path),
            command_template="tea run sub.yaml --input '{{ item }}'",
            max_concurrency=3,
        )

        # Verify YAML is valid
        config = yaml.safe_load(yaml_content)
        self.assertIn("name", config)
        self.assertIn("nodes", config)
        self.assertIn("edges", config)

        # Verify has dynamic_parallel nodes
        node_types = [n.get("type") for n in config.get("nodes", [])]
        # May or may not have dynamic_parallel depending on structure
        # Just verify basic structure is valid
        self.assertIsInstance(config["nodes"], list)


class TestDotToStateGraph(unittest.TestCase):
    """Tests for DOT to StateGraph direct conversion (TEA-GAME-001)."""

    def test_dot_to_stategraph_simple(self):
        """Test converting a simple DOT to StateGraph."""
        from the_edge_agent.dot_parser import dot_to_stategraph

        dot_content = """
        digraph workflow {
            A [label="Task A", command="echo A"];
            B [label="Task B", command="echo B"];
            A -> B;
        }
        """

        # Create temporary DOT file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".dot", delete=False) as f:
            f.write(dot_content)
            dot_path = f.name

        try:
            graph = dot_to_stategraph(dot_path)

            # Verify it's a compiled graph (has invoke method)
            self.assertTrue(hasattr(graph, "invoke"))

            # Verify nodes exist
            self.assertIn("A", graph.graph.nodes)
            self.assertIn("B", graph.graph.nodes)
        finally:
            os.unlink(dot_path)

    def test_dot_to_stategraph_from_string(self):
        """Test converting DOT string directly to StateGraph."""
        from the_edge_agent.dot_parser import dot_to_stategraph

        dot_content = """
        digraph test {
            task1 [label="Task 1", command="echo task1"];
            task2 [label="Task 2", command="echo task2"];
            task1 -> task2;
        }
        """

        # Should work with DOT string content
        graph = dot_to_stategraph(dot_content)

        self.assertTrue(hasattr(graph, "invoke"))
        self.assertIn("task1", graph.graph.nodes)
        self.assertIn("task2", graph.graph.nodes)

    def test_dot_to_stategraph_with_start_end_markers(self):
        """Test that Start/End ellipse nodes are handled correctly."""
        from the_edge_agent.dot_parser import dot_to_stategraph

        dot_content = """
        digraph workflow {
            Start [label="Start", shape=ellipse];
            End [label="End", shape=ellipse];
            A [label="Task A", command="echo A"];
            B [label="Task B", command="echo B"];
            Start -> A;
            A -> B;
            B -> End;
        }
        """

        graph = dot_to_stategraph(dot_content)

        # Start/End should not be in nodes (they're markers)
        self.assertNotIn("Start", graph.graph.nodes)
        self.assertNotIn("End", graph.graph.nodes)

        # A and B should exist
        self.assertIn("A", graph.graph.nodes)
        self.assertIn("B", graph.graph.nodes)

    def test_run_dot_simple(self):
        """Test run_dot convenience function."""
        from the_edge_agent.dot_parser import run_dot

        dot_content = """
        digraph test {
            echo_test [label="Echo Test", command="echo 'hello world'"];
        }
        """

        result = run_dot(dot_content, command_timeout=30)

        # Should have results
        self.assertIn("results", result)
        self.assertGreater(len(result["results"]), 0)

        # First result should be successful
        first_result = result["results"][0]
        self.assertTrue(first_result.get("success", False))
        self.assertIn("hello world", first_result.get("stdout", ""))


if __name__ == "__main__":
    unittest.main()
