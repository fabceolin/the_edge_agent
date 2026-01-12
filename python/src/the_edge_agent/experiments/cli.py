"""
CLI entry point for TEA experiments.

This module provides a command-line interface for running Opik experiments
on TEA YAML agents.

Usage:
    tea-experiments --agent agent.yaml --dataset test_cases --version 1.0
    python -m the_edge_agent.experiments --agent agent.yaml --dataset test_cases

Note:
    The CLI provides basic experiment execution. For custom metrics and
    advanced usage, use the Python API directly.
"""

import argparse
import json
import logging
import sys
from pathlib import Path
from typing import Optional


def main(args: Optional[list] = None) -> int:
    """
    Main CLI entry point for TEA experiments.

    Args:
        args: Optional list of command-line arguments. If None, uses sys.argv.

    Returns:
        Exit code (0 for success, non-zero for errors).
    """
    parser = argparse.ArgumentParser(
        prog="tea-experiments",
        description="Run Opik experiments for TEA YAML agents",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Dry run (validate without executing)
  tea-experiments --agent my_agent.yaml --dataset test_cases --dry-run

  # Run experiment with version tag
  tea-experiments --agent my_agent.yaml --dataset test_cases --version 2.0

  # Save results to JSON file
  tea-experiments --agent my_agent.yaml --dataset test_cases --output results.json

Note:
  Custom metrics are not supported via CLI. Use the Python API for custom metrics:

    from the_edge_agent.experiments import run_tea_experiment, BaseTeaMetric

    class MyMetric(BaseTeaMetric):
        name = "my_metric"
        def score(self, output, expected_output=None, **kwargs):
            return self.make_result(0.8, "Good match")

    result = run_tea_experiment(
        agent_yaml="agent.yaml",
        dataset_name="test_cases",
        metrics=[MyMetric()],
        experiment_name="my_experiment"
    )
""",
    )

    parser.add_argument(
        "--agent",
        "-a",
        required=True,
        metavar="FILE",
        help="Path to YAML agent file",
    )

    parser.add_argument(
        "--dataset",
        "-d",
        required=True,
        metavar="NAME",
        help="Opik dataset name",
    )

    parser.add_argument(
        "--version",
        "-v",
        default="1.0",
        metavar="TAG",
        help="Experiment version tag (default: 1.0)",
    )

    parser.add_argument(
        "--project",
        "-p",
        metavar="NAME",
        help="Opik project name (uses default if not specified)",
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate configuration without running experiment",
    )

    parser.add_argument(
        "--output",
        "-o",
        metavar="FILE",
        help="Save results to JSON file",
    )

    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose logging",
    )

    parsed_args = parser.parse_args(args)

    # Configure logging
    if parsed_args.verbose:
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        )
    else:
        logging.basicConfig(
            level=logging.INFO,
            format="%(levelname)s: %(message)s",
        )

    logger = logging.getLogger(__name__)

    # Validate agent path
    agent_path = Path(parsed_args.agent)
    if not agent_path.exists():
        print(f"Error: Agent file not found: {parsed_args.agent}", file=sys.stderr)
        return 1

    # Dry run mode
    if parsed_args.dry_run:
        print("[DRY RUN] Would run experiment:")
        print(f"  Agent: {parsed_args.agent}")
        print(f"  Dataset: {parsed_args.dataset}")
        print(f"  Version: {parsed_args.version}")
        print(f"  Project: {parsed_args.project or '(default)'}")
        print()

        # Validate Opik availability
        try:
            from .runner import _check_opik_available

            _check_opik_available()
            print("  Opik SDK: Available")
        except ImportError as e:
            print(f"  Opik SDK: NOT AVAILABLE - {e}", file=sys.stderr)
            return 1

        # Validate YAML syntax
        try:
            import yaml

            with open(agent_path, "r", encoding="utf-8") as f:
                yaml.safe_load(f)
            print("  YAML syntax: Valid")
        except Exception as e:
            print(f"  YAML syntax: INVALID - {e}", file=sys.stderr)
            return 1

        print()
        print("Validation passed. Remove --dry-run to execute.")
        return 0

    # Run experiment
    print(f"Running experiment: {parsed_args.agent} v{parsed_args.version}")
    print("Note: No custom metrics specified. Using empty metrics list.")
    print("      For custom metrics, use the Python API.")
    print()

    try:
        from .runner import run_tea_experiment

        # Generate experiment name from agent filename and version
        agent_name = agent_path.stem.replace(".yaml", "").replace(".", "_")
        experiment_name = f"{agent_name}_v{parsed_args.version}"

        result = run_tea_experiment(
            agent_yaml=str(agent_path),
            dataset_name=parsed_args.dataset,
            metrics=[],  # No metrics from CLI - use Python API for custom metrics
            experiment_name=experiment_name,
            project_name=parsed_args.project,
            experiment_config={
                "cli_version": parsed_args.version,
                "cli_invoked": True,
            },
        )

        # Output results
        if result.get("status") == "complete":
            print(f"Experiment complete: {result.get('experiment_name')}")
            print(f"  Dataset: {result.get('dataset_name')}")
            print(f"  Metrics: {result.get('metrics_count')}")
        else:
            print(f"Experiment error: {result.get('error')}", file=sys.stderr)

        # Save to file if requested
        if parsed_args.output:
            output_path = Path(parsed_args.output)
            with open(output_path, "w", encoding="utf-8") as f:
                json.dump(result, f, indent=2)
            print(f"Results saved to: {output_path}")

        return 0 if result.get("status") == "complete" else 1

    except ImportError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except Exception as e:
        logger.exception("Experiment failed")
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
