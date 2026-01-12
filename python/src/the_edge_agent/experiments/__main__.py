"""
Allow running experiments module as a script.

Usage:
    python -m the_edge_agent.experiments --agent agent.yaml --dataset test_cases
"""

from .cli import main

if __name__ == "__main__":
    import sys

    sys.exit(main())
