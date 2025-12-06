#!/usr/bin/env python3
"""
Example script demonstrating YAML-based agent configuration.

Usage:
    python run_yaml_agent.py examples/yaml_agent_example.yaml
    python run_yaml_agent.py examples/yaml_customer_support_example.yaml
"""

import sys
import json
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent.yaml_engine import YAMLEngine


def run_research_agent():
    """Run the simple research agent example."""
    print("=" * 80)
    print("Running Simple Research Agent")
    print("=" * 80)

    engine = YAMLEngine()
    graph = engine.load_from_file("examples/yaml_agent_example.yaml")

    # Execute with initial state
    initial_state = {
        "query": "artificial intelligence"
    }

    print(f"\nInitial query: {initial_state['query']}\n")

    # Run the graph
    for event in graph.stream(initial_state):
        event_type = event.get("type")
        node = event.get("node")

        if event_type == "state":
            print(f"✓ Completed node: {node}")
            # Optionally show state updates
            state = event.get("state", {})
            if "summary" in state and node == "summarize":
                print(f"  Summary generated: {len(state['summary'])} chars")
            elif "formatted_output" in state and node == "format_output":
                print(f"  Report formatted: {len(state['formatted_output'])} chars")

        elif event_type == "interrupt_before":
            print(f"⏸  Interrupt before: {node}")

        elif event_type == "interrupt_after":
            print(f"⏸  Interrupt after: {node}")

        elif event_type == "error":
            print(f"✗ Error at node {node}: {event.get('error')}")

        elif event_type == "final":
            print("\n" + "=" * 80)
            print("✓ Workflow completed!")
            print("=" * 80)
            final_state = event.get("state", {})

            if "formatted_output" in final_state:
                print("\n" + final_state["formatted_output"])

            print(f"\nFinal state keys: {list(final_state.keys())}")


def run_customer_support_agent():
    """Run the customer support agent example."""
    print("=" * 80)
    print("Running Customer Support Agent")
    print("=" * 80)

    engine = YAMLEngine()
    graph = engine.load_from_file("examples/yaml_customer_support_example.yaml")

    # Test different scenarios
    scenarios = [
        {
            "customer_id": "CUST-001",
            "customer_message": "I was charged twice for my subscription this month!",
        },
        {
            "customer_id": "CUST-002",
            "customer_message": "The app is not working and keeps showing an error.",
        },
        {
            "customer_id": "CUST-003",
            "customer_message": "I want to cancel my subscription and get a refund.",
        },
        {
            "customer_id": "CUST-004",
            "customer_message": "What are your business hours?",
        },
    ]

    for i, scenario in enumerate(scenarios, 1):
        print(f"\n{'─' * 80}")
        print(f"Scenario {i}: {scenario['customer_message'][:50]}...")
        print('─' * 80)

        # Run the graph
        for event in graph.stream(scenario):
            event_type = event.get("type")
            node = event.get("node")
            state = event.get("state", {})

            if event_type == "interrupt_after" and node == "classify_intent":
                print(f"\n📋 Classification Result:")
                print(f"   Intent: {state.get('intent', 'unknown')}")
                print(f"   Confidence: {state.get('confidence', 0):.2f}")
                print(f"   Escalate: {state.get('escalate', False)}")

            elif event_type == "state":
                if node.startswith("handle_"):
                    print(f"\n✓ Handled via: {node}")
                elif node == "send_response":
                    print(f"\n📧 Response sent")
                elif node == "escalate_to_human":
                    print(f"\n🚨 Escalated to human agent")

            elif event_type == "final":
                print(f"\n✓ Ticket {state.get('ticket_id', 'N/A')} processed")


def run_custom_yaml(yaml_path: str):
    """Run a custom YAML agent configuration."""
    print("=" * 80)
    print(f"Running agent from: {yaml_path}")
    print("=" * 80)

    engine = YAMLEngine()
    graph = engine.load_from_file(yaml_path)

    # Get initial state from user or use defaults
    print("\nEnter initial state as JSON (or press Enter for empty state):")
    user_input = input("> ").strip()

    if user_input:
        try:
            initial_state = json.loads(user_input)
        except json.JSONDecodeError:
            print("Invalid JSON, using empty state")
            initial_state = {}
    else:
        initial_state = {}

    print(f"\nStarting with state: {json.dumps(initial_state, indent=2)}\n")

    # Run the graph
    for event in graph.stream(initial_state):
        event_type = event.get("type")
        node = event.get("node")

        if event_type == "state":
            print(f"✓ {node}")

        elif event_type in ["interrupt_before", "interrupt_after"]:
            print(f"⏸  Interrupt at: {node}")
            state = event.get("state", {})
            print(f"   State: {json.dumps(state, indent=2)}")

        elif event_type == "error":
            print(f"✗ Error at {node}: {event.get('error')}")

        elif event_type == "final":
            print("\n" + "=" * 80)
            print("✓ Completed")
            print("=" * 80)
            print(f"Final state: {json.dumps(event.get('state', {}), indent=2)}")


def main():
    """Main entry point."""
    if len(sys.argv) > 1:
        yaml_path = sys.argv[1]
        run_custom_yaml(yaml_path)
    else:
        # Run built-in examples
        print("No YAML file specified. Running built-in examples...\n")

        run_research_agent()
        print("\n\n")
        run_customer_support_agent()

        print("\n\n" + "=" * 80)
        print("Examples completed!")
        print("=" * 80)
        print("\nTo run a custom YAML agent:")
        print("  python run_yaml_agent.py path/to/agent.yaml")


if __name__ == "__main__":
    main()
