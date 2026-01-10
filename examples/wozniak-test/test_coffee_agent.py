#!/usr/bin/env python3
"""
Test script for the Wozniak Coffee Agent with AI2-THOR.

This script tests the coffee-making agent directly without TEA,
useful for debugging and validating the AI2-THOR integration.

Usage:
    # Test with fixed scene (debugging)
    python examples/wozniak-test/test_coffee_agent.py --scene FloorPlan1

    # Test with procedural scene (zero-knowledge)
    python examples/wozniak-test/test_coffee_agent.py --scene Procedural

    # Headless mode (no display)
    python examples/wozniak-test/test_coffee_agent.py --headless

    # Record video of the navigation
    python examples/wozniak-test/test_coffee_agent.py --scene FloorPlan1 --record wozniak_test.mp4
"""

import argparse
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from ai2thor_helper import AI2THORHelper


def run_coffee_test(
    scene: str = "FloorPlan1",
    headless: bool = False,
    max_steps: int = 30,
    record: str | None = None,
):
    """Run the Wozniak Coffee Test.

    Args:
        scene: AI2-THOR scene name
        headless: Run without display
        max_steps: Maximum exploration steps
        record: Path to save video recording (MP4 or GIF)

    Returns:
        dict with test results
    """
    print(f"\n{'='*50}")
    print(f"WOZNIAK COFFEE TEST")
    print(f"Scene: {scene}")
    if record:
        print(f"Recording to: {record}")
    print(f"{'='*50}\n")

    # Initialize environment
    print("[1/8] Initializing AI2-THOR...")
    helper = AI2THORHelper(scene=scene, headless=headless)

    # Start recording if requested
    if record:
        helper.start_recording()

    # Get initial summary
    summary = helper.get_scene_summary()
    print(f"      Total objects: {summary['total_objects']}")
    print(f"      Coffee-related: {summary['coffee_related']}")
    print(f"      Coffee objects: {summary['coffee_objects']}")

    # Check current room
    current_room = helper.get_current_room()
    print(f"      Current room: {current_room}")

    # Phase 1.5: Find kitchen if not already there (for ProcTHOR)
    if current_room != "Kitchen" and scene == "Procedural":
        print("\n[1.5/8] Finding kitchen (starting outside)...")
        kitchen_found = helper.find_kitchen(max_steps=max_steps)
        if kitchen_found:
            current_room = helper.get_current_room()
            print(f"      Now in: {current_room}")
        else:
            print("      WARNING: Kitchen not found, continuing anyway...")

    # Phase 2: Scan for required items
    print("\n[2/8] Scanning for coffee items...")
    coffee_objects = helper.get_coffee_related_objects()

    found_items = {}
    for obj in coffee_objects:
        obj_type = obj["objectType"]
        if obj_type in ["CoffeeMachine", "Mug", "Cup", "Kettle"]:
            if obj_type not in found_items:
                found_items[obj_type] = {
                    "id": obj["objectId"],
                    "position": obj["position"],
                    "visible": obj["visible"],
                }
                print(f"      Found: {obj_type}")

    # Check what's missing
    required = ["CoffeeMachine"]
    container = (
        "Mug" if "Mug" in found_items else ("Cup" if "Cup" in found_items else None)
    )

    if not container:
        required.append("Mug/Cup")

    missing = [item for item in required if item not in found_items]

    # Phase 2: Explore if needed
    if missing:
        print(f"\n[3/8] Exploring for missing items: {missing}")

        # Try opening cabinets
        all_objects = helper.get_all_objects()
        cabinets = [
            o
            for o in all_objects
            if o["objectType"] in ["Cabinet", "Drawer"] and not o.get("isOpen", False)
        ]

        for i, cabinet in enumerate(cabinets[: min(5, len(cabinets))]):
            print(f"      Opening {cabinet['objectType']}...")
            helper.open_object(cabinet["objectId"])

            # Rescan after opening
            coffee_objects = helper.get_coffee_related_objects()
            for obj in coffee_objects:
                obj_type = obj["objectType"]
                if obj_type not in found_items and obj_type in ["Mug", "Cup"]:
                    found_items[obj_type] = {
                        "id": obj["objectId"],
                        "position": obj["position"],
                    }
                    print(f"      Discovered: {obj_type}")

        container = (
            "Mug" if "Mug" in found_items else ("Cup" if "Cup" in found_items else None)
        )
    else:
        print("\n[3/8] All required items visible, skipping exploration")

    # Phase 3: Navigate to coffee machine
    if "CoffeeMachine" in found_items:
        print("\n[4/8] Navigating to coffee machine...")
        machine = found_items["CoffeeMachine"]
        nav_success = helper.navigate_to_object(machine["id"])
        if nav_success:
            helper.rotate_to_object(machine["id"])
            print("      Arrived at coffee machine")
        else:
            print("      Navigation failed, trying to proceed anyway")
    else:
        print("\n[4/8] ERROR: No coffee machine found!")
        helper.stop()
        return {"success": False, "error": "No coffee machine"}

    # Phase 4: Pick up container
    if container:
        print(f"\n[5/8] Picking up {container}...")
        container_obj = found_items[container]
        helper.navigate_to_object(container_obj["id"])
        helper.rotate_to_object(container_obj["id"])
        helper.look_down()

        pickup_success = helper.pickup(container_obj["id"])
        if pickup_success:
            print(f"      Picked up {container}")
        else:
            print(f"      Could not pick up {container}")
    else:
        print("\n[5/8] WARNING: No container (mug/cup) found")
        pickup_success = False

    # Phase 5: Place container near machine
    if pickup_success:
        print("\n[6/8] Placing container near coffee machine...")
        machine = found_items["CoffeeMachine"]
        helper.navigate_to_object(machine["id"])

        # Find counter near machine
        all_objects = helper.get_all_objects()
        counters = [o for o in all_objects if o["objectType"] == "CounterTop"]

        if counters:
            # Sort by distance to machine
            machine_pos = machine["position"]
            counters.sort(
                key=lambda c: (
                    (c["position"]["x"] - machine_pos["x"]) ** 2
                    + (c["position"]["z"] - machine_pos["z"]) ** 2
                )
                ** 0.5
            )

            place_success = helper.put_on(counters[0]["objectId"])
            if place_success:
                print("      Placed container on counter")
            else:
                print("      Could not place container (continuing anyway)")
    else:
        print("\n[6/8] Skipping placement (no container held)")

    # Phase 6: Toggle coffee machine
    print("\n[7/8] Activating coffee machine...")
    machine = found_items["CoffeeMachine"]
    toggle_success = helper.toggle_on(machine["id"])

    machine_state = helper.get_object_state(machine["id"])
    is_on = machine_state.get("isToggled", False) if machine_state else False

    if toggle_success or is_on:
        print("      Coffee machine is ON!")
    else:
        print("      Could not activate (may need coffee/water)")

    # Final verification
    print("\n[8/8] Verification (Dona Maria Principle)...")
    print(f"\n{'='*50}")
    print("VERIFICATION (Dona Maria Principle)")
    print(f"{'='*50}")

    results = {
        "scene": scene,
        "coffee_machine_found": "CoffeeMachine" in found_items,
        "container_found": container is not None,
        "container_type": container,
        "machine_activated": is_on,
        "total_actions": len(helper.get_action_history()),
        "action_history": helper.get_action_history(),
    }

    print(
        f"Coffee Machine: {'FOUND' if results['coffee_machine_found'] else 'NOT FOUND'}"
    )
    print(f"Container: {results['container_type'] or 'NOT FOUND'}")
    print(f"Machine Status: {'ON' if results['machine_activated'] else 'OFF/READY'}")
    print(f"Total Actions: {results['total_actions']}")

    # Determine success
    success = results["coffee_machine_found"] and results["container_found"]
    results["success"] = success

    print(f"\n{'='*50}")
    if success:
        print("WOZNIAK COFFEE TEST: PASSED")
    else:
        print("WOZNIAK COFFEE TEST: NEEDS IMPROVEMENT")
    print(f"{'='*50}\n")

    # Save video if recording
    if record:
        helper.stop_recording()
        helper.save_video(record)
        results["video_path"] = record

    # Cleanup
    helper.stop()

    return results


def main():
    parser = argparse.ArgumentParser(description="Wozniak Coffee Test with AI2-THOR")
    parser.add_argument(
        "--scene",
        default="FloorPlan1",
        help="Scene name (FloorPlan1-30 or 'Procedural')",
    )
    parser.add_argument("--headless", action="store_true", help="Run without display")
    parser.add_argument(
        "--max-steps", type=int, default=30, help="Maximum exploration steps"
    )
    parser.add_argument(
        "--record",
        type=str,
        default=None,
        help="Path to save video recording (e.g., wozniak_test.mp4)",
    )

    args = parser.parse_args()

    results = run_coffee_test(
        scene=args.scene,
        headless=args.headless,
        max_steps=args.max_steps,
        record=args.record,
    )

    # Return exit code based on success
    sys.exit(0 if results["success"] else 1)


if __name__ == "__main__":
    main()
