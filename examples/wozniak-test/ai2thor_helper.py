"""
AI2-THOR Helper Module for Wozniak Coffee Test

This module provides a simplified interface to AI2-THOR for the coffee-making agent.
It handles environment initialization, object detection, and action execution.

Usage:
    from ai2thor_helper import AI2THORHelper

    helper = AI2THORHelper(scene="FloorPlan1")
    objects = helper.get_visible_objects()
    helper.navigate_to_object("CounterTop")
    helper.pickup("Mug|1|2|3")
"""

from typing import Optional
from pathlib import Path


class AI2THORHelper:
    """Helper class for AI2-THOR environment interaction."""

    # Coffee-related object types
    COFFEE_OBJECTS = {
        "CoffeeMachine",
        "Mug",
        "Cup",
        "Kettle",
        "StoveKnob",
        "Faucet",
        "SinkBasin",
        "Cabinet",
        "CounterTop",
        "Drawer",
    }

    def __init__(self, scene: str = "FloorPlan1", headless: bool = False):
        """Initialize AI2-THOR controller.

        Args:
            scene: Scene name (FloorPlan1-30 for kitchens, "Procedural" for ProcTHOR)
            headless: Run without display
        """
        from ai2thor.controller import Controller

        self.scene = scene
        self.headless = headless

        # Check if using ProcTHOR
        if scene == "Procedural":
            self._init_procthor(headless)
        else:
            self.controller = Controller(
                scene=scene,
                gridSize=0.25,
                rotateStepDegrees=90,
                renderDepthImage=False,
                renderInstanceSegmentation=False,
                headless=headless,
                width=640,
                height=480,
            )

        self.held_object = None
        self.action_history = []

        # Video recording
        self.frames = []
        self.recording = False

    def start_recording(self):
        """Start recording frames for video."""
        self.frames = []
        self.recording = True
        # Capture initial frame
        self._capture_frame()
        print("[VIDEO] Recording started")

    def stop_recording(self):
        """Stop recording frames."""
        self.recording = False
        print(f"[VIDEO] Recording stopped. {len(self.frames)} frames captured.")

    def _capture_frame(self, copies: int = 1):
        """Capture current frame if recording.

        Args:
            copies: Number of copies to add (for slower playback)
        """
        if not self.recording:
            return
        try:
            if self.controller.last_event is not None:
                frame = self.controller.last_event.frame
                if frame is not None:
                    # Add multiple copies for smoother/slower video
                    for _ in range(copies):
                        self.frames.append(frame.copy())
        except Exception:
            pass  # Silently ignore frame capture errors

    def save_video(self, output_path: str = "wozniak_test.mp4", fps: int = 5):
        """Save recorded frames as MP4 video.

        Args:
            output_path: Path to save the video
            fps: Frames per second

        Returns:
            True if successful, False otherwise
        """
        if not self.frames:
            print("[VIDEO] No frames to save!")
            return False

        try:
            import cv2
            import numpy as np

            # Get frame dimensions from first frame
            height, width = self.frames[0].shape[:2]

            # Create video writer
            fourcc = cv2.VideoWriter_fourcc(*"mp4v")
            out = cv2.VideoWriter(output_path, fourcc, fps, (width, height))

            for frame in self.frames:
                # Convert RGB to BGR for OpenCV
                bgr_frame = cv2.cvtColor(frame, cv2.COLOR_RGB2BGR)
                out.write(bgr_frame)

            out.release()
            print(f"[VIDEO] Saved {len(self.frames)} frames to {output_path}")
            return True

        except ImportError:
            print(
                "[VIDEO] OpenCV not installed. Install with: pip install opencv-python"
            )
            # Fallback: save as GIF using PIL
            return self._save_gif(output_path.replace(".mp4", ".gif"), fps)

    def _save_gif(self, output_path: str, fps: int = 10):
        """Fallback: save as GIF using PIL."""
        try:
            from PIL import Image

            images = [Image.fromarray(frame) for frame in self.frames]
            duration = int(1000 / fps)  # milliseconds per frame

            images[0].save(
                output_path,
                save_all=True,
                append_images=images[1:],
                duration=duration,
                loop=0,
            )
            print(f"[VIDEO] Saved {len(self.frames)} frames as GIF to {output_path}")
            return True
        except ImportError:
            print("[VIDEO] PIL not installed. Install with: pip install Pillow")
            return False

    def _init_procthor(
        self, headless: bool = False, start_outside_kitchen: bool = True
    ):
        """Initialize with ProcTHOR-10K procedural house."""
        from ai2thor.controller import Controller

        try:
            import prior

            # Load ProcTHOR-10K dataset
            dataset = prior.load_dataset("procthor-10k")
            # Get a random house from the training set
            import random

            house_idx = random.randint(0, len(dataset["train"]) - 1)
            house = dataset["train"][house_idx]

            print(f"[ProcTHOR] Loading house #{house_idx} from ProcTHOR-10K")

            self.controller = Controller(
                gridSize=0.25,
                rotateStepDegrees=90,
                renderDepthImage=False,
                headless=headless,
                width=640,
                height=480,
            )
            # Reset with the procedural house
            self.controller.reset(scene=house)
            self.scene = f"ProcTHOR-{house_idx}"
            self.house_data = house

            # Try to spawn outside the kitchen for true zero-knowledge
            if start_outside_kitchen:
                self._spawn_outside_kitchen()

        except ImportError:
            print(
                "[WARNING] 'prior' package not installed. Install with: pip install prior"
            )
            print("[WARNING] Falling back to FloorPlan1")
            self.controller = Controller(
                scene="FloorPlan1",
                gridSize=0.25,
                rotateStepDegrees=90,
                headless=headless,
                width=640,
                height=480,
            )
            self.scene = "FloorPlan1"
        except Exception as e:
            print(f"[WARNING] ProcTHOR init failed: {e}")
            print("[WARNING] Falling back to FloorPlan1")
            self.controller = Controller(
                scene="FloorPlan1",
                gridSize=0.25,
                rotateStepDegrees=90,
                headless=headless,
                width=640,
                height=480,
            )
            self.scene = "FloorPlan1"

    def _spawn_outside_kitchen(self):
        """Spawn the agent in a non-kitchen room."""
        import random

        # Get all rooms in the house
        rooms = self._get_rooms()
        if not rooms:
            print("[SPAWN] Could not identify rooms, using default position")
            return

        # Find non-kitchen rooms
        non_kitchen_rooms = [r for r in rooms if r["type"].lower() != "kitchen"]

        if not non_kitchen_rooms:
            print("[SPAWN] No non-kitchen rooms found, using default position")
            return

        # Choose a random non-kitchen room (prefer living room or bedroom)
        preferred = [
            r
            for r in non_kitchen_rooms
            if any(
                t in r["type"].lower()
                for t in ["living", "bedroom", "entrance", "hallway"]
            )
        ]

        if preferred:
            spawn_room = random.choice(preferred)
        else:
            spawn_room = random.choice(non_kitchen_rooms)

        print(f"[SPAWN] Starting in: {spawn_room['type']} (not kitchen)")

        # Get reachable positions in this room
        event = self.controller.step(action="GetReachablePositions")
        if not event.metadata["lastActionSuccess"]:
            return

        positions = event.metadata["actionReturn"]

        # Filter positions that are in the spawn room
        room_positions = []
        room_center = spawn_room.get("center", spawn_room.get("position", {}))
        room_size = spawn_room.get("size", {"x": 3, "z": 3})

        for pos in positions:
            # Check if position is roughly in the room
            if room_center:
                dx = abs(pos["x"] - room_center.get("x", 0))
                dz = abs(pos["z"] - room_center.get("z", 0))
                if dx < room_size.get("x", 3) and dz < room_size.get("z", 3):
                    room_positions.append(pos)

        if room_positions:
            spawn_pos = random.choice(room_positions)
            self.controller.step(
                action="Teleport",
                position=spawn_pos,
                rotation=dict(x=0, y=random.choice([0, 90, 180, 270]), z=0),
            )
            print(
                f"[SPAWN] Teleported to position: ({spawn_pos['x']:.1f}, {spawn_pos['z']:.1f})"
            )
        else:
            # Just use any position far from kitchen objects
            self._spawn_far_from_kitchen(positions)

    def _spawn_far_from_kitchen(self, positions: list):
        """Spawn at a position far from kitchen objects."""
        import random

        # Find kitchen objects
        kitchen_objects = [
            o
            for o in self.get_all_objects()
            if o["objectType"]
            in ["CoffeeMachine", "Toaster", "Microwave", "StoveBurner", "Fridge"]
        ]

        if not kitchen_objects or not positions:
            return

        # Calculate center of kitchen objects
        kitchen_x = sum(o["position"]["x"] for o in kitchen_objects) / len(
            kitchen_objects
        )
        kitchen_z = sum(o["position"]["z"] for o in kitchen_objects) / len(
            kitchen_objects
        )

        # Sort positions by distance from kitchen (farthest first)
        positions.sort(
            key=lambda p: -(
                ((p["x"] - kitchen_x) ** 2 + (p["z"] - kitchen_z) ** 2) ** 0.5
            )
        )

        # Take one of the farthest positions
        spawn_pos = random.choice(positions[: max(1, len(positions) // 4)])

        self.controller.step(
            action="Teleport",
            position=spawn_pos,
            rotation=dict(x=0, y=random.choice([0, 90, 180, 270]), z=0),
        )
        print(
            f"[SPAWN] Teleported far from kitchen: ({spawn_pos['x']:.1f}, {spawn_pos['z']:.1f})"
        )

    def _get_rooms(self) -> list:
        """Get list of rooms in the house (for ProcTHOR)."""
        rooms = []

        # Try to get room info from house data
        if hasattr(self, "house_data") and self.house_data:
            if "rooms" in self.house_data:
                for room in self.house_data["rooms"]:
                    room_type = room.get("roomType", room.get("type", "Unknown"))
                    rooms.append(
                        {
                            "type": room_type,
                            "center": (
                                room.get("floorPolygon", [{}])[0]
                                if room.get("floorPolygon")
                                else {}
                            ),
                            "id": room.get("id", ""),
                        }
                    )

        # Fallback: infer rooms from objects
        if not rooms:
            rooms = self._infer_rooms_from_objects()

        return rooms

    def _infer_rooms_from_objects(self) -> list:
        """Infer room types from objects present."""
        rooms = []
        objects = self.get_all_objects()
        obj_types = set(o["objectType"] for o in objects)

        # Kitchen indicators
        if any(
            t in obj_types
            for t in ["CoffeeMachine", "Toaster", "Microwave", "StoveBurner", "Fridge"]
        ):
            rooms.append({"type": "Kitchen", "center": None})

        # Living room indicators
        if any(t in obj_types for t in ["Sofa", "Television", "RemoteControl"]):
            rooms.append({"type": "LivingRoom", "center": None})

        # Bedroom indicators
        if any(t in obj_types for t in ["Bed", "Pillow", "AlarmClock"]):
            rooms.append({"type": "Bedroom", "center": None})

        # Bathroom indicators
        if any(t in obj_types for t in ["Toilet", "ShowerHead", "Bathtub"]):
            rooms.append({"type": "Bathroom", "center": None})

        return rooms

    def get_current_room(self) -> str:
        """Estimate which room the agent is currently in."""
        agent_pos = self.get_agent_position()["position"]
        visible = self.get_visible_objects()
        visible_types = set(o["objectType"] for o in visible)

        # Check for room-specific objects
        if any(
            t in visible_types
            for t in [
                "CoffeeMachine",
                "Toaster",
                "Microwave",
                "StoveBurner",
                "Fridge",
                "Sink",
            ]
        ):
            return "Kitchen"
        if any(t in visible_types for t in ["Sofa", "Television"]):
            return "LivingRoom"
        if any(t in visible_types for t in ["Bed", "Pillow"]):
            return "Bedroom"
        if any(t in visible_types for t in ["Toilet", "ShowerHead"]):
            return "Bathroom"

        return "Unknown"

    def find_kitchen(self, max_steps: int = 50) -> bool:
        """Explore the house to find the kitchen.

        Returns:
            True if kitchen found, False otherwise
        """
        print("[EXPLORE] Searching for kitchen...")

        for step in range(max_steps):
            # Check current room
            current_room = self.get_current_room()
            if current_room == "Kitchen":
                print(f"[EXPLORE] Found kitchen after {step} steps!")
                return True

            # Look for kitchen objects
            coffee_machine = self.find_object_by_type("CoffeeMachine")
            if coffee_machine and coffee_machine["visible"]:
                print(f"[EXPLORE] Found CoffeeMachine - must be kitchen!")
                return True

            # Exploration strategy: rotate and move
            if step % 4 == 0:
                self.rotate_right()
            elif step % 4 == 1:
                self.rotate_right()
            elif step % 4 == 2:
                self.rotate_right()
            else:
                # Try to move forward
                if not self.move_forward():
                    # If blocked, try rotating
                    self.rotate_right()
                    self.move_forward()

            # Check if we can see a door/passage
            visible = self.get_visible_objects()
            doors = [o for o in visible if "Door" in o["objectType"]]
            if doors:
                # Navigate towards door
                self.navigate_to_object(doors[0]["objectId"])

        print(f"[EXPLORE] Kitchen not found after {max_steps} steps")
        return False

    def get_all_objects(self) -> list:
        """Get all objects in the scene."""
        return self.controller.last_event.metadata["objects"]

    def get_visible_objects(self) -> list:
        """Get only visible objects."""
        return [o for o in self.get_all_objects() if o["visible"]]

    def get_coffee_related_objects(self) -> list:
        """Get objects relevant to coffee making."""
        all_objects = self.get_all_objects()
        return [
            o
            for o in all_objects
            if o["objectType"] in self.COFFEE_OBJECTS
            or any(
                k in o["objectType"].lower() for k in ["coffee", "mug", "cup", "kettle"]
            )
        ]

    def find_object_by_type(self, object_type: str) -> Optional[dict]:
        """Find first object of given type."""
        for obj in self.get_all_objects():
            if obj["objectType"] == object_type:
                return obj
        return None

    def get_agent_position(self) -> dict:
        """Get current agent position and rotation."""
        meta = self.controller.last_event.metadata
        return {
            "position": meta["agent"]["position"],
            "rotation": meta["agent"]["rotation"]["y"],
            "cameraHorizon": meta["agent"]["cameraHorizon"],
        }

    def navigate_to_object(self, object_id: str) -> bool:
        """Navigate close to an object.

        Uses GetShortestPath to find path, then teleports.
        """
        obj = None
        for o in self.get_all_objects():
            if o["objectId"] == object_id:
                obj = o
                break

        if not obj:
            return False

        # Try to get reachable position near object
        event = self.controller.step(action="GetReachablePositions")

        if not event.metadata["lastActionSuccess"]:
            return False

        # Find closest reachable position to object
        obj_pos = obj["position"]
        positions = event.metadata["actionReturn"]

        if not positions:
            return False

        # Sort by distance to object
        def dist(p):
            return ((p["x"] - obj_pos["x"]) ** 2 + (p["z"] - obj_pos["z"]) ** 2) ** 0.5

        positions.sort(key=dist)

        # Teleport to closest position
        target = positions[0]
        event = self.controller.step(action="Teleport", position=target)

        self._log_action("navigate_to", object_id, event.metadata["lastActionSuccess"])
        # Extra frames for smoother video (hold on this view)
        self._capture_frame(copies=10)
        return event.metadata["lastActionSuccess"]

    def rotate_to_object(self, object_id: str) -> bool:
        """Rotate to face an object."""
        obj = None
        for o in self.get_all_objects():
            if o["objectId"] == object_id:
                obj = o
                break

        if not obj:
            return False

        # Calculate direction to object
        agent_pos = self.get_agent_position()["position"]
        obj_pos = obj["position"]

        import math

        dx = obj_pos["x"] - agent_pos["x"]
        dz = obj_pos["z"] - agent_pos["z"]
        target_rotation = math.degrees(math.atan2(dx, dz))

        event = self.controller.step(
            action="Teleport",
            position=agent_pos,
            rotation=dict(x=0, y=target_rotation, z=0),
        )

        self._capture_frame(copies=5)
        return event.metadata["lastActionSuccess"]

    def pickup(self, object_id: str) -> bool:
        """Pick up an object."""
        event = self.controller.step(action="PickupObject", objectId=object_id)

        success = event.metadata["lastActionSuccess"]
        if success:
            self.held_object = object_id

        self._log_action("pickup", object_id, success)
        return success

    def put_on(self, receptacle_id: str) -> bool:
        """Put held object on a receptacle."""
        if not self.held_object:
            return False

        event = self.controller.step(action="PutObject", objectId=receptacle_id)

        success = event.metadata["lastActionSuccess"]
        if success:
            self.held_object = None

        self._log_action("put_on", receptacle_id, success)
        return success

    def open_object(self, object_id: str) -> bool:
        """Open a container (cabinet, drawer, fridge)."""
        event = self.controller.step(action="OpenObject", objectId=object_id)

        self._log_action("open", object_id, event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def close_object(self, object_id: str) -> bool:
        """Close a container."""
        event = self.controller.step(action="CloseObject", objectId=object_id)

        self._log_action("close", object_id, event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def toggle_on(self, object_id: str) -> bool:
        """Turn on an object (coffee machine, stove, faucet)."""
        event = self.controller.step(action="ToggleObjectOn", objectId=object_id)

        self._log_action("toggle_on", object_id, event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def toggle_off(self, object_id: str) -> bool:
        """Turn off an object."""
        event = self.controller.step(action="ToggleObjectOff", objectId=object_id)

        self._log_action("toggle_off", object_id, event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def fill_with_liquid(self, object_id: str, liquid_type: str = "water") -> bool:
        """Fill an object with liquid (requires being near sink/faucet)."""
        event = self.controller.step(
            action="FillObjectWithLiquid", objectId=object_id, fillLiquid=liquid_type
        )

        self._log_action("fill", object_id, event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def look_at(self, object_id: str) -> bool:
        """Look at an object."""
        event = self.controller.step(action="LookAtObjectCenter", objectId=object_id)

        return event.metadata["lastActionSuccess"]

    def move_forward(self) -> bool:
        """Move agent forward."""
        event = self.controller.step(action="MoveAhead")
        self._log_action("move", "forward", event.metadata["lastActionSuccess"])
        return event.metadata["lastActionSuccess"]

    def rotate_left(self) -> bool:
        """Rotate agent left."""
        event = self.controller.step(action="RotateLeft")
        self._capture_frame(copies=3)
        return event.metadata["lastActionSuccess"]

    def rotate_right(self) -> bool:
        """Rotate agent right."""
        event = self.controller.step(action="RotateRight")
        self._capture_frame(copies=3)
        return event.metadata["lastActionSuccess"]

    def look_down(self) -> bool:
        """Look down."""
        event = self.controller.step(action="LookDown")
        self._capture_frame(copies=3)
        return event.metadata["lastActionSuccess"]

    def look_up(self) -> bool:
        """Look up."""
        event = self.controller.step(action="LookUp")
        self._capture_frame(copies=3)
        return event.metadata["lastActionSuccess"]

    def get_object_state(self, object_id: str) -> Optional[dict]:
        """Get detailed state of an object."""
        for obj in self.get_all_objects():
            if obj["objectId"] == object_id:
                return {
                    "type": obj["objectType"],
                    "position": obj["position"],
                    "visible": obj["visible"],
                    "pickupable": obj["pickupable"],
                    "toggleable": obj["toggleable"],
                    "isToggled": obj.get("isToggled", False),
                    "openable": obj["openable"],
                    "isOpen": obj.get("isOpen", False),
                    "receptacle": obj["receptacle"],
                    "isFilledWithLiquid": obj.get("isFilledWithLiquid", False),
                    "fillLiquid": obj.get("fillLiquid", None),
                }
        return None

    def get_scene_summary(self) -> dict:
        """Get a summary of the current scene."""
        all_objects = self.get_all_objects()
        visible = [o for o in all_objects if o["visible"]]
        coffee_related = self.get_coffee_related_objects()

        # Count by type
        type_counts = {}
        for obj in all_objects:
            t = obj["objectType"]
            type_counts[t] = type_counts.get(t, 0) + 1

        return {
            "scene": self.scene,
            "total_objects": len(all_objects),
            "visible_objects": len(visible),
            "coffee_related": len(coffee_related),
            "agent_position": self.get_agent_position(),
            "held_object": self.held_object,
            "object_types": type_counts,
            "coffee_objects": [o["objectType"] for o in coffee_related],
        }

    def _log_action(self, action_type: str, target: str, success: bool):
        """Log an action for debugging."""
        self.action_history.append(
            {"action": action_type, "target": target, "success": success}
        )
        # Capture multiple frames after action for smoother video
        self._capture_frame(copies=5)

    def get_action_history(self) -> list:
        """Get history of actions taken."""
        return self.action_history

    def reset(self, scene: Optional[str] = None):
        """Reset the environment."""
        if scene:
            self.scene = scene
        self.controller.reset(scene=self.scene)
        self.held_object = None
        self.action_history = []

    def stop(self):
        """Stop the controller."""
        self.controller.stop()


def to_prolog_facts(objects: list) -> str:
    """Convert AI2-THOR objects to Prolog facts.

    Args:
        objects: List of AI2-THOR object metadata

    Returns:
        String of Prolog facts
    """
    facts = []

    for obj in objects:
        obj_type = obj["objectType"].lower()
        obj_id = obj["objectId"].replace("|", "_").replace("-", "_")
        pos = obj["position"]

        # Object existence
        facts.append(f"object({obj_type}, '{obj_id}').")

        # Position
        facts.append(
            f"position('{obj_id}', {pos['x']:.2f}, {pos['y']:.2f}, {pos['z']:.2f})."
        )

        # Properties
        if obj["visible"]:
            facts.append(f"visible('{obj_id}').")
        if obj["pickupable"]:
            facts.append(f"pickupable('{obj_id}').")
        if obj["toggleable"]:
            facts.append(f"toggleable('{obj_id}').")
        if obj.get("isToggled", False):
            facts.append(f"toggled_on('{obj_id}').")
        if obj["openable"]:
            facts.append(f"openable('{obj_id}').")
        if obj.get("isOpen", False):
            facts.append(f"is_open('{obj_id}').")
        if obj["receptacle"]:
            facts.append(f"receptacle('{obj_id}').")
        if obj.get("isFilledWithLiquid", False):
            liquid = obj.get("fillLiquid", "water")
            facts.append(f"filled_with('{obj_id}', {liquid}).")

    return "\n".join(facts)


# Quick test
if __name__ == "__main__":
    print("Testing AI2-THOR Helper...")

    helper = AI2THORHelper(scene="FloorPlan1")

    # Get scene summary
    summary = helper.get_scene_summary()
    print(f"\nScene: {summary['scene']}")
    print(f"Total objects: {summary['total_objects']}")
    print(f"Visible objects: {summary['visible_objects']}")
    print(f"Coffee-related: {summary['coffee_related']}")
    print(f"Coffee objects: {summary['coffee_objects']}")

    # Find coffee machine
    coffee_machine = helper.find_object_by_type("CoffeeMachine")
    if coffee_machine:
        print(f"\nCoffeeMachine found: {coffee_machine['objectId']}")
        print(f"  Position: {coffee_machine['position']}")
        print(f"  Toggleable: {coffee_machine['toggleable']}")

    # Find mug
    mug = helper.find_object_by_type("Mug")
    if mug:
        print(f"\nMug found: {mug['objectId']}")
        print(f"  Pickupable: {mug['pickupable']}")

    helper.stop()
    print("\nTest complete!")
