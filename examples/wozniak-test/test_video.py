#!/usr/bin/env python3
"""Quick test for video recording."""

from ai2thor.controller import Controller

print("Testing video recording directly with AI2-THOR...")

# Initialize with explicit image rendering
controller = Controller(
    scene="FloorPlan1", gridSize=0.25, rotateStepDegrees=90, width=640, height=480
)

print("Controller initialized!")

# Check if frame is available
frame = controller.last_event.frame
print(f"Frame type: {type(frame)}")
print(f"Frame shape: {frame.shape if frame is not None else 'None'}")

# Collect some frames
frames = []
frames.append(frame.copy())

for i in range(4):
    controller.step(action="RotateRight")
    f = controller.last_event.frame
    if f is not None:
        frames.append(f.copy())
    print(f"After rotate {i+1}: {len(frames)} frames")

print(f"\nTotal frames: {len(frames)}")

# Save as video
if frames:
    try:
        import cv2

        height, width = frames[0].shape[:2]
        fourcc = cv2.VideoWriter_fourcc(*"mp4v")
        out = cv2.VideoWriter("test_video.mp4", fourcc, 5, (width, height))

        for frame in frames:
            bgr = cv2.cvtColor(frame, cv2.COLOR_RGB2BGR)
            out.write(bgr)

        out.release()
        print("Saved to test_video.mp4")
    except ImportError:
        print("OpenCV not installed, trying PIL...")
        from PIL import Image

        images = [Image.fromarray(f) for f in frames]
        images[0].save(
            "test_video.gif",
            save_all=True,
            append_images=images[1:],
            duration=200,
            loop=0,
        )
        print("Saved to test_video.gif")

controller.stop()
print("Done!")
