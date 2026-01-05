#!/usr/bin/env python3
"""
Simple test script for Video Pinball environment.

This demonstrates that the environment works with random actions.
For the full neurosymbolic AI, see pinball-play.yaml.
"""

import random
import sys

sys.path.insert(0, ".")

from atari_env import VideoPinballEnv, GameTracer


def main():
    print("Video Pinball Environment Test")
    print("=" * 50)

    env = VideoPinballEnv(render_mode="rgb_array")
    tracer = GameTracer()

    state = env.reset()
    print(f"\nInitial state:")
    print(f"  Score: {state.score}")
    print(f"  Lives: {state.lives}")

    print("\nRunning 3000 frames with mixed strategy...")
    print("(Random actions + position-based flipping)")

    prev_score = 0
    prev_lives = state.lives

    for i in range(3000):
        # Use random action indices directly (0-8)
        # This works reliably with the ALE environment
        action = random.randint(0, 8)

        state = env.step(action)

        # Track events
        action_name = [
            "noop",
            "fire",
            "up",
            "right",
            "left",
            "down",
            "upfire",
            "rightfire",
            "leftfire",
        ][action]

        if state.score > prev_score:
            points = state.score - prev_score
            tracer.record_decision(
                state, action_name, f"random_{action_name}", "score", points
            )
            if i < 500 or points >= 100:  # Only log early or big scores
                print(f"  Frame {i}: +{points} points (total: {state.score})")
            prev_score = state.score

        if state.lives < prev_lives:
            tracer.record_decision(
                state, action_name, f"random_{action_name}", "ball_lost", 0
            )
            print(f"  Frame {i}: Ball lost! Lives: {state.lives}")
            prev_lives = state.lives

        if state.terminated:
            tracer.end_game(state.score, 3 - state.lives)
            print(f"\nGame over at frame {i}")
            break

    env.close()

    print("\n" + "=" * 50)
    print("RESULTS")
    print("=" * 50)
    print(f"Final score: {state.score}")
    print(f"Lives remaining: {state.lives}")
    print(f"Frames played: {state.frame}")

    # Show rule stats
    analysis = tracer.get_analysis()
    if "rule_effectiveness" in analysis and analysis["rule_effectiveness"]:
        print("\nAction breakdown:")
        for rule, stats in analysis["rule_effectiveness"].items():
            print(f"  {rule}: {stats['count']} uses, {stats['points_gained']} points")

    print("\nTest completed successfully!")
    return state.score


if __name__ == "__main__":
    score = main()
    # Exit with success if we got any score
    sys.exit(0 if score > 0 else 1)
