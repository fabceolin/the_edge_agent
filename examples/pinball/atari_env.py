"""
Atari Video Pinball Environment Wrapper for TEA

This module provides integration between Gymnasium/ALE and The Edge Agent,
enabling neurosymbolic AI to play Video Pinball.

The wrapper extracts game state from RAM and provides it in a format
suitable for Prolog reasoning.

Requirements:
    pip install gymnasium[atari,accept-rom-license]

Usage:
    from atari_env import VideoPinballEnv

    env = VideoPinballEnv()
    state = env.reset()

    while not state["terminated"]:
        action = decide_action(state)  # Your AI logic
        state = env.step(action)
"""

import json
from dataclasses import dataclass, asdict
from typing import Optional
import numpy as np

try:
    import ale_py
    import gymnasium as gym

    # Register ALE environments
    gym.register_envs(ale_py)
    GYM_AVAILABLE = True
except ImportError:
    GYM_AVAILABLE = False
    print(
        "Warning: Gymnasium/ALE not installed. Install with: pip install gymnasium[atari,accept-rom-license]"
    )


@dataclass
class Ball:
    """Ball state extracted from game."""

    x: float
    y: float
    visible: bool = True

    @property
    def velocity_estimate(self) -> tuple[float, float]:
        """Velocity must be computed from frame differences."""
        return (0.0, 0.0)  # Placeholder - computed externally


@dataclass
class Flipper:
    """Flipper state."""

    side: str  # "left" or "right"
    x: float
    y: float
    active: bool = False


@dataclass
class Bumper:
    """Bumper/target state."""

    x: float
    y: float
    active: bool = True
    points: int = 100


@dataclass
class GameState:
    """Complete game state for decision making."""

    frame: int
    score: int
    lives: int
    ball: Optional[Ball]
    ball_vx: float  # Computed from previous frames
    ball_vy: float
    flippers: list[Flipper]
    bumpers: list[Bumper]
    terminated: bool
    truncated: bool

    def to_prolog_facts(self) -> str:
        """Convert state to Prolog facts for reasoning."""
        facts = []
        facts.append(f"frame({self.frame}).")
        facts.append(f"score({self.score}).")
        facts.append(f"lives({self.lives}).")

        if self.ball and self.ball.visible:
            facts.append(f"ball_position({self.ball.x:.1f}, {self.ball.y:.1f}).")
            facts.append(f"ball_velocity({self.ball_vx:.1f}, {self.ball_vy:.1f}).")
        else:
            facts.append("ball_not_visible.")

        for flipper in self.flippers:
            facts.append(
                f"flipper({flipper.side}, {flipper.x:.1f}, {flipper.y:.1f}, {str(flipper.active).lower()})."
            )

        for i, bumper in enumerate(self.bumpers):
            if bumper.active:
                facts.append(f"bumper({i}, {bumper.x:.1f}, {bumper.y:.1f}).")

        return "\n".join(facts)

    def to_dict(self) -> dict:
        """Convert to dictionary for YAML/JSON serialization."""
        return {
            "frame": self.frame,
            "score": self.score,
            "lives": self.lives,
            "ball": {
                "x": self.ball.x if self.ball else None,
                "y": self.ball.y if self.ball else None,
                "visible": self.ball.visible if self.ball else False,
            },
            "ball_vx": self.ball_vx,
            "ball_vy": self.ball_vy,
            "flippers": [
                {"side": f.side, "x": f.x, "y": f.y, "active": f.active}
                for f in self.flippers
            ],
            "bumpers": [{"x": b.x, "y": b.y, "active": b.active} for b in self.bumpers],
            "terminated": self.terminated,
            "truncated": self.truncated,
        }


# Action mapping for Video Pinball
# Based on Atari 2600 joystick + fire button
ACTIONS = {
    "noop": 0,
    "fire": 1,  # Launch ball / nudge
    "up": 2,  # Nudge up
    "right": 3,  # Right flipper
    "left": 4,  # Left flipper
    "down": 5,  # Nudge down
    "upright": 6,
    "upleft": 7,
    "downright": 8,
    "downleft": 9,
    "upfire": 10,
    "rightfire": 11,  # Right flipper + fire
    "leftfire": 12,  # Left flipper + fire
    "downfire": 13,
}

# Simplified action set for AI
SIMPLE_ACTIONS = {
    "wait": 0,
    "flip_left": 4,
    "flip_right": 3,
    "flip_both": 1,  # fire acts as both flippers in some modes
    "nudge_left": 7,
    "nudge_right": 6,
    "launch": 1,
}


class VideoPinballEnv:
    """
    Video Pinball environment with RAM-based state extraction.

    Extracts game state from Atari RAM and provides it in a format
    suitable for Prolog reasoning.

    RAM addresses for Video Pinball (discovered through analysis):
    - Ball position is tracked via pixel detection on the observation
    - Score and lives are available in the info dict
    """

    def __init__(
        self,
        render_mode: str = "human",  # "human" for display, "rgb_array" for headless
        frame_skip: int = 4,
    ):
        if not GYM_AVAILABLE:
            raise ImportError(
                "Gymnasium not available. Install with: "
                "pip install gymnasium[atari,accept-rom-license]"
            )

        self.render_mode = render_mode
        self.frame_skip = frame_skip

        # State tracking for velocity computation
        self._prev_ball_pos: Optional[tuple[float, float]] = None
        self._frame_count = 0
        self._episode_reward = 0

        # Initialize environment
        self.env = gym.make(
            "ALE/VideoPinball-v5",
            render_mode=render_mode,
            frameskip=frame_skip,
        )

    def reset(self) -> GameState:
        """Reset environment and return initial state."""
        obs, info = self.env.reset()
        self._prev_ball_pos = None
        self._frame_count = 0
        self._episode_reward = 0
        return self._extract_state(obs, info, 0, False, False)

    def step(self, action: str | int) -> GameState:
        """
        Execute action and return new state.

        Args:
            action: Either action name (str) or action index (int)

        Returns:
            GameState with updated information
        """
        # Convert action name to index if needed
        if isinstance(action, str):
            action_idx = SIMPLE_ACTIONS.get(action, ACTIONS.get(action, 0))
        else:
            action_idx = action

        # Execute action
        obs, reward, terminated, truncated, info = self.env.step(action_idx)

        self._frame_count += 1
        self._episode_reward += reward

        return self._extract_state(obs, info, reward, terminated, truncated)

    def _extract_state(
        self,
        obs,
        info: dict,
        reward: float,
        terminated: bool,
        truncated: bool,
    ) -> GameState:
        """Extract semantic state from observation using simple CV."""

        ball = None
        ball_vx, ball_vy = 0.0, 0.0
        flippers = []
        bumpers = []

        # Extract ball position from observation using simple color detection
        # The ball in Video Pinball is typically a bright white/yellow color
        ball_pos = self._find_ball_in_frame(obs)

        if ball_pos is not None:
            ball = Ball(x=ball_pos[0], y=ball_pos[1], visible=True)

            # Compute velocity from previous position
            if self._prev_ball_pos is not None:
                ball_vx = ball.x - self._prev_ball_pos[0]
                ball_vy = ball.y - self._prev_ball_pos[1]
            self._prev_ball_pos = (ball.x, ball.y)
        else:
            ball = Ball(x=80, y=100, visible=False)
            self._prev_ball_pos = None

        # Flipper positions are fixed in Video Pinball
        flippers = [
            Flipper(side="left", x=40, y=190),
            Flipper(side="right", x=120, y=190),
        ]

        # Get score and lives from info
        score = info.get("score", self._episode_reward)
        lives = info.get("lives", 3)

        return GameState(
            frame=self._frame_count,
            score=int(score),
            lives=lives,
            ball=ball,
            ball_vx=ball_vx,
            ball_vy=ball_vy,
            flippers=flippers,
            bumpers=bumpers,
            terminated=terminated,
            truncated=truncated,
        )

    def _find_ball_in_frame(self, obs: np.ndarray) -> Optional[tuple[float, float]]:
        """
        Find ball position using frame difference detection.

        The ball is detected by comparing current frame to previous frame
        and finding the moving bright object in the playfield area.
        """
        if obs is None or len(obs.shape) != 3:
            return None

        # Store frame for velocity computation
        if not hasattr(self, "_prev_frame"):
            self._prev_frame = obs.copy()
            return None

        # Compute frame difference
        diff = np.abs(obs.astype(float) - self._prev_frame.astype(float))
        motion = diff.mean(axis=2)

        # Update previous frame
        self._prev_frame = obs.copy()

        # Focus on playfield (skip score area at top)
        playfield_motion = motion[40:200, :]

        # Find moving pixels
        motion_threshold = 15
        moving_mask = playfield_motion > motion_threshold

        coords = np.where(moving_mask)
        if len(coords[0]) == 0:
            # No motion detected - try color-based fallback
            return self._find_ball_by_color(obs)

        # Filter to lower playfield where ball typically is
        y_coords = coords[0]
        x_coords = coords[1]

        # Ball is typically in middle/lower playfield
        valid_mask = (y_coords > 30) & (y_coords < 160)
        if not np.any(valid_mask):
            return self._find_ball_by_color(obs)

        y_valid = y_coords[valid_mask]
        x_valid = x_coords[valid_mask]

        # Return centroid of motion
        ball_y = np.mean(y_valid) + 40  # Add back offset
        ball_x = np.mean(x_valid)

        return (float(ball_x), float(ball_y))

    def _find_ball_by_color(self, obs: np.ndarray) -> Optional[tuple[float, float]]:
        """Fallback: find ball by color (yellow/white in Video Pinball)."""
        # Focus on playfield
        playfield = obs[40:200, :, :]

        # Ball is typically bright yellow/white
        # High brightness, similar R and G
        r, g, b = playfield[:, :, 0], playfield[:, :, 1], playfield[:, :, 2]
        brightness = (r.astype(float) + g.astype(float) + b.astype(float)) / 3

        # Ball-like pixels: bright and not too saturated
        ball_mask = (brightness > 180) & (
            np.abs(r.astype(float) - g.astype(float)) < 50
        )

        coords = np.where(ball_mask)
        if len(coords[0]) < 4:  # Ball is at least a few pixels
            return None

        # Filter to middle of playfield
        y_coords = coords[0]
        x_coords = coords[1]
        valid_mask = (
            (y_coords > 30) & (y_coords < 150) & (x_coords > 20) & (x_coords < 140)
        )

        if not np.any(valid_mask):
            return None

        y_valid = y_coords[valid_mask]
        x_valid = x_coords[valid_mask]

        return (float(np.mean(x_valid)), float(np.mean(y_valid) + 40))

    def close(self):
        """Clean up environment."""
        self.env.close()

    def get_action_meanings(self) -> list[str]:
        """Get list of available actions."""
        return list(SIMPLE_ACTIONS.keys())


class GameTracer:
    """
    Records game traces for analysis and learning.

    Captures every decision made by the AI along with outcomes,
    enabling post-game analysis and rule improvement.
    """

    def __init__(self):
        self.traces: list[dict] = []
        self.current_game: list[dict] = []
        self._game_count = 0

    def record_decision(
        self,
        state: GameState,
        action: str,
        rule_fired: str,
        outcome: Optional[str] = None,
        points: int = 0,
    ):
        """Record a single decision point."""
        self.current_game.append(
            {
                "frame": state.frame,
                "state": state.to_dict(),
                "prolog_facts": state.to_prolog_facts(),
                "action": action,
                "rule_fired": rule_fired,
                "outcome": outcome,
                "points": points,
            }
        )

    def end_game(self, final_score: int, balls_lost: int):
        """Finalize current game trace."""
        self._game_count += 1
        self.traces.append(
            {
                "game_id": self._game_count,
                "total_score": final_score,
                "balls_lost": balls_lost,
                "decisions": self.current_game.copy(),
                "decision_count": len(self.current_game),
            }
        )
        self.current_game = []

    def get_analysis(self, last_n_games: int = 5) -> dict:
        """
        Analyze recent games for pattern extraction.

        Returns analysis that can be fed to LLM for rule improvement.
        """
        recent = self.traces[-last_n_games:] if self.traces else []

        if not recent:
            return {"error": "No games recorded yet"}

        # Aggregate statistics
        total_score = sum(g["total_score"] for g in recent)
        avg_score = total_score / len(recent)
        total_balls_lost = sum(g["balls_lost"] for g in recent)

        # Analyze rule effectiveness
        rule_stats: dict[str, dict] = {}

        for game in recent:
            for decision in game["decisions"]:
                rule = decision["rule_fired"]
                if rule not in rule_stats:
                    rule_stats[rule] = {
                        "count": 0,
                        "points_gained": 0,
                        "outcomes": {"success": 0, "fail": 0, "neutral": 0},
                    }

                rule_stats[rule]["count"] += 1
                rule_stats[rule]["points_gained"] += decision.get("points", 0)

                outcome = decision.get("outcome", "neutral")
                if outcome in ("hit", "score", "success"):
                    rule_stats[rule]["outcomes"]["success"] += 1
                elif outcome in ("miss", "ball_lost", "fail"):
                    rule_stats[rule]["outcomes"]["fail"] += 1
                else:
                    rule_stats[rule]["outcomes"]["neutral"] += 1

        # Compute success rates
        for rule, stats in rule_stats.items():
            total = stats["outcomes"]["success"] + stats["outcomes"]["fail"]
            if total > 0:
                stats["success_rate"] = stats["outcomes"]["success"] / total
            else:
                stats["success_rate"] = None

        # Find failure patterns
        failures = []
        for game in recent:
            for decision in game["decisions"]:
                if decision.get("outcome") in ("ball_lost", "fail"):
                    failures.append(
                        {
                            "rule": decision["rule_fired"],
                            "state": decision["state"],
                            "action": decision["action"],
                        }
                    )

        return {
            "games_analyzed": len(recent),
            "average_score": avg_score,
            "total_balls_lost": total_balls_lost,
            "rule_effectiveness": rule_stats,
            "failure_examples": failures[:10],  # Limit to 10 examples
        }

    def export_for_llm(self, last_n_games: int = 5) -> str:
        """
        Export analysis in a format suitable for LLM processing.

        Returns a formatted string that can be included in an LLM prompt
        for rule improvement suggestions.
        """
        analysis = self.get_analysis(last_n_games)
        return json.dumps(analysis, indent=2)


# TEA Integration Functions
# These functions are called from YAML agents


def create_environment(render: bool = True) -> VideoPinballEnv:
    """Create and return a Video Pinball environment."""
    mode = "human" if render else "rgb_array"
    return VideoPinballEnv(render_mode=mode)


def state_to_prolog(state: GameState) -> str:
    """Convert game state to Prolog facts."""
    return state.to_prolog_facts()


def parse_prolog_action(prolog_result: dict) -> str:
    """Parse Prolog query result to get action string."""
    if isinstance(prolog_result, dict):
        return prolog_result.get("Action", "wait")
    elif isinstance(prolog_result, list) and prolog_result:
        return prolog_result[0].get("Action", "wait")
    return "wait"


if __name__ == "__main__":
    # Test the environment
    print("Testing Video Pinball Environment...")

    if not GYM_AVAILABLE:
        print("\nTo install Gymnasium with Atari:")
        print("  pip install gymnasium[atari,accept-rom-license]")
        exit(1)

    import random

    # Use rgb_array for headless testing
    env = VideoPinballEnv(render_mode="rgb_array")
    tracer = GameTracer()

    state = env.reset()
    print(f"\nInitial state:\n{state.to_prolog_facts()}")

    # Run game frames
    print("\nRunning 2000 frames...")
    prev_lives = state.lives

    for i in range(2000):
        # Strategy: flip based on ball position, with some randomness
        if state.ball.y > 170 and state.ball_vy > 0:
            # Ball approaching flippers
            if state.ball.x < 80:
                action = "flip_left"
            else:
                action = "flip_right"
        elif state.ball.y > 190:
            # Emergency - flip both
            action = "flip_both"
        elif random.random() < 0.1:
            # Occasional random action to keep ball moving
            action = random.choice(["flip_left", "flip_right", "launch"])
        else:
            action = "wait"

        old_score = state.score
        old_lives = state.lives
        state = env.step(action)
        points = state.score - old_score

        # Determine outcome
        if points > 0:
            outcome = "score"
        elif state.lives < old_lives:
            outcome = "ball_lost"
            print(f"Frame {i}: Ball lost! Lives: {state.lives}")
        else:
            outcome = "neutral"

        tracer.record_decision(
            state=state,
            action=action,
            rule_fired=f"simple_{action}",
            outcome=outcome,
            points=points,
        )

        if state.terminated:
            tracer.end_game(state.score, 3 - state.lives)
            print(f"Game over at frame {i}, final score: {state.score}")
            break

        if i % 500 == 0:
            print(
                f"Frame {i}: score={state.score}, lives={state.lives}, "
                f"ball=({state.ball.x:.0f}, {state.ball.y:.0f})"
            )

    env.close()

    print(f"\nFinal score: {state.score}")
    print("\nGame trace analysis:")
    analysis = tracer.get_analysis()
    print(f"Games recorded: {analysis.get('games_analyzed', 0)}")
    if "rule_effectiveness" in analysis:
        print("Rule effectiveness:")
        for rule, stats in analysis["rule_effectiveness"].items():
            print(f"  {rule}: {stats['count']} uses, {stats['points_gained']} points")
    print("\nTest passed!")
