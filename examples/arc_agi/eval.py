"""Run N sequential episodes against ls20, restoring/persisting the worldview.

Outputs a CSV with one row per episode:
    episode, frames, score, observations, hypotheses, rules
"""

from __future__ import annotations

import argparse
import csv
import random
import subprocess
import sys
from pathlib import Path

from . import GAME_ID
from .reasoner import Reasoner
from .runtime import init_runtime, shutdown
from .run import build_graph
from .snapshot import latest_episode, restore_from_storage, snapshot_to_storage


def run_episode(
    db_path: Path,
    storage_uri: str | None,
    episode: int,
    seed: int,
    max_frames: int,
    epsilon: float,
    llm_every: int,
) -> dict[str, int | str]:
    """Run a single episode in a clean subprocess for runtime isolation.

    Returns a dict of metrics parsed from the child's stdout. Subprocess
    isolation is used because arc_agi sets up a global Arcade session per process.
    """
    cmd = [
        sys.executable, "-m", "examples.arc_agi.run",
        "--max-frames", str(max_frames),
        "--seed", str(seed),
        "--epsilon", str(epsilon),
        "--llm-every", str(llm_every),
        "--db", str(db_path),
        "--episode", str(episode),
    ]
    if storage_uri:
        cmd += ["--ltm-storage", storage_uri, "--restore-prev"]

    result = subprocess.run(cmd, capture_output=True, text=True, check=False)
    out = result.stdout + result.stderr

    metrics: dict[str, int | str] = {
        "episode": episode,
        "frames": 0,
        "score": 0,
        "observations": 0,
        "hypotheses": 0,
        "rules": 0,
        "status": "unknown",
    }
    for line in out.splitlines():
        if line.startswith("episode="):
            for tok in line.split():
                if "=" in tok:
                    k, v = tok.split("=", 1)
                    if k in ("frames", "score"):
                        try:
                            metrics[k] = int(v)
                        except ValueError:
                            pass
                    elif k == "state":
                        metrics["status"] = v
        elif line.startswith("graph:"):
            import re
            for key, label in (
                ("observation", "observations"),
                ("hypothesis", "hypotheses"),
                ("rule", "rules"),
            ):
                m = re.search(rf"'{key}': (\d+)", line)
                if m:
                    metrics[label] = int(m.group(1))
    return metrics


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--episodes", type=int, default=5)
    parser.add_argument("--max-frames", type=int, default=80)
    parser.add_argument("--epsilon", type=float, default=0.35)
    parser.add_argument("--llm-every", type=int, default=0)
    parser.add_argument("--db-dir", type=Path,
                        default=Path("examples/arc_agi/data/eval"))
    parser.add_argument("--ltm-storage", type=str, default=None,
                        help="if set, persistent learning across episodes via this fsspec URI")
    parser.add_argument("--csv", type=Path,
                        default=Path("examples/arc_agi/data/eval/results.csv"))
    args = parser.parse_args()

    args.db_dir.mkdir(parents=True, exist_ok=True)
    args.csv.parent.mkdir(parents=True, exist_ok=True)

    rows: list[dict[str, int | str]] = []
    for ep in range(args.episodes):
        db_path = args.db_dir / f"ep{ep:04d}.kuzu"
        if db_path.exists():
            db_path.unlink()
        m = run_episode(
            db_path=db_path,
            storage_uri=args.ltm_storage,
            episode=ep,
            seed=ep * 7 + 1,
            max_frames=args.max_frames,
            epsilon=args.epsilon,
            llm_every=args.llm_every,
        )
        rows.append(m)
        print(
            f"ep={ep:02d}  frames={m['frames']:>3}  score={m['score']}  "
            f"obs={m['observations']:>3}  hyps={m['hypotheses']:>3}  "
            f"rules={m['rules']:>2}  status={m['status']}"
        )

    with open(args.csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\nwrote {args.csv}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
