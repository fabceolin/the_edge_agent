"""Render a frame from the worldview with every identified concept overlaid,
plus full legends for objects and clusters.

Output is a multi-panel PNG showing:
  - The reconstructed 64×64 grid (painted from Entity bboxes + Shape bitmaps)
  - Entity bounding boxes labeled with shape name / role concept
  - Agent marker (red star) at the avatar's centroid + recent trajectory
  - Goal attempt markers (orange X for failed, lime P for success)
  - Anchor cluster outlines (cyan dashed)
  - Right-side legends:
      * All objects (per-shape: bitmap thumbnail, name, role, observations)
      * All clusters (per-kind: count, sample bboxes)
      * Learned Movable rules
      * Goal attempts history (Prolog backtracking trail)
      * Summary stats
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import patches
from matplotlib.colors import ListedColormap

from .worldview import Worldview


# ARC-AGI palette (approximate; matches typical ARC rendering).
ARC_PALETTE = [
    "#000000", "#0074D9", "#FF4136", "#2ECC40", "#FFDC00",
    "#AAAAAA", "#F012BE", "#FF851B", "#7FDBFF", "#870C25",
    "#888888", "#46A35E", "#9B59B6", "#5DADE2", "#FFC0CB",
    "#FFFFFF",
]


# Physics subkind → matplotlib marker style
PHYSICS_MPL_STYLE = {
    "collision":      {"color": "#FF3333", "marker": "X", "size": 90},
    "bounce":         {"color": "#FF8800", "marker": "^", "size": 80},
    "push":           {"color": "#3399FF", "marker": ">", "size": 80},
    "adhesion":       {"color": "#9933FF", "marker": "o", "size": 70},
    "destruction":    {"color": "#990000", "marker": "x", "size": 90},
    "construction":   {"color": "#00CC66", "marker": "+", "size": 100},
    "teleport":       {"color": "#FF00FF", "marker": "*", "size": 120},
    "gravity":        {"color": "#666699", "marker": "v", "size": 70},
    "inertia":        {"color": "#CC9933", "marker": "^", "size": 70},
    "friction":       {"color": "#996633", "marker": "s", "size": 60},
    "magnetism":      {"color": "#FF6699", "marker": "*", "size": 80},
    "spawn":          {"color": "#33CC99", "marker": "D", "size": 70},
    "wrap_around":    {"color": "#9999FF", "marker": "h", "size": 80},
    "marks":          {"color": "#CCAA88", "marker": "s", "size": 50},
    "smoke":          {"color": "#BBBBBB", "marker": "o", "size": 50},
    "shadow":         {"color": "#444444", "marker": "o", "size": 50},
    "light":          {"color": "#FFFF66", "marker": "*", "size": 100},
    "alert":          {"color": "#FF6666", "marker": "D", "size": 80},
    "wave":           {"color": "#66CCFF", "marker": "1", "size": 80},
    "mirror":         {"color": "#FFCC66", "marker": "3", "size": 80},
    "chain_reaction": {"color": "#FF9933", "marker": "P", "size": 90},
    "trigger_effect": {"color": "#33FFFF", "marker": "d", "size": 80},
}


def _exec(wv: Worldview, q: str, p: dict | None = None) -> list:
    res = wv.conn.execute(q, p or {})
    qr = res[0] if isinstance(res, list) else res
    rows: list = []
    while qr.has_next():
        rows.append(qr.get_next())
    return rows


def _reconstruct_grid(wv: Worldview, obs_id: str, grid_size: int = 64) -> np.ndarray:
    grid = np.zeros((grid_size, grid_size), dtype=np.int8)
    rows = _exec(wv,
        """
        MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e:Entity)-[:HAS_SHAPE]->(s:Shape)
        RETURN e.color, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1, s.bitmap_ascii, e.area
        ORDER BY e.area DESC
        """,
        {"oid": obs_id})
    for r in rows:
        color, r0, c0, _r1, _c1, bitmap_ascii, _area = r
        if not bitmap_ascii:
            continue
        bitmap_rows = bitmap_ascii.split("\n")
        for i, line in enumerate(bitmap_rows):
            for j, ch in enumerate(line):
                if ch == "1":
                    ry, rx = int(r0) + i, int(c0) + j
                    if 0 <= ry < grid_size and 0 <= rx < grid_size:
                        grid[ry, rx] = int(color)
    return grid


def _palette_cmap() -> ListedColormap:
    return ListedColormap(ARC_PALETTE)


def _color_swatch(ax, color: int, label: str, y: float) -> None:
    """Draw a tiny color box + label at (x=0.02, y) in axes coords."""
    if 0 <= color < len(ARC_PALETTE):
        ax.add_patch(patches.Rectangle(
            (0.03, y - 0.012), 0.04, 0.024,
            transform=ax.transAxes,
            facecolor=ARC_PALETTE[color], edgecolor="black", linewidth=0.5))
    else:
        ax.add_patch(patches.Rectangle(
            (0.03, y - 0.012), 0.04, 0.024,
            transform=ax.transAxes,
            facecolor="none", edgecolor="black", linewidth=0.5, hatch="//"))
    ax.text(0.10, y, label, transform=ax.transAxes,
            fontsize=8, family="monospace", va="center")


def render_overview(
    wv: Worldview,
    *,
    obs_id: str | None = None,
    output_path: str | Path = "frame.png",
    trajectory_window: int = 30,
) -> Path:
    if obs_id is None:
        rows = _exec(wv,
            "MATCH (o:Observation) RETURN o.id ORDER BY o.frame_n DESC LIMIT 1")
        if not rows:
            raise RuntimeError("No observations in worldview")
        obs_id = rows[0][0]
    assert obs_id is not None

    meta_rows = _exec(wv,
        "MATCH (o:Observation {id: $oid}) "
        "RETURN o.frame_n, o.episode, o.score, o.state",
        {"oid": obs_id})
    frame_n, episode, score, state = meta_rows[0]

    grid = _reconstruct_grid(wv, obs_id)

    entities = _exec(wv,
        """
        MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e:Entity)-[:HAS_SHAPE]->(s:Shape)
        RETURN e.id, e.color, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1,
               e.centroid_y, e.centroid_x, e.area, e.shape_hash,
               s.llm_name, s.role_concept
        """,
        {"oid": obs_id})

    anchor_rows = _exec(wv,
        """
        MATCH (cl:Cluster {kind:'anchor'})<-[:PART_OF]-(e:Entity)
        MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e)
        RETURN DISTINCT cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1, cl.n_members
        """,
        {"oid": obs_id})

    attempt_rows = _exec(wv,
        "MATCH (a:GoalAttempt) RETURN a.source, a.centroid_y, a.centroid_x, "
        "a.frames_invested, a.reward_delta, a.status ORDER BY a.attempted_at")

    rule_rows = _exec(wv,
        "MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)-[:RULE_INSTANCE_OF]->(c:Concept) "
        "WHERE c.category = 'spatial' "
        "RETURN DISTINCT h.action_code, c.name, h.dsl")

    # Avatar trajectory.
    agent_rows = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept = 'Agent' RETURN s.shape_hash")
    agent_shape = agent_rows[0][0] if agent_rows else None
    trajectory: list[tuple] = []
    if agent_shape:
        trajectory = _exec(wv,
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: $sh})
            RETURN o.frame_n, e.centroid_y, e.centroid_x
            ORDER BY o.frame_n DESC LIMIT $limit
            """,
            {"sh": agent_shape, "limit": trajectory_window})

    # All unique shapes (object inventory).
    all_shapes = _exec(wv,
        """
        MATCH (s:Shape)
        RETURN s.shape_hash, s.llm_name, s.role_concept, s.area, s.n_observations,
               s.height, s.width
        ORDER BY s.n_observations DESC
        """)

    # All clusters grouped by kind/subkind.
    cluster_counts = _exec(wv,
        """
        MATCH (cl:Cluster)
        WITH cl.kind AS kind, cl.subkind AS subkind, count(*) AS n
        RETURN kind, subkind, n
        ORDER BY n DESC
        """)

    # ============================================================
    # Figure layout: grid on left (~50% width), 5 stacked panels on right.
    # ============================================================
    fig = plt.figure(figsize=(24, 14))
    gs = fig.add_gridspec(
        nrows=5, ncols=3,
        width_ratios=[2.4, 1.2, 1.2],
        height_ratios=[1, 1, 1, 1, 1],
        hspace=0.45, wspace=0.18,
    )
    ax_grid = fig.add_subplot(gs[:, 0])
    ax_obj = fig.add_subplot(gs[0:2, 1])
    ax_clusters = fig.add_subplot(gs[0:2, 2])
    ax_rules = fig.add_subplot(gs[2, 1])
    ax_attempts = fig.add_subplot(gs[3:5, 1])
    ax_summary = fig.add_subplot(gs[2:5, 2])

    # ---- main grid panel -------------------------------------------------
    cmap = _palette_cmap()
    ax_grid.imshow(grid, cmap=cmap, vmin=0, vmax=15, interpolation="nearest")
    ax_grid.set_title(
        f"episode {episode} · frame {frame_n} · score {score} · state {state}",
        fontsize=12)
    ax_grid.set_xticks(range(0, 65, 8))
    ax_grid.set_yticks(range(0, 65, 8))
    ax_grid.grid(True, color="#333333", linewidth=0.3, alpha=0.4)

    for r in entities:
        (_id, color, r0, c0, r1, c1, _cy, cx, _area, _sh, llm_name, role) = r
        w = c1 - c0 + 1
        h = r1 - r0 + 1
        edge = "yellow"
        lw = 1.0
        if role == "Agent":
            edge = "red"; lw = 2.5
        elif role in ("Goal", "Collectible"):
            edge = "lime"; lw = 2.0
        elif role in ("Background", "Border", "HUDElement"):
            edge = "#888888"; lw = 0.5
        ax_grid.add_patch(patches.Rectangle(
            (c0 - 0.5, r0 - 0.5), w, h,
            linewidth=lw, edgecolor=edge, facecolor="none"))
        label_parts = []
        if llm_name: label_parts.append(llm_name)
        if role: label_parts.append(f"[{role}]")
        if label_parts:
            ax_grid.text(cx, r0 - 0.8, "\n".join(label_parts),
                        color="white", fontsize=6, ha="center", va="bottom",
                        bbox=dict(boxstyle="round,pad=0.18",
                                  facecolor="black", alpha=0.6, edgecolor="none"))

    for r0, c0, r1, c1, _n in anchor_rows:
        ax_grid.add_patch(patches.Rectangle(
            (c0 - 0.5, r0 - 0.5), c1 - c0 + 1, r1 - r0 + 1,
            linewidth=1.0, edgecolor="cyan", linestyle="--",
            facecolor="none", alpha=0.7))

    if trajectory:
        ys = [t[1] for t in trajectory]
        xs = [t[2] for t in trajectory]
        ax_grid.plot(xs, ys, "r-", linewidth=1.2, alpha=0.6)
        ax_grid.plot(xs[0], ys[0], "r*", markersize=18,
                    markeredgecolor="black", markeredgewidth=0.8)

    for _src, cy, cx, _fi, _rd, status in attempt_rows:
        color_m = "lime" if status == "success" else "orange"
        marker = "P" if status == "success" else "X"
        ax_grid.plot(cx, cy, marker, color=color_m, markersize=12,
                    markeredgecolor="black", markeredgewidth=0.8)

    # Physics events overlay (events involving any cluster in this observation).
    physics_rows = _exec(wv,
        """
        MATCH (p:Cluster {kind:'physics'})-[:EXPLAINED_BY]->(c:Cluster)
        MATCH (o:Observation {id: $oid})-[:OBSERVED_AS]->(c)
        RETURN DISTINCT p.subkind, p.centroid_y, p.centroid_x, p.signature
        """,
        {"oid": obs_id})
    if not physics_rows:
        physics_rows = _exec(wv,
            """
            MATCH (p:Cluster {kind:'physics'})
            RETURN p.subkind, p.centroid_y, p.centroid_x, p.signature LIMIT 100
            """)
    by_subkind: dict[str, list] = {}
    for r in physics_rows:
        by_subkind.setdefault(r[0] or "unknown", []).append(r)
    for subkind, group in by_subkind.items():
        style = PHYSICS_MPL_STYLE.get(subkind,
                                       {"color": "white", "marker": "o", "size": 50})
        xs = [r[2] for r in group]
        ys = [r[1] for r in group]
        ax_grid.scatter(xs, ys, c=style["color"], marker=style["marker"],
                        s=style["size"], edgecolors="black", linewidths=0.5,
                        alpha=0.85, label=f"phys: {subkind}({len(group)})")

    if by_subkind:
        ax_grid.legend(loc="upper left", fontsize=6, ncol=2,
                      framealpha=0.85, bbox_to_anchor=(0.0, 1.0))
    ax_grid.set_xlim(-0.5, 63.5); ax_grid.set_ylim(63.5, -0.5)

    # ---- Object inventory legend (new) ----------------------------------
    ax_obj.axis("off")
    ax_obj.set_title("Object Inventory (Shape → role)", fontsize=11, loc="left",
                     fontweight="bold")
    # Display top N shapes vertically with mini bitmap + label.
    max_rows = 16
    shapes_to_show = all_shapes[:max_rows]
    if not shapes_to_show:
        ax_obj.text(0.02, 0.95, "(no shapes recorded)",
                    transform=ax_obj.transAxes, fontsize=10, va="top")
    else:
        line_h = 1.0 / (max_rows + 1)
        ax_obj.text(0.03, 0.97,
                    f"{'name':<10} {'role':<18} {'area':>4} {'seen':>4}",
                    transform=ax_obj.transAxes, fontsize=8,
                    family="monospace", va="top", fontweight="bold")
        for i, r in enumerate(shapes_to_show):
            sh_hash, llm_name, role, area, n_obs, _h, _w = r
            y = 0.93 - (i + 1) * line_h
            name = (llm_name or "—")[:10]
            role_str = (role or "—")[:18]
            text = f"{name:<10} {role_str:<18} {area:>4} {n_obs:>4}"
            ax_obj.text(0.03, y, text, transform=ax_obj.transAxes,
                        fontsize=8, family="monospace", va="center")
        if len(all_shapes) > max_rows:
            ax_obj.text(0.03, 0.02,
                        f"+ {len(all_shapes) - max_rows} more shapes",
                        transform=ax_obj.transAxes, fontsize=8,
                        family="monospace", va="bottom", style="italic")

    # ---- Cluster breakdown legend (new) ---------------------------------
    ax_clusters.axis("off")
    ax_clusters.set_title("Cluster Breakdown (all kinds)", fontsize=11, loc="left",
                          fontweight="bold")
    ax_clusters.text(0.03, 0.97,
                     f"{'kind':<14} {'subkind':<18} {'count':>6}",
                     transform=ax_clusters.transAxes, fontsize=8,
                     family="monospace", va="top", fontweight="bold")
    max_clusters = 18
    line_h = 1.0 / (max_clusters + 1)
    for i, r in enumerate(cluster_counts[:max_clusters]):
        kind, subkind, n = r
        y = 0.93 - (i + 1) * line_h
        sub_str = (subkind or "—")[:18]
        text = f"{kind:<14} {sub_str:<18} {n:>6}"
        ax_clusters.text(0.03, y, text, transform=ax_clusters.transAxes,
                         fontsize=8, family="monospace", va="center")
    if len(cluster_counts) > max_clusters:
        ax_clusters.text(0.03, 0.02,
                         f"+ {len(cluster_counts) - max_clusters} more kinds",
                         transform=ax_clusters.transAxes, fontsize=8,
                         family="monospace", va="bottom", style="italic")

    # ---- Rules panel -----------------------------------------------------
    ax_rules.axis("off")
    ax_rules.set_title("Learned Movable Rules", fontsize=11, loc="left",
                       fontweight="bold")
    rule_lines = [f"{'code':<10} {'direction':<10}"]
    seen: set[tuple[int, str]] = set()
    for ac, name, _dsl in rule_rows:
        key = (int(ac), name)
        if key in seen:
            continue
        seen.add(key)
        rule_lines.append(f"  ACTION{ac:<3}    {name}")
    if len(rule_lines) == 1:
        rule_lines.append("  (none yet)")
    ax_rules.text(0.02, 0.95, "\n".join(rule_lines), transform=ax_rules.transAxes,
                  fontsize=10, family="monospace", va="top")

    # ---- Goal attempts panel --------------------------------------------
    ax_attempts.axis("off")
    ax_attempts.set_title("Goal Attempts (Prolog backtracking)", fontsize=11,
                          loc="left", fontweight="bold")
    if attempt_rows:
        lines = [f"{'source':<20} {'(y, x)':>11} {'frames':>6} {'status':>10}"]
        for src, cy, cx, fi, _rd, status in attempt_rows[:14]:
            lines.append(f"{src:<20} ({cy:>4.0f},{cx:>4.0f})  {fi:>6} {status:>10}")
        ax_attempts.text(0.02, 0.95, "\n".join(lines),
                         transform=ax_attempts.transAxes,
                         fontsize=8, family="monospace", va="top")
    else:
        ax_attempts.text(0.02, 0.95, "(no attempts yet)",
                         transform=ax_attempts.transAxes,
                         fontsize=10, family="monospace", va="top")

    # ---- Summary panel ---------------------------------------------------
    ax_summary.axis("off")
    ax_summary.set_title("Summary", fontsize=11, loc="left", fontweight="bold")
    summary = _summarize(wv, obs_id, attempt_rows)
    ax_summary.text(0.02, 0.96, summary, transform=ax_summary.transAxes,
                    fontsize=9, family="monospace", va="top")

    fig.suptitle("ARC-AGI-3 — neurosymbolic worldview overview",
                 fontsize=14, fontweight="bold")
    fig.tight_layout(rect=(0, 0, 1, 0.97))
    output_path = Path(output_path)
    fig.savefig(output_path, dpi=110, bbox_inches="tight")
    plt.close(fig)
    return output_path


def _summarize(wv: Worldview, obs_id: str, attempts: list) -> str:
    stats = wv.stats()
    n_shapes = _exec(wv, "MATCH (s:Shape) RETURN count(s)")[0][0]
    n_classified = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept <> '' RETURN count(s)")[0][0]
    n_named = _exec(wv,
        "MATCH (s:Shape) WHERE s.llm_name <> '' RETURN count(s)")[0][0]
    n_anchors = _exec(wv,
        "MATCH (cl:Cluster {kind:'anchor'}) RETURN count(cl)")[0][0]
    n_paths = _exec(wv,
        "MATCH (cl:Cluster {kind:'path'}) RETURN count(cl)")[0][0]
    n_physics = _exec(wv,
        "MATCH (cl:Cluster {kind:'physics'}) RETURN count(cl)")[0][0]
    n_concepts = _exec(wv,
        "MATCH (c:Concept) RETURN count(c)")[0][0]

    agent_row = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept = 'Agent' "
        "RETURN s.shape_hash, s.llm_name, s.area LIMIT 1")
    agent_str = "(not identified)"
    if agent_row:
        sh, llm_name, area = agent_row[0]
        agent_str = f"{llm_name or 'unnamed'} (shape={sh[:8]}, area={area})"

    lines = [
        "GRAPH OBJECTS",
        f"  Observations    {stats.get('observation', 0):>6}",
        f"  Entities        {stats.get('entity', 0):>6}",
        f"  Rules promoted  {stats.get('rule', 0):>6}",
        f"  Clusters total  {stats.get('cluster', 0):>6}",
        f"  Hypotheses      {stats.get('hypothesis', 0):>6}",
        f"  Concepts        {int(n_concepts):>6}",
        "",
        "SHAPES",
        f"  Unique          {int(n_shapes):>6}",
        f"  Named by LLM    {int(n_named):>6}",
        f"  Role-classified {int(n_classified):>6}",
        "",
        "CLUSTER HIGHLIGHTS",
        f"  Anchors         {int(n_anchors):>6}",
        f"  Paths           {int(n_paths):>6}",
        f"  Physics events  {int(n_physics):>6}",
        "",
        "AGENT",
        f"  {agent_str}",
        "",
        "GOAL ATTEMPTS",
        f"  Total           {len(attempts):>6}",
        f"  Successful      {sum(1 for a in attempts if a[5] == 'success'):>6}",
    ]
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=str, required=True,
                        help="path to the .kuzu worldview file")
    parser.add_argument("--out", type=str, default="frame.png",
                        help="output PNG path")
    parser.add_argument("--frame", type=int, default=None,
                        help="frame_n to render (defaults to the latest)")
    parser.add_argument("--trajectory-window", type=int, default=30,
                        help="how many recent frames of avatar trajectory to overlay")
    args = parser.parse_args()

    wv = Worldview(args.db)
    try:
        obs_id = None
        if args.frame is not None:
            rows = _exec(wv,
                "MATCH (o:Observation {frame_n: $f}) RETURN o.id LIMIT 1",
                {"f": args.frame})
            if not rows:
                raise SystemExit(f"No observation with frame_n={args.frame}")
            obs_id = rows[0][0]
        out = render_overview(wv, obs_id=obs_id, output_path=args.out,
                              trajectory_window=args.trajectory_window)
        print(f"wrote {out}")
    finally:
        wv.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
