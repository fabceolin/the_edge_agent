"""Interactive HTML viewer of the worldview using Plotly.

Renders a self-contained HTML file with:
  - the reconstructed 64×64 grid as a heatmap
  - hover tooltips on every entity / anchor / goal / physics event
  - togglable layers (entities, anchors, paths, goal attempts, physics)
  - a side panel listing all detected objects and clusters

Usage:
    python -m examples.arc_agi.visualize_html --db DB.kuzu --out overview.html
"""

from __future__ import annotations

import argparse
import html
from pathlib import Path

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from .worldview import Worldview


ARC_PALETTE = [
    "#000000", "#0074D9", "#FF4136", "#2ECC40", "#FFDC00",
    "#AAAAAA", "#F012BE", "#FF851B", "#7FDBFF", "#870C25",
    "#888888", "#46A35E", "#9B59B6", "#5DADE2", "#FFC0CB",
    "#FFFFFF",
]


# Physics subkind → marker style (color, plotly symbol, hover icon).
PHYSICS_STYLE = {
    "collision":      {"color": "#FF3333", "symbol": "circle-cross", "name": "collision"},
    "bounce":         {"color": "#FF8800", "symbol": "triangle-up", "name": "bounce"},
    "push":           {"color": "#3399FF", "symbol": "arrow-right",   "name": "push"},
    "adhesion":       {"color": "#9933FF", "symbol": "circle-dot",    "name": "adhesion"},
    "destruction":    {"color": "#990000", "symbol": "x-thin",        "name": "destruction"},
    "construction":   {"color": "#00CC66", "symbol": "cross-thin",    "name": "construction"},
    "teleport":       {"color": "#FF00FF", "symbol": "star",          "name": "teleport"},
    "gravity":        {"color": "#666699", "symbol": "triangle-down", "name": "gravity"},
    "inertia":        {"color": "#CC9933", "symbol": "arrow-up",      "name": "inertia"},
    "friction":       {"color": "#996633", "symbol": "square-dot",    "name": "friction"},
    "magnetism":      {"color": "#FF6699", "symbol": "asterisk",      "name": "magnetism"},
    "spawn":          {"color": "#33CC99", "symbol": "diamond-cross", "name": "spawn"},
    "wrap_around":    {"color": "#9999FF", "symbol": "hexagon",       "name": "wrap-around"},
    "marks":          {"color": "#CCAA88", "symbol": "square",        "name": "marks"},
    "smoke":          {"color": "#BBBBBB", "symbol": "circle-open",   "name": "smoke"},
    "shadow":         {"color": "#444444", "symbol": "circle-open-dot", "name": "shadow"},
    "light":          {"color": "#FFFF66", "symbol": "star-open",     "name": "light"},
    "alert":          {"color": "#FF6666", "symbol": "diamond-open",  "name": "alert"},
    "wave":           {"color": "#66CCFF", "symbol": "y-up",          "name": "wave"},
    "mirror":         {"color": "#FFCC66", "symbol": "y-left",        "name": "mirror"},
    "chain_reaction": {"color": "#FF9933", "symbol": "circle-x",      "name": "chain-reaction"},
    "trigger_effect": {"color": "#33FFFF", "symbol": "diamond-tall",  "name": "trigger"},
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
        for i, line in enumerate(bitmap_ascii.split("\n")):
            for j, ch in enumerate(line):
                if ch == "1":
                    ry, rx = int(r0) + i, int(c0) + j
                    if 0 <= ry < grid_size and 0 <= rx < grid_size:
                        grid[ry, rx] = int(color)
    return grid


def _build_grid_heatmap(grid: np.ndarray) -> go.Heatmap:
    # Build per-cell hover with palette name.
    z = grid
    customdata = np.empty(z.shape, dtype=object)
    for y in range(z.shape[0]):
        for x in range(z.shape[1]):
            customdata[y, x] = f"color={int(z[y,x])}"
    return go.Heatmap(
        z=z,
        colorscale=[(i / 15, ARC_PALETTE[i]) for i in range(16)],
        zmin=0, zmax=15,
        showscale=False,
        customdata=customdata,
        hovertemplate="cell (%{y}, %{x})<br>%{customdata}<extra></extra>",
        name="grid",
        hoverongaps=False,
    )


def _entity_traces(wv: Worldview, obs_id: str) -> list[go.Scatter]:
    entities = _exec(wv,
        """
        MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e:Entity)-[:HAS_SHAPE]->(s:Shape)
        RETURN e.color, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1,
               e.centroid_y, e.centroid_x, e.area, e.shape_hash,
               s.llm_name, s.role_concept
        """,
        {"oid": obs_id})
    if not entities:
        return []

    # Group entities by role for cleaner legend toggling.
    by_role: dict[str, list] = {}
    for r in entities:
        role = r[10] or "unclassified"
        by_role.setdefault(role, []).append(r)

    role_color = {
        "Agent": "#FF2222",
        "Goal": "#33FF33",
        "Collectible": "#33FF66",
        "Background": "#888888",
        "Border": "#999999",
        "HUDElement": "#AAAA88",
        "DirectionIndicator": "#FFCC00",
        "Container": "#3399FF",
        "Obstacle": "#FF9933",
        "unclassified": "#FFFF00",
    }

    traces: list[go.Scatter] = []
    for role, rows in by_role.items():
        xs, ys, hovers, sizes = [], [], [], []
        for color, r0, c0, r1, c1, cy, cx, area, sh, llm_name, _role in rows:
            xs.append(float(cx))
            ys.append(float(cy))
            hovers.append(
                f"<b>{llm_name or '(unnamed)'}</b><br>"
                f"role={role}<br>"
                f"color={color} area={area}<br>"
                f"bbox=({r0},{c0})-({r1},{c1})<br>"
                f"shape_hash={sh[:12]}"
            )
            sizes.append(max(8.0, min(20.0, area ** 0.5)))
        traces.append(go.Scatter(
            x=xs, y=ys, mode="markers",
            marker=dict(symbol="square-open",
                        color=role_color.get(role, "#FFFF00"),
                        size=sizes,
                        line=dict(color=role_color.get(role, "#FFFF00"), width=2)),
            text=hovers, hoverinfo="text",
            name=f"entities · {role} ({len(rows)})",
            legendgroup="entities",
            visible=True,
        ))
    return traces


def _anchor_traces(wv: Worldview, obs_id: str) -> list[go.Scatter]:
    rows = _exec(wv,
        """
        MATCH (cl:Cluster {kind:'anchor'})<-[:PART_OF]-(e:Entity)
        MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e)
        RETURN DISTINCT cl.id, cl.signature, cl.bbox_r0, cl.bbox_c0,
                        cl.bbox_r1, cl.bbox_c1, cl.n_members,
                        cl.centroid_y, cl.centroid_x
        """,
        {"oid": obs_id})
    if not rows:
        return []
    xs, ys, hovers = [], [], []
    for cl_id, sig, r0, c0, r1, c1, n_members, cy, cx in rows:
        xs.append(float(cx))
        ys.append(float(cy))
        hovers.append(
            f"<b>AnchorCluster</b><br>"
            f"members={n_members}<br>"
            f"bbox=({r0},{c0})-({r1},{c1})<br>"
            f"sig={sig[:24]}"
        )
    return [go.Scatter(
        x=xs, y=ys, mode="markers",
        marker=dict(symbol="square-open", color="cyan", size=14,
                    line=dict(color="cyan", width=1, dash="dash")),
        text=hovers, hoverinfo="text",
        name=f"anchors ({len(rows)})",
        legendgroup="anchors",
        visible="legendonly",
    )]


def _trajectory_trace(wv: Worldview, trajectory_window: int) -> list[go.Scatter]:
    agent_row = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept = 'Agent' "
        "RETURN s.shape_hash, s.llm_name LIMIT 1")
    if not agent_row:
        return []
    sh, name = agent_row[0]
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: $sh})
        RETURN o.frame_n, e.centroid_y, e.centroid_x
        ORDER BY o.frame_n DESC LIMIT $limit
        """,
        {"sh": sh, "limit": trajectory_window})
    if not rows:
        return []
    xs = [float(r[2]) for r in rows]
    ys = [float(r[1]) for r in rows]
    hovers = [f"avatar @ frame {r[0]}" for r in rows]
    return [
        go.Scatter(x=xs, y=ys, mode="lines",
                   line=dict(color="red", width=1.5),
                   hoverinfo="skip",
                   name=f"avatar trajectory ({len(rows)})",
                   legendgroup="agent"),
        go.Scatter(x=[xs[0]], y=[ys[0]], mode="markers",
                   marker=dict(symbol="star", color="red", size=18,
                               line=dict(color="black", width=1)),
                   text=[f"<b>avatar</b> ({name or '?'})<br>shape={sh[:12]}<br>"
                         f"current centroid=({ys[0]:.1f}, {xs[0]:.1f})"],
                   hoverinfo="text",
                   name="avatar now",
                   legendgroup="agent"),
    ]


def _goal_attempt_traces(wv: Worldview) -> list[go.Scatter]:
    rows = _exec(wv,
        "MATCH (a:GoalAttempt) "
        "RETURN a.source, a.centroid_y, a.centroid_x, a.frames_invested, "
        "a.reward_delta, a.status, a.attempted_at ORDER BY a.attempted_at")
    if not rows:
        return []
    by_status: dict[str, list] = {}
    for r in rows:
        by_status.setdefault(str(r[5]), []).append(r)
    traces = []
    for status, group in by_status.items():
        xs = [float(r[2]) for r in group]
        ys = [float(r[1]) for r in group]
        hovers = [
            f"<b>goal attempt #{i+1}</b><br>"
            f"source={r[0]}<br>"
            f"position=({r[1]:.1f}, {r[2]:.1f})<br>"
            f"frames invested={r[3]}<br>"
            f"reward_delta={r[4]}<br>"
            f"status={status}"
            for i, r in enumerate(group)
        ]
        color = "#00FF00" if status == "success" else "#FF9933"
        symbol = "star" if status == "success" else "x"
        traces.append(go.Scatter(
            x=xs, y=ys, mode="markers",
            marker=dict(symbol=symbol, color=color, size=14,
                        line=dict(color="black", width=1)),
            text=hovers, hoverinfo="text",
            name=f"goal attempts · {status} ({len(group)})",
            legendgroup="goals",
            visible=True,
        ))
    return traces


def _path_cluster_traces(wv: Worldview) -> list[go.Scatter]:
    rows = _exec(wv,
        """
        MATCH (cl:Cluster {kind:'path'})
        RETURN cl.id, cl.signature, cl.n_members,
               cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1
        """)
    if not rows:
        return []
    traces = []
    for cl_id, sig, n_hops, r0, c0, r1, c1 in rows:
        traces.append(go.Scatter(
            x=[c0, c1], y=[r0, r1], mode="lines+markers",
            line=dict(color="#FF66FF", width=1, dash="dot"),
            marker=dict(symbol="circle", color="#FF66FF", size=6),
            text=[f"PathCluster start", f"PathCluster end<br>{n_hops} hops"],
            hoverinfo="text",
            name=f"path {sig[:8]}",
            legendgroup="paths",
            showlegend=False,
            visible="legendonly",
        ))
    if traces:
        # Add a legend stub so the user can toggle the group on.
        traces.append(go.Scatter(
            x=[None], y=[None], mode="lines",
            line=dict(color="#FF66FF", width=1, dash="dot"),
            name=f"path clusters ({len(rows)})",
            legendgroup="paths",
            showlegend=True,
            visible="legendonly",
        ))
    return traces


def _physics_traces(wv: Worldview, obs_id: str) -> list[go.Scatter]:
    """Render physics-event markers. Includes events that involve any cluster
    observed in the target frame."""
    rows = _exec(wv,
        """
        MATCH (p:Cluster {kind:'physics'})-[r:EXPLAINED_BY]->(c:Cluster)
        MATCH (o:Observation {id: $oid})-[:OBSERVED_AS]->(c)
        RETURN DISTINCT p.id, p.subkind, p.signature,
                        p.centroid_y, p.centroid_x,
                        p.bbox_r0, p.bbox_c0, p.bbox_r1, p.bbox_c1,
                        p.velocity_dy, p.velocity_dx
        """,
        {"oid": obs_id})
    if not rows:
        # Fall back: show all physics events (post-hoc, not tied to obs).
        rows = _exec(wv,
            """
            MATCH (p:Cluster {kind:'physics'})
            RETURN p.id, p.subkind, p.signature,
                   p.centroid_y, p.centroid_x,
                   p.bbox_r0, p.bbox_c0, p.bbox_r1, p.bbox_c1,
                   p.velocity_dy, p.velocity_dx
            LIMIT 200
            """)
    if not rows:
        return []
    by_subkind: dict[str, list] = {}
    for r in rows:
        by_subkind.setdefault(r[1] or "unknown", []).append(r)

    traces = []
    for subkind, group in by_subkind.items():
        style = PHYSICS_STYLE.get(subkind, {"color": "white", "symbol": "circle", "name": subkind})
        xs, ys, hovers = [], [], []
        for r in group:
            _id, _sk, sig, cy, cx, r0, c0, r1, c1, vdy, vdx = r
            xs.append(float(cx))
            ys.append(float(cy))
            velocity_str = f"({int(vdy)}, {int(vdx)})" if vdy or vdx else "—"
            hovers.append(
                f"<b>physics · {subkind}</b><br>"
                f"sig={sig[:12]}<br>"
                f"centroid=({cy:.1f}, {cx:.1f})<br>"
                f"bbox=({r0},{c0})-({r1},{c1})<br>"
                f"velocity={velocity_str}"
            )
        traces.append(go.Scatter(
            x=xs, y=ys, mode="markers",
            marker=dict(symbol=style["symbol"], color=style["color"], size=14,
                        line=dict(color="black", width=1)),
            text=hovers, hoverinfo="text",
            name=f"physics · {style['name']} ({len(group)})",
            legendgroup="physics",
            visible="legendonly",
        ))
    return traces


def _summary_html(wv: Worldview, obs_id: str) -> str:
    stats = wv.stats()
    n_shapes = _exec(wv, "MATCH (s:Shape) RETURN count(s)")[0][0]
    n_named = _exec(wv,
        "MATCH (s:Shape) WHERE s.llm_name <> '' RETURN count(s)")[0][0]
    n_classified = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept <> '' RETURN count(s)")[0][0]
    n_attempts = _exec(wv, "MATCH (a:GoalAttempt) RETURN count(a)")[0][0]
    n_success = _exec(wv,
        "MATCH (a:GoalAttempt) WHERE a.status='success' RETURN count(a)")[0][0]
    agent_row = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept='Agent' "
        "RETURN s.shape_hash, s.llm_name, s.area LIMIT 1")
    agent_str = "(not identified)"
    if agent_row:
        sh, name, area = agent_row[0]
        agent_str = f"{name or 'unnamed'} (shape={sh[:8]}, area={area})"

    all_shapes = _exec(wv,
        "MATCH (s:Shape) "
        "RETURN s.shape_hash, s.llm_name, s.role_concept, s.area, s.n_observations "
        "ORDER BY s.n_observations DESC LIMIT 50")
    shape_rows = "".join(
        f"<tr><td>{html.escape((r[1] or '—')[:14])}</td>"
        f"<td>{html.escape((r[2] or '—')[:20])}</td>"
        f"<td>{r[3]}</td><td>{r[4]}</td>"
        f"<td><code>{html.escape(r[0][:10])}</code></td></tr>"
        for r in all_shapes
    )

    cluster_counts = _exec(wv,
        "MATCH (cl:Cluster) "
        "WITH cl.kind AS kind, cl.subkind AS subkind, count(*) AS n "
        "RETURN kind, subkind, n ORDER BY n DESC")
    cluster_rows = "".join(
        f"<tr><td>{html.escape(r[0])}</td>"
        f"<td>{html.escape(r[1] or '—')}</td><td>{r[2]}</td></tr>"
        for r in cluster_counts
    )

    rules = _exec(wv,
        "MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)-[:RULE_INSTANCE_OF]->(c:Concept) "
        "WHERE c.category='spatial' "
        "RETURN DISTINCT h.action_code, c.name")
    rules_html = "".join(
        f"<li><code>ACTION{r[0]}</code> → <b>{html.escape(r[1])}</b></li>"
        for r in sorted(set((int(r[0]), r[1]) for r in rules))
    ) or "<li><i>(no rules yet)</i></li>"

    return f"""
<h3>Summary</h3>
<ul>
  <li>Observations: <b>{stats.get('observation', 0)}</b></li>
  <li>Entities: <b>{stats.get('entity', 0)}</b></li>
  <li>Hypotheses: <b>{stats.get('hypothesis', 0)}</b></li>
  <li>Rules promoted: <b>{stats.get('rule', 0)}</b></li>
  <li>Clusters total: <b>{stats.get('cluster', 0)}</b></li>
  <li>Shapes: <b>{n_shapes}</b> (named: {n_named}, classified: {n_classified})</li>
  <li>Goal attempts: <b>{n_attempts}</b> ({n_success} success)</li>
  <li>Agent: <b>{html.escape(agent_str)}</b></li>
</ul>
<h3>Learned Movable Rules</h3>
<ul>{rules_html}</ul>
<h3>Object Inventory ({len(all_shapes)} shapes)</h3>
<table class="legend">
  <thead><tr><th>name</th><th>role</th><th>area</th><th>seen</th><th>hash</th></tr></thead>
  <tbody>{shape_rows}</tbody>
</table>
<h3>Cluster Breakdown</h3>
<table class="legend">
  <thead><tr><th>kind</th><th>subkind</th><th>count</th></tr></thead>
  <tbody>{cluster_rows}</tbody>
</table>
"""


def render_html(
    wv: Worldview,
    *,
    obs_id: str | None = None,
    output_path: str | Path = "overview.html",
    trajectory_window: int = 30,
) -> Path:
    if obs_id is None:
        rows = _exec(wv,
            "MATCH (o:Observation) RETURN o.id ORDER BY o.frame_n DESC LIMIT 1")
        if not rows:
            raise RuntimeError("No observations in worldview")
        obs_id = rows[0][0]
    assert obs_id is not None

    meta = _exec(wv,
        "MATCH (o:Observation {id: $oid}) "
        "RETURN o.frame_n, o.episode, o.score, o.state",
        {"oid": obs_id})
    frame_n, episode, score, state = meta[0]

    grid = _reconstruct_grid(wv, obs_id)
    fig = go.Figure()
    fig.add_trace(_build_grid_heatmap(grid))
    for tr in _entity_traces(wv, obs_id):
        fig.add_trace(tr)
    for tr in _anchor_traces(wv, obs_id):
        fig.add_trace(tr)
    for tr in _trajectory_trace(wv, trajectory_window):
        fig.add_trace(tr)
    for tr in _goal_attempt_traces(wv):
        fig.add_trace(tr)
    for tr in _path_cluster_traces(wv):
        fig.add_trace(tr)
    for tr in _physics_traces(wv, obs_id):
        fig.add_trace(tr)

    fig.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1,
                     showgrid=True, gridcolor="#333333", dtick=8)
    fig.update_xaxes(showgrid=True, gridcolor="#333333", dtick=8)
    fig.update_layout(
        title=(f"ARC-AGI-3 — episode {episode} · frame {frame_n} · "
               f"score {score} · state {state}"),
        height=820,
        margin=dict(l=10, r=10, t=60, b=10),
        plot_bgcolor="#000000",
        paper_bgcolor="#FFFFFF",
        legend=dict(
            orientation="v",
            yanchor="top", y=1, xanchor="left", x=1.02,
            bgcolor="rgba(255,255,255,0.8)",
            font=dict(size=10),
        ),
        hovermode="closest",
    )
    plot_html = fig.to_html(include_plotlyjs="cdn", full_html=False,
                            div_id="grid-plot")

    summary = _summary_html(wv, obs_id)
    doc = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>ARC-AGI-3 worldview · ep {episode} frame {frame_n}</title>
<style>
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
         margin: 0; padding: 12px; background:#f7f7f7; color:#222; }}
  .layout {{ display: grid; grid-template-columns: 1fr 380px; gap: 12px; }}
  .plot-pane {{ background:white; border-radius:6px; padding:8px;
                box-shadow:0 1px 3px rgba(0,0,0,0.08); }}
  .side {{ background:white; border-radius:6px; padding:12px 16px;
           box-shadow:0 1px 3px rgba(0,0,0,0.08); font-size:13px;
           max-height: 96vh; overflow:auto; }}
  table.legend {{ border-collapse:collapse; width:100%; font-size:11px;
                  margin: 4px 0 12px; }}
  table.legend th, table.legend td {{ border-bottom:1px solid #eee; padding:3px 6px;
                                       text-align:left; }}
  table.legend th {{ background:#fafafa; font-weight:600; }}
  code {{ font-family: 'SF Mono', Consolas, monospace; font-size:11px;
          background:#f0f0f0; padding:1px 4px; border-radius:3px; }}
  h3 {{ margin: 12px 0 4px; font-size: 14px; border-bottom:1px solid #ccc;
        padding-bottom:3px; }}
  ul {{ margin: 4px 0 8px 18px; padding:0; font-size:12px; }}
  .hint {{ color:#666; font-size:11px; margin-top:6px; }}
</style></head><body>
<div class="layout">
  <div class="plot-pane">
    {plot_html}
    <p class="hint">Click any item in the legend to toggle its layer.
    Hover over markers for full detail.</p>
  </div>
  <div class="side">{summary}</div>
</div>
</body></html>"""

    output_path = Path(output_path)
    output_path.write_text(doc, encoding="utf-8")
    return output_path


def render_html_animated(
    wv: Worldview,
    *,
    output_path: str | Path = "playback.html",
    frame_step: int = 1,
    trajectory_window: int = 20,
) -> Path:
    """Build a Plotly figure with one keyframe per Observation so the user
    can scrub / play through the whole episode in the browser."""
    obs_rows = _exec(wv,
        """
        MATCH (o:Observation)
        RETURN o.id, o.frame_n, o.episode, o.score, o.state
        ORDER BY o.frame_n
        """)
    if not obs_rows:
        raise RuntimeError("No observations in worldview")
    obs_rows = obs_rows[::max(1, int(frame_step))]
    n_frames = len(obs_rows)

    # Pre-compute static lookups so per-frame work is fast.
    agent_row = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept = 'Agent' RETURN s.shape_hash LIMIT 1")
    agent_shape = agent_row[0][0] if agent_row else None

    avatar_history = []
    if agent_shape:
        avatar_history = _exec(wv,
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: $sh})
            RETURN o.frame_n, e.centroid_y, e.centroid_x ORDER BY o.frame_n
            """,
            {"sh": agent_shape})
    avatar_by_frame: dict[int, tuple[float, float]] = {
        int(r[0]): (float(r[1]), float(r[2])) for r in avatar_history
    }

    all_attempts = _exec(wv,
        "MATCH (a:GoalAttempt) RETURN a.source, a.centroid_y, a.centroid_x, "
        "a.frames_invested, a.reward_delta, a.status, a.attempted_at "
        "ORDER BY a.attempted_at")

    physics_rows = _exec(wv,
        """
        MATCH (p:Cluster {kind:'physics'})-[:EXPLAINED_BY]->(c:Cluster)
        MATCH (o:Observation)-[:OBSERVED_AS]->(c)
        RETURN DISTINCT p.subkind, p.centroid_y, p.centroid_x,
                        p.signature, o.frame_n
        """)
    physics_by_frame: dict[int, list] = {}
    for r in physics_rows:
        physics_by_frame.setdefault(int(r[4]), []).append(r)

    def build_frame_traces(obs_id: str, frame_n: int):
        grid = _reconstruct_grid(wv, obs_id)
        ents = _exec(wv,
            """
            MATCH (o:Observation {id: $oid})-[:CONTAINS]->(e:Entity)
                  -[:HAS_SHAPE]->(s:Shape)
            RETURN e.color, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1,
                   e.centroid_y, e.centroid_x, e.area, e.shape_hash,
                   s.llm_name, s.role_concept
            """,
            {"oid": obs_id})
        ent_xs, ent_ys, ent_hovers, ent_colors = [], [], [], []
        role_palette = {
            "Agent": "#FF2222", "Goal": "#33FF33", "Collectible": "#33FF66",
            "Background": "#888888", "Border": "#999999", "HUDElement": "#AAAA88",
            "DirectionIndicator": "#FFCC00", "Container": "#3399FF",
            "Obstacle": "#FF9933",
        }
        for color, r0, c0, r1, c1, cy, cx, area, sh, name, role in ents:
            ent_xs.append(float(cx))
            ent_ys.append(float(cy))
            ent_hovers.append(
                f"<b>{name or 'unnamed'}</b><br>role={role or '—'}<br>"
                f"color={color} area={area}<br>"
                f"bbox=({r0},{c0})-({r1},{c1})<br>shape={sh[:10]}"
            )
            ent_colors.append(role_palette.get(role or "", "#FFFF00"))

        # Trajectory: window prior to current frame.
        traj_pts = [(f, *avatar_by_frame[f]) for f in sorted(avatar_by_frame)
                    if f <= frame_n]
        traj_pts = traj_pts[-trajectory_window:]
        traj_xs = [p[2] for p in traj_pts]
        traj_ys = [p[1] for p in traj_pts]
        avatar_xs = traj_xs[-1:] if traj_xs else []
        avatar_ys = traj_ys[-1:] if traj_ys else []

        # Goal attempts: only those attempted up to this frame's wall-clock.
        # (We use attempted_at ordering — approximated by index since the
        # database doesn't link attempts to specific frames yet.)
        max_attempt_idx = min(len(all_attempts), max(0, frame_n - 14) // 15 + 1)
        attempts_so_far = all_attempts[:max_attempt_idx]
        gx = [float(a[2]) for a in attempts_so_far if a[5] != "success"]
        gy = [float(a[1]) for a in attempts_so_far if a[5] != "success"]
        gh = [f"goal {i+1}: {a[0]} ({a[1]:.0f},{a[2]:.0f}) frames={a[3]} → {a[5]}"
              for i, a in enumerate(attempts_so_far) if a[5] != "success"]
        sx = [float(a[2]) for a in attempts_so_far if a[5] == "success"]
        sy = [float(a[1]) for a in attempts_so_far if a[5] == "success"]
        sh_h = [f"<b>SUCCESS</b><br>goal {i+1}: {a[0]} ({a[1]:.0f},{a[2]:.0f})"
                for i, a in enumerate(attempts_so_far) if a[5] == "success"]

        # Physics events visible at or before this frame.
        phys = []
        for fn, evs in physics_by_frame.items():
            if fn <= frame_n:
                phys.extend(evs)
        px = [float(r[2]) for r in phys]
        py = [float(r[1]) for r in phys]
        ph = [f"physics·{r[0]}<br>frame={r[4]}<br>centroid=({r[1]:.0f},{r[2]:.0f})"
              for r in phys]
        pc = [PHYSICS_STYLE.get(r[0], {"color": "white"})["color"] for r in phys]

        return [
            go.Heatmap(z=grid,
                      colorscale=[(i / 15, ARC_PALETTE[i]) for i in range(16)],
                      zmin=0, zmax=15, showscale=False,
                      hovertemplate="cell (%{y}, %{x})<br>color=%{z}<extra></extra>",
                      name="grid"),
            go.Scatter(x=ent_xs, y=ent_ys, mode="markers",
                       marker=dict(symbol="square-open",
                                   color=ent_colors, size=12,
                                   line=dict(width=2)),
                       text=ent_hovers, hoverinfo="text", name="entities"),
            go.Scatter(x=traj_xs, y=traj_ys, mode="lines",
                       line=dict(color="red", width=1.5),
                       hoverinfo="skip", name="trajectory"),
            go.Scatter(x=avatar_xs, y=avatar_ys, mode="markers",
                       marker=dict(symbol="star", color="red", size=20,
                                   line=dict(color="black", width=1)),
                       hovertemplate="avatar @ frame "
                                     f"{frame_n}<extra></extra>",
                       name="avatar"),
            go.Scatter(x=gx, y=gy, mode="markers",
                       marker=dict(symbol="x", color="#FF9933", size=14,
                                   line=dict(color="black", width=1)),
                       text=gh, hoverinfo="text",
                       name="goal attempts · failed"),
            go.Scatter(x=sx, y=sy, mode="markers",
                       marker=dict(symbol="star", color="#00FF00", size=18,
                                   line=dict(color="black", width=1)),
                       text=sh_h, hoverinfo="text",
                       name="goal attempts · success"),
            go.Scatter(x=px, y=py, mode="markers",
                       marker=dict(symbol="diamond", color=pc, size=10,
                                   line=dict(color="black", width=1)),
                       text=ph, hoverinfo="text", name="physics events"),
        ]

    # Build the initial figure from frame 0.
    obs0_id, frame_n0, *_meta = obs_rows[0]
    initial_traces = build_frame_traces(obs0_id, int(frame_n0))

    frames: list[go.Frame] = []
    for obs_id, frame_n, episode, score, state in obs_rows:
        traces = build_frame_traces(obs_id, int(frame_n))
        frames.append(go.Frame(
            data=traces,
            name=str(int(frame_n)),
            layout=go.Layout(
                title=(f"episode {episode} · frame {frame_n} · "
                       f"score {score} · state {state}"),
            ),
        ))

    fig = go.Figure(data=initial_traces, frames=frames)
    fig.update_yaxes(autorange="reversed", scaleanchor="x", scaleratio=1,
                     showgrid=True, gridcolor="#333333", dtick=8)
    fig.update_xaxes(showgrid=True, gridcolor="#333333", dtick=8)

    slider_steps = [
        dict(method="animate",
             args=[[f.name],
                   {"mode": "immediate",
                    "frame": {"duration": 0, "redraw": True},
                    "transition": {"duration": 0}}],
             label=f.name)
        for f in frames
    ]
    fig.update_layout(
        title=(f"ARC-AGI-3 playback · {n_frames} keyframes"),
        height=820,
        margin=dict(l=10, r=10, t=80, b=70),
        plot_bgcolor="#000000",
        paper_bgcolor="#FFFFFF",
        hovermode="closest",
        updatemenus=[dict(
            type="buttons",
            direction="left",
            x=0, y=-0.05, xanchor="left", yanchor="top",
            showactive=False,
            buttons=[
                dict(label="▶ Play",
                     method="animate",
                     args=[None,
                           {"frame": {"duration": 200, "redraw": True},
                            "fromcurrent": True,
                            "transition": {"duration": 0}}]),
                dict(label="⏸ Pause",
                     method="animate",
                     args=[[None],
                           {"frame": {"duration": 0, "redraw": False},
                            "mode": "immediate",
                            "transition": {"duration": 0}}]),
            ]
        )],
        sliders=[dict(
            active=0,
            currentvalue=dict(prefix="frame "),
            steps=slider_steps,
            pad=dict(t=40),
            x=0.1, y=-0.05, len=0.85,
        )],
    )

    plot_html = fig.to_html(include_plotlyjs="cdn", full_html=False,
                            div_id="playback")
    doc = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<title>ARC-AGI-3 playback · {n_frames} frames</title>
<style>
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
         margin: 0; padding: 8px; background:#f7f7f7; }}
  .container {{ background:white; border-radius:6px; padding:8px;
                box-shadow:0 1px 3px rgba(0,0,0,0.08); }}
  .hint {{ color:#666; font-size:11px; margin-top:6px; }}
</style></head><body>
<div class="container">
  {plot_html}
  <p class="hint">▶ plays the episode at 5 fps. Drag the slider to scrub.
  Click legend entries to toggle layers. Hover over markers for full detail.</p>
</div>
</body></html>"""
    output_path = Path(output_path)
    output_path.write_text(doc, encoding="utf-8")
    return output_path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=str, required=True)
    parser.add_argument("--out", type=str, default="overview.html")
    parser.add_argument("--frame", type=int, default=None)
    parser.add_argument("--trajectory-window", type=int, default=30)
    parser.add_argument("--animate", action="store_true",
                        help="render an animated playback of the whole episode")
    parser.add_argument("--frame-step", type=int, default=1,
                        help="when --animate, sample every Nth frame")
    args = parser.parse_args()

    wv = Worldview(args.db)
    try:
        if args.animate:
            out = render_html_animated(
                wv, output_path=args.out,
                frame_step=args.frame_step,
                trajectory_window=args.trajectory_window,
            )
            print(f"wrote animated {out}  ({out.stat().st_size // 1024} KB)")
            return 0
        obs_id = None
        if args.frame is not None:
            rows = _exec(wv,
                "MATCH (o:Observation {frame_n: $f}) RETURN o.id LIMIT 1",
                {"f": args.frame})
            if not rows:
                raise SystemExit(f"No observation with frame_n={args.frame}")
            obs_id = rows[0][0]
        out = render_html(wv, obs_id=obs_id, output_path=args.out,
                          trajectory_window=args.trajectory_window)
        print(f"wrote {out}  ({out.stat().st_size // 1024} KB)")
    finally:
        wv.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
