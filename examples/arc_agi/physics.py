"""Physics cluster detectors — emergent laws of the game world.

Each detector queries the worldview graph for recurring patterns and
materializes them as :Cluster nodes with ``kind='physics'`` and a specific
``subkind`` (gravity, inertia, friction, collision, bounce, push, adhesion,
magnetism, destruction, construction, spawn, decay, growth, teleport,
wrap_around, smoke, shadow, light, marks, alert, wave, mirror,
chain_reaction, trigger_effect).

All detectors are post-hoc — they operate on the already-recorded observation /
cluster / entity graph rather than per-frame. This keeps the hot loop fast and
lets the detection logic exploit the full episode window.
"""

from __future__ import annotations

import hashlib
import uuid
from collections import defaultdict


def _sig(parts: list[str]) -> str:
    return hashlib.blake2b("|".join(parts).encode(), digest_size=8).hexdigest()


def _exec(wv, query: str, params: dict | None = None):
    res = wv.conn.execute(query, params or {})
    qr = res[0] if isinstance(res, list) else res
    rows = []
    while qr.has_next():
        rows.append(qr.get_next())
    return rows


def _record_physics(
    wv, subkind: str, signature: str, *,
    bbox=(0, 0, 0, 0), centroid=(0.0, 0.0),
    velocity=(0, 0), n_members: int = 1,
    discovered_at_obs: str = "post-hoc",
) -> str | None:
    """Insert a physics cluster if its signature is novel for this subkind."""
    cluster_id = f"clu:physics:{subkind}:{signature}:{uuid.uuid4().hex[:6]}"
    # Check no existing duplicate by (subkind, signature)
    existing = _exec(wv,
        "MATCH (c:Cluster {kind:'physics', subkind:$sk, signature:$sig}) "
        "RETURN c.id LIMIT 1",
        {"sk": subkind, "sig": signature})
    if existing:
        return None
    wv.conn.execute(
        "CREATE (cl:Cluster {id: $id, kind: 'physics', subkind: $sk, signature: $sig, "
        "n_members: $n, bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1, "
        "centroid_y: $cy, centroid_x: $cx, "
        "velocity_dy: $vdy, velocity_dx: $vdx, rotation_deg: 0, "
        "discovered_at_obs: $obs})",
        {
            "id": cluster_id, "sk": subkind, "sig": signature, "n": n_members,
            "r0": bbox[0], "c0": bbox[1], "r1": bbox[2], "c1": bbox[3],
            "cy": centroid[0], "cx": centroid[1],
            "vdy": velocity[0], "vdx": velocity[1],
            "obs": discovered_at_obs,
        },
    )
    # Tag with PhysicsCluster + specific subkind concept.
    for concept_name in ("PhysicsCluster", "Cluster", _subkind_to_concept(subkind)):
        if not concept_name:
            continue
        wv.conn.execute(
            "MATCH (cl:Cluster {id: $cid}), (c:Concept {id: $kid}) "
            "CREATE (cl)-[:CLUSTER_INSTANCE_OF]->(c)",
            {"cid": cluster_id, "kid": f"concept:{concept_name}"},
        )
    return cluster_id


SUBKIND_CONCEPT = {
    "gravity": "GravityLaw", "inertia": "InertiaLaw", "friction": "FrictionLaw",
    "collision": "CollisionEvent", "bounce": "BounceEvent", "push": "PushEvent",
    "adhesion": "AdhesionEvent", "magnetism": "MagnetismLaw",
    "destruction": "DestructionEvent", "construction": "ConstructionEvent",
    "spawn": "SpawnLaw", "decay": "DecayLaw", "growth": "GrowthLaw",
    "teleport": "TeleportEvent", "wrap_around": "WrapAroundLaw",
    "smoke": "SmokeEffect", "shadow": "ShadowEffect", "light": "LightEffect",
    "marks": "MarkEffect", "alert": "AlertEffect", "wave": "WaveEffect",
    "mirror": "MirrorRelation", "chain_reaction": "ChainReaction",
    "trigger_effect": "TriggerEffect",
}


def _subkind_to_concept(subkind: str) -> str:
    return SUBKIND_CONCEPT.get(subkind, "")


# =============================================================================
# Tier 1: per-transition (2-frame window)
# =============================================================================

def detect_destruction(wv) -> int:
    """Motion/Proximity clusters with no successor → destruction events."""
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(cl:Cluster)
        WHERE cl.kind IN ['motion','proximity'] AND o.frame_n < $max_frame
          AND NOT EXISTS { MATCH (cl)-[:NEXT_CLUSTER]->() }
        RETURN cl.id, cl.signature, cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1,
               cl.centroid_y, cl.centroid_x
        """,
        {"max_frame": _max_frame(wv)})
    n = 0
    for r in rows:
        cid, sig, r0, c0, r1, c1, cy, cx = r
        new_id = _record_physics(wv, "destruction", _sig([f"destroy:{sig}:{cid}"]),
                                  bbox=(r0, c0, r1, c1), centroid=(cy, cx))
        if new_id:
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'source'}]->(cl)",
                {"pid": new_id, "cid": cid})
            n += 1
    return n


def detect_construction(wv) -> int:
    """Motion/Proximity clusters with no predecessor → construction events."""
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(cl:Cluster)
        WHERE cl.kind IN ['motion','proximity'] AND o.frame_n > 0
          AND NOT EXISTS { MATCH ()-[:NEXT_CLUSTER]->(cl) }
        RETURN cl.id, cl.signature, cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1,
               cl.centroid_y, cl.centroid_x
        """)
    n = 0
    for r in rows:
        cid, sig, r0, c0, r1, c1, cy, cx = r
        new_id = _record_physics(wv, "construction", _sig([f"construct:{sig}:{cid}"]),
                                  bbox=(r0, c0, r1, c1), centroid=(cy, cx))
        if new_id:
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'target'}]->(cl)",
                {"pid": new_id, "cid": cid})
            n += 1
    return n


def detect_collision(wv) -> int:
    """Two motion clusters in the same prev whose bboxes converge into overlap."""
    # Collisions: pairs of motion clusters in same observation with directions that intersect.
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(a:Cluster {kind:'motion'})
        MATCH (o)-[:OBSERVED_AS]->(b:Cluster {kind:'motion'})
        WHERE a.id < b.id
        RETURN a.id, b.id,
               a.centroid_y, a.centroid_x, a.velocity_dy, a.velocity_dx,
               b.centroid_y, b.centroid_x, b.velocity_dy, b.velocity_dx
        """)
    n = 0
    for r in rows:
        a_id, b_id, ay, ax, avdy, avdx, by, bx, bvdy, bvdx = r
        # Predict positions after 1 step.
        a_next = (ay + avdy, ax + avdx)
        b_next = (by + bvdy, bx + bvdx)
        if abs(a_next[0] - b_next[0]) <= 1 and abs(a_next[1] - b_next[1]) <= 1:
            sig = _sig([f"collide:{a_id}:{b_id}"])
            new_id = _record_physics(wv, "collision", sig,
                                      bbox=(int(min(ay, by)), int(min(ax, bx)),
                                            int(max(ay, by)), int(max(ax, bx))),
                                      centroid=((ay + by) / 2, (ax + bx) / 2))
            if new_id:
                for src in (a_id, b_id):
                    wv.conn.execute(
                        "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                        "CREATE (p)-[:EXPLAINED_BY {role:'participant'}]->(cl)",
                        {"pid": new_id, "cid": src})
                n += 1
    return n


def detect_bounce(wv) -> int:
    """Motion cluster whose velocity in t+1 is the negation of velocity in t."""
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'motion'})
        WHERE a.velocity_dy + b.velocity_dy = 0
          AND a.velocity_dx + b.velocity_dx = 0
          AND (a.velocity_dy <> 0 OR a.velocity_dx <> 0)
        RETURN a.id, b.id, a.centroid_y, a.centroid_x,
               a.velocity_dy, a.velocity_dx
        """)
    n = 0
    for r in rows:
        a_id, b_id, ay, ax, vdy, vdx = r
        sig = _sig([f"bounce:{a_id}:{b_id}"])
        new_id = _record_physics(wv, "bounce", sig,
                                  bbox=(int(ay), int(ax), int(ay), int(ax)),
                                  centroid=(ay, ax),
                                  velocity=(vdy, vdx))
        if new_id:
            for src in (a_id, b_id):
                wv.conn.execute(
                    "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                    "CREATE (p)-[:EXPLAINED_BY {role:'reflection'}]->(cl)",
                    {"pid": new_id, "cid": src})
            n += 1
    return n


def detect_push(wv) -> int:
    """A motion cluster A reaches a previously static entity B; in next frame, B becomes
    part of a motion cluster with similar velocity to A.
    """
    # Heuristic: two motion clusters in the same observation with adjacent bboxes
    # AND matching velocity → push event.
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(a:Cluster {kind:'motion'})
        MATCH (o)-[:OBSERVED_AS]->(b:Cluster {kind:'motion'})
        WHERE a.id < b.id
          AND a.velocity_dy = b.velocity_dy AND a.velocity_dx = b.velocity_dx
        RETURN a.id, b.id, a.centroid_y, a.centroid_x, b.centroid_y, b.centroid_x,
               a.velocity_dy, a.velocity_dx
        """)
    n = 0
    for r in rows:
        a_id, b_id, ay, ax, by, bx, vdy, vdx = r
        # Centroids close → adjacent → push.
        if abs(ay - by) <= 6 and abs(ax - bx) <= 6:
            sig = _sig([f"push:{a_id}:{b_id}"])
            new_id = _record_physics(wv, "push", sig,
                                      bbox=(int(min(ay, by)), int(min(ax, bx)),
                                            int(max(ay, by)), int(max(ax, bx))),
                                      centroid=((ay + by) / 2, (ax + bx) / 2),
                                      velocity=(vdy, vdx))
            if new_id:
                wv.conn.execute(
                    "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                    "CREATE (p)-[:EXPLAINED_BY {role:'pusher'}]->(cl)",
                    {"pid": new_id, "cid": a_id})
                wv.conn.execute(
                    "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                    "CREATE (p)-[:EXPLAINED_BY {role:'pushed'}]->(cl)",
                    {"pid": new_id, "cid": b_id})
                n += 1
    return n


def detect_adhesion(wv) -> int:
    """Two clusters that previously had different velocities now share velocity."""
    # Look at NEXT_CLUSTER chains where both clusters in the SAME observation moved together.
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'motion'})
        MATCH (o2:Observation)-[:OBSERVED_AS]->(b)
        MATCH (o2)-[:OBSERVED_AS]->(c:Cluster {kind:'motion'})
        WHERE b.id <> c.id
          AND b.velocity_dy = c.velocity_dy AND b.velocity_dx = c.velocity_dx
          AND (b.velocity_dy <> 0 OR b.velocity_dx <> 0)
        RETURN DISTINCT b.id, c.id, b.velocity_dy, b.velocity_dx,
               b.centroid_y, b.centroid_x
        LIMIT 50
        """)
    n = 0
    for r in rows:
        b_id, c_id, vdy, vdx, cy, cx = r
        sig = _sig([f"adhesion:{b_id}:{c_id}"])
        new_id = _record_physics(wv, "adhesion", sig, centroid=(cy, cx),
                                  velocity=(vdy, vdx))
        if new_id:
            for src in (b_id, c_id):
                wv.conn.execute(
                    "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                    "CREATE (p)-[:EXPLAINED_BY {role:'adhered'}]->(cl)",
                    {"pid": new_id, "cid": src})
            n += 1
    return n


def detect_teleport(wv) -> int:
    """In a single transition: a cluster vanished at A and an identical-signature
    cluster appeared at B (different position)."""
    # Build by-transition lookup of motion clusters with same signature but
    # disconnected (no NEXT_CLUSTER edge).
    rows = _exec(wv,
        """
        MATCH (o1:Observation)-[t:TRANSITION]->(o2:Observation)
        MATCH (o1)-[:OBSERVED_AS]->(a:Cluster)
        MATCH (o2)-[:OBSERVED_AS]->(b:Cluster)
        WHERE a.signature = b.signature AND a.id <> b.id AND a.kind = b.kind
          AND NOT EXISTS { MATCH (a)-[:NEXT_CLUSTER]->(b) }
          AND (abs(a.centroid_y - b.centroid_y) > 8 OR abs(a.centroid_x - b.centroid_x) > 8)
        RETURN a.id, b.id, a.centroid_y, a.centroid_x, b.centroid_y, b.centroid_x
        LIMIT 50
        """)
    n = 0
    for r in rows:
        a_id, b_id, ay, ax, by, bx = r
        sig = _sig([f"teleport:{a_id}:{b_id}"])
        new_id = _record_physics(wv, "teleport", sig,
                                  centroid=((ay + by) / 2, (ax + bx) / 2))
        if new_id:
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'src'}]->(cl)",
                {"pid": new_id, "cid": a_id})
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'dst'}]->(cl)",
                {"pid": new_id, "cid": b_id})
            n += 1
    return n


# =============================================================================
# Tier 2: K-frame window post-hoc
# =============================================================================

def _max_frame(wv) -> int:
    rows = _exec(wv, "MATCH (o:Observation) RETURN max(o.frame_n)")
    return int(rows[0][0]) if rows and rows[0][0] is not None else 0


def detect_gravity(wv, *, min_streak: int = 3) -> int:
    """Motion clusters across a streak of frames all sharing same down velocity
    (positive dy, zero dx) — i.e., entities accelerating/falling without action input.

    We look at successor chains of motion clusters via NEXT_CLUSTER edges with
    consistent dy>0, dx=0 over ≥ min_streak transitions.
    """
    # Naive: count, per shape_hash, the number of transitions where motion was (>0, 0).
    rows = _exec(wv,
        """
        MATCH (e:Entity)-[:PART_OF]->(cl:Cluster {kind:'motion'})
        WHERE cl.velocity_dy > 0 AND cl.velocity_dx = 0
        RETURN e.shape_hash, count(DISTINCT cl) AS n
        """)
    n_new = 0
    for sh, count_clusters in rows:
        if int(count_clusters) >= min_streak:
            sig = _sig([f"gravity:{sh}"])
            new_id = _record_physics(wv, "gravity", sig,
                                      n_members=int(count_clusters),
                                      velocity=(1, 0))
            if new_id:
                n_new += 1
    return n_new


def detect_inertia(wv, *, min_streak: int = 2) -> int:
    """Two consecutive motion clusters with the same velocity for the same shape."""
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'motion'})
        WHERE a.velocity_dy = b.velocity_dy AND a.velocity_dx = b.velocity_dx
          AND (a.velocity_dy <> 0 OR a.velocity_dx <> 0)
        RETURN a.id, b.id, a.velocity_dy, a.velocity_dx
        """)
    n_new = 0
    for r in rows:
        a_id, b_id, vdy, vdx = r
        sig = _sig([f"inertia:{a_id}:{b_id}"])
        new_id = _record_physics(wv, "inertia", sig, velocity=(vdy, vdx))
        if new_id:
            for src in (a_id, b_id):
                wv.conn.execute(
                    "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                    "CREATE (p)-[:EXPLAINED_BY {role:'continued'}]->(cl)",
                    {"pid": new_id, "cid": src})
            n_new += 1
    return n_new


def detect_friction(wv) -> int:
    """Two consecutive motion clusters where the second's |velocity| is strictly smaller
    than the first's, in the same direction.
    """
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'motion'})
        WHERE (a.velocity_dy * b.velocity_dy + a.velocity_dx * b.velocity_dx) > 0
          AND (a.velocity_dy * a.velocity_dy + a.velocity_dx * a.velocity_dx)
              > (b.velocity_dy * b.velocity_dy + b.velocity_dx * b.velocity_dx)
        RETURN a.id, b.id
        """)
    n_new = 0
    for a_id, b_id in rows:
        sig = _sig([f"friction:{a_id}:{b_id}"])
        new_id = _record_physics(wv, "friction", sig)
        if new_id:
            n_new += 1
    return n_new


def detect_spawn(wv, *, min_observations: int = 3) -> int:
    """A cluster signature that appears in ≥ N observations, repeatedly in
    nearby positions, with no predecessor → spawn law.
    """
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(cl:Cluster)
        WHERE NOT EXISTS { MATCH ()-[:NEXT_CLUSTER]->(cl) }
          AND o.frame_n > 0
        RETURN cl.signature, cl.kind, count(DISTINCT o.frame_n) AS n_frames,
               avg(cl.centroid_y) AS cy, avg(cl.centroid_x) AS cx
        """)
    n_new = 0
    for sig_raw, kind, n_frames, cy, cx in rows:
        if int(n_frames) < min_observations:
            continue
        sig = _sig([f"spawn:{kind}:{sig_raw}"])
        new_id = _record_physics(wv, "spawn", sig,
                                  centroid=(float(cy or 0), float(cx or 0)),
                                  n_members=int(n_frames))
        if new_id:
            n_new += 1
    return n_new


def detect_decay_growth(wv) -> tuple[int, int]:
    """Track Shape.area trends across consecutive observations. If a shape's
    bounding rectangle shrinks consistently → decay; if it grows → growth.

    Approximation: compare bbox areas of same-color entities across consecutive
    observations whose centroids are within a small radius.
    """
    # Simpler proxy: shapes whose n_observations grew (we don't track per-frame
    # area trend yet). Skip if no useful signal.
    return 0, 0


def detect_wrap_around(wv) -> int:
    """Motion cluster whose centroid jumps from one edge to the opposite edge in a
    single transition."""
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'motion'})
        WHERE abs(a.centroid_y - b.centroid_y) > 50 OR abs(a.centroid_x - b.centroid_x) > 50
        RETURN a.id, b.id
        """)
    n_new = 0
    for a_id, b_id in rows:
        sig = _sig([f"wrap:{a_id}:{b_id}"])
        new_id = _record_physics(wv, "wrap_around", sig)
        if new_id:
            n_new += 1
    return n_new


def detect_marks(wv) -> int:
    """DiffClusters that persist (no successor diff at same location → mark stayed)."""
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(d:Cluster {kind:'diff'})
        WHERE NOT EXISTS { MATCH (d)-[:NEXT_CLUSTER]->() }
        RETURN d.id, d.centroid_y, d.centroid_x, d.bbox_r0, d.bbox_c0, d.bbox_r1, d.bbox_c1
        """)
    n_new = 0
    for d_id, cy, cx, r0, c0, r1, c1 in rows:
        sig = _sig([f"marks:{d_id}"])
        new_id = _record_physics(wv, "marks", sig,
                                  bbox=(r0, c0, r1, c1), centroid=(cy, cx))
        if new_id:
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'trail'}]->(cl)",
                {"pid": new_id, "cid": d_id})
            n_new += 1
    return n_new


def detect_smoke(wv, *, fade_window: int = 5) -> int:
    """DiffClusters that DO have a successor (vanish within K frames) AND occur
    near a contemporary motion cluster's path → smoke trail.
    """
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(d:Cluster {kind:'diff'})
        WHERE EXISTS { MATCH (d)-[:NEXT_CLUSTER]->() }
        MATCH (o)-[:OBSERVED_AS]->(m:Cluster {kind:'motion'})
        WHERE abs(d.centroid_y - m.centroid_y) <= 5
          AND abs(d.centroid_x - m.centroid_x) <= 5
        RETURN DISTINCT d.id, m.id
        LIMIT 50
        """)
    n_new = 0
    for d_id, m_id in rows:
        sig = _sig([f"smoke:{d_id}:{m_id}"])
        new_id = _record_physics(wv, "smoke", sig)
        if new_id:
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'visual'}]->(cl)",
                {"pid": new_id, "cid": d_id})
            wv.conn.execute(
                "MATCH (p:Cluster {id:$pid}), (cl:Cluster {id:$cid}) "
                "CREATE (p)-[:EXPLAINED_BY {role:'source'}]->(cl)",
                {"pid": new_id, "cid": m_id})
            n_new += 1
    return n_new


def detect_shadow(wv) -> int:
    """Pair of motion clusters in same observation with same velocity AND constant
    spatial offset across multiple transitions."""
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(a:Cluster {kind:'motion'})
        MATCH (o)-[:OBSERVED_AS]->(b:Cluster {kind:'motion'})
        WHERE a.id < b.id
          AND a.velocity_dy = b.velocity_dy AND a.velocity_dx = b.velocity_dx
        WITH a.signature AS sa, b.signature AS sb,
             a.centroid_y - b.centroid_y AS dy, a.centroid_x - b.centroid_x AS dx,
             count(*) AS n
        WHERE n >= 2
        RETURN sa, sb, dy, dx, n
        """)
    n_new = 0
    for sa, sb, dy, dx, n in rows:
        sig = _sig([f"shadow:{sa}:{sb}:{dy:.0f}:{dx:.0f}"])
        new_id = _record_physics(wv, "shadow", sig, n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_light(wv) -> int:
    """ColorGroup clusters whose bbox grew/shrank radially across consecutive frames."""
    # Lightweight heuristic: same-signature color_group whose centroid stays put
    # but bbox area changes monotonically.
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'color_group'})-[:NEXT_CLUSTER]->(b:Cluster {kind:'color_group'})
        WHERE abs(a.centroid_y - b.centroid_y) < 1.0 AND abs(a.centroid_x - b.centroid_x) < 1.0
          AND (b.bbox_r1 - b.bbox_r0) * (b.bbox_c1 - b.bbox_c0)
            <> (a.bbox_r1 - a.bbox_r0) * (a.bbox_c1 - a.bbox_c0)
        RETURN a.signature, count(*) AS n
        """)
    n_new = 0
    for sig_a, n in rows:
        if int(n) < 2:
            continue
        sig = _sig([f"light:{sig_a}"])
        new_id = _record_physics(wv, "light", sig, n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_alert(wv) -> int:
    """Cluster whose color/signature oscillates between two values over consecutive frames."""
    rows = _exec(wv,
        """
        MATCH (a:Cluster)-[:NEXT_CLUSTER]->(b:Cluster)-[:NEXT_CLUSTER]->(c:Cluster)
        WHERE a.signature = c.signature AND a.signature <> b.signature
        RETURN a.signature, b.signature, count(*) AS n
        """)
    n_new = 0
    for sig_a, sig_b, n in rows:
        if int(n) < 2:
            continue
        sig = _sig([f"alert:{sig_a}:{sig_b}"])
        new_id = _record_physics(wv, "alert", sig, n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_wave(wv) -> int:
    """A pattern (motion cluster signature) that recurs at consecutive positions
    in a straight line over multiple frames."""
    # Same signature, NEXT_CLUSTER chain ≥ 3, with consistent dy/dx per step.
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'motion'})-[t1:NEXT_CLUSTER]->(b:Cluster)
                                          -[t2:NEXT_CLUSTER]->(c:Cluster)
        WHERE a.signature = b.signature AND b.signature = c.signature
          AND t1.dy = t2.dy AND t1.dx = t2.dx
        RETURN a.signature, t1.dy, t1.dx, count(*) AS n
        """)
    n_new = 0
    for sig_a, dy, dx, n in rows:
        if int(n) < 1:
            continue
        sig = _sig([f"wave:{sig_a}:{dy}:{dx}"])
        new_id = _record_physics(wv, "wave", sig,
                                  velocity=(int(dy), int(dx)),
                                  n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_magnetism(wv) -> int:
    """Motion clusters whose velocity vector consistently points toward another
    fixed cluster across multiple transitions."""
    # Cross-pair query: for each motion cluster M and each anchor A, check if
    # M's velocity vector aims at A's centroid.
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(m:Cluster {kind:'motion'})
        MATCH (o)-[:OBSERVED_AS]->(a:Cluster)
        WHERE a.kind IN ['anchor','proximity']
          AND (m.velocity_dy * (a.centroid_y - m.centroid_y) +
               m.velocity_dx * (a.centroid_x - m.centroid_x)) > 0
        WITH a.signature AS asig, m.signature AS msig, count(*) AS n
        WHERE n >= 3
        RETURN asig, msig, n
        LIMIT 20
        """)
    n_new = 0
    for asig, msig, n in rows:
        sig = _sig([f"magnetism:{asig}:{msig}"])
        new_id = _record_physics(wv, "magnetism", sig, n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_mirror(wv) -> int:
    """Pair of motion clusters with mirrored velocities across an axis."""
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:OBSERVED_AS]->(a:Cluster {kind:'motion'})
        MATCH (o)-[:OBSERVED_AS]->(b:Cluster {kind:'motion'})
        WHERE a.id < b.id
          AND (
              (a.velocity_dy + b.velocity_dy = 0 AND a.velocity_dx = b.velocity_dx)
            OR (a.velocity_dx + b.velocity_dx = 0 AND a.velocity_dy = b.velocity_dy)
          )
        RETURN a.signature, b.signature, count(*) AS n
        """)
    n_new = 0
    for sa, sb, n in rows:
        if int(n) < 2:
            continue
        sig = _sig([f"mirror:{sa}:{sb}"])
        new_id = _record_physics(wv, "mirror", sig, n_members=int(n))
        if new_id:
            n_new += 1
    return n_new


def detect_chain_reaction(wv) -> int:
    """Single transition produces multiple NON-color_group cluster changes.

    Filters out color_group transformations (which trivially fire for any pixel
    change), requiring ≥ 5 transformations of OTHER cluster kinds to qualify.
    """
    rows = _exec(wv,
        """
        MATCH (o1:Observation)-[t:TRANSITION]->(o2:Observation)
        WITH o1.frame_n AS f, t.action_code AS ac
        RETURN f, ac
        LIMIT 50
        """)
    n_new = 0
    for f, ac in rows:
        if ac is None:
            continue
        sub = _exec(wv,
            """
            MATCH (o1:Observation {frame_n: $f})-[:OBSERVED_AS]->(c:Cluster)-[n:NEXT_CLUSTER]->()
            WHERE n.change_kind = 'transformed' AND c.kind <> 'color_group'
            RETURN count(c) AS n
            """,
            {"f": f})
        if not sub:
            continue
        n_changes = int(sub[0][0])
        if n_changes < 5:
            continue
        sig = _sig([f"chain:{f}:{ac}"])
        new_id = _record_physics(wv, "chain_reaction", sig, n_members=n_changes)
        if new_id:
            n_new += 1
    return n_new


def detect_trigger_effect(wv) -> int:
    """Anchor cluster A whose state changes (transformed) and a DISTANT cluster B
    changes in the next observation."""
    rows = _exec(wv,
        """
        MATCH (a:Cluster {kind:'anchor'})-[:NEXT_CLUSTER {change_kind:'transformed'}]->()
        MATCH (b:Cluster)-[:NEXT_CLUSTER]->()
        WHERE a.id <> b.id
          AND abs(a.centroid_y - b.centroid_y) + abs(a.centroid_x - b.centroid_x) > 20
        RETURN DISTINCT a.signature, b.signature
        LIMIT 20
        """)
    n_new = 0
    for asig, bsig in rows:
        sig = _sig([f"trigger:{asig}:{bsig}"])
        new_id = _record_physics(wv, "trigger_effect", sig)
        if new_id:
            n_new += 1
    return n_new


# =============================================================================
# Master entry
# =============================================================================

def discover_all_physics(wv) -> dict[str, int]:
    """Run every detector. Returns counts per subkind."""
    return {
        # Tier 1
        "destruction": detect_destruction(wv),
        "construction": detect_construction(wv),
        "collision": detect_collision(wv),
        "bounce": detect_bounce(wv),
        "push": detect_push(wv),
        "adhesion": detect_adhesion(wv),
        "teleport": detect_teleport(wv),
        # Tier 2
        "gravity": detect_gravity(wv),
        "inertia": detect_inertia(wv),
        "friction": detect_friction(wv),
        "spawn": detect_spawn(wv),
        "wrap_around": detect_wrap_around(wv),
        "marks": detect_marks(wv),
        "smoke": detect_smoke(wv),
        "shadow": detect_shadow(wv),
        "light": detect_light(wv),
        "alert": detect_alert(wv),
        "wave": detect_wave(wv),
        "magnetism": detect_magnetism(wv),
        "mirror": detect_mirror(wv),
        "chain_reaction": detect_chain_reaction(wv),
        "trigger_effect": detect_trigger_effect(wv),
    }
