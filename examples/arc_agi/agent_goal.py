"""Identify the Agent (player avatar) and Goal locations from the graph.

The Agent is heuristically: the Shape with the most distinct Movable rules
linked to it via shape_hash. Falls back to the Shape whose role_concept == 'Agent'
when LLM naming has already classified one.

Goals are: anchor clusters whose role_concept is 'Goal', or — if none classified —
non-background anchor entities (entities that persist unchanged in many frames
and whose color is not in a "background" set inferred from area dominance).
"""

from __future__ import annotations

from dataclasses import dataclass

from .worldview import Worldview


@dataclass(frozen=True)
class AgentInfo:
    shape_hash: str
    n_movable_rules: int
    last_known_centroid: tuple[float, float] | None = None


@dataclass(frozen=True)
class GoalInfo:
    centroid: tuple[float, float]
    bbox: tuple[int, int, int, int]
    color: int
    source: str   # 'classified' | 'anchor_heuristic' | 'llm_inferred'


def _exec(wv: Worldview, q: str, p: dict | None = None) -> list:
    res = wv.conn.execute(q, p or {})
    qr = res[0] if isinstance(res, list) else res
    rows = []
    while qr.has_next():
        rows.append(qr.get_next())
    return rows


def identify_agent(wv: Worldview) -> AgentInfo | None:
    """Return the Shape most likely to be the Agent.

    A real avatar appears as **exactly one entity per frame** (the player you
    control). Decorative/HUD shapes that share a hash but appear multiple
    times per frame (e.g., a 2×2 corner indicator repeated in 4 corners) are
    NOT the agent even if movement rules accidentally fire for them.

    Selection rule: among shapes that have at least one promoted Movable rule,
    pick the one whose mean entities-per-frame is closest to 1.
    """
    import re
    # Candidate shapes: those with at least one Movable rule.
    rule_rows = _exec(wv,
        """
        MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)
              -[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'})
        WHERE h.dsl STARTS WITH 'TRANSLATE'
        RETURN h.dsl
        """)
    if not rule_rows:
        return None
    from collections import Counter
    rule_count: Counter[str] = Counter()
    for (dsl,) in rule_rows:
        m = re.search(r"shape_hash=([0-9a-f]+)", dsl)
        if m:
            rule_count[m.group(1)] += 1
    if not rule_count:
        return None

    # Score each candidate. Lower is better.
    #   score = |mean_entities_per_frame - 1.0| - 0.1 * rule_count
    best_sh = None
    best_score = float("inf")
    for sh in rule_count.keys():
        stat = _exec(wv,
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: $sh})
            WITH o.frame_n AS fn, count(e) AS n_per_frame
            RETURN avg(n_per_frame), count(DISTINCT fn)
            """,
            {"sh": sh})
        if not stat or stat[0][0] is None:
            continue
        mean_per_frame, n_frames = float(stat[0][0]), int(stat[0][1])
        if n_frames < 2:
            continue
        score = abs(mean_per_frame - 1.0) - 0.1 * rule_count[sh]
        if score < best_score:
            best_score = score
            best_sh = sh

    if best_sh is None:
        # Fallback to old behavior.
        rows = _exec(wv,
            "MATCH (s:Shape) WHERE s.role_concept = 'Agent' "
            "RETURN s.shape_hash LIMIT 1")
        if rows:
            sh = rows[0][0]
        else:
            sh = rule_count.most_common(1)[0][0]
    else:
        sh = best_sh

    # Count distinct Movable rules for this shape and fetch last centroid.
    rules = _exec(wv,
        """
        MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)-[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'})
        WHERE h.dsl CONTAINS $sh
        RETURN count(DISTINCT r.id) AS n
        """,
        {"sh": sh})
    n_rules = int(rules[0][0]) if rules else 0

    centroid_rows = _exec(wv,
        """
        MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: $sh})
        RETURN e.centroid_y, e.centroid_x, o.frame_n
        ORDER BY o.frame_n DESC LIMIT 1
        """,
        {"sh": sh})
    last_centroid = None
    if centroid_rows:
        last_centroid = (float(centroid_rows[0][0]), float(centroid_rows[0][1]))

    return AgentInfo(shape_hash=sh, n_movable_rules=n_rules,
                     last_known_centroid=last_centroid)


def identify_goals(wv: Worldview, *, max_goals: int = 20) -> list[GoalInfo]:
    """Return diverse candidate goal locations from 5 sources, deduplicated by
    proximity. Higher-confidence sources come first.

    Sources:
      1. Shape.role_concept == 'Goal' / 'Collectible'  (LLM-classified)
      2. Anchor clusters of small to medium area      (HUD-ish but persistent)
      3. DiffCluster locations                         (where pixels changed)
      4. Foreground entities                           (mid-area, not Agent/Background)
      5. LLM-suggested goal coordinates                (parsed from LLM_GOAL hyps)
    """
    candidates: list[GoalInfo] = []

    # Source 1: classified Goal/Collectible shapes.
    rows = _exec(wv,
        """
        MATCH (s:Shape)
        WHERE s.role_concept = 'Goal' OR s.role_concept = 'Collectible'
        MATCH (o:Observation)-[:CONTAINS]->(e:Entity {shape_hash: s.shape_hash})
        RETURN e.centroid_y, e.centroid_x, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1,
               e.color, o.frame_n
        ORDER BY o.frame_n DESC LIMIT 8
        """)
    for cy, cx, r0, c0, r1, c1, color, _ in rows:
        candidates.append(GoalInfo(
            centroid=(float(cy), float(cx)),
            bbox=(int(r0), int(c0), int(r1), int(c1)),
            color=int(color),
            source="classified",
        ))

    # Source 2: anchor clusters (markers / targets).
    rows = _exec(wv,
        """
        MATCH (cl:Cluster {kind:'anchor'})<-[:PART_OF]-(e:Entity)
        WHERE (cl.bbox_r1 - cl.bbox_r0 + 1) * (cl.bbox_c1 - cl.bbox_c0 + 1) >= 2
          AND (cl.bbox_r1 - cl.bbox_r0 + 1) * (cl.bbox_c1 - cl.bbox_c0 + 1) <= 200
        RETURN DISTINCT cl.centroid_y, cl.centroid_x,
                        cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1, e.color,
                        cl.n_members
        ORDER BY cl.n_members DESC LIMIT 10
        """)
    for row in rows:
        cy, cx, r0, c0, r1, c1, color = row[:7]
        candidates.append(GoalInfo(
            centroid=(float(cy), float(cx)),
            bbox=(int(r0), int(c0), int(r1), int(c1)),
            color=int(color),
            source="anchor_heuristic",
        ))

    # Source 3: DiffCluster locations — regions where pixels recently changed.
    rows = _exec(wv,
        """
        MATCH (cl:Cluster {kind:'diff'})
        RETURN DISTINCT cl.centroid_y, cl.centroid_x,
                        cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1
        LIMIT 10
        """)
    for cy, cx, r0, c0, r1, c1 in rows:
        candidates.append(GoalInfo(
            centroid=(float(cy), float(cx)),
            bbox=(int(r0), int(c0), int(r1), int(c1)),
            color=-1,
            source="diff_region",
        ))

    # Source 4: mid-area foreground entities (not Agent, not classified Background/Border).
    rows = _exec(wv,
        """
        MATCH (o:Observation)-[:CONTAINS]->(e:Entity)-[:HAS_SHAPE]->(s:Shape)
        WHERE s.role_concept <> 'Agent'
          AND s.role_concept <> 'Background'
          AND s.role_concept <> 'Border'
          AND s.role_concept <> 'HUDElement'
          AND e.area >= 5 AND e.area <= 100
        RETURN e.centroid_y, e.centroid_x, e.bbox_r0, e.bbox_c0, e.bbox_r1, e.bbox_c1,
               e.color, o.frame_n
        ORDER BY o.frame_n DESC LIMIT 10
        """)
    for cy, cx, r0, c0, r1, c1, color, _ in rows:
        candidates.append(GoalInfo(
            centroid=(float(cy), float(cx)),
            bbox=(int(r0), int(c0), int(r1), int(c1)),
            color=int(color),
            source="foreground_entity",
        ))

    # Source 5: LLM_GOAL hypotheses (parsed centroid hints if any encoded).
    # Currently LLM_GOAL stores natural-language; future iterations could embed coords.
    # For now, this source is a placeholder.

    # Deduplicate by centroid proximity (within 3 cells).
    deduped: list[GoalInfo] = []
    for g in candidates:
        if any(abs(g.centroid[0] - d.centroid[0]) < 3
               and abs(g.centroid[1] - d.centroid[1]) < 3 for d in deduped):
            continue
        deduped.append(g)
    return deduped[:max_goals]
