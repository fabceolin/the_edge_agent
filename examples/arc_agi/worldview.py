"""Worldview: KuzuDB-backed real-time graph of observations, entities, transitions, hypotheses, rules."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
import uuid

import kuzu

from .sensors import Perception


SCHEMA_PATH = Path(__file__).parent / "schema.cypher"


@dataclass
class TransitionRecord:
    prev_observation_id: str
    observation_id: str
    action_id: str
    reward: int


class Worldview:
    """Embedded KuzuDB graph capturing the agent's learning state."""

    def __init__(self, db_path: str | Path = ":memory:"):
        self.db_path = str(db_path)
        self.db = kuzu.Database(self.db_path)
        self.conn = kuzu.Connection(self.db)
        self._last_observation_id: str | None = None
        self.init_schema()

    def init_schema(self) -> None:
        raw = SCHEMA_PATH.read_text(encoding="utf-8")
        # Strip line comments BEFORE splitting on ';' so leading comments don't
        # swallow the first statement.
        no_comments = "\n".join(
            line for line in raw.splitlines() if not line.lstrip().startswith("//")
        )
        for stmt in no_comments.split(";"):
            s = stmt.strip()
            if s:
                self.conn.execute(s)
        self.seed_kernel_ontology()

    # ---- kernel ontology --------------------------------------------------
    KERNEL_CONCEPTS = [
        # affordances (parents)
        ("Movable", "affordance", None),
        ("Recolorable", "affordance", None),
        ("Toggleable", "affordance", None),
        ("Rotatable", "affordance", None),
        ("Rigid", "affordance", None),
        # semantic roles (assigned by shape namer + classifier)
        ("Agent", "role", "Movable"),
        ("Goal", "role", None),
        ("Collectible", "role", None),
        ("Obstacle", "role", "Rigid"),
        ("Container", "role", "Rigid"),
        ("Trigger", "role", "Toggleable"),
        ("PowerUp", "role", None),
        ("DirectionIndicator", "role", None),
        ("HUDElement", "role", None),
        ("Background", "role", None),
        ("Border", "role", "Rigid"),
        # spatial (KIND_OF Movable)
        ("MoveUp", "spatial", "Movable"),
        ("MoveDown", "spatial", "Movable"),
        ("MoveLeft", "spatial", "Movable"),
        ("MoveRight", "spatial", "Movable"),
        # cluster taxonomy
        ("Cluster", "structural", None),
        ("MotionCluster", "structural", "Cluster"),
        ("ProximityCluster", "structural", "Cluster"),
        ("ColorGroup", "structural", "Cluster"),
        ("RotationCluster", "structural", "Cluster"),
        ("DiffCluster", "structural", "Cluster"),
        ("ContainmentCluster", "structural", "Cluster"),
        ("AlignmentCluster", "structural", "Cluster"),
        ("AnchorCluster", "structural", "Cluster"),
        ("PathCluster", "structural", "Cluster"),
        # physics taxonomy
        ("PhysicsCluster", "physics", "Cluster"),
        ("GravityLaw", "physics", "PhysicsCluster"),
        ("InertiaLaw", "physics", "PhysicsCluster"),
        ("FrictionLaw", "physics", "PhysicsCluster"),
        ("CollisionEvent", "physics", "PhysicsCluster"),
        ("BounceEvent", "physics", "PhysicsCluster"),
        ("PushEvent", "physics", "PhysicsCluster"),
        ("AdhesionEvent", "physics", "PhysicsCluster"),
        ("MagnetismLaw", "physics", "PhysicsCluster"),
        ("DestructionEvent", "physics", "PhysicsCluster"),
        ("ConstructionEvent", "physics", "PhysicsCluster"),
        ("SpawnLaw", "physics", "PhysicsCluster"),
        ("DecayLaw", "physics", "PhysicsCluster"),
        ("GrowthLaw", "physics", "PhysicsCluster"),
        ("TeleportEvent", "physics", "PhysicsCluster"),
        ("WrapAroundLaw", "physics", "PhysicsCluster"),
        ("SmokeEffect", "physics", "PhysicsCluster"),
        ("ShadowEffect", "physics", "PhysicsCluster"),
        ("LightEffect", "physics", "PhysicsCluster"),
        ("MarkEffect", "physics", "PhysicsCluster"),
        ("AlertEffect", "physics", "PhysicsCluster"),
        ("WaveEffect", "physics", "PhysicsCluster"),
        ("MirrorRelation", "physics", "PhysicsCluster"),
        ("ChainReaction", "physics", "PhysicsCluster"),
        ("TriggerEffect", "physics", "PhysicsCluster"),
        # goals
        ("MatchPattern", "goal", None),
        ("ReachTarget", "goal", None),
        ("AvoidEntity", "goal", None),
        ("Collect", "goal", None),
        ("Arrange", "goal", None),
        # causal/temporal
        ("Cause", "causal", None),
        ("Effect", "causal", None),
        ("Before", "temporal", None),
        ("After", "temporal", None),
    ]

    # cluster.kind → kernel Concept names to link via CLUSTER_INSTANCE_OF
    CLUSTER_CONCEPT_MAP = {
        "motion": ["MotionCluster", "Cluster", "Movable", "Rigid"],
        "proximity": ["ProximityCluster", "Cluster"],
        "color_group": ["ColorGroup", "Cluster"],
        "rotation": ["RotationCluster", "Cluster", "Rotatable", "Rigid"],
        "diff": ["DiffCluster", "Cluster"],
        "containment": ["ContainmentCluster", "Cluster"],
        "alignment": ["AlignmentCluster", "Cluster"],
        "anchor": ["AnchorCluster", "Cluster"],
        "path": ["PathCluster", "Cluster"],
    }

    def seed_kernel_ontology(self) -> None:
        """Idempotently create the kernel Concept nodes and their taxonomy edges."""
        for name, category, parent in self.KERNEL_CONCEPTS:
            cid = f"concept:{name}"
            res = self.conn.execute(
                "MATCH (c:Concept {id: $id}) RETURN c.id LIMIT 1", {"id": cid}
            )
            if self._first_row(res) is not None:
                continue
            self.conn.execute(
                "CREATE (c:Concept {id: $id, name: $n, category: $cat, is_kernel: true})",
                {"id": cid, "n": name, "cat": category},
            )
        # Wire KIND_OF edges (idempotent: only add if missing).
        for name, _, parent in self.KERNEL_CONCEPTS:
            if not parent:
                continue
            res = self.conn.execute(
                "MATCH (c:Concept {id: $cid})-[:KIND_OF]->(p:Concept {id: $pid}) "
                "RETURN c.id LIMIT 1",
                {"cid": f"concept:{name}", "pid": f"concept:{parent}"},
            )
            if self._first_row(res) is not None:
                continue
            self.conn.execute(
                "MATCH (c:Concept {id: $cid}), (p:Concept {id: $pid}) "
                "CREATE (c)-[:KIND_OF]->(p)",
                {"cid": f"concept:{name}", "pid": f"concept:{parent}"},
            )

    # ---- classification ---------------------------------------------------
    def classify_rule(self, rule_id: str, dsl: str, action_code: int, support: int) -> None:
        """Link a freshly promoted Rule to kernel concepts based on its DSL."""
        concepts: list[str] = []
        if dsl.startswith("TRANSLATE("):
            tail = dsl.split("->", 1)[1].strip()  # e.g. "(-5, 0)"
            try:
                dy, dx = (int(x.strip()) for x in tail.strip("()").split(","))
            except ValueError:
                dy, dx = 0, 0
            concepts.append("Movable")
            if dy < 0 and dx == 0: concepts.append("MoveUp")
            elif dy > 0 and dx == 0: concepts.append("MoveDown")
            elif dy == 0 and dx < 0: concepts.append("MoveLeft")
            elif dy == 0 and dx > 0: concepts.append("MoveRight")
        elif dsl.startswith("RECOLOR("):
            concepts.append("Recolorable")
        elif dsl.startswith("LLM_GOAL"):
            concepts.append("MatchPattern")  # default; refined by goal-text inference elsewhere

        for cname in concepts:
            self.conn.execute(
                "MATCH (r:Rule {id: $rid}), (c:Concept {id: $cid}) "
                "CREATE (r)-[:RULE_INSTANCE_OF]->(c)",
                {"rid": rule_id, "cid": f"concept:{cname}"},
            )

        # Attach Provenance.
        prov_id = f"prov:{rule_id[5:]}"
        self.conn.execute(
            "CREATE (p:Provenance {id: $id, kind: $k, confidence: $c, "
            "evidence_count: $ec, created_at: $ts})",
            {
                "id": prov_id,
                "k": "induction",
                "c": 1.0,  # promoted = passed confidence threshold
                "ec": support,
                "ts": self._now(),
            },
        )
        self.conn.execute(
            "MATCH (r:Rule {id: $rid}), (p:Provenance {id: $pid}) "
            "CREATE (r)-[:HAS_PROVENANCE]->(p)",
            {"rid": rule_id, "pid": prov_id},
        )

    def tag_entity_affordances(self) -> int:
        """Mark Entity nodes with IS_A edges to affordance concepts when their
        shape_hash matches a promoted Movable/Recolorable rule.

        Returns the number of new IS_A edges created."""
        # Walk: Hypothesis DSL holds the shape_hash; rule classification holds the concept.
        # Join via PROMOTED_TO + RULE_INSTANCE_OF.
        res2 = self.conn.execute(
            """
            MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)-[:RULE_INSTANCE_OF]->(c:Concept)
            WHERE c.category IN ['affordance', 'spatial']
            RETURN h.dsl, c.id
            """
        )
        qr2 = res2[0] if isinstance(res2, list) else res2
        shape_to_concepts: dict[str, set[str]] = {}
        import re
        while qr2.has_next():
            dsl, cid = qr2.get_next()
            m = re.search(r"shape_hash=([0-9a-f]+)", dsl)
            if m:
                shape_to_concepts.setdefault(m.group(1), set()).add(cid)

        n_new = 0
        for shape_hash, cids in shape_to_concepts.items():
            for cid in cids:
                # Check if any Entity with this shape lacks the edge, then add.
                res3 = self.conn.execute(
                    """
                    MATCH (e:Entity {shape_hash: $sh})
                    WHERE NOT EXISTS { MATCH (e)-[:IS_A]->(:Concept {id: $cid}) }
                    RETURN e.id LIMIT 100
                    """,
                    {"sh": shape_hash, "cid": cid},
                )
                qr3 = res3[0] if isinstance(res3, list) else res3
                ids: list[str] = []
                while qr3.has_next():
                    ids.append(qr3.get_next()[0])
                for eid in ids:
                    self.conn.execute(
                        "MATCH (e:Entity {id: $eid}), (c:Concept {id: $cid}) "
                        "CREATE (e)-[:IS_A]->(c)",
                        {"eid": eid, "cid": cid},
                    )
                    n_new += 1
        return n_new

    def _now(self) -> datetime:
        return datetime.now(timezone.utc)

    def record_frame(
        self,
        perception: Perception,
        episode: int,
        frame_n: int,
        score: int,
        state: str,
    ) -> tuple[str, list[str]]:
        """Persist the observation and its entities. Returns (obs_id, entity_ids)
        where ``entity_ids[i]`` corresponds to ``perception.entities[i]``."""
        obs_id = f"obs:{episode}:{frame_n}:{uuid.uuid4().hex[:6]}"
        self.conn.execute(
            "CREATE (o:Observation {id: $id, episode: $ep, frame_n: $fn, "
            "grid_hash: $gh, score: $sc, state: $st, ts: $ts})",
            {
                "id": obs_id,
                "ep": episode,
                "fn": frame_n,
                "gh": perception.grid_hash,
                "sc": score,
                "st": state,
                "ts": self._now(),
            },
        )
        entity_ids: list[str] = []
        for idx, ent in enumerate(perception.entities):
            ent_id = (f"ent:{obs_id}:{idx}:{ent.color}:"
                      f"{ent.bbox[0]},{ent.bbox[1]}:{ent.shape_hash[:6]}")
            entity_ids.append(ent_id)
            self.conn.execute(
                "CREATE (e:Entity {id: $id, color: $c, "
                "bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1, "
                "centroid_y: $cy, centroid_x: $cx, area: $a, shape_hash: $sh})",
                {
                    "id": ent_id,
                    "c": ent.color,
                    "r0": ent.bbox[0], "c0": ent.bbox[1],
                    "r1": ent.bbox[2], "c1": ent.bbox[3],
                    "cy": ent.centroid[0], "cx": ent.centroid[1],
                    "a": ent.area, "sh": ent.shape_hash,
                },
            )
            self.conn.execute(
                "MATCH (o:Observation {id: $oid}), (e:Entity {id: $eid}) "
                "CREATE (o)-[:CONTAINS]->(e)",
                {"oid": obs_id, "eid": ent_id},
            )
        return obs_id, entity_ids

    def record_transition(
        self,
        prev_obs_id: str,
        next_obs_id: str,
        action_code: int,
        action_xy: tuple[int, int] | None,
        reward: int,
    ) -> str:
        action_id = f"act:{uuid.uuid4().hex[:8]}"
        x, y = action_xy if action_xy else (-1, -1)
        self.conn.execute(
            "CREATE (a:Action {id: $id, code: $c, x: $x, y: $y})",
            {"id": action_id, "c": action_code, "x": x, "y": y},
        )
        self.conn.execute(
            "MATCH (p:Observation {id: $pid}), (n:Observation {id: $nid}) "
            "CREATE (p)-[:TRANSITION {action_id: $aid, action_code: $ac, reward: $r}]->(n)",
            {"pid": prev_obs_id, "nid": next_obs_id, "aid": action_id, "ac": action_code, "r": reward},
        )
        return action_id

    @staticmethod
    def _first_row(res):
        """Kuzu's Connection.execute() returns QueryResult or list thereof; extract first row."""
        qr = res[0] if isinstance(res, list) else res
        if qr.has_next():
            return qr.get_next()
        return None

    def upsert_hypothesis(self, dsl: str, action_code: int) -> str:
        """Create a Hypothesis node if novel; otherwise increment its support."""
        existing = self.conn.execute(
            "MATCH (h:Hypothesis {dsl: $dsl}) RETURN h.id LIMIT 1",
            {"dsl": dsl},
        )
        row = self._first_row(existing)
        if row is not None:
            hyp_id = row[0]
            self.conn.execute(
                "MATCH (h:Hypothesis {id: $id}) SET h.support = h.support + 1",
                {"id": hyp_id},
            )
            return hyp_id
        hyp_id = f"hyp:{uuid.uuid4().hex[:10]}"
        self.conn.execute(
            "CREATE (h:Hypothesis {id: $id, dsl: $dsl, action_code: $ac, "
            "support: 1, refute: 0, status: 'candidate', created_at: $ts})",
            {"id": hyp_id, "dsl": dsl, "ac": action_code, "ts": self._now()},
        )
        return hyp_id

    def agent_visit_counts(self, shape_hash: str) -> dict[tuple[int, int], int]:
        """Return a map (rounded_y, rounded_x) → number of frames in which an
        entity of ``shape_hash`` occupied that cell. Used by the reasoner /
        planner to softly penalize revisiting cells the avatar has already
        explored."""
        res = self.conn.execute(
            """
            MATCH (e:Entity {shape_hash: $sh})
            RETURN e.centroid_y, e.centroid_x
            """,
            {"sh": shape_hash},
        )
        qr = res[0] if isinstance(res, list) else res
        counts: dict[tuple[int, int], int] = {}
        while qr.has_next():
            row = qr.get_next()
            key = (int(round(float(row[0]))), int(round(float(row[1]))))
            counts[key] = counts.get(key, 0) + 1
        return counts

    def stats(self) -> dict[str, int]:
        out: dict[str, int] = {}
        for label in ("Observation", "Entity", "Action", "Hypothesis", "Rule", "Cluster"):
            res = self.conn.execute(f"MATCH (n:{label}) RETURN count(n) AS c")
            row = self._first_row(res)
            out[label.lower()] = int(row[0]) if row is not None else 0
        return out

    # ---- cluster persistence ---------------------------------------------
    def record_cluster(
        self,
        *,
        kind: str,
        signature: str,
        member_entity_ids: list[str],
        observation_id: str,
        bbox: tuple[int, int, int, int],
        centroid: tuple[float, float],
        velocity: tuple[int, int] = (0, 0),
        rotation_deg: int = 0,
    ) -> str:
        import hashlib
        member_digest = hashlib.blake2b(
            ("|".join(sorted(member_entity_ids)) or "nomembers").encode(), digest_size=4
        ).hexdigest()
        cluster_id = f"clu:{kind}:{signature}:{observation_id[-8:]}:{member_digest}"
        self.conn.execute(
            "CREATE (cl:Cluster {id: $id, kind: $k, signature: $sig, n_members: $n, "
            "bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1, "
            "centroid_y: $cy, centroid_x: $cx, "
            "velocity_dy: $vdy, velocity_dx: $vdx, rotation_deg: $rot, "
            "discovered_at_obs: $obs})",
            {
                "id": cluster_id, "k": kind, "sig": signature, "n": len(member_entity_ids),
                "r0": bbox[0], "c0": bbox[1], "r1": bbox[2], "c1": bbox[3],
                "cy": centroid[0], "cx": centroid[1],
                "vdy": velocity[0], "vdx": velocity[1], "rot": rotation_deg,
                "obs": observation_id,
            },
        )
        for eid in member_entity_ids:
            self.conn.execute(
                "MATCH (e:Entity {id: $eid}), (cl:Cluster {id: $cid}) "
                "CREATE (e)-[:PART_OF]->(cl)",
                {"eid": eid, "cid": cluster_id},
            )
        self.conn.execute(
            "MATCH (o:Observation {id: $oid}), (cl:Cluster {id: $cid}) "
            "CREATE (o)-[:OBSERVED_AS]->(cl)",
            {"oid": observation_id, "cid": cluster_id},
        )
        for concept_name in self.CLUSTER_CONCEPT_MAP.get(kind, []):
            self.conn.execute(
                "MATCH (cl:Cluster {id: $cid}), (c:Concept {id: $kid}) "
                "CREATE (cl)-[:CLUSTER_INSTANCE_OF]->(c)",
                {"cid": cluster_id, "kid": f"concept:{concept_name}"},
            )
        return cluster_id

    # ---- shape persistence -----------------------------------------------
    def upsert_shape(
        self, shape_hash: str, bitmap_ascii: str, height: int, width: int, area: int
    ) -> None:
        """Insert Shape if absent, else increment n_observations counter."""
        res = self.conn.execute(
            "MATCH (s:Shape {shape_hash: $sh}) RETURN s.shape_hash LIMIT 1",
            {"sh": shape_hash},
        )
        if self._first_row(res) is not None:
            self.conn.execute(
                "MATCH (s:Shape {shape_hash: $sh}) SET s.n_observations = s.n_observations + 1",
                {"sh": shape_hash},
            )
            return
        self.conn.execute(
            "CREATE (s:Shape {shape_hash: $sh, bitmap_ascii: $b, height: $h, width: $w, "
            "area: $a, n_observations: 1, first_seen_at: $ts, "
            "llm_name: '', llm_confidence: 0.0, role_concept: ''})",
            {"sh": shape_hash, "b": bitmap_ascii, "h": height, "w": width, "a": area,
             "ts": self._now()},
        )

    def link_entity_to_shape(self, entity_id: str, shape_hash: str) -> None:
        self.conn.execute(
            "MATCH (e:Entity {id: $eid}), (s:Shape {shape_hash: $sh}) "
            "CREATE (e)-[:HAS_SHAPE]->(s)",
            {"eid": entity_id, "sh": shape_hash},
        )

    # ---- cluster temporal identity ---------------------------------------
    def link_next_clusters(
        self, prev_obs_id: str, curr_obs_id: str, action_code: int
    ) -> dict[str, int]:
        """Create :NEXT_CLUSTER edges between matching clusters in consecutive obs.

        Matching strategy (per curr cluster):
          1. Exact signature match in prev → 'persisted' (centroid stable)
             or 'translated' (centroid moved).
          2. Same kind + bbox overlap > 0 → 'transformed'.
          3. No match → no edge created (cluster 'appeared').

        Prev clusters with no outgoing edge after this call = 'vanished'.
        Returns counts per change_kind.
        """
        # Pull prev clusters.
        res = self.conn.execute(
            "MATCH (o:Observation {id: $oid})-[:OBSERVED_AS]->(c:Cluster) "
            "RETURN c.id, c.kind, c.signature, c.centroid_y, c.centroid_x, "
            "c.bbox_r0, c.bbox_c0, c.bbox_r1, c.bbox_c1",
            {"oid": prev_obs_id},
        )
        qr = res[0] if isinstance(res, list) else res
        prev_clusters: list[tuple] = []
        while qr.has_next():
            prev_clusters.append(tuple(qr.get_next()))

        # Pull curr clusters.
        res = self.conn.execute(
            "MATCH (o:Observation {id: $oid})-[:OBSERVED_AS]->(c:Cluster) "
            "RETURN c.id, c.kind, c.signature, c.centroid_y, c.centroid_x, "
            "c.bbox_r0, c.bbox_c0, c.bbox_r1, c.bbox_c1",
            {"oid": curr_obs_id},
        )
        qr = res[0] if isinstance(res, list) else res
        curr_clusters: list[tuple] = []
        while qr.has_next():
            curr_clusters.append(tuple(qr.get_next()))

        counts = {"persisted": 0, "translated": 0, "transformed": 0, "appeared": 0, "vanished": 0}
        matched_prev: set[str] = set()

        def bbox_overlap(a: tuple, b: tuple) -> int:
            ay0, ax0, ay1, ax1 = a
            by0, bx0, by1, bx1 = b
            iy0, iy1 = max(ay0, by0), min(ay1, by1)
            ix0, ix1 = max(ax0, bx0), min(ax1, bx1)
            if iy0 > iy1 or ix0 > ix1:
                return 0
            return (iy1 - iy0 + 1) * (ix1 - ix0 + 1)

        for cc in curr_clusters:
            cid, ckind, csig, ccy, ccx, cbox = cc[0], cc[1], cc[2], cc[3], cc[4], cc[5:]
            # 1) exact signature match
            same_sig = [p for p in prev_clusters
                       if p[2] == csig and p[1] == ckind and p[0] not in matched_prev]
            if same_sig:
                # Pick closest by centroid.
                best = min(same_sig,
                          key=lambda p: (p[3] - ccy) ** 2 + (p[4] - ccx) ** 2)
                pid, _, _, pcy, pcx, *_ = best
                dy, dx = ccy - pcy, ccx - pcx
                change_kind = "persisted" if (abs(dy) < 0.5 and abs(dx) < 0.5) else "translated"
                self._create_next_edge(pid, cid, action_code, change_kind, dy, dx)
                matched_prev.add(pid)
                counts[change_kind] += 1
                continue
            # 2) overlap match
            overlaps = [
                (p, bbox_overlap(p[5:], cbox)) for p in prev_clusters
                if p[1] == ckind and p[0] not in matched_prev
            ]
            overlaps = [(p, o) for p, o in overlaps if o > 0]
            if overlaps:
                best, _ = max(overlaps, key=lambda po: po[1])
                pid, _, _, pcy, pcx, *_ = best
                dy, dx = ccy - pcy, ccx - pcx
                self._create_next_edge(pid, cid, action_code, "transformed", dy, dx)
                matched_prev.add(pid)
                counts["transformed"] += 1
                continue
            counts["appeared"] += 1

        counts["vanished"] = len(prev_clusters) - len(matched_prev)
        return counts

    def _create_next_edge(
        self, prev_cluster_id: str, curr_cluster_id: str,
        action_code: int, change_kind: str, dy: float, dx: float,
    ) -> None:
        self.conn.execute(
            "MATCH (p:Cluster {id: $pid}), (c:Cluster {id: $cid}) "
            "CREATE (p)-[:NEXT_CLUSTER {action_code: $ac, change_kind: $ck, "
            "dy: $dy, dx: $dx}]->(c)",
            {"pid": prev_cluster_id, "cid": curr_cluster_id,
             "ac": action_code, "ck": change_kind, "dy": dy, "dx": dx},
        )

    # ---- semantic priors -------------------------------------------------
    BACKGROUND_ROLES = {"Background", "Border", "HUDElement"}

    def apply_semantic_priors(self) -> dict[str, int]:
        """Use Shape role classifications to bias the hypothesis pool:
          (a) reject RECOLOR hypotheses on cells inside a Background/Border anchor;
          (b) boost TRANSLATE hypotheses whose shape_hash is classified as Agent.

        Returns {'rejected': N, 'boosted': M}.
        """
        # (a) Reject background-confined RECOLOR hypotheses.
        # Get anchor bboxes whose shape role is Background/Border.
        res = self.conn.execute(
            """
            MATCH (s:Shape) WHERE s.role_concept IN ['Background', 'Border', 'HUDElement']
            RETURN s.shape_hash
            """
        )
        qr = res[0] if isinstance(res, list) else res
        bg_hashes: set[str] = set()
        while qr.has_next():
            bg_hashes.add(qr.get_next()[0])

        rejected = 0
        if bg_hashes:
            # Find anchor clusters whose member entities all have a background shape_hash.
            res = self.conn.execute(
                """
                MATCH (cl:Cluster {kind: 'anchor'})<-[:PART_OF]-(e:Entity)
                WHERE e.shape_hash IN $bgs
                RETURN cl.bbox_r0, cl.bbox_c0, cl.bbox_r1, cl.bbox_c1
                """,
                {"bgs": list(bg_hashes)},
            )
            qr = res[0] if isinstance(res, list) else res
            bg_bboxes: list[tuple[int, int, int, int]] = []
            while qr.has_next():
                row = qr.get_next()
                bg_bboxes.append((row[0], row[1], row[2], row[3]))

            # Reject any candidate RECOLOR hypothesis whose (r, c) falls inside a bg bbox.
            res = self.conn.execute(
                "MATCH (h:Hypothesis {status: 'candidate'}) "
                "WHERE h.dsl STARTS WITH 'RECOLOR' RETURN h.id, h.dsl"
            )
            qr = res[0] if isinstance(res, list) else res
            recolor_hyps: list[tuple[str, str]] = []
            while qr.has_next():
                row = qr.get_next()
                recolor_hyps.append((row[0], row[1]))

            for hid, dsl in recolor_hyps:
                import re
                m = re.search(r"r=(-?\d+), c=(-?\d+)", dsl)
                if not m:
                    continue
                r, c = int(m.group(1)), int(m.group(2))
                for r0, c0, r1, c1 in bg_bboxes:
                    if r0 <= r <= r1 and c0 <= c <= c1:
                        self.conn.execute(
                            "MATCH (h:Hypothesis {id: $id}) SET h.status = 'rejected'",
                            {"id": hid},
                        )
                        rejected += 1
                        break

        # (b) Boost Agent TRANSLATE hypotheses.
        res = self.conn.execute(
            "MATCH (s:Shape) WHERE s.role_concept = 'Agent' RETURN s.shape_hash"
        )
        qr = res[0] if isinstance(res, list) else res
        agent_hashes: set[str] = set()
        while qr.has_next():
            agent_hashes.add(qr.get_next()[0])

        boosted = 0
        for sh in agent_hashes:
            res = self.conn.execute(
                "MATCH (h:Hypothesis {status: 'candidate'}) "
                "WHERE h.dsl CONTAINS $sh AND h.dsl STARTS WITH 'TRANSLATE' "
                "RETURN h.id",
                {"sh": sh},
            )
            qr = res[0] if isinstance(res, list) else res
            ids = []
            while qr.has_next():
                ids.append(qr.get_next()[0])
            for hid in ids:
                self.conn.execute(
                    "MATCH (h:Hypothesis {id: $id}) SET h.support = h.support + 5",
                    {"id": hid},
                )
                boosted += 1

        return {"rejected": rejected, "boosted": boosted}

    # ---- post-hoc cluster discovery (anchor, path) -----------------------
    def discover_anchor_clusters(self, *, min_persistence: int = 5) -> int:
        """Create AnchorCluster nodes for (shape_hash, bbox) tuples that appeared
        unchanged across ≥ ``min_persistence`` distinct frames in this DB.

        Members are all Entity nodes matching that (shape, bbox) signature.
        Returns the number of new anchors created.
        """
        res = self.conn.execute(
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity)
            WITH e.shape_hash AS sh,
                 e.bbox_r0 AS r0, e.bbox_c0 AS c0,
                 e.bbox_r1 AS r1, e.bbox_c1 AS c1,
                 e.color AS color,
                 count(DISTINCT o.frame_n) AS n_frames
            WHERE n_frames >= $min
            RETURN sh, r0, c0, r1, c1, color, n_frames
            """,
            {"min": min_persistence},
        )
        qr = res[0] if isinstance(res, list) else res
        candidates: list[tuple] = []
        while qr.has_next():
            candidates.append(tuple(qr.get_next()))

        n_new = 0
        for sh, r0, c0, r1, c1, color, n_frames in candidates:
            sig = f"anchor:{color}:{sh}:{r0},{c0},{r1},{c1}"
            res = self.conn.execute(
                "MATCH (cl:Cluster {signature: $sig, kind: 'anchor'}) RETURN cl.id LIMIT 1",
                {"sig": sig},
            )
            if self._first_row(res) is not None:
                continue
            cluster_id = f"clu:anchor:{sig[7:]}"
            self.conn.execute(
                "CREATE (cl:Cluster {id: $id, kind: 'anchor', signature: $sig, "
                "n_members: $n, bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1, "
                "centroid_y: $cy, centroid_x: $cx, "
                "velocity_dy: 0, velocity_dx: 0, rotation_deg: 0, "
                "discovered_at_obs: 'post-hoc'})",
                {
                    "id": cluster_id, "sig": sig, "n": n_frames,
                    "r0": r0, "c0": c0, "r1": r1, "c1": c1,
                    "cy": (r0 + r1) / 2.0, "cx": (c0 + c1) / 2.0,
                },
            )
            # Link members
            self.conn.execute(
                "MATCH (e:Entity {shape_hash: $sh, color: $color, "
                "bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1}), "
                "(cl:Cluster {id: $cid}) "
                "CREATE (e)-[:PART_OF]->(cl)",
                {"sh": sh, "color": color, "r0": r0, "c0": c0, "r1": r1, "c1": c1,
                 "cid": cluster_id},
            )
            for cname in self.CLUSTER_CONCEPT_MAP.get("anchor", []):
                self.conn.execute(
                    "MATCH (cl:Cluster {id: $cid}), (c:Concept {id: $kid}) "
                    "CREATE (cl)-[:CLUSTER_INSTANCE_OF]->(c)",
                    {"cid": cluster_id, "kid": f"concept:{cname}"},
                )
            n_new += 1
        return n_new

    def discover_path_clusters(self, *, min_length: int = 3) -> int:
        """Chain motion clusters across consecutive observations by **spatial
        proximity**: cluster at frame_n+1 whose centroid is near where cluster
        at frame_n ended (centroid + velocity) is the same logical avatar's path.

        This is more robust than signature-based NEXT_CLUSTER because the
        signature changes when the action (and thus velocity) changes.
        """
        # Pull all motion clusters with their observation frame_n.
        res = self.conn.execute(
            """
            MATCH (o:Observation)-[:OBSERVED_AS]->(cl:Cluster {kind:'motion'})
            RETURN cl.id, o.frame_n, cl.centroid_y, cl.centroid_x,
                   cl.velocity_dy, cl.velocity_dx
            ORDER BY o.frame_n
            """
        )
        qr = res[0] if isinstance(res, list) else res
        clusters_by_frame: dict[int, list[tuple]] = {}
        while qr.has_next():
            cid, fn, cy, cx, vdy, vdx = qr.get_next()
            clusters_by_frame.setdefault(int(fn), []).append(
                (cid, float(cy), float(cx), float(vdy), float(vdx))
            )

        # Build successor map: cluster_at(fn) → cluster_at(fn+1) closest to its
        # predicted endpoint (centroid + velocity).
        succ: dict[str, str] = {}
        targets: set[str] = set()
        frames = sorted(clusters_by_frame.keys())
        for fn, next_fn in zip(frames, frames[1:]):
            if next_fn != fn + 1:
                continue
            next_clusters = clusters_by_frame[next_fn]
            for cid, cy, cx, vdy, vdx in clusters_by_frame[fn]:
                pred_y, pred_x = cy + vdy, cx + vdx
                # find nearest next cluster within radius 8.
                best, best_d = None, 8.0 ** 2
                for ncid, ncy, ncx, _, _ in next_clusters:
                    if ncid in targets:
                        continue
                    d = (ncy - pred_y) ** 2 + (ncx - pred_x) ** 2
                    if d < best_d:
                        best, best_d = ncid, d
                if best is not None:
                    succ[cid] = best
                    targets.add(best)

        starts = [src for src in succ if src not in targets]

        # Walk each chain.
        chains: list[list[str]] = []
        for start in starts:
            chain = [start]
            cur = start
            while cur in succ and len(chain) < 20:
                cur = succ[cur]
                chain.append(cur)
            if len(chain) >= min_length:
                chains.append(chain)

        if not chains:
            return 0

        # Fetch centroid coords for path endpoints in one batch.
        all_ids = {chain[0] for chain in chains} | {chain[-1] for chain in chains}
        res = self.conn.execute(
            "MATCH (c:Cluster) WHERE c.id IN $ids "
            "RETURN c.id, c.centroid_y, c.centroid_x",
            {"ids": list(all_ids)},
        )
        qr = res[0] if isinstance(res, list) else res
        coords: dict[str, tuple[float, float]] = {}
        while qr.has_next():
            row = qr.get_next()
            coords[row[0]] = (float(row[1]), float(row[2]))

        n_new = 0
        for chain in chains:
            start_id, end_id, hops = chain[0], chain[-1], len(chain) - 1
            sy, sx = coords.get(start_id, (0.0, 0.0))
            ey, ex = coords.get(end_id, (0.0, 0.0))
            sig = f"path:{start_id[-8:]}:{end_id[-8:]}"
            res = self.conn.execute(
                "MATCH (cl:Cluster {signature: $sig, kind: 'path'}) RETURN cl.id LIMIT 1",
                {"sig": sig},
            )
            if self._first_row(res) is not None:
                continue
            cluster_id = f"clu:path:{sig[5:]}"
            bbox = (
                int(min(sy, ey)), int(min(sx, ex)),
                int(max(sy, ey)), int(max(sx, ex)),
            )
            self.conn.execute(
                "CREATE (cl:Cluster {id: $id, kind: 'path', signature: $sig, "
                "n_members: $hops, bbox_r0: $r0, bbox_c0: $c0, bbox_r1: $r1, bbox_c1: $c1, "
                "centroid_y: $cy, centroid_x: $cx, "
                "velocity_dy: 0, velocity_dx: 0, rotation_deg: 0, "
                "discovered_at_obs: 'post-hoc'})",
                {"id": cluster_id, "sig": sig, "hops": hops + 1,
                 "r0": bbox[0], "c0": bbox[1], "r1": bbox[2], "c1": bbox[3],
                 "cy": (sy + ey) / 2.0, "cx": (sx + ex) / 2.0},
            )
            for cname in self.CLUSTER_CONCEPT_MAP.get("path", []):
                self.conn.execute(
                    "MATCH (cl:Cluster {id: $cid}), (c:Concept {id: $kid}) "
                    "CREATE (cl)-[:CLUSTER_INSTANCE_OF]->(c)",
                    {"cid": cluster_id, "kid": f"concept:{cname}"},
                )
            n_new += 1
        return n_new

    def close(self) -> None:
        self.conn.close()
        self.db.close()
