"""Reasoner: choose actions and promote validated hypotheses to rules.

Strategy (ε-greedy hybrid):
  exploit  (prob 1 - ε): pick the action with highest average reward observed in past
                          transitions originating from states containing the same shape
                          hashes as the current perception.
  explore  (prob ε):      pick the action least frequently tried at states containing
                          these shape hashes (novelty bonus).

Promotion: a Hypothesis with support ≥ SUPPORT_THRESHOLD and
support / (support + refute) ≥ CONFIDENCE_RATIO becomes a Rule.
"""

from __future__ import annotations

import random
from dataclasses import dataclass

from .sensors import Perception
from .worldview import Worldview


SUPPORT_THRESHOLD = 5
CONFIDENCE_RATIO = 0.8


@dataclass
class Decision:
    action_code: int
    mode: str        # 'exploit' | 'explore' | 'random'
    rationale: str


class Reasoner:
    def __init__(self, worldview: Worldview, action_space: list[int], epsilon: float = 0.3):
        self.wv = worldview
        self.action_space = action_space
        self.epsilon = epsilon
        # Cached plan: ordered list of action codes the planner produced.
        self._plan_queue: list[int] = []
        # Cached agent/goal/moves snapshot so we can replan periodically.
        self._frames_since_replan: int = 999
        self._replan_every: int = 25
        # Number of Movable rules at last replan (skip if unchanged).
        self._last_replan_rules: int = 0
        # Active goal-attempt state for Prolog backtracking.
        self._current_goal_sig: str | None = None
        self._current_goal_centroid: tuple[float, float] | None = None
        self._current_goal_source: str = ""
        self._frames_on_current_goal: int = 0
        self._score_at_goal_start: int = 0
        # If no reward within this window, the goal is marked failed.
        self._goal_patience: int = 15
        # Coverage-mode flag: when True, the current plan is exploring an
        # unvisited reachable cell (not a Prolog-chosen goal).
        self._in_coverage_mode: bool = False
        self._coverage_exhausted: bool = False  # set once no unvisited targets remain
        # Coverage targets attempted in this session (avoid retrying same cell
        # immediately if Dijkstra fails or we get sidetracked).
        self._tried_coverage_targets: set[tuple[int, int]] = set()
        # Direction inertia: track the last action taken so the explore branch
        # and the planner can softly prefer "keep going in the same direction".
        self._last_action_code: int | None = None
        self._consecutive_same_direction: int = 0

    def _exploit_action(self, shape_hashes: list[str]) -> tuple[int, float] | None:
        """Pick action with highest avg reward at past states matching these shapes."""
        if not shape_hashes:
            return None
        res = self.wv.conn.execute(
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity)
            WHERE e.shape_hash IN $shapes
            WITH DISTINCT o
            MATCH (o)-[t:TRANSITION]->(:Observation)
            WITH t.action_code AS code, avg(CAST(t.reward AS DOUBLE)) AS avg_r, count(*) AS n
            WHERE avg_r > 0.0
            RETURN code, avg_r ORDER BY avg_r DESC LIMIT 1
            """,
            {"shapes": shape_hashes},
        )
        qr = res[0] if isinstance(res, list) else res
        if qr.has_next():
            row = qr.get_next()
            return int(row[0]), float(row[1])
        return None

    def _explore_action(
        self,
        shape_hashes: list[str],
        *,
        agent_pos: tuple[int, int] | None = None,
        visit_counts: dict[tuple[int, int], int] | None = None,
        moves: list[tuple[int, int, int]] | None = None,
        last_action_code: int | None = None,
        inertia_bonus: float = 0.5,
    ) -> int | None:
        """Pick action least tried at past states with these shapes (action novelty).

        When ``agent_pos`` + ``visit_counts`` + ``moves`` are supplied, ties
        between least-tried actions are broken by predicted destination visit
        count — soft preference for cells the avatar hasn't visited yet.
        """
        if not shape_hashes:
            return None
        res = self.wv.conn.execute(
            """
            MATCH (o:Observation)-[:CONTAINS]->(e:Entity)
            WHERE e.shape_hash IN $shapes
            WITH DISTINCT o
            MATCH (o)-[t:TRANSITION]->(:Observation)
            RETURN t.action_code AS code, count(*) AS n
            """,
            {"shapes": shape_hashes},
        )
        qr = res[0] if isinstance(res, list) else res
        counts: dict[int, int] = {a: 0 for a in self.action_space}
        while qr.has_next():
            code, n = qr.get_next()
            counts[int(code)] = int(n)
        # When we have a movement model + visit history, score each action by:
        #   base       = action_count + 2 * next_cell_visit_count
        #   reversal   = +5  (anti-backtrack — avoid undoing the last move)
        #   right_turn = -1  (when switching direction, prefer 90° right)
        #   same_dir   = -0.5 (continue straight is also slightly preferred)
        # Then the action with the lowest score wins (random tie-break).
        if agent_pos is not None and visit_counts and moves:
            from .directions import build_direction_index
            dirs = build_direction_index(moves)
            move_map = dirs["by_action"]
            opposite = dirs["opposite"]
            right_of = dirs["right_of"]

            def base_score(action_code: int) -> float:
                m = move_map.get(action_code)
                if m is None:
                    return float(counts[action_code])
                ny, nx = agent_pos[0] + m[0], agent_pos[1] + m[1]
                s = float(counts[action_code]) \
                    + 2.0 * float(visit_counts.get((ny, nx), 0))
                if last_action_code is not None:
                    if action_code == last_action_code:
                        s -= 0.5  # same direction (inertia)
                    elif action_code == opposite(last_action_code):
                        s += 5.0  # anti-reverse penalty
                    elif action_code == right_of(last_action_code):
                        s -= 1.0  # right-hand turn bonus
                return s

            scores = {a: base_score(a) for a in self.action_space}
            best_score = min(scores.values())
            best = [a for a in self.action_space
                    if scores[a] == best_score]
            return random.choice(best)

        min_n = min(counts.values())
        least_tried = [a for a, c in counts.items() if c == min_n]
        return random.choice(least_tried) if least_tried else None

    def _llm_suggested_action(self) -> tuple[int, int] | None:
        """Best active LLM_GOAL hypothesis (highest support)."""
        res = self.wv.conn.execute(
            """
            MATCH (h:Hypothesis)
            WHERE h.dsl STARTS WITH 'LLM_GOAL' AND h.status = 'candidate'
            RETURN h.action_code AS code, h.support AS sup
            ORDER BY h.support DESC LIMIT 1
            """
        )
        qr = res[0] if isinstance(res, list) else res
        if qr.has_next():
            row = qr.get_next()
            return int(row[0]), int(row[1])
        return None

    def _count_movable_rules(self) -> int:
        res = self.wv.conn.execute(
            "MATCH (:Rule)-[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'}) "
            "RETURN count(*)"
        )
        qr = res[0] if isinstance(res, list) else res
        return int(qr.get_next()[0]) if qr.has_next() else 0

    def _abandon_goal(self, current_score: int) -> None:
        """Mark the current goal as 'no_reward' and clear cached plan so the
        next replan picks a new candidate via Prolog backtracking."""
        if self._current_goal_sig is None or self._current_goal_centroid is None:
            return
        from .prolog_goal import record_attempt
        reward_delta = current_score - self._score_at_goal_start
        record_attempt(
            self.wv,
            sig=self._current_goal_sig,
            centroid=self._current_goal_centroid,
            source=self._current_goal_source,
            frames_invested=self._frames_on_current_goal,
            reward_delta=reward_delta,
        )
        self._current_goal_sig = None
        self._plan_queue = []

    def _check_goal_timeout(self, current_score: int) -> bool:
        """Returns True if we should abandon the current goal."""
        if self._current_goal_sig is None:
            return False
        self._frames_on_current_goal += 1
        if current_score > self._score_at_goal_start:
            # Reward! Record success and keep going (but the plan_queue will
            # naturally exhaust and a fresh replan will pick the next goal).
            from .prolog_goal import record_attempt
            record_attempt(
                self.wv,
                sig=self._current_goal_sig,
                centroid=self._current_goal_centroid or (0.0, 0.0),
                source=self._current_goal_source,
                frames_invested=self._frames_on_current_goal,
                reward_delta=current_score - self._score_at_goal_start,
            )
            self._current_goal_sig = None
            self._plan_queue = []
            return True
        if self._frames_on_current_goal >= self._goal_patience:
            self._abandon_goal(current_score)
            return True
        return False

    def _maybe_replan(self, perception: Perception, current_score: int = 0) -> None:
        """Refresh agent/goal/plan when stale.

        Two conditions trigger a replan:
          (a) periodic timer (every ``_replan_every`` frames) AND new Movable rules
              have been learned since last replan;
          (b) the current goal timed out (``_check_goal_timeout`` returned True)
              and we need a fresh candidate from Prolog.
        """
        timed_out = self._check_goal_timeout(current_score)
        periodic_due = self._frames_since_replan >= self._replan_every

        if not (periodic_due or timed_out):
            self._frames_since_replan += 1
            return
        n_rules = self._count_movable_rules()
        if n_rules < 2:
            self._frames_since_replan = 0
            return
        # Skip the periodic replan if no new rules and we still have an active goal.
        if periodic_due and not timed_out and n_rules == self._last_replan_rules \
                and self._current_goal_sig is not None and self._plan_queue:
            self._frames_since_replan = 0
            return

        self._frames_since_replan = 0
        self._last_replan_rules = n_rules
        from .agent_goal import identify_agent, identify_goals
        from .planner import load_movable_rules, plan_path
        from .prolog_goal import select_next_goal

        agent = identify_agent(self.wv)
        if agent is None or agent.n_movable_rules == 0:
            self._plan_queue = []
            return
        live = [e for e in perception.entities if e.shape_hash == agent.shape_hash]
        if live:
            agent_now = type(agent)(
                shape_hash=agent.shape_hash,
                n_movable_rules=agent.n_movable_rules,
                last_known_centroid=live[0].centroid,
            )
        else:
            agent_now = agent

        # Build the empirical TraversalGraph from observed avatar transitions.
        from .traversal import build_traversal_graph
        tg = build_traversal_graph(self.wv)
        start_cell = (int(round(agent_now.last_known_centroid[0])),  # type: ignore[index]
                      int(round(agent_now.last_known_centroid[1])))  # type: ignore[index]

        # PRIORITY 1: coverage — head to a reachable but unvisited cell.
        if not self._coverage_exhausted:
            unvisited = tg.reachable_unvisited()
            # Prefer cells we haven't tried yet this session.
            fresh = unvisited - self._tried_coverage_targets
            pool = fresh if fresh else unvisited
            if pool and start_cell in tg.nodes:
                # Pick the FARTHEST candidate among the top-3 to push the
                # agent toward unexplored regions rather than looping near
                # current position. Random choice among top-3 keeps the
                # search non-deterministic.
                ranked = sorted(
                    pool,
                    key=lambda c: -(abs(c[0] - start_cell[0])
                                  + abs(c[1] - start_cell[1])),
                )
                target = random.choice(ranked[:3])
                self._tried_coverage_targets.add(target)
                # Plan via Dijkstra on the empirical graph to the cell ADJACENT
                # to target (we'll attempt the final step ourselves).
                # Find best entry-point in tg.nodes near target.
                if start_cell == target:
                    self._plan_queue = []
                    return
                # If the target IS reachable via a known visited neighbor, route there.
                best_neighbor = None
                best_dist = float("inf")
                for ac, (dy, dx) in tg.move_for_action.items():
                    nb = (target[0] - dy, target[1] - dx)
                    if nb in tg.nodes:
                        d = abs(nb[0] - start_cell[0]) + abs(nb[1] - start_cell[1])
                        if d < best_dist:
                            best_dist, best_neighbor = d, (nb, ac)
                if best_neighbor is not None:
                    nb_cell, final_action = best_neighbor
                    path_to_nb = (tg.dijkstra(start_cell, nb_cell)
                                  if start_cell != nb_cell else [])
                    actions = ([step[0] for step in path_to_nb] if path_to_nb else [])
                    actions.append(final_action)
                    self._plan_queue = actions
                    self._current_goal_sig = f"cov:{target[0]}:{target[1]}"
                    self._current_goal_centroid = (float(target[0]), float(target[1]))
                    self._current_goal_source = "coverage"
                    self._frames_on_current_goal = 0
                    self._score_at_goal_start = current_score
                    self._in_coverage_mode = True
                    return
            elif not unvisited:
                self._coverage_exhausted = True

        # PRIORITY 2: Prolog backtracking through candidate goals.
        candidates = identify_goals(self.wv, max_goals=20)
        next_goal = select_next_goal(self.wv, candidates)
        if next_goal is None:
            self._plan_queue = []
            return
        target_cell = (int(round(next_goal.centroid[0])),
                       int(round(next_goal.centroid[1])))

        # Try Dijkstra on the empirical graph first (highest fidelity).
        plan_actions: list[int] = []
        if start_cell in tg.nodes and target_cell in tg.nodes:
            empirical = tg.dijkstra(start_cell, target_cell)
            if empirical is not None:
                plan_actions = [step[0] for step in empirical]

        # Fall back to A* over the predicted movement model.
        if not plan_actions:
            from .agent_goal import GoalInfo
            chosen = GoalInfo(
                centroid=next_goal.centroid,
                bbox=(int(next_goal.centroid[0]), int(next_goal.centroid[1]),
                      int(next_goal.centroid[0]), int(next_goal.centroid[1])),
                color=-1,
                source=next_goal.source,
            )
            moves = load_movable_rules(self.wv)
            visit_counts = self.wv.agent_visit_counts(agent.shape_hash)
            plan = plan_path(agent_now, [chosen], moves,
                             visit_counts=visit_counts, revisit_penalty=1.0,
                             turn_penalty=0.3,
                             last_action_code=self._last_action_code)
            plan_actions = [step.action_code for step in plan]

        self._plan_queue = plan_actions
        self._current_goal_sig = next_goal.sig
        self._current_goal_centroid = next_goal.centroid
        self._current_goal_source = next_goal.source
        self._frames_on_current_goal = 0
        self._score_at_goal_start = current_score
        self._in_coverage_mode = False

    def decide(self, perception: Perception, current_score: int = 0) -> Decision:
        shape_hashes = list({e.shape_hash for e in perception.entities})

        # 1) planned move: coverage first, then Prolog-backtracking goals
        #    (the planner consults the empirical TraversalGraph + falls back
        #    to A* over predicted moves).
        self._maybe_replan(perception, current_score=current_score)
        if self._plan_queue:
            mode = "coverage" if self._in_coverage_mode else "planned"
            note = (f"target={self._current_goal_sig} "
                    f"remaining={len(self._plan_queue) - 1}")
            code = self._plan_queue.pop(0)
            self._last_action_code = code
            return Decision(code, mode, note)

        if random.random() >= self.epsilon:
            # 2) transition-history exploit
            exploit = self._exploit_action(shape_hashes)
            if exploit is not None:
                code, avg_r = exploit
                self._last_action_code = code
                return Decision(code, "exploit", f"avg_reward={avg_r:.2f}")
            # 3) LLM-suggested goal exploit
            llm = self._llm_suggested_action()
            if llm is not None:
                code, sup = llm
                self._last_action_code = code
                return Decision(code, "llm", f"goal-support={sup}")

        # Explore branch with visit-count tie-break — once movement rules are
        # learned, prefer actions that lead to less-visited cells.
        explore_kwargs: dict = {}
        if self._count_movable_rules() >= 2:
            from .agent_goal import identify_agent
            from .planner import load_movable_rules
            agent = identify_agent(self.wv)
            if agent is not None:
                live = [e for e in perception.entities
                       if e.shape_hash == agent.shape_hash]
                if live:
                    pos = (int(round(live[0].centroid[0])),
                           int(round(live[0].centroid[1])))
                    explore_kwargs = dict(
                        agent_pos=pos,
                        visit_counts=self.wv.agent_visit_counts(agent.shape_hash),
                        moves=load_movable_rules(self.wv),
                        last_action_code=self._last_action_code,
                    )
        novel = self._explore_action(shape_hashes, **explore_kwargs)
        if novel is not None:
            note = "least-tried+novel-cell+inertia" if explore_kwargs \
                   else "least-tried-at-shape"
            self._last_action_code = novel
            return Decision(novel, "explore", note)

        choice = random.choice(self.action_space)
        self._last_action_code = choice
        return Decision(choice, "random", "fallback")

    def promote_hypotheses(self) -> int:
        """Promote candidate hypotheses crossing confidence threshold to Rule nodes."""
        res = self.wv.conn.execute(
            """
            MATCH (h:Hypothesis {status: 'candidate'})
            WHERE h.support >= $sup
              AND (CAST(h.support AS DOUBLE) / CAST(h.support + h.refute AS DOUBLE)) >= $ratio
            RETURN h.id, h.dsl, h.action_code
            """,
            {"sup": SUPPORT_THRESHOLD, "ratio": CONFIDENCE_RATIO},
        )
        qr = res[0] if isinstance(res, list) else res
        promoted: list[tuple[str, str, int]] = []
        while qr.has_next():
            row = qr.get_next()
            promoted.append((row[0], row[1], int(row[2])))

        # Need the support count per promoted hypothesis for Provenance metadata.
        sup_by_id: dict[str, int] = {}
        if promoted:
            res2 = self.wv.conn.execute(
                "MATCH (h:Hypothesis) WHERE h.id IN $ids RETURN h.id, h.support",
                {"ids": [p[0] for p in promoted]},
            )
            qr2 = res2[0] if isinstance(res2, list) else res2
            while qr2.has_next():
                row = qr2.get_next()
                sup_by_id[row[0]] = int(row[1])

        n = 0
        for hyp_id, dsl, action_code in promoted:
            prolog_src = self._dsl_to_prolog(dsl, action_code)
            support = sup_by_id.get(hyp_id, 0)

            existing = self.wv.conn.execute(
                "MATCH (r:Rule {prolog_src: $src}) RETURN r.id LIMIT 1",
                {"src": prolog_src},
            )
            existing_row = self.wv._first_row(existing)

            if existing_row is not None:
                rule_id = existing_row[0]
                self.wv.conn.execute(
                    "MATCH (h:Hypothesis {id: $hid}), (r:Rule {id: $rid}) "
                    "CREATE (h)-[:PROMOTED_TO]->(r) "
                    "SET h.status = 'promoted'",
                    {"hid": hyp_id, "rid": rule_id},
                )
                self.wv.conn.execute(
                    "MATCH (r:Rule {id: $rid})-[:HAS_PROVENANCE]->(p:Provenance) "
                    "SET p.evidence_count = p.evidence_count + $sup",
                    {"rid": rule_id, "sup": support},
                )
            else:
                rule_id = f"rule:{hyp_id[4:]}"
                self.wv.conn.execute(
                    "CREATE (r:Rule {id: $id, prolog_src: $src, promoted_at: $ts, game_id: $g})",
                    {"id": rule_id, "src": prolog_src, "ts": self.wv._now(), "g": "ls20"},
                )
                self.wv.conn.execute(
                    "MATCH (h:Hypothesis {id: $hid}), (r:Rule {id: $rid}) "
                    "CREATE (h)-[:PROMOTED_TO]->(r) "
                    "SET h.status = 'promoted'",
                    {"hid": hyp_id, "rid": rule_id},
                )
                self.wv.classify_rule(rule_id, dsl, action_code, support)
                n += 1

        if n > 0:
            self.wv.tag_entity_affordances()
        return n

    @staticmethod
    def _dsl_to_prolog(dsl: str, action_code: int) -> str:
        if dsl.startswith("TRANSLATE("):
            return (
                f"effect(action({action_code}), entity_with_shape(SH), "
                f"translate(SH, {dsl.split('->')[1].strip()}))."
            )
        if dsl.startswith("RECOLOR("):
            return f"effect(action({action_code}), recolor({dsl.split('->')[1].strip()}))."
        return f"% unknown dsl: {dsl}"
