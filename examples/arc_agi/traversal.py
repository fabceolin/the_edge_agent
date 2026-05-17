"""TraversalGraph: an empirical map of the avatar's reachability.

Built from observed Avatar transitions in the worldview. Captures:
  - which cells the avatar actually entered
  - which (action_code) transitions were observed between cells
  - self-loops where the avatar tried an action and ended up in the same cell
    (a wall / no-op)

This is more accurate than the idealized "every cell is walkable" assumption
of the original A* planner. With a TraversalGraph we can:
  - Dijkstra a real shortest path through cells we've already proved walkable
  - Detect frontier cells (visited cells with at least one tried-but-failed
    direction worth re-attempting from elsewhere)
  - Detect reachable-but-unvisited cells (neighbors predicted by movement
    rules that we haven't yet entered)
"""

from __future__ import annotations

import heapq
from dataclasses import dataclass, field

from .worldview import Worldview


Cell = tuple[int, int]  # (row, col)


@dataclass
class Edge:
    action_code: int
    count: int = 0
    is_wall: bool = False  # self-loop = wall


@dataclass
class TraversalGraph:
    nodes: set[Cell] = field(default_factory=set)
    # adjacency: src_cell → list[(dst_cell, Edge)]
    adj: dict[Cell, list[tuple[Cell, Edge]]] = field(default_factory=dict)
    # per-cell wall map: cell → set of action_codes that produced self-loops
    walls: dict[Cell, set[int]] = field(default_factory=dict)
    # action_code → average (dy, dx) observed (movement model derived from data)
    move_for_action: dict[int, tuple[int, int]] = field(default_factory=dict)

    def add_edge(self, src: Cell, dst: Cell, action_code: int) -> None:
        self.nodes.add(src)
        self.nodes.add(dst)
        is_wall = src == dst
        bucket = self.adj.setdefault(src, [])
        for i, (existing_dst, edge) in enumerate(bucket):
            if existing_dst == dst and edge.action_code == action_code:
                edge.count += 1
                return
        bucket.append((dst, Edge(action_code=action_code, count=1, is_wall=is_wall)))
        if is_wall:
            self.walls.setdefault(src, set()).add(action_code)

    def dijkstra(self, start: Cell, goal: Cell) -> list[tuple[int, Cell]] | None:
        """Shortest path from start to goal. Returns ordered list of (action_code,
        destination_cell). None if unreachable.

        Edge weight = 1 + 5 * (1 - count/max_count_in_bucket). Frequently-used
        edges are cheaper (we're more confident about them); self-loops cost ∞.
        """
        if start not in self.nodes or goal not in self.nodes:
            return None
        frontier: list[tuple[float, Cell, list[tuple[int, Cell]]]] = []
        heapq.heappush(frontier, (0.0, start, []))
        best: dict[Cell, float] = {start: 0.0}
        while frontier:
            cost, cur, path = heapq.heappop(frontier)
            if cur == goal:
                return path
            for dst, edge in self.adj.get(cur, []):
                if edge.is_wall:
                    continue
                step = 1.0 / max(1, edge.count)  # confidence-weighted
                new_cost = cost + 1.0 + step
                if dst in best and best[dst] <= new_cost:
                    continue
                best[dst] = new_cost
                heapq.heappush(frontier, (new_cost, dst, path + [(edge.action_code, dst)]))
        return None

    def frontier_nodes(self) -> set[Cell]:
        """Visited cells that have at least one action_code with no successful
        outgoing edge (walls or never-attempted directions). These are the
        natural targets for exploration: from them, an untried action might
        reveal a new cell."""
        out: set[Cell] = set()
        all_actions = set(self.move_for_action.keys())
        for cell in self.nodes:
            tried: set[int] = {
                e.action_code for _, e in self.adj.get(cell, []) if not e.is_wall
            }
            blocked = self.walls.get(cell, set())
            if all_actions - tried - blocked:
                out.add(cell)
        return out

    def reachable_unvisited(self) -> set[Cell]:
        """Cells one move away from a visited cell that we've never entered.

        For each visited cell + each known move direction, compute the predicted
        landing cell. If that cell is not already in self.nodes and isn't blocked
        by a known wall, it's a reachable unvisited target.
        """
        out: set[Cell] = set()
        for cell in self.nodes:
            blocked = self.walls.get(cell, set())
            for action_code, (dy, dx) in self.move_for_action.items():
                if action_code in blocked:
                    continue
                target = (cell[0] + dy, cell[1] + dx)
                if 0 <= target[0] < 64 and 0 <= target[1] < 64 \
                   and target not in self.nodes:
                    out.add(target)
        return out


def build_traversal_graph(wv: Worldview) -> TraversalGraph:
    """Construct the empirical TraversalGraph from observed avatar transitions."""
    g = TraversalGraph()

    # Find the agent shape (most movable rules win).
    agent_row = _exec(wv,
        "MATCH (s:Shape) WHERE s.role_concept = 'Agent' RETURN s.shape_hash LIMIT 1")
    if not agent_row:
        # Fallback: shape mentioned most in TRANSLATE hypotheses.
        rows = _exec(wv,
            """
            MATCH (h:Hypothesis)-[:PROMOTED_TO]->(r:Rule)
                  -[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'})
            WHERE h.dsl STARTS WITH 'TRANSLATE'
            RETURN h.dsl
            """)
        if not rows:
            return g
        import re
        from collections import Counter
        counter: Counter = Counter()
        for (dsl,) in rows:
            m = re.search(r"shape_hash=([0-9a-f]+)", dsl)
            if m:
                counter[m.group(1)] += 1
        if not counter:
            return g
        agent_shape = counter.most_common(1)[0][0]
    else:
        agent_shape = agent_row[0][0]

    # Movement model from learned rules.
    rule_rows = _exec(wv,
        """
        MATCH (h:Hypothesis)-[:PROMOTED_TO]->(:Rule)
              -[:RULE_INSTANCE_OF]->(:Concept {name:'Movable'})
        WHERE h.dsl STARTS WITH 'TRANSLATE'
        RETURN h.dsl
        """)
    import re
    seen_moves: dict[int, tuple[int, int]] = {}
    for (dsl,) in rule_rows:
        m = re.match(r"TRANSLATE\(action=(\d+).*?->\s*\((-?\d+),\s*(-?\d+)\)", dsl)
        if m:
            seen_moves[int(m.group(1))] = (int(m.group(2)), int(m.group(3)))
    g.move_for_action = seen_moves

    # Pull all transitions whose source observation contains the agent. The
    # target observation also contains the agent (if avatar still alive).
    rows = _exec(wv,
        """
        MATCH (o1:Observation)-[t:TRANSITION]->(o2:Observation)
        MATCH (o1)-[:CONTAINS]->(e1:Entity {shape_hash: $sh})
        MATCH (o2)-[:CONTAINS]->(e2:Entity {shape_hash: $sh})
        RETURN e1.centroid_y, e1.centroid_x, t.action_code,
               e2.centroid_y, e2.centroid_x
        ORDER BY o1.frame_n
        """,
        {"sh": agent_shape})
    for r in rows:
        src = (int(round(float(r[0]))), int(round(float(r[1]))))
        action = int(r[2])
        dst = (int(round(float(r[3]))), int(round(float(r[4]))))
        g.add_edge(src, dst, action)
    return g


def _exec(wv: Worldview, q: str, p: dict | None = None) -> list:
    res = wv.conn.execute(q, p or {})
    qr = res[0] if isinstance(res, list) else res
    rows = []
    while qr.has_next():
        rows.append(qr.get_next())
    return rows
