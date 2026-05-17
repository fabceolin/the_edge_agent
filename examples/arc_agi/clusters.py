"""Cluster detectors: group color-blobs into higher-order entities.

Four detectors produce ClusterCandidate records that the worldview layer
persists as :Cluster nodes linked to their member Entities:

    motion        blobs that share the same (dy, dx) translation between frames
    proximity     blobs whose centroids are within ``max_dist`` pixels of each other
    color_group   blobs of the same color (one cluster per color present in the frame)
    rotation      blobs whose shape_hash changed in a way consistent with a 90/180/270
                  degree rotation (bbox dims swapped, area preserved, centroid stable)
"""

from __future__ import annotations

import hashlib
import math
from dataclasses import dataclass, field

import numpy as np

from .sensors import Entity, Perception


@dataclass(frozen=True)
class ClusterCandidate:
    kind: str                  # 'motion' | 'proximity' | 'color_group' | 'rotation'
    signature: str             # stable hash for cross-frame re-identification
    member_indices: tuple[int, ...]  # indices into perception.entities
    bbox: tuple[int, int, int, int]
    centroid: tuple[float, float]
    velocity: tuple[int, int] = (0, 0)
    rotation_deg: int = 0


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _union_bbox(entities: list[Entity]) -> tuple[int, int, int, int]:
    r0 = min(e.bbox[0] for e in entities)
    c0 = min(e.bbox[1] for e in entities)
    r1 = max(e.bbox[2] for e in entities)
    c1 = max(e.bbox[3] for e in entities)
    return r0, c0, r1, c1


def _weighted_centroid(entities: list[Entity]) -> tuple[float, float]:
    total = sum(e.area for e in entities) or 1
    cy = sum(e.centroid[0] * e.area for e in entities) / total
    cx = sum(e.centroid[1] * e.area for e in entities) / total
    return cy, cx


def _signature(parts: list[str]) -> str:
    return hashlib.blake2b("|".join(sorted(parts)).encode(), digest_size=8).hexdigest()


# ---------------------------------------------------------------------------
# Detector 1: motion clusters
# ---------------------------------------------------------------------------

def _match_by_shape_color(prev: list[Entity], curr: list[Entity]) -> list[tuple[int, int]]:
    """Greedy nearest-centroid match of entities sharing shape_hash and color."""
    pairs: list[tuple[int, int]] = []
    used: set[int] = set()
    for i, p in enumerate(prev):
        candidates = [
            (j, c) for j, c in enumerate(curr)
            if j not in used and c.shape_hash == p.shape_hash and c.color == p.color
        ]
        if not candidates:
            continue
        j, c = min(
            candidates,
            key=lambda jc: (jc[1].centroid[0] - p.centroid[0]) ** 2
                          + (jc[1].centroid[1] - p.centroid[1]) ** 2,
        )
        used.add(j)
        pairs.append((i, j))
    return pairs


def detect_motion_clusters(
    prev: Perception, curr: Perception, *, min_members: int = 1
) -> list[ClusterCandidate]:
    """Group `curr` entities by the (dy, dx) translation each underwent from `prev`.

    Static entities (dy=dx=0) are skipped — they're not motion-defined.
    """
    pairs = _match_by_shape_color(prev.entities, curr.entities)
    by_vel: dict[tuple[int, int], list[int]] = {}
    for i, j in pairs:
        dy = round(curr.entities[j].centroid[0] - prev.entities[i].centroid[0])
        dx = round(curr.entities[j].centroid[1] - prev.entities[i].centroid[1])
        if dy == 0 and dx == 0:
            continue
        by_vel.setdefault((dy, dx), []).append(j)

    clusters: list[ClusterCandidate] = []
    for (dy, dx), indices in by_vel.items():
        if len(indices) < min_members:
            continue
        members = [curr.entities[i] for i in indices]
        shapes = [m.shape_hash for m in members]
        sig = _signature([f"motion:{dy}:{dx}", *shapes])
        clusters.append(ClusterCandidate(
            kind="motion",
            signature=sig,
            member_indices=tuple(indices),
            bbox=_union_bbox(members),
            centroid=_weighted_centroid(members),
            velocity=(dy, dx),
        ))
    return clusters


# ---------------------------------------------------------------------------
# Detector 2: proximity clusters (single-link DBSCAN-like, no library deps)
# ---------------------------------------------------------------------------

def detect_proximity_clusters(
    perception: Perception, *, max_dist: float = 3.0, min_members: int = 2
) -> list[ClusterCandidate]:
    """Group color-blobs whose centroids are within `max_dist` pixels (single-linkage)."""
    n = len(perception.entities)
    if n == 0:
        return []
    parent = list(range(n))

    def find(x: int) -> int:
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a: int, b: int) -> None:
        ra, rb = find(a), find(b)
        if ra != rb:
            parent[ra] = rb

    for i in range(n):
        for j in range(i + 1, n):
            cy_i, cx_i = perception.entities[i].centroid
            cy_j, cx_j = perception.entities[j].centroid
            if math.hypot(cy_i - cy_j, cx_i - cx_j) <= max_dist:
                union(i, j)

    groups: dict[int, list[int]] = {}
    for i in range(n):
        groups.setdefault(find(i), []).append(i)

    out: list[ClusterCandidate] = []
    for indices in groups.values():
        if len(indices) < min_members:
            continue
        members = [perception.entities[i] for i in indices]
        shapes = [f"{m.color}:{m.shape_hash}" for m in members]
        sig = _signature(["proximity", *shapes])
        out.append(ClusterCandidate(
            kind="proximity",
            signature=sig,
            member_indices=tuple(indices),
            bbox=_union_bbox(members),
            centroid=_weighted_centroid(members),
        ))
    return out


# ---------------------------------------------------------------------------
# Detector 3: color groups (one cluster per color present)
# ---------------------------------------------------------------------------

def detect_diff_clusters(
    prev: Perception, curr: Perception, *, max_components: int = 8
) -> list[ClusterCandidate]:
    """Region(s) of cells that changed between prev → curr.

    Generalizes per-cell RECOLOR hypotheses into a single candidate per
    contiguous changed region.
    """
    if prev.grid.shape != curr.grid.shape:
        return []
    diff = prev.grid != curr.grid
    if not diff.any():
        return []
    from scipy import ndimage
    structure = ndimage.generate_binary_structure(2, 1)
    labeled, n_components = ndimage.label(diff, structure=structure)  # type: ignore[misc]
    out: list[ClusterCandidate] = []
    slices = ndimage.find_objects(labeled)
    for idx, sl in enumerate(slices[:max_components], start=1):
        if sl is None:
            continue
        mask = labeled[sl] == idx
        ys, xs = np.where(mask)
        if ys.size == 0:
            continue
        r0, c0 = sl[0].start, sl[1].start
        bbox = (r0, c0, r0 + sl[0].stop - sl[0].start - 1, c0 + sl[1].stop - sl[1].start - 1)
        centroid = (float(ys.mean() + r0), float(xs.mean() + c0))
        # Signature includes prev/curr color pair at centroid → identifies the "kind" of change.
        cy_i, cx_i = int(round(centroid[0])), int(round(centroid[1]))
        prev_c = int(prev.grid[cy_i, cx_i]) if 0 <= cy_i < prev.grid.shape[0] else -1
        curr_c = int(curr.grid[cy_i, cx_i]) if 0 <= cy_i < curr.grid.shape[0] else -1
        sig = _signature([f"diff:{prev_c}->{curr_c}:{int(mask.sum())}"])
        out.append(ClusterCandidate(
            kind="diff",
            signature=sig,
            member_indices=(),  # diff is grid-region, not entity-based
            bbox=bbox,
            centroid=centroid,
        ))
    return out


def detect_color_groups(
    perception: Perception, *, min_members: int = 1
) -> list[ClusterCandidate]:
    """One cluster per distinct color, members = all entities of that color.

    Useful for queries like "where are all the red things?" without grid scans.
    """
    by_color: dict[int, list[int]] = {}
    for i, e in enumerate(perception.entities):
        by_color.setdefault(e.color, []).append(i)

    out: list[ClusterCandidate] = []
    for color, indices in by_color.items():
        if len(indices) < min_members:
            continue
        members = [perception.entities[i] for i in indices]
        sig = _signature([f"color_group:{color}"])
        out.append(ClusterCandidate(
            kind="color_group",
            signature=sig,
            member_indices=tuple(indices),
            bbox=_union_bbox(members),
            centroid=_weighted_centroid(members),
        ))
    return out


# ---------------------------------------------------------------------------
# Detector 4: rotation clusters
# ---------------------------------------------------------------------------

def _entity_bitmap(perception: Perception, entity: Entity) -> np.ndarray:
    """Crop the binary mask of an entity from its perception grid."""
    r0, c0, r1, c1 = entity.bbox
    region = perception.grid[r0:r1 + 1, c0:c1 + 1]
    return (region == entity.color)


def detect_rotation_clusters(
    prev: Perception, curr: Perception, *,
    centroid_tol: float = 0.5,
    min_area: int = 4,
) -> list[ClusterCandidate]:
    """Detect entities whose shape rotated 90/180/270 degrees between frames.

    Tightened heuristic: requires (i) area preserved AND ≥ ``min_area``,
    (ii) centroid stable within ``centroid_tol``, (iii) bitmap actually matches
    a 90/180/270 rotation of the previous bitmap (not just dim swap).
    """
    out: list[ClusterCandidate] = []
    for j, c in enumerate(curr.entities):
        if c.area < min_area:
            continue
        c_bitmap = _entity_bitmap(curr, c)
        for p in prev.entities:
            if p.area != c.area or p.color != c.color:
                continue
            if abs(c.centroid[0] - p.centroid[0]) > centroid_tol:
                continue
            if abs(c.centroid[1] - p.centroid[1]) > centroid_tol:
                continue
            if p.shape_hash == c.shape_hash:
                continue
            p_bitmap = _entity_bitmap(prev, p)
            rotation_deg = 0
            for deg, k in ((90, 1), (180, 2), (270, 3)):
                rotated = np.rot90(p_bitmap, k=k)
                if rotated.shape == c_bitmap.shape and np.array_equal(rotated, c_bitmap):
                    rotation_deg = deg
                    break
            if rotation_deg == 0:
                continue
            sig = _signature([f"rotation:{p.shape_hash}->{c.shape_hash}:{rotation_deg}"])
            out.append(ClusterCandidate(
                kind="rotation",
                signature=sig,
                member_indices=(j,),
                bbox=c.bbox,
                centroid=c.centroid,
                rotation_deg=rotation_deg,
            ))
            break
    return out


# ---------------------------------------------------------------------------
# Unified entry point
# ---------------------------------------------------------------------------

def detect_containment_clusters(
    perception: Perception, *, min_inner_area: int = 1,
    max_outer_ratio: float = 0.5,
) -> list[ClusterCandidate]:
    """Entities whose bbox is entirely inside another entity's bbox.

    Skips outers that span > ``max_outer_ratio`` of the grid (typically the
    background filling the whole 64×64 frame, which trivially contains everything).
    """
    grid_area = perception.grid.shape[0] * perception.grid.shape[1]
    outer_area_cap = int(grid_area * max_outer_ratio)
    out: list[ClusterCandidate] = []
    for i, inner in enumerate(perception.entities):
        if inner.area < min_inner_area:
            continue
        for j, outer in enumerate(perception.entities):
            if i == j or outer.area <= inner.area or outer.area > outer_area_cap:
                continue
            o_r0, o_c0, o_r1, o_c1 = outer.bbox
            i_r0, i_c0, i_r1, i_c1 = inner.bbox
            if o_r0 <= i_r0 and o_c0 <= i_c0 and o_r1 >= i_r1 and o_c1 >= i_c1:
                sig = _signature([
                    f"contain:{outer.color}:{outer.shape_hash}:{inner.color}:{inner.shape_hash}"
                ])
                out.append(ClusterCandidate(
                    kind="containment",
                    signature=sig,
                    member_indices=(j, i),
                    bbox=outer.bbox,
                    centroid=outer.centroid,
                ))
                break  # one container per inner entity is enough
    return out


def detect_alignment_clusters(
    perception: Perception, *, tol: float = 1.0, min_members: int = 3
) -> list[ClusterCandidate]:
    """Entities aligned along the same row, column, or 45°-diagonal.

    Three sub-axes per call: row (y-stable), col (x-stable), diag (x+y or x-y stable).
    """
    n = len(perception.entities)
    if n < min_members:
        return []
    out: list[ClusterCandidate] = []

    def group_by(keyfn):
        groups: dict[int, list[int]] = {}
        for i, e in enumerate(perception.entities):
            k = int(round(keyfn(e)))
            groups.setdefault(k, []).append(i)
        return groups

    axes = [
        ("row", lambda e: e.centroid[0]),
        ("col", lambda e: e.centroid[1]),
        ("diag_sum", lambda e: e.centroid[0] + e.centroid[1]),
        ("diag_diff", lambda e: e.centroid[0] - e.centroid[1]),
    ]
    for axis_name, keyfn in axes:
        for k, indices in group_by(keyfn).items():
            if len(indices) < min_members:
                continue
            members = [perception.entities[i] for i in indices]
            # Filter to tolerance on the key axis.
            vals = [keyfn(m) for m in members]
            if max(vals) - min(vals) > tol:
                continue
            sig = _signature([f"align:{axis_name}:{k}", *[m.shape_hash for m in members]])
            out.append(ClusterCandidate(
                kind="alignment",
                signature=sig,
                member_indices=tuple(indices),
                bbox=_union_bbox(members),
                centroid=_weighted_centroid(members),
            ))
    return out


@dataclass
class ClusterReport:
    motion: list[ClusterCandidate] = field(default_factory=list)
    proximity: list[ClusterCandidate] = field(default_factory=list)
    color_group: list[ClusterCandidate] = field(default_factory=list)
    rotation: list[ClusterCandidate] = field(default_factory=list)
    diff: list[ClusterCandidate] = field(default_factory=list)
    containment: list[ClusterCandidate] = field(default_factory=list)
    alignment: list[ClusterCandidate] = field(default_factory=list)

    def all(self) -> list[ClusterCandidate]:
        return [*self.motion, *self.proximity, *self.color_group, *self.rotation,
                *self.diff, *self.containment, *self.alignment]


def detect_all(
    perception: Perception,
    prev: Perception | None = None,
    *,
    proximity_max_dist: float = 3.0,
    include_per_frame_heavy: bool = False,
) -> ClusterReport:
    """Detect clusters for a single frame.

    By default skips ``containment`` and ``alignment`` (which fire many candidates
    per frame and dominate the write cost). Enable them on a sample of frames
    only or run via post-hoc methods on the worldview.
    """
    report = ClusterReport(
        proximity=detect_proximity_clusters(perception, max_dist=proximity_max_dist),
        color_group=detect_color_groups(perception),
    )
    if include_per_frame_heavy:
        report.containment = detect_containment_clusters(perception)
        report.alignment = detect_alignment_clusters(perception)
    if prev is not None:
        report.motion = detect_motion_clusters(prev, perception)
        report.rotation = detect_rotation_clusters(prev, perception)
        report.diff = detect_diff_clusters(prev, perception)
    return report
