"""Sensors: convert raw 64x64 grid frames into symbolic entity descriptions."""

from __future__ import annotations

from dataclasses import dataclass, field
import hashlib

import numpy as np
from scipy import ndimage


@dataclass(frozen=True)
class Entity:
    color: int
    bbox: tuple[int, int, int, int]
    centroid: tuple[float, float]
    area: int
    shape_hash: str

    @property
    def width(self) -> int:
        return self.bbox[3] - self.bbox[1] + 1

    @property
    def height(self) -> int:
        return self.bbox[2] - self.bbox[0] + 1


@dataclass
class Perception:
    grid: np.ndarray
    grid_hash: str
    entities: list[Entity] = field(default_factory=list)


def _shape_hash(mask: np.ndarray) -> str:
    rows = np.where(mask.any(axis=1))[0]
    cols = np.where(mask.any(axis=0))[0]
    if rows.size == 0:
        return "empty"
    cropped = mask[rows.min():rows.max() + 1, cols.min():cols.max() + 1]
    return hashlib.blake2b(cropped.tobytes(), digest_size=8).hexdigest()


def perceive(grid: np.ndarray) -> Perception:
    """Extract entities from a 64x64 grid using 4-connectivity per color."""
    grid_hash = hashlib.blake2b(grid.tobytes(), digest_size=12).hexdigest()
    entities: list[Entity] = []
    structure = ndimage.generate_binary_structure(2, 1)
    for color in np.unique(grid):
        mask = grid == color
        labeled, n = ndimage.label(mask, structure=structure)  # type: ignore[misc]
        if n == 0:
            continue
        slices = ndimage.find_objects(labeled)
        for idx, sl in enumerate(slices, start=1):
            if sl is None:
                continue
            comp = labeled[sl] == idx
            ys, xs = np.where(comp)
            area = int(comp.sum())
            r0, c0 = sl[0].start, sl[1].start
            entities.append(
                Entity(
                    color=int(color),
                    bbox=(r0, c0, r0 + sl[0].stop - sl[0].start - 1, c0 + sl[1].stop - sl[1].start - 1),
                    centroid=(float(ys.mean() + r0), float(xs.mean() + c0)),
                    area=area,
                    shape_hash=_shape_hash(comp),
                )
            )
    return Perception(grid=grid, grid_hash=grid_hash, entities=entities)
