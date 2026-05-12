"""Shape extraction: one persistent record per distinct shape_hash.

Bitmaps are cropped from each entity's region and stored once per hash. The
ASCII form (`0`/`1` rows) is what we send to the LLM in a later pass for
naming and semantic classification.
"""

from __future__ import annotations

from .sensors import Entity, Perception


def entity_bitmap_ascii(perception: Perception, entity: Entity) -> str:
    """Return a compact ASCII bitmap of the entity's filled cells."""
    r0, c0, r1, c1 = entity.bbox
    region = (perception.grid[r0:r1 + 1, c0:c1 + 1] == entity.color)
    return "\n".join("".join("1" if v else "0" for v in row) for row in region)


def all_unique_shapes(perception: Perception) -> dict[str, tuple[Entity, str]]:
    """Map shape_hash → (representative entity, ascii bitmap) for this frame."""
    out: dict[str, tuple[Entity, str]] = {}
    for ent in perception.entities:
        if ent.shape_hash in out:
            continue
        out[ent.shape_hash] = (ent, entity_bitmap_ascii(perception, ent))
    return out
