"""Minimal vertical slice with KuzuDB worldview + symbolic induction wired in.

Loop:
    perceive  — compute Perception, record Observation + (Transition + induce) if prev exists
    decide    — random action (placeholder for reasoner)
    act       — env.step → store new raw obs in state
    (loop)    — back to perceive until max_frames or game finished
"""

from __future__ import annotations

import argparse
import random
from typing import Any

import the_edge_agent as tea

from . import GAME_ID
from .clusters import detect_all
from .induction import induce
from .llm_hypotheses import query_llm, write_suggestion_to_graph
from .reasoner import Reasoner
from .runtime import GameAction, get_runtime, init_runtime, shutdown
from .sensors import perceive
from .shapes import all_unique_shapes, entity_bitmap_ascii
from .snapshot import latest_episode, restore_from_storage, snapshot_to_storage


_ACTION_BY_VALUE: dict[int, GameAction] = {int(a.value): a for a in GameAction}


def _perceive_node(state: dict[str, Any]) -> dict[str, Any]:
    import traceback
    try:
        return _perceive_node_inner(state)
    except IndexError as e:
        print(f"\n[perceive crash] {e}")
        traceback.print_exc()
        raise


def _perceive_node_inner(state: dict[str, Any]) -> dict[str, Any]:
    rt = get_runtime()
    obs = state.get("_last_obs") or rt.env.observation_space
    # Empty frame typically signals a level transition or game-state change.
    # Skip perception this turn and let the loop continue.
    if not obs.frame:
        return {
            "_last_obs": obs,
            "frame_n": int(state.get("frame_n", 0)),
            "game_state": str(obs.state),
            "score": int(getattr(obs, "levels_completed", 0)),
        }
    grid = obs.frame[0]
    perception = perceive(grid)

    obs_id, entity_ids = rt.worldview.record_frame(
        perception=perception,
        episode=rt.episode,
        frame_n=int(state.get("frame_n", 0)),
        score=int(getattr(obs, "levels_completed", 0)),
        state=str(obs.state),
    )

    # Persist each unique shape once + link this frame's entities to their shapes.
    for shape_hash, (rep_entity, bitmap) in all_unique_shapes(perception).items():
        rt.worldview.upsert_shape(
            shape_hash=shape_hash,
            bitmap_ascii=bitmap,
            height=rep_entity.height,
            width=rep_entity.width,
            area=rep_entity.area,
        )
    for entity, eid in zip(perception.entities, entity_ids):
        rt.worldview.link_entity_to_shape(eid, entity.shape_hash)

    prev_perception = state.get("_prev_perception")
    prev_obs_id = state.get("_prev_obs_id")
    prev_action_code = state.get("_prev_action_code")
    n_new_hyps = 0
    if prev_perception is not None and prev_obs_id is not None and prev_action_code is not None:
        prev_score = int(state.get("score", 0))
        curr_score = int(getattr(obs, "levels_completed", 0))
        rt.worldview.record_transition(
            prev_obs_id=prev_obs_id,
            next_obs_id=obs_id,
            action_code=prev_action_code,
            action_xy=None,
            reward=curr_score - prev_score,
        )
        for hyp in induce(prev_perception, prev_action_code, perception):
            rt.worldview.upsert_hypothesis(hyp.dsl, hyp.action_code)
            n_new_hyps += 1

    # Per-frame: only the cheapest clusters that the hot path needs (motion,
    # used by induction and the planner). Everything else runs post-hoc.
    n_clusters = 0
    if prev_perception is not None:
        from .clusters import detect_motion_clusters
        for candidate in detect_motion_clusters(prev_perception, perception):
            member_ids = [entity_ids[i] for i in candidate.member_indices]
            rt.worldview.record_cluster(
                kind=candidate.kind,
                signature=candidate.signature,
                member_entity_ids=member_ids,
                observation_id=obs_id,
                bbox=candidate.bbox,
                centroid=candidate.centroid,
                velocity=candidate.velocity,
                rotation_deg=candidate.rotation_deg,
            )
            n_clusters += 1

    return {
        "_last_obs": obs,
        "_prev_perception": perception,
        "_prev_obs_id": obs_id,
        "perception": {
            "grid_hash": perception.grid_hash,
            "n_entities": len(perception.entities),
            "n_clusters": n_clusters,
            "new_hyps": n_new_hyps,
        },
        "game_state": str(obs.state),
        "score": int(getattr(obs, "levels_completed", 0)),
    }


def _decide_node(state: dict[str, Any]) -> dict[str, Any]:
    rt = get_runtime()
    reasoner: Reasoner = state["_reasoner"]
    perception = state.get("_prev_perception")
    current_score = int(state.get("score", 0))
    if perception is None:
        # Frame had no grid data (level transition). Default to a random action.
        chosen_code = random.choice([int(a.value) for a in rt.env.action_space])
        counts = dict(state.get("_decision_counts", {}))
        counts["random"] = counts.get("random", 0) + 1
        return {
            "chosen_action": chosen_code,
            "decision_mode": "random",
            "_chosen_action": _ACTION_BY_VALUE[chosen_code],
            "_decision_counts": counts,
        }
    decision = reasoner.decide(perception, current_score=current_score)
    chosen = _ACTION_BY_VALUE[decision.action_code]
    counts = dict(state.get("_decision_counts", {}))
    counts[decision.mode] = counts.get(decision.mode, 0) + 1
    return {
        "chosen_action": decision.action_code,
        "decision_mode": decision.mode,
        "_chosen_action": chosen,
        "_decision_counts": counts,
    }


def _act_node(state: dict[str, Any]) -> dict[str, Any]:
    rt = get_runtime()
    action: GameAction = state["_chosen_action"]
    next_obs = rt.env.step(action)
    frame_n = int(state.get("frame_n", 0)) + 1
    # Promote hypotheses to rules periodically so the planner can pick them up
    # mid-episode rather than only at the end.
    if frame_n > 0 and frame_n % 15 == 0:
        reasoner: Reasoner = state["_reasoner"]
        reasoner.promote_hypotheses()
        # Discover anchor clusters so the planner has goal candidates mid-episode.
        rt.worldview.discover_anchor_clusters(min_persistence=10)
    return {
        "_last_obs": next_obs,
        "_prev_action_code": int(action.value),
        "frame_n": frame_n,
        "game_state": str(next_obs.state),
    }


def _consult_llm_node(state: dict[str, Any]) -> dict[str, Any]:
    rt = get_runtime()
    perception = state.get("_prev_perception")
    if perception is None:
        return {"llm_consulted": False}
    suggestion = query_llm(perception, rt.worldview)
    if suggestion is None:
        return {"llm_consulted": False}
    write_suggestion_to_graph(rt.worldview, suggestion)
    return {
        "llm_consulted": True,
        "llm_goal": suggestion.goal,
        "llm_action": suggestion.suggested_action,
    }


def _continue(state: dict[str, Any]) -> str:
    frame_n = int(state.get("frame_n", 0))
    if frame_n >= state.get("max_frames", 50):
        state["_termination_reason"] = "max_frames"
        return tea.END
    game_state = str(state.get("game_state", ""))
    terminal = any(t in game_state for t in
                   ("GAME_OVER", "WIN", "LEVEL_COMPLETE", "DONE"))
    if terminal or (
        "FINISHED" in game_state and "NOT_FINISHED" not in game_state):
        state["_termination_reason"] = f"game_state={game_state}"
        return tea.END
    # Stuck detection: the whole frame's grid_hash hasn't changed for K frames.
    stuck_threshold = int(state.get("stuck_threshold", 0) or 0)
    if stuck_threshold > 0:
        prev_perception = state.get("_prev_perception")
        if prev_perception is not None:
            recent_hashes = state.get("_recent_grid_hashes", [])
            recent_hashes.append(prev_perception.grid_hash)
            recent_hashes = recent_hashes[-stuck_threshold:]
            state["_recent_grid_hashes"] = recent_hashes
            if len(recent_hashes) >= stuck_threshold \
               and len(set(recent_hashes)) == 1:
                state["_termination_reason"] = (
                    f"grid_unchanged_for_{stuck_threshold}_frames")
                return tea.END
    # Prolog backtracking exhaustion: reasoner reports no candidates left
    # AND coverage exhausted AND no rewards observed.
    reasoner = state.get("_reasoner")
    if (reasoner is not None
        and getattr(reasoner, "_coverage_exhausted", False)
        and not reasoner._plan_queue
        and state.get("score", 0) == 0
        and frame_n > 30):
        # Probe: if coverage is exhausted AND Prolog returns no next goal,
        # there's literally nothing left to try except keep exploring randomly.
        # We let the caller decide to halt or not via --halt-on-exhausted.
        if state.get("halt_on_exhausted", False):
            state["_termination_reason"] = "all_candidates_exhausted"
            return tea.END
    llm_every = int(state.get("llm_every", 0) or 0)
    if (
        llm_every > 0
        and frame_n > 0
        and frame_n % llm_every == 0
        and int(state.get("score", 0)) == 0
    ):
        return "consult_llm"
    return "perceive"


def build_graph() -> Any:
    g = tea.StateGraph({
        "frame_n": int,
        "max_frames": int,
        "score": int,
        "game_state": str,
        "perception": dict,
        "chosen_action": int,
        "llm_every": int,
    })
    g.add_node("perceive", run=_perceive_node)
    g.add_node("decide", run=_decide_node)
    g.add_node("act", run=_act_node)
    g.add_node("consult_llm", run=_consult_llm_node)
    g.set_entry_point("perceive")
    g.add_edge("perceive", "decide")
    g.add_edge("decide", "act")
    g.add_conditional_edges(
        "act",
        _continue,
        {"perceive": "perceive", "consult_llm": "consult_llm", tea.END: tea.END},
    )
    g.add_edge("consult_llm", "perceive")
    return g.compile()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--max-frames", type=int, default=50)
    parser.add_argument("--db", type=str, default=":memory:")
    parser.add_argument("--seed", type=int, default=0)
    parser.add_argument("--epsilon", type=float, default=0.3,
                        help="probability of exploration vs exploitation")
    parser.add_argument("--llm-every", type=int, default=0,
                        help="consult Claude via `claude -p` every N frames when score==0 (0=off)")
    parser.add_argument("--episode", type=int, default=0,
                        help="episode number (used for snapshot path)")
    parser.add_argument("--ltm-storage", type=str, default=None,
                        help="optional fsspec URI (file://, gs://, s3://) for cold-store snapshots")
    parser.add_argument("--restore-prev", action="store_true",
                        help="restore latest snapshot under --ltm-storage before running")
    parser.add_argument("--name-shapes", action="store_true",
                        help="after the loop, call LLM to name shapes and apply semantic priors")
    parser.add_argument("--stuck-threshold", type=int, default=20,
                        help="halt if avatar stays in same cell for N frames (0 disables)")
    parser.add_argument("--halt-on-exhausted", action="store_true",
                        help="halt when coverage is done and Prolog has tried all candidates")
    args = parser.parse_args()

    random.seed(args.seed)

    if args.restore_prev and args.ltm_storage and args.db != ":memory:":
        prev = latest_episode(args.ltm_storage, game_id=GAME_ID)
        if prev is not None:
            ok = restore_from_storage(
                args.ltm_storage,
                args.db,
                game_id=GAME_ID,
                episode=prev,
            )
            print(f"[restore] from episode_{prev:04d}: {'ok' if ok else 'miss'}")

    rt = init_runtime(db_path=args.db, episode=args.episode)
    try:
        action_codes = [int(a.value) for a in rt.env.action_space]
        reasoner = Reasoner(rt.worldview, action_codes, epsilon=args.epsilon)
        graph = build_graph()
        final_state: dict[str, Any] = {}
        for ev in graph.invoke({
            "frame_n": 0,
            "max_frames": args.max_frames,
            "score": 0,
            "llm_every": args.llm_every,
            "stuck_threshold": args.stuck_threshold,
            "halt_on_exhausted": args.halt_on_exhausted,
            "_reasoner": reasoner,
        }):
            if isinstance(ev, dict) and ev.get("type") == "final":
                final_state = ev.get("state", {})

        n_anchors = rt.worldview.discover_anchor_clusters(min_persistence=max(5, args.max_frames // 4))
        n_paths = rt.worldview.discover_path_clusters(min_length=3)

        # Physics detectors always run — they're cheap post-hoc queries and the
        # results are useful even when most detectors don't fire for this game.
        from .physics import discover_all_physics
        physics_counts = discover_all_physics(rt.worldview)

        # Name shapes and classify (cheap if no new shapes since last call).
        from .shape_namer import name_unnamed_shapes
        from .shape_classifier import classify_shapes
        n_named = name_unnamed_shapes(rt.worldview, min_observations=3) if args.name_shapes else 0
        n_classified = classify_shapes(rt.worldview) if args.name_shapes else 0
        priors = rt.worldview.apply_semantic_priors() if args.name_shapes else {"rejected": 0, "boosted": 0}

        promoted = reasoner.promote_hypotheses()
        stats = rt.worldview.stats()
        stats["decision_modes"] = final_state.get("_decision_counts", {})
        stats["anchors_new"] = n_anchors
        stats["paths_new"] = n_paths
        stats["named_shapes"] = n_named
        stats["classified_shapes"] = n_classified
        stats["priors_rejected"] = priors["rejected"]
        stats["priors_boosted"] = priors["boosted"]
        for subkind, count in physics_counts.items():
            if count > 0:
                stats[f"physics_{subkind}"] = count
        termination = final_state.get("_termination_reason", "unknown")
        print(
            f"\nepisode={rt.episode} "
            f"frames={final_state.get('frame_n')} "
            f"score={final_state.get('score')} "
            f"state={final_state.get('game_state')} "
            f"termination={termination}"
        )
        print(f"graph: {stats} (promoted_this_run={promoted})")
    finally:
        shutdown()

    # Snapshot AFTER shutdown so KuzuDB has flushed all writes to disk.
    if args.ltm_storage and args.db != ":memory:":
        dest = snapshot_to_storage(
            args.db,
            args.ltm_storage,
            game_id=GAME_ID,
            episode=args.episode,
        )
        print(f"[snapshot] -> {dest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
