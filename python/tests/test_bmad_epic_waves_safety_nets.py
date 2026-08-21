"""Safety nets ported from bmad-epic-waves.js into the tea YAML workflows.

Every test here covers a mechanism that exists because of a real incident: two runs
racing on the same working tree, a merge conflict auto-resolved when it should not have
been, a story dispatched against a cross-epic dependency that was never satisfied, and a
green suite that never collected the code it was supposed to prove.
"""

from __future__ import annotations

import subprocess
import sys
import textwrap
from pathlib import Path

import pytest
import yaml

from the_edge_agent.bmad_epic_waves_git import (
    acquire_main_repo_lock,
    classify_conflict,
    heartbeat_main_repo_lock,
    release_main_repo_lock,
    suite_covers_paths,
)

ROOT = Path(__file__).parents[2]
WORKFLOWS = ROOT / "examples/workflows"


def git(repo: Path, *args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=True
    ).stdout.strip()


def init_repo(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    git(path, "init", "-b", "main")
    git(path, "config", "user.name", "Test")
    git(path, "config", "user.email", "test@example.com")
    (path / "base.txt").write_text("base\n")
    git(path, "add", "base.txt")
    git(path, "commit", "-m", "base")
    return path


def run_node(workflow: str, name: str, state: dict):
    spec = yaml.safe_load((WORKFLOWS / f"{workflow}.yaml").read_text())
    code = next(node["run"] for node in spec["nodes"] if node["name"] == name)
    namespace: dict = {}
    exec("def node(state):\n" + textwrap.indent(code, "    "), namespace)
    return namespace["node"](state)


def conflicting_repo(
    tmp_path: Path, ours: str, theirs: str, filename: str = "shared.py"
) -> Path:
    """Leave `repo` sitting on an unresolved merge conflict in `filename`."""
    repo = init_repo(tmp_path / "repo")
    (repo / filename).write_text("a\nb\nc\n")
    git(repo, "add", filename)
    git(repo, "commit", "-m", "shared base")

    git(repo, "checkout", "-b", "story/one")
    (repo / filename).write_text(ours)
    git(repo, "commit", "-am", "one")

    git(repo, "checkout", "main")
    git(repo, "checkout", "-b", "story/two")
    (repo / filename).write_text(theirs)
    git(repo, "commit", "-am", "two")

    git(repo, "checkout", "main")
    git(repo, "merge", "--no-ff", "-m", "m1", "story/one")
    subprocess.run(
        ["git", "merge", "--no-ff", "-m", "m2", "story/two"],
        cwd=repo,
        capture_output=True,
        text=True,
        check=False,
    )
    return repo


# ---------------------------------------------------------------------------
# CROSS-RUN LOCK
# ---------------------------------------------------------------------------


def _spawn_holder(repo: Path, token: str) -> subprocess.Popen:
    """A holder in ANOTHER process — the only shape that really tests the queue."""
    script = textwrap.dedent(
        f"""
        import time
        from the_edge_agent.bmad_epic_waves_git import acquire_main_repo_lock
        r = acquire_main_repo_lock({str(repo)!r}, {token!r})
        print("held" if r["acquired"] else "failed", flush=True)
        time.sleep(60)
        """
    )
    proc = subprocess.Popen(
        [sys.executable, "-c", script], stdout=subprocess.PIPE, text=True
    )
    assert proc.stdout.readline().strip() == "held"
    return proc


def test_second_run_cannot_take_a_held_lock(tmp_path: Path) -> None:
    holder = _spawn_holder(tmp_path, "sibling-run")
    try:
        second = acquire_main_repo_lock(tmp_path, wait_seconds=0.3, poll_seconds=0.05)

        assert second["acquired"] is False
        assert second["reason"] == "lock_timeout"
    finally:
        holder.kill()
        holder.wait(timeout=10)


def test_a_second_token_in_the_same_process_is_refused_not_granted(
    tmp_path: Path,
) -> None:
    """flock is re-entrant per process; handing out a second owner would mean the
    first release drops the lock under the second one's feet."""
    first = acquire_main_repo_lock(tmp_path, "prep")
    assert first["acquired"] is True

    second = acquire_main_repo_lock(tmp_path, "integrate", wait_seconds=0)

    assert second["acquired"] is False
    assert second["reason"] == "already_held_here"
    assert release_main_repo_lock(tmp_path, "prep") is True


def test_release_refuses_a_token_that_is_not_the_holder(tmp_path: Path) -> None:
    mine = acquire_main_repo_lock(tmp_path)
    try:
        assert release_main_repo_lock(tmp_path, "someone-else") is False
        assert heartbeat_main_repo_lock(tmp_path, mine["owner"]) is True
    finally:
        assert release_main_repo_lock(tmp_path, mine["owner"]) is True


def test_a_live_holder_is_never_displaced_however_long_it_holds(tmp_path: Path) -> None:
    """No staleness window to age out of: the lock lasts exactly as long as the
    process, so a slow-but-alive holder cannot lose it to a waiter."""
    holder = _spawn_holder(tmp_path, "slow-suite")
    try:
        waiter = acquire_main_repo_lock(tmp_path, wait_seconds=0.3, poll_seconds=0.05)

        assert waiter["acquired"] is False
        assert waiter["reason"] == "lock_timeout"
    finally:
        holder.kill()
        holder.wait(timeout=10)


def test_a_dead_holder_frees_the_queue_immediately(tmp_path: Path) -> None:
    """What the old steal protocol existed for: run_waves kills the whole tmux
    session on timeout, and a story process dying mid-merge used to leave a lock
    standing until someone judged it stale."""
    holder = _spawn_holder(tmp_path, "doomed")
    holder.kill()
    holder.wait(timeout=10)

    taken = acquire_main_repo_lock(tmp_path, wait_seconds=5, poll_seconds=0.05)

    assert taken["acquired"] is True
    assert release_main_repo_lock(tmp_path, taken["owner"]) is True


def test_orphan_stash_blocks_the_lock_before_anything_merges(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    (repo / "base.txt").write_text("work nobody claimed\n")
    git(repo, "stash", "push", "-m", "orphan")

    blocked = acquire_main_repo_lock(repo, wait_seconds=0)

    assert blocked["acquired"] is False
    assert blocked["reason"] == "orphan_stash"
    assert acquire_main_repo_lock(repo, wait_seconds=0, check_stash=False)["acquired"]


def test_unusable_lock_path_is_reported_as_environment_not_as_held(
    tmp_path: Path,
) -> None:
    # A file where the .claude directory should be: mkdir cannot work here, and that must
    # never be read as "a sibling holds the lock" (which would wedge every future run).
    (tmp_path / ".claude").write_text("not a directory\n")

    result = acquire_main_repo_lock(tmp_path, wait_seconds=0)

    assert result["acquired"] is False
    assert result["reason"] == "environment"


# ---------------------------------------------------------------------------
# MERGE CONFLICT ELIGIBILITY
# ---------------------------------------------------------------------------


def test_pure_add_add_conflict_is_eligible(tmp_path: Path) -> None:
    repo = conflicting_repo(
        tmp_path,
        ours="a\nb\nc\ndef alpha():\n    return 1\n",
        theirs="a\nb\nc\ndef beta():\n    return 2\n",
    )

    verdict = classify_conflict(repo, ["shared.py"], test_command="pytest")

    assert verdict["eligible"] is True
    assert verdict["reason"] == "pure_add_add"


def test_conflict_is_never_eligible_without_a_suite_to_gate_it(tmp_path: Path) -> None:
    repo = conflicting_repo(
        tmp_path,
        ours="a\nb\nc\ndef alpha():\n    return 1\n",
        theirs="a\nb\nc\ndef beta():\n    return 2\n",
    )

    verdict = classify_conflict(repo, ["shared.py"], test_command="   ")

    assert verdict["eligible"] is False
    assert verdict["reason"] == "no_test_command"


def test_side_that_modified_existing_lines_is_not_insertion_only(
    tmp_path: Path,
) -> None:
    repo = conflicting_repo(
        tmp_path,
        ours="a\nB-CHANGED\nc\ndef alpha():\n    return 1\n",
        theirs="a\nb\nc\ndef beta():\n    return 2\n",
    )

    verdict = classify_conflict(repo, ["shared.py"], test_command="pytest")

    assert verdict["eligible"] is False
    assert "not_insertion_only" in verdict["reason"]


def test_both_sides_adding_the_same_identity_is_a_semantic_collision(
    tmp_path: Path,
) -> None:
    # Textually mergeable, semantically not: both stories register the same route.
    repo = conflicting_repo(
        tmp_path,
        ours='a\nb\nc\npath("relatorios/", relatorios_view, name="relatorios")\n',
        theirs='a\nb\nc\npath("relatorios/", outra_view, name="relatorios_v2")\n',
    )

    verdict = classify_conflict(repo, ["shared.py"], test_command="pytest")

    assert verdict["eligible"] is False
    assert "shared_identities" in verdict["reason"]


def test_add_add_of_a_brand_new_path_has_no_ancestor_and_is_never_eligible(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    git(repo, "checkout", "-b", "story/one")
    (repo / "0002_thing.py").write_text("migration one\n")
    git(repo, "add", "0002_thing.py")
    git(repo, "commit", "-m", "one")
    git(repo, "checkout", "main")
    git(repo, "checkout", "-b", "story/two")
    (repo / "0002_thing.py").write_text("migration two\n")
    git(repo, "add", "0002_thing.py")
    git(repo, "commit", "-m", "two")
    git(repo, "checkout", "main")
    git(repo, "merge", "--no-ff", "-m", "m1", "story/one")
    subprocess.run(
        ["git", "merge", "--no-ff", "-m", "m2", "story/two"],
        cwd=repo,
        capture_output=True,
        text=True,
        check=False,
    )

    verdict = classify_conflict(repo, ["0002_thing.py"], test_command="pytest")

    assert verdict["eligible"] is False
    assert "add_add_without_ancestor" in verdict["reason"]


def test_sprint_status_is_never_auto_reconciled(tmp_path: Path) -> None:
    repo = conflicting_repo(
        tmp_path,
        ours="a\nb\nc\nalpha: done\n",
        theirs="a\nb\nc\nbeta: done\n",
        filename="shared.py",
    )

    verdict = classify_conflict(
        repo, ["shared.py"], test_command="pytest", protected_paths=["shared.py"]
    )

    assert verdict["eligible"] is False
    assert "protected_path" in verdict["reason"]


@pytest.mark.parametrize(
    "path", ["poetry.lock", "package-lock.json", "node_modules/x.js"]
)
def test_generated_and_lock_files_are_never_eligible(tmp_path: Path, path: str) -> None:
    repo = init_repo(tmp_path / "repo")

    verdict = classify_conflict(repo, [path], test_command="pytest")

    assert verdict["eligible"] is False
    assert "generated_or_lock_file" in verdict["reason"]


# ---------------------------------------------------------------------------
# SUITE COVERAGE
# ---------------------------------------------------------------------------


def test_tests_delivered_outside_the_declared_scope_make_green_meaningless(
    tmp_path: Path,
) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "pyproject.toml").write_text(
        '[tool.pytest.ini_options]\ntestpaths = ["python/tests"]\n'
    )

    assert (
        suite_covers_paths(repo, ["apps/billing/tests/test_new.py"])["covers"] is False
    )
    assert suite_covers_paths(repo, ["python/tests/test_new.py"])["covers"] is True


def test_a_repo_without_declared_scope_is_unknown_not_false(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()

    assert suite_covers_paths(repo, ["anywhere/tests/test_x.py"])["covers"] is None


# ---------------------------------------------------------------------------
# GRAPH — cross-epic blockers and complexity tiers (build_waves, dry run)
# ---------------------------------------------------------------------------


def graph_state(repo: Path, graph_output: str, **overrides) -> dict:
    candidates = [
        {"key": "36-1-base", "title": "base", "status": "backlog", "epic_num": "36"},
        {
            "key": "36-2-consumidora",
            "title": "consome a base",
            "status": "backlog",
            "epic_num": "36",
        },
        {
            "key": "36-3-solta",
            "title": "independente",
            "status": "backlog",
            "epic_num": "36",
        },
    ]
    state = {
        "repo_path": str(repo),
        "epic_key": "epic-36",
        "epic_num": "36",
        "candidates": candidates,
        "stories": candidates,
        "sprint_status_path": "_bmad-output/implementation-artifacts/sprint-status.yaml",
        "implementation_artifacts_dir": "_bmad-output/implementation-artifacts",
        "target_branch": "main",
        "test_command": "pytest",
        "dry_run": True,
        "graph_output": {"content": graph_output},
    }
    state.update(overrides)
    return state


def test_omitted_dev_model_resolves_for_every_epic_wave_llm_call() -> None:
    spec = yaml.safe_load((WORKFLOWS / "bmad-epic-waves.yaml").read_text())
    expected = "{{ state.dev_model or ('claude-opus-5' if state.dev_harness == 'claude' else 'gpt-5.6-sol') }}"
    llm_models = [
        node["with"]["model"]
        for node in spec["nodes"]
        if node.get("uses") == "llm.call"
    ]

    assert llm_models
    assert set(llm_models) == {expected}
    prep = next(node for node in spec["nodes"] if node["name"] == "prep_create")
    assert prep["input"]["dev_model"] == expected


def test_review_harness_exposes_codex_default_and_claude_provider() -> None:
    epic = yaml.safe_load((WORKFLOWS / "bmad-epic-waves.yaml").read_text())
    cycle = yaml.safe_load((WORKFLOWS / "bmad-story-cycle.yaml").read_text())

    assert epic["state_schema"]["review_harness"] == "str"
    assert cycle["settings"]["shell_providers"]["claude_review"]["command"] == "claude"
    review = next(node for node in cycle["nodes"] if node["name"] == "code_review")
    assert "claude_review" in review["with"]["shell_provider"]
    assert "--model" in cycle["settings"]["shell_providers"]["claude_review"]["args"]


def test_graph_prompt_respects_literal_order_and_later_go_live_gates() -> None:
    spec = yaml.safe_load((WORKFLOWS / "bmad-epic-waves.yaml").read_text())
    graph = next(node for node in spec["nodes"] if node["name"] == "graph_llm")
    prompt = graph["with"]["messages"][0]["content"]

    assert "ordem literal de dependência ou serialização vence" in prompt
    assert "nunca crie a aresta de arquivo oposta" in prompt
    assert "evidência de go-live NÃO é dependência dura" in prompt
    assert "não emita `EXTERNAL_BLOCKER`" in prompt


def test_graph_provider_error_fails_closed_instead_of_building_zero_edges(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    state = graph_state(repo, "")
    state["graph_output"] = {
        "error": "Shell provider 'codex' requires a model parameter",
        "success": False,
    }

    with pytest.raises(RuntimeError, match="refusing to build an all-parallel plan"):
        run_node("bmad-epic-waves", "build_waves", state)


def test_empty_graph_provider_content_fails_closed(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")

    with pytest.raises(RuntimeError, match="returned empty content"):
        run_node("bmad-epic-waves", "build_waves", graph_state(repo, ""))


def test_cross_epic_blocker_skips_the_story_and_everything_downstream(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "EDGE: 36-1-base -> 36-2-consumidora [logic] EVIDENCE: consome o contrato\n"
        "EXTERNAL_BLOCKER: 36-1-base NEEDS 35-3-gate-do-tema STATUS in-progress "
        "EVIDENCE: Task 0 exige o gate do epic-35\n"
    )

    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    # A dependente cai junto: despachá-la produziria código contra um contrato inexistente.
    assert result["blocked_stories"] == ["36-1-base", "36-2-consumidora"]
    assert result["waves"] == [["36-3-solta"]]
    assert result["external_blockers"][0]["requires_key"] == "35-3-gate-do-tema"


def test_satisfied_cross_epic_dependency_does_not_block(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "EXTERNAL_BLOCKER: 36-1-base NEEDS 35-3-gate-do-tema STATUS done "
        "EVIDENCE: Task 0 exige o gate do epic-35\n"
    )

    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    assert result["blocked_stories"] == []


def test_blocker_naming_an_unresolvable_story_is_ignored_not_silently_applied(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    output = "EXTERNAL_BLOCKER: 99-nao-existe NEEDS 35-3-gate STATUS backlog EVIDENCE: ruído\n"

    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    assert result["blocked_stories"] == []
    assert result["external_blockers"] == []


def test_complexity_maps_each_story_to_its_tier_provider(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base [standard] contrato novo consumido por outra story\n"
        "RISK_FLAGS: 36-1-base [none] sem risco adicional\n"
        "COMPLEXITY: 36-3-solta [trivial] troca de constante\n"
        "RISK_FLAGS: 36-3-solta [none] sem risco adicional\n"
    )

    result = run_node(
        "bmad-epic-waves", "build_waves", graph_state(repo, output, auto_model=True)
    )

    assert result["story_tiers"] == {"36-1-base": "standard", "36-3-solta": "trivial"}
    assert result["story_routes"] == {"36-1-base": "standard", "36-3-solta": "trivial"}
    assert result["tier_providers"]["trivial"] == "codex_trivial"
    assert result["dot_payloads"]["36-1-base"]["dev_provider"] == "codex_standard"
    assert result["dot_payloads"]["36-1-base"]["fix_provider"] == "codex_high_risk"
    # A story que o classificador omitiu cai no default, e isso não é erro.
    assert "36-2-consumidora" not in result["story_tiers"]
    assert result["dot_payloads"]["36-2-consumidora"]["dev_provider"] == "codex"
    assert result["dot_payloads"]["36-2-consumidora"]["fix_provider"] == "codex_frontier"


def test_complexity_accepts_valid_bare_tier_from_shell_provider(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base frontier contrato transversal\n"
        "RISK_FLAGS: 36-1-base [none] sem risco adicional\n"
    )

    result = run_node(
        "bmad-epic-waves", "build_waves", graph_state(repo, output, auto_model=True)
    )

    assert result["story_tiers"] == {"36-1-base": "frontier"}
    assert result["story_routes"] == {"36-1-base": "frontier"}
    assert result["dot_payloads"]["36-1-base"]["dev_provider"] == "codex_frontier"


def test_complexity_is_ignored_when_auto_model_is_off(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = "COMPLEXITY: 36-1-base [trivial] qualquer coisa\n"

    result = run_node(
        "bmad-epic-waves",
        "build_waves",
        graph_state(repo, output, auto_model=False),
    )

    assert result["story_tiers"] == {}
    assert result["story_routes"] == {}


def test_explicit_dependencies_bypass_classification_and_keep_sol_xhigh_fallback(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    result = run_node(
        "bmad-epic-waves",
        "build_waves",
        graph_state(
            repo,
            "COMPLEXITY: 36-1-base [trivial] deve ser ignorada\n",
            deps={"36-2-consumidora": ["36-1-base"]},
        ),
    )

    assert result["story_tiers"] == {}
    assert result["story_routes"] == {}
    assert all(
        payload["dev_provider"] == "codex"
        for payload in result["dot_payloads"].values()
    )


def test_balanced_routing_is_default_and_risk_flags_only_raise_routes(
    tmp_path: Path,
) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base [trivial] edição curta\n"
        "RISK_FLAGS: 36-1-base [public_contract] muda contrato público\n"
        "COMPLEXITY: 36-2-consumidora [small] mudança localizada\n"
        "RISK_FLAGS: 36-2-consumidora [large_context] repo muito grande\n"
        "COMPLEXITY: 36-3-solta [standard] fluxo convencional\n"
        "RISK_FLAGS: 36-3-solta [schema_or_migration] altera schema\n"
    )

    # auto_model omitido: a política balanceada é o novo default.
    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    assert result["story_routes"] == {
        "36-1-base": "high_risk",
        "36-2-consumidora": "transversal",
        "36-3-solta": "frontier",
    }
    assert result["story_risk_flags"]["36-1-base"] == ["public_contract"]
    assert result["dot_payloads"]["36-1-base"]["dev_provider"] == "codex_high_risk"
    assert result["dot_payloads"]["36-2-consumidora"]["dev_provider"] == "codex_transversal"
    assert result["dot_payloads"]["36-3-solta"]["dev_provider"] == "codex_frontier"


def test_unknown_risk_flag_fails_safe_to_frontier(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base [trivial] edição curta\n"
        "RISK_FLAGS: 36-1-base [quantum_contract] flag fora do contrato\n"
    )

    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    assert result["story_routes"]["36-1-base"] == "frontier"
    assert result["story_risk_flags"]["36-1-base"] == ["unknown:quantum_contract"]
    assert result["dot_payloads"]["36-1-base"]["dev_provider"] == "codex_frontier"


def test_missing_or_malformed_risk_line_fails_safe_to_frontier(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base [trivial] edição curta\n"
        "RISK_FLAGS: 36-1-base sem-colchetes\n"
    )

    result = run_node("bmad-epic-waves", "build_waves", graph_state(repo, output))

    assert result["story_routes"]["36-1-base"] == "frontier"
    assert result["story_risk_flags"]["36-1-base"] == ["missing:risk_flags"]


def test_story_cycle_providers_match_the_balanced_policy() -> None:
    spec = yaml.safe_load((WORKFLOWS / "bmad-story-cycle.yaml").read_text())
    providers = spec["settings"]["shell_providers"]

    def args(name: str) -> list[str]:
        return providers[name]["args"]

    assert "model=gpt-5.6-luna" in args("codex_trivial")
    assert "model_reasoning_effort=low" in args("codex_trivial")
    assert "model=gpt-5.6-terra" in args("codex_small")
    assert "model_reasoning_effort=medium" in args("codex_small")
    assert "model=gpt-5.6-terra" in args("codex_standard")
    assert "model_reasoning_effort=high" in args("codex_standard")
    assert "model=gpt-5.6-terra" in args("codex_transversal")
    assert "model_reasoning_effort=xhigh" in args("codex_transversal")
    assert "model=gpt-5.6-sol" in args("codex_high_risk")
    assert "model_reasoning_effort=high" in args("codex_high_risk")
    assert "model=gpt-5.6-sol" in args("codex_frontier")
    assert "model_reasoning_effort=xhigh" in args("codex_frontier")
    assert "model={model}" in args("codex_review")
    assert "model_reasoning_effort=xhigh" in args("codex_review")
    assert "model=gpt-5.6-luna" in args("codex_finish")
    assert "model_reasoning_effort=low" in args("codex_finish")


def test_duplicate_complexity_entry_keeps_the_first_and_reports(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    output = (
        "COMPLEXITY: 36-1-base [trivial] primeira\n"
        "COMPLEXITY: 36-1-base [frontier] segunda\n"
    )

    result = run_node(
        "bmad-epic-waves", "build_waves", graph_state(repo, output, auto_model=True)
    )

    assert result["story_tiers"]["36-1-base"] == "trivial"


# ---------------------------------------------------------------------------
# STORY CYCLE — dev retry and verification-incomplete
# ---------------------------------------------------------------------------


def test_dev_that_died_without_a_report_is_retried_once() -> None:
    first = run_node("bmad-story-cycle", "parse_dev", {"dev_output": {"content": "  "}})
    assert first["_dev_next"] == "retry"
    assert first["dev_retried"] is True

    second = run_node(
        "bmad-story-cycle",
        "parse_dev",
        {"dev_output": {"content": "  "}, "dev_retried": True},
    )
    assert second["_dev_next"] == "review"


def test_a_dev_that_reported_a_halt_is_not_retried() -> None:
    halt = "HALT: falta a migration do epic-35, não dá para seguir sem ela nesta story."

    result = run_node(
        "bmad-story-cycle", "parse_dev", {"dev_output": {"content": halt}}
    )

    assert result["_dev_next"] == "review"


def test_verification_incomplete_is_read_from_the_explicit_marker_only() -> None:
    reported = run_node(
        "bmad-story-cycle",
        "parse_dev",
        {"dev_output": {"content": "DEV_DONE\nVERIFICATION_INCOMPLETE: true\n"}},
    )
    assert reported["verification_incomplete"] is True

    quoted = run_node(
        "bmad-story-cycle",
        "parse_dev",
        {
            "dev_output": {
                "content": "DEV_DONE\nrodei tudo; nada de VERIFICATION_INCOMPLETE aqui\n"
            }
        },
    )
    assert quoted["verification_incomplete"] is False


def test_dead_reviewer_is_retried_once_instead_of_counting_as_changes_requested() -> (
    None
):
    first = run_node(
        "bmad-story-cycle", "check_review_status", {"review_output": {"content": ""}}
    )
    assert first["review_status"] == "RETRY"

    second = run_node(
        "bmad-story-cycle",
        "check_review_status",
        {"review_output": {"content": ""}, "review_retried": True},
    )
    assert second["review_status"] == "CHANGES_REQUESTED"


def test_first_changes_requested_promotes_the_fix_provider() -> None:
    result = run_node(
        "bmad-story-cycle",
        "check_review_status",
        {
            "review_output": {
                "content": "FINAL_STATUS: CHANGES_REQUESTED\nFIX_ESCALATION: PROMOTE"
            },
            "fix_provider": "codex_standard",
            "frontier_provider": "codex_frontier",
            "max_review_cycles": 3,
        },
    )

    assert result["review_fix_attempt"] == 1
    assert result["fix_escalation"] == "PROMOTE"
    assert result["active_fix_provider"] == "codex_standard"


def test_structural_finding_jumps_directly_to_frontier_fix() -> None:
    result = run_node(
        "bmad-story-cycle",
        "check_review_status",
        {
            "review_output": {
                "content": "FINAL_STATUS: CHANGES_REQUESTED\nFIX_ESCALATION: FRONTIER"
            },
            "fix_provider": "codex_standard",
            "frontier_provider": "codex_frontier",
            "max_review_cycles": 3,
        },
    )

    assert result["fix_escalation"] == "FRONTIER"
    assert result["active_fix_provider"] == "codex_frontier"


def test_second_changes_requested_forces_frontier_even_without_marker() -> None:
    result = run_node(
        "bmad-story-cycle",
        "check_review_status",
        {
            "review_output": {"content": "FINAL_STATUS: CHANGES_REQUESTED"},
            "review_fix_attempt": 1,
            "fix_provider": "codex_standard",
            "frontier_provider": "codex_frontier",
            "max_review_cycles": 3,
        },
    )

    assert result["review_fix_attempt"] == 2
    assert result["fix_escalation"] == "FRONTIER"
    assert result["active_fix_provider"] == "codex_frontier"


def test_nonapproved_review_skips_the_finish_llm() -> None:
    spec = yaml.safe_load((WORKFLOWS / "bmad-story-cycle.yaml").read_text())
    node = next(node for node in spec["nodes"] if node["name"] == "check_review_status")
    blocked_route = next(
        route
        for route in node["goto"]
        if "BLOCKED" in route.get("if", "") and "in_worktree" in route.get("if", "")
    )

    assert blocked_route["to"] == "summary"


def test_exhausted_cycles_end_the_story_without_an_extra_confirmation_review() -> None:
    result = run_node(
        "bmad-story-cycle",
        "check_review_status",
        {
            "review_output": {"content": "FINAL_STATUS: CHANGES_REQUESTED"},
            "review_fix_attempt": 2,
            "max_review_cycles": 3,
        },
    )

    assert result["review_status"] == "MAX_ATTEMPTS"
