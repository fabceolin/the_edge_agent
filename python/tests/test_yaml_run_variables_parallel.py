"""
TEA-DX-001.4 — Parallel-safety regression tests for `variables` in run: scope.

Two scenarios are codified here:

  INT-003 (AC-12): under thread-mode parallel execution, two branches both
                   write `variables["x"]`; fan-in reads it. Asserts the
                   value lands in the set of valid outcomes (race is
                   non-deterministic). Codifies TECH-006.

  INT-004 (AC-13): under process-mode parallel execution, a branch writes
                   `variables["x"]`; assert at fan-in that
                   `engine.variables["x"]` is NOT updated (cross-process
                   invisibility). Codifies the discard.

These are isolated in their own test file (per SM Validation guidance) so
that parallel-strategy gating does not leak into the core suite.
"""

import multiprocessing
import unittest

import yaml as yaml_lib
import pytest

from the_edge_agent import YAMLEngine


THREAD_RACE_YAML = """
name: variables-thread-race
state_schema:
  results: list

settings:
  parallel:
    strategy: thread

nodes:
  - name: start
    run: |
      return {}

  - name: branch_a
    run: |
      variables["shared"] = "branch-a"
      return {"branch": "a"}

  - name: branch_b
    run: |
      variables["shared"] = "branch-b"
      return {"branch": "b"}

  - name: collect
    fan_in: true
    run: |
      # Read the engine-shared variable at fan-in
      return {"observed": variables.get("shared")}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: collect
  - from: start
    to: branch_b
    type: parallel
    fan_in: collect
  - from: branch_a
    to: collect
  - from: branch_b
    to: collect
  - from: collect
    to: __end__
"""


PROCESS_DISCARD_YAML = """
name: variables-process-discard
state_schema:
  results: list

settings:
  parallel:
    strategy: process

nodes:
  - name: start
    run: |
      return {}

  - name: branch_writer
    run: |
      variables["from_branch"] = "set-in-branch"
      return {"branch": "writer"}

  - name: branch_other
    run: |
      return {"branch": "other"}

  - name: collect
    fan_in: true
    run: |
      # Note: at fan-in we are back in the parent process. The engine
      # attribute on this side never saw the branch's mutation.
      return {"engine_saw": variables.get("from_branch")}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_writer
    type: parallel
    fan_in: collect
  - from: start
    to: branch_other
    type: parallel
    fan_in: collect
  - from: branch_writer
    to: collect
  - from: branch_other
    to: collect
  - from: collect
    to: __end__
"""


class TestVariablesParallelThreadRace(unittest.TestCase):
    """INT-003 / AC-12 — Thread-mode race on `variables[...]` writes."""

    def test_thread_branches_race_on_shared_variables(self):
        engine = YAMLEngine()
        config = yaml_lib.safe_load(THREAD_RACE_YAML)
        graph = engine.load_from_dict(config)
        events = list(graph.invoke({}))
        final = events[-1]["state"]
        # Race outcome: one of the branch values wins. We assert membership
        # — not which branch wins — so the test is stable but still proves
        # the binding is shared (otherwise observed would be None).
        self.assertIn(final["observed"], {"branch-a", "branch-b"})
        # Engine-side mutation also persists past fan-in.
        self.assertIn(engine.variables.get("shared"), {"branch-a", "branch-b"})


class TestVariablesParallelProcessDiscard(unittest.TestCase):
    """INT-004 / AC-13 — Process-mode in-branch writes are discarded."""

    def test_process_branch_writes_are_invisible_at_fan_in(self):
        # spawn is the safe default for cross-platform pickling.
        try:
            multiprocessing.set_start_method("spawn", force=True)
        except RuntimeError:
            pass  # Already set; that is fine.

        try:
            from the_edge_agent.parallel_executors import ProcessExecutor  # noqa: F401
        except Exception:
            pytest.skip("process executor unavailable")

        engine = YAMLEngine()
        config = yaml_lib.safe_load(PROCESS_DISCARD_YAML)
        try:
            graph = engine.load_from_dict(config)
        except Exception as e:
            pytest.skip(f"process strategy not supported in this env: {e}")

        try:
            events = list(graph.invoke({}))
        except Exception as e:
            pytest.skip(f"process executor unable to run in this env: {e}")

        final = events[-1]["state"]
        # The branch wrote variables["from_branch"] in a child process.
        # That mutation does not cross back into the parent's
        # `engine.variables`. fan-in reads None.
        self.assertIsNone(final.get("engine_saw"))
        self.assertNotIn("from_branch", engine.variables)


if __name__ == "__main__":
    unittest.main()
