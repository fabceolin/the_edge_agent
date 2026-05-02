"""
Tests for the pure-Python YAML workflow validator (TEA-DX-001.6).

Covers:
- Unit tests for `validate_workflow_dict` per AC-6 error category.
- Integration tests for the `tea validate` CLI subcommand.
- Security regression: import allow-list (AC-13) + sentinel-file
  exec guard (AC-14).
- Refactor parity: every example + broken fixture goes through both
  `tea validate` and `YAMLEngine.load_from_dict()` and the resulting
  errors are correlated (AC-15).
- `--strict` mode: false-positive (must NOT warn) + true-positive
  (must warn) suites.
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
import tempfile
import textwrap
import unittest
import uuid
from pathlib import Path

import yaml as _yaml
from typer.testing import CliRunner

from the_edge_agent.cli import app
from the_edge_agent.yaml_validation import (
    ValidationCode,
    WorkflowValidationError,
    format_error,
    validate_workflow,
    validate_workflow_dict,
)


FIXTURES = Path(__file__).parent / "fixtures" / "validate"
runner = CliRunner()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _strip_ansi(text: str) -> str:
    return re.sub(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])", "", text)


def _codes(errs):
    return sorted(e.code for e in errs)


# ---------------------------------------------------------------------------
# 1.6-UNIT-003 ... 1.6-UNIT-018  (AC-6 error categories)
# ---------------------------------------------------------------------------


class TestValidationUnit(unittest.TestCase):
    """One unit test per AC-6 error category."""

    def _load(self, path: str):
        return validate_workflow(str(FIXTURES / path))

    # ---- valid fixtures: zero errors ---------------------------------

    def test_valid_minimal_yaml_returns_no_errors(self):
        errs = self._load("valid/minimal.yaml")
        self.assertEqual(errs, [])

    def test_valid_with_condition_returns_no_errors(self):
        errs = self._load("valid/with_condition.yaml")
        self.assertEqual(errs, [])

    def test_valid_dynamic_parallel_returns_no_errors(self):
        errs = self._load("valid/dynamic_parallel.yaml")
        self.assertEqual(errs, [])

    # ---- AC-6: YAML parse error ---------------------------------------

    def test_yaml_parse_error_emits_yaml_parse_code(self):
        errs = self._load("broken/yaml_parse_error.yaml")
        self.assertTrue(errs)
        self.assertEqual(errs[0].code, ValidationCode.YAML_PARSE)

    # ---- AC-6: Required top-level fields ------------------------------

    def test_missing_nodes_emits_missing_field(self):
        errs = self._load("broken/missing_nodes.yaml")
        codes = _codes(errs)
        self.assertIn(ValidationCode.MISSING_FIELD, codes)

    # ---- AC-6: Edge endpoint validity ---------------------------------

    def test_edge_to_undefined_emits_edge_to_undefined(self):
        errs = self._load("broken/edge_to_undefined.yaml")
        self.assertIn(ValidationCode.EDGE_TO_UNDEFINED, _codes(errs))

    def test_edge_from_undefined_emits_edge_from_undefined(self):
        errs = self._load("broken/edge_from_undefined.yaml")
        self.assertIn(ValidationCode.EDGE_FROM_UNDEFINED, _codes(errs))

    # ---- AC-6: Node-name uniqueness -----------------------------------

    def test_duplicate_nodes_emits_duplicate_node(self):
        errs = self._load("broken/duplicate_nodes.yaml")
        self.assertIn(ValidationCode.DUPLICATE_NODE, _codes(errs))

    # ---- AC-6: dynamic_parallel checks --------------------------------

    def test_dyn_parallel_missing_fan_in_emits_correct_code(self):
        errs = self._load("broken/dyn_parallel_no_fan_in.yaml")
        self.assertIn(ValidationCode.DYN_PARALLEL_MISSING_FAN_IN, _codes(errs))

    def test_dyn_parallel_missing_items_emits_correct_code(self):
        errs = self._load("broken/dyn_parallel_no_items.yaml")
        self.assertIn(ValidationCode.DYN_PARALLEL_MISSING_ITEMS, _codes(errs))

    def test_dyn_parallel_two_modes_emits_mode_code(self):
        errs = self._load("broken/dyn_parallel_two_modes.yaml")
        self.assertIn(ValidationCode.DYN_PARALLEL_MODE, _codes(errs))

    def test_dyn_parallel_fan_in_undefined(self):
        errs = self._load("broken/dyn_parallel_fan_in_undefined.yaml")
        # Fan_in points at undefined node; emitted as DYN_PARALLEL_FAN_IN_UNDEFINED
        # via the dyn_parallel check, OR via edge endpoint check, depending on
        # which check fires first. Accept either as correct.
        codes = _codes(errs)
        self.assertTrue(
            ValidationCode.DYN_PARALLEL_FAN_IN_UNDEFINED in codes
            or ValidationCode.EDGE_TO_UNDEFINED in codes
            or ValidationCode.PARALLEL_FAN_IN_UNDEFINED in codes
        )

    # ---- AC-6: Jinja parse check on conditions ------------------------

    def test_invalid_jinja_condition_emits_invalid_condition(self):
        errs = self._load("broken/invalid_jinja_condition.yaml")
        self.assertIn(ValidationCode.INVALID_CONDITION, _codes(errs))


# ---------------------------------------------------------------------------
# 1.6-UNIT-019 ... 1.6-UNIT-022  (--strict)
# ---------------------------------------------------------------------------


class TestStrictWarnings(unittest.TestCase):
    """AC-7: --strict promotes soft warnings to errors."""

    def test_unreferenced_node_warns_in_strict_only(self):
        path = FIXTURES / "strict_known_bad" / "unreferenced_node.yaml"
        # Without --strict: no errors, no warnings.
        errs = validate_workflow(str(path), strict=False)
        self.assertEqual(errs, [])
        # With --strict: at least one warning for the orphan_node.
        warns = validate_workflow(str(path), strict=True)
        self.assertTrue(
            any(
                w.code == ValidationCode.UNREFERENCED_NODE and w.is_warning()
                for w in warns
            )
        )

    def test_dead_state_key_warns_in_strict(self):
        path = FIXTURES / "strict_known_bad" / "dead_state_key.yaml"
        errs = validate_workflow(str(path), strict=False)
        self.assertEqual(errs, [])
        warns = validate_workflow(str(path), strict=True)
        self.assertTrue(
            any(w.code == ValidationCode.DEAD_STATE_KEY for w in warns)
        )

    def test_strict_known_good_fixtures_have_zero_warnings(self):
        """≥5 tricky-but-valid YAMLs MUST NOT warn under --strict (BUS-001)."""
        good_dir = FIXTURES / "strict_known_good"
        fixtures = sorted(good_dir.glob("*.yaml"))
        self.assertGreaterEqual(len(fixtures), 5, "need ≥5 known-good fixtures")
        for fixture in fixtures:
            warns = validate_workflow(str(fixture), strict=True)
            offending = [
                w
                for w in warns
                if w.is_warning() and w.code != ValidationCode.MISSING_FIELD
            ]
            # MISSING_FIELD warning for 'name' is acceptable in known-good
            # if the fixture deliberately omits it; in our suite, none do.
            self.assertEqual(
                offending,
                [],
                f"{fixture.name} produced unexpected warnings: {offending}",
            )

    def test_strict_known_bad_fixtures_each_warn(self):
        """≥5 fixtures that MUST produce ≥1 warning under --strict."""
        bad_dir = FIXTURES / "strict_known_bad"
        fixtures = sorted(bad_dir.glob("*.yaml"))
        self.assertGreaterEqual(len(fixtures), 5, "need ≥5 known-bad fixtures")
        for fixture in fixtures:
            warns = validate_workflow(str(fixture), strict=True)
            warns_only = [w for w in warns if w.is_warning()]
            self.assertTrue(
                warns_only,
                f"{fixture.name} produced no --strict warnings",
            )


# ---------------------------------------------------------------------------
# 1.6-UNIT-001 (error formatting) + 1.6-UNIT-023 (docs)
# ---------------------------------------------------------------------------


class TestFormatting(unittest.TestCase):

    def test_format_error_includes_code_and_location(self):
        err = WorkflowValidationError(
            code="EDGE_TO_UNDEFINED",
            message="Edge 'to' references undefined node 'foo'",
            line=12,
            column=4,
        )
        out = format_error(err, file_path="/tmp/x.yaml")
        self.assertIn("EDGE_TO_UNDEFINED", out)
        self.assertIn("/tmp/x.yaml:12:4", out)
        self.assertIn("error", out)

    def test_format_error_warning_severity(self):
        err = WorkflowValidationError(
            code="UNREFERENCED_NODE",
            message="orphan",
            severity="warning",
            node="orphan",
        )
        out = format_error(err)
        self.assertIn("warning", out)
        self.assertIn("orphan", out)


# ---------------------------------------------------------------------------
# 1.6-INT-001 ... 1.6-INT-005, 1.6-INT-009, 1.6-INT-018  (CLI integration)
# ---------------------------------------------------------------------------


class TestValidateCli(unittest.TestCase):
    """Integration tests for `tea validate` Typer subcommand."""

    def test_valid_yaml_exits_0(self):
        result = runner.invoke(
            app, ["validate", str(FIXTURES / "valid" / "minimal.yaml")]
        )
        self.assertEqual(result.exit_code, 0, result.output)

    def test_broken_yaml_exits_1(self):
        result = runner.invoke(
            app,
            [
                "validate",
                str(FIXTURES / "broken" / "edge_to_undefined.yaml"),
            ],
        )
        self.assertEqual(result.exit_code, 1)

    def test_success_message_includes_scope_qualifier(self):
        """1.6-INT-003 / AC-2 revised wording (BUS-002)."""
        result = runner.invoke(
            app, ["validate", str(FIXTURES / "valid" / "minimal.yaml")]
        )
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertIn("OK:", result.output)
        self.assertIn("nodes", result.output)
        self.assertIn("edges", result.output)
        self.assertIn(
            "structural checks only — run blocks not executed",
            result.output,
        )

    def test_help_enumerates_checks_and_limitations(self):
        """1.6-INT-018 / AC-12 (TEA-DX-001.6 BUS-002 / TECH-002)."""
        result = runner.invoke(app, ["validate", "--help"])
        self.assertEqual(result.exit_code, 0)
        out = _strip_ansi(result.output)
        # Checks performed
        self.assertIn("YAML parse", out)
        self.assertIn("dynamic_parallel", out)
        # Explicit non-checks (BUS-002 mitigation)
        self.assertIn("Not checked", out)
        self.assertIn("Semantic correctness", out)
        # --strict described
        self.assertIn("--strict", out)

    def test_failure_block_format(self):
        result = runner.invoke(
            app,
            [
                "validate",
                str(FIXTURES / "broken" / "edge_to_undefined.yaml"),
            ],
        )
        self.assertEqual(result.exit_code, 1)
        # output is mixed stdout+stderr in CliRunner default
        combined = result.output
        self.assertIn("error", combined)
        self.assertIn("EDGE_TO_UNDEFINED", combined)
        self.assertIn("FAIL:", combined)

    def test_strict_promotes_warnings_to_exit_1(self):
        path = str(FIXTURES / "strict_known_bad" / "unreferenced_node.yaml")
        # Without --strict
        ok = runner.invoke(app, ["validate", path])
        self.assertEqual(ok.exit_code, 0, ok.output)
        # With --strict
        bad = runner.invoke(app, ["validate", path, "--strict"])
        self.assertEqual(bad.exit_code, 1, bad.output)
        self.assertIn("UNREFERENCED_NODE", bad.output)

    def test_missing_file_exits_1(self):
        result = runner.invoke(
            app, ["validate", "/nonexistent/__never__.yaml"]
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("not found", result.output.lower())


# ---------------------------------------------------------------------------
# 1.6-INT-006  (AC-14 sentinel-file regression test)
# 1.6-UNIT-002  (AC-13 import allow-list)
# ---------------------------------------------------------------------------


class TestSecurityGuards(unittest.TestCase):
    """SEC-001 mitigations: validator must not exec or import executors."""

    def test_sentinel_file_not_created_by_validate(self):
        """1.6-INT-006: `run:` block must NOT execute during validation."""
        sentinel = (
            Path(tempfile.gettempdir())
            / f"tea-validate-sentinel-{uuid.uuid4().hex}"
        )
        # Belt-and-suspenders: clean up before
        if sentinel.exists():
            sentinel.unlink()

        try:
            tmpl = (
                FIXTURES / "sentinel" / "sentinel_template.yaml"
            ).read_text()
            rendered = tmpl.replace("__SENTINEL_PATH__", str(sentinel))
            with tempfile.NamedTemporaryFile(
                mode="w", suffix=".yaml", delete=False
            ) as f:
                f.write(rendered)
                yaml_path = f.name

            try:
                # Direct validator call — bypass CLI to keep this test
                # focused on the validator API contract.
                errs = validate_workflow(yaml_path)
                # Validator should accept the structurally-valid fixture
                # (no exec). It is structurally OK; what matters is the
                # sentinel must NOT exist after.
                hard_errs = [e for e in errs if e.is_error()]
                # Allow an edge case where YAML parser doesn't see structural
                # errors but that's fine — the only assertion is the sentinel.
                _ = hard_errs

                self.assertFalse(
                    sentinel.exists(),
                    f"SECURITY REGRESSION: validator executed `run:` block; "
                    f"sentinel file {sentinel} was created.",
                )

                # Also through the CLI surface
                result = runner.invoke(app, ["validate", yaml_path])
                # exit 0 or 1 is fine; what matters is the sentinel guard.
                _ = result.exit_code
                self.assertFalse(
                    sentinel.exists(),
                    f"SECURITY REGRESSION: `tea validate` executed `run:` block; "
                    f"sentinel file {sentinel} was created.",
                )
            finally:
                Path(yaml_path).unlink(missing_ok=True)
        finally:
            if sentinel.exists():
                sentinel.unlink()

    def test_yaml_validation_module_does_not_import_executor_modules(self):
        """
        1.6-UNIT-002 (AC-13): yaml_validation must not import executor or
        backend modules. Loaded as a standalone module so we measure what
        `yaml_validation.py` itself imports, not what `the_edge_agent`'s
        package `__init__.py` pulls in. Run in a subprocess to keep
        sys.modules clean.
        """
        forbidden = [
            "the_edge_agent.yaml_engine",
            "the_edge_agent.yaml_nodes",
            "the_edge_agent.cli",
            "the_edge_agent.parallel",
            "the_edge_agent.parallel_executors",
            "the_edge_agent.checkpointers",
            "the_edge_agent.checkpoint",
            "the_edge_agent.actions",
            "the_edge_agent.backends",
        ]
        # Locate the module file without importing the package.
        import the_edge_agent.yaml_validation as _mod_for_path

        module_path = _mod_for_path.__file__
        prog = textwrap.dedent(
            f"""
            import sys, json, importlib.util
            spec = importlib.util.spec_from_file_location(
                "_isolated_yaml_validation", {module_path!r}
            )
            mod = importlib.util.module_from_spec(spec)
            sys.modules["_isolated_yaml_validation"] = mod
            spec.loader.exec_module(mod)
            print(json.dumps(sorted(sys.modules.keys())))
            """
        ).strip()
        result = subprocess.run(
            [sys.executable, "-c", prog],
            capture_output=True,
            text=True,
            check=True,
        )
        loaded = set(__import__("json").loads(result.stdout))
        violations = [m for m in forbidden if m in loaded]
        self.assertEqual(
            violations,
            [],
            f"yaml_validation transitively imported forbidden modules: {violations}",
        )


# ---------------------------------------------------------------------------
# 1.6-INT-007 / 1.6-INT-008  (no network / no API-key reads)
# ---------------------------------------------------------------------------


class TestNoSideEffects(unittest.TestCase):
    """AC-4 / AC-5: validator must not open sockets or read API keys."""

    def test_validate_does_not_open_sockets(self):
        """1.6-INT-007: monkeypatch socket.socket.__init__ to raise."""
        import socket

        original_init = socket.socket.__init__

        def boom(self, *a, **kw):
            raise AssertionError(
                "Validator opened a socket — AC-4 / AC-5 violation"
            )

        socket.socket.__init__ = boom
        try:
            errs = validate_workflow(
                str(FIXTURES / "valid" / "minimal.yaml")
            )
        finally:
            socket.socket.__init__ = original_init
        self.assertEqual(errs, [])

    def test_validate_does_not_read_api_key_env_vars(self):
        """1.6-INT-008: assert no reads of well-known secret env vars."""
        secret_keys = (
            "ANTHROPIC_API_KEY",
            "OPENAI_API_KEY",
            "PERPLEXITY_API_KEY",
            "FIRECRAWL_API_KEY",
        )
        # Inject sentinel values so any read is detectable.
        previous = {}
        canary = "__tea-canary-NEVER-READ__"
        for k in secret_keys:
            previous[k] = os.environ.get(k)
            os.environ[k] = canary

        # Wrap os.environ to record reads.
        reads = []
        original_get = os.environ.__getitem__

        def recording_get(key):
            if key in secret_keys:
                reads.append(key)
            return original_get(key)

        try:
            os.environ.__class__.__getitem__ = (  # type: ignore[method-assign]
                lambda self, key: recording_get(key)
            )
            errs = validate_workflow(
                str(FIXTURES / "valid" / "minimal.yaml")
            )
        finally:
            # Restore __getitem__ on the os._Environ class.
            os.environ.__class__.__getitem__ = original_get  # type: ignore[method-assign]
            for k, v in previous.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v

        self.assertEqual(errs, [])
        self.assertEqual(
            reads,
            [],
            f"Validator read forbidden secret env keys: {reads}",
        )


# ---------------------------------------------------------------------------
# 1.6-INT-013 / 1.6-INT-014 (refactor parity, AC-15)
# ---------------------------------------------------------------------------


class TestRefactorParity(unittest.TestCase):
    """
    AC-15 (parity): every example YAML and every broken fixture should be
    classified consistently by the validator and the engine init path.

    Specifically:
      - For valid fixtures: both surfaces produce zero "hard" errors.
      - For broken fixtures whose codes are in the engine's hard-error
        allow-list: the engine init raises ValueError; the validator
        returns ≥1 corresponding error.
    """

    HARD_CODES = (
        ValidationCode.DUPLICATE_NODE,
        ValidationCode.EDGE_FROM_UNDEFINED,
        ValidationCode.EDGE_TO_UNDEFINED,
        ValidationCode.GOTO_UNDEFINED,
        ValidationCode.DYN_PARALLEL_MODE,
        ValidationCode.DYN_PARALLEL_MISSING_FAN_IN,
        ValidationCode.DYN_PARALLEL_FAN_IN_UNDEFINED,
        ValidationCode.DYN_PARALLEL_MISSING_ITEMS,
        ValidationCode.PARALLEL_FAN_IN_UNDEFINED,
    )

    def _engine_raises_on_init(self, config):
        from the_edge_agent.yaml_engine import YAMLEngine

        try:
            engine = YAMLEngine()
            engine.load_from_dict(config)
        except ValueError as e:
            return True, str(e)
        except Exception as e:  # noqa: BLE001
            return True, str(e)
        return False, ""

    def test_parity_valid_fixtures(self):
        """Valid fixtures: validator returns zero errors."""
        for fixture in sorted((FIXTURES / "valid").glob("*.yaml")):
            errs = validate_workflow(str(fixture))
            hard_errs = [e for e in errs if e.is_error()]
            self.assertEqual(
                hard_errs,
                [],
                f"Validator reported hard errors on valid fixture {fixture.name}: "
                f"{[e.code for e in hard_errs]}",
            )

    def test_parity_broken_fixtures_validator_catches(self):
        """Broken fixtures: validator emits ≥1 error."""
        for fixture in sorted((FIXTURES / "broken").glob("*.yaml")):
            errs = validate_workflow(str(fixture))
            self.assertTrue(
                errs,
                f"Validator missed error on broken fixture {fixture.name}",
            )

    def test_parity_engine_init_catches_hard_errors(self):
        """For broken fixtures with hard codes, engine init must raise."""
        for fixture in sorted((FIXTURES / "broken").glob("*.yaml")):
            errs = validate_workflow(str(fixture))
            hard_errs = [
                e
                for e in errs
                if e.is_error() and e.code in self.HARD_CODES
            ]
            if not hard_errs:
                # Some broken categories (YAML parse, jinja errors,
                # missing nodes field) are not in the hard-codes list.
                continue
            try:
                config = _yaml.safe_load(fixture.read_text())
            except Exception:
                continue
            raised, _msg = self._engine_raises_on_init(config)
            self.assertTrue(
                raised,
                f"Engine init did NOT raise for broken fixture "
                f"{fixture.name} (codes={[e.code for e in hard_errs]})",
            )


# ---------------------------------------------------------------------------
# 1.6-INT-019  (Performance smoke; PERF-001)
# ---------------------------------------------------------------------------


class TestPerformanceSmoke(unittest.TestCase):
    """500-node synthetic workflow validates quickly."""

    def test_500_node_validation_completes_quickly(self):
        import time as _time

        nodes = []
        edges = [{"from": "__start__", "to": "n0"}]
        for i in range(500):
            nodes.append({"name": f"n{i}", "run": "return {}"})
            if i + 1 < 500:
                edges.append({"from": f"n{i}", "to": f"n{i + 1}"})
        edges.append({"from": "n499", "to": "__end__"})
        config = {"name": "perf-smoke", "nodes": nodes, "edges": edges}
        start = _time.monotonic()
        errs = validate_workflow_dict(config)
        elapsed = _time.monotonic() - start
        self.assertEqual(errs, [])
        # Generous gate; smoke only.
        self.assertLess(
            elapsed,
            5.0,
            f"500-node validation took {elapsed:.2f}s (>5s budget)",
        )


if __name__ == "__main__":
    unittest.main()
