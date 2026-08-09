"""Regression tests for security-sensitive dependency pins."""

import ast
from collections import Counter
from importlib import import_module
from importlib.metadata import version as installed_version
from pathlib import Path
import re

import pytest
from packaging.requirements import InvalidRequirement, Requirement
from packaging.version import Version


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
APPROVED_VERSIONS = {
    "litellm": Version("1.84.0"),
    "openai": Version("2.20.0"),
}
EXPECTED_DECLARATION_COUNTS = {
    Path("python/setup.py"): Counter({"openai": 6, "litellm": 3}),
    Path(".github/workflows/build-python-base.yaml"): Counter({"openai": 4}),
    Path(".github/workflows/build-python-prolog.yaml"): Counter({"openai": 2}),
}
WORKFLOW_PIN_PATTERN = re.compile(
    r"\b(?P<name>litellm|openai)(?:\[[^\]\s]+\])?\s*==\s*"
    r"(?P<version>[0-9]+(?:\.[0-9]+)+(?:[A-Za-z0-9.+-]*))",
    re.IGNORECASE,
)


def _setup_dependency_pins(path: Path) -> list[tuple[str, Version]]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    dict_key_ids = {
        id(key)
        for node in ast.walk(tree)
        if isinstance(node, ast.Dict)
        for key in node.keys
        if key is not None
    }
    pins = []

    for node in ast.walk(tree):
        if id(node) in dict_key_ids:
            continue
        if not isinstance(node, ast.Constant) or not isinstance(node.value, str):
            continue
        try:
            requirement = Requirement(node.value)
        except InvalidRequirement:
            continue

        name = requirement.name.lower()
        if name not in APPROVED_VERSIONS:
            continue

        specifiers = list(requirement.specifier)
        assert (
            len(specifiers) == 1 and specifiers[0].operator == "=="
        ), f"{path}: {name} must use one exact pin, found {requirement.specifier}"
        pins.append((name, Version(specifiers[0].version)))

    return pins


def _workflow_dependency_pins(path: Path) -> list[tuple[str, Version]]:
    pins = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.lstrip().startswith("#"):
            continue
        for match in WORKFLOW_PIN_PATTERN.finditer(line):
            pins.append((match.group("name").lower(), Version(match.group("version"))))
    return pins


def _dependency_pins(relative_path: Path) -> list[tuple[str, Version]]:
    path = REPOSITORY_ROOT / relative_path
    if path.name == "setup.py":
        return _setup_dependency_pins(path)
    return _workflow_dependency_pins(path)


@pytest.mark.parametrize(
    ("declaration", "expected_name"),
    [
        ("OpenAI == 2.20.0", "openai"),
        ("litellm[proxy]==1.84.0", "litellm"),
    ],
)
def test_workflow_pin_parser_handles_valid_requirement_variants(
    tmp_path: Path, declaration: str, expected_name: str
) -> None:
    workflow = tmp_path / "workflow.yaml"
    workflow.write_text(f"run: pip install {declaration}\n", encoding="utf-8")

    assert _workflow_dependency_pins(workflow) == [
        (expected_name, APPROVED_VERSIONS[expected_name])
    ]


@pytest.mark.parametrize("relative_path", EXPECTED_DECLARATION_COUNTS)
def test_security_sensitive_declarations_use_approved_versions(
    relative_path: Path,
) -> None:
    pins = _dependency_pins(relative_path)

    assert (
        Counter(name for name, _ in pins) == EXPECTED_DECLARATION_COUNTS[relative_path]
    )
    assert all(version == APPROVED_VERSIONS[name] for name, version in pins), pins


@pytest.mark.parametrize("package_name", APPROVED_VERSIONS)
def test_imported_development_dependencies_use_approved_versions(
    package_name: str,
) -> None:
    module = import_module(package_name)

    assert module is not None
    assert Version(installed_version(package_name)) == APPROVED_VERSIONS[package_name]
