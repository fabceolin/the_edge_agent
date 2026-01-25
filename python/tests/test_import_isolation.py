"""
Import Isolation Tests for TEA-ARCH-001.

These tests verify that:
1. HTTPResponse is properly located in the exceptions module
2. HTTPResponse can be imported from both old and new paths (backward compat)
3. HTTPResponse API is unchanged
4. stategraph.py source code no longer imports from actions package

Note: Full isolation of `from the_edge_agent import StateGraph` from optional
dependencies requires additional work on yaml_engine/actions lazy loading,
which is out of scope for TEA-ARCH-001 (see story Out of Scope section).
The yaml_engine module still imports the actions package, which pulls in
github_actions with its requests dependency.
"""


def test_stategraph_source_does_not_import_actions():
    """AC1: stategraph.py source should not import from actions package.

    This verifies the architectural fix - stategraph.py now imports HTTPResponse
    from exceptions module instead of actions.http_response_actions.
    """
    import ast
    import os

    # Read stategraph.py source
    stategraph_path = os.path.join(
        os.path.dirname(__file__),
        "..",
        "src",
        "the_edge_agent",
        "stategraph.py",
    )
    with open(stategraph_path, "r") as f:
        source = f.read()

    # Parse the AST
    tree = ast.parse(source)

    # Check all import statements
    actions_imports = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if "actions" in alias.name:
                    actions_imports.append(alias.name)
        elif isinstance(node, ast.ImportFrom):
            if node.module and "actions" in node.module:
                actions_imports.append(node.module)

    assert not actions_imports, (
        f"stategraph.py still imports from actions package: {actions_imports}. "
        f"HTTPResponse should be imported from the_edge_agent.exceptions"
    )


def test_exceptions_module_exists_and_has_httpresponse():
    """AC2: exceptions.py should exist and contain HTTPResponse class."""
    import os

    exceptions_path = os.path.join(
        os.path.dirname(__file__),
        "..",
        "src",
        "the_edge_agent",
        "exceptions.py",
    )
    assert os.path.exists(exceptions_path), "exceptions.py should exist"

    # Read and verify it contains HTTPResponse class
    with open(exceptions_path, "r") as f:
        source = f.read()

    assert (
        "class HTTPResponse" in source
    ), "exceptions.py should contain HTTPResponse class"


def test_httpresponse_backward_compat_import():
    """AC3: Old import path should still work."""
    from the_edge_agent.actions.http_response_actions import HTTPResponse

    resp = HTTPResponse(status=401, body={"error": "unauthorized"})
    assert resp.status == 401
    assert resp.body == {"error": "unauthorized"}


def test_httpresponse_new_import_path():
    """AC2: New canonical import path should work."""
    from the_edge_agent.exceptions import HTTPResponse

    resp = HTTPResponse(status=200, body={"ok": True})
    assert resp.status == 200
    assert resp.body == {"ok": True}


def test_httpresponse_package_level_import():
    """AC2: HTTPResponse should be importable from package level."""
    from the_edge_agent import HTTPResponse

    resp = HTTPResponse(status=403, body="forbidden")
    assert resp.status == 403


def test_httpresponse_api_unchanged():
    """AC2: HTTPResponse API must be unchanged."""
    from the_edge_agent.exceptions import HTTPResponse

    resp = HTTPResponse(
        status=500,
        body="error",
        headers={"X-Custom": "value"},
        content_type="text/plain",
    )
    assert resp.status == 500
    assert resp.body == "error"
    assert resp.headers == {"X-Custom": "value", "Content-Type": "text/plain"}
    assert resp.content_type == "text/plain"

    # Test to_dict method
    resp_dict = resp.to_dict()
    assert resp_dict["status"] == 500
    assert resp_dict["body"] == "error"
    assert "X-Custom" in resp_dict["headers"]


def test_httpresponse_is_exception():
    """AC2: HTTPResponse must be an Exception subclass."""
    from the_edge_agent.exceptions import HTTPResponse

    resp = HTTPResponse(status=404)
    assert isinstance(resp, Exception)

    # Verify it can be raised and caught
    try:
        raise resp
    except HTTPResponse as e:
        assert e.status == 404


def test_httpresponse_default_content_type_in_headers():
    """AC2: Content-Type should be set in headers by default."""
    from the_edge_agent.exceptions import HTTPResponse

    resp = HTTPResponse(status=200)
    assert "Content-Type" in resp.headers
    assert resp.headers["Content-Type"] == "application/json"


def test_httpresponse_same_class_both_paths():
    """AC3: Both import paths should return the same class."""
    from the_edge_agent.exceptions import HTTPResponse as NewHTTPResponse
    from the_edge_agent.actions.http_response_actions import (
        HTTPResponse as OldHTTPResponse,
    )

    # They should be the exact same class
    assert NewHTTPResponse is OldHTTPResponse
