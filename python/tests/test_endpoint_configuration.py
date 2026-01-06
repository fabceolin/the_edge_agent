"""
Tests for HTTP Endpoint Configuration (TEA-BUILTIN-015.7).

Tests cover:
- AC1: YAML endpoint section with path, method, params (EndpointConfig)
- AC2: HTTP method specification (GET, POST, PUT, DELETE, PATCH)
- AC3: Tags and OpenAPI metadata
- AC4: input_schema linkage
- AC5: output_schema linkage
- AC6: Path parameter parsing and validation
- AC7: Query parameter mapping with defaults
- AC8: OpenAPI 3.0 generation from routes
- AC9: Auth override at endpoint level
- AC10: Auto-registration into route registry
"""

import pytest
from typing import Dict, Any


class TestEndpointConfig:
    """Test EndpointConfig model and parsing."""

    def test_basic_endpoint_creation(self):
        """AC1: endpoint section defines path, method, params."""
        from the_edge_agent.http import EndpointConfig, HTTPMethod

        config = EndpointConfig(
            path="/api/v1/research",
            method=HTTPMethod.POST,
            summary="Execute research",
        )

        assert config.path == "/api/v1/research"
        assert config.method == HTTPMethod.POST
        assert config.summary == "Execute research"

    def test_path_normalization(self):
        """AC1: paths are normalized to start with /."""
        from the_edge_agent.http import EndpointConfig

        config = EndpointConfig(path="api/search", method="POST")
        assert config.path == "/api/search"

    def test_path_validation_double_slash(self):
        """AC1: paths cannot contain double slashes."""
        from the_edge_agent.http import EndpointConfig

        with pytest.raises(ValueError, match="double slashes"):
            EndpointConfig(path="/api//v1", method="GET")

    def test_http_methods(self):
        """AC2: all HTTP methods are supported."""
        from the_edge_agent.http import EndpointConfig, HTTPMethod

        for method in [
            HTTPMethod.GET,
            HTTPMethod.POST,
            HTTPMethod.PUT,
            HTTPMethod.DELETE,
            HTTPMethod.PATCH,
        ]:
            config = EndpointConfig(path="/test", method=method)
            assert config.method == method

    def test_method_from_string(self):
        """AC2: methods can be specified as strings."""
        from the_edge_agent.http import EndpointConfig, HTTPMethod

        config = EndpointConfig(path="/test", method="post")
        assert config.method == HTTPMethod.POST

    def test_invalid_method(self):
        """AC2: invalid methods are rejected."""
        from the_edge_agent.http import EndpointConfig

        with pytest.raises(ValueError, match="Invalid HTTP method"):
            EndpointConfig(path="/test", method="INVALID")

    def test_tags_and_metadata(self):
        """AC3: tags and OpenAPI metadata are supported."""
        from the_edge_agent.http import EndpointConfig

        config = EndpointConfig(
            path="/api/search",
            method="GET",
            summary="Search documents",
            description="Full-text search across all documents",
            tags=["Search", "Documents"],
        )

        assert config.summary == "Search documents"
        assert config.description == "Full-text search across all documents"
        assert config.tags == ["Search", "Documents"]


class TestPathParams:
    """Test path parameter parsing (AC6)."""

    def test_path_param_extraction(self):
        """AC6: path params are extracted from URL."""
        from the_edge_agent.http import EndpointConfig, PathParam

        config = EndpointConfig(
            path="/users/{user_id}/posts/{post_id}",
            method="GET",
            path_params={
                "user_id": PathParam(type="int"),
                "post_id": PathParam(type="str"),
            },
        )

        result = config.extract_path_params("/users/123/posts/abc")
        assert result == {"user_id": 123, "post_id": "abc"}

    def test_path_param_type_conversion(self):
        """AC6: path params are type-converted."""
        from the_edge_agent.http import PathParam

        # Integer
        int_param = PathParam(type="int")
        assert int_param.validate_value("42") == 42

        # Float
        float_param = PathParam(type="float")
        assert float_param.validate_value("3.14") == 3.14

        # String
        str_param = PathParam(type="str")
        assert str_param.validate_value("hello") == "hello"

    def test_path_param_pattern_validation(self):
        """AC6: path params can have regex patterns."""
        from the_edge_agent.http import PathParam

        param = PathParam(type="str", pattern=r"^[a-z]+$")
        assert param.validate_value("abc") == "abc"

        with pytest.raises(ValueError, match="does not match pattern"):
            param.validate_value("ABC123")

    def test_path_param_auto_creation(self):
        """AC6: undefined path placeholders get auto-created as str."""
        from the_edge_agent.http import EndpointConfig

        config = EndpointConfig(
            path="/users/{user_id}",
            method="GET",
            # path_params not specified
        )

        # user_id should be auto-created
        assert "user_id" in config.path_params
        assert config.path_params["user_id"].type == "str"

    def test_path_mismatch_error(self):
        """AC6: mismatched paths raise error."""
        from the_edge_agent.http import EndpointConfig, PathParam

        config = EndpointConfig(
            path="/users/{id}",
            method="GET",
            path_params={"id": PathParam(type="int")},
        )

        with pytest.raises(ValueError, match="does not match pattern"):
            config.extract_path_params("/posts/123")


class TestQueryParams:
    """Test query parameter mapping (AC7)."""

    def test_query_param_extraction(self):
        """AC7: query params are extracted and validated."""
        from the_edge_agent.http import EndpointConfig, QueryParam

        config = EndpointConfig(
            path="/search",
            method="GET",
            query_params={
                "q": QueryParam(type="str", required=True),
                "limit": QueryParam(type="int", default=10),
            },
        )

        result = config.extract_query_params({"q": "test", "limit": "25"})
        assert result == {"q": "test", "limit": 25}

    def test_query_param_defaults(self):
        """AC7: query params use defaults when not provided."""
        from the_edge_agent.http import EndpointConfig, QueryParam

        config = EndpointConfig(
            path="/list",
            method="GET",
            query_params={
                "page": QueryParam(type="int", default=1),
                "size": QueryParam(type="int", default=20),
            },
        )

        result = config.extract_query_params({})
        assert result == {"page": 1, "size": 20}

    def test_query_param_required(self):
        """AC7: required params raise error if missing."""
        from the_edge_agent.http import EndpointConfig, QueryParam

        config = EndpointConfig(
            path="/search",
            method="GET",
            query_params={
                "q": QueryParam(type="str", required=True),
            },
        )

        with pytest.raises(ValueError, match="Required parameter"):
            config.extract_query_params({})

    def test_query_param_map_to(self):
        """AC7: query params can map to different state keys."""
        from the_edge_agent.http import EndpointConfig, QueryParam

        config = EndpointConfig(
            path="/search",
            method="GET",
            query_params={
                "q": QueryParam(type="str", map_to="search_query"),
            },
        )

        result = config.extract_query_params({"q": "test"})
        assert result == {"search_query": "test"}

    def test_query_param_type_conversion(self):
        """AC7: query params are type-converted."""
        from the_edge_agent.http import QueryParam

        # Integer with range
        int_param = QueryParam(type="int", min=0, max=100)
        assert int_param.validate_value("50") == 50

        # Boolean
        bool_param = QueryParam(type="bool")
        assert bool_param.validate_value("true") is True
        assert bool_param.validate_value("false") is False

        # List
        list_param = QueryParam(type="list")
        assert list_param.validate_value("a,b,c") == ["a", "b", "c"]

    def test_query_param_range_validation(self):
        """AC7: numeric params enforce min/max."""
        from the_edge_agent.http import QueryParam

        param = QueryParam(type="int", min=1, max=100)

        with pytest.raises(ValueError, match="less than minimum"):
            param.validate_value("0")

        with pytest.raises(ValueError, match="greater than maximum"):
            param.validate_value("101")


class TestAuthConfig:
    """Test auth override at endpoint level (AC9)."""

    def test_auth_required_default(self):
        """AC9: auth is required by default."""
        from the_edge_agent.http import EndpointConfig

        config = EndpointConfig(path="/api", method="POST")
        assert not config.is_public()

    def test_auth_public_endpoint(self):
        """AC9: endpoints can be made public."""
        from the_edge_agent.http import EndpointConfig, AuthConfig

        config = EndpointConfig(
            path="/public",
            method="GET",
            auth=AuthConfig(required=False),
        )
        assert config.is_public()

    def test_auth_with_roles(self):
        """AC9: endpoints can require specific roles."""
        from the_edge_agent.http import EndpointConfig, AuthConfig

        config = EndpointConfig(
            path="/admin",
            method="DELETE",
            auth=AuthConfig(required=True, roles=["admin"]),
        )

        assert config.requires_role("admin")
        assert not config.requires_role("user")

    def test_auth_from_dict(self):
        """AC9: auth config can be specified as dict."""
        from the_edge_agent.http import EndpointConfig

        config = EndpointConfig(
            path="/api",
            method="POST",
            auth={"required": True, "roles": ["editor"]},
        )

        assert not config.is_public()
        assert config.requires_role("editor")


class TestRouteRegistry:
    """Test route registry and registration (AC10)."""

    def test_register_route(self):
        """AC10: routes can be registered."""
        from the_edge_agent.http import RouteRegistry, EndpointConfig

        registry = RouteRegistry()
        config = EndpointConfig(path="/test", method="POST")

        registry.register(
            agent_name="test_agent",
            config=config,
            handler=lambda state: {"result": "ok"},
        )

        assert len(registry.routes) == 1
        route = registry.get_route("POST", "/test")
        assert route is not None
        assert route.agent_name == "test_agent"

    def test_register_with_schemas(self):
        """AC10: routes can include input/output schemas (AC4, AC5)."""
        from the_edge_agent.http import RouteRegistry, EndpointConfig

        registry = RouteRegistry()
        config = EndpointConfig(path="/api", method="POST")

        registry.register(
            agent_name="api_agent",
            config=config,
            handler=lambda state: {},
            input_schema={"query": {"type": "str", "required": True}},
            output_schema={"result": {"type": "str"}},
        )

        route = registry.get_route("POST", "/api")
        assert route.input_schema is not None
        assert route.output_schema is not None

    def test_register_conflict(self):
        """AC10: duplicate routes raise error."""
        from the_edge_agent.http import RouteRegistry, EndpointConfig

        registry = RouteRegistry()
        config = EndpointConfig(path="/test", method="POST")

        registry.register("agent1", config, lambda s: {})

        with pytest.raises(ValueError, match="Route conflict"):
            registry.register("agent2", config, lambda s: {})

    def test_unregister(self):
        """AC10: routes can be unregistered."""
        from the_edge_agent.http import RouteRegistry, EndpointConfig

        registry = RouteRegistry()
        registry.register(
            "agent1",
            EndpointConfig(path="/a", method="GET"),
            lambda s: {},
        )
        registry.register(
            "agent1",
            EndpointConfig(path="/b", method="GET"),
            lambda s: {},
        )

        count = registry.unregister("agent1")
        assert count == 2
        assert len(registry.routes) == 0

    def test_global_registry(self):
        """AC10: global registry is available."""
        from the_edge_agent.http import (
            get_global_registry,
            reset_global_registry,
            EndpointConfig,
        )

        reset_global_registry()
        registry = get_global_registry()

        registry.register(
            "test",
            EndpointConfig(path="/global", method="GET"),
            lambda s: {},
        )

        # Should get same instance
        assert len(get_global_registry().routes) == 1

        reset_global_registry()
        assert len(get_global_registry().routes) == 0


class TestOpenAPIGeneration:
    """Test OpenAPI 3.0 generation from routes (AC8)."""

    def test_basic_openapi_spec(self):
        """AC8: OpenAPI spec is generated from routes."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "search",
            EndpointConfig(
                path="/api/search",
                method="GET",
                summary="Search documents",
            ),
            lambda s: {},
        )

        spec = generate_openapi_from_routes(registry, "test-api", "1.0.0")

        assert spec["openapi"] == "3.0.3"
        assert spec["info"]["title"] == "test-api"
        assert spec["info"]["version"] == "1.0.0"
        assert "/api/search" in spec["paths"]

    def test_openapi_path_params(self):
        """AC8: path params are included in OpenAPI."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            PathParam,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "get_user",
            EndpointConfig(
                path="/users/{user_id}",
                method="GET",
                path_params={"user_id": PathParam(type="int", description="User ID")},
            ),
            lambda s: {},
        )

        spec = generate_openapi_from_routes(registry)
        params = spec["paths"]["/users/{user_id}"]["get"]["parameters"]

        assert len(params) == 1
        assert params[0]["name"] == "user_id"
        assert params[0]["in"] == "path"
        assert params[0]["schema"]["type"] == "integer"
        assert params[0]["description"] == "User ID"

    def test_openapi_query_params(self):
        """AC8: query params are included in OpenAPI."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            QueryParam,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "list",
            EndpointConfig(
                path="/items",
                method="GET",
                query_params={
                    "limit": QueryParam(type="int", default=10, max=100),
                },
            ),
            lambda s: {},
        )

        spec = generate_openapi_from_routes(registry)
        params = spec["paths"]["/items"]["get"]["parameters"]

        assert len(params) == 1
        assert params[0]["name"] == "limit"
        assert params[0]["in"] == "query"
        assert params[0]["schema"]["default"] == 10
        assert params[0]["schema"]["maximum"] == 100

    def test_openapi_request_body(self):
        """AC8: request body is included for non-GET methods (AC4)."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "create",
            EndpointConfig(path="/items", method="POST"),
            lambda s: {},
            input_schema={"name": {"type": "str", "required": True}},
        )

        spec = generate_openapi_from_routes(registry)
        req_body = spec["paths"]["/items"]["post"]["requestBody"]

        assert req_body["required"] is True
        schema = req_body["content"]["application/json"]["schema"]
        assert "name" in schema["properties"]

    def test_openapi_response_schema(self):
        """AC8: response schema is included (AC5)."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "get",
            EndpointConfig(path="/item", method="GET"),
            lambda s: {},
            output_schema={"id": {"type": "int"}, "name": {"type": "str"}},
        )

        spec = generate_openapi_from_routes(registry)
        response = spec["paths"]["/item"]["get"]["responses"]["200"]

        assert "content" in response
        schema = response["content"]["application/json"]["schema"]
        assert "id" in schema["properties"]

    def test_openapi_security(self):
        """AC8: security is included for auth-required endpoints (AC9)."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            AuthConfig,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "secure",
            EndpointConfig(
                path="/secure",
                method="POST",
                auth=AuthConfig(required=True),
            ),
            lambda s: {},
        )

        spec = generate_openapi_from_routes(registry)
        operation = spec["paths"]["/secure"]["post"]

        assert "security" in operation
        assert {"bearerAuth": []} in operation["security"]
        assert "401" in operation["responses"]

    def test_openapi_tags(self):
        """AC8: tags are included in operation (AC3)."""
        from the_edge_agent.http import (
            RouteRegistry,
            EndpointConfig,
            generate_openapi_from_routes,
        )

        registry = RouteRegistry()
        registry.register(
            "search",
            EndpointConfig(
                path="/search",
                method="GET",
                tags=["Search", "Public"],
            ),
            lambda s: {},
        )

        spec = generate_openapi_from_routes(registry)
        tags = spec["paths"]["/search"]["get"]["tags"]

        assert tags == ["Search", "Public"]


class TestYAMLEngineIntegration:
    """Test YAMLEngine integration with endpoint configuration."""

    def test_endpoint_config_parsed_from_yaml(self):
        """AC10: endpoint config is parsed from YAML."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        graph = engine.load_from_dict(
            {
                "name": "test_agent",
                "endpoint": {
                    "path": "/api/v1/test",
                    "method": "POST",
                    "summary": "Test endpoint",
                },
                "nodes": [{"name": "node1", "run": "return {}"}],
            }
        )

        assert engine.endpoint_config is not None
        assert engine.endpoint_config.path == "/api/v1/test"
        assert hasattr(graph, "_endpoint_config")
        assert hasattr(graph, "_agent_name")
        assert graph._agent_name == "test_agent"

    def test_endpoint_config_with_params(self):
        """AC10: endpoint params are parsed correctly."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        graph = engine.load_from_dict(
            {
                "name": "user_agent",
                "endpoint": {
                    "path": "/users/{id}",
                    "method": "GET",
                    "path_params": {
                        "id": {"type": "int"},
                    },
                    "query_params": {
                        "include": {"type": "str", "default": "basic"},
                    },
                },
                "nodes": [{"name": "get_user", "run": "return {}"}],
            }
        )

        config = engine.endpoint_config
        assert "id" in config.path_params
        assert "include" in config.query_params
        assert config.query_params["include"].default == "basic"


class TestParseEndpointConfig:
    """Test parse_endpoint_config helper function."""

    def test_parse_from_agent_config(self):
        """parse_endpoint_config extracts endpoint from agent config."""
        from the_edge_agent.http import parse_endpoint_config

        agent_config = {
            "name": "my_agent",
            "endpoint": {
                "path": "/api/my",
                "method": "POST",
            },
            "nodes": [],
        }

        config = parse_endpoint_config(agent_config)
        assert config is not None
        assert config.path == "/api/my"

    def test_parse_returns_none_if_missing(self):
        """parse_endpoint_config returns None if no endpoint."""
        from the_edge_agent.http import parse_endpoint_config

        config = parse_endpoint_config({"name": "agent", "nodes": []})
        assert config is None

    def test_parse_handles_invalid_gracefully(self):
        """parse_endpoint_config returns None for invalid config."""
        from the_edge_agent.http import parse_endpoint_config

        # Non-dict endpoint
        config = parse_endpoint_config({"endpoint": "invalid"})
        assert config is None
