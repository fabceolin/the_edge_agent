"""
Public API import tests.

These tests verify that all public symbols can be imported from their expected
locations. This catches breaking changes during refactoring when symbols are
moved between modules but not re-exported.

TEA-QA-001: Prevent import breakage from refactoring
"""


class TestYamlEnginePublicAPI:
    """Verify yaml_engine module exports its public API."""

    def test_dotdict_importable_from_yaml_engine(self):
        """DotDict should be importable from yaml_engine for backward compatibility."""
        from the_edge_agent.yaml_engine import DotDict

        # Verify it's a dict subclass with dot-access capability
        assert issubclass(DotDict, dict)
        d = DotDict({"key": "value"})
        assert d.key == "value"

    def test_yaml_engine_importable(self):
        """YAMLEngine should be importable from yaml_engine."""
        from the_edge_agent.yaml_engine import YAMLEngine

        assert hasattr(YAMLEngine, "load_from_file")
        assert hasattr(YAMLEngine, "load_from_dict")

    def test_template_processor_importable(self):
        """TemplateProcessor should be importable from yaml_engine."""
        from the_edge_agent.yaml_engine import TemplateProcessor

        assert hasattr(TemplateProcessor, "process_template")

    def test_node_factory_importable(self):
        """NodeFactory should be importable from yaml_engine."""
        from the_edge_agent.yaml_engine import NodeFactory

        assert hasattr(NodeFactory, "create_run_function")
        assert hasattr(NodeFactory, "add_node_from_config")

    def test_edge_factory_importable(self):
        """EdgeFactory should be importable from yaml_engine."""
        from the_edge_agent.yaml_engine import EdgeFactory

        assert hasattr(EdgeFactory, "add_edge_from_config")
        assert hasattr(EdgeFactory, "process_goto_and_implicit_edges")

    def test_engine_config_importable(self):
        """EngineConfig should be importable from yaml_engine."""
        from the_edge_agent.yaml_engine import EngineConfig

        # Verify it's a dataclass-like config
        assert hasattr(EngineConfig, "__init__")

    def test_all_exports_match_importable(self):
        """All symbols in __all__ should be importable."""
        from the_edge_agent import yaml_engine

        for name in yaml_engine.__all__:
            assert hasattr(yaml_engine, name), f"{name} in __all__ but not importable"


class TestPackagePublicAPI:
    """Verify main package exports its public API."""

    def test_yaml_engine_from_package(self):
        """YAMLEngine should be importable from the main package."""
        from the_edge_agent import YAMLEngine

        assert hasattr(YAMLEngine, "load_from_file")

    def test_stategraph_from_package(self):
        """StateGraph should be importable from the main package."""
        from the_edge_agent import StateGraph, START, END

        assert START == "__start__"
        assert END == "__end__"
        assert hasattr(StateGraph, "add_node")

    def test_all_exports_match_importable(self):
        """All symbols in __all__ should be importable."""
        import the_edge_agent

        for name in the_edge_agent.__all__:
            # Skip version which is a string
            if name == "__version__":
                continue
            # Just verify the attribute exists (even if None due to missing deps)
            assert hasattr(the_edge_agent, name), f"{name} in __all__ but not defined"


class TestYamlTemplatesPublicAPI:
    """Verify yaml_templates module exports its symbols."""

    def test_dotdict_canonical_location(self):
        """DotDict's canonical location is yaml_templates."""
        from the_edge_agent.yaml_templates import DotDict

        d = DotDict({"nested": {"value": 42}})
        assert d.nested.value == 42

    def test_template_processor_canonical_location(self):
        """TemplateProcessor's canonical location is yaml_templates."""
        from the_edge_agent.yaml_templates import TemplateProcessor

        assert hasattr(TemplateProcessor, "process_template")
