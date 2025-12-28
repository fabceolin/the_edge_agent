"""
Tests for TEA-KIROKU-003: Core Kiroku YAML Workflow

This test suite validates the kiroku-document-writer.yaml agent implementation.
"""

import os
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

import yaml

# Get project root
PROJECT_ROOT = Path(__file__).parent.parent.parent
YAML_PATH = PROJECT_ROOT / "examples" / "academic" / "kiroku-document-writer.yaml"
SPEC_PATH = PROJECT_ROOT / "examples" / "academic" / "test-paper-spec.yaml"


class TestKirokuYAMLValidity(unittest.TestCase):
    """AC1: YAML agent is valid and executable via tea run"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)

    def test_yaml_loads_without_error(self):
        """KIROKU-003-UNIT-001: YAML schema validates"""
        self.assertIsNotNone(self.config)
        self.assertIn("name", self.config)
        self.assertEqual(self.config["name"], "kiroku-document-writer")

    def test_yaml_engine_loads_file(self):
        """KIROKU-003-UNIT-002: YAMLEngine.from_file loads kiroku YAML"""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        graph = engine.load_from_file(str(YAML_PATH))
        self.assertIsNotNone(graph)

    def test_graph_compiles(self):
        """KIROKU-003-INT-001: Graph compiles without error"""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        graph = engine.load_from_file(str(YAML_PATH))
        compiled = graph.compile()
        self.assertIsNotNone(compiled)

    def test_all_node_references_resolve(self):
        """KIROKU-003-INT-002: All node references resolve correctly"""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()
        graph = engine.load_from_file(str(YAML_PATH))

        # Get all node names
        nodes = list(graph.graph.nodes())
        self.assertIn("__start__", nodes)
        self.assertIn("__end__", nodes)

        # Check goto targets resolve
        for node_config in self.config.get("nodes", []):
            goto = node_config.get("goto")
            if isinstance(goto, list):
                for rule in goto:
                    target = rule.get("to")
                    if target and target != "__end__":
                        self.assertIn(
                            target, nodes, f"goto target '{target}' not found in nodes"
                        )


class TestKirokuStateSchema(unittest.TestCase):
    """AC2: State schema includes all required fields"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)
        cls.schema = cls.config.get("state_schema", {})

    def test_schema_has_all_metadata_fields(self):
        """KIROKU-003-UNIT-003a: Metadata fields present"""
        metadata = ["title", "hypothesis", "area_of_paper", "type_of_document"]
        for field in metadata:
            self.assertIn(field, self.schema, f"Missing metadata field: {field}")

    def test_schema_has_all_planning_fields(self):
        """KIROKU-003-UNIT-003b: Planning fields present"""
        planning = ["section_names", "number_of_paragraphs", "plan"]
        for field in planning:
            self.assertIn(field, self.schema, f"Missing planning field: {field}")

    def test_schema_has_all_drafting_fields(self):
        """KIROKU-003-UNIT-003c: Drafting fields present"""
        drafting = ["draft", "sentences_per_paragraph"]
        for field in drafting:
            self.assertIn(field, self.schema, f"Missing drafting field: {field}")

    def test_schema_has_all_review_fields(self):
        """KIROKU-003-UNIT-003d: Review fields present"""
        review = ["critique", "review_instructions", "review_topic_sentences"]
        for field in review:
            self.assertIn(field, self.schema, f"Missing review field: {field}")

    def test_schema_has_all_research_fields(self):
        """KIROKU-003-UNIT-003e: Research fields present"""
        research = ["content", "references", "cache"]
        for field in research:
            self.assertIn(field, self.schema, f"Missing research field: {field}")

    def test_schema_has_all_control_fields(self):
        """KIROKU-003-UNIT-003f: Control fields present"""
        control = ["revision_number", "max_revisions", "number_of_queries"]
        for field in control:
            self.assertIn(field, self.schema, f"Missing control field: {field}")

    def test_schema_has_flag_fields(self):
        """KIROKU-003-UNIT-004: Flag fields present with correct types"""
        self.assertIn("suggest_title_flag", self.schema)
        self.assertIn("generate_citations_flag", self.schema)
        self.assertEqual(self.schema["suggest_title_flag"], "bool")
        self.assertEqual(self.schema["generate_citations_flag"], "bool")

    def test_schema_field_count(self):
        """KIROKU-003-UNIT-005: Schema has expected field count (20+)"""
        self.assertGreaterEqual(len(self.schema), 20)


class TestKirokuNodes(unittest.TestCase):
    """AC3: All 13 nodes implemented"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)
        cls.nodes = cls.config.get("nodes", [])
        cls.node_names = [n["name"] for n in cls.nodes]

    def test_all_workflow_nodes_present(self):
        """KIROKU-003-INT-003: All workflow nodes defined"""
        expected_nodes = [
            "suggest_title",
            "suggest_title_review",
            "internet_search",
            "topic_sentence_writer",
            "topic_sentence_manual_review",
            "paper_writer",
            "writer_manual_reviewer",
            "reflection_reviewer",
            "reflection_manual_review",
            "write_abstract",
            "generate_references",
            "generate_citations",
            "generate_figure_captions",
        ]
        for node in expected_nodes:
            self.assertIn(node, self.node_names, f"Missing node: {node}")

    def test_suggest_title_uses_llm_call(self):
        """KIROKU-003-INT-003a: suggest_title uses llm.call"""
        node = next(n for n in self.nodes if n["name"] == "suggest_title")
        self.assertEqual(node.get("uses"), "llm.call")

    def test_internet_search_uses_web_search(self):
        """KIROKU-003-INT-004: internet_search uses web.search"""
        node = next(n for n in self.nodes if n["name"] == "internet_search")
        self.assertEqual(node.get("uses"), "web.search")

    def test_topic_sentence_writer_produces_plan(self):
        """KIROKU-003-INT-005: topic_sentence_writer has plan output"""
        node = next(n for n in self.nodes if n["name"] == "topic_sentence_writer")
        output = node.get("output", {})
        self.assertIn("plan", output)

    def test_paper_writer_produces_draft(self):
        """KIROKU-003-INT-006: paper_writer has draft output"""
        node = next(n for n in self.nodes if n["name"] == "paper_writer")
        output = node.get("output", {})
        self.assertIn("draft", output)

    def test_reflection_reviewer_produces_critique(self):
        """KIROKU-003-INT-007: reflection_reviewer has critique output"""
        node = next(n for n in self.nodes if n["name"] == "reflection_reviewer")
        output = node.get("output", {})
        self.assertIn("critique", output)

    def test_write_abstract_uses_llm(self):
        """KIROKU-003-INT-008: write_abstract uses llm.call"""
        node = next(n for n in self.nodes if n["name"] == "write_abstract")
        self.assertEqual(node.get("uses"), "llm.call")

    def test_generate_references_uses_llm(self):
        """KIROKU-003-INT-009: generate_references uses llm.call"""
        node = next(n for n in self.nodes if n["name"] == "generate_references")
        self.assertEqual(node.get("uses"), "llm.call")

    def test_generate_citations_uses_text_insert(self):
        """KIROKU-003-INT-010: generate_citations uses text.insert_citations"""
        node = next(n for n in self.nodes if n["name"] == "generate_citations")
        self.assertEqual(node.get("uses"), "text.insert_citations")

    def test_generate_figure_captions_uses_python(self):
        """KIROKU-003-INT-011: generate_figure_captions uses Python run block"""
        node = next(n for n in self.nodes if n["name"] == "generate_figure_captions")
        self.assertIn("run", node)
        self.assertIn("import re", node["run"])


class TestKirokuConditionalEdges(unittest.TestCase):
    """AC4: Conditional edges function correctly"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)
        cls.nodes = cls.config.get("nodes", [])

    def _get_node(self, name):
        return next(n for n in self.nodes if n["name"] == name)

    def test_suggest_title_flag_true_routes_to_suggest(self):
        """KIROKU-003-UNIT-006: suggest_title_flag=true routes to suggest_title"""
        node = self._get_node("check_suggest_title")
        goto = node.get("goto", [])
        # Find rule with suggest_title_flag condition
        rule = next(
            (r for r in goto if "suggest_title_flag" in str(r.get("if", ""))), None
        )
        self.assertIsNotNone(rule)
        self.assertEqual(rule["to"], "suggest_title")

    def test_suggest_title_flag_false_routes_to_search(self):
        """KIROKU-003-UNIT-007: suggest_title_flag=false routes to internet_search"""
        node = self._get_node("check_suggest_title")
        goto = node.get("goto", [])
        # Find fallback rule (no condition)
        fallback = next((r for r in goto if "if" not in r), None)
        self.assertIsNotNone(fallback)
        self.assertEqual(fallback["to"], "internet_search")

    def test_citations_flag_true_routes_to_refs(self):
        """KIROKU-003-UNIT-008: generate_citations_flag=true routes through citations"""
        node = self._get_node("check_citations")
        goto = node.get("goto", [])
        rule = next(
            (r for r in goto if "generate_citations_flag" in str(r.get("if", ""))), None
        )
        self.assertIsNotNone(rule)
        self.assertEqual(rule["to"], "generate_references")

    def test_citations_flag_false_skips_citations(self):
        """KIROKU-003-UNIT-009: generate_citations_flag=false skips citations"""
        node = self._get_node("check_citations")
        goto = node.get("goto", [])
        fallback = next((r for r in goto if "if" not in r), None)
        self.assertIsNotNone(fallback)
        self.assertEqual(fallback["to"], "generate_figure_captions")

    def test_revision_loop_condition(self):
        """KIROKU-003-UNIT-010: revision_number < max_revisions triggers reflection"""
        node = self._get_node("writer_manual_reviewer")
        goto = node.get("goto", [])
        rule = next(
            (r for r in goto if "revision_number" in str(r.get("if", ""))), None
        )
        self.assertIsNotNone(rule)
        self.assertEqual(rule["to"], "reflection_reviewer")

    def test_user_instruction_triggers_revision(self):
        """KIROKU-003-UNIT-011: user_instruction non-empty triggers revision"""
        node = self._get_node("writer_manual_reviewer")
        goto = node.get("goto", [])
        rule = next(
            (r for r in goto if "user_instruction" in str(r.get("if", ""))), None
        )
        self.assertIsNotNone(rule)
        self.assertEqual(rule["to"], "paper_writer")


class TestKirokuInterrupts(unittest.TestCase):
    """AC5: Interrupt points pause execution"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)
        cls.nodes = cls.config.get("nodes", [])

    def test_five_interrupt_points_defined(self):
        """KIROKU-003-INT-012: 5 interrupt points defined"""
        interrupt_nodes = [n["name"] for n in self.nodes if n.get("interrupt")]
        expected = [
            "suggest_title_review",
            "topic_sentence_manual_review",
            "writer_manual_reviewer",
            "reflection_manual_review",
            "generate_citations",
        ]
        for node in expected:
            self.assertIn(node, interrupt_nodes, f"Missing interrupt: {node}")

    def test_suggest_title_review_has_interrupt_before(self):
        """KIROKU-003-INT-012a: suggest_title_review has interrupt:before"""
        node = next(n for n in self.nodes if n["name"] == "suggest_title_review")
        self.assertEqual(node.get("interrupt"), "before")

    def test_topic_sentence_review_has_interrupt(self):
        """KIROKU-003-INT-013: topic_sentence_manual_review has interrupt"""
        node = next(
            n for n in self.nodes if n["name"] == "topic_sentence_manual_review"
        )
        self.assertEqual(node.get("interrupt"), "before")

    def test_writer_reviewer_has_interrupt(self):
        """KIROKU-003-INT-014: writer_manual_reviewer has interrupt"""
        node = next(n for n in self.nodes if n["name"] == "writer_manual_reviewer")
        self.assertEqual(node.get("interrupt"), "before")


class TestKirokuPrompts(unittest.TestCase):
    """AC6: Prompts preserved as Jinja2 templates"""

    @classmethod
    def setUpClass(cls):
        with open(YAML_PATH, "r") as f:
            cls.config = yaml.safe_load(f)
        cls.prompts = cls.config.get("data", {}).get("prompts", {})

    def test_all_eleven_prompts_defined(self):
        """KIROKU-003-UNIT-012: All 11 prompts defined in data.prompts"""
        expected = [
            "title",
            "topic_sentence",
            "topic_sentence_review",
            "paper_writer",
            "writer_review",
            "reflection_reviewer",
            "internet_search",
            "research_critique",
            "abstract_writer",
            "references",
            "task_template",
        ]
        for prompt in expected:
            self.assertIn(prompt, self.prompts, f"Missing prompt: {prompt}")

    def test_prompts_use_jinja2_syntax(self):
        """KIROKU-003-E2E-002a: Prompts use Jinja2 {{ state.var }} syntax"""
        for name, content in self.prompts.items():
            # At least some prompts should have state references
            if name in ["title", "paper_writer", "reflection_reviewer"]:
                self.assertIn(
                    "{{ state.", content, f"Prompt '{name}' missing Jinja2 state refs"
                )

    def test_paper_writer_prompt_has_required_vars(self):
        """KIROKU-003-E2E-002b: paper_writer has sentences_per_paragraph, task, content"""
        prompt = self.prompts["paper_writer"]
        self.assertIn("sentences_per_paragraph", prompt)
        self.assertIn("state.task", prompt)
        self.assertIn("state.content", prompt)


class TestKirokuFigureCaptions(unittest.TestCase):
    """Test generate_figure_captions Python run block"""

    def test_figure_caption_regex(self):
        """KIROKU-003-INT-011b: Figure caption regex works correctly"""
        import re

        draft = """
# Introduction

Some text here.

![Alt text for image](https://example.com/image.png)

More text.

![Another figure](https://example.com/fig2.png)

Conclusion.
"""
        pattern = r"!\[([^\]]*)\]\(([^\)]*)\)"
        matches = list(reversed(list(re.finditer(pattern, draft))))
        fig_num = len(matches)

        for match in matches:
            left, right = match.span()
            alt_text = match.group(1)
            url = match.group(2)
            caption = f'![]({url})\n\n<div align="center">Figure {fig_num}: {alt_text}</div>\n'
            draft = draft[:left] + caption + draft[right:]
            fig_num -= 1

        self.assertIn("Figure 1: Alt text for image", draft)
        self.assertIn("Figure 2: Another figure", draft)


class TestKirokuTestSpec(unittest.TestCase):
    """Test the test-paper-spec.yaml is valid"""

    def test_spec_file_exists(self):
        """Test spec file exists"""
        self.assertTrue(SPEC_PATH.exists())

    def test_spec_has_required_fields(self):
        """Test spec has minimum required fields"""
        with open(SPEC_PATH, "r") as f:
            spec = yaml.safe_load(f)

        required = ["title", "hypothesis", "area_of_paper", "section_names"]
        for field in required:
            self.assertIn(field, spec, f"Missing required field: {field}")


if __name__ == "__main__":
    unittest.main()
