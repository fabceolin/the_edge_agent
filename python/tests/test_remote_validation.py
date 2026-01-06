"""
Tests for remote execution validation (TEA-PARALLEL-001.4).

This module tests interrupt validation in remote execution scope
and backend warning emissions.

Test Coverage:
- AC6: Secrets backend awareness - Log when secrets.backend != env
- AC8: Interrupt point validation - Error if in remote scope
- AC9: Checkpoint warnings for non-distributed backends
- AC10: LTM warnings for local sqlite with remote strategy

Test IDs:
- 001.4-UNIT-007: Critical - Detect interrupt_before in scope
- 001.4-UNIT-008: Critical - Detect interrupt_after in scope
- 001.4-INT-006: ValidationError raised at compile-time
- 001.4-INT-007: Interrupt in scope raises validation error
- 001.4-INT-008: Checkpoint/LTM warnings logged
- 001.4-UNIT-006: Detect secrets.backend != env
"""

import logging
import pytest
from unittest.mock import patch, MagicMock

from the_edge_agent.stategraph import StateGraph, START, END
from the_edge_agent.parallel_executors import (
    RemoteExecutor,
    RemoteConfig,
)


class TestInterruptValidation:
    """Test suite for interrupt validation in remote scope."""

    def test_detect_interrupt_before_in_scope(self):
        """001.4-UNIT-007: Detect interrupt_before in remote scope."""
        graph = StateGraph({"value": int})

        # Build a simple graph
        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_a", run=lambda state: state)
        graph.add_node("step_a1", run=lambda state: state)
        graph.add_node("merge", run=lambda state: state)

        graph.add_edge("start", "branch_a")
        graph.add_edge("branch_a", "step_a1")
        graph.add_edge("step_a1", "merge")
        graph.set_entry_point("start")
        graph.set_finish_point("merge")

        # Compile with interrupt_before on step_a1
        graph.compile(
            interrupt_before=["step_a1"],
            checkpoint_dir="/tmp/test-checkpoint",
        )

        # Validate remote scope containing step_a1
        errors = graph.validate_remote_scope("branch_a", "merge")

        assert len(errors) == 1
        assert "step_a1" in errors[0]
        assert "interrupt_before" in errors[0]

    def test_detect_interrupt_after_in_scope(self):
        """001.4-UNIT-008: Detect interrupt_after in remote scope."""
        graph = StateGraph({"value": int})

        graph.add_node("start", run=lambda state: state)
        graph.add_node("branch_b", run=lambda state: state)
        graph.add_node("step_b1", run=lambda state: state)
        graph.add_node("merge", run=lambda state: state)

        graph.add_edge("start", "branch_b")
        graph.add_edge("branch_b", "step_b1")
        graph.add_edge("step_b1", "merge")
        graph.set_entry_point("start")
        graph.set_finish_point("merge")

        # Compile with interrupt_after on branch_b
        graph.compile(
            interrupt_after=["branch_b"],
            checkpoint_dir="/tmp/test-checkpoint",
        )

        # Validate remote scope containing branch_b
        errors = graph.validate_remote_scope("branch_b", "merge")

        assert len(errors) == 1
        assert "branch_b" in errors[0]
        assert "interrupt_after" in errors[0]

    def test_no_error_when_interrupt_outside_scope(self):
        """Test no errors when interrupt is outside remote scope."""
        graph = StateGraph({"value": int})

        graph.add_node("human_review", run=lambda state: state)
        graph.add_node("branch_a", run=lambda state: state)
        graph.add_node("branch_b", run=lambda state: state)
        graph.add_node("merge", run=lambda state: state)

        graph.add_edge("human_review", "branch_a")
        graph.add_edge("human_review", "branch_b")
        graph.add_edge("branch_a", "merge")
        graph.add_edge("branch_b", "merge")
        graph.set_entry_point("human_review")
        graph.set_finish_point("merge")

        # Compile with interrupt_after on human_review (BEFORE the parallel fan-out)
        graph.compile(
            interrupt_after=["human_review"],
            checkpoint_dir="/tmp/test-checkpoint",
        )

        # Validate remote scope - should be valid since interrupt is outside
        errors = graph.validate_remote_scope("branch_a", "merge")
        assert len(errors) == 0

        errors = graph.validate_remote_scope("branch_b", "merge")
        assert len(errors) == 0

    def test_multiple_interrupts_in_scope(self):
        """Test multiple interrupts detected in scope."""
        graph = StateGraph({"value": int})

        graph.add_node("branch_a", run=lambda state: state)
        graph.add_node("step_a1", run=lambda state: state)
        graph.add_node("step_a2", run=lambda state: state)
        graph.add_node("merge", run=lambda state: state)

        graph.add_edge("branch_a", "step_a1")
        graph.add_edge("step_a1", "step_a2")
        graph.add_edge("step_a2", "merge")
        graph.set_entry_point("branch_a")
        graph.set_finish_point("merge")

        # Compile with multiple interrupts
        graph.compile(
            interrupt_before=["step_a1"],
            interrupt_after=["step_a2"],
            checkpoint_dir="/tmp/test-checkpoint",
        )

        errors = graph.validate_remote_scope("branch_a", "merge")

        assert len(errors) == 2
        assert any("step_a1" in e and "interrupt_before" in e for e in errors)
        assert any("step_a2" in e and "interrupt_after" in e for e in errors)

    def test_get_nodes_between(self):
        """Test _get_nodes_between helper method."""
        graph = StateGraph({"value": int})

        graph.add_node("a", run=lambda state: state)
        graph.add_node("b", run=lambda state: state)
        graph.add_node("c", run=lambda state: state)
        graph.add_node("d", run=lambda state: state)

        graph.add_edge("a", "b")
        graph.add_edge("b", "c")
        graph.add_edge("c", "d")
        graph.set_entry_point("a")
        graph.set_finish_point("d")
        graph.compile()

        # Get nodes between a and d
        nodes = graph._get_nodes_between("a", "d")

        assert "a" in nodes
        assert "b" in nodes
        assert "c" in nodes
        assert "d" not in nodes  # End node excluded


class TestBackendWarnings:
    """Test suite for backend warning emissions."""

    def test_secrets_backend_info_logged(self, caplog):
        """001.4-UNIT-006: Detect secrets.backend != env and log info."""
        settings = {
            "secrets": {"backend": "aws_secrets_manager"},
            "ltm": {"backend": "duckdb", "path": "s3://bucket/ltm"},
            "checkpoint": {"backend": "s3"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.INFO):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Check info log for secrets backend
        assert any(
            "secrets.backend='aws_secrets_manager'" in record.message
            for record in caplog.records
        )

    def test_no_secrets_warning_when_env_backend(self, caplog):
        """001.4-INT-005 negative: No log when secrets.backend == 'env'."""
        settings = {
            "secrets": {"backend": "env"},
            "ltm": {"backend": "duckdb", "path": "s3://bucket/ltm"},
            "checkpoint": {"backend": "s3"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.INFO):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Should not see secrets backend message
        assert not any("secrets.backend" in record.message for record in caplog.records)

    def test_ltm_warning_for_local_sqlite(self, caplog):
        """001.4-INT-008: LTM warning for local sqlite with remote strategy."""
        settings = {
            "ltm": {"backend": "sqlite", "path": "/local/path/ltm.db"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.WARNING):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Check warning for LTM
        assert any(
            "LTM backend 'sqlite'" in record.message
            and "not shared across remote hosts" in record.message
            for record in caplog.records
        )

    def test_no_ltm_warning_for_cloud_storage(self, caplog):
        """Test no LTM warning when using cloud storage."""
        for cloud_path in ["s3://bucket/ltm", "gs://bucket/ltm", "az://container/ltm"]:
            caplog.clear()

            settings = {
                "ltm": {"backend": "sqlite", "path": cloud_path},
            }

            config = RemoteConfig(hosts=["user@server1"])

            with caplog.at_level(logging.WARNING):
                executor = RemoteExecutor(config=config, settings=settings)
                with executor:
                    pass

            # Should not see LTM warning for cloud paths
            ltm_warnings = [
                r
                for r in caplog.records
                if "LTM backend" in r.message and "WARNING" in r.levelname
            ]
            assert len(ltm_warnings) == 0, f"Unexpected warning for {cloud_path}"

    def test_checkpoint_warning_for_memory_backend(self, caplog):
        """001.4-INT-008: Checkpoint warning for memory backend."""
        settings = {
            "checkpoint": {"backend": "memory"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.WARNING):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Check warning for checkpoint
        assert any(
            "Checkpoint backend 'memory'" in record.message
            and "not shared across remote hosts" in record.message
            for record in caplog.records
        )

    def test_checkpoint_warning_for_file_backend(self, caplog):
        """Test checkpoint warning for file backend."""
        settings = {
            "checkpoint": {"backend": "file"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.WARNING):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        assert any(
            "Checkpoint backend 'file'" in record.message for record in caplog.records
        )

    def test_no_checkpoint_warning_for_distributed_backend(self, caplog):
        """Test no warning for distributed checkpoint backends."""
        settings = {
            "checkpoint": {"backend": "s3"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.WARNING):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Should not see checkpoint warning for distributed backends
        checkpoint_warnings = [
            r for r in caplog.records if "Checkpoint backend" in r.message
        ]
        assert len(checkpoint_warnings) == 0

    def test_all_warnings_emitted_together(self, caplog):
        """Test all warnings are emitted when multiple issues exist."""
        settings = {
            "secrets": {"backend": "gcp_secret_manager"},
            "ltm": {"backend": "sqlite", "path": "./local.db"},
            "checkpoint": {"backend": "memory"},
        }

        config = RemoteConfig(hosts=["user@server1"])

        with caplog.at_level(logging.INFO):
            executor = RemoteExecutor(config=config, settings=settings)
            with executor:
                pass

        # Should see all three warnings/infos
        messages = [r.message for r in caplog.records]

        has_secrets_info = any("secrets.backend" in m for m in messages)
        has_ltm_warning = any("LTM backend" in m for m in messages)
        has_checkpoint_warning = any("Checkpoint backend" in m for m in messages)

        assert has_secrets_info
        assert has_ltm_warning
        assert has_checkpoint_warning


class TestRemoteConfigValidation:
    """Test suite for RemoteConfig validation."""

    def test_remote_config_requires_hosts(self):
        """Test RemoteConfig raises error without hosts."""
        with pytest.raises(ValueError) as exc_info:
            RemoteConfig(hosts=[])
        assert "at least one host" in str(exc_info.value).lower()

    def test_remote_config_with_hosts(self):
        """Test RemoteConfig with valid hosts."""
        config = RemoteConfig(hosts=["user@server1"])
        assert config.hosts == ["user@server1"]
        assert config.basefile == "./tea"
        assert config.workdir == "/tmp/tea-jobs"
        assert config.cleanup is True

    def test_remote_config_with_all_options(self):
        """Test RemoteConfig with all options."""
        config = RemoteConfig(
            hosts=["user@server1", "user@server2"],
            basefile="/usr/local/bin/tea",
            workdir="/opt/tea-jobs",
            cleanup=False,
            env_vars={"include": ["API_KEY"], "mode": "ssh_env"},
        )

        assert len(config.hosts) == 2
        assert config.basefile == "/usr/local/bin/tea"
        assert config.workdir == "/opt/tea-jobs"
        assert config.cleanup is False
        assert config.env_vars == {"include": ["API_KEY"], "mode": "ssh_env"}


class TestRemoteExecutorContextManager:
    """Test suite for RemoteExecutor context manager."""

    def test_executor_requires_context_manager(self):
        """Test that executor raises error if used without context manager."""
        config = RemoteConfig(hosts=["user@server1"])
        executor = RemoteExecutor(config=config)

        with pytest.raises(RuntimeError) as exc_info:
            executor.submit(lambda: None)
        assert "context manager" in str(exc_info.value)

    def test_executor_cleanup_temp_files(self, tmp_path):
        """Test that executor cleans up temp files on exit."""
        config = RemoteConfig(hosts=["user@server1"])
        executor = RemoteExecutor(config=config)

        # Create a temp file and add to cleanup list
        temp_file = tmp_path / "test.sh"
        temp_file.write_text("#!/bin/bash\necho test")

        with executor:
            executor._temp_files.append(str(temp_file))

        # File should be cleaned up after context exits
        assert not temp_file.exists()
