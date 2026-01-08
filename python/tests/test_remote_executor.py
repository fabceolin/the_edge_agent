"""
Tests for RemoteExecutor - TEA-PARALLEL-001.3: Remote Executor Core

This module tests the RemoteExecutor implementation including:
- RemoteConfig dataclass validation
- GNU Parallel detection and command generation
- SSH fallback with asyncio
- State JSON serialization for remote transfer
- File transfer (SCP) for binary, YAML, and state
- Result collection and deserialization
- Configurable cleanup
- Timeout and retry integration with ParallelConfig

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

import asyncio
import json
import os
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, List
from unittest import mock
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

# Configure pytest-asyncio mode
pytestmark = pytest.mark.anyio

from the_edge_agent.parallel import ParallelConfig, ParallelFlowResult
from the_edge_agent.parallel_executors import (
    RemoteConfig,
    RemoteExecutor,
    RemoteExecutionError,
    SCPTransferError,
    FlowTask,
    ExecutorResult,
)


# =============================================================================
# Test Fixtures
# =============================================================================


@pytest.fixture
def temp_basefile():
    """Create a temporary basefile (mock tea binary)."""
    with tempfile.NamedTemporaryFile(mode="w", suffix="_tea", delete=False) as f:
        f.write("#!/bin/bash\necho 'mock tea binary'\n")
        path = f.name
    os.chmod(path, 0o755)
    yield path
    Path(path).unlink(missing_ok=True)


@pytest.fixture
def temp_yaml_file():
    """Create a temporary YAML workflow file."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
        f.write(
            """
name: test-workflow
state_schema:
  value: int
  result: str

nodes:
  - name: process
    run: |
      return {"result": f"processed-{state['value']}"}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        )
        path = f.name
    yield path
    Path(path).unlink(missing_ok=True)


@pytest.fixture
def remote_config(temp_basefile, temp_yaml_file):
    """Create a basic RemoteConfig for testing."""
    return RemoteConfig(
        hosts=["user@host1", "user@host2"],
        basefile=temp_basefile,
        workdir="/tmp/tea-test-jobs",
        cleanup=True,
        yaml_path=temp_yaml_file,
    )


@pytest.fixture
def remote_executor(remote_config):
    """Create a RemoteExecutor instance for testing."""
    return RemoteExecutor(remote_config)


# =============================================================================
# RemoteConfig Tests
# =============================================================================


class TestRemoteConfig:
    """Tests for RemoteConfig dataclass."""

    def test_remote_config_basic_creation(self, temp_basefile):
        """Test basic RemoteConfig creation with required fields."""
        config = RemoteConfig(
            hosts=["user@server1"],
            basefile=temp_basefile,
        )
        assert config.hosts == ["user@server1"]
        assert config.basefile == temp_basefile
        assert config.workdir == "/tmp/tea-jobs"  # default
        assert config.cleanup is True  # default

    def test_remote_config_custom_workdir(self, temp_basefile):
        """Test RemoteConfig with custom workdir."""
        config = RemoteConfig(
            hosts=["user@server1"],
            basefile=temp_basefile,
            workdir="/custom/path",
        )
        assert config.workdir == "/custom/path"

    def test_remote_config_cleanup_disabled(self, temp_basefile):
        """Test RemoteConfig with cleanup disabled."""
        config = RemoteConfig(
            hosts=["user@server1"],
            basefile=temp_basefile,
            cleanup=False,
        )
        assert config.cleanup is False

    def test_remote_config_multiple_hosts(self, temp_basefile):
        """Test RemoteConfig with multiple hosts."""
        config = RemoteConfig(
            hosts=["user@server1", "user@server2", "user@server3"],
            basefile=temp_basefile,
        )
        assert len(config.hosts) == 3

    def test_remote_config_empty_hosts_raises_error(self, temp_basefile):
        """Test that empty hosts list raises ValueError."""
        with pytest.raises(ValueError, match="At least one host must be specified"):
            RemoteConfig(
                hosts=[],
                basefile=temp_basefile,
            )

    def test_remote_config_with_yaml_path(self, temp_basefile, temp_yaml_file):
        """Test RemoteConfig with yaml_path."""
        config = RemoteConfig(
            hosts=["user@server1"],
            basefile=temp_basefile,
            yaml_path=temp_yaml_file,
        )
        assert config.yaml_path == temp_yaml_file

    def test_remote_config_with_env_vars(self, temp_basefile):
        """Test RemoteConfig with env_vars configuration."""
        config = RemoteConfig(
            hosts=["user@server1"],
            basefile=temp_basefile,
            env_vars={"mode": "ssh_env", "whitelist": ["API_KEY", "SECRET"]},
        )
        assert config.env_vars is not None
        assert config.env_vars["mode"] == "ssh_env"


# =============================================================================
# RemoteExecutor Basic Tests
# =============================================================================


class TestRemoteExecutorBasic:
    """Basic tests for RemoteExecutor."""

    def test_executor_strategy_property(self, remote_executor):
        """Test strategy property returns 'remote'."""
        assert remote_executor.strategy == "remote"

    def test_executor_max_workers_property(self, remote_executor):
        """Test max_workers returns number of hosts."""
        assert remote_executor.max_workers == 2  # Two hosts in fixture

    def test_executor_hosts_property(self, remote_executor):
        """Test hosts property returns configured hosts."""
        assert remote_executor.hosts == ["user@host1", "user@host2"]

    def test_executor_has_parallel_detection(self, remote_executor):
        """Test GNU Parallel detection."""
        # This tests if shutil.which was called - result depends on system
        assert isinstance(remote_executor.has_parallel, bool)

    def test_executor_context_manager(self, remote_executor):
        """Test executor works as context manager."""
        with remote_executor as exec:
            assert exec is remote_executor
            assert exec._executor is not None

        # After exit, executor should be cleaned up
        assert remote_executor._executor is None

    def test_executor_context_manager_without_enter_raises(self, remote_executor):
        """Test using executor without context manager raises error."""
        with pytest.raises(RuntimeError, match="must be used as a context manager"):
            remote_executor.submit(lambda: None)


# =============================================================================
# State Validation Tests
# =============================================================================


class TestRemoteExecutorValidation:
    """Tests for state validation."""

    def test_validate_state_json_serializable(self, remote_executor):
        """Test validation passes for JSON-serializable state."""
        state = {"value": 42, "name": "test", "items": [1, 2, 3]}
        # Should not raise
        remote_executor.validate_state(state)

    def test_validate_state_non_serializable_raises(self, remote_executor):
        """Test validation fails for non-JSON-serializable state."""
        state = {"func": lambda x: x}  # Functions are not JSON serializable
        with pytest.raises(ValueError, match="not JSON-serializable"):
            remote_executor.validate_state(state)

    def test_validate_state_with_flow_index(self, remote_executor):
        """Test validation includes flow index in error message."""
        state = {"obj": object()}
        with pytest.raises(ValueError, match="flow 5"):
            remote_executor.validate_state(state, flow_index=5)


# =============================================================================
# GNU Parallel Command Generation Tests
# =============================================================================


class TestGNUParallelCommandGeneration:
    """Tests for GNU Parallel command generation."""

    def test_generate_command_basic(self, remote_executor, temp_yaml_file):
        """Test basic GNU Parallel command generation."""
        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]
        config = ParallelConfig()

        cmd = remote_executor.generate_gnu_parallel_command(flows, config)

        assert "parallel" in cmd
        assert "--sshlogin" in cmd
        assert "user@host1,user@host2" in cmd
        assert "--basefile" in cmd

    def test_generate_command_with_cleanup(self, remote_executor, temp_yaml_file):
        """Test command generation with cleanup flag."""
        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]
        config = ParallelConfig()

        cmd = remote_executor.generate_gnu_parallel_command(flows, config)

        assert "--cleanup" in cmd

    def test_generate_command_without_cleanup(self, temp_basefile, temp_yaml_file):
        """Test command generation without cleanup flag."""
        config_no_cleanup = RemoteConfig(
            hosts=["user@host1"],
            basefile=temp_basefile,
            cleanup=False,
            yaml_path=temp_yaml_file,
        )
        executor = RemoteExecutor(config_no_cleanup)

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]
        config = ParallelConfig()

        cmd = executor.generate_gnu_parallel_command(flows, config)

        assert "--cleanup" not in cmd

    def test_generate_command_with_timeout(self, remote_executor, temp_yaml_file):
        """Test command generation with timeout."""
        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]
        config = ParallelConfig(timeout_seconds=30)

        cmd = remote_executor.generate_gnu_parallel_command(flows, config)

        assert "--timeout" in cmd
        assert "30" in cmd

    def test_generate_command_includes_entry_exit_points(
        self, remote_executor, temp_yaml_file
    ):
        """Test command includes entry-point and exit-point flags."""
        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]
        config = ParallelConfig()

        cmd = remote_executor.generate_gnu_parallel_command(flows, config)

        # Find the remote command string (it's the one containing cd and tea run)
        remote_cmd = None
        for part in cmd:
            if "tea" in part and "run" in part:
                remote_cmd = part
                break

        assert remote_cmd is not None, f"Remote command not found in {cmd}"
        assert "--entry-point {2}" in remote_cmd
        assert "--exit-point {3}" in remote_cmd

    def test_generate_command_without_yaml_path_raises(self, temp_basefile):
        """Test command generation raises without yaml_path."""
        config = RemoteConfig(
            hosts=["user@host1"],
            basefile=temp_basefile,
            yaml_path=None,
        )
        executor = RemoteExecutor(config)

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]

        with pytest.raises(ValueError, match="yaml_path must be set"):
            executor.generate_gnu_parallel_command(flows, ParallelConfig())


# =============================================================================
# Execute Method Tests (Mocked)
# =============================================================================


class TestRemoteExecutorExecute:
    """Tests for execute() method with mocked subprocess/SSH calls."""

    def test_execute_without_yaml_path_raises(self, temp_basefile):
        """Test execute() raises without yaml_path."""
        config = RemoteConfig(
            hosts=["user@host1"],
            basefile=temp_basefile,
            yaml_path=None,
        )
        executor = RemoteExecutor(config)

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]

        with pytest.raises(ValueError, match="yaml_path must be set"):
            executor.execute(flows, ParallelConfig())

    @patch("subprocess.run")
    def test_execute_with_gnu_parallel(self, mock_run, remote_executor, temp_yaml_file):
        """Test execute() with GNU Parallel available."""
        # Force GNU Parallel mode
        remote_executor.has_parallel = True

        # Mock subprocess result
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="",
            stderr="",
        )

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]

        # Create result file that would be expected
        with patch.object(remote_executor, "_execute_with_gnu_parallel") as mock_gnu:
            mock_gnu.return_value = [
                ParallelFlowResult.from_success(
                    branch="node_a",
                    state={"value": 1, "result": "processed"},
                    timing_ms=100.0,
                )
            ]
            results = remote_executor.execute(flows, ParallelConfig())

        assert len(results) == 1
        assert results[0].success

    def test_execute_with_ssh_fallback(self, remote_executor, temp_yaml_file):
        """Test execute() falls back to SSH when no GNU Parallel."""
        # Force SSH fallback mode
        remote_executor.has_parallel = False

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"value": 1}},
        ]

        with patch.object(remote_executor, "_execute_with_ssh_fallback") as mock_ssh:
            mock_ssh.return_value = [
                ParallelFlowResult.from_success(
                    branch="node_a",
                    state={"value": 1, "result": "processed"},
                    timing_ms=100.0,
                )
            ]

            # Need to wrap in asyncio.run since we're mocking the async method
            with patch("asyncio.run") as mock_asyncio_run:
                mock_asyncio_run.return_value = mock_ssh.return_value
                results = remote_executor.execute(flows, ParallelConfig())

            assert len(results) == 1


# =============================================================================
# SSH Async Fallback Tests (Mocked)
# =============================================================================


class TestSSHAsyncFallback:
    """Tests for async SSH fallback implementation."""

    async def test_ssh_run_basic(self, remote_executor):
        """Test _ssh_run executes SSH command."""
        mock_proc = AsyncMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = (b"output", b"")

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            result = await remote_executor._ssh_run("user@host", "ls -la")

        assert result == "output"

    async def test_ssh_run_failure_raises(self, remote_executor):
        """Test _ssh_run raises on command failure."""
        mock_proc = AsyncMock()
        mock_proc.returncode = 1
        mock_proc.communicate.return_value = (b"", b"command not found")

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            with pytest.raises(RemoteExecutionError, match="Command failed"):
                await remote_executor._ssh_run("user@host", "invalid_cmd")

    async def test_ssh_run_timeout(self, remote_executor):
        """Test _ssh_run handles timeout."""
        mock_proc = AsyncMock()
        mock_proc.kill = AsyncMock()
        mock_proc.wait = AsyncMock()

        async def slow_communicate():
            await asyncio.sleep(10)
            return b"", b""

        mock_proc.communicate = slow_communicate

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            with pytest.raises(asyncio.TimeoutError):
                await remote_executor._ssh_run("user@host", "sleep 100", timeout=0.1)

    async def test_scp_to_remote(self, remote_executor, temp_basefile):
        """Test _scp_to_remote copies files."""
        mock_proc = AsyncMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = (b"", b"")

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            await remote_executor._scp_to_remote("user@host", [temp_basefile])

        # Should have been called for the file
        assert mock_proc.communicate.called

    async def test_scp_to_remote_failure(self, remote_executor, temp_basefile):
        """Test _scp_to_remote raises on SCP failure."""
        mock_proc = AsyncMock()
        mock_proc.returncode = 1
        mock_proc.communicate.return_value = (b"", b"Permission denied")

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            with pytest.raises(SCPTransferError, match="Permission denied"):
                await remote_executor._scp_to_remote("user@host", [temp_basefile])

    async def test_scp_from_remote(self, remote_executor):
        """Test _scp_from_remote retrieves files."""
        mock_proc = AsyncMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = (b"", b"")

        with patch("asyncio.create_subprocess_exec", return_value=mock_proc):
            await remote_executor._scp_from_remote(
                "user@host", "/remote/file.json", "/local/file.json"
            )

        assert mock_proc.communicate.called


# =============================================================================
# Result Collection Tests
# =============================================================================


class TestResultCollection:
    """Tests for result collection and deserialization."""

    async def test_ssh_execute_single_success(
        self, remote_executor, temp_basefile, temp_yaml_file
    ):
        """Test successful single flow execution via SSH."""
        state = {"value": 42}

        # Mock all SSH operations
        async def mock_ssh_run(host, cmd, timeout=None):
            return ""

        async def mock_scp_to(host, files):
            pass

        async def mock_scp_from(host, remote, local):
            # Write mock result file
            with open(local, "w") as f:
                json.dump({"value": 42, "result": "processed"}, f)

        with patch.object(remote_executor, "_ssh_run", mock_ssh_run):
            with patch.object(remote_executor, "_scp_to_remote", mock_scp_to):
                with patch.object(remote_executor, "_scp_from_remote", mock_scp_from):
                    result = await remote_executor._ssh_execute_single(
                        host="user@host1",
                        entry_point="node_a",
                        exit_point="fan_in",
                        state=state,
                        config=ParallelConfig(),
                    )

        assert result.success
        assert result.branch == "node_a"
        assert result.state["result"] == "processed"
        assert result.timing_ms > 0

    async def test_ssh_execute_single_timeout(self, remote_executor):
        """Test timeout handling in single flow execution."""
        state = {"value": 42}

        async def mock_ssh_run(host, cmd, timeout=None):
            raise asyncio.TimeoutError("timed out")

        async def mock_scp_to(host, files):
            pass

        with patch.object(remote_executor, "_ssh_run", mock_ssh_run):
            with patch.object(remote_executor, "_scp_to_remote", mock_scp_to):
                result = await remote_executor._ssh_execute_single(
                    host="user@host1",
                    entry_point="node_a",
                    exit_point="fan_in",
                    state=state,
                    config=ParallelConfig(timeout_seconds=10),
                )

        assert not result.success
        assert result.timeout
        assert "timed out" in result.error.lower()

    async def test_ssh_execute_single_error(self, remote_executor):
        """Test error handling in single flow execution."""
        state = {"value": 42}

        async def mock_ssh_run(host, cmd, timeout=None):
            raise RemoteExecutionError(host, "Connection refused")

        async def mock_scp_to(host, files):
            pass

        with patch.object(remote_executor, "_ssh_run", mock_ssh_run):
            with patch.object(remote_executor, "_scp_to_remote", mock_scp_to):
                result = await remote_executor._ssh_execute_single(
                    host="user@host1",
                    entry_point="node_a",
                    exit_point="fan_in",
                    state=state,
                    config=ParallelConfig(),
                )

        assert not result.success
        assert "Connection refused" in result.error

    async def test_execute_with_ssh_fallback_multiple_flows(self, remote_executor):
        """Test SSH fallback with multiple flows."""

        async def mock_execute_single(host, entry_point, exit_point, state, config):
            return ParallelFlowResult.from_success(
                branch=entry_point,
                state={
                    "value": state["value"],
                    "result": f"processed-{state['value']}",
                },
                timing_ms=100.0,
            )

        with patch.object(remote_executor, "_ssh_execute_single", mock_execute_single):
            flows = [
                {
                    "entry_point": "node_a",
                    "exit_point": "fan_in",
                    "state": {"value": 1},
                },
                {
                    "entry_point": "node_b",
                    "exit_point": "fan_in",
                    "state": {"value": 2},
                },
                {
                    "entry_point": "node_c",
                    "exit_point": "fan_in",
                    "state": {"value": 3},
                },
            ]

            results = await remote_executor._execute_with_ssh_fallback(
                flows, ParallelConfig()
            )

        assert len(results) == 3
        assert all(r.success for r in results)
        assert results[0].state["result"] == "processed-1"
        assert results[1].state["result"] == "processed-2"
        assert results[2].state["result"] == "processed-3"


# =============================================================================
# Cleanup Tests
# =============================================================================


class TestRemoteCleanup:
    """Tests for remote file cleanup."""

    async def test_cleanup_after_success(self, remote_executor):
        """Test cleanup is called after successful execution."""
        cleanup_called = False

        async def mock_ssh_run(host, cmd, timeout=None):
            nonlocal cleanup_called
            if "rm -rf" in cmd:
                cleanup_called = True
            return ""

        async def mock_scp_to(host, files):
            pass

        async def mock_scp_from(host, remote, local):
            with open(local, "w") as f:
                json.dump({"result": "done"}, f)

        with patch.object(remote_executor, "_ssh_run", mock_ssh_run):
            with patch.object(remote_executor, "_scp_to_remote", mock_scp_to):
                with patch.object(remote_executor, "_scp_from_remote", mock_scp_from):
                    result = await remote_executor._ssh_execute_single(
                        host="user@host1",
                        entry_point="node_a",
                        exit_point="fan_in",
                        state={"value": 1},
                        config=ParallelConfig(),
                    )

        assert cleanup_called

    async def test_cleanup_disabled(self, temp_basefile, temp_yaml_file):
        """Test cleanup is not called when disabled."""
        config = RemoteConfig(
            hosts=["user@host1"],
            basefile=temp_basefile,
            cleanup=False,
            yaml_path=temp_yaml_file,
        )
        executor = RemoteExecutor(config)

        cleanup_called = False

        async def mock_ssh_run(host, cmd, timeout=None):
            nonlocal cleanup_called
            if "rm -rf" in cmd:
                cleanup_called = True
            return ""

        async def mock_scp_to(host, files):
            pass

        async def mock_scp_from(host, remote, local):
            with open(local, "w") as f:
                json.dump({"result": "done"}, f)

        with patch.object(executor, "_ssh_run", mock_ssh_run):
            with patch.object(executor, "_scp_to_remote", mock_scp_to):
                with patch.object(executor, "_scp_from_remote", mock_scp_from):
                    result = await executor._ssh_execute_single(
                        host="user@host1",
                        entry_point="node_a",
                        exit_point="fan_in",
                        state={"value": 1},
                        config=ParallelConfig(),
                    )

        assert not cleanup_called

    async def test_cleanup_failure_logged_not_raised(self, remote_executor):
        """Test cleanup failure is logged but doesn't fail the flow."""

        async def mock_ssh_run(host, cmd, timeout=None):
            if "rm -rf" in cmd:
                raise RemoteExecutionError(host, "Permission denied")
            return ""

        async def mock_scp_to(host, files):
            pass

        async def mock_scp_from(host, remote, local):
            with open(local, "w") as f:
                json.dump({"result": "done"}, f)

        with patch.object(remote_executor, "_ssh_run", mock_ssh_run):
            with patch.object(remote_executor, "_scp_to_remote", mock_scp_to):
                with patch.object(remote_executor, "_scp_from_remote", mock_scp_from):
                    result = await remote_executor._ssh_execute_single(
                        host="user@host1",
                        entry_point="node_a",
                        exit_point="fan_in",
                        state={"value": 1},
                        config=ParallelConfig(),
                    )

        # Flow should still succeed despite cleanup failure
        assert result.success


# =============================================================================
# Backend Warning Tests
# =============================================================================


class TestBackendWarnings:
    """Tests for backend configuration warnings."""

    def test_emit_backend_warnings_sqlite_ltm(self, remote_config, caplog):
        """Test warning for SQLite LTM with local path."""
        import logging

        executor = RemoteExecutor(
            remote_config,
            settings={"ltm": {"backend": "sqlite", "path": "./local.db"}},
        )

        with caplog.at_level(logging.WARNING):
            with executor:
                pass

        assert any("sqlite" in record.message.lower() for record in caplog.records)

    def test_emit_backend_warnings_memory_checkpoint(self, remote_config, caplog):
        """Test warning for memory checkpoint backend."""
        import logging

        executor = RemoteExecutor(
            remote_config,
            settings={"checkpoint": {"backend": "memory"}},
        )

        with caplog.at_level(logging.WARNING):
            with executor:
                pass

        assert any("memory" in record.message.lower() for record in caplog.records)

    def test_no_warning_for_cloud_ltm(self, remote_config, caplog):
        """Test no warning for cloud-based LTM paths."""
        import logging

        executor = RemoteExecutor(
            remote_config,
            settings={"ltm": {"backend": "sqlite", "path": "s3://bucket/data"}},
        )

        with caplog.at_level(logging.WARNING):
            with executor:
                pass

        # Should not have warning about sqlite since it's using S3
        assert not any(
            "isolated ltm state" in record.message.lower() for record in caplog.records
        )


# =============================================================================
# Round-Robin Host Assignment Tests
# =============================================================================


class TestHostAssignment:
    """Tests for round-robin host assignment."""

    def test_round_robin_host_selection(self, remote_executor):
        """Test hosts are assigned in round-robin fashion."""
        hosts_used = []

        with remote_executor:
            for _ in range(5):
                host = remote_executor._get_next_host()
                hosts_used.append(host)

        # Should cycle through hosts
        assert hosts_used[0] == "user@host1"
        assert hosts_used[1] == "user@host2"
        assert hosts_used[2] == "user@host1"
        assert hosts_used[3] == "user@host2"
        assert hosts_used[4] == "user@host1"


# =============================================================================
# submit_flow Tests
# =============================================================================


class TestSubmitFlow:
    """Tests for submit_flow method."""

    def test_submit_flow_returns_executor_result(self, remote_executor):
        """Test submit_flow returns ExecutorResult with Future."""
        task = FlowTask(
            branch="node_a",
            flow_func=lambda: None,
            state={"value": 42},
            config={},
            fan_in_node="fan_in",
            parallel_config=ParallelConfig(),
            start_time=time.time(),
        )

        with remote_executor:
            result = remote_executor.submit_flow(task, lambda *args: {"result": "ok"})

        assert isinstance(result, ExecutorResult)
        assert result.branch == "node_a"
        assert result.future is not None

    def test_submit_flow_without_context_raises(self, remote_executor):
        """Test submit_flow raises outside context manager."""
        task = FlowTask(
            branch="node_a",
            flow_func=lambda: None,
            state={"value": 42},
            config={},
            fan_in_node="fan_in",
            parallel_config=ParallelConfig(),
            start_time=time.time(),
        )

        with pytest.raises(RuntimeError, match="must be used as a context manager"):
            remote_executor.submit_flow(task, lambda *args: {"result": "ok"})


# =============================================================================
# Exception Classes Tests
# =============================================================================


class TestExceptionClasses:
    """Tests for custom exception classes."""

    def test_remote_execution_error(self):
        """Test RemoteExecutionError contains host and message."""
        error = RemoteExecutionError(
            "user@host1", "Connection refused", "stderr output"
        )
        assert error.host == "user@host1"
        assert error.stderr == "stderr output"
        assert "user@host1" in str(error)
        assert "Connection refused" in str(error)

    def test_scp_transfer_error(self):
        """Test SCPTransferError contains file path."""
        error = SCPTransferError("user@host1", "/path/to/file", "Permission denied")
        assert error.host == "user@host1"
        assert error.file_path == "/path/to/file"
        assert "Permission denied" in str(error)


# =============================================================================
# Integration Test (Skipped in CI)
# =============================================================================


@pytest.mark.skip(
    reason="Requires real SSH access - run manually for integration testing"
)
class TestRemoteExecutorIntegration:
    """Integration tests requiring real SSH access."""

    def test_real_remote_execution(self):
        """Test actual remote execution (requires SSH setup)."""
        pass
