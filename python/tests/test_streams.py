"""
Tests for Stream Channel Infrastructure (TEA-STREAM-001.1).

Test scenarios cover:
- AC1: StreamChannel dataclass (4 tests)
- AC2: StreamRegistry (4 tests)
- AC3: Multiple consumers (3 tests)
- AC4: Channel lifecycle (3 tests)
- AC5: SIGPIPE handling (2 tests)
- AC6: Buffer size (2 tests)

Total: 18 test scenarios
"""

import os
import sys
import signal
import unittest
from unittest.mock import patch, MagicMock

from the_edge_agent.streams import (
    StreamChannel,
    StreamDirection,
    StreamRegistry,
    PlatformError,
    validate_platform,
)


class TestStreamChannelDataclass(unittest.TestCase):
    """AC1: StreamChannel Dataclass (4 tests)"""

    def test_001_1_unit_001_required_fields(self):
        """001.1-UNIT-001: StreamChannel has required fields (name, direction, buffer_size)"""
        channel = StreamChannel(
            name="test_channel", direction=StreamDirection.STDOUT, buffer_size=32768
        )

        self.assertEqual(channel.name, "test_channel")
        self.assertEqual(channel.direction, StreamDirection.STDOUT)
        self.assertEqual(channel.buffer_size, 32768)

    def test_001_1_unit_002_default_buffer_size(self):
        """001.1-UNIT-002: Default buffer_size is 65536 (64KB)"""
        channel = StreamChannel(name="test_channel", direction=StreamDirection.STDOUT)

        self.assertEqual(channel.buffer_size, 65536)

    def test_001_1_unit_003_stream_direction_enum(self):
        """001.1-UNIT-003: StreamDirection enum has STDIN, STDOUT, STDERR"""
        self.assertEqual(StreamDirection.STDIN.value, "stdin")
        self.assertEqual(StreamDirection.STDOUT.value, "stdout")
        self.assertEqual(StreamDirection.STDERR.value, "stderr")

        # Verify all expected members exist
        directions = {d.value for d in StreamDirection}
        self.assertEqual(directions, {"stdin", "stdout", "stderr"})

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_unit_004_create_pipe_returns_valid_fds(self):
        """001.1-UNIT-004: create_pipe() returns valid file descriptors"""
        channel = StreamChannel(name="test_channel", direction=StreamDirection.STDOUT)

        try:
            read_fd, write_fd = channel.create_pipe()

            # Verify fds are integers
            self.assertIsInstance(read_fd, int)
            self.assertIsInstance(write_fd, int)

            # Verify fds are valid (can be used for operations)
            self.assertGreaterEqual(read_fd, 0)
            self.assertGreaterEqual(write_fd, 0)

            # Verify fds are stored in channel
            self.assertEqual(channel.read_fd, read_fd)
            self.assertEqual(channel.write_fd, write_fd)
        finally:
            channel.close()


class TestStreamRegistry(unittest.TestCase):
    """AC2: StreamRegistry (4 tests)"""

    def test_001_1_unit_005_register_creates_new_channel(self):
        """001.1-UNIT-005: register() creates new channel"""
        registry = StreamRegistry()

        channel = registry.register(
            name="output", direction=StreamDirection.STDOUT, node_name="producer"
        )

        self.assertIsInstance(channel, StreamChannel)
        self.assertEqual(channel.name, "output")
        self.assertEqual(channel.direction, StreamDirection.STDOUT)
        self.assertIn("output", registry.channels)

    def test_001_1_unit_006_register_returns_existing_channel(self):
        """001.1-UNIT-006: register() returns existing channel if name exists"""
        registry = StreamRegistry()

        # Create first channel
        channel1 = registry.register(
            name="data_pipe", direction=StreamDirection.STDOUT, node_name="producer"
        )

        # Register same name - should return existing
        channel2 = registry.register(
            name="data_pipe", direction=StreamDirection.STDIN, node_name="consumer"
        )

        self.assertIs(channel1, channel2)
        self.assertEqual(len(registry.channels), 1)

    def test_001_1_unit_007_get_returns_none_for_unknown(self):
        """001.1-UNIT-007: get() returns None for unknown channel"""
        registry = StreamRegistry()

        result = registry.get("nonexistent")

        self.assertIsNone(result)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_unit_008_cleanup_closes_all_channels(self):
        """001.1-UNIT-008: cleanup() closes all channels"""
        registry = StreamRegistry()

        # Create and register channels with pipes
        channel1 = registry.register("ch1", StreamDirection.STDOUT)
        channel2 = registry.register("ch2", StreamDirection.STDOUT)

        channel1.create_pipe()
        channel2.create_pipe()

        # Store fds for verification
        ch1_read = channel1.read_fd
        ch1_write = channel1.write_fd

        # Cleanup
        registry.cleanup()

        # Verify channels dict is cleared
        self.assertEqual(len(registry.channels), 0)

        # Verify fds are closed (attempting to close again should fail silently
        # or the fd should be invalid)
        # Note: we can't directly verify closure, but we can verify cleanup happened
        self.assertTrue(channel1._closed)
        self.assertTrue(channel2._closed)


class TestMultipleConsumers(unittest.TestCase):
    """AC3: Multiple Consumers (3 tests)"""

    def test_001_1_unit_009_add_consumer_adds_to_list(self):
        """001.1-UNIT-009: add_consumer() adds node to consumers list"""
        channel = StreamChannel(name="broadcast", direction=StreamDirection.STDOUT)

        channel.add_consumer("node_a")
        channel.add_consumer("node_b")

        self.assertIn("node_a", channel.consumers)
        self.assertIn("node_b", channel.consumers)
        self.assertEqual(len(channel.consumers), 2)

    def test_001_1_unit_010_is_broadcast_multiple_consumers(self):
        """001.1-UNIT-010: is_broadcast returns True for multiple consumers"""
        channel = StreamChannel(name="broadcast", direction=StreamDirection.STDOUT)

        # Single consumer
        channel.add_consumer("node_a")
        self.assertFalse(channel.is_broadcast)

        # Multiple consumers
        channel.add_consumer("node_b")
        self.assertTrue(channel.is_broadcast)

    def test_001_1_unit_011_consumer_count_reflects_registered(self):
        """001.1-UNIT-011: consumer_count reflects registered consumers"""
        channel = StreamChannel(name="multi", direction=StreamDirection.STDOUT)

        self.assertEqual(channel.consumer_count, 0)

        channel.add_consumer("a")
        self.assertEqual(channel.consumer_count, 1)

        channel.add_consumer("b")
        self.assertEqual(channel.consumer_count, 2)

        # Duplicate should not increase count
        channel.add_consumer("a")
        self.assertEqual(channel.consumer_count, 2)


class TestChannelLifecycle(unittest.TestCase):
    """AC4: Channel Lifecycle (3 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_int_001_full_lifecycle(self):
        """001.1-INT-001: Full lifecycle: create → write → read → close"""
        channel = StreamChannel(name="lifecycle_test", direction=StreamDirection.STDOUT)

        try:
            # Create
            read_fd, write_fd = channel.create_pipe()
            self.assertIsNotNone(read_fd)
            self.assertIsNotNone(write_fd)

            # Write
            test_data = b"Hello, pipe!"
            bytes_written = os.write(write_fd, test_data)
            self.assertEqual(bytes_written, len(test_data))

            # Read
            data_read = os.read(read_fd, 1024)
            self.assertEqual(data_read, test_data)
        finally:
            # Close
            channel.close()

        # Verify closed state
        self.assertTrue(channel._closed)
        self.assertIsNone(channel.read_fd)
        self.assertIsNone(channel.write_fd)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_int_002_data_integrity(self):
        """001.1-INT-002: Data written to write_fd readable from read_fd"""
        channel = StreamChannel(name="integrity_test", direction=StreamDirection.STDOUT)

        try:
            channel.create_pipe()

            # Write various data types
            test_cases = [
                b"simple text",
                b"\x00\x01\x02\x03",  # Binary data
                b"line1\nline2\nline3",  # Multi-line
                b"a" * 1000,  # Larger data
            ]

            for test_data in test_cases:
                os.write(channel.write_fd, test_data)
                received = os.read(channel.read_fd, len(test_data) + 100)
                self.assertEqual(received, test_data)
        finally:
            channel.close()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_int_003_close_idempotent(self):
        """001.1-INT-003: close() is idempotent (can call multiple times)"""
        channel = StreamChannel(
            name="idempotent_test", direction=StreamDirection.STDOUT
        )

        channel.create_pipe()

        # Multiple close calls should not raise
        channel.close()
        channel.close()
        channel.close()

        # State should be closed
        self.assertTrue(channel._closed)
        self.assertIsNone(channel.read_fd)
        self.assertIsNone(channel.write_fd)


class TestSIGPIPEHandling(unittest.TestCase):
    """AC5: SIGPIPE Handling (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_unit_012_sigpipe_handler_sets_sig_ign(self):
        """001.1-UNIT-012: install_sigpipe_handler() sets SIG_IGN"""
        registry = StreamRegistry()

        # Save original handler
        original_handler = signal.getsignal(signal.SIGPIPE)

        try:
            registry.install_sigpipe_handler()

            # Verify SIG_IGN is set
            current_handler = signal.getsignal(signal.SIGPIPE)
            self.assertEqual(current_handler, signal.SIG_IGN)

            # Verify flag is set
            self.assertTrue(registry._sigpipe_handled)

            # Second call should be no-op
            registry.install_sigpipe_handler()
            self.assertTrue(registry._sigpipe_handled)
        finally:
            # Restore original handler
            signal.signal(signal.SIGPIPE, original_handler)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_int_004_broken_pipe_no_crash(self):
        """001.1-INT-004: Broken pipe doesn't crash process"""
        registry = StreamRegistry()
        registry.install_sigpipe_handler()

        # Save original handler
        original_handler = signal.getsignal(signal.SIGPIPE)

        channel = StreamChannel(
            name="broken_pipe_test", direction=StreamDirection.STDOUT
        )

        try:
            channel.create_pipe()

            # Close read end to simulate broken pipe
            os.close(channel.read_fd)
            channel.read_fd = None

            # Writing should raise BrokenPipeError, not crash with SIGPIPE
            with self.assertRaises(BrokenPipeError):
                # Write enough data to trigger the error
                os.write(channel.write_fd, b"x" * 100000)
        finally:
            channel.close()
            # Restore original handler
            signal.signal(signal.SIGPIPE, original_handler)


class TestBufferSize(unittest.TestCase):
    """AC6: Buffer Size (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_unit_013_custom_buffer_size_fcntl(self):
        """001.1-UNIT-013: Custom buffer_size passed to fcntl"""
        channel = StreamChannel(
            name="custom_buffer",
            direction=StreamDirection.STDOUT,
            buffer_size=131072,  # 128KB
        )

        try:
            # Patch fcntl.fcntl at the module level
            with patch("fcntl.fcntl") as mock_fcntl:
                channel.create_pipe()

                # Verify fcntl was called with F_SETPIPE_SZ (1031)
                mock_fcntl.assert_called_once()
                args = mock_fcntl.call_args[0]
                self.assertEqual(args[1], 1031)  # F_SETPIPE_SZ
                self.assertEqual(args[2], 131072)  # buffer size
        finally:
            channel.close()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_1_unit_014_fcntl_failure_graceful_fallback(self):
        """001.1-UNIT-014: Graceful fallback when fcntl fails"""
        channel = StreamChannel(
            name="fcntl_fail", direction=StreamDirection.STDOUT, buffer_size=131072
        )

        try:
            # Patch fcntl.fcntl at the module level
            with patch("fcntl.fcntl") as mock_fcntl:
                # Simulate permission error
                mock_fcntl.side_effect = PermissionError("Operation not permitted")

                # Should not raise, should fall back gracefully
                read_fd, write_fd = channel.create_pipe()

                # Verify pipe was still created successfully
                self.assertIsNotNone(read_fd)
                self.assertIsNotNone(write_fd)
        finally:
            channel.close()


class TestValidatePlatform(unittest.TestCase):
    """Platform validation tests"""

    def test_validate_platform_windows_raises(self):
        """validate_platform() raises PlatformError on Windows"""
        with patch.object(sys, "platform", "win32"):
            with self.assertRaises(PlatformError) as ctx:
                validate_platform()

            self.assertIn("Windows is not supported", str(ctx.exception))
            self.assertIn("parallel_strategy: thread", str(ctx.exception))

    def test_validate_platform_linux_passes(self):
        """validate_platform() passes on Linux"""
        with patch.object(sys, "platform", "linux"):
            # Should not raise
            validate_platform()

    def test_validate_platform_darwin_passes(self):
        """validate_platform() passes on macOS"""
        with patch.object(sys, "platform", "darwin"):
            # Should not raise
            validate_platform()


class TestStreamRegistryValidation(unittest.TestCase):
    """Registry validation tests"""

    def test_validate_detects_orphan_consumer(self):
        """validate() detects stdin without matching stdout producer"""
        registry = StreamRegistry()

        # Only register a consumer (stdin) without producer
        registry.register("orphan_stream", StreamDirection.STDIN, "consumer_node")

        errors = registry.validate()

        self.assertEqual(len(errors), 1)
        self.assertIn("orphan_stream", errors[0])
        self.assertIn("consumed but never produced", errors[0])

    def test_validate_passes_with_producer_consumer_pair(self):
        """validate() passes when stdin has matching stdout"""
        registry = StreamRegistry()

        # Register producer first
        registry.register("data_pipe", StreamDirection.STDOUT, "producer_node")
        # Register consumer
        registry.register("data_pipe", StreamDirection.STDIN, "consumer_node")

        errors = registry.validate()

        self.assertEqual(len(errors), 0)

    def test_get_producers_returns_stdout_channels(self):
        """get_producers() returns channels with stdout/stderr direction"""
        registry = StreamRegistry()

        registry.register("stdout_ch", StreamDirection.STDOUT)
        registry.register("stderr_ch", StreamDirection.STDERR)
        registry.register("stdin_ch", StreamDirection.STDIN)

        producers = registry.get_producers()

        self.assertEqual(len(producers), 2)
        names = {c.name for c in producers}
        self.assertEqual(names, {"stdout_ch", "stderr_ch"})

    def test_get_consumers_returns_stdin_channels(self):
        """get_consumers() returns channels with stdin direction"""
        registry = StreamRegistry()

        registry.register("stdout_ch", StreamDirection.STDOUT)
        registry.register("stdin_ch1", StreamDirection.STDIN)
        registry.register("stdin_ch2", StreamDirection.STDIN)

        consumers = registry.get_consumers()

        self.assertEqual(len(consumers), 2)
        names = {c.name for c in consumers}
        self.assertEqual(names, {"stdin_ch1", "stdin_ch2"})


class TestStreamChannelEdgeCases(unittest.TestCase):
    """Edge case tests"""

    def test_create_pipe_on_closed_channel_raises(self):
        """create_pipe() raises RuntimeError if channel is closed"""
        channel = StreamChannel(name="closed_channel", direction=StreamDirection.STDOUT)
        channel._closed = True

        with self.assertRaises(RuntimeError) as ctx:
            channel.create_pipe()

        self.assertIn("has been closed", str(ctx.exception))

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_create_all_pipes(self):
        """create_all_pipes() creates pipes for all channels"""
        registry = StreamRegistry()

        registry.register("ch1", StreamDirection.STDOUT)
        registry.register("ch2", StreamDirection.STDOUT)

        registry.create_all_pipes()

        try:
            for channel in registry.channels.values():
                self.assertIsNotNone(channel.read_fd)
                self.assertIsNotNone(channel.write_fd)
        finally:
            registry.cleanup()


if __name__ == "__main__":
    unittest.main()
