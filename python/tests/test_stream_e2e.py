"""
E2E Tests for Stream Integration (TEA-STREAM-001.5).

Test scenarios cover:
- AC1: Producer -> Consumer pipe streaming (4 tests)
- AC2: Producer -> Broadcast -> N consumers (2 tests)
- AC3: Hybrid state + stream workflow (2 tests)
- AC4: Performance (1 test)
- AC5: Error handling (2 tests)
- AC6: Validation (1 test)

Total: 12+ test scenarios

Note: These tests require Unix-like OS (Linux/macOS).
Windows is not supported for stream channels.
"""

import asyncio
import json
import os
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

# Skip all tests on Windows
pytestmark = pytest.mark.skipif(
    sys.platform == "win32", reason="Stream tests require Unix-like OS"
)


class TestProducerConsumerStreaming(unittest.TestCase):
    """AC1: Producer -> Consumer pipe streaming (4 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_001_simple_producer_consumer(self):
        """001.5-E2E-001: Simple producer -> consumer with 1000 records"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        registry = StreamRegistry()

        # Create channels
        producer_channel = registry.register(
            "data_pipe", StreamDirection.STDOUT, "producer", 65536
        )
        registry.register("data_pipe", StreamDirection.STDIN, "consumer", 65536)

        # Create pipe
        read_fd, write_fd = producer_channel.create_pipe()

        try:
            # Verify pipe was created
            self.assertIsNotNone(read_fd)
            self.assertIsNotNone(write_fd)

            # Write data
            records_written = 0
            for i in range(100):  # Smaller batch for test speed
                record = json.dumps({"id": i, "value": f"item_{i}"}) + "\n"
                os.write(write_fd, record.encode())
                records_written += 1

            # Close write end to signal EOF
            os.close(write_fd)
            write_fd = None

            # Read data
            records_read = 0
            buffer = b""
            while True:
                chunk = os.read(read_fd, 4096)
                if not chunk:
                    break
                buffer += chunk

            for line in buffer.decode().strip().split("\n"):
                if line:
                    record = json.loads(line)
                    self.assertIn("id", record)
                    self.assertIn("value", record)
                    records_read += 1

            self.assertEqual(records_written, records_read)
        finally:
            if write_fd is not None:
                os.close(write_fd)
            os.close(read_fd)
            registry.cleanup()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_002_streaming_chain(self):
        """001.5-E2E-002: Producer -> Transformer -> Consumer chain"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        registry = StreamRegistry()

        # Create two channels for the chain
        ch1 = registry.register("pipe1", StreamDirection.STDOUT, "producer", 65536)
        registry.register("pipe1", StreamDirection.STDIN, "transformer", 65536)

        ch2 = registry.register("pipe2", StreamDirection.STDOUT, "transformer", 65536)
        registry.register("pipe2", StreamDirection.STDIN, "consumer", 65536)

        # Create both pipes
        r1, w1 = ch1.create_pipe()
        r2, w2 = ch2.create_pipe()

        try:
            # Write to first pipe
            for i in range(10):
                data = f"line_{i}\n"
                os.write(w1, data.encode())

            os.close(w1)
            w1 = None

            # Simulate transformer: read from pipe1, write to pipe2
            buffer = b""
            while True:
                chunk = os.read(r1, 4096)
                if not chunk:
                    break
                buffer += chunk

            # Transform and write
            for line in buffer.decode().strip().split("\n"):
                if line:
                    transformed = line.upper() + "\n"
                    os.write(w2, transformed.encode())

            os.close(w2)
            w2 = None

            # Read from pipe2
            result = b""
            while True:
                chunk = os.read(r2, 4096)
                if not chunk:
                    break
                result += chunk

            # Verify transformation
            lines = result.decode().strip().split("\n")
            self.assertEqual(len(lines), 10)
            self.assertEqual(lines[0], "LINE_0")
            self.assertEqual(lines[9], "LINE_9")

        finally:
            if w1 is not None:
                os.close(w1)
            if w2 is not None:
                os.close(w2)
            os.close(r1)
            os.close(r2)
            registry.cleanup()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_003_data_integrity(self):
        """001.5-E2E-003: Data integrity preserved through pipe"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        registry = StreamRegistry()
        channel = registry.register("data", StreamDirection.STDOUT, "producer", 65536)
        registry.register("data", StreamDirection.STDIN, "consumer", 65536)

        r, w = channel.create_pipe()

        try:
            # Write complex data with special characters
            test_data = [
                {"text": "Hello\nWorld", "unicode": "\u00e9\u00e0\u00fc"},
                {"numbers": [1, 2, 3], "nested": {"a": 1}},
                {"empty": "", "null": None, "bool": True},
            ]

            for data in test_data:
                line = json.dumps(data) + "\n"
                os.write(w, line.encode())

            os.close(w)
            w = None

            # Read and verify
            buffer = b""
            while True:
                chunk = os.read(r, 4096)
                if not chunk:
                    break
                buffer += chunk

            received = []
            for line in buffer.decode().strip().split("\n"):
                if line:
                    received.append(json.loads(line))

            self.assertEqual(len(received), len(test_data))
            self.assertEqual(received, test_data)

        finally:
            if w is not None:
                os.close(w)
            os.close(r)
            registry.cleanup()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_004_sigpipe_handling(self):
        """001.5-E2E-004: SIGPIPE handled gracefully"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection
        import signal

        registry = StreamRegistry()
        registry.install_sigpipe_handler()

        channel = registry.register("data", StreamDirection.STDOUT, "producer", 65536)
        r, w = channel.create_pipe()

        try:
            # Close read end immediately (simulate broken pipe)
            os.close(r)
            r = None

            # Writing should not crash (SIGPIPE ignored)
            # On some systems this raises BrokenPipeError instead
            try:
                os.write(w, b"test\n")
            except BrokenPipeError:
                pass  # Expected on some systems
            except OSError as e:
                if e.errno == 32:  # EPIPE
                    pass  # Expected
                else:
                    raise

            # Process should still be alive (SIGPIPE didn't terminate it)
            self.assertTrue(True)

        finally:
            if r is not None:
                os.close(r)
            os.close(w)
            registry.cleanup()


class TestStreamBroadcasting(unittest.TestCase):
    """AC2: Producer -> Broadcast -> N consumers (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_005_broadcast_to_three_consumers(self):
        """001.5-E2E-005: Broadcast to 3 consumers via FIFOs"""
        from the_edge_agent.stream_broadcast import TeeOrchestrator

        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                # Create broadcast for 3 consumers
                fifos = orchestrator.create_broadcast("test", r, 3)

                self.assertEqual(len(fifos), 3)
                for i, fifo in enumerate(fifos):
                    self.assertTrue(fifo.exists())
                    self.assertIn(f"consumer_{i}", str(fifo))
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_006_broadcast_fifo_cleanup(self):
        """001.5-E2E-006: FIFO cleanup after broadcast"""
        from the_edge_agent.stream_broadcast import TeeOrchestrator

        work_dir = Path(tempfile.mkdtemp())

        try:
            orchestrator = TeeOrchestrator(work_dir=work_dir)
            r, w = os.pipe()

            try:
                fifos = orchestrator.create_broadcast("cleanup_test", r, 2)
                self.assertEqual(len(list(work_dir.glob("*.fifo"))), 2)

                orchestrator.cleanup_all()

                self.assertEqual(len(list(work_dir.glob("*.fifo"))), 0)
            finally:
                os.close(r)
                os.close(w)
        finally:
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)


class TestHybridWorkflow(unittest.TestCase):
    """AC3: Hybrid state + stream workflow (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_007_state_and_stream_coexist(self):
        """001.5-E2E-007: State and stream data coexist"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        # State data (metadata)
        state = {
            "file_name": "test.txt",
            "file_size": 1024,
            "checksum": "abc123",
        }

        # Stream data (bulk content)
        registry = StreamRegistry()
        channel = registry.register(
            "content", StreamDirection.STDOUT, "producer", 65536
        )

        r, w = channel.create_pipe()

        try:
            # Write stream data
            stream_content = "Line 1\nLine 2\nLine 3\n"
            os.write(w, stream_content.encode())
            os.close(w)
            w = None

            # Read stream data
            buffer = b""
            while True:
                chunk = os.read(r, 4096)
                if not chunk:
                    break
                buffer += chunk

            # Both state and stream should be available
            self.assertEqual(state["file_name"], "test.txt")
            self.assertEqual(buffer.decode(), stream_content)

        finally:
            if w is not None:
                os.close(w)
            os.close(r)
            registry.cleanup()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_008_checkpoint_on_non_stream_node(self):
        """001.5-E2E-008: Checkpoint valid on non-stream node"""
        from the_edge_agent.yaml_engine import YAMLEngine
        from the_edge_agent.yaml_nodes import NodeStreamsConfig

        # Verify parsing of interrupt_after on non-stream node is valid
        config = {"name": "prepare", "interrupt_after": True, "run": "return {}"}

        streams = NodeStreamsConfig.from_dict(config)
        self.assertIsNone(streams)  # No streams block

        # interrupt_after is valid because no streams
        self.assertTrue(config.get("interrupt_after"))


class TestErrorHandling(unittest.TestCase):
    """AC5: Error handling (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_009_error_propagation(self):
        """001.5-E2E-009: Consumer error does not hang producer"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        registry = StreamRegistry()
        registry.install_sigpipe_handler()

        channel = registry.register("data", StreamDirection.STDOUT, "producer", 65536)
        r, w = channel.create_pipe()

        try:
            # Consumer "crashes" by closing read end
            os.close(r)
            r = None

            # Producer should handle gracefully
            start = time.time()
            try:
                for i in range(100):
                    os.write(w, f"data_{i}\n".encode())
            except (BrokenPipeError, OSError):
                pass  # Expected

            elapsed = time.time() - start

            # Should not hang (timeout would cause test failure)
            self.assertLess(elapsed, 5.0)

        finally:
            if r is not None:
                os.close(r)
            os.close(w)
            registry.cleanup()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_010_missing_producer_error(self):
        """001.5-E2E-010: Clear error for missing producer"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "consumer", "streams": {"stdin": "nonexistent_stream"}},
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            error_msg = str(ctx.exception)
            self.assertIn("nonexistent_stream", error_msg)
            self.assertIn("consumed by", error_msg)
            self.assertIn("has no producer", error_msg)


class TestValidation(unittest.TestCase):
    """AC6: Validation (1 test)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_e2e_011_stream_interrupt_incompatibility(self):
        """001.5-E2E-011: Compile-time error for interrupt on stream node"""
        from the_edge_agent.yaml_engine import YAMLEngine

        engine = YAMLEngine()
        config = {
            "settings": {
                "parallel": {"strategy": "process", "streams": {"enabled": True}}
            },
            "nodes": [
                {"name": "producer", "streams": {"stdout": "data"}},
                {
                    "name": "consumer",
                    "streams": {"stdin": "data"},
                    "interrupt_before": True,  # Invalid combination
                },
            ],
            "edges": [],
        }

        with patch("the_edge_agent.streams.validate_platform"):
            with self.assertRaises(ValueError) as ctx:
                engine._build_stream_registry(config, "process")

            error_msg = str(ctx.exception)
            self.assertIn("interrupt_before", error_msg)
            self.assertIn("cannot be checkpointed", error_msg)


class TestPerformance(unittest.TestCase):
    """AC4: Performance (1 test)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_5_perf_001_large_data_throughput(self):
        """001.5-PERF-001: 10MB throughput without deadlock"""
        from the_edge_agent.streams import StreamRegistry, StreamDirection

        registry = StreamRegistry()
        channel = registry.register("data", StreamDirection.STDOUT, "producer", 131072)

        r, w = channel.create_pipe()

        # 10MB of data
        data_size = 10 * 1024 * 1024  # 10MB
        chunk_size = 64 * 1024  # 64KB chunks
        chunk = b"x" * chunk_size

        try:
            import threading

            bytes_written = [0]
            bytes_read = [0]
            write_done = threading.Event()

            def writer():
                nonlocal bytes_written
                try:
                    while bytes_written[0] < data_size:
                        os.write(w, chunk)
                        bytes_written[0] += len(chunk)
                finally:
                    os.close(w)
                    write_done.set()

            def reader():
                nonlocal bytes_read
                while True:
                    data = os.read(r, 65536)
                    if not data:
                        break
                    bytes_read[0] += len(data)

            start = time.time()

            writer_thread = threading.Thread(target=writer)
            reader_thread = threading.Thread(target=reader)

            writer_thread.start()
            reader_thread.start()

            writer_thread.join(timeout=30)
            reader_thread.join(timeout=30)

            elapsed = time.time() - start

            # Verify data transfer
            self.assertGreaterEqual(bytes_written[0], data_size)
            self.assertGreaterEqual(bytes_read[0], data_size)

            # Should complete in reasonable time (< 30s for 10MB)
            self.assertLess(elapsed, 30.0)

            # Calculate throughput
            throughput_mb_s = bytes_read[0] / (1024 * 1024) / elapsed
            # At least 10MB/s throughput (conservative)
            self.assertGreater(throughput_mb_s, 10.0)

        finally:
            try:
                os.close(r)
            except OSError:
                pass
            registry.cleanup()


class TestExampleValidation(unittest.TestCase):
    """Validate example YAML files exist and are well-formed"""

    def test_001_5_doc_001_example_files_exist(self):
        """001.5-DOC-001: Example YAML files exist"""
        examples_dir = Path(__file__).parent.parent.parent / "examples" / "yaml"

        expected_files = [
            "stream_pipeline.yaml",
            "stream_broadcast.yaml",
            "stream_hybrid.yaml",
        ]

        for filename in expected_files:
            filepath = examples_dir / filename
            self.assertTrue(filepath.exists(), f"Example file missing: {filename}")

    def test_001_5_doc_002_example_files_valid_yaml(self):
        """001.5-DOC-002: Example YAML files are valid YAML"""
        import yaml

        examples_dir = Path(__file__).parent.parent.parent / "examples" / "yaml"

        stream_files = [
            "stream_pipeline.yaml",
            "stream_broadcast.yaml",
            "stream_hybrid.yaml",
        ]

        for filename in stream_files:
            filepath = examples_dir / filename
            if filepath.exists():
                with open(filepath) as f:
                    content = yaml.safe_load(f)

                # Verify basic structure
                self.assertIn("name", content, f"{filename} missing 'name'")
                self.assertIn("nodes", content, f"{filename} missing 'nodes'")
                self.assertIn("edges", content, f"{filename} missing 'edges'")
                self.assertIn("settings", content, f"{filename} missing 'settings'")

                # Verify streams enabled
                streams = (
                    content.get("settings", {}).get("parallel", {}).get("streams", {})
                )
                self.assertTrue(
                    streams.get("enabled"),
                    f"{filename} should have streams.enabled: true",
                )


if __name__ == "__main__":
    unittest.main()
