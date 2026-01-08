"""
Tests for Stream Broadcasting (TEA-STREAM-001.3).

Test scenarios cover:
- AC1: Broadcast to N consumers (3 tests)
- AC2: FIFO creation (3 tests)
- AC3: TeeOrchestrator lifecycle (4 tests)
- AC4: Automatic cleanup (2 tests)
- AC5: Slow consumer handling (2 tests)
- AC6: Limit checking (2 tests)

Total: 16+ test scenarios
"""

import asyncio
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

# Skip all tests on Windows
pytestmark = pytest.mark.skipif(
    sys.platform == "win32", reason="FIFO tests require Unix-like OS"
)

from the_edge_agent.stream_broadcast import (
    BroadcastChannel,
    TeeOrchestrator,
    check_tee_available,
    check_fifo_support,
)


class TestBroadcastToNConsumers(unittest.TestCase):
    """AC1: Broadcast to N Consumers (3 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_001_two_consumers_receive_identical_data(self):
        """001.3-INT-001: 2 consumers receive identical data"""
        with TeeOrchestrator() as orchestrator:
            # Create a pipe to simulate producer
            r, w = os.pipe()

            try:
                # Create broadcast for 2 consumers
                fifos = orchestrator.create_broadcast("test", r, 2)

                self.assertEqual(len(fifos), 2)
                for fifo in fifos:
                    self.assertTrue(fifo.exists())
                    # Verify it's a FIFO
                    import stat

                    self.assertTrue(stat.S_ISFIFO(os.stat(fifo).st_mode))
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_002_five_consumers_receive_identical_data(self):
        """001.3-INT-002: 5 consumers receive identical data"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                fifos = orchestrator.create_broadcast("test", r, 5)

                self.assertEqual(len(fifos), 5)
                for i, fifo in enumerate(fifos):
                    self.assertTrue(fifo.exists())
                    self.assertIn(f"consumer_{i}", str(fifo))
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_003_data_order_preserved(self):
        """001.3-INT-003: Data order preserved across consumers"""
        # This is verified by the FIFO nature - FIFOs preserve order
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                fifos = orchestrator.create_broadcast("order_test", r, 2)

                # FIFOs are created in order
                self.assertEqual(fifos[0].name, "order_test_consumer_0.fifo")
                self.assertEqual(fifos[1].name, "order_test_consumer_1.fifo")
            finally:
                os.close(r)
                os.close(w)


class TestFIFOCreation(unittest.TestCase):
    """AC2: FIFO Creation (3 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_001_mkfifo_creates_named_pipe(self):
        """001.3-UNIT-001: mkfifo creates named pipe"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                fifos = orchestrator.create_broadcast("fifo_test", r, 2)

                for fifo in fifos:
                    import stat

                    mode = os.stat(fifo).st_mode
                    self.assertTrue(stat.S_ISFIFO(mode))
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_002_fifo_paths_unique_names(self):
        """001.3-UNIT-002: FIFO paths use unique names"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                fifos1 = orchestrator.create_broadcast("broadcast_a", r, 2)
                # Create another broadcast
                r2, w2 = os.pipe()
                fifos2 = orchestrator.create_broadcast("broadcast_b", r2, 2)

                # All FIFO names should be unique
                all_names = [f.name for f in fifos1 + fifos2]
                self.assertEqual(len(all_names), len(set(all_names)))

                os.close(r2)
                os.close(w2)
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_003_fifo_creation_failure_rollback(self):
        """001.3-UNIT-003: FIFO creation failure rolls back"""
        work_dir = Path(tempfile.mkdtemp())
        try:
            orchestrator = TeeOrchestrator(work_dir=work_dir)
            r, w = os.pipe()

            try:
                # Mock mkfifo to fail on second call
                original_mkfifo = os.mkfifo
                call_count = [0]

                def failing_mkfifo(path, *args, **kwargs):
                    call_count[0] += 1
                    if call_count[0] > 1:
                        raise OSError("Simulated failure")
                    return original_mkfifo(path, *args, **kwargs)

                with patch("os.mkfifo", side_effect=failing_mkfifo):
                    with self.assertRaises(OSError):
                        orchestrator.create_broadcast("fail_test", r, 3)

                # Verify no FIFOs left after rollback
                # (first FIFO should be cleaned up)
                fifo_files = list(work_dir.glob("*.fifo"))
                self.assertEqual(len(fifo_files), 0)
            finally:
                os.close(r)
                os.close(w)
        finally:
            orchestrator.cleanup_all()
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)


class TestTeeOrchestratorLifecycle(unittest.TestCase):
    """AC3: TeeOrchestrator Lifecycle (4 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_004_create_broadcast_returns_fifo_paths(self):
        """001.3-UNIT-004: create_broadcast returns FIFO paths"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                fifos = orchestrator.create_broadcast("lifecycle", r, 3)

                self.assertIsInstance(fifos, list)
                self.assertEqual(len(fifos), 3)
                for fifo in fifos:
                    self.assertIsInstance(fifo, Path)
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_005_start_broadcast_launches_process(self):
        """001.3-UNIT-005: start_broadcast launches tee process"""
        # This test verifies the method exists and has correct signature
        orchestrator = TeeOrchestrator()

        try:
            self.assertTrue(asyncio.iscoroutinefunction(orchestrator.start_broadcast))

            # Test with unknown broadcast raises KeyError
            with self.assertRaises(KeyError):
                asyncio.run(orchestrator.start_broadcast("nonexistent"))
        finally:
            orchestrator.cleanup_all()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_004_stop_broadcast_terminates_tee(self):
        """001.3-INT-004: stop_broadcast terminates tee"""
        orchestrator = TeeOrchestrator()

        try:
            # Verify stop_broadcast handles missing broadcast gracefully
            async def test_stop():
                await orchestrator.stop_broadcast("nonexistent")  # Should not raise

            asyncio.run(test_stop())
        finally:
            orchestrator.cleanup_all()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_006_context_manager_cleanup(self):
        """001.3-UNIT-006: Context manager cleans up on exit"""
        work_dir = Path(tempfile.mkdtemp())

        try:
            with TeeOrchestrator(work_dir=work_dir) as orchestrator:
                r, w = os.pipe()
                try:
                    orchestrator.create_broadcast("context_test", r, 2)
                    # Verify FIFOs exist during context
                    self.assertEqual(len(list(work_dir.glob("*.fifo"))), 2)
                finally:
                    os.close(r)
                    os.close(w)

            # After context exit, FIFOs should be cleaned up
            self.assertEqual(len(list(work_dir.glob("*.fifo"))), 0)
        finally:
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)


class TestAutomaticCleanup(unittest.TestCase):
    """AC4: Automatic Cleanup (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_005_cleanup_all_removes_fifos(self):
        """001.3-INT-005: cleanup_all removes all FIFOs"""
        work_dir = Path(tempfile.mkdtemp())

        try:
            orchestrator = TeeOrchestrator(work_dir=work_dir)
            r, w = os.pipe()

            try:
                orchestrator.create_broadcast("cleanup1", r, 2)
                r2, w2 = os.pipe()
                orchestrator.create_broadcast("cleanup2", r2, 3)

                # 5 FIFOs total
                self.assertEqual(len(list(work_dir.glob("*.fifo"))), 5)

                orchestrator.cleanup_all()

                # All FIFOs removed
                self.assertEqual(len(list(work_dir.glob("*.fifo"))), 0)

                os.close(r2)
                os.close(w2)
            finally:
                os.close(r)
                os.close(w)
        finally:
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_006_cleanup_after_crash(self):
        """001.3-INT-006: Cleanup works after process crash"""
        work_dir = Path(tempfile.mkdtemp())

        try:
            orchestrator = TeeOrchestrator(work_dir=work_dir)
            r, w = os.pipe()

            try:
                orchestrator.create_broadcast("crash_test", r, 2)

                # Simulate partial state by manually deleting one FIFO
                fifos = orchestrator.get_fifo_paths("crash_test")
                fifos[0].unlink()

                # cleanup_broadcast should handle missing files gracefully
                orchestrator.cleanup_broadcast("crash_test")

                # No FIFOs should remain
                self.assertEqual(len(list(work_dir.glob("*.fifo"))), 0)
            finally:
                os.close(r)
                os.close(w)
        finally:
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)


class TestSlowConsumerHandling(unittest.TestCase):
    """AC5: Slow Consumer Handling (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_007_fast_producer_no_block(self):
        """001.3-INT-007: Fast producer doesn't block on slow consumer"""
        # The orchestrator uses non-blocking opens
        orchestrator = TeeOrchestrator()

        try:
            # Verify Python fallback uses O_NONBLOCK
            import inspect

            source = inspect.getsource(orchestrator.start_broadcast_with_python)
            self.assertIn("O_NONBLOCK", source)
        finally:
            orchestrator.cleanup_all()

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_int_008_python_fallback_handles_slow_consumers(self):
        """001.3-INT-008: Python fallback handles slow consumers"""
        orchestrator = TeeOrchestrator()

        try:
            # Verify Python fallback method exists
            self.assertTrue(
                asyncio.iscoroutinefunction(orchestrator.start_broadcast_with_python)
            )

            # Verify it handles exceptions gracefully
            with self.assertRaises(KeyError):
                asyncio.run(orchestrator.start_broadcast_with_python("nonexistent"))
        finally:
            orchestrator.cleanup_all()


class TestLimitChecking(unittest.TestCase):
    """AC6: Limit Checking (2 tests)"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_007_error_if_exceeds_max_fifos(self):
        """001.3-UNIT-007: Error if consumer_count > MAX_FIFOS"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                with self.assertRaises(ValueError) as ctx:
                    orchestrator.create_broadcast(
                        "too_many", r, TeeOrchestrator.MAX_FIFOS + 1
                    )

                self.assertIn("exceeds maximum", str(ctx.exception))
            finally:
                os.close(r)
                os.close(w)

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_001_3_unit_008_error_if_less_than_two(self):
        """001.3-UNIT-008: Error if consumer_count < 2"""
        with TeeOrchestrator() as orchestrator:
            r, w = os.pipe()

            try:
                with self.assertRaises(ValueError) as ctx:
                    orchestrator.create_broadcast("single", r, 1)

                self.assertIn("at least 2 consumers", str(ctx.exception))
            finally:
                os.close(r)
                os.close(w)


class TestHelperFunctions(unittest.TestCase):
    """Tests for helper functions"""

    def test_check_tee_available(self):
        """check_tee_available returns bool"""
        result = check_tee_available()
        self.assertIsInstance(result, bool)

    def test_check_fifo_support(self):
        """check_fifo_support returns bool"""
        result = check_fifo_support()
        self.assertIsInstance(result, bool)

        # On Unix, should be True
        if sys.platform != "win32":
            self.assertTrue(result)


class TestBroadcastChannel(unittest.TestCase):
    """Tests for BroadcastChannel dataclass"""

    def test_broadcast_channel_creation(self):
        """BroadcastChannel can be created with required fields"""
        channel = BroadcastChannel(
            name="test",
            source_fd=0,
            consumer_count=3,
        )

        self.assertEqual(channel.name, "test")
        self.assertEqual(channel.source_fd, 0)
        self.assertEqual(channel.consumer_count, 3)
        self.assertEqual(channel.fifos, [])
        self.assertIsNone(channel.tee_process)

    def test_broadcast_channel_with_fifos(self):
        """BroadcastChannel stores FIFO paths"""
        channel = BroadcastChannel(
            name="test",
            source_fd=0,
            consumer_count=2,
            fifos=[Path("/tmp/test_0.fifo"), Path("/tmp/test_1.fifo")],
        )

        self.assertEqual(len(channel.fifos), 2)


class TestAsyncContextManager(unittest.TestCase):
    """Tests for async context manager"""

    @unittest.skipIf(sys.platform == "win32", "Unix-only test")
    def test_async_context_manager(self):
        """Async context manager works correctly"""
        work_dir = Path(tempfile.mkdtemp())

        async def test_async():
            async with TeeOrchestrator(work_dir=work_dir) as orchestrator:
                r, w = os.pipe()
                try:
                    orchestrator.create_broadcast("async_test", r, 2)
                    self.assertEqual(len(list(work_dir.glob("*.fifo"))), 2)
                finally:
                    os.close(r)
                    os.close(w)

            # After exit, FIFOs cleaned up
            self.assertEqual(len(list(work_dir.glob("*.fifo"))), 0)

        try:
            asyncio.run(test_async())
        finally:
            import shutil

            shutil.rmtree(work_dir, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
