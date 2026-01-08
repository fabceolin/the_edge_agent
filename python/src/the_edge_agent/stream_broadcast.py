"""
Stream broadcasting via tee and FIFOs.

This module provides broadcast functionality for streaming data from a single
producer to multiple consumers using named pipes (FIFOs) and the tee command.

Features:
- Create named FIFOs for multi-consumer support
- Orchestrate tee process for stream duplication
- Python fallback when tee is unavailable
- Automatic cleanup of FIFOs on completion
- Context manager support for safe resource management
"""

import asyncio
import atexit
import os
import shutil
import tempfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Dict


@dataclass
class BroadcastChannel:
    """
    A channel that broadcasts to multiple consumers via FIFOs.

    Attributes:
        name: Unique name for this broadcast
        source_fd: File descriptor to read from (producer's stdout)
        consumer_count: Number of consumers to broadcast to
        fifos: List of FIFO paths for consumers
        tee_process: The tee subprocess (set after start_broadcast)
    """

    name: str
    source_fd: int
    consumer_count: int
    fifos: List[Path] = field(default_factory=list)
    tee_process: Optional[asyncio.subprocess.Process] = None
    _forward_task: Optional[asyncio.Task] = None


class TeeOrchestrator:
    """
    Orchestrate stream broadcasting via tee and FIFOs.

    Creates named pipes (FIFOs) for each consumer and uses `tee` command
    to duplicate the source stream to all consumers.

    Example:
        >>> async with TeeOrchestrator() as orchestrator:
        ...     fifos = orchestrator.create_broadcast("output", source_fd, 3)
        ...     # Start consumers reading from fifos
        ...     await orchestrator.start_broadcast("output")
        ...     # All 3 consumers receive same data
    """

    # System limit for open files (conservative default)
    MAX_FIFOS = 256

    def __init__(self, work_dir: Optional[Path] = None):
        """
        Initialize the orchestrator.

        Args:
            work_dir: Directory for FIFO files. If None, creates temp dir.
        """
        if work_dir is None:
            self.work_dir = Path(tempfile.mkdtemp(prefix="tea_broadcast_"))
            self._owns_work_dir = True
        else:
            self.work_dir = work_dir
            self.work_dir.mkdir(parents=True, exist_ok=True)
            self._owns_work_dir = False

        self.broadcasts: Dict[str, BroadcastChannel] = {}
        self._cleanup_registered = False

        # Register atexit cleanup
        if not self._cleanup_registered:
            atexit.register(self._atexit_cleanup)
            self._cleanup_registered = True

    def _atexit_cleanup(self) -> None:
        """Cleanup called at process exit."""
        try:
            self.cleanup_all()
        except Exception:
            pass

    def create_broadcast(
        self, name: str, source_fd: int, consumer_count: int
    ) -> List[Path]:
        """
        Create FIFOs for broadcasting and prepare tee process.

        Args:
            name: Unique name for this broadcast
            source_fd: File descriptor to read from (producer's stdout)
            consumer_count: Number of consumers to broadcast to

        Returns:
            List of FIFO paths for consumers to read from

        Raises:
            ValueError: If consumer_count exceeds MAX_FIFOS or < 2
            OSError: If FIFO creation fails
        """
        if consumer_count > self.MAX_FIFOS:
            raise ValueError(
                f"Consumer count {consumer_count} exceeds maximum {self.MAX_FIFOS}. "
                f"Consider reducing parallel branches or using a different pattern."
            )

        if consumer_count < 2:
            raise ValueError(
                f"Broadcast requires at least 2 consumers, got {consumer_count}. "
                f"Use direct pipe for single consumer."
            )

        # Create FIFOs
        fifos: List[Path] = []
        for i in range(consumer_count):
            fifo_path = self.work_dir / f"{name}_consumer_{i}.fifo"
            try:
                os.mkfifo(fifo_path)
                fifos.append(fifo_path)
            except OSError as e:
                # Cleanup already created FIFOs
                for f in fifos:
                    try:
                        f.unlink(missing_ok=True)
                    except OSError:
                        pass
                raise OSError(f"Failed to create FIFO {fifo_path}: {e}")

        # Store broadcast info
        self.broadcasts[name] = BroadcastChannel(
            name=name, source_fd=source_fd, consumer_count=consumer_count, fifos=fifos
        )

        return fifos

    async def start_broadcast(self, name: str) -> None:
        """
        Start the tee process for a broadcast.

        Must be called after consumers have opened their FIFOs for reading,
        otherwise tee will block.

        Args:
            name: Broadcast name from create_broadcast()

        Raises:
            KeyError: If broadcast name not found
            RuntimeError: If broadcast already started
        """
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")

        broadcast = self.broadcasts[name]

        if broadcast.tee_process is not None:
            raise RuntimeError(f"Broadcast {name} already started")

        # Check if tee is available
        if not check_tee_available():
            # Fall back to Python implementation
            await self.start_broadcast_with_python(name)
            return

        # Build tee command
        # tee writes to all FIFOs except last, last gets stdout
        tee_targets = [str(f) for f in broadcast.fifos[:-1]]
        last_fifo = broadcast.fifos[-1]

        # Open last FIFO for writing (tee stdout goes here)
        last_fifo_fd = os.open(str(last_fifo), os.O_WRONLY | os.O_NONBLOCK)

        try:
            broadcast.tee_process = await asyncio.create_subprocess_exec(
                "tee",
                *tee_targets,
                stdin=broadcast.source_fd,
                stdout=last_fifo_fd,
                stderr=asyncio.subprocess.PIPE,
            )
        finally:
            os.close(last_fifo_fd)

    async def start_broadcast_with_python(self, name: str) -> None:
        """
        Start broadcast using pure Python (no tee command).

        Useful when tee is not available or for more control.

        Args:
            name: Broadcast name from create_broadcast()

        Raises:
            KeyError: If broadcast name not found
        """
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")

        broadcast = self.broadcasts[name]

        # Open all FIFOs for writing (non-blocking to avoid deadlock)
        fifo_fds: List[int] = []
        try:
            for fifo in broadcast.fifos:
                fd = os.open(str(fifo), os.O_WRONLY | os.O_NONBLOCK)
                fifo_fds.append(fd)
        except OSError as e:
            # Cleanup opened fds
            for fd in fifo_fds:
                try:
                    os.close(fd)
                except OSError:
                    pass
            raise OSError(f"Failed to open FIFO for writing: {e}")

        # Read from source, write to all FIFOs
        async def forward():
            loop = asyncio.get_event_loop()
            try:
                while True:
                    # Read chunk from source
                    try:
                        data = await loop.run_in_executor(
                            None, os.read, broadcast.source_fd, 65536
                        )
                    except OSError:
                        break

                    if not data:
                        break

                    # Write to all FIFOs
                    for fd in fifo_fds:
                        try:
                            os.write(fd, data)
                        except OSError:
                            # Consumer closed, ignore
                            pass
            finally:
                for fd in fifo_fds:
                    try:
                        os.close(fd)
                    except OSError:
                        pass

        # Start forwarding task
        broadcast._forward_task = asyncio.create_task(forward())

    def get_fifo_paths(self, name: str) -> List[Path]:
        """
        Get FIFO paths for a broadcast's consumers.

        Args:
            name: Broadcast name

        Returns:
            List of FIFO paths

        Raises:
            KeyError: If broadcast name not found
        """
        if name not in self.broadcasts:
            raise KeyError(f"Unknown broadcast: {name}")
        return self.broadcasts[name].fifos

    async def stop_broadcast(self, name: str) -> None:
        """
        Stop a broadcast and its tee process.

        Args:
            name: Broadcast name to stop
        """
        if name not in self.broadcasts:
            return

        broadcast = self.broadcasts[name]

        # Stop tee process
        if broadcast.tee_process:
            broadcast.tee_process.terminate()
            try:
                await asyncio.wait_for(broadcast.tee_process.wait(), timeout=5.0)
            except asyncio.TimeoutError:
                broadcast.tee_process.kill()
                await broadcast.tee_process.wait()
            broadcast.tee_process = None

        # Cancel forward task
        if broadcast._forward_task:
            broadcast._forward_task.cancel()
            try:
                await broadcast._forward_task
            except asyncio.CancelledError:
                pass
            broadcast._forward_task = None

    def cleanup_broadcast(self, name: str) -> None:
        """
        Remove FIFOs for a broadcast.

        Args:
            name: Broadcast name to cleanup
        """
        if name not in self.broadcasts:
            return

        broadcast = self.broadcasts[name]

        for fifo in broadcast.fifos:
            try:
                fifo.unlink(missing_ok=True)
            except OSError:
                pass

        del self.broadcasts[name]

    def cleanup_all(self) -> None:
        """Remove all FIFOs and the work directory."""
        for name in list(self.broadcasts.keys()):
            self.cleanup_broadcast(name)

        # Remove work directory if we created it and it's empty
        try:
            if self._owns_work_dir and self.work_dir.exists():
                shutil.rmtree(self.work_dir, ignore_errors=True)
        except OSError:
            pass

    async def cleanup_all_async(self) -> None:
        """Async cleanup: stop all broadcasts, remove FIFOs."""
        for name in list(self.broadcasts.keys()):
            await self.stop_broadcast(name)
            self.cleanup_broadcast(name)

        # Remove work directory
        try:
            if self._owns_work_dir and self.work_dir.exists():
                shutil.rmtree(self.work_dir, ignore_errors=True)
        except OSError:
            pass

    def __enter__(self) -> "TeeOrchestrator":
        """Enter context manager."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        """Exit context manager and cleanup."""
        self.cleanup_all()

    async def __aenter__(self) -> "TeeOrchestrator":
        """Enter async context manager."""
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb) -> None:
        """Exit async context manager and cleanup."""
        await self.cleanup_all_async()


def check_tee_available() -> bool:
    """
    Check if tee command is available.

    Returns:
        True if tee is available, False otherwise
    """
    return shutil.which("tee") is not None


def check_fifo_support() -> bool:
    """
    Check if the platform supports FIFOs (named pipes).

    Returns:
        True if platform supports FIFOs (Unix-like), False otherwise (Windows)
    """
    import sys

    return sys.platform != "win32" and hasattr(os, "mkfifo")
