"""
Stream channel infrastructure for Unix pipe streaming.

This module provides named stream channels for inter-process communication
using OS pipes. It supports:
- Named channels with stdin/stdout/stderr direction
- Multiple consumers (fan-out preparation)
- Configurable buffer sizes
- SIGPIPE handling for graceful broken pipe handling
"""

from dataclasses import dataclass, field
from typing import Optional, List, Dict, Tuple
from enum import Enum
import os
import signal
import sys


class StreamDirection(Enum):
    """Direction of stream data flow."""

    STDIN = "stdin"
    STDOUT = "stdout"
    STDERR = "stderr"


class PlatformError(Exception):
    """Raised when stream channels are used on unsupported platform."""

    pass


@dataclass
class StreamChannel:
    """
    Named stream channel for inter-process communication.

    Attributes:
        name: Unique channel name
        direction: stdin/stdout/stderr indicating flow direction
        buffer_size: Pipe buffer size in bytes (default 64KB)
        read_fd: Read file descriptor (set after create_pipe())
        write_fd: Write file descriptor (set after create_pipe())
        consumers: List of node names consuming this channel
    """

    name: str
    direction: StreamDirection
    buffer_size: int = 65536  # 64KB default (OS pipe buffer)

    # Runtime state (set after create_pipe())
    read_fd: Optional[int] = None
    write_fd: Optional[int] = None
    consumers: List[str] = field(default_factory=list)
    _closed: bool = False

    def create_pipe(self) -> Tuple[int, int]:
        """
        Create OS pipe, return (read_fd, write_fd).

        Optionally resizes pipe buffer on Linux if buffer_size differs from default.

        Returns:
            Tuple of (read_fd, write_fd) file descriptors

        Raises:
            RuntimeError: If channel has been closed
        """
        if self._closed:
            raise RuntimeError(f"Channel '{self.name}' has been closed")

        r, w = os.pipe()

        # Resize pipe buffer on Linux (F_SETPIPE_SZ = 1031)
        if self.buffer_size != 65536:
            try:
                import fcntl

                fcntl.fcntl(w, 1031, self.buffer_size)
            except (ImportError, OSError, PermissionError):
                pass  # Not supported or insufficient permissions, use default

        self.read_fd, self.write_fd = r, w
        return r, w

    def close(self) -> None:
        """Close pipe file descriptors. Idempotent - safe to call multiple times."""
        if self._closed:
            return

        if self.read_fd is not None:
            try:
                os.close(self.read_fd)
            except OSError:
                pass  # Already closed
            self.read_fd = None

        if self.write_fd is not None:
            try:
                os.close(self.write_fd)
            except OSError:
                pass  # Already closed
            self.write_fd = None

        self._closed = True

    def add_consumer(self, node_name: str) -> None:
        """
        Register a consumer node for this channel.

        Args:
            node_name: Name of the node consuming this channel
        """
        if node_name not in self.consumers:
            self.consumers.append(node_name)

    @property
    def consumer_count(self) -> int:
        """Number of registered consumers."""
        return len(self.consumers)

    @property
    def is_broadcast(self) -> bool:
        """True if channel has multiple consumers."""
        return len(self.consumers) > 1

    def __del__(self):
        """Ensure file descriptors are closed on garbage collection."""
        self.close()


@dataclass
class StreamRegistry:
    """
    Registry of named stream channels for a workflow execution.

    Manages channel lifecycle: registration, pipe creation, and cleanup.
    """

    channels: Dict[str, StreamChannel] = field(default_factory=dict)
    _sigpipe_handled: bool = False

    def register(
        self,
        name: str,
        direction: StreamDirection,
        node_name: Optional[str] = None,
        buffer_size: int = 65536,
    ) -> StreamChannel:
        """
        Register a new stream channel or add consumer to existing.

        Args:
            name: Unique channel name
            direction: stdin/stdout/stderr
            node_name: Node registering this channel
            buffer_size: Pipe buffer size in bytes

        Returns:
            The StreamChannel (new or existing)
        """
        if name in self.channels:
            # Channel exists - add consumer if stdin
            channel = self.channels[name]
            if direction == StreamDirection.STDIN and node_name:
                channel.add_consumer(node_name)
            return channel

        # Create new channel
        channel = StreamChannel(name=name, direction=direction, buffer_size=buffer_size)
        if node_name:
            channel.add_consumer(node_name)

        self.channels[name] = channel
        return channel

    def get(self, name: str) -> Optional[StreamChannel]:
        """Get channel by name, or None if not found."""
        return self.channels.get(name)

    def get_producers(self) -> List[StreamChannel]:
        """Get all channels with stdout/stderr direction (producers)."""
        return [
            c
            for c in self.channels.values()
            if c.direction in (StreamDirection.STDOUT, StreamDirection.STDERR)
        ]

    def get_consumers(self) -> List[StreamChannel]:
        """Get all channels with stdin direction (consumers)."""
        return [
            c for c in self.channels.values() if c.direction == StreamDirection.STDIN
        ]

    def create_all_pipes(self) -> None:
        """Create OS pipes for all registered channels."""
        for channel in self.channels.values():
            if channel.read_fd is None:
                channel.create_pipe()

    def cleanup(self) -> None:
        """Close all channels and release resources."""
        for channel in self.channels.values():
            channel.close()
        self.channels.clear()

    def install_sigpipe_handler(self) -> None:
        """
        Install SIGPIPE handler to prevent crashes on broken pipes.

        Uses SIG_IGN to ignore SIGPIPE, allowing write operations to
        return EPIPE error instead of terminating the process.
        """
        if self._sigpipe_handled:
            return

        # SIGPIPE only exists on Unix-like systems
        if hasattr(signal, "SIGPIPE"):
            signal.signal(signal.SIGPIPE, signal.SIG_IGN)
        self._sigpipe_handled = True

    def validate(self) -> List[str]:
        """
        Validate channel configuration.

        Returns:
            List of validation error messages (empty if valid)
        """
        errors = []

        # Check that all stdin channels have a matching stdout producer
        producer_names = {
            c.name
            for c in self.channels.values()
            if c.direction == StreamDirection.STDOUT
        }

        for channel in self.channels.values():
            if channel.direction == StreamDirection.STDIN:
                if channel.name not in producer_names:
                    errors.append(
                        f"Stream '{channel.name}' is consumed but never produced. "
                        f"Add a node with 'streams.stdout: {channel.name}'"
                    )

        return errors


def validate_platform() -> None:
    """
    Validate that the current platform supports stream channels.

    Raises:
        PlatformError: If platform is not supported (Windows)
    """
    if sys.platform == "win32":
        raise PlatformError(
            "Stream channels require Unix-like OS (Linux/macOS). "
            "Windows is not supported. Use parallel_strategy: thread instead."
        )
