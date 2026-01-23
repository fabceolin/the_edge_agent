# Story TEA-RALPHY-001.8: Progress Reporting & Notifications

## Status
Deferred

> **Note:** Deferred in favor of [TEA-RALPHY-002](./TEA-RALPHY-002-minimal-ralphy.md) which takes a simpler approach using tmux output via `tea from dot --tmux`.

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Story

**As a** workflow operator,
**I want** real-time progress reporting and completion notifications,
**So that** I know when tasks complete without watching the terminal.

## Acceptance Criteria

1. Progress callback hook for node completion
2. Percentage tracking based on completed tasks
3. Desktop notification on workflow completion
4. Support macOS, Linux, Windows notification APIs
5. Optional webhook for external integrations
6. Tmux pane management for real-time command output visibility
7. Route individual TEA node execution output to dedicated tmux panes

## Tasks / Subtasks

- [ ] Create `ProgressTracker` class (AC: 1, 2)
  - [ ] Create `python/src/the_edge_agent/tracking/progress_tracker.py`
  - [ ] Implement `ProgressTracker` with `total_nodes`, `completed_nodes`, `current_node`
  - [ ] Calculate percentage: `(completed / total) * 100`
  - [ ] Emit progress events to registered callbacks
  - [ ] Store progress in `state["_progress"]`
- [ ] Integrate with workflow execution (AC: 1)
  - [ ] Modify `python/src/the_edge_agent/stategraph.py`
  - [ ] Call `progress_tracker.on_node_start(node_name)` before node execution
  - [ ] Call `progress_tracker.on_node_complete(node_name)` after node execution
  - [ ] Handle parallel node tracking (count as single unit or individual)
- [ ] Implement `notify.send` action (AC: 3, 4)
  - [ ] Create `python/src/the_edge_agent/actions/notify_actions.py`
  - [ ] Detect platform via `sys.platform`
  - [ ] macOS implementation:
    - [ ] Use `osascript -e 'display notification "message" with title "TEA"'`
    - [ ] Fallback to `terminal-notifier` if available
  - [ ] Linux implementation:
    - [ ] Use `notify-send "TEA" "message"`
    - [ ] Check for `notify-send` availability
  - [ ] Windows implementation:
    - [ ] Use PowerShell `New-BurntToastNotification` or `[Windows.UI.Notifications]`
    - [ ] Fallback to `msg` command
  - [ ] Add `sound` parameter (boolean, default: True)
  - [ ] Add `urgency` parameter (low, normal, critical)
- [ ] Implement `notify.webhook` action (AC: 5)
  - [ ] Create webhook sender in `notify_actions.py`
  - [ ] Support POST with JSON payload
  - [ ] Support custom headers (for auth tokens)
  - [ ] Retry on failure with exponential backoff
  - [ ] Support Slack, Discord, Teams webhook formats
- [ ] Add progress callback registration (AC: 1)
  - [ ] Create `settings.progress.callbacks` configuration
  - [ ] Support multiple callback types: `webhook`, `file`, `state`
  - [ ] File callback: append progress to log file
- [ ] Implement `tmux.create_pane` action (AC: 6)
  - [ ] Create `python/src/the_edge_agent/actions/tmux_actions.py`
  - [ ] Detect if running inside tmux session (`$TMUX` env var)
  - [ ] Create new pane with `tmux split-window`
  - [ ] Support horizontal (`-h`) and vertical (`-v`) splits
  - [ ] Name panes with `select-pane -T "pane_name"`
  - [ ] Return pane ID for subsequent operations
  - [ ] Handle case when not in tmux (graceful degradation)
- [ ] Implement `tmux.send_command` action (AC: 7)
  - [ ] Send commands to specific pane via `tmux send-keys -t {pane_id}`
  - [ ] Support streaming output to pane
  - [ ] Clear pane before new command (`tmux send-keys -t {pane_id} "clear" Enter`)
- [ ] Implement `tmux.watch_pane` action (AC: 6, 7)
  - [ ] Capture pane output via `tmux capture-pane -p -t {pane_id}`
  - [ ] Monitor for completion markers
  - [ ] Return output content
- [ ] Implement `tmux.close_pane` action (AC: 6)
  - [ ] Close pane via `tmux kill-pane -t {pane_id}`
  - [ ] Handle already-closed panes gracefully
- [ ] Implement `TmuxPaneManager` class (AC: 6, 7)
  - [ ] Create `python/src/the_edge_agent/tracking/tmux_manager.py`
  - [ ] Manage pane lifecycle for parallel node execution
  - [ ] Create pane per parallel task
  - [ ] Route node output to assigned pane
  - [ ] Cleanup panes on workflow completion
  - [ ] Support `settings.tmux.enabled` configuration
  - [ ] Support `settings.tmux.layout` (tiled, even-horizontal, even-vertical)
- [ ] Integrate with workflow execution (AC: 7)
  - [ ] Modify `python/src/the_edge_agent/stategraph.py`
  - [ ] Create pane on node start (if tmux enabled)
  - [ ] Route `llm.call` shell provider output to pane
  - [ ] Show progress header in each pane
- [ ] Add unit tests
  - [ ] Create `python/tests/test_notify_actions.py`
  - [ ] Mock subprocess calls for platform notifications
  - [ ] Test webhook with mock server
  - [ ] Test progress percentage calculation
  - [ ] Create `python/tests/test_tmux_actions.py`
  - [ ] Mock tmux subprocess calls
  - [ ] Test pane creation and cleanup
  - [ ] Test command routing

## Dev Notes

### Source Tree

```
python/src/the_edge_agent/
├── tracking/
│   ├── __init__.py
│   ├── token_tracker.py         # From 001.7
│   ├── cost_config.py           # From 001.7
│   ├── progress_tracker.py      # NEW: Progress tracking
│   └── tmux_manager.py          # NEW: Tmux pane lifecycle management
├── actions/
│   ├── llm_actions.py
│   ├── notify_actions.py        # NEW: Notification actions
│   └── tmux_actions.py          # NEW: Tmux pane actions
└── stategraph.py                # MODIFY: Progress hooks + tmux integration
```

### Progress Tracker Implementation

```python
# python/src/the_edge_agent/tracking/progress_tracker.py
from dataclasses import dataclass, field
from typing import Callable, List, Optional, Dict, Any
from datetime import datetime
from enum import Enum

class NodeStatus(Enum):
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"

@dataclass
class NodeProgress:
    name: str
    status: NodeStatus = NodeStatus.PENDING
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    error: Optional[str] = None

@dataclass
class ProgressTracker:
    total_nodes: int
    nodes: Dict[str, NodeProgress] = field(default_factory=dict)
    callbacks: List[Callable[["ProgressTracker"], None]] = field(default_factory=list)

    @property
    def completed_count(self) -> int:
        return sum(1 for n in self.nodes.values() if n.status == NodeStatus.COMPLETED)

    @property
    def percentage(self) -> float:
        if self.total_nodes == 0:
            return 100.0
        return (self.completed_count / self.total_nodes) * 100

    @property
    def current_node(self) -> Optional[str]:
        for name, node in self.nodes.items():
            if node.status == NodeStatus.RUNNING:
                return name
        return None

    def on_node_start(self, name: str) -> None:
        self.nodes[name] = NodeProgress(
            name=name,
            status=NodeStatus.RUNNING,
            started_at=datetime.utcnow()
        )
        self._notify_callbacks()

    def on_node_complete(self, name: str, error: Optional[str] = None) -> None:
        if name in self.nodes:
            self.nodes[name].status = NodeStatus.FAILED if error else NodeStatus.COMPLETED
            self.nodes[name].completed_at = datetime.utcnow()
            self.nodes[name].error = error
        self._notify_callbacks()

    def _notify_callbacks(self) -> None:
        for callback in self.callbacks:
            try:
                callback(self)
            except Exception:
                pass  # Don't let callback errors break execution

    def to_state(self) -> Dict[str, Any]:
        return {
            "total": self.total_nodes,
            "completed": self.completed_count,
            "percentage": round(self.percentage, 1),
            "current_node": self.current_node,
            "nodes": {
                name: {
                    "status": node.status.value,
                    "started_at": node.started_at.isoformat() if node.started_at else None,
                    "completed_at": node.completed_at.isoformat() if node.completed_at else None,
                }
                for name, node in self.nodes.items()
            }
        }
```

### Notify Action Implementation

```python
# python/src/the_edge_agent/actions/notify_actions.py
import subprocess
import sys
import json
from typing import Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)

def notify_send(
    message: str,
    title: str = "TEA Workflow",
    sound: bool = True,
    urgency: str = "normal",  # low, normal, critical
    **kwargs
) -> Dict[str, Any]:
    """
    Send desktop notification cross-platform.

    Args:
        message: Notification body text
        title: Notification title
        sound: Play notification sound
        urgency: Notification priority level

    Returns:
        {"success": bool, "platform": str, "error": str|None}
    """
    platform = sys.platform
    result = {"success": False, "platform": platform, "error": None}

    try:
        if platform == "darwin":  # macOS
            # Try osascript first (built-in)
            sound_cmd = 'with sound name "default"' if sound else ""
            script = f'display notification "{message}" with title "{title}" {sound_cmd}'
            subprocess.run(
                ["osascript", "-e", script],
                check=True,
                capture_output=True
            )
            result["success"] = True

        elif platform == "linux":
            # Use notify-send (libnotify)
            urgency_map = {"low": "low", "normal": "normal", "critical": "critical"}
            cmd = ["notify-send", "-u", urgency_map.get(urgency, "normal"), title, message]
            subprocess.run(cmd, check=True, capture_output=True)
            result["success"] = True

        elif platform == "win32":  # Windows
            # Use PowerShell toast notification
            ps_script = f'''
            [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] | Out-Null
            $template = [Windows.UI.Notifications.ToastTemplateType]::ToastText02
            $xml = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent($template)
            $text = $xml.GetElementsByTagName("text")
            $text[0].AppendChild($xml.CreateTextNode("{title}")) | Out-Null
            $text[1].AppendChild($xml.CreateTextNode("{message}")) | Out-Null
            $toast = [Windows.UI.Notifications.ToastNotification]::new($xml)
            [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier("TEA").Show($toast)
            '''
            subprocess.run(
                ["powershell", "-Command", ps_script],
                check=True,
                capture_output=True
            )
            result["success"] = True

        else:
            result["error"] = f"Unsupported platform: {platform}"

    except subprocess.CalledProcessError as e:
        result["error"] = f"Command failed: {e.stderr.decode() if e.stderr else str(e)}"
    except FileNotFoundError as e:
        result["error"] = f"Notification tool not found: {e}"
    except Exception as e:
        result["error"] = str(e)

    return result


def notify_webhook(
    url: str,
    payload: Dict[str, Any],
    headers: Optional[Dict[str, str]] = None,
    format: str = "json",  # json, slack, discord
    timeout: int = 30,
    **kwargs
) -> Dict[str, Any]:
    """
    Send webhook notification.

    Args:
        url: Webhook URL
        payload: Data to send
        headers: Optional HTTP headers
        format: Payload format (json, slack, discord)
        timeout: Request timeout in seconds

    Returns:
        {"success": bool, "status_code": int, "error": str|None}
    """
    import requests

    result = {"success": False, "status_code": None, "error": None}

    # Format payload for specific services
    if format == "slack":
        payload = {"text": payload.get("message", str(payload))}
    elif format == "discord":
        payload = {"content": payload.get("message", str(payload))}

    try:
        response = requests.post(
            url,
            json=payload,
            headers=headers or {"Content-Type": "application/json"},
            timeout=timeout
        )
        result["status_code"] = response.status_code
        result["success"] = response.status_code < 400

        if not result["success"]:
            result["error"] = f"HTTP {response.status_code}: {response.text[:200]}"

    except requests.RequestException as e:
        result["error"] = str(e)

    return result


# Action registry
def register_actions(registry: dict, engine=None):
    registry["notify.send"] = notify_send
    registry["notify.webhook"] = notify_webhook
```

### Tmux Actions Implementation

```python
# python/src/the_edge_agent/actions/tmux_actions.py
import subprocess
import os
from typing import Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)


def is_tmux_available() -> bool:
    """Check if we're running inside a tmux session."""
    return os.environ.get("TMUX") is not None


def tmux_create_pane(
    name: str,
    direction: str = "vertical",  # vertical, horizontal
    size: int = 50,  # percentage
    command: Optional[str] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create a new tmux pane for displaying command output.

    Args:
        name: Pane title/identifier
        direction: Split direction (vertical=-v, horizontal=-h)
        size: Pane size as percentage
        command: Optional command to run in the new pane

    Returns:
        {"success": bool, "pane_id": str, "error": str|None}
    """
    result = {"success": False, "pane_id": None, "error": None}

    if not is_tmux_available():
        result["error"] = "Not running inside tmux session"
        return result

    try:
        split_flag = "-v" if direction == "vertical" else "-h"

        # Create new pane and capture its ID
        cmd = ["tmux", "split-window", split_flag, "-p", str(size), "-P", "-F", "#{pane_id}"]
        if command:
            cmd.extend([command])

        proc = subprocess.run(cmd, capture_output=True, text=True, check=True)
        pane_id = proc.stdout.strip()

        # Name the pane for identification
        subprocess.run(
            ["tmux", "select-pane", "-t", pane_id, "-T", name],
            check=True,
            capture_output=True
        )

        result["success"] = True
        result["pane_id"] = pane_id

    except subprocess.CalledProcessError as e:
        result["error"] = f"tmux command failed: {e.stderr}"
    except Exception as e:
        result["error"] = str(e)

    return result


def tmux_send_command(
    pane_id: str,
    command: str,
    clear_first: bool = True,
    **kwargs
) -> Dict[str, Any]:
    """
    Send a command to a specific tmux pane.

    Args:
        pane_id: Target pane ID
        command: Command to execute
        clear_first: Clear pane before sending command

    Returns:
        {"success": bool, "error": str|None}
    """
    result = {"success": False, "error": None}

    if not is_tmux_available():
        result["error"] = "Not running inside tmux session"
        return result

    try:
        if clear_first:
            subprocess.run(
                ["tmux", "send-keys", "-t", pane_id, "clear", "Enter"],
                check=True,
                capture_output=True
            )

        # Send the actual command
        subprocess.run(
            ["tmux", "send-keys", "-t", pane_id, command, "Enter"],
            check=True,
            capture_output=True
        )

        result["success"] = True

    except subprocess.CalledProcessError as e:
        result["error"] = f"tmux send-keys failed: {e.stderr}"
    except Exception as e:
        result["error"] = str(e)

    return result


def tmux_capture_output(
    pane_id: str,
    start_line: int = 0,
    end_line: int = -1,
    **kwargs
) -> Dict[str, Any]:
    """
    Capture output from a tmux pane.

    Args:
        pane_id: Target pane ID
        start_line: Starting line (0 = top)
        end_line: Ending line (-1 = bottom)

    Returns:
        {"success": bool, "output": str, "error": str|None}
    """
    result = {"success": False, "output": None, "error": None}

    if not is_tmux_available():
        result["error"] = "Not running inside tmux session"
        return result

    try:
        cmd = ["tmux", "capture-pane", "-p", "-t", pane_id]
        if start_line > 0:
            cmd.extend(["-S", str(start_line)])
        if end_line != -1:
            cmd.extend(["-E", str(end_line)])

        proc = subprocess.run(cmd, capture_output=True, text=True, check=True)
        result["success"] = True
        result["output"] = proc.stdout

    except subprocess.CalledProcessError as e:
        result["error"] = f"tmux capture-pane failed: {e.stderr}"
    except Exception as e:
        result["error"] = str(e)

    return result


def tmux_close_pane(
    pane_id: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Close a tmux pane.

    Args:
        pane_id: Target pane ID

    Returns:
        {"success": bool, "error": str|None}
    """
    result = {"success": False, "error": None}

    if not is_tmux_available():
        result["error"] = "Not running inside tmux session"
        return result

    try:
        subprocess.run(
            ["tmux", "kill-pane", "-t", pane_id],
            check=True,
            capture_output=True
        )
        result["success"] = True

    except subprocess.CalledProcessError as e:
        # Pane might already be closed
        if "can't find pane" in str(e.stderr):
            result["success"] = True  # Already closed is OK
        else:
            result["error"] = f"tmux kill-pane failed: {e.stderr}"
    except Exception as e:
        result["error"] = str(e)

    return result


def tmux_set_layout(
    layout: str = "tiled",
    **kwargs
) -> Dict[str, Any]:
    """
    Set tmux window layout.

    Args:
        layout: Layout type (tiled, even-horizontal, even-vertical, main-horizontal, main-vertical)

    Returns:
        {"success": bool, "error": str|None}
    """
    result = {"success": False, "error": None}

    if not is_tmux_available():
        result["error"] = "Not running inside tmux session"
        return result

    try:
        subprocess.run(
            ["tmux", "select-layout", layout],
            check=True,
            capture_output=True
        )
        result["success"] = True

    except subprocess.CalledProcessError as e:
        result["error"] = f"tmux select-layout failed: {e.stderr}"
    except Exception as e:
        result["error"] = str(e)

    return result


# Action registry
def register_tmux_actions(registry: dict, engine=None):
    registry["tmux.create_pane"] = tmux_create_pane
    registry["tmux.send_command"] = tmux_send_command
    registry["tmux.capture_output"] = tmux_capture_output
    registry["tmux.close_pane"] = tmux_close_pane
    registry["tmux.set_layout"] = tmux_set_layout
```

### Tmux Pane Manager Implementation

```python
# python/src/the_edge_agent/tracking/tmux_manager.py
from dataclasses import dataclass, field
from typing import Dict, Optional, List
import logging

from ..actions.tmux_actions import (
    is_tmux_available,
    tmux_create_pane,
    tmux_send_command,
    tmux_close_pane,
    tmux_set_layout,
)

logger = logging.getLogger(__name__)


@dataclass
class TmuxPaneManager:
    """
    Manages tmux panes for parallel workflow node execution.

    Creates a pane per executing node so operators can monitor
    the real-time output of each AI agent (claude, codex, etc.).
    """
    enabled: bool = True
    layout: str = "tiled"  # tiled, even-horizontal, even-vertical
    panes: Dict[str, str] = field(default_factory=dict)  # node_name -> pane_id
    _initialized: bool = False

    def __post_init__(self):
        if self.enabled and not is_tmux_available():
            logger.warning("Tmux not available, disabling pane management")
            self.enabled = False

    def on_node_start(self, node_name: str, command: Optional[str] = None) -> Optional[str]:
        """
        Create a pane for the starting node.

        Args:
            node_name: Name of the workflow node
            command: Optional command to display in pane

        Returns:
            pane_id if created, None otherwise
        """
        if not self.enabled:
            return None

        result = tmux_create_pane(
            name=node_name,
            direction="vertical",
            size=50
        )

        if result["success"]:
            pane_id = result["pane_id"]
            self.panes[node_name] = pane_id

            # Display header in pane
            header = f"═══ TEA Node: {node_name} ═══"
            tmux_send_command(pane_id, f"echo '{header}'", clear_first=True)

            # Rebalance layout
            if len(self.panes) > 1:
                tmux_set_layout(self.layout)

            return pane_id
        else:
            logger.warning(f"Failed to create pane for {node_name}: {result['error']}")
            return None

    def send_to_node(self, node_name: str, command: str) -> bool:
        """
        Send a command to the node's pane.

        Args:
            node_name: Name of the workflow node
            command: Command to execute

        Returns:
            True if sent successfully
        """
        if not self.enabled or node_name not in self.panes:
            return False

        pane_id = self.panes[node_name]
        result = tmux_send_command(pane_id, command, clear_first=False)
        return result["success"]

    def on_node_complete(self, node_name: str, keep_pane: bool = False) -> None:
        """
        Handle node completion.

        Args:
            node_name: Name of the workflow node
            keep_pane: If True, keep pane open for review
        """
        if not self.enabled or node_name not in self.panes:
            return

        pane_id = self.panes[node_name]

        # Show completion message
        tmux_send_command(pane_id, f"echo '✓ Node {node_name} completed'", clear_first=False)

        if not keep_pane:
            # Optionally close after delay (let user see output)
            pass  # Keep open by default for debugging

    def cleanup(self) -> None:
        """Close all managed panes."""
        if not self.enabled:
            return

        for node_name, pane_id in list(self.panes.items()):
            tmux_close_pane(pane_id)
            del self.panes[node_name]

    def route_shell_output(self, node_name: str, output: str) -> None:
        """
        Route shell command output to the node's pane.

        Args:
            node_name: Name of the workflow node
            output: Output text to display
        """
        if not self.enabled or node_name not in self.panes:
            return

        pane_id = self.panes[node_name]
        # Echo output to pane (for streaming display)
        for line in output.split('\n'):
            if line.strip():
                tmux_send_command(pane_id, f"echo '{line}'", clear_first=False)
```

### YAML Integration

```yaml
# Workflow with progress, notifications, and tmux panes
settings:
  progress:
    track: true
    callbacks:
      - type: file
        path: "./progress.log"

  tmux:
    enabled: true                    # Enable tmux pane management
    layout: tiled                    # tiled, even-horizontal, even-vertical
    keep_panes_on_complete: true     # Keep panes open for review
    show_node_header: true           # Display node name header in each pane

nodes:
  - name: task_1
    run: |
      import time
      time.sleep(1)
      return {"step": 1}

  - name: task_2
    run: |
      import time
      time.sleep(1)
      return {"step": 2}

  - name: notify_complete
    uses: notify.send
    with:
      title: "Workflow Complete"
      message: "All tasks finished successfully!"
      sound: true

  - name: webhook_notify
    uses: notify.webhook
    with:
      url: "{{ secrets.SLACK_WEBHOOK_URL }}"
      format: slack
      payload:
        message: "TEA workflow completed: {{ state._progress.percentage }}%"

edges:
  - from: __start__
    to: task_1
  - from: task_1
    to: task_2
  - from: task_2
    to: notify_complete
  - from: notify_complete
    to: webhook_notify
  - from: webhook_notify
    to: __end__
```

## Testing

**Test Location:** `python/tests/test_notify_actions.py`

```python
import pytest
from unittest.mock import patch, MagicMock
from the_edge_agent.actions.notify_actions import notify_send, notify_webhook
from the_edge_agent.tracking.progress_tracker import ProgressTracker, NodeStatus

class TestNotifySend:
    @patch("subprocess.run")
    def test_macos_notification(self, mock_run):
        with patch("sys.platform", "darwin"):
            result = notify_send("Test message", title="Test")
            assert result["success"] == True
            assert result["platform"] == "darwin"
            mock_run.assert_called_once()

    @patch("subprocess.run")
    def test_linux_notification(self, mock_run):
        with patch("sys.platform", "linux"):
            result = notify_send("Test message", urgency="critical")
            assert result["success"] == True
            mock_run.assert_called_once()
            # Verify urgency flag
            call_args = mock_run.call_args[0][0]
            assert "-u" in call_args
            assert "critical" in call_args

    def test_unsupported_platform(self):
        with patch("sys.platform", "unknown"):
            result = notify_send("Test")
            assert result["success"] == False
            assert "Unsupported" in result["error"]


class TestNotifyWebhook:
    @patch("requests.post")
    def test_json_webhook(self, mock_post):
        mock_post.return_value = MagicMock(status_code=200)

        result = notify_webhook(
            url="https://example.com/webhook",
            payload={"message": "Test"}
        )

        assert result["success"] == True
        assert result["status_code"] == 200

    @patch("requests.post")
    def test_slack_format(self, mock_post):
        mock_post.return_value = MagicMock(status_code=200)

        notify_webhook(
            url="https://hooks.slack.com/...",
            payload={"message": "Test"},
            format="slack"
        )

        # Verify Slack format
        call_kwargs = mock_post.call_args[1]
        assert call_kwargs["json"] == {"text": "Test"}


class TestProgressTracker:
    def test_percentage_calculation(self):
        tracker = ProgressTracker(total_nodes=4)

        assert tracker.percentage == 0.0

        tracker.on_node_start("node1")
        tracker.on_node_complete("node1")
        assert tracker.percentage == 25.0

        tracker.on_node_start("node2")
        tracker.on_node_complete("node2")
        assert tracker.percentage == 50.0

    def test_callback_invocation(self):
        callback_calls = []
        tracker = ProgressTracker(
            total_nodes=2,
            callbacks=[lambda t: callback_calls.append(t.percentage)]
        )

        tracker.on_node_start("node1")
        tracker.on_node_complete("node1")

        # Callback called on start and complete
        assert len(callback_calls) == 2


class TestTmuxActions:
    @patch("os.environ.get")
    def test_tmux_detection_inside_tmux(self, mock_env):
        """Test detecting tmux when running inside tmux."""
        mock_env.return_value = "/tmp/tmux-1000/default,12345,0"
        from the_edge_agent.actions.tmux_actions import is_tmux_available
        assert is_tmux_available() == True

    @patch("os.environ.get")
    def test_tmux_detection_outside_tmux(self, mock_env):
        """Test detecting tmux when NOT running inside tmux."""
        mock_env.return_value = None
        from the_edge_agent.actions.tmux_actions import is_tmux_available
        assert is_tmux_available() == False

    @patch("subprocess.run")
    @patch("os.environ.get")
    def test_tmux_create_pane(self, mock_env, mock_run):
        """Test creating a tmux pane."""
        mock_env.return_value = "/tmp/tmux"  # Simulate inside tmux
        mock_run.return_value = MagicMock(stdout="%5\n", returncode=0)

        from the_edge_agent.actions.tmux_actions import tmux_create_pane
        result = tmux_create_pane(name="test-node", direction="vertical")

        assert result["success"] == True
        assert result["pane_id"] == "%5"

    @patch("subprocess.run")
    @patch("os.environ.get")
    def test_tmux_send_command(self, mock_env, mock_run):
        """Test sending command to a pane."""
        mock_env.return_value = "/tmp/tmux"
        mock_run.return_value = MagicMock(returncode=0)

        from the_edge_agent.actions.tmux_actions import tmux_send_command
        result = tmux_send_command(pane_id="%5", command="echo hello")

        assert result["success"] == True

    @patch("os.environ.get")
    def test_graceful_degradation_outside_tmux(self, mock_env):
        """Test actions gracefully fail when not in tmux."""
        mock_env.return_value = None  # Not in tmux

        from the_edge_agent.actions.tmux_actions import tmux_create_pane
        result = tmux_create_pane(name="test-node")

        assert result["success"] == False
        assert "Not running inside tmux" in result["error"]


class TestTmuxPaneManager:
    @patch("the_edge_agent.actions.tmux_actions.is_tmux_available")
    @patch("the_edge_agent.actions.tmux_actions.tmux_create_pane")
    @patch("the_edge_agent.actions.tmux_actions.tmux_send_command")
    def test_pane_manager_lifecycle(self, mock_send, mock_create, mock_available):
        """Test full pane manager lifecycle."""
        mock_available.return_value = True
        mock_create.return_value = {"success": True, "pane_id": "%5"}
        mock_send.return_value = {"success": True}

        from the_edge_agent.tracking.tmux_manager import TmuxPaneManager

        manager = TmuxPaneManager(enabled=True, layout="tiled")

        # Create pane on node start
        pane_id = manager.on_node_start("execute_with_ai")
        assert pane_id == "%5"
        assert "execute_with_ai" in manager.panes

        # Send output to pane
        success = manager.send_to_node("execute_with_ai", "claude -p 'test'")
        assert success == True

        # Cleanup
        manager.cleanup()
        assert len(manager.panes) == 0
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_callback_hook | Progress callbacks invoked | 1 |
| test_percentage_calculation | Correct % calculation | 2 |
| test_desktop_notification | Cross-platform notify | 3 |
| test_platform_detection | Detect macOS/Linux/Windows | 4 |
| test_webhook_integration | Webhook with JSON payload | 5 |
| test_slack_format | Slack webhook format | 5 |
| test_tmux_detection | Detect tmux availability | 6 |
| test_tmux_create_pane | Create pane with ID | 6 |
| test_tmux_send_command | Send command to pane | 7 |
| test_tmux_capture_output | Capture pane output | 6, 7 |
| test_tmux_close_pane | Close pane cleanly | 6 |
| test_tmux_layout | Set layout correctly | 6 |
| test_pane_manager_lifecycle | Create/route/cleanup panes | 6, 7 |
| test_graceful_degradation | Works when not in tmux | 6 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-18 | 0.2 | Added tmux pane management for real-time command output visibility (AC 6, 7) | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
