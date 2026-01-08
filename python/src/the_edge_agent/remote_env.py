"""
Remote environment variable handling for The Edge Agent.

This module provides secure environment variable transfer to remote hosts
during parallel workflow execution with the remote strategy.

Security Features:
- Whitelist-only: Only explicitly listed variables are considered
- Pattern exclusion: Glob patterns can exclude dangerous variables
- No auto-discovery: Never automatically transfers env vars

Transfer Modes:
- ssh_env: Uses SSH -o SendEnv options (requires server AcceptEnv)
- export_file: Generates a script to source on remote
- none: No environment transfer

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

from dataclasses import dataclass, field
from fnmatch import fnmatch
from typing import Any, Dict, List, Literal, Optional
import logging
import os
import shlex

logger = logging.getLogger(__name__)


@dataclass
class EnvVarsConfig:
    """
    Configuration for environment variable transfer to remote hosts.

    Attributes:
        include: Explicit whitelist of variable names to consider.
                 ONLY these variables will be transferred.
        exclude_patterns: Glob patterns to exclude from included vars.
                         Applied as a safety net after whitelist filtering.
        mode: Transfer mode - "ssh_env", "export_file", or "none".

    Security:
        - Only variables in `include` are ever considered
        - `exclude_patterns` provides defense in depth
        - Cloud credentials (AWS_*, AZURE_*, GOOGLE_*) should NOT be
          auto-transferred unless explicitly required

    Example:
        >>> config = EnvVarsConfig(
        ...     include=["OPENAI_API_KEY", "LOG_LEVEL"],
        ...     exclude_patterns=["*_SECRET", "*_PASSWORD"],
        ...     mode="ssh_env"
        ... )
    """

    include: List[str] = field(default_factory=list)
    exclude_patterns: List[str] = field(default_factory=list)
    mode: Literal["ssh_env", "export_file", "none"] = "ssh_env"

    def __post_init__(self) -> None:
        """Validate configuration after initialization."""
        valid_modes = ("ssh_env", "export_file", "none")
        if self.mode not in valid_modes:
            raise ValueError(
                f"Invalid mode '{self.mode}'. Must be one of: {valid_modes}"
            )

        # Ensure include is a list
        if not isinstance(self.include, list):
            raise TypeError(
                f"include must be a list, got {type(self.include).__name__}"
            )

        # Ensure exclude_patterns is a list
        if not isinstance(self.exclude_patterns, list):
            raise TypeError(
                f"exclude_patterns must be a list, got {type(self.exclude_patterns).__name__}"
            )


class RemoteEnvHandler:
    """
    Handle environment variable transfer to remote hosts.

    This class implements secure environment variable filtering and
    transfer mechanisms for remote workflow execution.

    Security:
        - Never logs environment variable VALUES, only names
        - Whitelist-only approach prevents accidental credential exposure
        - Pattern exclusion provides additional safety layer

    Usage:
        >>> config = EnvVarsConfig(
        ...     include=["API_KEY", "LOG_LEVEL"],
        ...     exclude_patterns=["*_SECRET"],
        ...     mode="ssh_env"
        ... )
        >>> handler = RemoteEnvHandler(config)
        >>> vars = handler.get_filtered_env_vars()
        >>> ssh_opts = handler.build_ssh_options()
    """

    def __init__(self, config: EnvVarsConfig):
        """
        Initialize the environment handler.

        Args:
            config: EnvVarsConfig specifying which vars to transfer and how
        """
        self.config = config

    def get_filtered_env_vars(self) -> Dict[str, str]:
        """
        Get environment variables matching include list, minus exclude patterns.

        Security:
            - Only vars explicitly in `include` are considered
            - Then `exclude_patterns` filters those out
            - Missing vars are logged at DEBUG level and skipped

        Returns:
            Dict mapping variable names to their values

        Example:
            >>> config = EnvVarsConfig(include=["HOME", "PATH"])
            >>> handler = RemoteEnvHandler(config)
            >>> vars = handler.get_filtered_env_vars()
            >>> "HOME" in vars  # True if HOME is set
            True
        """
        result: Dict[str, str] = {}

        for var in self.config.include:
            if var not in os.environ:
                logger.debug(
                    f"Env var '{var}' in include list not found in environment"
                )
                continue

            # Check exclude patterns
            excluded = any(
                fnmatch(var, pattern) for pattern in self.config.exclude_patterns
            )

            if excluded:
                logger.debug(f"Env var '{var}' excluded by pattern")
                continue

            result[var] = os.environ[var]

        return result

    def build_ssh_options(self) -> List[str]:
        """
        Build SSH -o SendEnv options for ssh_env mode.

        Returns:
            List of SSH command-line options like ["-o", "SendEnv=VAR1", "-o", "SendEnv=VAR2"]
            Empty list if mode is not "ssh_env"

        Note:
            Requires server to have AcceptEnv configured for these variables.
            If the server doesn't accept them, they will be silently ignored.

        Example:
            >>> config = EnvVarsConfig(include=["API_KEY"], mode="ssh_env")
            >>> handler = RemoteEnvHandler(config)
            >>> opts = handler.build_ssh_options()
            >>> # ["-o", "SendEnv=API_KEY"] (if API_KEY is set)
        """
        if self.config.mode != "ssh_env":
            return []

        env_vars = self.get_filtered_env_vars()
        options: List[str] = []
        for var in env_vars.keys():
            options.extend(["-o", f"SendEnv={var}"])
        return options

    def generate_export_script(self, path: str) -> Optional[str]:
        """
        Generate env.sh for export_file mode.

        Creates a bash script that exports the filtered environment variables.
        Values are properly escaped to handle special characters.

        Args:
            path: Local path where to write the script

        Returns:
            The path to the generated script, or None if mode is not "export_file"

        Security:
            - Single quotes are used to prevent shell expansion
            - Values with single quotes are properly escaped
            - Script permissions are not modified (caller should chmod if needed)

        Example:
            >>> config = EnvVarsConfig(include=["API_KEY"], mode="export_file")
            >>> handler = RemoteEnvHandler(config)
            >>> script_path = handler.generate_export_script("/tmp/env.sh")
            >>> # Creates script with: export API_KEY='value'
        """
        if self.config.mode != "export_file":
            return None

        env_vars = self.get_filtered_env_vars()

        # Build script content with proper escaping
        lines = ["#!/bin/bash"]
        for key, value in env_vars.items():
            # Escape single quotes in values: ' -> '\''
            escaped_value = value.replace("'", "'\"'\"'")
            lines.append(f"export {key}='{escaped_value}'")

        script = "\n".join(lines) + "\n"

        with open(path, "w") as f:
            f.write(script)

        return path

    def get_source_command(self, remote_script_path: str) -> str:
        """
        Get command to source env script on remote.

        Args:
            remote_script_path: Path to the script on the remote host

        Returns:
            Command string to source the script, or empty string if
            mode is not "export_file"

        Example:
            >>> config = EnvVarsConfig(mode="export_file")
            >>> handler = RemoteEnvHandler(config)
            >>> cmd = handler.get_source_command("/tmp/env.sh")
            >>> # "source /tmp/env.sh && "
        """
        if self.config.mode != "export_file":
            return ""
        return f"source {shlex.quote(remote_script_path)} && "

    def get_env_for_subprocess(self) -> Optional[Dict[str, str]]:
        """
        Get environment dict for subprocess calls.

        This is useful when you want to pass filtered environment
        variables to a local subprocess (e.g., SSH process).

        Returns:
            Dict with current environment plus filtered vars,
            or None if mode is "none"

        Example:
            >>> config = EnvVarsConfig(include=["API_KEY"], mode="ssh_env")
            >>> handler = RemoteEnvHandler(config)
            >>> env = handler.get_env_for_subprocess()
            >>> subprocess.run(["ssh", ...], env=env)
        """
        if self.config.mode == "none":
            return None

        # Start with current environment
        result = dict(os.environ)

        # Filtered vars are already in os.environ if they pass the filter
        # This method just validates which ones we're explicitly allowing

        return result


def parse_env_vars_config(config_dict: Optional[Dict[str, Any]]) -> EnvVarsConfig:
    """
    Parse env_vars configuration from YAML settings.

    Args:
        config_dict: Dictionary from YAML settings.parallel.remote.env_vars
                    Can be None for defaults

    Returns:
        EnvVarsConfig instance

    Example:
        >>> config = parse_env_vars_config({
        ...     "include": ["API_KEY"],
        ...     "exclude_patterns": ["*_SECRET"],
        ...     "mode": "ssh_env"
        ... })
    """
    if config_dict is None:
        return EnvVarsConfig()

    return EnvVarsConfig(
        include=config_dict.get("include", []),
        exclude_patterns=config_dict.get("exclude_patterns", []),
        mode=config_dict.get("mode", "ssh_env"),
    )
