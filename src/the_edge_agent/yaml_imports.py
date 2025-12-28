"""
External action module loading for YAMLEngine.

This module provides the ImportLoader class for loading external action modules
from local paths and installed packages. It supports:

- Path imports: Load from local Python files relative to YAML location
- Package imports: Load from installed pip packages
- Namespacing: Prefix actions with namespace (e.g., `custom.my_action`)
- Circular import prevention: Track loaded modules
- Contract validation: Require `register_actions(registry, engine)` function
- Metadata logging: Optional `__tea_actions__` for version/description

TEA-PY-008.4: Extracted from yaml_engine.py for modularity.

Example usage:
    >>> from the_edge_agent.yaml_imports import ImportLoader
    >>> loader = ImportLoader(engine)
    >>> loader.load_imports([
    ...     {"path": "./actions/custom.py", "namespace": "custom"},
    ...     {"package": "tea_actions_slack", "namespace": "slack"}
    ... ], yaml_dir="/path/to/yaml")
"""

import os
import importlib
import importlib.util
import logging
from typing import Any, Callable, Dict, List, Optional, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine

logger = logging.getLogger(__name__)


class ImportLoader:
    """
    Loader for external action modules from paths and packages.

    This class handles dynamic loading of action modules for YAMLEngine,
    supporting both local Python files and installed packages with namespace
    prefixing.

    Attributes:
        _engine: Reference to YAMLEngine for accessing actions_registry
        _loaded_modules: Set of loaded module identifiers for circular import prevention
    """

    def __init__(self, engine: "YAMLEngine"):
        """
        Initialize the import loader with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - actions_registry for action registration
        """
        self._engine = engine
        self._loaded_modules: Set[str] = set()

    def load_imports(
        self, imports: List[Dict[str, Any]], yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load external action modules from the imports section.

        Supports two import types:
        - path: Local Python file relative to YAML file
        - package: Installed Python package

        Args:
            imports: List of import configurations from YAML
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            ValueError: If import configuration is invalid
            FileNotFoundError: If local path does not exist
            ImportError: If package cannot be imported

        Example imports configuration:
            imports:
              - path: ./actions/custom.py
                namespace: custom
              - package: tea_actions_slack
                namespace: slack
        """
        if not imports:
            return

        errors: List[str] = []

        for imp in imports:
            namespace = imp.get("namespace", "")

            try:
                if "path" in imp:
                    self._load_from_path(imp["path"], namespace, yaml_dir)
                elif "package" in imp:
                    self._load_from_package(imp["package"], namespace)
                else:
                    errors.append(
                        f"Invalid import: must specify 'path' or 'package'. Got: {imp}"
                    )
            except Exception as e:
                errors.append(str(e))

        if errors:
            raise ValueError(
                f"Failed to load imports:\n" + "\n".join(f"  - {e}" for e in errors)
            )

    def _load_from_path(
        self, path: str, namespace: str, yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load actions from a local Python file.

        Args:
            path: Path to Python file (relative or absolute)
            namespace: Namespace prefix for registered actions
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            FileNotFoundError: If the file does not exist
            ValueError: If the module doesn't have register_actions function
        """
        # Resolve relative paths from YAML file location
        if yaml_dir and not os.path.isabs(path):
            full_path = os.path.normpath(os.path.join(yaml_dir, path))
        else:
            full_path = os.path.abspath(path)

        # Check for circular imports
        if full_path in self._loaded_modules:
            logger.debug(f"Skipping already loaded module: {full_path}")
            return

        if not os.path.exists(full_path):
            raise FileNotFoundError(
                f"Action module not found: {full_path} (from path: {path})"
            )

        # Create unique module name for dynamic import
        module_name = f"_tea_import_{abs(hash(full_path))}"

        # Load module dynamically
        spec = importlib.util.spec_from_file_location(module_name, full_path)
        if spec is None or spec.loader is None:
            raise ValueError(f"Cannot load module from: {full_path}")

        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        # Validate contract: module must have register_actions
        if not hasattr(module, "register_actions"):
            raise ValueError(
                f"Module {path} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Module {path} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, path, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self._engine)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, path)

        # Mark as loaded
        self._loaded_modules.add(full_path)

    def _load_from_package(self, package: str, namespace: str) -> None:
        """
        Load actions from an installed Python package.

        Args:
            package: Package name (supports dotted names like 'tea_actions.slack')
            namespace: Namespace prefix for registered actions

        Raises:
            ImportError: If package cannot be imported
            ValueError: If package doesn't have register_actions function
        """
        # Check for circular imports
        if package in self._loaded_modules:
            logger.debug(f"Skipping already loaded package: {package}")
            return

        try:
            module = importlib.import_module(package)
        except ImportError as e:
            raise ImportError(
                f"Failed to import package '{package}': {e}. "
                f"Ensure the package is installed: pip install {package}"
            )

        # Validate contract: module must have register_actions
        if not hasattr(module, "register_actions"):
            raise ValueError(
                f"Package {package} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Package {package} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, package, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self._engine)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, package)

        # Mark as loaded
        self._loaded_modules.add(package)

    def _log_module_metadata(self, module: Any, source: str, namespace: str) -> None:
        """
        Log optional __tea_actions__ metadata from an imported module.

        Args:
            module: The imported module object
            source: Source identifier (path or package name) for logging
            namespace: Namespace being used for this import
        """
        if hasattr(module, "__tea_actions__"):
            metadata = module.__tea_actions__
            if isinstance(metadata, dict):
                version = metadata.get("version", "unknown")
                description = metadata.get("description", "")
                declared_actions = metadata.get("actions", [])

                logger.info(
                    f"Loaded actions from {source} "
                    f"(namespace: {namespace or 'root'}, version: {version})"
                )
                if description:
                    logger.debug(f"  Description: {description}")
                if declared_actions:
                    logger.debug(f"  Declared actions: {declared_actions}")

    def _merge_registry_with_namespace(
        self, local_registry: Dict[str, Callable], namespace: str, source: str
    ) -> None:
        """
        Merge actions from local registry into main registry with namespace prefix.

        Args:
            local_registry: Dictionary of actions registered by the module
            namespace: Namespace prefix to apply
            source: Source identifier for error messages

        Logs a warning if an action would override an existing action.
        """
        for name, func in local_registry.items():
            # Apply namespace prefix
            full_name = f"{namespace}.{name}" if namespace else name

            # Warn about overrides
            if full_name in self._engine.actions_registry:
                logger.warning(
                    f"Action '{full_name}' from {source} overrides existing action"
                )

            self._engine.actions_registry[full_name] = func
