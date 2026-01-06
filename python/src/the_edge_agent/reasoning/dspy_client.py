"""
DSPy Client Wrapper for YAMLEngine (TEA-AGENT-001.7).

This module provides a wrapper for DSPy integration, handling:
- LM configuration from settings
- API key management with secure handling
- Fallback detection and graceful degradation
- Compiled module persistence
- Version tracking

Example:
    >>> client = DSPyClient.from_settings(settings)
    >>> if client.is_available:
    ...     cot = client.get_chain_of_thought("question -> answer")
    ...     result = cot(question="What is 2+2?")
    >>> else:
    ...     # Use native fallback
    ...     result = native_cot(question="What is 2+2?")
"""

import json
import logging
import os
import hashlib
from dataclasses import dataclass, field
from typing import Any, Callable, Dict, List, Optional, Union

logger = logging.getLogger(__name__)

# Sentinel for DSPy availability
_dspy_available: Optional[bool] = None


def dspy_available() -> bool:
    """Check if DSPy is available for import."""
    global _dspy_available
    if _dspy_available is None:
        try:
            import dspy

            _dspy_available = True
        except ImportError:
            _dspy_available = False
    return _dspy_available


@dataclass
class DSPyConfig:
    """
    Configuration for DSPy client.

    Attributes:
        enabled: Whether DSPy integration is enabled
        model: Default LLM model to use (e.g., "gpt-4", "claude-3-opus")
        api_key: Optional API key override (default: from environment)
        api_base: Optional API base URL override
        temperature: Default temperature for LLM calls
        max_tokens: Default max tokens for LLM calls
        teleprompter: Default teleprompter for compilation
        cache_dir: Directory for compiled prompt cache
    """

    enabled: bool = True
    model: str = "gpt-4"
    api_key: Optional[str] = None
    api_base: Optional[str] = None
    temperature: float = 0.7
    max_tokens: Optional[int] = None
    teleprompter: str = "BootstrapFewShot"
    cache_dir: Optional[str] = None

    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> "DSPyConfig":
        """Create DSPyConfig from dictionary (e.g., from YAML settings)."""
        return cls(
            enabled=config.get("enabled", True),
            model=config.get("model", "gpt-4"),
            api_key=config.get("api_key"),
            api_base=config.get("api_base"),
            temperature=config.get("temperature", 0.7),
            max_tokens=config.get("max_tokens"),
            teleprompter=config.get("teleprompter", "BootstrapFewShot"),
            cache_dir=config.get("cache_dir"),
        )

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary, masking sensitive fields."""
        result = {
            "enabled": self.enabled,
            "model": self.model,
            "temperature": self.temperature,
            "teleprompter": self.teleprompter,
        }
        # Mask API key for security
        if self.api_key:
            result["api_key"] = "***MASKED***"
        if self.api_base:
            result["api_base"] = self.api_base
        if self.max_tokens:
            result["max_tokens"] = self.max_tokens
        if self.cache_dir:
            result["cache_dir"] = self.cache_dir
        return result


@dataclass
class CompiledPrompt:
    """
    Represents a compiled DSPy prompt configuration.

    Attributes:
        module_type: Type of module (cot, react, predict)
        signature: DSPy signature string
        version: Version identifier for the compiled prompt
        demos: Few-shot demonstration examples
        metadata: Additional metadata (training info, metrics, etc.)
    """

    module_type: str
    signature: str
    version: str
    demos: List[Dict[str, Any]] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Serialize to dictionary for persistence."""
        return {
            "module_type": self.module_type,
            "signature": self.signature,
            "version": self.version,
            "demos": self.demos,
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "CompiledPrompt":
        """Deserialize from dictionary."""
        return cls(
            module_type=data.get("module_type", "cot"),
            signature=data.get("signature", "question -> answer"),
            version=data.get("version", "1.0"),
            demos=data.get("demos", []),
            metadata=data.get("metadata", {}),
        )

    @staticmethod
    def compute_version(signature: str, demos: List[Dict]) -> str:
        """Compute a version hash based on signature and demos."""
        content = json.dumps({"signature": signature, "demos": demos}, sort_keys=True)
        return hashlib.sha256(content.encode()).hexdigest()[:12]


class DSPyClient:
    """
    Client wrapper for DSPy integration.

    Provides a unified interface for DSPy operations with:
    - Automatic LM configuration from settings
    - Compiled module management
    - Graceful fallback when DSPy unavailable
    - Security-conscious API key handling
    """

    def __init__(self, config: Optional[DSPyConfig] = None):
        """
        Initialize DSPy client.

        Args:
            config: DSPy configuration (default: from environment)
        """
        self.config = config or DSPyConfig()
        self._lm = None
        self._compiled_modules: Dict[str, Any] = {}
        self._compiled_prompts: Dict[str, CompiledPrompt] = {}
        self._initialized = False

    @classmethod
    def from_settings(cls, settings: Dict[str, Any]) -> "DSPyClient":
        """
        Create DSPyClient from YAML settings.

        Args:
            settings: Settings dict from YAML (settings.dspy section)

        Returns:
            Configured DSPyClient instance
        """
        dspy_settings = settings.get("dspy", {})
        config = DSPyConfig.from_dict(dspy_settings)
        return cls(config)

    @property
    def is_available(self) -> bool:
        """Check if DSPy is available and enabled."""
        return dspy_available() and self.config.enabled

    def initialize(self) -> bool:
        """
        Initialize DSPy with configured LM.

        Returns:
            True if initialization successful, False otherwise
        """
        if not self.is_available:
            logger.debug("DSPy not available or disabled")
            return False

        if self._initialized:
            return True

        try:
            import dspy

            # Build LM configuration
            model = self.config.model

            # Map common model names to provider prefixes
            if (
                model.startswith("gpt-")
                or model.startswith("o1")
                or model.startswith("o3")
            ):
                lm_model = f"openai/{model}"
            elif model.startswith("claude-"):
                lm_model = f"anthropic/{model}"
            elif model.startswith("gemini-"):
                lm_model = f"google/{model}"
            elif "/" in model:
                # Already has provider prefix
                lm_model = model
            else:
                # Assume OpenAI format
                lm_model = f"openai/{model}"

            # Build LM kwargs
            lm_kwargs = {"temperature": self.config.temperature}
            if self.config.max_tokens:
                lm_kwargs["max_tokens"] = self.config.max_tokens
            if self.config.api_key:
                lm_kwargs["api_key"] = self.config.api_key
            if self.config.api_base:
                lm_kwargs["api_base"] = self.config.api_base

            # Create and configure LM
            self._lm = dspy.LM(lm_model, **lm_kwargs)
            dspy.configure(lm=self._lm)

            self._initialized = True
            logger.debug(f"DSPy initialized with model: {model}")
            return True

        except Exception as e:
            logger.warning(f"DSPy initialization failed: {e}")
            return False

    def get_chain_of_thought(
        self,
        signature: str = "question -> answer",
        compiled_prompt: Optional[CompiledPrompt] = None,
    ) -> Optional[Any]:
        """
        Get a ChainOfThought module.

        Args:
            signature: DSPy signature string
            compiled_prompt: Optional pre-compiled prompt configuration

        Returns:
            DSPy ChainOfThought module, or None if unavailable
        """
        if not self.initialize():
            return None

        try:
            import dspy

            cot = dspy.ChainOfThought(signature)

            # Apply compiled demos if provided
            if compiled_prompt and compiled_prompt.demos:
                for demo in compiled_prompt.demos:
                    cot.demos.append(
                        dspy.Example(**demo).with_inputs(
                            *self._get_input_fields(signature)
                        )
                    )

            return cot

        except Exception as e:
            logger.warning(f"Failed to create ChainOfThought: {e}")
            return None

    def get_react(
        self,
        signature: str = "goal -> result",
        tools: Optional[List[Callable]] = None,
        max_iters: int = 10,
    ) -> Optional[Any]:
        """
        Get a ReAct module.

        Args:
            signature: DSPy signature string
            tools: List of tool functions
            max_iters: Maximum iterations

        Returns:
            DSPy ReAct module, or None if unavailable
        """
        if not self.initialize():
            return None

        try:
            import dspy

            return dspy.ReAct(signature, tools=tools or [], max_iters=max_iters)

        except Exception as e:
            logger.warning(f"Failed to create ReAct: {e}")
            return None

    def get_predict(self, signature: str = "input -> output") -> Optional[Any]:
        """
        Get a Predict module.

        Args:
            signature: DSPy signature string

        Returns:
            DSPy Predict module, or None if unavailable
        """
        if not self.initialize():
            return None

        try:
            import dspy

            return dspy.Predict(signature)

        except Exception as e:
            logger.warning(f"Failed to create Predict: {e}")
            return None

    def compile(
        self,
        module: Any,
        trainset: List[Dict[str, Any]],
        teleprompter: Optional[str] = None,
        metric: Optional[Callable] = None,
        **kwargs,
    ) -> Optional[Any]:
        """
        Compile a DSPy module with a teleprompter.

        Args:
            module: DSPy module to compile
            trainset: Training examples
            teleprompter: Teleprompter to use (default: from config)
            metric: Metric function for optimization
            **kwargs: Additional teleprompter arguments

        Returns:
            Compiled module, or None if compilation fails
        """
        if not self.initialize():
            return None

        try:
            import dspy

            # Convert trainset to DSPy Examples
            examples = []
            for ex in trainset:
                example = dspy.Example(**ex)
                # Infer input fields from keys
                if "question" in ex:
                    example = example.with_inputs("question")
                elif "goal" in ex:
                    example = example.with_inputs("goal")
                elif "input" in ex:
                    example = example.with_inputs("input")
                examples.append(example)

            # Select teleprompter
            tp_name = teleprompter or self.config.teleprompter

            # Default metric
            def default_metric(example, prediction, trace=None):
                pred = getattr(prediction, "answer", None) or getattr(
                    prediction, "result", None
                )
                return 1.0 if pred and len(str(pred)) > 0 else 0.0

            metric_fn = metric or default_metric

            # Create teleprompter
            if tp_name == "BootstrapFewShot":
                tp = dspy.BootstrapFewShot(metric=metric_fn, **kwargs)
            elif tp_name == "BootstrapFewShotWithRandomSearch":
                tp = dspy.BootstrapFewShotWithRandomSearch(
                    metric=metric_fn,
                    num_candidate_programs=kwargs.get("num_candidate_programs", 5),
                    **{
                        k: v for k, v in kwargs.items() if k != "num_candidate_programs"
                    },
                )
            elif tp_name == "MIPRO" or tp_name == "MIPROv2":
                tp = dspy.MIPROv2(
                    metric=metric_fn,
                    num_candidates=kwargs.get("num_candidates", 5),
                    **{k: v for k, v in kwargs.items() if k != "num_candidates"},
                )
            else:
                logger.warning(
                    f"Unknown teleprompter: {tp_name}, using BootstrapFewShot"
                )
                tp = dspy.BootstrapFewShot(metric=metric_fn)

            # Compile
            compiled = tp.compile(module, trainset=examples)
            return compiled

        except Exception as e:
            logger.error(f"DSPy compilation failed: {e}")
            return None

    def optimize(
        self,
        module: Any,
        trainset: List[Dict[str, Any]],
        valset: List[Dict[str, Any]],
        metric: Optional[Callable] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Optimize a DSPy module against a validation set.

        Args:
            module: DSPy module to optimize
            trainset: Training examples
            valset: Validation examples
            metric: Metric function
            **kwargs: Additional optimization arguments

        Returns:
            Optimization results including best configuration and scores
        """
        if not self.initialize():
            return {"error": "DSPy not available", "success": False}

        try:
            import dspy
            from dspy.evaluate import Evaluate

            # Convert to Examples
            def to_examples(
                data: List[Dict], input_fields: Optional[List[str]] = None
            ) -> List[Any]:
                examples = []
                for ex in data:
                    example = dspy.Example(**ex)
                    if input_fields:
                        example = example.with_inputs(*input_fields)
                    elif "question" in ex:
                        example = example.with_inputs("question")
                    elif "goal" in ex:
                        example = example.with_inputs("goal")
                    elif "input" in ex:
                        example = example.with_inputs("input")
                    examples.append(example)
                return examples

            train_examples = to_examples(trainset)
            val_examples = to_examples(valset)

            # Default exact match metric
            def exact_match(example, prediction, trace=None):
                expected = getattr(example, "answer", None) or getattr(
                    example, "result", None
                )
                predicted = getattr(prediction, "answer", None) or getattr(
                    prediction, "result", None
                )
                if expected is None or predicted is None:
                    return 0.0
                return 1.0 if str(expected).strip() == str(predicted).strip() else 0.0

            metric_fn = metric or exact_match

            # Compile with training data
            compiled_module = self.compile(module, trainset, metric=metric_fn, **kwargs)
            if compiled_module is None:
                return {"error": "Compilation failed", "success": False}

            # Evaluate on validation set
            evaluator = Evaluate(devset=val_examples, metric=metric_fn, num_threads=1)
            val_score = evaluator(compiled_module)

            # Also evaluate on training set for comparison
            train_evaluator = Evaluate(
                devset=train_examples, metric=metric_fn, num_threads=1
            )
            train_score = train_evaluator(compiled_module)

            return {
                "success": True,
                "compiled_module": compiled_module,
                "train_score": train_score,
                "val_score": val_score,
                "training_examples": len(train_examples),
                "validation_examples": len(val_examples),
            }

        except Exception as e:
            logger.error(f"DSPy optimization failed: {e}")
            return {"error": str(e), "success": False}

    def store_compiled_module(
        self, key: str, module: Any, prompt: CompiledPrompt
    ) -> None:
        """
        Store a compiled module for later use.

        Args:
            key: Unique key for the module
            module: Compiled DSPy module
            prompt: Compiled prompt configuration
        """
        self._compiled_modules[key] = module
        self._compiled_prompts[key] = prompt
        logger.debug(f"Stored compiled module: {key}")

    def get_compiled_module(self, key: str) -> Optional[Any]:
        """Get a stored compiled module by key."""
        return self._compiled_modules.get(key)

    def get_compiled_prompt(self, key: str) -> Optional[CompiledPrompt]:
        """Get a stored compiled prompt configuration by key."""
        return self._compiled_prompts.get(key)

    def list_compiled_modules(self) -> List[str]:
        """List all stored compiled module keys."""
        return list(self._compiled_modules.keys())

    def export_compiled_prompts(self) -> Dict[str, Dict]:
        """Export all compiled prompts for persistence."""
        return {key: prompt.to_dict() for key, prompt in self._compiled_prompts.items()}

    def import_compiled_prompts(self, data: Dict[str, Dict]) -> int:
        """
        Import compiled prompts from persistence.

        Args:
            data: Dictionary of key -> prompt dict

        Returns:
            Number of prompts imported
        """
        count = 0
        for key, prompt_dict in data.items():
            try:
                prompt = CompiledPrompt.from_dict(prompt_dict)
                self._compiled_prompts[key] = prompt

                # Recreate module from prompt
                if self.is_available:
                    if prompt.module_type == "cot":
                        module = self.get_chain_of_thought(prompt.signature, prompt)
                    elif prompt.module_type == "react":
                        module = self.get_react(prompt.signature)
                    else:
                        module = self.get_predict(prompt.signature)

                    if module:
                        self._compiled_modules[key] = module

                count += 1
            except Exception as e:
                logger.warning(f"Failed to import prompt {key}: {e}")

        return count

    def _get_input_fields(self, signature: str) -> List[str]:
        """Extract input field names from signature."""
        try:
            input_part = signature.split("->")[0].strip()
            return [f.strip() for f in input_part.split(",")]
        except Exception:
            return ["input"]

    def close(self) -> None:
        """Clean up resources."""
        self._compiled_modules.clear()
        self._compiled_prompts.clear()
        self._lm = None
        self._initialized = False
