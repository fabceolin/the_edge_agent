"""
Validation Actions for YAML Agents (TEA-YAML-004).

Provides the validate.extraction action for generic extraction validation:
- Structural validation via extraction_schema
- Semantic validation via Prolog constraints
- LLM grounding via semantic probes

Usage in YAML:
    nodes:
      - name: validate
        uses: validate.extraction
        with:
          entities: "{{ state.entities }}"
          relationships: "{{ state.relationships }}"
          source_text: "{{ state.text }}"
"""

from typing import Any, Callable, Dict, List, Optional

from ..extraction_validation import (
    ExtractionSchema,
    ValidationConstraints,
    SemanticProbe,
    ValidationLoggingConfig,
    ExtractionValidator,
    generate_extraction_prompt,
)


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register validation actions in the YAMLEngine registry.

    Actions registered:
        - validate.extraction: Run multi-layer extraction validation
        - validate.generate_prompt: Generate schema-guided extraction prompt

    Args:
        registry: Action registry to populate
        engine: YAMLEngine instance for accessing runtime and settings
    """

    def validate_extraction_action(
        state: Dict[str, Any],
        entities: Optional[List[Dict[str, Any]]] = None,
        relationships: Optional[List[Dict[str, Any]]] = None,
        source_text: Optional[str] = None,
        schema: Optional[Dict[str, Any]] = None,
        constraints: Optional[Dict[str, Any]] = None,
        probes: Optional[List[Dict[str, Any]]] = None,
        logging: Optional[Dict[str, Any]] = None,
        agent_name: str = "unknown",
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Validate extracted entities and relationships (AC: 16-18).

        Runs all three validation layers:
        1. Structural validation against extraction_schema
        2. Semantic validation using Prolog constraints
        3. LLM grounding using semantic probes

        Args:
            state: Current workflow state
            entities: List of extracted entities (or from state['entities'])
            relationships: List of extracted relationships (or from state['relationships'])
            source_text: Original source text (or from state['text'])
            schema: ExtractionSchema config dict (or from engine config)
            constraints: ValidationConstraints config dict (or from engine config)
            probes: List of SemanticProbe config dicts (or from engine config)
            logging: ValidationLoggingConfig dict (or from engine config)
            agent_name: Agent name for logging

        Returns:
            Dict with:
                - valid: bool
                - errors: List of error dicts
                - validated_at: ISO timestamp
        """
        # Get data from state if not provided
        if entities is None:
            entities = state.get("entities", [])
        if relationships is None:
            relationships = state.get("relationships", [])
        if source_text is None:
            source_text = state.get("text", "")

        # Get validation config from engine if not provided inline
        engine_config = getattr(engine, "_extraction_validation_config", {})

        # Parse schema
        schema_obj = None
        if schema:
            schema_obj = ExtractionSchema.from_dict(schema)
        elif "extraction_schema" in engine_config:
            schema_obj = ExtractionSchema.from_dict(engine_config["extraction_schema"])

        # Parse constraints
        constraints_obj = None
        if constraints:
            constraints_obj = ValidationConstraints.from_dict(constraints)
        elif "validation_constraints" in engine_config:
            constraints_obj = ValidationConstraints.from_dict(
                engine_config["validation_constraints"]
            )

        # Parse probes
        probes_list = []
        if probes:
            probes_list = [SemanticProbe.from_dict(p) for p in probes]
        elif "semantic_probes" in engine_config:
            probes_list = [
                SemanticProbe.from_dict(p) for p in engine_config["semantic_probes"]
            ]

        # Parse logging config
        logging_config = ValidationLoggingConfig()
        if logging:
            logging_config = ValidationLoggingConfig.from_dict(logging)
        elif "validation_logging" in engine_config:
            logging_config = ValidationLoggingConfig.from_dict(
                engine_config["validation_logging"]
            )

        # Get LLM call function for probes, wrapped to adapt signature
        raw_llm_call = registry.get("llm.call")

        # Capture workflow state for llm_call closure
        workflow_state = state

        # TEA-YAML-004a: Get LLM defaults from engine settings (AC-1, AC-2)
        engine_settings = getattr(engine, "_settings", {})
        llm_settings = engine_settings.get("llm", {})
        default_provider = llm_settings.get("provider", "ollama")
        default_model = llm_settings.get("model", "gemma3:4b")

        def llm_call(state=None, messages=None, max_tokens=10, **kwargs):
            """
            Wrapper to adapt llm.call signature for semantic probes.

            TEA-YAML-004a: Fixed LLM integration with proper provider support.
            """
            # TEA-YAML-004a AC-6: Clear error when llm.call is unavailable
            if raw_llm_call is None:
                return {
                    "response": "",
                    "error": "llm.call action is not available. Ensure LLM actions are registered.",
                    "error_type": "llm_call_error",
                }

            # TEA-YAML-004a AC-3: State-level provider/model override engine defaults
            provider = workflow_state.get("llm_provider", default_provider)
            model_name = workflow_state.get("llm_model", default_model)

            # TEA-YAML-004a AC-4, AC-5: Build model string for litellm
            # Follows llm_actions.py pattern: "provider/model-name"
            if provider == "ollama":
                full_model = f"ollama/{model_name}"
            elif provider == "azure":
                full_model = f"azure/{model_name}"
            elif provider == "openai":
                full_model = f"openai/{model_name}"
            elif provider == "anthropic":
                full_model = f"anthropic/{model_name}"
            elif provider == "auto":
                # Auto-detect: pass model name directly to litellm
                full_model = model_name
            else:
                # Unknown provider - pass as-is (let litellm handle it)
                full_model = (
                    f"{provider}/{model_name}" if "/" not in model_name else model_name
                )

            try:
                # Call the action with positional args (state, model, messages)
                result = raw_llm_call(
                    state or {},  # positional: state
                    full_model,  # positional: model
                    messages or [],  # positional: messages
                    max_tokens=max_tokens,
                )

                # Adapt response format for semantic probe executor
                if isinstance(result, dict) and "content" in result:
                    return {"response": result["content"]}
                return result

            except Exception as e:
                # TEA-YAML-004a AC-7, AC-8: Error includes provider, model, and reason
                return {
                    "response": "",
                    "error": f"LLM call failed with provider={provider}, model={model_name}: {str(e)}",
                    "error_type": "llm_call_error",
                    "provider": provider,
                    "model": model_name,
                }

        # Get Prolog runtime from engine
        prolog_runtime = getattr(engine, "_prolog_runtime", None)
        prolog_timeout = getattr(engine, "_prolog_timeout", 30.0)
        prolog_sandbox = getattr(engine, "_prolog_sandbox", True)

        # Get Jinja environment from engine
        jinja_env = getattr(engine, "_jinja_env", None)

        # Create validator
        validator = ExtractionValidator(
            schema=schema_obj,
            constraints=constraints_obj,
            probes=probes_list,
            logging_config=logging_config,
            llm_call=llm_call,
            prolog_runtime=prolog_runtime,
            jinja_env=jinja_env,
            prolog_timeout=prolog_timeout,
            prolog_sandbox=prolog_sandbox,
        )

        # Run validation
        result = validator.validate(
            entities=entities,
            relationships=relationships,
            source_text=source_text,
            state=state,
            agent_name=agent_name,
        )

        return result.to_dict()

    def generate_extraction_prompt_action(
        state: Dict[str, Any],
        schema: Optional[Dict[str, Any]] = None,
        constraints: Optional[Dict[str, Any]] = None,
        confidence_tracking: bool = False,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Generate a schema-guided extraction prompt (AC: 23-27).

        Creates an extraction prompt that guides the LLM according to
        the declared ontology (symbolic→neural flow).

        Args:
            state: Current workflow state
            schema: ExtractionSchema config dict
            constraints: ValidationConstraints config dict
            confidence_tracking: Include confidence score instructions

        Returns:
            Dict with:
                - extraction_prompt: Generated prompt string
        """
        # Get config from engine if not provided
        engine_config = getattr(engine, "_extraction_validation_config", {})

        # Parse schema
        schema_obj = None
        if schema:
            schema_obj = ExtractionSchema.from_dict(schema)
        elif "extraction_schema" in engine_config:
            schema_obj = ExtractionSchema.from_dict(engine_config["extraction_schema"])

        if not schema_obj:
            return {"extraction_prompt": ""}

        # Parse constraints
        constraints_obj = None
        if constraints:
            constraints_obj = ValidationConstraints.from_dict(constraints)
        elif "validation_constraints" in engine_config:
            constraints_obj = ValidationConstraints.from_dict(
                engine_config["validation_constraints"]
            )

        # Use schema's confidence_tracking if not explicitly provided
        if not confidence_tracking and schema_obj:
            confidence_tracking = schema_obj.confidence_tracking

        prompt = generate_extraction_prompt(
            schema_obj,
            constraints_obj,
            confidence_tracking,
        )

        return {"extraction_prompt": prompt}

    # Register actions
    registry["validate.extraction"] = validate_extraction_action
    registry["actions.validate_extraction"] = validate_extraction_action
    registry["validate.generate_prompt"] = generate_extraction_prompt_action
    registry["actions.validate_generate_prompt"] = generate_extraction_prompt_action
