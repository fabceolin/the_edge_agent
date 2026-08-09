# Deferred Work

## Dependabot dependency follow-ups (2026-08-09)

- Assign an owner and automated/time-bounded recheck for the upstream-blocked ChromaDB critical alert; reevaluate when a release outside `>=1.0.0, <=1.5.9` is compatible with `crewai==1.6.1`.
- Centralize repeated OpenAI/LiteLLM pins or generate them from one constraints source to reduce drift across packaging and build workflows.
- Reconcile the pre-existing `examples/requirements.txt` pin `openai==0.28` with examples that use the modern `OpenAI` client API and the package extras.
