# Story TEA-KIROKU-005: Integration Testing & Documentation

## Status: Draft

## Story

**As a** TEA maintainer,
**I want** comprehensive integration tests and documentation for the Kiroku migration,
**so that** we can validate the migration is complete and help others learn from this example.

## Acceptance Criteria

1. **AC1:** Teste E2E gera paper completo a partir de spec YAML de exemplo
2. **AC2:** Output TEA é funcionalmente equivalente ao output LangGraph original
3. **AC3:** Guia de migração documentado em `docs/examples/langraph-to-tea-migration.md`
4. **AC4:** Exemplo funcional em `examples/academic/kiroku-document-writer.yaml`
5. **AC5:** Spec de exemplo em `examples/academic/sample-paper-spec.yaml`
6. **AC6:** README do projeto atualizado mencionando caso de uso acadêmico
7. **AC7:** Testes rodam em CI (GitHub Actions)
8. **AC8:** Documentação inclui troubleshooting e FAQs

## Tasks / Subtasks

- [ ] **Task 1: Criar spec de exemplo** (AC: 5)
  - [ ] Criar `examples/academic/sample-paper-spec.yaml`
  - [ ] Paper simples: 3-4 seções, hipótese clara
  - [ ] Incluir comentários explicando cada campo

- [ ] **Task 2: Validar exemplo funcional** (AC: 4)
  - [ ] Executar workflow completo
  - [ ] Verificar output Markdown
  - [ ] Verificar formatação correta

- [ ] **Task 3: Comparar com output original** (AC: 2)
  - [ ] Gerar paper com mesmo input no Kiroku original
  - [ ] Gerar paper com TEA
  - [ ] Documentar diferenças (se houver)
  - [ ] Confirmar equivalência funcional

- [ ] **Task 4: Escrever guia de migração** (AC: 3, 8)
  - [ ] Criar `docs/examples/langraph-to-tea-migration.md`
  - [ ] Seções: Introduction, Key Concepts, Step-by-Step, Troubleshooting
  - [ ] Mapear conceitos LangGraph → TEA
  - [ ] Incluir before/after code examples

- [ ] **Task 5: Criar teste E2E** (AC: 1, 7)
  - [ ] Criar `python/tests/integration/test_kiroku_e2e.py`
  - [ ] Mockar chamadas LLM para reprodutibilidade
  - [ ] Validar estrutura do output
  - [ ] Adicionar ao CI workflow

- [ ] **Task 6: Atualizar README** (AC: 6)
  - [ ] Adicionar seção "Academic Writing Example"
  - [ ] Link para exemplo e documentação
  - [ ] Quickstart command

## Dev Notes

### Provider Configuration

Uses same providers as TEA-KIROKU-003:
- **LLM:** Azure OpenAI (see story 003 for env vars)
- **Web Search:** Perplexity (see story 003 for env vars)

**Testing Note:** For CI/automated tests, mock the LLM and Perplexity calls to avoid API costs and ensure deterministic results.

### Sample Paper Spec

```yaml
# examples/academic/sample-paper-spec.yaml
title: "Edge Computing for Machine Learning"
hypothesis: |
  Edge computing can significantly reduce latency in machine learning
  inference while maintaining acceptable accuracy levels.
area_of_paper: "Computer Science / Edge Computing"
type_of_document: "Technical Report"

section_names:
  - Introduction
  - Background
  - Methodology
  - Results
  - Conclusion

number_of_paragraphs:
  Introduction: 3
  Background: 4
  Methodology: 3
  Results: 3
  Conclusion: 2

results: |
  - Latency reduced by 60% compared to cloud-based inference
  - Accuracy maintained at 95% of original model
  - Power consumption reduced by 40%

sentences_per_paragraph: 4
max_revisions: 2
number_of_queries: 5
suggest_title: true
generate_citations: true
```

### Migration Guide Structure

```markdown
# Migrating from LangGraph to TEA

## Introduction
- Why migrate?
- What TEA offers

## Key Concept Mapping

| LangGraph | TEA |
|-----------|-----|
| StateGraph | YAML agent |
| add_node() | nodes: section |
| add_edge() | edges: section |
| add_conditional_edges() | edges with when: |
| interrupt_before | interrupt: before |
| MemorySaver | settings.checkpointer |
| ChatOpenAI | llm.call action |

## Step-by-Step Migration

### 1. Define State Schema
Before (Python):
```python
class AgentState(TypedDict):
    title: str
    draft: str
```

After (YAML):
```yaml
state_schema:
  title: str
  draft: str
```

### 2. Convert Nodes
...

## Troubleshooting

### Q: My prompts don't work the same way
A: Check Jinja2 template syntax...

### Q: Conditional edges aren't working
A: Ensure the when: expression returns boolean...
```

### Test Structure

```python
# python/tests/integration/test_kiroku_e2e.py

import pytest
from unittest.mock import patch
from the_edge_agent import YAMLEngine

# Mock LLM responses for reproducibility
MOCK_RESPONSES = {
    "suggest_title": "Edge-Optimized Neural Networks",
    "topic_sentence_writer": "## Introduction\n1. Topic sentence 1...",
    # ...
}

@pytest.fixture
def mock_llm():
    with patch('the_edge_agent.actions.llm_actions.call_llm') as mock:
        def side_effect(messages, **kwargs):
            # Return appropriate mock based on context
            pass
        mock.side_effect = side_effect
        yield mock

def test_kiroku_full_workflow(mock_llm):
    engine = YAMLEngine.from_file("examples/academic/kiroku-document-writer.yaml")

    initial_state = {
        "title": "Test Paper",
        "hypothesis": "Testing works",
        # ... minimal state
    }

    # Run without interrupts for testing
    result = engine.run(initial_state, skip_interrupts=True)

    assert "draft" in result
    assert "## Introduction" in result["draft"]
    assert "## References" in result["draft"]
```

### CI Configuration

```yaml
# .github/workflows/python-tests.yaml
# Add to existing workflow

- name: Run Kiroku Integration Tests
  run: |
    cd python
    pytest tests/integration/test_kiroku_e2e.py -v
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-27 | 0.1 | Initial story creation | Sarah (PO Agent) |

## Dev Agent Record

_To be filled during implementation_

## QA Results

### Test Design Assessment - 2024-12-27

**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-KIROKU-005-test-design-20251227.md`

#### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 18 |
| Unit Tests | 2 (11%) |
| Integration Tests | 6 (33%) |
| E2E Tests | 10 (56%) |
| P0 (Critical) | 6 |
| P1 (High) | 8 |
| P2 (Medium) | 4 |

#### AC Coverage

| AC | Tests | Key Scenarios |
|----|-------|---------------|
| AC1 (E2E paper generation) | 4 | Full workflow, sections, content |
| AC2 (LangGraph equivalence) | 3 | Structure match, state parity |
| AC3 (Migration guide) | 3 | Doc exists, examples execute |
| AC4 (Functional example) | 2 | YAML loads, dry-run succeeds |
| AC5 (Sample spec) | 2 | Spec exists, fields complete |
| AC6 (README update) | 1 | Academic use case mentioned |
| AC7 (CI tests) | 2 | test_kiroku_e2e.py in CI |
| AC8 (Troubleshooting docs) | 1 | FAQ section present |

#### Equivalence Testing Strategy

- Archive LangGraph baseline output
- Compare TEA output structure
- Validate section ordering matches
- State field mapping verification

#### CI Integration

```yaml
pytest tests/integration/test_kiroku_e2e.py -v --timeout=300
```

#### Risk Mitigations

- Mock LLM for deterministic CI tests
- Archived baseline for comparison
- Documentation examples auto-validated

#### Recommendations

1. Store baseline in `tests/fixtures/kiroku_baseline/`
2. Use timeout of 300s for full workflow
3. Skip interrupts in automated tests
