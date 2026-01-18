# BUG.001: HierarchicalLTMBackend YAML Config Mismatch

**Status:** Open
**Priority:** High
**Reported:** 2026-01-18
**Reporter:** Dev Agent (RX.43 post-deployment testing)
**Affected Version:** TEA v0.9.70

---

## Summary

The `HierarchicalLTMBackend` does not accept the nested YAML configuration format documented in CLAUDE.md. When YAMLEngine tries to instantiate the backend with the documented A3 mode config, it fails with:

```
Failed to configure LTM backend from YAML: HierarchicalLTMBackend.__init__() got an unexpected keyword argument 'catalog_config'
```

The backend falls back to SQLite, which works but defeats the purpose of hierarchical LTM with PostgreSQL catalog.

---

## Environment

- **TEA Version:** 0.9.70
- **Python Version:** 3.12
- **Deployment:** Firebase Cloud Functions (Gen2)
- **Context:** RX.43 A3 Mode LTM Migration

---

## Steps to Reproduce

1. Create a YAML agent with A3 mode LTM config (as documented in CLAUDE.md):

```yaml
settings:
  ltm:
    backend: hierarchical

    catalog:
      type: sqlalchemy
      url: "${SUPABASE_DATABASE_URL}"
      pool_size: 10
      lazy: true

    storage:
      uri: "${LTM_GCS_BUCKET_URI:-gs://rankellix-ltm-prod/ltm/}"

    hierarchy:
      enabled: true
      levels: [agency, firm, user, session]
      root_entity:
        type: agency
        id: "${AGENCY_ID:-rankellix}"
      defaults:
        agency: "none"
        firm: "_unassigned"
        user: "_unassigned"

    performance:
      metadata_cache:
        enabled: true
        ttl_seconds: 600
      parallel_reads:
        threads: 8

    index:
      format: parquet
      row_group_size: 122880
      compression: zstd
```

2. Load the agent using YAMLEngine:

```python
from engine import YAMLEngine

engine = YAMLEngine(enable_ltm=True)
graph = engine.load_from_file("agent.yaml")
```

3. Observe the error in logs:
```
Failed to configure LTM backend from YAML: HierarchicalLTMBackend.__init__() got an unexpected keyword argument 'catalog_config'
```

---

## Expected Behavior

YAMLEngine should parse the nested YAML config and transform it to the flat parameters expected by `HierarchicalLTMBackend`:

```python
backend = HierarchicalLTMBackend(
    catalog_url="postgresql://...",  # from catalog.url
    storage_uri="gs://...",          # from storage.uri
    hierarchy_levels=["agency", "firm", "user", "session"],  # from hierarchy.levels
    hierarchy_defaults={"agency": "none", ...},  # from hierarchy.defaults
    pool_size=10,                    # from catalog.pool_size
    index_config={"format": "parquet", ...},  # from index
    performance_config={"metadata_cache_ttl": 600, ...},  # from performance
    lazy=True,                       # from catalog.lazy
)
```

---

## Actual Behavior

YAMLEngine passes the nested config directly to the backend constructor:

```python
# What YAMLEngine appears to be doing:
backend = HierarchicalLTMBackend(
    catalog_config={"type": "sqlalchemy", "url": "...", ...},  # WRONG
    storage={"uri": "..."},                                     # WRONG
    hierarchy={"levels": [...], ...},                           # WRONG
)
```

This fails because `HierarchicalLTMBackend.__init__()` expects flat parameters, not nested dicts.

---

## Root Cause Analysis

The mismatch is between:

1. **Documented YAML format** (CLAUDE.md): Nested config with `catalog:`, `storage:`, `hierarchy:` blocks
2. **Backend constructor** (`hierarchical_ltm.py:178`): Flat parameters like `catalog_url`, `storage_uri`, `hierarchy_levels`

The YAMLEngine's LTM config parser doesn't transform the nested YAML structure into flat parameters for `HierarchicalLTMBackend`.

---

## Code References

### Backend Constructor (current)
**File:** `python/src/the_edge_agent/memory/hierarchical_ltm.py:178-189`

```python
def __init__(
    self,
    catalog_url: str,               # Expects flat string
    storage_uri: str,               # Expects flat string
    hierarchy_levels: List[str],    # Expects flat list
    hierarchy_defaults: Optional[Dict[str, str]] = None,
    index_config: Optional[Dict[str, Any]] = None,
    performance_config: Optional[Dict[str, Any]] = None,
    pool_size: int = 10,
    inline_threshold: int = 1024,
    lazy: bool = False,
):
```

### Documented YAML Config (CLAUDE.md)
Uses nested blocks:
- `catalog.url` instead of `catalog_url`
- `storage.uri` instead of `storage_uri`
- `hierarchy.levels` instead of `hierarchy_levels`

---

## Proposed Solutions

### Option A: Update YAMLEngine Config Parser (Recommended)

Add a transformation layer in YAMLEngine that converts nested YAML to flat parameters:

```python
def _parse_hierarchical_ltm_config(config: dict) -> dict:
    """Transform nested YAML config to flat HierarchicalLTMBackend params."""
    catalog = config.get("catalog", {})
    storage = config.get("storage", {})
    hierarchy = config.get("hierarchy", {})
    index = config.get("index", {})
    performance = config.get("performance", {})

    return {
        "catalog_url": catalog.get("url"),
        "storage_uri": storage.get("uri"),
        "hierarchy_levels": hierarchy.get("levels", []),
        "hierarchy_defaults": hierarchy.get("defaults", {}),
        "pool_size": catalog.get("pool_size", 10),
        "inline_threshold": config.get("inline_threshold", 1024),
        "lazy": catalog.get("lazy", False),
        "index_config": {
            "row_group_size": index.get("row_group_size", 122880),
            "compression": index.get("compression", "zstd"),
        } if index else None,
        "performance_config": {
            "metadata_cache_ttl": performance.get("metadata_cache", {}).get("ttl_seconds", 600),
            "threads": performance.get("parallel_reads", {}).get("threads", 8),
        } if performance else None,
    }
```

### Option B: Update Backend to Accept Nested Config

Modify `HierarchicalLTMBackend.__init__()` to accept either flat params or nested config dict:

```python
def __init__(
    self,
    catalog_url: str = None,
    storage_uri: str = None,
    hierarchy_levels: List[str] = None,
    # ... other flat params ...
    # NEW: Accept nested config dict
    catalog_config: Optional[Dict] = None,
    storage_config: Optional[Dict] = None,
    hierarchy_config: Optional[Dict] = None,
    **kwargs,
):
    # If nested config provided, extract values
    if catalog_config:
        catalog_url = catalog_config.get("url")
        self._pool_size = catalog_config.get("pool_size", 10)
    # ... etc
```

### Option C: Hybrid (both changes)

Support both flat params (for programmatic use) and nested config (for YAML).

---

## Workaround

Currently, agents fall back to SQLite backend when hierarchical config fails. This is functional but suboptimal for production use cases requiring PostgreSQL catalog with GCS storage.

---

## Impact

- **Affected:** All YAML agents using A3 mode (`backend: hierarchical`)
- **Severity:** Medium (fallback works, but defeats A3 purpose)
- **Users Affected:** RX.40, RX.43 stories in spa-base

---

## Acceptance Criteria

- [ ] **AC-1:** YAML config with nested `catalog:`, `storage:`, `hierarchy:` blocks correctly instantiates `HierarchicalLTMBackend`
- [ ] **AC-2:** Flat parameter API remains backward-compatible for programmatic use
- [ ] **AC-3:** Error message is clear if required config is missing
- [ ] **AC-4:** Unit tests cover both YAML-style and programmatic initialization
- [ ] **AC-5:** CLAUDE.md documentation matches actual supported config format

---

## Related

- **TEA-LTM-015:** Hierarchical LTM Backend
- **TEA-LTM-013:** Entity Hierarchy
- **RX.40:** Hierarchical LTM Backend (spa-base)
- **RX.43:** Agent A3 LTM Migration (spa-base)

---

## Change Log

| Date | Description | Author |
|------|-------------|--------|
| 2026-01-18 | Bug report created | Dev Agent |
