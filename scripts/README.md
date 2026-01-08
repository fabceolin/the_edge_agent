# Scripts Directory

This directory contains utility scripts for project management and automation.

## Story Status Management

### 📊 `story_status_audit.py`

**Purpose:** Analyze and report on story statuses across the project.

**Quick Start:**

```bash
# Basic audit
python scripts/story_status_audit.py

# Find stories ready for development
python scripts/story_status_audit.py --filter ready

# Export to CSV
python scripts/story_status_audit.py --format csv --output status_report.csv
```

**Documentation:** See [docs/guides/story-status-audit-guide.md](../docs/guides/story-status-audit-guide.md)

---

### ✏️ `batch_update_stories.py`

**Purpose:** Update story statuses in bulk from a CSV file.

**Quick Start:**

```bash
# 1. Create updates CSV (see status_updates_template.csv)
cp scripts/status_updates_template.csv my_updates.csv

# 2. Edit CSV with your updates
# filepath,new_status
# docs/stories/TEA-BUILTIN-001.md,Ready for Development

# 3. Preview changes (dry run)
python scripts/batch_update_stories.py --input my_updates.csv --dry-run

# 4. Apply changes
python scripts/batch_update_stories.py --input my_updates.csv --backup
```

**Documentation:** See [docs/guides/story-status-audit-guide.md#batch-status-updates](../docs/guides/story-status-audit-guide.md#batch-status-updates)

---

## Common Workflows

### Weekly Story Audit

```bash
# 1. Run audit and save report
python scripts/story_status_audit.py > reports/status_$(date +%Y%m%d).txt

# 2. Check for stories needing attention
python scripts/story_status_audit.py --filter no-status

# 3. Export to CSV for sprint planning
python scripts/story_status_audit.py --filter ready --format csv --output sprint_candidates.csv
```

### Bulk Status Update

```bash
# 1. Generate audit to identify stories
python scripts/story_status_audit.py --format csv --output all_stories.csv

# 2. Filter in spreadsheet (e.g., category=no_status)
# 3. Create updates CSV with filtered stories

# 4. Dry run to preview
python scripts/batch_update_stories.py --input updates.csv --dry-run

# 5. Apply with backup
python scripts/batch_update_stories.py --input updates.csv --backup
```

### Sprint Planning

```bash
# Export ready stories for sprint planning
python scripts/story_status_audit.py --filter ready --format markdown --output SPRINT_CANDIDATES.md

# Open SPRINT_CANDIDATES.md in editor
# Select stories for sprint
# Update status to "In Progress" when started
```

---

## File Reference

| File | Purpose | Type |
|------|---------|------|
| `story_status_audit.py` | Analyze and report story statuses | Tool |
| `batch_update_stories.py` | Bulk update story statuses | Tool |
| `status_updates_template.csv` | Example CSV for batch updates | Template |
| `README.md` | This file | Documentation |

---

## Standard Status Values

Use these standardized status values for consistency:

| Status | When to Use |
|--------|-------------|
| `Draft` | Story needs refinement before development |
| `Ready for Development` | Story is ready to assign to developers |
| `In Progress` | Active development underway |
| `Ready for Review` | PR submitted, awaiting code review |
| `Done` | Completed, merged, and verified |
| `Backlog` | Deprioritized, not planned for current sprint |
| `Approved` | Approved for future development |

---

## Tips

### Find Stories Missing Status

```bash
python scripts/story_status_audit.py --filter no-status | tee missing_status.txt
```

### Export All Story Metadata

```bash
python scripts/story_status_audit.py --format json --output story_metadata.json
```

### Create Weekly Report

```bash
python scripts/story_status_audit.py --format markdown --output reports/STATUS_$(date +%Y%m%d).md
```

---

## Support

For detailed usage instructions, see:
- [Story Status Audit Guide](../docs/guides/story-status-audit-guide.md)

For issues or questions, contact the Product Owner (Sarah) or open an issue.
