# Story Status Audit Guide

This guide explains how to use the **Story Status Audit Tool** to analyze and report on story statuses across your project.

## Table of Contents

- [Quick Start](#quick-start)
- [Installation](#installation)
- [Usage Examples](#usage-examples)
- [Output Formats](#output-formats)
- [Status Categories](#status-categories)
- [Batch Status Updates](#batch-status-updates)
- [Best Practices](#best-practices)

---

## Quick Start

```bash
# Run basic audit (console output)
python scripts/story_status_audit.py

# Find stories ready for development
python scripts/story_status_audit.py --filter ready

# Export to CSV for spreadsheet analysis
python scripts/story_status_audit.py --format csv --output status_report.csv

# Generate markdown report for documentation
python scripts/story_status_audit.py --format markdown --output STATUS_REPORT.md
```

---

## Installation

The audit tool is a standalone Python script with no external dependencies (uses only Python standard library).

**Requirements:**
- Python 3.7+
- Story files in `docs/stories/*.md`

**Make the script executable (optional):**

```bash
chmod +x scripts/story_status_audit.py
./scripts/story_status_audit.py  # Run directly
```

---

## Usage Examples

### 1. Console Report (Default)

Display a comprehensive report in the terminal:

```bash
python scripts/story_status_audit.py
```

**Output:**
```
================================================================================
STORY STATUS AUDIT REPORT
================================================================================

✅ READY FOR DEVELOPMENT (16 stories)
--------------------------------------------------------------------------------
  [Ready] TEA-BUILTIN-001.5.cloud-native-ltm.md
  [Ready for Dev] TEA-YAML-004-generic-extraction-validation.md
  ...

📝 DRAFT (6 stories)
--------------------------------------------------------------------------------
  [Draft] TEA-BUILTIN-005.opik-integration-epic.md
  ...

⚠️  NO STATUS (73 stories)
--------------------------------------------------------------------------------
  - DOC-001.consolidate-yaml-docs.md
  - TEA-BUILTIN-008-llamaextract-integration-epic.md
  ...

================================================================================
SUMMARY
================================================================================
Total stories: 227
Workable (Ready/Draft/Approved): 26
Needs status assignment: 73
```

### 2. Filter Stories by Status

**Find all stories ready for development:**

```bash
python scripts/story_status_audit.py --filter ready
```

**Find all draft stories:**

```bash
python scripts/story_status_audit.py --filter draft
```

**Find all stories missing status:**

```bash
python scripts/story_status_audit.py --filter no-status
```

### 3. Export to CSV

Export for analysis in Excel, Google Sheets, or other tools:

```bash
python scripts/story_status_audit.py --format csv --output reports/status_audit_$(date +%Y%m%d).csv
```

**CSV columns:**
- `story_id` - Story identifier (e.g., TEA-BUILTIN-001)
- `filename` - Markdown filename
- `type` - Story type (Feature, Epic, Technical Debt, Documentation)
- `epic` - Parent epic name
- `status` - Raw status value from file
- `category` - Normalized status category
- `has_ac` - Has Acceptance Criteria (boolean)
- `has_tasks` - Has Tasks section (boolean)
- `has_dod` - Has Definition of Done (boolean)
- `qa_complete` - QA section filled out (boolean)
- `dev_complete` - Dev Agent Record filled out (boolean)

### 4. Export to JSON

Export for programmatic processing:

```bash
python scripts/story_status_audit.py --format json --output status_report.json
```

**JSON structure:**
```json
{
  "summary": {
    "total": 227,
    "by_category": {
      "ready_dev": 16,
      "draft": 6,
      "no_status": 73,
      ...
    },
    "workable": 26,
    "needs_triage": 73
  },
  "stories": [
    {
      "filename": "TEA-BUILTIN-001.5.cloud-native-ltm.md",
      "story_id": "TEA-BUILTIN-001.5",
      "epic": "TEA-BUILTIN-001",
      "type": "Feature",
      "status": "Ready",
      "category": "ready_dev",
      "has_ac": true,
      "has_tasks": true,
      "has_dod": true,
      "qa_complete": false,
      "dev_complete": false
    },
    ...
  ]
}
```

### 5. Generate Markdown Report

Create a formatted markdown report for documentation:

```bash
python scripts/story_status_audit.py --format markdown --output docs/STATUS_REPORT.md
```

This generates a GitHub-flavored markdown file with:
- Summary statistics
- Stories grouped by status category
- Links to story files

---

## Output Formats

### Console (Default)

- **Best for**: Quick checks, CI/CD pipelines
- **Features**: Color-coded categories, grouped display
- **Usage**: `--format console` (default)

### CSV

- **Best for**: Spreadsheet analysis, filtering, sorting
- **Features**: All metadata fields, importable to Excel/Sheets
- **Usage**: `--format csv --output FILE.csv`

### JSON

- **Best for**: Automation, integration with other tools
- **Features**: Structured data with summary statistics
- **Usage**: `--format json --output FILE.json`

### Markdown

- **Best for**: Documentation, GitHub wikis, PRs
- **Features**: Formatted report with emoji, links
- **Usage**: `--format markdown --output FILE.md`

---

## Status Categories

The audit tool normalizes status values into these categories:

| Category | Status Keywords | Description |
|----------|----------------|-------------|
| **ready_dev** | `Ready for Dev`, `Ready for Development`, `Ready`, `Approved - Ready for Development` | Stories ready to assign to developers |
| **draft** | `Draft`, `Planned` | Stories needing refinement |
| **backlog** | `Backlog`, `Proposed`, `Pending` | Deprioritized stories |
| **ready_review** | `Ready for Review`, `Ready for Merge` | Stories awaiting review |
| **approved** | `Approved` | Stories approved but not yet started |
| **in_progress** | `In Progress`, `WIP`, `Work in Progress` | Active development |
| **done** | `Done`, `Complete`, `Completed`, `Merged`, `Superseded`, `QA Approved` | Completed stories |
| **no_status** | (missing or empty `## Status` section) | Stories needing status assignment |
| **other** | Any other value | Non-standard status values |

---

## Batch Status Updates

### Finding Stories That Need Status

**Step 1: Generate CSV report**

```bash
python scripts/story_status_audit.py --format csv --output status_audit.csv
```

**Step 2: Filter in spreadsheet**

Open `status_audit.csv` and filter by:
- `category = no_status` - Stories needing status
- `has_ac = TRUE` AND `has_tasks = TRUE` - Likely ready for dev
- `qa_complete = TRUE` AND `dev_complete = TRUE` - Likely done

**Step 3: Batch update script**

Create `scripts/batch_update_status.sh`:

```bash
#!/bin/bash
# Batch update story statuses

# Array of stories to mark as "Ready for Development"
ready_stories=(
    "docs/stories/DOC-001.consolidate-yaml-docs.md"
    "docs/stories/TEA-DOCS-001-readme-neurosymbolic-focus.md"
    "docs/stories/TEA-PY-003-while-loop-node.md"
)

for story in "${ready_stories[@]}"; do
    echo "Updating $story..."

    # Check if ## Status section exists
    if grep -q "^## Status" "$story"; then
        # Replace empty status with "Ready for Development"
        sed -i '/^## Status$/{ n; s/^$/Ready for Development/; }' "$story"
    else
        # Insert ## Status section after first heading
        sed -i '0,/^# /a\\n## Status\n\nReady for Development' "$story"
    fi
done

echo "Status updates complete!"
```

**Step 4: Run the batch update**

```bash
chmod +x scripts/batch_update_status.sh
./scripts/batch_update_status.sh
```

**Step 5: Verify changes**

```bash
python scripts/story_status_audit.py --filter ready
```

### Python Batch Update Script

For more control, use Python:

```python
#!/usr/bin/env python3
"""Batch update story statuses based on CSV input."""

import csv
import re

def update_status(filepath, new_status):
    """Update or insert status in a story file."""
    with open(filepath, 'r') as f:
        content = f.read()

    # Check if ## Status exists
    if '## Status' in content:
        # Replace the line after ## Status
        content = re.sub(
            r'(^## Status\s*\n+)(.+?)$',
            rf'\g<1>{new_status}',
            content,
            flags=re.MULTILINE
        )
    else:
        # Insert after first heading
        content = re.sub(
            r'(^# .+?\n)',
            rf'\g<1>\n## Status\n\n{new_status}\n',
            content,
            count=1
        )

    with open(filepath, 'w') as f:
        f.write(content)

# Read updates from CSV
with open('status_updates.csv', 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        filepath = row['filepath']
        new_status = row['new_status']

        print(f"Updating {filepath} to '{new_status}'")
        update_status(filepath, new_status)

print("Batch update complete!")
```

**Create `status_updates.csv`:**

```csv
filepath,new_status
docs/stories/DOC-001.consolidate-yaml-docs.md,Ready for Development
docs/stories/TEA-DOCS-001-readme-neurosymbolic-focus.md,Ready for Development
docs/stories/TEA-BUILTIN-008-llamaextract-integration-epic.md,Backlog
```

**Run:**

```bash
python scripts/batch_update_stories.py
```

---

## Best Practices

### Regular Audits

Run weekly audits to maintain status hygiene:

```bash
# Weekly status check
python scripts/story_status_audit.py > reports/status_$(date +%Y%m%d).txt

# Track stories needing attention
python scripts/story_status_audit.py --filter no-status
```

### Sprint Planning

Use the audit tool during sprint planning:

```bash
# Export ready stories to CSV
python scripts/story_status_audit.py --filter ready --format csv --output sprint_candidates.csv

# Open in spreadsheet, sort by epic/priority, assign to sprint
```

### CI/CD Integration

Add to your CI pipeline:

```yaml
# .github/workflows/story-audit.yml
name: Story Status Audit

on:
  pull_request:
    paths:
      - 'docs/stories/**'

jobs:
  audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run story audit
        run: |
          python scripts/story_status_audit.py
          python scripts/story_status_audit.py --filter no-status | tee audit_warnings.txt

          # Fail if new stories added without status
          if [ -s audit_warnings.txt ]; then
            echo "::warning::Stories without status detected"
          fi
```

### Status Standardization

Maintain consistency by using standard status values:

**Recommended status values:**

```
Draft                     # Story needs refinement
Ready for Development     # Story ready for dev assignment
In Progress               # Active development
Ready for Review          # Awaiting PR review
Done                      # Completed and merged
Backlog                   # Deprioritized
```

**Update non-standard statuses:**

```bash
# Find non-standard statuses
python scripts/story_status_audit.py --filter other

# Update manually or with batch script
```

---

## Troubleshooting

### Issue: Script doesn't find stories

**Solution:** Check `--stories-dir` parameter:

```bash
python scripts/story_status_audit.py --stories-dir path/to/stories
```

### Issue: Status not detected

**Solution:** The audit tool supports multiple formats:

**Format 1 (Preferred):**
```markdown
## Status

Ready for Development
```

**Format 2 (Old - Supported but deprecated):**
```markdown
## Status: Ready for Development
```

**Not supported:**
```markdown
## Status Ready for Development  # ❌ Wrong (no colon, no newline)
```

**Note:** The batch update tool will automatically convert Format 2 to Format 1 when updating stories.

### Issue: CSV file encoding errors

**Solution:** Use UTF-8 compatible tools (VS Code, LibreOffice, modern Excel)

---

## Related Documentation

- [Story Template](../../.bmad-core/templates/story-tmpl.yaml)
- [PO Master Checklist](../../.bmad-core/checklists/po-master-checklist.md)
- [Validate Next Story Task](../../.bmad-core/tasks/validate-next-story.md)

---

## Support

For issues or feature requests, contact the Product Owner (Sarah) or open an issue in the project repository.
