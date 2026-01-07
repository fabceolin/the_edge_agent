# Story Status Tools - Quick Reference

## 🚀 Common Commands

### Check Story Status

```bash
# Full audit report
python scripts/story_status_audit.py

# Find stories ready to work on
python scripts/story_status_audit.py --filter ready

# Find stories missing status
python scripts/story_status_audit.py --filter no-status

# Find draft stories needing refinement
python scripts/story_status_audit.py --filter draft
```

### Export Reports

```bash
# CSV for spreadsheet analysis
python scripts/story_status_audit.py --format csv --output status.csv

# JSON for automation
python scripts/story_status_audit.py --format json --output status.json

# Markdown for documentation
python scripts/story_status_audit.py --format markdown --output STATUS_REPORT.md
```

### Batch Update Statuses

```bash
# 1. Create CSV with updates
cat > my_updates.csv << 'EOF'
filepath,new_status
docs/stories/TEA-BUILTIN-001.md,Ready for Development
docs/stories/TEA-RUST-042.md,Done
EOF

# 2. Preview changes (dry run)
python scripts/batch_update_stories.py --input my_updates.csv --dry-run

# 3. Apply changes with backup
python scripts/batch_update_stories.py --input my_updates.csv --backup
```

## 📋 Standard Status Values

| Status | Meaning |
|--------|---------|
| `Draft` | Needs refinement |
| `Ready for Development` | Ready to assign |
| `In Progress` | Active development |
| `Ready for Review` | Awaiting code review |
| `Done` | Completed |
| `Backlog` | Deprioritized |

## 🔍 Useful Filters

| Filter | Shows |
|--------|-------|
| `--filter ready` | Ready for Development |
| `--filter draft` | Draft stories |
| `--filter no-status` | Missing status |
| `--filter in_progress` | Active work |
| `--filter done` | Completed stories |

## 📊 Sprint Planning Workflow

```bash
# 1. Export ready stories
python scripts/story_status_audit.py --filter ready --format csv --output sprint_candidates.csv

# 2. Open in spreadsheet, filter by epic/priority

# 3. Create batch update for selected stories
# filepath,new_status
# docs/stories/TEA-BUILTIN-001.5.md,In Progress
# docs/stories/TEA-RUST-043.1.md,In Progress

# 4. Apply updates
python scripts/batch_update_stories.py --input sprint_stories.csv --backup
```

## 🧹 Weekly Audit

```bash
# Save weekly report
python scripts/story_status_audit.py > reports/status_$(date +%Y%m%d).txt

# Check for issues
python scripts/story_status_audit.py --filter no-status | tee issues.txt
```

## 📖 Full Documentation

- **Audit Guide**: [docs/guides/story-status-audit-guide.md](../docs/guides/story-status-audit-guide.md)
- **Scripts README**: [scripts/README.md](README.md)

## 💡 Pro Tips

1. **Always dry-run first**: Use `--dry-run` to preview batch updates
2. **Use backups**: Always use `--backup` when batch updating
3. **Regular audits**: Run weekly to maintain status hygiene
4. **CSV workflow**: Export → Filter → Update for bulk changes
5. **Commit after updates**: Review and commit status changes immediately

---

**Need help?** See the full documentation or contact the Product Owner (Sarah).
