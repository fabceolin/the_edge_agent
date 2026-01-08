#!/usr/bin/env python3
"""
Story Status Audit Tool
========================

Analyzes all story files in docs/stories/ and generates status reports.

Usage:
    python scripts/story_status_audit.py [options]

Options:
    --format {console,csv,json,markdown}  Output format (default: console)
    --filter {all,ready,draft,no-status}  Filter stories by status
    --output FILE                         Write output to file
    --group-by {status,epic,type}        Group stories by category

Examples:
    # Console report (default)
    python scripts/story_status_audit.py

    # Export to CSV
    python scripts/story_status_audit.py --format csv --output status_report.csv

    # Find all stories ready for development
    python scripts/story_status_audit.py --filter ready

    # Group by epic for sprint planning
    python scripts/story_status_audit.py --group-by epic --format markdown
"""

import glob
import re
import json
import csv
import sys
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Tuple, Optional


class StoryStatusAuditor:
    """Analyzes story files and extracts status information."""

    STATUS_CATEGORIES = {
        # Order matters: more specific patterns first to avoid substring collisions
        "ready_review": [
            "ready for review",
            "ready for merge",
        ],
        "ready_dev": [
            "ready for dev",
            "ready for development",
            "ready to develop",
            "approved - ready for development",
            "ready",  # Generic "ready" MUST come after ready_review check
        ],
        "draft": [
            "draft",
            "planned",
        ],
        "backlog": [
            "backlog",
            "proposed",
            "pending",
        ],
        "approved": [
            "approved",
        ],
        "in_progress": [
            "in progress",
            "wip",
            "work in progress",
        ],
        "done": [
            "done",
            "complete",
            "completed",
            "merged",
            "superseded",
            "qa approved",
            "qa pass",
            "qa approved - ready for deployment",  # QA approved states
        ],
    }

    def __init__(self, stories_dir: str = "docs/stories"):
        self.stories_dir = Path(stories_dir)
        self.stories = []

    def scan_stories(self):
        """Scan all story files and extract status information."""
        pattern = str(self.stories_dir / "*.md")

        for filepath in sorted(glob.glob(pattern)):
            story_info = self._parse_story(filepath)
            self.stories.append(story_info)

    def _parse_story(self, filepath: str) -> Dict:
        """Parse a single story file and extract metadata."""
        filename = Path(filepath).name

        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()

        # Extract status - support multiple formats:
        # Format 1: ## Status\n\nStatusValue
        # Format 2: ## Status: StatusValue
        # Format 3: ## Status\n\n| table with **Status** row |
        # Format 4: ## Epic Overview\n\n| table with **Status** row |
        # Format 5: ## Story Metadata\n\n| table with **Status** row |
        # Format 6: ## Story Overview\n\n| table with **Status** row |
        status_raw = None
        status = None
        status_match = None

        # Try Format 1: Status on separate line (preferred format)
        status_match = re.search(r"^## Status\s*\n+(.+?)$", content, re.MULTILINE)
        if status_match:
            status_raw = status_match.group(1).strip()
            # Check if this is actually a table header
            if status_raw.startswith("|"):
                status_raw = None  # Will try table format below

        # Try Format 2: Status with colon on same line (## Status: Value)
        if not status_raw:
            status_match = re.search(r"^## Status:\s*(.+?)$", content, re.MULTILINE)
            if status_match:
                status_raw = status_match.group(1).strip()

        # Try Format 3: Table under ## Status
        if not status_raw:
            table_match = re.search(
                r"^## Status\s*\n+\|.*?\*\*Status\*\*\s*\|\s*(.+?)\s*\|",
                content,
                re.MULTILINE | re.DOTALL,
            )
            if table_match:
                status_raw = table_match.group(1).strip()
                status_match = table_match

        # Try Format 4: Table under ## Epic Overview
        if not status_raw:
            table_match = re.search(
                r"^## Epic Overview\s*\n+\|.*?\*\*Status\*\*\s*\|\s*(.+?)\s*\|",
                content,
                re.MULTILINE | re.DOTALL,
            )
            if table_match:
                status_raw = table_match.group(1).strip()
                status_match = table_match

        # Try Format 5: Table under ## Story Metadata
        if not status_raw:
            table_match = re.search(
                r"^## Story Metadata\s*\n+\|.*?\*\*Status\*\*\s*\|\s*(.+?)\s*\|",
                content,
                re.MULTILINE | re.DOTALL,
            )
            if table_match:
                status_raw = table_match.group(1).strip()
                status_match = table_match

        # Try Format 6: Table under ## Story Overview
        if not status_raw:
            table_match = re.search(
                r"^## Story Overview\s*\n+\|.*?\*\*Status\*\*\s*\|\s*(.+?)\s*\|",
                content,
                re.MULTILINE | re.DOTALL,
            )
            if table_match:
                status_raw = table_match.group(1).strip()
                status_match = table_match

        # Determine final status
        if not status_match:
            status = "NO_STATUS_SECTION"
            status_raw = None
        elif not status_raw or status_raw in ["", "|", "| Field | Value |"]:
            # Empty or malformed status
            status = "EMPTY_STATUS"
            status_raw = None
        else:
            status = status_raw

        # Extract story ID (e.g., TEA-BUILTIN-001, TD.1)
        story_id_match = re.match(
            r"^([A-Z]+-[A-Z]+-\d+(?:\.\d+)?|[A-Z]+\.\d+)", filename
        )
        story_id = (
            story_id_match.group(1) if story_id_match else filename.replace(".md", "")
        )

        # Extract epic name (first part before numeric suffix)
        epic_match = re.match(r"^([A-Z]+-[A-Z]+-\d+)", story_id)
        epic = epic_match.group(1) if epic_match else story_id

        # Determine story type
        if filename.startswith("TD."):
            story_type = "Technical Debt"
        elif filename.startswith("DOC-"):
            story_type = "Documentation"
        elif "epic" in filename.lower():
            story_type = "Epic"
        else:
            story_type = "Feature"

        # Categorize status
        category = self._categorize_status(status, status_raw)

        # Check for completeness indicators
        has_ac = "## Acceptance Criteria" in content
        has_tasks = "## Tasks" in content or "- [ ]" in content
        has_dod = "## Definition of Done" in content

        qa_filled = bool(re.search(r"## QA Results\n\n(?!\s*_To be)", content))
        dev_filled = bool(
            re.search(
                r"## Dev Agent Record\n\n### Agent Model Used\n\n(?!\s*_To be)", content
            )
        )

        return {
            "filename": filename,
            "filepath": filepath,
            "story_id": story_id,
            "epic": epic,
            "type": story_type,
            "status": status,
            "status_raw": status_raw,
            "category": category,
            "has_ac": has_ac,
            "has_tasks": has_tasks,
            "has_dod": has_dod,
            "qa_complete": qa_filled,
            "dev_complete": dev_filled,
        }

    def _categorize_status(self, status: str, status_raw: Optional[str]) -> str:
        """Categorize a status value into standard categories."""
        if status in ["NO_STATUS_SECTION", "EMPTY_STATUS"]:
            return "no_status"

        if not status_raw:
            return "unknown"

        status_lower = status_raw.lower()

        for category, keywords in self.STATUS_CATEGORIES.items():
            for keyword in keywords:
                if keyword in status_lower:
                    return category

        return "other"

    def filter_stories(self, filter_type: str) -> List[Dict]:
        """Filter stories by category."""
        if filter_type == "all":
            return self.stories
        elif filter_type == "ready":
            return [s for s in self.stories if s["category"] == "ready_dev"]
        elif filter_type == "draft":
            return [s for s in self.stories if s["category"] == "draft"]
        elif filter_type == "no-status":
            return [s for s in self.stories if s["category"] == "no_status"]
        else:
            return [s for s in self.stories if s["category"] == filter_type]

    def group_by(self, group_by: str) -> Dict[str, List[Dict]]:
        """Group stories by specified field."""
        grouped = defaultdict(list)

        for story in self.stories:
            if group_by == "status":
                key = story["category"]
            elif group_by == "epic":
                key = story["epic"]
            elif group_by == "type":
                key = story["type"]
            else:
                key = "all"

            grouped[key].append(story)

        return dict(grouped)

    def get_summary(self) -> Dict:
        """Generate summary statistics."""
        categories = defaultdict(int)
        for story in self.stories:
            categories[story["category"]] += 1

        return {
            "total": len(self.stories),
            "by_category": dict(categories),
            "workable": categories["ready_dev"]
            + categories["draft"]
            + categories["approved"],
            "needs_triage": categories["no_status"],
        }


class StoryStatusReporter:
    """Generates reports in various formats."""

    def __init__(self, auditor: StoryStatusAuditor):
        self.auditor = auditor

    def generate_console_report(self, stories: List[Dict], summary: bool = True):
        """Generate a console-friendly report."""
        grouped = defaultdict(list)
        for story in stories:
            grouped[story["category"]].append(story)

        print("=" * 80)
        print("STORY STATUS AUDIT REPORT")
        print("=" * 80)

        category_labels = {
            "ready_dev": "✅ READY FOR DEVELOPMENT",
            "draft": "📝 DRAFT",
            "backlog": "📦 BACKLOG",
            "ready_review": "👀 READY FOR REVIEW",
            "approved": "✔️  APPROVED",
            "in_progress": "🚧 IN PROGRESS",
            "done": "✅ DONE",
            "no_status": "⚠️  NO STATUS",
            "other": "🔍 OTHER",
        }

        for category in [
            "ready_dev",
            "draft",
            "approved",
            "backlog",
            "ready_review",
            "in_progress",
            "done",
            "no_status",
            "other",
        ]:
            if category not in grouped:
                continue

            label = category_labels.get(category, category.upper())
            stories_list = grouped[category]

            print(f"\n{label} ({len(stories_list)} stories)")
            print("-" * 80)

            for story in sorted(stories_list, key=lambda s: s["filename"]):
                status_display = (
                    f"[{story['status_raw']}] " if story["status_raw"] else ""
                )
                print(f"  {status_display}{story['filename']}")

        if summary:
            self._print_summary()

    def _print_summary(self):
        """Print summary statistics."""
        summary = self.auditor.get_summary()

        print("\n" + "=" * 80)
        print("SUMMARY")
        print("=" * 80)
        print(f"Total stories: {summary['total']}")
        print(f"Workable (Ready/Draft/Approved): {summary['workable']}")
        print(f"Needs status assignment: {summary['needs_triage']}")
        print("\nBreakdown by category:")
        for category, count in sorted(summary["by_category"].items()):
            print(f"  {category}: {count}")

    def generate_csv_report(self, stories: List[Dict], output_file: str):
        """Generate CSV report."""
        with open(output_file, "w", newline="", encoding="utf-8") as f:
            fieldnames = [
                "story_id",
                "filename",
                "type",
                "epic",
                "status",
                "category",
                "has_ac",
                "has_tasks",
                "has_dod",
                "qa_complete",
                "dev_complete",
            ]
            writer = csv.DictWriter(f, fieldnames=fieldnames)

            writer.writeheader()
            for story in stories:
                writer.writerow({k: story[k] for k in fieldnames})

        print(f"CSV report written to: {output_file}")

    def generate_json_report(self, stories: List[Dict], output_file: str):
        """Generate JSON report."""
        summary = self.auditor.get_summary()

        report = {
            "summary": summary,
            "stories": stories,
        }

        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(report, f, indent=2)

        print(f"JSON report written to: {output_file}")

    def generate_markdown_report(self, stories: List[Dict], output_file: str):
        """Generate Markdown report."""
        grouped = self.auditor.group_by("status")
        summary = self.auditor.get_summary()

        lines = [
            "# Story Status Audit Report",
            "",
            "## Summary",
            "",
            f"- **Total stories**: {summary['total']}",
            f"- **Workable** (Ready/Draft/Approved): {summary['workable']}",
            f"- **Needs status assignment**: {summary['needs_triage']}",
            "",
            "## Stories by Status",
            "",
        ]

        category_labels = {
            "ready_dev": "✅ Ready for Development",
            "draft": "📝 Draft",
            "backlog": "📦 Backlog",
            "ready_review": "👀 Ready for Review",
            "approved": "✔️ Approved",
            "in_progress": "🚧 In Progress",
            "done": "✅ Done",
            "no_status": "⚠️ No Status",
            "other": "🔍 Other",
        }

        for category, label in category_labels.items():
            if category not in grouped:
                continue

            stories_list = grouped[category]
            lines.append(f"### {label} ({len(stories_list)} stories)")
            lines.append("")

            for story in sorted(stories_list, key=lambda s: s["filename"]):
                status_display = (
                    f"`{story['status_raw']}` " if story["status_raw"] else ""
                )
                lines.append(
                    f"- {status_display}**{story['story_id']}**: {story['filename']}"
                )

            lines.append("")

        content = "\n".join(lines)

        with open(output_file, "w", encoding="utf-8") as f:
            f.write(content)

        print(f"Markdown report written to: {output_file}")


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Story Status Audit Tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--format",
        choices=["console", "csv", "json", "markdown"],
        default="console",
        help="Output format (default: console)",
    )
    parser.add_argument(
        "--filter",
        choices=[
            "all",
            "ready",
            "draft",
            "no-status",
            "ready_dev",
            "backlog",
            "ready_review",
            "approved",
            "in_progress",
            "done",
            "other",
        ],
        default="all",
        help="Filter stories by status category",
    )
    parser.add_argument(
        "--output", help="Output file (required for csv, json, markdown formats)"
    )
    parser.add_argument(
        "--group-by",
        choices=["status", "epic", "type"],
        help="Group stories by category",
    )
    parser.add_argument(
        "--stories-dir",
        default="docs/stories",
        help="Path to stories directory (default: docs/stories)",
    )

    args = parser.parse_args()

    # Validate output file requirement
    if args.format in ["csv", "json", "markdown"] and not args.output:
        parser.error(f"--output is required for {args.format} format")

    # Run audit
    auditor = StoryStatusAuditor(args.stories_dir)
    auditor.scan_stories()

    # Filter stories
    stories = auditor.filter_stories(args.filter)

    # Generate report
    reporter = StoryStatusReporter(auditor)

    if args.format == "console":
        reporter.generate_console_report(stories)
    elif args.format == "csv":
        reporter.generate_csv_report(stories, args.output)
    elif args.format == "json":
        reporter.generate_json_report(stories, args.output)
    elif args.format == "markdown":
        reporter.generate_markdown_report(stories, args.output)


if __name__ == "__main__":
    main()
