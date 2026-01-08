#!/usr/bin/env python3
"""
Batch Story Status Updater
===========================

Updates story statuses in bulk based on a CSV input file.

Usage:
    python scripts/batch_update_stories.py --input updates.csv [--dry-run]

CSV Format:
    filepath,new_status
    docs/stories/TEA-BUILTIN-001.md,Ready for Development
    docs/stories/TEA-RUST-042.md,Done

Options:
    --input FILE    CSV file with updates (required)
    --dry-run       Preview changes without writing files
    --backup        Create .bak files before updating
"""

import csv
import re
import argparse
import shutil
from pathlib import Path
from typing import Optional


def update_story_status(
    filepath: str, new_status: str, dry_run: bool = False, backup: bool = False
) -> bool:
    """
    Update or insert status in a story file.

    Args:
        filepath: Path to story markdown file
        new_status: New status value to set
        dry_run: If True, only show what would be changed
        backup: If True, create .bak file before updating

    Returns:
        True if update was successful or would succeed (dry run)
    """
    try:
        with open(filepath, "r", encoding="utf-8") as f:
            original_content = f.read()

        content = original_content

        # Remove old format status (## Status: Value) if it exists
        old_format_removed = False
        if re.search(r"^## Status:\s*.+?$", content, re.MULTILINE):
            content = re.sub(r"^## Status:\s*.+?$", "", content, flags=re.MULTILINE)
            old_format_removed = True

        # Check if ## Status section exists (new format)
        status_exists = bool(re.search(r"^## Status\s*$", content, re.MULTILINE))

        if status_exists:
            # Update existing status
            # Match: "## Status\n\n<old_status>\n" and replace <old_status>
            updated_content = re.sub(
                r"(^## Status\s*\n+)(.+?)(\n|$)",
                rf"\g<1>{new_status}\g<3>",
                content,
                count=1,
                flags=re.MULTILINE,
            )

            # Check if status was actually empty
            if updated_content == content:
                # Status line was empty, insert status
                updated_content = re.sub(
                    r"(^## Status\s*\n+)",
                    rf"\g<1>{new_status}\n",
                    content,
                    count=1,
                    flags=re.MULTILINE,
                )

            action = (
                "UPDATE"
                if not old_format_removed
                else "UPDATE (converted from old format)"
            )
        else:
            # Insert new ## Status section after first heading
            # Find first # heading
            heading_match = re.search(r"^# .+?$", content, re.MULTILINE)

            if heading_match:
                heading_end = heading_match.end()
                updated_content = (
                    content[:heading_end]
                    + f"\n\n## Status\n\n{new_status}\n"
                    + content[heading_end:]
                )
                action = "INSERT"
            else:
                print(f"  ⚠️  WARNING: No main heading found in {filepath}")
                return False

        if dry_run:
            if updated_content != original_content:
                print(f"  [{action}] {filepath}")
                print(f"    Status: '{new_status}'")
                return True
            else:
                print(f"  [NO CHANGE] {filepath}")
                return True
        else:
            # Create backup if requested
            if backup:
                backup_path = filepath + ".bak"
                shutil.copy2(filepath, backup_path)
                print(f"  📦 Backup created: {backup_path}")

            # Write updated content
            with open(filepath, "w", encoding="utf-8") as f:
                f.write(updated_content)

            print(f"  ✅ [{action}] {filepath} → '{new_status}'")
            return True

    except FileNotFoundError:
        print(f"  ❌ ERROR: File not found: {filepath}")
        return False
    except Exception as e:
        print(f"  ❌ ERROR: Failed to update {filepath}: {e}")
        return False


def validate_csv(csv_path: str) -> bool:
    """Validate CSV format before processing."""
    try:
        with open(csv_path, "r", encoding="utf-8") as f:
            reader = csv.DictReader(f)

            required_fields = ["filepath", "new_status"]
            if not all(field in reader.fieldnames for field in required_fields):
                print(f"❌ ERROR: CSV must have columns: {', '.join(required_fields)}")
                print(f"   Found: {', '.join(reader.fieldnames)}")
                return False

            # Check if file is empty
            rows = list(reader)
            if not rows:
                print("❌ ERROR: CSV file is empty")
                return False

            print(f"✅ CSV validated: {len(rows)} updates to process")
            return True

    except FileNotFoundError:
        print(f"❌ ERROR: CSV file not found: {csv_path}")
        return False
    except Exception as e:
        print(f"❌ ERROR: Failed to read CSV: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(
        description="Batch update story statuses from CSV",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--input", required=True, help="CSV file with updates (filepath,new_status)"
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Preview changes without writing files"
    )
    parser.add_argument(
        "--backup", action="store_true", help="Create .bak files before updating"
    )

    args = parser.parse_args()

    # Validate CSV
    if not validate_csv(args.input):
        return 1

    # Process updates
    print("\n" + "=" * 80)
    if args.dry_run:
        print("DRY RUN MODE - No files will be modified")
    else:
        print("BATCH STATUS UPDATE")
    print("=" * 80 + "\n")

    success_count = 0
    fail_count = 0

    with open(args.input, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)

        for idx, row in enumerate(reader, 1):
            filepath = row["filepath"]
            new_status = row["new_status"]

            print(f"[{idx}] Processing: {filepath}")

            if update_story_status(filepath, new_status, args.dry_run, args.backup):
                success_count += 1
            else:
                fail_count += 1

            print()

    # Summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"✅ Successful updates: {success_count}")
    if fail_count > 0:
        print(f"❌ Failed updates: {fail_count}")

    if args.dry_run:
        print("\n💡 This was a dry run. Use without --dry-run to apply changes.")
    elif args.backup:
        print("\n📦 Backup files created with .bak extension")

    return 0 if fail_count == 0 else 1


if __name__ == "__main__":
    exit(main())
