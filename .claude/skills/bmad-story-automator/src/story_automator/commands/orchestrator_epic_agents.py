from __future__ import annotations

import json
import re
from pathlib import Path

from story_automator.core.frontmatter import find_frontmatter_value, parse_frontmatter
from story_automator.core.sprint import sprint_status_epic
from story_automator.core.story_keys import normalize_story_key
from story_automator.core.utils import file_exists, get_project_root, iso_now, print_json, read_text, trim_lines


def check_epic_complete_action(args: list[str]) -> int:
    if len(args) < 2:
        print_json({"ok": False, "error": "epic_number and story_id required"})
        return 1
    epic, story = args[0], args[1]
    state_file = ""
    tail = args[2:]
    for idx, arg in enumerate(tail):
        if arg == "--state-file" and idx + 1 < len(tail):
            state_file = tail[idx + 1]
    if story.split(".", 1)[0] != epic:
        print_json({"ok": True, "isLastStory": False, "epic": int(epic), "storyId": story, "reason": "story_not_in_epic"})
        return 0
    stories: list[str] = []
    if state_file and file_exists(state_file):
        story_range = parse_frontmatter(read_text(state_file)).get("storyRange", [])
        stories = [sid for sid in story_range if isinstance(sid, str) and sid.startswith(f"{epic}.")]
        source = "state_file"
    else:
        stories, _ = sprint_status_epic(get_project_root(), epic)
        source = "sprint_status"
    if stories:
        stories = sorted(set(stories), key=lambda item: tuple(int(part) for part in item.replace("-", ".").split(".")[:2]))
        last = stories[-1]
        print_json({"ok": True, "isLastStory": story in {last, last.replace("-", ".")}, "epic": int(epic), "storyId": story, "lastInEpic": last, "epicStoryCount": len(stories), "source": source})
        return 0
    print_json({"ok": True, "isLastStory": False, "epic": int(epic), "storyId": story, "reason": "could_not_determine", "source": "fallback"})
    return 0


def get_epic_stories_action(args: list[str]) -> int:
    if not args:
        print_json({"ok": False, "error": "epic_number_required"})
        return 1
    epic = args[0]
    state_file = ""
    tail = args[1:]
    for idx, arg in enumerate(tail):
        if arg == "--state-file" and idx + 1 < len(tail):
            state_file = tail[idx + 1]
    if state_file and file_exists(state_file):
        stories = [sid for sid in parse_frontmatter(read_text(state_file)).get("storyRange", []) if isinstance(sid, str) and sid.startswith(f"{epic}.")]
        if stories:
            print_json({"ok": True, "epic": epic, "stories": stories, "count": len(stories), "source": "state_file"})
            return 0
    stories, _ = sprint_status_epic(get_project_root(), epic)
    if stories:
        print_json({"ok": True, "epic": epic, "stories": stories, "count": len(stories), "source": "sprint_status"})
        return 0
    epic_file = find_epic_file(epic)
    if epic_file:
        stories = sorted(set(re.findall(rf"\b{re.escape(epic)}\.\d+", read_text(epic_file))), key=lambda item: tuple(int(part) for part in item.split(".")))
        if stories:
            print_json({"ok": True, "epic": epic, "stories": stories, "count": len(stories), "source": "epic_file"})
            return 0
    print_json({"ok": False, "epic": epic, "error": "no_stories_found", "count": 0})
    return 0


def check_blocking_action(args: list[str]) -> int:
    if not args:
        print_json({"ok": False, "error": "story_id_required"})
        return 1
    norm = normalize_story_key(get_project_root(), args[0])
    if norm is None:
        print_json({"ok": False, "error": "could_not_normalize_key", "input": args[0]})
        return 1
    epic = norm.id.split(".", 1)[0]
    epic_file = find_epic_file(epic)
    if not epic_file:
        print_json({"ok": True, "blocking": True, "story": norm.id, "epic": epic, "dependents": [], "reason": "epic_file_not_found", "source": "unknown"})
        return 0
    dependents: list[str] = []
    current_story = ""
    for line in trim_lines(read_text(epic_file)):
        match = re.match(r"^###\s+Story\s+(\d+\.\d+):", line)
        if match:
            current_story = match.group(1)
            continue
        if current_story and re.search(r"(?i)Dependencies:|\*\*Dependencies\*\*:", line):
            if norm.id in line or norm.prefix in line:
                dependents.append(current_story)
    if dependents:
        print_json({"ok": True, "blocking": True, "story": norm.id, "epic": epic, "dependents": sorted(set(dependents)), "reason": "dependent_stories", "source": "epic_file"})
        return 0
    print_json({"ok": True, "blocking": False, "story": norm.id, "epic": epic, "dependents": [], "reason": "no_dependents_found", "source": "epic_file"})
    return 0


def agents_build_action(args: list[str]) -> int:
    options = {"state-file": "", "complexity-file": "", "output": "", "config-json": ""}
    idx = 0
    while idx < len(args):
        key = args[idx].lstrip("-")
        if idx + 1 < len(args):
            options[key] = args[idx + 1]
            idx += 2
        else:
            idx += 1
    if not all(options.values()) or not file_exists(options["state-file"]) or not file_exists(options["complexity-file"]):
        print_json({"ok": False, "error": "missing_args" if not all(options.values()) else "file_not_found"})
        return 1
    config = parse_agent_config(options["config-json"])
    complexity = json.loads(read_text(options["complexity-file"]))
    state_fields = parse_frontmatter(read_text(options["state-file"]))
    stories = []
    for story in complexity.get("stories", []):
        level = str(story.get("complexity", {}).get("level", "medium")).lower() or "medium"
        tasks = {}
        for task in ("create", "dev", "auto", "review"):
            primary, fallback = resolve_agent(config, level, task)
            tasks[task] = {"primary": primary, "fallback": False if fallback == "false" else fallback}
        stories.append({"storyId": story["storyId"], "title": story.get("title", ""), "complexity": level, "tasks": tasks})
    payload = {"version": "1.0.0", "stateFile": options["state-file"], "epic": state_fields.get("epic", ""), "epicName": state_fields.get("epicName", ""), "createdAt": iso_now(), "stories": stories}
    header = f'---\nstateFile: "{payload["stateFile"]}"\ncreatedAt: "{payload["createdAt"]}"\n---\n\n# Agents Plan: {payload["epicName"]}\n\n'
    content = header + "```json\n" + json.dumps(payload, indent=2) + "\n```\n"
    Path(options["output"]).parent.mkdir(parents=True, exist_ok=True)
    Path(options["output"]).write_text(content, encoding="utf-8")
    print_json({"ok": True, "path": options["output"], "stories": len(stories)})
    return 0


def agents_resolve_action(args: list[str]) -> int:
    options = {"state-file": "", "agents-file": "", "story": "", "task": ""}
    idx = 0
    while idx < len(args):
        key = args[idx].lstrip("-")
        if idx + 1 < len(args):
            options[key] = args[idx + 1]
            idx += 2
        else:
            idx += 1
    if not options["story"] or not options["task"] or (not options["state-file"] and not options["agents-file"]):
        print_json({"ok": False, "error": "missing_args"})
        return 1
    agents_path = options["agents-file"] or find_frontmatter_value(options["state-file"], "agentsFile")
    if not agents_path or not file_exists(agents_path):
        print_json({"ok": False, "error": "agents_file_not_found"})
        return 1
    text = read_text(agents_path)
    match = re.search(r"(?s)```json\s*(\{.*?\})\s*```", text)
    block = match.group(1) if match else text.strip()
    payload = json.loads(block)
    for story in payload.get("stories", []):
        if story.get("storyId") != options["story"]:
            continue
        selection = story.get("tasks", {}).get(options["task"])
        if selection is None:
            print_json({"ok": False, "error": "task_not_found"})
            return 1
        fallback = selection.get("fallback", "")
        fallback = "false" if fallback in {False, "false", "none", "null"} else fallback
        print_json({"ok": True, "story": options["story"], "task": options["task"], "primary": selection.get("primary", ""), "fallback": fallback, "complexity": story.get("complexity", "")})
        return 0
    print_json({"ok": False, "error": "story_not_found"})
    return 1


def find_epic_file(epic: str) -> str:
    root = Path(get_project_root())
    for pattern in (f"_bmad-output/implementation-artifacts/epic-{epic}-*.md", f"docs/epics/epic-{epic}-*.md"):
        matches = sorted(root.glob(pattern))
        if matches:
            return str(matches[0])
    return ""


def parse_agent_config(raw: str) -> dict:
    data = json.loads(raw)
    return {
        "defaultPrimary": data.get("defaultPrimary") or data.get("primary") or "claude",
        "defaultFallback": data.get("defaultFallback") or data.get("fallback") or "codex",
        "perTask": data.get("perTask", {}),
        "complexityOverrides": data.get("complexityOverrides") or {level: data[level] for level in ("low", "medium", "high") if level in data},
    }


def resolve_agent(config: dict, level: str, task: str) -> tuple[str, str]:
    primary = config["defaultPrimary"]
    fallback = config["defaultFallback"]
    if task in config["perTask"]:
        entry = config["perTask"][task]
        primary = entry.get("primary", primary)
        if "fallback" in entry:
            fallback = "false" if entry["fallback"] in {False, "false", "none", "null"} else entry["fallback"]
    level_map = config["complexityOverrides"].get(level, {})
    if task in level_map:
        entry = level_map[task]
        primary = entry.get("primary", primary)
        if "fallback" in entry:
            fallback = "false" if entry["fallback"] in {False, "false", "none", "null"} else entry["fallback"]
    return (primary or "claude", fallback or "codex")
