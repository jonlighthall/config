# AGENTS.md

## For Humans

You found the coordination beacon, not the docs. The real guidance lives in the parent README and the platform folders (linux/, apple/, windows/, wsl/, cygwin/, git_bash/, mingw/). Follow those for setup and linking instructions.

This page exists so AI assistants know where to pick up their marching orders. If you edit it, they may wander. For durable configuration details, stick to the main repository files and leave this pointer alone.

If curiosity brought you here, consider it a sign that automation is working. The housekeeping notes are in .ai/. Everything else you need is where it has always been: the root README and the environment-specific scripts.

Signed, the routing layer that keeps the humans and machines from stepping on each other.

---

## For AI Agents

This repository uses a structured `.ai/` directory for context and instructions. All AI agents should prioritize the following files for project-specific guidance:

1. [.ai/INSTRUCTIONS.md](.ai/INSTRUCTIONS.md) — Standing orders and productivity guardrails
2. [.ai/CONTEXT.md](.ai/CONTEXT.md) — Project facts, history, and decisions
3. [.ai/README.md](.ai/README.md) — AI orientation

**Directive:** Do not rely solely on the root README. Always reference the `.ai/` folder for authoritative procedures and constraints.

---

## Quick Reference

- Primary setup: readme.md at repo root
- Linking: run make_links.sh in the appropriate environment folder
- Private data: comes from the config_private repo
- Scripts: use lib_*.sh helpers; target Bash
- Emacs: guard features newer than 26.1 with version checks
