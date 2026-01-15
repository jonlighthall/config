# Execute These Instructions

> **ACTION MODE:** Do not describe what you will do. Do not ask for confirmation. Do not offer options. Read the instructions, then execute them. Report what you did, not what you could do.

**Set up `.ai/` context files for this repository.**

Scan the repo, consolidate any existing meta-content, and create:

**In `.ai/` folder (three files):**
- `README.md` — For AI orientation and human redirection
- `CONTEXT.md` — Facts, decisions, history
- `INSTRUCTIONS.md` — Procedures, standing orders

**At repository root (one file):**
- `AGENTS.md` — Universal entry point for AI tools

**Use UPPERCASE filenames exactly as shown.** This is intentional—it follows the convention for meta-files like README.md, LICENSE, CHANGELOG, etc.

> ⚠️ **AGENTS.md is required** for repo-root bootstraps. Do not skip it unless this is a subfolder setup.

Follow the detailed steps below.

---

<!--
USAGE: This file works two ways:
1. Drag/drop or attach to any AI chat (VS Code Copilot, Claude, ChatGPT, etc.)
2. Copy/paste the entire contents into a chat

The imperative header tells the AI what to do immediately.
Different AI models have different default behaviors—some are more "consultative"
(describe plans, ask permission) while others are more "autonomous" (just do it).
The ACTION MODE directive at the top pushes consultative models toward autonomous behavior.
-->

---

## Harvest Command (for existing chats)

**Use this command in chats that already have valuable context you want to preserve.**

Copy/paste this into an existing chat:

```
Read the `.ai/` folder (or create one using the BOOTSTRAP.md file if it doesn't exist).
Then review this entire chat and update the .ai/ files to reflect:

1. **Key decisions** made in this conversation
2. **Facts discovered** (constraints, patterns, validated approaches)
3. **Deprecated information** — if this chat contradicts what's already documented,
   flag it in a "Superseded" or "Deprecated" section rather than deleting
4. **Work completed** — update the History section if appropriate

**Just do it.** Don't ask "should I add X?" — if something from this chat is worth preserving, add it. The whole point of harvesting is to capture context without me re-explaining it.

**Contradiction handling:**
- If this chat is NEWER than what's documented: Update the documented info, move old info to a "Superseded" note if it's instructive to preserve
- If this chat is OLDER than what's documented: Don't overwrite newer decisions, but note if this chat adds context that's still relevant
- If unclear: That's the ONE thing worth asking about

After updating, show me a summary of what changed (not what you're thinking about changing).
```

**Short version** (for chats where `.ai/` already exists):

```
Harvest this chat: Read `.ai/CONTEXT.md` and `.ai/INSTRUCTIONS.md`, review this conversation, and update the files with any decisions, facts, or constraints discovered here. Don't ask permission — just update and show me what changed.
```

**Processing order for multiple chats:**
- Work chronologically (oldest → newest) so later decisions naturally override earlier ones
- Or work by importance (most critical chats first) if chronology is unclear
- Tag harvested chats somehow (e.g., rename with "[harvested]" prefix) to track progress

**Harvesting when topic folders exist:**
- Route content to the appropriate location (topic-specific → topic files; cross-cutting → project-wide)
- If harvesting creates enough topic-specific content to justify a new topic folder, create it
- Add navigation links in project-wide files only if needed for discoverability

**What to skip:**
- Trivial Q&A that didn't result in decisions
- Purely exploratory chats that were abandoned
- Chats where the conclusions are already documented elsewhere

**What requires permission:**
- Harvesting content → **Just do it** (low risk, additive)
- Deleting source files → **Ask first** (destructive)
- Structural improvements (adding TOC, refactoring sections) → **Just do it** if obvious improvement; explain what you changed
- Creating/reorganizing topic folders → **Just do it** and report (you're the primary consumer of these files)
- Deduplication → **Just do it** (single source of truth is always better)

**Don't offer multiple options** when the work has a clear logical conclusion. If you deduplicated content but left dangling references, fix the references. If you created a topic folder but didn't update links, update the links. Complete the logical unit of work. Example: Don't ask "Should I consolidate these two topic folders?" if the overlap is obvious—just do it and report what changed.

**Harvesting productivity-related context:**
- If the chat reveals patterns or preferences around workflow efficiency (e.g., avoiding rabbit holes or low-value iterations), integrate them into the productivity guardrail in `CONTEXT.md` if it exists, or add it if justified.
- Keep phrasing general and professional—no personal details. Focus on observable behaviors and project alignment.
- Just do it if low-risk; ask if it involves sensitive information.

**Updating AGENTS.md during harvest:**
- If a root-level `AGENTS.md` exists, check if it accurately reflects the current `.ai/` folder structure
- If new `.ai/` folders were created, add them to the `AGENTS.md` navigation table
- If project descriptions have evolved, update the quick reference section
- `AGENTS.md` should stay minimal—detailed context belongs in `.ai/` files

---

## Task

Create or consolidate an `.ai/` folder for this repository with exactly three files, plus an optional root-level pointer:

**In `.ai/` folder:**
1. **README.md** — Minimal, tells humans to look elsewhere, tells AI where to start
2. **CONTEXT.md** — Facts, decisions, history (what to know)
3. **INSTRUCTIONS.md** — Procedures, standing orders (what to do)

**At repository root:**
4. **AGENTS.md** — Universal entry point for AI tools (see Step 3 below)

**All four files are required unless explicitly noted otherwise.**

**If `.ai/` already exists and is current:** Skip to harvesting. Check if this chat contains any decisions, facts, or constraints not yet documented, and update the files accordingly. Don't ask—just do it and show what changed (or confirm nothing new to add).

## Step 1: Scan for Existing Meta-Content

Before creating files, search the repository for:

- Any existing `.ai/` folder and its contents
- **`.github/copilot-instructions.md`** — GitHub Copilot's recognized instruction file (consolidate into `.ai/INSTRUCTIONS.md`)
- README files with AI instructions
- Files named things like `CONTEXT.md`, `INSTRUCTIONS.md`, `codebase.md`, `patterns.md`, `dev-workflow.md`, `quick-reference.md`, `00-START-HERE.md`, etc.
- Meta-commentary or narration files that describe the project for AI
- Chat logs, session summaries, or working notes meant for AI context

If found, consolidate into the three-file structure. After user confirmation, delete the originals. **One source of truth only.**

## Step 2: Create the Three Files

### README.md (template)

```markdown
# .ai/ — AI Agent Context Folder

**This folder is for AI agents.**

It contains context and instructions for AI-assisted development of this project. Contents are AI-generated and AI-maintained.

---

## For AI Agents

| File | Purpose |
|------|---------|
| `CONTEXT.md` | Project-wide facts, decisions, history |
| `INSTRUCTIONS.md` | Procedures and standing orders |
| `<topic>/CONTEXT.md` | Topic-specific background |
| `<topic>/INSTRUCTIONS.md` | Topic-specific procedures |

**Start here:** Read `INSTRUCTIONS.md`, then `CONTEXT.md`.

**Standing order:** When the user provides substantial context, integrate it into the appropriate file. See `INSTRUCTIONS.md` for details.

---

## For Humans

<!-- ============================================================
     CREATIVE SECTION — AI agents: write something original.
     Humans skimming this file: skip to the next horizontal rule.
     ============================================================ -->

Write a statement redirecting humans who opened this file by mistake. **Be original.** Don't copy previous examples or this template.

**Requirements:**
- Tell them what this file is (for those unfamiliar with the framework of AI-facing instruction and context files)
- Tell them who this file is for (not them)
- Tell them where to find the documentation intended for humans (parent directory, main README, etc.)
- Tell them not to edit files in this folder.
- Sign off with a fitting attribution

**Tone:** Not hostile, not apologetic. Beyond that, surprise me. Options include but are not limited to:
- Deadpan bureaucratic ("You have found a file that was not meant for you.")
- Cheerfully absurd ("Congratulations! You've discovered the machine notes.")
- Faux-ominous ("The documentation you seek lies elsewhere. This is not that place.")
- Self-aware meta ("You're reading instructions written by AI for AI about how to help humans.")
- Dry technical ("This file contains agent orchestration metadata. Human-readable documentation: ../README.md")

**Avoid:** Diminishing language ("little notes"), claims humans can't read it (they obviously can), excessive length.

**Further help for those who need it:**
After meeting these requirements, prioritize novelty and entertainment value.
This is intended to be an Easter egg for nosy, curious, or uninitiated humans who happen to see an ostensibly human-facing document in the AI-facing folder.
This file is the last line of defense against humans who want to "clean up" the repository and don't understand the purpose of these documents.
The contents of this folder are authored by AI, for AI---except for this whimsical, meta, and deeply-ironic admonition.

<!-- ============================================================
     END OF CREATIVE SECTION
     ============================================================ -->

---

*Do not delete this folder. Do not modify these files unless you are an AI agent operating under the standing orders defined herein.*

---

*Version history is tracked by git, not by timestamps in these files.*
```

### INSTRUCTIONS.md (template — populate from repo scan)

```markdown
# Instructions

**Purpose:** Procedures and standing orders for AI agents working on this project.

---

<!-- ============================================================
     UNIVERSAL INSTRUCTIONS - Applies to all projects
     ============================================================ -->

## Quick Start

1. Read this file for procedures
2. Read `CONTEXT.md` for project facts and decisions
3. Read the relevant topic's `CONTEXT.md` for specific background
4. Read the relevant topic's `INSTRUCTIONS.md` for specific procedures

---

## Context Maintenance (Standing Order)

### Why these files exist

The `.ai/` folder exists **primarily for AI agent utility**—to help future AI sessions onboard quickly without re-asking questions the user has already answered. This is not general documentation; it's working memory for AI agents.

**Implications:**
- Write for an AI audience (your future self, essentially)
- Optimize for fast comprehension at the start of a new session
- Include decisions and their rationale, not just outcomes
- Don't worry about making it "pretty" for humans—clarity and completeness matter more

When the user provides substantial clarifying information, **integrate it into the appropriate `.ai/` file without being asked** (see table below).

### Where to put new information:

| Type of Information | Destination |
|---------------------|-------------|
| Project-wide decisions, facts, history | `.ai/CONTEXT.md` |
| Project-wide procedures, standing orders | `.ai/INSTRUCTIONS.md` |
| Topic-specific history, validation, decisions | `<topic>/CONTEXT.md` |
| Topic-specific procedures, checklists | `<topic>/INSTRUCTIONS.md` |

### When to create a topic folder:

Topic folders add granularity but also overhead. **Default to project-wide files; split when justified.**

**This assessment is your responsibility.** You are the one consuming these files, so you are best positioned to judge when a topic has grown unwieldy or when splitting would improve future session onboarding. Don't ask the user "should I create a topic folder?"—evaluate the criteria below and either do it or don't.

**CREATE a topic folder when:**
- The topic has its own lifecycle (can be "completed" independently)
- It has unique decisions or terminology that don't apply elsewhere
- Multiple AI sessions will focus specifically on this topic
- Adding to project-wide files would exceed ~100 lines for this topic alone
- You notice yourself scrolling past large blocks of irrelevant context to find what you need

**DON'T create a topic folder when:**
- It's a one-off task or short-term work
- The context fits in a few paragraphs
- It shares most decisions with the main project
- You're unsure (start in project-wide files; split later if needed)

**If topic folders already exist:** Use them. Maintain them per the guidance below.

**Single source of truth:** When you create a topic folder, **move** the content—don't copy it. Project-wide files should contain brief pointers to topic files, not duplicated content. The topic folder is authoritative for that subject. If you find duplication, eliminate it.

**Topic folder maintenance:**

Over time, topic folders may overlap with each other or with project-wide content. You are responsible for keeping the `.ai/` structure tidy—apply these heuristics:

- **Deduplication:** If two topic folders cover overlapping ground, consolidate. Move specific content to one folder, keep a pointer in the other. Example: if `security/INSTRUCTIONS.md` and `deployment/INSTRUCTIONS.md` both describe credential handling, consolidate into one and cross-reference.
- **Collapse small folders:** If a topic folder shrinks to a few paragraphs after content moves elsewhere, consider collapsing it back into project-wide files. A folder should be substantive enough to justify the added navigation overhead.
- **Prune stale folders:** If a topic folder hasn't been touched in multiple sessions and its content is small enough to fit in project-wide files, migrate it. Mark the consolidation in git commit.
- **Always maintain single source of truth:** No duplicated content across folders. Ever. If you find it, fix it immediately.

For any of these maintenance actions, just do it and report what changed—it's low-risk housekeeping. If you're uncertain whether a folder should be deprecated, ask the user.

**When you create or reorganize:** Just do it and report what changed. Don't ask permission for organizational improvements—they're low-risk and you're the primary beneficiary. Explain your reasoning briefly so the user can object if they disagree, but don't treat obvious housekeeping as requiring approval.

### When to update:

**DO update when:**
- User provides ≥2-3 sentences of explanatory context
- User answers clarifying questions about the project
- User makes a decision that should persist across sessions
- User corrects a misconception (especially if AI-generated)

**DON'T update for:**
- Routine edits, minor corrections
- Conversational exchanges
- Information already documented

### Why this matters:

Context files exist so future AI sessions don't need to re-ask the same questions. If you receive substantial context and don't document it, the next session will be less effective.

### Handling conflicts:

Topic-specific files may override project-wide decisions, but **conflicts must be explicitly documented**.

**If you notice a conflict:**
1. Check if the topic file explicitly notes the override (e.g., "Exception: this component uses X despite project-wide guidance")
2. If the override is documented → follow the topic-specific guidance
3. If the override is NOT documented → ask the user which applies before proceeding

**When creating an intentional override:** Add a note in the topic file explaining what is being overridden and why.

### Handling deprecated/superseded information:

When harvesting context from old chats or updating documentation with newer decisions:

**Newer decisions take precedence**, but preserve the evolution if it's instructive:

```markdown
### [Decision Name]
**Current:** [What we do now]

**Superseded:** Previously we tried [X] but switched because [reason].
(Chat from YYYY-MM-DD)
```

**When to preserve the old approach:**
- It explains *why* we don't do something (prevents re-asking)
- It documents a failed experiment (prevents repeating mistakes)
- It shows the evolution of thinking

**When to simply delete:**
- Trivial or obvious corrections
- Typos/errors with no instructive value
- Exploratory ideas that were never actually tried

**If chronology is unclear:** Ask the user which version is current before overwriting.

**Autonomy imperative in song form:**
>Fates favor the ones who help themselves\
The rest feel the sting of the lash as they row the boat to Hell


### If you cannot write to these files:

Some AI tools have read-only access. If you receive substantial context but cannot update the `.ai/` files, summarize what should be added and ask the user to update the files manually.

---

## General Quality Standards

### Before Editing:
1. Verify you have sufficient context
2. Check terminology against `CONTEXT.md`
3. Use the author's preferred voice (see `CONTEXT.md`)

### After Editing:
1. Verify the edit didn't break anything (compilation, syntax, etc.)
2. Update the relevant `CONTEXT.md` if you made decisions that should persist
3. Check for errors introduced

### When Uncertain:
- Ask clarifying questions before making changes
- Document assumptions in the relevant `CONTEXT.md`
- Prefer minimal changes over extensive rewrites

### Technical Writing Standards

**Evaluative and promotional language:**
- Avoid evaluative claims not backed by citations (e.g., "most needed", "best", "significant")
- Avoid promotional language (e.g., "powerful", "revolutionary", "unprecedented")
- State facts objectively; let the reader draw conclusions
- If a comparative claim is necessary, cite supporting evidence

**Examples:**
- ❌ "precisely the scenarios where PE methods are most needed"
- ✓ "scenarios requiring explicit treatment of density variation"
- ❌ "offers significant computational advantages"
- ✓ "offers computational advantages" (or cite performance benchmarks)

---

## Changelog Maintenance (Software Projects Only)

**Skip this section for document/paper-writing projects.**

For versioned software, maintain a `CHANGELOG.md` following [Keep a Changelog](https://keepachangelog.com/) format:

**When to add an entry:**
- Affects user-facing behavior (CLI flags, output format, thresholds)
- Changes classification semantics or comparison logic
- Adds or removes major capabilities
- Fixes bugs that affected results

**When NOT to add an entry:**
- Internal refactors with no user-visible change
- Documentation updates
- Code style/formatting changes

**Categories:** Added, Changed, Fixed, Removed, Deprecated, Security

**Format:**
```markdown
## [Unreleased]
### Fixed
- Sub-LSB threshold now correctly applies to differences below machine epsilon
```

Move entries from `[Unreleased]` to a versioned section on release.

---

<!-- ============================================================
     PROJECT-SPECIFIC INSTRUCTIONS
     ============================================================ -->

## This Project

[Populate this section based on repository scan. Include:]
- File patterns and locations
- Build/test commands
- Tool-specific standards
- Workflow guidance
```

### CONTEXT.md (template — populate from repo scan and user input)

```markdown
# Context

**Purpose:** Facts, decisions, and history for AI agents working on this project.

---

<!-- ============================================================
     UNIVERSAL CONTEXT - Applies to all projects
     ============================================================ -->

## Author

**Name:** [Ask user]
**Role:** [Ask user]
**Organization:** [Ask user if relevant]
**Domain:** [Infer from repo or ask]

**Primary tools:**
- [Infer from repo: languages, frameworks, editors]

**General preferences:**
- Impersonal voice for expository writing
- Mathematical "we" acceptable in derivations ("we substitute...", "we obtain...")
- Avoid editorial "we" ("we believe...", "we recommend...")
- Prefer minimal changes over extensive rewrites

**Writing style:**
- Target the style appropriate for the project (technical docs, code comments, etc.)
- **Target audience:** [Infer or ask]
- **Level of detail:** Appropriate for the audience

**What NOT to do:**
- **No meta-commentary:** Don't create summary markdown files after edits. If context is worth preserving, put it in `.ai/` files.
- **No hyper-literal headers:** If asked to "add a clarifying statement," don't create a section titled "Clarifying Statement." Integrate naturally.
- **No AI self-narration:** Don't describe what you're doing in the document itself. Just do it.

**Productivity guardrail (enabled by default—delete this section if not wanted):**

To support efficient workflows, AI agents should monitor for patterns that may indicate diminishing returns or misalignment with project goals. This is not about overriding user requests but providing an objective perspective to help maintain momentum.

**Guiding principle:**

> "There are only two types of dissertations. Perfect dissertations and finished dissertations."

When work meets its functional requirements and communicates its content effectively, it is ready. A finished deliverable advances the project; a perpetually-refined draft does not.

**Observe for:**
- Repeated iterations on low-impact details (e.g., cosmetic formatting, alignment, or refinements that don't affect functionality, clarity, or outcomes).
- Lines of inquiry or tasks that appear tangential to stated project goals, tasks, or priorities (e.g., exploring unrelated optimizations when core features remain incomplete).
- Requests showing signs of scope creep or fixation, where effort invested exceeds proportional value to advancing the project.

**Assessment process:**
- For any proposed action or user request, briefly evaluate its alignment: Does it directly contribute to project goals (as documented in this file or the task at hand)? Is it a productive step forward, or potentially an indulgence in non-essential perfectionism?
- If misalignment is likely (based on observable patterns, not assumptions), gently note it with phrases like: *"This seems like it might be venturing into diminishing returns—does it align with our core goals, or should we prioritize [suggest alternative]?"* or *"This could be polishing beyond what's needed for progress—your call, but the current state advances the project effectively."*
- Always defer to the user; do not refuse or block actions. Tone: collaborative and factual, focused on project efficiency.

This guardrail applies across sessions—reference prior chats or documented goals for context. If patterns persist, suggest revisiting project priorities in `INSTRUCTIONS.md`.

---

<!-- ============================================================
     PROJECT-SPECIFIC CONTEXT
     ============================================================ -->

## This Project

[Populate this section based on repository scan. Include:]
- Project overview and purpose
- Key files and their roles
- Important decisions and their rationale
- Constraints and limitations
- Dependencies and relationships
- Any "do not touch" or "known issues" notes

## Superseded Decisions

[Optional section for preserving instructive history. Format:]

<!--
### [Decision Name]
**Current:** [What we do now]
**Previously:** [What we tried before]
**Why changed:** [Reason for the change]
**Source:** [Chat date or context if known]

Only keep entries that prevent re-asking the same questions or repeating failed experiments.
Delete this comment block when adding real entries.
-->
```

## Step 3: Create AGENTS.md at Repository Root

**Why this file exists:**

`AGENTS.md` is emerging as the universal entry point for AI coding agents (2025–2026). Tools like GitHub Copilot, Cursor, Claude Code, and others are increasingly hard-coded to look for this file at the repository root. By creating it as a "pointer" to your `.ai/` folder, you get:

- **Tool compatibility** — New tools find your context automatically
- **Clean separation** — Detailed instructions stay in `.ai/`; the root stays professional
- **No duplication** — Single source of truth remains in `.ai/` files

**Do NOT use symlinks.** They break on Windows, confuse Git, and fail in web interfaces. Use a hard pointer (plain Markdown file with links).

**Create `AGENTS.md` at the repository root** with this structure:

```markdown
# AGENTS.md

## For Humans

[CREATIVE SECTION — See parameters below]

---

## For AI Agents

This repository uses a structured `.ai/` directory for context and instructions.
All AI agents should prioritize the following files for project-specific guidance:

1. **[.ai/INSTRUCTIONS.md](.ai/INSTRUCTIONS.md)** — Standing orders and productivity guardrails
2. **[.ai/CONTEXT.md](.ai/CONTEXT.md)** — Project facts, history, and decisions
3. **[.ai/README.md](.ai/README.md)** — AI orientation

**Directive:** Do not rely solely on the root `README.md`. Always reference the `.ai/` folder for authoritative procedures and constraints.

---

## Quick Reference

[Add project-specific summary here: key directories, build commands, important constraints]

---

*This file is the universal entry point. For detailed context, always defer to `.ai/`.*
```

**"For Humans" section:** Same creative parameters as `.ai/README.md` (see Step 2 above). Since `AGENTS.md` sits at the repo root, it's *more* likely to be discovered by confused humans—make the redirect clear but keep it brief. Must mention: where to find real docs, what this file does, don't edit it.

**For monorepos or multi-project workspaces:**

If the repository contains multiple projects with separate `.ai/` folders, the root `AGENTS.md` should list them all:

```markdown
## Repository Structure

| Location | Project | Description |
|----------|---------|-------------|
| `project-a/.ai/` | Project A | [Brief description] |
| `project-b/.ai/` | Project B | [Brief description] |

Navigate to the appropriate `.ai/` folder for project-specific guidance.
```

**DEFAULT ACTION: Create AGENTS.md.** Only skip if:

- This is a subfolder `.ai/` setup (not at repo root) — e.g., `documents/report/.ai/`
- An `AGENTS.md` already exists at the repo root (consolidate into it instead)

**If you're bootstrapping at the repository root, you MUST create AGENTS.md.**

## Step 4: Populate from Scan

After creating all four files (three in `.ai/`, one at repo root):

1. Fill in the Author section by asking the user (or inferring if obvious)
2. Scan the repo structure and populate the project-specific sections
3. If consolidating existing files, preserve all useful content
4. Inform the user: *"Productivity guardrails are enabled by default—AI will gently flag when work appears to be venturing into diminishing returns or scope creep. You can remove or customize this section in CONTEXT.md if you prefer."*
5. Ask user to review before deleting source files

## Key Principles

- **Four files total** — three in `.ai/`, one `AGENTS.md` at repo root (topic folders only when justified)
- **AGENTS.md is required** for repo-root bootstraps (skip only for subfolder `.ai/` setups)
- **UPPERCASE filenames** — `README.md`, `CONTEXT.md`, `INSTRUCTIONS.md`, `AGENTS.md`
- **No symlinks** — use hard pointers (plain Markdown with links)
- **No timestamps** — git tracks history
- **Conflicts must be explicit** — silent precedence is dangerous
- **Integrate context proactively** — don't wait to be asked
- **No meta-commentary outside `.ai/`** — keep the repo clean
- **Ensure `.ai/` is tracked by git** — if new files don't appear in `git status`, check `.gitignore` for rules like `**/` that might exclude it

## If You Made a Mistake

**Created lowercase files?** Don't try to rename them directly on case-insensitive filesystems. Use:
```bash
git mv context.md temp.md && git mv temp.md CONTEXT.md
```

**Files disappeared during rename?** Check for backup files (`*.md~`) and `git status`. Commit before risky operations.

**General rule:** When in doubt, commit first. Git is your safety net.

## After Setup

**Present a summary in the chat** so the user can review without reading the files. Then ask targeted questions.

### Template for post-setup summary:

```
I've created the `.ai/` folder. Here's my understanding of this project:

**Project:** [1-2 sentence description of what the project does]

**Key components:**
- [file/folder]: [what it does]
- [file/folder]: [what it does]
- [etc.]

**Author context I inferred:**
- Role: [what you guessed]
- Domain: [what you guessed]
- Primary tools: [languages, frameworks]

**Decisions/constraints I documented:**
- [any patterns, conventions, or constraints you noticed]

**AGENTS.md:** [Created at repo root / Consolidated into existing / Skipped (subfolder setup — explain why)]

**⚠️ If AGENTS.md is missing and this is a repo-root bootstrap, go back and create it now.**

**Questions:**

1. Is this understanding correct? Any major mischaracterizations?
2. [If consolidating] I consolidated content from [list files]. Should I delete them now?
3. What key decisions or constraints should I add? (e.g., "never touch X", "Y is deprecated", "Z is the pattern to follow")
```

Then integrate corrections into CONTEXT.md.

---

**— END OF BOOTSTRAP INSTRUCTIONS —**

*If you don't see this line, the file was truncated. Request the complete file before proceeding.*
