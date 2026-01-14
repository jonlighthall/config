# Instructions

**Purpose:** Procedures and standing orders for AI agents working on this project.

---

## Quick Start

1. Read this file for procedures
2. Read `CONTEXT.md` for project facts and decisions
3. Ask clarifying questions if the project state is unclear

# Instructions

**Purpose:** Procedures and standing orders for AI agents working on this project.

---

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

**If topic folders already exist:** Use them. Don't consolidate without user direction.

**Single source of truth:** When you create a topic folder, **move** the content—don't copy it. Project-wide files should contain brief pointers to topic files, not duplicated content. If you find duplication, eliminate it.

**When you do create or reorganize:** Just do it and report what changed. Don't ask permission for organizational improvements—they're low-risk and you're the primary beneficiary. Explain your reasoning briefly so the user can object if they disagree, but don't treat obvious housekeeping as requiring approval.

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

### If you cannot write to these files:

Some AI tools have read-only access. If you receive substantial context but cannot update the `.ai/` files, summarize what should be added and ask the user to update the files manually.

---

## General Quality Standards

### Before Editing:
1. Verify you have sufficient context
2. Check terminology against `CONTEXT.md`
3. Use the author's preferred voice (see `CONTEXT.md`)
4. Verify cross-platform compatibility — this repo spans Windows, macOS, Linux, WSL, Cygwin, MinGW, and Git Bash
5. Understand symlink strategy before modifying linking logic
6. For Emacs Lisp: verify version compatibility (minimum 26.1; guard newer features)

### After Editing:
1. Verify the edit didn't break anything (compilation, syntax, etc.)
2. Update the relevant `CONTEXT.md` if you made decisions that should persist
3. Check for errors introduced and cross-platform impact
4. Verify symlinks and paths still resolve correctly on the target platform

### When Uncertain:
- Ask clarifying questions before making changes
- Document assumptions in the relevant `CONTEXT.md`
- Prefer minimal changes over extensive rewrites
- Test on multiple platforms if possible (or note which platforms were considered)

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

## This Project

**Scope:** Cross-platform dotfiles/config repository supporting Linux, macOS, Windows (native, WSL, Cygwin, Git Bash, MinGW), plus site-specific overlays.

**Key patterns and files:**
- Root scripts `make_links.sh`, `get_repos.sh`, `install_repos.sh` orchestrate setup; environment folders contain their own `make_links.sh` variants.
- Common shell utilities live in `lib_*.sh` (colors, debug, traps, formatting, conditional echoing).
- Bash configuration variants (`.bashrc_*`, `.bash_aliases_*`) map to specific environments; `_remote`/`_vpn` suffixes are used for connectivity-specific aliases.
- Emacs configuration is centralized in `emacs_all.el` and loaded everywhere; guard features newer than Emacs 26.1 with version checks.

**Procedures:**
- **Linking strategy:** Run the environment's `make_links.sh` to create symlinks from home to repo-managed files. Keep platform path handling correct (WSL `wslpath`, Windows separators, Cygwin conversions). Verify both absolute and relative link resolution.
- **Repository integration:** After cloning this repo, run `./get_repos.sh` to fetch dependent repos (including `config_private` for sensitive content) and `./install_repos.sh` if present for orchestration. Do not assume other repos exist; check before use.
- **Shell scripts:** Target Bash; avoid Zsh-specific constructs. Use the provided `lib_*.sh` helpers instead of re-implementing logging or traps. Preserve environment-specific logic instead of collapsing variants.
- **Bash configuration:** Keep global settings in shared files; place environment-specific additions in the corresponding variant. Preserve naming conventions for remote/VPN aliases.
- **Emacs Lisp:** Maintain compatibility with Emacs 26.1; wrap newer features (27.1+, 28+) in version guards. Keep style consistent (defun, hooks, custom-set-variables) and comment non-obvious compatibility decisions.

**Diagnostics:**
- For shell scripts, rely on `lib_dbg.sh`, `lib_cond_echo.sh`, and `lib_traps.sh` for debug, verbosity, and trap handling.
- For Emacs Lisp, add concise comments when gating features and consider `eval-and-compile` if compile-time behavior matters.

**Recommendation posture:** Favor incremental changes and compatibility. Avoid proposing architecture overhauls; document trade-offs if suggesting cross-platform-impacting changes.
