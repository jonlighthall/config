# Context

**Purpose:** Facts, decisions, and history for AI agents working on this project.

---

## Author

**Name:** Jon Lighthall
**Role:** Systems engineer / configuration manager
**Organization:** Independent / multi-platform
**Domain:** Linux/Unix system administration, shell scripting, configuration management

**Primary tools:**
- Bash shell scripting
- Emacs (Lisp configuration)
- Git
- Cross-platform configuration (Windows, macOS, Linux, WSL)

**General preferences:**
- Impersonal, technical voice
- Avoid editorializing ("we believe")
- Minimal changes over extensive rewrites
- Preserve existing structure and patterns

**What NOT to do:**
- No meta-commentary outside `.ai/` files
- Don't suggest refactoring without understanding the cross-platform dependencies
- Don't modify `.bashrc*` files without checking compatibility across environments

**Productivity guardrail:**
- Watch for low-impact iteration (cosmetic tweaks) or tangents away from core configuration goals. If effort looks disproportionate, call it out briefly and suggest a more aligned path, deferring to the user.

---

## This Project: `config`

**Overview:**
Multi-platform user configuration repository containing shell profiles, Emacs settings, and environment-specific configurations for Windows, macOS, Linux, WSL, Cygwin, MinGW, and Git Bash.

**Purpose:**
- Centralized, version-controlled dotfiles and configurations
- Easy setup and linking across heterogeneous environments
- Separation of environment-specific settings by folder
- Integration with a separate private repository for sensitive data

**Repository structure:**

| Folder | Purpose |
|--------|---------|
| `linux/` | Linux-specific settings |
| `apple/` | macOS configuration |
| `windows/` | Windows native settings |
| `wsl/` | Windows Subsystem for Linux |
| `cygwin/` | Cygwin environment |
| `git_bash/` | Git Bash (MinGW) |
| `mingw/` | MinGW packages and setup |
| `nrl/` | Site-specific configuration (NRL) |
| `oracle/` | Site-specific configuration (Oracle Linux) |
| `private/` | Sensitive data (linked from `config_private` repo) |

**Key files:**

| File | Purpose |
|------|---------|
| `emacs_all.el` | Universal Emacs settings (loaded on all platforms) |
| `readme.md` | User documentation and setup instructions |
| `.bashrc*` | Bash profiles and aliases (environment-specific variants) |
| `lib_*.sh` | Bash utility libraries (colors, debugging, formatting, etc.) |
| `make_links.sh` | Symlink installation script (root level) |
| `get_repos.sh` | Clone and setup related repositories |
| `.bash_aliases*` | Bash aliases for various environments |

**Key scripts and their roles:**

- `make_links.sh` — Creates symlinks from home directory to config files in this repo. Each environment folder has its own `make_links.sh` for environment-specific setup.
- `get_repos.sh` — Clones dependent repositories (see referenced git_author.sh, lib_*.sh)
- `git_author.sh` — Configures git author info (stored in `nrl/` for site-specific overrides)
- `install_repos.sh` — Orchestrates repository cloning and setup

**Dependencies:**
- Git (required for cloning and version control)
- Bash (for all shell scripts)
- Emacs (optional, for Lisp configuration)
- SSH keys (required for SSH-based repository cloning)
- WSL VPN kit (for WSL connectivity if behind corporate VPN)

**Cross-platform constraints:**
- Paths differ significantly (Windows `\`, WSL `/`, macOS case-insensitive filesystem)
- Shell availability varies (Git Bash ≠ native Windows shell)
- Emacs version compatibility matters (26.1+ vs. 27.1+ for new features like `display-fill-column-indicator-mode`)
- SSH and credential handling differs per environment

**Known issues / patterns:**
- Emacs 26.1 doesn't support `display-fill-column-indicator-mode` (introduced in 27.1) — version checks required in `emacs_all.el`
- Private configurations are in a separate repository (`config_private`) and symlinked from `private/`
- Site-specific settings (NRL, Oracle) stored in dedicated folders
- Aliases for remote VPN connections stored in separate files (`bash_aliases_*`)

**Recent changes:**
- Fixed Emacs version compatibility in `emacs_all.el` (version check for `display-fill-column-indicator-mode`)
- Added productivity guardrail to `.ai/CONTEXT.md` to flag low-impact or tangential work

---

## Superseded Decisions

None documented yet.
