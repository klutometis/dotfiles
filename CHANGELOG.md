# Changelog

## 2026-01-24

### Consolidated Emacs Configuration into Single File

**Changes**:
- Merged `~/etc/dotfiles/dot-emacs.d/init-settings.el` into `init.el`
- All global settings, helper functions, and keybindings now in `(use-package emacs)` stanza
- Removed obsolete `(load "~/.emacs.d/init-settings.el")` statement
- Added missing function definitions: `copy-line` and `kill-line-backward`
- Removed duplicate `C-c C-h` keybinding (kept only `C-c h` for help)
- Removed unused `org-store-link` binding (not using org mode)
- Fixed `consult-apropos` autoload error by changing to built-in `apropos`

**Rationale**: Single-file configuration improves discoverability and eliminates the need to know about separate settings files. All global configuration is now in one logical place within the `emacs` package stanza.

**Files affected**:
- `~/etc/dotfiles/dot-emacs.d/init.el` - merged and cleaned up
- `~/etc/dotfiles/dot-emacs.d/init-settings.el` - no longer loaded (kept as backup)


### Modernized Auto-Completion Stack

**Changes**:
- Replaced `company-mode` with `corfu` (fast, capf-native completion UI)
- Replaced commented-out `lsp-mode` with `eglot` (built-in LSP client)
- Added `cape` for completion source stacking (LSP + files + spell + dabbrev)
- Configured eglot for C++, Python, TypeScript, JavaScript, and Shell modes
- Auto-completion now triggers after 2 characters with 0.1s delay

**Rationale**: Corfu + eglot provides a modern, lightweight completion stack that integrates seamlessly with Vertico/Consult. Eglot is built into Emacs 29+ and requires minimal configuration. Cape allows stacking multiple completion sources (LSP, files, spell checking) in a single unified interface.

**What you get**:
- Popup completions with TAB/S-TAB navigation (vs hippie-expand's cycling)
- LSP-aware completions for supported languages
- File path completions
- Spell-check suggestions integrated into completions
- Dynamic abbreviations from open buffers

**Language servers required**:
- C/C++: `clangd`
- Python: `python-lsp-server` or `pyright`
- TypeScript: `typescript-language-server`
- Bash: `bash-language-server`


### Added Hermetic Language Server Installation

**Changes**:
- Updated `~/bin/configure-system.sh` to install language servers hermetically
- Replaced `nvm` with `mise` (Rust-based, 3-40x faster, polyglot version manager)
- Added `uv` for Python tool installation (10-100x faster than pip, proper isolation)
- Automated installation of: clangd, python-lsp-server, typescript-language-server, bash-language-server
- Updated `~/.zshenv` to initialize mise instead of nvm/pyenv

**Rationale**: Hermetic installation prevents system contamination and sudo requirements. Modern tools (uv, mise) are significantly faster and provide better isolation than traditional package managers (pip, nvm, pyenv).

**Language servers installed**:
- C/C++: `clangd` (via apt, with clang-tidy and IWYU)
- Python: `python-lsp-server` (via uv tool)
- TypeScript: `typescript-language-server` (via mise + npm)
- Bash: `bash-language-server` (via mise + npm)

**Benefits**:
- No system package conflicts
- No sudo needed (except for clangd)
- Faster installation and updates
- Easy per-project version management (with mise)
- Integrates automatically with Emacs eglot

**Documentation**: See `~/var/doc/emacs.md` for Emacs usage and `~/var/build/emacs.md` for build instructions


### Enhanced Consult Search with Preview

**Changes**:
- Added `C-c R` keybinding for `consult-ripgrep` from current directory (`C-c r` still searches from project root)
- Enabled automatic preview for ripgrep/grep results (0.2s debounce to avoid flickering)
- Enabled instant preview for yank-ring (`M-y`) - restores Helm-like behavior
- Added `consult-customize` configuration for preview across all search commands

**Rationale**: Addresses usability issues from Helm migration. Users needed `C-u C-c r` to search current directory (now just `C-c R`). Preview functionality replicates Helm's useful "see before you jump" behavior while maintaining Vertico's speed and clean interface.

**Usage**:
- `C-c R` - ripgrep from current directory
- `C-c r` - ripgrep from project root
- Navigate with `C-n`/`C-p` to see automatic preview
- Type more terms to narrow results (space-separated)
- Use `!word` to exclude matches containing "word"


### Replaced Helm with Vertico/Consult Completion Stack

**Changes**:
- Replaced Helm with modern Vertico/Consult/Marginalia/Embark stack in `~/etc/dotfiles/dot-emacs.d/init.el`
- Updated savehist configuration to use Vertico/Consult history variables
- Removed helm-specific history variables (`helm-M-x-input-history`, `helm-find-files-history`, `helm-grep-history`)
- Added `file-name-history` and `consult--grep-history` to persist search patterns across sessions

**Rationale**: Vertico/Consult provides a more modular, faster, and actively maintained completion framework compared to Helm. Uses standard Emacs completion APIs.

### Fixed ikill Self-Termination Bug

**Problem**: `~/bin/ikill` would occasionally kill itself when matching patterns, especially when it appeared first in the process list.

**Changes**:
- Added filtering to exclude processes containing 'ikill' in their command line
- Filters out current process ID (`$$`) from results
- Prevents premature termination when ikill matches its own search pattern

**Rationale**: Self-termination prevented ikill from completing its job. Filtering ensures only target processes are affected.

### Added NPM Registry Configuration

**Changes**:
- Added `NPM_CONFIG_REGISTRY=https://registry.npmjs.org/` to `~/.zshenv`

**Rationale**: Explicitly sets npm to use the official registry across all shells and npm operations.

### Added Git Submodule Update Script

**Problem**: `git submodule update --remote` leaves submodules in detached HEAD state, requiring manual branch checkout.

**Changes**:
- Created `~/bin/git-update-submodules` script
- Updates all submodules with `--merge` flag to avoid detached HEAD
- Automatically detects and checks out correct branch (main/master) for each submodule
- Handles mixed main/master repositories gracefully

**Rationale**: Automates the tedious process of updating submodules and ensuring they're on proper branches. Works for all present and future submodules without manual configuration.

## 2026-01-16

### Added Go Toolchain and Bluetuith Bluetooth Manager

**Changes**:
- Added Go installation section to `configure-system.sh`
- Added bluetuith installation via `go install github.com/darkhz/bluetuith@latest`
- Go version fetched dynamically from official API (no hardcoded versions)

**Details**:
- Go installed to `/usr/local/go` (standard location)
- Bluetuith binary placed in `~/go/bin`
- Both `/usr/local/go/bin` and `~/go/bin` added to PATH
- Installations only run if commands not already present

**Rationale**: Bluetuith is a terminal-based Bluetooth manager that provides better control than GUI tools. Go installation method ensures cross-distro compatibility and always-latest versions.

## 2026-01-13

### Removed Unstable TigerVNC X11 Extension

**Problem**: X server crashing 5-6 times daily due to TigerVNC X11 extension.

**Changes**:
- Removed `/etc/X11/xorg.conf.d/10-vnc.conf` (manually created config)
- Purged `tigervnc-xorg-extension` package
- Removed X11/udev keyboard configuration sections from `configure-system.sh` (replaced by `~/bin/watch-keyboard-hotplug.sh`)

**Rationale**: VNC extension running inside X server process was too unstable for daily use. Will use external VNC server (e.g., `x11vnc`) when needed - external servers are isolated from X server, so crashes don't bring down the desktop.

### Cleaned Up configure-system.sh

**Changes**:
- Removed `ENABLE_KEYBOARD_CONFIG` flag (no longer needed)
- Removed X11 configuration deployment section
- Removed udev rules deployment section
- Script now automatically reloads sxhkd if running (instead of instructing user)

**Rationale**: Keyboard configuration now handled by event-driven `~/bin/watch-keyboard-hotplug.sh`. Old X11/udev approach was replaced.
