# Changelog

## 2026-01-24

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
