# Changelog

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
