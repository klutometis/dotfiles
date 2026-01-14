# Changelog

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
