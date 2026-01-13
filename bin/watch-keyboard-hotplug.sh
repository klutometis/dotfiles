#!/bin/bash
# Watch for keyboard device hotplug events using inotify
# This is event-driven (not polling) and very efficient

set -e

# Source bashlog
. "$HOME/lib/bashlog/log.sh"

# Configure bashlog
export BASHLOG_FILE=1
export BASHLOG_FILE_PATH="$HOME/var/log/keyboard-hotplug.log"
mkdir -p "$(dirname "$BASHLOG_FILE_PATH")"

# Ensure inotify-tools is installed
if ! command -v inotifywait &> /dev/null; then
    log error "inotifywait not found. Install with: sudo apt-get install inotify-tools"
    exit 1
fi

log info "Starting keyboard hotplug watcher..."
log info "Watching /dev/input for new devices..."

# Watch /dev/input for new event devices
inotifywait -m -e create /dev/input 2>&1 | while read -r directory action file; do
    # Only trigger on eventN devices (keyboards)
    if [[ "$file" =~ ^event[0-9]+$ ]]; then
        log info "New input device detected: $file"
        
        # Brief delay to let device initialize
        sleep 0.5
        
        # Run keyboard setup script
        log info "Running keyboard setup..."
        if "$HOME/bin/set-keyboard-options.sh" &>> "$BASHLOG_FILE_PATH"; then
            log info "Keyboard setup completed successfully"
        else
            log error "Keyboard setup failed (exit code: $?)"
        fi
    fi
done
