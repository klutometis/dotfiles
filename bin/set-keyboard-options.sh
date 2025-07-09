#!/bin/bash

# Script to apply keyboard-specific layouts and options
# Call this from ~/.xprofile after xmodmap
#
# NOTE: This is a workaround for X11 InputClass configurations in
# /etc/X11/xorg.conf.d/99-keyboard-happy-hacking.conf that were being
# activated correctly but then overridden somewhere else in the stack.
# After days of debugging, this runtime approach proved more reliable.

set_keyboard_layout() {
  local device_name="$1"
  local layout="$2"
  local variant="$3"
  local options="$4"

  # Find device ID by name
  local device_id=$(xinput list | grep "$device_name" | grep -o 'id=[0-9]*' | cut -d= -f2)

  if [ -n "$device_id" ]; then
    echo "Setting keyboard layout for '$device_name' (ID: $device_id)"
    setxkbmap -device "$device_id" -layout "$layout" -variant "$variant" -option "$options"
  else
    echo "Device '$device_name' not found, skipping..."
  fi
}

# Apply settings for HHKB
set_keyboard_layout "Topre Corporation HHKB Professional" "us" "dvorak" "compose:rwin"

# Apply settings for Carbon internal keyboard
set_keyboard_layout "AT Translated Set 2 keyboard" "us" "dvorak" "caps:ctrl_modifier,compose:prsc"

# Apply global fallback (for any devices not specifically configured)
setxkbmap -layout us -variant dvorak -option terminate:ctrl_alt_bksp

echo "Keyboard layouts applied successfully"
