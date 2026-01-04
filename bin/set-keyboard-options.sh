#!/bin/bash

# Script to apply keyboard-specific layouts and options
# Call this from ~/.xprofile after xmodmap
#
# NOTE: This is a workaround for X11 InputClass configurations in
# /etc/X11/xorg.conf.d/99-keyboard-happy-hacking.conf that were being
# activated correctly but then overridden somewhere else in the stack.
# After days of debugging, this runtime approach proved more reliable.

# Add logging for udev debugging (before X11 check)
if [ -n "$ACTION" ]; then
  logger -t "keyboard-setup" "udev triggered script - ACTION=$ACTION, DISPLAY=$DISPLAY"
else
  logger -t "keyboard-setup" "manually triggered script - DISPLAY=$DISPLAY"
fi

# Script now runs as user via su, so X11 environment should be available

# Set DISPLAY if not already set (needed for udev context)
# When run via udev, the script doesn't inherit DISPLAY even with su
if [ -z "$DISPLAY" ]; then
  export DISPLAY=:0
fi

# Track if any layouts were applied
LAYOUT_APPLIED=false

set_keyboard_layout() {
  local device_name="$1"
  local layout="$2"
  local variant="$3"
  local options="$4"

  # Find device ID by name, but only keyboard slaves
  local device_id=$(xinput list | grep "$device_name" | grep "slave  keyboard" | grep -o 'id=[0-9]*' | cut -d= -f2)

  if [ -n "$device_id" ]; then
    echo "Setting keyboard layout for '$device_name' (ID: $device_id)"
    setxkbmap -device "$device_id" -layout "$layout" -variant "$variant" -option "$options"
    LAYOUT_APPLIED=true
  else
    echo "Device '$device_name' not found, skipping..."
  fi
}

apply_xmodmap_for_device() {
  local device_name="$1"

  if ! xinput list | grep -q "$device_name.*slave  keyboard"; then
    return
  fi

  echo "Applying xmodmap customizations for '$device_name'"

  case "$device_name" in
    "HHKB-Studio1 Keyboard")
      xmodmap - << 'EOF'
CAF! Fix the compose key assignments after setxkbmap
! Physical right Win (134) should be Alt_R, not Compose
keycode 134 = Alt_R
! Ensure physical right Alt (108) is Compose only, no mod4
clear mod4
keycode 108 = Multi_key
add mod4 = Super_L
EOF
      ;;
      # Add other device-specific customizations here
  esac
}

# Apply settings for HHKB
set_keyboard_layout "Topre Corporation HHKB Professional" "us" "dvorak" "compose:rwin"

# Apply settings for Carbon internal keyboard
set_keyboard_layout "AT Translated Set 2 keyboard" "us" "dvorak" "caps:ctrl_modifier,compose:ralt"

# Apply settings for Carbon internal keyboard
set_keyboard_layout "Logitech K400 Plus" "us" "dvorak" "caps:ctrl_modifier,compose:ralt"

# Apply settings for HHKB-Studio1 (swap all alt/win keys, compose on right alt)
set_keyboard_layout "HHKB-Studio1 Keyboard" "us" "dvorak" "altwin:swap_alt_win,compose:ralt-<"
apply_xmodmap_for_device "HHKB-Studio1 Keyboard"

# Apply settings for Keychron K2 HE
set_keyboard_layout "Keychron Keychron K2 HE" "us" "dvorak" "caps:ctrl_modifier,compose:ralt"

# Apply settings for Keychron Link (2.4G wireless)
set_keyboard_layout "Keychron  Keychron Link" "us" "dvorak" "caps:ctrl_modifier,compose:ralt"

# Apply global fallback only if no specific layouts were applied
if [ "$LAYOUT_APPLIED" = false ]; then
  echo "No specific keyboards found, applying global fallback"
  setxkbmap -layout us -variant dvorak -option terminate:ctrl_alt_bksp
  xmodmap - << 'EOF'
clear mod4
add mod4 = Super_L
EOF
fi

echo "Keyboard layouts applied successfully"

# Apply custom key mappings (after setxkbmap)
if [ -e ~/.Xmodmap ]; then
  echo "Applying xmodmap..."
  xmodmap ~/.Xmodmap
fi
