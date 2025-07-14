#!/bin/bash
# Deploy system configuration files

set -e

echo "Deploying system configuration files..."

# X11 configs
if [ -d ~/etc/X11/xorg.conf.d ]; then
    echo "Copying X11 configuration files..."
    sudo cp ~/etc/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/
    sudo chown root:root /etc/X11/xorg.conf.d/*
    sudo chmod 644 /etc/X11/xorg.conf.d/*
    echo "X11 configs deployed successfully"
    echo "  - Carbon laptop keyboard config"
    echo "  - Happy Hacking Keyboard config"
else
    echo "No X11 configs found in ~/etc/X11/xorg.conf.d/"
fi

# udev rules for keyboard layout
if [ -d ~/etc/udev/rules.d ]; then
    echo "Copying udev rules..."
    sudo cp ~/etc/udev/rules.d/* /etc/udev/rules.d/
    sudo chown root:root /etc/udev/rules.d/*
    sudo chmod 644 /etc/udev/rules.d/*
    echo "udev rules deployed successfully"
    echo "  - Keyboard layout rules"
    
    # Reload udev rules and trigger for existing devices
    echo "Reloading udev rules..."
    sudo udevadm control --reload-rules
    sudo udevadm trigger --subsystem-match=input
    echo "udev rules reloaded and triggered"
else
    echo "No udev rules found in ~/etc/udev/rules.d/"
fi

# Font configuration for bitmapped fonts
echo "Configuring fonts for bitmapped font support..."

# Remove conflicting font configs if they exist
FONT_CONF_DIR="/etc/fonts/conf.d"
CONFLICTING_CONFIGS=(
    "70-no-bitmaps-and-emoji.conf"
    "70-no-bitmaps.conf"
    "70-no-bitmaps-except-emoji.conf"
    "10-scale-bitmap-fonts.conf"
)

for config in "${CONFLICTING_CONFIGS[@]}"; do
    if [ -e "$FONT_CONF_DIR/$config" ]; then
        echo "Removing conflicting font config: $config"
        sudo rm -f "$FONT_CONF_DIR/$config"
    fi
done

# Create symlink for bitmap font support
BITMAP_CONF="70-yes-bitmaps.conf"
BITMAP_SOURCE="/usr/share/fontconfig/conf.avail/$BITMAP_CONF"
BITMAP_TARGET="$FONT_CONF_DIR/$BITMAP_CONF"

if [ -f "$BITMAP_SOURCE" ]; then
    if [ ! -L "$BITMAP_TARGET" ]; then
        echo "Creating symlink for bitmap font support..."
        sudo ln -sf "$BITMAP_SOURCE" "$BITMAP_TARGET"
        echo "Bitmap font support enabled"
    else
        echo "Bitmap font symlink already exists"
    fi
else
    echo "Warning: $BITMAP_SOURCE not found - bitmap font support may not be available"
fi

# Rebuild font cache
echo "Rebuilding font cache..."
sudo fc-cache -f -v > /dev/null 2>&1
echo "Font cache rebuilt"

# Other system configs as needed
# sudo cp ~/etc/dictd.conf /etc/dictd.conf

echo "System configuration deployment complete!"
echo "You may need to restart X11 for changes to take effect."
