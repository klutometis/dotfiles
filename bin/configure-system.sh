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

# GTK key theme configuration
echo "Configuring GTK key theme..."
# Note: While we have GTK key theme configs in dotfiles:
#   - ~/.gtkrc-2.0: gtk-key-theme-name = "Emacs"
#   - ~/.config/gtk-3.0/settings.ini: gtk-key-theme-name = Emacs
# Some GTK3 applications (e.g. Chrome) don't respect the config files.
# Per https://wiki.archlinux.org/title/GTK#Keyboard_shortcuts:
# "desktop environments and applications can override these settings"
# and GSettings takes precedence over settings.ini in many cases.
if command -v gsettings &> /dev/null; then
    gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
    echo "GTK3 Emacs key theme set via gsettings"
else
    echo "Warning: gsettings not found - GTK3 Emacs key theme may not work in some applications"
fi

# Configure Downloads directory
echo "Configuring Downloads directory redirection..."
mkdir -p ~/var/web

if [ -d ~/Downloads ] || [ -L ~/Downloads ]; then
    echo "Removing existing ~/Downloads..."
    rm -rf ~/Downloads
fi

echo "Creating symlink ~/Downloads -> ~/var/web..."
ln -sf ~/var/web ~/Downloads
echo "Downloads directory configured to use ~/var/web"

# Privoxy ad blocking setup
echo "Setting up privoxy ad blocking lists..."
if command -v privoxy &> /dev/null; then
    # Create build directory if it doesn't exist
    BUILD_DIR="$HOME/build"
    mkdir -p "$BUILD_DIR"
    
    # Clone or update privoxy-blocklist
    if [ ! -d "$BUILD_DIR/privoxy-blocklist" ]; then
        echo "Cloning privoxy-blocklist..."
        git clone https://github.com/Andrwe/privoxy-blocklist.git "$BUILD_DIR/privoxy-blocklist"
    else
        echo "Updating privoxy-blocklist..."
        cd "$BUILD_DIR/privoxy-blocklist"
        git pull
    fi
    
    cd "$BUILD_DIR/privoxy-blocklist"
    
    # Run twice - first time initializes config files
    echo "Initializing privoxy blocklist configuration..."
    sudo ./privoxy-blocklist.sh -f attribute_global_name -f attribute_global_exact -f attribute_global_contain -f attribute_global_startswith -f attribute_global_endswith -f class_global -f id_global || true
    
    echo "Applying privoxy blocklist filters..."
    sudo ./privoxy-blocklist.sh -f attribute_global_name -f attribute_global_exact -f attribute_global_contain -f attribute_global_startswith -f attribute_global_endswith -f class_global -f id_global
    
    # Restart privoxy to apply changes
    echo "Restarting privoxy..."
    sudo systemctl restart privoxy || sudo service privoxy restart || true
    
    echo "Privoxy ad blocking setup complete"
else
    echo "Warning: privoxy not installed - skipping ad blocking setup"
fi

# Other system configs as needed
# sudo cp ~/etc/dictd.conf /etc/dictd.conf

echo "System configuration deployment complete!"
echo "You may need to restart X11 for changes to take effect."
