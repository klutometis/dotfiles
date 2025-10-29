#!/bin/bash
# Deploy system configuration files

set -e

# Configuration flags
ENABLE_KEYBOARD_CONFIG=${ENABLE_KEYBOARD_CONFIG:-false}

echo "Deploying system configuration files..."

# =============================================================================
# Package Installation Section
# =============================================================================

echo "Checking and installing system packages..."

# Install sxhkd if not present
if ! command -v sxhkd &> /dev/null; then
    echo "Installing sxhkd..."
    sudo apt-get update
    sudo apt-get install -y sxhkd
fi

# Install surfraw if not present
if ! command -v surfraw &> /dev/null; then
    echo "Installing surfraw..."
    sudo apt-get update
    sudo apt-get install -y surfraw
fi

# Install rofi if not present
if ! command -v rofi &> /dev/null; then
    echo "Installing rofi..."
    sudo apt-get update
    sudo apt-get install -y rofi
fi

# =============================================================================
# Python Tools Installation (via uv)
# =============================================================================

echo "Setting up Python tools via uv..."

# Install uv if not present
if ! command -v uv &> /dev/null; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
fi

# Install/update aider-ce from git
# NOTE: Temporarily pinned to v0.87.13 due to issues with HEAD
# TODO: Remove version pin once HEAD is fixed and working again
echo "Installing aider-ce (Community Edition)..."
uv tool install --force git+https://github.com/dwash96/aider-ce.git@v0.87.13


# =============================================================================
# Directory Symlinks Configuration
# =============================================================================

echo "Configuring standard directory symlinks..."

# Create target directories
mkdir -p ~/var/{doc,music,pictures,videos,templates,public,web}

# Function to create symlink safely
create_directory_symlink() {
    local source="$1"
    local target="$2"
    
    if [ -L "$source" ]; then
        # It's already a symlink, check if it points to the right place
        current_target=$(readlink "$source")
        if [ "$current_target" = "$target" ]; then
            echo "  ✓ $source already linked to $target"
        else
            echo "  Updating symlink $source -> $target"
            rm "$source"
            ln -sf "$target" "$source"
        fi
    elif [ -d "$source" ]; then
        # It's a real directory, move contents and create symlink
        echo "  Moving contents from $source to $target"
        if [ "$(ls -A $source)" ]; then
            # Directory has contents
            mv "$source"/* "$target"/ 2>/dev/null || true
            mv "$source"/.* "$target"/ 2>/dev/null || true
        fi
        rmdir "$source"
        ln -sf "$target" "$source"
        echo "  Created symlink $source -> $target"
    else
        # Doesn't exist, create symlink
        ln -sf "$target" "$source"
        echo "  Created symlink $source -> $target"
    fi
}

# Create all the symlinks
echo "Setting up directory symlinks..."
create_directory_symlink ~/Desktop ~/
create_directory_symlink ~/Documents ~/var/doc
create_directory_symlink ~/Music ~/var/music
create_directory_symlink ~/Pictures ~/var/pictures
create_directory_symlink ~/Videos ~/var/videos
create_directory_symlink ~/Templates ~/var/templates
create_directory_symlink ~/Public ~/var/public
create_directory_symlink ~/Downloads ~/var/web  # This one should already exist

echo "Directory symlinks configured"

# =============================================================================
# X11 Configuration
# =============================================================================

if [ "$ENABLE_KEYBOARD_CONFIG" = "true" ]; then
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
else
    echo "Skipping X11 keyboard configuration (ENABLE_KEYBOARD_CONFIG=false)"
fi

# =============================================================================
# udev Rules
# =============================================================================

if [ "$ENABLE_KEYBOARD_CONFIG" = "true" ]; then
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
else
    echo "Skipping udev keyboard rules (ENABLE_KEYBOARD_CONFIG=false)"
fi

# =============================================================================
# DHCP Client Configuration
# =============================================================================

echo "Configuring DHCP client domain search..."

DHCLIENT_CONF="/etc/dhcp/dhclient.conf"

# Check if DHCP_DOMAIN_SEARCH is set in environment
if [ -n "$DHCP_DOMAIN_SEARCH" ]; then
    DOMAIN_SEARCH_LINE="supersede domain-search $DHCP_DOMAIN_SEARCH;"
    
    # Check if the configuration already exists
    if [ -f "$DHCLIENT_CONF" ]; then
        if grep -q "supersede domain-search" "$DHCLIENT_CONF"; then
            echo "DHCP domain-search configuration already exists"
        else
            echo "Adding domain-search configuration to dhclient.conf..."
            echo "" | sudo tee -a "$DHCLIENT_CONF" > /dev/null
            echo "# Custom domain search configuration" | sudo tee -a "$DHCLIENT_CONF" > /dev/null
            echo "$DOMAIN_SEARCH_LINE" | sudo tee -a "$DHCLIENT_CONF" > /dev/null
            echo "Domain-search configuration added"
            
            # Restart DHCP client to apply changes
            echo "Restarting DHCP client..."
            sudo dhclient -r && sudo dhclient
            echo "DHCP client restarted"
        fi
    else
        echo "Creating dhclient.conf with domain-search configuration..."
        echo "# Custom domain search configuration" | sudo tee "$DHCLIENT_CONF" > /dev/null
        echo "$DOMAIN_SEARCH_LINE" | sudo tee -a "$DHCLIENT_CONF" > /dev/null
        echo "dhclient.conf created with domain-search configuration"
        
        # Restart DHCP client to apply changes
        echo "Restarting DHCP client..."
        sudo dhclient -r && sudo dhclient
        echo "DHCP client restarted"
    fi
else
    echo "DHCP_DOMAIN_SEARCH not set - skipping domain search configuration"
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

# =============================================================================
# GTK Configuration
# =============================================================================

# GTK key theme configuration
echo "Configuring GTK key theme..."
if command -v gsettings &> /dev/null; then
    gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
    echo "GTK3 Emacs key theme set via gsettings"
else
    echo "Warning: gsettings not found - GTK3 Emacs key theme may not work in some applications"
fi

# =============================================================================
# Privoxy Ad Blocking
# =============================================================================

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

echo ""
echo "System configuration deployment complete!"
echo "You may need to:"
if [ "$ENABLE_KEYBOARD_CONFIG" = "true" ]; then
    echo "  1. Restart X11 for keyboard changes to take effect"
    echo "  2. Reload sxhkd configuration: pkill -USR1 -x sxhkd"
    echo "  3. Source your shell configuration for PATH updates"
else
    echo "  1. Reload sxhkd configuration: pkill -USR1 -x sxhkd"
    echo "  2. Source your shell configuration for PATH updates"
    echo "  Note: Keyboard configuration was skipped (set ENABLE_KEYBOARD_CONFIG=true to enable)"
fi
