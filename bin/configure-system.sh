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

# Install picom if not present
if ! command -v picom &> /dev/null; then
    echo "Installing picom..."
    sudo apt-get update
    sudo apt-get install -y picom
fi

# Install i3 if not present
if ! command -v i3 &> /dev/null; then
    echo "Installing i3 window manager..."
    sudo apt-get update
    sudo apt-get install -y i3
fi

# Install alacritty if not present
if ! command -v alacritty &> /dev/null; then
    echo "Installing alacritty terminal..."
    sudo apt-get update
    sudo apt-get install -y alacritty
fi

# Install fzf if not present
if ! command -v fzf &> /dev/null; then
    echo "Installing fzf..."
    sudo apt-get update
    sudo apt-get install -y fzf
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

# Install mcp-proxy via uv
echo "Installing mcp-proxy..."
uv tool install git+https://github.com/sparfenyuk/mcp-proxy

# =============================================================================
# Node.js & npm Installation (via nvm)
# =============================================================================

echo "Setting up Node.js and npm via nvm..."

# Install nvm if not present
if [ ! -d "$HOME/.nvm" ]; then
    echo "Installing nvm..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/refs/heads/master/install.sh | bash
    
    # Load nvm for current session
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
else
    echo "nvm already installed"
    # Load nvm for current session
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
fi

# Install latest Node.js and npm if not present
if ! command -v node &> /dev/null; then
    echo "Installing Node.js..."
    nvm install node
    nvm use node
    nvm install-latest-npm
else
    echo "Node.js already installed ($(node --version))"
fi

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
# DNS Configuration (Simple & Clean)
# =============================================================================

echo "Configuring DNS..."

# Step 1: Configure dnsmasq to use Google DNS upstream
echo "Configuring dnsmasq upstream DNS..."
sudo mkdir -p /etc/dnsmasq.d
sudo tee /etc/dnsmasq.d/google-dns.conf > /dev/null <<'EOF'
# Forward all DNS queries to Google DNS
server=8.8.8.8
server=8.8.4.4
EOF

# Restart dnsmasq if it's running
if systemctl is-active --quiet dnsmasq; then
    echo "Restarting dnsmasq..."
    sudo systemctl restart dnsmasq
fi

# Step 2: Configure dhclient to only use 127.0.0.1 (ignore DHCP nameservers)
echo "Configuring dhclient to use only localhost DNS..."
sudo tee /etc/dhcp/dhclient.conf > /dev/null <<'EOF'
# Force only localhost DNS - dnsmasq handles everything
supersede domain-name-servers 127.0.0.1;
EOF

# Step 2.5: Tell NetworkManager to ignore DHCP DNS servers
echo "Configuring NetworkManager to ignore DHCP DNS..."
sudo mkdir -p /etc/NetworkManager/conf.d
sudo tee /etc/NetworkManager/conf.d/dns.conf > /dev/null <<'EOF'
[main]
dns=dnsmasq

[connection]
ipv4.ignore-auto-dns=yes
ipv6.ignore-auto-dns=yes
EOF

# Step 3: Configure static search domains via resolvconf (if available)
if [ -d /etc/resolvconf/resolv.conf.d ]; then
    echo "Configuring search domains..."
    sudo tee /etc/resolvconf/resolv.conf.d/base > /dev/null <<'EOF'
search corp.google.com prod.google.com prodz.google.com google.com
EOF

    # Step 4: Regenerate resolv.conf
    echo "Regenerating resolv.conf..."
    sudo resolvconf -u
else
    echo "Skipping resolvconf search domain configuration (not available on this system)"
fi

# Step 5: Restart NetworkManager to apply changes
echo "Restarting NetworkManager..."
sudo systemctl restart NetworkManager

# Wait for DNS to be ready using wait-for-it
echo "Waiting for DNS connectivity..."
wait-for-it 8.8.8.8:53 --timeout=30 --strict

echo "DNS configured:"
echo "  ✓ Applications query: 127.0.0.1 (dnsmasq)"
echo "  ✓ dnsmasq forwards to: 8.8.8.8, 8.8.4.4 (Google DNS)"
echo "  ✓ Search domains: corp.google.com prod.google.com prodz.google.com google.com"

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
echo ""
echo "DNS Configuration Applied:"
echo "  ✓ NetworkManager configured to use Google DNS (8.8.8.8, 8.8.4.4)"
echo "  ✓ dnsmasq configured to forward to Google DNS"
echo "  ✓ Auto-DNS from DHCP disabled"
echo ""
if [ "$ENABLE_KEYBOARD_CONFIG" = "true" ]; then
    echo "You may need to:"
    echo "  1. Restart X11 for keyboard changes to take effect"
    echo "  2. Reload sxhkd configuration: pkill -USR1 -x sxhkd"
    echo "  3. Source your shell configuration for PATH updates"
else
    echo "You may need to:"
    echo "  1. Reload sxhkd configuration: pkill -USR1 -x sxhkd"
    echo "  2. Source your shell configuration for PATH updates"
    echo "  Note: Keyboard configuration was skipped (set ENABLE_KEYBOARD_CONFIG=true to enable)"
fi
echo ""
echo "To verify DNS is working:"
echo "  cat /etc/resolv.conf    # Should show nameserver 127.0.0.1 and search domains"
echo "  dig google.com          # Should be fast (~10-20ms)"
