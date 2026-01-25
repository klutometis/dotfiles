#!/bin/bash
# Deploy system configuration files

set -e

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

# Install i3lock if not present
if ! command -v i3lock &> /dev/null; then
  echo "Installing i3lock..."
  sudo apt-get update
  sudo apt-get install -y i3lock
fi

# Install i3status if not present
if ! command -v i3status &> /dev/null; then
  echo "Installing i3status..."
  sudo apt-get update
  sudo apt-get install -y i3status
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

# Install inotify-tools if not present
if ! command -v inotifywait &> /dev/null; then
  echo "Installing inotify-tools..."
  sudo apt-get update
  sudo apt-get install -y inotify-tools
fi

# Install gh (GitHub CLI) if not present
if ! command -v gh &> /dev/null; then
  echo "Installing GitHub CLI (gh)..."
  sudo apt-get update
  sudo apt-get install -y gh
fi

# Install ripgrep if not present
if ! command -v ripgrep &> /dev/null; then
  echo "Installing ripgrep..."
  sudo apt-get update
  sudo apt-get install -y ripgrep
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
# Go Installation & Tools
# =============================================================================

echo "Setting up Go and Go tools..."

# Install Go if not present
if ! command -v go &> /dev/null; then
  echo "Installing Go..."
  
  # Fetch latest Go version using canonical method
  GO_VERSION=$(curl -s "https://go.dev/dl/?mode=json" | jq -r '.[0].version')
  GO_TARBALL="${GO_VERSION}.linux-amd64.tar.gz"
  
  echo "Latest Go version: ${GO_VERSION}"
  
  # Download Go
  cd /tmp
  wget -q "https://go.dev/dl/${GO_TARBALL}"
  
  # Remove any previous installation and extract
  sudo rm -rf /usr/local/go
  sudo tar -C /usr/local -xzf "${GO_TARBALL}"
  
  # Clean up
  rm "${GO_TARBALL}"
  
  # Add to PATH for current session
  export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
  
  echo "Go ${GO_VERSION} installed"
else
  echo "Go already installed ($(go version))"
  # Ensure Go paths are in current session
  export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
fi

# Install bluetuith if not present
if ! command -v bluetuith &> /dev/null; then
  echo "Installing bluetuith..."
  go install github.com/darkhz/bluetuith@latest
fi


# =============================================================================
# Language Server Installation (Hermetic, Isolated Environments)
# =============================================================================

echo "Setting up language servers for Emacs eglot..."

# -----------------------------------------------------------------------------
# C/C++ Language Server - clangd (native Ubuntu package)
# -----------------------------------------------------------------------------
if ! command -v clangd &> /dev/null; then
  echo "Installing clangd..."
  sudo apt-get update
  sudo apt-get install -y clangd
else
  echo "clangd already installed"
fi

# -----------------------------------------------------------------------------
# Python Package Manager - uv (Rust-based, hermetic)
# Replaces: pip, pipx, pyenv - 10-100x faster, proper isolation
# -----------------------------------------------------------------------------
if ! command -v uv &> /dev/null; then
  echo "Installing uv (modern Python package manager)..."
  curl -LsSf https://astral.sh/uv/install.sh | sh
  # Add to PATH for current session
  export PATH="$HOME/.cargo/bin:$PATH"
else
  echo "uv already installed"
fi

# -----------------------------------------------------------------------------
# Python Language Server - python-lsp-server (via uv tool)
# Isolated installation, won't interfere with system Python
# -----------------------------------------------------------------------------
if ! command -v pylsp &> /dev/null; then
  echo "Installing python-lsp-server via uv..."
  uv tool install python-lsp-server
else
  echo "python-lsp-server already installed"
fi

# -----------------------------------------------------------------------------
# Node.js Version Manager - mise (Rust-based, multi-language, hermetic)
# Replaces: nvm, fnm, volta - Fastest, true isolation, no shell hooks
# Also manages other tools (Python, Ruby, etc.) if needed later
# -----------------------------------------------------------------------------
if ! command -v mise &> /dev/null; then
  echo "Installing mise (modern polyglot version manager)..."
  curl https://mise.run | sh
  # Add to PATH for current session
  export PATH="$HOME/.local/bin:$PATH"
  
  # Initialize mise for current shell (bash/zsh auto-detected)
  eval "$(mise activate bash)"
else
  echo "mise already installed"
fi

# -----------------------------------------------------------------------------
# Node.js Installation via mise
# Hermetic, per-user installation separate from system Node
# -----------------------------------------------------------------------------
if ! mise ls node &> /dev/null || [ -z "$(mise ls node 2>/dev/null)" ]; then
  echo "Installing Node.js via mise..."
  mise use --global node@lts
  # Ensure mise Node is in PATH
  eval "$(mise activate bash)"
else
  echo "Node.js already installed via mise"
fi

# -----------------------------------------------------------------------------
# TypeScript Language Server (via mise Node's npm)
# Isolated to mise's Node, won't touch system npm
# -----------------------------------------------------------------------------
if ! command -v typescript-language-server &> /dev/null; then
  echo "Installing typescript-language-server..."
  mise x -- npm install -g typescript-language-server typescript
else
  echo "typescript-language-server already installed"
fi

# -----------------------------------------------------------------------------
# Bash Language Server (via mise Node's npm)
# -----------------------------------------------------------------------------
if ! command -v bash-language-server &> /dev/null; then
  echo "Installing bash-language-server..."
  mise x -- npm install -g bash-language-server
else
  echo "bash-language-server already installed"
fi

echo ""
echo "Language server installation complete!"
echo "Installed servers:"
echo "  - clangd (C/C++): $(which clangd 2>/dev/null || echo 'not found')"
echo "  - pylsp (Python): $(which pylsp 2>/dev/null || echo 'not found')"
echo "  - typescript-language-server: $(which typescript-language-server 2>/dev/null || echo 'not found')"
echo "  - bash-language-server: $(which bash-language-server 2>/dev/null || echo 'not found')"
echo ""
echo "Note: You may need to restart your shell or run:"
echo "  export PATH=\"\$HOME/.cargo/bin:\$HOME/.local/bin:\$HOME/.local/share/mise/shims:\$PATH\""
echo ""



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
      mv "$source"/* "$target"/ 2> /dev/null || true
      mv "$source"/.* "$target"/ 2> /dev/null || true
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
create_directory_symlink ~/Downloads ~/var/web # This one should already exist

echo "Directory symlinks configured"

# =============================================================================
# DNS Configuration (Simple & Clean)
# =============================================================================

echo "Configuring DNS..."

# Step 1: Configure dnsmasq to use Google DNS upstream
echo "Configuring dnsmasq upstream DNS..."
sudo mkdir -p /etc/dnsmasq.d
sudo tee /etc/dnsmasq.d/google-dns.conf > /dev/null << 'EOF'
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
sudo tee /etc/dhcp/dhclient.conf > /dev/null << 'EOF'
# Force only localhost DNS - dnsmasq handles everything
supersede domain-name-servers 127.0.0.1;
EOF

# Step 2.5: Tell NetworkManager to ignore DHCP DNS servers
echo "Configuring NetworkManager to ignore DHCP DNS..."
sudo mkdir -p /etc/NetworkManager/conf.d
sudo tee /etc/NetworkManager/conf.d/dns.conf > /dev/null << 'EOF'
[main]
dns=dnsmasq

[connection]
ipv4.ignore-auto-dns=yes
ipv6.ignore-auto-dns=yes
EOF

# Step 3: Configure static search domains via resolvconf (if available)
if [ -d /etc/resolvconf/resolv.conf.d ]; then
  echo "Configuring search domains..."
  sudo tee /etc/resolvconf/resolv.conf.d/base > /dev/null << 'EOF'
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

# Reload sxhkd if running
if pgrep -x sxhkd > /dev/null; then
  echo "Reloading sxhkd configuration..."
  pkill -USR1 -x sxhkd
  echo "  ✓ sxhkd reloaded"
fi

echo ""
echo "System configuration deployment complete!"
echo ""
echo "DNS Configuration Applied:"
echo "  ✓ NetworkManager configured to use Google DNS (8.8.8.8, 8.8.4.4)"
echo "  ✓ dnsmasq configured to forward to Google DNS"
echo "  ✓ Auto-DNS from DHCP disabled"
echo ""
echo "To verify DNS is working:"
echo "  cat /etc/resolv.conf    # Should show nameserver 127.0.0.1 and search domains"
echo "  dig google.com          # Should be fast (~10-20ms)"
echo ""
echo "Note: Source your shell configuration to pick up PATH updates:"
echo "  source ~/.bashrc  # or ~/.zshrc"
