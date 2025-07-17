#!/usr/bin/env bash
# Bootstrap dotfiles when homedir is the repo but configs are in etc/dotfiles

set -e

cd ~

echo "=== Bootstrapping dotfiles in home directory ==="

# 1. Initialize git repo if needed
if [ ! -d .git ]; then
    echo "Initializing git repository..."
    git init
    git remote add origin git@github.com:klutometis/dotfiles.git
else
    echo "Git repository already initialized"
fi

# 2. Configure git to not show untracked files
git config status.showUntrackedFiles no

# 3. Pull the dotfiles
echo "Fetching dotfiles..."
git fetch origin main
git checkout -f main  # Force checkout to overwrite any existing files
git branch --set-upstream-to=origin/main main

# 4. Initialize submodules
echo "Initializing submodules..."
git submodule update --init --recursive

# 5. Unlock git-crypt
if command -v git-crypt &> /dev/null; then
    if ! git-crypt status &> /dev/null; then
        echo "Please unlock git-crypt with: git-crypt unlock"
        echo "You need the git-crypt key to access encrypted files"
        read -p "Press enter when ready..."
        git-crypt unlock
    fi
else
    echo "Error: git-crypt not installed"
    echo "Install with: sudo apt install git-crypt"
    exit 1
fi

# 6. Run stow to create symlinks
echo "Creating dotfile symlinks with stow..."
if command -v stow &> /dev/null; then
    stow -v --restow --adopt dotfiles
else
    echo "Error: stow not installed"
    echo "Install with: sudo apt install stow"
    exit 1
fi

# 7. Run system configuration
echo "Running system configuration..."
~/bin/configure-system.sh

echo "=== Bootstrap Complete ==="
echo "Please restart your session for all changes to take effect"
