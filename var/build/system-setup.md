# System Setup Guide

## Overview
Complete setup instructions for a new system with this dotfiles configuration. This guide covers cloning the repository, setting up dotfiles, configuring GPG/pass, and building custom software.

## Prerequisites
- Git configured with SSH keys
- Basic build tools (make, gcc, cmake, etc.)
- Sudo access for package installation

## Steps

### 1. Set up dotfiles repository
Since this configuration uses the home directory as the git repository:

```bash
cd ~/
git init
git remote add origin git@github.com:klutometis/dotfiles.git
git fetch origin
git checkout -b main
git pull origin main
git submodule init
git submodule update --recursive
```

**Alternative approach:** For future consideration, you could use a "bare repository" approach:
```bash
git clone --bare git@github.com:klutometis/dotfiles.git ~/.dotfiles
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dotfiles checkout
dotfiles submodule init
dotfiles submodule update --recursive
```
This keeps git metadata separate while maintaining home directory as working tree.

### 2. Set up dotfiles with stow
```bash
stow dotfiles --adopt --restow
```

This will create symlinks for all dotfiles and adopt any existing files that conflict.

**Note:** Since the home directory is the git repository, the dotfiles are already in place after the git checkout.

### 3. Set up pass and GPG keys
```bash
# Import GPG keys (assuming they're in ~/tmp)
gpg --import ~/tmp/private.key
gpg --import ~/tmp/public.key

# Find your key ID
gpg --list-keys

# Trust the key (replace [key-id] with actual key ID)
gpg --edit-key [key-id]
# In GPG prompt: trust -> 5 -> y -> quit

# Initialize pass with your key
pass init [key-id]

# Clean up temporary key files
rm ~/tmp/*.key
```

### 4. Set up Chrome profile symlinks
```bash
# Start Chrome to create initial profiles, then set up symlinks
cd ~/.config/google-chrome
ln -sf Default work-profile
ln -sf "Profile 1" personal-profile
```

**Note:** You may need to adjust which profile directories map to work/personal based on your actual setup.

### 5. Build custom software
Build the following components using their respective guides:

#### i3 window manager
```bash
# See: var/build/i3.md
```

#### Emacs
```bash
# See: var/build/emacs.md
```

#### Tree-sitter markdown
```bash
# See: var/build/tree-sitter-markdown.md
```

### 6. Additional system setup

#### Create keyboard options script
Make sure the keyboard setup script is executable:
```bash
chmod +x ~/bin/set-keyboard-options.sh
```

#### Set up X11 keyboard configuration
```bash
# Create or verify the X11 keyboard config
sudo mkdir -p /etc/X11/xorg.conf.d/
# The 99-keyboard-happy-hacking.conf should already be in place via dotfiles
```

#### Install system packages
Install any system-specific packages needed:
```bash
# Example packages (adjust for your distribution)
sudo apt-get install alacritty tmux redshift xscreensaver autorandr pass
```

#### Configure autorandr
Set up display profiles:
```bash
# Set up your primary display configuration
autorandr --save standalone
```

### 7. Verify setup
After completing all steps:

1. **Test keyboard configuration:**
   ```bash
   # After restarting X11, test compose key functionality
   setxkbmap -query  # Should show dvorak layout
   ```

2. **Test Chrome profiles:**
   ```bash
   ls -la ~/.config/google-chrome/work-profile
   ls -la ~/.config/google-chrome/personal-profile
   ```

3. **Test pass functionality:**
   ```bash
   pass --version
   pass ls
   ```

## Notes
- The keyboard setup uses a runtime workaround script instead of pure X11 configuration due to system stack interference
- Chrome profile symlinks provide consistent naming across different machines
- GPG key trust level 5 (ultimate) is appropriate for your own keys
- Build processes may require additional system dependencies not listed here
- Some configurations may need machine-specific adjustments

## Troubleshooting
- Check `/tmp/xinitrc.log` and `/tmp/xprofile.log` for X11 startup issues
- Verify all symlinks are correctly created with `ls -la`
- Ensure GPG keys are properly trusted with `gpg --list-keys`
- Test keyboard layouts with `setxkbmap -query` after X11 restart
