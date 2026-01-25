# Home Directory Dotfiles

A complete home directory configuration system with dotfiles, system configs, and utilities.

## Quick Start

For a new system, bootstrap everything with:

```bash
wget https://raw.githubusercontent.com/klutometis/dotfiles/main/bin/bootstrap-dotfiles.sh
chmod +x bootstrap-dotfiles.sh
./bootstrap-dotfiles.sh
```

This script handles the entire setup: git initialization, dotfile symlinks, system configuration, and secrets unlocking.

## Directory Structure

### Configuration
- **`etc/dotfiles/`** - All dotfiles (prefixed with `dot-`) that get symlinked to `~/`
- **`etc/X11/`** - X11 keyboard configurations  
- **`etc/udev/`** - udev rules for hardware detection
- **`bin/configure-system.sh`** - Deploys system-level configs with proper permissions

### Documentation & Build Instructions  
- **`var/build/`** - Build/compile instructions paired with automation (e.g., `emacs.md` for compiling Emacs from source)
- **`var/doc/`** - Usage documentation, configuration guides, working notes (e.g., `emacs.md` for Emacs usage/keybindings)

### Security
- **Git-crypt** - Encrypts sensitive files (unlocked during bootstrap)
- **GNU Pass** - Password store integration via `dot-password-store`
- **`dot-env-secrets`** - Encrypted environment variables

### Utilities
- **`bin/`** - Personal utility scripts (battery status, emacs variants, media controls, etc.)
- **`prg/`** - Personal projects and programs

### Applications Configured
- **Editors**: Emacs (with extensive config), Vim
- **Window Managers**: i3, Qtile, Ratpoison, EXWM  
- **Browsers**: Conkeror, Vimium configs
- **Terminal**: Alacritty, tmux, zsh
- **Media**: mplayer controls, redshift configs
- **Development**: Git, surfraw web search, various language tools

## Features

- **One-command setup** via bootstrap script
- **System integration** with proper permissions for X11/udev configs
- **Secrets management** with git-crypt and pass
- **Comprehensive documentation** following established conventions
- **Modular configuration** with application-specific files
- **Cross-system compatibility** with different keyboard/display setups
