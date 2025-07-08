# Building Emacs for Terminal Use

## Overview

Building a terminal-only Emacs from source. This build is optimized for performance in a non-graphical environment, removing all X11 and GUI-related dependencies.

## Prerequisites

This build requires `tree-sitter` to be built from source first, and development libraries for native compilation and terminal rendering.

### 1. Build and Install `tree-sitter`

Emacs requires a recent version of `tree-sitter`.

```bash
mkdir -p ~/build
cd ~/build
rm -rf tree-sitter
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make
sudo make install
sudo ldconfig
```

### 2. Install Emacs Build Dependencies

Install the necessary libraries for a terminal-only build with native compilation.

```bash
sudo apt install build-essential libncurses-dev libgccjit-14-dev libjansson-dev texinfo libdbus-1-dev
```

## Configuration

Navigate to the Emacs source directory (`~/build/emacs`) to configure the build.

```bash
cd ~/build
rm -rf emacs
git clone https://github.com/emacs-mirror/emacs.git
cd emacs
./autogen.sh
./configure --without-x --with-tree-sitter --with-modules --with-native-compilation
```

## Final Build Configuration

- **Window System**: None (Terminal only)
- **Graphics**: None
- **Images**: None
- **Features**: Native compilation, Tree-sitter, Dynamic modules

## Build & Install

```bash
make -j$(nproc)
sudo make install
```

## Notes

- This is a terminal-only (`-nw`) build of Emacs.
- All X11, GUI, and image library support has been removed for a leaner build.
- Native compilation is enabled for better performance.
