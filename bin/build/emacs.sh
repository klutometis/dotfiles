#!/bin/bash

# Build Emacs for Terminal Use
# Based on var/build/emacs.md

set -e  # Exit on any error

echo "=== Building Emacs from source ==="
echo "This script will build a terminal-only Emacs with native compilation support."
echo

# Step 1: Build and Install tree-sitter
echo "Step 1: Building tree-sitter..."
mkdir -p ~/build
cd ~/build
rm -rf tree-sitter
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make
sudo make install
sudo ldconfig
echo "tree-sitter build complete."
echo

# Step 2: Install Emacs Build Dependencies
echo "Step 2: Installing build dependencies..."
sudo apt install -y build-essential libncurses-dev libgccjit-14-dev libjansson-dev texinfo libdbus-1-dev
echo "Dependencies installed."
echo

# Step 3: Build Emacs
echo "Step 3: Building Emacs..."
cd ~/build
rm -rf emacs
git clone https://github.com/emacs-mirror/emacs.git
cd emacs
./autogen.sh
./configure --without-x --with-tree-sitter --with-modules --with-native-compilation
make -j$(nproc)
sudo make install
echo "Emacs build complete."
echo

echo "=== Build finished successfully ==="
echo "Terminal-only Emacs with native compilation has been installed."
echo "Run 'emacs -nw' to start Emacs in terminal mode."
