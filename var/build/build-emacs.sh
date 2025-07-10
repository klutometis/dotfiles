#!/bin/bash
#
# This script automates the building and installation of a terminal-only Emacs
# from source, along with its `tree-sitter` dependency. The steps are taken
# from the `var/build/emacs.md` file.

# Exit immediately if a command exits with a non-zero status.
set -e
# Print each command to the terminal before executing it.
set -x

### 1. Build and Install `tree-sitter`
echo "--- Building and installing tree-sitter ---"
mkdir -p ~/build
cd ~/build
rm -rf tree-sitter
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make
sudo make install
sudo ldconfig

### 2. Install Emacs Build Dependencies
echo "--- Installing Emacs build dependencies ---"
sudo apt install build-essential libncurses-dev libgccjit-14-dev libjansson-dev texinfo libdbus-1-dev

### 3. Configure Emacs
echo "--- Cloning and configuring Emacs ---"
cd ~/build
rm -rf emacs
git clone https://github.com/emacs-mirror/emacs.git
cd emacs
./autogen.sh
./configure --without-x --with-tree-sitter --with-modules --with-native-compilation

### 4. Build & Install Emacs
echo "--- Building and installing Emacs ---"
make -j$(nproc)
sudo make install

echo "--- Emacs installation complete ---"
