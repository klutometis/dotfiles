# Building Emacs for EXWM

## Overview

Building Emacs from source with graphical support for EXWM window manager.

## Prerequisites

Starting from a fresh Ubuntu system, we need development libraries for graphics,
text rendering, and desktop integration.

## Package Installation

### Basic Image Libraries

```bash
sudo apt install libjpeg-dev libpng-dev libgif-dev libtiff-dev
```

### Desktop Integration

```bash
sudo apt install libdbus-1-dev libxi-dev dbus-x11
```

### Enhanced Graphics & Text Rendering

```bash
sudo apt install libcairo2-dev libmagickwand-dev libharfbuzz-dev libwebkit2gtk-4.1-dev
```

## Configuration

```bash
./configure --with-tree-sitter --with-x-toolkit=lucid --with-modules --with-native-compilation --with-json
```

## Final Build Configuration

- **Window System**: X11 with LUCID toolkit
- **Graphics**: Cairo, SVG, HarfBuzz, Freetype
- **Images**: JPEG, PNG, GIF, TIFF, WebP
- **Integration**: DBus, XInput2, GSettings
- **Features**: Native compilation, Tree-sitter, Dynamic modules
- **Missing**: ImageMagick, Xwidgets (couldn't resolve dependencies)

## Build & Install

```bash
make -j$(nproc)
sudo make install
```

## Notes

- Built for EXWM compatibility with enhanced graphics support
- Native compilation enabled for better performance
- DBus support for desktop integration
- Configure warned about unrecognized `--with-json` option (likely built-in)
