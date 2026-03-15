# Plan: Migrate from alacritty+tmux to wezterm

## Summary

Evaluated kitty → hit font rendering wall (no subpixel, no bitmap support).
WezTerm renders Fixed6x13.ttf identically to alacritty via `MONOCHROME` +
`NO_HINTING` flags, AND supports kitty graphics protocol for inline images
in emacs.

## What changed

| File | Status |
|------|--------|
| `~/.config/wezterm/wezterm.lua` | Created — full config |
| `~/.emacs.d/init.el` | Modified — kitty-graphics.el guard accepts WezTerm |
| `~/.zshrc` | Modified — wezterm aliases, kitty PATH |
| `~/.config/fontconfig/fonts.conf` | Created — hinting/AA off for Fixed6x13 |
| `~/.local/share/fonts/Fixed6x13.ttf` | Created — bitmap2ttf conversion |
| `~/bin/terminal-wezterm` | Created — launcher |
| `~/bin/wezterm-appimage` | Installed — AppImage binary |
| `~/bin/config-system.sh` | Created — reproducible setup |
| `~/notes/wezterm.md` | Created — user guide |

## What was NOT changed

| File | Notes |
|------|-------|
| `~/bin/terminal` | alacritty+tmux launcher — untouched |
| `~/bin/terminal-kitty` | kitty launcher — untouched, fallback |
| `~/.tmux.conf` | untouched — used for remote SSH |
| `~/.config/alacritty/alacritty.toml` | untouched |
| `~/.config/kitty/kitty.conf` | untouched — fallback |

## Key technical decisions

1. **Font**: Fixed6x13.ttf (PCF→TTF via bitmap2ttf) + `MONOCHROME` + `NO_HINTING`
2. **Graphics**: kitty graphics protocol (`enable_kitty_graphics = true`)
3. **Leader key**: C-z (matches tmux muscle memory)
4. **Brightness**: `foreground_text_hsb.brightness = 1.2`
5. **Bold**: bright colors, not bold font face (`bold_brightens_ansi_colors`)
6. **Remote**: tmux over SSH (unchanged); wezterm SSH domains available but not configured
7. **Local multiplexing**: wezterm native panes/tabs (no tmux locally)

## Future work

- **Promote terminal-wezterm**: once stable, rename to `terminal` (move old one to `terminal-alacritty`)
- **SSH domains**: consider wezterm's native SSH multiplexing instead of tmux for remote
- **Unix domains**: enable `wezterm connect` for local session persistence (detach/reattach)
- **Remove kitty config**: once wezterm is confirmed stable long-term
