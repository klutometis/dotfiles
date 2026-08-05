local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

-- ── Window title from $WEZTERM_TITLE ────────────────────────
-- The terminal-wezterm launcher sets WEZTERM_TITLE="terminal (session)"
wezterm.on('format-window-title', function(tab, pane, tabs, panes, config)
  local title = os.getenv('WEZTERM_TITLE')
  if title then
    return title
  end
  return tab.active_pane.title
end)

-- ── Font ────────────────────────────────────────────────────
-- Fixed6x13 Sharp: the 6x13 PCFs reconverted by ~/bin/bitmap2ttf-sharp with
-- upem 1300 (exactly 100 font units per pixel), gasp=GRIDFIT, integer-ppem
-- forced, and glyphs emitted as exact rectangles instead of traced outlines.
--
-- The old Fixed6x13.ttf needed MONOCHROME|NO_HINTING to look tolerable: it
-- had upem 999 (not divisible by 13, so no edge could land on a pixel) and a
-- gasp table demanding DOGRAY. MONOCHROME papered over that by discarding the
-- greys after the fact, which also threw away any chance of correct stem
-- placement — hence NO_HINTING to stop FreeType making it worse.
--
-- None of that is needed now. DEFAULT load flags render this correctly,
-- because the outlines are already on the pixel grid.
--
-- 9.75pt at 96 DPI = exactly 13px, the font's native size.
config.font = wezterm.font('Fixed6x13 Sharp')
config.font_size = 9.75
config.freetype_load_flags = 'DEFAULT'
config.freetype_load_target = 'Normal'

-- Disable font shaping/ligatures (bitmap font, no ligatures)
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- Brightness boost was compensating for antialiased stems reading as grey.
-- A grid-fitted font puts full-intensity pixels down, so 1.0 is correct now;
-- anything higher blows out the colours.
config.foreground_text_hsb = {
  hue = 1.0,
  saturation = 1.0,
  brightness = 1.0,
}

-- ── Colors (from ~/.config/alacritty/alacritty.toml) ────────
config.colors = {
  foreground = '#ffffff',
  background = '#000000',
  ansi = {
    '#000000', -- black
    '#ff7c4d', -- red
    '#22ff00', -- green
    '#ffcc00', -- yellow
    '#3399ff', -- blue
    '#ff61df', -- magenta
    '#00ffff', -- cyan
    '#888888', -- white
  },
  brights = {
    '#000000', -- bright black
    '#ff7c4d', -- bright red
    '#22ff00', -- bright green
    '#ffcc00', -- bright yellow
    '#3399ff', -- bright blue
    '#ff61df', -- bright magenta
    '#00ffff', -- bright cyan
    '#ffffff', -- bright white
  },
}

-- Bold uses bright colors AND bold font (like alacritty's draw_bold_text_with_bright_colors)
config.bold_brightens_ansi_colors = 'BrightAndBold'

-- ── Cursor ──────────────────────────────────────────────────
config.default_cursor_style = 'SteadyBar'

-- ── Bell ────────────────────────────────────────────────────
config.audible_bell = 'Disabled'

-- ── Tab bar ─────────────────────────────────────────────────
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false

-- ── Window ──────────────────────────────────────────────────
config.window_close_confirmation = 'NeverPrompt'
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

-- ── Graphics protocol ───────────────────────────────────────
-- Enables kitty graphics protocol (used by kitty-graphics.el in emacs)
config.enable_kitty_graphics = true

-- ── Scrollback ──────────────────────────────────────────────
config.scrollback_lines = 10000

-- ── Leader key: C-z (like tmux / our kitty setup) ──────────
config.leader = { key = 'z', mods = 'CTRL', timeout_milliseconds = 2000 }

config.keys = {
  -- Panes (splits)
  { key = '|', mods = 'LEADER|SHIFT', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
  { key = '-', mods = 'LEADER',       action = act.SplitVertical   { domain = 'CurrentPaneDomain' } },
  { key = 'h', mods = 'LEADER',       action = act.ActivatePaneDirection 'Left' },
  { key = 'j', mods = 'LEADER',       action = act.ActivatePaneDirection 'Down' },
  { key = 'k', mods = 'LEADER',       action = act.ActivatePaneDirection 'Up' },
  { key = 'l', mods = 'LEADER',       action = act.ActivatePaneDirection 'Right' },
  { key = 'z', mods = 'LEADER',       action = act.TogglePaneZoomState },

  -- Tabs
  { key = 'c', mods = 'LEADER',       action = act.SpawnTab 'CurrentPaneDomain' },
  { key = 'n', mods = 'LEADER',       action = act.ActivateTabRelative(1) },
  { key = 'p', mods = 'LEADER',       action = act.ActivateTabRelative(-1) },

  -- Send C-z through to the application (press C-z twice)
  { key = 'z', mods = 'LEADER|CTRL',  action = act.SendKey { key = 'z', mods = 'CTRL' } },

  -- Font size
  { key = '=', mods = 'CTRL',         action = act.IncreaseFontSize },
  { key = '-', mods = 'CTRL',         action = act.DecreaseFontSize },
  { key = '0', mods = 'CTRL',         action = act.ResetFontSize },
}

-- ── SSH domains (remote work — alternative to tmux over ssh) ─
-- Uncomment and customize to use wezterm's native SSH multiplexing:
-- config.ssh_domains = {
--   {
--     name = 'devbox',
--     remote_address = 'devbox.example.com',
--     multiplexing = 'None',  -- use 'WezTerm' for wezterm mux on remote
--     assume_shell = 'Posix',
--   },
-- }

return config
