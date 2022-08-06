local wezterm = require 'wezterm'
local act = wezterm.action

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)
-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)
local SOLID_RECTANGLE = utf8.char(0x2590)

local scheme = wezterm.color.get_builtin_schemes()['nordfox']
scheme.scrollbar_thumb = '#3B4252'

wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    local edge_background = '#333333'
    local background = '#4C566A'
    local foreground = '#D8DEE9'

    if tab.is_active then
      background = '#BF616A'
      foreground = '#ECEFF4'
    elseif hover then
      background = (wezterm.color.parse '#BF616A'):darken(0.35)
      foreground = '#ECEFF4'
    end

    local edge_foreground = background

    -- ensure that the titles fit in the available space,
    -- and that we have room for the edges.
    local title = wezterm.truncate_right(tab.active_pane.title, max_width - 2)

    return {
      { Background = { Color = edge_background } },
      { Foreground = { Color = edge_foreground } },
      { Text = SOLID_RECTANGLE },
      { Background = { Color = background } },
      { Foreground = { Color = foreground } },
      { Text = title },
      { Background = { Color = edge_background } },
      { Foreground = { Color = edge_foreground } },
      { Text = SOLID_RIGHT_ARROW },
    }
  end
)

return {
  font = wezterm.font_with_fallback {
      'Fira code',
      'Noto Sans JP',
  },
  font_size = 13.1,
  color_schemes = {
      ['myscheme'] = scheme,
  },
  color_scheme = 'myscheme',
  -- window_background_opacity = 0.9,
  window_decorations = "RESIZE",
  enable_scroll_bar = true,
  keys = {
    { key = 'h', mods = 'SUPER|CTRL', action = act.MoveTabRelative(-1)},
    { key = 'l', mods = 'SUPER|CTRL', action = act.MoveTabRelative(1)},
  },
}
