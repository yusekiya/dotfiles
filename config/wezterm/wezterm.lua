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
      'Fira Code',
      'Noto Sans JP',
  },
  font_size = 13.1,
  use_ime = true,
  color_schemes = {
      ['myscheme'] = scheme,
  },
  color_scheme = 'myscheme',
  -- window_background_opacity = 0.9,
  window_decorations = "RESIZE",
  enable_scroll_bar = true,
  leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 1000 },
  keys = {
    { key = 'h', mods = 'SUPER|CTRL', action = act.MoveTabRelative(-1)},
    { key = 'l', mods = 'SUPER|CTRL', action = act.MoveTabRelative(1)},
    {
        key = '-',
        mods = 'LEADER',
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
        key = '|',
        mods = 'LEADER',
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
        key = 'h',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Left',
    },
    {
        key = 'j',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Down',
    },
    {
        key = 'k',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Up',
    },
    {
        key = 'l',
        mods = 'LEADER',
        action = act.ActivatePaneDirection 'Right',
    },
    {
        key = '0',
        mods = 'LEADER',
        action = act.PaneSelect {
            alphabet = 'asdfjkl'
        },
    },
    {
      key = 'H',
      mods = 'LEADER',
      action = act.AdjustPaneSize { 'Left', 5 },
    },
    {
      key = 'J',
      mods = 'LEADER',
      action = act.AdjustPaneSize { 'Down', 5 },
    },
    { key = 'K', mods = 'LEADER', action = act.AdjustPaneSize { 'Up', 5 } },
    {
      key = 'L',
      mods = 'LEADER',
      action = act.AdjustPaneSize { 'Right', 5 },
    },
  },
}
