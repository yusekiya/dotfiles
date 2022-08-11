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
    local zoomed = ''
    if tab.active_pane.is_zoomed then
        zoomed = 'Z | '
    end

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
      { Text = zoomed .. title },
      { Background = { Color = edge_background } },
      { Foreground = { Color = edge_foreground } },
      { Text = SOLID_RIGHT_ARROW },
    }
  end
)

local function update_ssh_status(window, pane)
    local foreground = '#8FBCBB'
	local text = pane:get_domain_name()
	if text == "local" then
		text = ""
	end
	return {
        { Foreground = { Color = foreground } },
		{ Text = text .. "  " },
	}
end

wezterm.on("update-right-status", function(window, pane)
	local ssh = update_ssh_status(window, pane)
	window:set_right_status(wezterm.format(ssh))
end)

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

function merge_all(...)
  local ret = {}
  for _, tbl in ipairs({...}) do
    for k, v in pairs(tbl) do
      ret[k] = v
    end
  end
  return ret
end


-- Load site local configurations
local sitecfg = {}
if file_exists(wezterm.config_dir .. '/site_cfg.lua') then
    sitecfg = require "site_cfg"
end

local cfg =  {
  font = wezterm.font_with_fallback {
      'Fira Code',
      'Noto Sans JP',
  },
  font_size = 13.0,
  initial_cols = 120,
  initial_rows = 40,
  tab_max_width = 20,
  use_ime = true,
  color_schemes = {
      ['myscheme'] = scheme,
  },
  color_scheme = 'myscheme',
  window_background_opacity = 0.90,
  window_decorations = "RESIZE",
  enable_scroll_bar = true,
  leader = { key = 'q', mods = 'CTRL', timeout_milliseconds = 1000 },
  keys = {
    { key = 'h', mods = 'SUPER|CTRL', action = act.MoveTabRelative(-1)},
    { key = 'l', mods = 'SUPER|CTRL', action = act.MoveTabRelative(1)},
    { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollToPrompt(-1) },
    { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollToPrompt(1) },
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
    {
      key = 'z',
      mods = 'LEADER',
      action = wezterm.action.TogglePaneZoomState,
    },
  },
}

return merge_all(cfg, sitecfg)
