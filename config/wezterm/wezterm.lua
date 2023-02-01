local wezterm = require 'wezterm'
local act = wezterm.action

-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = utf8.char(0xe0b0)
-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = utf8.char(0xe0b2)
local SOLID_RECTANGLE = utf8.char(0x2590)

-- wezterm.on(
--   "format-tab-title",
--   function(tab, tabs, panes, config, hover, max_width)
--     local edge_background = '#333333'
--     local background = '#4C566A'
--     local foreground = '#D8DEE9'
--     local zoomed = ''
--     if tab.active_pane.is_zoomed then
--         zoomed = 'Z | '
--     end

--     if tab.is_active then
--       background = '#BF616A'
--       foreground = '#ECEFF4'
--     elseif hover then
--       background = (wezterm.color.parse '#BF616A'):darken(0.35)
--       foreground = '#ECEFF4'
--     end

--     local edge_foreground = background

--     -- ensure that the titles fit in the available space,
--     -- and that we have room for the edges.
--     local title = wezterm.truncate_right(tab.active_pane.title, max_width - 2)

--     return {
--       { Background = { Color = edge_background } },
--       { Foreground = { Color = edge_foreground } },
--       { Text = SOLID_RECTANGLE },
--       { Background = { Color = background } },
--       { Foreground = { Color = foreground } },
--       { Text = zoomed .. title },
--       { Background = { Color = edge_background } },
--       { Foreground = { Color = edge_foreground } },
--       { Text = SOLID_RIGHT_ARROW },
--     }
--   end
-- )

local function update_ssh_status(window, pane)
    -- local foreground = '#8FBCBB'
    local foreground = '#5E81AC'
    local background = '#D8DEE9'
	local text = pane:get_domain_name()
	if text == "local" then
		text = ""
        background = '#333333'
	end
	return {
        { Foreground = { Color = foreground } },
        { Background = { Color = background } },
		{ Text = "  " .. text .. "  " },
	}
end

wezterm.on("update-status", function(window, pane)
	local ssh = update_ssh_status(window, pane)
	window:set_right_status(wezterm.format(ssh))
end)

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

-- Load site local configurations
sitecfg = {}
if file_exists(wezterm.config_dir .. '/site_cfg.lua') then
    sitecfg = require "site_cfg"
end

cfg = {
  font = wezterm.font_with_fallback {
      'Fira Code',
      'Noto Sans JP',
  },
  font_size = 12.0,
  initial_cols = 120,
  initial_rows = 40,
  tab_max_width = 20,
  use_ime = true,
  color_scheme = "nordfox",
  colors = {
    scrollbar_thumb = '#4C566A',
    tab_bar = {
        background = '#333333',
        active_tab = {
            bg_color = '#BF616A',
            fg_color = '#ECEFF4',
        },
        inactive_tab = {
            bg_color = '#4C566A',
            fg_color = '#D8DEE9',
        },
        inactive_tab_hover = {
            bg_color = (wezterm.color.parse '#BF616A'):darken(0.35),
            fg_color = '#ECEFF4',
        },
    },
  },
  inactive_pane_hsb = {
    saturation = 0.9,
    brightness = 0.6,
  },
  window_background_opacity = 0.90,
  window_decorations = "RESIZE",
  enable_scroll_bar = true,
  window_padding = {
    right = '1.5cell',
  },
  adjust_window_size_when_changing_font_size = false,
  leader = { key = 'Space', mods = 'SHIFT', timeout_milliseconds = 1000 },
  keys = {
    { key = 's', mods = 'LEADER', action = wezterm.action.ActivateKeyTable { name = 'pane_control', one_shot = false, } },
    { key = 'h', mods = 'SUPER|CTRL', action = act.MoveTabRelative(-1)},
    { key = 'l', mods = 'SUPER|CTRL', action = act.MoveTabRelative(1)},
    {
        key = 't', mods = 'SUPER|SHIFT',
        action = act.SpawnCommandInNewTab {
            cwd = wezterm.home_dir,
        },
    },
    {
        key = 'n', mods = 'SUPER|SHIFT',
        action = act.SpawnCommandInNewWindow {
            cwd = wezterm.home_dir,
        },
    },
    { key = 'UpArrow', mods = 'SHIFT', action = act.ScrollToPrompt(-1) },
    { key = 'DownArrow', mods = 'SHIFT', action = act.ScrollToPrompt(1) },
    {
        key = '0',
        mods = 'LEADER',
        action = act.PaneSelect {
            alphabet = 'asdfjkl'
        },
    },
    {
      key = 'z',
      mods = 'LEADER',
      action = wezterm.action.TogglePaneZoomState,
    },
  },

  key_tables = {
    pane_control = {
      { key = '-', action = act.SplitVertical { domain = 'CurrentPaneDomain' }, },
      { key = '|', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }, },
      { key = 'h', action = act.ActivatePaneDirection 'Left' },
      { key = 'j', action = act.ActivatePaneDirection 'Down' },
      { key = 'k', action = act.ActivatePaneDirection 'Up' },
      { key = 'l', action = act.ActivatePaneDirection 'Right' },
      { key = 'H', action = act.AdjustPaneSize { 'Left', 5 } },
      { key = 'J', action = act.AdjustPaneSize { 'Down', 2 } },
      { key = 'K', action = act.AdjustPaneSize { 'Up', 2 } },
      { key = 'L', action = act.AdjustPaneSize { 'Right', 5 } },
      { key = 'Escape', action = 'PopKeyTable' },
      { key = 'Enter', action = 'PopKeyTable' },
    },
  },
}

for k,v in pairs(sitecfg) do cfg[k] = v end
return cfg
