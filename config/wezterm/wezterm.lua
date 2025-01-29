local wezterm = require("wezterm")
local act = wezterm.action

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider
-- The left separator in tab bar
local LEFT_SEP = wezterm.nerdfonts.ple_upper_right_triangle
-- The right separator in tab bar
local RIGHT_SEP = wezterm.nerdfonts.ple_lower_left_triangle

-- Background colors in status bar from right to left
local BG_COLORS = {
	"#88C0D0",
	"#81A1C1",
	"#5E81AC",
}
-- Text color in status bar
local TEXT_COLOR = "#333333"

-- This function returns the suggested title for a tab.
-- It prefers the title that was set via `tab:set_title()`
-- or `wezterm cli set-tab-title`, but falls back to the
-- title of the active pane in that tab.
function tab_title(tab_info)
	local title = tab_info.tab_title
	-- if the tab title is explicitly set, take that
	if title and #title > 0 then
		return title
	end
	-- Otherwise, use the title from the active pane
	-- in that tab
	return tab_info.active_pane.title
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local edge_background = "#333333"
	local background = "#434C5E"
	local foreground = "#D8DEE9"

	if tab.is_active then
		background = "#128BC7"
		foreground = "#E6E6E6"
	elseif hover then
		background = (wezterm.color.parse("#5E81AC")):darken(0.35)
		foreground = "#ECEFF4"
	end

	local edge_foreground = background

	local title = tab_title(tab)

	-- ensure that the titles fit in the available space,
	-- and that we have room for the edges.
	title = wezterm.truncate_right(title, max_width - 2)

	return {
		{ Background = { Color = edge_background } },
		{ Foreground = { Color = edge_foreground } },
		{ Text = LEFT_SEP },
		{ Background = { Color = background } },
		{ Foreground = { Color = foreground } },
		{ Attribute = { Intensity = "Bold" } },
		{ Text = "  " .. title .. "  " },
		{ Background = { Color = edge_background } },
		{ Foreground = { Color = edge_foreground } },
		{ Text = RIGHT_SEP },
	}
end)

-- cf. https://wezfurlong.org/wezterm/config/lua/window/set_right_status.html
wezterm.on("update-status", function(window, pane)
	-- Table containing cells from right to left
	local cells = {}
	-- Figure out the cwd and host of the current pane.
	-- This will pick up the hostname for the remote host if your
	-- shell is using OSC 7 on the remote host.
	local cwd_uri = pane:get_current_working_dir()
	local hostname = ""
	if cwd_uri then
		if type(cwd_uri) == "userdata" then
			hostname = cwd_uri.host or pane:get_domain_name()
		else
			cwd_uri = cwd_uri:sub(8)
			local slash = cwd_uri:find("/")
			if slash then
				hostname = cwd_uri:sub(1, slash - 1)
			end
		end
	end
	local dot = hostname:find("[.]")
	if hostname:match("%.local$") then
		hostname = "local"
	elseif dot then
		hostname = hostname:sub(1, dot - 1)
	elseif hostname == "" then
		hostname = pane:get_domain_name()
	end
	table.insert(cells, hostname)
	-- Add workspace name
	table.insert(cells, window:active_workspace())

	-- Elements in status bar
	local elements = {}
	local num_cells = #cells
	-- Convert a cell into elements
	function push(text, is_last)
		table.insert(elements, { Foreground = { Color = BG_COLORS[num_cells] } })
		table.insert(elements, { Text = SOLID_LEFT_ARROW })
		table.insert(elements, { Foreground = { Color = TEXT_COLOR } })
		table.insert(elements, { Background = { Color = BG_COLORS[num_cells] } })
		table.insert(elements, { Attribute = { Intensity = "Bold" } })
		table.insert(elements, { Text = " " .. text .. " " })
		num_cells = num_cells - 1
	end
	-- Build elements
	while #cells > 0 do
		local cell = table.remove(cells)
		push(cell, #cells == 0)
	end
	window:set_right_status(wezterm.format(elements))
end)

function file_exists(name)
	local f = io.open(name, "r")
	if f ~= nil then
		io.close(f)
		return true
	else
		return false
	end
end

-- Load site local configurations
sitecfg = {}
if file_exists(wezterm.config_dir .. "/site_cfg.lua") then
	sitecfg = require("site_cfg")
end

cfg = {
	font = wezterm.font_with_fallback({
		"Fira Code",
		"Noto Sans JP",
	}),
	font_size = 12.0,
	initial_cols = 120,
	initial_rows = 40,
	tab_max_width = 100,
	line_height = 1.05,
	use_fancy_tab_bar = false,
	tab_bar_at_bottom = true,
	show_close_tab_button_in_tabs = false,
	show_new_tab_button_in_tab_bar = false,
	use_ime = true,
	color_scheme = "nordfox",
	colors = {
		scrollbar_thumb = "#4C566A",
		tab_bar = {
			background = "#333333",
			active_tab = {
				bg_color = "#128BC7",
				fg_color = "#E6E6E6",
			},
			inactive_tab = {
				bg_color = "#434C5E",
				fg_color = "#D8DEE9",
			},
			inactive_tab_hover = {
				bg_color = (wezterm.color.parse("#5E81AC")):darken(0.35),
				fg_color = "#ECEFF4",
			},
		},
	},
	window_frame = {
		-- The font used in the tab bar.
		-- Roboto Bold is the default; this font is bundled
		-- with wezterm.
		-- Whatever font is selected here, it will have the
		-- main font setting appended to it to pick up any
		-- fallback fonts you may have used there.
		-- font = wezterm.font { family = 'Roboto', weight = 'Bold' },

		-- The size of the font in the tab bar.
		-- Default to 10.0 on Windows but 12.0 on other systems
		-- font_size = 12.0,

		-- The overall background color of the tab bar when
		-- the window is focused
		active_titlebar_bg = "#333333",

		-- The overall background color of the tab bar when
		-- the window is not focused
		inactive_titlebar_bg = "#333333",
	},
	inactive_pane_hsb = {
		saturation = 0.9,
		brightness = 0.6,
	},
	window_background_opacity = 0.90,
	window_decorations = "RESIZE",
	enable_scroll_bar = false,
	window_padding = {
		left = "1.0cell",
		right = "0",
		top = "1.0cell",
		bottom = "0.5cell",
	},
	adjust_window_size_when_changing_font_size = false,
	leader = { key = "Space", mods = "SHIFT", timeout_milliseconds = 1000 },
	keys = {
		{
			key = "s",
			mods = "LEADER",
			action = wezterm.action.ActivateKeyTable({ name = "pane_control", one_shot = false }),
		},
		{ key = "w", mods = "LEADER", action = wezterm.action.ActivateKeyTable({ name = "workspace" }) },
		{ key = "p", mods = "SUPER|SHIFT", action = wezterm.action.ActivateCommandPalette },
		{ key = "c", mods = "SUPER", action = wezterm.action.ActivateCopyMode },
		{ key = "h", mods = "SUPER|CTRL", action = act.MoveTabRelative(-1) },
		{ key = "l", mods = "SUPER|CTRL", action = act.MoveTabRelative(1) },
		{
			key = "t",
			mods = "SUPER|SHIFT",
			action = act.SpawnCommandInNewTab({
				cwd = wezterm.home_dir,
			}),
		},
		{
			key = "n",
			mods = "SUPER|SHIFT",
			action = act.SpawnCommandInNewWindow({
				cwd = wezterm.home_dir,
			}),
		},
		{ key = "UpArrow", mods = "SHIFT", action = act.ScrollToPrompt(-1) },
		{ key = "DownArrow", mods = "SHIFT", action = act.ScrollToPrompt(1) },
		{
			key = "0",
			mods = "LEADER",
			action = act.PaneSelect({
				alphabet = "asdfjkl",
			}),
		},
		{
			key = "z",
			mods = "LEADER",
			action = wezterm.action.TogglePaneZoomState,
		},
		{
			key = "p",
			mods = "SUPER",
			action = wezterm.action_callback(function(window, pane)
				local is_nvim = pane:get_foreground_process_name():match(".*/([^/]+)$") == "nvim"
				if not is_nvim then
					return
				end
				window:perform_action({ SendKey = { key = "p", mods = "CTRL|ALT|SHIFT" } }, pane)
			end),
		},
	},
	key_tables = {
		pane_control = {
			{ key = "-", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
			{ key = "|", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
			{ key = "h", action = act.ActivatePaneDirection("Left") },
			{ key = "j", action = act.ActivatePaneDirection("Down") },
			{ key = "k", action = act.ActivatePaneDirection("Up") },
			{ key = "l", action = act.ActivatePaneDirection("Right") },
			{ key = "H", action = act.AdjustPaneSize({ "Left", 5 }) },
			{ key = "J", action = act.AdjustPaneSize({ "Down", 2 }) },
			{ key = "K", action = act.AdjustPaneSize({ "Up", 2 }) },
			{ key = "L", action = act.AdjustPaneSize({ "Right", 5 }) },
			{ key = "Escape", action = "PopKeyTable" },
			{ key = "Enter", action = "PopKeyTable" },
		},
		workspace = {
			{ key = "l", action = act.ShowLauncherArgs({ flags = "WORKSPACES", title = "Select workspace" }) },
			{
				key = "r",
				action = act.PromptInputLine({
					description = "(wezterm) Set workspace title:",
					action = wezterm.action_callback(function(win, pane, line)
						if line then
							wezterm.mux.rename_workspace(wezterm.mux.get_active_workspace(), line)
						end
					end),
				}),
			},
			{
				key = "n",
				action = act.PromptInputLine({
					description = "(wezterm) Create new workspace:",
					action = wezterm.action_callback(function(window, pane, line)
						if line then
							window:perform_action(
								act.SwitchToWorkspace({
									name = line,
								}),
								pane
							)
						end
					end),
				}),
			},
		},
	},
}

for k, v in pairs(sitecfg) do
	cfg[k] = v
end
return cfg
