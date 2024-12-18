local wezterm = require 'wezterm'
local act = wezterm.action

-- The filled in variant of the < symbol
local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
-- The filled in variant of the > symbol
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

-- Background colors in status bar from right to left
local BG_COLORS = {
    '#88C0D0',
    '#81A1C1',
    '#5E81AC',
}
-- Text color in status bar
local TEXT_COLOR = '#4C566A'

-- c.f. https://wezfurlong.org/wezterm/config/lua/window/set_right_status.html
wezterm.on("update-status", function(window, pane)
    -- Table containing cells from right to left
    local cells = {}
    -- Add workspace name
    table.insert(cells, window:active_workspace())
    -- Add domain name unless it is local
    local domain = pane:get_domain_name()
    if domain ~= "local" then
        table.insert(cells, domain)
    end
    -- Elements in status bar
    local elements = {}
    local num_cells = #cells
    -- Convert a cell into elements
    function push(text, is_last)
        table.insert(elements, { Foreground = { Color = BG_COLORS[num_cells] } })
        table.insert(elements, { Text = SOLID_LEFT_ARROW })
        table.insert(elements, { Foreground = { Color = TEXT_COLOR } })
        table.insert(elements, { Background = { Color = BG_COLORS[num_cells] } })
        table.insert(elements, { Text = ' ' ..  text .. '  ' })
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
    use_fancy_tab_bar = true,
    use_ime = true,
    color_scheme = "nordfox",
    colors = {
        scrollbar_thumb = '#4C566A',
        tab_bar = {
            background = '#333333',
            active_tab = {
                bg_color = '#5E81AC',
                fg_color = '#ECEFF4',
            },
            inactive_tab = {
                bg_color = '#434C5E',
                fg_color = '#D8DEE9',
            },
            inactive_tab_hover = {
                bg_color = (wezterm.color.parse '#5E81AC'):darken(0.35),
                fg_color = '#ECEFF4',
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
        font_size = 12.0,

        -- The overall background color of the tab bar when
        -- the window is focused
        active_titlebar_bg = '#333333',

        -- The overall background color of the tab bar when
        -- the window is not focused
        inactive_titlebar_bg = '#333333',
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
        { key = 'w', mods = 'LEADER', action = wezterm.action.ActivateKeyTable { name = 'workspace' } },
        { key = 'p', mods = 'SUPER', action = wezterm.action.ActivateCommandPalette },
        { key = 'c', mods = 'SUPER', action = wezterm.action.ActivateCopyMode },
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
        workspace = {
            { key = "l", action = act.ShowLauncherArgs { flags = 'WORKSPACES' , title = "Select workspace" } },
            { key = "r", 
                action = act.PromptInputLine {
                    description = '(wezterm) Set workspace title:',
                    action = wezterm.action_callback(function(win,pane,line)
                        if line then
                        wezterm.mux.rename_workspace(
                            wezterm.mux.get_active_workspace(),
                            line
                        )
                        end
                    end),
                }
            },
            { key = "n", 
                action = act.PromptInputLine {
                    description = "(wezterm) Create new workspace:",
                    action = wezterm.action_callback(function(window, pane, line)
                        if line then
                        window:perform_action(
                            act.SwitchToWorkspace {
                            name = line,
                            },
                            pane
                        )
                        end
                    end),
                },
            },
        },
    },
}

for k,v in pairs(sitecfg) do cfg[k] = v end
return cfg
