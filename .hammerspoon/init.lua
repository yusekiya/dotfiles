-- Configurations
-- PaperWM
PaperWM = hs.loadSpoon("PaperWM")
-- Ctrl + Alt + GUI key
cag_key = {"ctrl", "alt", "cmd"}
-- Ctrl + Shift + Alt + GUI key
hypr_key = {"ctrl", "shift", "alt", "cmd"}
PaperWM:bindHotkeys({
    -- switch to a new focused window in tiled grid
    focus_left  = {cag_key, "h"},
    focus_down  = {cag_key, "j"},
    focus_up    = {cag_key, "k"},
    focus_right = {cag_key, "l"},

    -- switch windows by cycling forward/backward
    -- (forward = down or right, backward = up or left)
    -- focus_prev = {{"alt", "cmd"}, "k"},
    -- focus_next = {{"alt", "cmd"}, "j"},

    -- move windows around in tiled grid
    swap_left  = {hypr_key, "h"},
    swap_down  = {hypr_key, "j"},
    swap_up    = {hypr_key, "k"},
    swap_right = {hypr_key, "l"},

    -- alternative: swap entire columns, rather than
    -- individual windows (to be used instead of
    -- swap_left / swap_right bindings)
    swap_column_left  = {hypr_key, "left"},
    swap_column_right = {hypr_key, "right"},

    -- position and resize focused window
    center_window        = {cag_key, "c"},
    full_width           = {cag_key, "f"},
    cycle_width          = {cag_key, "r"},
    -- reverse_cycle_width  = {{"ctrl", "alt", "cmd"}, "r"},
    cycle_height         = {cag_key, "t"},
    -- reverse_cycle_height = {{"ctrl", "alt", "cmd", "shift"}, "r"},

    -- increase/decrease width
    increase_width = {cag_key, "]"},
    decrease_width = {cag_key, "["},

    -- increase/decrease height
    increase_height = {cag_key, "="},
    decrease_height = {cag_key, "-"},

    -- move focused window into / out of a column
    slurp_in = {hypr_key, "i"},
    barf_out = {hypr_key, "o"},

    -- move the focused window into / out of the tiling layer
    toggle_floating = {hypr_key, "f"},

    -- focus the first / second / etc window in the current space
    -- focus_window_1 = {cag_key, "1"},
    -- focus_window_2 = {cag_key, "2"},
    -- focus_window_3 = {cag_key, "3"},
    -- focus_window_4 = {cag_key, "4"},
    -- focus_window_5 = {cag_key, "5"},
    -- focus_window_6 = {cag_key, "6"},
    -- focus_window_7 = {cag_key, "7"},
    -- focus_window_8 = {cag_key, "8"},
    -- focus_window_9 = {cag_key, "9"},

    -- switch to a new Mission Control space
    switch_space_l = {cag_key, "up"},
    switch_space_r = {cag_key, "down"},
    switch_space_1 = {cag_key, "1"},
    switch_space_2 = {cag_key, "2"},
    switch_space_3 = {cag_key, "3"},
    switch_space_4 = {cag_key, "4"},
    switch_space_5 = {cag_key, "5"},
    switch_space_6 = {cag_key, "6"},
    switch_space_7 = {cag_key, "7"},
    switch_space_8 = {cag_key, "8"},
    switch_space_9 = {cag_key, "9"},

    -- move focused window to a new space and tile
    move_window_1 = {hypr_key, "1"},
    move_window_2 = {hypr_key, "2"},
    move_window_3 = {hypr_key, "3"},
    move_window_4 = {hypr_key, "4"},
    move_window_5 = {hypr_key, "5"},
    move_window_6 = {hypr_key, "6"},
    move_window_7 = {hypr_key, "7"},
    move_window_8 = {hypr_key, "8"},
    move_window_9 = {hypr_key, "9"}
})
PaperWM.window_gap = 10
PaperWM.swipe_fingers = 3
PaperWM.drag_window = {"alt", "cmd"}
PaperWM.lift_window = {"alt", "shift", "cmd"}
PaperWM.window_ratios = { 1/2, 1/3, 2/3 }

-- WarpMouse
WarpMouse = hs.loadSpoon("WarpMouse")

-- Start plugins
PaperWM:start()
WarpMouse:start()
