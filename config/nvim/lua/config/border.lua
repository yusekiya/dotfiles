-- Border shared by small popups (diagnostics, hover, completion, gitsigns preview).
-- A vertical bar on the left marks the popup; one blank cell on the right and bottom; no top row.
-- Order: top-left, top, top-right, right, bottom-right, bottom, bottom-left, left.
-- Each char must be one cell wide (nvim_open_win rejects "┃ "), so the left bar is
-- U+258E LEFT ONE QUARTER BLOCK: it sits at the cell's left edge and leaves the rest
-- of the cell blank as the gap before the text.
-- The noice hover view (K, signature help) uses these chars too, through nui (lua/plugins/ui.lua).
return { "", "", "", " ", " ", " ", "▎", "▎" }
