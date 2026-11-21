-- Border shared by small popups (diagnostics, hover, completion, gitsigns preview).
-- Like border = "solid" (one blank cell of padding) but without the top row.
-- Order: top-left, top, top-right, right, bottom-right, bottom, bottom-left, left.
-- The noice hover view takes padding instead of border chars; keep it in sync
-- (lua/plugins/ui.lua).
return { "", "", "", " ", " ", " ", " ", " " }
