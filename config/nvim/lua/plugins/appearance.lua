return {
  -- the colorscheme should be available when starting Neovim
  {
    "gbprod/nord.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      require("nord").setup({
        diff = { mode = "fg" },
        search = { theme = "vscode" },
        borders = true,
        transparent = true,
        errors = { mode = "none" },
        styles = {
          comments = { fg = "#8891A2" },
          keywords = {},
          functions = {},
          variables = {},
          errors = {},
          bufferline = {
            current = { bold = false },
            modified = { bold = false, italic = true },
          },
        },
        on_highlights = function(highlights, colors)
          highlights["LineNr"] = { fg = "#6A7282", bg = colors.none, bold = false }
          highlights["NeoTreeDotfile"] = { fg = "#8891A2" }
          highlights["NeoTreeIndentMarker"] = { fg = "#8891A2" }
          highlights["NeoTreeMessage"] = { fg = "#8891A2" }
          highlights["SnacksIndent"] = { fg = "#697180" }
          highlights["SnacksIndentScope"] = { fg = "#B48EAD" }
          highlights["SnacksDashboardIcon"] = { fg = "#8fbcbb" }
          highlights["SnacksDashboardDesc"] = { fg = "#d8dee9", bold = true }
          highlights["SnacksDashboardKey"] = { fg = "#8fbcbb" }
        end,
      })
      vim.cmd.colorscheme("nord")
    end,
  },
  -- status bar
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme = "nord",
      },
    },
  },
}
