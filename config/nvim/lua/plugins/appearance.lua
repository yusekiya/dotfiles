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
  -- highlight indent
  {
    "nvimdev/indentmini.nvim",
    config = function()
      vim.cmd.highlight("IndentLine guifg=#697180")
      vim.cmd.highlight("IndentLineCurrent guifg=#B48EAD")
      require("indentmini").setup()
    end,
  },
  -- greeter
  {
    "nvimdev/dashboard-nvim",
    event = "VimEnter",
    config = function()
      local cmd = "cat"
      if vim.fn.executable("tte") == 1 then
        local cmd_table = {
          "tte",
          " --anchor-canvas s",
          " beams --beam-delay 2 --beam-row-speed-range 20-60 --beam-column-speed-range 8-12",
          " --final-gradient-direction diagonal",
          " <",
        }
        cmd = table.concat(cmd_table)
      end
      require("dashboard").setup({
        theme = "doom",
        preview = {
          command = cmd,
          file_path = vim.fn.stdpath("config") .. "/logo/saturn.txt",
          file_width = 66,
          file_height = 23,
        },
        config = {
          center = {
            {
              icon = "  > ",
              icon_hl = "Keyword",
              desc = "New file",
              desc_hl = "String",
              key = "e",
              key_hl = "Number",
              key_format = " %s", -- `%s` will be substituted with value of `key`
              action = "ene",
            },
            {
              icon = "  > ",
              icon_hl = "Keyword",
              desc = "Toggle file explorer",
              desc_hl = "String",
              key = "SPC ee",
              key_hl = "Number",
              key_format = " %s", -- `%s` will be substituted with value of `key`
              action = "Neotree",
            },
            {
              icon = "󰱼  > ",
              icon_hl = "Keyword",
              desc = "Find file",
              desc_hl = "String",
              key = "SPC ff",
              key_hl = "Number",
              key_format = " %s", -- `%s` will be substituted with value of `key`
              action = "Telescope find_files",
            },
            {
              icon = "󰅙  > ",
              icon_hl = "Keyword",
              desc = "Quit NVIM",
              desc_hl = "String",
              key = "q",
              key_hl = "Number",
              key_format = " %s", -- `%s` will be substituted with value of `key`
              action = "qa",
            },
          },
          footer = {},
        },
      })
    end,
    dependencies = { { "nvim-tree/nvim-web-devicons" } },
  },
}
