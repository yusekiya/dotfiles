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
    "goolord/alpha-nvim",
    lazy = true,
    event = "VimEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local alpha = require("alpha")
      local dashboard = require("alpha.themes.dashboard")
      -- set header
      dashboard.section.header.val = {
        "                                                     ",
        "  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ",
        "  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ",
        "  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ",
        "  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ",
        "  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ",
        "  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ",
        "                                                     ",
      }
      -- set nemu
      dashboard.section.buttons.val = {
        dashboard.button("e", "  > New File", "<Cmd>ene<cr>"),
        dashboard.button("SPC ee", "  > Toggle file explorer", "<Cmd>Neotree<cr>"),
        dashboard.button("SPC ff", "󰱼  > Find file", "<Cmd>Telescope find_files<cr>"),
        dashboard.button("q", "󰅙  > Quit NVIM", "<Cmd>qa<cr>"),
      }
      -- send config to alpha
      alpha.setup(dashboard.opts)
      -- disable folding on alpha buffer
      vim.cmd([[autocmd FileType alpha setlocal nofoldenable]])
    end,
  },
}
