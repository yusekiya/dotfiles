return {
  -- UI for messages, cmdline, and popupmenu
  {
    "folke/noice.nvim",
    lazy = true,
    event = "VeryLazy",
    opts = {
      -- add any options here
    },
    dependencies = {
      "MunifTanjim/nui.nvim",
      {
        "rcarriga/nvim-notify",
        opts = {
          timeout = 3000,
        },
      },
    },
  },
  -- Filer
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    lazy = true,
    cmd = "Neotree",
    keys = {
      { "<leader>ee", "<Cmd>Neotree toggle=true<cr>", mode = "n", desc = "Toggle file explorer" },
      { "<leader>ef", "<Cmd>Neotree focus<cr>", mode = "n", desc = "Focus on file explorer" },
      { "<leader>ec", "<Cmd>Neotree close<cr>", mode = "n", desc = "Close file explorer" },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
    },
    opts = {
      filesystem = {
        filtered_items = {
          visible = false,
          hide_dotfiles = true,
          hide_gitignored = true,
        },
        use_libuv_file_watcher = true,
      },
    },
  },
  -- Outline
  {
    "stevearc/aerial.nvim",
    lazy = true,
    event = "VeryLazy",
    keys = {
      { "<leader>oo", "<Cmd>AerialToggle<cr>", mode = "n", desc = "Toggle outline window" },
      { "<leader>on", "<Cmd>AerialNavToggle<cr>", mode = "n", desc = "Toggle outline navigation window" },
      {
        "<leader>fo",
        function()
          require("aerial").snacks_picker()
        end,
        mode = "n",
        desc = "Find Outline",
      },
      { "<C-'>", "<Cmd>AerialToggle!<cr>", mode = "n", desc = "Toggle outline window" },
    },
    opts = {},
    -- Optional dependencies
    dependencies = {
      "folke/snacks.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
  },
  -- terminal
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    lazy = true,
    keys = {
      { "<leader>tt", "<Cmd>ToggleTermToggleAll<cr>", desc = "Toggle all terminal windows" },
      { "<leader>tf", "<Cmd>ToggleTerm direction=float<cr>", desc = "Open float terminal" },
      { "<leader>th", "<Cmd>ToggleTerm direction=horizontal<cr>", desc = "Open terminal horizontally" },
      { "<leader>tv", "<Cmd>ToggleTerm direction=vertical<cr>", desc = "Open terminal vertically" },
      { "<leader>g", "<Cmd>lua _Lazygit_toggle()<cr>", desc = "Open lazygit" },
    },
    config = function()
      require("toggleterm").setup({
        size = function(term)
          if term.direction == "horizontal" then
            return 30
          elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
          else
            return 20
          end
        end,
        shade_terminals = false,
        highlights = {
          Normal = {
            guibg = "#2E3440",
          },
          NormalFloat = {
            link = "Normal",
          },
          FloatBorder = {
            guifg = "#8FBCBB",
          },
        },
        float_opts = {
          border = "double",
        },
      })
      -- lazygit setup
      local terminal = require("toggleterm.terminal").Terminal
      local lazygit = terminal:new({ cmd = "lazygit", direction = "float", hidden = true })
      function _Lazygit_toggle()
        lazygit:toggle()
      end
    end,
  },
  -- autocompletion
  {
    event = { "InsertEnter", "CmdLineEnter" },
    "saghen/blink.cmp",
    -- optional: provides snippets for the snippet source
    dependencies = { "rafamadriz/friendly-snippets", "L3MON4D3/LuaSnip" },
    -- use a release tag to download pre-built binaries
    version = "1.*",
    ---@module 'blink.cmp'
    opts = {
      -- See :h blink-cmp-config-keymap for defining your own keymap
      keymap = { preset = "super-tab" },
      appearance = {
        nerd_font_variant = "mono",
      },
      -- (Default) Only show the documentation popup when manually triggered
      completion = { documentation = { auto_show = false } },
      snippets = { preset = "luasnip" },
      sources = {
        default = { "lsp", "path", "snippets", "buffer" },
      },
      fuzzy = { implementation = "prefer_rust_with_warning" },
    },
    opts_extend = { "sources.default" },
  },
  -- session manager
  {
    "rmagatti/auto-session",
    lazy = false,
    keys = {
      { "<leader>wr", "<Cmd>SessionRestore<cr>", mode = "n", desc = "Restore session for cwd" },
      { "<leader>ws", "<Cmd>SessionSave<cr>", mode = "n", desc = "Save session" },
    },
    ---enables autocomplete for opts
    ---@module "auto-session"
    opts = {
      enabled = false,
      suppressed_dirs = { "~/", "~/Downloads", "~/Documents", "~/Desktop" },
      pre_save_cmds = {
        "Neotree close", -- exclude neotree window
      },
    },
  },
  -- show keymap when lazy
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 500
    end,
    opts = {
      spec = {
        -- <leader>v is mapped to visual select with treesitter
        { "<leader>f", group = "fuzzy finder" },
        { "<leader>e", group = "file explorer" },
        { "<leader>h", group = "git hunk" },
        { "<leader>o", group = "outline" },
        { "<leader>s", group = "splitted window" },
        { "<leader>t", group = "terminal" },
        { "<leader>w", group = "workspace" },
        { "<leader>x", group = "trouble" },
        { "<leader>a", group = "AI" },
      },
    },
  },
  -- interface for diagnostics, references, and other lists
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons", "folke/todo-comments.nvim" },
    opts = {
      focus = true,
    },
    cmd = "Trouble",
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<CR>", desc = "Open trouble workspace diagnostics" },
      {
        "<leader>xd",
        "<cmd>Trouble diagnostics toggle filter.buf=0<CR>",
        desc = "Open trouble document diagnostics",
      },
      { "<leader>xq", "<cmd>Trouble quickfix toggle<CR>", desc = "Open trouble quickfix list" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<CR>", desc = "Open trouble location list" },
      -- { "<leader>xt", "<cmd>Trouble todo toggle<CR>", desc = "Open todos in trouble" },
    },
  },
  -- highlight code block that is added/changed/deleted from the previous git commit
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre" },
    opts = {
      on_attach = function(bufnr)
        local gitsigns = require("gitsigns")
        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map("n", "]c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "]c", bang = true })
          else
            gitsigns.nav_hunk("next")
          end
        end, { desc = "go to next git hunk" })

        map("n", "[c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "[c", bang = true })
          else
            gitsigns.nav_hunk("prev")
          end
        end, { desc = "go to previous git hunk" })

        -- Actions
        map("n", "<leader>hs", gitsigns.stage_hunk, { desc = "stage git hunk" })
        map("n", "<leader>hr", gitsigns.reset_hunk, { desc = "reset git hunk" })
        map("v", "<leader>hs", function()
          gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "stage selected hunk" })

        map("v", "<leader>hr", function()
          gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "reset selected hunk" })
        map("n", "<leader>hS", gitsigns.stage_buffer, { desc = "stage whole buffer" })
        map("n", "<leader>hR", gitsigns.reset_buffer, { desc = "reset whole buffer" })
        map("n", "<leader>hh", gitsigns.preview_hunk, { desc = "preview git hunk" })
        map("n", "<leader>hi", gitsigns.preview_hunk_inline, { desc = "preview git hunk inline" })

        map("n", "<leader>hb", function()
          gitsigns.blame_line({ full = true })
        end, { desc = "git blame" })

        map("n", "<leader>hd", gitsigns.diffthis, { desc = "git diff with index" })

        map("n", "<leader>hD", function()
          gitsigns.diffthis("~")
        end, { desc = "git diff with previous commit" })

        map("n", "<leader>hq", gitsigns.setqflist, { desc = "populate quickfix list with hunks" })
        map("n", "<leader>hQ", function()
          gitsigns.setqflist("all")
        end, { desc = "populate quickfix list for all modified files" })
      end,
    },
  },
  -- animated glow/highlight effects to operation (undo, redo, yank, paste and more)
  {
    "y3owk1n/undo-glow.nvim",
    version = "*", -- use stable releases
    opts = {
      animation = {
        enabled = true,
        duration = 300,
        animation_type = "zoom",
        window_scoped = true,
      },
      highlights = {
        undo = {
          hl_color = { bg = "#693232" }, -- Dark muted red
        },
        redo = {
          hl_color = { bg = "#2F4640" }, -- Dark muted green
        },
        yank = {
          hl_color = { bg = "#7A683A" }, -- Dark muted yellow
        },
        paste = {
          hl_color = { bg = "#325B5B" }, -- Dark muted cyan
        },
        search = {
          hl_color = { bg = "#5C475C" }, -- Dark muted purple
        },
        -- comment = {
        --   hl_color = { bg = "#7A5A3D" }, -- Dark muted orange
        -- },
        cursor = {
          hl_color = { bg = "#7A5A3D" }, -- Dark muted orange
        },
      },
      priority = 2048 * 3,
    },
    keys = {
      {
        "u",
        function()
          require("undo-glow").undo()
        end,
        mode = "n",
        desc = "Undo with highlight",
        noremap = true,
      },
      {
        "U",
        function()
          require("undo-glow").redo()
        end,
        mode = "n",
        desc = "Redo with highlight",
        noremap = true,
      },
      {
        "p",
        function()
          require("undo-glow").paste_below()
        end,
        mode = "n",
        desc = "Paste below with highlight",
        noremap = true,
      },
      {
        "P",
        function()
          require("undo-glow").paste_above()
        end,
        mode = "n",
        desc = "Paste above with highlight",
        noremap = true,
      },
      {
        "n",
        function()
          require("undo-glow").search_next({
            animation = {
              animation_type = "strobe",
            },
          })
        end,
        mode = "n",
        desc = "Search next with highlight",
        noremap = true,
      },
      {
        "N",
        function()
          require("undo-glow").search_prev({
            animation = {
              animation_type = "strobe",
            },
          })
        end,
        mode = "n",
        desc = "Search prev with highlight",
        noremap = true,
      },
    },
    init = function()
      vim.api.nvim_create_autocmd("TextYankPost", {
        desc = "Highlight when yanking (copying) text",
        callback = function()
          require("undo-glow").yank()
        end,
      })

      -- This only handles neovim instance and do not highlight when switching panes in tmux
      vim.api.nvim_create_autocmd("CursorMoved", {
        desc = "Highlight when cursor moved significantly",
        callback = function()
          require("undo-glow").cursor_moved({
            animation = {
              animation_type = "slide",
            },
          })
        end,
      })

      -- This will handle highlights when focus gained, including switching panes in tmux
      vim.api.nvim_create_autocmd("FocusGained", {
        desc = "Highlight when focus gained",
        callback = function()
          ---@type UndoGlow.CommandOpts
          local opts = {
            animation = {
              animation_type = "slide",
            },
          }

          opts = require("undo-glow.utils").merge_command_opts("UgCursor", opts)
          local pos = require("undo-glow.utils").get_current_cursor_row()

          require("undo-glow").highlight_region(vim.tbl_extend("force", opts, {
            s_row = pos.s_row,
            s_col = pos.s_col,
            e_row = pos.e_row,
            e_col = pos.e_col,
            force_edge = opts.force_edge == nil and true or opts.force_edge,
          }))
        end,
      })

      vim.api.nvim_create_autocmd("CmdlineLeave", {
        desc = "Highlight when search cmdline leave",
        callback = function()
          require("undo-glow").search_cmd({
            animation = {
              animation_type = "fade",
            },
          })
        end,
      })
    end,
  },
  {
    "brenoprata10/nvim-highlight-colors",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      render = "virtual",
    },
  },
}
