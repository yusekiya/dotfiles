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
  -- Fuzzy finder
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.8",
    lazy = true,
    keys = {
      { "<C-A-S-p>", "<Cmd>Telescope commands<cr>", mode = "n" },
      { "<leader>ff", "<Cmd>Telescope find_files<cr>", mode = "n" },
      { "<leader>fg", "<Cmd>Telescope live_grep<cr>", mode = "n" },
      { "<leader>fb", "<Cmd>Telescope buffers<cr>", mode = "n" },
      { "<leader>fh", "<Cmd>Telescope help_tags<cr>", mode = "n" },
    },
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  -- Outline
  {
    "stevearc/aerial.nvim",
    lazy = true,
    event = "VeryLazy",
    keys = {
      { "<leader>oo", "<Cmd>AerialToggle<cr>", mode = "n", desc = "Toggle outline window" },
      { "<leader>on", "<Cmd>AerialNavToggle<cr>", mode = "n", desc = "Toggle outline navigation window" },
      { "<leader>of", ":Telescope aerial<cr>", mode = "n", desc = "Outline search" },
      { "<C-'>", "<Cmd>AerialToggle<cr>", mode = "n", desc = "Toggle outline window" },
    },
    opts = {},
    -- Optional dependencies
    dependencies = {
      "nvim-telescope/telescope.nvim",
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
      { "<C-w>", [[<C-\><C-n><C-w>]], mode = "t", desc = "Window manager" },
      { "jk", [[<C-\><C-n>]], mode = "t", desc = "Terminal normal model" },
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
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-buffer", -- source for text in buffer
      "hrsh7th/cmp-path", -- source for file system paths
      {
        "L3MON4D3/LuaSnip",
        version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        build = "make install_jsregexp", -- install jsregexp (optional!)
      },
      "saadparwaiz1/cmp_luasnip", -- for autocompletion
      "rafamadriz/friendly-snippets", -- useful snippets
      "onsails/lspkind.nvim", -- vscode-like pictograms
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local lspkind = require("lspkind")
      -- loads vscode style snippets
      require("luasnip.loaders.from_vscode").lazy_load()
      cmp.setup({
        completion = {
          completeopt = "menu,menuone,preview,noselect",
        },
        snippet = { -- configure how nvim-cmp interacts with snippet engine
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-p>"] = cmp.mapping.select_prev_item(), -- previous item suggestion
          ["<C-n>"] = cmp.mapping.select_next_item(), -- next item suggestion
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(), -- show completion suggestions
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }),
        }),
        -- sources for autocompletion
        sources = cmp.config.sources({
          { name = "nvim_lsp" }, -- lsp
          { name = "luasnip" }, -- snippets
          { name = "cody" }, -- AI suggestions
          { name = "buffer" }, -- text within current buffer
          { name = "path" }, -- file system paths
        }),
        -- configure lspkind for vscode-like pictograms in completion nemu
        formatting = {
          format = lspkind.cmp_format({
            maxwidth = 50,
            ellipsis_char = "...",
          }),
          expandable_indicator = true,
          fields = { "abbr", "kind", "menu" },
        },
      })
    end,
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
    ---@type AutoSession.Config
    opts = {
      enabled = false,
      suppressed_dirs = { "~/", "~/Downloads", "~/Documents", "~/Desktop" },
      pre_save_cmds = {
        "Neotree close", -- exclude neotree window
      },
    },
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 500
    end,
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
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
        map("n", "<leader>hp", gitsigns.preview_hunk, { desc = "preview git hunk" })
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
}
