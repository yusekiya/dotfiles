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
      { "<leader>ef", "<Cmd>Neotree focus<cr>",       mode = "n", desc = "Focus on file explorer" },
      { "<leader>ec", "<Cmd>Neotree close<cr>",       mode = "n", desc = "Close file explorer" },
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
      { "<C-A-S-p>",  "<Cmd>Telescope commands<cr>",   mode = "n" },
      { "<leader>ff", "<Cmd>Telescope find_files<cr>", mode = "n" },
      { "<leader>fg", "<Cmd>Telescope live_grep<cr>",  mode = "n" },
      { "<leader>fb", "<Cmd>Telescope buffers<cr>",    mode = "n" },
      { "<leader>fh", "<Cmd>Telescope help_tags<cr>",  mode = "n" },
    },
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  -- Outline
  {
    "stevearc/aerial.nvim",
    lazy = true,
    event = "VeryLazy",
    keys = {
      { "<leader>oo", "<Cmd>AerialToggle<cr>",    mode = "n", desc = "Toggle outline window" },
      { "<leader>on", "<Cmd>AerialNavToggle<cr>", mode = "n", desc = "Toggle outline navigation window" },
      { "<leader>of", ":Telescope aerial<cr>",    mode = "n", desc = "Outline search" },
      { "<C-'>",      "<Cmd>AerialToggle<cr>",    mode = "n", desc = "Toggle outline window" },
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
      { "<leader>tt", "<Cmd>ToggleTermToggleAll<cr>",             desc = "Toggle all terminal windows" },
      { "<leader>tf", "<Cmd>ToggleTerm direction=float<cr>",      desc = "Open float terminal" },
      { "<leader>th", "<Cmd>ToggleTerm direction=horizontal<cr>", desc = "Open terminal horizontally" },
      { "<leader>tv", "<Cmd>ToggleTerm direction=vertical<cr>",   desc = "Open terminal vertically" },
      { "<leader>g",  "<Cmd>lua _Lazygit_toggle()<cr>",           desc = "Open lazygit" },
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
      { "<leader>ws", "<Cmd>SessionSave<cr>",    mode = "n", desc = "Save session" },
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
      { "<leader>xq", "<cmd>Trouble quickfix toggle<CR>",    desc = "Open trouble quickfix list" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<CR>",     desc = "Open trouble location list" },
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
  {
    "brenoprata10/nvim-highlight-colors",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      render = "virtual",
    },
  },
}
