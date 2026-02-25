return {
  -- toggle comment/uncomment
  {
    "numToStr/Comment.nvim",
    opts = {
      toggler = {
        line = "gcc",
        block = "gbc",
      },
      opleader = {
        line = "gc",
        block = "gb",
      },
    },
  },
  -- expand selection area
  {
    "terryma/vim-expand-region",
    keys = {
      { "v", "<Plug>(expand_region_expand)", mode = "v", desc = "Expand region recursively" },
      { "<C-v>", "<Plug>(expand_region_shrink)", mode = "v", desc = "Shrink region recursively" },
    },
    lazy = true,
  },
  -- select/change surround parentheses
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    lazy = true,
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end,
  },
  -- new text objects
  {
    "chrisgrieser/nvim-various-textobjs",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      keymaps = {
        useDefaults = true,
      },
    },
  },
  -- cursor jump
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        search = {
          enabled = false,
        },
        char = {
          keys = { "f", "F", [";"] = ":", "," },
        },
      },
    },
    keys = {
      {
        "<Enter>",
        mode = { "n", "x", "o" },
        function()
          require("flash").jump()
        end,
        desc = "Jump with Flash",
      },
      {
        "<leader>v",
        mode = { "n", "x", "o" },
        function()
          require("flash").treesitter()
        end,
        desc = "Visual select with Flash Treesitter",
      },
      {
        "/",
        mode = "o",
        function()
          require("flash").remote()
        end,
        desc = "Search remote with Flash",
      },
      {
        "t",
        mode = { "o", "x" },
        function()
          require("flash").treesitter_search()
        end,
        desc = "Search remote with Treesitter",
      },
      {
        "<c-s>",
        mode = { "c" },
        function()
          require("flash").toggle()
        end,
        desc = "Toggle Flash Search",
      },
    },
  },
  -- auto pairs
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/nvim-cmp",
    },
    config = function()
      -- import nvim-autopairs
      local autopairs = require("nvim-autopairs")
      -- configure autopairs
      autopairs.setup({
        check_ts = true, -- enable treesitter
        ts_config = {
          lua = { "string" }, -- don't add pairs in lua string treesitter nodes
          javascript = { "template_string" }, -- don't add pairs in javascript template_string treesitter nodes
          java = false, -- don't check treesitter on java
        },
      })
      -- import nvim-autopairs completion functionality
      local cmp_autopairs = require("nvim-autopairs.completion.cmp")
      -- import nvim-cmp plugin
      local cmp = require("cmp")
      -- make autopairs and completion work together
      cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
    end,
  },
  -- substitute a word in register
  {
    "gbprod/substitute.nvim",
    lazy = true,
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local substitute = require("substitute")
      -- configure plugin
      substitute.setup()
      -- set keymaps
      local keymap = vim.keymap
      keymap.set("n", "s", substitute.operator, { desc = "Substitute with motion" })
      keymap.set("n", "ss", substitute.line, { desc = "Substitute line" })
      keymap.set("n", "S", substitute.eol, { desc = "Substitute to end of line" })
      keymap.set("x", "s", substitute.visual, { desc = "Substitute in visual mode" })
    end,
  },
  -- snippets
  {
    "L3MON4D3/LuaSnip",
    event = { "BufReadPre", "BufNewFile" },
    version = "v2.*",
    build = "make install_jsregexp",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      vim.api.nvim_create_user_command("LuaSnipEdit", ':lua require("luasnip.loaders").edit_snippet_files()', {})
      require("luasnip.loaders.from_lua").lazy_load({
        paths = { "./luasnippets" },
      })
      require("luasnip.loaders.from_vscode").lazy_load({
        exclude = { "latex", "tex" },
      })
      local ls = require("luasnip")
      ls.config.setup({
        -- Enable autotriggered snippets
        enable_autosnippets = true,
        -- Use Tab (or some other key if you prefer) to trigger visual selection
        store_selection_keys = "<Tab>",
        update_events = "TextChanged,TextChangedI",
      })
      -- Enable LaTeX snippets in Markdown
      ls.filetype_extend("markdown", { "tex" })
    end,
  },
  -- formatting
  {
    "stevearc/conform.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local conform = require("conform")
      conform.setup({
        formatters_by_ft = {
          javascript = { "prettier" },
          typescript = { "prettier" },
          javascriptreact = { "prettier" },
          typescriptreact = { "prettier" },
          css = { "prettier" },
          html = { "prettier" },
          json = { "prettier" },
          yaml = { "prettier" },
          markdown = { "prettier" },
          lua = { "stylua" },
          python = { "ruff_fix", "ruff_format", "ruff_organize_imports" },
        },
        format_on_save = {
          lsp_fallback = true,
          async = false,
          timeout_ms = 1000,
        },
      })
      vim.keymap.set({ "n", "v" }, "<leader>mp", function()
        conform.format({
          lsp_fallback = true,
          async = false,
          timeout_ms = 1000,
        })
      end, { desc = "Format file or range (in visual mode)" })
    end,
  },
}
