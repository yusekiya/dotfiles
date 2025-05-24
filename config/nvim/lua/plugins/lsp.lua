return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "folke/snacks.nvim",
    },
    config = function()
      -- local lspconfig = require("lspconfig")
      local keymap = vim.keymap

      vim.diagnostic.config({ jump = { float = true } })
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", {}),
        callback = function(ev)
          -- Buffer local mappings
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf, silent = true }
          local opts_nowait = { nowait = true, buffer = ev.buf, silent = true }

          -- keymaps
          -- show definition, references
          opts_nowait.desc = "Show LSP references"
          keymap.set("n", "gr", function() Snacks.picker.lsp_references() end, opts_nowait)

          -- go to declaration
          opts.desc = "Go to declaration"
          keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

          -- show lsp definitions
          opts.desc = "Show LSP definitions"
          keymap.set("n", "gd", function() Snacks.picker.lsp_definitions() end, opts)

          -- show lsp implementations
          opts.desc = "Show LSP implementations"
          keymap.set("n", "gi", function() Snacks.picker.lsp_implementations() end, opts)

          -- show lsp type definitions
          opts.desc = "Show LSP type definitions"
          keymap.set("n", "gt", function() Snacks.picker.lsp_type_definitions() end, opts)

          -- see available code actions, in visual mode will apply to selection
          opts.desc = "See available code actions"
          keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts)

          -- smart rename
          opts.desc = "Smart rename"
          keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

          -- symbols
          opts.desc = "LSP Symbols"
          keymap.set("n", "<leader>ss", function() Snacks.picker.lsp_symbols() end, opts)

          -- workspace symbols
          opts.desc = "LSP Workspace Symbols"
          keymap.set("n", "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end, opts)

          -- show  diagnostics for file
          opts.desc = "Show buffer diagnostics"
          keymap.set("n", "<leader>D", function() Snacks.picker.diagnostics_buffer() end, opts)

          -- show diagnostics for line
          opts.desc = "Show line diagnostics"
          keymap.set("n", "<leader>d", vim.diagnostic.open_float, opts)

          -- show documentation for what is under cursor
          opts.desc = "Show documentation for what is under cursor"
          keymap.set("n", "K", vim.lsp.buf.hover, opts)

          -- mapping to restart lsp if necessary
          opts.desc = "Restart LSP"
          keymap.set("n", "<leader>rs", ":LspRestart<CR>", opts)
        end,
      })

      -- Change the Diagnostic symbols in the sign column (gutter)
      local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
      end

      vim.lsp.enable({ "harper_ls", "texlab", "lua_ls", "ruff", "pyright" })
    end,
  },
}
