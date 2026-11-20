return {
  {
    "lervag/vimtex",
    lazy = false,
    tag = "v2.16",
    init = function()
      if vim.fn.executable("zathura") and vim.fn.has("mac") == 0 then
        -- Disable zathura by default on mac because it required additional config to work
        vim.g.vimtex_view_method = "zathura"
      elseif vim.fn.executable("skimpdf") then
        vim.g.vimtex_view_method = "skim"
      end
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_quickfix_mode = 0
      vim.g.vimtex_toc_config = { split_pos = "vert rightbelow" }

      vim.api.nvim_create_autocmd("User", {
        pattern = "VimtexEventTocCreated",
        group = vim.api.nvim_create_augroup("vimtex_toc_keymaps", { clear = true }),
        callback = function()
          local opts = { buffer = 0, nowait = true, silent = true }
          pcall(vim.keymap.del, "n", "<Space>", { buffer = 0 })
          vim.keymap.set("n", "<CR>", "<Cmd>call b:toc.activate_current(0)<CR>", opts)
          vim.keymap.set("n", "o", "<Cmd>call b:toc.activate_current(1)<CR>", opts)
        end,
      })
    end,
  },
  -- {
  --   "let-def/texpresso.vim",
  --   ft = "tex",
  --   config = function()
  --     require("texpresso").attach()
  --   end,
  -- },
}
