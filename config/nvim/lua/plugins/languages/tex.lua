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
