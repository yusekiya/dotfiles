return {
  {
    "lervag/vimtex",
    lazy = false,
    tag = "v2.16",
    init = function()
      if vim.fn.has("mac") == 1 and vim.fn.executable("skimpdf") then
        vim.g.vimtex_view_method = "skim"
      end
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_quickfix_mode = 0
    end,
  },
  {
    "let-def/texpresso.vim",
    ft = "tex",
    config = function()
      require('texpresso').attach()
    end
  }
}
