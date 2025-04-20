return {
  {
    "lervag/vimtex",
    lazy = false,
    tag = "v2.16",
    init = function()
      -- vim.g.vimtex_view_method = "zathura"
      vim.g.vimtex_quickfix_enabled = 1
      vim.g.vimtex_quickfix_mode = 0
    end,
  },
}
