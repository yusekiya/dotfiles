return {
  -- {
  --   "GCBallesteros/jupytext.nvim",
  --   event = { "BufRead *.ipynb", "BufNewFile *.ipynb" },
  --   config = true,
  --   opts = { style = "percent" },
  -- },
  {
    "ajbucci/ipynb.nvim",
    lazy = false,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "neovim/nvim-lspconfig",
    },
    opts = {},
  },
}
