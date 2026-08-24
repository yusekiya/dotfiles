return {
  {
    "GCBallesteros/jupytext.nvim",
    event = { "BufRead *.ipynb", "BufNewFile *.ipynb" },
    config = true,
    opts = { style = "percent" },
  },
  {
    "ajbucci/ipynb.nvim",
    event = { "BufRead *.ipynb", "BufNewFile *.ipynb" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "neovim/nvim-lspconfig",
    },
    opts = {},
  },
}
