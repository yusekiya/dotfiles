return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    config = function()
      local nvim_treesitter = require("nvim-treesitter")
      nvim_treesitter.setup({})
      nvim_treesitter.install({
        "bash",
        "comment",
        "c",
        "css",
        "csv",
        "diff",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "python",
        "rust",
        "ssh_config",
        "tmux",
        "toml",
        "vim",
        "xml",
        "yaml",
        "regex",
        "vimdoc",
      })
    end,
  },
  -- sticky line
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = { "BufNewFile", "BufReadPre" },
  },
}
