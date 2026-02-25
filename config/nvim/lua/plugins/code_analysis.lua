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
        "c",
        "comment",
        "css",
        "csv",
        "diff",
        "html",
        "javascript",
        "json",
        "latex",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "regex",
        "rust",
        "ssh_config",
        "tmux",
        "toml",
        "vim",
        "vimdoc",
        "xml",
        "yaml",
        "zsh",
      })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "lua",
          "python",
          "html",
          "css",
          "javascript",
          "typescript",
          "c",
          "cpp",
          "rust",
          "go",
          "sh",
          "zsh",
          "yaml",
          "json",
          "markdown",
          "vim",
          "tex",
        },
        callback = function()
          vim.treesitter.start()
        end,
      })
    end,
  },
  -- sticky line
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = { "BufNewFile", "BufReadPre" },
  },
}
