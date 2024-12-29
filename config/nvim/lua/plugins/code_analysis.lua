return {
    {
        "nvim-treesitter/nvim-treesitter",
        event = { "BufRead", "BufNewFile", "InsertEnter" },
        build = ":TSUpdate",
        main = 'nvim-treesitter.configs',
        opts = {
            ensure_installed = {
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
            },
            sync_install = false,
            auto_install = true,
            highlight = { enable = true },
            indent = { enable = true },
        },
    },
}
