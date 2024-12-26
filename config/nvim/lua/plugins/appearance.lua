return {
    -- the colorscheme should be available when starting Neovim
    {
        "shaunsingh/nord.nvim",
        lazy = false, -- make sure we load this during startup if it is your main colorscheme
        priority = 1000, -- make sure to load this before all the other start plugins
        config = function()
            -- load the colorscheme here
            vim.cmd([[colorscheme nord]])
            -- config
            vim.g.nord_contrast = true
            vim.g.nord_borders = false
            vim.g.nord_disable_background = true
            vim.g.nord_cursorline_transparent = true
            vim.g.nord_enable_sidebar_background = false
            vim.g.nord_italic = false
            vim.g.nord_uniform_diff_background = true
            vim.g.nord_bold = true
            -- apply config
            require('nord').set()
        end,
    },
    -- status bar
    {
        "nvim-lualine/lualine.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        opts = {
            options = {
                theme = "nord",
            },
        },
    },
    -- highlight indent
    {
        "nvimdev/indentmini.nvim",
        config = function()
            vim.cmd.highlight("IndentLine guifg=#434C5E")
            vim.cmd.highlight("IndentLineCurrent guifg=#7A8AAB")
            require("indentmini").setup()
        end,
    },
}
