return {
    -- the colorscheme should be available when starting Neovim
    {
        "gbprod/nord.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            require("nord").setup({
                diff = { mode = "fg" },
                search = { theme = "vscode" },
                borders = true,
                transparent = true,
                errors = { mode = "none" },
                styles = {
                    comments = { fg = "#8891A2" },
                    keywords = {},
                    functions = {},
                    variables = {},
                    errors = {},
                    bufferline = {
                        current = { bold = false },
                        modified = { bold = false, italic = true },
                    },
                },
                on_highlights = function(highlights, colors)
                    highlights["LineNr"] = { fg = "#8891A2", bg = colors.none }
                end,
            })
            vim.cmd.colorscheme("nord")
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
