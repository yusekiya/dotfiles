return {
    -- toglle comment/uncomment
    {
        "numToStr/Comment.nvim",
        opts = {
            toggler = {
                line = "<leader>cc",
                block = "<leader>bc",
            },
            opleader = {
                line = "<leader>c",
                block = "<leader>b",
            },
        }
    },
    -- expand selection area
    {
        "terryma/vim-expand-region",
        keys = {
            { "v", "<Plug>(expand_region_expand)", mode = "v", desc = "Expand region recursively"},
            { "<C-v>", "<Plug>(expand_region_shrink)", mode = "v", desc = "Shrink region recursively"},
        },
        lazy = true,
    },
    -- select/change surround parentheses
    {
        "kylechui/nvim-surround",
        version = "*", -- Use for stability; omit to use `main` branch for the latest features
        lazy = true,
        event = "VeryLazy",
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    },
    -- sneak move
    {
        "ggandor/leap.nvim",
        keys = {
            { "s", mode = { "n", "x", "o" }, desc = "Leap Forward to" },
            { "S", mode = { "n", "x", "o" }, desc = "Leap Backward to" },
            { "gs", mode = { "n", "x", "o" }, desc = "Leap from Windows" },
        },
        config = function()
            require("leap").add_default_mappings(true)
        end
    },
}
