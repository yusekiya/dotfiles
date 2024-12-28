return {
    {
        "folke/noice.nvim",
        lazy = true,
        event = "VeryLazy",
        opts = {
            -- add any options here
        },
        dependencies = {
            "MunifTanjim/nui.nvim",
        },
    },
    {
        'nvim-telescope/telescope.nvim', tag = '0.1.8',
        lazy= true,
        keys = {
            {"<leader>ff", "<Cmd>Telescope find_files<cr>", mode = "n"},
            {"<leader>fg", "<Cmd>Telescope live_grep<cr>", mode = "n"},
            {"<leader>fb", "<Cmd>Telescope buffers<cr>", mode = "n"},
            {"<leader>fh", "<Cmd>Telescope help_tags<cr>", mode = "n"},
        },
        dependencies = { 'nvim-lua/plenary.nvim' },
    },
}
