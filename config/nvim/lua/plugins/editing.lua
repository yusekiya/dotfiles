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
    -- auto pairs
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        dependencies = {
            "hrsh7th/nvim-cmp",
        },
        config = function()
            -- import nvim-autopairs
            local autopairs = require("nvim-autopairs")
            -- configure autopairs
            autopairs.setup({
                check_ts = true, -- enable treesitter
                ts_config = {
                    lua = { "string" }, -- don't add pairs in lua string treesitter nodes
                    javascript = { "template_string" }, -- don't add pairs in javascript template_string treesitter nodes
                    java = false, -- don't check treesitter on java
                },
            })
            -- import nvim-autopairs completion functionality
            local cmp_autopairs = require("nvim-autopairs.completion.cmp")
            -- import nvim-cmp plugin
            local cmp = require("cmp")
            -- make autopairs and completion work together
            cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())
        end,
    },
}
