return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = false },
    dashboard = { enabled = false },
    explorer = { enabled = false },
    indent = { enabled = false },
    input = { enabled = false },
    picker = {
      enabled = true,
      win = {
        input = {
          keys = {
            ["<c-a>"] = false,
            ["<c-b>"] = false,
            ["<c-f>"] = false,
            ["<c-e>"] = false,
            ["<c-d>"] = { "preview_scroll_down", mode = { "i", "n" } },
            ["<c-u>"] = { "preview_scroll_up", mode = { "i", "n" } },
          }
        }
      }
    },
    notifier = { enabled = false },
    quickfile = { enabled = false },
    scope = { enabled = false },
    scroll = { enabled = false },
    statuscolumn = { enabled = false },
    words = { enabled = false },
  },
  keys = {
    -- find
    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>ff", function() Snacks.picker.smart() end,   desc = "Smart Find Files" },
    -- grep
    {
      "<leader>fg",
      function() Snacks.picker.grep({ matcher = { cwd_bonus = true } }) end,
      desc = "Grep"
    },
  }
}
