return {
  {
    "coder/claudecode.nvim",
    dependencies = {
      "folke/snacks.nvim", -- Optional for enhanced terminal
    },
    opts = {
      -- Server options
      port_range = { min = 10000, max = 65535 },
      auto_start = true,
      log_level = "info",
      -- Terminal options
      terminal = {
        split_side = "right",
        split_width_percentage = 0.3,
        provider = "snacks", -- or "native"
      },
      -- Diff options
      diff_opts = {
        auto_close_on_accept = true,
        vertical_split = true,
      },
    },
    config = true,
    keys = {
      { "<leader>ac", "<cmd>ClaudeCode<cr>",      desc = "Toggle Claude" },
      { "<leader>as", "<cmd>ClaudeCodeSend<cr>",  mode = "v",            desc = "Send to Claude" },
      { "<leader>ao", "<cmd>ClaudeCodeOpen<cr>",  desc = "Open Claude" },
      { "<leader>ax", "<cmd>ClaudeCodeClose<cr>", desc = "Close Claude" },
    },
  }
}
