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
      { "<leader>a", nil, desc = "AI" },
      { "<leader>ac", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
      { "<leader>af", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
      { "<leader>ar", "<cmd>ClaudeCode --resume<cr>", desc = "Resume Claude" },
      { "<leader>aC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
      { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send to Claude" },
      {
        "<leader>as",
        "<cmd>ClaudeCodeTreeAdd<cr>",
        desc = "Add file",
        ft = { "NvimTree", "neo-tree" },
      },
      { "<leader>ao", "<cmd>ClaudeCodeOpen<cr>", desc = "Open Claude" },
      { "<leader>ax", "<cmd>ClaudeCodeClose<cr>", desc = "Close Claude" },
    },
  },
}
