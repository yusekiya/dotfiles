return {
  {
    "GeorgesAlkhouri/nvim-aider",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim",
      "MunifTanjim/nui.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    cmd = { "Aider", "AiderEdit", "AiderChatToggle", "AiderResetChat" },
    keys = {
      { "<leader>aa", "<cmd>Aider<cr>", desc = "Start Aider" },
      { "<leader>ae", "<cmd>AiderEdit<cr>", desc = "Edit with Aider" },
      { "<leader>at", "<cmd>AiderChatToggle<cr>", desc = "Toggle Aider chat" },
      { "<leader>ar", "<cmd>AiderResetChat<cr>", desc = "Reset Aider chat" },
    },
    opts = {
      -- API provider configuration
      provider = {
        -- Options: anthropic, openai, ollama, etc.
        name = "anthropic",
        -- Model to use (depends on provider)
        model = "claude-3-opus-20240229",
        -- API key (can also be set via environment variable)
        -- api_key = "your_api_key_here", -- or use ANTHROPIC_API_KEY env var
      },
      -- UI configuration
      ui = {
        -- Width of chat window (can be number or function)
        width = function()
          return math.floor(vim.o.columns * 0.4)
        end,
        -- Border style for floating windows
        border = "rounded",
      },
      -- Additional options
      auto_focus_chat = true,
      -- File patterns to include/exclude
      file_patterns = {
        include = { "**/*" },
        exclude = { ".git/", "node_modules/" },
      },
    },
  },
}
