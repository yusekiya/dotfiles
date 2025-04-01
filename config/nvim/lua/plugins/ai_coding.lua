return {
  {
    "GeorgesAlkhouri/nvim-aider",
    cmd = { "AiderTerminalToggle", "AiderHealth" },
    keys = {
      { "<leader>aa", "<cmd>AiderTerminalToggle<cr>", desc = "Open Aider" },
      { "<leader>as", "<cmd>AiderTerminalSend<cr>", desc = "Send to Aider", mode = { "n", "v" } },
      { "<leader>ac", "<cmd>AiderQuickSendCommand<cr>", desc = "Send Command To Aider" },
      { "<leader>ab", "<cmd>AiderQuickSendBuffer<cr>", desc = "Send Buffer To Aider" },
      { "<leader>a+", "<cmd>AiderQuickAddFile<cr>", desc = "Add File to Aider" },
      { "<leader>a-", "<cmd>AiderQuickDropFile<cr>", desc = "Drop File from Aider" },
      { "<leader>ar", "<cmd>AiderQuickReadOnlyFile<cr>", desc = "Add File as Read-Only" },
    },
    dependencies = {
      "folke/snacks.nvim",
      --- The below dependencies are optional
      "catppuccin/nvim",
      "nvim-tree/nvim-tree.lua",
      --- Neo-tree integration
      {
        "nvim-neo-tree/neo-tree.nvim",
        opts = function(_, opts)
          require("nvim_aider.neo_tree").setup(opts)
        end,
      },
    },
    config = true,
    opts = {
      args = {
        "--no-auto-commits",
        "--pretty",
        "--stream",
      },
      theme = {
        user_input_color = "#D8DEE9",
        tool_output_color = "#81A1C1",
        tool_error_color = "#BF616A",
        tool_warning_color = "#EBCB8B",
        assistant_output_color = "#B48EAD",
        completion_menu_color = "#ECEFF4",
        completion_menu_bg_color = "#24273a",
        completion_menu_current_color = "#ECEFF4",
        completion_menu_current_bg_color = "#4C566A",
      },
    },
  },
}
