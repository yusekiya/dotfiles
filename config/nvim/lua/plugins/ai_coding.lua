return {
  {
    "sourcegraph/sg.nvim",
    event = "LspAttach",
    keys = {
      { "<leader>//", "<Cmd>CodyToggle<cr>", mode = "n", desc = "Toggle cody chat window" },
      { "<leader>ac", "<Cmd>CodyChat<cr>", mode = "n", desc = "Start a new chat" },
      { "<leader>ar", "<Cmd>CodyRestart<cr>", mode = "n", desc = "Restart cody" },
      { "<leader>aa", "<Cmd>'<,'>CodyAsk<cr>", mode = "x", desc = "Ask a question" },
      { "<leader>ae", "<Cmd>'<,'>CodyExplain<cr>", mode = "x", desc = "Explain current selection" },
      { "<leader>at", "<Cmd>'<,'>CodyTask<cr>", mode = "x", desc = "Instruct cody to perform a task" },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("sg").setup({
        -- enable_cody = false,
      })
    end,
  },
}
