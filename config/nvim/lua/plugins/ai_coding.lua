return {
  {
    "sourcegraph/sg.nvim",
    event = "LspAttach",
    keys = {
      { "<leader>//", "<Cmd>CodyToggle<cr>", mode = "n", desc = "Toggle cody chat window" },
      { "<leader>/c", "<Cmd>CodyChat<cr>", mode = "n", desc = "Start a new chat" },
      { "<leader>/r", "<Cmd>CodyRestart<cr>", mode = "n", desc = "Restart cody" },
      { "<leader>/a", "<Cmd>'<,'>CodyAsk<cr>", mode = "x", desc = "Ask a question" },
      { "<leader>/e", "<Cmd>'<,'>CodyExplain<cr>", mode = "x", desc = "Explain current selection" },
      { "<leader>/t", "<Cmd>'<,'>CodyTask<cr>", mode = "x", desc = "Instruct cody to perform a task" },
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
