return {
  -- {
  --   "GCBallesteros/jupytext.nvim",
  --   event = { "BufRead *.ipynb", "BufNewFile *.ipynb" },
  --   config = true,
  --   opts = { style = "percent" },
  -- },
  {
    "ajbucci/ipynb.nvim",
    cmd = { "NotebookCreate", "NotebookListKernels" },
    init = function()
      -- ipynb.nvim owns `BufReadCmd *.ipynb`, so no lazy event fires early
      -- enough: claim the event here and hand the buffer over once loaded.
      vim.api.nvim_create_autocmd({ "BufReadCmd", "BufNewFile" }, {
        pattern = "*.ipynb",
        group = vim.api.nvim_create_augroup("ipynb_lazy_load", { clear = true }),
        callback = function(ev)
          if ev.file:match("^%w+://") then
            return
          end
          require("lazy").load({ plugins = { "ipynb.nvim" } })
          require("ipynb.io").open_notebook(ev.buf, ev.file)
        end,
      })
    end,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "neovim/nvim-lspconfig",
    },
    opts = {},
    config = function(_, opts)
      require("ipynb").setup(opts)

      -- Recreate the kernel bridge instead of using ipynb.nvim's in-process restart.
      local function hard_restart()
        local kernel = require("ipynb.kernel")
        local state = require("ipynb.state").get()
        if not state then
          return
        end
        require("ipynb.output").clear_all_outputs(state)
        for _, cell in ipairs(state.cells) do
          cell.execution_count = nil
        end
        require("ipynb.visuals").render_all(state)
        kernel.shutdown(state)
        vim.defer_fn(function()
          kernel.connect(state, {})
        end, 500)
      end

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "ipynb",
        group = vim.api.nvim_create_augroup("ipynb_hard_restart", { clear = true }),
        callback = function(ev)
          vim.schedule(function()
            if not vim.api.nvim_buf_is_valid(ev.buf) then
              return
            end
            vim.api.nvim_buf_create_user_command(ev.buf, "NotebookKernelRestart", hard_restart, {
              desc = "Restart kernel (recreate bridge)",
            })
            vim.keymap.set("n", "<leader>k0", hard_restart, { buffer = ev.buf, desc = "Kernel restart" })
          end)
        end,
      })
    end,
  },
}
