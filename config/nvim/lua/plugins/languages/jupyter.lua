return {
  {
    "benlubas/molten-nvim",
    version = "^1.0.0", -- use version <2.0.0 to avoid breaking changes
    dependencies = { "3rd/image.nvim" },
    event = "CmdLineEnter",
    build = ":UpdateRemotePlugins",
    init = function()
      -- options
      vim.g.molten_image_provider = "image.nvim"
      vim.g.molten_output_win_max_height = 20
      vim.g.molten_auto_open_output = false
      vim.g.molten_wrap_output = true
      vim.g.molten_virt_text_output = true
      -- keymap
      vim.keymap.set("n", "<localleader>mi", ":MoltenInit<CR>", { silent = true, desc = "Initialize Molten plugin" })
      local keymap = function(mode, key, cmd, desc)
        vim.keymap.set(mode, key, cmd, { desc = desc, buffer = true, silent = true })
      end
      vim.api.nvim_create_autocmd("User", {
        pattern = "MoltenInitPost",
        callback = function()
          keymap("n", "<localleader>e", ":MoltenEvaluateOperator<CR>", "Run operator selection")
          keymap("n", "<localleader>\\", ":MoltenEvaluateLine<CR>", "Run single line")
          keymap("n", "<localleader>r", ":MoltenReevaluateCell<CR>", "Re-evaluate cell")
          keymap("v", "<localleader><CR>", ":<C-u>MoltenEvaluateVisual<CR>gv", "Run visual selection")
          keymap("n", "<localleader>oh", ":MoltenHideOutput<CR>", "Hide output")
          keymap("n", "<localleader>oo", ":noautocmd MoltenEnterOutput<CR>", "Show/enter output")
        end,
      })
    end,
  },
  {
    -- see the image.nvim readme for more information about configuring this plugin
    "3rd/image.nvim",
    event = "VeryLazy",
    opts = {
      backend = "kitty", -- whatever backend you would like to use
      max_width = 100,
      max_height = 12,
      max_height_window_percentage = math.huge,
      max_width_window_percentage = math.huge,
      window_overlap_clear_enabled = true, -- toggles images when windows are overlapped
      window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
    },
  },
  {
    "GCBallesteros/jupytext.nvim",
    config = true,
    opts = { style = "percent" },
  },
}
