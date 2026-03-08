return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  config = function()
    local cmd = "cat"
    if vim.fn.executable("tte") == 1 then
      local cmd_table = {
        "tte",
        " --anchor-canvas s",
        -- " beams --beam-delay 2 --beam-row-speed-range 20-60 --beam-column-speed-range 8-12 --final-gradient-stops FFFFFF 00D1FF 8A008A",
        -- " scattered --movement-speed 0.2",
        -- " randomsequence --speed 0.01 --starting-color 2E3440 --final-gradient-stops BDFFEA AB9DFF FF9048",
        " expand --movement-speed 0.4 --expand-easing OUT_SINE --final-gradient-stops BDFFEA AB9DFF FF9048",
        " --final-gradient-direction diagonal",
        " <",
      }
      cmd = table.concat(cmd_table)
    end
    local logo_file = vim.fn.stdpath("config") .. "/logo/saturn.txt"
    vim.api.nvim_set_hl(0, "SnacksDashboardIcon", { fg = "#8fbcbb" })
    vim.api.nvim_set_hl(0, "SnacksDashboardDesc", { fg = "#d8dee9", bold = true })
    vim.api.nvim_set_hl(0, "SnacksDashboardKey", { fg = "#8fbcbb" })
    local opts = {
      bigfile = { enabled = false },
      explorer = { enabled = false },
      indent = {
        hl = "SnacksIndent",
        scope = {
          hl = "SnacksIndentScope",
        },
      },
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
              ["<c-s>"] = { "edit_split", mode = { "i", "n" } },
              ["<c-v>"] = { "edit_vsplit", mode = { "i", "n" } },
            },
          },
        },
      },
      notifier = { enabled = false },
      quickfile = { enabled = false },
      scope = { enabled = false },
      scroll = { enabled = false },
      statuscolumn = { enabled = false },
      terminal = {},
      win = {
        position = "float",
        height = 0.9,
        width = 0.9,
        zindex = 50,
      },
      words = { enabled = false },
      dashboard = {
        width = 66,
        row = nil,
        col = nil,
        preset = {
          keys = {
            { icon = "  > ", key = "n", desc = "New file", action = ":ene | startinsert" },
            { icon = "  > ", key = "r", desc = "Recent files", action = ":lua Snacks.dashboard.pick('oldfiles')" },
            { icon = "  > ", key = "e", desc = "File explorer", action = ":Neotree" },
            { icon = "󰱼  > ", key = "f", desc = "Find file", action = ":lua Snacks.picker.smart()" },
            { icon = "󱎸  > ", key = "g", desc = "Find text", action = ":lua Snacks.picker.grep()" },
            { icon = "󰒲  > ", key = "L", desc = "Lazy", action = ":Lazy", enabled = package.loaded.lazy ~= nil },
            { icon = "  > ", key = "q", desc = "Quit", action = ":qa" },
          },
        },
        sections = {
          { section = "terminal", cmd = cmd .. " " .. logo_file, height = 23, padding = 1, ttl = 5 * 60 },
          { section = "keys", gap = 1, padding = 1 },
          { icon = " ", title = "Projects", section = "projects", indent = 2, padding = 2 },
          { section = "startup" },
        },
      },
    }
    require("snacks").setup(opts)
  end,
  keys = {
    -- find
    {
      "<leader>fb",
      function()
        Snacks.picker.buffers()
      end,
      desc = "Buffers",
    },
    {
      "<leader>ff",
      function()
        Snacks.picker.smart()
      end,
      desc = "Smart Find Files",
    },
    -- grep
    {
      "<leader>fg",
      function()
        Snacks.picker.grep({ matcher = { cwd_bonus = true } })
      end,
      desc = "Grep",
    },
    -- commands
    {
      "<leader>fc",
      function()
        Snacks.picker.commands()
      end,
      desc = "Find Commands",
    },
  },
  requires = { "nvim-tree/nvim-web-devicons" },
}
