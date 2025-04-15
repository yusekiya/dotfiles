-- options
local opt = vim.o
opt.encoding = "utf-8"
vim.scriptencoding = "utf-8"
opt.ambiwidth = "single"
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.autoindent = true
opt.smartindent = true
opt.visualbell = false
opt.number = true
opt.showmatch = false
opt.matchtime = 1
opt.ignorecase = true
opt.smartcase = true
opt.shiftround = true
opt.virtualedit = "block"
opt.colorcolumn = "100"
opt.spell = true
opt.spelllang = "en,cjk"
opt.pumheight = 20
opt.scrolloff = 3
opt.clipboard = "unnamedplus"
opt.wildmode = "longest:full,full"
opt.cursorline = true
opt.termguicolors = true

-- mapleader
-- Make sure that `mapleader` and `maplocalleader` is setup before loadin lazy.nvim.
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Sync clipboard between OS and Neovim.
-- Function to set OSC 52 clipboard
local function set_osc52_clipboard()
  local function paste()
    return {
      vim.fn.split(vim.fn.getreg(""), "\n"),
      vim.fn.getregtype(""),
    }
  end

  vim.g.clipboard = {
    name = "OSC 52",
    copy = {
      ["+"] = require("vim.ui.clipboard.osc52").copy("+"),
      ["*"] = require("vim.ui.clipboard.osc52").copy("*"),
    },
    paste = {
      ["+"] = paste,
      ["*"] = paste,
    },
  }
end

local function is_running_on_wezterm_mux_server()
  local wezexe = os.getenv("WEZTERM_EXECUTABLE")
  if wezexe ~= nil then
    return wezexe:find("wezterm-mux-server", 1, true) or nil
  else
    return nil
  end
end

if os.getenv("SSH_CLIENT") ~= nil or os.getenv("SSH_TTY") ~= nil or is_running_on_wezterm_mux_server() ~= nil then
  set_osc52_clipboard()
end

-- user defined commands
vim.api.nvim_create_user_command("WClean", "%s/\\s\\+$//g", {})

-- keybindings
local setkey = vim.keymap.set
-- commands
setkey("ca", "wc", "WClean")
-- emacs-like cursor motion in insert mode
setkey("i", "<C-a>", "<Home>")
setkey("i", "<C-e>", "<End>")
setkey("i", "<C-b>", "<Left>")
setkey("i", "<C-f>", "<Right>")
setkey("i", "<C-d>", "<Del>")
setkey("i", "<C-k>", "<Esc>lc$")
setkey("i", "<C-y>", "<Esc>pa")
-- others
setkey({ "n", "v" }, "gh", "^")
setkey({ "n", "v" }, "gl", "$")
setkey("i", "jk", "<Esc>")
setkey("n", "<Esc><Esc>", "<Cmd>nohlsearch<CR>", { silent = true })
setkey({ "n", "v" }, "j", "gj")
setkey({ "n", "v" }, "k", "gk")
setkey("v", "y", "y`]")
setkey({ "n", "x" }, ";", ":")
setkey({ "n", "x" }, ":", ";")
setkey("n", "+", "<C-a>")
setkey("n", "-", "<C-x>")
-- keybinds for pane
setkey("n", "<leader>sj", "<C-w>j", { desc = "Go to the down window" })
setkey("n", "<leader>sk", "<C-w>k", { desc = "Go to the up window" })
setkey("n", "<leader>sl", "<C-w>l", { desc = "Go to the right window" })
setkey("n", "<leader>sh", "<C-w>h", { desc = "Go to the left window" })
setkey("n", "<leader>sJ", "<C-w>J", { desc = "Move window to far bottom" })
setkey("n", "<leader>sK", "<C-w>K", { desc = "Move window to far top" })
setkey("n", "<leader>sL", "<C-w>L", { desc = "Move window to far right" })
setkey("n", "<leader>sH", "<C-w>H", { desc = "Move window to far left" })
setkey("n", "<leader>sr", "<C-w>r", { desc = "Rotate windows upwards" })
setkey("n", "<leader>sn", "gt")
setkey("n", "<leader>sp", "gT")
setkey("n", "<leader>s=", "<C-w>=", { desc = "Equally high and wide" })
setkey("n", "<leader>sw", "<C-w>w", { desc = "Switch windows" })
setkey("n", "<leader>so", "<C-w>_<C-w>|", { desc = "Maximize window" })
setkey("n", "<leader>s-", "<Cmd>split<CR>", { desc = "Split window horizontally" })
setkey("n", "<leader>s|", "<Cmd>vsplit<CR>", { desc = "Split window vertically" })
-- keybinds for terminal
setkey("t", "<C-q>", [[<C-\><C-n>]], { desc = "Terminal normal mode" })
-- keybinds for commands
-- The following settings are made with reference to the comment at the below URL:
-- https://github.com/neovim/neovim/issues/9953#issuecomment-1732700161
vim.api.nvim_set_keymap("c", "<Up>", 'wildmenumode() ? "<Left>" : "<Up>"', { expr = true, noremap = true })
vim.api.nvim_set_keymap("c", "<Down>", 'wildmenumode() ? "<Right>" : "<Down>"', { expr = true, noremap = true })
vim.api.nvim_set_keymap("c", "<Left>", 'wildmenumode() ? "<Up>" : "<Left>"', { expr = true, noremap = true })
vim.api.nvim_set_keymap("c", "<Right>", 'wildmenumode() ? "<Down>" : "<Right>"', { expr = true, noremap = true })

-- load plugins
-- current plugin manager: lazy.nvim
require("config.lazy")
