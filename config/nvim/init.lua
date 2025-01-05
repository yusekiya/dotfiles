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
opt.cursorline=true

-- mapleader
-- Make sure that `mapleader` and `maplocalleader` is setup before loadin lazy.nvim.
vim.g.mapleader = ' '
vim.g.maplocalleader = "\\"

-- user defined commands
vim.api.nvim_create_user_command(
    "WClean",
    "%s/\\s\\+$//g",
    {}
)

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
setkey('', "<leader>h", "^")
setkey('', "<leader>l", "$")
setkey("i", "jk", "<Esc>")
setkey("n", "<Esc><Esc>", "<Cmd>nohlsearch<CR>", { silent = true })
setkey({"n", "v"}, "j", "gj")
setkey({"n", "v"}, "k", "gk")
setkey("v", "y", "y`]")
setkey("n", ";", ":")
setkey("n", ":", ";")
setkey("n", "+", "<C-a>")
setkey("n", "-", "<C-x>")
-- keybinds for pane
setkey("n", "<leader>sj", "<C-w>j")
setkey("n", "<leader>sk", "<C-w>k")
setkey("n", "<leader>sl", "<C-w>l")
setkey("n", "<leader>sh", "<C-w>h")
setkey("n", "<leader>sJ", "<C-w>J")
setkey("n", "<leader>sK", "<C-w>K")
setkey("n", "<leader>sL", "<C-w>L")
setkey("n", "<leader>sH", "<C-w>H")
setkey("n", "<leader>sr", "<C-w>r")
setkey("n", "<leader>sn", "gt")
setkey("n", "<leader>sp", "gT")
setkey("n", "<leader>s=", "<C-w>=")
setkey("n", "<leader>sw", "<C-w>w")
setkey("n", "<leader>so", "<C-w>_<C-w>|")
setkey("n", "<leader>sO", "<C-w>=")
setkey("n", "<leader>sN", "<Cmd><C-u>bn<CR>")
setkey("n", "<leader>sP", "<Cmd><C-u>bp<CR>")
setkey("n", "<leader>s-", "<Cmd>split<CR>")
setkey("n", "<leader>s|", "<Cmd>vsplit<CR>")

-- load plugins
-- current plugin manager: lazy.nvim
require("config.lazy")

