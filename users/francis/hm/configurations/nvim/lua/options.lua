-- General vim options
local opt = vim.opt

-- Encoding
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

-- No backup/swap/lockfiles
opt.backup = false
opt.swapfile = false
opt.writebackup = false

-- Line numbers
opt.number = true
opt.relativenumber = true

-- Columns and wrapping
opt.colorcolumn = "101"
opt.signcolumn = "yes"
opt.wrap = false

-- Indentation: 4-space tabs
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.expandtab = true
opt.smartindent = true

-- Search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- Appearance
opt.termguicolors = true
opt.cursorline = true
opt.showmode = false
opt.laststatus = 3
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Splits
opt.splitbelow = true
opt.splitright = true

-- Completion
opt.completeopt = { "menu", "menuone", "noselect" }
opt.pumheight = 10

-- Misc
opt.mouse = "a"
opt.clipboard = "unnamedplus"
opt.undofile = true
opt.updatetime = 250
opt.timeoutlen = 300
opt.fillchars = { eob = " " }
opt.list = true
opt.listchars = { trail = "~", tab = "  " }
opt.showmatch = true
opt.confirm = true

-- Final newline
vim.opt.fixendofline = true

-- Disable netrw (nvim-tree takes over)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
