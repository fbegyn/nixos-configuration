-- Space-leader keybindings
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = vim.keymap.set

-- Use escape to clear search highlight
map("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlight" })

-- Window navigation (SPC w)
map("n", "<leader>wv", "<cmd>vsplit<CR>", { desc = "Split vertical" })
map("n", "<leader>ws", "<cmd>split<CR>", { desc = "Split horizontal" })
map("n", "<leader>wd", "<cmd>close<CR>", { desc = "Close window" })
map("n", "<leader>wh", "<C-w>h", { desc = "Window left" })
map("n", "<leader>wj", "<C-w>j", { desc = "Window down" })
map("n", "<leader>wk", "<C-w>k", { desc = "Window up" })
map("n", "<leader>wl", "<C-w>l", { desc = "Window right" })
map("n", "<leader>w=", "<C-w>=", { desc = "Equal width" })

-- Buffer management (SPC b)
map("n", "<leader>bd", "<cmd>bdelete<CR>", { desc = "Delete buffer" })
map("n", "<leader>bn", "<cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bprevious<CR>", { desc = "Previous buffer" })

-- File ops (SPC f)
map("n", "<leader>fs", "<cmd>w<CR>", { desc = "Save file" })

-- Quit (SPC q)
map("n", "<leader>qq", "<cmd>qa<CR>", { desc = "Quit all" })
map("n", "<leader>qw", "<cmd>wqa<CR>", { desc = "Save and quit all" })

-- Diagnostics
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line diagnostics" })
map("n", "<leader>cq", vim.diagnostic.setloclist, { desc = "Diagnostics to loclist" })

-- Better movement
map("n", "J", "mzJ`z", { desc = "Join lines (keep cursor)" })
map("n", "<C-d>", "<C-d>zz", { desc = "Half page down (center)" })
map("n", "<C-u>", "<C-u>zz", { desc = "Half page up (center)" })

-- Move lines in visual mode
map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })
