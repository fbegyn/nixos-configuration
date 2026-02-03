-- UI plugins: gruvbox, lualine, indent-blankline, which-key

-- Gruvbox
require("gruvbox").setup({
  contrast = "hard",
})
vim.cmd.colorscheme("gruvbox")

-- Lualine
require("lualine").setup({
  options = {
    theme = "gruvbox",
    section_separators = "",
    component_separators = "|",
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
})

-- Indent-blankline
require("ibl").setup({
  indent = { char = "|" },
  scope = { enabled = true },
})

-- Which-key
local wk = require("which-key")
wk.setup({
  delay = 250,
})
wk.add({
  { "<leader>b", group = "buffer" },
  { "<leader>c", group = "code" },
  { "<leader>f", group = "file" },
  { "<leader>g", group = "git" },
  { "<leader>q", group = "quit" },
  { "<leader>s", group = "search" },
  { "<leader>t", group = "toggle" },
  { "<leader>w", group = "window" },
})
