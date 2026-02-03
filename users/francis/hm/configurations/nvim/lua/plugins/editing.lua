-- Editing plugins: surround, comment, autopairs, undotree

-- nvim-surround
require("nvim-surround").setup({})

-- Comment.nvim
require("Comment").setup({})

-- nvim-autopairs (with cmp integration)
local autopairs = require("nvim-autopairs")
autopairs.setup({})

local cmp_autopairs = require("nvim-autopairs.completion.cmp")
local cmp = require("cmp")
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

-- Undotree
vim.keymap.set("n", "<leader>tu", "<cmd>UndotreeToggle<CR>", { desc = "Toggle undotree" })
