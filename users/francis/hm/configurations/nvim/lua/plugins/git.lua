-- Git: fugitive + gitsigns
require("gitsigns").setup({
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "-" },
    changedelete = { text = "~" },
  },
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns
    local map = vim.keymap.set

    map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk" })
    map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Previous hunk" })
    map("n", "<leader>gp", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
    map("n", "<leader>gs", gs.stage_hunk, { buffer = bufnr, desc = "Stage hunk" })
    map("n", "<leader>gu", gs.undo_stage_hunk, { buffer = bufnr, desc = "Undo stage hunk" })
    map("n", "<leader>gR", gs.reset_hunk, { buffer = bufnr, desc = "Reset hunk" })
  end,
})

-- Fugitive keymaps
local map = vim.keymap.set
map("n", "<leader>gg", "<cmd>Git<CR>", { desc = "Git status" })
map("n", "<leader>gl", "<cmd>Git log --oneline<CR>", { desc = "Git log" })
map("n", "<leader>gr", "<cmd>e!<CR>", { desc = "Revert buffer" })
map("n", "<leader>gb", "<cmd>Git blame<CR>", { desc = "Git blame" })
