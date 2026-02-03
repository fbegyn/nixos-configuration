-- Toggleterm: terminal
require("toggleterm").setup({
  direction = "float",
  float_opts = {
    border = "curved",
  },
  open_mapping = nil,
})

vim.keymap.set("n", "<leader>'", "<cmd>ToggleTerm<CR>", { desc = "Toggle terminal" })
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
