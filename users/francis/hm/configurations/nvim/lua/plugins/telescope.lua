-- Telescope: fuzzy finder
local telescope = require("telescope")
local builtin = require("telescope.builtin")

telescope.setup({
  defaults = {
    prompt_prefix = "   ",
    selection_caret = "  ",
    sorting_strategy = "ascending",
    layout_config = {
      horizontal = { prompt_position = "top" },
    },
  },
  pickers = {
    find_files = { hidden = true },
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },
})

telescope.load_extension("fzf")

local map = vim.keymap.set
map("n", "<leader><leader>", builtin.commands, { desc = "Commands" })
map("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
map("n", "<leader>fr", builtin.oldfiles, { desc = "Recent files" })
map("n", "<leader>bb", builtin.buffers, { desc = "Switch buffer" })
map("n", "<leader>sr", builtin.live_grep, { desc = "Ripgrep" })
map("n", "<leader>ss", builtin.current_buffer_fuzzy_find, { desc = "Search in buffer" })
map("n", "<leader>sh", builtin.help_tags, { desc = "Help tags" })
map("n", "<leader>sd", builtin.diagnostics, { desc = "Diagnostics" })
map("n", "<leader>sk", builtin.keymaps, { desc = "Keymaps" })
map("n", "<leader>pf", builtin.find_files, { desc = "Find in project" })
map("n", "<leader>pg", builtin.live_grep, { desc = "Grep in project" })
map("n", "<leader>pp", builtin.find_files, { desc = "Project files" })
