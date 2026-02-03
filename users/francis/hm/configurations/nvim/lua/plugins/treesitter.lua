-- Treesitter: highlight + indent (grammars installed via Nix)
require("nvim-treesitter.configs").setup({
  -- Grammars are managed by Nix, not installed by treesitter
  auto_install = false,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = { "org" },
  },
  indent = {
    enable = true,
  },
})
