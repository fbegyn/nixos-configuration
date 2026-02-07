{ pkgs, ... }:
let
  treesitterWithGrammars = pkgs.vimPlugins.nvim-treesitter.withPlugins (p: [
    p.bash
    p.c
    p.css
    p.dockerfile
    p.elixir
    p.go
    p.gomod
    p.gosum
    p.heex
    p.html
    p.javascript
    p.json
    p.lua
    p.make
    p.markdown
    p.markdown_inline
    p.nix
    p.python
    p.rust
    p.toml
    p.tsx
    p.typescript
    p.vimdoc
    p.yaml
  ]);
in
{
  programs.neovim = {
    vimAlias = true;
    vimdiffAlias = true;

    # extraLuaConfig = builtins.readFile ./init.lua;

    plugins = with pkgs.vimPlugins; [
      # UI
      gruvbox-nvim
      lualine-nvim
      indent-blankline-nvim
      which-key-nvim
      nvim-web-devicons

      # Telescope
      telescope-nvim
      telescope-fzf-native-nvim

      # Completion
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      luasnip
      cmp_luasnip
      friendly-snippets

      # LSP
      nvim-lspconfig
      fidget-nvim

      # Treesitter
      treesitterWithGrammars

      # Git
      fugitive
      gitsigns-nvim

      # File tree
      nvim-tree-lua

      # Editing
      nvim-surround
      comment-nvim
      nvim-autopairs
      undotree

      # Terminal
      toggleterm-nvim

      # Org
      orgmode

      # Misc
      direnv-vim
    ];

    extraPackages = with pkgs; [
      # LSP servers
      gopls
      rust-analyzer
      nil
      elixir-ls
      pyright
      deno
      lua-language-server
      nodePackages.bash-language-server
      # ansible-language-server
      ansible-lint
      ruff

      # Tools used by telescope / plugins
      ripgrep
      fd
    ];
  };
}
