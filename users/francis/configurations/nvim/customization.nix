{ pkgs }:

let
  vimrc   = pkgs.callPackage ./init.vim.nix {};
  plugins = pkgs.callPackage ./plugins.nix {};
in
{
  customRC = vimrc;
  vam = {
    knownPlugins = pkgs.vimPlugins // plugins;
    pluginDictionaries = [
      {
        names = [
          "limelight-vim"
          "fugitive"
          "gitgutter"
          "supertab"
          "multiple-cursors"
          "nerdtree"
          "nerdtree-git-plugin"
          "vim-commentary"
          "fzf-vim"
          "ctrlp"
          "airline"
          "tmux-navigator"
          "surround"
          "vim-better-whitespace"
          "auto-pairs"
          "goyo"

          "LanguageClient-neovim"
          "deoplete"

          #"vimwiki"
          #"vim-zettel"
          "neuron.vim"

          "smart-tabs"

          "vim-go"

          "rust-vim"

          "alchemist-vim"
          "vim-elixir"

          "vim-nix"

          "vim-jsonnet"
          "vim-toml"
          "indentLine"
        ];
      }
    ];
  };
}
