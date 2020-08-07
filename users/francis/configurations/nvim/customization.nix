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
      { name = "limelight-vim"; }
      { name = "fugitive"; }
      { name = "gitgutter"; }
      { name = "supertab"; }
      { name = "multiple-cursors"; }
      { name = "nerdtree"; }
      { name = "nerdtree-git-plugin"; }
      { name = "vim-commentary"; }
      #{ name = "fzf-source"; }
      #{ name = "fzf.vim"; }
      { name = "ctrlp"; }
      { name = "airline"; }
      { name = "tmux-navigator"; }
      { name = "surround"; }
      { name = "vim-better-whitespace"; }
      { name = "auto-pairs"; }
      { name = "goyo"; }

      { name = "ncm2"; }
      { name = "ncm2-bufword"; }
      { name = "ncm2-path"; }
      { name = "ncm2-tmux"; }
      { name = "ncm2-ultisnips"; }

      { name = "vimwiki"; }
      { name = "vim-zettel"; }

      { name = "youcompleteme"; }
      { name = "LanguageClient-neovim"; }
      { name = "vim-go"; }

      { name = "rust-vim"; }

      { name = "alchemist-vim"; }
      { name = "vim-elixir"; }
      #{ name = "vim-mix"; }

      { name = "vim-nix"; }

      { name = "vim-jsonnet"; }
      { name = "vim-toml"; }
      { name = "indentLine"; }

    ];
  };
}
