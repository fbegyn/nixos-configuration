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
      { name = "youcompleteme"; }
      { name = "LanguageClient-neovim"; }
      { name = "goyo"; }
      { name = "limelight-vim"; }
      { name = "vim-go"; }
      { name = "fugitive"; }
      { name = "gitgutter"; }
      { name = "supertab"; }
      { name = "multiple-cursors"; }
      { name = "nerdtree"; }
      { name = "nerdtree-git-plugin"; }
      { name = "vim-commentary"; }
      #{ name = "fzf"; }
      { name = "ctrlp"; }
      { name = "airline"; }
      #{ name = "srcery-theme"; }
      { name = "tmux-navigator"; }
      { name = "surround"; }
      { name = "vim-better-whitespace"; }
      { name = "auto-pairs"; }
      { name = "rust-vim"; }
      { name = "vim-jsonnet"; }
      { name = "vim-toml"; }

      { name = "ncm2"; }
      { name = "ncm2-bufword"; }
      { name = "ncm2-path"; }
      { name = "ncm2-tmux"; }
      { name = "ncm2-ultisnips"; }
    ];
  };
}
