{ pkgs, stdenv, ... }:
let
  general     = builtins.readFile ./vimrc/general.vim;
  movements     = builtins.readFile ./vimrc/movements.vim;
  tabsbufs     = builtins.readFile ./vimrc/tabsbuffers.vim;
  colors     = builtins.readFile ./vimrc/colors.vim;
  srcry = builtins.readFile ./vimrc/srcery.vim;

  go     = builtins.readFile ./vimrc/languages/go.vim;
  rust     = builtins.readFile ./vimrc/languages/rust.vim;

  goplugin = builtins.readFile ./vimrc/plugins/go.vim;

  ctrlp = builtins.readFile ./vimrc/plugins/ctrlp.vim;
  nerdtree = builtins.readFile ./vimrc/plugins/nerdtree.vim;
  indentline = builtins.readFile ./vimrc/plugins/indentline.vim;
  goyo = builtins.readFile ./vimrc/plugins/goyo.vim;
  multicursor = builtins.readFile ./vimrc/plugins/multicursor.vim;
  vimzettel = builtins.readFile ./vimrc/plugins/vim-zettel.vim;
  goPlugin = builtins.readFile ./vimrc/plugins/go.vim;
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      limelight-vim
      fugitive
      gitgutter
      multiple-cursors
      fzf-vim
      airline
      surround
      vim-better-whitespace
      auto-pairs
      indentLine
      tmux-navigator
      goyo-vim
      vimtex
      vim-go
      The_NERD_tree
      #nerdtree-git-plugin
      coc-nvim
      vim-ledger
      vim-nix
    ];
    extraConfig = ''
      ${general}
      ${srcry}
      ${colors}
      ${movements}
      ${tabsbufs}

      ${go}
      ${rust}

      ${nerdtree}
      ${ctrlp}
      ${indentline}
      ${multicursor}
      ${goyo}
      ${vimzettel}

      ${goplugin}
    '';
  };
}
