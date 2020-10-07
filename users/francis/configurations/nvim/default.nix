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

  customPlugins = {
    vim-zettel = pkgs.vimUtils.buildVimPlugin {
      name = "vim-zettel";
      src = pkgs.fetchFromGitHub {
        owner = "michal-h21";
        repo = "vim-zettel";
        rev = "15e011c3001fcee956b0eb705c1f5ac366c52ba9";
        sha256 = "0df5kxr08iqkn639bv3ls3saqh804fqvma7ns52h1a9lr6isz9cy";
      };
    };
    vim-neuron = pkgs.vimUtils.buildVimPlugin {
      name = "neuron.vim";
      src = pkgs.fetchFromGitHub {
        owner = "ihsanturk";
        repo = "neuron.vim";
        rev = "97cd8a2b521c56c6eec0889582ddfff590346db1";
        sha256 = "0mli0msvx2j28qq2h2abil4qwzyvxa5ikw7gyqyb3inmq31c70zi";
      };
    };
  };
in
{
  home-manager.users.francis = { pkgs, ... }: {
    home.sessionVariables = { EDITOR = "nvim"; };
    home.packages = with pkgs; [
      rnix-lsp
    ];
    programs.neovim = {
      enable = true;
      vimAlias = true;
      vimdiffAlias = true;
      plugins = with pkgs.vimPlugins // customPlugins; [
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
        #deoplete-nvim
        The_NERD_tree
        nerdtree-git-plugin
        #LanguageClient-neovim
        coc-nvim
        vim-neuron
        vim-zettel
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
  };
}
