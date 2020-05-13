{ stdenv, writeText }:

let
    general     = builtins.readFile ./vimrc/general.vim;
    movements     = builtins.readFile ./vimrc/movements.vim;
    tabsbufs     = builtins.readFile ./vimrc/tabsbuffers.vim;
    colors     = builtins.readFile ./vimrc/colors.vim;
    srcry = builtins.readFile ./vimrc/srcery.vim;

    languages = import ./vimrc/languages.nix;

    plug = import ./vimrc/pluginsconfig.nix;
in

''
    ${general}

    ${srcry}
    ${colors}

    ${movements}

    ${tabsbufs}

    ${plug}
''
