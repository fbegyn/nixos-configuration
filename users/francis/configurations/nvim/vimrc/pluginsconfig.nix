let
    languageserver = builtins.readFile ./plugins/languageserver.vim;
    ctrlp = builtins.readFile ./plugins/ctrlp.vim;
    nerdtree = builtins.readFile ./plugins/nerdtree.vim;
    go = builtins.readFile ./plugins/go.vim;
    indentline = builtins.readFile ./plugins/indentline.vim;
    goyo = builtins.readFile ./plugins/goyo.vim;
    multicursor = builtins.readFile ./plugins/multicursor.vim;
    vimzettel = builtins.readFile ./plugins/vim-zettel.vim;
    deoplete = builtins.readFile ./plugins/deoplete.vim;
in

''
    ${languageserver}

    ${nerdtree}
    ${ctrlp}

    ${indentline}
    ${multicursor}

    ${deoplete}

    ${go}

    ${vimzettel}
''
