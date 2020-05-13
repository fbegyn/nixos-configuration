let
    languageserver = builtins.readFile ./plugins/languageserver.vim;
    ctrlp = builtins.readFile ./plugins/ctrlp.vim;
    nerdtree = builtins.readFile ./plugins/nerdtree.vim;
    go = builtins.readFile ./plugins/go.vim;
    indentline = builtins.readFile ./plugins/indentline.vim;
    goyo = builtins.readFile ./plugins/goyo.vim;
    multicursor = builtins.readFile ./plugins/multicursor.vim;
in

''
    ${languageserver}

    ${nerdtree}
    ${ctrlp}

    ${indentline}
    ${multicursor}

    ${go}
''
