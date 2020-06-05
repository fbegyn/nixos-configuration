{ stdenv, writeText }:

let
    go     = builtins.readFile ./languages/go.vim;
in

''
    ${go}
''
