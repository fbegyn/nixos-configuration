{ pkgs, fetchgit }:

let
  buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix;
in {
  #"fzf.vim" = buildVimPlugin {
  #  name = "fzf.vim";
  #  src = pkgs.fetchFromGitHub {
  #    owner = "junegunn";
  #    repo = "fzf.vim";
  #    rev = "a74605b81d0dc2b28d35d9a1ab414dbdcbe3f45b";
  #    sha256 = "1i0lnkbymrjs12ryggqd9fafh0919n7k21afjmb5qb6c7cm15vsf";
  #  };
  #  dependencies = [];
  #};
  #"fzf-source" = buildVimPlugin {
  #  name = "fzf-source";
  #  src = pkgs.fetchFromGitHub {
  #    owner = "junegunn";
  #    repo = "fzf";
  #    rev = "97a725fbd0e54cbc07e4d72661ea2bd2bb7c01c1";
  #    sha256 = "1i0lnkbymrjs12ryggqd9fafh0919n7k21afjmb5qb6c7cm15vsf";
  #  };
  #  dependencies = [];
  #};
  "vim-zettel" = buildVimPlugin {
    name = "vim-zettel";
    src = pkgs.fetchFromGitHub {
      owner = "michal-h21";
      repo = "vim-zettel";
      rev = "15e011c3001fcee956b0eb705c1f5ac366c52ba9";
      sha256 = "0df5kxr08iqkn639bv3ls3saqh804fqvma7ns52h1a9lr6isz9cy";
    };
    dependencies = [];
  };
  #"vim-mix" = buildVimPlugin {
  #  name = "vim-mix";
  #  src = pkgs.fetchFromGitHub {
  #    owner = "mattreducs";
  #    repo = "vim-mix";
  #    rev = "92709183b21eb6881b5fcd077b8ce93a26df430e";
  #    sha256 = "0df5kxr08iqkn639bv3ls3saqh804fqvma7ns52h1a9lr6isz9cy";
  #  };
  #  dependencies = [];
  #};
}
