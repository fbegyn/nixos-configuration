{ pkgs, fetchgit }:

let
  buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix;
in {
  #"vim-trailing-whitespace" = buildVimPlugin {
  #  name = "vim-trailing-whitespace";
  #  src = fetchgit {
  #    url = "https://github.com/bronson/vim-trailing-whitespace";
  #    rev = "d4ad27de051848e544e360482bdf076b154de0c1";
  #    sha256 = "594769a6f901407609b635a5041966456bfd91b13437169a4562857544e1dca3";
  #  };
  #  dependencies = [];
  #};
  "fzf" = buildVimPlugin {
    name = "fzf";
    src = pkgs.fetchFromGitHub {
      owner = "junegunn";
      repo = "fzf.vim";
      rev = "a74605b81d0dc2b28d35d9a1ab414dbdcbe3f45b";
      sha256 = "10l01a8xaivz6n01x6hzfx7gd0igd0wcf9ril0sllqzbq7yx2bbk";
    };
    dependencies = [];
  };
}
