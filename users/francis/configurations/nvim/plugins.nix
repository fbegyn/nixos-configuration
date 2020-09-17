{ pkgs, fetchgit }:

let
  buildVimPlugin = pkgs.vimUtils.buildVimPluginFrom2Nix;
in {
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

  "smart-tabs" = buildVimPlugin {
    name = "smart-tabs";
    src = pkgs.fetchFromGitHub {
      owner = "dpc";
      repo = "vim-smarttabs";
      rev = "3ca9c53770f52a8807b923cc939e6f076ea71b6c";
      sha256 = "1ig7g06z0jg42lk6pcpdmfqar12xb2lhy8l59844kla4d5fid7xv";
    };
    dependencies = [];
  };

  "deoplete" = buildVimPlugin {
    name = "deoplete";
    src = pkgs.fetchFromGitHub {
      owner = "shougo";
      repo = "deoplete.nvim";
      rev = "e897e0142759eb7ffbded565389243cab6a09a91";
      sha256 = "00qvpp7r7wnccfzfxq9xa4cyxzr25zy32mpxscnbixc7cv5y981x";
    };
    dependencies = [];
  };

  "neuron.vim" = buildVimPlugin {
    name = "neuron.vim";
    src = pkgs.fetchFromGitHub {
      owner = "ihsanturk";
      repo = "neuron.vim";
      rev = "97cd8a2b521c56c6eec0889582ddfff590346db1";
      sha256 = "0mli0msvx2j28qq2h2abil4qwzyvxa5ikw7gyqyb3inmq31c70zi";
    };
    dependencies = [ ];
  };
}
