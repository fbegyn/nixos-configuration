{ pkgs, lib, ... }:

let
  nvim = pkgs.neovim.override {
    # don't alias neovim to vim, yet.
    vimAlias = true;

    configure = (import ./customization.nix { pkgs = pkgs; });
  };
in [
  nvim
  pkgs.python
  pkgs.ctags
]
