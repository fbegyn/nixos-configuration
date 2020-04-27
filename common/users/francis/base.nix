{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
  ];

  home.packages = with pkgs; [
    fzf
  ];

  programs.home-manager.enable = true;

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;

    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
    };
  };
}
