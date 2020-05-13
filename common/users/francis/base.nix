{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./python.nix
    ./tmux
  ];

  home.packages = let
    nvimpackage = import ./nvim/package.nix pkgs;
  in
   nvimpackage ++ [
     pkgs.fzf
     pkgs.unzip
   ];

  programs.home-manager.enable = true;
  services.lorri.enable = true;

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
