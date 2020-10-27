{ config, pkgs, ... }:

{
  imports = [
    ./cachix.nix
  ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
      unstable = import (builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz") {
          config = config.nixpkgs.config;
        };
    };
  };

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];

  boot = {
    extraModprobeConfig = ''
      options hid_apple iso_layout=0
    '';
  };
}
