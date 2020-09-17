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
