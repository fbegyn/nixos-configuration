{ config, lib, pkgs, ... }:

with lib; {

  options.francis.gui.enable = mkEnableOption "Enables GUI programs";

  config = {
    boot.cleanTmpDir = true;

    nix = {
      autoOptimiseStore = true;
      trustedUsers = [ "root" "francis" ];
    };

    nixpkgs.config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball
          "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
          };
      };
    };

    security.pam.loginLimits = [{
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "unlimited";
    }];
  };
}
