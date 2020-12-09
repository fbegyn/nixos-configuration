{ config, pkgs, ... }:

{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      master = import (builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/master.tar.gz") {
          config = config.nixpkgs.config;
        };
    };
  };
}
