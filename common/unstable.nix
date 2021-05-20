{ config, pkgs, ... }:

let
  browser-eid-overlay = import ../overlays/browser-eid.nix;
in {
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import (builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
          config = config.nixpkgs.config;
          overlays = [
            browser-eid-overlay
          ];
        };
    };
  };
}
