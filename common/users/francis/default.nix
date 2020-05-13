{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  home.packages = with pkgs; [
    import (./nvim/customnvim.nix)
  ];
}
