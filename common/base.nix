{ config, pkgs, ... }:

{
  imports = [
    ./cachix.nix
    ./unstable.nix
  ];

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];
}
