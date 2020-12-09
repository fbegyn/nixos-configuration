{ config, pkgs, ... }:

{
  imports = [
    ./cachix.nix
    ./unstable.nix
    ./nur.nix
  ];

  environment.systemPackages = with pkgs; [
    vim
    wget
  ];
}
