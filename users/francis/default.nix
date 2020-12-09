{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./ssh.nix
  ];

  virtualisation.podman.enable = true;
}
