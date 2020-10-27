{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./ssh.nix
  ];

  virtualisation.docker.enable = true;
  virtualisation.podman.enable = true;
}
