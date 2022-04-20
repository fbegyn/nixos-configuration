{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./ssh.nix
    ./yubikey.nix
  ];

  virtualisation.podman.enable = true;
  virtualisation.docker.enable = true;
}
