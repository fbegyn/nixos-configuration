{ config, pkgs, ... }:

{
  home-manager.users.francis.home.stateVersion = "22.05";
  imports = [
    ./variables.nix
    ./ssh.nix
    ./yubikey.nix
  ];

  virtualisation.podman.enable = true;
  virtualisation.docker.enable = true;
  environment.systemPackages = with pkgs; [
    podman-compose
  ];
}
