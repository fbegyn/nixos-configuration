{ config, pkgs, ... }:

{
  home-manager.users.francis.home.stateVersion = "22.05";
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./ssh.nix
    ./yubikey.nix
  ];

  virtualisation.podman.enable = true;
  environment.systemPackages = with pkgs; [
    podman-compose
  ];
}
