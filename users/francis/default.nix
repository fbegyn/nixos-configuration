{ config, pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ./variables.nix
    ./ssh.nix
  ];

  services.emacs = {
    enable = false;
    install = true;
  };

  virtualisation.podman.enable = true;
}
