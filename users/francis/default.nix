{ config, pkgs, ... }:

{
  imports = [
    ./variables.nix
    ./ssh.nix
  ];

  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    exfat
  ];
}
