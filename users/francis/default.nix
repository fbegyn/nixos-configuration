{ config, pkgs, ... }:

{
  imports = [
    ./variables.nix
    ./ssh.nix
  ];

  environment.systemPackages = with pkgs; [
    exfat
  ];
}
