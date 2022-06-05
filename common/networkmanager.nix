{ config, pkgs, ... }:

{
  programs = {
    nm-applet.enable = true;
  };

  environment.systemPackages = with pkgs.unstable; [
    networkmanagerapplet
  ];

  networking.networkmanager.packages = with pkgs.unstable; [
    networkmanagerapplet
    libnma
  ];
}
