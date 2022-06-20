{ config, pkgs, ... }:

{
  programs = {
    nm-applet.enable = true;
  };

  environment.systemPackages = with pkgs.unstable; [
    networkmanagerapplet
    libnma
  ];

  # networking.networkmanager.packages = with pkgs.unstable; [
  # ];
}
