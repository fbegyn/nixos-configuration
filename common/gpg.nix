{ config, pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  environment.systemPackages = [
    pkgs.pinentry-gnome
  ];
}

