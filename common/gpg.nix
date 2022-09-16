{ config, pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  environment.systemPackages = [
    pkgs.pinentry-gtk2
  ];
}

