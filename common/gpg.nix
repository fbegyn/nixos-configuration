{ config, pkgs, ... }:

{
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };

  environment.systemPackages = [
    pkgs.pinentry
    pkgs.pinentry_gtk2
  ];
}

