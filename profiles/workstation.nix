# Shared baseline for the interactive machines (bia, ania, geras).
{ config, pkgs, ... }:

{
  imports = [
    ../common/bluetooth.nix
    ../common/eid.nix
    ../common/fonts.nix
    ../common/gpg.nix
    ../common/ios.nix
    ../common/moonlander.nix
    ../common/network-tools.nix
    ../common/printer.nix
    ../common/video-accel.nix
    ../common/webcam.nix

    ../users
    ../users/hm.nix
    ../users/francis
    ../users/francis/gui.nix
    ../users/francis/niri
  ];

  time.timeZone = "Europe/Brussels";

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client";
  };

  home-manager.users.francis.emacs.emacsPackage = pkgs.unstable.emacs31-pgtk;
  services.emacs = {
    enable = true;
    package = config.home-manager.users.francis.emacs.package;
  };
}
